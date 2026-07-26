#lang racket

(require "server.rkt" "cmd.rkt" "enc.rkt" "env.rkt" 
         "../solution.rkt" 
         (only-in racket [remove-duplicates unique])
         (only-in "smtlib2.rkt" reset set-option check-sat get-model get-unsat-core push pop set-logic)
         (only-in "../../base/core/term.rkt" expression expression? term term? term-type get-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bitvector-size bv bv?
                  @bvshl @bvlshr @bvor @bvrol @bvror @bvsub @bvurem)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         (only-in "../../base/core/reporter.rkt" current-reporter))

(provide (all-defined-out))

(define (unique/reverse xs)
  (reverse (unique xs)))

(define (find-solver binary base-path [user-path #f])
  (cond
    [(and (path-string? user-path) (file-exists? user-path)) user-path]
    [(file-exists? base-path) base-path]
    [(file-exists? (path-replace-suffix base-path ".exe")) (path-replace-suffix base-path ".exe")]
    [else (or (find-executable-path binary) #f)]))


(define (make-send-options conf)
  (match-define (config options _ logic) conf)
  (lambda (server)
    (server-write server
      (unless (false? logic)
        (set-logic logic))
      (for ([opt (in-list (sort (hash-keys options) symbol<?))])
        (set-option opt (hash-ref options opt))))))


(struct solver (server config asserts mins maxs env level)
  #:mutable)


(struct config (options path logic))


(define (solver-assert self bools [wfcheck #f])
  (unless (list? bools)
    (raise-argument-error 'solver-assert "(listof boolean?)" bools))
  (define wfcheck-cache (mutable-set))
  (set-solver-asserts!
   self
   (append (for/list ([b bools] #:unless (equal? b #t))
             (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
               (error 'assert "expected a boolean value, given ~s" b))
             (when wfcheck
               (wfcheck b wfcheck-cache))
             b)
           (solver-asserts self))))

(define (solver-minimize self nums)
  (unless (null? nums)
    (error 'solver-minimize "optimization isn't supported by solver ~v" self)))
   
(define (solver-maximize self nums)
  (unless (null? nums)
    (error 'solver-maximize "optimization isn't supported by solver ~v" self)))
   
(define (solver-clear self)
  (solver-shutdown self))
   
(define (solver-shutdown self)
  (solver-clear-stacks! self)
  (solver-clear-env! self)
  (server-shutdown (solver-server self)))

(define (solver-push self)
  (match-define (solver server _ (app unique asserts) (app unique/reverse mins) (app unique/reverse maxs) env level) self)
  (server-write
   server
   (begin
     ((current-reporter) 'encode-start)
     (encode self env asserts mins maxs)
     ((current-reporter) 'encode-finish)
     (push)))
  (solver-clear-stacks! self)
  (set-solver-level! self (cons (dict-count env) level)))
   
(define (solver-pop self [k 1])
  (match-define (solver server _ _ _ _ env level) self)
  (when (or (<= k 0) (> k (length level)))
    (error 'solver-pop "expected 1 <= k <= ~a, given ~a" (length level) k))
  (server-write server (pop k))
  (solver-clear-stacks! self)
  (for ([lvl level][i k])
    (clear! env lvl))
  (set-solver-level! self (drop level k)))
     
(define (solver-check self [read-solution read-solution])
  (match-define (solver server _ (app unique asserts) (app unique/reverse mins) (app unique/reverse maxs) env _) self)
  (cond [(ormap false? asserts) (unsat)]
        [else (server-write
               server
               (begin
                 ((current-reporter) 'encode-start)
                 (encode self env asserts mins maxs)
                 ((current-reporter) 'encode-finish)
                 (check-sat)))
              ((current-reporter) 'solve-start)
              (solver-clear-stacks! self)
              (define ret (read-solution server env))
              ((current-reporter) 'solve-finish (sat? ret))
              ret]))
   
(define (solver-debug self)
  (error 'solver-debug "debugging isn't supported by solver ~v" self))

(define (solver-options self)
  (config-options (solver-config self)))

; x and y must be bitvectors (not unions) of the same length.
; shift1 and shift2 are shift operators.
(define-syntax-rule (bvrotate x y shift1 shift2)
  (let* ([sz (bitvector-size (get-type y))]
         [n (bv sz sz)]
         [amount (@bvurem y n)])
    (@bvor (shift1 x amount) (shift2 x (@bvsub n amount)))))

; Implement basic support for non-indexed bit rotation operations which works
; on all solvers that do not implement a specific extension for it. This
; reduces the bit rotation to a bitwise or of left shift and right shift.
; A solver which has a dedicated SMT-LIB extension for this operation could
; override this method to encode directly to that extension. This results in
; significantly improved performance with, e.g., Z3, so bit rotation SMT-LIB
; extensions are preferred when available.
(define (solver-custom-encode self expr env quantified)
  (match expr
    [(expression (== @bvrol) x y)
     (enc self (bvrotate x y @bvshl @bvlshr) env quantified)]
    [(expression (== @bvror) x y)
     (enc self (bvrotate x y @bvlshr @bvshl) env quantified)]
    [_ #f]))

(define (solver-clear-stacks! self)
  (set-solver-asserts! self '())
  (set-solver-mins! self '())
  (set-solver-maxs! self '()))

(define (solver-clear-env! self)
  (set-solver-env! self (env))
  (set-solver-level! self '()))


; Reads the SMT solution from the server.
; The solution consists of 'sat or 'unsat, followed by
; followed by a suitably formatted s-expression.  The
; output of this procedure is a hashtable from constant
; identifiers to their SMTLib values (if the solution is 'sat);
; a non-empty list of assertion identifiers that form an
; unsatisfiable core (if the solution is 'unsat and a
; core was extracted); #f (if the solution is
; 'unsat and no core was extracted); or 'unknown otherwise.
(define (read-solution server env #:unsat-core? [unsat-core? #f])
  (decode (parse-solution server #:unsat-core? unsat-core?) env))

(define (parse-solution server #:unsat-core? [unsat-core? #f])
  (parameterize ([current-readtable (make-readtable #f #\# #\a #f)]) ; read BV literals as symbols
    (match (server-read server (read))
      [(== 'sat)
       (server-write server (get-model))
       (let loop ()
         (match (server-read server (read))
           [(list (== 'objectives) _ ...) (loop)]
           ; The SMT-LIB spec says that a model should be just a list of
           ; `define-fun`s, but many SMT solvers used to prefix that list
           ; with `model`, so let's support both versions.
           ; https://groups.google.com/g/smt-lib/c/5xpcIxdQ8-A/m/X4uQ7dIgAwAJ
           [(or (list (== 'model) def ...) (list def ...))
            (for/hash ([d def] #:when (and (pair? d) (equal? (car d) 'define-fun)))
              (values (cadr d) d))]
           [other (error 'read-solution "expected model, given ~a" other)]))]
      [(== 'unsat)
       (if unsat-core?
           (begin
             (server-write server (get-unsat-core))
             (match (server-read server (read))
               [(list (? symbol? name) ...) name]
               [other (error 'read-solution "expected unsat core, given ~a" other)]))
           'unsat)]
      [(== 'unknown) 'unknown]
      [other (error 'read-solution "unrecognized solver output: ~a" other)])))
