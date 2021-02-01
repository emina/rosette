#lang racket

(require racket/exn
         rackunit
         rosette/lib/trace/compile
         rosette/lib/roseunit
         rosette/lib/trace/tool
         rosette/lib/util/syntax
         (only-in rosette clear-state!)
         racket/runtime-path
         "../config.rkt")

(define overwrite-mode
  (match (current-command-line-arguments)
    [(vector "overwrite-on-demand") 'on-demand]
    [(vector "overwrite") 'overwrite]
    [_ #f]))

(printf "Current mode: ~a\n" (current-command-line-arguments))

;; convert a path to a filename string so that it can be written and read back
(define (path->name p)
  (match-define-values (_ fname _) (split-path p))
  (path->string fname))

;; normalize datum in a way that is idempotent
(define (serialize e)
  (call-with-input-string (with-output-to-string (thunk (write e))) read))

(define (format-output stats trace _original-map)
  (define (format-activated-syntax activated-syntax)
    (match-define (list stx path line col) activated-syntax)
    (list stx (path->name path) line col))


  (define (format-stack-elem/tail proc path line col)
    (list (or (object-name proc) proc) (path->name path) line col))
  (define (format-each-stack-elem/tail elem)
    (match elem
      [#f #f]
      [(list 'uncertified _ _) #f]
      [(list _ elem _) (apply format-stack-elem/tail elem)]))
  (define (format-first-stack-elem/tail xs)
    (match xs
      ['() '()]
      [(cons #f xs) (format-first-stack-elem/tail xs)]
      [(cons (list 'certified a b) _) #:when (eq? a b) '()]
      [(cons (list _ _ elem) _) (list (apply format-stack-elem/tail elem))]))


  (define (format-trace-elem elem)
    (match-define (list ex activated-syntax activated-stack pc) elem)
    ;; get rid of path information
    (define ex-out
      (parameterize ([error-print-context-length 0])
        (substring-top (exn->string ex) 40)))
    (define activated-syntax-out (format-activated-syntax activated-syntax))
    (define activated-stack-out
      (append (format-first-stack-elem/tail activated-stack)
              (filter-map format-each-stack-elem/tail activated-stack)))
    (list ex-out activated-syntax-out activated-stack-out pc))
  (serialize
   `((#:stats ,stats)
     (#:trace ,(map format-trace-elem (take-top trace 10))))))

(define (take-top xs n)
  (for/list ([x (in-list xs)] [_ (in-range n)]) x))

(define (substring-top s n)
  (cond
    [(<= (string-length s) n) s]
    [else (string-append (substring s 0 n) "....")]))

(define ns (current-namespace))

(define (user-agree?)
  (printf "Overwrite this file? (y/N): ")
  (equal? "y" (string-trim (read-line))))

(define (run-trace-test fname mode)
  (define (perform-test output)
    (define outpath (build-path here-dir "output" mode (~a fname ".out")))
    (define (do-overwrite)
      (with-output-to-file outpath #:exists 'replace
        (thunk (pretty-write output)))
      (printf "Wrote new output for `~a`: ~v\n" fname output))
    (cond
      [(file-exists? outpath)
       (cond
         [(not overwrite-mode)
          (check-equal? output (call-with-input-file outpath read))]
         [(eq? overwrite-mode 'overwrite) (do-overwrite)]
         [else (check-equal? output (call-with-input-file outpath read))
               (when (and (not (equal? output (call-with-input-file outpath read)))
                          (user-agree?))
                 (do-overwrite))])]
      [else (do-overwrite)]))

  (define (exn-handler e)
    ((error-display-handler)
     (if (exn? e)
         (exn-message e)
         (~a e))
     e)
    (perform-test
     `((#:error ,(parameterize ([error-print-context-length 0]) (exn->string e))))))

  (clear-state!)

  (parameterize ([current-compile symbolic-trace-compile-handler]
                 [current-namespace (make-base-namespace)]
                 [error-print-width default-error-print-width])
    (namespace-attach-module ns 'rosette)
    (with-handlers ([exn:fail? exn-handler])
      (do-trace
       (thunk
        (dynamic-require `(file ,(path->string (build-path here-dir "code" fname))) #f))
       #:entry-handler
       (λ (entry add-trace! _current-original-map)
         (add-trace! entry))
       #:post-proc
       (λ (stats trace original-map)
         (perform-test (format-output stats trace original-map)))))))

(define-runtime-path here-dir ".")

(define (run-mode tests mode params)
  (for/list ([filename (in-list tests)])
    (define test-file (string->path filename))
    (test-suite+
     (~a (path-string->string test-file) " (" mode ")")
     (let loop ([params params])
       (match params
         ['() (run-trace-test test-file mode)]
         [(cons (list p v) params) (parameterize ([p v]) (loop params))])))))

(define regular-tests '("ex-1-1.rkt"
                        "ex-1-2.rkt"
                        "ex-1-3.rkt"
                        "ex-2.rkt"
                        "ex-3.rkt"
                        "tail.rkt"
                        "non-tail.rkt"
                        "core-form.rkt"
                        "assertion.rkt"
                        "if.rkt"
                        "infeasible.rkt"
                        "macro.rkt"
                        "macro-define.rkt"
                        "solver-limitation.rkt"
                        "test-track-form.rkt"
                        "error.rkt"
                        "forall.rkt"
                        "infeasible-solver.rkt"
                        "list.rkt"
                        "list-2.rkt"
                        "no-error.rkt"
                        "test-stack.rkt"
                        "toplevel.rkt"))

(define solver-tests '("assertion.rkt"
                       "if.rkt"
                       "infeasible-solver.rkt"
                       "infeasible.rkt"
                       "solver-limitation.rkt"))

(define assertion-tests '("assertion.rkt"
                          "if.rkt"))

(define all-tests '("ex-1-1.rkt"
                    "ex-1-2.rkt"
                    "ex-1-3.rkt"
                    "ex-2.rkt"
                    "ex-3.rkt"))

(define regular-suites
  (run-mode regular-tests "regular" `()))
(define solver-suites
  (run-mode solver-tests "solver" `([,symbolic-trace-skip-infeasible-solver? #t])))
(define assertion-suites
  (run-mode assertion-tests "assertion" `([,symbolic-trace-skip-assertion? #t])))
(define all-suites
  (run-mode all-tests "all" `([,symbolic-trace-skip-infeasible-solver? #t]
                              [,symbolic-trace-skip-assertion? #t])))

(module+ test
  (require rackunit/text-ui)

  (for-each run-tests regular-suites)
  (for-each run-tests solver-suites)
  (for-each run-tests assertion-suites)
  (for-each run-tests all-suites))
