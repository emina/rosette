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

(define overwriting? (match (current-command-line-arguments)
                       [(vector "overwrite") #t]
                       [_ #f]))

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
  (define (format-activated-stack-elem elem)
    (match-define (list proc path line col) elem)
    (list (or (object-name proc) proc) (path->name path) line col))
  (define (format-trace-elem elem)
    (match-define (list ex activated-syntax activated-stack pc) elem)
    ;; get rid of path information
    (define ex-out
      (parameterize ([error-print-context-length 0])
        (substring-top (exn->string ex) 40)))
    (define activated-syntax-out (format-activated-syntax activated-syntax))
    (define activated-stack-out (map format-activated-stack-elem activated-stack))
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

(define (run-trace-test fname mode)
  (define (perform-test output)
    (define outpath (build-path here-dir "output" mode (~a fname ".out")))
    (cond
      [(and (file-exists? outpath) (not overwriting?))
       (check-equal? output (call-with-input-file outpath read))]
      [else
       (with-output-to-file outpath #:exists 'replace
         (thunk (pretty-write output)))
       (printf "Wrote new output for `~a`: ~v\n" fname output)]))

  (define (exn-handler e)
    (perform-test
     `((#:error ,(parameterize ([error-print-context-length 0]) (exn->string e))))))

  (parameterize ([current-compile symbolic-trace-compile-handler]
                 [error-print-width default-error-print-width])
    (clear-state!)
    (with-handlers ([exn:fail? exn-handler])
      (do-trace
       (thunk
        (dynamic-require `(file ,(path->string (build-path here-dir "code" mode fname))) #f))
       #:entry-handler
       (λ (entry add-trace! _current-original-map)
         (add-trace! entry))
       #:post-proc
       (λ (stats trace original-map)
         (perform-test (format-output stats trace original-map)))))))

(define-runtime-path here-dir ".")

(define (run-mode mode params)
  (for/list ([test-file (directory-list (build-path here-dir "code" mode))])
    (test-suite+
     (~a (path-string->string test-file) " (" mode ")")
     (let loop ([params params])
       (match params
         ['() (run-trace-test test-file mode)]
         [(cons (list p v) params) (parameterize ([p v]) (loop params))])))))

(define regular-suites
  (run-mode "regular" `()))
(define solver-suites
  (run-mode "solver" `([,symbolic-trace-skip-infeasible-solver? #t])))
(define assertion-suites
  (run-mode "assertion" `([,symbolic-trace-skip-assertion? #t])))
(define all-suites
  (run-mode "all" `([,symbolic-trace-skip-infeasible-solver? #t]
                    [,symbolic-trace-skip-assertion? #t])))

(module+ test
  (require rackunit/text-ui)

  (for-each run-tests regular-suites)
  (for-each run-tests solver-suites)
  (for-each run-tests assertion-suites)
  (for-each run-tests all-suites))
