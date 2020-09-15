#lang racket

(require rosette/lib/trace/compile
         rosette/lib/trace/tool
         (only-in rosette clear-state!)
         racket/runtime-path
         "../config.rkt")

(define ns (current-namespace))
(define ITERATIONS 10)

(define (run-trace-test fname)
  (clear-state!)
  (parameterize ([current-compile symbolic-trace-compile-handler]
                 [current-namespace (make-base-namespace)]
                 [error-print-width default-error-print-width])
    (namespace-attach-module ns 'rosette)
    (with-handlers ([exn:fail? (λ (e)
                                 ((error-display-handler)
                                  (if (exn? e)
                                      (exn-message e)
                                      (~a e))
                                  e))])
      (do-trace
       (thunk
        (dynamic-require `(file ,(path->string (build-path here-dir "stress" fname))) #f))
       #:entry-handler (λ (entry add-trace! _current-original-map)
                         (add-trace! entry))
       #:post-proc void))))

(define-runtime-path here-dir ".")

(define (run-mode tests)
  (for ([filename (in-list tests)])
    (define test-file (string->path filename))
    (match-define-values (_ cpu _ _)
      (time-apply (thunk (for ([_i (in-range ITERATIONS)])
                           (run-trace-test test-file))) '()))
    (printf "Time: ~a\n" (~r (/ cpu ITERATIONS) #:precision 2))))

(run-mode '("non-tail.rkt" "tail.rkt"))
