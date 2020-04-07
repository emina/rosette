#lang racket

(require racket/exn
         rackunit
         rosette/lib/trace/compile
         rosette/lib/roseunit
         rosette/lib/trace/tool
         rosette/lib/util/syntax
         (only-in rosette clear-state!)
         racket/runtime-path)

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

(define (format-output stats trace original-map)
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
      (parameterize ([error-print-context-length 0]) (exn->string ex)))
    (define activated-syntax-out (format-activated-syntax activated-syntax))
    (define activated-stack-out (map format-activated-stack-elem activated-stack))
    (list ex-out activated-syntax-out activated-stack-out pc))
  (serialize
   `((#:stats ,stats)
     (#:trace ,(map format-trace-elem (take-top trace 10))))))

(define (take-top xs n)
  (for/list ([x (in-list xs)] [_ (in-range n)]) x))


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

  (parameterize ([current-compile symbolic-trace-compile-handler])
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

(module+ test
  (require rackunit/text-ui)

  (define (run-mode mode)
    (for ([test-file (directory-list (build-path here-dir "code" mode))])
      (run-tests
       (test-suite+
        (~a (path-string->string test-file) " (" mode ")")
        (run-trace-test test-file mode)))))

  (run-mode "regular")

  (parameterize ([symbolic-trace-skip-infeasible-solver? #t])
    (run-mode "solver"))

  (parameterize ([symbolic-trace-skip-assertion? #t])
    (run-mode "assertion"))

  (parameterize ([symbolic-trace-skip-infeasible? #t])
    (run-mode "infeasible"))

  (parameterize ([symbolic-trace-skip-infeasible-solver? #t]
                 [symbolic-trace-skip-assertion? #t]
                 [symbolic-trace-skip-infeasible? #t])
    (run-mode "all")))
