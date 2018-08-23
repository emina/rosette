#lang racket

(require rackunit
         racket/runtime-path
         rosette/lib/profile/data
         rosette/lib/profile/graph
         rosette/lib/profile/tool
         rosette/lib/profile/renderer/renderer
         rosette/lib/roseunit
         (only-in rosette clear-state!))
(provide regression-test)


(define-runtime-path output-path "output")


(struct regression-renderer (path)
  #:transparent
  #:methods gen:renderer
  [(define start-renderer void)
   (define (finish-renderer self profile)
     (check-profile profile (regression-renderer-path self)))])


; Check whether the profile matches the one saved in `path`.
; If `path` doesn't exist, write the output of the new profile there instead of checking.
(define (check-profile profile path)
  (define new-graph (profile-state->graph profile))
  (define new-list
    (let loop ([graph new-graph])
      (cons (let ([proc (profile-data-procedure (profile-node-data graph))])
              (or (object-name proc) proc))
            (for/list ([c (profile-node-children graph)]) (loop c)))))
  (define outpath (build-path output-path (format "~a.out" path)))
  (if (file-exists? outpath)
      (let ([old-list (call-with-input-file outpath read)])
        (check-equal? new-list old-list))
      (let ()
        (with-output-to-file outpath (thunk (write new-list)))
        (printf "Wrote new output for `~a`: ~v\n" path new-list))))



(define-syntax-rule (regression-test test-name path code)
  (test-suite+ test-name
    (let ([renderer% (lambda (source name [options (hash)])
                       (regression-renderer path))]
          [ns (make-base-namespace)])
      (clear-state!)
      (parameterize ([current-namespace ns])
        (profile-thunk
         (lambda () code)
         #:renderer renderer%)))))
