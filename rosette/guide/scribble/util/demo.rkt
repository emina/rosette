#lang rosette

(provide demo define-demo print-forms-alt)

(define-syntax-rule (demo form)
  (begin
    (printf "> ~a\n" 'form)
    (with-handlers ([exn:fail? (lambda (e)
                                 (eprintf "~a\n" (exn-message e))
                                 (clear-vc!)
                                 (solver-shutdown (current-solver)))])
      (let ([v form])
        (unless (void? v)
          (printf "~a\n" v))))))

(define-syntax-rule (define-demo id form ...)
  (define (id)
    (printf "\n--------~a--------\n" 'id)
    form ...))

(require (only-in rosette/lib/synthax generate-forms))

(define (print-forms-alt sol)
  (define stxs (generate-forms sol))
  (pretty-write (syntax->datum (car stxs)))
  (for ([stx (cdr stxs)])
    (newline)
    (pretty-write (syntax->datum stx))))

