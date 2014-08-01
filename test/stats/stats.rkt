#lang racket

(require racket/date)

(provide run/time run/time/log)

(define-syntax-rule (run/time/log (runner arg ...) file)
  (call-with-output-file 
      file
    (lambda (out)
        (run/time #'runner runner (list arg ...) out))
    #:mode   'text
    #:exists 'append))

(define (run/time runner-id runner args [port (current-output-port)])
  (let-values ([(val cpu real gc) (time-apply runner args)])
    (print-runtime port runner-id cpu real gc)))

(define (print-runtime port runner-id cpu real gc)
  (define now (current-date))
  (fprintf port
           "[~a.~a.~a ~a:~a:~a] [~a] [cpu=~a, real=~a, gc=~a]\n" 
           (date-year now) 
           (~r (date-month now) #:min-width 2 #:pad-string "0") 
           (~r (date-day now) #:min-width 2 #:pad-string "0") 
           (~r (date-hour now) #:min-width 2 #:pad-string "0") 
           (~r (date-minute now) #:min-width 2 #:pad-string "0") 
           (~r (date-second now) #:min-width 2 #:pad-string "0") 
           (format-id runner-id) 
           cpu real gc))

(define (format-id id)
  (if (identifier? id)
      (format "~a ~a" 
              (match (syntax-source id)
                [(and (? path? p) 
                      (app path->string (pregexp #px".*(rosette/.*)" (list _ rest))))
                 rest]
                [(? path? p) (path->string p)]
                [p p])
              (syntax->datum id))
      (format "~a" id)))