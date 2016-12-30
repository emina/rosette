#lang racket

(provide server server-run)

(struct server (path opts))

(define-syntax-rule (server-run s expr)
  (let* ([temp (format "temp-~a.mip" (current-seconds))]
         [out-port (open-output-file temp #:exists 'truncate)])
    (fprintf (current-error-port) (format "Generate ILP program at ~a\n" temp))
    (parameterize ([current-output-port out-port])
      (begin0 
        expr
        (flush-output (current-output-port)))
      )
    (close-output-port out-port)
    (fprintf (current-error-port) (format "Run ~a ~a\n" (server-path s) (append (server-opts s) (list temp))))
    (let-values ([(p out in err) 
                  (apply subprocess #f #f #f (server-path s) (append (server-opts s) (list temp)))])
      (wait-and-print p out)
      )
    
    (fprintf (current-error-port) (format "Remove ~a\n" temp))
    (system (format "rm ~a" temp))
  ))

(define (wait-and-print p out)
  (print-all out))

(define (print-all out)
  (define line (read-line out))
  (unless (eof-object? line)
    (pretty-display line)
    (print-all out)))
    
    