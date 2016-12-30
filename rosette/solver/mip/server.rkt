#lang racket

(require (only-in rosette sat unsat))
(provide server server-run)

(struct server (path opts))

(define-syntax-rule (server-run s encode decode)
  (let* ([temp (format "temp-~a.mip" (current-seconds))]
         [out-port (open-output-file temp #:exists 'truncate)])
    (fprintf (current-error-port) (format "Generate ILP program at ~a\n" temp))
    (parameterize ([current-output-port out-port])
      (begin0 
        encode
        (flush-output (current-output-port)))
      )
    (close-output-port out-port)
    (fprintf (current-error-port) (format "Run ~a ~a\n" (server-path s) (append (server-opts s) (list temp))))
    (define-values (p out in err) 
      (apply subprocess #f #f #f (server-path s) (append (server-opts s) (list temp))))
    (define sol (print-and-decode out decode))
    
    (fprintf (current-error-port) (format "Remove ~a\n" temp))
    (system (format "rm ~a" temp))
    sol
  ))

(define-syntax-rule (print-and-decode out expr)
  (if (print-until-solution out)
      (parameterize ([current-input-port out])
        expr)
      (unsat)
      ))

(define (print-until-solution out)
  (define line (read-line out))
  (pretty-display line)

  (if (eof-object? line)
      #f
      (if (regexp-match #rx"CPLEX> Incumbent solution" line)
          #t
          (print-until-solution out))))
    