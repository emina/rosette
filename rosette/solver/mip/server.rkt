#lang racket

(require (only-in rosette sat unsat))
(provide server server-run)

(struct server (path opts))

(define-syntax-rule (server-run s encode decode)
  (let* ([temp (format "temp-~a.mip" (current-seconds))]
         [out-port (open-output-file temp #:exists 'truncate)])
    ;; Encode and print to file.
    (fprintf (current-error-port) (format "Generate ILP program at ~a\n" temp))
    (define t1 (current-seconds))
    (parameterize ([current-output-port out-port])
      (begin0 
        encode
        (flush-output (current-output-port)))
      )
    (close-output-port out-port)
    
    (define t2 (current-seconds))
    (fprintf (current-error-port) (format "Encoding time: ~a\n" (- t2 t1)))

    ;; Run CPLEX solver.
    (fprintf (current-error-port) (format "Run ~a ~a\n" (server-path s) (append (server-opts s) (list temp))))
    (define-values (p out in err) 
      (apply subprocess #f #f #f (server-path s) (append (server-opts s) (list temp))))

    ;; Print progress and decode the solution.
    (define sol (print-and-decode out decode))
    (define t3 (current-seconds))
    (fprintf (current-error-port) (format "Running & decoding time: ~a\n" (- t3 t2)))
    
    ;(fprintf (current-error-port) (format "Remove ~a\n" temp))
    ;(system (format "rm ~a" temp))
    sol
  ))

;; Relay output from CPLEX to stdout and decode the solution.
(define-syntax-rule (print-and-decode out decode)
  (if (print-until-solution out)
      (let ([t1 (current-seconds)]
            [sol
             (parameterize ([current-input-port out])
               decode)]
            [t2 (current-seconds)])
        (fprintf (current-error-port) (format "Decoding time: ~a\n" (- t2 t1)))
        sol)
      (unsat)
      ))

;; Relay output from CPLEX at 'out' port to stdout until eof or a solution is found.
;; Return #t if a solution is found, otherwise return #f.
(define (print-until-solution out)
  (define line (read-line out))
  (pretty-display line)

  (if (eof-object? line)
      #f
      (if (regexp-match #rx"CPLEX> Incumbent solution" line)
          #t
          (print-until-solution out))))
    