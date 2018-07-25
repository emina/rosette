#lang racket

(require (only-in rosette sat unsat) racket/file)
(provide server server-run server-path)

(struct server (path opts))

(define-syntax-rule (server-run s timeout encode decode mst-read mst-write verbose)
  (let* ([tempdir (make-temporary-file "cplex~a" 'directory)]
         [temp (build-path tempdir "cplex.mip")]
         [temp-log (build-path tempdir "cplex.out")]
         [out-port (open-output-file temp #:exists 'truncate)])
    ;; Encode and print to file.
    (when verbose
      (fprintf (current-error-port) (format "Generate ILP program at ~a\n" temp)))

    (with-handlers ([exn:fail?
                     (λ (e)
                       (close-output-port out-port)
                       (unless verbose
                         (delete-directory/files tempdir))
                       (raise e))])
      (parameterize ([current-output-port out-port])
        (when timeout (pretty-display (format "set dettimelimit ~a" timeout)))
        (begin0 
          encode
          (flush-output (current-output-port)))
        (when mst-read (pretty-display (format "read ~a" mst-read)))
        (pretty-display "optimize")
        (pretty-display "display solution variables -")
        (pretty-display (format "write ~a all" mst-write))
        ))
    (close-output-port out-port)

    ;; Run CPLEX solver.
    (when verbose
      (fprintf (current-error-port) (format "Run ~a ~a\n" (server-path s) (append (server-opts s) (list temp)))))
    (define-values (p out in err)
      (parameterize ([current-directory tempdir])
        (apply subprocess #f #f #f (server-path s) (append (server-opts s) (list temp)))))

    ;; Print progress and decode the solution.
    (define log-port (open-output-file temp-log #:exists 'truncate))
    (define sol (print-and-decode out decode log-port verbose))
    (close-output-port log-port)
    
    (unless verbose
      (delete-directory/files tempdir))

    sol
  ))

;; Relay output from CPLEX to stdout and decode the solution.
(define-syntax-rule (print-and-decode out decode log-port verbose)
  (if (print-until-solution out log-port verbose)
      (let ([t1 (current-seconds)]
            [sol
             (parameterize ([current-input-port out])
               decode)]
            [t2 (current-seconds)])
        ;(fprintf (current-error-port) (format "Decoding time: ~a\n" (- t2 t1)))
        sol)
      (unsat)
      )
  )
  

;; Relay output from CPLEX at 'out' port to stdout until eof or a solution is found.
;; Return #t if a solution is found, otherwise return #f.
(define (print-until-solution out log-port verbose)
  (define line (read-line out))
  (when verbose (pretty-display line))
  (pretty-display line log-port)
  (flush-output log-port)

  (if (eof-object? line)
      #f
      (if (regexp-match #rx"Solution Value" line)
          #t
          (print-until-solution out log-port verbose))))
    
