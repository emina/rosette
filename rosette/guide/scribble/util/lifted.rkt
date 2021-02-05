#lang racket

(provide select rosette-evaluator rosette-log-evaluator logfile opaque)

(require 
  (for-label racket racket/generic)
  (only-in rosette rosette bv)
  racket/sandbox racket/serialize scribble/eval
  (only-in scribble/manual elem racket))

(define lifted? 
  (let ([lifted (apply set (rosette))])
    (lambda (id) (set-member? lifted id))))

(define (select racket-ids)
   (apply elem 
          (add-between (map (lambda (id) (racket #,#`#,id)) 
                            (filter lifted? racket-ids)) ", ")))

(define (showable v)
  (match v
    [(list '#%brackets rest ...)
     `(⟦ ,@(map showable rest) ⟧)]
    [(list '#%braces rest ...)
     `(⦃ ,@(map showable rest) ⦄)]
    [(list 'bv (? integer? x) (? integer? len))
     (bv x len)]
    [(? list?)
     (map showable v)]
    [_ v]))

(define (show v [port (current-output-port)])
  (unless (void? v)
    (define s (format "~v" v))
    (cond
      [(<= (string-length s) (pretty-print-columns))
       (fprintf port "~a" s)]
      [else 
       (define out (open-output-string))
       (parameterize ([read-square-bracket-with-tag #t]
                      [read-curly-brace-with-tag #t])
         (pretty-write (showable (read (open-input-string s))) out))
       (let* ([str (get-output-string out)]
              [str (regexp-replace* #px"\\(⟦\\s*" str "[")]
              [str (regexp-replace* #px"\\s*⟧\\)" str "]")]
              [str (regexp-replace* #px"\\(⦃\\s*" str "{")]
              [str (regexp-replace* #px"\\s*⦄\\)" str "}")])
         (fprintf port "~a" str))])))
    
(define (rosette-evaluator [eval-limits #f] [lang 'rosette/safe])
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-path-permissions `((execute ,(byte-regexp #".*")))]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits eval-limits]
                  [current-print show])
     (make-evaluator lang)))

(define (logfile root [filename "log"])
  (build-path root (format "~a.txt" filename)))

(serializable-struct opaque (str)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (opaque-str self)))])

(define (serialize-for-logging v)
  (match v
    [(or (? boolean?) (? number?) (? string?) (? symbol?) (? void?)) v]
    [_
     (define out (open-output-string))
     (show v out)
     (opaque (get-output-string out))]))

(define (serializing-evaluator evaluator)
  (lambda (expr)
    (define v (evaluator expr))
    (printf "~a" (get-output evaluator))
    (eprintf "~a" (get-error-output evaluator))
    (serialize-for-logging v)))


(define (rosette-log-evaluator logfile [eval-limits #f] [lang 'rosette/safe])  
  (if (file-exists? logfile)
      (make-log-based-eval logfile 'replay)
      (parameterize ([current-eval (serializing-evaluator (rosette-evaluator eval-limits lang))])
        (make-log-based-eval logfile 'record))))

