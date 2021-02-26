#lang racket

(provide select rosette-evaluator rosette-log-evaluator logfile opaque format-opaque)

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

(define (format-opaque format-str . args)
  (opaque (apply format format-str args)))

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


(define (show v [port (current-output-port)])
  (unless (void? v)
    (define s (format "~v" v))
    (cond
      [(<= (string-length s) (pretty-print-columns))
       (fprintf port "~a\n" s)]
      [else 
       (define out (open-output-string))
       (parameterize ([read-square-bracket-with-tag #t]
                      [read-curly-brace-with-tag #t]
                      [current-readtable tuple-readtable-open])
         (pretty-write (showable (read (open-input-string s))) out))
       (let* ([str (get-output-string out)]
              [str (regexp-replace* #px"\\(⟦\\s*" str "[")]
              [str (regexp-replace* #px"\\s*⟧\\)" str "]")]
              [str (regexp-replace* #px"\\(⦃\\s*" str "{")]
              [str (regexp-replace* #px"\\s*⦄\\)" str "}")])
         (fprintf port "~a\n" str))])))

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

; Adapted from https://docs.racket-lang.org/reference/readtables.html

(define (parse-nonempty port read-one src)
    (let ([ch (peek-char port)])
      (if (eof-object? ch)
          null
          (case ch
            [(#\>) (read-char port)
                   null]
            [else
             (cons (read-one)
                   (parse-nonempty port read-one src))]))))

(define (parse port read-one src)
  (if (eq? #\> (peek-char port))
      null
      (cons (read-one)
            (parse-nonempty port read-one src))))

(define (wrap l)
  (define out (open-output-string))
  (fprintf out "#<")
  (for ([v l])
    (match v
      [(list 'unquote w)
       (fprintf out ", ~a" w)]
      [_  (fprintf out "~a" v)]))
  (fprintf out ">")
  (opaque (get-output-string out)))


(define parse-open-tuple
    (case-lambda
     [(ch port)
      (parameterize ([current-readtable (tuple-readtable-close)])
        (wrap (parse port
                     (lambda ()
                       (read/recursive port #f ))
                     (object-name port))))]
     [(ch port src line col pos)
      (datum->syntax
       #f
       (parameterize ([current-readtable (tuple-readtable-close)])
         (wrap (parse port
                      (lambda ()
                        (read-syntax/recursive src port #f))
                      src)))
       (let-values ([(l c p) (port-next-location port)])
         (list src line col pos (and pos (- p pos)))))]))

(define tuple-readtable-open
  (make-readtable #f #\< 'dispatch-macro parse-open-tuple))

(define (tuple-readtable-close)
  (make-readtable (current-readtable) #\> 'terminating-macro (lambda _ null)))
