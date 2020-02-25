#lang racket

(provide select rosette-evaluator rosette-log-evaluator logfile opaque)

(require 
  (for-label racket racket/generic)
  (only-in rosette rosette union union-contents union?)
  racket/sandbox racket/serialize scribble/eval
  (only-in scribble/manual elem racket))

(define lifted? 
  (let ([lifted (apply set (rosette))])
    (lambda (id) (set-member? lifted id))))

(define (select racket-ids)
   (apply elem 
          (add-between (map (lambda (id) (racket #,#`#,id)) 
                            (filter lifted? racket-ids)) ", ")))
(define (rosette-printer v)
  (match v
    [(? void?) (void)]
    [(? custom-write?) 
     ((custom-write-accessor v) v (current-output-port) 1)]
    [(? pair?) (printf "'~a" v)]
    [(? null?) (printf "'()")]
    [(? symbol?) (printf "'~a" v)]
    [_  (printf "~a" v)]))

(define (rosette-evaluator [eval-limits #f] [lang 'rosette/safe])
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-path-permissions `((execute ,(byte-regexp #".*")))]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits eval-limits]
                  [current-print rosette-printer])
     (make-evaluator lang)))

(define (logfile root [filename "log"])
  (build-path root (format "~a.txt" filename)))

(define (serialize-for-logging v)
  (match v
    [(or (? boolean?) (? number?) (? string?) (? void?)) v]
    [(? box?) (box (serialize-for-logging (unbox v)))]
    [(? pair?) (cons (serialize-for-logging (car v)) (serialize-for-logging (cdr v)))]
    [(? list?) (map serialize-for-logging v)]
    [(? vector?) (list->vector (map serialize-for-logging (vector->list v)))]
    [(? struct?)
     (let ([output-str (open-output-string)])
       (display v output-str)
       (opaque (get-output-string output-str)))]
    [_ v]))

(serializable-struct opaque (str)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (opaque-str self)))])

(define (serializing-evaluator evaluator)
  (lambda (expr) (serialize-for-logging (evaluator expr))))

(define (rosette-log-evaluator logfile [eval-limits #f] [lang 'rosette/safe])  
  (if (file-exists? logfile)
      (make-log-based-eval logfile 'replay)
      (parameterize ([current-eval (serializing-evaluator (rosette-evaluator eval-limits lang))])
        (make-log-based-eval logfile 'record))))

