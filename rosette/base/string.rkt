#lang racket

(require "term.rkt" "op.rkt" "union.rkt" "bool.rkt" "num.rkt" "any.rkt" "generic.rkt" "merge.rkt")

(provide @string? @string-length @string-append @substring @string-set! @string-copy! @string-fill!)
         
(define (string/cast v)
  (match v
    [(? string?) (values #t v)]
    [(term _ (== @string?)) (values #t v)]
    [(union : [g (and (app type-of (== @string?)) u)] _ ...) (values g u)]
    [_ (values #f v)]))

(define (string/equal? x y)
  (match* (x y)
    [((? string?) (? string?)) (equal? x y)]
    [(_ _) (=? x y)]))

; The value of the force? parameter is ignored since 
; we treat all strings as immutable and therefore always
; compressible (forced) into a term.
(define (string/compress force? ps)
  (match ps
    [(list _) ps]
    [(list (cons g a) (cons (expression (== !) g) b)) (list (cons #t (ite g a b)))]
    [(list (cons (expression (== !) g) b) (cons g a)) (list (cons #t (ite g a b)))]
    [(list (cons g a) ...)
     (list (cons (apply || g)
                 (apply @string-append (for/list ([guard g][str a]) (ite guard str "")))))]))
     
    
(define-primitive-type @string? 
  #:pred     (instance-of? string? @string?) 
  #:least-common-supertype (lambda (t) (if (eq? t @string?) @string? @any?))
  #:eq?      string/equal?
  #:equal?   string/equal?
  #:cast     string/cast
  #:compress string/compress)

(define nary-type (op/-> (#:rest @string?) @string?))

(define (string-append-simplify xs)
  (match xs
    [(list) xs]
    [(list _) xs]
    [(list-rest (? string? x) ..2 rest)
     (list* (apply string-append x) (string-append-simplify rest))]
    [(list x rest ...) (list* x (string-append-simplify rest))]))
     
  
(define-op @string-append
  #:name 'string-append
  #:type nary-type 
  #:op 
  (case-lambda [() ""]
               [(x) x]
               [(x y) (match* (x y)
                        [((? string?) (? string?)) (string-append x y)]
                        [(_ _) (expression @string-append x y)])]
               [xs (match (string-append-simplify xs)
                     [(list x) x]
                     [ys (apply expression @string-append ys)])]))

(define-op @string-length
  #:name 'string-length
  #:type (op/-> (@string?) @number?)
  #:op
  (match-lambda [(? string? x) (string-length x)]
                [x (expression @string-length x)]))

(define-op @substring
  #:name 'substring
  #:type (op/-> (@string? @number? #:rest @number?) @string?)
  #:pre  (case-lambda [(s i) (&& (@>= i 0) (@<= i (@string-length s)))]
                      [(s i j) (&& (@>= i 0) (@<= i j) (@<= j (@string-length s)))])
  #:op
  (lambda (s i [j (@string-length s)])
    (if (and (string? s) (number? i) (number? j)) 
        (substring s i j)
        (expression @substring s i j))))
      
; We are going to disable all mutation operations on strings.

(define disable-mutation (lambda xs (error 'string-set! "string mutation not supported")))

(define @string-set! (impersonate-procedure string-set! disable-mutation))    
(define @string-fill! (impersonate-procedure string-fill! disable-mutation))
(define @string-copy! (impersonate-procedure string-copy! disable-mutation))
   

                     
         
