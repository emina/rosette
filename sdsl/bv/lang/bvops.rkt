#lang s-exp rosette

(require "library.rkt")

(provide bvadd bvsub bvneg bvmul bvdiv bvrem
         bvnot bvand bvor bvxor 
         bvredand bvredor 
         bvshl bvshr bvushr 
         bveq bvle bvlt bvge bvgt 
         bvule bvult bvuge bvugt
         bvlib verify32 finitize)

(define-syntax-rule (bool->num b) (if b 1 0))

(define-syntax-rule (bvop op)     
  (lambda (x y) (finitize (op (finitize x) (finitize y)))))

(define-syntax-rule (bvcmp op) 
  (lambda (x y) (bool->num (op (finitize x) (finitize y)))))

(define-syntax-rule (bvucmp op)   
  (lambda (x y) 
    (let ([x (finitize x)]
          [y (finitize y)])
      (bool->num (if (equal? (< x 0) (< y 0)) (op x y) (op y x))))))

(define-syntax-rule (bvshift op)
  (lambda (x y)
    (let ([x (finitize x)]
          [y (if (or (term? y) (and (<= 0 y) (<= y (current-bitwidth)))) y (current-bitwidth))])
      (finitize (op x y)))))

(define bvadd (bvop +))
(define bvsub (bvop -))
(define bvmul (bvop *))
(define bvdiv (bvop quotient)) 
(define bvrem (bvop remainder)) 
(define bvneg (lambda (x) (bvsub 0 x)))

(define bvnot (lambda (x) (bitwise-not (finitize x))))
(define bvand (bvop bitwise-and))
(define bvor  (bvop bitwise-ior))
(define bvxor (bvop bitwise-xor))
(define bvredor (lambda (x) (bvnot (bveq (finitize x) 0))))
(define bvredand (lambda (x) (bveq (finitize x) -1)))

(define bvshl  (bvshift <<))
(define bvshr  (bvshift >>))
(define bvushr (bvshift >>>))

(define bveq  (bvcmp =))
(define bvle  (bvcmp <=))
(define bvlt  (bvcmp <))
(define bvge  (bvcmp >=))
(define bvgt  (bvcmp >))
(define bvule (bvucmp <=))
(define bvult (bvucmp <))
(define bvuge (bvucmp >=))
(define bvugt (bvucmp >))

(define-syntax (bvlib stx)
  (syntax-case stx ()
    [(_ [#:all k]) #`(bvlib [{bvadd bvsub bvneg bvmul bvdiv bvrem
                              bvnot bvand bvor bvxor bvshl bvshr bvushr bvredor bvredand
                              bveq bvle bvlt bvge bvgt bvule bvult bvuge bvugt} k])] 
    [(_ [{bvop ...} k] ...) 
     (let ([expanded (for/list ([spec (syntax->list #'([{bvop ...} k] ...))])
                       (with-syntax ([({op ...} n) spec])
                         #'([op n] ...)))])
       #`(library #,@(append-map syntax->list expanded)))]))


(define (verify32 impl ref-impl)
  (parameterize ([current-bitwidth 32])
   (define x (for/list ([i (in-range (procedure-arity impl))])
               (define-symbolic* in number?)
               in))
   (verify (assert (= (apply impl x) (apply ref-impl x))))))
 

(define (finitize num) 
  (match (coerce num number?)
    [(? term? v) v]
    [v (let* ([bitwidth (current-bitwidth)]
              [mask (arithmetic-shift -1 bitwidth)]
              [masked (bitwise-and (bitwise-not mask) v)])
         (if (bitwise-bit-set? masked (- bitwidth 1))
             (bitwise-ior mask masked)  
             masked))]))
