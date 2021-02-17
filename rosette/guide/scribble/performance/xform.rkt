#lang rosette


(define-values (Add Sub Sqr Nop)
  (values (bv 0 2) (bv 1 2) (bv 2 2) (bv 3 2)))

(define (calculate prog [acc (bv 0 4)])
  (cond                      ; An interpreter for 
    [(null? prog) acc]       ; calculator programs.
    [else                    ; A program is list of 
     (define ins (car prog)) ; '(op) or '(op arg)
     (define op (car ins))   ; instructions that up-
     (calculate              ; date acc, where op is 
      (cdr prog)             ; a 2-bit opcode and arg
      (cond                  ; is a 4-bit constant.
        [(eq? op Add) (bvadd acc (cadr ins))]
        [(eq? op Sub) (bvsub acc (cadr ins))]
        [(eq? op Sqr) (bvmul acc acc)]
        [else         acc]))]))

(define (list-set lst idx val) ; Functionally sets
  (match lst                   ; lst[idx] to val.
    [(cons x xs)
     (if (= idx 0) 
         (cons val xs)
         (cons x (list-set xs (- idx 1) val)))]
    [_ lst]))

(define (list-set* lst idx val)
  (match lst
    [(cons x xs)
     (cons (if (= idx 0) val x)
           (list-set* xs (- idx 1) val))]
    [_ lst]))

(define (sub->add prog idx list-set)        
  (define ins (list-ref prog idx))  
  (if (eq? (car ins) Sub)          
      (list-set prog idx (list Add (bvneg (cadr ins))))
      prog))

(define (verify-xform xform N list-set)  
  (define P                     
    (for/list ([i N])          
      (define-symbolic* op (bitvector 2))   
      (define-symbolic* arg (bitvector 4))
      (if (eq? op Sqr) (list op) (list op arg))))
  (define-symbolic* acc (bitvector 4))
  (define-symbolic* idx integer?)
  (define xP (xform P idx list-set))
  (verify  
   (assert (eq? (calculate P acc) (calculate xP acc)))))

(module+ slow
  (clear-terms!)
  (clear-vc!)
  (time (verify-xform sub->add 20 list-set)))

(module+ fast
  (clear-terms!)
  (clear-vc!)
  (time (verify-xform sub->add 20 list-set*)))