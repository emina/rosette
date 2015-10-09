#lang racket

(require "location.rkt" "instruction.rkt" rosette/query/eval rosette/base/util/log)

(provide fragment fragment->syntax)

; Given a library of instructions and a solution, returns 
; a sorted fragment of all instructions on which the final (output)
; instruction is data dependent according to the given solution.
; The output takes the form of a list in which the ith element is 
; the instruction with output location i iff the output depends on 
; the value produced that instruction. Otherwise, the ith element is #f.
(define (fragment #:library lib #:solution sol)
  (let* ([locs (sort (for/list ([inst lib])
                       (list* inst 
                              (sol (instruction-output inst)) 
                              (map sol (instruction-inputs inst))))
                     location>? #:key cadr)]
         [reached (for/fold ([reached (apply set (cdar locs))]) ([ls (cdr locs)])
                    (if (set-member? reached (cadr ls))
                        (set-union reached (apply set (cddr ls)))
                        reached))])
    (reverse (for/list ([ls locs]) (and (set-member? reached (cadr ls)) (car ls))))))

; Given a solution, a library of instructions, and list of syntactic identifiers 
; to use as names for inputs to the code fragment, produces a syntactic representation 
; of a lambda-expression for the given library with respect to the given solution. 
; The solution must have a concrete binding for input and output locations of every 
; instruction in the given library.
(define (fragment->syntax #:inputs inputs #:library lib #:solution sol) 
  (let* ([fragment (filter identity (fragment #:library lib #:solution sol))]
         [reg-ids (generate-temporaries (make-list (length fragment) 't))]
         [loc-table (make-hash)]
         [loc->id (begin (for ([(in idx) (in-indexed inputs)])
                           (hash-set! loc-table (location idx) in))
                         (for ([reg reg-ids] [inst fragment])
                           (hash-set! loc-table (sol (instruction-output inst)) reg))
                         (compose1 (curry hash-ref loc-table) sol))])
    #`(lambda #,inputs
        (let* #,(for/list ([inst fragment])
                  (let ([id (loc->id (instruction-output inst))]) 
                    (if (null? (instruction-inputs inst)) 
                        #`[#,id #,(evaluate ((instruction-proc inst)) sol)]  ; constant
                        #`[#,id (#,(instruction-id inst) #,@(map loc->id (instruction-inputs inst)))])))
          #,(last reg-ids)))))
    
    
                                                                        
  
    