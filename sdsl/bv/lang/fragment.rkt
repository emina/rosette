#lang racket

(require "program.rkt" rosette/query/eval)

(provide fragment fragment->syntax)

; Given a BV program and a solution, returns 
; a sorted fragment of all instructions on which the final (output)
; instruction is data dependent according to the given solution.
; The output takes the form of a list in which the ith element is 
; the instruction with output location i iff the output depends on 
; the value produced that instruction. Otherwise, the ith element is #f.
(define (fragment prog sol)
  (let* ([lib (prog-insts prog)]
         [locs (sort (for/list ([inst lib])
                       (list* inst 
                              (sol (inst-out inst)) 
                              (map sol (inst-in inst))))
                     > #:key cadr)]
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
(define (fragment->syntax inputs prog sol) 
  (let* ([fragment (filter identity (fragment prog sol))]
         [reg-ids (generate-temporaries (make-list (length fragment) 't))]
         [loc-table (make-hash)]
         [loc->id (begin (for ([(in idx) (in-indexed inputs)])
                           (hash-set! loc-table idx in))
                         (for ([reg reg-ids] [inst fragment])
                           (hash-set! loc-table (sol (inst-out inst)) reg))
                         (compose1 (curry hash-ref loc-table) sol))])
    #`(lambda #,inputs
        (let* #,(for/list ([inst fragment])
                  (let ([id (loc->id (inst-out inst))]) 
                    (if (null? (inst-in inst)) 
                        #`[#,id #,(evaluate ((inst-op inst)) sol)]  ; constant
                        #`[#,id (#,(op-id (inst-op inst)) #,@(map loc->id (inst-in inst)))])))
          #,(last reg-ids)))))
    
    
                                                                        
  
    