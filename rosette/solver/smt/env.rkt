#lang racket

(require racket/syntax 
         (only-in "smtlib2.rkt" Int Real Bool BitVec
                  declare-const declare-fun define-const define-fun assert
                  [< smt/<] [<= smt/<=]) 
         "../../base/core/term.rkt" 
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bitvector-size)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-hash env]) ref-const! ref-expr! clear! smt-type)

(define (smt-id base n) (format-symbol "~a~a" base n))

(define (smt-type t)
  (match t
    [(== @boolean?) Bool]
    [(== @integer?) Int]
    [(== @real?) Real]
    [(? bitvector? t) (BitVec (bitvector-size t))]
    [_ (error 'smt-type "expected primitive-solvable? type, given ~a" t)]))

; Clears the given environment of bindings for all Rosette
; values bound to an SMT identifier whose integer suffix is
; greater or equal to the given value.  Note that every
; Rosette value in the given dictionary has a unique integer
; suffix if they are created via the ref! macro.
(define (clear! env n)
  (define to-evict
    (for/list ([(k v) (in-hash env)]
               #:when (>= (string->number (substring (symbol->string  (if (symbol? v) v (car v))) 1)) n))
      k))
  (for ([k to-evict])
    (hash-remove! env k)))

; Retrieves the SMT identifier id for the Rosette constant v from the environment env.
; 
; If env[v] = id or env[v] = (variable id), then id is returned. Additionally, if 
; env[v] = (variable id) and v is not quantified, then env is updated to bind v to id,
; and id is declared in the SMT encoding using declare-fun.
;
; If env[v] is undefined, ref-const! creates the identifier id, which takes the 
; form (format-symbol "c~a" i), where i is the size of env just before the call.
; If v is not quantified, then v is bound to id in env, and d is declared in the SMT
; encoding using declare-fun. Otherwise, v is bound to (variable id) in env.
(define (ref-const! v env quantified)
  (match (hash-ref env v #f)
    [#f
     (let ([id (smt-id 'c (hash-count env))])
       (if (member v quantified)
           (hash-set! env v (variable id))
           (declare-fun! env v id))
       id)]
    [(? symbol? id) id]
    [(variable id)
     (unless (member v quantified)
       (declare-fun! env v id))
     id]))

(struct variable (id) #:transparent)

(define (declare-fun! defs v id)
  (let ([t (term-type v)])
    (declare-fun id (map smt-type (solvable-domain t)) (smt-type (solvable-range t)))
    (hash-set! defs v id)))

; Retrieves the SMT encoding for the Rosette expression e in the environment env.
; If env has a binding for (cons e quantified), that binding is returned. 
; Otherwise, ref-expr! evaluates (encoder e env quantified) to obtain the encoding enc.
; If enc is not an s-expression (a pair), it is returned. Otherwise, ref-expr! extends
; the SMT encoding with (define-fun id ([arg-id type] ...) enc), where arg-id's are the
; SMT identifiers for the values in the quantified list. The identifier id takes the form 
; (format-symbol "e~a" i), where i is the size of the env dictionary just before the call.
; If the quantified list is empty, env is extended with a binding from e to id, and id
; is returned. Otherwise, env is extended with a binding from e to (e arg-id ...) and
; (e arg-id ...) is returned.
(define (ref-expr! e env quantified encoder)
  (let ([k (cons e quantified)])
    (or (hash-ref env k #f) 
        (match (encoder e env quantified) 
          [(? pair? enc)
           (let ([id (smt-id 'e (hash-count env))])
             (cond [(null? quantified)
                    (hash-set! env k id)
                    (define-const id (smt-type (type-of e)) enc)
                    id]
                   [else
                    (define qids
                      (for/list ([q quantified])
                        (match (hash-ref env q)
                          [(? symbol? qid) qid]
                          [(variable qid) qid])))
                    (define-fun id (for/list ([q quantified][qid qids]) (list qid (smt-type (type-of q))))
                      (smt-type (type-of e))
                      enc)
                    (define app-id (cons id qids))
                    (hash-set! env k app-id)
                    app-id]))]
          [enc enc]))))
