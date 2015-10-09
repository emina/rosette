#lang racket

(require "../../base/util/ord-dict.rkt" 
         "../../base/core/term.rkt"  "../../base/core/bool.rkt" "../../base/core/num.rkt"
         "../../base/struct/enum.rkt")

(provide universe universe-size domain-of 
         domain? domain-min domain-max
         domain-indices domain-values domain-value domain-atom)

; Declares and returns a Kodkod universe for @boolean?, 
; @number?, and all enum? types in kernel/enum.
(define (universe)
  (define univ (ord-dict))
  (dict-set! univ @boolean? (bool-domain))
  (dict-set! univ @number? (int-domain))
  (for ([t enums])
    (dict-set! univ t (scalar-domain t)))
  univ)
 
; Returns the domain for the given type in the given universe, 
; or throws an error if the universe has no domain for the type.
(define (domain-of univ type)
  (dict-ref univ type))

; Returns the size of the given universe.
(define (universe-size univ)
  (add1 (apply max (map domain-max (dict-values univ)))))

;------------------------------------------------------------------------------
; Note that we use a representation in which domains completely overlap---they 
; each include atoms [0..k), where k is the size of the domain.  As a result, the 
; atom 0, for example, represents both the constant #t and the integer 1.
; This is sound as long as overlapping domains correspond to disjoint types.
;------------------------------------------------------------------------------

; Returns a new domain for the boolean type that consists of the value #t.
; The value #f is represented by the empty relation.
(define (bool-domain)
  (domain 
   @boolean? 1 #f
   (case-lambda 
     [() (in-value #t)]
     [(rel) 
      (match rel
        [(list (list 0)) #t]
        [(list) #f]
        [_ (error 'interpret "expected a boolean relation, given ~a" rel)])])))

; Returns a new domain for the number type that consists of the bit values 
; 2^0, ..., 2^(bw-2), - 2^(bw-1), where bw is (current-bitwidth).  All 
; other number values are represented as sets (unary relations) over these 
; bit values.
(define (int-domain)
  (let* ([bw (current-bitwidth)]
         [bits (list->vector `(,@(for/list ([i (- bw 1)]) (expt 2 i)) 
                               ,(- (expt 2 (- bw 1)))))])
    (domain 
     @number? bw #f
     (case-lambda 
       [() (in-vector bits)]
       [(rel) 
        (match rel 
          [(list (list atom) ...) (for/sum ([a atom]) (vector-ref bits a))]
          [_ (error 'interpret "expected a number relation, given ~a" rel)])]))))

; Returns a new domain for the given enum? type.  The resulting domain 
; consists of the scalar values in (enum-members type). 
(define (scalar-domain type)
  (define vals (enum-members type))
  (domain 
   type (vector-length vals)
   ordinal
   (case-lambda
     [() (in-vector vals)]
     [(rel) 
      (match rel
        [(list (list atom)) (vector-ref vals atom)]
        [_ (error 'interpret "expected a ~a relation, given ~a" type rel)])])))
         
; Represents a domain of Kodkod values (atoms) for a given Rosette type. A 
; domain's atoms are identified by their positions in the universe; the indices 
; occupied by the domain can be accessed through (domain-indices dom).  Each domain 
; is also equipped with an intepretation  procedure.  When given no arguments, the
; procedure returns a sequence of all Rosette values that are representable by a single 
; atom drawn from the domain; the ith value in the sequence is representable by the ith 
; atom (position) in the domain.  When given a Kodkod relation, the interpretation procedure 
; returns the Rosette value that is represented by the input relation.
(struct domain (type size indexer interpretation) 
  #:property prop:custom-write 
  (lambda (self port mode) 
    (fprintf port "domain-~a[~a:~a]" (domain-type self) (domain-min self) (domain-max self))))

; Returns the smallest atom index in this domain.
(define (domain-min dom) 0)

; Returns the largest atom index in this domain.
(define (domain-max dom) (sub1 (domain-size dom)))

; Returns a sequence over the indices of atoms that comprise 
; the given domain.  The indices are ordered from the smallest 
; (i.e., the offset of the domain in the universe) to the largest.
(define (domain-indices dom) (in-range (domain-min dom) (add1 (domain-max dom))))

; Returns a sequence of all Rosette values that representable by a 
; single atom drawn from the given domain.   The ith element of the 
; sequence is representable by the ith atom in the domain, which is 
; the (domain-min dom) + ith atom in the entire universe.  
(define (domain-values dom) ((domain-interpretation dom)))

; Returns the Rosette value that is represented by the given relation.
; The relation is required to take the form of a list of tuples, where each 
; tuple is a list of atom indices drawn from (domain-indices dom).  
; The relation may contain zero or more tuples (depending on the domain's type) 
; that collectively represent a single Rosette value.
(define (domain-value dom rel) ((domain-interpretation dom) rel))

; Returns the index of the atom that represents the given 
; value in the given scalar domain.  Throws an error if the value 
; is not a member of the specified domain, or if the domain is 
; not scalar.
(define (domain-atom dom val) ((domain-indexer dom) val))
