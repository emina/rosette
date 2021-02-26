#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/query  
           rosette/base/core/term  
           (only-in rosette/base/base assert vc clear-vc! define-symbolic
                    length-bv list-ref-bv list-set-bv
                    take-bv take-right-bv
                    drop-bv drop-right-bv
                    list-tail-bv split-at-bv split-at-right-bv
                    union? bitvector bitvector? bv?
                    bitvector->natural integer->bitvector) 
           racket)
          scribble/core scribble/html-properties scribble/examples racket/sandbox racket/runtime-path
          "../util/lifted.rkt")


@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "pairs-log")))

@(define pairs:constructors+selectors (select '(pair? null? cons car cdr null list? list list* build-list)))
@(define list-operations (select '(length list-ref list-tail append reverse)))
@(define list-iteration (select '(map andmap ormap for-each foldl foldr)))
@(define list-filtering (select '(filter remove remq remv remove* remq* remv* sort)))
@(define list-searching (select '(member memv memq memf findf assoc assv assq assf)))
@(define more-pair-ops (select '(caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)))
@(define more-list-ops (select '(empty cons? empty? first rest second third fourth fifth sixth seventh eighth ninth tenth last last-pair make-list take drop split-at takef dropf splitf-at take-right drop-right split-at-right takef-right dropf-right splitf-at-right add-between append* flatten remove-duplicates filter-map count partition range append-map filter-not shuffle permutations in-permutations argmin argmax list-set)))

@title[#:tag "sec:pair"]{Pairs and Lists}

A pair combines two values, and a list is either the 
constant @racket[null] or a pair whose second 
element is a list.  Pairs and lists are transparent immutable values, and they may 
be concrete or symbolic.  
Two pairs or two lists are @racket[eq?] (resp. @racket[equal?])
if their corresponding elements are @racket[eq?] (resp. @racket[equal?]).

As values of @tech[#:key "unsolvable type"]{unsolvable types}, symbolic pairs 
and lists cannot be created 
via @seclink["sec:symbolic-constants"]{@code{define-symbolic[*]}}. 
Instead, they are created by applying pair- or list-producing procedures to symbolic inputs, 
or by controlling the application of such procedures with symbolic values.  This   
pattern for creating non-primitive symbolic values generalizes to all unsolvable datatypes.

@examples[#:eval rosette-eval
(define-symbolic x y z n integer?)
(code:line (define xs (take (list x y z) n))        (code:comment "(1) xs is a symbolic list."))
(define sol (solve (assert (null? xs))))
(evaluate xs sol)
(define sol 
  (solve (begin
           (assert (= (length xs) 2))
           (assert (not (equal? xs (reverse xs))))
           (assert (equal? xs (sort xs <))))))
(evaluate xs sol)]

@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(code:line (define p (if b (cons 1 2) (cons 4 #f))) (code:comment "(2) p is a symbolic pair."))
(define sol (solve (assert (boolean? (cdr p)))))
(evaluate p sol)
(define sol (solve (assert (odd? (car p)))))
(evaluate p sol)
]

@section{Lifted Operations on Pairs and Lists}

Rosette lifts the following operations on pairs and lists:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Pair Operations} @pairs:constructors+selectors)
      (list @elem{List Operations} @list-operations)
      (list @elem{List Iteration} @list-iteration)
      (list @elem{List Filtering} @list-filtering)
      (list @elem{List Searching} @list-searching)
      (list @elem{Additional Pair Operations} @more-pair-ops)
      (list @elem{Additional List Operations} @more-list-ops))]

@(kill-evaluator rosette-eval)
@(set! rosette-eval (rosette-evaluator))

@section{Additional Operations on Pairs and Lists}

Rosette provides the following procedures for operating on lists using @seclink["sec:bitvectors"]{bitvector} indices and lengths. These procedures produce symbolic values that avoid @racketlink[bitvector->natural]{casting} their bitvector arguments to integers, leading to @seclink["sec:notes"]{more efficiently solvable queries}. 

@declare-exporting[rosette/base/base #:use-sources (rosette/base/base)]

@defproc[(length-bv [lst list?] [t (or/c bitvector? union?)]) bv?]{
Equivalent to @racket[(integer->bitvector (length lst) t)] but avoids the @racket[integer->bitvector] cast for better solving performance.

@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define xs (if b '(1 2) '(3 4 5 6)))
xs
(integer->bitvector (length xs) (bitvector 4))
(length-bv xs (bitvector 4))]
}

@defproc[(list-ref-bv [lst list?] [pos bv?]) any/c]{
Equivalent to @racket[(list-ref lst (bitvector->natural pos))] but avoids the @racket[bitvector->natural] cast for better solving performance.

@examples[#:eval rosette-eval
(define-symbolic p (bitvector 1))
(define xs '(1 2 3 4))
(code:comment "Uses a cast and generates a redundant assertion on the range of p:")
(list-ref xs (bitvector->natural p))
(vc)
(clear-vc!)
(code:comment "No cast and no redundant range assertion:")
(list-ref-bv xs p)
(vc)
(code:comment "But the range assertion is generated when needed:")
(define-symbolic q (bitvector 4))
(list-ref-bv xs q)
(vc)]
              
}

@(rosette-eval '(clear-vc!))
@defproc[(list-set-bv [lst list?] [pos bv?] [val any/c]) list?]{
Equivalent to @racket[(list-set lst (bitvector->natural pos) val)] but avoids the @racket[bitvector->natural] cast for better solving performance.

@examples[#:eval rosette-eval
(define-symbolic p (bitvector 1))
(define xs '(1 2 3 4))
(code:comment "Uses a cast and generates a redundant assertion on the range of p:")
(list-set xs (bitvector->natural p) 5)
(vc)
(clear-vc!)
(code:comment "No cast and no redundant range assertion:")
(list-set-bv xs p 5)
(vc)
(code:comment "But the range assertion is generated when needed:")
(define-symbolic q (bitvector 4))
(list-set-bv xs q 5)
(vc)]              
}

@(rosette-eval '(clear-vc!))
@defproc*[([(take-bv [lst any/c] [pos bv?]) list?]
           [(take-right-bv [lst any/c] [pos bv?]) any/c]
           [(drop-bv [lst any/c] [pos bv?]) any/c]
           [(drop-right-bv [lst any/c] [pos bv?]) list?]
           [(list-tail-bv [lst any/c] [pos bv?]) any/c])]{
                                                          
 Equivalent to @racket[take], @racket[take-right],
 @racket[drop], @racket[drop-right], or @racket[list-tail]
 applied to @racket[lst] and
 @racket[(bitvector->natural pos)], but avoids the
 @racket[bitvector->natural] cast for better solving
 performance.

 @examples[#:eval rosette-eval
(define-symbolic p (bitvector 1))
(define xs (cons 1 (cons 2 (cons 3 4))))
(code:comment "Uses a cast and generates a redundant assertion on the range of p:")
(take xs (bitvector->natural p))
(vc)
(clear-vc!)
(code:comment "No cast and no redundant range assertion:")
(take-bv xs p)
(vc)
(code:comment "But the range assertion is generated when needed:")
(define-symbolic q (bitvector 4))
(take-bv xs q)
(vc)]              
}

@(rosette-eval '(clear-vc!))
@defproc*[([(split-at-bv [lst any/c] [pos bv?]) (list? any/c)]
           [(split-at-right-bv [lst any/c] [pos bv?]) (list? any/c)])]{

 Equivalent to
 @racket[(split-at lst (bitvector->natural pos))] or
 @racket[(split-at-right lst (bitvector->natural pos))], but
 avoids the @racket[bitvector->natural] cast for better
 solving performance.

 @examples[#:eval rosette-eval
(define-symbolic p (bitvector 1))
(define xs (cons 1 2))
(code:comment "Uses a cast and generates a redundant assertion on the range of p:")
(split-at xs (bitvector->natural p))
(vc)
(clear-vc!)
(code:comment "No cast and no redundant range assertion:")
(split-at-bv xs p)
(vc)
(code:comment "But the range assertion is generated when needed:")
(define-symbolic q (bitvector 4))
(split-at-bv xs q)
(vc)]}

@(kill-evaluator rosette-eval)
