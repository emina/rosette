#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/query  
           rosette/base/core/term  
           (only-in rosette/base/core/safe assert) 
           racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox racket/runtime-path
          "../util/lifted.rkt")


@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "pairs-log")))

@(define pairs:constructors+selectors (select '(pair? null? cons car cdr null list? list list* build-list)))
@(define list-operations (select '(length list-ref list-tail append reverse)))
@(define list-iteration (select '(map andmap ormap for-each foldl foldr)))
@(define list-filtering (select '(filter remove remq remv remove* remq* remv* sort)))
@(define list-searching (select '(member memv memq memf findf assoc assv assq assf)))
@(define more-pair-ops (select '(caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)))
@(define more-list-ops (select '(empty cons? empty? first rest second third fourth fifth sixth seventh eighth ninth tenth last last-pair make-list take drop split-at takef dropf splitf-at take-right drop-right split-at-right takef-right dropf-right splitf-at-right add-between append* flatten remove-duplicates filter-map count partition range append-map filter-not shuffle permutations in-permutations argmin argmax )))

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
(code:line (define xs (take (list x y z) n))        (code:comment "(1) xs is a symbolic list"))
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
(code:line (define p (if b (cons 1 2) (cons 4 #f))) (code:comment "(2) p is a symbolic pair"))
(define sol (solve (assert (boolean? (cdr p)))))
(evaluate p sol)
(define sol (solve (assert (odd? (car p)))))
(evaluate p sol)
]

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
