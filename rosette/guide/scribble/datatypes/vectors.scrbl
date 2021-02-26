#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/query 
           rosette/base/core/term  
           (only-in rosette/base/base assert define-symbolic union?
                    vc clear-vc! bitvector bitvector? bv?
                    bitvector->natural integer->bitvector
                    vector-length-bv vector-ref-bv vector-set!-bv) 
           racket)
          scribble/core scribble/html-properties scribble/examples racket/sandbox racket/runtime-path
          "../util/lifted.rkt")


@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "vectors-log")))

@(define vector-ops (select '(vector? make-vector vector vector-immutable vector-length vector-ref vector-set! vector->list list->vector vector->immutable-vector vector-fill! vector-copy! vector->values build-vector immutable?)))
@(define more-vector-ops (select '(vector-set*! vector-map vector-map! vector-append vector-take vector-take-right vector-drop vector-drop-right vector-split-at vector-split-at-right vector-copy vector-filter vector-filter-not vector-count vector-argmin vector-argmax vector-member vector-memv vector-memq)))


@title[#:tag "sec:vec"]{Vectors}

A vector is a fixed-length (im)mutable array. 
Vectors may be concrete or symbolic, and they may be accessed using concrete 
or symbolic indices.  A concrete vector supports constant-time access for 
concrete slot indices, and linear-time access for symbolic slot indices.  
A symbolic vector supports (worst-case) linear- and quadratic-time access for concrete and 
symbolic indices, respectively. Access time for symbolic vectors is given with 
respect to the longest possible concrete array to which any symbolic vector 
could @racket[evaluate] under any @racket[solution?]. 

Like @seclink["sec:pair"]{pairs and lists}, immutable vectors are transparent immutable values:
two such vectors are @racket[eq?] if they have the same length and @racket[eq?] contents.
Mutable vectors are references rather than values, and two mutable vectors are @racket[eq?] if and only if they 
point to the same array object.  Two vectors (regardless of mutability) are @racket[equal?] 
if they have the same length and @racket[equal?] contents.

@examples[#:eval rosette-eval
(define v1 (vector 1 2 #f))
(define v2 (vector 1 2 #f))
(eq? v1 v2)
(equal? v1 v2)
(define v3 (vector-immutable 1 2 #f))
(define v4 (vector-immutable 1 2 #f))
(eq? v3 v4)
(equal? v1 v3)
]

@examples[#:eval rosette-eval
(define-symbolic x y z n integer?)
(code:line (define xs (take (list x y z) n))        (code:comment "xs is a symbolic list."))
(code:line (define vs (list->vector xs))            (code:comment "vs is a symbolic vector."))
(define sol
  (solve
   (begin
     (assert (< n 3))
     (assert (= 4 (vector-ref vs (sub1 n)))))))
(evaluate n sol)
(evaluate (list x y z) sol)
(evaluate vs sol)
(evaluate xs sol)]

@section{Lifted Operations on Vectors}

The following vector operations are lifted to work on both concrete and symbolic values:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Vector Operations} @elem{@vector-ops,  @more-vector-ops}))]

@(rosette-eval '(clear-vc!))

@section{Additional Operations on Vectors}

Rosette provides the following procedures for operating on vectors using @seclink["sec:bitvectors"]{bitvector} indices and lengths. These procedures produce symbolic values that avoid @racketlink[bitvector->natural]{casting} their bitvector arguments to integers, leading to @seclink["sec:notes"]{more efficiently solvable queries}.

@declare-exporting[rosette/base/base #:use-sources (rosette/base/base)]


@defproc[(vector-length-bv [vec vector?] [t (or/c bitvector? union?)]) bv?]{
Equivalent to @racket[(integer->bitvector (vector-length vec) t)] but avoids the @racket[integer->bitvector] cast for better solving performance.

@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define xs (if b (vector 1 2) (vector 3 4 5 6)))
xs
(integer->bitvector (vector-length xs) (bitvector 4))
(vector-length-bv xs (bitvector 4))]
}

@(rosette-eval '(clear-vc!))
@defproc[(vector-ref-bv [vec vector?] [pos bv?]) any/c]{
Equivalent to @racket[(vector-ref vec (bitvector->natural pos))] but avoids the @racket[bitvector->natural] cast for better solving performance.

@examples[#:eval rosette-eval
(define-symbolic p (bitvector 1))
(define xs (vector 1 2 3 4))
(code:comment "Uses a cast and generates a redundant assertion on the range of p:")
(vector-ref xs (bitvector->natural p))
(vc)
(clear-vc!)
(code:comment "No cast and no redundant range assertion:")
(vector-ref-bv xs p)
(vc)
(code:comment "But the range assertion is generated when needed:")
(define-symbolic q (bitvector 4))
(vector-ref-bv xs q)
(vc)]
}

@(rosette-eval '(clear-vc!))
@defproc[(vector-set!-bv [vec vector?] [pos bv?] [val any/c]) void?]{
Equivalent to @racket[(vector-set! vec (bitvector->natural pos) val)] but avoids the @racket[bitvector->natural] cast for better solving performance.

@examples[#:eval rosette-eval
(define-symbolic p (bitvector 1))
(define xs (vector 1 2 3 4))
(code:comment "Uses a cast and generates a redundant assertion on the range of p:")
(vector-set! xs (bitvector->natural p) 5)
xs
(vc)
(clear-vc!)
(code:comment "No cast and no redundant range assertion:")
(define xs (vector 1 2 3 4))
(vector-set!-bv xs p 5)
xs
(vc)
(code:comment "But the range assertion is generated when needed:")
(define-symbolic q (bitvector 4))
(define xs (vector 1 2 3 4))
(vector-set!-bv xs q 5)
xs
(vc)]              
}

