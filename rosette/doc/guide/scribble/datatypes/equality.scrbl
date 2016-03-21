#lang scribble/manual

@(require (for-label rosette/base/form/define racket)
          (for-label (only-in rosette/base/base function?))
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:equality"]{Equality}

Rosette supports two generic equality predicates, @racket[eq?] and @racket[equal?]. 
The @racket[equal?] predicate follows the Racket semantics, extended to work with symbolic values. 
In particular, two values are @racket[equal?] only when they are @racket[eq?], unless a more permissive 
notion of @racket[equal?] is specified for a particular datatype. 

@examples[#:eval rosette-eval
(equal? 1 #t)
(equal? 1 1.0)
(equal? (list 1) (list 1.0))
(equal? (box 1) (box 1))
(equal? (list (box 1)) (list (box 1)))
(define-symbolic n integer?)
(equal? n 1)
(equal? (box n) (box 1))
(define-symbolic f g (~> integer? integer?))
(code:line (equal? f g)             (code:comment "f and g are different procedures"))]
@(kill-evaluator rosette-eval)
@(set! rosette-eval (rosette-evaluator))

The @racket[eq?] predicate follows the Racket semantics for opaque or mutable datatypes,
such as procedures or vectors, but not for transparent immutable datatypes, such as
lists, or transparent solvable types, such as reals.
Rosette treats these transparent types as @emph{value types}, 
while Racket does not. Racket's @racket[eq?] may therefore return @racket[#f] when 
given two instances of such a transparent type, regardless of their contents.  
Rosette's @racket[eq?], in contrast, returns true when given two
transparent solvable values that are @racket[equal?],
or two transparent immutable values with @racket[eq?] contents.   

@examples[#:eval rosette-eval
(eq? 1 #t)
(code:line (eq? 1 1.0)                (code:comment "equal? transparent solvable values"))
(code:line (eq? (list 1) (list 1.0))  (code:comment "transparent immutable values with eq? contents"))
(code:line (eq? (box 1) (box 1))      (code:comment "but boxes are mutable, so eq? follows Racket"))
(eq? (list (box 1)) (list (box 1)))
(define-symbolic n integer?)
(eq? n 1)
(eq? (box n) (box 1))
(define-symbolic f g (~> integer? integer?))
(code:line (eq? f g)                  (code:comment "and procedures are opaque, so eq? follows Racket"))
(eq? f f)]

@(kill-evaluator rosette-eval)
