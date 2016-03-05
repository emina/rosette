#lang scribble/manual

@(require (for-label 
           rosette/base/define racket)
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
(equal? (list 1) (list 1))
(equal? (box 1) (box 1))
(equal? (list (box 1)) (list (box 1)))
(define-symbolic n number?)
(equal? (box n) (box 1))]

The @racket[eq?] predicate follows the Racket semantics for primitive and mutable datatypes, but
not for transparent immutable datatypes, such as lists.  Rosette treats instances of such datatypes as values, 
while Racket treats them as references. Racket's @racket[eq?] therefore returns @racket[#f] when 
given two instances of a transparent immutable type, regardless of their contents.  
The lifted @racket[eq?], in contrast, returns @racket[#t] when the given instances have 
@racket[eq?] contents.   

@examples[#:eval rosette-eval
(eq? 1 1)
(eq? (list 1) (list 1))
(eq? (box 1) (box 1))
(eq? (list (box 1)) (list (box 1)))
(define-symbolic n number?)
(eq? n 1)]

@(kill-evaluator rosette-eval)
