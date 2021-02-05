#lang scribble/manual

@(require (for-label rosette/base/form/define rosette/query/query racket
                     (only-in rosette/base/base assert))
          scribble/core scribble/html-properties scribble/examples racket/sandbox racket/runtime-path
          "../util/lifted.rkt")

@(define box-ops (select '(box? box box-immutable unbox set-box! box-cas!)))

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "boxes-log")))

@title[#:tag "sec:box"]{Boxes}

A box is a single (im)mutable storage cell, which behaves like a one-element (im)mutable @seclink["sec:vec"]{vector}.
Like vectors, immutable boxes are treated as transparent immutable values:  they are @racket[eq?] when their
contents are @racket[eq?].  Mutable boxes are references rather than values, so they are @racket[eq?] only when
they point to the same box object.  Boxes can be concrete or symbolic, and they can contain both symbolic and concrete values.

@examples[#:eval rosette-eval
(define v1 (box 1))
(define v2 (box 1))
(eq? v1 v2)
(equal? v1 v2)
(define v3 (box-immutable 1))
(define v4 (box-immutable 1))
(eq? v3 v4)
(equal? v1 v3)
]

@examples[#:eval rosette-eval
(define-symbolic x integer?)
(define-symbolic b boolean?)
(code:line (define v1 (box x))           (code:comment "v1 is a box with symbolic contents."))
(code:line (define v2 (if b v1 (box 3))) (code:comment "v2 is a symbolic box."))
(define sol (solve (assert (= 4 (unbox v2)))))
sol
(evaluate v1 sol)
(evaluate v2 sol)
(evaluate (eq? v1 v2) sol)]

Lifted box operations are shown below.  
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @box-ops))]

@(kill-evaluator rosette-eval)
