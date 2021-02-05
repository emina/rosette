#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/query 
           rosette/base/core/term  
           (only-in rosette/base/base assert) 
           racket)
          scribble/core scribble/html-properties scribble/examples racket/sandbox racket/runtime-path
          "../util/lifted.rkt")


@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "procedures-log")))

@(define proc-ops (select '(procedure? apply compose compose1 procedure-rename procedure->method procedure-closure-contents-eq? )))
@(define more-proc-ops (select '(identity const thunk thunk* negate curry curryr normalized-arity? normalize-arity arity=? arity-includes? prop:procedure)))

@title[#:tag "sec:proc"]{Procedures}

Rosette procedures are references to procedure objects, just like in Racket.  
Two procedure references are @racket[eq?] and @racket[equal?] only if they point to the 
same procedure object.  Procedures may be concrete or symbolic.  Symbolic procedures  
may, in the worst case, take as much time to execute as the slowest concrete procedure to 
which any symbolic procedure could @racket[evaluate] under any @racket[solution?].

@(rosette-eval '(require (only-in racket string->symbol)))
@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define-symbolic x integer?)
(code:line (define p (if b * -)) (code:comment "p is a symbolic procedure."))
(define sol (synthesize #:forall (list x)
                        #:guarantee (assert (= x (p x 1)))))
(evaluate p sol)
(define sol (synthesize #:forall (list x)
                        #:guarantee (assert (= x (p x 0)))))
(evaluate p sol)                                                         
]

Rosette lifts the following operations on procedures:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{@proc-ops,  @more-proc-ops}))]

@(kill-evaluator rosette-eval)