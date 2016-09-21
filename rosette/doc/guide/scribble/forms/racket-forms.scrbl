#lang scribble/manual

@(require (for-label rosette/base/form/define)
          (for-label racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define modules 
   (select '(module module* module+ #%module-begin #%printing-module-begin 
              #%plain-module-begin #%declare)))
@(define import/export 
   (select '(require only-in except-in prefix-in rename-in combine-in 
             relative-in only-meta-in lib file planet submod local-require provide 
             all-defined-out all-from-out rename-out except-out prefix-out struct-out 
             combine-out protect-out for-meta for-syntax for-template for-label 
             #%require #%provide ))) 
@(define literals (select '(quote #%datum)))
@(define wrappers (select '(#%expression #%top #%top-interaction)))
@(define apps (select '(#%app #%plain-app)))
@(define procs (select '(lambda λ case-lambda #%plain-lambda)))
@(define local-binding 
   (select '(let let* letrec let-values let*-values letrec-values let-syntax 
              letrec-syntax let-syntaxes letrec-syntaxes letrec-syntaxes+values)))
@(define local-defs (select '(local)))
@(define conditionals (select '(if cond else and or)))
@(define dispatch (select '(case)))
@(define definitions 
   (select '(define define-values define-syntax define-syntaxes 
              define-for-syntax define-values-for-syntax)))
@(define sequencing (select '(begin begin0 begin-for-syntax)))
@(define guarded-eval (select '(when unless)))
@(define assignment (select '(set! set!-values)))
@(define quasiquoting (select '(quasiquote unquote))); unquote-splicing)))
@(define syntax-quoting (select '(quote-syntax)))

@(define rosette-eval (rosette-evaluator))

@title[#:tag "ch:syntactic-forms:racket"]{Lifted Racket Forms}

Rosette lifts the following @seclink["syntax" #:doc '(lib "scribblings/reference/reference.scrbl")]{Racket forms}:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Modules} @modules)
      (list @elem{Import and Export} @import/export)
      (list @elem{Literals} @literals)
      (list @elem{Wrappers} @wrappers)
      (list @elem{Procedure Applications} @apps)
      (list @elem{Procedure Expressions} @procs)
      (list @elem{Local Binding} @local-binding)
      (list @elem{Local Definitions} @local-defs)
      (list @elem{Conditionals} @conditionals)
      (list @elem{Dispatch} @dispatch)
      (list @elem{Definitions} @definitions)
      (list @elem{Sequencing} @sequencing)
      (list @elem{Guarded Evaluation} @guarded-eval)
      (list @elem{Assignment} @assignment)
      (list @elem{Quasiquoting} @quasiquoting)
      (list @elem{Syntax Quoting} @syntax-quoting))]

Lifted forms have the same meaning in Rosette programs as they do in Racket programs. For example, the Racket expression @racket[(if #, @var[test-expr] #, @var[then-expr] #, @var[else-expr])] evaluates @var[test-expr] first and then, depending on the outcome, it returns the result of evaluating either @var[then-expr] or @var[else-expr]. Rosette preserves this interpretation of @racket[if] for concrete values, and also extends it to work with symbolic values:
@interaction[#:eval rosette-eval
 (let ([y 0])
   (if #t (void) (set! y 3))
   (printf "y unchanged: ~a\n" y)
   (if #f (set! y 3) (void)) 
   (printf "y unchanged: ~a\n" y)
   (define-symbolic x boolean?)
   (if x (void) (set! y 3)) 
   (printf "y symbolic: ~a\n" y))]
                     

@(kill-evaluator rosette-eval)
