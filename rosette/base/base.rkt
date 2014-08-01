#lang racket

;; Rosette (lifted) syntax and procedures
(require 
  (for-syntax racket/syntax (only-in "lift.rkt" drop@)) 
  racket/provide 
  "primitive.rkt" 
  "any.rkt" 
  "list.rkt" 
  "box.rkt" 
  "vector.rkt" 
  "procedure.rkt" 
  "struct.rkt" 
  "enum.rkt"
  "equality.rkt" 
  "reflect.rkt" 
  "generics.rkt" 
  "state.rkt"
  "module.rkt"  
  "define.rkt" 
  "app.rkt" 
  "assert.rkt" 
  "control.rkt") 

(provide 
 (filtered-out
  drop@
  (combine-out
   (except-out (all-from-out "primitive.rkt") @||)
   (all-from-out "list.rkt" 
                 "box.rkt"
                 "vector.rkt" 
                 "procedure.rkt"
                 "struct.rkt"
                 "enum.rkt"
                 "equality.rkt"
                 "reflect.rkt"
                 "generics.rkt"    
                 "state.rkt"
                 "module.rkt"
                 "define.rkt"
                 "app.rkt"
                 "assert.rkt"
                 "control.rkt")))
 (rename-out [@any? any/c] [@|| ||]))

(require racket/local)

;; Racket syntax and procedures that can be used without being lifted
(provide
 ; require and provide forms
 require 
 only-in except-in prefix-in rename-in 
 combine-in relative-in only-meta-in
 lib file planet submod
 provide 
 all-defined-out all-from-out rename-out except-out 
 prefix-out struct-out combine-out protect-out
 for-meta for-syntax for-template for-label
 ; literals
 quote #%datum
 ; expression wrapper
 #%expression
 ; variable references and #%top
 #%top
 ; procedure expressions
 lambda case-lambda λ #%plain-lambda
 ; local binding
 let let* letrec 
 let-values let*-values letrec-values
 let-syntax letrec-syntax let-syntaxes letrec-syntaxes
 letrec-syntaxes+values
 ; local definitions
 local
 ; definitions
 define define-values define-syntax define-syntaxes
 define-for-syntax define-values-for-syntax
 ; sequencing
 begin begin0 begin-for-syntax
 ; assignment:  this is handled by whole-module rewriting (see module.rkt)
 set! set!-values
 ; quasiquoting
 quasiquote unquote unquote-splicing 
 ; syntax-quoting
 quote-syntax
 ; booleans
 true false
 ; numbers
 pi
 ; procedures
 identity const thunk thunk* curry curryr compose compose1
 ; void
 void
 ; macros
 syntax-case syntax-case* with-syntax
 syntax quasisyntax unsyntax unsyntax-splicing
 syntax/loc quasisyntax/loc quote-syntax/prune
 syntax-rules syntax-id-rules
 define-syntax-rule ... _
 local-expand expand expand-syntax expand-once
 expand-syntax-once expand-to-top-form
 expand-syntax-to-top-form
 ; input and output
 read read-syntax
 write display print displayln fprintf printf eprintf format newline
 pretty-print pretty-write pretty-display pretty-format 
 call-with-input-file
 current-input-port current-output-port current-error-port eof
 ; operating system
 time current-seconds current-milliseconds
 )




 

