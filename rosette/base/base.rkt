#lang racket

;; ------ Rosette (lifted) syntax and  procedures ------ ;; 
(require 
  (for-syntax racket/syntax (only-in "core/lift.rkt" drop@)) 
  racket/provide 
  "core/bool.rkt" "core/real.rkt" "core/numerics.rkt" "core/bitvector.rkt"
  "core/function.rkt"
  "core/procedure.rkt" "core/equality.rkt" "core/distinct.rkt" "core/reflect.rkt" 
  "adt/box.rkt" "adt/list.rkt" "adt/vector.rkt" 
  "struct/struct.rkt" "struct/generics.rkt"
  "form/state.rkt" "form/define.rkt" "form/control.rkt" "form/module.rkt" "form/app.rkt") 

(provide
  (rename-out [@|| ||]) ; The character sequence || does not play nicely with the filtered-out form.
  (filtered-out drop@ 
    (combine-out   
     ; core/bool.rkt
     pc with-asserts with-asserts-only asserts clear-asserts!
     @assert @boolean? @false? @! @&& @=> @<=> @forall @exists
     ; core/real.rkt
     @integer? @real? @= @< @<= @>= @> 
     @+ @* @- @/ @quotient @remainder @modulo @abs
     @integer->real @real->integer @int? 
     ; core/numerics.rkt
     @number? @positive? @negative? @zero? @even? @odd?
     @add1 @sub1 @sgn @truncate @floor @ceiling @min @max
     @exact->inexact @inexact->exact @expt 
     ; core/bitvector.rkt
     bv @bv? bitvector bitvector-size bitvector? 
     @bveq @bvslt @bvsgt @bvsle @bvsge @bvult @bvugt @bvule @bvuge
     @bvnot @bvor @bvand @bvxor @bvshl @bvlshr @bvashr
     @bvneg @bvadd @bvsub @bvmul @bvudiv @bvsdiv @bvurem @bvsrem @bvsmod
     @concat @extract @sign-extend @zero-extend 
     @integer->bitvector @bitvector->integer @bitvector->natural
     ; core/function.rkt
     @fv? ~> function?
     ; core/distinct.rkt
     @distinct?
     ; core/equality.rkt
     @eq? @equal?
     ; core/reflect.rkt
     symbolics type? solvable? @any/c type-of type-cast for/all for*/all
     term? constant? expression? 
     term expression constant term-type
     term=? term->datum clear-terms! term-cache
     union? union union-contents union-guards union-values
     union-filter in-union in-union* in-union-guards in-union-values
     ; adt/box.rkt
     @box @box-immutable @box? @unbox @set-box!
     ; adt/list.rkt : Pair Constructors and Selectors
     @pair? @null? @cons @car @cdr @null @list? @list 
     ; adt/list.rkt : List Operations
     @length @list-ref @list-tail @append @reverse
     ; adt/list.rkt : List Iteration
     @map @andmap @ormap @for-each @foldl @foldr
     ; adt/list.rkt : List Filtering
     @filter @remove @remq @remove* @remq* @sort 
     ; adt/list.rkt : List Searching 
     @member @memq @memf @findf @assoc @assq @assf 
     ; adt/list.rkt : Pair Accessor Shorthands
     @caar @cadr @cdar @cddr
     @caaar @caadr @cadar @caddr @cdaar @cdadr @cddar @cdddr 
     @caaaar @caaadr @caadar @caaddr @cadaar @cadadr @caddar @cadddr 
     @cdaaar @cdaadr @cdadar @cdaddr @cddaar @cddadr @cdddar @cddddr
     ; adt/list.rkt : Additional List Functions and Synonyms
     @cons? @empty? @first @rest @second @third @fourth @fifth @sixth @seventh @eighth @ninth @tenth 
     @last @last-pair 
     @take @drop @split-at @take-right @drop-right @split-at-right
     @add-between @append* @flatten @remove-duplicates 
     @filter-map @count @partition @append-map @filter-not @shuffle 
     @argmin @argmax 
     ; adt/list.rkt : Non-Standard Functions
     @insert @replace
     ; adt/vector.rkt : Basic Functions
     @vector? @vector @vector-immutable 
     @vector-length @vector-ref @vector-set! @vector->list @list->vector @vector->immutable-vector 
     @vector-fill! @vector-copy! 
     ; adt/vector.rkt : Additional Vector Functions
     @vector-append
     ; adt/procedure.rkt
     @procedure? @apply @procedure-rename @negate @void? 
     ; struct/struct.rkt
     struct struct-field-index define/generic define-struct
     ; struct/generics.rkt
     @define-generics @make-struct-type-property
     ; form/state.rkt
     current-oracle oracle oracle? 
     ; form/define.rkt
     define-symbolic define-symbolic*
     ; form/control.rkt
     @if @and @or @not @nand @nor @xor @implies
     @unless @when @cond @case else
     ; form/module.rkt
     @#%module-begin @#%top-interaction @module @module @module+
     ; form/app.rkt
     #%app #%plain-app 
     )))

;; ------ Racket syntax and procedures that can be used without being lifted ------ ;; 

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
 ; structs
 prop:procedure gen:equal+hash  gen:custom-write
 ; macros
 syntax-case syntax-case* with-syntax
 syntax quasisyntax unsyntax unsyntax-splicing
 syntax/loc quasisyntax/loc quote-syntax/prune
 syntax-rules syntax-id-rules
 define-syntax define-syntax-rule ... _ 
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




 

