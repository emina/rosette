#lang scribble/manual

@(require (for-label 
           rosette/base/form/define  
           rosette/base/core/term 
           (only-in rosette/base/core/union union?)
           (only-in rosette/base/base bv bv? bitvector bitvector? bitvector-size
                    bveq bvslt bvsgt bvsle bvsge bvult bvugt bvule bvuge
                    bvnot bvor bvand bvxor bvshl bvlshr bvashr
                    bvneg bvadd bvsub bvmul bvudiv bvsdiv bvurem bvsrem bvsmod
                    concat extract sign-extend zero-extend 
                    integer->bitvector bitvector->integer bitvector->natural
                    bit lsb msb bvzero? bvadd1 bvsub1
                    bvsmin bvsmax bvumin bvumax
                    rotate-left rotate-right bvrol bvror
                    bool->bitvector bitvector->bool bitvector->bits
                    assert vc))
          (for-label racket)
          scribble/core scribble/html-properties scribble/examples racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:bitvectors"]{Bitvectors}

@declare-exporting[rosette/base/base #:use-sources (rosette/base/base)]

Rosette extends Racket with a primitive bitvector datatype whose values are
fixed-size words---or, machine integers.  Mainstream programming languages, such as
C or Java, support bitvector types with a few fixed sizes (e.g., 8 bits, 16 bits,
and 32 bits). Rosette supports bitvectors of arbitrary size, as well as both signed and
unsigned versions of various bitvector operations (such as comparisons, division, remainder, etc.).
Technically, Rosette's bitvector datatype embeds the
@hyperlink["http://smtlib.cs.uiowa.edu/logics-all.shtml#QF_BV"]{theory of bitvectors}
into a programming language.

@examples[#:eval rosette-eval
(code:line (bv 4 (bitvector 7))        (code:comment "A bitvector literal of size 7."))
(code:line (bv 4 7)                    (code:comment "A shorthand for the same literal."))
(code:line (define-symbolic x y (bitvector 7)) (code:comment "Symbolic bitvector constants."))
(code:line (bvslt (bv 4 7) (bv -1 7))  (code:comment "Signed 7-bit < comparison of 4 and -1."))
(code:line (bvult (bv 4 7) (bv -1 7))  (code:comment "Unsigned 7-bit < comparison of 4 and -1."))
(define-symbolic b boolean?)
(code:line (bvadd x (if b y (bv 3 4))) (code:comment "This typechecks only when b is true,"))
(code:line (vc)                        (code:comment "so Rosette emits a corresponding assertion."))]

@defproc[(bitvector [size (and/c integer? positive? (not/c term?) (not/c union?))]) bitvector?]{
                                                                                                
  Returns a type predicate that recognizes bitvectors of the given @racket[size].
  Note that @racket[size] must be a concrete positive integer.                                                        
  The type predicate itself is recognized by the @racket[bitvector?] predicate.
  
  @examples[#:eval rosette-eval
   (define bv6? (bitvector 6))
   (bv6? 1)
   (bv6? (bv 3 6))
   (bv6? (bv 3 5))
   (define-symbolic b boolean?)
   (bv6? (if b (bv 3 6) #t))]
}

@defproc[(bitvector? [v any/c]) boolean?]{
                                          
  Returns true if @racket[v] is a concrete type predicate that recognizes bitvector values.
                  
   @examples[#:eval rosette-eval
   (define bv6? (bitvector 6))
   (define bv7? (bitvector 7))
   (define-symbolic b boolean?)
   (code:line (bitvector? bv6?)             (code:comment "A concrete bitvector type."))
   (code:line (bitvector? (if b bv6? bv7?)) (code:comment "Not a concrete type."))
   (code:line (bitvector? integer?)         (code:comment "Not a bitvector type."))
   (code:line (bitvector? 3)                (code:comment "Not a type."))]}

@defproc[(bv [val (and/c integer? (not/c term?) (not/c union?))]
             [size (and/c (or/c bitvector? (and/c integer? positive?))
                          (not/c term?) (not/c union?))]) bv?]{
  Returns a bitvector literal of the given @racket[size], which may be given either as a
  concrete @racket[bitvector?] type or a concrete positive integer.}

@defproc[(bv? [v any/c]) boolean?]{
  Recognizes concrete or symbolic bitvector values of any size.
  @examples[#:eval rosette-eval
   (bv? 1)
   (bv? (bv 1 1))
   (bv? (bv 2 2))
   (define-symbolic b boolean?)
   (bv? (if b (bv 3 6) #t))]
}

@(rosette-eval '(clear-vc!))

@section{Comparison Operators}

@defproc*[([(bveq [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvslt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvult [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsle [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvule [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsgt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvugt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsge [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvuge [x (bitvector n)] [y (bitvector n)]) boolean?])]{
                                                                    
Compares two bitvector values of the same bitvector type. 
Comparison relations include 
equality (@racket[bveq]) and signed / unsigned versions of
<, <=, >, and >= (@racket[bvslt], @racket[bvult], @racket[bvsle], @racket[bvule],
@racket[bvsgt], and @racket[bvugt]).

@examples[#:eval rosette-eval
(code:line (define-symbolic u v (bitvector 7)) (code:comment "Symbolic bitvector constants."))
(code:line (bvslt (bv 4 7) (bv -1 7))  (code:comment "Signed 7-bit < comparison of 4 and -1."))
(code:line (bvult (bv 4 7) (bv -1 7))  (code:comment "Unsigned 7-bit < comparison of 4 and -1."))
(define-symbolic b boolean?)
(code:line (bvsge u (if b v (bv 3 4))) (code:comment "This typechecks only when b is true,"))
(code:line (vc)                        (code:comment "so Rosette emits a corresponding assertion."))]
}

@(rosette-eval '(clear-vc!))

@section{Bitwise Operators}

@defproc[(bvnot [x (bitvector n)]) (bitvector n)]{
                                                  
 Returns the bitwise negation of the given bitvector value.
 
 @examples[#:eval rosette-eval
 (bvnot (bv -1 4))
 (bvnot (bv 0 4))
 (define-symbolic b boolean?)
 (code:line (bvnot (if b 0 (bv 0 4))) (code:comment "This typechecks only when b is false,"))
 (code:line (vc)                      (code:comment "so Rosette emits a corresponding assertion."))]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(bvand [x (bitvector n)] ...+) (bitvector n)]
           [(bvor  [x (bitvector n)] ...+) (bitvector n)]
           [(bvxor [x (bitvector n)] ...+) (bitvector n)])]{
                                                            
Returns the bitwise "and", "or", "xor" of one or more bitvector values of the same type.

 @examples[#:eval rosette-eval
 (bvand (bv -1 4) (bv 2 4))
 (bvor  (bv 0 3)  (bv 1 3))
 (bvxor (bv -1 5) (bv 1 5))
 (define-symbolic b boolean?)
 (code:line
  (bvand (bv -1 4)
         (if b 0 (bv 2 4))) (code:comment "This typechecks only when b is false,"))
 (code:line
  (vc)                      (code:comment "so Rosette emits a corresponding assertion."))]
}

  
@(rosette-eval '(clear-vc!))
@defproc*[([(bvshl  [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvlshr [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvashr [x (bitvector n)] [y (bitvector n)]) (bitvector n)])]{
                                                                          
Returns the left, logical right, or arithmetic right shift of @racket[x]  by
@racket[y] bits, where @racket[x] and @racket[y] are bitvector values of the same type.

 @examples[#:eval rosette-eval
 (bvshl  (bv 1 4) (bv 2 4))
 (bvlshr (bv -1 3) (bv 1 3))
 (bvashr (bv -1 5) (bv 1 5))
 (define-symbolic b boolean?)
 (code:line (bvshl (bv -1 4)
                   (if b 0 (bv 2 4))) (code:comment "This typechecks only when b is false,"))
 (code:line (vc)                      (code:comment "so Rosette emits a corresponding assertion."))]
}

@section{Arithmetic Operators}

@defproc[(bvneg [x (bitvector n)]) (bitvector n)]{
                                                  
 Returns the arithmetic negation of the given bitvector value.
 
 @examples[#:eval rosette-eval
 (bvneg (bv -1 4))
 (bvneg (bv 0 4))
 (define-symbolic z (bitvector 3))
 (bvneg z)]
}

@(rosette-eval '(clear-vc!))
@(rosette-eval '(clear-terms!))

@defproc*[([(bvadd [x (bitvector n)] ...+) (bitvector n)]
           [(bvsub [x (bitvector n)] ...+) (bitvector n)]
           [(bvmul [x (bitvector n)] ...+) (bitvector n)])]{
                                                            
Returns the sum, difference, or product of one or more bitvector values of the same type.

 @examples[#:eval rosette-eval
 (bvadd (bv -1 4) (bv 2 4))
 (bvsub (bv 0 3)  (bv 1 3))
 (bvmul (bv -1 5) (bv 1 5))
 (define-symbolic b boolean?)
 (bvadd (bv -1 4) (bv 2 4) (if b (bv 1 4) "bad"))
 (vc)]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(bvsdiv [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvudiv [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvsrem [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvurem [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvsmod [x (bitvector n)] [y (bitvector n)]) (bitvector n)])]{
                                                                          
Returns (un)signed quotient, remainder, or modulo of two bitvector values of the same type.
All five operations are defined even when the second argument is zero.

 @examples[#:eval rosette-eval
 (bvsdiv (bv -3 4) (bv 2 4))
 (bvudiv (bv -3 3) (bv 2 3))
 (bvsmod (bv 1 5) (bv 0 5))
 (define-symbolic b boolean?)
 (bvsrem (bv -3 4) (if b (bv 2 4) "bad"))
 (vc)]
}

@(rosette-eval '(clear-vc!))

@section{Conversion Operators}

@defproc[(concat [x bv?] ...+) bv?]{
                                    
 Returns the bitwise concatenation of the given bitvector values.
 
 @examples[#:eval rosette-eval
 (concat (bv -1 4) (bv 0 1) (bv -1 3))
 (define-symbolic b boolean?)
 (concat (bv -1 4) (if b (bv 0 1) (bv 0 2)) (bv -1 3))]
}

@defproc[(extract [i integer?] [j integer?] [x (bitvector n)]) (bitvector (+ 1 (- i j)))]{
                                                                                          
 Extracts bits @racket[i] down to @racket[j] from a bitvector of size @racket[n], yielding a
 bitvector of size i - j + 1.  This procedure assumes that @racket[n] > @racket[i] >= @racket[j] >= 0.
 
 @examples[#:eval rosette-eval
 (extract 2 1 (bv -1 4))
 (extract 3 3 (bv 1 4))
 (define-symbolic i j integer?)
 (extract i j (bv 1 2)) 
 (vc)]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(sign-extend [x bv?] [t (or/c bitvector? union?)]) bv?]
           [(zero-extend [x bv?] [t (or/c bitvector? union?)]) bv?])]{
                                                                      
Returns a bitvector of type @racket[t] that represents the (un)signed
extension of the bitvector @racket[x].
Note that both @racket[x] and @racket[t] may be symbolic. The size of @racket[t]
must not be smaller than the size of @racket[x]'s type.

 @examples[#:eval rosette-eval
 (sign-extend (bv -3 4) (bitvector 6))
 (zero-extend (bv -3 4) (bitvector 6))
 (define-symbolic b c boolean?)
 (zero-extend (bv -3 4) (if b (bitvector 5) (bitvector 6)))
 (zero-extend (bv -3 4) (if b (bitvector 5) "bad"))
 (vc)
 (zero-extend (bv -3 4) (if c (bitvector 5) (bitvector 1)))
 (vc)]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(bitvector->integer [x bv?]) integer?]
           [(bitvector->natural [x bv?]) integer?])]{
                                                     
Returns the (un)signed integer value of the given bitvector.

 @examples[#:eval rosette-eval
 (bitvector->integer (bv -1 4))
 (bitvector->natural (bv -1 4))
 (define-symbolic b boolean?)
 (bitvector->integer (if b (bv -1 3) (bv -3 4)))
 (bitvector->integer (if b (bv -1 3) "bad"))
 (vc)]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(integer->bitvector [i integer?] [t (or/c bitvector? union?)]) bv?])]{
                                                                                  
Returns a bitvector of type @racket[t] that represents the @var[k] lowest order bits
of the integer @racket[i], where @var[k] is the size of @racket[t].              
Note that both @racket[i] and @racket[t] may be symbolic.

 @examples[#:eval rosette-eval
 (integer->bitvector 4 (bitvector 2))
 (integer->bitvector 15 (bitvector 4))
 (define-symbolic b c boolean?)
 (integer->bitvector (if b pi 3) (if c (bitvector 5) (bitvector 6)))
 (vc)]
}

@section{Additional Operators}

@(rosette-eval '(clear-vc!))

@defproc[(bit [i integer?] [x (bitvector n)]) (bitvector 1)]{
                                                             
 Extracts the @racket[i]th bit from the bitvector @racket[x] of size @racket[n], yielding a
 bitvector of size 1.  This procedure assumes that @racket[n] > @racket[i] >= 0.
 
 @examples[#:eval rosette-eval
 (bit 1 (bv 3 4))
 (bit 2 (bv 1 4))
 (define-symbolic i integer?)
 (define-symbolic x (bitvector 4))
 (bit i x) 
 (vc)]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(lsb [x (bitvector n)]) (bitvector 1)]
           [(msb [x (bitvector n)]) (bitvector 1)])]{
                                                     
 Returns the least or most significant bit of @racket[x].
                                              
 @examples[#:eval rosette-eval
  (lsb (bv 3 4))
  (msb (bv 3 4))
  (define-symbolic x (bitvector 4))
  (define-symbolic y (bitvector 8))
  (lsb (if b x y))
  (msb (if b x y))
  ]
}

@(rosette-eval '(clear-vc!))

@defproc[(bvzero? [x (bitvector n)]) boolean?]{
                                               
Returns @racket[(bveq x (bv 0 n))].
        
 @examples[#:eval rosette-eval
  (define-symbolic x (bitvector 4))
  (bvzero? x)
  (define-symbolic y (bitvector 8))
  (bvzero? y)
  (define-symbolic b boolean?)
  (bvzero? (if b x y))
 ]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(bvadd1 [x (bitvector n)]) (bitvector n)]
           [(bvsub1 [x (bitvector n)]) (bitvector n)])]{
                                                        
 Returns @racket[(bvadd x (bv 1 n))] or @racket[(bvsub x (bv 1 n))].
         
 @examples[#:eval rosette-eval
  (define-symbolic x (bitvector 4))
  (bvadd1 x)
  (define-symbolic y (bitvector 8))
  (bvsub1 y)
  (define-symbolic b boolean?)
  (bvadd1 (if b x y))
  (bvsub1 (if b x y))
  ]
}


@(rosette-eval '(clear-vc!))

@defproc*[([(bvsmin [x (bitvector n)] ...+) (bitvector n)]
           [(bvumin [x (bitvector n)] ...+) (bitvector n)]
           [(bvsmax [x (bitvector n)] ...+) (bitvector n)]
           [(bvumax [x (bitvector n)] ...+) (bitvector n)])]{
                                                             
Returns the (un)signed minimum or maximum of one or more bitvector values of the same type.

 @examples[#:eval rosette-eval
 (bvsmin (bv -1 4) (bv 2 4))
 (bvumin (bv -1 4) (bv 2 4))
 (bvsmax (bv -1 4) (bv 2 4))
 (bvumax (bv -1 4) (bv 2 4))
 (define-symbolic b boolean?)
 (bvsmin (bv -1 4) (bv 2 4) (if b (bv 1 4) (bv 3 8)))
 (vc)]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(bvrol [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvror [x (bitvector n)] [y (bitvector n)]) (bitvector n)])]{
                                                                         
Returns the left or right rotation of @racket[x] by @racket[(bvurem y n)] bits, where
@racket[x] and @racket[y] are bitvector values of the same type.

 @examples[#:eval rosette-eval
 (bvrol (bv 3 4) (bv 2 4))
 (bvrol (bv 3 4) (bv -2 4))
 (define-symbolic b boolean?)
 (code:line
  (bvror (bv 3 4)
         (if b 0 (bv 2 4))) (code:comment "This typechecks only when b is false,"))
 (code:line
  (vc)                      (code:comment "so Rosette emits a corresponding assertion."))]
}

@(rosette-eval '(clear-vc!))

@defproc*[([(rotate-left  [i integer?] [x (bitvector n)]) (bitvector n)]
           [(rotate-right [i integer?] [x (bitvector n)]) (bitvector n)])]{
                                                                           
Returns the left or right rotation of @racket[x] by @racket[i] bits.
These procedures assume that @racket[n] > @racket[i] >= 0. See @racket[bvrol]
and @racket[bvror] for an alternative way to perform rotations that usually
leads to faster solving times.

 @examples[#:eval rosette-eval
 (rotate-left 3 (bv 3 4))
 (rotate-right 1 (bv 3 4))
 (define-symbolic i integer?)
 (define-symbolic b boolean?)
 (rotate-left i (if b (bv 3 4) (bv 7 8))) 
 (vc)
 ]
}

@(rosette-eval '(clear-vc!))

@defproc[(bitvector->bits [x (bitvector n)]) (listof (bitvector n))]{
                                                                     
Returns the bits of @racket[x] as a list, i.e., @racket[(list (bit 0 x) ... (bit (- n 1) x))].
                    
 @examples[#:eval rosette-eval
  (bitvector->bits (bv 3 4))
  (define-symbolic y (bitvector 2))
  (bitvector->bits y)
  (define-symbolic b boolean?)
  (bitvector->bits (if b (bv 3 4) y)) ]
}

@defproc[(bitvector->bool [x (bitvector n)]) boolean?]{
Returns @racket[(not (bvzero? x))].
}

@(rosette-eval '(clear-vc!))

@defproc[(bool->bitvector [b any/c] [t (or/c positive-integer? (bitvector n)) (bitvector 1)]) bv?]{
                                                                                                   
Returns @racket[(bv 0 t)] if @racket[(false? b)] and otherwise returns @racket[(bv 1 t)], where
@racket[t] is @racket[(bitvector 1)] by default. If provided, @racket[t] must be a concrete positive
integer or a concrete bitvector type value.

 @examples[#:eval rosette-eval
  (bool->bitvector #f 3)
  (bool->bitvector "non-false-value")
  (define-symbolic b boolean?)
  (bool->bitvector b)
 ]
}

@(kill-evaluator rosette-eval)
