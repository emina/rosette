#lang scribble/manual

@(require scribble/bnf
          (only-in racket/draw read-bitmap)
          (for-label
           rosette/lib/value-browser
           (only-in racket/gui snip%)
           rosette/base/form/define
           rosette/base/core/term
           (only-in rosette/base/base bv bitvector ~>)
           racket)
          scribble/core scribble/html-properties racket/runtime-path)

@(define-runtime-path root ".")

@title[#:tag "sec:utility-libs"]{Utility Libraries}

Following are utilities that help aiding development of solver-aided programs.

@section{Value Browser Library}

@defmodule[rosette/lib/value-browser]

@defproc[(render-value/snip [val any/c] [#:handler handler (-> any/c (-> any/c (is-a?/c snip%)) #,(tech "layout")) (λ (value rec) #f)]) (is-a?/c snip%)]{
Constructs a @racket[snip%] object that displays information about @racket[val] and could switch between short form and long form.
In DrRacket, evaluating the snip at the top-level will display the snip in the REPL.
It could be similarly displayed via @racket[print].

The snip can correctly display any value of @seclink["ch:built-in-datatypes"]{lifted datatypes}. Any other kind of values (e.g., @racket[string], @racket[hash]) will be displayed with the kind @racket[other] without further details. The optional argument @racket[handler] can be supplied to customize the display of the other kind of values. See @tech{layout} and the examples below for more details.
}

@defproc[(render-value/window [val any/c] [#:handler handler (-> any/c (-> any/c (is-a?/c snip%)) #,(tech "layout")) (λ (value rec) #f)]) (is-a?/c snip%)]{
Similar to @racket[render-value/snip], but displays the snip in a new window frame. This is useful when the program is run in command-line.
}

@subsection{Layout}

A @deftech{layout} is an embedded DSL describing the tabular layout to display a value of unlifted datatypes in the value browser. It has the following grammar:

@BNF[(list @nonterm{layout}
           @BNF-alt[@litchar{#f}
                    @elem{a @racket[list] of @nonterm{row}s}])
     (list @nonterm{row}
           @BNF-alt[@litchar{'#:gap}
                    @elem{a @racket[list] of @nonterm{col}s}])
     (list @nonterm{col}
           @BNF-alt[
             @elem{a @racket[snip%]}
             @nonterm{string}
             @BNF-seq[@litchar{(emph} @nonterm{string} @litchar{)}]])
     (list @nonterm{string}
           @elem{a @racket[string]})]

where @racket[#f] means the value is atomic (which will be categorized under the kind @code{other}), @racket['#:gap] means a vertical gap, and @racket[emph] means emphasis.

@subsection{Examples}

The following are values of lifted datatypes, so the value browser can display them correctly.

@(define (get-image name)
   (call-with-input-file (build-path root name)
                         (lambda (in) (read-bitmap in 'png #:backing-scale 2))))

@racketblock[
#, @elem{>} (define-symbolic n integer?)
#, @elem{>} (render-value/snip (list 1
                                     #t
                                     n
                                     (bv 1 (bitvector 2))
                                     +))
(code:comment "after expanding the snip")
#,(get-image "browser-atomic.png")
#, @elem{>} (define-symbolic f (~> integer? integer?))
#, @elem{>} (render-value/snip (list (+ n 1)
                                     (if (= n 0) 1 2)
                                     (f 1)))
#,(get-image "browser-expr.png")
#, @elem{>} (struct foo [a b])
#, @elem{>} (render-value/snip (list (list 1 2)
                                     (foo 1 2)
                                     (vector 1 2)
                                     (box 1)
                                     (if (= n 1) 2 "a")))
#,(get-image "browser-compound.png")
]

However, when we try to display a value of unlifted datatypes like a @racket[hash] or a @racket[set], the browser will simply categorize it as @code{other} without further details.

@racketblock[
#, @elem{>} (render-value/snip (list (hash 1 2 3 4)
                                     (set 1 2 3)))
#,(get-image "browser-unlifted.png")
]

To allow a detailed display of hashes, supply a @racket[handler] that
returns a desired @tech{layout} when a hash is passed in.
For instance, we might want @racket[(hash a b c d)] to have the following layout:

@racketblock[
`([(emph "Kind: ") "hash"]
  #:gap
  [,snip-a]
  [,snip-b]
  #:gap
  [,snip-c]
  [,snip-d])
]

where @racket[snip-a], @racket[snip-b], @racket[snip-c], and @racket[snip-d] can be
obtained by applying the recursive renderer to @racket[a], @racket[b], @racket[c],
and @racket[d] respectively.

The revised code then would be:

@racketblock[
#, @elem{>} (render-value/snip (list (hash 1 2 3 4)
                                     (set 1 2 3))
                     #:handler (λ (value rec)
                                 (cond
                                   [(hash? value)
                                    (append*
                                     '([(emph "Kind: ") "hash"])
                                     (for/list ([(k v) (in-hash value)])
                                       `(#:gap
                                         [,(rec k)]
                                         [,(rec v)])))]
                                   [else #f])))
#,(get-image "browser-unlifted-corrected.png")
]

which now displays hashes with details. Note that you should use the recursive renderer (provided as the second argument) rather than calling @racket[render-value/snip] directly so that the correct handler is used when rendering sub-value (e.g., a hash of hashes).
