#lang scribble/manual

@(require (for-label racket)
          scribble/core scribble/html-properties 
          "../util/lifted.rkt")


@(define io (select '(input-port? output-port? port? close-input-port close-output-port port-closed? port-closed-evt current-input-port current-output-port current-error-port file-stream-port? terminal-port? eof eof-object? read read-syntax read/ recursive read-syntax/ recursive read-language read-case-sensitive read-square-bracket-as-paren read-curly-brace-as-paren read-accept-box read-accept-compiled read-accept-bar-quote read-accept-graph read-decimal-as-inexact read-accept-dot read-accept-infix-dot read-accept-quasiquote read-accept-reader read-accept-lang current-readtable call-with-default-reading-parameterization current-reader-guard read-on-demand-source port-read-handler write display print displayln fprintf printf eprintf format print-pair-curly-braces print-mpair-curly-braces print-unreadable print-graph print-struct print-box print-vector-length print-hash-table print-boolean-long-form print-reader-abbreviations print-as-expression print-syntax-width current-write-relative-directory port-write-handler port-display-handler port-print-handler global-port-print-handler pretty-print pretty-write pretty-display pretty-format pretty-print-handler)))
@(define os (select '(time current-seconds current-milliseconds)))

@title[#:tag "sec:racket-libs"]{Exported Racket Libraries}

Rosette exports the following facilities from the core Racket libraries:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Input and Output} @io)
      (list @elem{Operating System} @os))]

These facilities are safe to use in Rosette programs, even in the presence of symbolic values, assertions, and solver-aided queries. They are not, however, @tech[#:key "lifted constructs"]{lifted}:  if their Racket implementation expects a concrete value of a given type, they will fail when given a symbolic value.  These constructs are safe to use in the sense that they will fail in a predictable fashion, according to their concrete Racket specification, instead of causing the enclosing Rosette program to exhibit unexpected behavior.

The @racket[rosette/safe] language allows programs to import arbitrary Racket code using the standard @racket[require] mechanism.  This is strongly discouraged, however, unless the use of such code obeys the restrictions outlined in the @seclink["ch:unsafe"]{Chapter 8}.  Violating these restrictions may lead to incorrect program behavior, crashes, and loss of data (for programs that perform external side-effects, such as writing to files).  In other words, arbitrary Racket code is, by default, unsafe to use.

