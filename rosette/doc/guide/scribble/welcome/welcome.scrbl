#lang scribble/manual

@(require (for-label racket))


@title[#:tag "ch:getting-started"]{Getting Started}

Rosette is a @emph{solver-aided} programming system with two components: 

@itemlist[@item{A @emph{programming language} that extends a  
                  subset of Racket with @seclink["ch:essentials"]{constructs} for accessing 
                  a constraint solver.  With the solver's help, Rosette
                  can answer interesting questions about programs---such as, whether 
                  they are buggy and if so, how to repair them.}
          @item{A @emph{symbolic virtual machine} (SVM) that executes Rosette programs and 
                compiles them to logical constraints.  The SVM enables Rosette 
                to use the solver to automatically reason about program behaviors.}]

The name "Rosette" refers both to the language and the whole system.

@section[#:tag "sec:get"]{Installing Rosette}

To install Rosette, you will need to

@itemlist[@item{Download and install @hyperlink["http://racket-lang.org"]{Racket} (version 6.6).}
           @item{Use Racket's @tt{raco} tool to install Rosette:
                 @nested{
                         @verbatim|{> raco pkg install rosette}|}}]

@section[#:tag "sec:run"]{Interacting with Rosette}

You can interact with Rosette programs just as you would with Racket programs:  either through the @hyperlink["http://docs.racket-lang.org/guide/intro.html"]{DrRacket} IDE or through the @hyperlink["http://docs.racket-lang.org/guide/other-editors.html"]{@tt{racket}} command-line interpreter.  We suggest that you use DrRacket, especially at the beginning.

Example Rosette programs can be found in the @tt{rosette/sdsl} folder.  Most of these are implemented in @emph{solver-aided domain-specific languages} (SDSLs) that are embedded in the Rosette language.  To interact with an @hyperlink["https://github.com/emina/rosette/blob/master/sdsl/fsm/demo.rkt"]{example program}, open it in DrRacket and hit Run!

@section[#:tag "sec:langs"]{Rosette Dialects}

The Rosette system ships with two dialects of the Rosette language: 

@itemlist[@item{a @emph{safe} dialect, which is used throughout this guide, and}
          @item{an @emph{unsafe} dialect, which is briefly described in the @seclink["ch:unsafe"]{last chapter}.}]



To use the safe dialect, start your programs with the following line:

@racketmod[rosette/safe]

To use the unsafe dialect, type this line instead:

@racketmod[rosette]

We strongly recommend that you start with the safe dialect, which includes a core subset of Racket.  The unsafe dialect includes all of Racket, but unless you understand and observe the restrictions on using non-core features, your seemingly correct programs may crash or produce unexpected results.

