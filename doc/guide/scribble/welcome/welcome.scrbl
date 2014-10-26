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

Rosette is built on top of Racket, and it ships with a Java-based solver. 
To install Rosette, you will need to

@itemlist[@item{@hyperlink["http://docs.racket-lang.org"]{Download} and install Racket (version 6.1 or later).}
           @item{Make sure that the default Java installation on your machine is a 64-bit server VM, version 1.7x:
                 @nested{
                         @verbatim{> java -version
                                   java version "1.7.0_25"
                                   Java(TM) SE Runtime Environment (build 1.7.0_25-b15)
                                   Java HotSpot(TM) 64-Bit Server VM (build 23.25-b01, mixed mode)}}}
           @item{Obtain the Rosette source code from GitHub:
                 @nested{
                         @verbatim|{> git clone git@github.com:emina/rosette.git
                                    > ls rosette
                                    LICENSE README.md bin guide rosette sdsl test}|}}
           @item{Use Racket's @tt{raco} tool to install Rosette as one of your Racket collections:
                 @nested{
                         @verbatim|{> cd rosette
                                    > raco link rosette
                                    > raco setup -l rosette}|}}]

Your Rosette installation includes binaries for the
@hyperlink["http://alloy.mit.edu/kodkod/"]{Kodkod} 
constraint solver, and it is ready for use as-is. If you 
want to experiment with different solvers, you can also
(optionally) install the the @hyperlink["http://z3.codeplex.com"]{Z3} 
solver from Microsoft Research, or the @hyperlink["http://cvc4.cs.nyu.edu/web/"]{CVC4}
solver from NYU:  simply place the solver binary into the @tt{rosette/bin} folder.

@section[#:tag "sec:run"]{Interacting with Rosette}

You can interact with Rosette programs just as you would with Racket programs:  either through the @hyperlink["http://docs.racket-lang.org/guide/intro.html"]{DrRacket} IDE or through the @hyperlink["http://docs.racket-lang.org/guide/other-editors.html"]{@tt{racket}} command-line interpreter.  We suggest that you use DrRacket, especially at the beginning.

Example Rosette programs can be found in the @tt{rosette/sdsl} folder.  Most of these are implemented in @emph{solver-aided domain-specific languages} (SDSLs) that are embedded in the Rosette language.  To interact with an @hyperlink["https://github.com/emina/rosette/blob/master/sdsl/fsm/demo.rkt"]{example program}, open it in DrRacket and hit Run!

@section[#:tag "sec:langs"]{Rosette Dialects}

The Rosette system ships with two dialects of the Rosette language: 

@itemlist[@item{a @emph{safe} dialect, which is used throughout this guide, and}
          @item{an @emph{unsafe} dialect, which is briefly described in the @seclink["ch:unsafe"]{last chapter}.}]



To use the safe dialect, start your programs with the following line:

@racketmod[s-exp rosette/safe]

To use the unsafe dialect, type this line instead:

@racketmod[s-exp rosette]

We strongly recommend that you start with the safe dialect, which includes a core subset of Racket.  The unsafe dialect includes all of Racket, but unless you understand and observe the restrictions on using non-core features, your seemingly correct programs may crash or produce unexpected results.

