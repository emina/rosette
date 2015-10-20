rosette
=======

[![Build Status](https://travis-ci.org/emina/rosette.svg?branch=master)](https://travis-ci.org/emina/rosette)

This repository includes the source code and default solver binaries
for the Rosette solver-aided host language, as well as several example
solver-aided DSLs.

### Installing Rosette

* Download and install Racket 6.1 from http://racket-lang.org

* Make sure that the default Java installation on your system is a
  64-bit server VM, version 1.7x:

  `$ java -version`  
  `java version "1.7.0_25"`  
  `Java(TM) SE Runtime Environment (build 1.7.0_25-b15)`  
  `Java HotSpot(TM) 64-Bit Server VM (build 23.25-b01, mixed mode)`

* Clone the rosette repository:

  `$ git clone git@github.com:emina/rosette.git`

* Use Racket's `raco` tool to install Rosette as one of your Racket collections:

  `$ cd rosette`  
  `$ raco link rosette`  
  `$ raco setup -l rosette`  

* Rosette ships with the [Kodkod](http://alloy.mit.edu/kodkod/) solver 
  binaries, but it also supports [Z3](http://z3.codeplex.com) and 
  [CVC4](http://cvc4.cs.nyu.edu/web/).  To use Z3 or CVC4, 
  download (or build) the binaries for your system and put them in the `rosette/bin` directory.

### Executing Rosette programs

* Open the target program in DrRacket (e.g., [`rosette/sdsl/fsm/demo.rkt`](https://github.com/emina/rosette/blob/master/sdsl/fsm/demo.rkt))
  and hit run!

* DrRacket is the preferred way to execute Rosette programs.  If you
  need to use the command line, make sure to first compile the program:

  `$ raco make <your program>`  
  `$ racket -r <your program>`  

### Available languages

* Rosette ships with two languages: `#lang s-exp rosette/safe` and  `#lang s-exp rosette`.

* The `rosette/safe` language includes only constructs that are safe to
  use with symbolic values.  This (for now) excludes some nice Racket
  features, such as iteration constructs.  The semantics of these
  constructs can be expressed in the core language, however, so no
  expressiveness is lost (just convenience).  It is recommended for
  new users of Rosette to start with the `rosette/safe` language.  To
  see the list of syntactic forms and procedures provided by
  `rosette/safe`, type the following into the Rosette REPL:
  
  `> (rosette)`  
  `'(define assert let let* ...)`

* The `rosette` language includes all of Racket.  This places the burden
  on the programmer to decide whether a given Racket construct (which
  is not overriden by Rosette) is safe to use in a given context.
  Rosette provides no guarantees or checks for programs that use
  unsafe constructs.  In the best case, such a program will fail with
  an exception if a symbolic value flows to a construct that does not
  support it.  In the worst case, it will continue executing with
  incorrect semantics or cause more serious problems (e.g., data loss if 
  it writes to a file).

* For more on Rosette, see:

  - Emina Torlak.  [_The Rosette Guide_](http://homes.cs.washington.edu/~emina/rosette/guide/index.html).
  - Emina Torlak and Rastislav Bodik. [_A lightweight symbolic
  virtual machine for solver-aided host languages._](http://people.csail.mit.edu/emina/pubs/rosette.pldi14.pdf) In PLDI'14.

  - Emina Torlak and Rastislav Bodik. [_Growing solver-aided
  languages with rosette._](http://people.csail.mit.edu/emina/pubs/rosette.onward13.pdf) In Onward!'13.
