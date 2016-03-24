---
title: The Rosette Language
---

## About {#about}

Rosette is a solver-aided programming language that extends
[Racket](http://racket-lang.org/) with language constructs for program
synthesis, verification, and more. To verify or synthesize code,
Rosette compiles it to logical constraints solved with
off-the-shelf [SMT](http://smtlib.cs.uiowa.edu) solvers. By combining
virtualized access to solvers with Racket's metaprogramming, Rosette
makes it easy to develop synthesis and verification tools for new
languages.  You simply write an interpreter for your language in
Rosette, and you get the tools for free!

To learn more, take a look at [The Rosette Guide]({{site.doc_dir}}/index.html),
[applications](apps.html), or publications:

* Emina Torlak and Rastislav
  Bodik. A Lightweight Symbolic Virtual Machine for Solver-Aided Host Languages.
PLDI 2014. [ [pdf](http://homes.cs.washington.edu/~emina/pubs/rosette.onward13.pdf)  | [ACM](http://dl.acm.org/citation.cfm?id=2594340) ]
* Emina Torlak and Rastislav Bodik. Growing Solver-Aided Languages
  with Rosette. Onward! 2013. [ [pdf](http://homes.cs.washington.edu/~emina/pubs/rosette.onward13.pdf)
  | [ACM](http://dl.acm.org/citation.cfm?id=2509586) ]



