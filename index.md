---
title: About
---

## About Rosette

Rosette is a solver-aided programming language that extends
[Racket](http://racket-lang.org/) with language constructs for program
synthesis, verification, and more. To verify or synthesize code,
Rosette compiles it to logical constraints solved with
off-the-shelf [SMT](http://smtlib.cs.uiowa.edu) solvers. By combining
virtualized access to solvers with Racket's metaprogramming, Rosette
makes it easy to develop synthesis and verification tools for new
languages.  You simply write an interpreter for your language in
Rosette, and you get the tools for free!

```racket
#lang rosette

(define (interpret formula)
  (match formula
    [`(∧ ,expr ...) (apply && (map interpret expr))]
    [`(∨ ,expr ...) (apply || (map interpret expr))]
    [`(¬ ,expr)     (! (interpret expr))]
    [lit            (constant lit boolean?)]))

; This implements a SAT solver.
(define (SAT formula)
  (solve (assert (interpret formula))))  

(SAT `(∧ r o (∨ s e (¬ t)) t (¬ e)))
```

To learn more, take a look at [The Rosette Guide]({{site.doc_dir}}/index.html),
[this talk](https://www.youtube.com/watch?v=KpDyuMIb_E0&index=25&list=PLZdCLR02grLp4W4ySd1sHPOsK83gvqBQp),
[applications](apps.html), or publications:  

{: .bibliography}
* [1] Emina Torlak and Rastislav Bodik. A Lightweight Symbolic Virtual Machine for Solver-Aided Host Languages. PLDI 2014.
([ACM](http://dl.acm.org/citation.cfm?id=2594340), [PDF](http://homes.cs.washington.edu/~emina/pubs/rosette.pldi14.pdf))
* [2] Emina Torlak and Rastislav Bodik. Growing Solver-Aided Languages with Rosette. Onward! 2013. ([ACM](http://dl.acm.org/citation.cfm?id=2509586), [PDF](http://homes.cs.washington.edu/~emina/pubs/rosette.onward13.pdf))
