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

To learn more, take a look at [The Rosette Guide](https://docs.racket-lang.org/rosette-guide/index.html),
[this talk](https://www.youtube.com/watch?v=KpDyuMIb_E0&index=25&list=PLZdCLR02grLp4W4ySd1sHPOsK83gvqBQp),
[applications](apps.html), [courses](courses.html), or [publications](pubs.html).


### Acknowledgments  

This research was supported in part by awards from the National Science
Foundation (NSF CCF [1651225][], [1337415][], [1139138][], and [0916351][]), the
Department of Energy (DOE DE-SC0005136 and DOE FOA-0000619), and gifts from
Intel, Nokia, and Samsung. Rosette extends the [Racket](http://racket-lang.org/)
programming language, and uses the [Z3](https://github.com/Z3Prover/z3) solver
from Microsoft Research. Many thanks to the authors of these systems for making
them freely available, and to Rosette users for many helpful comments and
suggestions.


[0916351]: https://www.nsf.gov/awardsearch/showAward?AWD_ID=0916351
[1139138]: https://www.nsf.gov/awardsearch/showAward?AWD_ID=1139138
[1337415]: https://www.nsf.gov/awardsearch/showAward?AWD_ID=1337415
[1651225]: https://www.nsf.gov/awardsearch/showAward?AWD_ID=1651225
