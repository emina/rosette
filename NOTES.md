# Release Notes

## Version 4.1

This is a minor bug-fixing release.

## Version 4.0

This is a major release with significant changes to the language and the runtime.  Rosette 4.0 is *not backward compatible* with Rosette 3.x. But porting Rosette 3.x code to Rosette 4.0 should be straightforward for most applications.

This release includes the following features:

- Support for assumptions (see `assume`).
- New symbolic evaluation core that tracks verification conditions (VCs) rather than path conditions and assertions.
- New symbolic reflection constructs for working with VCs, including `vc`, `with-vc`, and `clear-vc!`.
- New symbolic reflection facilities for managing symbolic `terms`, including the option of using a garbage-collected data structure.
- Updated `verify`, `synthesize`, `solve`, and `optimize` queries.
- New synthesis library with efficient support for grammar holes (see `define-grammar`).
- New list and vector operators that use bitvectors instead of integers.
- Updates to The Rosette Guide to document the new language in detail.

The following features have been removed:

- The `debug` query.
- Reflection facilities for working with path conditions and assertions: `pc`, `with-asserts`, `with-asserts-only`, `clear-asserts!`, and `asserts`.
- Support for CPLEX.

## Version 3.2

This release includes minor updates and a new [value destructuring library].

[value destructuring library]: https://docs.racket-lang.org/rosette-guide/sec_utility-libs.html#%28part._.Value_.Destructuring_.Library%29

## Version 3.1

This release includes bug fixes and updates Rosette to use the latest version of Z3 as its default SMT solver.

This release also includes the following new functionality contributed by [Sorawee Porncharoenwase][]:

- An interactive [value browser][] to help programmers navigate and read complex symbolic values.
- An *error tracer* for finding bugs in Rosette programs that manifest as exceptions intercepted during symbolic evaluation. To use the error tracer, run the command `raco symtrace <prog>`. The [debugging][] chapter in the Rosette guide describes some common issues due to intercepted exceptions, how to test for them, and how to find them with the error tracer.


[Sorawee Porncharoenwase]: https://github.com/sorawee
[debugging]: https://docs.racket-lang.org/rosette-guide/ch_error-tracing.html
[value browser]: https://docs.racket-lang.org/rosette-guide/sec_utility-libs.html#%28part._.Value_.Browser_.Library%29

## Version 3.0

This is a major release with significant changes to the language and the runtime.  Rosette 3.0 is *not backward compatible* with Rosette 2.x. But porting Rosette 2.x code to Rosette 3.0 should be straightforward for most applications.

The semantics of Rosette 3.0 differs from Rosette 2.x in two ways:

- The `current-bitwidth` parameter that controls the reasoning precision is set to `#f` by default. As a result, symbolic constants that are declared to be integers or reals are interpreted in the theories of integers and reals, respectively. This means that the semantics of assertions over these types follows that of Racket. But reasoning about such assertions is expensive (or undecidable), so Rosette 3.0 still provides the option of approximating integer and real constants with finite-precision bitvectors. The key difference is that programs must now *explicitly opt into* this approximation by setting `current-bitwidth` to a positive integer.
- If `current-bitwidth` is set to a positive integer _k_, the solutions produced by the `verify`, `synthesize`, `solve`, and `debug` queries are guaranteed to be correct under the _k_-bit semantics for integer and real constants. They are _not_ guaranteed to be sound with respect to the infinite-precision semantics.

This release also includes the following new functionality and features contributed by [James Bornholt][] and [Phitchaya Mangpo Phothilimthana][]:

- Developed a new *symbolic profiler* for diagnosing performance issues in Rosette programs. The symbolic profiler instruments Rosette and tracks key performance metrics to identify potential issues. To use the symbolic profiler, run the command `raco symprofile program.rkt`. A new [performance][] chapter in the Rosette guide details common performance issues and how to use the symbolic profiler to identify them.
- Extended and generalized the interface to constraint solvers. The new interface allows the client code to specify a path to the solver, set the logic, provide solver-specific configuration options, and export the problem encodings sent to the solver.
- Added support for four new solvers: [Boolector][], [CVC4][], [Yices][], and [CPLEX][]. These solvers are not included in the default distribution and need to be installed separately for use with Rosette.

[performance]: https://docs.racket-lang.org/rosette-guide/ch_performance.html
[Boolector]: https://docs.racket-lang.org/rosette-guide/sec_solvers-and-solutions.html#%28def._%28%28lib._rosette%2Fsolver%2Fsmt%2Fboolector..rkt%29._boolector%29%29
[CVC4]: https://docs.racket-lang.org/rosette-guide/sec_solvers-and-solutions.html#%28def._%28%28lib._rosette%2Fsolver%2Fsmt%2Fcvc4..rkt%29._cvc4%29%29
[Yices]: https://docs.racket-lang.org/rosette-guide/sec_solvers-and-solutions.html#%28def._%28%28lib._rosette%2Fsolver%2Fsmt%2Fyices..rkt%29._yices%29%29
[CPLEX]: https://docs.racket-lang.org/rosette-guide/sec_solvers-and-solutions.html#%28def._%28%28lib._rosette%2Fsolver%2Fmip%2Fcplex..rkt%29._cplex%29%29
[Phitchaya Mangpo Phothilimthana]: https://github.com/mangpo

## Version 2.2

This release includes bug fixes and the following updates:

- Added support for quantified formulas.  Quantifiers can appear in assertions passed to `solve` and `verify` queries.  They should not be used with `synthesize` queries.  When using quantified formulas, `current-bitwidth` must be set to `#f`.
- Added the `unknown` solution type. An `unknown` solution is returned if the underlying solver cannot decide if a given set of constraints is (un)satisfiable.
- Added the `distinct?` predicate that returns true iff all of its arguments are pairwaise un-equal.  This has a direct (efficient) translation to Z3 if the arguments are primitive solvable values (booleans, integers, reals, or bitvectors).

## Version 2.1

This release includes the following updates to Rosette 2.0:

- Added support for the `push` / `pop` interface to Z3.
- Switched to log-based evaluation for Rosette documentation.  Documentation generation no longer depends on Z3.
- Improved the implementation of the lifted `struct` construct. The new implementation is a minimal patch to the corresponding Racket implementation, and it enables creation and use of `struct`s in the REPL.
- Improved the implementation of `#%top-interaction` to disallow mutation of top-level variables in the REPL.  This enables definition and use of recursive procedures in the REPL, as well as definition and use of generic interfaces.

## Version 2.0

This is a major release with significant changes to the language and
the symbolic evaluator.  Rosette 2.0 is *not backward compatible* with
Rosette 1.x.

This release includes the following features:

- New symbolic datatypes.

  - Replaced the `number?` type with `integer?` and `real?` types.
    These datatypes are translated to the theories of integers and
    reals if `current-bitwidth` is set to `#f`.  Otherwise, they are
    translated to bitvectors of length `(current-bitwidth)`.

   - Added the `bitvector?` datatype, which embeds the SMT theory of
     bitvectors into Rosette.

   - Added the `function?` datatype, which embeds uninterpreted
   functions into Rosette.

- New solver-aided queries.

  - Changed the behavior of solver-aided queries to no longer throw
  exceptions when a model is not found.  Instead they return an
  `unsat?` solution.

  - Changed the `solve` and `verify` queries to ensure that any
  solution obtained with finite-precision reasoning is correct under
  the aribitrary-precision semantics of integers and reals.

  - Added the `optimize` query, which exposes Z3's optimization
   features.

- Improved implementation for the `define-synthax` form and other
  high-level synthesis constructs.

- Improved printing of symbolic values by [James Bornholt][].

- Ported sample SDSLs to Rosette 2.0.

- Updated The Rosette Guide to document the new language in detail.

The following features have been removed:

- Support for Kodkod and CVC4 solvers.

- Support for the `enum` datatype.

- Support for internal logging via `current-log-handler`.

[James Bornholt]: https://github.com/jamesbornholt

## Version 1.1

- This release includes a new reader for `rosette` and `rosette/safe`
  implemented by [bmastenbrook](https://github.com/bmastenbrook).

- It also includes a fix for a bug in the evaluation of symbolic
  boxes.  Thanks to Alan Borning for reporting it.

## Version 1.0

- This is the initial release of the Rosette language and Symbolic
  Virtual Machine, as described in [PLDI'14][1] and [Onward13][2].

- It includes two symbolic datatypes: `boolean?` and `number?`.
  Assertions over numbers are translated to the theory of bitvectors.

- Rosette 1.0 supports the Kodkod, Z3, and CVC4 solvers.

- This release also includes the source code for three solver-aided
  DSLs: WebSynth (web scraping by demonstration), IFC (verification
  for secure stack machine semantics), and SynthCL (synthesis and
  verification for an Open-CL imperative language).


[1]: http://dl.acm.org/citation.cfm?id=2594340
[2]: http://dl.acm.org/citation.cfm?id=2509586

