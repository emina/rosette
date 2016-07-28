# Release Notes

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
  
- Improved printing of symbolic values by [James Bornholt](https://homes.cs.washington.edu/~bornholt/).

- Ported sample SDSLs to Rosette 2.0.

- Updated The Rosette Guide to document the new language in detail.

The following features have been removed:

- Support for Kodkod and CVC4 solvers.

- Support for the `enum` datatype.

- Support for internal logging via `current-log-handler`.

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

