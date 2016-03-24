# Release Notes

## Version 2.0

This is a major release with significant features to the language and
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

