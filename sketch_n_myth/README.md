# Sketch-n-Myth

## Building and Running

***Important note:***
Sketch-n-Myth is built with OCaml v4.08.1. When first working with the
Sketch-n-Myth codebase, please run `make deps` to download all the necessary
Opam dependencies.

***To build and run the server:***
From the root directory (where this `README` is), run `make serve`.

***To build without running:***
From the root directory (where this `README` is), run `make build`.

## Module Conventions

Utility/helper modules end with the character "2" so as to be consistent with
one another as well as distinct from the OCaml standard library (e.g.
[`List2`](src/list2.mli) and [`Option2`](src/option2.mli)).

The module [`Stdlib2`](lib/stdlib2/stdlib2.mli) found in the
[`lib/stdlib2/`](lib/stdlib2/) directory contains a small set of core utilities
and is automatically `open`ed in every file.

All other source code can be found in the [`src`](src/) directory.

## Index

| Concept                                     | File (in [`src/`](src/))
| ------------------------------------------- | ------------------------------
| Syntax of Core Sketch-n-Myth                | [`lang.ml`](src/lang.ml)
| Result classification                       | [`res.mli`](src/res.mli)/[`res.ml`](src/res.ml)
| Type checking                               | In Elm codebase
| Type equality                               | [`type.mli`](src/type.mli)/[`type.ml`](src/type.ml)
| Evaluation                                  | [`eval.mli`](src/eval.mli)/[`eval.ml`](src/eval.ml)
| Resumption                                  | [`eval.mli`](src/eval.mli)/[`eval.ml`](src/eval.ml)
| Example syntax                              | [`lang.ml`](src/lang.ml)
| Result/value/example coercion               | [`res.mli`](src/res.mli)/[`res.ml`](src/res.ml)
| Example (world) satisfication               | Metatheory only
| Constraint syntax                           | [`lang.ml`](src/lang.ml)
| Constraint merging                          | [`constraints.mli`](src/constraints.mli)/[`constraints.ml`](src/constraints.ml)
| Live bidirectional example satisfaction     | [`uneval.mli`](src/uneval.mli)/[`uneval.ml`](src/uneval.ml)
| Example unevaluation                        | [`uneval.mli`](src/uneval.mli)/[`uneval.ml`](src/uneval.ml)
| Result consistency                          | [`res.mli`](src/res.mli)/[`res.ml`](src/res.ml)
| Assertion satisfaction and simplification   | [`uneval.mli`](src/uneval.mli)/[`uneval.ml`](src/uneval.ml)
| Constraint solving                          | [`solve.mli`](src/solve.mli)/[`solve.ml`](src/solve.ml)
| Hole filling                                | [`fill.mli`](src/fill.mli)/[`fill.ml`](src/fill.ml)
| Type-and-example-directed refinement        | [`refine.mli`](src/refine.mli)/[`refine.ml`](src/refine.ml)
| Type-and-example-directed branching         | [`branch.mli`](src/branch.mli)/[`branch.ml`](src/branch.ml)
| Type-directed guessing (term generation)    | [`term_gen.mli`](src/term_gen.mli)/[`term_gen.ml`](src/term_gen.ml)
