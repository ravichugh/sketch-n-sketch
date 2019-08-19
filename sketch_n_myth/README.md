# Building and Running

*To build and run the server:* From the top-level directory, run `make serve`.
*To build without running:* From the top-level directory, run `make build`.

# Module Conventions

Utility/helper modules end with the character "2" so as to be consistent with
one another as well as distinct from the OCaml standard library (e.g.
[`List2`](src/list2.ml)).

The module [`Pervasives2`](lib/pervasives2/pervasives2.ml) found in the
[`lib/pervasives2/`](lib/pervasives2/) directory contains a small set of core
utilities and is automatically `open`ed in every file.

All other source code can be found in the [`src`](src/) directory.

# Index

| Concept                                     | File (in [`src/`](src/))
| ------------------------------------------- | ------------------------------
| Syntax of Core Sketch-n-Myth                | [`lang.ml`](src/lang.ml)
| Result classification                       | [`res.ml`](src/res.ml)
| Type checking                               | In Elm codebase
| Evaluation                                  | [`eval.ml`](src/eval.ml)
| Resumption                                  | [`eval.ml`](src/eval.ml)
| Example syntax                              | [`lang.ml`](src/lang.ml)
| Result/value/example coercion               | [`res.ml`](src/res.ml)
| Example (world) satisfication               | Metatheory only
| Constraint syntax                           | [`lang.ml`](src/lang.ml)
| Constraint merging                          | [`lang.ml`](src/constraints.ml)
| Live bidirectional example satisfaction     | [`uneval.ml`](src/uneval.ml)
| Example unevaluation                        | [`uneval.ml`](src/uneval.ml)
| Result consistency                          | [`res.ml`](src/res.ml)
| Assertion satisfaction and simplification   | [`uneval.ml`](src/uneval.ml)
| Constraint solving                          | ---
| Hole filling                                | ---
| Type-and-example-directed refinement        | ---
| Type-and-example-directed branching         | ---
| Type-directed guessing (term generation)    | [`term_gen.ml`](src/term_gen.ml)
