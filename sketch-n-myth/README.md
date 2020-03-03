# Smyth

## Building and Running

***Important note:***
Smyth is built with OCaml v4.08.1. When first working with the Smyth codebase,
please run `make deps` to download all the necessary opam dependencies.

For best performance, please use an installation of OCaml that supports
[flambda](https://caml.inria.fr/pub/docs/manual-ocaml/flambda.html)
(e.g. `4.08.1+flambda` obtained via `opam switch create 4.08.1+flambda`).

***To build and run the server:***
From the root directory of the project (where this `README` is), run `make
serve`.
The server (which is implemented in Python in `serve.py` and calls the
Smyth executable) will be hosted on port 9090.

***To build without running:***
From the root directory of the project (where this `README` is), run `make
build`.  This command creates the Smyth executable in
`_build/default/src/main.exe`, which is accessible via the `smyth` symlink in
the root directory of the project.

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
| Syntax of Core Smyth                        | [`lang.ml`](src/lang.ml)
| Result classification                       | [`res.mli`](src/res.mli)/[`res.ml`](src/res.ml)
| Type checking                               | In Elm codebase
| Type equality                               | [`type.mli`](src/type.mli)/[`type.ml`](src/type.ml)
| Evaluation                                  | [`eval.mli`](src/eval.mli)/[`eval.ml`](src/eval.ml)
| Resumption                                  | [`eval.mli`](src/eval.mli)/[`eval.ml`](src/eval.ml)
| Example syntax                              | [`lang.ml`](src/lang.ml)
| Result-value coercions                      | [`res.mli`](src/res.mli)/[`res.ml`](src/res.ml)
| Example-value coercions                     | [`example.mli`](src/example.mli)/[`example.ml`](src/example.ml)
| Example satisfaction                        | [`example.mli`](src/example.mli)/[`example.ml`](src/example.ml)
| Constraint syntax                           | [`lang.ml`](src/lang.ml)
| Constraint satisfaction                     | [`constraints.mli`](src/constraints.mli)/[`constraints.ml`](src/constraints.ml)
| Constraint merging                          | [`constraints.mli`](src/constraints.mli)/[`constraints.ml`](src/constraints.ml)
| Live bidirectional example checking         | [`uneval.mli`](src/uneval.mli)/[`uneval.ml`](src/uneval.ml)
| Example unevaluation                        | [`uneval.mli`](src/uneval.mli)/[`uneval.ml`](src/uneval.ml)
| Result consistency                          | [`res.mli`](src/res.mli)/[`res.ml`](src/res.ml)
| Assertion satisfaction and simplification   | [`uneval.mli`](src/uneval.mli)/[`uneval.ml`](src/uneval.ml)
| Constraint simplification                   | [`solve.mli`](src/solve.mli)/[`solve.ml`](src/solve.ml)
| Constraint solving                          | [`solve.mli`](src/solve.mli)/[`solve.ml`](src/solve.ml)
| Hole filling                                | [`fill.mli`](src/fill.mli)/[`fill.ml`](src/fill.ml)
| Type-and-example-directed refinement        | [`refine.mli`](src/refine.mli)/[`refine.ml`](src/refine.ml)
| Type-and-example-directed branching         | [`branch.mli`](src/branch.mli)/[`branch.ml`](src/branch.ml)
| Type-directed guessing (term generation)    | [`term_gen.mli`](src/term_gen.mli)/[`term_gen.ml`](src/term_gen.ml)
