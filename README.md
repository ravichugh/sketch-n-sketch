<!--
<img src="img/sketch-n-sketch-logo.png"
     align="right" style="padding: 0px;" height="40px" />
-->

# Sketch-n-Sketch

Direct manipulation interfaces are useful in many domains, but the lack of
programmability in a high-level language makes it difficult to develop complex
and reusable content. We envision *direct manipulation programming systems* that allow
users to freely mix between programmatic and direct manipulation.

<!-- TODO widths on GitHub -->

<span style="display: inline-block; width: 222px; text-align: right;">**Direct Manipulation Programming**</span>
  = Programmatic + Direct Manipulation <br/>
<span style="display: inline-block; width: 222px; text-align: right;">**Sketch-n-Sketch**</span>
  = Direct Manipulation Programming for SVG


## [Project Page][ProjectPage]

Check out the [main project page][ProjectPage] for more details
and to try out the latest release.


## Quick Syntax Reference

```
  e  ::=
      |   n
      |   s
      |   (\p e)
      |   (\(p1 ... pn) e)
      |   (e1 e2)
      |   (e1 e2 e3 ... en)
      |   (let p e1 e2)
      |   (letrec p e1 e2)
      |   (def p e1) e2
      |   (def p T) e
      |   (defrec p e1) e2
      |   (if e1 e2 e3)
      |   (case e (p1 e1) ... (pn en))
      |   (typecase p (t1 e1) ... (tn en))
      |   []
      |   [e1 | e2]
      |   [e1 .... en]
      |   [e1 .... en | erest]
      |   (op0)
      |   (op1 e1)
      |   (op2 e1 e2)
      |   ;single-line-comment e
      |   #option value e
      |   (typ p T)
      |   (e : T)

  T  ::=
      |   Num | Bool | String
      |   TypeAliasName
      |   (-> T1 ... Tn)
      |   (List T)
      |   [T1 .... Tn]
      |   [T1 .... Tn | Trest]
      |   (forall a T) | (forall (a1 ... an) T)
      |   a | b | c | ...
      |   _
```

Extra parentheses are not permitted.
(Don't you think there are enough already?)

## Syntax Guide

### Constants

```
  e  ::=
      |   n         -- numbers (all are floating point)
      |   s         -- strings (use single-quotes, not double)
      |   b         -- booleans
```

```
  n  ::=  123
      |   3.14
      |   -3.14

      |   3.14!     -- frozen constants (may not be changed by sync)
      |   3.14?     -- thawed constants (may be changed by sync)
      |   3.14~     -- assign to at most one zone

      |   3{0-6}          -- auto-generate an integer slider
      |   3.14{0.0-6.28}  -- auto-generate a numeric slider
```

```
  s  ::=  'hello' | "world" | '"hello world"' |...
```

```
  b  ::=  true | false
```

### Primitive Operators

```
  e  ::=  ...
      |   (op0)
      |   (op1 e1)
      |   (op2 e1 e2)
```

```
  op0  ::=  pi
  op1  ::=  cos | sin | arccos | arcsin
        |   floor | ceiling | round
        |   toString
        |   sqrt
        |   explode             : String -> List String
  op2  ::=  + | - | * | /
        |   < | =
        |   mod | pow
        |   arctan2
```

### Conditionals

```
  e  ::=  ...
      |   (if e1 e2 e3)
```

### Lists

```
  e  ::=  ...
      |   []
      |   [e1 | e2]
      |   [e1 .... en]           -- desugars to [e1 | [e2 | ... | [en | []]]]
      |   [e1 .... en | erest]   -- desugars to [e1 | [e2 | ... | [en | erest]]]
```

### Patterns

```
  p  ::=  x
      |   n | s | b
      |   [p1 | p2]
      |   [p1 ... pn]
      |   [p1 ... pn | prest]
      |   x@p
```

### Case Expressions

```
  e  ::=  ...
      |   (case e (p1 e1) ... (pn en))
```

### Functions

```
  e  ::=  ...
      |   (\p e)
      |   (\(p1 ... pn) e)    -- desugars to (\p1 (\p2 (... (\pn) e)))
```


### Function Application

```
  e  ::=  ...
      |   (e1 e2)
      |   (e1 e2 e3 ... en)   -- desugars to ((((e1 e2) e3) ...) en)
```

### Let-Bindings

```
  e  ::=  ...
      |   (let p e1 e2)
      |   (letrec f (\x e1) e2)
      |   (def p e1) e2           -- desugars to (let p e1 e2)
      |   (defrec f (\x e1)) e2   -- desugars to (letrec f (\x e1) e2)
```

### Comments and Options

```
  e  ::=  ...
      |   ;single-line-comment e
      |   #option value e
```

Comments and options are terminated by newlines.
All options should appear at the top of the program, before the first
non-comment expression.

### Standard Prelude

See [`prelude.little`][Prelude] for the standard library included by every program.

### SVG

The result of a `little` program should be an "HTML node."
Nodes are either text elements or SVG elements, represented as

```
  h  ::=  ['TEXT' e]
      |   [shapeKind attrs children]
```

where

```
  shapeKind  ::=  'svg' | 'circle' | 'rect' | 'polygon' | 'text' | ...
  attrs      ::=  [ ['attr1' e1] ... ['attrn' e2] ]
  children   ::=  [ h1 ... hn ]
```

Each attribute expression should compute a pair value
in one of the following forms

```
  [ 'fill'          colorValue     ]
  [ 'stroke'        colorValue     ]
  [ 'stroke-width'  numValue       ]
  [ 'points'        pointsValue    ]
  [ 'd'             pathValue      ]
  [ 'transform'     transformValue ]
  [ anyStringValue  anyStringValue ]   -- thin wrapper over full SVG format
```

where

```
  colorValue      ::=  n                   -- color number [0, 500)
                   |   [n n]               -- color number and transparency
                   |   [n n n n]           -- RGBA

  pointsValue     ::=  [[nx_1 ny_1] ... ]       -- list of points

  pathValue       ::=  pcmd_1 ++ ... ++ pcmd_n  -- list of path commands

  transformValue  ::=  [ tcmd_1 ... tcmd_n ]    -- list of transform commands

  pcmd            ::=  [ 'Z' ]                      -- close path
                   |   [ 'M' n1 n2 n3 ]             -- move-to
                   |   [ 'L' n1 n2 n3 ]             -- line-to
                   |   [ 'Q' n1 n2 n3 n4 ]          -- quadratic Bezier
                   |   [ 'C' n1 n2 n3 n4 n5 n6 ]    -- cubic Bezier
                   |   [ 'H' n1 ]
                   |   [ 'V' n1 ]
                   |   [ 'T' n1 n2 n3 ]
                   |   [ 'S' n1 n2 n3 n4 ]
                   |   [ 'A' n1 n2 n3 n4 n5 n6 n7 ]

  tcmd            ::=  [ 'rotate' nAngle nx ny ]
                   |   [ 'scale' n1 n2 ]
                   |   [ 'translate' n1 n2 ]
```

See [this][SvgPath] and [this][SvgTransform] for more information
about SVG paths and transforms. Notice that `pathValue` is a flat list,
whereas `transformValue` is a list of lists.

See [`prelude.little`][Prelude] for a small library of SVG-manipulating functions.

The [Prelude][Prelude], the examples that come with the editor,
the [Tutorial](http://ravichugh.github.io/sketch-n-sketch/tutorial/index.html),
and the Appendix of [this technical report](http://arxiv.org/pdf/1507.02988v3.pdf)
provide more details about the above Little encodings of different SVG attributes.
You can also peek at the `valToAttr` function in [`LangSvg.elm`][LangSvg].


## Building the Project

In the following, `SKETCH-N-SKETCH` stands for the directory
of the project clone.

1. Install [Elm v0.18](http://elm-lang.org/)
2. `cd SKETCH-N-SKETCH/src`
3. `make clean`
4. Launch `SKETCH-N-SKETCH/build/out/index.html`

Note: The parser has a performance issue that we have not yet addressed.
If the application runs out of stack space,
[try this](https://github.com/ravichugh/sketch-n-sketch/issues/84).


## Little "REPL"

```
 % elm-repl
Elm REPL 0.4 (Elm Platform 0.15)
...
> import Eval exposing (parseAndRun)
> parseAndRun "(+ 'hello ' 'world')"
"'hello world'" : String
> parseAndRun "(list0N 10)"
"[0 1 2 3 4 5 6 7 8 9 10]" : String
```


## Adding Examples

To add a new example to the New menu:

1. Create a file `examples/newExample.little` for your `newExample`.

2. In `ExamplesTemplate.elm`, add the lines:

   * `LITTLE_TO_ELM newExample`
   * `, makeExample "New Example Name" newExample`

3. From the `src/` directory, run `make examples`.

4. Launch Sketch-n-Sketch.

## Solver Server

For solving complicated formulae or multi-equation systems, Sketch-n-Sketch relies on an external computer algebra system ([REDUCE](http://www.reduce-algebra.com/)). A solver server exposes REDUCE over the Websockets protocol.

To use Sketch-n-Sketch locally, you do not need to run the solver server—it will try to connect to our public solver server.

However, if you want to run the solver server locally:

1. Download [websocketd](http://websocketd.com/), e.g. with `$ brew install websocketd`
2. Make sure you have any version of [Ruby](https://www.ruby-lang.org/) installed, check with e.g. `$ ruby --version`
3. `$ cd solver_server`
4. `$ make test_reduce` This will download, build, and test REDUCE.
5. `$ make run-server`

If a local server is running, Sketch-n-Sketch will try to connect to it first.


## Running Tests

If you hack on Sketch-n-Sketch, there are some tests to run. Writing more tests is, of course, encouraged.

Run once:

```
$ ./tests/test.sh
```

Run when files change (requires [fswatch](https://emcrisostomo.github.io/fswatch/)):

```
$ ./watchtest
```

To run only tests with a certain string in their name, set `SNS_TESTS_FILTER`:

```
$ SNS_TESTS_FILTER=unparser ./watchtest
```

To write a new test, make a function of type `() -> String` named `somethingTest` in a `tests/myTests.elm` file. If the function returns the string `"ok"` it is considered a passing test, otherwise the returned string will be displayed as a failure message.

You can also return a list of test functions, `() -> List (() -> String)`, and each test will be run individually.

See [existing tests](https://github.com/ravichugh/sketch-n-sketch/tree/master/tests) for examples.

### Adding/Removing Elm Packages

If you add or remove a package from the project, the package list for the tests needs to be updated as well. Simply run `node tests/refresh_elm-packages.js` to copy over the main `elm-packages.json` into the tests directory.

[Prelude]: https://github.com/ravichugh/sketch-n-sketch/blob/master/examples/prelude.little
[LangSvg]: https://github.com/ravichugh/sketch-n-sketch/blob/master/src/LangSvg.elm
[ProjectPage]: http://ravichugh.github.io/sketch-n-sketch
[SvgPath]: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
[SvgTransform]: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform

