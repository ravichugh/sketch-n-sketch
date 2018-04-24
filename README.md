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
  = Direct Manipulation Programming for SVG/HTML Documents


## [Project Page][ProjectPage]

Check out the [main project page][ProjectPage] for more details
and to try out the latest release.


## Quick Syntax Reference

TODO explain Elm-like syntax... Pick Little/Leo as a name..

```
  program  ::=  x1 = e1; ...; xn = en; e
            |   x1 = e1; ...; xn = en     -- where xi = main for some i

  e  ::=
      |   n
      |   s
      |   \p -> e
      |   \p1 ... pn -> e
      |   e1 e2
      |   e1 <| e2                  -- infix application
      |   e2 |> e1                  -- infix reverse application
      |   e1 e2 e3 ... en
      |   opn e1 ... en
      |   L p = e1 in e2
      |   L x p1 ... pn = e1 in e2
      |   if e1 then e2 else e3
      |   case e of p1 -> e1; ...; pn -> en
      |   e1 :: e2
      |   []
      |   [e1, ..., en]
      |   (e1, ..., en)
      |   {f1 = e1; ...; fn = en}
      |   e.f
      |   TODO regular expressions
      |   TODO eval
      |   -- single-line-comment; e
      |   #option value e
      |   (e)

  L  ::=  let | letrec
```

<!-- no types for now... -->

## Syntax Guide

### Programs

A program is a series of top-level definitions followed by an expression.
If the final expression is omitted, it implicitly refers to the variable `main`.
By convention, the `main` definition is often the last top-level definition.

```
  program  ::=  x1 = e1; ...; xn = en; e
            |   x1 = e1; ...; main = e
```

### Constants

```
  e  ::=
      |   n         -- numbers (all are floating point)
      |   s         -- strings (use double- or single-quotes at the outermost level)
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
  b  ::=  True | False
```

```
  s  ::=  "hello" | 'world'
      |   "'hello world'"     -- quotation marks can be nested
      |   """S"""             -- long-string literals
```

TODO describe long-string literals S

### Primitive Operators

```
  e  ::=  ...
      |   op0
      |   op1 e1
      |   op2 e1 e2
```

```
  op0  ::=  pi
  op1  ::=  cos | sin | arccos | arcsin
        |   floor | ceiling | round
        |   toString
        |   sqrt
        |   explode             : String -> List String
        |   ...
  op2  ::=  + | - | * | /
        |   == | < | <= | ...
        |   mod | pow
        |   arctan2
        |   ...
```

TODO empty dictionary and dictionary operators

### Conditionals

```
  e  ::=  ...
      |   if e1 then e2 else e3
```

### Lists

```
  e  ::=  ...
      |   e1 :: e2
      |   []
      |   [e1, ..., en]          -- desugars to e1 :: e2 :: ... :: []
```

### Tuples

TODO

### Records

TODO

We sometimes say "module" to describe a records that begins with a capital
letter.

### Patterns

```
  p  ::=  x
      |   n | s | b
      |   p1 :: p2
      |   []
      |   [p1, ..., pn]
      |   (p1, ..., pn)
      |   x@p
      |   {f1 = p1; ..., fn = pn}
      |   {f1, ..., fn}              -- desugars to {f1 = f1, ..., fn = fn}
```

### Case Expressions

```
  e  ::=  ...
      |   case e of p1 -> e1; ...; pn -> en
      |   case of p1 -> e1; ...; pn -> en     -- desugars to \x -> case x of p1 -> e1; ...; pn -> en
```

### Functions

```
  e  ::=  ...
      |   \p -> e
      |   \p1 ... pn -> e     -- desugars to \p1 -> \p2 -> ... -> \pn -> e
```

### Function Application

```
  e  ::=  ...
      |   e1 e2
      |   e1 <| e2                  -- infix application
      |   e2 |> e1                  -- infix reverse application
      |   e1 e2 e3 ... en           -- desugars to ((((e1 e2) e3) ...) en)
```

### Let-Bindings

```
  L  ::=  let | letrec
  e  ::=  ...
      |   L p = e1 in e2
      |   L f p1 ... pn = e1 in e2   -- desugars to L f = \p1 ... pn -> e1 in e2
```

### Comments and Options

```
  e  ::=  ...
      |   --single-line-comment; e
      |   --# option: value; e
```

Comments and options are terminated by newlines.
All options should appear at the top of the program, before the first
non-comment expression.

### Standard Prelude

See [`preludeLeo.elm`][Prelude] for the standard library included by every program.

### SVG

The result of a `little` program should be an "HTML node."
Nodes are either text elements or SVG elements, represented as

```
  h  ::=  ["TEXT", e]
      |   [shapeKind, attrs, children]
```

where

```
  shapeKind  ::=  "svg" | "circle" | "rect" | "polygon" | "text" | ...
  attrs      ::=  [ ["attr1", e1], ..., ["attrn", e2] ]
  children   ::=  [ h1, ..., hn ]
```

Each attribute expression should compute a pair value
in one of the following forms

```
  [ "fill"          , colorValue     ]
  [ "stroke"        , colorValue     ]
  [ "stroke-width"  , numValue       ]
  [ "points"        , pointsValue    ]
  [ "d"             , pathValue      ]
  [ "transform"     , transformValue ]
  [ anyStringValue  , anyStringValue ]   -- thin wrapper over full SVG format
```

where

```
  colorValue      ::=  n                   -- color number [0, 500)
                   |   [n, n]              -- color number and transparency
                   |   [n, n, n, n]        -- RGBA

  pointsValue     ::=  [[nx_1, ny_1], ... ]     -- list of points

  pathValue       ::=  pcmd_1 ++ ... ++ pcmd_n  -- list of path commands

  transformValue  ::=  [ tcmd_1, ..., tcmd_n ]  -- list of transform commands

  pcmd            ::=  [ "Z" ]                            -- close path
                   |   [ "M", n1, n2, n3 ]                -- move-to
                   |   [ "L", n1, n2, n3 ]                -- line-to
                   |   [ "Q", n1, n2, n3, n4 ]            -- quadratic Bezier
                   |   [ "C", n1, n2, n3, n4, n5, n6 ]    -- cubic Bezier
                   |   [ "H", n1 ]
                   |   [ "V", n1 ]
                   |   [ "T", n1, n2, n3 ]
                   |   [ "S", n1, n2, n3, n4 ]
                   |   [ "A", n1, n2, n3, n4, n5, n6, n7 ]

  tcmd            ::=  [ "rotate", nAngle, nx, ny ]
                   |   [ "scale", n1, n2 ]
                   |   [ "translate", n1, n2 ]
```

See [this][SvgPath] and [this][SvgTransform] for more information
about SVG paths and transforms. Notice that `pathValue` is a flat list,
whereas `transformValue` is a list of lists.

See [`preludeLeo.elm`][Prelude] for a small library of SVG-manipulating functions.

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

Note: If the packages are not installed properly, you might see a message like TYPE MISMATCH and
that you are expecting `Maybe (Dict a ( b, c ))` but a value is `Maybe (Dict comparable ( a, b ))`
If that is the case, look at the `makefile`, the last two commands of `elm-stuff/packages` may not
have been executed properly. This can happen if another software tries to mix with packages installation,
such as Dropbox.

Welcome to the sketch-n-sketch wiki!


### Steps to recompile Elm in Windows

Make sure to run these commands with administrator rights.

- First install Haskell platform 7.10.3 https://www.haskell.org/platform/prior.html
- Go to a fresh cloned version of https://github.com/elm-lang/elm-platform.git
- Go to the folder `installers`
- Run
  `runhaskell BuildFromSource.hs 0.18`     (note the 0.18)
- Go to the (newly created) folder `installers\Elm-Platform\elm-compiler`
- Use Brian’s branch for elm-compile: https://github.com/brianhempel/elm-compiler/tree/faster_exhaustiveness_checker_0.18  
  For that you can execute the following command:  
  `git remote add brian https://github.com/brianhempel/elm-compiler.git`  
  `git fetch brian`  
  `git checkout faster_exhaustiveness_checker_0.18`  

- Comment out line 188 in the file `installers\BuildFromSource.hs` which should look like 
  `--      mapM_ (uncurry (makeRepo root)) repos`
- Re-run the install script again in `installers\`  
  `runhaskell BuildFromSource.hs 0.18`
- It will throw some fatal errors but that’s fine.
- Last step: copy elm-make.exe from `installers\Elm-Platform\0.18\elm-make\dist\dist-sandbox-6fb8af3\build\elm-make` to replace the `elm-make.exe` of a fresh 0.18 Elm installation.


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
5. `$ make run_server`

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

[Prelude]: https://github.com/ravichugh/sketch-n-sketch/blob/master/examples/preludeLeo.elm
[LangSvg]: https://github.com/ravichugh/sketch-n-sketch/blob/master/src/LangSvg.elm
[ProjectPage]: http://ravichugh.github.io/sketch-n-sketch
[SvgPath]: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
[SvgTransform]: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform


