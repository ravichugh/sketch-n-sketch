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

We support a (almost) superset of Elm Syntax.

```
  program  ::=  x1 = e1; ...; xn = en; e
            |   x1 = e1; ...; xn = en     -- where xi = main for some i

  e  ::=
      |   constant
      |   variable
      |   \p -> e
      |   \p1 ... pn -> e
      |   e1 e2
      |   e1 <| e2                  -- infix application
      |   e2 |> e1                  -- infix reverse application
      |   e1 e2 e3 ... en
      |   opn e1 ... en
      |   Let p = e1 in e2
      |   Let x p1 ... pn = e1 in e2
      |   if e1 then e2 else e3
      |   case e of p1 -> e1; ...; pn -> en
      |   e1 :: e2
      |   []
      |   [e1, ..., en]
      |   (e1, ..., en)
      |   {f1 = e1; ...; fn = en}
      |   e.f
      |   #option:value
          e
      |   (e)
      |   ()

  p  ::= constant
       | variable
       | p as variable
       | [p1, ..., pn]
       | (p1, ..., pn)
       | p1 :: pn
       | {f1 = p1, ... fn = pn}
  opn ::= 
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

#### Long string literals with interpolation

    """Here @y @x.hello @(let y = "2" in y)
    @let t = "third" in
    This is on a @t line"""

is roughly equivalent to `"Here " + S y + " " + S x.hello +
 " " + S (let y = "2" in y) + "\n" + (let t = "third" in "This is on a " + t + " line"`
where S converts its argument to a string if it is not.
Note that inline @let always require the space after "in" to be a newline.

### Primitive Operators

```
  e  ::=  ...
      |   op0
      |   op1 e1
      |   op2 e1 e2
      |   e1 opi e2
      |   (op)
      |   (op,...,op)
```

```
  op0  ::=  pi
  op1  ::=  cos | sin | arccos | arcsin
        |   floor | ceiling | round
        |   toString
        |   sqrt
        |   explode             : String -> List String

  op2  ::=  mod | pow
        |   arctan2

  opi  ::=  + | - | * | /
        |   == | < | <= | > | >= | /=
        |   && | `||` | `|>` | `<|` | << | >>
        
  e  ::= ...
       | __DictEmpty__ e
       | __DictFromList__ e
       | __DictInsert__ eK eV eD
       | __DictRemove__ eK eD
       | __DictGet__ eK eD
```

For your convenience, the prelude defines a `Dict` record exposing `empty`, `fromList`, `member`, `contains`, `remove`, `get`` and `apply`.

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

```
 e  ::=   ...
      |   ()
      |   (e1, ... en)
      |   (,...,)               -- desugars to \e1 ... en -> (e1,..., en)
```

### Records


```
 e  ::=   ...
      |   { f1 = e1, ...., fn = en}
      |   e.f1
      |   .f1.f2(...).fn              -- desugars to \x -> x.f1.f2(...).fn

```

Note that `f1 a b = e` in a key/value definition of a record is equivalent to `f1 = \a b -> e`.
The comma is optional for defining a new key/value pair if
1) there are two newlines before
or 2) there is one newline before and the column of the key is at most the column of the key before it.
We sometimes say "module" to describe a record that begins with a capital
letter.

### Patterns

```
  p  ::=  x
      |   n | s | b
      |   p1 :: p2
      |   p as x
      |   []
      |   [p1, ..., pn]
      |   (p1, ..., pn)
      |   {f1 = p1; ..., fn = pn}
      |   {f1, ..., fn}              -- desugars to {f1 = f1, ..., fn = fn}
```

### Case Expressions

```
  e  ::=  ...
      |   case e of p1 -> e1; ...; pn -> en
      |   case of p1 -> e1; ...; pn -> en     -- desugars to \x -> case x of p1 -> e1; ...; pn -> en
```

The semicolon is optional for a branch if 1) there is a newline 2) the column of the start of this branch matches the column of the start of the first branch.

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
  e  ::=  ...
      |   let p1 = e1, p2 = e2 in e2
      |   let f p1 ... pn = e1 in e2   -- desugars to L f = \p1 ... pn -> e1 in e2
```

### Built-in functions
```
__evaluate__ environment string
```
Evaluates the program present in the string under the given environment, which is a list of (string, value).
It returns either `Ok result` or `Err String` an error message.
This function is reversible. You can even push back corrections to the line of the program included in the error message.
For the environment, you can use the meta-variable `__CurrentEnv__` (without arguments) that returns the current environment.
In the prelude, the function `evaluate` uses the empty environment and directly returns the result, or raises an error.

```
{ apply = f, update = g}.apply X
Update.lens { apply = f, update = g } x
```
On evaluation, it returns the result of computing `f x`.
On evaluation update, given a new output value `v'`, it computes `g { input = x, outputOld = v, outputNew = v' }`` which should return either a
`Ok (Inputs [x1, x2...])`, `Ok (InputsWithDiffs [(x1, Just diff1), (x2, Just diff2)])`  or `Err "error_message"`.
If the former, propagates the new value `x1` to the expression `X` with differences `diff1`,
then on a second round the value `x2`  to the expression `X` with differences `diff2`.
Note that the second version is a wrapper around the first, but can be used to actually _build lenses_.
`Update.lens2` helps to build 2-arguments lenses, and so-on.


```
__updateApp__ {fun=FUNCTION,input=ARGUMENT,output=NEWOUTPUT[,oldOutput=OLD OUTPUT][,outputDiff=OUTPUT DIFF]}
```
Takes a single record defining `fun` (a function), an `input` and an new `output`. For performance, we can also provide the old output `oldOutput` and the output difference `outputDiff`.
This solves the evaluation problem `fun input <-- output`.
Returns `{ values = [x1, x2...]}`, `{ values = [x1, x2...], diffs = [Just diff1, Just diff2 ...]}`  or `{error = "error_message"}` (same as lenses for chaining)
The values `x1, x2, ...` are the possible new input. Solutions that modify `fun` are discarded.

You can still update both function and argument by the following trick:
`__updateApp__ {fun (f,a) = f a, input = (FUNCTION, ARGUMENT), output = NEWOUTPUT}`

If necessary, `outputOld` and `oldOut` are all synonyms of `oldOutput`.
`diffOutput`, `diffOut` and `outDiff` are all synonyms of `outputDiff`.

```
__merge__ original (listModifiedDiff)
```
It takes an original value (especially useful for functions and lists) and a list of (modified value, `Maybe Diff`) where each value is associated to a (possible) difference. To compute such differences, use `__diff__`.
Returns the merge of all modifications. Does not prompt on ambiguity.

```
__diff__ original modified
```
Computes a `Result String (Maybe Diff)` (differences) of the differences between the original and the modified value.
If you are sure there is no error, you can convert this result to a single `Maybe Diff` by using the function `.args._1`

```
join__ (list of strings)
```
Performs a reversible join between strings, deleting strings from the list if necessary.

```
__mbwraphtmlnode__
```
Wraps a String, an Int or an HTML Element into a list of HTML Elements.
Idempotent on any other values.
This function is internally used by the HTML interpolation for children. You should not need it.

```
__mbstylesplit__
```
Reversibly explodes a string representing a element's `style` into a list of key/values.
Idempotent on any other values.
This function is internally used by the attribute interpolation for children. You should not need it.

```
error string
```
As its name indicates, stops the execution by raising the given error as a string. Cannot be recovered. Used by Debug.crash

```
getCurrentTime ()
```
Return the current time in milliseconds.
This function is not reversible.

```
toggleGlobalBool ()
```
Flips a global boolean and returns it.
This function is not reversible.

```
__jsEval__ string
```
Evaluates arbitrary JavaScript using JavaScript's eval. Converts back the value to an interpretable value in our language, i.e. integers to integers, strings to strings, records to records, arrays to list.
Can be useful to execute a program with user-defined values (e.g. `let username = __jsEval__ "document.getElementById('username')" in ...`).
This function is not reversible -- use it mostly for non-visible control flow (settings, appearance, language...)
 or in the `apply` or `update` field of a lens`.

#### Prevent evaluation update changes (freeze)

All the freeze expressions behave like the identity function in the direction of evaluation.
In the direction of update, they each have different ways to prevent changes to be back-propagated to the program.

```
freeze exp
Update.freeze exp
```
Prevents any changes to be pushed back to the expression (changes to the values of variables and structural changes).

```
expressionFreeze exp
Update.expressionFreeze exp
```
Prevents any structural changes to be made to the expression (but allows changes to the values of variables)

```
Update.sizeFreeze [expressionc computing a list...]
```
Prevents any insertions and deletions to be made to the given list, but lets through changes to individual elements themselves.

```
Update.softFreeze exp
```
Does not prevent changes to the output of the computation, just ignore them.
This is useful for value that should never be updated but should not prevent the update to succeed.

```
String.update.freezeRight [expressionc computing a string]
String.update.freezeLeft [expressionc computing a string]
```
Blocks any changes to a string is not an insertion to the left (resp. right) of it.


### `transient` and `ignore` elements/attributes

If an attribute's name starts with `transient`, the update algorithm will treat it like it does not even exists. Same for elements whose tagName is `transient`.
If an attribute's name starts with `ignore`, the update algorithm will not propagate changes made to it.

Therefore, the code should not produce `transient` elements or attributes, but they should be created either by third-party tools (e.g. toolbar) or scripts inside the generated document. `ignore` attributes not be created from scripts but be already defined in the code.

If needed in the future, we could add other elements (e.g. `ignore`) or attributes decribing if the element or some attributes are transient or ignorable.

### Comments and Options

Comments are part of whitespace and can be one-line `-- Comment` or nested multi-line `{- This is {-a-} comment -}`.
Options are one-line comments immediately starting with "#", then a keyword, then a value.

```
  e  ::=  ...
      |   --single-line-comment; e
      |   --# option: value; e
```

Options are separated by expressions by a newline (the semicolon above is actually a newline).

### Html literals with interpolation

Most HTML and SVG is valid in our language, except for comments (Elm would not allow use to define them) and parameters without quotes (they are considered as variables).

```
  e ::= node
  
  node  ::= <ident attributes>child*</ident>
          | <ident>                              -- if the indent is a void element (br, img, ...)
          | <ident attributes/>                  -- if the element is auto-closing (svg tags only)
          | <@e attributes>child*</@>
  
  attributes ::= ident1=e1 ... identn=en         -- expressions must not contain spaces.
               | attributes @e attributes        -- in this case, e should return a list of attributes, i.e. [["class", "d"]]
  
  child ~=  @@                                   -- for the @ symbol
          | @i                                   -- inserts a node, a list of nodes, a string or a number.
          | innerHTML text
          | node
  i ::= variable {.identifier}* { (e) | tuple | record | list}* [ '<|'   v | i ]
          -- i is parsed without in-between spaces.
     |  (e)                                      -- If you use top-level parentheses @(e), nothing will be parsed after ')'

```

 Some samples of what kind of interpolation is possible:

| Html syntax                         | Code equivalent |
| ----------------------------------- | --------------- |
| `<h1 id=x>Hello</h1>`               | `["h1", [["id", x]], [["TEXT", "Hello"]]]` |
| `<h1 id=x @attrs>Hello @world</h1>` | `["h1", [["id", x]] ++ attrs, [["TEXT", "Hello "]] ++ world]`   |
| `<@(t)>Hi</@>`                      | `[t, [], [["TEXT", "Hi"]]]`  |
| <code>let b t x = &lt;b title=t&gt;@x &lt;/b&gt; in &lt;div&gt;@b("h")&lt;&vert;&lt;span&gt;&lt;/span&gt;&lt;/div&gt;</code> | `let b x = ["b",[],[x]] in ["div", [], [b ("h") <| ["span", [], []]]]`  |


Note that style attributes support both syntax for their values (array of key/values arrays, and strings).
In the innerHTML of a tag, you can interpolate string, integers, nodes and list of nodes (there is an automatic conversion).

## Standard Prelude

See [`preludeLeo.elm`][Prelude] for the standard library included by every program.

### SVG

The result of a program should be an "HTML node."
Nodes are either text elements, HTML nodes or SVG nodes, represented as

```
  h  ::=  ["TEXT", e]
      |   [tagName, attrs, children]
```

where

```
  tagName  ::=  "div" | "span" | "script" .... | "svg" | "circle" | "rect" | "polygon" | "text" | ...
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
3. `make`
4. Launch `SKETCH-N-SKETCH/build/out/index.html`

Note: The parser has a performance issue that we have not yet addressed.
If the application runs out of stack space,
[try this](https://github.com/ravichugh/sketch-n-sketch/issues/84).

Note: If the packages are not installed properly, you might see a message like TYPE MISMATCH and
that you are expecting `Maybe (Dict a ( b, c ))` but a value is `Maybe (Dict comparable ( a, b ))`
If that is the case, look at the `makefile`, the last two commands of `elm-stuff/packages` may not
have been executed properly. This can happen if another software tries to mix with packages installation,
such as Dropbox.

### Steps to install Elm in Arch Linux

There is a package [`elm-platform-bin`](https://aur.archlinux.org/packages/elm-platform-bin) on AUR, but at the moment it will
install version 0.19.1 instead.

In order to install Elm 0.18.0 do the following:

```bash
git clone https://aur.archlinux.org/elm-platform-bin.git
cd elm-platform-bin
git checkout 1fa002d66ca484b07b4b37656684d9e2630c4a26
makepkg -si
```

This alone will not work, because the [source](https://dl.bintray.com/elmlang/elm-platform/0.18.0/linux-x64.tar.gz) gives an error now.
Nevertheless, [Web Archive link](https://web.archive.org/web/20210502114042/https://dl.bintray.com/elmlang/elm-platform/0.18.0/linux-x64.tar.gz) redirects to [another site on Cloudfront](https://web.archive.org/web/20210502114043if_/https://d29vzk4ow07wi7.cloudfront.net/6885046e3c17517c080e02d121e8c0b8298362b256d827adcb46e9cb7138d71e?response-content-disposition=attachment%3Bfilename%3D%22linux-x64.tar.gz%22&Policy=eyJTdGF0ZW1lbnQiOiBbeyJSZXNvdXJjZSI6Imh0dHAqOi8vZDI5dnprNG93MDd3aTcuY2xvdWRmcm9udC5uZXQvNjg4NTA0NmUzYzE3NTE3YzA4MGUwMmQxMjFlOGMwYjgyOTgzNjJiMjU2ZDgyN2FkY2I0NmU5Y2I3MTM4ZDcxZT9yZXNwb25zZS1jb250ZW50LWRpc3Bvc2l0aW9uPWF0dGFjaG1lbnQlM0JmaWxlbmFtZSUzRCUyMmxpbnV4LXg2NC50YXIuZ3olMjIiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIjp7IkFXUzpFcG9jaFRpbWUiOjE2MTk5NTYzNjJ9LCJJcEFkZHJlc3MiOnsiQVdTOlNvdXJjZUlwIjoiMC4wLjAuMC8wIn19fV19&Signature=FGbS4yf~or5bxGzAVyFFSkTKkL64b9GuJaHnuD-4ZAbqMAkUqarlcEHw4cgVDquRMH86TnmtaLak62OleZAAA2K-mhcjS1FK3TTOTdWgbStNsjA6AeEZMfLRHx1DrKquYtahKSJvgiVy66eFXWpWFYorRUgRzxkML-jsvCZ-MwMZ-fi8zcbTZyDuiUKCxVTh~z5coozPIwlhioCn9gOClNgg1x3ptOAqLeZ41PzAQjGXTkRMeJbc98xr1rw34IK7TvrAlvNsCFHLnbJu11RGrbcNjMwjZB87HJ1vUYFT51BVssFu2EPBdweG2k0I4cb-jxm8RztwFy-gAAF85C1Png__&Key-Pair-Id=APKAIFKFWOMXM2UMTSFA), so we can edit the line in `PKGBUILD` to:

```bash
source=(elm-platform-${pkgver}.tar.gz::'https://web.archive.org/web/20210502114043if_/https://d29vzk4ow07wi7.cloudfront.net/6885046e3c17517c080e02d121e8c0b8298362b256d827adcb46e9cb7138d71e?response-content-disposition=attachment%3Bfilename%3D%22linux-x64.tar.gz%22&Policy=eyJTdGF0ZW1lbnQiOiBbeyJSZXNvdXJjZSI6Imh0dHAqOi8vZDI5dnprNG93MDd3aTcuY2xvdWRmcm9udC5uZXQvNjg4NTA0NmUzYzE3NTE3YzA4MGUwMmQxMjFlOGMwYjgyOTgzNjJiMjU2ZDgyN2FkY2I0NmU5Y2I3MTM4ZDcxZT9yZXNwb25zZS1jb250ZW50LWRpc3Bvc2l0aW9uPWF0dGFjaG1lbnQlM0JmaWxlbmFtZSUzRCUyMmxpbnV4LXg2NC50YXIuZ3olMjIiLCJDb25kaXRpb24iOnsiRGF0ZUxlc3NUaGFuIjp7IkFXUzpFcG9jaFRpbWUiOjE2MTk5NTYzNjJ9LCJJcEFkZHJlc3MiOnsiQVdTOlNvdXJjZUlwIjoiMC4wLjAuMC8wIn19fV19&Signature=FGbS4yf~or5bxGzAVyFFSkTKkL64b9GuJaHnuD-4ZAbqMAkUqarlcEHw4cgVDquRMH86TnmtaLak62OleZAAA2K-mhcjS1FK3TTOTdWgbStNsjA6AeEZMfLRHx1DrKquYtahKSJvgiVy66eFXWpWFYorRUgRzxkML-jsvCZ-MwMZ-fi8zcbTZyDuiUKCxVTh~z5coozPIwlhioCn9gOClNgg1x3ptOAqLeZ41PzAQjGXTkRMeJbc98xr1rw34IK7TvrAlvNsCFHLnbJu11RGrbcNjMwjZB87HJ1vUYFT51BVssFu2EPBdweG2k0I4cb-jxm8RztwFy-gAAF85C1Png__&Key-Pair-Id=APKAIFKFWOMXM2UMTSFA')
```

Then rerun `makepkg -si`.

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

[Prelude]: https://github.com/ravichugh/sketch-n-sketch/blob/dev/examples/preludeLeo.elm
[LangSvg]: https://github.com/ravichugh/sketch-n-sketch/blob/dev/src/LangSvg.elm
[ProjectPage]: http://ravichugh.github.io/sketch-n-sketch
[SvgPath]: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
[SvgTransform]: https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform


