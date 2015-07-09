<img src="img/sketch-n-sketch-logo.png"
     align="right" style="padding: 0px;" height="40px" />

# Sketch-N-Sketch

<!-- <br/> -->

<!-- Sketch-N-Sketch: Program Synthesis for Direct Manipulation -->

Direct manipulation interfaces are useful in many domains, but the lack of
programmability in a high-level language makes it difficult to develop complex
and reusable content. We envision *prodirect manipulation* editors that allow
users to freely mix between programmatic and direct manipulation.

<!-- TODO widths on GitHub -->

<span style="display: inline-block; width: 222px; text-align: right;">**Prodirect Manipulation**</span>
  = Programmatic + Direct Manipulation <br/>
<span style="display: inline-block; width: 222px; text-align: right;">**Sketch-N-Sketch**</span>
  = Prodirect Manipulation Editor for SVG

## Demo

[`v0.0`]("") `(Jul 2015)` Alpha Version

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
      |   (defrec p e1) e2
      |   (if e1 e2 e3)
      |   (case e (p1 e1) ... (pn en))
      |   []
      |   [e1 | e2]
      |   [e1 .... en]
      |   [e1 .... en | erest]
      |   (op0)
      |   (op1 e1)
      |   (op2 e1 e2)
      |   ;single-line-comment e
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
```

```
  s  ::=  'hello' | 'world' | ...
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
  op2  ::=  + | - | * | /
        |   < | =
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
      |   [p1 | p2]
      |   [p1 ... pn]
      |   [p1 ... pn | prest]
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

TODO explain attrs for different shapes

See [`prelude.little`][Prelude] for a small library of SVG-manipulating functions.

[Prelude]: https://github.com/ravichugh/sketch-n-sketch/blob/master/examples/prelude.little

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

If you want to add a new example to the dropdown menu:

1. Create a `examples/newExample.little` file for your `newExample`.

2. In `ExamplesTemplate.elm`, add the lines:

   * `LITTLE_TO_ELM yourExample`
   * `, makeExample "newExampleName" newExample`

3. From the `src/` directory, run `make examples`.

4. Re-launch Sketch-N-Sketch.
