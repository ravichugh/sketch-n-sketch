# Sketch-N-Sketch

Sketch-N-Sketch: Program Synthesis for Direct Manipulation

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
      |   (letrec f (\x e1) e2)
      |   (op e1 e2)
      |   (if e1 e2 e3)
      |   (case e (p1 e1) ... (pn en))
      |   []
      |   [e1 | e2]
      |   [e1 .... en]
      |   [e1 .... en | erest]
```

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
  op2  ::=  + | - | * | / | <
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
```

### Standard Prelude

See `Prelude.elm` for the standard library included by every program.

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

See `Prelude.elm` for a small library of SVG-manipulating functions.

