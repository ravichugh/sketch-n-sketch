module ElmLang exposing
  ( symbols
  , isSymbol
  , isInfixOperator
  , arity
  )

import Set exposing (Set)

import Lang exposing (..)

-- See: https://groups.google.com/forum/#!msg/elm-dev/0AHSnDdkSkQ/E0SVU70JEQAJ
symbols : Set Char
symbols =
  Set.fromList
    [ '+'
    , '-'
    , '/'
    , '*'
    , '='
    --, '.' Disallowed to be able to assign records to operator symbols.
    , '<'
    , '>'
    -- Disallow ':' (reserved for colon types)
    -- , ':'
    , '&'
    , '|'
    , '^'
    , '?'
    , '%'
    , '#'
    , '~'
    , '!'
    ]

isSymbol : Char -> Bool
isSymbol char =
  Set.member char symbols

arity : Op -> Int
arity op =
  case op.val of
    Pi ->
      0
    DictEmpty ->
      0
    CurrentEnv ->
      0
    DictFromList ->
      1
    Cos ->
      1
    Sin ->
      1
    ArcCos ->
      1
    ArcSin ->
      1
    Floor ->
      1
    Ceil ->
      1
    Round ->
      1
    ToStr ->
      1
    Sqrt ->
      1
    Explode ->
      1
    Plus ->
      2
    Minus ->
      2
    Mult ->
      2
    Div ->
      2
    Lt ->
      2
    Eq ->
      2
    Mod ->
      2
    Pow ->
      2
    ArcTan2 ->
      2
    DictInsert ->
      3
    DictGet ->
      2
    DictRemove ->
      2
    DebugLog ->
      1
    NoWidgets ->
      1
    ToStrExceptStr ->
      1
    RegexExtractFirstIn ->
      2

isInfixOperator : Op -> Bool
isInfixOperator op =
  case op.val of
    Pi ->
      False
    DictEmpty ->
      False
    CurrentEnv ->
      False
    DictFromList ->
      False
    Cos ->
      False
    Sin ->
      False
    ArcCos ->
      False
    ArcSin ->
      False
    Floor ->
      False
    Ceil ->
      False
    Round ->
      False
    ToStr ->
      False
    Sqrt ->
      False
    Explode ->
      False
    Plus ->
      True
    Minus ->
      True
    Mult ->
      True
    Div ->
      True
    Lt ->
      True
    Eq ->
      True
    Mod ->
      False
    Pow ->
      False
    ArcTan2 ->
      False
    DictInsert ->
      False
    DictGet ->
      False
    DictRemove ->
      False
    DebugLog ->
      False
    NoWidgets ->
      False
    ToStrExceptStr ->
      False
    RegexExtractFirstIn ->
      False
