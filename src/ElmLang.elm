module ElmLang exposing
  ( symbols
  , isSymbol
  , isInfixOperator
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

isInfixOperator : Op -> Bool
isInfixOperator op =
  case op.val of
    Pi ->
      False
    DictEmpty ->
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
