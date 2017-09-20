module ElmLang exposing
  ( symbols
  , isSymbol
  , isOperator
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
    , '.'
    , '<'
    , '>'
    , ':'
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

isOperator : Exp -> Bool
isOperator e =
  case e.val.e__ of
    EVar _ identifier ->
      String.all isSymbol identifier

    _ ->
      False
