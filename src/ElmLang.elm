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
    , '.'
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


infixOp_s : Set Op_
infixOp_s =
  Set.fromList
    [ Plus
    , Minus
    , Mult
    , Div
    , Lt
    , Eq
    ]


isInfixOperator : Op -> Bool
isInfixOperator op =
  Set.member op.val infixOp_s
