module Example exposing
  ( Example
  , parse
  )

import Char

import Parser exposing (..)
import Parser.LanguageKit as LanguageKit
import ParserUtils exposing (try, token, singleLineString)

import Lang exposing (Ident, Num)

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type Example
  = ExConstructor Ident Example
  | ExNum Num
  | ExBool Bool
  | ExString String
  | ExTuple (List Example)
  | ExPartialFunction (List (Example, Example)) -- TODO Should be (v, ex)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')

capitalIdentifier : Parser String
capitalIdentifier =
  succeed (++)
    |= keep (Exactly 1) Char.isUpper
    |= keep zeroOrMore (\c -> Char.isUpper c || Char.isLower c)

exConstructor : Parser Example
exConstructor =
  lazy <| \_ ->
    inContext "constructor example" <|
      succeed ExConstructor
        |= capitalIdentifier
        |= example

exNum : Parser Example
exNum =
  let
    sign =
      oneOf
        [ succeed (-1)
            |. symbol "-"
        , succeed 1
        ]
  in
    try <|
      inContext "number example" <|
        succeed (\s n -> ExNum (s * n))
          |= sign
          |= float

exBool : Parser Example
exBool =
  inContext "boolean example" <|
    map ExBool <|
      oneOf
        [ token "True" True
        , token "False" False
        ]

exString : Parser Example
exString =
  inContext "string example" <|
    map (\(_, content) -> ExString content)
      singleLineString

exTuple : Parser Example
exTuple =
  lazy <| \_ ->
    inContext "tuple example" <|
      map ExTuple <|
        LanguageKit.sequence
          { start = "("
          , separator = ","
          , end = ")"
          , spaces = spaces
          , item = example
          , trailing = LanguageKit.Forbidden
          }

exPartialFunction : Parser Example
exPartialFunction =
  lazy <| \_ ->
    let
      binding : Parser (Example, Example)
      binding =
        succeed (,)
          |= example
          |. spaces
          |. symbol "->"
          |. spaces
          |= example
    in
      inContext "partial function example" <|
        map ExPartialFunction <|
          LanguageKit.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = spaces
            , item = binding
            , trailing = LanguageKit.Forbidden
            }

example : Parser Example
example =
  lazy <| \_ ->
    oneOf
       [ exConstructor
       , exNum
       , exBool
       , exString
       , exTuple
       , exPartialFunction
       ]

parse : String -> Result Parser.Error Example
parse =
  run example