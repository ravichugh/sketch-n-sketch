module ElmParser exposing
  ( parse
  )

import Char
import Set exposing (Set)

import Parser as P exposing (..)
import Parser.LanguageKit as LK

import ParserUtils exposing (..)
import LangParserUtils exposing (..)

import Lang exposing (..)

--==============================================================================
--= General
--==============================================================================

keywords : Set String
keywords =
  Set.fromList
    [ "let"
    , "in"
    , "case"
    , "of"
    , "if"
    , "then"
    , "else"
    ]

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

isRestChar : Char -> Bool
isRestChar char =
  Char.isLower char ||
  Char.isUpper char ||
  Char.isDigit char ||
  char == '_'

isSymbol : Char -> Bool
isSymbol char =
  Set.member char symbols

littleIdentifier : ParserI Ident
littleIdentifier =
  trackInfo <|
    LK.variable
      Char.isLower
      isRestChar
      keywords

bigIdentifier : ParserI Ident
bigIdentifier =
  trackInfo <|
    LK.variable
      Char.isUpper
      isRestChar
      keywords

symbolIdentifier : ParserI Ident
symbolIdentifier =
  trackInfo <|
    keep oneOrMore isSymbol

--==============================================================================
-- Base Values
--==============================================================================

--------------------------------------------------------------------------------
-- Bools
--------------------------------------------------------------------------------

bool : ParserI EBaseVal
bool =
  inContext "bool" <|
    trackInfo <|
      map EBool <|
        oneOf
          [ token "True" True
          , token "False" False
          ]

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

singleLineString : ParserI EBaseVal
singleLineString =
  inContext "single-line string" <|
    trackInfo <|
      succeed (EString "\"")
        |. symbol "\""
        |= keep zeroOrMore (\c -> c /= '\"')
        |. symbol "\""

multiLineString : ParserI EBaseVal
multiLineString =
  inContext "multi-line string" <|
    trackInfo <|
      map (EString "\"\"\"") <|
        inside "\"\"\""

--------------------------------------------------------------------------------
-- General Base Values
--------------------------------------------------------------------------------

baseValue : ParserI EBaseVal
baseValue =
  inContext "base value" <|
    oneOf
      [ multiLineString
      , singleLineString
      , bool
      ]

--==============================================================================
-- Patterns
--==============================================================================

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variablePattern : Parser Pat
variablePattern =
  inContext "variable pattern" <|
    mapPat_ <|
      paddedBefore
        ( \ws name ->
            PVar ws name noWidgetDecl
        )
        littleIdentifier

--------------------------------------------------------------------------------
-- Constants TODO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValuePattern : Parser Pat
baseValuePattern =
  inContext "base value pattern" <|
    mapPat_ <|
      paddedBefore PBase baseValue

--------------------------------------------------------------------------------
-- Lists TODO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- As-Patterns (@-Patterns) TODO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- General Patterns
--------------------------------------------------------------------------------

pattern : Parser Pat
pattern =
  inContext "pattern" <|
    oneOf
      [ --lazy <| \_ -> patternList
      --, lazy <| \_ -> asPattern
      --, constantPattern
        baseValuePattern
      , variablePattern
      ]

--==============================================================================
-- Expressions
--==============================================================================

--------------------------------------------------------------------------------
-- Constants TODO
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValueExpression : Parser Exp
baseValueExpression =
  inContext "base value expression" <|
    mapExp_ <|
      paddedBefore EBase baseValue

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variableExpression : Parser Exp
variableExpression =
  mapExp_ <|
    paddedBefore EVar littleIdentifier

--------------------------------------------------------------------------------
-- Functions (lambdas)
--------------------------------------------------------------------------------

function : Parser Exp
function =
  inContext "function" <|
    lazy <| \_ ->
      mapExp_ <|
        paddedBefore
          ( \wsBefore (parameters, body) ->
              EFun wsBefore parameters body space0
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "\\"
                |= repeat oneOrMore pattern
                |. symbol "->"
                |= expression
          )

--------------------------------------------------------------------------------
-- General Expressions
--------------------------------------------------------------------------------

expression : Parser Exp
expression =
  inContext "expression" <|
    lazy <| \_ ->
      oneOf
        [ -- constantExpression
          baseValueExpression
        -- , lazy <| \_ -> typeAlias
        -- , lazy <| \_ -> conditional
        -- , lazy <| \_ -> letBinding
        -- , lazy <| \_ -> caseExpression
        -- , lazy <| \_ -> typeCaseExpression
        -- , lazy <| \_ -> typeDeclaration
        -- , lazy <| \_ -> typeAnnotation
        -- , lazy <| \_ -> list
         , lazy <| \_ -> function
        -- , lazy <| \_ -> functionApplication
        -- , lazy <| \_ -> operator
        -- , lazy <| \_ -> comment
        -- , lazy <| \_ -> option
        , variableExpression
        ]

--==============================================================================
-- Exports
--==============================================================================

--------------------------------------------------------------------------------
-- Parser Runners
--------------------------------------------------------------------------------

parse : String -> Result P.Error Exp
parse =
  run expression
