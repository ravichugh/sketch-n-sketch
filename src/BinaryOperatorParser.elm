module BinaryOperatorParser exposing
  ( Associativity(..)
  , Precedence
  , PrecedenceTable
  , emptyPrecedenceTable
  , addOperator
  , buildPrecedenceTable
  , binaryOperator
  )

import Parser exposing (..)

import Dict exposing (Dict)

--==============================================================================
--= Data Structures
--==============================================================================

type Associativity
  = Left
  | Right

type alias Precedence =
  Int

type alias OperatorInfo =
  (Associativity, Precedence)

type PrecedenceTable =
  PT (Dict String OperatorInfo)

emptyPrecedenceTable : PrecedenceTable
emptyPrecedenceTable =
  PT Dict.empty

addOperator
  : (String, Associativity, Precedence) -> PrecedenceTable -> PrecedenceTable
addOperator (op, assoc, prec) (PT table) =
  PT <|
    Dict.insert op (assoc, prec) table

buildPrecedenceTable
  : List (Precedence, List String, List String) -> PrecedenceTable
buildPrecedenceTable =
  let
    build associativity precedence operators table =
      List.foldl
        (\op -> addOperator (op, associativity, precedence))
        table
        operators

    buildPrecedenceLevel (precedence, lefts, rights) table =
      table
        |> build Left precedence lefts
        |> build Right precedence rights
  in
    List.foldl buildPrecedenceLevel emptyPrecedenceTable

getOperatorInfo : String -> PrecedenceTable -> Maybe OperatorInfo
getOperatorInfo op (PT table) =
  Dict.get op table

--==============================================================================
--= Parsers
--==============================================================================

--------------------------------------------------------------------------------
-- Precedence climbing algorithm described by:
--------------------------------------------------------------------------------
-- http://eli.thegreenplace.net/2012/08/02/
--   parsing-expressions-by-precedence-climbing
--------------------------------------------------------------------------------

binaryOperator
  :  { precedenceTable : PrecedenceTable
     , minimumPrecedence : Precedence
     , expression : Parser exp
     , operator : Parser op
     , representation : op -> String
     , combiner : exp -> op -> exp -> exp
     }
  -> Parser exp
binaryOperator args =
  let
    { precedenceTable
    , minimumPrecedence
    , expression
    , operator
    , representation
    , combiner
    } =
      args

    operatorAndExpression : Parser (op, exp)
    operatorAndExpression =
      flip andThen operator <| \op ->
        let
          opRepresentation =
            representation op
        in
          case getOperatorInfo opRepresentation precedenceTable of
            Just (associativity, precedence) ->
              let
                nextMinimumPrecedence =
                  case associativity of
                    Left ->
                      precedence

                    Right ->
                      precedence + 1
              in
                map (\right -> (op, right)) <|
                  binaryOperator
                    { args | minimumPrecedence = nextMinimumPrecedence }

            Nothing ->
              fail <|
                "trying to parse operator "
                  ++ opRepresentation
                  ++ " but no information for it was found in the precedence"
                  ++ " table"
  in
    succeed (\left (op, right) -> combiner left op right)
      |= expression
      |= operatorAndExpression
