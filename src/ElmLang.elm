module ElmLang exposing (..)

import Position exposing (dummyPosition)
import Whitespace exposing (Whitespace, dummyWhitespace)
import Range exposing (Ranged)
import Padding exposing (Padded)
import Utils

--==============================================================================
--= General
--==============================================================================

type alias Identifier =
  String

type alias Program =
  List STerm

type ElmInput
  = Empty Whitespace
  | Nonempty Program

--==============================================================================
--= Patterns
--==============================================================================

--------------------------------------------------------------------------------
-- Pattern Information
--------------------------------------------------------------------------------

type alias PNamedInfo =
  { identifier : Identifier
  }

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

type Pattern
  = PNamed PNamedInfo

getIdentifiers : Pattern -> List String
getIdentifiers pattern =
  case pattern of
    PNamed { identifier } ->
      [ identifier ]

--------------------------------------------------------------------------------
-- PIds
--------------------------------------------------------------------------------

type alias PId =
  Int

dummyPId : PId
dummyPId =
  -1

--------------------------------------------------------------------------------
-- P-Terms
--------------------------------------------------------------------------------

type alias PTerm =
  Ranged
    ( Padded
        { pattern : Pattern
        , pid : PId
        }
    )

pTerm_ : Pattern -> PTerm
pTerm_ pattern =
  { start = dummyPosition
  , end = dummyPosition
  , before = dummyWhitespace
  , after = dummyWhitespace
  , pid = dummyPId
  , pattern = pattern
  }

--==============================================================================
--= Expressions
--==============================================================================
-- Helpful: http://elm-lang.org/docs/syntax

--------------------------------------------------------------------------------
-- Specials
--------------------------------------------------------------------------------
-- These are the built-in variables/functions

type Special
  = Add

specialify : Identifier -> Maybe Special
specialify identifier =
  case identifier of
    "(+)" ->
      Just Add
    _ ->
      Nothing

showSpecial : Special -> String
showSpecial special =
  case special of
    Add ->
      "(+)"

--------------------------------------------------------------------------------
-- Expression Information
--------------------------------------------------------------------------------

type alias ELineCommentInfo =
  { text : String
  , termAfter : Maybe ETerm
  }

type alias EBlockCommentInfo =
  { text : String
  , termAfter : Maybe ETerm
  }

type alias EBoolInfo =
  { bool : Bool
  }

type alias EIntInfo =
  { int : Int
  }

type alias EFloatInfo =
  { float : Float
  }

type alias ECharInfo =
  { char : Char
  }

type alias EStringInfo =
  { string : String
  }

type alias EMultiLineStringInfo =
  { string : String
  }

type alias EEmptyListInfo =
  { whitespace : Whitespace
  }

type alias EEmptyRecordInfo =
  { whitespace : Whitespace
  }

-- Functions are curried, so lambdas always take only one parameter
type alias ELambdaInfo =
  { parameter : PTerm
  , body : ETerm
  }

type alias EVariableInfo =
  { identifier : Identifier
  }

type alias EParenInfo =
  { inside : ETerm
  }

type alias EListInfo =
  { members : List ETerm
  }

type alias ERecordInfo =
  { base : Maybe ETerm
  , entries : List (PTerm, ETerm)
  }

type alias EConditionalInfo =
  { condition : ETerm
  , trueBranch : ETerm
  , falseBranch : ETerm
  }

-- Functions are curried, so they always take only one argument
type alias EFunctionApplicationInfo =
  { function : ETerm
  , argument : ETerm
  }

type alias EBinaryOperatorInfo =
  { operator : Identifier
  , left : ETerm
  , right : ETerm
  }

type alias ESpecialInfo =
  { special : Special
  , arguments : List ETerm
  }

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

type Expression
  = ELineComment ELineCommentInfo
  | EBlockComment EBlockCommentInfo
  | EBool EBoolInfo
  | EInt EIntInfo
  | EFloat EFloatInfo
  | EChar ECharInfo
  | EString EStringInfo
  | EMultiLineString EMultiLineStringInfo
  | EEmptyList EEmptyListInfo
  | EEmptyRecord EEmptyRecordInfo
  | ELambda ELambdaInfo
  | EVariable EVariableInfo
  | EParen EParenInfo
  | EList EListInfo
  | ERecord ERecordInfo
  | EConditional EConditionalInfo
  | EFunctionApplication EFunctionApplicationInfo
  | EBinaryOperator EBinaryOperatorInfo
  | ESpecial ESpecialInfo

prefixifyOperator : Identifier -> Identifier
prefixifyOperator operator =
  "(" ++ operator ++ ")"

buildLambda : List PTerm -> ETerm -> ETerm
buildLambda parameters body =
  List.foldr
    ( \parameter bodyAcc ->
        eTerm_ <|
          ELambda
            { parameter =
                parameter
            , body =
                bodyAcc
            }
    )
    body
    parameters

--------------------------------------------------------------------------------
-- EIds
--------------------------------------------------------------------------------

type alias EId =
  Int

dummyEId : EId
dummyEId =
  -1

--------------------------------------------------------------------------------
-- E-Terms
--------------------------------------------------------------------------------

type alias ETerm =
  Ranged
    ( Padded
        { expression : Expression
        , eid : EId
        }
    )

eTerm_ : Expression -> ETerm
eTerm_ expression =
  { start = dummyPosition
  , end = dummyPosition
  , before = dummyWhitespace
  , after = dummyWhitespace
  , eid = dummyEId
  , expression = expression
  }

--==============================================================================
--= Statements
--==============================================================================

--------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------

type alias Definition =
  { name : PTerm
  , parameters : List PTerm
  , body : ETerm
  }

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

type Statement
  = SDefinition Definition
  | SLineComment ELineCommentInfo
  | SBlockComment EBlockCommentInfo

--------------------------------------------------------------------------------
-- SIds
--------------------------------------------------------------------------------

type alias SId =
  Int

dummySId : SId
dummySId =
  -1

--------------------------------------------------------------------------------
-- S-Terms
--------------------------------------------------------------------------------

type alias STerm =
  Ranged
    ( Padded
        { statement : Statement
        , sid : SId
        }
    )

sTerm_ : Statement -> STerm
sTerm_ statement =
  { start = dummyPosition
  , end = dummyPosition
  , before = dummyWhitespace
  , after = dummyWhitespace
  , sid = dummySId
  , statement = statement
  }
