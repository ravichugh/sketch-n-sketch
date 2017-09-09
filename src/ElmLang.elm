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

type Program
  = Empty Whitespace
  | Nonempty ETerm

--==============================================================================
--= Patterns
--==============================================================================

--------------------------------------------------------------------------------
-- Pattern Information
--------------------------------------------------------------------------------

type alias PNamedInfo =
  { name : Identifier
  }

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

type Pattern
  = PNamed PNamedInfo

getIdentifier : Pattern -> String
getIdentifier pattern =
  case pattern of
    PNamed { name } ->
      name

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

type alias EVariableInfo =
  { identifier : Identifier
  }

-- Functions are curried, so lambdas always take only one parameter
type alias ELambdaInfo =
  { parameter : PTerm
  , body : ETerm
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
  | EVariable EVariableInfo
  | ELambda ELambdaInfo
  | EParen EParenInfo
  | EList EListInfo
  | ERecord ERecordInfo
  | EConditional EConditionalInfo
  | EFunctionApplication EFunctionApplicationInfo
  | EBinaryOperator EBinaryOperatorInfo

prefixifyOperator : Identifier -> Identifier
prefixifyOperator operator =
  "(" ++ operator ++ ")"

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
