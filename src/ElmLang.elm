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

pterm_ : Pattern -> PTerm
pterm_ pattern =
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

type alias ELambdaInfo =
  { parameters : List PTerm
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

type alias EFunctionApplicationInfo =
  { function : ETerm
  , arguments : List ETerm
  }

type alias EBinaryOperatorInfo =
  { operator : String
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
  | ELambda ELambdaInfo
  | EVariable EVariableInfo
  | EParen EParenInfo
  | EList EListInfo
  | ERecord ERecordInfo
  | EConditional EConditionalInfo
  | EFunctionApplication EFunctionApplicationInfo
  | EBinaryOperator EBinaryOperatorInfo

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

eterm_ : Expression -> ETerm
eterm_ expression =
  { start = dummyPosition
  , end = dummyPosition
  , before = dummyWhitespace
  , after = dummyWhitespace
  , eid = dummyEId
  , expression = expression
  }

--==============================================================================
--= Term Helpers
--==============================================================================

span : List (Ranged (Padded a)) -> (Ranged (Padded a)) -> (Ranged (Padded a))
span insides wrapper =
  case (List.head insides, Utils.maybeLast insides) of
    (Just left, Just right) ->
      { wrapper
          | start =
              left.start
          , end =
              right.end
          , before =
              { start =
                  left.start
              , end =
                  left.start
              , ws =
                  ""
              }
          , after =
              { start =
                  right.end
              , end =
                  right.end
              , ws =
                  ""
              }
      }
    _ ->
      wrapper

spannedFunctionApplication : ETerm -> List ETerm -> ETerm
spannedFunctionApplication function arguments =
  let
    wrapper =
      eterm_ <|
        EFunctionApplication
          { function = function
          , arguments = arguments
          }
  in
    span arguments wrapper

spannedBinaryOperator : String -> ETerm -> ETerm -> ETerm
spannedBinaryOperator operator left right =
  let
    wrapper =
      eterm_ <|
        EBinaryOperator
          { operator = operator
          , left = left
          , right = right
          }
  in
    span [left, right] wrapper

