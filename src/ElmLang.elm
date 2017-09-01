module ElmLang exposing
  ( Pattern(..)
  , Expression(..)
  , PTerm
  , ETerm
  , pterm_
  , eterm_
  , ebinop_
  )

import Position exposing (dummyPosition)
import Whitespace exposing (Whitespace, dummyWhitespace)
import Range exposing (Ranged)
import Padding exposing (Padded)

-- General

type alias Identifier =
  String

-- Patterns

type Pattern =
  PNamed
    { name : Identifier }

-- Expressions

-- Helpful: http://elm-lang.org/docs/syntax

type Expression
  = ELineComment
      { text : String
      , termAfter : Maybe ETerm
      }
  | EBlockComment
      { text : String
      , termAfter : Maybe ETerm
      }
  | EVariable
      { identifier : Identifier
      }
  | EBool
      { bool : Bool
      }
  | EInt
      { int : Int
      }
  | EFloat
      { float : Float
      }
  | EChar
      { char : Char
      }
  | EString
      { string : String
      }
  | EMultiLineString
      { string : String
      }
  | EList
      { members : List ETerm
      }
  | EEmptyList
      { space : Whitespace
      }
  | ERecord
      { base : Maybe ETerm
      , entries : List (PTerm, ETerm)
      }
  | EEmptyRecord
      { space : Whitespace
      }
  | EConditional
      { condition : ETerm
      , trueBranch : ETerm
      , falseBranch : ETerm
      }
  | EFunctionApplication
      { function : ETerm
      , arguments : List ETerm
      }
  | EBinaryOperator
     { operator : String
     , left : ETerm
     , right : ETerm
     }

type alias PId =
  Int

dummyPId : PId
dummyPId =
  -1

type alias EId =
  Int

dummyEId : EId
dummyEId =
  -1


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

ebinop_ : String -> ETerm -> ETerm -> ETerm
ebinop_ operator left right =
  { start =
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
  , eid =
      dummyEId
  , expression =
      EBinaryOperator
        { operator =
            operator
        , left =
            left
        , right =
            right
        }
  }
