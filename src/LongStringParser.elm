module LongStringParser exposing
  ( contentUnparse,
    contentParser
  )

import Char
import Set exposing (Set)

import Parser as P exposing (..)
import Parser.LanguageKit as LK

import ParserUtils exposing (..)
import LangParserUtils exposing (..)
import Utils

import Lang exposing (..)
import Info exposing (..)
import ElmLang

import FastParser

--Parses and unparses long interpolated strings.
contentUnparse : Exp -> String
contentUnparse e = Debug.crash "Not implemented"
{--  case e.val.e__ of
    EBase sp0 (EString _ s) ->
    EOp sp1 Plus [EBase sp0 (EString _ s), exp] sp2 ->
      case exp of

    EConst WS Num Loc WidgetDecl
    EBase WS EBaseVal
    EVar WS Ident
    EFun WS (List Pat) Exp WS -- WS: before (, before )
     TODO remember paren whitespace for multiple pats, like TForall
     | EFun WS (OneOrMany Pat) Exp WS
    EApp WS Exp (List Exp) ApplicationType WS
    EList WS (List Exp) WS (Maybe Exp) WS
    EIf WS Exp WS{-then-} Exp WS{-else-} Exp WS{-REMOVE-}
    ECase WS Exp (List Branch) WS
    ETypeCase WS Exp (List TBranch) WS
    ELet WS LetKind Rec Pat WS{-=-} Exp WS{-in or ;-} Exp WS{-REMOVE-}
    EComment WS String Exp
    EOption WS (WithInfo String) WS (WithInfo String) Exp
    ETyp WS Pat Type Exp WS
    EColonType WS Exp WS Type WS
    ETypeAlias WS Pat Type Exp WS
    EParens WS Exp ParensStyle WS
    EHole WS (Maybe Val) -- Internal intermediate, should not appear in code. (Yet.)
]--}

contentParser : Parser Exp
contentParser = fail "Not implemented" --Debug.crash "Not implemented"

