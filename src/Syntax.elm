module Syntax exposing
  ( Syntax(..)
  , parser
  , unparser
  , patternUnparser
  )

import FastParser
import ElmParser
import LangUnparser
import ElmUnparser
import Lang

import Parser

type Syntax
  = Little
  | Elm

parser : Syntax -> String -> Result Parser.Error Lang.Exp
parser syntax =
  case syntax of
    Little ->
      FastParser.parseE

    Elm ->
      ElmParser.parse

unparser : Syntax -> Lang.Exp -> String
unparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparse

    Elm ->
      ElmUnparser.unparse

patternUnparser : Syntax -> Lang.Pat -> String
patternUnparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparsePat

    Elm ->
      ElmUnparser.unparsePattern
