module Helpers.TestTemplates exposing (..)

import Helpers.Matchers exposing (..)
import Helpers.Utils exposing (..)

import Lang
import LangParser2
import LangUnparser
import OurParser2 exposing (formatError)


testCodeTransform : (Lang.Exp -> Lang.Exp) -> String -> String -> String
testCodeTransform transformer inputCode expectedOutputCode =
  case LangParser2.parseE inputCode of
    Err s ->
      "can't parse: " ++ formatError s ++ "\n"

    Ok inputExp ->
      let transformedExp  = transformer inputExp in
      let transformedCode = LangUnparser.unparse transformedExp in
      shouldEqual (squish transformedCode) (squish expectedOutputCode)
