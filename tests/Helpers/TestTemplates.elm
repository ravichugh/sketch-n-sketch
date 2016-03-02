module Helpers.TestTemplates where

import Helpers.Matchers exposing (..)
import Helpers.Utils exposing (..)

import LangParser2
import LangUnparser
import Lang


testCodeTransform : (Lang.Exp -> Lang.Exp) -> String -> String -> String
testCodeTransform transformer inputCode expectedOutputCode =
  case LangParser2.parseE inputCode of
    Err s ->
      "can't parse: " ++ s ++ "\n" ++ inputCode

    Ok inputExp ->
      let transformedExp  = transformer inputExp in
      let transformedCode = LangUnparser.unparse transformedExp in
      (squish transformedCode) `shouldEqual` (squish expectedOutputCode)
