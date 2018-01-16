module Helpers.TestTemplates exposing (..)

import Helpers.Matchers exposing (..)
import Helpers.Utils exposing (..)

import FastParser
import LangUnparser
import Lang


testCodeTransform : (Lang.Exp -> Lang.Exp) -> String -> String -> String
testCodeTransform transformer inputCode expectedOutputCode =
  case FastParser.parseE inputCode of
    Err s ->
      "can't parse: " ++ toString s ++ "\n" ++ inputCode

    Ok inputExp ->
      let transformedExp  = transformer inputExp in
      let transformedCode = LangUnparser.unparse transformedExp in
      expectEqual (squish transformedCode) (squish expectedOutputCode)
