module Helpers.TestTemplates exposing (..)

import Helpers.Matchers exposing (..)
import Helpers.Utils exposing (..)

import LangParser2
import LangUnparser
import Lang


testCodeTransform : (Lang.Exp -> Lang.Exp) -> String -> String -> String
testCodeTransform transformer inputCode expectedOutputCode =
  case LangParser2.parseE inputCode of
    Err (s, _) ->
      "can't parse: " ++ s ++ "\n" ++ inputCode

    Ok inputExp ->
      let transformedExp  = transformer inputExp in
      let transformedCode = LangUnparser.unparse transformedExp in
      expectEqual (squish transformedCode) (squish expectedOutputCode)
