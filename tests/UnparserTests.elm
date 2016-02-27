module UnparserTests where

import Helpers.Matchers exposing (..)

import String

import LangParser2
import LangUnparser

testParseUnparseMatch littleStr =
  case LangParser2.parseE littleStr of
    Err s ->
      "can't parse: " ++ s ++ "\n" ++ littleStr
    Ok parsed ->
      let unparsed =
        LangUnparser.unparse parsed
      in
      unparsed `shouldEqual` littleStr

littlePrograms =
  [ "5"
  , "55"
  , " 55"
  , " 55{0-5.6}"
  , "true"
  , "  true"
  , "false"
  , "  false"
  , "'string'"
  , "  'string'"
  , "variable"
  , "  variable"
  , "(\\_ 'exp')"
  , " (\\ x 'exp'  )"
  , "(\\( a b) 'exp')"
  , " (\\[a b] 'exp'  )"
  , "(func arg1 arg2)"
  , " (func arg1 arg2  )"
  , " (   (func arg1    ) arg2 arg3  )"
  , "(pi)"
  , "  (pi  )"
  , "(sqrt 5.5)"
  , "  (sqrt 5.5  )"
  , "(+ 1 2 )"
  , "  (+ 3 4  )"
  , "[]"
  , "  [ ]"
  , "[ a ]"
  , "[ a b c ]"
  , "  [ a  b   ]"
  , "[ a b   | []  ]"
  , "[ a b   | c  ]"
  , "[ a b   | [  c ]  ]"
  , "[||]"
  , "  [| |]"
  , "[| 1 2 3 |]"
  , "  [|   1 2  |]"
  , "[| 1 2 6..8 |]"
  , "  [| 1 2 6  ..   8 |]"
  , "(if (= a b) 'body1' 'body2')"
  , "   (if  (= a b )  'body1' 'body2'     )"
  , "(case (+ 4 5) (1 'body1') ([h|tail] 'body2') (true 'body3') (var 'body4'))"
  , "  (case (+ 4 5)\n   (1 'body1')\n   ([ h  | tail   ]\n   'body2') (  true\n   'body3')\n   (var 'body4')\n  )"
  , "(let [a b] exp 'body')"
  , "  (letrec [ a b ] exp\n'body'\n  )"
  , "(def x 6) x"
  , "(def [a b | t] exp) 'body'"
  , "  (defrec a exp   ) 'body'"
  , "  ; Comment\n\n'exp'"
  , "# unannotated-numbers: n?\n'exp'"
  , "  # unannotated-numbers:    n?\n\n  'exp'"
  ]

unparserTests : () -> List (() -> String)
unparserTests () =
  List.map
    (\code -> \() -> testParseUnparseMatch code)
    littlePrograms
