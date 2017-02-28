module UnparserTests exposing (..)

import Helpers.Matchers exposing (..)

import String

import LangParser2
import LangUnparser
import OurParser2 exposing (formatError)

testParseUnparseMatch littleStr =
  case LangParser2.parseE littleStr of
    Err s ->
      "can't parse: " ++ formatError s ++ "\n"
    Ok parsed ->
      let unparsed =
        LangUnparser.unparse parsed
      in
      shouldEqual unparsed littleStr

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
  , "\"doubled-quoted string\""
  , "\"dq string \\\\ with \\\"escapes\\\"\""
  , "'sq string \\\\ with \\'escapes\\''"
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
  , "(if (= a b) 'body1' 'body2')"
  , "   (if  (= a b )  'body1' 'body2'     )"
  , "(case (+ 4 5) (1 'body1') ([h|tail] 'body2') (true 'body3') (var 'body4'))"
  , "  (case (+ 4 5)\n   (1 'body1')\n   ([ h  | tail   ]\n   'body2') (  true\n   'body3')\n   (var 'body4')\n  )"
  , "(typecase x (Int (+ x 1)) ([Int String] (length x)) (_ 0))"
  , "  (typecase [x y] ([Int Int] (+ x y)) (_ 0))"
  , "(let pair@[a b] exp 'body')"
  , "  (letrec pair @ [ a b ] exp\n'body'\n  )"
  , "(typ x Num) e"
  , "  (typ [a b c] [Num String Null | Bool]) e"
  , "(typ f (-> Num a (List a))) e"
  , "(typ f (-> (Dict String (List Num)) Bool)) e"
  , "(typ f (union Num String Rgba)) e"
  , "((gt 6 7) : Bool)"
  , "(def Rgba [Num Num Num Num])"
  , "(def [Rgba KVPair] [[Num Num Num Num] [String String]])"
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
