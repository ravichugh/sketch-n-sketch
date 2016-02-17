module SimplifyTests where

import Helpers.Matchers exposing (..)
import Helpers.Utils exposing (..)

import String

import LangTransform
import LangParser2
import LangUnparser

testSimplification inputCode expectedSimplification =
  case LangParser2.parseE inputCode of
    Err s ->
      "can't parse: " ++ s ++ "\n" ++ inputCode

    Ok inputExp ->
      let simplifiedExp  = LangTransform.simplify inputExp in
      let simplifiedCode = LangUnparser.unparse simplifiedExp in
      (squish simplifiedCode) `shouldEqual` (squish expectedSimplification)


testNoSimplification inputCode =
  testSimplification inputCode inputCode


unusedSimpleAssignmentTest () = testSimplification   "(let dead 6 'body')"   "'body'"
usedSimpleAssignmentTest   () = testNoSimplification "(let alive 6 alive)"


multipleAssignmentTests () =
  [ \() -> testSimplification   "(let [dead1  dead2  dead3]  [6 7 8] 'body')"                 "'body'"
  , \() -> testSimplification   "(let [alive1 dead2  dead3]  [6 7 8] alive1)"                 "(let alive1          6     alive1)"
  , \() -> testSimplification   "(let [alive1 dead2  alive3] [6 7 8] [alive1 alive3])"        "(let [alive1 alive3] [6 8] [alive1 alive3])"
  , \() -> testNoSimplification "(let [alive1 alive2 alive3] [6 7 8] [alive1 alive2 alive3])"
  ]


-- mapExp runs bottom-up so this magically works
unusedVariableChainTest () =
  testSimplification "(let dead1 6 (let dead2 dead1 'body'))"  "'body'"


simpleAssignmentChainTests () =
  [ \() -> testSimplification "(let redundant 6 (let alive           redundant     alive))"              "(let alive           6     alive)"
  , \() -> testSimplification "(let redundant 6 (let [alive1 alive2] [redundant 7] (+ alive1 alive2)))"  "(let [alive1 alive2] [6 7] (+ alive1 alive2))"
  ]

usedVariableChainTest () =
  testNoSimplification "(let alive1 6 (let alive2 alive1 (+ alive1 alive2)))"


shadowedAssignmentChainTest () =
  testSimplification "(let shadow 6 (let shadow 7 (let alive shadow alive)))"  "(let alive 7 alive)"

