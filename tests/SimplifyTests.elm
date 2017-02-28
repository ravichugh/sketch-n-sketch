module SimplifyTests exposing (..)

import Helpers.TestTemplates exposing (..)

import LangTransform


testSimplification inputCode expectedSimplification =
  Helpers.TestTemplates.testCodeTransform
    LangTransform.simplify
    inputCode
    expectedSimplification


testNoSimplification inputCode =
  testSimplification inputCode inputCode


unusedSimpleAssignmentTest () = testSimplification   "(let dead 6 'body')"   "'body'"
usedSimpleAssignmentTest   () = testNoSimplification "(let alive 6 alive)"


multipleAssignmentTests () =
  [ \() -> testSimplification   "(let [dead1  dead2  dead3]  [6 7 8] 'body')"                 "'body'"
  , \() -> testSimplification   "(let [alive1 dead2  dead3]  [6 7 8] alive1)"                 "(let alive1          6     alive1)"
  , \() -> testSimplification   "(let [alive1 dead2  alive3] [6 7 8] [alive1 alive3])"        "(let [alive1 alive3] [6 8] [alive1 alive3])"
  , \() -> testNoSimplification "(let [alive1 alive2 alive3] [6 7 8] [alive1 alive2 alive3])"
  , \() -> testNoSimplification "(let [alive mistake] [6] [mistake])"
  ]


-- mapExp runs bottom-up so this magically works.
unusedVariableChainTest () =
  testSimplification "(let dead1 6 (let dead2 dead1 'body'))"  "'body'"


simpleAssignmentChainTests () =
  [ \() -> testSimplification "(let redundant 6 (let alive           redundant     alive))"              "(let alive           6     alive)"
  , \() -> testSimplification "(let redundant 6 (let [alive1 alive2] [redundant 7] (+ alive1 alive2)))"  "(let [alive1 alive2] [6 7] (+ alive1 alive2))"
  ]


shadowedAssignmentChainTest () =
  testSimplification "(let shadow 6 (let shadow 7 (let alive shadow alive)))"  "(let alive 7 alive)"


-- More shadow testing.
scopeHandlingTests () =
  [ \() -> testNoSimplification "(let name1 5 (let name2 name1 (let name1 6 (+ name1 name2))))"
  , \() -> testNoSimplification "(let name1 5 (let name2 name1 (let name2 6 (+ name1 name2))))"
  , \() -> testNoSimplification "(let name1 5 (let name2 name1 (\\name1 [name1 name2])))"
  , \() -> testNoSimplification "(let name1 5 (let name2 name1 (\\name2 [name1 name2])))"
  , \() -> testNoSimplification "(let name1 5 (let name2 name1 (case 6 (name1 [name1 name2]) (name2 [name1 name2]))))"
  ]


-- Same shadow trickiness as scopes above, but in the assigns expressions.
letrecScopeTests () =
  [ \() -> testNoSimplification "(let name1 5 (let name2 name1 (letrec name1 (\\() [name1 name2]) [name1 name2])))"
  , \() -> testNoSimplification "(let name1 5 (let name2 name1 (letrec name2 (\\() [name1 name2]) [name1 name2])))"
  , \() -> testSimplification   "(let name1 5 (let name2 name1 (let    name1 (\\() [name1 name2]) [name1 name2])))"
                                "(let name1 5 (let name2 name1 (let    name1 (\\() [name1 name1]) [name1 name2])))"
  ]


multiuseTest () =
  testNoSimplification "(let alive 6 (let [renamed dummy] [alive (+ 6 alive)]) [renamed dummy])"


doublyNamedValueTest () =
  testSimplification "(let name1 6 (let name2 name1 (+ name1 name2)))"  "(let name1 6 (+ name1 name1))"


doublyNamedValueInMultipleAssignmentsTest () =
  testSimplification
    "(let [name1 dummy1] [6 7] (let [name2 dummy2] [name1 9] [name1 name2 dummy1 dummy2]))"
    "(let [name1 dummy1] [6 7] (let dummy2 9 [name1 name1 dummy1 dummy2]))"

