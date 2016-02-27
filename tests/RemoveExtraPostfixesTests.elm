module RemoveExtraPostfixesTests where

import Helpers.TestTemplates exposing (..)

import LangTransform


testPostfixRemoval inputCode expectedOutputCode =
  Helpers.TestTemplates.testCodeTransform
    (LangTransform.removeExtraPostfixes ["Orig", "'"])
    inputCode
    expectedOutputCode


testNoChange inputCode =
  testPostfixRemoval inputCode inputCode


removeExtraPostfixesTests () =
  [ \() -> testNoChange       "(let x      6 (+ x x))"
  , \() -> testPostfixRemoval "(let x'     6 (+ x' x'))"          "(let x 6 (+ x x))"
  , \() -> testPostfixRemoval "(let xOrig  6 (+ xOrig xOrig))"    "(let x 6 (+ x x))"
  , \() -> testPostfixRemoval "(let xOrig' 6 (+ xOrig' xOrig'))"  "(let x 6 (+ x x))"
  , \() -> testPostfixRemoval "(let x'Orig 6 (+ x'Orig x'Orig))"  "(let x 6 (+ x x))"
  , \() -> testNoChange       "(let x' 6 (let x  7 (+ x x)))"
  , \() -> testNoChange       "(let x  6 (let x' 7 (+ x' x')))"
  ]
