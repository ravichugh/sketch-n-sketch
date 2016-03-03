module FeatureEquationTests where

import Helpers.TestTemplates exposing (..)

import ValueBasedTransform exposing (..)
import Lang exposing (..)

-- testPostfixRemoval inputCode expectedOutputCode =
--   Helpers.TestTemplates.testCodeTransform
--     (LangTransform.removeExtraPostfixes ["_orig", "'"])
--     inputCode
--     expectedOutputCode
--
--
-- testNoChange inputCode =
--   testPostfixRemoval inputCode inputCode
--
--
-- removeExtraPostfixesTests () =
--   [ \() -> testNoChange       "(let x       6 (+ x x))"
--   , \() -> testPostfixRemoval "(let x'      6 (+ x' x'))"            "(let x 6 (+ x x))"
--   , \() -> testPostfixRemoval "(let x_orig  6 (+ x_orig x_orig))"    "(let x 6 (+ x x))"
--   , \() -> testPostfixRemoval "(let x_orig' 6 (+ x_orig' x_orig'))"  "(let x 6 (+ x x))"
--   , \() -> testPostfixRemoval "(let x'_orig 6 (+ x'_orig x'_orig))"  "(let x 6 (+ x x))"
--   , \() -> testNoChange       "(let x'      6 (let x  7 (+ x x)))"
--   , \() -> testNoChange       "(let x       6 (let x' 7 (+ x' x')))"
--   ]


-- constVal n =
--   vConst


testEqnDistribute () =
  "ok"
  -- let eqn =
  --   EqnOp Mult
  --     [EqnVal
  --     , ]
  -- in
