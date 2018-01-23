module UpdateTests exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)

import Lang exposing (..)

import Utils
import Eval
import Syntax

import Results

type alias State = { numTests: Int, nthAssertion: Int, numSuccess: Int, numFailed: Int, currentName: String, errors: String }
init_state = State 0 0 0 0 "" ""
summary: State -> String
summary state =
  Debug.log (state.errors ++ "\n-------------------\n "++toString state.numSuccess ++"/" ++ toString state.numTests++ " tests passed\n-------------------") "ok"

test: String -> State -> State
test name state =
  --let _ = Debug.log name "testing" in
  --let res = body <| name in
  {state | nthAssertion = 1, currentName = name} -- Debug.log name "all tests passed"

success state = { state |
  numTests = state.numTests + 1,
  numSuccess = state.numSuccess + 1,
  nthAssertion = state.nthAssertion + 1
  }

fail state newError = { state |
  numTests = state.numTests + 1,
  numFailed = state.numFailed + 1,
  nthAssertion = state.nthAssertion + 1,
  errors = state.errors ++ "\n" ++ newError
  }

assertEqual: a -> a -> State  -> State
assertEqual x y state =
  if x == y then success state else fail state <| "[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++ "] Expected \n" ++
      toString y ++ ", got\n" ++ toString x

updateAssert: Env -> Exp -> Val -> Val -> Env -> Exp  -> State  -> State
updateAssert env exp origOut newOut expectedEnv expectedExp state =
  let expected = envToString expectedEnv ++ " |- " ++ unparse expectedExp in
  let problemdesc = ("\nFor problem:" ++
    envToString env ++ " |- " ++ unparse exp ++ " <-- " ++ valToString newOut ++
    " (was " ++ valToString origOut ++ ")") in
  case update env exp origOut newOut of
    Results.Oks (Results.LazyCons (envX, expX) _) ->
      let obtained = envToString envX ++ " |- " ++ unparse expX in
      if obtained == expected then success state else
        fail state <|
          "[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++
          "] Expected \n" ++ expected ++  ", got\n" ++ envToString envX ++ " |- " ++ unparse expX ++ problemdesc
    Results.Oks Results.LazyNil ->
       fail state <|
                   "[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++
                   "] Expected \n" ++ expected ++  ", got no solutions without error" ++ problemdesc
    Results.Errs msg ->
       fail state <|
                 "[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++
                 "] Expected \n" ++ expected ++  ", got\n" ++ msg ++ problemdesc

updateElmAssert: List (String, String) -> String -> String -> List (String, String) -> String -> State -> State
updateElmAssert envStr expStr newOutStr expectedEnvStr expectedExpStr state =
  case Utils.projOk [parseEnv envStr, parseEnv expectedEnvStr] of
    Err error -> fail state error
    Ok [env, expectedEnv] ->
       case Utils.projOk [parse expStr, parse newOutStr, parse expectedExpStr] of
           Err error -> fail state error
           Ok [exp, newOut, expectedExp] ->
             case Utils.projOk [evalEnv env exp, eval newOut] of
             Err error -> fail state error
             Ok [out, newOut] -> updateAssert env exp out newOut expectedEnv expectedExp state
             Ok _ -> fail state "???"
           Ok _ -> fail state "???"
    Ok _ -> fail state "???"

parse = Syntax.parser Syntax.Elm >> Result.mapError (\p -> toString p)
unparse = Syntax.unparser Syntax.Elm
evalEnv env exp = Eval.doEval Syntax.Elm env exp |> Result.map (Tuple.first >> Tuple.first)
eval exp = Eval.doEval Syntax.Elm [] exp |> Result.map (Tuple.first >> Tuple.first)

parseEnv: List (String, String) -> Result String Env
parseEnv envStr =
  envStr
  |> List.map (\(name, exp) ->
    parse exp
    |> Result.mapError (\pError -> toString pError)
    |> Result.andThen (\x ->
      evalEnv [] x
      |> Result.map (\x -> (name, x))))
  |> Utils.projOk
  |> Result.mapError (\pError -> toString pError)


dws = ws "  "
dws2 = ws "  "
dws1 = ws " "
dws3 = ws "   "
dws4 = ws "    "
dws5 = ws "     "
dws6 = ws "      "

val x = Val x (Provenance [] (tConst (ws "") 0) []) (Parents [])

tVal n = val <| (VConst Nothing (n, dummyTrace))
tVClosure maybeIndent pats body env = val <| VClosure maybeIndent pats body env
tVList vals = val <| VList vals
tVBool truth = val <| VBase (VBool truth)
tVStr chars = val <| VBase (VString chars)

tConst ws num = withDummyExpInfo <| EConst ws num dummyLoc noWidgetDecl
tBool space truth   = withDummyExpInfo <| EBase space (EBool truth)
tString space chars = withDummyExpInfo <| EBase space (EString defaultQuoteChar chars)
tVar space name =withDummyExpInfo <| EVar space name
tFun sp0 pats body sp1 = (withDummyExpInfo <| EFun sp0 pats body sp1)
tApp sp0 fun args sp1 = withDummyExpInfo <| EApp sp0 fun args sp1
tList sp0 exps sp1 = withDummyExpInfo <| EList sp0 exps (ws "") Nothing sp1
--tListCons sp0 exps sp1 tail sp2 = EList sp0 exps sp1 (Just tail) sp2

tPVar space name = withDummyPatInfo <| PVar space name noWidgetDecl
tPAs sp0 name sp1 pat= withDummyPatInfo <| PAs sp0 name sp1 pat
tPList sp0 listPat sp1= withDummyPatInfo <| PList sp0 listPat (ws "") Nothing sp1
tPListCons sp0 listPat sp1 tailPat sp2 = withDummyPatInfo <| PList sp0 listPat sp1 (Just tailPat) sp1

all_tests = init_state
  {--}
  |> test "triCombineTest"
  |> assertEqual
      (triCombine [("x", (tVal 1)), ("y", (tVal 1)), ("z", (tVal 1))]
                  [("x", (tVal 1)), ("y", (tVal 2)), ("z", (tVal 2))]
                  [("x", (tVal 3)), ("y", (tVal 1)), ("z", (tVal 3))]
                 )[("x", (tVal 3)), ("y", (tVal 2)), ("z", (tVal 2))]
  |> test "update const"
  |> updateElmAssert [] "   1"   "2"
                     [] "   2"
  |> test "update boolean and strings"
  |> updateElmAssert [] "   True"   "False"
                     [] "   False"
  |> updateElmAssert [] "   'Hello'"   "'World'"
                     [] "   'World'"
  |> test "update var"
  |> updateElmAssert [("x", "1")] "x"   "2"
                     [("x", "2")] "x"
  |> updateElmAssert [("x", "1"), ("y", "3")] " x"   "2"
                     [("x", "2"), ("y", "3")] " x"
  |> updateElmAssert [("y", "3"), ("x", "1")] "  x"   "2"
                     [("y", "3"), ("x", "2")] "  x"
  |> test "update fun"
  |> updateElmAssert [] "\\  x ->   1"   "\\  x ->   2"
                     [] "\\  x ->   2"
  |> test "update fun with 2 arguments"
  |> updateElmAssert [] "\\x y -> 1" "\\y x -> 2"
                     [] "\\y x -> 2"
  |> test "update nested fun with 2 and 1 arguments"
  |> updateElmAssert [] "\\x y -> \\z -> 1" "\\y z -> \\x -> 3"
                     [] "\\y z -> \\x -> 3"
  |> test "update app identity"
  |> updateElmAssert [] "(\\x -> x)   1" "2"
                     [] "(\\x -> x)   2"
  |> test "update app constant"
  |> updateElmAssert [] "(\\x -> 1)   3" "2"
                     [] "(\\x -> 2)   3"
  |> test "update pattern 'as' (\\x as y -> x [or y]) 1"
  |> updateElmAssert [] "(\\(x  as y) -> x)   1" "2"
                     [] "(\\(x  as y) -> x)   2"
  |> updateElmAssert [] "(\\(x  as y) -> y)   1" "2"
                     [] "(\\(x  as y) -> y)   2"
  |> test "update pattern list (\\[x, y] -> x or y) [1, 2]"
  |> updateElmAssert [] "(\\[x,  y] -> x)   [1, 2]" "3"
                     [] "(\\[x,  y] -> x)   [3, 2]"
  |> updateElmAssert [] "(\\[x,  y] -> y)   [1, 2]" "3"
                     [] "(\\[x,  y] -> y)   [1, 3]"
  |> test "update pattern list with tail (\\[x | y] -> x or y) [1, 2]"
  --|> updateElmAssert [] "(\\[x | y] -> x)   [1, 2]" "3"
  --                   [] "(\\[x | y] -> x)   [3, 2]"
    |> updateAssert
                    [] (tApp dws5
              (tFun dws1 [tPListCons dws2 [tPVar dws3 "x"] dws1 (tPVar dws4 "y") dws1] (tVar dws6 "x") dws3) [tList dws [(tConst dws 1), (tConst dws 2)] dws] dws6) (tVal 1) (tVal 3)
                    [] (tApp dws5
              (tFun dws1 [tPListCons dws2 [tPVar dws3 "x"] dws1 (tPVar dws4 "y") dws1] (tVar dws6 "x") dws3) [tList dws [(tConst dws 3), (tConst dws 2)] dws] dws6)
    |> updateAssert
                    [] (tApp dws5
              (tFun dws1 [tPListCons dws2 [tPVar dws3 "x"] dws1 (tPVar dws4 "y") dws1] (tVar dws6 "y") dws3) [tList dws [(tConst dws 1), (tConst dws 2)] dws] dws6) (tVList [tVal 2]) (tVList [tVal 3])
                    [] (tApp dws5
              (tFun dws1 [tPListCons dws2 [tPVar dws3 "x"] dws1 (tPVar dws4 "y") dws1] (tVar dws6 "y") dws3) [tList dws [(tConst dws 1), (tConst dws 3)] dws] dws6)

  |> test "update app (\\x y -> x) 1 2"
  |> updateElmAssert [] "(\\x y -> x) 1 2" "3"
                     [] "(\\x y -> x) 3 2"
  |> test "update app (\\x y -> y) 1 2"
  |> updateElmAssert
    [] "(\\x y -> y) 1 2" "3"
    [] "(\\x y -> y) 1 3"
  |> test "update if-then-else"
  |> updateElmAssert
    [] "if True then 1 else 2" "3"
    [] "if True then 3 else 2"
  |> updateElmAssert
    [] "if False then 1 else 2" "3"
    [] "if False then 1 else 3"
  |> summary
