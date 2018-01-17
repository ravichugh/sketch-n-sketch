module UpdateTests exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)

import Lang exposing (..)
import LangUnparser exposing (..)


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


assertEqual: a -> a -> State  -> State
assertEqual x y state =
  if x == y then { state |
    numTests = state.numTests + 1,
    numSuccess = state.numSuccess + 1,
    nthAssertion = state.nthAssertion + 1
  } else { state |
    numTests = state.numTests + 1,
    numFailed = state.numFailed + 1,
    nthAssertion = state.nthAssertion + 1,
    errors = state.errors ++ "\n[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++ "] Expected \n" ++
      toString y ++ ", got\n" ++ toString x
  }

assertEqualU: Result String (Env, Exp) -> Result String (Env, Exp)  -> State  -> State
assertEqualU x y state =
  if x == y then { state |
    numTests = state.numTests + 1,
    numSuccess = state.numSuccess + 1,
    nthAssertion = state.nthAssertion + 1
  } else { state |
    numTests = state.numTests + 1,
    numFailed = state.numFailed + 1,
    nthAssertion = state.nthAssertion + 1,
    errors = state.errors ++ "\n[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++ "] Expected \n" ++
      case (x, y) of
        (Ok (envX, expX), Ok (envY, expY)) -> envToString envY ++ " |- " ++ unparse expY ++
          ", got\n" ++ envToString envX ++ " |- " ++ unparse expX
        (Err msg, Ok (envY, expY)) -> envToString envY ++ " |- " ++ unparse expY ++
          ", got\n" ++ msg
        _ -> toString y ++ ", got\n" ++ toString x
  }

dws = ws "  "
dws2 = ws "  "
dws1 = ws " "
dws3 = ws "   "
dws4 = ws "    "
dws5 = ws "     "
dws6 = ws "      "

tVal n = vConst (n, dummyTrace)
tVClosure maybeIndent pats body env = val <| VClosure maybeIndent pats body env
tVList vals = val <| VList vals

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
--tPListCons sp0 listPat sp1 tailPat sp2 = withDummyPatInfo <| PList sp0 listPat sp1 (Just tailPat) sp1

all_tests = init_state
  {--}
  |> test "triCombineTest"
  |> assertEqual
      (triCombine [("x", (tVal 1)), ("y", (tVal 1)), ("z", (tVal 1))]
                  [("x", (tVal 1)), ("y", (tVal 2)), ("z", (tVal 2))]
                  [("x", (tVal 3)), ("y", (tVal 1)), ("z", (tVal 3))]
                 )[("x", (tVal 3)), ("y", (tVal 2)), ("z", (tVal 2))]
  |> test "update const"
      |> assertEqualU
          (update [] (tConst dws 1) (tVal 1) (tVal 2))
          (Ok    ([], tConst dws 2))
  |> test "update boolean and strings"
      |> assertEqualU
          (update [] (tBool dws True) vTrue vFalse)
          (Ok    ([], tBool dws False))
      |> assertEqualU
          (update [] (tString dws "Hello") (vStr "Hello") (vStr "World"))
          (Ok    ([], tString dws "World"))
  |> test "update var"
      |> assertEqualU
          (update [("x", (tVal 1))] (tVar dws "x") (tVal 1) (tVal 2))
          (Ok    ([("x", (tVal 2))], tVar dws "x"))
      |> assertEqualU
          (update [("x", (tVal 1)), ("y", (tVal 3))] (tVar dws "x") (tVal 1) (tVal 2))
          (Ok    ([("x", (tVal 2)), ("y", (tVal 3))], tVar dws "x"))
      |> assertEqualU
          (update [("y", (tVal 3)), ("x", (tVal 1))] (tVar dws "x") (tVal 1) (tVal 2))
          (Ok    ([("y", (tVal 3)), ("x", (tVal 2))], tVar dws "x"))
  |> test "update fun"
        |> assertEqualU
          (update [] (tFun dws [tPVar dws1 "x"] (tConst dws2 1) dws3) (tVClosure Nothing (tPVar dws1 "x") (tConst dws2 1) [])
                                                                       (tVClosure Nothing (tPVar dws1 "y") (tConst dws2 2) []))
          (Ok    ([], tFun dws [tPVar dws1 "y"] (tConst dws2 2) dws3))
  |> test "update fun with 2 arguments"
          |> assertEqualU
            (update [] (tFun dws [tPVar dws1 "x", tPVar dws1 "y"] (tConst dws2 1) dws3) (tVClosure Nothing (tPVar dws1 "x") (tFun dws [tPVar dws1 "y"] (tConst dws2 1) dws) [])
                                                                                        (tVClosure Nothing (tPVar dws1 "y") (tFun dws [tPVar dws1 "x"] (tConst dws2 2) dws) []))
            (Ok    ([], tFun dws [tPVar dws1 "y", tPVar dws1 "x"] (tConst dws2 2) dws3))
  |> test "update nested fun with 2 and 1 arguments"
          |> assertEqualU
            (update [] (tFun dws [tPVar dws1 "x", tPVar dws1 "y"] (tFun dws [tPVar dws1 "z"] (tConst dws2 1) dws3) dws3)
                            (tVClosure Nothing (tPVar dws1 "x") (tFun dws [tPVar dws1 "y"] (tFun dws [tPVar dws1 "z"] (tConst dws2 1) dws3) dws) [])
                            (tVClosure Nothing (tPVar dws1 "y") (tFun dws [tPVar dws1 "z"] (tFun dws [tPVar dws1 "x"] (tConst dws2 3) dws3) dws) []))
            (Ok    ([], tFun dws [tPVar dws1 "y", tPVar dws1 "z"] (tFun dws [tPVar dws1 "x"] (tConst dws2 3) dws3) dws3))
  |> test "update app (\\x -> x) 1"
      |> assertEqualU
        (update [] (tApp dws5 (tFun dws1 [tPVar dws4 "x"] (tVar dws6 "x") dws3) [tConst dws 1] dws6) (tVal 1) (tVal 2))
        (Ok    ([], tApp dws5 (tFun dws1 [tPVar dws4 "x"] (tVar dws6 "x") dws3) [tConst dws 2] dws6))
  |> test "update app (\\x -> 1) 3"
      |> assertEqualU
        (update [] (tApp dws5
          (tFun dws1 [tPVar dws4 "x"] (tConst dws2 1) dws3) [tConst dws 3] dws6) (tVal 1) (tVal 2))
        (Ok ([], tApp dws5
          (tFun dws1 [tPVar dws4 "x"] (tConst dws2 2) dws3) [tConst dws 3] dws6))
  |> test "update pattern 'as' (\\x as y -> x [or y]) 1"
      |> assertEqualU
              (update [] (tApp dws5
                (tFun dws1 [tPAs dws2 "x" dws3 (tPVar dws4 "y")] (tVar dws6 "x") dws3) [tConst dws 1] dws6) (tVal 1) (tVal 2))
              (Ok ([], tApp dws5
                (tFun dws1 [tPAs dws2 "x" dws3 (tPVar dws4 "y")] (tVar dws6 "x") dws3) [tConst dws 2] dws6))
      |> assertEqualU
            (update [] (tApp dws5
              (tFun dws1 [tPAs dws2 "x" dws3 (tPVar dws4 "y")] (tVar dws6 "y") dws3) [tConst dws 1] dws6) (tVal 1) (tVal 2))
            (Ok ([], tApp dws5
              (tFun dws1 [tPAs dws2 "x" dws3 (tPVar dws4 "y")] (tVar dws6 "y") dws3) [tConst dws 2] dws6))
  |> test "update pattern list (\\[x, y] -> x or y) [1, 2]"
      |> assertEqualU
              (update [] (tApp dws5
                (tFun dws1 [tPList dws2 [tPVar dws3 "x", tPVar dws4 "y"] dws1] (tVar dws6 "x") dws3) [tList dws [(tConst dws 1), (tConst dws 2)] dws] dws6) (tVal 1) (tVal 3))
              (Ok    ([], tApp dws5
                (tFun dws1 [tPList dws2 [tPVar dws3 "x", tPVar dws4 "y"] dws1] (tVar dws6 "x") dws3) [tList dws [(tConst dws 3), (tConst dws 2)] dws] dws6))
      |> assertEqualU
              (update [] (tApp dws5
                (tFun dws1 [tPList dws2 [tPVar dws3 "x", tPVar dws4 "y"] dws1] (tVar dws6 "y") dws3) [tList dws [(tConst dws 1), (tConst dws 2)] dws] dws6) (tVal 2) (tVal 3))
              (Ok    ([], tApp dws5
                (tFun dws1 [tPList dws2 [tPVar dws3 "x", tPVar dws4 "y"] dws1] (tVar dws6 "y") dws3) [tList dws [(tConst dws 1), (tConst dws 3)] dws] dws6))

  {--
  |> test "update app (\\x y -> x) 1 2"
        |> assertEqualU
          (update [] (tApp dws5
            (tFun dws1 [tPVar dws4 "x", tPVar dws4 "y"] (tVar dws6 "x") dws3) [tConst dws 1, tConst dws 2] dws6) (tVal 1) (tVal 3))
          (Ok ([], tApp dws5
            (tFun dws1 [tPVar dws4 "x", tPVar dws4 "y"] (tVar dws6 "x") dws3) [tConst dws 3, tConst dws 2] dws6))
  |> test "update app (\\x y -> y) 1 2"
        |> assertEqualU
          (update [] (tApp dws5
            (tFun dws1 [tPVar dws4 "x", tPVar dws4 "y"] (tVar dws6 "x") dws3) [tConst dws 1, tConst dws 2] dws6) (tVal 2) (tVal 3))
          (Ok ([], tApp dws5
            (tFun dws1 [tPVar dws4 "x", tPVar dws4 "y"] (tVar dws6 "x") dws3) [tConst dws 1, tConst dws 3] dws6))
   --}
  |> summary
