module UpdateTests exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)

import Lang exposing (..)
import LangUnparser exposing (..)

assertEqual x y z =
  if x == y then z else Debug.crash <| "[" ++ z ++ "] Expected \n" ++ toString y ++ ", got\n" ++ toString x

envToString: Env -> String
envToString env =
  case env of
    [] -> ""
    (v, value)::tail -> v ++ "->" ++ unparse (val_to_exp (ws "") value) ++ " " ++ (envToString tail)

assertEqualU x y z =
  if x == y then z else
    case (x, y) of
      (Ok (envX, expX), Ok (envY, expY)) ->
        Debug.crash <| "[" ++ z ++ "] Expected \n" ++ envToString envX ++ " |- " ++ unparse expY ++ ", got\n" ++ envToString envY ++ " |- " ++ unparse expX
      _ -> Debug.crash <| "[" ++ z ++ "] Expected \n" ++ toString y ++ ", got\n" ++ toString x


dws = ws "  "
dws2 = ws "  "
dws1 = ws " "
dws3 = ws "   "
dws4 = ws "    "
dws5 = ws "     "
dws6 = ws "      "


test name body count =
  --let _ = Debug.log name "testing" in
  let res = body <| name in
  count + 1 -- Debug.log name "all tests passed"

summary: Int -> String
summary count =
  Debug.log ("-------------------\nAll "++toString count++" tests passed\n-------------------") "ok"


tVal n = vConst (n, dummyTrace)
tConst ws num = withDummyExpInfo <| EConst ws num dummyLoc noWidgetDecl
tBool space truth   = withDummyExpInfo <| EBase space (EBool truth)
tString space chars = withDummyExpInfo <| EBase space (EString defaultQuoteChar chars)
tVar space name =withDummyExpInfo <| EVar space name
tFun sp0 pats body sp1 = (withDummyExpInfo <| EFun sp0 pats body sp1)
tPVar space name = withDummyPatInfo <| PVar space name noWidgetDecl
tApp sp0 fun args sp1 = withDummyExpInfo <| EApp sp0 fun args sp1


all_tests = 0
  {--}
   |> test "triCombineTest" (\testing-> testing
      |> assertEqual
          (triCombine [("x", (tVal 1)), ("y", (tVal 1)), ("z", (tVal 1))]
                      [("x", (tVal 1)), ("y", (tVal 2)), ("z", (tVal 2))]
                      [("x", (tVal 3)), ("y", (tVal 1)), ("z", (tVal 3))]
                     )[("x", (tVal 3)), ("y", (tVal 2)), ("z", (tVal 2))]
    )
  |> test "update const" (\testing -> testing
      |> assertEqualU
          (update [] (tConst dws 1) (tVal 1) (tVal 2))
          (Ok    ([], tConst dws 2))
    )
  |> test "update boolean and strings" (\testing -> testing
      |> assertEqualU
          (update [] (tBool dws True) vTrue vFalse)
          (Ok    ([], tBool dws False))
      |> assertEqualU
          (update [] (tString dws "Hello") (vStr "Hello") (vStr "World"))
          (Ok    ([], tString dws "World"))
    )
  |> test "update var" (\testing -> testing
      |> assertEqualU
          (update [("x", (tVal 1))] (tVar dws "x") (tVal 1) (tVal 2))
          (Ok    ([("x", (tVal 2))], tVar dws "x"))
      |> assertEqualU
          (update [("x", (tVal 1)), ("y", (tVal 3))] (tVar dws "x") (tVal 1) (tVal 2))
          (Ok    ([("x", (tVal 2)), ("y", (tVal 3))], tVar dws "x"))
      |> assertEqualU
          (update [("y", (tVal 3)), ("x", (tVal 1))] (tVar dws "x") (tVal 1) (tVal 2))
          (Ok    ([("y", (tVal 3)), ("x", (tVal 2))], tVar dws "x"))
    )
  |> test "update app (\\x -> x) 1" (\testing -> testing
      |> assertEqualU
        (update [] (tApp dws5 (tFun dws1 [tPVar dws4 "x"] (tVar dws6 "x") dws3) [tConst dws 1] dws6) (tVal 1) (tVal 2))
        (Ok    ([], tApp dws5 (tFun dws1 [tPVar dws4 "x"] (tVar dws6 "x") dws3) [tConst dws 2] dws6))
    ) --}
  |> test "update app (\\x -> 1) 3" (\testing -> testing
      |> assertEqualU
        (update [] (tApp dws5
          (tFun dws1 [tPVar dws4 "x"] (tConst dws2 1) dws3) [tConst dws 3] dws6) (tVal 1) (tVal 2))
        (Ok ([], tApp dws5
          (tFun dws1 [tPVar dws4 "x"] (tConst dws2 2) dws3) [tConst dws 3] dws6))
    )
  |> summary
