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


vOne = vConst (1, dummyTrace)
vTwo = vConst (2, dummyTrace)
vThree = vConst (3, dummyTrace)

dws = ws "  "
dws1 = ws " "
dws3 = ws "   "
dws4 = ws "    "
dws5 = ws "    "
dws6 = ws "    "

const ws num = withDummyExpInfo <| EConst ws num dummyLoc noWidgetDecl


test name body count =
  --let _ = Debug.log name "testing" in
  let res = body <| name in
  count + 1 -- Debug.log name "all tests passed"

summary: Int -> String
summary count =
  Debug.log ("-------------------\nAll "++toString count++" tests passed\n-------------------") "ok"

all_tests = 0
  {--}
   |> test "triCombineTest" (\testing-> testing
      |> assertEqual
          (triCombine [("x", vOne), ("y", vOne), ("z", vOne)]
                    [("x", vOne), ("y", vTwo), ("z", vTwo)]
                    [("x", vThree), ("y", vOne), ("z", vThree)])
          [("x", vThree), ("y", vTwo), ("z", vTwo)]
    )
  |> test "update const" (\testing -> testing
      |> assertEqualU
          (update [] (const dws 1) vOne vTwo)
          (Ok ([], const dws 2))
    )
  |> test "update boolean and strings" (\testing -> testing
      |> assertEqualU
          (update [] (withDummyExpInfo <| EBase dws (EBool True)) vTrue vFalse)
          (Ok ([], withDummyExpInfo <| EBase dws (EBool False)))
      |> assertEqualU
          (update [] (withDummyExpInfo <| EBase dws (EString defaultQuoteChar "Hello")) (vStr "Hello") (vStr "World"))
          (Ok ([], withDummyExpInfo <| EBase dws (EString defaultQuoteChar "World")))
    )
  |> test "update var" (\testing -> testing
      |> assertEqualU
          (update [("x", vOne)] (withDummyExpInfo <| EVar dws "x") vOne vTwo)
          (Ok ([("x", vTwo)], withDummyExpInfo <| EVar dws "x"))
      |> assertEqualU
          (update [("x", vOne), ("y", vThree)] (withDummyExpInfo <| EVar dws "x") vOne vTwo)
          (Ok ([("x", vTwo), ("y", vThree)], withDummyExpInfo <| EVar dws "x"))
      |> assertEqualU
          (update [("y", vThree), ("x", vOne)] (withDummyExpInfo <| EVar dws "x") vOne vTwo)
          (Ok ([("y", vThree), ("x", vTwo)], withDummyExpInfo <| EVar dws "x"))
    )
  |> test "update app (\\x -> x) 1" (\testing -> testing
      |> assertEqualU
        (update [] (withDummyExpInfo <| EApp dws5
          (withDummyExpInfo <| EFun dws1 [withDummyPatInfo <| PVar dws4 "x" noWidgetDecl] (eVar "x") dws3) [const dws 1] dws6) vOne vTwo)
        (Ok ([], withDummyExpInfo <| EApp dws5
          (withDummyExpInfo <| EFun dws1 [withDummyPatInfo <| PVar dws4 "x" noWidgetDecl] (eVar "x") dws3) [const dws 2] dws6))
    ) --}
  |> test "update app (\\x -> 1) 3" (\testing -> testing
      |> assertEqualU
        (update [] (withDummyExpInfo <| EApp dws5
          (withDummyExpInfo <| EFun dws1 [withDummyPatInfo <| PVar dws4 "x" noWidgetDecl] (eConst 1 dummyLoc) dws3) [const dws 3] dws6) vOne vTwo)
        (Ok ([], withDummyExpInfo <| EApp dws5
          (withDummyExpInfo <| EFun dws1 [withDummyPatInfo <| PVar dws4 "x" noWidgetDecl] (eConst 2 dummyLoc) dws3) [const dws 3] dws6))
    )
  |> summary
