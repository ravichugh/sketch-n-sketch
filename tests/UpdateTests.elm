module UpdateTests exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)

import Lang exposing (..)

assertEqual x y z =
  if x == y then z else Debug.crash <| "[" ++ z ++ "] Expected " ++ toString x ++ " == " ++ toString y

vOne = vConst (1, dummyTrace)
vTwo = vConst (2, dummyTrace)
vThree = vConst (3, dummyTrace)

dws = ws "  "

test name body count =
  --let _ = Debug.log name "testing" in
  let res = body <| name in
  count + 1 -- Debug.log name "all tests passed"

summary: Int -> String
summary count =
  Debug.log ("-------------------\nAll "++toString count++" tests passed\n-------------------") "ok"


all_tests = 0
  |> test "triCombineTest" (\testing-> testing
      |> assertEqual
          [("x", vThree), ("y", vTwo), ("z", vTwo)]
          (triCombine [("x", vOne), ("y", vOne), ("z", vOne)]
                    [("x", vOne), ("y", vTwo), ("z", vTwo)]
                    [("x", vThree), ("y", vOne), ("z", vThree)])
    )
  |> test "update const" (\testing -> testing
      |> assertEqual
          (update [] (EConst dws 1 dummyLoc noWidgetDecl) vOne vTwo)
          (Ok ([], EConst dws 2 dummyLoc noWidgetDecl))
    )
  |> test "update boolean and strings" (\testing -> testing
      |> assertEqual
          (update [] (EBase dws (EBool True)) vTrue vFalse)
          (Ok ([], EBase dws (EBool False)))
      |> assertEqual
          (update [] (EBase dws (EString defaultQuoteChar "Hello")) (vStr "Hello") (vStr "World"))
          (Ok ([], EBase dws (EString defaultQuoteChar "World")))
    )
  |> test "update var" (\testing -> testing
      |> assertEqual
          (update [("x", vOne)] (EVar dws "x") vOne vTwo)
          (Ok ([("x", vTwo)], EVar dws "x"))
      |> assertEqual
          (update [("x", vOne), ("y", vThree)] (EVar dws "x") vOne vTwo)
          (Ok ([("x", vTwo), ("y", vThree)], EVar dws "x"))
      |> assertEqual
          (update [("y", vThree), ("x", vOne)] (EVar dws "x") vOne vTwo)
          (Ok ([("y", vThree), ("x", vTwo)], EVar dws "x"))
    )
  |> test "update" (\testing -> testing
      |> assertEqual
        (update [("x", vOne)] (EVar dws "x") vOne vTwo)
        (Ok ([("x", vTwo)], EVar dws "x"))
    )
  |> summary
