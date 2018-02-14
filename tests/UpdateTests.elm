module UpdateTests exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)

import Lang exposing (..)

import Utils
import Eval
import Syntax
import Lazy
import Results

type alias State = { numTests: Int, nthAssertion: Int, numSuccess: Int, numFailed: Int, currentName: String, errors: String, ignore: Bool }
init_state = State 0 0 0 0 "" "" False
summary: State -> String
summary state =
  Debug.log (state.errors ++ "\n-------------------\n "++toString state.numSuccess ++"/" ++ toString state.numTests++ " tests passed\n-------------------") "ok"

test: String -> State -> State
test name state =
  --let _ = Debug.log name " [Start]" in
  --let res = body <| name in
  {state | nthAssertion = 1, currentName = name} -- Debug.log name "all tests passed"

ignore: Bool -> State -> State
ignore b state =
  {state | ignore = b}

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

log state msg =
  "[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++ "] " ++ msg

assertEqual: a -> a -> State  -> State
assertEqual x y state =
  if state.ignore then state else
  if x == y then success state else fail state <| "[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++ "] Expected \n" ++
      toString y ++ ", got\n" ++ toString x

updateAssert: Env -> Exp -> Val -> Val -> Env -> String  -> State  -> State
updateAssert env exp origOut newOut expectedEnv expectedExpStr state =
  if state.ignore then state else
  let expected = envToString expectedEnv ++ " |- " ++ expectedExpStr in
  let problemdesc = ("\nFor problem:" ++
    envToString env ++ " |- " ++ unparse exp ++ " <-- " ++ valToString newOut ++
    " (was " ++ valToString origOut ++ ")") in
  case update env exp origOut (Update.Raw newOut) Results.LazyNil of
    Results.Oks (Results.LazyCons (envX, expX) lazyTail) ->
      let obtained = envToString envX ++ " |- " ++ unparse expX in
      if obtained == expected then success state else
        case Lazy.force lazyTail of
          Results.LazyCons (envX2, expX2) lazyTail2 ->
            let obtained2 = envToString envX2 ++ " |- " ++ unparse expX2 in
            if obtained2 == expected then success state else
            fail state <|
              log state <| "Expected \n" ++ expected ++  ", got\n" ++ obtained ++ " and " ++ obtained2 ++ problemdesc
          Results.LazyNil ->
            fail state <|
              log state <| "Expected \n" ++ expected ++  ", got\n" ++ obtained ++ problemdesc
    Results.Oks Results.LazyNil ->
       fail state <| log state <| "Expected \n" ++ expected ++  ", got no solutions without error" ++ problemdesc
    Results.Errs msg ->
       fail state <| log state <| "Expected \n" ++ expected ++  ", got\n" ++ msg ++ problemdesc

updateElmAssert: List (String, String) -> String -> String -> List (String, String) -> String -> State -> State
updateElmAssert envStr expStr newOutStr expectedEnvStr expectedExpStr state =
  if state.ignore then state else
  case Utils.projOk [parseEnv envStr, parseEnv expectedEnvStr] of
    Err error -> fail state <| log state <| "Error while parsing environments: " ++ error
    Ok [env, expectedEnv] ->
       case Utils.projOk [parse expStr, parse newOutStr] of
           Err error -> fail state <| log state <| "Error while parsing expressions or outputs: " ++ error
           Ok [exp, newOut] ->
             --let _ = Debug.log (log state <| toString exp) () in
             case Utils.projOk [evalEnv env exp, eval newOut] of
             Err error -> fail state <| log state <| "Error while evaluating the expression or the output: " ++ toString exp ++ "," ++ toString newOut ++ ": " ++ error
             Ok [out, newOut] -> updateAssert env exp out newOut expectedEnv expectedExpStr state
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
tApp sp0 fun args sp1 = withDummyExpInfo <| EApp sp0 fun args SpaceApp sp1
tList sp0 exps sp1 = withDummyExpInfo <| EList sp0 (List.map (\e -> (ws "", e)) exps) (ws "") Nothing sp1
--tListCons sp0 exps sp1 tail sp2 = EList sp0 exps sp1 (Just tail) sp2

tPVar space name = withDummyPatInfo <| PVar space name noWidgetDecl
tPAs sp0 name sp1 pat= withDummyPatInfo <| PAs sp0 name sp1 pat
tPList sp0 listPat sp1= withDummyPatInfo <| PList sp0 listPat (ws "") Nothing sp1
tPListCons sp0 listPat sp1 tailPat sp2 = withDummyPatInfo <| PList sp0 listPat sp1 (Just tailPat) sp1

all_tests = init_state
  |> test "triCombineTest"
  --|> ignore True
  --|> ignore False
  |> assertEqual
      (triCombine (tList space0  [tVar space0 "x", tVar space0 "y"] space0)
                  [("y", (tVal 2)), ("x", (tVal 1))]
                  [("y", (tVal 2)), ("x", (tVal 1))]
                  [("y", (tVal 2)), ("x", (tVal 3))]
                 )[("y", (tVal 2)), ("x", (tVal 3))]
  |> assertEqual
      (triCombine (tList space0  [tVar space0 "x", tVar space0 "y", tVar space0 "z"] space0)
                  [("x", (tVal 1)), ("y", (tVal 1)), ("z", (tVal 1))]
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
  |> test "update pattern 'as' (\\(x as y) -> x [or y]) 1"
  |> updateElmAssert [] "(\\x  as y -> x)   1" "2"
                     [] "(\\x  as y -> x)   2"
  |> updateElmAssert [] "(\\(x  as y) -> y)   1" "2"
                     [] "(\\(x  as y) -> y)   2"
  |> test "update pattern list (\\[x, y] -> x or y) [1, 2]"
  |> updateElmAssert [] "(\\[x,  y] -> x)   [1, 2]" "3"
                     [] "(\\[x,  y] -> x)   [3, 2]"
  |> updateElmAssert [] "(\\[x,  y] -> y)   [1, 2]" "3"
                     [] "(\\[x,  y] -> y)   [1, 3]"
  |> test "update pattern list with tail (\\[x | [y]] -> x or y) [1, 2]"
  |> updateElmAssert [] "(\\x :: y -> x)   [1, 2]" "3"
                     [] "(\\x :: y -> x)   [3, 2]"
  |> updateElmAssert [] "(\\x :: [y] -> y)   [1, 2]" "3"
                     [] "(\\x :: [y] -> y)   [1, 3]"
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
  |> test "String concatenation"
    |> updateElmAssert [] "'Hello'  + 'world'" "'Hello world'"
                       [] "'Hello '  + 'world'"
  |> test "update arithmetic operations"
      |> updateElmAssert
        [("x", "1"), ("y", "2")] "  x+ y" "4"
        [("x", "2"), ("y", "2")] "  x+ y"
      |> updateElmAssert
        [("x", "1"), ("y", "2")] "  x+ y" "4"
        [("x", "1"), ("y", "3")] "  x+ y"
      |> updateElmAssert
        [("x", "5"), ("y", "2")] "  x- y" "1"
        [("x", "3"), ("y", "2")] "  x- y"
      |> updateElmAssert
        [("x", "2"), ("y", "3")] "  x* y" "8"
        [("x", "2"), ("y", "4")] "  x* y"
      |> updateElmAssert
        [("x", "18"), ("y", "3")] "  x/ y" "7"
        [("x", "21"), ("y", "3")] "  x/ y"
      |> updateElmAssert
        [("x", "3"), ("y", "4")] " pow x   y" "16"
        [("x", "2"), ("y", "4")] " pow x   y"
      |> updateElmAssert
        [("x", "-1"), ("y", "3")] " pow x   y" "1"
        [("x", "1"), ("y", "3")] " pow x   y"
      |> updateElmAssert
          [("x", "-2"), ("y", "3")] " pow x   y" "27"
          [("x", "3"), ("y", "3")] " pow x   y"
      |> updateElmAssert
        [("x", "-1"), ("y", "0")] " pow x   y" "-1"
        [("x", "-1"), ("y", "1")] " pow x   y"
      |> updateElmAssert
        [("x", "-1"), ("y", "3")] " pow x   y" "-8"
        [("x", "-2"), ("y", "3")] " pow x   y"
      |> updateElmAssert
        [("x", "17"), ("y", "8")] " mod x  y" "3"
        [("x", "19"), ("y", "8")] " mod x  y"
      |> updateElmAssert
        [("x", "1"), ("y", "0")] "arctan2 y x" "1.5707963267948966"
        [("x", "6.123233995736766e-17"), ("y", "1")] "arctan2 y x"
      |> updateElmAssert
        [("x", "1"), ("y", "0")] "arctan2 y x" "3.141592653589793"
        [("x", "-1"), ("y", "1.2246467991473532e-16")] "arctan2 y x"
      |> updateElmAssert
        [("x", "1"), ("y", "0")] "arctan2 y x" "-1.5707963267948966"
        [("x", "6.123233995736766e-17"), ("y", "-1")] "arctan2 y x"
      |> updateElmAssert
        [("x", "0.1")] "cos x" "0"
        [("x", "1.570796326794897")] "cos x"
      |> updateElmAssert
        [("x", "-0.1")] "cos x" "0"
        [("x", "-1.570796326794897")] "cos x"
      |> updateElmAssert
        [("x", "0.1")] "cos x" "0"
        [("x", "1.570796326794897")] "cos x"
      |> updateElmAssert
        [("x", "0.1")] "sin x" "1"
        [("x", "1.570796326794897")] "sin x"
      |> updateElmAssert
        [("x", "-0.1")] "sin x" "-1"
        [("x", "-1.570796326794897")] "sin x"
      |> updateElmAssert
        [("x", "0")] "arccos x" "0"
        [("x", "1")] "arccos x"
      |> updateElmAssert
        [("x", "0")] "arcsin x" "1.5707963267948966"
        [("x", "1")] "arcsin x"
      |> updateElmAssert
        [("x", "17.5")] "floor x" "15"
        [("x", "15.5")] "floor x"
      |> updateElmAssert
        [("x", "17.5")] "ceiling x" "15"
        [("x", "14.5")] "ceiling x"
      |> updateElmAssert
        [("x", "17.75")] "round x" "15"
        [("x", "14.75")] "round x"
      |> updateElmAssert
        [("x", "17.25")] "round x" "15"
        [("x", "15.25")] "round x"
      |> updateElmAssert
        [("x", "16")] "sqrt x" "3"
        [("x", "9")] "sqrt x"
  |> test "case of calls"
      |> updateElmAssert
        [("x", "[7, 1]")] "case x of\n  [a, b] -> a + b;\n  u -> 0;" "5"
        [("x", "[4, 1]")] "case x of\n  [a, b] -> a + b;\n  u -> 0;"
      |> updateElmAssert
        [("x", "[7]")] "case x of\n  [a, b] -> a + b;\n  u -> 0;" "5"
        [("x", "[7]")] "case x of\n  [a, b] -> a + b;\n  u -> 5;"
  |> test "non-rec let"
      |> updateElmAssert
        [] "let   x= 5 in\nlet y  =2  in [x, y]" "[6, 3]"
        [] "let   x= 6 in\nlet y  =3  in [x, y]"
        --}
  |> test "list constructor"
      |> updateElmAssert
        [] "let   x= 1 in\nlet y  =[2]  in x :: x :: y" "[3, 1, 2]"
        [] "let   x= 3 in\nlet y  =[2]  in x :: x :: y"
      |> updateElmAssert
        [] "let   x= 1 in\nlet y  =2  in [x, x, y]" "[3, 1, 2]"
        [] "let   x= 3 in\nlet y  =2  in [x, x, y]"
  --|> ignore False
      |> updateElmAssert
        [] "let   x= 1 in\nlet y  =2  in [x, x, y]" "[1, 3, 2]"
        [] "let   x= 3 in\nlet y  =2  in [x, x, y]"
  --|> ignore True
      |> updateElmAssert
        [] "let   x= 1 in\nlet y  =2  in [x, x, y]" "[1, 1, 3]"
        [] "let   x= 1 in\nlet y  =3  in [x, x, y]"
      |> updateElmAssert
        [] "let   x= 1 in\nlet y  =[2]  in x   :: x :: y" "[1, 3, 2]"
        [] "let   x= 3 in\nlet y  =[2]  in x   :: x :: y"
      |> updateElmAssert
        [] "let   x= 1 in\nlet y  =[2]  in x  :: x :: y" "[1, 1, 3]"
        [] "let   x= 1 in\nlet y  =[3]  in x  :: x :: y"
  |> test "rec let"
      |> updateElmAssert
        [] "letrec f x = if x == 0 then x else (f (x - 1)) in\n f 2" "3"
        [] "letrec f x = if x == 0 then x else (f (x - 1)) in\n f 5"
  |> test "Comments"
      |> updateElmAssert
        [] "--This is a comment\n  1" "2"
        [] "--This is a comment\n  2"
  |> test "Strings"
      |> updateElmAssert
        [] "   \"This is a string\"" "\"Hello world\""
        [] "   \"Hello world\""
      |> updateElmAssert
        [] "   \"This is a string\"" "\"Hello' \\\" wo\\\\rld\""
        [] "   \"Hello' \\\" wo\\\\rld\""
      |> updateElmAssert
        [] "   'This is a string'" "\"Hello world\""
        [] "   'Hello world'"
      |> updateElmAssert
        [] "   'This is a string'" "\"Hello' \\\" wo\\\\rld\""
        [] "   'Hello\\' \" wo\\\\rld'"
  |> test "Many solutions"
      |> updateElmAssert
        [("h3", "\\content -> ['h3', [], ['TEXT', content]]")] "let x = 0 + 0 in\nh3 (toString x)" "['h3', [], ['TEXT', '1']]"
        [("h3", "\\content -> ['h3', [], ['TEXT', content]]")] "let x = 0 + 1 in\nh3 (toString x)"
      |> updateElmAssert
        [("h3", "\\content -> ['h3', [], ['TEXT', content]]")] "let x = 0 + 0 in\nh3 (toString x)" "['h3', [], ['TEXT', '1']]"
        [("h3", "\\content -> ['h3', [], ['TEXT', content]]")] "let x = 1 + 0 in\nh3 (toString x)"
  |> test "Multiline string literals"
      |> updateElmAssert
          [] "\"\"\"Hello @(if 1 == 2 then \"big\" else \"\"\"very @(\"small\")\"\"\") world\"\"\"" "\"Hello very tiny world\""
          [] "\"\"\"Hello @(if 1 == 2 then \"big\" else \"\"\"very @(\"tiny\")\"\"\") world\"\"\""
      |> updateElmAssert
        [] "let x = \"Hello\" in \"\"\"@x world\"\"\"" "\"Hello big world\""
        [] "let x = \"Hello\" in \"\"\"@x big world\"\"\""
      |> updateElmAssert
        [] "let x = \"Hello\" in \"\"\"@x world\"\"\"" "\"Hello big world\""
        [] "let x = \"Hello big\" in \"\"\"@x world\"\"\""
      |> updateElmAssert
        [] "let x = \"Hello\" in \"\"\"@x world\"\"\"" "\"Helloworld\""
        [] "let x = \"Hello\" in \"\"\"@(x)world\"\"\""
      |> updateElmAssert
        [] "\"\"\"@let x = \"Hello\"\n@x world\"\"\"" "\"Hello big world\""
        [] "\"\"\"@let x = \"Hello big\"\n@x world\"\"\""
      |> updateElmAssert
        [] "\"\"\"@let x = \"Hello\"\n@x world\"\"\"" "\"Hello big world\""
        [] "\"\"\"@let x = \"Hello\"\n@x big world\"\"\""
      |> updateElmAssert
        [] "\"\"\"@let x = (\"Hello\" + \n \" big\")\n@x world\"\"\"" "\"Hello tall world\""
        [] "\"\"\"@let x = (\"Hello\" + \n \" tall\")\n@x world\"\"\""
  |> summary
