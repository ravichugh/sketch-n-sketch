module UpdateTests exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)
import UpdateRegex exposing (..)
import UpdateStack exposing (..)
import UpdateUtils exposing (..)
import Lang exposing (..)
import GroupStartMap exposing (..)
import Regex
import Utils
import Eval
import Syntax
import Lazy
import Results
import LazyList
import LangUtils exposing (..)
import ParserUtils
import HTMLValParser
import Set

type StateChanger = StateChanger (State -> State)

type alias State = { numTests: Int, nthAssertion: Int, numSuccess: Int, numFailed: Int, currentName: String, errors: String, ignore: Bool, toLaunch: List StateChanger, onlyOnly: Bool }
init_state = State 0 0 0 0 "" "" False [] False

-- Metacommands to gather states without executing them
delay: (() -> State -> State) -> State -> State
delay thetest state =
  if state.ignore then state else
  thetest () state

only: (State -> State) -> State -> State
only stateChanger state =
  let newState = stateChanger { state | toLaunch = [], onlyOnly = False} in
  let newState2 =flush newState in
  {newState2  | onlyOnly = True, toLaunch = []} -- wipe out other tests not launched

gather: (State -> State) -> State -> State
gather stateChanger state =
  { state | toLaunch = state.toLaunch ++ [StateChanger stateChanger] }

flush: State -> State
flush state =
  let defaultState = {state | toLaunch = [] } in
  if state.onlyOnly then defaultState else
  List.foldl (\(StateChanger sc) s -> sc s) defaultState state.toLaunch



summary: State -> String
summary state_ =
  let state = flush state_ in
  Debug.log (state.errors ++ "\n-------------------\n "++toString state.numSuccess ++"/" ++ toString state.numTests++ " tests passed\n-------------------") "ok"

test_: String -> State -> State
test_ name state =
  --let _ = Debug.log name " [Start]" in
  --let res = body <| name in
  {state | nthAssertion = 1, currentName = name} -- Debug.log name "all tests passed"
test s = gather <| test_ s

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

nthEnv = [("nth", "letrec nth list index = case list of head::tail -> if index == 0 then head else nth tail (index - 1); [] -> null in nth")]

genericAssertEqual: (a -> String) -> (a -> a -> Bool) -> a -> a -> State  -> State
genericAssertEqual eToString isEqual obtained expected state =
  if state.ignore then state else
  if isEqual obtained expected then success state else fail state <| "[" ++ state.currentName ++ ", assertion #" ++ toString state.nthAssertion ++ "] Expected \n" ++
      eToString expected ++ ", got\n" ++ eToString obtained

assertEqual_: a -> a -> State  -> State
assertEqual_ = genericAssertEqual (toString) (==)
assertEqual a b = gather <| assertEqual_ a b

assertEqualVal_: Val -> Val -> State  -> State
assertEqualVal_ = genericAssertEqual valToString (\x y -> valToString x == valToString y)
assertEqualVal  v1 v2 = gather <| assertEqualVal_ v1 v2

updateAssert_: Env -> Exp -> Val -> Val -> Env -> String  -> State  -> State
updateAssert_ env exp origOut newOut expectedEnv expectedExpStr state =
  if state.ignore then state else
  let expected = envToString expectedEnv ++ " |- " ++ expectedExpStr in
  let problemdesc = ("\nFor problem:" ++
    envToString env ++ " |- " ++ unparse exp ++ " <-- " ++ valToString newOut ++
    " (was " ++ valToString origOut ++ ")") in
  case update (updateContext env exp origOut newOut) LazyList.Nil of
    Results.Oks (LazyList.Cons (envX, expX) lazyTail as ll) ->
      let _ = LazyList.toList ll in
      let obtained = envToString envX ++ " |- " ++ unparse expX in
      if obtained == expected then success state else
        case Lazy.force lazyTail of
          LazyList.Cons (envX2, expX2) lazyTail2 ->
            let obtained2 = envToString envX2 ++ " |- " ++ unparse expX2 in
            if obtained2 == expected then success state else
            fail state <|
              log state <| "Expected \n" ++ expected ++  ", got\n" ++ obtained ++ " and " ++ obtained2 ++ problemdesc
          LazyList.Nil ->
            fail state <|
              log state <| "Expected \n" ++ expected ++  ", got\n" ++ obtained ++ problemdesc
    Results.Oks LazyList.Nil ->
       fail state <| log state <| "Expected \n" ++ expected ++  ", got no solutions without error" ++ problemdesc
    Results.Errs msg ->
       fail state <| log state <| "Expected \n" ++ expected ++  ", got\n" ++ msg ++ problemdesc
updateAssert env exp origOut newOut expectedEnv expectedExpStr =
  gather <| updateAssert_ env exp origOut newOut expectedEnv expectedExpStr

evalElmAssert_: List (String, String) -> String -> String -> State -> State
evalElmAssert_ envStr expStr expectedResStr state =
  if state.ignore then state else
    case Utils.projOk [parseEnv envStr] of
      Err error -> fail state <| log state <| "Error while parsing environments: " ++ error
      Ok [env] ->
         case Utils.projOk [parse expStr, parse expectedResStr] of
             Err error -> fail state <| log state <| "Error while parsing expressions or outputs: " ++ error
             Ok [exp, res] ->
               --let _ = Debug.log (log state <| toString exp) () in
               case Utils.projOk [evalEnv env exp, eval res] of
               Err error -> fail state <| log state <| "Error while evaluating the expression or the expected result: " ++ unparse exp ++ "," ++ unparse res ++ ": " ++ error
               Ok [out, expectedOut] -> assertEqualVal_ out expectedOut state
               Ok _ -> fail state "???"
             Ok _ -> fail state "???"
      Ok _ -> fail state "???"
evalElmAssert envStr expStr expectedResStr = gather <| evalElmAssert_ envStr expStr expectedResStr

evalElmAssert2_: Env -> String -> String -> State -> State
evalElmAssert2_ env expStr expectedResStr state =
  if state.ignore then state else
  case Utils.projOk [parse expStr, parse expectedResStr] of
      Err error -> fail state <| log state <| "Error while parsing expressions or outputs: " ++ error
      Ok [exp, res] ->
        --let _ = Debug.log (log state <| toString exp) () in
        case Utils.projOk [evalEnv env exp, eval res] of
        Err error -> fail state <| log state <| "Error while evaluating the expression or the expected result: " ++ unparse exp ++ "," ++ unparse res ++ ": " ++ error
        Ok [out, expectedOut] -> assertEqualVal_ out expectedOut state
        Ok _ -> fail state "???"
      Ok _ -> fail state "???"
evalElmAssert2 envStr expStr expectedResStr = gather <| evalElmAssert2_ envStr expStr expectedResStr


updateElmAssert_: List (String, String) -> String -> String -> List (String, String) -> String -> State -> State
updateElmAssert_ envStr expStr newOutStr expectedEnvStr expectedExpStr state =
  if state.ignore then state else
  case Utils.projOk [parseEnv envStr, parseEnv expectedEnvStr] of
    Err error -> fail state <| log state <| "Error while parsing environments: " ++ error
    Ok [env, expectedEnv] ->
       case Utils.projOk [parse expStr, parse newOutStr] of
           Err error -> fail state <| log state <| "Error while parsing expressions or outputs: " ++ error
           Ok [exp, newOut] ->
             --let _ = Debug.log (log state <| toString exp) () in
             case Utils.projOk [evalEnv env exp, eval newOut] of
             Err error -> fail state <| log state <| "Error while evaluating the expression or the output: " ++ Syntax.unparser Syntax.Elm exp ++ "," ++ Syntax.unparser Syntax.Elm newOut ++ ": " ++ error
             Ok [out, newOut] -> updateAssert_ env exp out newOut expectedEnv expectedExpStr state
             Ok _ -> fail state "???"
           Ok _ -> fail state "???"
    Ok _ -> fail state "???"
updateElmAssert envStr expStr newOutStr expectedEnvStr expectedExpStr = gather <| updateElmAssert_ envStr expStr newOutStr expectedEnvStr expectedExpStr

updateElmAssert2_: Env -> String -> String -> String -> State -> State
updateElmAssert2_ env expStr newOutStr expectedExpStr state =
  if state.ignore then state else
  case Utils.projOk [parse expStr, parse newOutStr] of
      Err error -> fail state <| log state <| "Error while parsing expressions or outputs: " ++ error
      Ok [exp, newOut] ->
        --let _ = Debug.log (log state <| toString exp) () in
        case Utils.projOk [evalEnv env exp, eval newOut] of
        Err error -> fail state <| log state <| "Error while evaluating the expression or the output: " ++ Syntax.unparser Syntax.Elm exp ++ "," ++ Syntax.unparser Syntax.Elm newOut ++ ": " ++ error
        Ok [out, newOut] -> updateAssert_ env exp out newOut env expectedExpStr state
        Ok _ -> fail state "???"
      Ok _ -> fail state "???"
updateElmAssert2 env expStr newOutStr expectedExpStr = gather <| updateElmAssert2_ env expStr newOutStr expectedExpStr

parse = Syntax.parser Syntax.Elm >> Result.mapError (\p -> ParserUtils.showError p)
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

onlySpecific = False

all_tests = init_state
  |> ignore onlySpecific
  |> test "triCombineTest"
  |> delay (\() -> assertEqual
      (mergeEnvGeneral (tList space0  [tVar space0 "x", tVar space0 "y"] space0)  (Set.fromList ["x", "y"])
                  [("y", (tVal 2)), ("x", (tVal 1))]
                  [("y", (tVal 2)), ("x", (tVal 1))]
                  [("y", (tVal 2)), ("x", (tVal 3))]
                 )[("y", (tVal 2)), ("x", (tVal 3))])
  |> delay (\() -> assertEqual
      (mergeEnvGeneral (tList space0  [tVar space0 "x", tVar space0 "y", tVar space0 "z"] space0) (Set.fromList ["x", "y", "z"])
                  [("x", (tVal 1)), ("y", (tVal 1)), ("z", (tVal 1))]
                  [("x", (tVal 1)), ("y", (tVal 2)), ("z", (tVal 2))]
                  [("x", (tVal 3)), ("y", (tVal 1)), ("z", (tVal 3))]
                 )[("x", (tVal 3)), ("y", (tVal 2)), ("z", (tVal 2))])
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
        [("x", "[7, 1]")] "case x of\n  [a, b] -> a + b\n  u -> 0" "5"
        [("x", "[4, 1]")] "case x of\n  [a, b] -> a + b\n  u -> 0"
      |> updateElmAssert
        [("x", "[7]")] "case x of\n  [a, b] -> a + b\n  u -> 0" "5"
        [("x", "[7]")] "case x of\n  [a, b] -> a + b\n  u -> 5"
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
      |> updateElmAssert
        [] "let   x= 1 in\nlet y  =2  in [x, x, y]" "[1, 3, 2]"
        [] "let   x= 3 in\nlet y  =2  in [x, x, y]"
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
        [("h3", "\\text -> ['h3', [], [['TEXT', text]]]")] "x = 0 + 0\n\nh3 (toString x)" "['h3', [], [['TEXT', '1']]]"
        [("h3", "\\text -> ['h3', [], [['TEXT', text]]]")] "x = 0 + 1\n\nh3 (toString x)"
      |> updateElmAssert
        [("h3", "\\text -> ['h3', [], [['TEXT', text]]]")] "x = 0 + 0\n\nh3 (toString x)" "['h3', [], [['TEXT', '1']]]"
        [("h3", "\\text -> ['h3', [], [['TEXT', text]]]")] "x = 1 + 0\n\nh3 (toString x)"
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
  |> test "Finding all regex matches"
    |> assertEqual (LazyList.toList <| allInterleavingsIn ["A", "BB", "C"] "xAyBBBzAoBBpCC")
      [ ["x","y","BzAoBBp","C"]
      , ["x","y","BzAoBBpC",""]
      , ["x", "yB", "zAoBBp","C"]
      , ["x", "yB", "zAoBBpC",""]
      , ["x", "yBBBzAo", "p","C"]
      , ["x", "yBBBzAo", "pC",""]
      , ["xAyBBBz", "o", "p","C"]
      , ["xAyBBBz", "o", "pC",""]
      ]
  |> test "GroupStartMap"
    |> assertEqual (List.map (\{submatches} -> submatches) (GroupStartMap.find Regex.All "a((bc)|cd)d" "aabcdacdd"))
        [[{match = Just "bc", start = 2}, {match = Just "bc", start = 2}], [{match = Just "cd", start = 6}, {match = Nothing, start = -1}]]
    |> assertEqual (GroupStartMap.replace Regex.All "a((bc)|cd)d" (
      \match -> String.concat <| List.map (\subm -> toString subm.start ++ Maybe.withDefault "null" subm.match) match.submatches
      ) "aabcdacdd")
        "a2bc2bc6cd-1null"
  |> test "replaceAllIn"
    |> evalElmAssert [] "replaceAllIn \"l\" \"L\" \"Hello world\"" "\"HeLLo worLd\""
    |> evalElmAssert nthEnv "replaceAllIn \"a(b|c)\" \"o$1\" \"This is acknowledgeable\"" "\"This is ocknowledgeoble\""
    |> evalElmAssert nthEnv "replaceAllIn \"a(b|c)\" (\\{group = [t, c]} -> \"oa\" + (if c == \"b\" then \"c\" else \"b\")) \"This is acknowledgeable\"" "\"This is oabknowledgeoacle\""
    |> updateElmAssert nthEnv "replaceAllIn \"e\" \"ee\" \"\"\"See some examples from File...\"\"\"" "\"Seeee somee emexamplees from Filee...\""
                       nthEnv "replaceAllIn \"e\" \"eme\" \"\"\"See some examples from File...\"\"\""
    |> updateElmAssert nthEnv "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"ee\" \"\"\"\n            See some examples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
                              "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT',                           \"\"\"\n            Seeee somee eecxamplees from Filee -> Neew From Teemplatee in\n            thee meenu bar, or by preessing thee Preevious and Neext\n            buttons in thee top-right corneer.\n           \"\"\"]]]\n        ]\n      ]"
                       nthEnv "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"eec\" \"\"\"\n            See some examples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
    |> updateElmAssert nthEnv "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"ee\" \"\"\"\n            See some examples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
                              "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT',                           \"\"\"\n            Seeee somee eecxamplees from Filee -> Neew From Teemplatee in\n            thee meenu bar, or by preessing thee Preevious and Neext\n            buttons in thee top-right corneer.\n           \"\"\"]]]\n        ]\n      ]"
                       nthEnv "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"ee\" \"\"\"\n            See some ecxamples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
    |>
      updateElmAssert [] "extractFirstIn \"^\\\\s*(S(\\\\w)+ (\\\\w))\" \"\"\" See some examples\"\"\" |> case of [\"Just\", [big, s1, s2]] -> big + s1 + s2; e -> \"not the right shape\"" "\"Sea ses\""
                      [] "extractFirstIn \"^\\\\s*(S(\\\\w)+ (\\\\w))\" \"\"\" Sea some examples\"\"\" |> case of [\"Just\", [big, s1, s2]] -> big + s1 + s2; e -> \"not the right shape\""
    |> evalElmAssert [] "extractFirstIn \"\"\"([\\w:_-]*)\"\"\" \"data-array=\\\"17\\\"\" |> case of [_, [id]] -> id; _ -> \"Nothing\" " "\"data-array\""
    |> evalElmAssert nthEnv "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"[Markdown](http://test)\"" "\"<a href='http://test'>Markdown</a>\""
    |> evalElmAssert nthEnv "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"[Markdown](https://fr.wikipedia.org/wiki/Markdown)\"" "\"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a>\""
    |> updateElmAssert nthEnv "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"#[Markdown](https://fr.wikipedia.org/wiki/Markdown)\"" "\"#<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdoooown</a>\""
                       nthEnv "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"#[Markdoooown](https://fr.wikipedia.org/wiki/Markdown)\""
    |> updateElmAssert [] "replaceAllIn \"x|y\" (\\{match} -> if match == \"x\" then \"5\" else \"7\") \"x,y\"" "\"6,8\""
                             [] "replaceAllIn \"x|y\" (\\{match} -> if match == \"x\" then \"6\" else \"8\") \"x,y\""
  --|> test "Record construction, extraction and pattern "
  --  |>
  |> test "Partial application"
    |> updateElmAssert
      [] "letrec map f l = case l of head::tail -> f head::map f tail; [] -> [] in let td color txt = [ 'td', [['style', ['border', 'padding', ['background-color', color]]]], [['TEXT', txt]]] in map (td 'green') ['hello', 'world']"
         "[[ 'td', [['style', ['border', 'padding', ['background-color', 'red']]]], [['TEXT', 'hello']]], [ 'td', [['style', ['border', 'padding', ['background-color', 'green']]]], [['TEXT', 'world']]]]"
      [] "letrec map f l = case l of head::tail -> f head::map f tail; [] -> [] in let td color txt = [ 'td', [['style', ['border', 'padding', ['background-color', color]]]], [['TEXT', txt]]] in map (td 'red') ['hello', 'world']"
  --|> ignore False
  |> test "Multiple solutions"
      |> updateElmAssert
        [] "['A' + 'B', 'C' + 'D']" "['AGB', 'CGD']"
        [] "['AG' + 'B', 'CG' + 'D']"
  |> test "Records"
      |> updateElmAssert
        [] "{ a= 1, b= 2}.b" "3"
        [] "{ a= 1, b= 3}.b"
      |> updateElmAssert
        [] "{ a= 1, b= 2}.a" "3"
        [] "{ a= 3, b= 2}.a"
      |> updateElmAssert
        [] "let x = { a= 1, b= 2} in { x | a = 2 }.a" "3"
        [] "let x = { a= 1, b= 2} in { x | a = 3 }.a"
      |> updateElmAssert
        [] "let x = { a= 1, b= 2} in { x | a = 2 }.b" "3"
        [] "let x = { a= 1, b= 3} in { x | a = 2 }.b"
  |> test "Indented lists, second position of list > 1 elements"
      |> updateElmAssert
        [] "[1, 2, 3, 4]" "[1, 5, 2, 3, 4]"
        [] "[1, 5, 2, 3, 4]"
      |> updateElmAssert
        [] "[ 1,\n 2,\n 3,\n 4]" "[1, 5, 2, 3, 4]"
        [] "[ 1,\n 5,\n 2,\n 3,\n 4]"
      |> updateElmAssert
        [] "[ 1\n,  2\n,  3\n,  4]" "[1, 5, 2, 3, 4]"
        [] "[ 1\n,  5\n,  2\n,  3\n,  4]"
      |> updateElmAssert
        [] "  [ 1\n,    2\n,    3\n,    4]" "[1, 5, 2, 3, 4]"
        [] "  [ 1\n,    5\n,    2\n,    3\n,    4]"
  |> test "Indented lists, second position of list with 1 element"
      |> updateElmAssert
        [] "[1]" "[1, 2]"
        [] "[1, 2]"
      |> updateElmAssert
        [] "[ 1]" "[ 1, 2]"
        [] "[ 1, 2]"
      |> updateElmAssert
        [] "[ 1\n]" "[ 1, 2]"
        [] "[ 1\n, 2\n]"
      |> updateElmAssert
        [] "[\n  1]" "[ 1, 2]"
        [] "[\n  1,\n  2]"
  |> test "Indented lists, first position of list with 1 element"
        |> updateElmAssert
          [] "[1]" "[2, 1]"
          [] "[2, 1]"
        |> updateElmAssert
          [] "[ 1]" "[2, 1]"
          [] "[ 2, 1]"
        |> updateElmAssert
          [] "[ 1\n]" "[2, 1]"
          [] "[ 2\n, 1\n]"
        |> updateElmAssert
          [] "[\n  1]" "[2, 1]"
          [] "[\n  2,\n  1]"
  |> test "Indented lists, first position of list > 1 element"
          |> updateElmAssert
            [] "[1, 2]" "[0, 1, 2]"
            [] "[0, 1, 2]"
          |> updateElmAssert
            [] "[ 1, 2]" "[0, 1, 2]"
            [] "[ 0, 1, 2]"
          |> updateElmAssert
            [] "[ 1\n,  2\n,  3\n,  4]" "[0, 1, 2, 3, 4]"
            [] "[ 0\n,  1\n,  2\n,  3\n,  4]"
  |> test "Indented lists, first position of empty list"
            |> updateElmAssert
              [] "[]" "[1]"
              [] "[1]"
            |> updateElmAssert
              [] "[ ]" "[1]"
              [] "[1 ]"
            |> updateElmAssert
              [] "  []" "[[1, 2]]"
              [] "  [[1, 2]]"
  |> test "parsing HTML"
  |> updateElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"hello\""  "[[\"HTMLInner\",\"hello world\"]]"
               "parseHTML \"hello world\""
  |> updateElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"hello<br>world\""  "[[\"HTMLInner\",\"hello\"], [\"HTMLElement\", \"br\", [], \" \", [\"RegularEndOpening\"], [], [\"VoidClosing\"]], [\"HTMLInner\",\"sworld\"]]"
               "parseHTML \"hello<br >sworld\""
  |> updateElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
                 "parseHTML \"<?help>\""  "[[\"HTMLComment\",\"Less_Greater\",\"Injection: adding more chars like >, <, and -->\"]]"
                 "parseHTML \"<!--Injection: adding more chars like >, <, and ~~>-->\""
  |> evalElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"<h3>Hello world</h3>\"" "[[\"HTMLElement\", \"h3\", [], \"\", [\"RegularEndOpening\"], [[\"HTMLInner\",\"Hello world\"]], [\"RegularClosing\", \"\"]]]"
  |> evalElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a>\"" "[[\"HTMLElement\", \"a\", [[\"HTMLAttribute\", \" \", \"href\", [\"HTMLAttributeString\", \"\", \"\", \"'\", \"https://fr.wikipedia.org/wiki/Markdown\"]]], \"\", [\"RegularEndOpening\"], [[\"HTMLInner\",\"Markdown\"]], [\"RegularClosing\", \"\"]]]"
  |> updateElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a> demo\""
                       "[[\"HTMLElement\", \"a\", [[\"HTMLAttribute\", \" \", \"href\", [\"HTMLAttributeString\", \"\", \"\", \"'\", \"https://fr.wikipedia.org/wiki/Markdown\"]]], \"\", [\"RegularEndOpening\"], [[\"HTMLInner\",\"Markdown\"]], [\"RegularClosing\", \"\"]], [\"HTMLInner\",\" demonstration\"]]"
               "parseHTML \"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a> demonstration\""
  |> updateElmAssert [("color", "\"white\""), ("padding", "[\"padding\", \"3px\"]")] "[padding, [\"background-color\", color]]"
                            "[[\"padding\", \"3px\"], [ \"background-color\", \"lightgray\"]]"
                           [("color", "\"lightgray\""), ("padding", "[\"padding\", \"3px\"]")] "[padding, [\"background-color\", color]]"
    -- Add the test <i>Hello <b>world</span></i> --> <i>Hello <b>world</b></i>  (make sure to capture all closing tags)
  |> summary
