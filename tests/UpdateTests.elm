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
import Results exposing (Results)
import LazyList
import LangUtils exposing (..)
import ParserUtils
import HTMLValParser
import Set
import ImpureGoodies
import EvalUpdate exposing (builtinEnv)
import ExamplesGenerated
import ElmParser

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

onlyLast: State -> State
onlyLast state =
  case Utils.maybeLast state.toLaunch of
    Just (StateChanger stateChanger) ->
      only stateChanger state
    _ -> Debug.crash "No tests to only test before the use of 'onlyLast'. Add '|> onlyLast' after a test you want to run alone"

onlyAfter = skipBefore

skipBefore: State -> State
skipBefore state = { state | toLaunch = [] }

onlyBefore: State -> State
onlyBefore =
  flush >> ignore True

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
  let _ = Debug.log name " [Start]" in
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

updateAssert_: Bool -> Env -> Exp -> Val -> Val -> Env -> String  -> State  -> State
updateAssert_ checkEnv env exp origOut newOut expectedEnv expectedExpStr state =
  if state.ignore then state else
  let expected = (if checkEnv then envToString expectedEnv else "") ++ " |- " ++ expectedExpStr in
  let problemdesc = ("\nFor problem:" ++
    (if checkEnv then envToString env else "") ++ " |- " ++ unparse exp ++ " <-- " ++ valToString newOut ++
    " (was " ++ valToString origOut ++ ")") in
  case UpdateUtils.defaultVDiffs origOut newOut of
    Err msg -> fail state <| log state <| "This diff is not allowed:" ++ msg
    Ok (LazyList.Nil) -> fail state <| "Internal error: no diffs"
    Ok (LazyList.Cons Nothing _) -> fail state <| log state <| "There was no diff between the previous output and the new output"
    Ok ll->
      --let _ = ImpureGoodies.log <| "Diffs observed: " ++ toString (LazyList.toList ll) in
      case Ok (LazyList.filterMap identity ll) |> Results.andThen (\diffs ->
        update LazyList.Nil LazyList.Nil (updateContext "initial" env exp [] origOut newOut diffs)) of
        Ok (LazyList.Cons (envX, expX) lazyTail as ll) ->
          let _ = LazyList.toList ll in
          --let _ = ImpureGoodies.log <| toString expX.changes in
          --let _ = ImpureGoodies.log <| eDiffsToString "" exp expX.val (expX.changes |> Maybe.withDefault (EConstDiffs EAnyDiffs)) in
          let obtained = (if checkEnv then envToString envX.val else if envX.changes == [] then "" else toString envX.changes) ++ " |- " ++ unparse expX.val in
          if obtained == expected then success state else
            case Lazy.force lazyTail of
              LazyList.Cons (envX2, expX2) lazyTail2 ->
                let obtained2 = (
                  if checkEnv then envToString envX2.val else
                  if envX2.changes == [] then "" else toString envX2.changes) ++ " |- " ++ unparse expX2.val in
                if obtained2 == expected then success state else
                fail state <|
                  log state <| "Expected \n'" ++ expected ++  "'\n, got\n'" ++ obtained ++ "'\n and \n'" ++ obtained2 ++ problemdesc
              LazyList.Nil ->
                fail state <|
                  log state <| "Expected \n'" ++ expected ++  "'\n, got\n'" ++ obtained ++ "'\n" ++ problemdesc
        Ok LazyList.Nil ->
           fail state <| log state <| "Expected \n'" ++ expected ++  "', got no solutions without error" ++ problemdesc
        Err msg ->
           fail state <| log state <| "Expected \n'" ++ expected ++  "', got '" ++ msg ++ problemdesc
updateAssert env exp origOut newOut expectedEnv expectedExpStr =
  gather <| updateAssert_ True env exp origOut newOut expectedEnv expectedExpStr

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
             Err error ->
               let _ = Debug.log (toString exp) () in
               fail state <| log state <| "Error while evaluating the expression or the output: " ++ Syntax.unparser Syntax.Elm exp ++ " <- " ++ Syntax.unparser Syntax.Elm newOut ++ ": " ++ error
             Ok [out, newOut] -> updateAssert_ True env exp out newOut expectedEnv expectedExpStr state
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
        Ok [out, newOut] -> updateAssert_ True env exp out newOut env expectedExpStr state
        Ok _ -> fail state "???"
      Ok _ -> fail state "???"
updateElmAssert2 env expStr newOutStr expectedExpStr = gather <| updateElmAssert2_ env expStr newOutStr expectedExpStr

updateElmPrelude_: String -> (String -> String) -> (String -> String) -> State -> State
updateElmPrelude_ expStr outReplacer expectedExpReplacer state =
  if state.ignore then state else
  case parse expStr of
    Err error -> fail state <| log state <| "Error while parsing expressions or outputs: " ++ error
    Ok exp ->
      case evalEnv EvalUpdate.preludeEnv exp of
         Err error -> fail state <| log state <| "Error while evaluating the expression: " ++ Syntax.unparser Syntax.Elm exp ++ ": " ++ error
         Ok oldOut ->
           case oldOut |> valToString |> outReplacer |> parse |> Result.andThen eval of
             Err error ->  fail state <| log state <| "Error while parsing expressions or outputs: " ++ error
             Ok newOut ->
               let expectedExpStr = expectedExpReplacer expStr in
               updateAssert_ False EvalUpdate.preludeEnv exp oldOut newOut EvalUpdate.preludeEnv expectedExpStr state
updateElmPrelude expStr outReplacer expectedExpReplacer = gather <| updateElmPrelude_ expStr outReplacer expectedExpReplacer

replaceStr before after = Regex.replace Regex.All (Regex.regex before) (\_ -> after)
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

display_prelude_message = case ElmParser.preludeNotParsed of
  Nothing -> ()
  Just msg -> Debug.log msg ()

all_tests = init_state
  |> ignore onlySpecific
  |> test "triCombineTest"
  |> delay (\() -> assertEqual
      (mergeEnv
                  [("y", (tVal 2)), ("x", (tVal 1))]
                  [("y", (tVal 2)), ("x", (tVal 1))] []
                  [("y", (tVal 2)), ("x", (tVal 3))] [(1, VConstDiffs)]
                 ) ([("y", (tVal 2)), ("x", (tVal 3))], [(1, VConstDiffs)]))
  |> delay (\() -> assertEqual
      (mergeEnv
                  [("x", (tVal 1)), ("y", (tVal 1)), ("z", (tVal 1))]
                  [("x", (tVal 1)), ("y", (tVal 2)), ("z", (tVal 2))] [(1, VConstDiffs), (2, VConstDiffs)]
                  [("x", (tVal 3)), ("y", (tVal 1)), ("z", (tVal 3))] [(0, VConstDiffs), (2, VConstDiffs)]
                 ) ([("x", (tVal 3)), ("y", (tVal 2)), ("z", (tVal 3))], [(0, VConstDiffs), (1, VConstDiffs), (2, VConstDiffs)]))
  |> test "update mutual recursion"
  |> updateElmAssert [] "x =y\ny= 1\nx" "2"
                     [] "x =y\ny= 2\nx"
  |> updateElmAssert [] "a = g 85\nf x = g (x - 17)\ng y = if y < 17 then y else f y\na" "2"
                     [] "a = g 87\nf x = g (x - 17)\ng y = if y < 17 then y else f y\na"
  |> updateElmAssert [] "a = g 85\nf x = g (x - 17)\ng y = if y < 17 then y else f y\na" "2"
                     [] "a = g 85\nf x = g (x - 15)\ng y = if y < 17 then y else f y\na"
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
        [("x", "3"), ("y", "4")] " x ^  y" "16"
        [("x", "2"), ("y", "4")] " x ^  y"
      |> updateElmAssert
        [("x", "-1"), ("y", "3")] "  x ^   y" "1"
        [("x", "1"), ("y", "3")] "  x ^   y"
      |> updateElmAssert
          [("x", "-2"), ("y", "3")] " x ^   y" "27"
          [("x", "3"), ("y", "3")] " x ^   y"
      |> updateElmAssert
        [("x", "-1"), ("y", "0")] "x ^   y" "-1"
        [("x", "-1"), ("y", "1")] "x ^   y"
      |> updateElmAssert
        [("x", "-1"), ("y", "3")] " x ^   y" "-8"
        [("x", "-2"), ("y", "3")] " x ^   y"
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
        [] "let f x = if x == 0 then x else (f (x - 1)) in\n f 2" "3"
        [] "let f x = if x == 0 then x else (f (x - 1)) in\n f 5"
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
      |> updateElmAssert
        [] "   'Welcome to this world'" "\"Welcome again to this world\""
        [] "   'Welcome again to this world'"
  |> test "Strings concatenation"
      |> updateElmAssert [] "(\"Hello\" + \" \") + \"world\"" "\"Good morning world\""
                         [] "(\"Good morning\" + \" \") + \"world\""
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
        [] "\"\"\"@let x = \"Hello\" in\n@x world\"\"\"" "\"Hello big world\""
        [] "\"\"\"@let x = \"Hello big\" in\n@x world\"\"\""
      |> updateElmAssert
        [] "\"\"\"@let x = \"Hello\" in\n@x world\"\"\"" "\"Hello big world\""
        [] "\"\"\"@let x = \"Hello\" in\n@x big world\"\"\""
      |> updateElmAssert
        [] "\"\"\"@let x = (\"Hello\" + \n \" big\") in\n@x world\"\"\"" "\"Hello tall world\""
        [] "\"\"\"@let x = (\"Hello\" + \n \" tall\") in\n@x world\"\"\""
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
    |> evalElmAssert2 builtinEnv "replaceAllIn \"l\" \"L\" \"Hello world\"" "\"HeLLo worLd\""
    |> evalElmAssert2 builtinEnv "replaceAllIn \"a(b|c)\" \"o$1\" \"This is acknowledgeable\"" "\"This is ocknowledgeoble\""
    |> evalElmAssert2 builtinEnv "replaceAllIn \"a(b|c)\" (\\{group = [t, c]} -> \"oa\" + (if c == \"b\" then \"c\" else \"b\")) \"This is acknowledgeable\"" "\"This is oabknowledgeoacle\""
    |> updateElmAssert2 builtinEnv "replaceAllIn \"e\" \"ee\" \"\"\"See some examples from File...\"\"\"" "\"Seeee somee emexamplees from Filee...\""
                                   "replaceAllIn \"e\" \"eme\" \"\"\"See some examples from File...\"\"\""
    |> updateElmAssert2 builtinEnv "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"ee\" \"\"\"\n            See some examples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
                              "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT',                           \"\"\"\n            Seeee somee eecxamplees from Filee -> Neew From Teemplatee in\n            thee meenu bar, or by preessing thee Preevious and Neext\n            buttons in thee top-right corneer.\n           \"\"\"]]]\n        ]\n      ]"
                                   "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"eec\" \"\"\"\n            See some examples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
    |> updateElmAssert2 builtinEnv "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"ee\" \"\"\"\n            See some examples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
                              "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT',                           \"\"\"\n            Seeee somee eecxamplees from Filee -> Neew From Teemplatee in\n            thee meenu bar, or by preessing thee Preevious and Neext\n            buttons in thee top-right corneer.\n           \"\"\"]]]\n        ]\n      ]"
                                   "[ 'div'\n      , []\n      , [ ['h2', [], [['TEXT', 'Welcome to Sketch-n-Sketch Docs!']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', 'Type something here...']]]\n        , ['br', [], []]\n        , ['p', [], [['TEXT', replaceAllIn \"e\" \"ee\" \"\"\"\n            See some ecxamples from File -> New From Template in\n            the menu bar, or by pressing the Previous and Next\n            buttons in the top-right corner.\n           \"\"\"]]]\n        ]\n      ]"
    |>
      updateElmAssert [] "extractFirstIn \"^\\\\s*(S(\\\\w)+ (\\\\w))\" \"\"\" See some examples\"\"\" |> case of Just [big, s1, s2] -> big + s1 + s2; e -> \"not the right shape\"" "\"Sea ses\""
                      [] "extractFirstIn \"^\\\\s*(S(\\\\w)+ (\\\\w))\" \"\"\" Sea some examples\"\"\" |> case of Just [big, s1, s2] -> big + s1 + s2; e -> \"not the right shape\""
    |> evalElmAssert [] "extractFirstIn \"\"\"([\\w:_-]*)\"\"\" \"data-array=\\\"17\\\"\" |> case of Just [id] -> id; _ -> \"Nothing\" " "\"data-array\""
    |> evalElmAssert2 builtinEnv "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"[Markdown](http://test)\"" "\"<a href='http://test'>Markdown</a>\""
    |> evalElmAssert2 builtinEnv "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"[Markdown](https://fr.wikipedia.org/wiki/Markdown)\"" "\"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a>\""
    |> updateElmAssert2 builtinEnv "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"#[Markdown](https://fr.wikipedia.org/wiki/Markdown)\"" "\"#<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdoooown</a>\""
                                   "replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" \"#[Markdoooown](https://fr.wikipedia.org/wiki/Markdown)\""
    |> updateElmAssert2 builtinEnv "replaceAllIn \"x|y\" (\\{match} -> if match == \"x\" then \"5\" else \"7\") \"x,y\"" "\"6,8\""
                                   "replaceAllIn \"x|y\" (\\{match} -> if match == \"x\" then \"6\" else \"8\") \"x,y\""
  --|> test "Record construction, extraction and pattern "
  --  |>
  |> test "Partial application"
    |> updateElmAssert
      [] "let map f l = case l of head::tail -> f head::map f tail; [] -> [] in let td color txt = [ 'td', [['style', ['border', 'padding', ['background-color', color]]]], [['TEXT', txt]]] in map (td 'green') ['hello', 'world']"
         "[[ 'td', [['style', ['border', 'padding', ['background-color', 'red']]]], [['TEXT', 'hello']]], [ 'td', [['style', ['border', 'padding', ['background-color', 'green']]]], [['TEXT', 'world']]]]"
      [] "let map f l = case l of head::tail -> f head::map f tail; [] -> [] in let td color txt = [ 'td', [['style', ['border', 'padding', ['background-color', color]]]], [['TEXT', txt]]] in map (td 'red') ['hello', 'world']"
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
               "parseHTML \"hello\""  "[HTMLInner \"hello world\"]"
               "parseHTML \"hello world\""
  |> updateElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"hello<br>world\""  "[HTMLInner \"hello\", HTMLElement \"br\" [] \" \" RegularEndOpening [] VoidClosing, HTMLInner \"sworld\"]"
               "parseHTML \"hello<br >sworld\""
  |> updateElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
                 "parseHTML \"<?help>\""  "[HTMLComment (Less_Greater \"Injection: adding more chars like >, <, and -->\")]"
                 "parseHTML \"<!--Injection: adding more chars like >, <, and ~~>-->\""
  |> evalElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"<h3>Hello world</h3>\"" "[HTMLElement \"h3\" [] \"\" RegularEndOpening [HTMLInner \"Hello world\"] (RegularClosing \"\")]"
  |> evalElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a>\"" "[HTMLElement \"a\" [HTMLAttribute \" \" \"href\" (HTMLAttributeString \"\" \"\" \"'\" \"https://fr.wikipedia.org/wiki/Markdown\")] \"\" RegularEndOpening [HTMLInner \"Markdown\"] (RegularClosing \"\")]"
  |> updateElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)]
               "parseHTML \"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a> demo\""
                       "[HTMLElement \"a\" [HTMLAttribute \" \" \"href\" (HTMLAttributeString \"\" \"\" \"'\" \"https://fr.wikipedia.org/wiki/Markdown\")] \"\" RegularEndOpening [HTMLInner \"Markdown\"] (RegularClosing \"\"), HTMLInner \" demonstration\"]"
               "parseHTML \"<a href='https://fr.wikipedia.org/wiki/Markdown'>Markdown</a> demonstration\""
  |> evalElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)] "parseHTML \"Hello world\""  "[HTMLInner \"Hello world\"]"
  |> evalElmAssert2 [("parseHTML", HTMLValParser.htmlValParser)] "parseHTML \"<i><b>Hello</i></b>World\""  "[ HTMLElement \"i\" [] \"\" RegularEndOpening [ HTMLElement \"b\" [] \"\" RegularEndOpening [ HTMLInner \"Hello\"] ForgotClosing] (RegularClosing \"\"), HTMLInner \"</b>World\"]"
  |> updateElmAssert [("color", "\"white\""), ("padding", "[\"padding\", \"3px\"]")] "[padding, [\"background-color\", color]]"
                            "[[\"padding\", \"3px\"], [ \"background-color\", \"lightgray\"]]"
                           [("color", "\"lightgray\""), ("padding", "[\"padding\", \"3px\"]")] "[padding, [\"background-color\", color]]"
  |> test "freeze"
  |> updateElmAssert [("freeze", "\\x -> x")] "freeze 0 + 1" "2"
                     [("freeze", "\\x -> x")] "freeze 0 + 2"
  |> updateElmAssert [("freezeExpression", "\\x -> x")] "let x = 1 in freezeExpression (0 + x)" "2"
                     [("freezeExpression", "\\x -> x")] "let x = 2 in freezeExpression (0 + x)"
  |> test "dictionary"
  |> updateElmAssert [("x", "1")] "__DictGet__ \"a\" (__DictFromList__ [(\"a\", x)])" "Just 2"
                     [("x", "2")] "__DictGet__ \"a\" (__DictFromList__ [(\"a\", x)])"
  |> updateElmAssert [("x", "1")] "__DictRemove__ \"b\" (__DictFromList__ [(\"a\", x), (\"b\", 2)])" "__DictFromList__ [(\"a\", 2)]"
                     [("x", "2")] "__DictRemove__ \"b\" (__DictFromList__ [(\"a\", x), (\"b\", 2)])"
  |> updateElmAssert [("x", "1")] "__DictInsert__ \"b\" x (__DictFromList__ [(\"a\", x)])" "__DictFromList__ [(\"a\", 1), (\"b\", 2)]"
                     [("x", "2")] "__DictInsert__ \"b\" x (__DictFromList__ [(\"a\", x)])"
  |> updateElmAssert [] "__DictInsert__ \"b\" 1 (__DictFromList__ [(\"a\", 2), (\"b\", 3)])" "__DictFromList__ [(\"a\", 1), (\"b\", 2)]"
                     [] "__DictInsert__ \"b\" 2 (__DictFromList__ [(\"a\", 1), (\"b\", 3)])"
  |> test "recursive delayed"
  |> updateElmAssert [] "f =\n  let x = 1 in\n  \\y -> x + y\n\nf 2" "4"
                     [] "f =\n  let x = 2 in\n  \\y -> x + y\n\nf 2"
  |> updateElmAssert [] "f =\n  let x = 1 in\n  \\y -> x + y\n\nf 2" "4"
                     [] "f =\n  let x = 1 in\n  \\y -> x + y\n\nf 3"
  |> test "All diffs"
  |> assertEqual (allStringDiffs "abcabcabc" "abxabcabc" |> Results.toList)
                    [[DiffEqual "ab", DiffRemoved "c",  DiffAdded "x", DiffEqual "abcabc"]]
  |> assertEqual (allStringDiffs "ab3ab" "abab" |> Results.toList)
                    [[DiffEqual "ab", DiffRemoved "3", DiffEqual "ab"]]
  |> assertEqual (allStringDiffs "aa3aa" "aa" |> Results.toList)
                    [[DiffEqual "aa", DiffRemoved "3aa"],
                     [DiffRemoved "aa3", DiffEqual "aa"]]
  |> assertEqual (alldiffs identity ["a", "b", "3", "a", "b"] ["a", "b", "a", "b"] |> Results.toList)
                    [[DiffEqual ["a", "b"], DiffRemoved ["3"], DiffEqual ["a", "b"]]]
  |> assertEqual (alldiffs identity ["a", "a", "3", "a", "a"] ["a", "a"] |> Results.toList)
                    [[DiffEqual ["a", "a"], DiffRemoved ["3", "a", "a"]],
                     [DiffRemoved ["a", "a", "3"], DiffEqual ["a", "a"]]]
  |> test "Type parsing"
  |> evalElmAssert [] "type List a = Nil | Cons a\n2" "2"
  |> evalElmAssert [] "let type List a = Nil | Cons a in 2" "2"
  |> evalElmAssert [] "let type List a = Nil | Cons a\nin 2" "2"
  |> evalElmAssert [] "let\n  type List a = Nil | Cons a\nin 2" "2"
  --|> skipBefore
  |> test "updateReplace"
  -- newStart newEnd ... repStart repEnd
  |> evalElmAssert2 [("updateReplace", UpdateRegex.updateReplace EvalUpdate.eval EvalUpdate.update)]
        "updateReplace \"ab\" \"abc\" \"12ab567ab90\" (VStringDiffs [StringUpdate 0 1 2])"
        "(\"12abc567abc90\", VStringDiffs [StringUpdate 0 1 2, StringUpdate 1 3 3, StringUpdate 6 8 3])"
  -- repStart repEnd ... newStart newEnd
  |> evalElmAssert2 [("updateReplace", UpdateRegex.updateReplace EvalUpdate.eval EvalUpdate.update)]
        "updateReplace \"ab\" \"abc\" \"12ab567ab90\" (VStringDiffs [StringUpdate 5 5 1])"
        "(\"12abc567abc90\", VStringDiffs [StringUpdate 2 4 3, StringUpdate 5 5 1, StringUpdate 6 8 3])"
  -- newStart repStart repEnd ...  newEnd
  |> evalElmAssert2 [("updateReplace", UpdateRegex.updateReplace EvalUpdate.eval EvalUpdate.update)]
        "updateReplace \"ab\" \"abc\" \"12ab567ab90\" (VStringDiffs [StringUpdate 1 2 4])"
        "(\"12abc567abc90\", VStringDiffs [StringUpdate 1 2 5, StringUpdate 4 6 3])"
  -- newStart repStart newEnd ... repEnd
  |> evalElmAssert2 [("updateReplace", UpdateRegex.updateReplace EvalUpdate.eval EvalUpdate.update)]
        "updateReplace \"ab\" \"abc\" \"12ab567ab90\" (VStringDiffs [StringUpdate 1 7 2])"
        "(\"12abc567abc90\", VStringDiffs [StringUpdate 1 8 4, StringUpdate 11 13 3])"
  -- repStart newStart newEnd ... repEnd
  |> evalElmAssert2 [("updateReplace", UpdateRegex.updateReplace EvalUpdate.eval EvalUpdate.update)]
        "updateReplace \"abc\" \"abcd\" \"12abc567abc90\" (VStringDiffs [StringUpdate 3 9 1])"
        "(\"12abcd567abcd90\", VStringDiffs [StringUpdate 2 10 4, StringUpdate 13 16 4])"
  -- repStart newStart repEnd ... newEnd
  |> evalElmAssert2 [("updateReplace", UpdateRegex.updateReplace EvalUpdate.eval EvalUpdate.update)]
        "updateReplace \"ab\" \"abc\" \"12ab567ab90\" (VStringDiffs [StringUpdate 3 7 2])"
        "(\"12abc567abc90\", VStringDiffs [StringUpdate 2 7 4, StringUpdate 9 11 3])"
  |>  evalElmAssert2 [("updateReplace", UpdateRegex.updateReplace EvalUpdate.eval EvalUpdate.update)]
        "updateReplace \"\"\"(<(ul|ol)>(?:(?!</\\2>)[\\s\\S])*)</li>\\s*<li>\"\"\" \"$1</li>\\n</$2>\\n<$2>\\n\\t<li>\" \"<ul><li>a</li><li>b</li></ul>\" (VStringDiffs [StringUpdate 14 14 10])"
        "(\"<ul><li>a</li>\\n</ul>\\n<ul>\\n\\t<li>b</li></ul>\", VStringDiffs [StringUpdate 0 14 37])"
    -- Add the test <i>Hello <b>world</span></i> --> <i>Hello <b>world</b></i>  (make sure to capture all closing tags)
  |> updateElmAssert2 builtinEnv "replaceAllIn \"\\\\$(\\\\w+|\\\\$)\" (\\m -> m.match) \"printer\"" "\"$translation1\""
                                 "replaceAllIn \"\\\\$(\\\\w+|\\\\$)\" (\\m -> m.match) \"$translation1\""

  --|> onlyBefore
  |> updateElmPrelude (
      ExamplesGenerated.mapMaybeLens
        |> replaceStr "\n$" "" -- Remove newlines
        |> replaceStr "showValues \\[maybeState1, maybeState2, maybeState3, maybeState4\\]" "showValues [maybeState3, maybeState4]"
        |> replaceStr "\r?\nmaybeState(1|2).*" ""
      ) (replaceStr "New Jersey" "New Jersay")
      (replaceStr "New Jersey" "New Jersay")
  |> skipBefore
  |> summary
