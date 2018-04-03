module UpdateBenchmarks exposing (..)

import Helpers.Matchers exposing (..)

import Update exposing (..)
import UpdateRegex exposing (..)
import UpdateStack exposing (..)
import UpdateUtils exposing (..)
import Lang exposing (..)
import Regex exposing (replace, regex, HowMany(..))
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
import ExamplesGenerated
import Dict
import EvalUpdate
import ImpureGoodies

nToAverageOn = 10

programs = Dict.fromList [
  ("Markdown", ExamplesGenerated.fromleo_markdown),
  ("Recipe", ExamplesGenerated.fromleo_recipe),
  ("Budgetting", ExamplesGenerated.fromleo_conference_budgetting)
  ]

type Benchmark = BUpdate String (List (String -> String))

replaceBy: String -> String -> (String -> String)
replaceBy r result =
  replace (AtMost 1) (regex r) (\_ -> result)

benchmarks: List Benchmark
benchmarks = [
  BUpdate "Budgetting" [replaceBy "-18000" "0"],
  BUpdate "Markdown" [replaceBy "demo" "demonstration",
                      replaceBy "bidirectional" "two-directional",
                      replaceBy "\"Do not use CTRL\\+V\"" "\"Do not use CTRL+V\"]]],[ \"li\", [], [ [ \"TEXT\", \"But use everything else\""], --",
  BUpdate "Recipe" [replaceBy "Soft chocolate" "Delicious soft chocolate"]--, --Text transformation
  --                  replaceBy "20 small cakes" "80 small cakes" --Number lens
   --                 replaceBy "small cakes"    "small cake_80s_", --Plural lens
    --                replaceBy "cup of "        "cup_2s_ of " --Plural lens
                    --replaceBy "cups of "       "cup of ",  -- Second lens
                    --replaceBy "\"2000\""       "\"1000\"", -- Simulate press on button
                    --replaceBy "\"A pinch of salt\"" "\"A pinch of salt\"]]],[ \"li\", [], [ [ \"TEXT\", \"Optional chocolate chips\""
                    --]
  ]

applyTransform: (String -> String) -> Val -> Val
applyTransform replacement oldOut =
  valToString oldOut |> replacement |> parse |> Utils.fromOk "parse newout" |> eval |> Utils.fromOk "eval newout"


mbcmp: (number -> number -> number) -> Maybe number -> number -> Maybe number
mbcmp f x n = case x of
  Nothing -> Just n
  Just y -> Just (f y n)

mbmin = mbcmp min
mbmax = mbcmp max
mbacc = mbcmp (+)

toMinuteSeconds: Float -> String
toMinuteSeconds ms =
  let s = ceiling (ms / 1000) in
  if s < 60 then "0:" ++ String.padLeft 2 '0' (toString s)
  else toString (toFloat s / 60) ++ ":" ++ String.padLeft 2 '0' (toString (s % 60))

runBenchmark: Benchmark -> String
runBenchmark b = case b of
  BUpdate benchmarkname replacements ->
    let numberOfUpdates = List.length replacements in
    let prog = Dict.get benchmarkname programs |> Utils.fromJust_ "Prog" in
    let (progExp, (_, _, parseProgTime)) = averageTimedRun nToAverageOn (\_ -> parse prog |> Utils.fromOk "parse prog") in
    let (oldOut, (_, _, evalProgTime)) = averageTimedRun nToAverageOn (\_ -> evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog") in
    let evalTime = parseProgTime + evalProgTime in

    -- Returns (List of the total time of the session, and as many update times as there are replacements)
    let session: Bool -> (Float, List Float)
        session unopt =
      let (_, _, updateTimes, evalTimes, modifTimes) = List.foldl (\replacement (progExp, oldOut, updateTimes, evalTimes, modifTimes) ->
           --let _ = ImpureGoodies.log (toString unopt) in
           --let _ = ImpureGoodies.log "Current program:" in
           --let _ = ImpureGoodies.log (unparse progExp) in
           --let _ = ImpureGoodies.log "New modifications:" in
           let (newOut, newOutTime) = ImpureGoodies.timedRun <| \_ -> applyTransform replacement oldOut in
           --let _ = ImpureGoodies.log (valToString newOut) in
           let (newProgExp, updateTime) = (if unopt then
                ImpureGoodies.timedRun <| \_ ->
                update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut VUnoptimizedDiffs) LazyList.Nil
              else
                ImpureGoodies.timedRun <| \_ ->
                let diffs = UpdateUtils.defaultVDiffs oldOut newOut |> Utils.fromOk (benchmarkname ++ "defaultVDiffs") |> Utils.fromJust_ (benchmarkname ++ "defaultVDiffs") in
                update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut diffs) LazyList.Nil) |> \(x, t) -> case x of
                   Results.Oks (LazyList.Cons (headEnv, headExp) lazyTail as ll) -> (headExp.val, t)
                   Results.Errs msg -> Debug.crash msg
                   _ -> Debug.crash <| "No solution for " ++ benchmarkname
           in
           --let _ = ImpureGoodies.log "new program:" in
           --let _ = ImpureGoodies.log (unparse newProgExp) in
           let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog" in
           (newProgExp, realNewOut, updateTime::updateTimes, realNewOutTime::evalTimes, newOutTime::modifTimes)
           ) (progExp, oldOut, [], [], []) replacements
      in
      (List.sum updateTimes + List.sum evalTimes + List.sum modifTimes, List.reverse updateTimes)
    in
    let unoptResults: List (Float, List Float)
        unoptResults = tryMany nToAverageOn <| \_ ->
         session True
    in
    let optResults: List (Float, List Float)
        optResults = tryMany nToAverageOn <| \_ ->
         session False
    in
    let allUpdateTimes = List.concatMap Tuple.second in
    let allUnoptTimes = allUpdateTimes unoptResults in
    let allOptTimes = allUpdateTimes optResults in
    let fastestUnopt = List.minimum allUnoptTimes |> Utils.fromJust_ "minimum unopt" in
    let fastestOpt   = List.minimum allOptTimes   |> Utils.fromJust_ "minimum opt" in
    let slowestUnopt = List.maximum allUnoptTimes |> Utils.fromJust_ "maximum unopt" in
    let slowestOpt   = List.maximum allOptTimes   |> Utils.fromJust_ "maximum opt" in
    let averageUnopt = List.sum allUnoptTimes / toFloat (nToAverageOn * numberOfUpdates) in
    let averageOpt   = List.sum allOptTimes   / toFloat (nToAverageOn * numberOfUpdates) in

    let averageUnoptSessionTime = toMinuteSeconds ((List.map Tuple.first unoptResults |> List.sum) / nToAverageOn) in
    let averageOptSessionTime   = toMinuteSeconds ((List.map Tuple.first optResults |> List.sum) / nToAverageOn) in

    let speedup unopt opt = " (" ++ toString (toFloat (floor (10 * unopt / opt)) / 10) ++ "x)" in
    let speedupfastest = speedup fastestUnopt fastestOpt in
    let speedupslowest = speedup slowestUnopt slowestOpt in
    let speedupaverage = speedup averageUnopt averageOpt in
    let latexRow = "\\tableRow   {" ++
    String.padLeft 20 ' ' benchmarkname ++ "} {" ++
    String.pad 3 ' ' (toString <| loc prog) ++ "} {" ++
    String.pad 4 ' ' (toString <| ceiling evalTime) ++ "} { "++
    String.pad 6 ' ' (averageUnoptSessionTime ++ "/" ++ averageOptSessionTime) ++"  } { "++
    String.pad 3 ' ' (toString numberOfUpdates) ++" } {" ++
    String.pad 15 ' ' (toString fastestUnopt ++ "/" ++ toString fastestOpt ++ speedupfastest) ++ "} {" ++
    String.pad 15 ' ' (toString slowestUnopt ++ "/" ++ toString slowestOpt ++ speedupslowest) ++ "} {" ++
    String.pad 15 ' ' (toString (ceiling averageUnopt) ++ "/" ++ toString (ceiling averageOpt) ++ speedupaverage) ++ "} \\\\"
    in
    let _ = ImpureGoodies.log latexRow in
    let rendersession results = List.map (\(session, upds) ->
       "\n% session: " ++ toString session ++ ", updates: " ++
         (List.map toString upds |> String.join ",")) results |> String.join "" in
    let rawdata = "\n% " ++ benchmarkname ++ " - Unopt" ++ rendersession unoptResults ++
      "\n% " ++ benchmarkname ++ " - Opt" ++ rendersession optResults in
    rawdata

header =
  ImpureGoodies.log """
%
% Benchmark Rows
%
% \\tableRow {                    } {   } {    } {     Session     } {  Fastest Upd  } {  Slowest Upd  } {  Average Upd  }
% \\tableRow {     Example        } {LOC} {Eval} {  Time  } {\\#Upd } {  Unopt / Opt  } {  Unopt / Opt  } {  Unopt / Opt  }"""

compute = List.foldl (\b acc -> acc ++ runBenchmark b) "" benchmarks |> ImpureGoodies.log

parse = Syntax.parser Syntax.Elm >> Result.mapError (\p -> ParserUtils.showError p)
unparse = Syntax.unparser Syntax.Elm
evalEnv env exp = Eval.doEval Syntax.Elm env exp |> Result.map (Tuple.first >> Tuple.first)
eval exp = Eval.doEval Syntax.Elm [] exp |> Result.map (Tuple.first >> Tuple.first)

tryMany: Int -> (() -> a) -> List a
tryMany n callback =
  List.range 1 n |>
  List.map (\i -> callback ())

averageTimedRun: Int -> (() -> a) -> (a, (Float, Float, Float))
averageTimedRun n callback =
  List.range 1 n |>
  List.foldl (\_ (oldResult, (oldFastest, oldSlowest, oldAcc)) ->
    let (newResult, newTime) = ImpureGoodies.timedRun callback in
    let newFastest = Maybe.map (min newTime) oldFastest |> Maybe.withDefault newTime |> Just in
    let newSlowest = Maybe.map (max newTime) oldSlowest |> Maybe.withDefault newTime |> Just in
    (Just newResult, (newFastest, newSlowest, oldAcc + newTime))) (Nothing, (Nothing, Nothing, 0)) |>
  \(newResult, (fastest, slowest, sumTime)) -> (newResult |> Utils.fromJust, (Utils.fromJust fastest, Utils.fromJust slowest, sumTime / toFloat n))

loc s = String.lines s |> List.length