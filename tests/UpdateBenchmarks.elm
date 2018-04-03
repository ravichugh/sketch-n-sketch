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

programs = Dict.fromList [("Markdown", ExamplesGenerated.fromleo_markdown)]

type Benchmark = BUpdate String (String -> String)

benchmarks: List Benchmark
benchmarks = [
  BUpdate    "Markdown" (replace (AtMost 1) (regex "demo") (\_ -> "demonstration"))
  ]

runBenchmark: Benchmark -> String
runBenchmark b = case b of
  BUpdate progname replacement ->
    let prog = Dict.get progname programs |> Utils.fromJust_ "Prog" in
    let (progExp, (_, _, parseProgTime)) = averageTimedRun nToAverageOn (\_ -> parse prog |> Utils.fromOk "parse prog") in
    let (oldOut, (_, _, evalProgTime)) = averageTimedRun nToAverageOn (\_ -> evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog") in
    let evalTime = parseProgTime + evalProgTime in
    let newOut = valToString oldOut |> replacement |> parse |> Utils.fromOk "parse newout" |> eval |> Utils.fromOk "eval newout" in

    let (_, (fastestUnopt, slowestUnopt, averageUnopt)) =
      averageTimedRun nToAverageOn (\_ -> update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut VConstDiffs) LazyList.Nil)
    in

    let (newProgExp, (fastestOpt, slowestOpt, averageOpt)) = averageTimedRun nToAverageOn (\_ ->
         let diffs = UpdateUtils.defaultVDiffs oldOut newOut |> Utils.fromOk "defaultVDiffs" |> Utils.fromJust_ "defaultVDiffs" in
         case update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut diffs) LazyList.Nil of
            Results.Oks (LazyList.Cons head lazyTail as ll) -> head
            Results.Errs msg -> Debug.crash msg
            _ -> Debug.crash <| "No solution for " ++ progname
      )
    in
    let speedup unopt opt = " (" ++ toString (toFloat (floor (10 * unopt / opt)) / 10) ++ "x)" in
    let speedupfastest = speedup fastestUnopt fastestOpt in
    let speedupslowest = speedup slowestUnopt slowestOpt in
    let speedupaverage = speedup averageUnopt averageOpt in
    "\\tableRow   {" ++
    String.padLeft 20 ' ' progname ++ "} {" ++
    String.pad 3 ' ' (toString <| loc prog) ++ "} {" ++
    String.pad 4 ' ' (toString <| ceiling evalTime) ++ "} { ?  } {  ?  } {" ++
    String.pad 15 ' ' (toString fastestUnopt ++ "/" ++ toString fastestOpt ++ speedupfastest) ++ "} {" ++
    String.pad 15 ' ' (toString slowestUnopt ++ "/" ++ toString slowestOpt ++ speedupslowest) ++ "} {" ++
    String.pad 15 ' ' (toString averageUnopt ++ "/" ++ toString averageOpt ++ speedupaverage) ++ "}"
    |> ImpureGoodies.log

header =
  ImpureGoodies.log """
%
% Benchmark Rows
%
% \\tableRow {                    } {   } {    } {   Session  } {  Fastest Upd  } {  Slowest Upd  } {  Average Upd  }
% \\tableRow {     Example        } {LOC} {Eval} {Time} {\\#Upd} {  Unopt / Opt  } {  Unopt / Opt  } {  Unopt / Opt  }"""

compute = header::List.map runBenchmark benchmarks

parse = Syntax.parser Syntax.Elm >> Result.mapError (\p -> ParserUtils.showError p)
unparse = Syntax.unparser Syntax.Elm
evalEnv env exp = Eval.doEval Syntax.Elm env exp |> Result.map (Tuple.first >> Tuple.first)
eval exp = Eval.doEval Syntax.Elm [] exp |> Result.map (Tuple.first >> Tuple.first)


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