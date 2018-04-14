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
import LangSvg
import HTMLParser
import HTMLValParser

finalMode = False

nToAverageOn = if finalMode then 10 else 1

programs = Dict.fromList [
  ("Table of States A", ExamplesGenerated.tableOfStatesA),
  ("Table of States B", ExamplesGenerated.tableOfStatesB),
  ("Markdown Recursive", ExamplesGenerated.fromleo_markdown),
  ("Markdown Linear",    ExamplesGenerated.fromleo_markdown_optimized),
  ("Markdown",    ExamplesGenerated.fromleo_markdown_optimized),
  ("Markdown with lens", ExamplesGenerated.fromleo_markdown_optimized),
  ("Markdown w/o lens", ExamplesGenerated.fromleo_markdown_optimized_lensless),
  ("Recipe", ExamplesGenerated.fromleo_recipe),
  ("Budgetting", ExamplesGenerated.fromleo_conference_budgetting)
  ]

type OutTransform = StringTransform (String -> String) | ValTransform (Val -> Val) | HTMLTransform (String -> String)

type Transform = NoTransform | OutputTransform OutTransform | ProgTransform (String -> String)

type Benchmark = BUpdate Int String (List Transform)

replaceByMatch: HowMany -> ((String -> String) -> a) -> String -> (Regex.Match -> String) -> a
replaceByMatch howmany final r replacement =
  final <| Regex.replace howmany (regex r) replacement

replaceBy: HowMany -> ((String -> String) -> a) -> String -> String -> a
replaceBy howmany final r result =
  replaceByMatch howmany final r (\_ -> result)

replaceStringBy: String -> String -> Transform
replaceStringBy = replaceBy (AtMost 1) (StringTransform >> OutputTransform)

replaceHtmlBy: String -> String -> Transform
replaceHtmlBy = replaceBy (AtMost 1) (HTMLTransform >> OutputTransform)

replaceHtmlByReg = replaceByMatch (AtMost 1) (HTMLTransform >> OutputTransform)

replaceMultiple: List Transform -> Transform
replaceMultiple transforms =
  OutputTransform <| ValTransform (\input ->
    List.foldl (\t v ->
      case t of
        NoTransform -> v
        OutputTransform to ->
          applyTransform to v
        ProgTransform _ ->
          Debug.crash "Cannot invoke ProgTransform in a replaceMultiple"
    ) input transforms
  )

replaceStringAllBy = replaceBy All (StringTransform >> OutputTransform)
replaceHtmlAllBy = replaceBy All (HTMLTransform >> OutputTransform)

replaceProgBy = replaceBy Regex.All ProgTransform

transform_markdown_ab_linear = [ NoTransform
   , replaceHtmlBy "demo" "showcase"
   --, replaceHtmlBy "fr\\." "en."
   , replaceHtmlBy "bidirectional" "two-directional"
   , replaceHtmlBy "regex" "_regex_"
   , replaceStringBy "\\[\\s*\"TEXT\",\\s*\" showcase\"\\s*\\]\\s*\\]\\s*\\],"
                     "  [    \"TEXT\",    \" showcase\"      ]      ]      ],[\"div\", [], [[\"TEXT\", \"Hello\"]]],"
   , replaceHtmlBy "Do not use CTRL\\+V" "Do not use CTRL+V</li><li>But use everything else"
   , replaceHtmlBy " to introduce" " <br>to introduce"
   --, replaceHtmlBy "</code>" "</code></li><li>Add a numbered point"
   --, replaceHtmlBy "h4" "h2"
   ]

transform_markdown_ab_lens = [ NoTransform
  , replaceHtmlBy "demo" "showcase"
  , replaceHtmlBy "fr\\." "en."
  , replaceHtmlBy "bidirectional" "two-directional"
  , replaceHtmlBy "regex" "_regex_"
  , replaceHtmlAllBy "list" "lists"
  , replaceHtmlBy " to introduce" " <br>to introduce"
  , replaceHtmlBy "h4" "h2"
  ]

benchmarks: List Benchmark
benchmarks = [
  {--
  BUpdate "Budgetting" [ NoTransform
                       , replaceHtmlBy "-18000" "0"
                       ],
  --}
  {--}
  BUpdate (2*60+36) "Table of States A" [ NoTransform
    , replaceProgBy "\"Alabama\", \"AL?\", \"\"" "\"Alabama\", \"AL?\", \"Montgomery\""
    , replaceProgBy "\"Alaska\", \"AL?\", \"\"" "\"Alaska\", \"AL?\", \"Juneau\""
    , replaceHtmlBy "AL?" "AL"
    , replaceHtmlBy "AL?" "AK"
    , replaceHtmlBy ", AR" "Phoenix, AZ"
    , replaceProgBy "\\+ *\", \"" "+ Update.freeze \", \""
    , replaceMultiple [NoTransform
        , replaceHtmlBy ", AR?" "Little Rock, AR"
        , replaceHtmlBy ", CA?" "Sacramento, CA"
        , replaceHtmlBy ", CO?" "Denver, CO"
        , replaceHtmlBy ", CO?" "Hartford, CT"
        ]
    , replaceHtmlBy "lightgray" "yellow"
    , replaceHtmlBy "yellow" "lightblue"
    , replaceHtmlBy "lightblue" "lightcoral"
    , replaceHtmlBy "lightcoral" "lightgoldenrodyellow"
    , replaceHtmlBy "padding: 3px" "padding: 3px; background-color: orange"
    , replaceHtmlBy "background-color: orange" "background-color: orangered"
    , replaceHtmlBy "background-color: orangered" "background-color: orange"
  ],
  BUpdate (0*60+43) "Table of States B" [NoTransform
    , replaceMultiple [NoTransform
        , replaceHtmlBy "color: \"gray\"" "color: \"coral\""
        , replaceHtmlByReg "has-been-clicked=( ?)\"True\"" (\m -> "has-been-clicked=" ++ (m.submatches |> List.head |> Utils.fromJust_ "BUpdate" |> Maybe.withDefault "") ++ "\"False\"")
        ]
    ],
  --}
  {--}
  --BUpdate "Markdown Recursive" transform_markdown_ab_linear,
  BUpdate 0 "Markdown" transform_markdown_ab_linear,
  --BUpdate "Markdown with lens" transform_markdown_ab_lens,
  --BUpdate "Markdown w/o lens" transform_markdown_ab_lens,
  --}
  {--
  BUpdate "Recipe" [ NoTransform
                   --, replaceHtmlBy "Soft chocolate" "Delicious soft chocolate" --Text transformation
                   , replaceHtmlBy "20 small cakes" "80 small cakes" --Number lens
                   , replaceHtmlBy "cup of "        "cup_2s_ of " --Plural lens
                   , replaceHtmlBy "cups of "       "cup of "  -- Second lens
                   , replaceHtmlBy "small cakes"    "small cakes for _12_ persons_12s_" --Plural lens
                   , replaceHtmlBy "12 persons"    "18 persons" --Plural lens
                   , replaceHtmlBy "'2000'"       "'1000'" -- Simulate press on button
                   , replaceStringBy "\"A pinch of salt\"" "\"A pinch of salt\"]]],[ \"li\", [], [ [ \"TEXT\", \"Optional chocolate chips\""
                   ],
  --}
  BUpdate 0 "" []
  ]

valToHtml: Val -> String
valToHtml oldOut =
  let slate = LangSvg.resolveToRootedIndexedTree Syntax.Elm 1 1 0 oldOut |> Utils.fromOk "html newOut" in
  let html = LangSvg.printHTML False slate in
  html

applyTransform: OutTransform -> Val -> Val
applyTransform replacement oldOut =
  case replacement of
    StringTransform replacement ->
      valToString oldOut |> replacement |>parse |> Utils.fromOk "parse newout" |> eval |> Utils.fromOk "eval newout"
    ValTransform replacement -> replacement oldOut
    HTMLTransform replacement ->
      --let _ = ImpureGoodies.log html in
      let newHTML = replacement (valToHtml oldOut) in
      case HTMLParser.parseHTMLString newHTML of
        Ok [node] ->
          HTMLValParser.htmlNodeToElmViewInLeo (builtinVal "UpdateBenchmarks") node
        Ok nodes -> Debug.crash <| "Expected only one node after applying HTML transformation, got " ++ toString (List.length nodes)
        Err msg -> Debug.crash (ParserUtils.showError msg)

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

speedup unopt opt = " (" ++ toString (toFloat (floor (10 * unopt / opt)) / 10) ++ "x)"

runBenchmark: Benchmark -> (String, Int, Int, Int, Int, List Float, List Float)
runBenchmark b = case b of
  BUpdate sessionTime benchmarkname replacements ->
    if benchmarkname == "" then ("", 0, 0, 0, 0, [], []) else
    let finalReplacements = List.filter (\x -> x/= NoTransform) replacements in
    let numberOfUpdates = List.length (List.filter (\x ->
      case x of
         ProgTransform _ -> False
         _ -> True
       ) finalReplacements) in
    let prog = Dict.get benchmarkname programs |> Utils.fromJust_ "Prog"
    in
    let (progExp, (_, _, parseProgTime)) = averageTimedRun nToAverageOn (\_ -> parse prog |> Utils.fromOk "parse prog") in
    let (oldOut, (_, _, evalProgTime)) = averageTimedRun nToAverageOn (\_ -> evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog") in
    let evalTime = parseProgTime + evalProgTime in
    let origProgExp = progExp in
    -- Returns (List of the total time of the session, and as many update times as there are replacements)
    let session: Int -> Bool -> (Float, List Float)
        session i unopt =
      let _ = if not finalMode then ImpureGoodies.log <| "Session #" ++ toString i ++ " " ++
            (if unopt then "unoptimized" else "optimized") ++" for " ++ benchmarkname  else "" in
      let (_, _, updateTimes, evalTimes, modifTimes) = List.foldl (\step (progExp, oldOut, updateTimes, evalTimes, modifTimes) ->
           let _ = if not finalMode then ImpureGoodies.log <| "Apply transformation..."  else ""in
           --let _ = ImpureGoodies.log (toString unopt) in
           --let _ = ImpureGoodies.log "Current program:" in
           --let _ = ImpureGoodies.log (unparse progExp) in
           --let _ = ImpureGoodies.log "Current out:" in
           --let _ = ImpureGoodies.log (valToHtml oldOut) in
           --let _ = ImpureGoodies.log ("New modifications: " ++ toString replacement)  in
           case step of
             NoTransform -> Debug.crash "NoTransform should have been removed"
             ProgTransform p ->
               case parse (p (unparse progExp)) of
                 Err msg -> Debug.crash msg
                 Ok newProgExp ->
                   let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv newProgExp |> Utils.fromOk "eval changed prog" in
                   (newProgExp, realNewOut, updateTimes, realNewOutTime::evalTimes, modifTimes)
             OutputTransform replacement ->
               let (newOut, newOutTime) = ImpureGoodies.timedRun <| \_ -> applyTransform replacement oldOut in
               let _ = if not finalMode then ImpureGoodies.log <| "It took " ++ toString newOutTime ++ "ms. Update with newOut..."  else "" in
               --let _ = ImpureGoodies.log (valToHtml newOut) in
               let (newProgExp, updateTime) = (if unopt then
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut VUnoptimizedDiffs)
                  else
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.doUpdateWithoutLog progExp oldOut newOut) |> \(x, t) -> case x of
                       Results.Oks (LazyList.Cons (headEnv, headExp) lazyTail as ll) -> (headExp.val, t)
                       Results.Errs msg -> Debug.crash msg
                       _ -> Debug.crash <| "No solution for " ++ benchmarkname
               in
               let _ = if not finalMode then ImpureGoodies.log <| "It took " ++ toString updateTime ++ "ms. Recomputing realOut..."  else "" in
               --let _ = ImpureGoodies.log "new program:" in
               --let _ = ImpureGoodies.log (unparse newProgExp) in
               let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv newProgExp |> Utils.fromOk "eval prog" in
               let _ = if not finalMode then ImpureGoodies.log <| "It took " ++ toString realNewOutTime ++ "ms."  else "" in
               --let _ = ImpureGoodies.log "Real out:" in
               --let _ = ImpureGoodies.log (valToHtml realNewOut) in
               (newProgExp, realNewOut, updateTime::updateTimes, realNewOutTime::evalTimes, newOutTime::modifTimes)
               ) (progExp, oldOut, [], [], []) finalReplacements
      in
      (List.sum updateTimes + List.sum evalTimes + List.sum modifTimes, List.reverse updateTimes)
    in
    let optResults: List (Float, List Float)
        optResults = tryMany nToAverageOn <| \i ->
         session i False
    in
    let unoptResults: List (Float, List Float)
        unoptResults = optResults {--tryMany nToAverageOn <| \i ->
             session i True--}
    in
    let allUpdateTimes = List.concatMap Tuple.second in
    let allUnoptTimes = allUpdateTimes unoptResults in
    let allOptTimes = allUpdateTimes optResults in
    let fastestUnopt = List.minimum allUnoptTimes |> Utils.fromJust_ "minimum unopt" in
    let fastestOpt   = List.minimum allOptTimes   |> Utils.fromJust_ "minimum opt" in
    let slowestUnopt = List.maximum allUnoptTimes |> Utils.fromJust_ "maximum unopt" in
    let slowestOpt   = List.maximum allOptTimes   |> Utils.fromJust_ "maximum opt" in
    let averageUnopt = average allUnoptTimes in
    let averageOpt   = average allOptTimes in

    let averageUnoptSessionTime = toMinuteSeconds <| average <| List.map Tuple.first unoptResults in
    let averageOptSessionTime   = toMinuteSeconds <| average <| List.map Tuple.first optResults in

    let speedupfastest = speedup fastestUnopt fastestOpt in
    let speedupslowest = speedup slowestUnopt slowestOpt in
    let speedupaverage = speedup averageUnopt averageOpt in
    let locprog = loc prog in
    let finalEvalTime = ceiling evalTime in
    let latexRow = "\\tableRow   {" ++
    String.padLeft 20 ' ' benchmarkname ++ "} {" ++
    String.pad 3 ' ' (toString <| locprog) ++ "} {" ++
    String.pad 4 ' ' (toString <| finalEvalTime) ++ "} { "++
    String.pad 6 ' ' (toMinuteSeconds (toFloat sessionTime)) ++"  } { "++
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
    (rawdata, locprog, finalEvalTime, sessionTime, numberOfUpdates, allUnoptTimes, allOptTimes)

header =
  ImpureGoodies.log """
\\newcommand{\\benchmarks}{
%
% Benchmark Rows
%
% \\tableRow {                    } {   } {    } {     Session     } {  Fastest Upd  } {  Slowest Upd  } {  Average Upd  }
% \\tableRow {     Example        } {LOC} {Eval} {  Time  } {\\#Upd } {  Unopt / Opt  } {  Unopt / Opt  } {  Unopt / Opt  }"""

compute = List.foldl (\b (acc, loc, eval, time, upd, updUnopt, updOpt) ->
                     let (acc2, loc2, eval2, time2, upd2, updUnopt2, updOpt2) = runBenchmark b in
                     (acc ++ acc2, loc + loc2, eval + eval2, time + time2, upd + upd2, updUnopt ++ updUnopt2, updOpt ++ updOpt2))
                     ("", 0, 0, 0, 0, [], []) benchmarks |>
                       (\(acc, loc, eval, time, upd, updUnopt, updOpt) ->
                         let _ = ImpureGoodies.log acc in
                         let unoptaverage = average updUnopt in
                         let optaverage = average updOpt in
                         ImpureGoodies.log <| """
}
\\newcommand{\\benchmarksloc}{""" ++ toString loc ++ """}
\\newcommand{\\benchmarksnum}{""" ++ toString (List.length benchmarks) ++ """}
\\newcommand{\\benchmarkseval}{""" ++ toString eval ++ """}
\\newcommand{\\benchmarkssessiontime}{""" ++ toMinuteSeconds (toFloat time) ++ """}
\\newcommand{\\benchmarksnumupd}{""" ++ toString upd ++ """}
\\newcommand{\\benchmarksaverageunoptupd}{""" ++ toString (floor unoptaverage) ++ """}
\\newcommand{\\benchmarksaverageoptupd}{""" ++ toString (floor optaverage) ++ """}
\\newcommand{\\benchmarksaveragespeedup}{""" ++ speedup unoptaverage optaverage ++ """}
""")




parse = Syntax.parser Syntax.Elm >> Result.mapError (\p -> ParserUtils.showError p)
unparse = Syntax.unparser Syntax.Elm
evalEnv env exp = Eval.doEval Syntax.Elm env exp |> Result.map (Tuple.first >> Tuple.first)
eval exp = Eval.doEval Syntax.Elm [] exp |> Result.map (Tuple.first >> Tuple.first)

average: List Num -> Float
average l = List.sum l / toFloat (List.length l)

tryMany: Int -> (Int -> a) -> List a
tryMany n callback =
  List.reverse <| List.foldl (\i b -> callback i::b) [] (List.range 1 n)

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