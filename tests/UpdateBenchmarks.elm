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

exportMode = True
fastButWrong = True {-- -- Add a } to make the benchmark run for all
  && False
--}

nToAverageOn = if fastButWrong then 1 else 10

programs = Dict.fromList [
  ("Table of States A", ExamplesGenerated.tableOfStatesA),
  ("Table of States B", ExamplesGenerated.tableOfStatesB),
  --("Markdown Recursive", ExamplesGenerated.fromleo_markdown),
  --("Markdown Linear",    ExamplesGenerated.fromleo_markdown_optimized),
  ("Markdown",    ExamplesGenerated.fromleo_markdown_optimized),
  --("Markdown with lens", ExamplesGenerated.fromleo_markdown_optimized),
  --("Markdown w/o lens", ExamplesGenerated.fromleo_markdown_optimized_lensless),
  ("Recipe", ExamplesGenerated.fromleo_recipe2),
  ("Budgetting", ExamplesGenerated.fromleo_conference_budgetting),
  ("Programmable doc", ExamplesGenerated.fromleo_programmabledoc),
  ("Model view Controller", ExamplesGenerated.fromleo_modelviewcontroller)
  ]

type OutTransform =
  StringTransform String (String -> String) | ValTransform String (Val -> Val) | HTMLTransform String (String -> String)

type Transform = NoTransform
  | OutputTransform OutTransform
  | ProgTransform (String -> String)
  | SetNextChoice Int
  | TestOutputContains String (Val -> String)

type Benchmark = BUpdate Int String (List Transform)

replaceByMatch: HowMany -> ((String -> String) -> a) -> String -> (Regex.Match -> String) -> a
replaceByMatch howmany final r replacement =
  final <| Regex.replace howmany (regex r) replacement

replaceBy: HowMany -> ((String -> String) -> a) -> String -> String -> a
replaceBy howmany final r result =
  replaceByMatch howmany final r (\_ -> result)

replaceStringBy: String -> String -> Transform
replaceStringBy r replacement = replaceBy (AtMost 1) (StringTransform (r ++ "->" ++ replacement) >> OutputTransform) r replacement

replaceHtmlBy: String -> String -> Transform
replaceHtmlBy r replacement = replaceBy (AtMost 1) (HTMLTransform (r ++ "->" ++ replacement)>> OutputTransform) r replacement

replaceHtmlByReg r replacement = replaceByMatch (AtMost 1) (HTMLTransform (r ++ "->") >> OutputTransform) r replacement

replaceMultiple: String -> List Transform -> Transform
replaceMultiple msg transforms =
  OutputTransform <| ValTransform msg (\input ->
    List.foldl (\t v ->
      case t of
        SetNextChoice _ -> Debug.crash "Cannot invoke SeteNextChoice in a replaceMultiple"
        TestOutputContains _ _ -> Debug.crash "Cannot invok TestOutputContains in a replaceMupliple"
        NoTransform -> v
        OutputTransform to ->
          applyTransform to v
        ProgTransform _ ->
          Debug.crash "Cannot invoke ProgTransform in a replaceMultiple"
    ) input transforms
  )

replaceStringAllBy r replacement = replaceBy All (StringTransform (r ++ "->" ++ replacement)>> OutputTransform) r replacement
replaceHtmlAllBy r replacement = replaceBy All (HTMLTransform (r ++ "->" ++ replacement) >> OutputTransform) r replacement

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

at l n = Utils.nth l n |> Utils.fromOk "at" |> Maybe.withDefault ""

benchmarks_: List Benchmark
benchmarks_ = [
  {--}
  BUpdate 0 "Model view Controller" [NoTransform
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Increment)" "trigger='#'"
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Increase multiplier to 3)" "trigger='#'"
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Multiply by 3)" "trigger='#'"
    , replaceHtmlBy "51" "27"
    , replaceHtmlBy "1\\+n\\*3" "(1+n)*2"
    , replaceHtmlBy "trigger=''(?=.*\\r?\\n.*Custom Code)" "trigger='#'"
    , replaceHtmlBy "architect" "design"
    , replaceMultiple "dashes in title" [NoTransform
        , replaceHtmlBy "Model-View-Controller" "Model View Controller"
        , replaceHtmlBy "model-view-controller" "model view controller"
       ]
    , replaceHtmlBy "Increment" "+1"
    , replaceHtmlBy "Decrease multiplier to" "Decrease to"
  ],
  --}
  {--}
  BUpdate 0 "Budgetting" [ NoTransform
                       , SetNextChoice 3
                       , replaceHtmlBy "-18000" "0"
                       , replaceProgBy "sponsors *= *20000" "sponsors = Update.freeze 35000"
                       , SetNextChoice 2
                       , replaceHtmlBy "3000" "0"
                       , replaceProgBy "sponsors = Update.freeze 35000" "sponsors = Update.freeze 29000"
                       , SetNextChoice 2
                       , replaceHtmlBy "-6000" "0"
                       , replaceHtmlBy "-3000" "0"
                       ],
  --}
  {--}
  BUpdate (2*60+36) "Table of States A" [ NoTransform
    , replaceProgBy "\"Alabama\", \"AL?\", \"\"" "\"Alabama\", \"AL?\", \"Montgomery\""
    , replaceProgBy "\"Alaska\", \"AL?\", \"\"" "\"Alaska\", \"AL?\", \"Juneau\""
    , replaceHtmlBy "AL\\?" "AL"
    , replaceHtmlBy "AL\\?" "AK"
    , replaceHtmlBy ", AR" "Phoenix, AZ"
    , replaceProgBy "\\+ *\", \"" "+ Update.freeze \", \""
    , replaceMultiple "The last four states" [NoTransform
        , replaceHtmlBy ", AR\\?" "Little Rock, AR"
        , replaceHtmlBy ", CA\\?" "Sacramento, CA"
        , replaceHtmlBy ", CO\\?" "Denver, CO"
        , replaceHtmlBy ", CO\\?" "Hartford, CT"
        ]
    , replaceHtmlBy "lightgray" "yellow"
    , replaceHtmlBy "yellow" "lightblue"
    , replaceHtmlBy "lightblue" "lightcoral"
    , replaceHtmlBy "lightcoral" "lightgoldenrodyellow"
    , replaceHtmlBy "padding:3px" "padding:3px; background-color:orange"
    , replaceHtmlBy "background-color:orange" "background-color:orangered"
    , replaceHtmlBy "background-color:orangered" "background-color:orange"
  ],
  --}
  BUpdate (0*60+43) "Table of States B" [NoTransform
    , replaceMultiple "Click on the last + button" [NoTransform
        , replaceHtmlByReg "(Hartford.*(\r?\n).*(\r?\n).*)style='color:gray'" (\m -> at m.submatches 0 ++ "style='color:coral'")
        , replaceHtmlByReg "has-been-clicked='False'(.*(\r?\n).*(\r?\n).*Connecticut)" (\m -> "has-been-clicked='True'" ++ at m.submatches 0)
        ]
    , replaceMultiple "Click on the last + button" [NoTransform
        , replaceHtmlByReg "(\\?, \\?.*(\r?\n).*(\r?\n).*)style='color:gray'" (\m -> at m.submatches 0 ++ "style='color:coral'")
        , replaceHtmlByReg "has-been-clicked='False'(.*(\r?\n).*(\r?\n).*\\?)" (\m -> "has-been-clicked='True'" ++ at m.submatches 0)
             ]
    , replaceHtmlBy "\\?" "Delaware"
    , replaceHtmlBy "\\?" "Dover"
    , replaceHtmlBy "\\?" "DE"
    , replaceHtmlBy "\\?" "Florida"
    , replaceHtmlBy "\\?, \\?" "Tallahassee, FL"
    ],
  --}
  {--}
  --BUpdate "Markdown Recursive" transform_markdown_ab_linear,
  BUpdate 0 "Markdown" transform_markdown_ab_linear,
  --BUpdate "Markdown with lens" transform_markdown_ab_lens,
  --BUpdate "Markdown w/o lens" transform_markdown_ab_lens,
  --}
  {--}
  BUpdate (3*60+51) "Recipe" [ NoTransform
                   , replaceHtmlBy " alt='cupcakes'" " alt='cupcakes' style='\\n  float:  right;  \\n:  '"
                   --, TestOutputContains "float:  right" valToHtml
                   , replaceHtmlBy "Chocolate almond cakes"        "Chocolate Almond Cupcakes"
                   --, TestOutputContains "Chocolate Almond Cupcakes" valToHtml
                   , replaceStringBy "\\[\\s*\"h1\",\\s*\\[\\],\\s*\\[\\s*\\[\\s*\"TEXT\",\\s*\"Chocolate Almond Cupcakes\""
                                      "[ \"h1\", [ [ \"style\", [ [ \"\\n  font-family\", \" cursive\" ], [ \" \\n\",\" \"]]] ], [ [ \"TEXT\", \"Chocolate Almond Cupcakes\""
                   , replaceHtmlBy "border:4px solid black;padding:20px" "border:4px solid black;padding:20px;background-color:chocolate"
                   , replaceHtmlBy "x='1000'(?=.*\\r?\\n.*\\r?\\n.*Halve)" "x='500'"
                   --, TestOutputContains "10 small" valToHtml
                   , replaceHtmlBy "melted chocolate\\s*</li>" "melted chocolate</li><li>_10_g of chocolate chip_10s_</li>"
                   , replaceHtmlBy "10 small" "80 small"
                   , replaceHtmlBy "2 cup of" "2 cup_2s_ of"
                   , replaceHtmlBy "2 cups of" "2 cup of"
                   , replaceHtmlBy "40 small" "30 small"
                   , replaceHtmlBy "30g of chocolate" "1g of chocolate"
                   , replaceHtmlBy "small cakes" "small cake_1s_"
                   --, SetNextChoice 2
                   , replaceHtmlBy "x='50'" "x='100'"
                   , replaceHtmlBy "x='100'" "x='200'"
                   , replaceHtmlBy "x='200'" "x='400'"
                   , replaceHtmlBy "x='400'" "x='800'"
                   , replaceHtmlBy "x='800'" "x='1600'"
                   ],
  --}
  BUpdate 0 "" []
  ]

benchmarks = List.filterMap (\c -> case c of
  BUpdate _ "" _ -> Nothing
  _ -> Just c ) benchmarks_

valToHtml: Val -> String
valToHtml oldOut =
  let slate = LangSvg.resolveToRootedIndexedTree Syntax.Elm 1 1 0 oldOut |> Utils.fromOk "html newOut" in
  let html = LangSvg.printHTML False slate in
  html

transformName: OutTransform -> String
transformName x = case x of
  StringTransform name _ -> name
  HTMLTransform name _ -> name
  ValTransform name _ -> name

transformValToString: OutTransform -> Val -> String
transformValToString x = case x of
  StringTransform _ _ -> valToString
  HTMLTransform _ _ -> valToHtml
  ValTransform _ _ -> valToHtml

applyTransform: OutTransform -> Val -> Val
applyTransform replacement oldOut =
  case replacement of
    StringTransform _ replacement ->
      valToString oldOut |> replacement |>parse |> Utils.fromOk "parse newout" |> eval |> Utils.fromOk "eval newout"
    ValTransform _ replacement -> replacement oldOut
    HTMLTransform _ replacement ->
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

sToMinutsSeconds: Float -> String
sToMinutsSeconds s = msToMinutsSeconds (s * 1000)

msToMinutsSeconds: Float -> String
msToMinutsSeconds ms =
  let s = ceiling (ms / 1000) in
  if s < 60 then "0:" ++ String.padLeft 2 '0' (toString s)
  else toString (floor (toFloat s / 60) )++ ":" ++ String.padLeft 2 '0' (toString (s % 60))

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
      let _ = if not exportMode then ImpureGoodies.log <| "Session #" ++ toString i ++ " " ++
            (if unopt then "unoptimized" else "optimized") ++" for " ++ benchmarkname  else "" in
      let (_, _, updateTimes, evalTimes, modifTimes, _) =
           List.foldl (\step (progExp, oldOut, updateTimes, evalTimes, modifTimes, choice) ->
           {-if (evalEnv EvalUpdate.preludeEnv progExp |> Utils.fromOk "eval prog" |> valToString) /= (valToString oldOut) then
             Debug.crash "invariant failed: the oldOut is not generated by the newOut"
           else-}
           let _ = if not exportMode then ImpureGoodies.log <| "Apply transformation..."  else ""in
           --let _ = ImpureGoodies.log (toString unopt) in
           --let _ = ImpureGoodies.log "Current program:" in
           --let _ = ImpureGoodies.log (unparse progExp) in
           --let _ = ImpureGoodies.log "Current out:" in
           --let _ = ImpureGoodies.log (valToHtml oldOut) in
           --let _ = ImpureGoodies.log ("New modifications: " ++ toString replacement)  in
           case step of
             NoTransform -> Debug.crash "NoTransform should have been removed"
             SetNextChoice n -> (progExp, oldOut, updateTimes, evalTimes, modifTimes, n)
             TestOutputContains re vToString ->
               if Regex.contains (Regex.regex re) (vToString oldOut) then
                 (progExp, oldOut, updateTimes, evalTimes, modifTimes, choice)
               else
                 Debug.crash <| "The output did not contain '" ++ re ++ "' : " ++ vToString oldOut ++ ", program having generated it:\n " ++ unparse progExp
             ProgTransform p ->
               case parse (p (unparse progExp)) of
                 Err msg -> Debug.crash msg
                 Ok newProgExp ->
                   let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv newProgExp |> Utils.fromOk "eval changed prog" in
                   (newProgExp, realNewOut, updateTimes, realNewOutTime::evalTimes, modifTimes, 1)
             OutputTransform replacement ->
               --let _ = Debug.log ("oldOut\n" ++ valToString oldOut) () in
               --let _ = Debug.log ("oldOut\n" ++ valToHtml oldOut) () in
               let (newOut, newOutTime) = ImpureGoodies.timedRun <| \_ -> applyTransform replacement oldOut in
               --let _ = Debug.log ("newOut\n" ++ valToString newOut) () in
               --let _ = Debug.log ("newOut\n" ++ valToHtml newOut) () in
               let _ = if not exportMode then ImpureGoodies.log <| "It took " ++ toString newOutTime ++ "ms. Update with newOut..."  else "" in
               --let _ = ImpureGoodies.log (valToHtml newOut) in
               let (newProgExp_, newProgExpDiffs, updateTime) = (if unopt then
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut VUnoptimizedDiffs)
                  else
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.doUpdateWithoutLog progExp oldOut newOut) |> \(x, t) -> case x of
                       Results.Oks (LazyList.Nil) -> Debug.crash <| "No solution for " ++ benchmarkname
                       Results.Oks ll -> let (newEnv, newExp) = LazyList.elemAt (choice - 1) (LazyList.filter (\(env, e) -> env.changes == []) ll) |> Utils.fromJust_ "LazyList"
                         in
                         if newExp.changes == Nothing then
                           Debug.crash <| "Expected a change to the expression, got Nothing.\nTransform =  " ++ transformName replacement ++ "\n" ++ (transformValToString replacement newOut)
                         else
                          (newExp.val, newExp.changes |> Utils.fromJust,  t)
                       Results.Errs msg -> Debug.crash <| msg ++ "Transform =  " ++ transformName replacement ++ "\n" ++ (transformValToString replacement newOut)  ++ "\n" ++ unparse progExp
               in
               let newProgExp = parse (unparse newProgExp_) |> Utils.fromOk_ in
               --let _ = ImpureGoodies.log (eDiffsToString "" progExp newProgExp newProgExpDiffs) in
               let _ = if not exportMode then ImpureGoodies.log <| "It took " ++ toString updateTime ++ "ms. Recomputing realOut..."  else "" in
               --let _ = ImpureGoodies.log "new program:" in
               --let _ = ImpureGoodies.log (unparse newProgExp) in
               let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv newProgExp |> Utils.fromOk "eval prog" in
               let _ = if not exportMode then ImpureGoodies.log <| "It took " ++ toString realNewOutTime ++ "ms."  else "" in
               --let _ = ImpureGoodies.log "Real out:" in
               --let _ = ImpureGoodies.log (valToHtml realNewOut) in
               (newProgExp, realNewOut, updateTime::updateTimes, realNewOutTime::evalTimes, newOutTime::modifTimes, 1)
               ) (progExp, oldOut, [], [], [], 1) finalReplacements
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

    let averageUnoptSessionTime = msToMinutsSeconds <| average <| List.map Tuple.first unoptResults in
    let averageOptSessionTime   = msToMinutsSeconds <| average <| List.map Tuple.first optResults in

    let speedupfastest = speedup fastestUnopt fastestOpt in
    let speedupslowest = speedup slowestUnopt slowestOpt in
    let speedupaverage = speedup averageUnopt averageOpt in
    let locprog = loc prog in
    let finalEvalTime = ceiling evalTime in
    let latexRow = "\\tableRow   {" ++
    String.padLeft 20 ' ' benchmarkname ++ "} {" ++
    String.pad 3 ' ' (toString <| locprog) ++ "} {" ++
    String.pad 4 ' ' (toString <| finalEvalTime) ++ "} { "++
    String.pad 6 ' ' (sToMinutsSeconds (toFloat sessionTime)) ++"  } { "++
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
\\newcommand{\\benchmarkssessiontime}{""" ++ sToMinutsSeconds (toFloat time) ++ """}
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