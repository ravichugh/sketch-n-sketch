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
import Dict exposing (Dict)


exportMode = True {-- -- Add a } to make the benchmark run for all
                    && False --}
fastButWrong = False {-- -- Add a } to make the benchmark run for all
  && False --}
bypassUnopt = False{-- -- Add a } to make the benchmark bypass the unopt
  || True --}
--displayCache = True

nToAverageOn = if fastButWrong then 1 else 10

benchmarkCache: Dict String (List ((Float, List Float), (List Int, List Float)), List ((Float, List Float), (List Int, List Float)))
benchmarkCache = Dict.fromList [ ("", ([], []))
 %%% next , ("Linked-Text Editor",([((11364,[2110,1937,2189,2348,2182]),([1,1,1,1,2],[0,0,0,0,1])),((11033,[2217,2153,1949,2135,1992]),([1,1,1,1,2],[1,0,0,0,2])),((9321,[1870,1812,1703,1802,1710]),([1,1,1,1,2],[0,0,0,0,1])),((9655,[1772,1888,1809,1787,1955]),([1,1,1,1,2],[0,0,0,0,2])),((10301,[1978,1901,1969,1990,1994]),([1,1,1,1,2],[0,0,0,0,1])),((11046,[1903,2141,1934,1940,2464]),([1,1,1,1,2],[0,0,0,0,2])),((11906,[2901,2164,2162,2057,1980]),([1,1,1,1,2],[0,0,0,0,1])),((11538,[2257,2359,2173,2130,2101]),([1,1,1,1,2],[0,0,0,0,2])),((11222,[2124,2455,2064,2039,2009]),([1,1,1,1,2],[0,0,0,0,1])),((9233,[1950,1895,1596,1618,1629]),([1,1,1,1,2],[0,0,0,0,1]))],[((51781,[11886,12366,12191,7508,7352]),([1,1,1,1,1],[0,0,0,0,0])),((51154,[11938,12097,11737,6658,8249]),([1,1,1,1,1],[0,0,0,0,0])),((55379,[13175,12868,13305,8098,7438]),([1,1,1,1,1],[0,0,0,0,0])),((58094,[12846,14204,14536,8619,7307]),([1,1,1,1,1],[0,0,0,0,0])),((58495,[13245,13793,13536,8459,8887]),([1,1,1,1,1],[0,0,0,0,0])),((56469,[14108,13544,13365,7141,7804]),([1,1,1,1,1],[0,0,0,0,0])),((54262,[12596,13497,13084,7210,7426]),([1,1,1,1,1],[0,0,0,0,0])),((55861,[13339,13077,13350,7900,7682]),([1,1,1,1,1],[0,0,0,0,0])),((53034,[12810,12918,12196,6991,7630]),([1,1,1,1,1],[0,0,0,0,0])),((53242,[12342,12620,12605,7994,7195]),([1,1,1,1,1],[0,0,0,0,0]))]))
 ]


programs = Dict.fromList [
  ("Table of States A", ExamplesGenerated.tableOfStatesA),
  ("Table of States B", ExamplesGenerated.tableOfStatesB),
  ("Table of States C", ExamplesGenerated.tableOfStatesC),
  --("Markdown Recursive", ExamplesGenerated.fromleo_markdown),
  --("Markdown Linear",    ExamplesGenerated.fromleo_markdown_optimized),
  ("Markdown",    ExamplesGenerated.fromleo_markdown_optimized),
  --("Markdown with lens", ExamplesGenerated.fromleo_markdown_optimized),
  --("Markdown w/o lens", ExamplesGenerated.fromleo_markdown_optimized_lensless),
  ("Scalable Recipe", ExamplesGenerated.fromleo_recipe2),
  ("Budgetting", ExamplesGenerated.fromleo_conference_budgetting),
  ("Linked-Text Editor", ExamplesGenerated.fromleo_linkedtexteditor),
  ("MVC", ExamplesGenerated.fromleo_modelviewcontroller),
  ("Translation Editor", ExamplesGenerated.fromleo_translatabledoc)
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

click_on_plus capital state =  [NoTransform
  , replaceHtmlByReg ("("++capital++".*(\r?\n).*(\r?\n).*)style='color:gray'") (\m -> at m.submatches 0 ++ "style='color:coral'")
  , replaceHtmlByReg ("has-been-clicked='( ?)False'(.*(\r?\n).*(\r?\n).*"++state++")") (\m -> "has-been-clicked='"++at m.submatches 0 ++ "True'" ++ at m.submatches 1)
  ]

transform_table_of_states_b = [NoTransform
  , replaceMultiple "Click on the last + button" <| click_on_plus "Hartford" "Connecticut"
  , replaceMultiple "Click on the last generated + button" <| click_on_plus "\\?, \\?" "\\?"
  , replaceHtmlBy "\\?" "Delaware"
  , replaceHtmlBy "\\?" "Dover"
  , replaceHtmlBy "\\?" "DE"
  , replaceHtmlBy "\\?" "Florida"
  , replaceHtmlBy "\\?, \\?" "Tallahassee, FL"
  ]

at l n = Utils.nth l n |> Utils.fromOk "at" |> Maybe.withDefault ""

benchmarks: List Benchmark
benchmarks = [
  {--
  BUpdate 0 "Translation Editor" [NoTransform
      , replaceHtmlBy "printer" "{printer}"
    ],
  --}
  --{--{--{--{--{--{--{--
  {--}
  BUpdate (0*60+53) "Linked-Text Editor" [NoTransform
    , replaceHtmlBy "P\\(1\\)" "H(1)"
    {--}
    , replaceHtmlBy "H\\(n\\)" "G(m)"
    , replaceMultiple "prove" [NoTransform
       , replaceHtmlBy "we want to prove" "we want to $prove"
       , replaceHtmlBy "need to prove" "need to $prove"
       , replaceHtmlBy "need to prove" "need to $prove"
       , replaceHtmlBy "we can prove" "we can $prove"
    ]
    , replaceHtmlBy "we want to prove" "we want to show"
    , replaceHtmlBy "we want to show" "we want to really show"
    --}
  ],
  --{--{--{--{--{--{--
  {--}
  BUpdate (1*60+11) "MVC" [NoTransform
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
  BUpdate (0*60+45) "Budgetting" [ NoTransform
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
  {--}
  BUpdate (0*60+43) "Table of States B" transform_table_of_states_b,
  --}
  {--
  BUpdate (0*60+43) "Table of States C" transform_table_of_states_b,
  --}
  {--}
  --BUpdate "Markdown Recursive" transform_markdown_ab_linear,
  BUpdate (2*60+8) "Markdown" transform_markdown_ab_linear,
  --BUpdate "Markdown with lens" transform_markdown_ab_lens,
  --BUpdate "Markdown w/o lens" transform_markdown_ab_lens,
  --}
  {--}
  BUpdate (3*60+51) "Scalable Recipe" [ NoTransform
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
  ] |>
  List.filterMap (\c -> case c of
  BUpdate _ "" _ -> Nothing
  _ -> Just c )

next = "next"

(%%%) : a -> b -> a
(%%%) a b = a

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

speedup unopt opt =
  let inside = toString (toFloat (floor (10 * unopt / opt)) / 10) ++ "x" in
  " (" ++ (if inside == "NaNx" then "\\nospeedup" else inside)++ ")"

runBenchmark: Benchmark -> (String, Int, Int, Int, Int, List Float, List Float, (List Int, List Float, List Float))
runBenchmark b = case b of
  BUpdate sessionTime benchmarkname replacements ->
    if benchmarkname == "" then ("", 0, 0, 0, 0, [], [], ([], [], []))
    else
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
    -- Returns (List of the total time of the session,
    -- and as many update times as there are replacements
    -- Returns the number of ambiguities encountered, and a list of all the times taken to compute all remaining solutions
    let session: Int -> Bool -> ((Float, List Float), (List Int, List Float))
        session i unopt =
      let sessionName = "Session #" ++ toString i ++ " " ++
         (if unopt then "unoptimized" else "optimized") ++" for " ++ benchmarkname in
      let _ = if not exportMode then ImpureGoodies.log <| sessionName  else "" in
      let (_, _, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, _) =
           Utils.foldLeft
                (progExp, oldOut, [],          [],        [],         [],          [],              1) finalReplacements <|
              (\(progExp, oldOut, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, nextChoice) step ->
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
             SetNextChoice n -> (progExp, oldOut, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, n)
             TestOutputContains re vToString ->
               if Regex.contains (Regex.regex re) (vToString oldOut) then
                 (progExp, oldOut, updateTimes, evalTimes, modifTimes, ambiguities, timeAmbiguities, nextChoice)
               else
                 Debug.crash <| "The output did not contain '" ++ re ++ "' : " ++ vToString oldOut ++ ", program having generated it:\n " ++ unparse progExp
             ProgTransform p ->
               case parse (p (unparse progExp)) of
                 Err msg -> Debug.crash msg
                 Ok newProgExp ->
                   let (realNewOut, realNewOutTime) = ImpureGoodies.timedRun <| \_ -> evalEnv EvalUpdate.preludeEnv newProgExp |> Utils.fromOk "eval changed prog" in
                   (newProgExp, realNewOut, updateTimes, realNewOutTime::evalTimes, modifTimes, ambiguities, timeAmbiguities, 1)
             OutputTransform replacement ->
               let replacementName = sessionName ++ transformName replacement in
               --let _ = Debug.log ("oldOut\n" ++ valToString oldOut) () in
               --let _ = Debug.log ("oldOut\n" ++ valToHtml oldOut) () in
               let (newOut, newOutTime) = ImpureGoodies.timedRun <| \_ -> applyTransform replacement oldOut in
               --let _ = Debug.log ("newOut\n" ++ valToString newOut) () in
               --let _ = Debug.log ("newOut\n" ++ valToHtml newOut) () in
               let _ = if not exportMode then ImpureGoodies.log <| "It took " ++ toString newOutTime ++ "ms. Update with newOut..."  else "" in
               --let _ = ImpureGoodies.log (valToHtml newOut) in
               let (newProgExp_, newProgExpDiffs, updateTime, numAmbiguities, timeAllOtherSolutions) = (if unopt then
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.update (updateContext "initial" EvalUpdate.preludeEnv progExp oldOut newOut VUnoptimizedDiffs)
                  else
                    ImpureGoodies.timedRun <| \_ ->
                    EvalUpdate.doUpdateWithoutLog progExp oldOut newOut) |> \(x, t) -> case x of
                       Results.Oks (LazyList.Nil) -> Debug.crash <| "No solution for " ++ replacementName
                       Results.Oks (LazyList.Cons (headUEnv, headUExp) _ as ll) ->
                         let (allSolutions, timeAllOtherSolutions) = ImpureGoodies.timedRun <| \_ -> LazyList.toList ll in
                         let numAmbiguities = List.length allSolutions in
                         let nonChangingEnv = LazyList.filter (\(env, e) -> List.all (\(i, x) -> x == VUnoptimizedDiffs) env.changes && e.changes /= Nothing) <| LazyList.fromList allSolutions in
                         case nonChangingEnv of
                              LazyList.Nil -> Debug.crash <| unparse headUExp.val ++ "All solutions of " ++ replacementName ++ " modify the environment or do not modify the expression: " ++ envDiffsToString EvalUpdate.preludeEnv headUEnv.val headUEnv.changes
                              _ ->
                         let (newEnv, newExp) = LazyList.elemAt (nextChoice - 1) nonChangingEnv |> Utils.fromJust_ "LazyList head"
                         in
                         if newExp.changes == Nothing then
                           Debug.crash <| "In " ++ replacementName++ ", expected a change to the expression, got Nothing\n" ++ (transformValToString replacement newOut)
                         else
                          (newExp.val, newExp.changes |> Utils.fromJust,  t, numAmbiguities, timeAllOtherSolutions)
                       Results.Errs msg -> Debug.crash <| msg ++ "Transform =  " ++ replacementName ++ "\n" ++ (transformValToString replacement newOut)  ++ "\n" ++ unparse progExp
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
               (newProgExp, realNewOut, updateTime::updateTimes, realNewOutTime::evalTimes, newOutTime::modifTimes, numAmbiguities::ambiguities, timeAllOtherSolutions::timeAmbiguities, 1)
               )
      in
      ((List.sum updateTimes + List.sum evalTimes + List.sum modifTimes, List.reverse updateTimes), (List.reverse ambiguities, List.reverse timeAmbiguities))
    in
    let (optResults, unoptResults) =
         case Dict.get benchmarkname benchmarkCache of
          Nothing -> (tryMany nToAverageOn <| \i ->
                        session i False,
                      tryMany nToAverageOn <| \i ->
                        session i True)
          Just x -> x
    in
    let allUpdateTimes = List.concatMap (Tuple.second << Tuple.first) in
    let allUnoptTimes = allUpdateTimes unoptResults in
    let allOptTimes = allUpdateTimes optResults in
    -- It's useless to consider ambiguities accross sessions because they are the same. But the times may vary

    let allTimeAmbiguities = List.concatMap (Tuple.second << Tuple.second) in
    let ambiguities = List.head >> Utils.fromJust_ "ambiguity" >> Tuple.second >> Tuple.first in
    let ambiguitiesOpt = ambiguities optResults in
    let timeAmbiguitiesOpt = allTimeAmbiguities optResults in
    --let ambiguitiesUnopt = ambiguities unoptResults in
    let timeAmbiguitiesUnopt = allTimeAmbiguities unoptResults in

    let onlyRelevantAmbiguities ambiguities timeAmbiguities =
      List.filterMap identity (List.map2 (\a t -> if a == 1 then Nothing else Just t) ambiguities timeAmbiguities) in

    let timeAmbiguitiesOptPruned = onlyRelevantAmbiguities ambiguitiesOpt timeAmbiguitiesOpt in
    --let ambiguitiesUnopt = ambiguities unoptResults in
    let timeAmbiguitiesUnoptPruned = onlyRelevantAmbiguities ambiguitiesOpt timeAmbiguitiesUnopt in


    let fastestUnopt = fastest allUnoptTimes in
    let fastestOpt   = fastest allOptTimes   in
    let slowestUnopt = slowest allUnoptTimes in
    let slowestOpt   = slowest allOptTimes   in
    let averageUnopt = average allUnoptTimes in
    let averageOpt   = average allOptTimes   in

    let minAmbiguity = minimum ambiguitiesOpt in
    let maxAmbiguity = maximum ambiguitiesOpt in
    let averageAmbiguity = average <| List.map toFloat ambiguitiesOpt in
    let fastestAmbiguityUnopt = if (maxAmbiguity == 1) then 0 else fastest timeAmbiguitiesUnoptPruned in
    let fastestAmbiguityOpt   = if (maxAmbiguity == 1) then 0 else fastest timeAmbiguitiesOptPruned   in
    let slowestAmbiguityUnopt = if (maxAmbiguity == 1) then 0 else slowest timeAmbiguitiesUnoptPruned in
    let slowestAmbiguityOpt   = if (maxAmbiguity == 1) then 0 else slowest timeAmbiguitiesOptPruned   in
    let averageAmbiguityUnopt = if (maxAmbiguity == 1) then 0 else average timeAmbiguitiesUnoptPruned in
    let averageAmbiguityOpt   = if (maxAmbiguity == 1) then 0 else average timeAmbiguitiesOptPruned   in

    --let averageUnoptSessionTime = msToMinutsSeconds <| average <| List.map (Tuple.first << Tuple.first) unoptResults in
    --let averageOptSessionTime   = msToMinutsSeconds <| average <| List.map (Tuple.first << Tuple.first) optResults in

    let locprog = loc prog in
    let finalEvalTime = ceiling evalTime in
    let displazyunoptoptspeedup timeUnopt timeOpt =
      if timeUnopt < 5 && timeOpt < 5 then
         String.pad 15 ' ' ("\\lessthanms{5}")
      else
         String.pad 15 ' ' (s timeUnopt ++ "/" ++ s timeOpt ++ speedup timeUnopt timeOpt)
    in
    let latexRow = "\\tableRow   {" ++
    String.padRight 20 ' ' benchmarkname ++ "} {" ++
    String.pad 3 ' ' (toString <| locprog) ++ "} {" ++
    String.pad 4 ' ' (toString <| finalEvalTime) ++ "} { "++
    String.pad 6 ' ' (sToMinutsSeconds (toFloat sessionTime)) ++"  } { "++
    String.pad 3 ' ' (toString numberOfUpdates) ++" } {" ++
    displazyunoptoptspeedup fastestUnopt fastestOpt ++ " & " ++
    displazyunoptoptspeedup slowestUnopt slowestOpt ++ " & " ++
    displazyunoptoptspeedup averageUnopt averageOpt ++ "} " ++
    "{  " ++ (if (minAmbiguity == maxAmbiguity)
         then "\\always{} " ++ toString minAmbiguity ++ " & " ++ toString minAmbiguity  ++ "        "
         else " " ++ toString minAmbiguity ++ " to " ++ toString maxAmbiguity ++ "      & " ++ String.pad 9 ' ' ((String.left 4 (toString averageAmbiguity)))) ++ "} " ++
    "{" ++ (if(minAmbiguity == maxAmbiguity) then
         String.pad 15 ' ' "\\noambiguity" ++ " & " ++ String.pad 15 ' ' "\\noambiguity" ++ " & " ++String.pad 15 ' ' "\\noambiguity"
      else
      displazyunoptoptspeedup fastestAmbiguityUnopt fastestAmbiguityOpt ++ " & " ++
      displazyunoptoptspeedup slowestAmbiguityUnopt slowestAmbiguityOpt ++ " & " ++
      displazyunoptoptspeedup averageAmbiguityUnopt averageAmbiguityOpt
     )++ "} "
    in
    let _ = ImpureGoodies.log latexRow in
    let rendersession results = List.map (\((session, upds), (ambiguities, timeAmbiguities)) ->
       "\n% session: " ++ toString session ++ ", updates: " ++
         (List.map toString upds |> String.join ",") ++ ",\n%solutions: " ++
         (List.map toString ambiguities |> String.join ",") ++ " / times: " ++
         (List.map toString timeAmbiguities |> String.join ",")
         ) results |> String.join "" in
    let rawdata = "\n% " ++ benchmarkname ++ " - Unopt" ++ rendersession unoptResults ++
      "\n% " ++ benchmarkname ++ " - Opt" ++ rendersession optResults in
    let result = (rawdata, locprog, finalEvalTime, sessionTime, numberOfUpdates, allUnoptTimes, allOptTimes,
      (ambiguitiesOpt, timeAmbiguitiesUnoptPruned, timeAmbiguitiesOptPruned)) in
    let _ = if fastButWrong then "" else ImpureGoodies.log (" %%% next , " ++ toString (benchmarkname, (optResults, unoptResults))) in
    result

header =
  ImpureGoodies.log """
\\newcommand{\\benchmarks}{
%
% Benchmark Rows
%
% \\tableRow {                    } {   } {    } {     Session     } {  Fastest Upd  } {  Slowest Upd  } {  Average Upd  } { #Amb     } {  Fastest Amb. } {  Slowest Amb. } {  Average Amb. }
% \\tableRow {     Example        } {LOC} {Eval} {  Time  } {\\#Upd } {  Unopt / Opt  } {  Unopt / Opt  } {  Unopt / Opt  } { Min/Max/Average } {  Unopt / Opt  } {  Unopt / Opt  } {  Unopt / Opt  }"""

compute = (Utils.foldLeft
    ("",  0,   0,    0,    0,   [],       [],     ([],          [],       []))     benchmarks <|
  (\(acc, loc, eval, time, upd, updUnopt, updOpt, (ambiguities, ambUnopt, ambOpt)) b ->
     let (acc2, loc2, eval2, time2, upd2, updUnopt2, updOpt2, (ambiguities2, ambUnopt2, ambOpt2)) = runBenchmark b in
     (acc ++ acc2, loc + loc2, eval + eval2, time + time2, upd + upd2,
      updUnopt ++ updUnopt2, updOpt ++ updOpt2, (ambiguities ++ ambiguities2, ambUnopt ++ ambUnopt2, ambOpt ++ ambOpt2) ))
  ) |>
       (\(acc, loc, eval, time, upd, updUnopt, updOpt,(ambiguities, unoptAmb, optAmb)) ->
         let _ = ImpureGoodies.log acc in
         let unoptaverage = average updUnopt in
         let optaverage = average updOpt in
         let unoptaverageamb = average unoptAmb in
         let optaverageamb = average optAmb in
         ImpureGoodies.log <| """}
\\newcommand{\\benchmarksloc}{""" ++ toString loc ++ """}
\\newcommand{\\benchmarkslocfloored}{""" ++ toString (floor (toFloat loc / toFloat 100) * 100) ++ """}
\\newcommand{\\benchmarksnum}{""" ++ toString (List.length benchmarks) ++ """}
\\newcommand{\\benchmarkseval}{""" ++ toString eval ++ """}
\\newcommand{\\benchmarkssessiontime}{""" ++ sToMinutsSeconds (toFloat time) ++ """}
\\newcommand{\\benchmarksnumupd}{""" ++ toString upd ++ """}
\\newcommand{\\benchmarksaverageunoptupd}{""" ++ s unoptaverage ++ """}
\\newcommand{\\benchmarksaverageoptupd}{""" ++ s optaverage ++ """}
\\newcommand{\\benchmarksaveragespeedup}{""" ++ speedup unoptaverage optaverage ++ """}
\\newcommand{\\benchmarksamb}{\\total{"""++ toString (minimum ambiguities) ++ " to " ++ toString (maximum ambiguities) ++ "} & \total{"++(String.left 4 (toString <| average (List.map toFloat ambiguities)))++"""} }
\\newcommand{\\benchmarksaverageunoptupdamb}{"""++s unoptaverageamb ++"""}
\\newcommand{\\benchmarksaverageoptupdamb}{"""++s optaverageamb ++"""}
\\newcommand{\\benchmarksaveragespeedupamb}{"""++speedup unoptaverageamb optaverageamb++"""}
""")




parse = Syntax.parser Syntax.Elm >> Result.mapError (\p -> ParserUtils.showError p)
unparse = Syntax.unparser Syntax.Elm
evalEnv env exp = Eval.doEval Syntax.Elm env exp |> Result.map (Tuple.first >> Tuple.first)
eval exp = Eval.doEval Syntax.Elm [] exp |> Result.map (Tuple.first >> Tuple.first)

average: List Float -> Float
average l = List.sum l / toFloat (List.length l)

minimum: List comparable -> comparable
minimum l = List.minimum l |> Utils.fromJust

fastest = minimum

maximum: List comparable -> comparable
maximum l = List.maximum l |> Utils.fromJust

slowest = maximum

s: Float -> String
s f = toString (ceiling f)

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