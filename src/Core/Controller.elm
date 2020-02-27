module Core.Controller exposing
  ( msgRequestRun
  , msgRequestSynthesis
  , msgLoadExample
  , msgRequestBenchmark
  , msgRequestRandomSample
  )

import MyRandom as Random
import Dict exposing (Dict)
import Task exposing (Task)

import Core.Lang as C
import Core.Bridge as B
import Core.Compile
import Core.Uncompile
import Core.Reference as Reference

import Lang as L
import UnLang as U
import Types2 as T
import Utils
import Syntax
import PBESuite
import TopOnePBESuite
import BaseCasePBESuite

import Model exposing (Model, Msg(..))

import ImpureGoodies

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

timeDecimals : Int
timeDecimals =
  3

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

handleError :
  Model
    -> Result String a
    -> (a -> Result String (Model, Cmd Msg))
    -> (Model, Cmd Msg)
handleError model computation callback =
  let
    modelError : String -> (Model, Cmd Msg)
    modelError e =
      ({ model | unExpOutput = Err e }, Cmd.none)
  in
    computation
      |> Result.mapError modelError
      |> Result.andThen (callback >> Result.mapError modelError)
      |> Utils.fromResult

handleErrorPure :
  Model
    -> Result String a
    -> (a -> (Model, Cmd Msg))
    -> (Model, Cmd Msg)
handleErrorPure model computation callback =
  handleError model computation (callback >> Ok)

showAsyncError : B.Error -> String
showAsyncError e =
  case e of
    B.HttpError httpError ->
      "Http error: " ++ toString httpError

    B.ServerError serverError ->
      "Server error: " ++ serverError

--------------------------------------------------------------------------------
-- Running
--------------------------------------------------------------------------------

run :
  String
    -> Result
         String
         (L.Exp, B.Async (Result String (C.Res, C.ResumptionAssertions)))
run code =
  case Syntax.parser Syntax.Leo code of
    Ok lexp ->
      case Core.Compile.exp lexp of
        Ok cexp ->
          Ok (lexp, B.eval cexp)

        Err e ->
          Err <|
            "Compilation error: " ++ toString e

    Err e ->
      Err <|
        "Parse error: " ++ toString e

msgRequestRun : Msg
msgRequestRun =
  NewModelAndCmd "Request Core Run" <| \model ->
    handleErrorPure model (run model.code) <| \(untypedExp, task) ->
      let
        (typedExp, holeEnv) =
          T.typecheck untypedExp

        aceTypeInfo =
          T.aceTypeInfo typedExp
      in
        ( { model
              | inputExp =
                  typedExp
              , holeEnv =
                  holeEnv
              , codeBoxInfo =
                  Model.updateCodeBoxInfo aceTypeInfo model
              , pbeSynthesisResult =
                  Nothing
          }
        , Task.attempt msgReceiveRun task
        )

msgReceiveRun :
  Result B.Error (Result String (C.Res, C.ResumptionAssertions)) -> Msg
msgReceiveRun response =
  NewModelAndCmd "Receive Core Run" <| \model ->
    handleError model (Result.mapError showAsyncError response) <|
      \evalResponse ->
        case evalResponse of
          Ok (res, assertions) ->
            Ok
              ( { model | unExpOutput =
                    Ok (Core.Uncompile.res res, assertions)
                }
              , Cmd.none
              )

          Err e ->
            Err <|
              "Evaluation error: " ++ e

--------------------------------------------------------------------------------
-- Synthesizing
--------------------------------------------------------------------------------

synthesize :
  L.Exp
    -> C.ResumptionAssertions
    -> Result String (B.Async (List C.HoleFilling, Float, Bool))
synthesize exp assertions =
  let
    (datatypeEnv, holeEnv) =
      exp
        |> T.typecheck
        |> Tuple.mapFirst T.getDataTypeDefs
  in
    case Core.Compile.holeContext holeEnv of
      Ok holeContext ->
        case Core.Compile.datatypeContext datatypeEnv of
          Ok datatypeContext ->
            Ok <|
              B.synthesize (holeContext, datatypeContext, assertions)

          Err e ->
            Err <|
              "Datatype context compilation error: " ++ toString e

      Err e ->
        Err <|
          "Hole context compilation error: " ++ toString e

msgRequestSynthesis : Int -> Msg
msgRequestSynthesis cutoff =
  NewModelAndCmd "Request Core Synthesis" <| \model ->
    case model.unExpOutput of
      Ok (_, assertions) ->
        handleErrorPure model (synthesize model.inputExp assertions) <| \task ->
          ( { model | codeAtPbeSynthesis = Just model.code }
          , Task.attempt (msgReceiveSynthesis cutoff) task
          )

      _ ->
        (model, Cmd.none)

msgReceiveSynthesis :
  Int -> Result B.Error (List C.HoleFilling, Float, Bool) -> Msg
msgReceiveSynthesis cutoff response =
  NewModelAndCmd "Receive Core Synthesis" <| \model ->
    handleErrorPure model (Result.mapError showAsyncError response) <|
      \(holeFillings, timeTaken, timedOut) ->
        ( { model | pbeSynthesisResult =
              Just
                { holeFillings =
                    holeFillings
                      |> List.map Core.Uncompile.holeFilling
                      |> List.take cutoff
                , timeTaken =
                    timeTaken
                , timedOut =
                    timedOut
                }
          }
        , Cmd.none
        )

--------------------------------------------------------------------------------
-- Loading Examples
--------------------------------------------------------------------------------

msgLoadExample : String -> String -> Msg
msgLoadExample name programText =
  NewModelAndCmd "Load PBE Suite Example" <| \model ->
    ( Model.closeDialogBox Model.PBESuiteList <|
        { model
            | code = programText
            , unExpOutput = Err ""
        }
    , Cmd.none
    )

--------------------------------------------------------------------------------
-- Benchmarking
--------------------------------------------------------------------------------

runAndSynthesize :
  String -> Task String (L.Exp, List C.HoleFilling, Float, Bool)
runAndSynthesize code =
  case run code of
    Ok (lexp, runTask) ->
      flip Task.andThen (Task.mapError showAsyncError runTask) <|
        \evalResponse ->
          case evalResponse of
            Ok (_, assertions) ->
              case synthesize lexp assertions of
                Ok synthesisTask ->
                  synthesisTask
                    |> Task.mapError showAsyncError
                    |> Task.map (\(hf, t, b) -> (lexp, hf, t, b))

                Err e ->
                  Task.fail e

            Err e ->
              Task.fail <|
                "Evaluation error: " ++ e

    Err e ->
      Task.fail e

tryHoleFilling : L.Exp -> U.HoleFilling -> Task String Bool
tryHoleFilling root holeFilling =
  case
    root
      |> L.fillHoles (Dict.toList holeFilling)
      |> Core.Compile.exp
  of
    Ok cexp ->
      cexp
        |> B.eval
        |> Task.mapError showAsyncError
        |> Task.map (Result.map (Tuple.second >> List.isEmpty))
        |> Task.map (Result.withDefault False)

    Err e ->
      Task.fail <|
        "Compilation error: " ++ toString e

type alias Benchmark =
  { name : String
  , full :
      { window : U.Window
      , timeTaken : Float
      , exampleCount : Int
      }
  , restricted :
      Maybe
        { window : U.Window
        , timeTaken : Float
        , exampleCount : Int
        , validTopRecursive : Int
        , validTopNonRecursive : Int
        , validOthers : Int
        , topIsValid : Bool
        }
  }

average : List Benchmark -> Maybe Benchmark
average benchmarks =
  case benchmarks of
    [] ->
      Nothing

    head :: tail ->
      if not <| List.all (.name >> (==) head.name) tail then
        Nothing
      else
        let
          headFull =
            head.full

          extractFull full =
            if
              full.exampleCount == headFull.exampleCount
                && full.window == headFull.window
            then
              Just full.timeTaken
            else
              Nothing

          maybeFull =
            benchmarks
              |> List.map (.full >> extractFull)
              |> Utils.projJusts
              |> Maybe.map Utils.avg
              |> Maybe.map
                   ( \averageTime ->
                       { headFull | timeTaken = averageTime }
                   )
        in
          case maybeFull of
            Nothing ->
              Nothing

            Just full ->
              case head.restricted of
                Nothing ->
                  Just
                    { name = head.name
                    , full = full
                    , restricted = Nothing
                    }

                Just headR ->
                  let
                    extractRestricted maybeR =
                      case maybeR of
                        Nothing ->
                          Nothing

                        Just r ->
                          if
                            r.exampleCount == headR.exampleCount
                              && r.validTopRecursive
                                   == headR.validTopRecursive
                              && r.validTopNonRecursive
                                   == headR.validTopNonRecursive
                              && r.validOthers
                                   == headR.validOthers
                              && r.window
                                   == headR.window
                          then
                            Just r.timeTaken
                          else
                            Nothing

                    maybeRestricted =
                      benchmarks
                        |> List.map (.restricted >> extractRestricted)
                        |> Utils.projJusts
                        |> Maybe.map Utils.avg
                        |> Maybe.map
                             ( \averageTime ->
                                 { headR | timeTaken = averageTime }
                             )
                  in
                    case maybeRestricted of
                      Nothing ->
                        Nothing

                      Just restricted ->
                        Just
                          { name = head.name
                          , full = full
                          , restricted = Just restricted
                          }

benchmark :
  { name : String
  , definitions : String
  , fullExamples : { code : String, count : Int }
  , restrictedExamples : Maybe { code : String, count : Int }
  } -> Task String Benchmark
benchmark { name, definitions, fullExamples, restrictedExamples } =
  flip Task.andThen
    ( runAndSynthesize (definitions ++ fullExamples.code)
    ) <| \(referenceExp, fullHoleFillings, fullTimeTaken, fullTimedOut) ->
  let
    () = Debug.log ("Benchmarking example '" ++ name ++ "'") ()

    window : List C.HoleFilling -> U.Window
    window =
      Utils.dedup >> List.map Core.Uncompile.holeFilling >> U.window

    validCount : List U.HoleFilling -> Task String Int
    validCount =
      List.map (tryHoleFilling referenceExp)
        >> Task.sequence
        >> Task.map (Utils.count identity)

    restrictedTask =
      case restrictedExamples of
        Just { code, count } ->
          flip Task.andThen
            ( runAndSynthesize (definitions ++ code)
                |> Task.map (\(a, b, c, d) -> (b, c, d))
                |> Task.onError
                     ( \e ->
                         Debug.log e <|
                           Task.succeed ([], 0.0, True)
                     )
            ) <| \( restrictedHoleFillings
                  , restrictedTimeTaken
                  , restrictedTimedOut
                  ) ->
          let
            restrictedWindow =
              window restrictedHoleFillings

            makeRestricted
             validTopRecursive validTopNonRecursive validOthers topValid =
              Just
                { window = restrictedWindow
                , timeTaken = restrictedTimeTaken
                , exampleCount = count
                , validTopRecursive = validTopRecursive
                , validTopNonRecursive = validTopNonRecursive
                , validOthers = validOthers
                , topIsValid = topValid == 1
                }
          in
            Task.map4 makeRestricted
              (validCount restrictedWindow.topRecursive)
              (validCount restrictedWindow.topNonRecursive)
              (validCount restrictedWindow.others)
              ( validCount <|
                  Utils.maybeToList <|
                    U.topSolution restrictedWindow
              )

        Nothing ->
          Task.succeed Nothing

    makeBenchmark restricted =
      { name =
          name
      , full =
          { window = window fullHoleFillings
          , timeTaken = fullTimeTaken
          , exampleCount = fullExamples.count
          }
      , restricted =
          restricted
      }
  in
    Task.map makeBenchmark restrictedTask

showBenchmarks : Int -> List Benchmark -> String
showBenchmarks replications benchmarks =
  let
    (leftExperimentName, rightExperimentName) =
      if List.length benchmarks == Dict.size TopOnePBESuite.suite then
        ("One", "Two")
      else if List.length benchmarks == Dict.size BaseCasePBESuite.suite then
        -- This used to be ("ThreeFull", "ThreeFewer")
        ("", "Three")
      else
        ("Unknown", "Unknown")

    showFull data =
      "\\benchmarkExperiment"
        ++ leftExperimentName
        ++ "{"
        ++ toString data.exampleCount
        ++ "}{"
        ++ toString (U.solutionCount data.window)
        ++ "}{"
        ++ Utils.formatFloat timeDecimals data.timeTaken
        ++ "}"

    showRestricted maybeData fullExampleCount =
      case maybeData of
        Just data ->
          let
            totalValid =
              [ .validTopRecursive, .validTopNonRecursive, .validOthers ]
                |> List.map ((|>) data)
                |> List.sum

            smallestSolutionSuccess =
              U.topSolution data.window
          in
            "\\benchmarkExperiment"
              ++ rightExperimentName
              ++ "{"
              ++ toString data.exampleCount
              ++ " ("
              ++ toString
                   ( round <| 100 *
                       toFloat data.exampleCount / toFloat fullExampleCount
                   )
              ++ "\\%)}{"
              ++ toString totalValid
              ++ "}{"
              ++ toString (U.solutionCount data.window)
              ++ "}{"
              ++ Utils.formatFloat timeDecimals data.timeTaken
              ++ "}{"
              ++ toString data.validTopRecursive
              ++ "}{"
              ++ toString (List.length data.window.topRecursive)
              ++ "}{"
              ++ toString data.validTopNonRecursive
              ++ "}{"
              ++ toString (List.length data.window.topNonRecursive)
              ++ "}{"
              ++ ( if data.validTopRecursive > 0 then
                     "R"
                   else
                     "N"
                 )
              ++ "}"
              ++ " % Top 1: "
              ++ toString data.topIsValid

        Nothing ->
          "\\benchmarkExperimentTwo{---}{---}{---}{---}{---}{---}{---}{---}{---}{---}"

    showBenchmark : Benchmark -> String
    showBenchmark b =
      "\\benchmarkName{"
        ++ (Utils.escapeUnderscores b.name)
        ++ "}<br />"
        ++ ( if leftExperimentName /= "" then
               showFull b.full
                 ++ "<br />"
             else
               ""
           )
        ++ showRestricted b.restricted b.full.exampleCount
        ++ "<br />"

    averageSizePercent : Float
    averageSizePercent =
      let
        sizePercent b =
          case b.restricted of
            Nothing ->
              1

            Just data ->
              toFloat data.exampleCount / toFloat b.full.exampleCount
      in
        benchmarks
          |> List.map sizePercent
          |> Utils.average
          |> Maybe.withDefault 1
  in
    "% Replications: n = "
      ++ toString replications
      ++ "<br /><br />"
      ++ "% Average size percent: "
      ++ toString (round <| 100 * averageSizePercent)
      ++ "%<br /><br />"
      ++ ( benchmarks
             |> List.map showBenchmark
             |> String.join "\\\\<br />"
             |> Utils.stringMultiReplace
                  [ ("\\benchmarkName{bool\\_", "\\benchmarkNameBool{")
                  , ("\\benchmarkName{list\\_", "\\benchmarkNameList{")
                  , ("\\benchmarkName{nat\\_" , "\\benchmarkNameNat{")
                  , ("\\benchmarkName{tree\\_", "\\benchmarkNameTree{")
                  ]
         )

msgRequestBenchmark
  : Int -> (Dict String (String, String, Int, String, Int)) -> Msg
msgRequestBenchmark replications suite =
  NewModelAndCmd "Request PBE Benchmark" <| \model ->
    ( model
    , suite
        |> Dict.map
             ( \name
               ( definitions
               , fullExamplesCode
               , fullExamplesCount
               , restrictedExamplesCode
               , restrictedExamplesCount
               ) ->
                 let
                   example =
                     { name =
                         name
                     , definitions =
                         definitions
                     , fullExamples =
                         { code = fullExamplesCode
                         , count = fullExamplesCount
                         }
                     , restrictedExamples =
                         if restrictedExamplesCount == -1 then
                           Nothing
                         else
                           Just
                             { code = restrictedExamplesCode
                             , count = restrictedExamplesCount
                             }
                     }
                 in
                   example
                     |> benchmark
                     |> List.repeat replications
                     |> Task.sequence
                     |> Task.andThen
                          ( average
                              >> Maybe.map Task.succeed
                              >> Maybe.withDefault
                                   ( Task.fail <|
                                       "Non-replicable benchmark: '"
                                         ++ name
                                         ++ "'"
                                   )
                          )
             )
        |> Dict.values
        |> Task.sequence
        |> Task.attempt (msgReceiveBenchmark replications)
    )

msgReceiveBenchmark : Int -> Result String (List Benchmark) -> Msg
msgReceiveBenchmark replications response =
  NewModelAndCmd "Receive PBE Benchmark" <| \model ->
    let
      output =
        case response of
          Ok benchmarks ->
            showBenchmarks replications benchmarks

          Err e ->
            "Could not generate benchmark table. " ++ e
    in
    let _ =
      ImpureGoodies.newPage output
    in
      (model, Cmd.none)

--------------------------------------------------------------------------------
-- Random Sampling and Trials
--------------------------------------------------------------------------------

showAllSamplingExperiments : List (List (List Benchmark)) -> String
showAllSamplingExperiments =
  let
    extract :
      Benchmark
        -> Maybe
             { name : String
             , success : Bool
             , fullExampleCount : Int
             , randomExampleCount : Int
             }
    extract { name, full, restricted } =
      Maybe.map
        ( \r ->
             { name = name
             , success = r.topIsValid
             , fullExampleCount = full.exampleCount
             , randomExampleCount = r.exampleCount
             }
        )
        restricted

    results : List Benchmark -> Result String ((String, Int), (Int, Float))
    results bs =
      case Utils.projJusts (List.map extract bs) of
        Nothing ->
          Err "Random sampling not found."

        Just foundResults ->
          case foundResults of
            [] ->
              Err "k = 0"

            head :: tail ->
              if
                not <|
                  List.all
                    ( \{name, fullExampleCount, randomExampleCount} ->
                        name == head.name
                          && fullExampleCount == head.fullExampleCount
                          && randomExampleCount == head.randomExampleCount
                    )
                    tail
              then
                Err "Non-replicable random sampling."
              else
                let
                  name =
                    head.name

                  n =
                    List.length foundResults

                  randomExampleCount =
                    head.randomExampleCount

                  numerator =
                    toFloat <|
                      Utils.count .success foundResults

                  denominator =
                    toFloat n
                in
                  Ok
                    ( (name, n)
                    , (randomExampleCount, numerator / denominator)
                    )

    showExperiment : List (List Benchmark) -> String
    showExperiment experiment =
      case Utils.projOk (List.map results experiment) of
        Err e ->
          e

        Ok experimentResults ->
          case Utils.collapseEqual (List.map Tuple.first experimentResults) of
            Nothing ->
              "Inconsistent name and/or N"

            Just (name, n) ->
              experimentResults
                |> List.map Tuple.second
                |> List.map
                     ( \(k, p) ->
                         toString k
                           ++ ", "
                           ++ Utils.formatFloat 2 p
                     )
                |> String.join "<br>"
                |> (++) "<b>k, p</b><br>"
                |> (++) ("N = " ++ toString n ++ "<br>")
                |> (++) (name ++ "<br>")
  in
    List.map showExperiment >> String.join "<hr>"

msgRequestRandomSample :
  Random.Generator (List (List (List Reference.BenchmarkInput))) -> Msg
msgRequestRandomSample benchmarkInputs =
  NewModelAndCmd "Request Random Sample" <| \model ->
    ( model
    , Random.generate msgReceiveRandomSample benchmarkInputs
    )

msgReceiveRandomSample :
  List (List (List Reference.BenchmarkInput)) -> Msg
msgReceiveRandomSample bis =
  NewModelAndCmd "Receive Random Sample" <| \model ->
    ( model
    , bis
        |> List.map
             (List.map (List.map benchmark >> Task.sequence) >> Task.sequence)
        |> Task.sequence
        |> Task.attempt msgReceiveRandomSampleResults
    )

msgReceiveRandomSampleResults :
  Result String (List (List (List Benchmark))) -> Msg
msgReceiveRandomSampleResults response =
  NewModelAndCmd "Receive Random Sample Results" <| \model ->
    let
      output =
        case response of
          Ok allExperiments ->
            showAllSamplingExperiments allExperiments

          Err e ->
            "Could not generate trial table. " ++ e
    in
    let _ =
      ImpureGoodies.newPage output
    in
      (model, Cmd.none)
