module ExampleTests where

import String

import Eval
import ExamplesGenerated as Examples
import LangUnparser
import LangParser2 exposing (parseE)
import OurParser2 exposing (formatError)

testEvalsWithoutErrors name code =
  case parseE code of
    Err s ->
      "can't parse " ++ name ++ ": " ++ formatError s ++ "\n"
    Ok exp ->
      -- Elm will crash first if Eval fails.
      case Eval.run exp of
        Ok (val, widgets) -> "ok"
        Err s             -> "evaluation error running " ++ name ++ ":\n" ++ s

testReparsedUnparsedEvalsWithoutErrors name code =
  case parseE code of
    Err s ->
      "can't parse " ++ name ++ ": " ++ formatError s ++ "\n"
    Ok parsed ->
      let unparsed = LangUnparser.unparse parsed in
      case parseE unparsed of
        Err s ->
          "can't re-parse unparsed " ++ name ++ ": " ++ formatError s ++ "\n" ++ unparsed
        Ok reparsedExp ->
          -- Elm will crash first if Eval fails.
          case Eval.run reparsedExp of
            Ok (val, widgets) -> "ok"
            Err s             -> "evaluation error running unparsed/reparsed " ++ name ++ ":\n" ++ s

testExample name code =
  [ (\() -> testEvalsWithoutErrors name code)
  , (\() -> testReparsedUnparsedEvalsWithoutErrors name code)
  ]

exampleTests : () -> List (() -> List (() -> String))
exampleTests () =
  List.map
    (\(name, code, thunk) -> \() -> testExample name code)
    Examples.list
