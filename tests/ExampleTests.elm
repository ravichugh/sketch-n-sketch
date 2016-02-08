module ExampleTests where

import String

import ExamplesGenerated as Examples

import LangParser2 exposing (parseE)
import Eval
import LangUnparser

testEvalsWithoutErrors name code =
  case parseE code of
    Err s ->
      "can't parse " ++ name ++ ": " ++ s ++ "\n" ++ code
    Ok exp ->
      -- Elm will crash first if Eval fails.
      case Eval.run exp of
        (val, widgets) -> "ok"

testReparsedUnparsedEvalsWithoutErrors name code =
  case parseE code of
    Err s ->
      "can't parse " ++ name ++ ": " ++ s ++ "\n" ++ code
    Ok parsed ->
      let unparsed = LangUnparser.unparse parsed in
      case parseE unparsed of
        Err s ->
          "can't re-parse unparsed " ++ name ++ ": " ++ s ++ "\n" ++ unparsed
        Ok reparsedExp ->
          -- Elm will crash first if Eval fails.
          case Eval.run reparsedExp of
            (val, widgets) -> "ok"

testExample name code =
  [ (\() -> testEvalsWithoutErrors name code)
  , (\() -> testReparsedUnparsedEvalsWithoutErrors name code)
  ]

exampleTests : () -> List (() -> List (() -> String))
exampleTests () =
  List.map
    (\(name, code, thunk) -> \() -> testExample name code)
    Examples.list
