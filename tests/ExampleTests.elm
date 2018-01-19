module ExampleTests exposing (..)

import String

import ExamplesGenerated as Examples

import Syntax

import FastParser exposing (parseE)
import Eval
import LangUnparser

testEvalsWithoutErrors name code =
  case parseE code of
    Err s ->
      "can't parse " ++ name ++ ": " ++ toString s ++ "\n" ++ code
    Ok exp ->
      -- Elm will crash first if Eval fails.
      case Eval.run Syntax.Little exp of
        Ok (val, widgets) -> "ok"
        Err s             -> "evaluation error running " ++ name ++ ":\n" ++ s

testReparsedUnparsedEvalsWithoutErrors name code =
  case parseE code of
    Err s ->
      "can't parse " ++ name ++ ": " ++ toString s ++ "\n" ++ code
    Ok parsed ->
      let unparsed = LangUnparser.unparse parsed in
      case parseE unparsed of
        Err s ->
          "can't re-parse unparsed " ++ name ++ ": " ++ toString s ++ "\n" ++ unparsed
        Ok reparsedExp ->
          -- Elm will crash first if Eval fails.
          case Eval.run Syntax.Little reparsedExp of
            Ok (val, widgets) -> "ok"
            Err s             -> "evaluation error running unparsed/reparsed " ++ name ++ ":\n" ++ s

testExample name code =
  [ (\() -> testEvalsWithoutErrors name code)
  , (\() -> testReparsedUnparsedEvalsWithoutErrors name code)
  ]

exampleTests : () -> List (() -> List (() -> String))
exampleTests () =
  List.map
    (\(name, (code, thunk)) -> \() -> testExample name code)
    Examples.list
