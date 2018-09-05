module Syntax exposing
  ( Syntax(..)
  , parser
  , unparser
  , patternUnparser
  , typeUnparser
  , convertSyntax
  , sourceExtension
  , iconExtension
  , fromFileExtension
  , encode
  )

import Result
import Parser
import Json.Encode

import FastParser
import ElmParser
import LangUnparser
import ElmUnparser
import Lang
import File

type Syntax
  = Little
  | Elm

parser : Syntax -> String -> Result Parser.Error Lang.Exp
parser syntax =
  case syntax of
    Little ->
      FastParser.parseE

    Elm ->
      ElmParser.parse

unparser : Syntax -> Lang.Exp -> String
unparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparse

    Elm ->
      ElmUnparser.unparse

patternUnparser : Syntax -> Lang.Pat -> String
patternUnparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparsePat

    Elm ->
      ElmUnparser.unparsePattern

typeUnparser : Syntax -> Lang.Type -> String
typeUnparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparseType

    Elm ->
      ElmUnparser.unparseType

convertSyntax : Syntax -> Syntax -> String -> Result Parser.Error String
convertSyntax oldSyntax newSyntax code =
  code
    |> parser oldSyntax
    |> Result.map (unparser newSyntax)

sourceExtension : Syntax -> File.FileExtension
sourceExtension syntax =
  case syntax of
    Elm ->
      File.ElmFile

    Little ->
      File.LittleFile

iconExtension : Syntax -> File.FileExtension
iconExtension syntax =
  case syntax of
    Elm ->
      File.ElmIcon

    Little ->
      File.LittleIcon

fromFileExtension : File.FileExtension -> Syntax
fromFileExtension extension =
  case extension of
    File.ElmFile ->
      Elm

    File.ElmIcon ->
      Elm

    File.LittleFile ->
      Little

    File.LittleIcon ->
      Little

encode : Syntax -> Json.Encode.Value
encode syntax =
  Json.Encode.string <|
    case syntax of
      Elm ->
        "Elm"

      Little ->
        "Little"
