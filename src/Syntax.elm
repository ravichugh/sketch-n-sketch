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
import LeoParser
import LangUnparser
import LeoUnparser
import Lang
import File

type Syntax
  = Little
  | Leo

parser : Syntax -> String -> Result Parser.Error Lang.Exp
parser syntax =
  case syntax of
    Little ->
      FastParser.parseE

    Leo ->
      LeoParser.parse

unparser : Syntax -> Lang.Exp -> String
unparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparse

    Leo ->
      LeoUnparser.unparse

patternUnparser : Syntax -> Lang.Pat -> String
patternUnparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparsePat

    Leo ->
      LeoUnparser.unparsePattern

typeUnparser : Syntax -> Lang.Type -> String
typeUnparser syntax =
  case syntax of
    Little ->
      LangUnparser.unparseType

    Leo ->
      LeoUnparser.unparseType

convertSyntax : Syntax -> Syntax -> String -> Result Parser.Error String
convertSyntax oldSyntax newSyntax code =
  code
    |> parser oldSyntax
    |> Result.map (unparser newSyntax)

sourceExtension : Syntax -> File.FileExtension
sourceExtension syntax =
  case syntax of
    Leo ->
      File.LeoFile

    Little ->
      File.LittleFile

iconExtension : Syntax -> File.FileExtension
iconExtension syntax =
  case syntax of
    Leo ->
      File.LeoIcon

    Little ->
      File.LittleIcon

fromFileExtension : File.FileExtension -> Syntax
fromFileExtension extension =
  case extension of
    File.LeoFile ->
      Leo

    File.LeoIcon ->
      Leo

    File.LittleFile ->
      Little

    File.LittleIcon ->
      Little

encode : Syntax -> Json.Encode.Value
encode syntax =
  Json.Encode.string <|
    case syntax of
      Leo ->
        "Leo"

      Little ->
        "Little"
