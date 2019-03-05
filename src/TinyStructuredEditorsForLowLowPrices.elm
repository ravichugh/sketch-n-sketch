module TinyStructuredEditorsForLowLowPrices exposing (..)

import Set exposing (Set)

import Html exposing (Html)
import Html.Attributes as Attr
-- import Html.Events exposing
--   ( onClick, onInput, onMouseEnter, onMouseLeave
--   , onWithOptions, defaultOptions
--   )
import VirtualDom

import Lang
-- import LangSvg exposing (..)
import Model exposing (Msg)
import Types2

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)
import TinyStructuredEditorsForLowLowPricesEval


-- type alias DataConDef = (Ident, List Type)
-- type alias DataTypeDef = (Ident, List DataConDef)
-- getDataTypeDefs : Exp -> List DataTypeDef

valToSpecificActions : Lang.Exp -> Lang.Val -> List SpecificAction
valToSpecificActions exp val =
  let
    dataTypeDefs = Types2.getDataTypeDefs exp
  in
  []


renderingFunctions : Lang.Exp -> List RenderingFunction
renderingFunctions exp =
  ["intervalToString"]


functionPickerAndEditor : Lang.Env -> Lang.Exp -> Lang.Val -> RenderingFunction -> List (Html Msg)
functionPickerAndEditor env program valueOfInterest renderingFunction =
  let
    renderingFunctionPicker =
      let optionNames = renderingFunctions program in
      Html.select [] (optionNames |> List.map (\optionName -> Html.option [Attr.name optionName] [VirtualDom.text optionName]))

  in
  [ renderingFunctionPicker
  , structuredEditor env program valueOfInterest renderingFunction
  ]


structuredEditor : Lang.Env -> Lang.Exp -> Lang.Val -> RenderingFunction -> Html Msg
structuredEditor env program valueOfInterest renderingFunction =
  let
    text = VirtualDom.text

    pathToString : ProjectionPath -> String
    pathToString path =
      case path of
        []            -> "•"
        n::deeperPath -> toString n ++ "." ++ pathToString deeperPath

    pathSetToString : Set ProjectionPath -> String
    pathSetToString pathSet =
      case Set.toList pathSet of
        []    -> "∅"
        paths -> "{" ++ String.join "," (List.map pathToString paths) ++ "}"

    renderStringTaggedWithProjectionPaths : StringTaggedWithProjectionPaths -> List (Html Msg)
    renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths =
      let superscript string = Html.sup [] [VirtualDom.text string] in
      case stringTaggedWithProjectionPaths of
        TaggedString string pathSet ->
          [ text <| "\"" ++ string ++ "\""
          , superscript <| pathSetToString pathSet
          ]

        TaggedStringAppend stringTaggedWithProjectionPaths1 stringTaggedWithProjectionPaths2 pathSet ->
          [ text "(" ] ++
          renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths1 ++
          [ text " ++ " ] ++
          renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths2 ++
          [ text ")"
          , superscript <| pathSetToString pathSet
          ]
  in
  case TinyStructuredEditorsForLowLowPricesEval.evalToStringTaggedWithProjectionPaths env program valueOfInterest renderingFunction of
    Ok stringTaggedWithProjectionPaths -> Html.span [] <| renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths
    Err errorMsg                       -> text errorMsg

-- renderStringTaggedWithProjectionPaths : StringTaggedWithProjectionPaths -> Html Msg