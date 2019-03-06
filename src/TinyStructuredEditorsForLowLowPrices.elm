module TinyStructuredEditorsForLowLowPrices exposing (prepare, functionPickerAndEditor)

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
-- import Types2
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)
import TinyStructuredEditorsForLowLowPricesEval


-- type alias DataConDef = (Ident, List Type)
-- type alias DataTypeDef = (Ident, List DataConDef)
-- getDataTypeDefs : Exp -> List DataTypeDef

-- valToSpecificActions : Lang.Exp -> Lang.Val -> List SpecificAction
-- valToSpecificActions exp val =
--   let
--     dataTypeDefs = Types2.getDataTypeDefs exp
--   in
--   []


----------- Controller -----------

-- Do all the computation after a program run.
--
-- I hate caching but if we instead perform the work on
-- demand in the view then the GUI slows to a crawl.
prepare : TinyStructuredEditorsForLowLowPricesTypes.ModelState -> Lang.Env -> Lang.Exp -> Lang.Val -> TinyStructuredEditorsForLowLowPricesTypes.ModelState
prepare oldModelState env program valueOfInterest =
  let
    renderingFunctionNames =
      expToRenderingFunctionNames program

    maybeRenderingFunctionName =
      -- Use the previously selected function, if it's still available.
      oldModelState.maybeRenderingFunctionName
      |> Utils.filterMaybe (flip List.member renderingFunctionNames)
      |> Utils.plusMaybe (List.head renderingFunctionNames)

    stringTaggedWithProjectionPathsResult =
      case maybeRenderingFunctionName of
        Just renderingFunctionName ->
          TinyStructuredEditorsForLowLowPricesEval.evalToStringTaggedWithProjectionPaths
              env
              program
              valueOfInterest
              renderingFunctionName

        Nothing ->
          Err "No rendering function chosen."
  in
  { renderingFunctionNames                = renderingFunctionNames
  , maybeRenderingFunctionName            = maybeRenderingFunctionName
  , stringTaggedWithProjectionPathsResult = stringTaggedWithProjectionPathsResult
  }

expToRenderingFunctionNames : Lang.Exp -> List Ident
expToRenderingFunctionNames exp =
  ["intervalToString"]


-------------- View -----------

functionPickerAndEditor : TinyStructuredEditorsForLowLowPricesTypes.ModelState -> List (Html Msg)
functionPickerAndEditor modelState =
  let
    renderingFunctionPicker =
      let optionNames = modelState.renderingFunctionNames in
      Html.select [] (optionNames |> List.map (\optionName -> Html.option [Attr.name optionName] [VirtualDom.text optionName]))

  in
  [ renderingFunctionPicker
  , structuredEditor modelState.stringTaggedWithProjectionPathsResult
  ]


structuredEditor : Result String StringTaggedWithProjectionPaths -> Html Msg
structuredEditor stringTaggedWithProjectionPathsResult =
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
  case stringTaggedWithProjectionPathsResult of
    Ok stringTaggedWithProjectionPaths -> Html.span [] <| renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths
    Err errorMsg                       -> text errorMsg
