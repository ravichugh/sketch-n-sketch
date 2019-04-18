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
import TinyStructuredEditorsForLowLowPricesActions


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

    stringTaggedWithSpecificActionsResult =
      stringTaggedWithProjectionPathsResult
      |> Result.map
        (TinyStructuredEditorsForLowLowPricesActions.generateActionsForValueAndAssociateWithStringLocations program valueOfInterest)

  in
  { renderingFunctionNames                = renderingFunctionNames
  , maybeRenderingFunctionName            = maybeRenderingFunctionName
  , stringTaggedWithProjectionPathsResult = stringTaggedWithProjectionPathsResult
  , stringTaggedWithSpecificActionsResult = stringTaggedWithSpecificActionsResult
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
  [ Html.div [] [renderingFunctionPicker]
  , stringTaggedWithProjectionPathsToHtml modelState.stringTaggedWithProjectionPathsResult
  , structuredEditor modelState.stringTaggedWithSpecificActionsResult
  ]


structuredEditor : Result String StringTaggedWithSpecificActions -> Html Msg
structuredEditor stringTaggedWithSpecificActionsResult =
  case stringTaggedWithSpecificActionsResult of
    Ok stringTaggedWithSpecificActions ->
      Html.div [Attr.style [("font-size", "18px")]] <|
        ( tagSet stringTaggedWithSpecificActions
          |> Set.toList
          |> List.map
              (\specificAction ->
                case specificAction of
                  Replace projectionPath taggedValue -> "Replace " ++ toString projectionPath ++ " with " ++ toString taggedValue.v
                  Scrub projectionPath               -> "Scrub "   ++ toString projectionPath
              )
          |> List.map (Html.div [] << List.singleton << VirtualDom.text)
        )
    Err errorMsg -> VirtualDom.text errorMsg


stringTaggedWithProjectionPathsToHtml : Result String StringTaggedWithProjectionPaths -> Html Msg
stringTaggedWithProjectionPathsToHtml stringTaggedWithProjectionPathsResult =
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
      let renderPathSet pathSet = Html.sup [Attr.style [("opacity", "0.5")]] [text <| pathSetToString pathSet] in
      case stringTaggedWithProjectionPaths of
        TaggedString string pathSet ->
          [ Html.span [Attr.style [("font-weight", "bold"), ("color", "#0c0")]] [text <| "\"" ++ string ++ "\""]
          , renderPathSet pathSet
          ]

        TaggedStringAppend stringTaggedWithProjectionPaths1 stringTaggedWithProjectionPaths2 pathSet ->
          [ text "(" ] ++
          renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths1 ++
          [ text " ++ " ] ++
          renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths2 ++
          [ text ")"
          , renderPathSet pathSet
          ]
  in
  case stringTaggedWithProjectionPathsResult of
    Ok stringTaggedWithProjectionPaths -> Html.div [Attr.style [("font-size", "18px")]] <| renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths
    Err errorMsg                       -> text errorMsg
