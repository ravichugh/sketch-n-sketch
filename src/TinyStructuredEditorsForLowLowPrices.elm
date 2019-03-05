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
  -- let
  --   -- specificActions = valToSpecificActions exp val
  --
  --   -- stringTaggedWithProjectionPaths =
  -- in
  case TinyStructuredEditorsForLowLowPricesEval.evalToStringTaggedWithProjectionPaths env program valueOfInterest renderingFunction of
    Ok stringTaggedWithProjectionPaths -> VirtualDom.text "yeah"
    Err errorMsg                       -> VirtualDom.text errorMsg

-- renderStringTaggedWithProjectionPaths : StringTaggedWithProjectionPaths -> Html Msg