module TinyStructuredEditorsForLowLowPrices exposing (prepare, selectPath, deselectPath)

import Dict
import Set

import Lang
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)
import TinyStructuredEditorsForLowLowPricesDesugaring
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

    valueOfInterestTagged =
      valueOfInterest
      |> TinyStructuredEditorsForLowLowPricesDesugaring.desugarVal
      |> TinyStructuredEditorsForLowLowPricesEval.tagVal []

    desugaredEnv = TinyStructuredEditorsForLowLowPricesDesugaring.desugarEnv env

    stringTaggedWithProjectionPathsResult =
      case maybeRenderingFunctionName of
        Just renderingFunctionName ->
          TinyStructuredEditorsForLowLowPricesEval.evalToStringTaggedWithProjectionPaths
              desugaredEnv
              valueOfInterestTagged
              renderingFunctionName

        Nothing ->
          Err "No rendering function chosen."

    stringProjectionPathToSpecificActions =
      stringTaggedWithProjectionPathsResult
      |> Result.toMaybe
      |> Maybe.map (TinyStructuredEditorsForLowLowPricesActions.generateActionsForValueAndAssociateWithStringLocations program valueOfInterestTagged)
      |> Maybe.withDefault Dict.empty

  in
  { oldModelState
  | renderingFunctionNames                = renderingFunctionNames
  , maybeRenderingFunctionName            = maybeRenderingFunctionName
  , desugaredEnv                          = desugaredEnv
  , valueOfInterestTagged                 = valueOfInterestTagged
  , stringTaggedWithProjectionPathsResult = stringTaggedWithProjectionPathsResult
  , stringProjectionPathToSpecificActions = stringProjectionPathToSpecificActions
  }


selectPath : TinyStructuredEditorsForLowLowPricesTypes.ModelState -> ProjectionPath -> TinyStructuredEditorsForLowLowPricesTypes.ModelState
selectPath oldModelState projectionPath =
  { oldModelState | selectedPaths = Set.insert projectionPath oldModelState.selectedPaths }


deselectPath : TinyStructuredEditorsForLowLowPricesTypes.ModelState -> ProjectionPath -> TinyStructuredEditorsForLowLowPricesTypes.ModelState
deselectPath oldModelState projectionPath =
  { oldModelState | selectedPaths = Set.remove projectionPath oldModelState.selectedPaths }


expToRenderingFunctionNames : Lang.Exp -> List Ident
expToRenderingFunctionNames exp =
  ["intervalToString"]
