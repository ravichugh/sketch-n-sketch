-- Hook numbers up to SnS's live synchronization.
module TinyStructuredEditorsForLowLowPricesScrub exposing (prepareLiveUpdates)

import Dict exposing (Dict)
import Either exposing (Either(..))
import Set

import Lang
import LeoParser
import ShapeWidgets -- for RealZone
import Sync
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)


-- Based on Sync.prepareLiveUpdates_
prepareLiveUpdates : Sync.Options -> Lang.Exp -> Lang.Val -> Sync.LiveInfo
prepareLiveUpdates syncOptions langExp langValOfInterest =
  let
    initSubstPlus = LeoParser.substPlusOf langExp
    initSubst     = Dict.map (always .val) initSubstPlus

    initMaybeCounts =
      if syncOptions.feelingLucky == Sync.heuristicsFair then
        Just (Right Dict.empty)
      else if syncOptions.feelingLucky == Sync.heuristicsBiased then
        Just (Left (getLocationCounts syncOptions langValOfInterest))
      else {- options.feelingLucky == heuristicsNone -}
        Nothing

    projectionPathNumTrs = langValToProjectionPathNumTrs [] langValOfInterest
  in
  { initSubstPlus = initSubstPlus
  , triggers      = computeTriggers (syncOptions, initSubst) projectionPathNumTrs initMaybeCounts
  }


-- Count the number of projection paths that use each loc.
getLocationCounts : Sync.Options -> Lang.Val -> Dict Lang.Loc Int
getLocationCounts syncOptions langValOfInterest =
  langValOfInterest
  |> langValToProjectionPathNumTrs []
  |> Utils.foldl
      Dict.empty
      (\(projectionPath, (num, tr)) counts ->
        Set.foldl Sync.updateCount counts (Sync.locsOfTrace syncOptions tr)
      )


-- Find the projection paths with numbers+traces in the ADT.
--
-- Based on TinyStructuredEditorsForLowLowPricesDesugaring.desugarVal and TinyStructuredEditorsForLowLowPricesEval.tagVal
langValToProjectionPathNumTrs : ProjectionPath -> Lang.Val -> List (ProjectionPath, Lang.NumTr)
langValToProjectionPathNumTrs path langVal =
  let
    recurseChildI childI = langValToProjectionPathNumTrs (path ++ [childI])
  in
  case langVal.v_ of
    Lang.VList []                  -> []
    Lang.VList (headVal::tailVals) ->
      -- VLists are desugared to core language VCtor's of Nil | Cons a (List a).
      -- Immitate the resulting paths for the a's.
      recurseChildI 1 headVal ++ recurseChildI 2 (Lang.VList tailVals |> Lang.replaceV_ langVal)
    Lang.VDict d        -> []
    Lang.VRecord d      ->
      case Lang.valToMaybeCtorNameAndArgVals langVal of
        Just (ctorName, argLangVals) -> argLangVals |> Utils.mapi1 (uncurry recurseChildI) |> List.concat
        Nothing                      -> []

    Lang.VConst offsetProvenance (num, tr)      -> [(path, (num, tr))]
    Lang.VBase (Lang.VBool True)                -> []
    Lang.VBase (Lang.VBool False)               -> []
    Lang.VBase (Lang.VString string)            -> []
    Lang.VBase Lang.VNull                       -> []
    Lang.VClosure recNames pats bodyExp funcEnv -> []
    Lang.VFun _ _ _ _                           -> []


-- Based on Sync.computeWidgetTriggers
computeTriggers : (Sync.Options, Lang.Subst) -> List (ProjectionPath, Lang.NumTr) -> Sync.MaybeCounts -> Sync.Triggers
computeTriggers (syncOptions, subst) projectionPathNumTrs initMaybeCounts =
  let
    triggersAndMaybeCountsInitAcc = (Dict.empty, initMaybeCounts)

    addTriggerForProjectionPathTrace (path, (num, tr)) triggersAndMaybeCountsAcc =
      -- Based on the WOffset1D case in Sync.computeWidgetTriggers
      Sync.addTrigger
          syncOptions
          1 -- shape id
          (ShapeWidgets.ZTSEFLLPScrub path) -- realZone
          [tr] -- zone traces
          (Utils.unwrap1 >> \maybeLoc ->
            Sync.mapMaybeToList maybeLoc (\loc ->
              ( "", "dy", loc, tr
              , \solutionsCache _ (_,dy) -> Sync.solveOne solutionsCache subst loc (num - toFloat dy) tr
              ))
          )
          triggersAndMaybeCountsAcc

    (triggers, _) =
      projectionPathNumTrs
      |> Utils.foldl
          triggersAndMaybeCountsInitAcc
          addTriggerForProjectionPathTrace
  in
  triggers
