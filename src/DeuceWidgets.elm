module DeuceWidgets exposing (..) -- TODO

import Lang exposing (..)
import LangTools
import Utils
import Keys

import Set exposing (Set)
import Dict exposing (Dict)


--------------------------------------------------------------------------------

type alias DeuceState =
  { selectedWidgets : List DeuceWidget   -- not Set b/c not comparable
  , hoveredWidgets : List DeuceWidget    -- not Set b/c not comparable
  , hoveredMenuPath : List Int
  , renameVarTextBox : String
  }

type DeuceWidget
  = DeuceExp EId
  | DeucePat PathedPatternId
  | DeuceLetBindingEquation EId
  | DeuceExpTarget ExpTargetPosition
  | DeucePatTarget PatTargetPosition


isTargetPosition : DeuceWidget -> Bool
isTargetPosition widget =
  case widget of
    DeuceExp _                -> False
    DeucePat _                -> False
    DeuceLetBindingEquation _ -> False
    DeuceExpTarget _          -> True
    DeucePatTarget _          -> True


isSubWidget : Exp -> DeuceWidget -> DeuceWidget -> Bool
isSubWidget program widget superWidget =
  let isSubEId subEId superEId =
    superEId
    |> findExpByEId program
    |> Maybe.map (\superExp -> List.member subEId (allEIds superExp))
    |> Maybe.withDefault False
  in
  let boundExpContains subEId superLetEId =
    superLetEId
    |> findExpByEId program
    |> Maybe.andThen LangTools.expToMaybeLetBoundExp
    |> Maybe.map (\superBoundExp -> List.member subEId (allEIds superBoundExp))
    |> Maybe.withDefault False
  in
  let isSubPPId (subScopeId, subPath) (superScopeId, superPath) =
    subScopeId == superScopeId && Utils.isPrefix superPath subPath
  in
  case (widget, superWidget) of
    (DeuceExpTarget subExpTarget,       DeuceExpTarget superExpTarget)       -> subExpTarget == superExpTarget
    (DeucePatTarget subPatTarget,       DeucePatTarget superPatTarget)       -> subPatTarget == superPatTarget
    (_,                                 DeuceExpTarget _)                    -> False
    (_,                                 DeucePatTarget _)                    -> False
    (DeuceExp subEId,                   DeuceExp superEId)                   -> isSubEId subEId superEId
    (DeuceExp _,                        DeucePat _)                          -> False
    (DeuceExp subEId,                   DeuceLetBindingEquation superLetEId) -> boundExpContains subEId superLetEId
    (DeucePat subPPId,                  DeuceExp superEId)                   -> isSubEId (pathedPatIdToScopeEId subPPId) superEId
    (DeucePat subPPId,                  DeucePat superPPId)                  -> isSubPPId subPPId superPPId
    (DeucePat subPPId,                  DeuceLetBindingEquation superLetEId) -> pathedPatIdToScopeEId subPPId == superLetEId || boundExpContains (pathedPatIdToScopeEId subPPId) superLetEId
    (DeuceLetBindingEquation subLetEId, DeuceExp superEId)                   -> isSubEId subLetEId superEId
    (DeuceLetBindingEquation _,         DeucePat _)                          -> False
    (DeuceLetBindingEquation subLetEId, DeuceLetBindingEquation superLetEId) -> subLetEId == superLetEId || boundExpContains subLetEId superLetEId
    (DeuceExpTarget (_, subEId),        DeuceExp superEId)                   -> subEId /= superEId && isSubEId subEId superEId
    (DeuceExpTarget _,                  DeucePat _)                          -> False
    (DeuceExpTarget (_, subEId),        DeuceLetBindingEquation superLetEId) -> boundExpContains subEId superLetEId
    (DeucePatTarget (_, subPPId),       DeuceExp superEId)                   -> isSubEId (pathedPatIdToScopeEId subPPId) superEId
    (DeucePatTarget (_, subPPId),       DeucePat superPPId)                  -> subPPId /= superPPId && isSubPPId subPPId superPPId
    (DeucePatTarget (_, subPPId),       DeuceLetBindingEquation superLetEId) -> pathedPatIdToScopeEId subPPId == superLetEId || boundExpContains (pathedPatIdToScopeEId subPPId) superLetEId


toDeuceWidget : Dict PId PathedPatternId -> CodeObject -> Maybe DeuceWidget
toDeuceWidget patMap codeObject =
  case codeObject of
    E e ->
      Just <|
        DeuceExp e.val.eid
    P e p ->
      Maybe.map DeucePat <|
        Dict.get p.val.pid patMap
    T _ ->
      Nothing
    LBE eid ->
      Just <|
        DeuceLetBindingEquation eid.val
    ET ba _ et ->
      Just <|
        DeuceExpTarget (ba, et.val.eid)
    PT ba _ _ pt ->
      Maybe.map
        ( \ppid ->
            DeucePatTarget (ba, ppid)
        )
        ( Dict.get pt.val.pid patMap
        )
    TT _ _ _ ->
      Nothing

emptyDeuceState : DeuceState
emptyDeuceState =
  { selectedWidgets = []
  , hoveredWidgets = []
  , hoveredMenuPath = []
  , renameVarTextBox = ""
  }

setHoveredMenuPath path m =
  let deuceState = m.deuceState in
  { m | deuceState = { deuceState | hoveredMenuPath = path } }

clearHoveredMenuPath = setHoveredMenuPath []
