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
  , mbKeyboardFocusedWidget : Maybe DeuceWidget
  , hoveredWidgets : List DeuceWidget    -- not Set b/c not comparable
      -- not used for styling anymore (see .code-object-polygon:hover),
      -- but still tracking in case other UI elements depend on knowing
      -- hovered widgets
  , hoveredMenuPath : List Int
  , renameVarText : String
  , smartCompleteText : String
  }

type DeuceWidget
  = DeuceExp EId
  | DeucePat PathedPatternId
  | DeuceLetBindingEquation (EId, BindingNumber)
  | DeuceDeclTarget DeclarationTargetPosition
  | DeuceExpTarget ExpTargetPosition
  | DeucePatTarget PatTargetPosition
  | DeuceType -- TODO TId


isTargetPosition : DeuceWidget -> Bool
isTargetPosition widget =
  case widget of
    DeuceExp _                -> False
    DeucePat _                -> False
    DeuceLetBindingEquation _ -> False
    DeuceType                 -> False
    DeuceExpTarget _          -> True
    DeucePatTarget _          -> True
    DeuceDeclTarget _         -> True

-- TODO: This is not totally correct because of DeuceLetBindingEquation which now has (EId, BindingNumber)
-- It appears that this function returns true if both widgets are the same
isSubWidget : Exp -> DeuceWidget -> DeuceWidget -> Bool
isSubWidget program widget superWidget =
  let isSubEId subEId superEId =
    superEId
    |> findExpByEId program
    |> Maybe.map (\superExp -> List.member subEId (allEIds superExp))
    |> Maybe.withDefault False
  in
  let boundExpContains subEId superLetEId superBn =
    superLetEId
    |> findExpByEId program
    |> Maybe.andThen LangTools.declarationsOf
    |> Maybe.map getDeclarationsInOrder
    |> Maybe.andThen (flip Utils.nth superBn >> Result.toMaybe)
    |> Maybe.andThen letExpOf
    |> Maybe.map (\(LetExp _ _ _ _ _ superboundExp) -> List.member subEId (allEIds superboundExp))
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
    (DeuceExp subEId,                   DeuceLetBindingEquation (letId, bn)) -> boundExpContains subEId letId bn
    (DeucePat subPPId,                  DeuceExp superEId)                   -> isSubEId (pathedPatIdToScopeEId subPPId) superEId
    (DeucePat subPPId,                  DeucePat superPPId)                  -> isSubPPId subPPId superPPId
    (DeucePat subPPId,                  DeuceLetBindingEquation (letId, bn)) -> pathedPatIdToScopeEId subPPId == letId || boundExpContains (pathedPatIdToScopeEId subPPId) letId bn
    (DeuceLetBindingEquation (letId, bn), DeuceExp superEId)                 -> isSubEId letId superEId
    (DeuceLetBindingEquation _,         DeucePat _)                          -> False
    (DeuceLetBindingEquation (letId1, bn1), DeuceLetBindingEquation (letId2, bn2)) ->
      letId1 == letId2 && bn1 == bn2 || boundExpContains letId1 letId2 bn2
    (DeuceExpTarget (_, subEId),        DeuceExp superEId)                   -> subEId /= superEId && isSubEId subEId superEId
    (DeuceExpTarget _,                  DeucePat _)                          -> False
    (DeuceExpTarget (_, subEId),        DeuceLetBindingEquation (superLetEId, bn)) ->
      boundExpContains subEId superLetEId bn
    (DeucePatTarget (_, subPPId),       DeuceExp superEId)                   -> isSubEId (pathedPatIdToScopeEId subPPId) superEId
    (DeucePatTarget (_, subPPId),       DeucePat superPPId)                  -> subPPId /= superPPId && isSubPPId subPPId superPPId
    (DeucePatTarget (_, subPPId),       DeuceLetBindingEquation (superLetEId, bn)) ->
      pathedPatIdToScopeEId subPPId == superLetEId || boundExpContains (pathedPatIdToScopeEId subPPId) superLetEId bn
    (DeuceDeclTarget b1,                DeuceDeclTarget b2)                  -> b1 == b2
    (_,                                 DeuceDeclTarget b2)                 -> False
    (DeuceDeclTarget (_, (subEId, bn)), DeuceExp superEId)                  -> subEId /= superEId && isSubEId subEId superEId
    (DeuceDeclTarget (_, (subEId, bn)), _)                                  -> False
    -- TODO
    (DeuceType,                         DeuceType)                           -> True
    (_,                                 DeuceType)                           -> False
    (DeuceType,                         _)                                   -> False

toDeuceWidget : Dict PId PathedPatternId -> CodeObject -> Maybe DeuceWidget
toDeuceWidget patMap codeObject =
  case codeObject of
    E e ->
      Just <|
        DeuceExp (expEId e)
    P e p ->
      Maybe.map DeucePat <|
        Dict.get p.val.pid patMap
    T _ ->
      Just <|
        DeuceType -- TODO tid
    D eid bNum ->
      Just <|
        DeuceLetBindingEquation (eid.val, bNum)
    DT beforeAfter ws exp bnum ->
      Just <|
        DeuceDeclTarget (beforeAfter, (expEId exp, bnum))
    ET ba _ et ->
      Just <|
        DeuceExpTarget (ba, (expEId et))
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
  , mbKeyboardFocusedWidget = Nothing
  , hoveredWidgets = []
  , hoveredMenuPath = []
  , renameVarText = ""
  , smartCompleteText = ""
  }

setHoveredMenuPath path m =
  let deuceState = m.deuceState in
  { m | deuceState = { deuceState | hoveredMenuPath = path } }

clearHoveredMenuPath = setHoveredMenuPath []
