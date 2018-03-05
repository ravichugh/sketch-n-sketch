module FocusedEditingContext exposing (..)

import Lang exposing (..)
import LangTools


editingContextFromMarkers : Exp -> Maybe (EId, Maybe EId)
editingContextFromMarkers program =
  let expToMaybeFocusedExp exp =
    case exp.val.e__ of
      EComment _ " *** Focused Definition ***" commentBody ->
        LangTools.firstNonComment commentBody
        |> LangTools.expToMaybeLetBoundExp
      EComment _ " *** Focused Expression ***" commentBody ->
        Just (LangTools.firstNonComment commentBody)
      _ ->
        Nothing
  in
  let expToMaybeExampleCall exp =
    case exp.val.e__ of
      EComment _ " *** Example Call ***" commentBody -> findFirstNode isApp commentBody
      _                                              -> Nothing
  in
  mapFirstSuccessNode expToMaybeFocusedExp program
  |> Maybe.map
      (\focusedExp ->
        if isFunc focusedExp then
          let maybeExampleCallEId = mapFirstSuccessNode expToMaybeExampleCall program |> Maybe.map (.val >> .eid) in
          (focusedExp.val.eid, maybeExampleCallEId)
        else
          (focusedExp.val.eid, Nothing)
      )


clearEditingContextMarkers : Exp -> Exp
clearEditingContextMarkers exp =
  exp
  |> mapExp
      (\exp ->
        case exp.val.e__ of
          EComment _ " *** Focused Definition ***" commentBody -> commentBody |> copyPrecedingWhitespace exp
          EComment _ " *** Focused Expression ***" commentBody -> commentBody |> copyPrecedingWhitespace exp
          EComment _ " *** Example Call ***"       commentBody -> commentBody |> copyPrecedingWhitespace exp
          _                                                    -> exp
      )


setEditingContextMarkers : EId -> Maybe EId -> Exp -> Exp
setEditingContextMarkers focusedEId maybeExampleCallEId program =
  program
  |> mapExp
      (\exp ->
        case (exp.val.eid == focusedEId, LangTools.expToMaybeLetBoundExp exp) of
          (True, _) ->
            eComment " *** Focused Expression ***" (exp |> replacePrecedingWhitespace ("  " ++ indentationAt exp.val.eid program))
            |> copyPrecedingWhitespace exp

          (_, Just boundExp) ->
            if List.member focusedEId (expEffectiveEIds boundExp) then
              -- Need to clear deeper markers because expression will already have been marked.
              eComment " *** Focused Definition ***" (clearEditingContextMarkers exp |> replacePrecedingWhitespace (indentationAt exp.val.eid program))
              |> copyPrecedingWhitespace exp
            else
              exp

          _ ->
            exp
      )
  |> case maybeExampleCallEId of
      Nothing             -> identity
      Just exampleCallEId ->
        mapExp
            (\exp ->
              if exp.val.eid == exampleCallEId then
                eComment " *** Example Call ***" (exp |> replacePrecedingWhitespace ("  " ++ indentationAt exp.val.eid program))
                |> copyPrecedingWhitespace exp
              else
                exp
            )
