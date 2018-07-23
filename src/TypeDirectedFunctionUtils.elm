module TypeDirectedFunctionUtils exposing (..)

import FocusedEditingContext
import Lang exposing (..)
import LangTools
import SlowTypeInference
import Types
import Utils

import Dict

--------------------------------------------------------------------------------

-- Find all functions in given program (no prelude) whose type matches the given predicate.
--
-- Returns list of (fName, fExp, typeSig), fExp is an EFun
--
-- To skip finding functions by inferred type, pass an empty dict for the typeGraph
getFunctionsByPredicateOnType : (Type -> Bool) -> SlowTypeInference.TC2Graph -> Exp -> Maybe (EId, a) -> List (Ident, Exp, Type)
getFunctionsByPredicateOnType hasDesiredType typeGraph program editingContext =
  let viewerEId = FocusedEditingContext.eidAtEndOfDrawingContext editingContext program in
  let boundExpsInScope =
    LangTools.expEnvAt_ program viewerEId
    |> Utils.fromJust_ "getFunctionsByPredicateOnType expEnvAt_"
    |> Dict.toList
    |> List.filterMap
        (\(ident, expBinding) ->
          case expBinding of
            LangTools.Bound boundExp -> Just (expEffectiveExp boundExp)
            LangTools.BoundUnknown   -> Nothing
        )
  in
  let explicitlyAnnotatedFunctions =
    findWithAncestorsByEId program viewerEId
    |> Utils.fromJust_ "getFunctionsByPredicateOnType findWithAncestorsByEId"
    |> List.filterMap
        (\exp ->
          case exp.val.e__ of
            ETyp _ typePat tipe body _ -> -- Only single types at a time for now.
              if hasDesiredType tipe then
                case LangTools.expToMaybeLetPatAndBoundExp (LangTools.firstNonComment body) of
                  Just (letPat, boundExp) ->
                    case (typePat.val.p__, letPat.val.p__) of
                      (PVar _ typeIdent _, PVar _ letIdent _) ->
                        if typeIdent == letIdent && List.member (expEffectiveExp boundExp) boundExpsInScope
                        then Just (typeIdent, expEffectiveExp boundExp, tipe)
                        else Nothing
                      _ -> Nothing
                  _ -> Nothing
              else
                Nothing
            _ -> Nothing
        )
  in
  if Dict.size typeGraph > 0 then
    let
      -- typeGraph = SlowTypeInference.typecheck program -- |> Debug.log "type graph"
      -- _ = Utils.log <| Syntax.unparser Syntax.Elm (LangTools.justFindExpByEId program viewerEId)
      -- _ = ImpureGoodies.logRaw (SlowTypeInference.graphVizString program typeGraph)
      otherDrawableFunctions =
        LangTools.expPatEnvAt_ program viewerEId
        |> Utils.fromJust_ "getFunctionsByPredicateOnType expPatEnvAt_"
        |> Dict.toList
        |> List.filterMap
            (\(ident, (pat, expBinding)) ->
              let inferred = SlowTypeInference.maybeTypes pat.val.pid typeGraph in
              -- let _ = inferred |> List.map (\tipe -> Debug.log ident (Syntax.typeUnparser Syntax.Elm tipe)) in
              -- let _ = Debug.log ident (SlowTypeInference.constraintsOnSubgraph pat.val.pid typeGraph) in
              case (inferred, expBinding) of
                ([tipe], LangTools.Bound boundExp) ->
                  if hasDesiredType tipe
                  then Just (ident, expEffectiveExp boundExp, tipe)
                  else Nothing
                _ -> Nothing
            )

      -- _ =
      --   -- boundExpsInScope
      --   flattenExpTree program
      --   |> List.map
      --       (\boundExp ->
      --         case Dict.get boundExp.val.eid typeInfo.finalTypes of
      --           Just (Just tipe) -> Utils.log <| "exp type: " ++ toString tipe ++ " for " ++ Syntax.unparser Syntax.Little boundExp
      --           _                -> Utils.log <| "no type for " ++ Syntax.unparser Syntax.Little boundExp
      --       )
    in
    explicitlyAnnotatedFunctions ++ otherDrawableFunctions |> Utils.dedupBy (\(ident, _, _) -> ident)
  else
    explicitlyAnnotatedFunctions


maybeFillInArgPrimitive : Type -> Maybe Exp
maybeFillInArgPrimitive argType =
  Maybe.map (if Types.isPointType argType then identity else identity) <| -- eAsPoint
    case argType.val of
      TNum _                         -> Just <| eConstDummyLoc 0
      TBool _                        -> Just <| eFalse
      TString _                      -> Just <| eStr "string"
      TNull _                        -> Just <| eNull
      TList _ _ _                    -> Just <| eTuple []
      TDict _ _ _ _                  -> Just <| eOp DictEmpty []
      TTuple _ headTypes _ Nothing _ -> List.map maybeFillInArgPrimitive headTypes |> Utils.projJusts |> Maybe.map eTuple
      TUnion _ (firstType::_) _      -> maybeFillInArgPrimitive firstType
      TVar _ _                       -> Just <| eTuple []
      TWildcard _                    -> Just <| eTuple []
      TNamed _ "Color"               -> Just <| eConstDummyLoc 0
      TNamed _ "StrokeWidth"         -> Just <| eConstDummyLoc 5
      TNamed _ "Point"               -> Just <| eTuple [eInt0 0, eInt 0]
      TNamed _ "Width"               -> Just <| eConstDummyLoc 162 -- Golden ratio
      TNamed _ "Height"              -> Just <| eConstDummyLoc 100
      TNamed _ "Count"               -> Just <| withDummyExpInfo <| EConst space1 3 dummyLoc (rangeSlider IntSlider 1 10)
      TNamed _ "Radians"             -> Just <| withDummyExpInfo <| EConst space1 0 dummyLoc (rangeSlider NumSlider -3.14 3.14)
      TNamed _ "Degrees"             -> Just <| withDummyExpInfo <| EConst space1 0 dummyLoc (rangeSlider IntSlider -180 180)
      TNamed _ "Radius"              -> Just <| eConstDummyLoc 150
      _                              -> Nothing
