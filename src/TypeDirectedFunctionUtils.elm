module TypeDirectedFunctionUtils exposing (..)

import AlgorithmJish
import FocusedEditingContext
import Lang exposing (..)
import Syntax
import Types
import Utils

import Dict exposing (Dict)

--------------------------------------------------------------------------------

-- Find all functions in given program (no prelude) whose type matches the given predicate.
--
-- Returns list of (fName, typeSig)
getFunctionsByPredicateOnType : (Type -> Bool) -> AlgorithmJish.IdToTypeAndContextThunk -> Exp -> Maybe (EId, a) -> List (Ident, Type)
getFunctionsByPredicateOnType hasDesiredType idToTypeAndContextThunk program editingContext =
  let viewerEId = FocusedEditingContext.eidAtEndOfDrawingContext editingContext program in
  case Dict.get viewerEId idToTypeAndContextThunk of
    Just (_, typingContextThunk) ->
      let typingContext = typingContextThunk () in
      let typesInScope = typingContext |> Utils.removeShadowedKeys in
      typingContext
      -- |> List.map
      --     (\(ident, tipe) ->
      --       let _ = Debug.log ident (Syntax.typeUnparser Syntax.Elm tipe) in
      --       (ident, tipe)
      --     )
      |> List.filter (\(ident, tipe) -> hasDesiredType tipe)
      |> List.map (Tuple.mapSecond Types.prettify)

    Nothing ->
      []


clearlyNotShapeOrListOfShapesType : Type -> Bool
clearlyNotShapeOrListOfShapesType tipe =
  Types.isPointType tipe ||
  Types.isPointListType tipe ||
  (
    maybeFillInArgPrimitive tipe
    |> Maybe.map clearlyNotShapeOrListOfShapesExp
    |> Maybe.withDefault False
  )


clearlyNotShapeOrListOfShapesExp : Exp -> Bool
clearlyNotShapeOrListOfShapesExp exp =
  case (expEffectiveExp exp).val.e__ of
    EConst _ _ _ _              -> True
    EBase _ _                   -> True
    EList _ ((_,head)::_) _ _ _ -> let effectiveHead = expEffectiveExp head in if isList effectiveHead then clearlyNotShapeOrListOfShapesExp effectiveHead else not (isString effectiveHead || isVar effectiveHead || isApp effectiveHead)
    _                           -> False


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
      TNamed _ "Ratio"               -> Just <| eConstDummyLoc 0.62 -- Golden Ratio
      TNamed _ "Color"               -> Just <| eConstDummyLoc 0
      TNamed _ "StrokeWidth"         -> Just <| eConstDummyLoc 5
      TNamed _ "Point"               -> Just <| eTuple [eInt0 0, eInt 0]
      TNamed _ "Vec2D"               -> Just <| eTuple [eInt0 0, eInt 0]
      TNamed _ "Width"               -> Just <| eConstDummyLoc 162 -- Golden ratio
      TNamed _ "Height"              -> Just <| eConstDummyLoc 100
      TNamed _ "HalfWidth"           -> Just <| eConstDummyLoc 81 -- Golden ratio
      TNamed _ "HalfHeight"          -> Just <| eConstDummyLoc 50
      TNamed _ "Count"               -> Just <| withDummyExpInfo <| EConst space1 3 dummyLoc (rangeSlider IntSlider 0 10)
      TNamed _ "Radians"             -> Just <| withDummyExpInfo <| EConst space1 0 dummyLoc (rangeSlider NumSlider -3.14 3.14)
      TNamed _ "Degrees"             -> Just <| withDummyExpInfo <| EConst space1 0 dummyLoc (rangeSlider IntSlider -180 180)
      TNamed _ "Radius"              -> Just <| eConstDummyLoc 150
      TNamed _ "Distance"            -> Just <| eConstDummyLoc 150
      _                              -> Nothing
