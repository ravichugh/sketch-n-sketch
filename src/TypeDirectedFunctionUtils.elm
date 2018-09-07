module TypeDirectedFunctionUtils exposing (..)

import AlgorithmJish
import FocusedEditingContext
import Lang exposing (..)
import Syntax
import Types
import Utils

import Set exposing (Set)
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
      --       let _ = Debug.log ident (Syntax.typeWithRolesUnparser Syntax.Elm tipe) in
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


roleExps : List (Ident, Exp)
roleExps =
  [ ("Ratio"              , eConstDummyLoc 0.62) -- Golden Ratio
  , ("Color"              , eConstDummyLoc 0)
  , ("StrokeWidth"        , eConstDummyLoc 5)
  , ("Point"              , eTuple [eInt0 0, eInt 0])
  , ("Vec2D"              , eTuple [eInt0 0, eInt 0])
  , ("Width"              , eConstDummyLoc 162) -- Golden ratio
  , ("Height"             , eConstDummyLoc 100)
  , ("HalfWidth"          , eConstDummyLoc 81) -- Golden ratio
  , ("HalfHeight"         , eConstDummyLoc 50)
  , ("HorizontalDistance" , eConstDummyLoc 81) -- Golden ratio
  , ("VerticalDistance"   , eConstDummyLoc 50)
  , ("Count"              , withDummyExpInfo <| EConst space1 3 dummyLoc (rangeSlider IntSlider 0 10))
  , ("PivotsOnTwo"        , withDummyExpInfo <| EConst space1 2 dummyLoc (rangeSlider IntSlider 1 5)) -- Possibly a depth
  , ("PivotsOnOne"        , withDummyExpInfo <| EConst space1 2 dummyLoc (rangeSlider IntSlider 0 4)) -- Possibly a depth
  , ("PivotsOnZero"       , withDummyExpInfo <| EConst space1 1 dummyLoc (rangeSlider IntSlider -1 3)) -- Possibly a depth
  , ("Radians"            , withDummyExpInfo <| EConst space1 0 dummyLoc (rangeSlider NumSlider -3.14 3.14))
  , ("Degrees"            , withDummyExpInfo <| EConst space1 0 dummyLoc (rangeSlider IntSlider -180 180))
  , ("Radius"             , eConstDummyLoc 100)
  , ("Distance"           , eConstDummyLoc 100)
  ]


maybeFillInArgPrimitive : Type -> Maybe Exp
maybeFillInArgPrimitive argType =
  Maybe.map (if Types.isPointType argType then identity else identity) <| -- eAsPoint
    let maybeFilledByRoles =
      argType.val.roles
      |> Set.toList
      |> Utils.mapFirstSuccess (flip Utils.maybeFind roleExps)
    in
    case maybeFilledByRoles of
      Just roleExp ->
        Just roleExp
      Nothing ->
        case argType.val.t__ of
          TNum _                    -> Just <| eConstDummyLoc 0
          TBool _                   -> Just <| eFalse
          TString _                 -> Just <| eStr "string"
          TNull _                   -> Just <| eNull
          TList _ _ _               -> Just <| eTuple []
          TDict _ _ _ _             -> Just <| eOp DictEmpty []
          TTuple _ headTypes _ _ _  -> List.map maybeFillInArgPrimitive headTypes |> Utils.projJusts |> Maybe.map eTuple
          TUnion _ types _          -> Utils.mapFirstSuccess maybeFillInArgPrimitive types
          TVar _ _                  -> Just <| eTuple []
          TWildcard _               -> Just <| eTuple []
          TNamed _ aliasName        -> Utils.maybeFind aliasName roleExps
          _                         -> Nothing
