module FindRepeatTools exposing (..)

import FastParser
import TypeDirectedFunctionUtils
import Lang exposing (..)
import LangTools
import SlowTypeInference
import Types
import Utils

import Dict


-- Returns list of (fName, fExp, typeSig), fExp is an EFun
preludeRepetitionFunctions : List (Ident, Exp, Type)
preludeRepetitionFunctions =
  TypeDirectedFunctionUtils.getFunctionsByPredicateOnType
      isRepetitionFunctionType
      Dict.empty
      FastParser.prelude
      Nothing


-- Returns list of (fName, fExp, typeSig), fExp is an EFun
getRepetitionFunctions : Exp -> SlowTypeInference.TC2Graph -> Maybe (EId, a) -> List (Ident, Exp, Type)
getRepetitionFunctions program typeGraph editingContext =
  TypeDirectedFunctionUtils.getFunctionsByPredicateOnType
      isRepetitionFunctionType
      typeGraph
      program
      editingContext ++
  preludeRepetitionFunctions
  |> Utils.dedupBy Utils.fst3 -- Remove shadowed prelude functions.


-- Functions of form: ... -> Point -> ... -> List Point
--
-- Dual is in ValueBasedTransform.repeatUsingFunction where the args are actually filled in.
isRepetitionFunctionType : Type -> Bool
isRepetitionFunctionType tipe =
  case tipe.val of
    TArrow _ argTypes _ ->
      case Utils.maybeUnconsLast argTypes of
        Just (inputTypes, returnType) -> List.any Types.isPointType inputTypes && (Types.maybeListElementsType returnType |> Maybe.map Types.isPointType |> Maybe.withDefault False)
        _                             -> False

    _ -> False

