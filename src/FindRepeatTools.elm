module FindRepeatTools exposing (..)

import AlgorithmJish
import FastParser
import TypeDirectedFunctionUtils
import Lang exposing (..)
import LangTools
import Types
import Utils

import Dict


-- Returns list of (fName, typeSig), fExp is an EFun
getRepetitionFunctions : Exp -> AlgorithmJish.IdToTypeAndContextThunk -> Maybe (EId, a) -> List (Ident, Type)
getRepetitionFunctions program idToTypeAndContextThunk editingContext =
  TypeDirectedFunctionUtils.getFunctionsByPredicateOnType
      isRepetitionFunctionType
      idToTypeAndContextThunk
      program
      editingContext


-- Functions of form: ... -> Point -> ... -> List Point
--
-- Dual is in ValueBasedTransform.repeatUsingFunction where the args are actually filled in.
isRepetitionFunctionType : Type -> Bool
isRepetitionFunctionType tipe =
  case Types.typeToMaybeArgTypesAndReturnType tipe of
    Just (inputTypes, returnType) -> List.any Types.isPointType inputTypes && (Types.maybeListElementsType returnType |> Maybe.map Types.isPointType |> Maybe.withDefault False)
    _                             -> False

