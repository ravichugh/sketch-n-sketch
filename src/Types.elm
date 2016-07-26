module Types where

import Lang exposing (..)
import Utils
import Dict

equal : Type -> Type -> Bool
equal t1 t2 =
  (astsMatch t1 t2) && (identifiersEquivalent t1 t2)

-- Do the types match, ignoring type variable names?
astsMatch t1 t2 =
  case (t1.val, t2.val) of
    (TNum _, TNum _)       -> True
    (TBool _, TBool _)     -> True
    (TString _, TString _) -> True
    (TList _ listType1 _, TList _ listType2 _) -> astsMatch listType1 listType2
    (TDict _ keyType1 valueType1 _,
     TDict _ keyType2 valueType2 _) -> (astsMatch keyType1 keyType2) && (astsMatch valueType1 valueType2)
    (TTuple _ typeList1 _ maybeRestType1 _,
     TTuple _ typeList2 _ maybeRestType2 _) ->
      let maybeRestTypesMatch =
        case (maybeRestType1, maybeRestType2) of
          (Nothing, Nothing)               -> True
          (Just restType1, Just restType2) -> astsMatch restType1 restType2
          _                                -> False
      in
      maybeRestTypesMatch && (Utils.listsEqualBy astsMatch typeList1 typeList2)
    (TArrow _ typeList1 _,
     TArrow _ typeList2 _) -> Utils.listsEqualBy astsMatch typeList1 typeList2
    (TVar _ _, TVar _ _)   -> True
    _                      -> False

-- Presuming the types have the same AST structure, do the identifiers used
-- produce the same semantic meaning?
--
-- e.g. (-> (List a) (List b)) is equivalent to (-> (List b) (List c))
--      but not to (-> (List y) (List y))
--
identifiersEquivalent t1 t2 =
  let flatIdents t =
    case t.val of
      TNum _    -> []
      TBool _   -> []
      TString _ -> []
      TList _ listType _          -> flatIdents listType
      TDict _ keyType valueType _ -> (flatIdents keyType) ++ (flatIdents valueType)
      TTuple _ typeList _ maybeRestType _ ->
        let restTypeIdents =
          case maybeRestType of
            Just restType -> flatIdents restType
            Nothing       -> []
        in
        (List.concatMap flatIdents typeList) ++ restTypeIdents

      TArrow _ typeList _ -> List.concatMap flatIdents typeList
      TVar _ ident        -> [ident]
  in
  Utils.oneToOneMappingExists (flatIdents t1) (flatIdents t2)
