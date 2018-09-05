module Types exposing (..)

import Lang exposing (..)
import ValUnparser exposing (..)
import Pos exposing (..)
import Info exposing (..)
import FastParser as Parser
import LangUnparser exposing (unparse, unparsePat, unparseType, unparseTypeWithUniformWhitespace)
import Utils
import Ace
import Config

import Dict exposing (Dict)
import Set exposing (Set)
import String


-- AST Helpers for Types -----------------------------------------------------

withDummyRangeAndNoRoles t__ = withDummyRange { t__ = t__, roles = Set.empty }

tBool   = withDummyRangeAndNoRoles (TBool space1)
tNum    = withDummyRangeAndNoRoles (TNum space1)
tString = withDummyRangeAndNoRoles (TString space1)
tNull   = withDummyRangeAndNoRoles (TNull space1)
tVar x  = withDummyRangeAndNoRoles (TVar space1 x)

tTupleRest ts tRest = withDummyRangeAndNoRoles (TTuple space1 ts space0 tRest space0)
tTuple ts = tTupleRest ts Nothing

tList t = withDummyRangeAndNoRoles (TList space1 t space0)

tDict t1 t2 =  withDummyRangeAndNoRoles (TDict space1 t1 t2 space0)

tArrows ts = withDummyRangeAndNoRoles (TArrow space1 ts space0)
tArrow t0 t1 = tArrows [t0, t1]

tForall vars t =
  case vars of
    []    -> Debug.crash "AlgorithmJish tForall: no vars"
    [a]   -> withDummyRangeAndNoRoles (TForall space1 (One (space1, a)) t space0)
    a::bs -> let typeVars = (space0, a) :: List.map (\a -> (space1, a)) bs in
             withDummyRangeAndNoRoles (TForall space1 (Many space1 typeVars space0) t space0)

tUnion ts = withDummyRangeAndNoRoles (TUnion space1 ts space0)


-- Other Helpers ------------------------------------------------------------

isForall : Type -> Bool
isForall tipe =
  case tipe.val.t__ of
    TForall _ _ _ _ -> True
    _               -> False


isListNotTuple : Type -> Bool
isListNotTuple tipe =
  case tipe.val.t__ of
    TList _ _ _     -> True
    TForall _ _ t _ -> isListNotTuple t
    _               -> False


isListOrTuple : Type -> Bool
isListOrTuple tipe =
  case tipe.val.t__ of
    TList _ _ _      -> True
    TTuple _ _ _ _ _ -> True
    TForall _ _ t _  -> isListOrTuple t
    _                -> False


maybeListElementsType : Type -> Maybe Type
maybeListElementsType tipe =
  case tipe.val.t__ of
    TList _ elementsType _ -> Just elementsType
    TForall _ _ t _        -> maybeListElementsType t
    _                      -> Nothing


-- List with heads and tail of all the same type can be considered TList instead of TTuple
simplifyTailedTuple : Type -> Type
simplifyTailedTuple tipe =
  let wrap t__ = replaceT__ tipe t__ in
  case tipe.val.t__ of
    TTuple ws1 ts ws2 (Just tTail) ws3 ->
      case maybeListOrHomogenousTupleElementsType tipe of
        Just t1 -> wrap (TList ws1 t1 ws3)
        Nothing -> tipe

    _ ->
      tipe


allListsOrHomogenousTuplesOfSameTipe : List Type -> Bool
allListsOrHomogenousTuplesOfSameTipe tipes =
  List.all isListOrTuple tipes &&
  List.all (not << typeContains isForall) tipes &&
  case List.concatMap childTypes tipes of
    []    -> True
    t::ts -> List.all (equalUnderSameTypeVars t) ts


maybeListOrHomogenousTupleElementsType : Type -> Maybe Type
maybeListOrHomogenousTupleElementsType tipe =
  let figureItOut () =
    case childTypes tipe of
      []    -> Just (tForall ["a"] (tVar "a"))
      t::ts ->
        if List.all (equalUnderSameTypeVars t) ts && List.all (not << typeContains isForall) (t::ts)
        then Just t
        else Nothing
  in
  case tipe.val.t__ of
    TList _ elementsType _       -> figureItOut ()
    TTuple _ heads _ maybeTail _ -> figureItOut ()
    TForall _ _ t _              -> maybeListOrHomogenousTupleElementsType t
    _                            -> Nothing


-- A "Point" type is either:
--   - A `[Num, Num]` pair tuple
--   - A `Point` type alias
--   - A `[Num, Num | a]` type, because type inference will type a literal [Num, Num] expression as a [Num, Num | a]
isPointType : Type -> Bool
isPointType tipe =
  Set.member "Point" (typeToRoles tipe) ||
  case tipe.val.t__ of
    TForall _ _ t _ -> isPointType t
    TTuple _ heads _ maybeTail _ ->
      case heads |> List.map (.val >> .t__) of
        [TNum _, TNum _] ->
          case maybeTail |> Maybe.map (.val >> .t__) of
            Nothing         -> True
            Just (TVar _ _) -> True
            _               -> False
        _                -> False
    _ -> False


isPointListType : Type -> Bool
isPointListType tipe =
  maybeListElementsType tipe
  |> Maybe.map isPointType
  |> Maybe.withDefault False


isNumType : Type -> Bool
isNumType tipe =
  case tipe.val.t__ of
    TForall _ _ t _ -> isNumType t
    TNum _          -> True
    _               -> False


typeToMaybeAliasIdent : Type -> Maybe Ident
typeToMaybeAliasIdent tipe =
  case tipe.val.t__ of
    TNamed _ aliasName -> Just aliasName
    _                  -> Nothing


typeToRoles : Type -> Set Ident
typeToRoles tipe =
  case typeToMaybeAliasIdent tipe of
    Just aliasName -> Set.insert aliasName tipe.val.roles -- In case role not inserted by parser or synthesis.
    Nothing        -> tipe.val.roles


typeToMaybeArgTypesAndReturnType : Type -> Maybe (List Type, Type)
typeToMaybeArgTypesAndReturnType tipe =
  case (prettify tipe).val.t__ of
    TForall _ _ t1 _ ->
      typeToMaybeArgTypesAndReturnType t1

    TArrow _ types _ ->
      case (Utils.dropLast 1 types, Utils.maybeLast types) of
        (argTypes, Just returnType) -> Just (argTypes, returnType)
        _                           -> Nothing

    _ -> Nothing


-- Roles on arrows not preserved (args are okay)
inlineArrow : Type -> Type
inlineArrow tipe =
  case tipe.val.t__ of
    TArrow ws1 types ws2 ->
      case Utils.maybeUnconsLast types of
        Just (leftTypes, lastType) ->
          case (inlineArrow lastType).val.t__ of
            TArrow _ rightTypes _ -> replaceT__ tipe (TArrow ws1 (leftTypes ++ rightTypes) ws2)
            _                     -> tipe
        Nothing -> tipe
    _ -> tipe


prettify : Type -> Type
prettify = mapType (flattenUnion >> inlineArrow)


-- Flattens immediately nested unions. Dumb flattening, no dedup.
-- Roles not propogated upward (on purpose).
flattenUnion : Type -> Type
flattenUnion tipe =
  case tipe.val.t__ of
    TUnion ws1 types ws2 ->
      let newTypes =
        types
        |> List.concatMap
            (\t ->
              case (flattenUnion t).val.t__ of
                TUnion _ tChildren _ -> tChildren
                _                    -> [t]
            )
      in
      replaceT__ tipe (TUnion ws1 newTypes ws2)

    _ -> tipe


-- Do the types match, modulo type variable renaming?
equal : Type -> Type -> Bool
equal t1 t2 =
  let
    t1FreeIdents     = freeIdentifiersList t1 |> Utils.dedup
    t2FreeIdents     = freeIdentifiersList t2 |> Utils.dedup
    t2IdentToT1Ident = Utils.zip t2FreeIdents t1FreeIdents |> Dict.fromList
  in
  equal_ t1 t2 t2IdentToT1Ident


equalUnderSameTypeVars : Type -> Type -> Bool
equalUnderSameTypeVars t1 t2 =
  let
    identifiers      = freeIdentifiersList t1 ++ freeIdentifiersList t2 |> Utils.dedup
    t2IdentToT1Ident = identifiers |> List.map (\ident -> (ident, ident)) |> Dict.fromList
  in
  equal_ t1 t2 t2IdentToT1Ident


equal_ : Type -> Type -> Dict Ident Ident -> Bool
equal_ t1 t2 t2IdentToT1Ident =
  case ((flattenUnion (inlineArrow t1)).val.t__, (flattenUnion (inlineArrow t2)).val.t__) of
    (TNum _, TNum _)       -> True
    (TBool _, TBool _)     -> True
    (TString _, TString _) -> True
    (TNull _, TNull _)     -> True
    (TList _ listType1 _, TList _ listType2 _) -> equal_ listType1 listType2 t2IdentToT1Ident
    (TDict _ keyType1 valueType1 _,
     TDict _ keyType2 valueType2 _) -> equal_ keyType1 keyType2 t2IdentToT1Ident && equal_ valueType1 valueType2 t2IdentToT1Ident
    (TTuple _ typeList1 _ maybeRestType1 _,
     TTuple _ typeList2 _ maybeRestType2 _) ->
      let maybeRestTypesMatch =
        case (maybeRestType1, maybeRestType2) of
          (Nothing, Nothing)               -> True
          (Just restType1, Just restType2) -> equal_ restType1 restType2 t2IdentToT1Ident
          _                                -> False
      in
      maybeRestTypesMatch && Utils.listsEqualBy (\t1 t2 -> equal_ t1 t2 t2IdentToT1Ident) typeList1 typeList2
    (TArrow _ typeList1 _,
     TArrow _ typeList2 _) -> Utils.listsEqualBy (\t1 t2 -> equal_ t1 t2 t2IdentToT1Ident) typeList1 typeList2
    (TUnion _ ((_::_) as typeList1) _,
     TUnion _ ((_::_) as typeList2) _) ->
       (typeList1 |> List.all (\t1 -> typeList2 |> List.any (\t2 -> equal_ t1 t2 t2IdentToT1Ident))) &&
       (typeList2 |> List.all (\t2 -> typeList1 |> List.any (\t1 -> equal_ t1 t2 t2IdentToT1Ident)))
    (TUnion _ ((_::_) as typeList1) _, _) -> typeList1 |> List.all (\t1 -> equal_ t1 t2 t2IdentToT1Ident)
    (_, TUnion _ ((_::_) as typeList2) _) -> typeList2 |> List.all (\t2 -> equal_ t1 t2 t2IdentToT1Ident)
    (TNamed _ ident1,
     TNamed _ ident2)      -> ident1 == ident2
    (TVar _ ident1,
     TVar _ ident2)        -> Dict.get ident2 t2IdentToT1Ident |> Maybe.map ((==) ident1) |> Maybe.withDefault False
    (TWildcard _,
     TWildcard _)          -> True
    (TForall _ (One (_, ident1)) body1 _,
     TForall _ (One (_, ident2)) body2 _) -> equal_ body1 body2 (Dict.insert ident2 ident1 t2IdentToT1Ident)
    (TForall _ (Many _ typeVars1 _) body1 _,
     TForall _ (Many _ typeVars2 _) body2 _) ->
      case Utils.maybeZip (List.map Tuple.second typeVars2) (List.map Tuple.second typeVars1) of -- Ignore preceding whitespace
        Just ident2ident1Pairs -> equal_ body1 body2 (Utils.insertAll ident2ident1Pairs t2IdentToT1Ident)
        _                      -> False

    _ -> False


-- Is t1 a subtype (superset) of t2?
--
-- Not yet smart enough to concretize type variables.
--
-- Requires ASTs to be ordered the same (e.g. Union [Num, String] is not equal to Union [String, Num])
isSubtype : Type -> Type -> Bool
isSubtype t1 t2 =
  let
    t1FreeIdents     = freeIdentifiersList t1 |> Utils.dedup
    t2FreeIdents     = freeIdentifiersList t2 |> Utils.dedup
    t2IdentToT1Ident = Utils.zip t2FreeIdents t1FreeIdents |> Dict.fromList
  in
  isSubtype_ t1 t2 t2IdentToT1Ident


-- Is t1 a subtype (superset) of t2?
--
-- Not yet smart enough to concretize type variables.
--
-- Requires ASTs to be ordered the same (e.g. Union [Num, String] is not equal to Union [String, Num])
isSubtype_ : Type -> Type -> Dict Ident Ident -> Bool
isSubtype_ t1 t2 t2IdentToT1Ident =
  case (t1.val.t__, t2.val.t__) of
    (TList _ listType1 _, _) -> isSubtype_ (replaceT__ t1 (TTuple space1 [] space1 (Just listType1) space1)) t2 t2IdentToT1Ident
    (_, TList _ listType2 _) -> isSubtype_ t1 (replaceT__ t2 (TTuple space1 [] space1 (Just listType2) space1)) t2IdentToT1Ident
    (TNum _, TNum _)       -> True
    (TBool _, TBool _)     -> True
    (TString _, TString _) -> True
    (TNull _, TNull _)     -> True
    (TDict _ keyType1 valueType1 _,
     TDict _ keyType2 valueType2 _) -> isSubtype_ keyType1 keyType2 t2IdentToT1Ident && isSubtype_ valueType1 valueType2 t2IdentToT1Ident
    (TTuple _ typeList1 _ maybeRestType1 _,
     TTuple _ typeList2 _ maybeRestType2 _) ->
      let
        (heads, t1LeftoverHeads, t2LeftoverHeads) = Utils.zipAndLeftovers typeList1 typeList2
      in
      List.all (\(t1, t2) -> isSubtype_ t1 t2 t2IdentToT1Ident) heads &&
      case (t1LeftoverHeads, t2LeftoverHeads, maybeRestType1, maybeRestType2) of
        ([],  [], _, Nothing)   -> True
        (_::_, _, _, Nothing)   -> False
        (_, _::_, Nothing, _)   -> False
        (_, _, Nothing, Just _) -> False
        ([], _::_, Just restType1, Nothing) ->
          List.all (\t2Head -> isSubtype_ restType1 t2Head t2IdentToT1Ident) t2LeftoverHeads
        (_, _, Just restType1, Just restType2) ->
          isSubtype_ restType1 restType2 t2IdentToT1Ident &&
          List.all (\t1Head -> isSubtype_ t1Head restType2 t2IdentToT1Ident) t1LeftoverHeads &&
          List.all (\t2Head -> isSubtype_ restType1 t2Head t2IdentToT1Ident) t2LeftoverHeads
    (TArrow _ typeList1 _,
     TArrow _ typeList2 _) -> Utils.listsEqualBy (\t1 t2 -> isSubtype_ t1 t2 t2IdentToT1Ident) typeList1 typeList2
    (TUnion _ typeList1 _,
     TUnion _ typeList2 _) -> Utils.listsEqualBy (\t1 t2 -> isSubtype_ t1 t2 t2IdentToT1Ident) typeList1 typeList2
    (TNamed _ ident1,
     TNamed _ ident2)      -> ident1 == ident2
    (TVar _ ident1,
     TVar _ ident2)        -> Dict.get ident2 t2IdentToT1Ident |> Maybe.map ((==) ident1) |> Maybe.withDefault False
    (TWildcard _, _)       -> True
    (TForall _ (One (_, ident1)) body1 _,
     TForall _ (One (_, ident2)) body2 _) -> isSubtype_ body1 body2 (Dict.insert ident2 ident1 t2IdentToT1Ident)
    (TForall _ (Many _ typeVars1 _) body1 _,
     TForall _ (Many _ typeVars2 _) body2 _) ->
      case Utils.maybeZip (List.map Tuple.second typeVars2) (List.map Tuple.second typeVars1) of -- Ignore preceding whitespace
        Just ident2ident1Pairs -> isSubtype_ body1 body2 (Utils.insertAll ident2ident1Pairs t2IdentToT1Ident)
        _                      -> False

    _ -> False


-- Lists free type variable identifiers in order, no deduplication.
freeIdentifiersList : Type -> List Ident
freeIdentifiersList tipe =
  case tipe.val.t__ of
    TNum _                                     -> []
    TBool _                                    -> []
    TString _                                  -> []
    TNull _                                    -> []
    TList _ listType _                         -> freeIdentifiersList listType
    TDict _ keyType valueType _                -> freeIdentifiersList keyType ++ freeIdentifiersList valueType
    TTuple _ typeList _ maybeRestType _        -> List.concatMap freeIdentifiersList (typeList ++ Maybe.withDefault [] (Maybe.map List.singleton maybeRestType))
    TArrow _ typeList _                        -> List.concatMap freeIdentifiersList typeList
    TUnion _ typeList _                        -> List.concatMap freeIdentifiersList typeList
    TNamed _ _                                 -> []
    TVar _ ident                               -> [ident]
    TWildcard _                                -> []
    TForall _ (One (_, ident)) body _          -> freeIdentifiersList body |> List.filter ((/=) ident)
    TForall _ (Many _ spaceAndIdents _) body _ -> let (_, idents) = List.unzip spaceAndIdents in freeIdentifiersList body |> List.filter (not << flip List.member idents)


valIsType : Val -> Type -> Bool
valIsType val tipe =
  let unsupported msg =
    Debug.crash <| "typing values against " ++ msg ++ " is not supported"
  in
  case (val.v_, tipe.val.t__) of
    (VConst _ _, TNum _)             -> True
    (VBase (VBool _), TBool _)       -> True
    (VBase (VString _), TString _)   -> True
    (VBase VNull, TNull _)           -> True
    (VList list, TList _ listType _) -> List.all (\v -> valIsType v listType) list
    (_, TDict _ _ _ _)               -> unsupported "dictionary types"
    (VList vlist, TTuple _ typeList _ maybeRestType _) ->
      let typeListsMatch =
        List.foldl
            (\(v, t) res -> res && valIsType v t)
            True
            (Utils.zip vlist typeList)
      in
      case maybeRestType of
        Nothing ->
          typeListsMatch && (List.length vlist == List.length typeList)
        Just restType ->
          typeListsMatch &&
          List.length vlist >= List.length typeList &&
          List.all
              (\v -> valIsType v restType)
              (List.drop (List.length typeList) vlist)
    (_, TArrow _ _ _)        -> unsupported "arrow types"
    (_, TUnion _ typeList _) -> List.any (valIsType val) typeList
    (_, TNamed _ _)          -> unsupported "type aliases"
    (_, TVar _ _)            -> unsupported "type variables"
    (_, TWildcard _)         -> True
    _                        -> False


valToMaybeType : Val -> Maybe Type
valToMaybeType val =
  case val.v_ of
    VConst _ _        -> Just tNum
    VBase (VBool _)   -> Just tBool
    VBase (VString _) -> Just tString
    VBase VNull       -> Just tNull
    VList vs          ->
      case vs |> List.map valToMaybeType |> Utils.projJusts of
        Just []    -> Just (tForall ["a"] (tList (tVar "a")))
        Just types ->
          case Utils.dedup types of
            [t] ->
              -- Special case for points, always!
              if List.length types == 2 && equal tNum t
              then Just (tTuple [tNum, tNum])
              else Just (tList t)
            _ -> Just (tTuple types)
        _ -> Nothing

    _ -> Nothing


matchPatToType : Pat -> Type -> Maybe (List (Ident, Type))
matchPatToType pat tipe =
  case (pat.val.p__, tipe.val.t__) of
    (PVar _ ident _, _) ->
      Just [(ident, tipe)]
    (PList _ pHeads _ Nothing _, TTuple _ tHeads _ Nothing _) ->
      Utils.maybeZip pHeads tHeads
      |> Maybe.andThen (List.map (uncurry matchPatToType) >> Utils.projJusts)
      |> Maybe.map List.concat
    _ ->
      Nothing
