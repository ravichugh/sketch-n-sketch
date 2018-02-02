module DumbTypeInference exposing (..)

import FastParser
import Lang exposing (..)
import LangTools
import Utils

import Dict exposing (Dict)
import Set exposing (Set)


type Type
  = TNum
  | TBool
  | TString
  | TNull
  | TTuple (List Type) (Maybe Type)
  | TArrow Type Type
  | TUnion (List Type)
  | TNamed Ident
  | TVar Ident
  -- | TForall WS (OneOrMany (WS, Ident)) Type WS
  -- | TWildcard
  | TEmpty (List String) -- Type error messages (assumes n >= 1, do not create a TEmpty with [])


tEmptyMessages : Type -> List String
tEmptyMessages tipe =
  case tipe of
    TEmpty messages -> messages
    _               -> []


applyTSubst : Dict Ident Type -> Type -> Type
applyTSubst typeIdentToNewType tipe =
  tipe
  |> mapType
      (\tipe ->
        case tipe of
          TVar ident -> Dict.getWithDefault ident tipe typeIdentToNewType
          _          -> tipe
      )


mapType : (Type -> Type) -> Type -> Type
mapType f tipe =
  let recurse = mapType f in
  case tipe.val of
    TNum                   -> f tipe
    TBool                  -> f tipe
    TString                -> f tipe
    TNull                  -> f tipe
    TTuple heads maybeTail -> f <| TTuple (List.map recurse heads) (Maybe.map recurse maybeTail)
    TArrow lType rType     -> f <| TArrow (recurse lType) (recurse rType)
    TUnion types           -> f <| TUnion (List.map recurse types)
    TNamed _               -> f tipe
    TVar _                 -> f tipe
    TEmpty _               -> f tipe


flattenType : Type -> List Type
flattenType tipe =
  case tipe.val of
    TNum                   -> [tipe]
    TBool                  -> [tipe]
    TString                -> [tipe]
    TNull                  -> [tipe]
    TTuple heads maybeTail -> tipe :: List.concatMap flattenType (heads ++ Utils.maybeToList maybeTail)
    TArrow lType rType     -> tipe :: List.concatMap flattenType [lType, rType]
    TUnion types           -> tipe :: List.concatMap flattenType types
    TNamed _               -> [tipe]
    TVar _                 -> [tipe]
    TEmpty _               -> [tipe]


anyInType : (Type -> Bool) -> Type -> Bool
anyInType pred tipe =
  flattenType tipe |> List.any pred


applyTSubstToDict : Dict Ident Type -> Dict a Type -> Dict a Type
applyTSubstToDict tSubst dict =
  dict
  |> Dict.map (\_ tipe -> applyTSubst tSubst tipe)


mergeTSubsts : Dict Ident Type -> Dict Ident Type
mergeTSubsts update old =
  let
    tSubst = Dict.union update old
    converge n tSubst =
      let newTSubst = applyTSubstToDict tSubst tSubst in
      if newTSubst == tSubst then
        tSubst
      else if n <= 0
        let _ = Debug.log "TSubst didn't converge!!!!" newTSubst in
        newTSubst
      else
        coverge (n-1) newTSubst
  in
  converge 100 tSubst


-- Unifies types within each given list (not between lists).
-- Threads/substitutes the freeTypeVarToNewType dict.
-- Returns (list of unified, freeTypeVarToNewType)
-- Output list will have one result per group (|groups| == |output|).
unifyGroups : List (List Type) -> (List Type, Dict Ident Type)
unifyGroups groups =
  case groups of
    []                  -> ([], Dict.empty)
    [group]             -> unifyAll group
    group1::otherGroups ->
      let
        (otherGroupsUnified, freeTypeVarToNewType) = unifyGroups otherGroups
        (unified, freeTypeVarToNewType2)           = unifyAll (List.map (applyTSubst freeTypeVarToNewType) group1)
        freeTypeVarToNewType3                      = mergeTSubsts freeTypeVarToNewType2 freeTypeVarToNewType
      in
      ( unified :: List.map (applyTSubst freeTypeVarToNewType3) otherGroupsUnified
      , freeTypeVarToNewType3
      )


-- Returns (unified, freeTypeVarToNewType)
unifyAll : List Type -> (Type, Dict Ident Type)
unifyAll types =
  case types of
    []           -> let _ = Utils.log "unifyAll shouldn't be given an empty list of types!!" in TEmpty ["BUG: Empty type list in unifyAll"]
    [tipe]       -> (tipe, Dict.empty)
    tipe::others ->
      let
        (othersUnified, freeTypeVarToNewType) = unifyAll others
        (unified, freeTypeVarToNewType2)      = unify othersUnified (applyTSubst freeTypeVarToNewType tipe)
      in
      ( unified
      , mergeTSubsts freeTypeVarToNewType2 freeTypeVarToNewType
      )

-- Returns (unified, freeTypeVarToNewType)
unify : Type -> Type -> (Type, Dict Ident Type)
unify typeA typeB =
  let
    ret t = (t, Dict.empty)
    typeMismatch () = (TEmpty ["Types don't match: " ++ toString typeA ++ " vs. " ++ toString typeB], Dict.empty)
  in
  case (typeA, typeB) of
    (TEmpty m1, TEmpty m2)-> ret <| TEmpty [m1, m2]
    (TEmpty _, _)         -> ret typeA
    (_, TEmpty _)         -> ret typeB

    (TVar ident, TVar ident2) ->
      if ident == ident2 then
        ret typeA
      else
        ( typeA
        , Dict.singleton ident2 typeA
        )

    (TVar ident, _) -> if anyInType ((==) typeA) typeB then typeMismatch () else (typeB, Dict.singleton ident typeB)
    (_, TVar ident) -> if anyInType ((==) typeB) typeA then typeMismatch () else (typeA, Dict.singleton ident typeA)

    -- Unification on unions is limited.
    -- Unions may be overly narrowed but will not be erronously matched.
    (TUnion aTypes, TUnion bTypes) ->
      case Utils.intersectAsSet aTypes bTypes of
        [] ->
          Utils.cartProd aTypes bTypes
          |> Utils.mapFirstSuccess
              (\(aType, bType) ->
                let (unified, freeTypeVarToNewType) = unify aType bType in
                case unified of
                  TEmpty _ -> Nothing
                  _        -> Just (unified, freeTypeVarToNewType)
              )
          |> Maybe.withDefault (typeMismatch ())

        intersection ->
          ret <| TUnion intersection

    (TUnion aTypes, bType) ->
      aTypes
      |> Utils.mapFirstSuccess
          (\aType ->
            let (unified, freeTypeVarToNewType) = unify aType bType in
            case unified of
              TEmpty _ -> Nothing
              _        -> Just (unified, freeTypeVarToNewType)
          )
      |> Maybe.withDefault (typeMismatch ())

    (_, TUnion _) -> unify typeB typeA

    (TNamed ident, TNamed ident) -> ret typeA
    (TNamed ident, _)            -> typeMismatch () -- Type aliases are opaque for now.
    (_, TNamed ident)            -> typeMismatch () -- Type aliases are opaque for now.

    (TNum,    TNum)    -> ret <| TNum
    (TBool,   TBool)   -> ret <| TBool
    (TString, TString) -> ret <| TString
    (TNull,   TNull)   -> ret <| TNull

    -- Explicit to let exhaustiveness checker help us ensure we didn't miss anything.
    (TNum,    _) -> typeMismatch ()
    (TBool,   _) -> typeMismatch ()
    (TString, _) -> typeMismatch ()
    (TNull,   _) -> typeMismatch ()

    ( TTuple aHeads Nothing
    , TTuple bHeads Nothing ) ->
      case Utils.maybeZip aHeads bHeads of
        Just headPairs ->
          let (headUnifications, freeTypeVarToNewType) = unifyGroups (List.map pairToList headPairs) in
          ( guardTEmpties headUnifications (TTuple headUnifications Nothing)
          , freeTypeVarToNewType
          )
        Nothing -> typeMismatch ()

    ( TTuple aHeads Nothing
    , TTuple bHeads (Just bTail) ) ->
      case Utils.zipAndLeftovers aHeads bHeads of
        (_, _, _::_)                      -> typeMismatch ()
        (headsMatched, leftoverAHeads, _) ->
          let
            unificationGroups = (headsMatched ++ List.map ((,) bTail) leftoverAHeads) |> List.map pairToList
            (headUnifications, freeTypeVarToNewType) = unifyGroups unificationGroups
          in
          ( guardTEmpties headUnifications (TTuple headUnifications Nothing)
          , freeTypeVarToNewType
          )

    ( TTuple _ (Just _)
    , TTuple _ Nothing ) ->
      unify typeB typeA

    ( TTuple aHead (Just aTail)
    , TTuple bHead (Just bTail) ) ->
      case Utils.zipAndLeftovers aHeads bHeads of
        let
          (headsMatched, leftoverAHeads, leftoverBHeads) = Utils.zipAndLeftovers aHeads bHeads
          unificationGroups =
            ( (aTail, bTail) ++
              headsMatched ++
              List.map ((,) bTail) leftoverAHeads ++
              List.map ((,) aTail) leftoverBHeads
            ) |> List.map pairToList
          (unifications, freeTypeVarToNewType) = unifyGroups unificationGroups
          (tailUnification, headUnifications)  = Utils.uncons "DumbTypeInference.unify" unifications
        in
        ( guardTEmpties unifications (TTuple headUnifications (Just tailUnification))
        , freeTypeVarToNewType
        )

    (TTuple _ _, _) -> typeMismatch ()

    ( TArrow aLType aRType
    , TArrow bLType bRType ) ->
      let
        (unifications, freeTypeVarToNewType) = unifyGroups [[aLType, bLType], [aRType, bRType]]
        (lUnified, rUnified)                 = Utils.listToPair "DumbTypeInference.unify" unifications
      in
      ( guardTEmpties unifications (TArrow lUnified rUnified)
      , freeTypeVarToNewType
      )

    (TArrow _ _, _) -> typeMismatch ()


unifyAllFreeVarTypes : List (Type, Dict Ident Type) -> (List Type, Dict Ident Type)
unifyAllFreeVarTypes typesWithFreeVarTypes =
  let
    (types, freeVarTypeDicts)                  = List.unzip typesWithFreeVarTypes
    (freeTypeVarToNewType, identToUnifiedType) = unifyFreeVarDicts freeVarTypeDicts
  in
  ( types |> List.map (applyTSubst freeTypeVarToNewType)
  , identToUnifiedType
  )


unifyFreeVarDicts : List (Dict Ident Type) -> (Dict Ident Type, Dict Ident Type)
unifyFreeVarDicts freeVarTypeDicts =
  let
    identToVariousTypes =
      freeVarTypeDicts
      |> List.concatMap Dict.toList
      |> Utils.pairsToDictOfLists

    (identToUnifiedTypeRaw, freeTypeVarToNewType) =
      identToVariousTypes
      |> List.foldl
          (\ident types (identToUnifiedTypeRaw, freeTypeVarToNewType) ->
            let
              typesToUnify                     = types |> List.map (applyTSubst freeTypeVarToNewType)
              (unified, freeTypeVarToNewType2) = unifyAll typesToUnify
              freeTypeVarToNewType3            = mergeTSubsts freeTypeVarToNewType2 freeTypeVarToNewType
            in
            ( identToUnifiedTypeRaw |> Dict.insert ident unified
            , freeTypeVarToNewType3
            )
          )
          (Dict.empty, Dict.empty)

    identToUnifiedType = applyTSubstToDict freeTypeVarToNewType identToUnifiedTypeRaw
  in
  ( freeTypeVarToNewType
  , identToUnifiedType
  )


unifyFreeVarTypes
  :  (Type,       Dict Ident Type)
  -> (Type,       Dict Ident Type)
  -> (Type, Type, Dict Ident Type)
unifyFreeVarTypes (lType, lFreeVarTypes) (rType, rFreeVarTypes) =
  case unifyAllFreeVarTypes [(lType, lFreeVarTypes), (rType, rFreeVarTypes)] of
    ([lTypeUpdate, rTypeUpdated], freeVarsUnified) -> (lTypeUpdate, rTypeUpdated, freeVarsUnified)
    _                                              -> Debug.crash "unifyAllFreeVarTypes didn't satisfy its shape invariant!"

--   let
--     (identToUnifiedType, freeTypeVarToNewType) = unifyFreeVarTypes_ lFreeVarTypes rFreeVarTypes
--   in
--   ( applyTSubst freeTypeVarToNewType rType
--   , applyTSubst freeTypeVarToNewType lType
--   , Dict.union
--       identToUnifiedType
--       ( Dict.union
--           (applyTSubstToDict freeTypeVarToNewType lFreeVarTypes)
--           (applyTSubstToDict freeTypeVarToNewType rFreeVarTypes)
--       )
--   )
--
-- unifyFreeVarTypes_
--   :  Dict Ident Type
--   -> Dict Ident Type
--   -> (Dict Ident Type, Dict Ident Type)
-- unifyFreeVarTypes_ lFreeVarTypes rFreeVarTypes =
--   let
--     identToLeftRightTypes = Utils.dictOverlaps lFreeVarTypes rFreeVarTypes
--     (identToUnifiedType, freeTypeVarToNewType) =
--       identToLeftRightTypes
--       |> Dict.foldl
--           (\ident (lFreeType, rFreeType) (identToUnifiedType, freeTypeVarToNewType) ->
--             let
--               (updatedLType, updatedRType)     = (applyTSubst freeTypeVarToNewType lFreeType, applyTSubst freeTypeVarToNewType rFreeType)
--               (unified, freeTypeVarToNewType2) = unify updatedLType updatedRType
--               freeTypeVarToNewType3            = mergeTSubsts freeTypeVarToNewType2 freeTypeVarToNewType
--             in
--             ( applyTSubstToDict freeTypeVarToNewType3 identToUnifiedType |> Dict.insert ident unified
--             , freeTypeVarToNewType3
--             )
--           )
--           (Dict.empty, Dict.empty)
--   in
--   (identToUnifiedType, freeTypeVarToNewType)


-- If left type is non-empty, return right type.
guardTEmpty : Type -> Type -> Type
guardTEmpty guard tipe =
  case guard of
    TEmpty _ -> guard
    _        -> tipe


guardTEmpties : List Type -> Type -> Type
guardTEmpties guards tipe =
  case List.concatMap tEmptyMessages guards of
    []       -> tipe
    messages -> TEmpty messages


-- Bottom-up only.
-- BUG: Need to push type information back down b/c not all successful unifications are with type vars
-- so variable type information can't just be represented by adding free type vars.
-- Return (exp type, free var to type)
synthesizeType : Exp -> (Type, Dict Ident Type)
synthesizeType exp =
  case exp.val.e__ of
    EBase _ (EBool _)     -> (TBool,   Dict.empty)
    EBase _ (EString _ _) -> (TString, Dict.empty)
    EBase _ ENull         -> (TNull,   Dict.empty)
    EConst _ _ _ _        -> (TNum,    Dict.empty)
    EVar _ ident          ->
      let
        tVarName = "a" ++ toString exp.val.eid
        tipe     = TVar tVarName
      in
      ( tipe
      , Dict.singleton ident tipe
      )

    EFun _ argPats fBody _ ->
      let
        (bodyType, freeVarTypes) = synthesizeType fBody
        (patTypesUnified, freeVarTypesFinal) =
          argPats
          |> List.map synthesizePatType
          |> List.foldl
              (\(patType, patFreeVarTypes, patFreeTypeVars) (patTypesUnified, freeVarTypes, freeTypeVars) ->
                ...
              )
              ([], freeVarTypes)
      in
      ...

    EApp _ fExp [] _ -> -- Shouldn't happen.
      synthesizeType fExp

    EApp _ fExp [argExp] _ ->
      let
        (fSimpleType, argSimpleType, freeVarTypes) = unifyFreeVarTypes (synthesizeType fExp) (synthesizeType argExp)
        tVarName = "a" ++ toString exp.val.eid
        tVar     = TVar tVarName
        (fUnified, freeTypeVarToNewType) = unify (TArrow argSimpleType tVar) fSimpleType
      in
      ( guardTEmpty fUnified (Utils.getWithDefault tVarName tVar freeTypeVarToNewType)
      , applyTSubstToDict freeTypeVarToNewType freeVarTypes
      )

    EApp _ fExp argExps _ ->
      Debug.crash "DumpTypeInference.synthesizeType: Expected EApp to be desugared already!"

    EList _ heads _ Nothing _ ->
      let (headTypes, freeVarTypes) = unifyAllFreeVarTypes (List.map synthesizeType heads) in
      ( TTuple headTypes Nothing
      , freeVarTypes
      )

    EList _ heads _ (Just tail) _ ->
      case unifyAllFreeVarTypes (List.map synthesizeType (tail::heads)) of
        (tailType::headTypes, freeVarTypes) ->
          ( TTuple headTypes (Just tailType)
          , freeVarTypes
          )

        _ -> Debug.crash "unifyAllFreeVarTypes failed to honor its structural invariant!"

    EOp _ op operands _          ->
      let (operandTypes, freeVarTypes) = unifyAllFreeVarTypes (List.map synthesizeType operands) in
      case (op.val, operandTypes) of
        (Pi,         [])       -> (TNum, freeVarTypes)
        (ToStr,      [_])      -> (TString, freeVarTypes)
        (DebugLog,   [t1])     -> (t1, freeVarTypes)
        (Eq,         [_, _])   -> (TBool, freeVarTypes) -- (a -> b -> Bool), see Eval.eval
        (Cos,        [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Sin,        [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (ArcCos,     [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (ArcSin,     [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (ArcTan2,    [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TNum, t1, t2] in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Floor,      [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Ceil,       [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Round,      [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Sqrt,       [t1])     -> let (unified, freeTypeVarToNewType) = unify    TNum t1        in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Plus,       [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TUnion (TNum, TString), t1, t2] in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes) Num
        (Minus,      [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TNum, t1, t2] in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Mult,       [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TNum, t1, t2] in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Div,        [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TNum, t1, t2] in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Lt,         [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TNum, t1, t2] in (guardTEmpty unified TBool, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Mod,        [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TNum, t1, t2] in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (Pow,        [t1, t2]) -> let (unified, freeTypeVarToNewType) = unifyAll [TNum, t1, t2] in (unified, applyTSubstToDict freeTypeVarToNewType freeVarTypes)
        (NoWidgets,  [t1])     -> (t1, freeVarTypes)
        (Explode,    [t1])     -> let (unified, freeTypeVarToNewType) = unify    TString t1     in (guardTEmpty unified (TTuple [] (Just TString)), applyTSubstToDict freeTypeVarToNewType freeVarTypes) -- (String -> List String)
        _                      -> (TEmpty ["Bad operation"], freeVarTypes)

    EIf _ condExp thenExp elseExp _ ->
      ...
      eidIs (expToTC thenExp) ++ eidIs (expToTC elseExp) ++ [EIdIsType condExp.val.eid TBool]
    ELet _ _ _ pat boundExp letBody _ ->
      -- tryMatchExpPatToPIds : Pat -> Exp -> List (PId, Exp)
      -- let
      --   pidToExp           = LangTools.tryMatchExpPatToPIds pat boundExp
      --   (matchedPIds, _)   = List.unzip pidToExp
      --   unmatchedPIds      = Utils.diffAsSet (allPIds pat) matchedPIds
      --   matchedConstraints = pidToExp |> List.map (\(pid, boundExp) -> PIdIsEId pid boundExp.val.eid)
      --   unmatchedErrors    = unmatchedPIds |> List.map (\pid -> PIdIsEmpty pid "PId didn't match in let exp")
      -- in
      gatherPatConstraints pat ++
      [PIdIsEId pat.val.pid boundExp.val.eid] ++
      eidIs (expToTC letBody)
    ECase _ scrutinee bs _ ->
      gatherPatsConstraints (branchPats bs) ++
      (branchPats bs |> List.map (\bPat -> PIdIsEId bPat.val.pid scrutinee.val.eid)) ++
      (branchExps bs |> List.concatMap (eidIs << expToTC))
    ETypeCase _ scrutinee bs _ ->
      [EIdIsType scrutinee.val.eid <| TUnion (tbranchTypes bs |> List.map typeToTC)] ++
      (tbranchExps bs |> List.concatMap (eidIs << expToTC))
    EComment _ _ e1     -> eidIs (expToTC e1)
    EOption _ _ _ _ e1  -> eidIs (expToTC e1)
    ETyp _ pat tipe e _ ->
      gatherPatConstraints pat ++
      [PIdIsType pat.val.pid (typeToTC tipe)] ++
      eidIs (expToTC e)
    EColonType _ e _ tipe _ ->
      eidIs (typeToTC tipe) ++
      eidIs (expToTC e)
    ETypeAlias _ pat tipe e _ ->
      let aliasConstraints =
        case Types.matchTypeAlias pat tipe of
          Just identToType -> identToType |> List.map (\(ident, tipe) -> TypeAlias ident (typeToTC tipe))
          Nothing          -> let _ = Debug.log "Could not match type alias" pat in [PIdIsEmpty pat.val.pid "Type alias malformed"]
      in
      aliasConstraints ++
      eidIs (expToTC e)
    EParens _ e _ -> eidIs (expToTC e)
    EHole _ _     -> []


typecheck : Exp -> TypeDict
typecheck program =
  Dict.empty


maybeTypes : Id -> TypeDict -> List Type
maybeTypes id dict =
  []

