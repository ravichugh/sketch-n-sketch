module AlgorithmJish exposing (TypingContext, IdToTypeAndContextThunk, inferTypes, inferOne, doesUnify, preludeTypeContext)

import FastParser
import ImpureGoodies
import Lang exposing (..)
import Syntax
import Types exposing (tBool, tNum, tString, tNull, tVar, tTupleRest, tTuple, tList, tDict, tArrows, tArrow, tForall)
import Utils

import Dict exposing (Dict)
import Set exposing (Set)



unparse : Type -> String
unparse t = Syntax.typeWithRolesUnparser Syntax.Elm t

unparseExp : Exp -> String
unparseExp e = Syntax.unparser Syntax.Elm e


-- Algorithm J(-ish)
--
-- Yet another attempt at type inference.
--
-- Mostly following https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_J
--
--      x : σ ∈ Γ    τ = inst(σ)
-- Var --------------------------
--           Γ ⊢ x : τ
--
--
--      Γ ⊢ e0 : τ0    Γ ⊢ e1 : τ1    τ2 = newvar    unify(τ0, τ1 → τ2)
-- App ------------------------------------------------------------------
--                            Γ ⊢ e0 e1 : τ2
--
--
--      τ = newvar    Γ, x : τ ⊢ e : τ'
-- Abs ---------------------------------
--            Γ ⊢ λx. e : τ → τ'
--
--
--      Γ ⊢ e0 : τ    Γ, x : Γ'(τ) ⊢ e1 : τ'
-- Let --------------------------------------
--           Γ ⊢ let x = e0 in e1 : τ'
--
--
--  Γ'(τ) = ∀ᾱ. τ
--  Γ'(τ) "quantifies all monotype variables [in τ] not bound in Γ"
--

type alias TypingContext = List (Ident, Type)
type alias IdToTypeAndContext = Dict Int (Type, TypingContext)
type alias IdToTypeAndContextThunk = Dict Int (Type, (() -> TypingContext))

type alias EquivalenceClass =
  { representative : Type -- Roles are maintained on this member, so be sure to mind their preservation.
  , members : Set Ident
  }


-- All fields mutable.
type alias TypesHeap =
  { nextVarId : Int
  , equivalenceClasses : Dict Ident EquivalenceClass -- Type var to equivalence class
  , idToTypeAndContext : IdToTypeAndContext -- EId/PId to type and typing context. Collected (write-only) during inference and not finalized, at least while it is in the TypesHeap (TVars need to be looked up in equivalenceClasses).
  , errors : List String -- For now (to measure success or not).
  }


-- "The procedure inst(σ) specializes the polytype σ by copying the term and
-- replacing the bound type variables consistently by new monotype variables."
inst : TypesHeap -> Type -> Type
inst typesHeap t =
  inst_ typesHeap Dict.empty t


inst_ : TypesHeap -> Dict Ident Type -> Type -> Type
inst_ typesHeap subst t =
  let recurse = inst_ typesHeap subst in
  let wrap t__ = replaceT__ t t__ in
  addRoles t.val.roles <|
  case t.val.t__ of
    TForall _ _ t1 _ ->
      let
        boundIdents = tForallIdents t

        newSubst =
          boundIdents
          |> Utils.foldl
              subst
              (\boundIdent newSubst ->
                Dict.insert boundIdent (newVar typesHeap) newSubst
              )
      in
      inst_ typesHeap newSubst t1

    TVar _ ident             -> Utils.getWithDefault ident t subst
    TList ws1 t1 ws2         -> wrap <| TList ws1 (recurse t1) ws2
    TDict ws1 t1 t2 ws2      -> wrap <| TDict ws1 (recurse t1) (recurse t2) ws2
    TArrow ws1 ts ws2        -> wrap <| TArrow ws1 (List.map recurse ts) ws2
    TUnion ws1 ts ws2        -> newVar typesHeap -- wrap <| TUnion ws1 (List.map recurse ts) ws2
    TTuple ws1 ts ws2 mt ws3 -> wrap <| TTuple ws1 (List.map recurse ts) ws2 (Maybe.map recurse mt) ws3

    TNum _      -> t
    TBool _     -> t
    TString _   -> t
    TNull _     -> t
    TNamed _ _  -> t
    TWildcard _ -> t



-- Mutates typesHeap to generate a fresh type variable.
newVar : TypesHeap -> Type
newVar typesHeap =
  let varId = typesHeap.nextVarId in
  let _ = ImpureGoodies.mutateRecordField typesHeap "nextVarId" (varId + 1) in
  tVar ("a" ++ toString varId)


--  Γ'(τ) = ∀ᾱ. τ
--  Γ'(τ) "quantifies all monotype variables [in τ] not bound in Γ"
quantifyUnboundVars : TypingContext -> Type -> Type
quantifyUnboundVars context t =
  let
    unboundIdentifiers =
      Types.freeIdentifiersList t
      |> Utils.dedup
      |> List.filter (\ident -> Utils.maybeFind ident context == Nothing)
  in
  if unboundIdentifiers == []
  then t
  else tForall unboundIdentifiers t


-- Order is not comparable for some reason.
compareOrder : Order -> Order -> Order
compareOrder a b =
  let orderToInt order =
    case order of
      LT -> -1
      EQ -> 0
      GT -> 1
  in
  compare (orderToInt a) (orderToInt b)


-- What type is preferred as the representative when unifying? (Lower == more preferred)
typeCompare : Type -> Type -> Order
typeCompare ta tb =
  let andThenCompare order2Thunk order1 =
    if order1 == EQ
    then order2Thunk ()
    else order1
  in
  case (ta.val.t__, tb.val.t__) of
    (TVar _ ident1, TVar _ ident2)                 -> compare ident1 ident2
    (_, TVar _ _)                                  -> LT
    (TVar _ _, _)                                  -> GT
    (TForall _ _ ta_ _, _)                         -> let _ = Utils.log "Warning: TForall in typeCompare" in typeCompare ta_ tb
    (_, TForall _ _ tb_ _)                         -> let _ = Utils.log "Warning: TForall in typeCompare" in typeCompare ta tb_
    (TNamed _ name1, TNamed _ name2)               -> let _ = Utils.log "Warning: TNamed in typeCompare" in compare name1 name2
    (_, TNamed _ _)                                -> let _ = Utils.log "Warning: TNamed in typeCompare" in LT
    (TNamed _ _, _)                                -> let _ = Utils.log "Warning: TNamed in typeCompare" in GT
    (TList _ ta_ _, TList _ tb_ _)                 -> typeCompare ta_ tb_
    (TTuple _ _ _ _ _, TList _ _ _)                -> LT
    (TList _ _ _, TTuple _ _ _ __ )                -> GT
    (TTuple _ _ _ Nothing _
    ,TTuple _ _ _ (Just _) _)                      -> LT -- Any tuple without a tail is more restrictive.
    (TTuple _ _ _ (Just _) _
    ,TTuple _ _ _ Nothing _)                       -> GT -- Any tuple without a tail is more restrictive.
    (TTuple _ [] _ (Just taRest) _
    ,TTuple _ [] _ (Just tbRest) _)                -> typeCompare taRest tbRest
    (TTuple _ (ta1::taHeadRest) _ taMaybeRest _
    ,TTuple _ (tb1::tbHeadRest) _ tbMaybeRest _)   ->
      case typeCompare ta1 tb1 of
        LT -> LT
        EQ -> typeCompare (tTupleRest taHeadRest taMaybeRest) (tTupleRest tbHeadRest tbMaybeRest)
        GT -> GT
    (TDict _ ta1 ta2 _, TDict _ tb1 tb2 _)         -> typeCompare ta2 tb2 |> andThenCompare (\() -> typeCompare ta1 tb1)
    (TArrow _ [ta1, ta2] _, TArrow _ [tb1, tb2] _) -> typeCompare ta2 tb2 |> andThenCompare (\() -> typeCompare ta1 tb1)
    (TArrow _ (ta1::ta2::ta3::taRest) _, _)        -> typeCompare (tArrow ta1 (tArrows (ta2::ta3::taRest))) tb
    (_, TArrow _ (tb1::tb2::tb3::tbRest) _)        -> typeCompare ta (tArrow tb1 (tArrows (tb2::tb3::tbRest)))
    _                                              -> EQ -- or incomparable


newVarEquivalenceClass : Type -> EquivalenceClass
newVarEquivalenceClass tVar =
  { representative = tVar
  , members        = Set.singleton (tVarToIdent tVar)
  }


find : TypesHeap -> Type -> EquivalenceClass
find typesHeap t =
  case t.val.t__ of
    TForall _ _ _ _ ->
      let _ = Utils.log <| "Internal error: Should never try to find or union a polytype! " ++ unparse t in
      newVarEquivalenceClass (newVar typesHeap)

    TVar _ ident ->
      Dict.get ident typesHeap.equivalenceClasses
      |> Maybe.withDefault (newVarEquivalenceClass t)

    _ ->
      { representative = t
      , members        = Set.empty
      }


findRepresentativesAndShareRoles : TypesHeap -> Type -> Type -> (Type, Type)
findRepresentativesAndShareRoles typesHeap t0 t1 =
  let
    aClass = find typesHeap t0
    bClass = find typesHeap t1

    roles = Set.union aClass.representative.val.roles bClass.representative.val.roles

    _ = addRolesToClass typesHeap roles t0
    _ = addRolesToClass typesHeap roles t1
  in
  ( (find typesHeap t0).representative
  , (find typesHeap t1).representative
  )


addRolesToClass : TypesHeap -> Set Ident -> Type -> ()
addRolesToClass typesHeap roles t =
  let
    class = find typesHeap t

    tWithRoles = addRoles roles class.representative

    newClass = { class | representative = tWithRoles }

    newEquivalenceClasses =
      newClass.members
      |> Set.toList
      |> Utils.foldl
          typesHeap.equivalenceClasses
          (\memberIdent newEquivalenceClasses -> Dict.insert memberIdent newClass newEquivalenceClasses)

    _ = ImpureGoodies.mutateRecordField typesHeap "equivalenceClasses" newEquivalenceClasses
  in
  ()


tVarToIdent : Type -> Ident
tVarToIdent t =
  case t.val.t__ of
    TVar _ ident -> ident
    _            -> Debug.crash <| "tVarToIdent was not given a TVar!! " ++ unparse t


tForallIdents : Type -> List Ident
tForallIdents t =
  case t.val.t__ of
    TForall _ (One (_, ident)) _ _    -> [ident]
    TForall _ (Many _ wsIdents _) _ _ -> List.map Tuple.second wsIdents
    _                                 -> Debug.crash <| "tForallIdents was not given a TForall!! " ++ unparse t


-- Mutably affect the union between the type variable and the other type.
-- O(something)
union : TypesHeap -> Type -> Type -> ()
union typesHeap t1 t2 =
  let
    class1 : EquivalenceClass
    class1 = find typesHeap t1

    class2 : EquivalenceClass
    class2 = find typesHeap t2

    newRepresentative =
      Utils.minimumByComparator typeCompare [class1.representative, class2.representative]
      |> Utils.fromJust_ "AlgorithmJish.union should never hit here"
      |> addRoles class1.representative.val.roles
      |> addRoles class2.representative.val.roles

    newClass =
      { representative = newRepresentative
      , members        = Set.union class1.members class2.members
      }

    newEquivalenceClasses =
      newClass.members
      |> Set.toList
      |> Utils.foldl
          typesHeap.equivalenceClasses
          (\memberIdent newEquivalenceClasses -> Dict.insert memberIdent newClass newEquivalenceClasses)

    _ = ImpureGoodies.mutateRecordField typesHeap "equivalenceClasses" newEquivalenceClasses
  in
  ()


-- Mutates TypesHeap to affect the unification.
unify : TypesHeap -> Type -> Type -> ()
unify typesHeap t0 t1 =
  let
    (ta, tb) = findRepresentativesAndShareRoles typesHeap t0 t1

    doesOccurIn : Ident -> Type -> Bool
    doesOccurIn ident t =
      finalizeType typesHeap.equivalenceClasses t
      |> Types.freeIdentifiersList
      |> List.any ((==) ident)

    fail errMsg =
      let
        fullMsg = "Could not unify " ++ unparse t0 ++ " with " ++ unparse t1 ++ " — " ++ errMsg
        _ = Utils.log fullMsg
        _ = ImpureGoodies.mutateRecordField typesHeap "errors" (fullMsg :: typesHeap.errors)
      in
      ()
  in
  case (ta.val.t__, tb.val.t__) of
    (TForall _ _ ta_ _, _)                         -> fail "Internal error: should never be unifying a polytype"
    (_, TForall _ _ tb_ _)                         -> fail "Internal error: should never be unifying a polytype"
    (TVar _ aIdent, TVar _ bIdent)                 -> union typesHeap ta tb
    (_, TVar _ ident)                              -> if doesOccurIn ident ta then fail "occurs check failed" else union typesHeap t0 t1
    (TVar _ ident, _)                              -> if doesOccurIn ident tb then fail "occurs check failed" else union typesHeap t0 t1
    -- (TNamed _ name1, TNamed _ name2)               -> if name1 == name2 then () else fail "type alias names don't match (we're a little dumb right now)"
    -- (_, TNamed _ _)                                -> LT -- Hrm.
    -- (TNamed _ _, _)                                -> GT -- Hrm.
    (TList _ ta_ _, TList _ tb_ _)                 -> unify typesHeap ta_ tb_
    (TTuple _ ta1s _ taMaybeRest _, TList _ tb_ _) -> let _ = ta1s ++ Utils.maybeToList taMaybeRest |> List.map (unify typesHeap tb_) in ()
    (TList _ ta_ _, TTuple _ tb1s _ tbMaybeRest _) -> let _ = tb1s ++ Utils.maybeToList tbMaybeRest |> List.map (unify typesHeap ta_) in ()
    (TTuple _ ta1s _ taMaybeRest _
    ,TTuple _ tb1s _ tbMaybeRest _) ->
      let (headPairs, aLeftovers, bLeftovers) = Utils.zipAndLeftovers ta1s tb1s in
      let _ = List.map (uncurry (unify typesHeap)) headPairs in
      let _ =
        case (taMaybeRest, tbMaybeRest) of
          (Just taRest, Just tbRest) -> unify typesHeap taRest tbRest
          _                          -> ()
      in
      -- If heads match but one tuple has a tail but the other doesn't, there's no way to propogate the information
      -- about a missing tail. So, heh.
      case (aLeftovers, taMaybeRest, bLeftovers, tbMaybeRest) of
        ([], _, [], _)          -> ()
        ([], Just taRest, _, _) -> let _ = List.map (unify typesHeap taRest) bLeftovers in ()
        (_, _, [], Just tbRest) -> let _ = List.map (unify typesHeap tbRest) aLeftovers in ()
        _                       -> fail "Tuple shapes don't match"
    (TDict _ ta1 ta2 _, TDict _ tb1 tb2 _)         -> let _ = unify typesHeap ta1 tb1 in unify typesHeap ta2 tb2
    (TArrow _ [ta1, ta2] _, TArrow _ [tb1, tb2] _) -> let _ = unify typesHeap ta1 tb1 in unify typesHeap ta2 tb2
    (TArrow _ (ta1::ta2::ta3::taRest) _, _)        -> unify typesHeap (tArrow ta1 (tArrows (ta2::ta3::taRest))) t1
    (_, TArrow _ (tb1::tb2::tb3::tbRest) _)        -> unify typesHeap t0 (tArrow tb1 (tArrows (tb2::tb3::tbRest)))
    (TNum _, TNum _)                               -> ()
    (TBool _, TBool _)                             -> ()
    (TString _, TString _)                         -> ()
    (TNull _, TNull _)                             -> ()
    _                                              -> if ta.val.t__ == tb.val.t__ then () else fail "mismatched"


resolveTypeAliases : Dict Ident Type -> Type -> Type
resolveTypeAliases typeAliasToType tipe =
  let recurse = resolveTypeAliases typeAliasToType in
  tipe
  |> mapType
      (\t ->
        case t.val.t__ of
          TNamed _ ident ->
            case Dict.get ident typeAliasToType of
              Just replacement ->
                recurse replacement -- Please don't make recursive type aliases or this will not terminate.
                |> addRoles (Set.insert ident t.val.roles) -- TNamed role should have been inserted by parser but just in case...

              Nothing ->
                let _ = Utils.log <| "resolveTypeAliases: Type alias not found! " ++ ident in
                tForall ["a"] (tVar "a" |> addRoles (Set.insert ident t.val.roles))

          _ ->
            t
      )


-- For Prelude, we just use the existing type annotations.
preludeTypeContext : TypingContext
preludeTypeContext =
  topLevelTypeContextAnnotationsOnly Dict.empty [] FastParser.prelude
  -- |> List.map
  --     (\(ident, tipe) ->
  --       let _ = Debug.log ident (Syntax.typeWithRolesUnparser Syntax.Elm tipe) in
  --       (ident, tipe)
  --     )


topLevelTypeContextAnnotationsOnly : Dict Ident Type -> TypingContext -> Exp -> TypingContext
topLevelTypeContextAnnotationsOnly typeAliasToType context e =
  -- c.f. LangTools.maybeSameLevelChild
  let recurse = topLevelTypeContextAnnotationsOnly typeAliasToType context in
  case e.val.e__ of
    ETyp _ pat tipe body _ ->
      case Types.matchPatToType pat tipe of
        Just identToType ->
          let moreContext =
            -- Resolve type aliases and add any missing Foralls.
            identToType
            |> List.map (Tuple.mapSecond (resolveTypeAliases typeAliasToType >> quantifyUnboundVars []))
          in
          topLevelTypeContextAnnotationsOnly typeAliasToType (moreContext ++ context) body

        Nothing ->
          let _ = Debug.log "Could not match type annotation" (pat, tipe) in
          recurse body

    ETypeAlias _ pat tipe body _ ->
      case Types.matchPatToType pat tipe of
        Just identToType ->
          let newTypeAliasToType = Dict.union (Dict.fromList identToType) typeAliasToType in
          topLevelTypeContextAnnotationsOnly newTypeAliasToType context body

        Nothing ->
          let _ = Debug.log "Could not match type alias" (pat, tipe) in
          recurse body

    EColonType _ body _ _ _   -> recurse body -- Annotation ignored.
    ELet _ _ _ _ _ _ _ body _ -> recurse body -- Assume all vars in the let were annotated
    EComment _ _ e            -> recurse e
    EOption _ _ _ _ e         -> recurse e
    _                         -> context


-- Very optimistic, but does log errors in typesHeap.errors.
--
-- Precondition: All ids in program are unique (FastParser.freshen).
--
-- The returned dictionary does not have the Foralls around let-bound identifiers.
-- This may be okay for our purposes.
inferTypes : Exp -> IdToTypeAndContextThunk
inferTypes program =
  let
    typesHeap = { nextVarId = 1, equivalenceClasses = Dict.empty, idToTypeAndContext = Dict.empty, errors = [] }
    _ = infer typesHeap preludeTypeContext program
  in
  -- Finalize idToTypeAndContext
  -- (Map all type vars in a type to the variable class's representative; also simplifies some TTuples to TLists)
  typesHeap.idToTypeAndContext
  |> Dict.map
      (\_ (dumbType, context) ->
        ( finalizeType typesHeap.equivalenceClasses dumbType
        , (\() -> context |> List.map (Tuple.mapSecond (finalizeType typesHeap.equivalenceClasses))) -- Finalizing types in the context is slow—because we do it for every id—so defer it so we only do it where necessary.
        )
      )


-- E.g. to discover the type of an expression about to be inserted into a program.
inferOne : TypingContext -> Exp -> Type
inferOne context e =
  let
    maxId =
      context
      |> List.concatMap (Tuple.second >> Types.freeIdentifiersList)
      |> Utils.dedup
      |> List.map
          (\ident ->
            case String.uncons ident of
              Just ('a', rest) ->
                String.toInt rest
                |> Utils.fromOkWithDefault 0

              _ ->
                0
          )
      |> List.maximum
      |> Maybe.withDefault 0

    typesHeap = { nextVarId = maxId + 1, equivalenceClasses = Dict.empty, idToTypeAndContext = Dict.empty, errors = [] }
    dumbType = infer typesHeap context e
  in
  finalizeType typesHeap.equivalenceClasses dumbType


-- Do the two types unify (assuming universal quantification around each for each's type variables)?
doesUnify : Type -> Type -> Bool
doesUnify t1 t2 =
  let
    typesHeap = { nextVarId = 1, equivalenceClasses = Dict.empty, idToTypeAndContext = Dict.empty, errors = [] }

    -- Ensure no var name collisions between incoming types (by renaming all vars).
    t1Reinstantiated = inst typesHeap (quantifyUnboundVars [] t1)
    t2Reinstantiated = inst typesHeap (quantifyUnboundVars [] t2)

    _ = unify typesHeap t1Reinstantiated t2Reinstantiated
  in
  typesHeap.errors == []


-- Map all type vars in a type to the variable class's representative
-- And simplify any tailed tuples of all the same type
finalizeType : Dict Ident EquivalenceClass -> Type -> Type
finalizeType equivalenceClasses dumbType =
  finalizeType_ [] equivalenceClasses dumbType


finalizeType_ : List Ident -> Dict Ident EquivalenceClass -> Type -> Type
finalizeType_ referencesFollowed equivalenceClasses dumbType =
  let recurse = finalizeType equivalenceClasses in
  let wrap t__ = replaceT__ dumbType t__ in
  case dumbType.val.t__ of
    TForall ws1 vars t1 ws2 ->
      -- Load these new variables into the context as variables to potentially shadowing any existing entries in the context.
      let moreEquivalenceClasses =
        tForallIdents dumbType
        |> List.map (\ident -> (ident, { representative = tVar ident, members = Set.singleton ident })) -- Members will be unused here.
        |> Dict.fromList
      in
      wrap (TForall ws1 vars (finalizeType (Dict.union moreEquivalenceClasses equivalenceClasses) t1) ws2)

    TVar _ ident ->
      case Dict.get ident equivalenceClasses of
        Just { representative } ->
          addRoles dumbType.val.roles <|
          if List.member ident referencesFollowed then
            -- Probably not quite the right handling of recursive types (which we shouldn't have unless there's a type error in the program) but this will keep us from freezing.
            representative
          else
            finalizeType_ (ident::referencesFollowed) equivalenceClasses representative

        Nothing ->
          dumbType

    TNum _         -> dumbType
    TBool _        -> dumbType
    TString _      -> dumbType
    TNull _        -> dumbType
    TNamed _ ident -> let _ = Utils.log <| "Encountered a type alias in finalizeType, but should not have!! " ++ ident in dumbType
    TWildcard _    -> dumbType

    TList ws1 t1 ws2         -> wrap (TList ws1 (recurse t1) ws2)
    TDict ws1 t1 t2 ws2      -> wrap (TDict ws1 (recurse t1) (recurse t2) ws2)
    TArrow ws1 ts ws2        -> wrap (TArrow ws1 (List.map recurse ts) ws2)
    TUnion ws1 ts ws2        -> wrap (TUnion ws1 (List.map recurse ts) ws2)
    TTuple ws1 ts ws2 mt ws3 -> Types.simplifyTailedTuple <| wrap (TTuple ws1 (List.map recurse ts) ws2 (Maybe.map recurse mt) ws3)


type TupleTypingPreference = PreferTuples | PreferLists

infer : TypesHeap -> TypingContext -> Exp -> Type
infer typesHeap context e =
  let
    fail : TypesHeap -> String -> Type
    fail typesHeap errMsg =
      let
        _ = Utils.log errMsg
        _ = ImpureGoodies.mutateRecordField typesHeap "errors" (errMsg :: typesHeap.errors)
      in
      newVar typesHeap

    recordType : TypesHeap -> Int -> Type -> TypingContext -> ()
    recordType typesHeap id tipe context =
      if id > 0 then
        let newIdToType = Dict.insert id (tipe, context) typesHeap.idToTypeAndContext in
        let _ = ImpureGoodies.mutateRecordField typesHeap "idToTypeAndContext" newIdToType in
        ()
      else
        ()

    -- So we can add role information after-the-fact to certain types.
    newVarAs : TypesHeap -> Type -> Type
    newVarAs typesHeap tipe =
      let t = newVar typesHeap in
      let _ = unify typesHeap t tipe in
      t

    patToTypeAndContext : TupleTypingPreference -> TypesHeap -> Pat -> (Type, TypingContext)
    patToTypeAndContext tupleTypingPreference typesHeap p =
      let recurse = patToTypeAndContext tupleTypingPreference typesHeap in
      -- Log PId type
      (\(patType, contextFromPat) ->
        let _ = recordType typesHeap p.val.pid patType context in
        (patType, contextFromPat)
      ) <|
      case p.val.p__ of
        PVar _ ident _ ->
          let t = newVar typesHeap in
          ( t, [(ident, t)] )

        PConst _ _            -> ( newVarAs typesHeap tNum, [] )
        PBase _ (EBool _)     -> ( tBool,                   [] )
        PBase _ (EString _ _) -> ( tString,                 [] )
        PBase _ ENull         -> ( tNull,                   [] )
        PWildcard _           -> ( newVar typesHeap,        [] )

        -- For function args and let bindings, want to be strict here and type as tuples, not lists.
        --
        -- For case pats we can be more loose and assume the any tuples patterns could be matching against arbitrary-length lists.
        PList _ ps _ Nothing _ ->
          let (ts, contexts) = List.unzip <| List.map recurse ps in
          case tupleTypingPreference of
            PreferTuples ->
              ( newVarAs typesHeap <| tTuple ts
              , List.concat contexts
              )

            PreferLists ->
              let tTail = newVar typesHeap in
              ( newVarAs typesHeap <| tTupleRest ts (Just tTail)
              , List.concat contexts
              )

        PList _ ps _ (Just pTail) _ ->
          let (ts, contexts)       = List.unzip <| List.map recurse ps in
          let (tTail, contextTail) = recurse pTail in
          ( tTupleRest ts (Just tTail)
          , List.concat contexts ++ contextTail
          )

        PAs _ ident _ pChild ->
          let (tChild, contextChild) = recurse pChild in
          ( tChild
          , (ident, tChild)::contextChild
          )

        PParens _ pChild _ ->
          recurse pChild

  in
  -- Log inferred type
  (\inferred ->
    let _ = recordType typesHeap e.val.eid inferred context in
    inferred
  ) <|
  -- Tag [Num, Num] pairs as points
  tagWithRolesBasedOnStructure typesHeap <|
  case e.val.e__ of
    EVar _ x ->
      --
      --      x : σ ∈ Γ    τ = inst(σ)
      -- Var --------------------------
      --           Γ ⊢ x : τ
      --
      -- "The procedure inst(σ) specializes the polytype σ by copying the term and replacing the bound type variables consistently by new monotype variables."
      case Utils.maybeFind x context of
        Just sigma ->
          let t = inst typesHeap sigma in
          t

        Nothing ->
          fail typesHeap <| "Could not find " ++ x ++ " in type context! (Undefined var? Unannotated prelude item?)"

    EApp _ e0 [] _ _   -> fail typesHeap <| "Empty EApp! " ++ unparseExp e
    EApp _ e0 [e1] _ _ ->
      --
      --      Γ ⊢ e0 : τ0    Γ ⊢ e1 : τ1    τ2 = newvar    unify(τ0, τ1 → τ2)
      -- App ------------------------------------------------------------------
      --                            Γ ⊢ e0 e1 : τ2
      --
      -- "'newvar' produces a new monotype variable."
      let t0 = infer typesHeap context e0 in
      let t1 = infer typesHeap context e1 in
      let t2 = newVar typesHeap in
      let _ = unify typesHeap t0 (tArrow t1 t2) in
      t2

    EApp _ e0 (e1::eRest) _ _ ->
      infer typesHeap context (eApp (eApp e0 [e1]) eRest)

    EFun _ ps fBody _ -> -- Assumes no duplicate names in ps
      --
      --      τ = newvar    Γ, x : τ ⊢ e : τ'
      -- Abs ---------------------------------
      --            Γ ⊢ λx. e : τ → τ'
      --
      -- "'newvar' produces a new monotype variable."
      let
        (argTypes, contexts) = List.unzip <| List.map (patToTypeAndContext PreferTuples typesHeap) ps
        bodyContext = List.concat contexts ++ context
        returnType = infer typesHeap bodyContext fBody
      in
      tArrows (argTypes ++ [returnType])

    ELet _ _ isRec p _ e1 _ e2 _ ->
      let
        -- I hope this is right for recursive lets!
        (pType, pContext) = patToTypeAndContext PreferTuples typesHeap p

        boundExpContext =
          if isRec
          then pContext ++ context
          else context

        t1 = infer typesHeap boundExpContext e1
        _  = unify typesHeap pType t1
      in
      let handleRegular () =
        let t2 = infer typesHeap (pContext ++ context) e2 in
        t2
      in
      -- Only going to do let-polymorphism for single var assignments of arrow type. (Handling quantification with tuples + as-patterns is trickier than it's worth.)
      --
      -- Not doing polymorphism of non-arrows may facilitate better role propogation.
      --
      --      Γ ⊢ e1 : τ1    Γ, x : Γ'(τ1) ⊢ e2 : τ2
      -- Let --------------------------------------
      --           Γ ⊢ let x = e1 in e2 : τ2
      --
      --
      --  Γ'(τ) = ∀ᾱ. τ
      --  Γ'(τ) "quantifies all monotype variables [in τ] not bound in Γ"
      --
      let handlePolymorphic ident =
        let
          t1_ =
            -- This back-and-forth bit seems to have been fixed either by the copious introduction
            -- of type vars (so we can do role propogation) or by unify recursing on its inputs rather
            -- than on the found representatives (also for role propogation).
            if False then -- isRec then
              -- I'm not smart enough to figure out how not to need these steps.
              -- How many times back and forth is sufficient?!
              let t1_  = infer typesHeap boundExpContext e1 in
              let _    = unify typesHeap pType t1_ in
              let t1__ = infer typesHeap boundExpContext e1 in
              let _    = unify typesHeap pType t1__ in
              t1__
            else
              t1

          quantifiedT1 = quantifyUnboundVars context (finalizeType typesHeap.equivalenceClasses t1_)

          -- Re-log PId type
          _ = recordType typesHeap p.val.pid quantifiedT1 context

          t2 = infer typesHeap ((ident, quantifiedT1)::context) e2
        in
        -- No need to worry about a value restriction b/c no side effects.
        t2
      in
      case p.val.p__ of
        PVar _ ident _ ->
          case (find typesHeap t1).representative.val.t__ of
            TArrow _ _ _ -> handlePolymorphic ident
            _            -> handleRegular ()
        _ -> handleRegular ()

    EConst _ n _ _          -> newVarAs typesHeap tNum |> tagWithRolesBasedOnNumber typesHeap n
    EBase _ (EBool _)       -> tBool
    EBase _ (EString _ _)   -> tString
    EBase _ ENull           -> tNull
    EOp _ op es _ ->
      let tOperands = List.map (infer typesHeap context) es in
      tagWithRolesBasedOnOperation typesHeap op.val tOperands <|
      newVarAs typesHeap <|
      let
        operandsShouldBe typesHeap ts =
          Utils.zip tOperands ts
          |> List.map (uncurry (unify typesHeap))

        allOperandsShouldBe typesHeap t =
          List.map (unify typesHeap t) tOperands

        mathOp typesHeap =
          let _ = allOperandsShouldBe typesHeap tNum in
          tNum

        passthrough typesHeap =
          List.head tOperands
          |> Maybe.withDefault (newVar typesHeap)
      in
      case op.val of
        Pi             -> tNum
        DictEmpty      -> tDict (newVar typesHeap) (newVar typesHeap)
        Cos            -> mathOp typesHeap
        Sin            -> mathOp typesHeap
        ArcCos         -> mathOp typesHeap
        ArcSin         -> mathOp typesHeap
        Abs            -> mathOp typesHeap
        Floor          -> mathOp typesHeap
        Ceil           -> mathOp typesHeap
        Round          -> mathOp typesHeap
        ToStr          -> tString
        OptNumToString -> tString
        Sqrt           -> mathOp typesHeap
        Explode        -> let _ = allOperandsShouldBe typesHeap tString in tList tString -- String -> List String
        DebugLog       -> passthrough typesHeap
        NoWidgets      -> passthrough typesHeap
        Plus           -> mathOp typesHeap -- Technically polymorphic (strings or numbers), but that's too hard. We never use strings in our examples yet.
        Minus          -> mathOp typesHeap
        Mult           -> mathOp typesHeap
        Div            -> mathOp typesHeap
        Lt             -> let _ = allOperandsShouldBe typesHeap tNum in tBool
        Eq             -> let _ = allOperandsShouldBe typesHeap (newVar typesHeap) in tBool
        Mod            -> mathOp typesHeap
        Pow            -> mathOp typesHeap
        Ln             -> mathOp typesHeap
        ArcTan2        -> mathOp typesHeap
        DictGet ->
          -- Actually returns valueType ∪ {Null}
          -- We'll type it as k -> Dict k v -> a
          let
            (k, v, a) = (newVar typesHeap, newVar typesHeap, newVar typesHeap)
            _         = operandsShouldBe typesHeap [k, tDict k v]
          in
          a

        DictRemove ->
          -- k -> Dict k v -> Dict k v
          let
            (k, v) = (newVar typesHeap, newVar typesHeap)
            _      = operandsShouldBe typesHeap [k, tDict k v]
          in
          tDict k v

        DictInsert ->
          -- k -> v -> Dict k v -> Dict k v
          let
            (k, v) = (newVar typesHeap, newVar typesHeap)
            _      = operandsShouldBe typesHeap [k, v, tDict k v]
          in
          tDict k v

    EList _ es _ maybeTail _ ->
      let
        tHeads     = es |> List.map (Tuple.second >> infer typesHeap context)
        maybeTTail = maybeTail |> Maybe.map (infer typesHeap context)
        tInTail    = newVar typesHeap
        _          = maybeTTail |> Maybe.map (unify typesHeap (tList tInTail))
      in
      newVarAs typesHeap <| tTupleRest tHeads (Just tInTail)

    EIf _ e1 _ e2 _ e3 _ ->
      let
        t1 = infer typesHeap context e1
        _  = unify typesHeap t1 tBool
      in
      let
        t2 = infer typesHeap context e2
        t3 = infer typesHeap context e3
        -- _ = Utils.log <| "e3 pre-unify: " ++ unparse (finalizeType typesHeap.equivalenceClasses t3)
      in
      let
        _  = unify typesHeap t2 t3
      in
      -- let _ = Utils.log <| "e2: " ++ unparse (finalizeType typesHeap.equivalenceClasses t2) in
      -- let _ = Utils.log <| "e3: " ++ unparse (finalizeType typesHeap.equivalenceClasses t3) in
      t3

    ECase _ e1 branches _ ->
      let
        t1 = infer typesHeap context e1

        (patTypes, branchTypes) =
          branches
          |> List.map .val
          |> List.map
              (\(Branch_ _ p e _) ->
                let
                  (pType, pContext) = patToTypeAndContext PreferLists typesHeap p
                  eType = infer typesHeap (pContext ++ context) e
                in
                (pType, eType)
              )
          |> List.unzip
      in
      let
        _  = patTypes |> List.map (unify typesHeap t1)
        t2 = newVar typesHeap
        _  = branchTypes |> List.map (unify typesHeap t2)
      in
      t2

    ETypeCase _ e1 tbranches _ ->
      let
        t1 = infer typesHeap context e1

        branchTypes = tbranchExps tbranches |> List.map (infer typesHeap context)
      in
      let
        t2 = newVar typesHeap
        _  = branchTypes |> List.map (unify typesHeap t2)
      in
      t2

    EComment _ _ e1              -> infer typesHeap context e1
    EOption _ _ _ _ e1           -> infer typesHeap context e1
    ETyp _ pat tipe e1 _         -> infer typesHeap context e1 -- Yes, ignoring the annotation.
    EColonType _ e1 _ tipe _     -> infer typesHeap context e1 -- Yes, ignoring the annotation.
    ETypeAlias _ pat tipe e1 _   -> infer typesHeap context e1
    EParens _ e1 _ _             -> infer typesHeap context e1
    EHole _ (HolePBE examples _) ->
      let
        exampleExps  = examples |> List.map (\(_, _, _, e) -> e)
        exampleTypes = exampleExps |> List.map (infer typesHeap context)
        t            = newVar typesHeap
        _            = exampleTypes |> List.map (unify typesHeap t)
      in
      t

    EHole _ (HoleVal val) ->
      let valToType val =
        case val.v_ of
          VConst _ _        -> tNum
          VBase (VBool _)   -> tBool
          VBase (VString _) -> tString
          VBase VNull       -> tNull
          VList vs          -> tTupleRest (vs |> List.map valToType) (Just (newVar typesHeap))
          VDict _           -> tDict (newVar typesHeap) (newVar typesHeap)
          VClosure _ _ _ _  -> tArrow (newVar typesHeap) (newVar typesHeap)
      in
      valToType val

    EHole _ _ ->
      newVar typesHeap


--------------------------------------
-- Role Handling                    --
--------------------------------------

--
-- Our special role rules here (beyond propogation w/unification).
--
-- The information propogation dynamics of our current setup isn't super great so we may miss some roles.
--


--
-- 0 = Zero
-- 1 = One
--

tagWithRolesBasedOnNumber : TypesHeap -> Num -> Type -> Type
tagWithRolesBasedOnNumber typesHeap n tipe =
  let roles =
    case n of
      0 -> Set.singleton "Zero"
      1 -> Set.singleton "One"
      _ -> Set.empty
  in
  if Set.size roles > 0 then
    let _ = addRolesToClass typesHeap roles tipe in
    tipe
  else
    tipe


--
-- [Num, Num] = [X, Y] : Point
--

tagWithRolesBasedOnStructure : TypesHeap -> Type -> Type
tagWithRolesBasedOnStructure typesHeap tipe =
  let typeClass = find typesHeap tipe in
  case typeClass.representative.val.t__ of
    TTuple ws1 [t1, t2] ws2 mt ws3 ->
      let
        aRep = (find typesHeap t1).representative
        bRep = (find typesHeap t2).representative

        tupleISPoint = Set.member "Point" typeClass.representative.val.roles
        aIsX         = Set.member "X"     aRep.val.roles
        bIsY         = Set.member "Y"     bRep.val.roles
      in
      if tupleISPoint && aIsX && bIsY then
        tipe
      else
        case (aRep.val.t__, bRep.val.t__) of
          (TNum _, TNum _) ->
            let
              _ = addRolesToClass typesHeap (Set.singleton "Point") tipe
              _ = addRolesToClass typesHeap (Set.singleton "X") t1
              _ = addRolesToClass typesHeap (Set.singleton "Y") t2
            in
            tipe

          _ ->
            tipe

    _ ->
      tipe



-- Operation-based rules -----------


whatever        = always True
is role         = Set.member role
isntAnyOf roles = not << Utils.anyOverlapListSet roles
unchanged       = Set.empty
becomes role    = Set.singleton role


-- Add roles if predicates match.
--
-- ( (returnTypePred, arg1Pred, arg2Pred), (newReturnRoles, newArg1Roles, newArg2Roles) )
--
-- Because of the information flow direction, I don't think it's ever worth trying to test the return type (it should have no roles the point these are evaluated during inference)


-- Addition rules ----
--
-- X = X + HorizontalDistance
-- Y = Y + VerticalDistance
--
-- HorizontalDistance = HorizontalDistance + HorizontalDistance
-- VerticalDistance   = VerticalDistance + VerticalDistance
--
additionRules =
  [ ( ( whatever , is "X"                                , isntAnyOf ["X", "HorizontalDistance"] ), ( becomes "X"                  , unchanged                    , becomes "HorizontalDistance" ) ) -- (X) = X + (HorizontalDistance)
  , ( ( whatever , isntAnyOf ["X", "HorizontalDistance"] , is "X"                                ), ( becomes "X"                  , becomes "HorizontalDistance" , unchanged                    ) ) -- (X) = (HorizontalDistance) + X
  , ( ( whatever , is "HorizontalDistance"               , isntAnyOf ["X"]                       ), ( becomes "HorizontalDistance" , unchanged                    , becomes "HorizontalDistance" ) ) -- (HorizontalDistance) = HorizontalDistance + (HorizontalDistance)
  , ( ( whatever , isntAnyOf ["X"]                       , is "HorizontalDistance"               ), ( becomes "HorizontalDistance" , becomes "HorizontalDistance" , unchanged                    ) ) -- (HorizontalDistance) = (HorizontalDistance) + HorizontalDistance
  , ( ( whatever , is "Y"                                , isntAnyOf ["Y", "VerticalDistance"]   ), ( becomes "Y"                  , unchanged                    , becomes "VerticalDistance"   ) ) -- (Y) = Y + (VerticalDistance)
  , ( ( whatever , isntAnyOf ["Y", "VerticalDistance"]   , is "Y"                                ), ( becomes "Y"                  , becomes "VerticalDistance"   , unchanged                    ) ) -- (Y) = (VerticalDistance) + Y
  , ( ( whatever , is "VerticalDistance"                 , isntAnyOf ["Y"]                       ), ( becomes "VerticalDistance"   , unchanged                    , becomes "VerticalDistance"   ) ) -- (VerticalDistance) = VerticalDistance + (VerticalDistance)
  , ( ( whatever , isntAnyOf ["Y"]                       , is "VerticalDistance"                 ), ( becomes "VerticalDistance"   , becomes "VerticalDistance"   , unchanged                    ) ) -- (VerticalDistance) = (VerticalDistance) + VerticalDistance
  ]

-- Subtraction rules ----
--
-- X = X - HorizontalDistance
-- Y = Y - VerticalDistance
--
-- HorizontalDistance = HorizontalDistance - HorizontalDistance
-- VerticalDistance   = VerticalDistance - VerticalDistance
--
-- HorizontalDistance = X - X
-- VerticalDistance   = Y - Y
--
subtractionRules =
  [ ( ( whatever , is "X"                                , is "HorizontalDistance" ), ( becomes "X"                  , unchanged , unchanged ) ) -- (X) = X - HorizontalDistance
  , ( ( whatever , is "Y"                                , is "VerticalDistance"   ), ( becomes "Y"                  , unchanged , unchanged ) ) -- (Y) = Y - VerticalDistance
  , ( ( whatever , is "HorizontalDistance"               , is "HorizontalDistance" ), ( becomes "HorizontalDistance" , unchanged , unchanged ) ) -- (HorizontalDistance) = HorizontalDistance - HorizontalDistance
  , ( ( whatever , is "VerticalDistance"                 , is "VerticalDistance"   ), ( becomes "VerticalDistance"   , unchanged , unchanged ) ) -- (VerticalDistance)   = VerticalDistance   - VerticalDistance
  , ( ( whatever , is "X"                                , is "X"                  ), ( becomes "HorizontalDistance" , unchanged , unchanged ) ) -- (HorizontalDistance) = X - X
  , ( ( whatever , is "Y"                                , is "Y"                  ), ( becomes "VerticalDistance"   , unchanged , unchanged ) ) -- (VerticalDistance)    = Y - Y
  ]

-- Less Than rules ----
--
-- var < 1  =>  var = PivotsOnOne (possible a depth)
--
lessThanRules =
  [ ( ( whatever , is "One"  , whatever  ), ( unchanged , unchanged              , becomes "PivotsOnOne"  ) )
  , ( ( whatever , whatever  , is "One"  ), ( unchanged , becomes "PivotsOnOne"  , unchanged              ) )
  , ( ( whatever , is "Zero" , whatever  ), ( unchanged , unchanged              , becomes "PivotsOnZero" ) )
  , ( ( whatever , whatever  , is "Zero" ), ( unchanged , becomes "PivotsOnZero" , unchanged              ) )
  ]

tagWithRolesBasedOnOperation : TypesHeap -> Op_ -> List Type -> Type -> Type
tagWithRolesBasedOnOperation typesHeap op_ ts returnType =
  case ts of
    [t1, t2] ->
      let opRules =
        case op_ of
          Plus  -> additionRules
          Minus -> subtractionRules
          Lt    -> lessThanRules
          _     -> []
      in
      if opRules /= [] then
        let
          aRep   = (find typesHeap t1).representative
          bRep   = (find typesHeap t2).representative
          retRep = (find typesHeap returnType).representative
        in
        case opRules |> Utils.findFirst (\((returnTypePred, arg1Pred, arg2Pred), _) -> returnTypePred retRep.val.roles && arg1Pred aRep.val.roles && arg2Pred bRep.val.roles) of
          Just (_, (retRoles, aRoles, bRoles)) ->
            let
              _ = if retRoles /= unchanged then addRolesToClass typesHeap retRoles returnType else ()
              _ = if aRoles   /= unchanged then addRolesToClass typesHeap aRoles   t1         else ()
              _ = if bRoles   /= unchanged then addRolesToClass typesHeap bRoles   t2         else ()
            in
            returnType

          Nothing ->
            returnType

      else
        returnType

    _ ->
      returnType