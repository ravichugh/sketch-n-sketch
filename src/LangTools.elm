module LangTools exposing (..)

-- Most of these methods used to be in Lang.elm
--
-- Extracted to avoid circular dependencies.
--
-- Lots of methods for dealing with identifiers and renaming.
--
-- Things in here are only used by LangTransform and ValueBasedTransform,
-- currently. Also used in InterfaceView to find unfrozen locs to animate in our
-- (possibly defunct) relate attributes selection screen.

import Eval
import Lang exposing (..)
import LangParser2
import Utils
import LangUnparser
import Types

import Dict
import Set


-- For ranking synthesized expressions
nodeCount : Exp -> Int
nodeCount exp =
  case exp.val.e__ of
    EConst _ _ _ _          -> 1
    EBase _ _               -> 1
    EVar _ x                -> 1
    EFun _ ps e _           -> 1 + patsNodeCount ps + nodeCount e
    EOp _ op es _           -> 1 + expsNodeCount es
    EList _ es _ (Just e) _ -> 1 + expsNodeCount es + nodeCount e
    EList _ es _ Nothing _  -> 1 + expsNodeCount es
    EIf _ e1 e2 e3 _        -> 1 + expsNodeCount [e1, e2, e3]
    -- Cases have a set of parens around each branch. I suppose each should count as a node.
    ECase _ e1 bs _         -> 1 + (List.length bs) + nodeCount e1 + patsNodeCount (branchPats bs) + expsNodeCount (branchExps bs)
    ETypeCase _ p tbs _     -> 1 + (List.length tbs) + patNodeCount p + typesNodeCount (tbranchTypes tbs) + expsNodeCount (tbranchExps tbs)
    -- ETypeCase _ e1 tbranches _  ->
    EApp _ e1 es _          -> 1 + nodeCount e1 + expsNodeCount es
    ELet _ _ _ p e1 e2 _    -> 1 + patNodeCount p + nodeCount e1 + nodeCount e2
    EComment _ _ e1         -> 0 + nodeCount e1 -- Comments don't count.
    EOption _ _ _ _ e1      -> 1 + nodeCount e1
    ETyp _ p t e1 _         -> 1 + patNodeCount p + typeNodeCount t + nodeCount e1
    EColonType _ e1 _ t _   -> 1 + typeNodeCount t + nodeCount e1
    ETypeAlias _ p t e1 _   -> 1 + patNodeCount p + typeNodeCount t + nodeCount e1


expsNodeCount : List Exp -> Int
expsNodeCount exps =
  exps |> List.map nodeCount |> List.sum

patNodeCount : Pat -> Int
patNodeCount pat =
  case pat.val of
    PVar _ _ _                  -> 1
    PConst _ _                  -> 1
    PBase _ _                   -> 1
    PList _ pats _ (Just pat) _ -> 1 + patsNodeCount pats + patNodeCount pat
    PList _ pats _ Nothing    _ -> 1 + patsNodeCount pats
    PAs _ _ _ pat               -> 1 + patNodeCount pat

patsNodeCount : List Pat -> Int
patsNodeCount pats =
  pats |> List.map patNodeCount |> List.sum

typeNodeCount : Type -> Int
typeNodeCount tipe =
  case tipe.val of
    TNum _                          -> 1
    TBool _                         -> 1
    TString _                       -> 1
    TNull _                         -> 1
    TList _ t _                     -> 1 + typeNodeCount t
    TDict _ kt vt _                 -> 1 + typeNodeCount kt + typeNodeCount vt
    TTuple _ ts _ Nothing _         -> 1 + typesNodeCount ts
    TTuple _ ts _ (Just t) _        -> 1 + typesNodeCount ts + typeNodeCount t
    TArrow _ ts _                   -> 1 + typesNodeCount ts
    TUnion _ ts _                   -> 1 + typesNodeCount ts
    TNamed _ _                      -> 1
    TVar _ _                        -> 1
    TForall _ (One (_, _)) t _      -> 1 + typeNodeCount t
    TForall _ (Many _ idents _) t _ -> 1 + List.length idents + typeNodeCount t
    TWildcard _                     -> 1

typesNodeCount : List Type -> Int
typesNodeCount types =
  types |> List.map typeNodeCount |> List.sum




patternListsEqual : List Pat -> List Pat -> Bool
patternListsEqual patsA patsB =
  Utils.maybeZip patsA patsB
  |> Maybe.map (List.all (\(patA, patB) -> patternsEqual patA patB))
  |> Maybe.withDefault False

-- More syntactically strict than "will these two patterns match/reject the same things?", i.e. identifier strings must also match exactly
patternsEqual : Pat -> Pat -> Bool
patternsEqual patA patB =
  case (patA.val, patB.val) of
    (PVar ws1A identA wdA,               PVar ws1B identB wdB)               -> identA == identB
    (PConst ws1A nA,                     PConst ws1B nB)                     -> nA == nB
    (PBase ws1A (EBool boolA),           PBase ws1B (EBool boolB))           -> boolA == boolB
    (PBase ws1A (EString qcA strA),      PBase ws1B (EString qcB strB))      -> strA == strB
    (PBase ws1A ENull,                   PBase ws1B ENull)                   -> True
    (PList ws1A psA ws2A Nothing ws3A,   PList ws1B psB ws2B Nothing ws3B)   -> patternListsEqual psA psB
    (PList ws1A psA ws2A (Just pA) ws3A, PList ws1B psB ws2B (Just pB) ws3B) -> patternListsEqual (pA::psA) (pB::psB)
    (PAs ws1A identA ws2A pA,            PAs ws1B identB ws2B pB)            -> identA == identB && patternsEqual pA pB
    _                                                                        -> False


-- Traverse baseExp and otherExp, comparing them to each other node by node.
-- When they differ, adds the differing node in otherExp to the return list.
--
-- For finding what expressions are being removed when an expression is replaced by a function.
--
-- The sister function here is ExpressionBasedTransform.passiveSynthesisSearch.merge
extraExpsDiff : Exp -> Exp -> List Exp
extraExpsDiff baseExp otherExp =
  let childDiffs () =
    case Utils.maybeZip (childExps baseExp) (childExps otherExp) of
      Just childPairs -> childPairs |> List.concatMap (\(aChild, bChild) -> extraExpsDiff aChild bChild)
      Nothing         -> [otherExp]
  in
  case (baseExp.val.e__, otherExp.val.e__) of
    (EConst ws1A nA locA wdA,              EConst ws1B nB locB wdB)              -> if nA == nB then [] else [otherExp]
    (EBase ws1A (EBool True),              EBase ws1B (EBool True))              -> []
    (EBase ws1A (EBool False),             EBase ws1B (EBool False))             -> []
    (EBase ws1A (EString qcA strA),        EBase ws1B (EString qcB strB))        -> if strA == strB then [] else [otherExp]
    (EBase ws1A ENull,                     EBase ws1B ENull)                     -> []
    (EVar ws1A identA,                     EVar ws1B identB)                     -> if identA == identB then [] else [otherExp]
    (EFun ws1A psA eA ws2A,                EFun ws1B psB eB ws2B)                -> if patternListsEqual psA psB then extraExpsDiff eA eB else [otherExp]
    (EOp ws1A opA esA ws2A,                EOp ws1B opB esB ws2B)                -> if opA.val == opB.val then childDiffs () else [otherExp]
    (EList ws1A esA ws2A Nothing ws3A,     EList ws1B esB ws2B Nothing ws3B)     -> childDiffs ()
    (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)   -> childDiffs ()
    (EApp ws1A fA esA ws2A,                EApp ws1B fB esB ws2B)                -> childDiffs ()
    (ELet ws1A kindA recA pA e1A e2A ws2A, ELet ws1B kindB recB pB e1B e2B ws2B) -> if recA == recB && patternsEqual pA pB then extraExpsDiff e1A e1B ++ extraExpsDiff e2A e2B else [otherExp]
    (EIf ws1A e1A e2A e3A ws2A,            EIf ws1B e1B e2B e3B ws2B)            -> extraExpsDiff e1A e1B ++ extraExpsDiff e2A e2B ++ extraExpsDiff e3A e3B
    (ECase ws1A eA branchesA ws2A,         ECase ws1B eB branchesB ws2B)         -> Utils.maybeZip branchesA branchesB |> Maybe.andThen (\branchPairs -> let bValPairs = branchPairs |> List.map (\(bA, bB) -> (bA.val, bB.val)) in if bValPairs |> List.all (\(Branch_ bws1A bpatA beA bws2A, Branch_ bws1B bpatB beB bws2B) -> patternsEqual bpatA bpatB) then Just (childDiffs ()) else Nothing) |> Maybe.withDefault [otherExp]
    (ETypeCase ws1A patA tbranchesA ws2A,  ETypeCase ws1B patB tbranchesB ws2B)  -> if patternsEqual patA patB then Utils.maybeZip tbranchesA tbranchesB |> Maybe.andThen (\tbranchPairs -> let tbValPairs = tbranchPairs |> List.map (\(tbA, tbB) -> (tbA.val, tbB.val)) in if tbValPairs |> List.all (\(TBranch_ tbws1A tbtypeA tbeA tbws2A, TBranch_ tbws1B tbtypeB tbeB tbws2B) -> Types.equal tbtypeA tbtypeB) then Just (childDiffs ()) else Nothing) |> Maybe.withDefault [otherExp] else [otherExp]
    (EComment wsA sA e1A,                  _)                                    -> extraExpsDiff e1A otherExp
    (_,                                    EComment wsB sB e1B)                  -> extraExpsDiff baseExp e1B
    (EOption ws1A s1A ws2A s2A e1A,        EOption ws1B s1B ws2B s2B e1B)        -> [otherExp]
    (ETyp ws1A patA typeA eA ws2A,         ETyp ws1B patB typeB eB ws2B)         -> if patternsEqual patA patB && Types.equal typeA typeB then extraExpsDiff eA eB else [otherExp]
    (EColonType ws1A eA ws2A typeA ws3A,   EColonType ws1B eB ws2B typeB ws3B)   -> if Types.equal typeA typeB then extraExpsDiff eA eB else [otherExp]
    (ETypeAlias ws1A patA typeA eA ws2A,   ETypeAlias ws1B patB typeB eB ws2B)   -> if patternsEqual patA patB && Types.equal typeA typeB then extraExpsDiff eA eB else [otherExp]
    _                                                                            -> [otherExp]


replaceConstsWithVars : Dict.Dict LocId Ident -> Exp -> Exp
replaceConstsWithVars locIdToNewName exp =
  let replacer exp__ =
    case exp__ of
      EConst ws n (locId, frozen, ident) wd ->
        case Dict.get locId locIdToNewName of
          Just newName -> EVar ws newName
          Nothing      -> exp__
      _ -> exp__
  in
  mapExpViaExp__ replacer exp


-- These should use syncOptions
-- Or we should remove the frozen by default config option
unfrozenLocIdsAndNumbers : Exp -> List (LocId, Num)
unfrozenLocIdsAndNumbers exp =
  allLocsAndNumbers exp
  |> List.filter (\((locId, annotation, _), n) -> annotation /= "!" && not (LangParser2.isPreludeLocId locId))
  |> List.map (\((locId, _, _), n) -> (locId, n))


-- These should use syncOptions
-- Or we should remove the frozen by default config option
frozenLocIdsAndNumbers : Exp -> List (LocId, Num)
frozenLocIdsAndNumbers exp =
  allLocsAndNumbers exp
  |> List.filter (\((locId, annotation, _), n) -> annotation == "!" || LangParser2.isPreludeLocId locId)
  |> List.map (\((locId, _, _), n) -> (locId, n))


allLocsAndNumbers : Exp -> List (Loc, Num)
allLocsAndNumbers exp =
  foldExpViaE__
    (\e__ acc ->
      case e__ of
        EConst _ n loc _ -> (loc, n) :: acc
        _                -> acc
    )
    []
    exp


allLocIds exp =
  allLocsAndNumbers exp
  |> List.map (\((locId, _, _), _) -> locId)


justFindExpByEId : EId -> Exp -> Exp
justFindExpByEId eid exp =
  findExpByEId eid exp
  |> Utils.fromJust__ (\() -> "Couldn't find eid " ++ toString eid ++ " in " ++ LangUnparser.unparseWithIds exp)


-- Is the expression in the body of only defs/comments/options?
--
-- The "top level" is a single path on the tree, so walk it and look
-- for the target expression.
isTopLevel : Exp -> Exp -> Bool
isTopLevel exp program =
  if exp == program then
    True
  else
    case maybeTopLevelChild program of
      Just child -> isTopLevel exp child
      Nothing    -> False


maybeTopLevelChild : Exp -> Maybe Exp
maybeTopLevelChild exp =
  case exp.val.e__ of
    ETyp _ _ _ body _       -> Just body
    EColonType _ body _ _ _ -> Just body
    ETypeAlias _ _ _ body _ -> Just body
    ELet _ Def _ _ _ body _ -> Just body
    EComment _ _ e          -> Just e
    EOption _ _ _ _ e       -> Just e
    _                       -> Nothing


topLevelExps : Exp -> List Exp
topLevelExps program =
  case maybeTopLevelChild program of
    Just child -> program::(topLevelExps child)
    Nothing    -> [program]


lastExp : Exp -> Exp
lastExp exp =
  case childExps exp of
    []       -> exp
    children -> lastExp <| Utils.last "LangTools.lastExp" children


identifiersVisibleAtProgramEnd : Exp -> Set.Set Ident
identifiersVisibleAtProgramEnd program =
  let lastEId = (lastExp program).val.eid in
  visibleIdentifiersAtEIds program (Set.singleton lastEId)


-- e.g. "rect1 x" for (def rect1 (let x = ... in ...) ...)
locDescription program loc =
  let (locId, _, ident) = loc in
  let baseIdent = if ident == "" then "k"++(toString locId) else ident in
  let scopeNamesLiftedThrough = scopeNamesLocLiftedThrough program loc in
  String.join " " (scopeNamesLiftedThrough ++ [baseIdent])


-- Still needs to be rewritten to handle scopes created by case branches.
--
-- We care about the parent scope names for all the nested let/def assigns the
-- LocId appears in--we don't care about let/def bodies.
scopeNamesLocLiftedThrough : Exp -> Loc -> List Ident
scopeNamesLocLiftedThrough newLetBody targetLoc =
  let (targetLocId, _, ident) = targetLoc in
  case scopeNamesLocLiftedThrough_ targetLocId [] newLetBody of
    [] ->
      []

    scopeNames::[] ->
      -- Last element may be the original identifier: if so, remove it.
      case List.head (List.reverse scopeNames) of
        Nothing ->
          []

        Just lastScopeName ->
          if lastScopeName == ident
          then Utils.removeLastElement scopeNames
          else scopeNames

    _ ->
      Debug.crash <| "Found locId "++(toString targetLocId)++" more than once in the expression: "++(toString newLetBody)


-- Returns array of matches (easiest to implement).
-- Should only be at most one match, though.
scopeNamesLocLiftedThrough_ targetLocId scopeNames exp =
  case exp.val.e__ of
    ELet _ _ _ pat assigns body _ ->
      let scopeNames_ =
        case pat.val of
          PVar _ ident _  -> scopeNames ++ [ident]
          PAs _ ident _ _ -> scopeNames ++ [ident]
          _               -> scopeNames
      in
      -- Ident should only be added to assigns. Otherwise you get lots of junk
      -- names.
      (scopeNamesLocLiftedThrough_ targetLocId scopeNames_ assigns) ++
      (scopeNamesLocLiftedThrough_ targetLocId scopeNames  body)

    EConst _ _ (locId, _, _) _ ->
       if locId == targetLocId
       then [scopeNames]
       else []

    _ ->
      let recurse exp = scopeNamesLocLiftedThrough_ targetLocId scopeNames exp in
      List.concatMap recurse (childExps exp)


-- Returns the common ancestor just inside the deepest common scope -- the expression you want to wrap with new defintions.
-- If the nearest common ancestor is itself a scope, returns that instead.
justInsideDeepestCommonScope : Exp -> (Exp -> Bool) -> Exp
justInsideDeepestCommonScope exp pred =
  let allWithAncestors = -- debugLog "locsAncestors" <|
    findAllWithAncestors pred exp
  in
  let commonAncestors = Utils.commonPrefix allWithAncestors in
  -- isScope needs to see the node's parent...because case statements
  -- produce many scopes out of one expression
  -- The below adds a maybe parent to each node, so we get List (List
  -- (Maybe Exp, Exp))
  let commonAncestorsWithParents = -- debugLog "locsAncestorsWithParents" <|
    Utils.zip (Nothing :: (List.map Just commonAncestors)) commonAncestors
  in
  -- Pluck out [exp, deepestCommonScope, justInsideDeepestCommonScope]
  let candidates =
    let ancestorJustInsideCommonScope =
      commonAncestorsWithParents
      |> List.reverse
      |> Utils.takeWhile (\(parent, node) -> not <| isScope parent node)
      |> Utils.takeLast 1
      |> List.map Tuple.second
    in
    let deepestCommonScope =
      commonAncestorsWithParents
      |> List.filter (\(parent, node) -> isScope parent node)
      |> Utils.takeLast 1
      |> List.map Tuple.second
    in
    exp :: (deepestCommonScope ++ ancestorJustInsideCommonScope)
  in
  Utils.last_ candidates


-- Given [ [("a", eConst 4 dummyLoc), ("b", eConst 5 dummyLoc)], [("c", eConst 6 dummyLoc)] ] False bodyExp
--
-- Produces an Exp of:
--
-- (let [a c] [4 5]
-- (let [c] [6]
--   bodyExp))
--
wrapWithLets : List (List (String, Exp)) -> Bool -> Exp -> Exp
wrapWithLets listOfListsOfNamesAndAssigns isTopLevel bodyExp =
  let nonEmptyListOfListsOfNamesAndAssigns =
    List.filter
        (not << List.isEmpty)
        listOfListsOfNamesAndAssigns
  in
  case nonEmptyListOfListsOfNamesAndAssigns of
    [] ->
      bodyExp

    _::_ ->
      let oldPrecedingWhitespace = precedingWhitespace bodyExp in
      -- Insure one newline after first let
      let extraWhitespace =
        if String.contains "\n" oldPrecedingWhitespace then "" else "\n"
      in
      -- Limit to one newline for all lets
      let limitedOldPrecedingWhitespace =
        case String.split "\n" oldPrecedingWhitespace |> List.reverse of
          indentation::_ -> "\n" ++ indentation
          []             -> oldPrecedingWhitespace
      in
      let preceedingWs = extraWhitespace ++ limitedOldPrecedingWhitespace in
      let letOrDef = if isTopLevel then Def else Let in
      let wrappedWithLets =
        nonEmptyListOfListsOfNamesAndAssigns
        |> List.foldr
            (\letNamesAndAssigns innerExp ->
              eLetOrDef letOrDef letNamesAndAssigns innerExp
              |> replacePrecedingWhitespace preceedingWs
            )
            (addPrecedingWhitespace extraWhitespace bodyExp)
      in
      wrappedWithLets

preludeIdentifiers = Eval.initEnv |> List.map Tuple.first |> Set.fromList


identifiersSetPlusPrelude : Exp -> Set.Set Ident
identifiersSetPlusPrelude exp =
  Set.union (identifiersSet exp) preludeIdentifiers


identifiersSet : Exp -> Set.Set Ident
identifiersSet exp =
  identifiersList exp
  |> Set.fromList


identifiersSetInPat : Pat -> Set.Set Ident
identifiersSetInPat pat =
  identifiersListInPat pat
  |> Set.fromList


identifiersSetInPats : List Pat -> Set.Set Ident
identifiersSetInPats pats =
  List.map identifiersSetInPat pats
  |> Utils.unionAll


identifiersList : Exp -> List Ident
identifiersList exp =
  let folder e__ acc =
    case e__ of
      EVar _ ident ->
        ident::acc

      EFun _ pats _ _ ->
        (List.concatMap identifiersListInPat pats) ++ acc

      ECase _ _ branches _ ->
        let pats = branchPats branches in
        (List.concatMap identifiersListInPat pats) ++ acc

      ETypeCase _ pat _ _ ->
        (identifiersListInPat pat) ++ acc

      ELet _ _ _ pat _ _ _ ->
        (identifiersListInPat pat) ++ acc

      _ ->
        acc
  in
  foldExpViaE__
    folder
    []
    exp


-- Look for all non-free identifiers in the expression.
identifiersSetPatsOnly : Exp -> Set.Set Ident
identifiersSetPatsOnly exp =
  identifiersListPatsOnly exp
  |> Set.fromList


identifiersListPatsOnly : Exp -> List Ident
identifiersListPatsOnly exp =
  let folder e__ acc =
    case e__ of
      EFun _ pats _ _ ->
        (List.concatMap identifiersListInPat pats) ++ acc

      ECase _ _ branches _ ->
        let pats = branchPats branches in
        (List.concatMap identifiersListInPat pats) ++ acc

      -- If we are looking for introduced variables, should exclude ETypeCase
      -- ETypeCase _ pat _ _ ->
      --   (identifiersListInPat pat) ++ acc

      ELet _ _ _ pat _ _ _ ->
        (identifiersListInPat pat) ++ acc

      _ ->
        acc
  in
  foldExpViaE__
    folder
    []
    exp


identifiersListInPat : Pat -> List Ident
identifiersListInPat pat =
  case pat.val of
    PVar _ ident _              -> [ident]
    PList _ pats _ (Just pat) _ -> List.concatMap identifiersListInPat (pat::pats)
    PList _ pats _ Nothing    _ -> List.concatMap identifiersListInPat pats
    PAs _ ident _ pat           -> ident::(identifiersListInPat pat)
    _                           -> []


identifiersListInPats : List Pat -> List Ident
identifiersListInPats pats =
  List.concatMap
    identifiersListInPat
    pats


identifierCounts : Exp -> Dict.Dict Ident Int
identifierCounts exp =
  List.foldl
    (\ident counts ->
      Dict.update
        ident
        (\old ->
          case old of
            Just count -> Just (count + 1)
            Nothing    -> Just 1
        )
        counts
    )
    Dict.empty
    (identifiersList exp)


-- If suggestedName is not in existing names, returns it.
-- Otherwise appends a number (starting at i) that doesn't collide.
nonCollidingName : Ident -> Int -> Set.Set Ident -> Ident
nonCollidingName suggestedName i existingNames =
  if not (Set.member suggestedName existingNames) then
    suggestedName
  else
    let newName = suggestedName ++ (toString i) in
    if not (Set.member newName existingNames)
    then newName
    else nonCollidingName suggestedName (i+1) existingNames


renameIdentifierInPat old new pat =
  renameIdentifiersInPat (Dict.singleton old new) pat


renameIdentifiersInPat subst pat =
  let recurse = renameIdentifiersInPat subst in
  let recurseList = List.map recurse in
  let pat__ =
    case pat.val of
      PVar ws ident wd ->
        case Dict.get ident subst of
          Just new -> PVar ws new wd
          Nothing  -> pat.val

      PList ws1 pats ws2 Nothing ws3 ->
        PList ws1 (recurseList pats) ws2 Nothing ws3

      PList ws1 pats ws2 (Just pRest) ws3 ->
        PList ws1 (recurseList pats) ws2 (Just (recurse pRest)) ws3

      PAs ws1 ident ws2 innerPat ->
        case Dict.get ident subst of
          Just new -> PAs ws1 new ws2 (recurse innerPat)
          Nothing  -> PAs ws1 ident ws2 (recurse innerPat)

      _ ->
        pat.val
  in
  { pat | val = pat__ }


renameIdentifierInPats old new pats =
  List.map
    (renameIdentifierInPat old new)
    pats


renameIdentifiersInPats subst pats =
  List.map
    (renameIdentifiersInPat subst)
    pats


renameIdentifier : Ident -> Ident -> Exp -> Exp
renameIdentifier old new exp =
  renameIdentifiers (Dict.singleton old new) exp


renameIdentifiers : Dict.Dict Ident Ident -> Exp -> Exp
renameIdentifiers subst exp =
  let exp__Renamer e__ =
    case e__ of
      EVar ws ident ->
        case Dict.get ident subst of
          Just new -> EVar ws new
          Nothing  -> e__

      EFun ws1 pats body ws2 ->
        EFun ws1 (renameIdentifiersInPats subst pats) body ws2

      ECase ws1 e1 branches ws2 ->
        let branches_ =
          List.map
              (mapValField (\(Branch_ bws1 pat ei bws2) -> Branch_ bws1 (renameIdentifiersInPat subst pat) ei bws2))
              branches
        in
        ECase ws1 e1 branches_ ws2

      ELet ws1 kind rec pat assign body ws2 ->
        ELet ws1 kind rec (renameIdentifiersInPat subst pat) assign body ws2

      _ ->
        e__
  in
  mapExpViaExp__
    exp__Renamer
    exp


expToMaybeNum : Exp -> Maybe Num
expToMaybeNum exp =
  case exp.val.e__ of
    EConst _ n _ _ -> Just n
    _              -> Nothing


-- This is a rather generous definition of literal.
isLiteral : Exp -> Bool
isLiteral exp =
  Set.size (freeIdentifiers exp) == 0


-- Which vars in this exp refer to something outside this exp?
freeIdentifiers : Exp -> Set.Set Ident
freeIdentifiers exp =
  freeIdentifiers_ Set.empty exp


freeIdentifiers_ : Set.Set Ident -> Exp -> Set.Set Ident
freeIdentifiers_ boundIdentsSet exp =
  let recurse () =
    List.map (freeIdentifiers_ boundIdentsSet) (childExps exp)
    |> Utils.unionAll
  in
  case exp.val.e__ of
    EConst _ i l wd             -> Set.empty
    EBase _ v                   -> Set.empty
    EVar _ x                    -> if Set.member x boundIdentsSet then Set.empty else Set.singleton x
    EFun _ ps e _               -> freeIdentifiers_ (Set.union (identifiersSetInPats ps) boundIdentsSet) e
    EOp _ op es _               -> recurse ()
    EList _ es _ m _            -> recurse ()
    EIf _ e1 e2 e3 _            -> recurse ()
    ECase _ e1 bs _             ->
      let freeInScrutinee = freeIdentifiers_ boundIdentsSet e1 in
      let freeInEachBranch =
        (List.map .val bs)
        |> List.map (\(Branch_ _ bPat bExp _) -> freeIdentifiers_ (Set.union (identifiersSetInPat bPat) boundIdentsSet) bExp)
      in
      List.foldl
          Set.union
          freeInScrutinee
          freeInEachBranch

    ETypeCase _ p tbranches _   -> Set.union (Set.diff (identifiersSetInPat p) boundIdentsSet) (recurse ())
    -- ETypeCase _ e1 tbranches _  -> recurse ()
    EApp _ e1 es _              -> recurse ()
    ELet _ _ False p e1 e2 _    ->
      let freeInAssigns = freeIdentifiers_ boundIdentsSet e1 in
      let freeInBody    = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
      Set.union freeInAssigns freeInBody

    ELet _ _ True p e1 e2 _ ->
      let freeInAssigns = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e1 in
      let freeInBody    = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
      Set.union freeInAssigns freeInBody

    EComment _ _ e1       -> recurse ()
    EOption _ _ _ _ e1    -> recurse ()
    ETyp _ _ _ e1 _       -> recurse ()
    EColonType _ e1 _ _ _ -> recurse ()
    ETypeAlias _ _ _ e1 _ -> recurse ()
    -- EVal _                -> Debug.crash "LangTools.freeIdentifiers_: shouldn't have an EVal in given expression"
    -- EDict _               -> Debug.crash "LangTools.freeIdentifiers_: shouldn't have an EDict in given expression"


-- Renames free variables only, which is great!
-- Preserves EIds (for Brainstorm)
renameVarsUntilBound : Dict.Dict Ident Ident -> Exp -> Exp
renameVarsUntilBound renamings exp =
  let renamer newName e =
    -- let _ = Debug.log ("Renaming " ++ newName ++ " on") e in
    case e.val.e__ of
      EVar ws oldName -> replaceE__ e (EVar ws newName)
      _               -> Debug.crash <| "LangTools.renameVarsUntilBound: renamer should only be passed an EVar, but given: " ++ (toString e)
  in
  let fnSubst =
    renamings
    |> Dict.map (\_ newName -> (renamer newName))
  in
  transformVarsUntilBound fnSubst exp


-- Transforms only free variables.
-- Preserves EIds (for Brainstorm)
transformVarsUntilBound : Dict.Dict Ident (Exp -> Exp) -> Exp -> Exp
transformVarsUntilBound subst exp =
  let recurse e = transformVarsUntilBound subst e in
  let recurseWithout introducedIdents e =
    let newSubst =
      List.foldl
          Dict.remove
          subst
          (Set.toList introducedIdents)
    in
    if Dict.size newSubst == 0 then
      e
    else
      transformVarsUntilBound newSubst e
  in
  case exp.val.e__ of
    -- EVal _                      -> exp
    EConst _ _ _ _              -> exp
    EBase _ _                   -> exp
    EVar _ ident                ->
      case Dict.get ident subst of
        Just f  -> f exp
        Nothing -> exp

    EFun ws1 ps e ws2           -> replaceE__ exp (EFun ws1 ps (recurseWithout (identifiersSetInPats ps) e) ws2)
    EOp ws1 op es ws2           -> replaceE__ exp (EOp ws1 op (List.map recurse es) ws2)
    EList ws1 es ws2 m ws3      -> replaceE__ exp (EList ws1 (List.map recurse es) ws2 (Maybe.map recurse m) ws3)

    EIf ws1 e1 e2 e3 ws2        -> replaceE__ exp (EIf ws1 (recurse e1) (recurse e2) (recurse e3) ws2)
    ECase ws1 e1 bs ws2         ->
      let newScrutinee = recurse e1 in
      let newBranches =
        bs
        |> List.map
            (mapValField (\(Branch_ bws1 bPat bExp bws2) ->
              Branch_ bws1 bPat (recurseWithout (identifiersSetInPat bPat) bExp) bws2
            ))
      in
      replaceE__ exp (ECase ws1 newScrutinee newBranches ws2)


    ETypeCase ws1 scrutinee tbranches ws2 ->
      Debug.crash "need to change typecase scrutinee to expression; pluck from brainstorm branch"
    -- Brainstorm changed typecase scrutinee to an evaluated expression
    -- ETypeCase ws1 scrutinee tbranches ws2 ->
    --   let newScrutinee = recurse scrutinee in
    --   let newTBranches =
    --     tbranches
    --     |> List.map
    --         (mapValField (\(TBranch_ bws1 bType bExp bws2) ->
    --           TBranch_ bws1 bType (recurse bExp) bws2
    --         ))
    --   in
    --   replaceE__ exp (ETypeCase ws1 newScrutinee newTBranches ws2)

    EApp ws1 e1 es ws2              -> replaceE__ exp (EApp ws1 (recurse e1) (List.map recurse es) ws2)
    ELet ws1 kind False p e1 e2 ws2 ->
      replaceE__ exp (ELet ws1 kind False p (recurse e1) (recurseWithout (identifiersSetInPat p) e2) ws2)

    ELet ws1 kind True p e1 e2 ws2 ->
      replaceE__ exp (ELet ws1 kind True p (recurseWithout (identifiersSetInPat p) e1) (recurseWithout (identifiersSetInPat p) e2) ws2)

    EComment ws s e1                -> replaceE__ exp (EComment ws s (recurse e1))
    EOption ws1 s1 ws2 s2 e1        -> replaceE__ exp (EOption ws1 s1 ws2 s2 (recurse e1))
    ETyp ws1 pat tipe e ws2         -> replaceE__ exp (ETyp ws1 pat tipe (recurse e) ws2)
    EColonType ws1 e ws2 tipe ws3   -> replaceE__ exp (EColonType ws1 (recurse e) ws2 tipe ws3)
    ETypeAlias ws1 pat tipe e ws2   -> replaceE__ exp (ETypeAlias ws1 pat tipe (recurse e) ws2)

    -- EDict _                         -> Debug.crash "LangTools.transformVarsUntilBound: shouldn't have an EDict in given expression"



-- What variable names are in use at any of the given locations?
-- For help finding unused names during synthesis.
visibleIdentifiersAtEIds : Exp -> Set.Set EId -> Set.Set Ident
visibleIdentifiersAtEIds program eids =
  let programIdents = visibleIdentifiersAtEIds_ Set.empty program eids in
  let preludeIdents = List.map Tuple.first Eval.initEnv |> Set.fromList in
  Set.union programIdents preludeIdents


visibleIdentifiersAtEIds_ : Set.Set Ident -> Exp -> Set.Set EId -> Set.Set Ident
visibleIdentifiersAtEIds_ idents exp eids =
  let ret deeperIdents =
    -- If any child was a target EId, then deeperIdents is a superset of idents,
    -- so no need to union.
    if (0 == Set.size deeperIdents) && Set.member exp.val.eid eids then
      idents
    else
      deeperIdents
  in
  let recurse e =
    visibleIdentifiersAtEIds_ idents e eids
  in
  let recurseAllChildren () =
    childExps exp |> List.map recurse |> Utils.unionAll
  in
  let recurseWithNewIdents pats e =
    visibleIdentifiersAtEIds_ (Set.union (identifiersSetInPats pats) idents) e eids
  in
  case exp.val.e__ of
    -- EVal _           -> ret Set.empty
    EConst _ _ _ _   -> ret Set.empty
    EBase _ _        -> ret Set.empty
    EVar _ ident     -> ret Set.empty -- Referencing a var doesn't count.
    EFun _ ps e _    -> ret <| recurseWithNewIdents ps e
    EOp _ op es _    -> ret <| recurseAllChildren ()
    EList _ es _ m _ -> ret <| recurseAllChildren ()
    EIf _ e1 e2 e3 _ -> ret <| recurseAllChildren ()
    ECase _ e1 bs _  ->
      let scrutineeResult = recurse e1 in
      let branchResults =
        bs
        |> List.map .val
        |> List.map
            (\(Branch_ _ bPat bExp _) -> recurseWithNewIdents [bPat] bExp)
      in
      ret <| Utils.unionAll (scrutineeResult::branchResults)

    ETypeCase _ scrutinee tbranches _ -> ret <| recurseAllChildren ()
    EApp _ e1 es _                    -> ret <| recurseAllChildren ()
    ELet _ kind False p e1 e2 _       ->
      let assignResult = recurse e1 in
      let bodyResult   = recurseWithNewIdents [p] e2 in
      ret <| Set.union assignResult bodyResult

    ELet _ kind True p e1 e2 _ ->
      let assignResult = recurseWithNewIdents [p] e1 in
      let bodyResult   = recurseWithNewIdents [p] e2 in
      ret <| Set.union assignResult bodyResult

    EComment _ s e1           -> ret <| recurse e1
    EOption _ s1 _ s2 e1      -> ret <| recurse e1
    ETyp _ pat tipe e _       -> ret <| recurse e
    EColonType _ e _ tipe _   -> ret <| recurse e
    ETypeAlias _ pat tipe e _ -> ret <| recurse e

    -- EDict _                   -> Debug.crash "LangTools.visibleIdentifiersAtEIds_: shouldn't have an EDict in given expression"
