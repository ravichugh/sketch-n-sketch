module LangTools where

-- Most of these methods used to be in Lang.elm
--
-- Extracted to avoid circular dependencies.
--
-- Things in here are only used by LangTransform and ValueBasedTransform,
-- currently. Also used in InterfaceView to find unfrozen locs to animate in our
-- (possibly defunct) relate attributes selection screen.

import Eval
import Lang exposing (..)
import LangParser2
import Utils

import Dict
import Set


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
      let scopeNames' =
        case pat.val of
          PVar _ ident _  -> scopeNames ++ [ident]
          PAs _ ident _ _ -> scopeNames ++ [ident]
          _               -> scopeNames
      in
      -- Ident should only be added to assigns. Otherwise you get lots of junk
      -- names.
      (scopeNamesLocLiftedThrough_ targetLocId scopeNames' assigns) ++
      (scopeNamesLocLiftedThrough_ targetLocId scopeNames  body)

    EConst _ _ (locId, _, _) _ ->
       if locId == targetLocId
       then [scopeNames]
       else []

    _ ->
      let recurse exp = scopeNamesLocLiftedThrough_ targetLocId scopeNames exp in
      List.concatMap recurse (childExps exp)


preludeIdentifiers = Eval.initEnv |> List.map fst |> Set.fromList


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


renameIdentifierInPat old new pat =
  renameIdentifiersInPat (Dict.singleton old new) pat
  -- let recurse = renameIdentifierInPat old new in
  -- let recurseList = List.map recurse in
  -- let pat_' =
  --   case pat.val of
  --     PVar ws ident wd ->
  --       if ident == old
  --       then PVar ws new wd
  --       else pat.val
  --
  --     PList ws1 pats ws2 Nothing ws3 ->
  --       PList ws1 (recurseList pats) ws2 Nothing ws3
  --
  --     PList ws1 pats ws2 (Just pRest) ws3 ->
  --       PList ws1 (recurseList pats) ws2 (Just (recurse pRest)) ws3
  --
  --     PAs ws1 ident ws2 innerPat ->
  --       if ident == old
  --       then PAs ws1 new ws2 (recurse innerPat)
  --       else PAs ws1 ident ws2 (recurse innerPat)
  --
  --     _ ->
  --       pat.val
  -- in
  -- { pat | val = pat_' }


renameIdentifiersInPat subst pat =
  let recurse = renameIdentifiersInPat subst in
  let recurseList = List.map recurse in
  let pat_' =
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
  { pat | val = pat_' }


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
  -- let exp__Renamer e__ =
  --   case e__ of
  --     EVar ws ident ->
  --       if ident == old
  --       then EVar ws new
  --       else e__
  --
  --     EFun ws1 pats body ws2 ->
  --       EFun ws1 (renameIdentifierInPats old new pats) body ws2
  --
  --     ECase ws1 e1 branches ws2 ->
  --       let branches' =
  --         List.map
  --             (mapValField (\(Branch_ bws1 pat ei bws2) -> Branch_ bws1 (renameIdentifierInPat old new pat) ei bws2))
  --             branches
  --       in
  --       ECase ws1 e1 branches' ws2
  --
  --     ELet ws1 kind rec pat assign body ws2 ->
  --       ELet ws1 kind rec (renameIdentifierInPat old new pat) assign body ws2
  --
  --     _ ->
  --       e__
  -- in
  -- mapExpViaExp__
  --   exp__Renamer
  --   exp


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
        let branches' =
          List.map
              (mapValField (\(Branch_ bws1 pat ei bws2) -> Branch_ bws1 (renameIdentifiersInPat subst pat) ei bws2))
              branches
        in
        ECase ws1 e1 branches' ws2

      ELet ws1 kind rec pat assign body ws2 ->
        ELet ws1 kind rec (renameIdentifiersInPat subst pat) assign body ws2

      _ ->
        e__
  in
  mapExpViaExp__
    exp__Renamer
    exp


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
    EIndList _ rs _             -> recurse ()
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

    ETypeCase _ e1 tbranches _  -> recurse ()
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
    EVal _                -> Debug.crash "Brainstorm.freeIdentifiers_: shouldn't have an EVal in given expression"
    EDict _               -> Debug.crash "Brainstorm.freeIdentifiers_: shouldn't have an EDict in given expression"


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

  -- let recurseSimple = renameVarsUntilBound renamings in
  -- if Dict.size renamings == 0 then
  --   exp
  -- else
  --   case exp.val.e__ of
  --     EConst _ _ _ _ -> exp
  --     EBase _ _      -> exp
  --     EVar ws1 ident ->
  --       case Dict.get ident renamings of
  --         Just newIdent -> replaceE__ exp (EVar ws1 newIdent)
  --         Nothing       -> exp
  --
  --     EFun ws1 pats body ws2 ->
  --       let renamings' = identifiersSetInPats pats |> Set.foldl Dict.remove renamings in
  --       replaceE__ exp (EFun ws1 pats (renameVarsUntilBound renamings' body) ws2)
  --
  --     EOp ws1 op argExps ws2 ->
  --       replaceE__ exp (EOp ws1 op (List.map recurseSimple argExps) ws2)
  --
  --     EList ws1 headExps ws2 maybeRestExp ws3 ->
  --       replaceE__ exp (EList ws1 (List.map recurseSimple headExps) ws2 (Maybe.map maybeRestExp) ws3)
  --
  --     EIndList ws1 ranges ws2 ->
  --       let rangeRecurse range_ =
  --         case range_ of
  --           RInterval e1 ws e2 -> RInterval (recurseSimple e1) ws (recurseSimple e2)
  --           RPoint e1          -> RPoint (recurseSimple e1)
  --       in
  --       replaceE__ exp (EIndList ws1 (List.map (mapValField rangeRecurse) ranges) ws2)
  --
  --     EIf ws1 e1 e2 e3 ws2 ->
  --       replaceE__ exp (EIf ws1 (recurseSimple e1) (recurseSimple e2) (recurseSimple e3) ws2)
  --
  --     ECase ws1 scrutinee branches ws2 ->
  --       let branchRecurse (Branch_ bws1 bPat bExp bws2) =
  --         let renamings' = identifiersSetInPat bPat |> Set.foldl Dict.remove renamings in
  --         Branch_ bws1 bPat (renameVarsUntilBound renamings' bExp) bws2
  --       in
  --       replaceE__ exp (ECase ws1 (recurseSimple scrutinee) (List.map (mapValField branchRecurse) branches) ws2)
  --
  --     ETypeCase ws1 scrutinee tbranches ws2 ->
  --       let tbranchRecurse (TBranch_ bws1 tipe bExp bws2) =
  --         TBranch_ bws1 tipe (recurseSimple bExp) bws2
  --       in
  --       replaceE__ exp (ETypeCase ws1 (recurseSimple scrutinee) (List.map (mapValField tbranchRecurse) tbranches) ws2)
  --
  --     EApp ws1 funcExp argExps ws2 ->
  --
  --
  --     ELet _ _ False p e1 e2 _    ->
  --       let freeInAssigns = freeIdentifiers_ boundIdentsSet e1 in
  --       let freeInBody    = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
  --       Set.union freeInAssigns freeInBody
  --
  --     ELet _ _ True p e1 e2 _ ->
  --       let freeInAssigns = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e1 in
  --       let freeInBody    = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
  --       Set.union freeInAssigns freeInBody
  --
  --     EComment _ _ e1       -> recurse ()
  --     EOption _ _ _ _ e1    -> recurse ()
  --     ETyp _ _ _ e1 _       -> recurse ()
  --     EColonType _ e1 _ _ _ -> recurse ()
  --     ETypeAlias _ _ _ e1 _ -> recurse ()
  --     EVal _                -> Debug.crash "Brainstorm.freeIdentifiers_: shouldn't have an EVal in given expression"
  --     EDict _               -> Debug.crash "Brainstorm.freeIdentifiers_: shouldn't have an EDict in given expression"


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
    EVal _                      -> exp
    EConst _ _ _ _              -> exp
    EBase _ _                   -> exp
    EVar _ ident                ->
      case Dict.get ident subst of
        Just f  -> f exp
        Nothing -> exp

    EFun ws1 ps e ws2           -> replaceE__ exp (EFun ws1 ps (recurseWithout (identifiersSetInPats ps) e) ws2)
    EOp ws1 op es ws2           -> replaceE__ exp (EOp ws1 op (List.map recurse es) ws2)
    EList ws1 es ws2 m ws3      -> replaceE__ exp (EList ws1 (List.map recurse es) ws2 (Maybe.map recurse m) ws3)
    EIndList ws1 rs ws2         ->
      let newRanges =
        rs
        |> List.map
            (mapValField (\range ->
              case range of
                RInterval e1 rws e2 -> RInterval (recurse e1) rws (recurse e2)
                RPoint e1           -> RPoint (recurse e1)
            ))
      in
      replaceE__ exp (EIndList ws1 newRanges ws2)

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
      let newScrutinee = recurse scrutinee in
      let newTBranches =
        tbranches
        |> List.map
            (mapValField (\(TBranch_ bws1 bType bExp bws2) ->
              TBranch_ bws1 bType (recurse bExp) bws2
            ))
      in
      replaceE__ exp (ETypeCase ws1 newScrutinee newTBranches ws2)

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

    EDict _                         -> Debug.crash "LangTools.transformVarsUntilBound: shouldn't have an EDict in given expression"
