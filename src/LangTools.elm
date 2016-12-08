module LangTools exposing (..)

-- Most of these methods used to be in Lang.elm
--
-- Extracted to avoid circular dependencies.
--
-- Things in here are only used by LangTransform and ValueBasedTransform,
-- currently. Also used in InterfaceView to find unfrozen locs to animate in our
-- (possibly defunct) relate attributes selection screen.

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


-- Is the expression in the body of only defs/comments/options?
--
-- The "top level" is a single path on the tree, so walk it and look
-- for the target expression.
isTopLevel : Exp -> Exp -> Bool
isTopLevel exp program =
  if exp == program then
    True
  else
    case program.val.e__ of
      ELet _ Def _ _ _ body _ -> isTopLevel exp body
      EComment _ _ e          -> isTopLevel exp e
      EOption _ _ _ _ e       -> isTopLevel exp e
      _                       -> False


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


identifiersSet : Exp -> Set.Set Ident
identifiersSet exp =
  identifiersList exp
  |> Set.fromList


identifiersSetInPat : Pat -> Set.Set Ident
identifiersSetInPat pat =
  identifiersListInPat pat
  |> Set.fromList


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


identifiersListInPat : Pat -> List Ident
identifiersListInPat pat =
  case pat.val of
    PVar _ ident _              -> [ident]
    PList _ pats _ (Just pat) _ -> List.concatMap identifiersListInPat (pat::pats)
    PList _ pats _ Nothing    _ -> List.concatMap identifiersListInPat pats
    PAs _ ident _ pat           -> ident::(identifiersListInPat pat)
    _                           -> []


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
  let recurse = renameIdentifierInPat old new in
  let recurseList = List.map recurse in
  let pat__ =
    case pat.val of
      PVar ws ident wd ->
        if ident == old
        then PVar ws new wd
        else pat.val

      PList ws1 pats ws2 Nothing ws3 ->
        PList ws1 (recurseList pats) ws2 Nothing ws3

      PList ws1 pats ws2 (Just pRest) ws3 ->
        PList ws1 (recurseList pats) ws2 (Just (recurse pRest)) ws3

      PAs ws1 ident ws2 innerPat ->
        if ident == old
        then PAs ws1 new ws2 (recurse innerPat)
        else PAs ws1 ident ws2 (recurse innerPat)

      _ ->
        pat.val
  in
  { pat | val = pat__ }


renameIdentifierInPats old new pats =
  List.map
    (renameIdentifierInPat old new)
    pats


renameIdentifier : Ident -> Ident -> Exp -> Exp
renameIdentifier old new exp =
  let exp__Renamer e__ =
    case e__ of
      EVar ws ident ->
        if ident == old
        then EVar ws new
        else e__

      EFun ws1 pats body ws2 ->
        EFun ws1 (renameIdentifierInPats old new pats) body ws2

      ECase ws1 e1 branches ws2 ->
        let branches_ =
          List.map
              (mapValField (\(Branch_ bws1 pat ei bws2) -> Branch_ bws1 (renameIdentifierInPat old new pat) ei bws2))
              branches
        in
        ECase ws1 e1 branches_ ws2

      ETypeCase ws1 pat tbranches ws2 ->
        ETypeCase ws1 (renameIdentifierInPat old new pat) tbranches ws2

      ELet ws1 kind rec pat assign body ws2 ->
        ELet ws1 kind rec (renameIdentifierInPat old new pat) assign body ws2

      _ ->
        e__
  in
  mapExpViaExp__
    exp__Renamer
    exp
