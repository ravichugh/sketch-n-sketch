--
-- LangTransform
--
-- Transformations of the code that do not change its output.
--

module LangTransform exposing (cleanCode, simplify, removeExtraPostfixes)

import Set
import Dict
import String

import Lang exposing (..)
import LangTools exposing (..)
import LangUnparser exposing (..)
import LangParser2
import OurParser2
import Utils


cleanCode : Exp -> Exp
cleanCode exp =
  exp
  |> simplify
  |> removeExtraPostfixes ["_orig", "'"]
  |> LangParser2.freshen

-- Rename e.g. `x_orig_orig_orig` to `x_orig` (presuming there
-- is no `x_orig_orig` nor `x_orig` but there is `x`.)
--
-- Since renamings could conflict if performed in batches,
-- we rename one variable at a time and then re-evaluate
-- the valid renamings, repeating until there are no more
-- renamings to perform.
removeExtraPostfixes : List String -> Exp -> Exp
removeExtraPostfixes postfixes exp =
  let usedNames = identifiersSet exp in
  let oldAndNewNames =
    List.concatMap
        (\ident ->
          List.concatMap
            (\postfix ->
              if String.endsWith postfix ident
              then [(ident, (String.dropRight (String.length postfix) ident))]
              else []
            )
            postfixes
        )
        (Set.toList usedNames)
  in
  let renameToPerform =
    Utils.findFirst
        (\(_, newName) -> not <| Set.member newName usedNames)
        oldAndNewNames
  in
  case renameToPerform of
    Just (oldName, newName) ->
      let exp_ = renameIdentifier oldName newName exp in
      removeExtraPostfixes postfixes exp_

    Nothing ->
      exp


-- 1. Removes unused variables.
-- 2. Inlines variables assigned directly to another variable and not otherwise
--    used.
-- 3. Variables that are merely a renaming of a variable in the higher scope are
--    renamed to the name from the higher scope.
--
-- (This means that, effectively, when an outer variable is renamed to some
-- inner variable: if the outer variable is only used once, the inner variable
-- name wins, otherwise the outer variable name wins.)
simplify : Exp -> Exp
simplify exp =
  -- Prefer the first two transformations over the third.
  let repeatedlyApplyFirstTwo e =
    let firstTwoSimplified =
      e
      |> inlineTrivialRenamings
      |> removeUnusedVars
    in
    if firstTwoSimplified == e then
      e
    else
      repeatedlyApplyFirstTwo firstTwoSimplified
  in
  let simplified =
    repeatedlyApplyFirstTwo exp
    |> changeRenamedVarsToOuter
  in
  if simplified == exp then
    exp
  else
    simplify simplified


removeUnusedVars : Exp -> Exp
removeUnusedVars exp =
  let remover e__ =
    case e__ of
      ELet ws1 letKind rec pat assign body ws2 ->
        let usedNames = identifiersSet body in
        let letRemoved =
          body.val.e__
        in
        case (pat.val, assign.val.e__) of
          -- Simple assignment.
          (PVar _ ident _, _) ->
            if Set.member ident usedNames then
              e__
            else
              letRemoved

          -- Check if as-pattern is used
          (PAs asWs ident _ innerPat, _) ->
            if Set.member ident usedNames then
              e__
            else
              ELet ws1 letKind rec (replacePrecedingWhitespacePat asWs innerPat) assign body ws2

          -- List assignment, no tail.
          (PList pws1 pats pws2 Nothing pws3, EList aws1 assigns aws2 Nothing aws3) ->
            if List.length pats /= List.length assigns then
              e__
            else
              let patsAssigns = Utils.zip pats assigns in
              let usedPatsAssigns =
                List.filter
                    (\(pat, assign) ->
                      case pat.val of
                        PVar _ ident _ -> Set.member ident usedNames
                        _              -> True
                    )
                    patsAssigns
              in
              case List.length usedPatsAssigns of
                0 ->
                  letRemoved

                1 ->
                  let (thePat, theAssign) = Utils.head_ usedPatsAssigns in
                  let newPat    = replacePrecedingWhitespacePat pws1 thePat in
                  let newAssign = replacePrecedingWhitespace aws1 theAssign in
                  ELet ws1 letKind rec newPat newAssign body ws2

                _ ->
                  if List.length usedPatsAssigns == List.length pats then
                    e__
                  else
                    let (usedPats, usedAssigns) = List.unzip usedPatsAssigns in
                    let newPat    = withDummyRange <| PList pws1 (cleanupPatListWhitespace " " usedPats) pws2 Nothing pws3 in
                    let newAssign = withDummyPos   <| EList aws1 (cleanupListWhitespace " " usedAssigns) aws2 Nothing aws3 in
                    ELet ws1 letKind rec newPat newAssign body ws2

          _ ->
            e__

      _ ->
        e__
  in
  mapExpViaExp__ remover exp


-- Single var assignment and one-to-one list assignments.
--
-- Returns [(identifer, assignedExpression), ... ]
simpleIdentsAndAssigns : Pat -> Exp -> List (Ident, Exp)
simpleIdentsAndAssigns letPat letAssign =
  case (letPat.val, letAssign.val.e__) of
    -- Simple assignment.
    (PVar _ ident _, _) ->
      [(ident, letAssign)]

    -- List assignment, no tail.
    (PList pws1 pats pws2 Nothing pws3, EList aws1 assigns aws2 Nothing aws3) ->
      let patsAssigns = Utils.zip pats assigns in
      let simplePatsAssigns =
        List.filterMap
            (\(pat, assign) ->
              case pat.val of
                PVar _ ident _ -> Just (ident, assign)
                _              -> Nothing
            )
            patsAssigns
      in
      simplePatsAssigns

    _ ->
      []


-- Inline single-use variables that are merely assigned to another variable.
inlineTrivialRenamings : Exp -> Exp
inlineTrivialRenamings exp =
  let inlineReplaceIfTrivialRename targetIdent newExp e__ =
    case e__ of
      ELet ws1 letKind rec pat assign body ws2 ->
        case (pat.val, assign.val.e__) of
          -- Simple assignment.
          (PVar _ _ _, EVar oldWs assignIdent) ->
            if assignIdent == targetIdent then
              let newExpAdjustedWs = replacePrecedingWhitespace oldWs newExp in
              ELet ws1 letKind rec pat newExpAdjustedWs body ws2
            else
              e__

          -- List assignment, no tail.
          (PList _ _ _ Nothing _, EList aws1 assigns aws2 Nothing aws3) ->
            let newAssigns =
              List.map
                  (\assignExp ->
                    case assignExp.val.e__ of
                      EVar _ assignIdent ->
                        if assignIdent == targetIdent then
                          let oldPrecedingWs = precedingWhitespace assignExp in
                          replacePrecedingWhitespace oldPrecedingWs newExp
                        else
                          assignExp

                      _ ->
                        assignExp
                  )
                  assigns
            in
            let newAssignsListExp =
              withDummyPos <| EList aws1 newAssigns aws2 Nothing aws3
            in
            ELet ws1 letKind rec pat newAssignsListExp body ws2

          _ ->
            e__

      _ ->
        e__
  in
  let inliner e__ =
    case e__ of
      ELet ws1 letKind rec pat assign body ws2 ->
        let nameCounts = identifierCounts body in
        let letRemoved newBody =
          let oldPrecedingWs = precedingWhitespaceExp__ e__ in
          (replacePrecedingWhitespace oldPrecedingWs newBody).val.e__
        in
        let identsAndAssignsInliningCandidates =
          List.filter
              (\(ident, assign) ->
                1 == (Utils.getWithDefault ident 0 nameCounts)
              )
              (simpleIdentsAndAssigns pat assign)
        in
        let newBody =
          List.foldl
              (\(ident, assign) resultExp ->
                mapExpViaExp__ (inlineReplaceIfTrivialRename ident assign) resultExp
              )
              body
              identsAndAssignsInliningCandidates
        in
        ELet ws1 letKind rec pat assign newBody ws2

      _ ->
        e__
  in
  mapExpViaExp__ inliner exp


-- If a variable is merely a renaming of a variable in a higher scope, all its
-- occurances are replace by the name from the higher scope.
--
-- Example:
--
-- (let x 6
-- (let redundant x
--   (+ redundant 2)))
--
-- ...becomes...
--
-- (let x 6
-- (let redundant x
--   (+ x 2)))
--
-- The `redundant` variable is now unsused and can be removed with
-- removeUnusedVars.
--
changeRenamedVarsToOuter : Exp -> Exp
changeRenamedVarsToOuter exp =
  changeRenamedVarsToOuter_ Dict.empty exp


changeRenamedVarsToOuter_ renamings exp =
  let wrap e__ = OurParser2.WithInfo (Exp_ e__ exp.val.eid) exp.start exp.end in
  let recurse = changeRenamedVarsToOuter_ renamings in
  let e__New =
    let e__ = exp.val.e__ in
    let removeIdentsFromRenaming identsToRemove renamings =
      Dict.filter
          (\oldName newName ->
            (not <| Set.member oldName identsToRemove) &&
            (not <| Set.member newName identsToRemove)
          )
          renamings
    in
    case e__ of
      EConst ws n loc wd -> e__
      EBase ws baseV     -> e__

      EVar ws ident ->
        case Dict.get ident renamings of
          Just newName -> EVar ws newName
          Nothing      -> e__

      ELet ws1 letKind rec pat assign body ws2 ->
        -- Newly assigned variables that shadow an outer variable should be
        -- removed from the renaming dictionary because that name means
        -- something else now, now what's in the dictionary.
        let newlyAssignedIdents = identifiersSetInPat pat in
        let renamingsShadowsRemoved = removeIdentsFromRenaming newlyAssignedIdents renamings in
        let assign_ =
           if rec then
             changeRenamedVarsToOuter_ renamingsShadowsRemoved assign
           else
             recurse assign
        in
        let identsAndAssigns = simpleIdentsAndAssigns pat assign_ in
        let simpleRenamings =
          List.filterMap
              (\(ident, assign) ->
                case assign.val.e__ of
                  EVar _ assignIdent -> Just (ident, assignIdent)
                  _                  -> Nothing
              )
              identsAndAssigns
        in
        let renamings_ = Dict.union (Dict.fromList simpleRenamings) renamingsShadowsRemoved in
        let body_ = changeRenamedVarsToOuter_ renamings_ body in
        ELet ws1 letKind rec pat assign_ body_ ws2

      EFun ws1 pats body ws2  ->
        let newlyAssignedIdents =
          List.map identifiersSetInPat pats
          |> List.foldl Set.union Set.empty
        in
        let renamingsShadowsRemoved = removeIdentsFromRenaming newlyAssignedIdents renamings in
        EFun ws1 pats (changeRenamedVarsToOuter_ renamingsShadowsRemoved body) ws2

      EApp ws1 e1 es ws2     -> EApp ws1 (recurse e1) (List.map recurse es) ws2
      EOp ws1 op es ws2      -> EOp ws1 op (List.map recurse es) ws2
      EList ws1 es ws2 m ws3 -> EList ws1 (List.map recurse es) ws2 (Utils.mapMaybe recurse m) ws3
      EIf ws1 e1 e2 e3 ws2      -> EIf ws1 (recurse e1) (recurse e2) (recurse e3) ws2
      ECase ws1 e1 branches ws2 ->
        -- TODO remove branch pat vars from renamings here (shadow
        -- checking)
        let newBranches =
          List.map
              (mapValField (\(Branch_ bws1 branchPat branchBody bws2) ->
                let newlyAssignedIdents = identifiersSetInPat branchPat in
                let renamingsShadowsRemoved = removeIdentsFromRenaming newlyAssignedIdents renamings in
                Branch_ bws1 branchPat (changeRenamedVarsToOuter_ renamingsShadowsRemoved branchBody) bws2
              ))
              branches
        in
        ECase ws1 (recurse e1) newBranches ws2
      ETypeCase ws1 pat tbranches ws2 ->
        -- TODO remove branch pat vars from renamings here (shadow
        -- checking)
        let newBranches =
          List.map
              (mapValField (\(TBranch_ bws1 branchType branchBody bws2) ->
                -- let newlyAssignedIdents = identifiersSetInPat branchPat in
                -- let renamingsShadowsRemoved = removeIdentsFromRenaming newlyAssignedIdents renamings in
                TBranch_ bws1 branchType (recurse branchBody) bws2
              ))
              tbranches
        in
        ETypeCase ws1 pat newBranches ws2
      EComment ws s e1              -> EComment ws s (recurse e1)
      EOption ws1 s1 ws2 s2 e1      -> EOption ws1 s1 ws2 s2 (recurse e1)
      ETyp ws1 pat tipe e ws2       -> ETyp ws1 pat tipe (recurse e) ws2
      EColonType ws1 e ws2 tipe ws3 -> EColonType ws1 (recurse e) ws2 tipe ws3
      ETypeAlias ws1 pat tipe e ws2 -> ETypeAlias ws1 pat tipe (recurse e) ws2
  in
  wrap e__New

