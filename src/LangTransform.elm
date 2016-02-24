module LangTransform (simplify) where

import Set
import Dict

import Lang exposing (..)
import LangUnparser exposing (..)
import OurParser2
import Utils


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


removeUnusedVars exp =
  let remover e__ =
    case e__ of
      ELet ws1 letKind rec pat assign body ws2 ->
        let usedNames = Lang.identifiersSet body in
        let letRemoved =
          let oldPreceedingWs = preceedingWhitespaceExp__ e__ in
          (replacePreceedingWhitespace oldPreceedingWs body).val.e__
        in
        case (pat.val, assign.val.e__) of
          -- Simple assignment.
          (PVar _ ident _, _) ->
            if Set.member ident usedNames then
              e__
            else
              letRemoved

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
                  let newPat    = replacePreceedingWhitespacePat pws1 thePat in
                  let newAssign = replacePreceedingWhitespace aws1 theAssign in
                  ELet ws1 letKind rec newPat newAssign body ws2

                _ ->
                  let newPat    = withDummyRange <| PList pws1 (List.map fst usedPatsAssigns) pws2 Nothing pws3 in
                  let newAssign = withDummyPos   <| EList aws1 (List.map snd usedPatsAssigns) aws2 Nothing aws3 in
                  ELet ws1 letKind rec newPat newAssign body ws2

          _ ->
            e__

      _ ->
        e__
  in
  mapExpViaExp__ remover exp


-- Single var assignment and one-to-one list assignments.
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
inlineTrivialRenamings exp =
  let inlineReplaceIfTrivialRename targetIdent newExp e__ =
    case e__ of
      ELet ws1 letKind rec pat assign body ws2 ->
        case (pat.val, assign.val.e__) of
          -- Simple assignment.
          (PVar _ _ _, EVar oldWs assignIdent) ->
            if assignIdent == targetIdent then
              let newExpAdjustedWs = replacePreceedingWhitespace oldWs newExp in
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
                          let oldPreceedingWs = preceedingWhitespace assignExp in
                          replacePreceedingWhitespace oldPreceedingWs newExp
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
        let nameCounts = Lang.identifierCounts body in
        let letRemoved newBody =
          let oldPreceedingWs = preceedingWhitespaceExp__ e__ in
          (replacePreceedingWhitespace oldPreceedingWs newBody).val.e__
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


changeRenamedVarsToOuter exp =
  changeRenamedVarsToOuter_ Dict.empty exp


changeRenamedVarsToOuter_ renamings exp =
  let wrap e__ = OurParser2.WithInfo (Exp_ e__ exp.val.eid) exp.start exp.end in
  let recurse = changeRenamedVarsToOuter_ renamings in
  let e__' =
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
          Nothing      -> exp.val.e__

      ELet ws1 letKind rec pat assign body ws2 ->
        -- Newly assigned variables that shadow an outer variable should be
        -- removed from the renaming dictionary because that name means
        -- something else now, now what's in the dictionary.
        let newlyAssignedIdents = identifiersSetInPat pat in
        let renamingsShadowsRemoved = removeIdentsFromRenaming newlyAssignedIdents renamings in
        let assign' =
           if rec then
             changeRenamedVarsToOuter_ renamingsShadowsRemoved assign
           else
             recurse assign
        in
        let identsAndAssigns = simpleIdentsAndAssigns pat assign' in
        let simpleRenamings =
          List.filterMap
              (\(ident, assign) ->
                case assign.val.e__ of
                  EVar _ assignIdent -> Just (ident, assignIdent)
                  _                  -> Nothing
              )
              identsAndAssigns
        in
        let renamings' = Dict.union (Dict.fromList simpleRenamings) renamingsShadowsRemoved in
        let body' = changeRenamedVarsToOuter_ renamings' body in
        ELet ws1 letKind rec pat assign' body' ws2

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
      EIndList ws1 rs ws2    ->
        let rangeRecurse r_ = case r_ of
          Interval e1 ws e2 -> Interval (recurse e1) ws (recurse e2)
          Point e1          -> Point (recurse e1)
        in
        EIndList ws1 (List.map (mapValField rangeRecurse) rs) ws2
      EIf ws1 e1 e2 e3 ws2      -> EIf ws1 (recurse e1) (recurse e2) (recurse e3) ws2
      ECase ws1 e1 branches ws2 ->
        -- May need to remove branch pat vars from renamings here (shadow
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
      EComment ws s e1         -> EComment ws s (recurse e1)
      EOption ws1 s1 ws2 s2 e1 -> EOption ws1 s1 ws2 s2 (recurse e1)
  in
  wrap e__'

