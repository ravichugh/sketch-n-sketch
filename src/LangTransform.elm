module LangTransform (simplify) where

import Set

import Lang exposing (..)
import LangUnparser exposing (..)
import Utils


-- Removes unused variables
-- Inlines variables assigned directly to another variable and not otherwise used
simplify : Exp -> Exp
simplify exp =
  let simplified = exp |> inlineTrivialRenamings |> removeUnusedVars in
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
        let identsAndAssigns =
          case (pat.val, assign.val.e__) of
            -- Simple assignment.
            (PVar _ ident _, _) ->
              [(ident, assign)]

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
        in
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
              identsAndAssigns
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
