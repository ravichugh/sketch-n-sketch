--
-- LangSimplify
--
-- Simplifications of the code that do not change its output.
--

module LangSimplify exposing
  ( cleanCode
  , simplify
  , removeExtraPostfixes
  , removeUnusedLetPats
  , removeUnusedLetPatsMatching
  , simplifyAssignments
  , changeRenamedVarsToOuter
  )

import Set
import Dict
import String

import Lang exposing (..)
import LangTools exposing (..)
import LangUnparser exposing (..)
import LangUtils exposing (..)
import ElmParser as Parser
import Utils


cleanCode : Exp -> Exp
cleanCode program =
  program
  |> simplify
  |> removeExtraPostfixes ["_orig", "'"]
  |> mapExpTopDown (\e -> if isLet e then reflowLetWhitespace program e else e)
  |> Parser.freshen


-- Rename e.g. `x_orig_orig_orig` to `x_orig` (presuming there
-- is no `x_orig_orig` nor `x_orig` but there is `x`.)
removeExtraPostfixes : List String -> Exp -> Exp
removeExtraPostfixes postfixes program =
  let maybeNewName oldName =
    postfixes
    |> Utils.findFirst (\postfix -> String.endsWith postfix oldName)
    |> Maybe.map (\postfix -> String.dropRight (String.length postfix) oldName)
  in
  -- Does not remove postfixes if bound in parameter lists and case branches and recursive lets
  let newProgram =
    program
    |> mapExpViaExp__
        (\e__ ->
          case e__ of
            ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse assign])]) wsIn body ->
              let (newPat, newBody) =
                identifiersListInPat pat
                |> List.foldl
                    (\oldName (pat, body) ->
                      case maybeNewName oldName of
                        Nothing -> (pat, body)
                        Just newName ->
                          -- First condition here is technically not supurflous: can happen if a variable is unused.
                          if (not <| List.member newName (identifiersListInPat pat)) && (not <| Set.member newName <| visibleIdentifiersAtPredicateNoPrelude body (\e -> expToMaybeIdent e == Just oldName)) then
                            (renameIdentifierInPat oldName newName pat, renameVarUntilBound oldName newName body)
                          else
                            (pat, body)
                    )
                    (pat, body)
              in
              if pat /= newPat then
                ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat newPat fs wse assign])]) wsIn newBody
              else
                e__

            _ ->
              e__
        )
  in
  if identifiersList newProgram == identifiersList program then
    program
  else
    removeExtraPostfixes postfixes newProgram


-- Old version; doesn't handle scoping.
-- removeExtraPostfixes : List String -> Exp -> Exp
-- removeExtraPostfixes postfixes exp =
--   let usedNames = identifiersSet exp in
--   let oldAndNewNames =
--     List.concatMap
--         (\ident ->
--           List.concatMap
--             (\postfix ->
--               if String.endsWith postfix ident
--               then [(ident, (String.dropRight (String.length postfix) ident))]
--               else []
--             )
--             postfixes
--         )
--         (Set.toList usedNames)
--   in
--   let renameToPerform =
--     Utils.findFirst
--         (\(_, newName) -> not <| Set.member newName usedNames)
--         oldAndNewNames
--   in
--   case renameToPerform of
--     Just (oldName, newName) ->
--       let exp_ = renameIdentifier oldName newName exp in
--       removeExtraPostfixes postfixes exp_
--
--     Nothing ->
--       exp


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
      |> removeUnusedLetPats
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


removeUnusedLetPats : Exp -> Exp
removeUnusedLetPats exp =
  removeUnusedLetPatsMatching (always True) exp


removeUnusedLetPatsMatching : (Pat -> Bool) -> Exp -> Exp
removeUnusedLetPatsMatching predicate exp =
  let remover e__ =
    case e__ of
       ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse assign])]) wsIn body ->
        let usedNames = freeIdentifiers body in
        let letRemoved =
          body.val.e__
        in
        case (pat.val.p__, assign.val.e__) of
          -- Simple assignment.
          (PVar _ ident _, _) ->
            if Set.member ident usedNames || not (predicate pat) then
              e__
            else
              letRemoved

          -- Check if as-pattern is used
          (PAs wsBefore innerPat1 wsAs innerPat2, _) ->
            let innerPat1useless = not <| case innerPat1.val.p__ of
               PVar _ ident _ -> Set.member ident usedNames || not (predicate pat)
               _ -> True
            in
            let innerPat2useless = not <| case innerPat2.val.p__ of
               PVar _ ident _ -> Set.member ident usedNames || not (predicate pat)
               _ -> True
            in
            if innerPat1useless && innerPat2useless then letRemoved
            else if innerPat1useless then
              ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat (replacePrecedingWhitespacePat wsBefore.val innerPat2) fs wse assign])]) wsIn body
            else if innerPat2useless then
              ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat (replacePrecedingWhitespacePat wsBefore.val innerPat1) fs wse assign])]) wsIn body
            else e__

          -- List assignment, no tail.
          (PList pws1 pats pws2 Nothing pws3, EList aws1 assigns aws2 Nothing aws3) ->
            if List.length pats /= List.length assigns then
              e__
            else
              let patsAssigns = Utils.zip pats (Utils.listValues assigns) in
              let usedPatsAssigns =
                List.filter
                    (\(pat, assign) ->
                      case pat.val.p__ of
                        PVar _ ident _ -> Set.member ident usedNames || not (predicate pat)
                        _              -> True
                    )
                    patsAssigns
              in
              case List.length usedPatsAssigns of
                0 ->
                  letRemoved

                1 ->
                  let (thePat, theAssign) = Utils.head_ usedPatsAssigns in
                  let newPat    = replacePrecedingWhitespacePat pws1.val thePat in
                  let newAssign = replacePrecedingWhitespace aws1.val theAssign in
                  ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat newPat fs wse newAssign])]) wsIn body

                _ ->
                  if List.length usedPatsAssigns == List.length pats then
                    e__
                  else
                    let (usedPats, usedAssigns) = List.unzip usedPatsAssigns in
                    let newPat    = replaceP__ pat    <| PList pws1 (usedPats    |> imitatePatListWhitespace pats)    pws2 Nothing pws3 in
                    let newAssign = replaceE__ assign <| EList aws1 (Utils.listValuesMake assigns (usedAssigns |> imitateExpListWhitespace (Utils.listValues assigns))) aws2 Nothing aws3 in
                    ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat newPat fs wse newAssign])]) wsIn body

          _ ->
            e__

       _ ->
        e__
  in
  mapExpViaExp__ remover exp


-- Remove assignments of [] to [] and of *RemoveMe* to whatever (produced by CodeMotion.pluck)
-- Flatten assignment of singleton [exp] to singleton [var] (also often produced by CodeMotion.pluck)
simplifyPatBoundExp : Pat -> Exp -> Maybe (Pat, Exp)
simplifyPatBoundExp pat boundExp =
  case (pat.val.p__, boundExp.val.e__) of
    (PVar ws1 "*RemoveMe*" wd, _) ->
      Nothing

    (PAs ws1 childPat1 ws2 childPat2, _) ->
      case simplifyPatBoundExp childPat2 boundExp of
        Just (newChildPat, newBoundExp) -> Just (replaceP__ pat (PAs ws1 childPat1 ws2 newChildPat), newBoundExp)
        Nothing                         ->
          case simplifyPatBoundExp childPat1 boundExp of
            Just (newChildPat, newBoundExp) -> Just (replaceP__ pat (PAs ws1 newChildPat ws2 childPat2), newBoundExp)
            Nothing                         -> Nothing

    ( PList pws1 ps pws2 maybePTail pws3
    , EList ews1 es ews2 maybeETail ews3
    ) ->
      let (newPs, newEs) =
        Utils.filterMapTogetherPreservingLeftovers simplifyPatBoundExp ps (Utils.listValues es)
      in
      let (newMaybePTail, newMaybeETail) =
        case (maybePTail, maybeETail) of
          (Just pTail, Just eTail) ->
            case simplifyPatBoundExp pTail eTail of
              Just (newPTail, newETail) -> (Just newPTail, Just newETail)
              Nothing                   -> (Nothing, Nothing)

          _ ->
            (maybePTail, maybeETail)
      in
      case (newPs, newEs, newMaybePTail, newMaybeETail) of
        ([], [], Nothing, Nothing) ->
          Nothing

        ([newP], [newE], Nothing, Nothing) ->
          Just (newP, newE)

        _ ->
          Just <|
              ( replaceP__ pat       <| PList pws1 (newPs |> imitatePatListWhitespace ps) pws2 newMaybePTail pws3
              , replaceE__ boundExp <| EList ews1 (Utils.listValuesMake es (newEs |> imitateExpListWhitespace (Utils.listValues es))) ews2 newMaybeETail ews3
              )

    _ ->
      Just (pat, boundExp)


-- Remove assignments of [] to [] and *RemoveMe* to whatever (produced by CodeMotion.pluck)
-- Flatten assignment of singleton [exp] to singleton [var] (also often produced by CodeMotion.pluck)
simplifyAssignments : Exp -> Exp
simplifyAssignments program =
  program
  |> mapExp
      (\exp ->
        case exp.val.e__ of
          ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse boundExp])]) wsIn body ->
            case simplifyPatBoundExp pat boundExp of
              Just (newPat, newBoundExp) ->
                replaceE__ exp <|
                ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat (ensureWhitespacePat newPat) fs wse (ensureWhitespaceExp newBoundExp)])]) wsIn body

              Nothing ->
                body

          _ ->
            exp
      )


-- Single var assignment and one-to-one list assignments.
--
-- Returns [(identifer, assignedExpression), ... ]
simpleIdentsAndAssigns : Pat -> Exp -> List (Ident, Exp)
simpleIdentsAndAssigns letPat letAssign =
  case (letPat.val.p__, letAssign.val.e__) of
    -- Simple assignment.
    (PVar _ ident _, _) ->
      [(ident, letAssign)]

    -- List assignment, no tail.
    (PList pws1 pats pws2 Nothing pws3, EList aws1 assigns aws2 Nothing aws3) ->
      let patsAssigns = Utils.zip pats (Utils.listValues assigns) in
      let simplePatsAssigns =
        List.filterMap
            (\(pat, assign) ->
              case pat.val.p__ of
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
       ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse assign])]) wsIn body ->
        case (pat.val.p__, assign.val.e__) of
          -- Simple assignment.
          (PVar _ _ _, EVar oldWs assignIdent) ->
            if assignIdent == targetIdent then
              let newExpAdjustedWs = replacePrecedingWhitespace oldWs.val newExp in
              ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse newExpAdjustedWs])]) wsIn body
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
                  (Utils.listValues assigns)
            in
            let newAssignsListExp =
              withDummyExpInfo <| EList aws1 (Utils.listValuesMake assigns newAssigns) aws2 Nothing aws3
            in
            ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse newAssignsListExp])]) wsIn body

          _ ->
            e__

       _ ->
        e__
  in
  let inliner e__ =
    case e__ of
       ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse assign])]) wsIn body -> -- TODO: How to generalize to multiple declarations?
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
        ELet ws1 letKind (Declarations po tpes anns [(isRec, [LetExp mbwsc wsPat pat fs wse assign])]) wsIn newBody

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
-- removeUnusedLetPats.
--
changeRenamedVarsToOuter : Exp -> Exp
changeRenamedVarsToOuter exp =
  changeRenamedVarsToOuter_ Dict.empty exp

changeRenamedVarsToOuter_ : Dict.Dict Ident Ident -> Exp -> Exp
changeRenamedVarsToOuter_ renamings exp =
  let _ = Debug.log "TODO: LangSimplify.changeRenamedVarsToOuter. The current implementation might incorrectly rename variables shadowed by complex patterns" ()
  in exp {-
  let recurse = changeRenamedVarsToOuter_ renamings in
  let e__ = exp.val.e__ in
  let removeIdentsFromRenaming identsToRemove renamings =
     Dict.filter
        (\oldName newName ->
          (not <| Set.member oldName identsToRemove) &&
          (not <| Set.member newName identsToRemove)
        )
        renamings
  in
  replaceE__ exp <|
  case e__ of
    EConst ws n loc wd -> e__
    EBase ws baseV     -> e__

    EVar ws ident ->
      case Dict.get ident renamings of
        Just newName -> EVar ws newName
        Nothing      -> e__

    ELet ws1 letKind rec pat ws2 assign ws3 body ws4 ->
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
      ELet ws1 letKind rec pat ws2 assign_ ws3 body_ ws4

    EFun ws1 pats body ws2  ->
      let newlyAssignedIdents =
        List.map identifiersSetInPat pats
        |> List.foldl Set.union Set.empty
      in
      let renamingsShadowsRemoved = removeIdentsFromRenaming newlyAssignedIdents renamings in
      EFun ws1 pats (changeRenamedVarsToOuter_ renamingsShadowsRemoved body) ws2

    EApp ws1 e1 es appType ws2 -> EApp ws1 (recurse e1) (List.map recurse es) appType ws2
    EOp ws1 wso op es ws2      -> EOp ws1 wso op (List.map recurse es) ws2
    EList ws1 es ws2 m ws3     -> EList ws1 (Utils.listValuesMap recurse es) ws2 (Maybe.map recurse m) ws3
    ERecord ws1 m es ws2       -> ERecord ws1 (Maybe.map (Tuple.mapFirst recurse) m) (Utils.recordValuesMap recurse es) ws2
    ESelect ws0 e ws1 ws2 i    -> ESelect ws0 (recurse e) ws1 ws2 i
    EIf ws1 e1 ws2 e2 ws3 e3 ws4 -> EIf ws1 (recurse e1) ws2 (recurse e2) ws3 (recurse e3) ws4
    ECase ws1 e1 branches ws2  ->
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
    EColonType ws1 e ws2 tipe ws3         -> EColonType ws1 (recurse e) ws2 tipe ws3
    EParens ws1 e pStyle ws2              -> EParens ws1 (recurse e) pStyle ws2
    EHole ws mv                           -> e__
    -}
