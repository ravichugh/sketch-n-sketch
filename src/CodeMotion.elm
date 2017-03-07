module CodeMotion exposing
  ( moveDefinitionPat, moveDefinitionBeforeEId
  )

import Lang exposing (..)
import LangTools exposing (..)
import LangUnparser exposing (unparse, unparseWithIds, unparsePat)
import LangParser2
-- import DependenceGraph exposing
  -- (ScopeGraph, ScopeOrder(..), parentScopeOf, childScopesOf)
import InterfaceModel exposing (SynthesisResult(..))
import Utils

import Dict


type alias PatBoundExp = (Pat, Exp)
type alias RebuildLetExp = Exp -> Exp

-- returns the binding (p, e1) to be removed from let-expression at scopeId,
-- the body of the let at the scopeId
-- and a function foo that rewrites this let-expression by wrapping the
-- residual pattern and residual bound expression (that remain after plucking)
-- around a rewritten body (provided as an argument)
pluck : PatternId -> Exp -> Maybe (PatBoundExp, RebuildLetExp)
pluck ((scopeEId, scopeBranchI), path) program =
  findExpByEId program scopeEId
  |> Maybe.andThen (\scope -> pluck_ scope path program)


pluck_ : Exp -> List Int -> Exp -> Maybe (PatBoundExp, RebuildLetExp)
pluck_ scopeExp path program =
  let (maybePluckedAndMaybeNewPatAndBoundExp, ws1, letKind, isRec, ws2) =
    case scopeExp.val.e__ of
      ELet ws1 letKind False p e1 e2 ws2 -> (pluck__ p e1 path, ws1, letKind, False, ws2)
      _                                  -> Debug.crash <| "pluck_: bad Exp__ " ++ unparseWithIds scopeExp
  in
  case maybePluckedAndMaybeNewPatAndBoundExp of
    Nothing ->
      Nothing

    Just (pluckedPatBoundExp, maybeNewPatAndBoundExp) ->
      let wrapWithResidualLet =
        -- let (oldPat, oldBoundExp) =
        case maybeNewPatAndBoundExp of
          Nothing ->
            identity

          Just (newPat, newBoundExp) ->
            (\newBody -> replaceE__ scopeExp (ELet ws1 letKind isRec newPat newBoundExp newBody ws2))
      in
      Just (pluckedPatBoundExp, wrapWithResidualLet)


pluck__ : Pat -> Exp -> List Int -> Maybe (PatBoundExp, Maybe (Pat, Exp))
pluck__ p e1 path =
  case (p.val, e1.val.e__, path) of
    (_, _, []) ->
      Just ((p, e1), Nothing)

    (PAs _ _ _ childPat, _, i::is) ->
      -- TODO: allow but mark unsafe if as-pattern is used
      let _ = Debug.log "can't pluck out of as-pattern yet (unsafe)" in
      Nothing

    ( PList pws1 ps pws2 maybePTail pws3
    , EList ews1 es ews2 maybeETail ews3
    , i::is
    ) ->
      if List.length ps >= i && List.length es >= i then
        let (pi, ei) = (Utils.geti i ps, Utils.geti i es) in
        pluck__ pi ei is
        |> Maybe.map
            (\(plucked, maybeNewPatAndBoundExp) ->
              let (newPs, newEs) =
                case maybeNewPatAndBoundExp of
                  Just (newPat, newBoundExp) ->
                    ( Utils.replacei i newPat ps      |> imitatePatListWhitespace ps
                    , Utils.replacei i newBoundExp es |> imitateExpListWhitespace es
                    )

                  Nothing ->
                    ( Utils.removei i ps |> imitatePatListWhitespace ps
                    , Utils.removei i es |> imitateExpListWhitespace es
                    )
              in
              case (newPs, newEs, maybePTail, maybeETail) of
                ([], [], Nothing, Nothing) ->
                  ( plucked
                  , Nothing
                  )

                ([newP], [newE], Nothing, Nothing) ->
                  -- Delistify singleton lists
                  ( plucked
                  , Just (copyPrecedingWhitespacePat p newP,
                          copyPrecedingWhitespace e1 newE)
                  )

                _ ->
                  ( plucked
                  , Just (replaceP_ p   (PList pws1 newPs pws2 maybePTail pws3),
                          replaceE__ e1 (EList ews1 newEs ews2 maybeETail ews3))
                  )

            )
      else if List.length ps == List.length es && i == 1 + List.length ps && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
        -- Recursing into the tail binding
        let pi = Utils.fromJust maybePTail in
        let ei = Utils.fromJust maybeETail in
        pluck__ pi ei is
        |> Maybe.map
            (\(plucked, maybeNewPatAndBoundExp) ->
              case maybeNewPatAndBoundExp of
                Just (newTailPat, newTailBoundExp) ->
                  ( plucked
                  , Just (replaceP_ p   (PList pws1 ps pws2 (Just newTailPat) pws3),
                          replaceE__ e1 (EList ews1 es ews2 (Just newTailBoundExp) ews3))
                  )

                Nothing ->
                  ( plucked
                  , Just (replaceP_ p   (PList pws1 ps pws2 Nothing pws3),
                          replaceE__ e1 (EList ews1 es ews2 Nothing ews3))
                  )
            )
      else
        Debug.log "pluck index longer than head list of PList or EList" Nothing

    _ ->
      let _ = Debug.log ("pluck_: bad pattern " ++ unparsePat p) path in
      Nothing
------------------------------------------------------------------------------


strUnsafeBool isUnsafe = if isUnsafe then "[UNSAFE] " else ""


------------------------------------------------------------------------------


-- TODO: fix moving a subpattern in front of the scope

-- Moving a definition is safe if all identifiers resolve to the same bindings.
--
-- More specifically:
--   - All free variables in the moved assignment still resolve to the same bindings
--   - All previous references to the moved identifier still resolve to that identifer
--   - All other variables uses of the same name do not resolve to the moved identifier
--
-- Returns (list of safe results, list of unsafe results)
moveDefinitionBeforeEId : PatternId -> EId -> Exp -> (List SynthesisResult, List SynthesisResult)
moveDefinitionBeforeEId sourcePatId targetEId program =
  let ((sourceScopeEId, _), _) = sourcePatId in
  case pluck sourcePatId program of
    Nothing -> Debug.crash <| "could not pluck pat " ++ toString sourcePatId ++ " from " ++ unparseWithIds program
    Just ((pluckedPat, boundExp), wrapWithResidualLet) ->
      let insertedLetEId = LangParser2.maxId program + 1 in
      let newProgram =
        flip mapExp program <| \e ->
          -- Written oddly, but this correctly handles the cases
          -- where sourceScopeEId == targetEId; both when a subpattern is extracted
          -- right before the scope, and when you try to move an entire scope
          -- right before itself
          let newE =
            if e.val.eid == sourceScopeEId
            then wrapWithResidualLet (expToLetBody e)
            else e
          in
          if e.val.eid == targetEId then
            let letOrDef = if isTopLevelEId targetEId program then Def else Let in
            withDummyPosEId insertedLetEId <|
              ELet (precedingWhitespace newE) letOrDef False
                (ensureWhitespacePat pluckedPat) (ensureWhitespaceExp boundExp)
                (ensureWhitespaceExp newE) ""
          else
            newE
      in
      -- let _ = Debug.log ("old: \n" ++ unparseWithIds program) () in
      -- let _ = Debug.log ("new: \n" ++ unparseWithIds newProgram) () in
      let oldScopeBody   = justFindExpByEId program sourceScopeEId |> expToLetBody in
      let movedScopeBody = justFindExpByEId newProgram insertedLetEId |> expToLetBody in
      let isSafe =
        let identUsesSafe =
          -- Effectively comparing EIds of all uses of the identifiers.
          -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
          let identSafe ident =
            Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident movedScopeBody)
          in
          List.all identSafe (identifiersListInPat pluckedPat)
        in
        let boundExpVarsSafe =
          freeVars boundExp
          |> List.all (\e -> bindingScopeIdFor e program == bindingScopeIdFor e newProgram)
        in
        identUsesSafe && boundExpVarsSafe
      in
      let caption =
        strUnsafeBool (not isSafe) ++ " Move " ++ (unparsePat >> Utils.squish) pluckedPat ++ " before " ++ (unparse >> Utils.squish >> Utils.niceTruncateString 20 "...") movedScopeBody
      in
      let result =
        SynthesisResult { description = caption, exp = newProgram, sortKey = [], children = Nothing }
      in
      if isSafe
      then ([result], [])
      else ([], [result])


------------------------------------------------------------------------------


-- Returns (list of safe results, list of unsafe results)
moveDefinitionPat : PatternId -> PatternId -> Exp -> (List SynthesisResult, List SynthesisResult)
moveDefinitionPat sourcePatId targetPatId program =
  let ((sourceScopeEId, _), _) = sourcePatId in
  let ((targetEId, _), _) = targetPatId in
  case pluck sourcePatId program of
    Nothing -> Debug.crash <| "could not pluck pat " ++ toString sourcePatId ++ " from " ++ unparseWithIds program
    Just ((pluckedPat, boundExp), wrapWithResidualLet) ->
      let newProgram =
        flip mapExp program <| \e ->
          if e.val.eid == sourceScopeEId then
            let sourceBody = expToLetBody e in
            if sourceScopeEId /= targetEId
              then wrapWithResidualLet sourceBody
              else insertPat_ (pluckedPat, boundExp) targetPatId (wrapWithResidualLet sourceBody)
          else if e.val.eid == targetEId then
            insertPat_ (pluckedPat, boundExp) targetPatId e
          else
            e
      in
      -- let _ = Debug.log ("old: \n" ++ unparseWithIds program) () in
      -- let _ = Debug.log ("new: \n" ++ unparseWithIds newProgram) () in
      let oldScopeBody = justFindExpByEId program sourceScopeEId |> expToLetBody in
      let newScope     = justFindExpByEId newProgram targetEId in
      let newScopeBody = expToLetBody newScope in
      let isSafe =
        let identUsesSafe =
          -- Effectively comparing EIds of all uses of the identifier.
          -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
          let identSafe ident =
            Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident newScopeBody)
          in
          List.all identSafe (identifiersListInPat pluckedPat)
        in
        let boundExpVarsSafe =
          freeVars boundExp
          |> List.all (\e -> bindingScopeIdFor e program == bindingScopeIdFor e newProgram)
        in
        let noDuplicateNamesInPat =
          let namesDefinedAtNewScope = identifiersListInPat (expToLetPat newScope) in
          namesDefinedAtNewScope == Utils.dedup namesDefinedAtNewScope
        in
        identUsesSafe && boundExpVarsSafe && noDuplicateNamesInPat
      in
      let caption =
        strUnsafeBool (not isSafe) ++ " Move " ++ (unparsePat >> Utils.squish) pluckedPat ++ " to make " ++ (unparsePat >> Utils.squish >> Utils.niceTruncateString 20 "...") (expToLetPat newScope)
      in
      let result =
        SynthesisResult { description = caption, exp = newProgram, sortKey = [], children = Nothing }
      in
      if isSafe
      then ([result], [])
      else ([], [result])


insertPat_ : PatBoundExp -> PatternId -> Exp -> Exp
insertPat_ (patToInsert, boundExp) ((_, targetBranchI), targetPath) exp =
  case exp.val.e__ of
    ELet ws1 letKind rec p e1 e2 ws2 ->
      case insertPat__ (patToInsert, boundExp) p e1 targetPath of
        Just (newPat, newBoundExp) ->
          replaceE__ exp (ELet ws1 letKind rec newPat newBoundExp e2 ws2)

        Nothing ->
          let _ = Debug.log "insertPat_: pattern, path " (p.val, targetPath) in
          exp

    _ ->
      let _ = Debug.log "insertPat_: not ELet" exp.val.e__ in
      exp


insertPat__ : PatBoundExp -> Pat -> Exp -> List Int -> Maybe (Pat, Exp)
insertPat__ (patToInsert, boundExp) p e1 path =
  let maybeNewP_E__Pair =
    case (p.val, e1.val.e__, path) of
      (PVar pws1 _ _, _, [i]) ->
        Just ( PList pws1                      (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") "" Nothing ""
             , EList (precedingWhitespace e1)  (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ") "" Nothing "" )

      (PAs pws1 _ _ _, _, [i]) ->
        Just ( PList pws1                      (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") "" Nothing ""
             , EList (precedingWhitespace e1)  (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ") "" Nothing "" )

      (PAs pws1 _ _ _, _, i::is) ->
        -- TODO: allow but mark unsafe if as-pattern is used
        let _ = Debug.log "can't insert into as-pattern yet (unsafe)" in
        Nothing

      ( PList pws1 ps pws2 maybePTail pws3
      , EList ews1 es ews2 maybeETail ews3
      , [i]
      ) ->
        if List.length ps + 1 >= i && List.length es + 1 >= i then
          Just ( PList pws1 (Utils.inserti i patToInsert ps |> imitatePatListWhitespace ps) pws2 Nothing pws3
               , EList ews1 (Utils.inserti i boundExp es    |> imitateExpListWhitespace es) ews2 Nothing ews3 )
        else
          let _ = Debug.log "can't insert into this list (note: cannot insert on list tail)" (unparsePat p, unparse e1, path) in
          Nothing

      ( PList pws1 ps pws2 maybePTail pws3
      , EList ews1 es ews2 maybeETail ews3
      , i::is
      ) ->
        if List.length ps >= i && List.length es >= i then
          let (pi, ei) = (Utils.geti i ps, Utils.geti i es) in
          insertPat__ (patToInsert, boundExp) pi ei is
          |> Maybe.map
              (\(newPat, newBoundExp) ->
                let (newPs, newEs) =
                  ( Utils.replacei i newPat ps      |> imitatePatListWhitespace ps
                  , Utils.replacei i newBoundExp es |> imitateExpListWhitespace es
                  )
                in
                (PList pws1 newPs pws2 maybePTail pws3,
                 EList ews1 newEs ews2 maybeETail ews3)

              )
        else if List.length ps == List.length es && i == 1 + List.length ps && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
          -- Recursing into the tail binding
          let pi = Utils.fromJust maybePTail in
          let ei = Utils.fromJust maybeETail in
          insertPat__ (patToInsert, boundExp) pi ei is
          |> Maybe.map
              (\(newPat, newBoundExp) ->
                (PList pws1 ps pws2 (Just newPat) pws3,
                 EList ews1 es ews2 (Just newBoundExp) ews3)
              )
        else
          let _ = Debug.log "can't insert into this list (note: cannot insert on list tail)" (unparsePat p, unparse e1, path) in
          Nothing

      _ ->
        let _ = Debug.log "insertPat__: pattern, path " (p.val, path) in
        Nothing
  in
  case maybeNewP_E__Pair of
    Just (newP_, newE__) ->
      Just (replaceP_ p newP_, replaceE__ e1 newE__)

    Nothing ->
      Nothing
