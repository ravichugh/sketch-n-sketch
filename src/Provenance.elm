module Provenance exposing (..)

import FastParser
import Lang exposing (..)
import LangTools
import LangUnparser
import Utils

import Set exposing (Set)


-- Mirror of basedOn provenance in Eval.eval
--
-- Though note: if code branches, the value produced by the branch
-- expression can only be based on on the value from the taken branch.
--
-- For answering the question: if all of these children appear in an
-- EId explanation, can they be consolidated into this parent?
expNonControlFlowChildren : Exp -> List Exp
expNonControlFlowChildren exp =
  case exp.val.e__ of
    EConst _ _ _ _                           -> []
    EBase _ _                                -> []
    EVar _ _                                 -> []
    EFun _ _ _ _                             -> [] -- Provenance implicit: when func returns a val, it's based on a terminal expression inside the EFun.
    EOp _ _ _ _                              -> childExps exp
    EList _ _ _ _ _                          -> childExps exp
    EApp _ func es _                         -> es -- Provenance implicit: when func returns a val, it's based on a terminal expression inside the EFun.
    ELet _ _ _ _ boundExp body _             -> [body] -- lets are actually pass-through and won't appear the val provenance tree; not sure it matters what we return here
    EIf _ predicate trueBranch falseBranch _ -> [trueBranch, falseBranch]
    ECase _ scrutinee branches _             -> branchExps branches
    ETypeCase _ scrutinee tbranches _        -> tbranchExps tbranches
    EComment _ _ _                           -> childExps exp
    EOption _ _ _ _ _                        -> childExps exp
    ETyp _ _ _ _ _                           -> childExps exp
    EColonType _ _ _ _ _                     -> childExps exp
    ETypeAlias _ _ _ _ _                     -> childExps exp


-- Attempt to consume as many parents as possible.
--
-- No obvious reasonable way to incorporate an expFilter here.
expandEIdInterpretationOutward : Exp -> List EId -> List EId
expandEIdInterpretationOutward program interpretation =
  let
    -- Simple expansion of inner expressions into parents (e.g. we want the expression (3 : Num) instead of just the inner 3).
    interpPiecesExpanded =
      interpretation
      |> List.map (LangTools.outerSameValueExpByEId program >> .val >> .eid)

    -- If all of some expression's children are in an interpretation, then we can use that expression instead of its children.
    parents =
      interpPiecesExpanded
      |> List.map (parentByEId program)
      |> Utils.filterJusts -- Found exps only
      |> Utils.filterJusts -- Parent of entire program is "Nothing"
      |> Utils.dedupBy (.val >> .eid)
    interpExpandedIntoParents =
      parents
      |> List.foldl
          (\parent interpretation ->
            let parentNonControlFlowChildrenEIds = expNonControlFlowChildren parent |> List.map (.val >> .eid) in
            if Utils.isSublistAsSet parentNonControlFlowChildrenEIds interpretation then
              parent.val.eid :: Utils.diffAsSet interpretation parentNonControlFlowChildrenEIds
            else
              interpretation
          )
          interpPiecesExpanded
  in
  if interpretation == interpExpandedIntoParents then
    interpretation
  else
    -- Keep expanding until no more progress.
    expandEIdInterpretationOutward program interpExpandedIntoParents


----- Val basedOn tree provenance ----------------------------------

-- All possible interpretations of "which expressions brought this value into being".
--
-- Namely, the provenance forms a tree of values: the below returns the EId sets associated
-- with the leaves of all possible prunings of that tree of values, such that all leaves are
-- in the program.
--
-- Or, as written below, a value comes from either:
--   1. The immediate expression that produced the value OR
--   2. Any combination of the expressions that produced the values this value was based on.
--
-- Example, eids given as letters below each exp:
--
-- (def var 10)
-- a        b
--
-- (+ 20 (sqrt var))
-- c  d  e f    g
--
-- Interpretations are:
-- [ {c}
-- , {d, e}
-- , {d, g}
-- , {d, b}
-- ]
--
-- Note: Expression f does not appear in the provenance--which function to call is
-- considered "control flow".
--
-- Note: Naively, a value modified in prelude by a prelude value just before output will
-- poison the whole tree and result in no interpretations. Example:
--
-- Prelude: (def midpoint (\([x1 y1] [x2 y2]) -> [(/ (+ x1 x2) 2) (/ (+ y1 y2) 2)])
--
-- Program: (midpoint [20 30] [50 60])
--
-- Provenance tree of x coordinate in the output value [35 45]:
--
--    (value 35)
--        |
--        |
-- (/ (+ x1 x2) 2)
--       /       \
--      /         \
-- (+ x1 x2)       2
--   /    \
-- x1      x2
--  |      |
-- 20*      50*
--
-- Only starred expressions are from the program: there is no tree pruning with
-- leaves all in the program.
--
-- So see "valTreeToAllProgramEIdInterpretationsIgnoringUninterpretedSubtrees", of which
-- valTreeToMost(Proximal/Distal)ProgramEIdInterpretation computes a member of.


-- Naive version looking for interpretations all in the program (unused because of poisoning described above).
valTreeToAllProgramEIdInterpretations : Val -> List (Set EId)
valTreeToAllProgramEIdInterpretations val =
  valTreeToAllProgramEIdInterpretations_ False (always True) val


---- SOME PROOFS THAT DON'T MATTER. ----
--
-- All singleton-val interpretations of a value are more proximal than any multi-val interpretation.
--
-- Let v1 ≤ v2 mean v1 is at least as low on the tree as (i.e. v1 is in the subtree rooted at) v2.
--
-- Proof, by contradiction:
--
-- Assuming a counterexample ({v1}, multiIntep), v1 is either in some subtree of an element of multiIntep (Case 1)
-- or not (Case 2).
--
-- Case 1: v1 ≤ multiIntep
--
-- Assume a singleton intepretation {v1} ≤ multiIntep; i.e. ∃ v2 ∈ multiIntep such that v1 ≤ v2.
--
-- Then ∃ a path to from root to a v3 ∈ multiIntep \ {v1} that does not contain v1.
--
-- ⇒ all explanations with v1 also include another val
-- ⇒ {v1} not a valid interpretation: contradiction.
--
-- Case 2: v1 ≰ multiIntep
--
-- If ∃ v2 ∈ multiIntep such that v1 > v2, then {v1} is more proximal and is not a counterexample.

-- If ∄ v2 ∈ multiIntep such that v1 > v2, then ∃ a path to from root to any v3 ∈ multiIntep that does not contain v1.
--   ⇒ all explanations with v1 also include another val
--   ⇒ {v1} not a valid interpretation: contradiction.
--
-- Done.
--
--
-- DOES THIS RESULT TRANSLATE TO EID INTERPRETATIONS?
-- Can a multi-val interpretation collapse into a singleton after a multi-val interpretaion?
--
-- No, it does not apply because yes, a multi-val interpretation can collapse into a singleton after another multi-val interpretation:
--
-- (def y 10)
-- [y y]
--
--   value [10 10]
--          |
--        [y y]
--        /   \
--       y*    y*
--      /       \
--    10†        10†
--
-- Interpretation * is more proximal than the † interpretation, but * will be 2 EIds (from the 2 EVars) while † will only be one.


-- Not subjected to poisoning, but not actually used because of combinatorical explosion of interpretations.
--
-- valTreeToMost(Distal/Proximal)ProgramEIdInterpretation each return a member of this.
valTreeToAllProgramEIdInterpretationsIgnoringUninterpretedSubtrees : (Exp -> Bool) -> Val -> List (Set EId)
valTreeToAllProgramEIdInterpretationsIgnoringUninterpretedSubtrees expFilter val =
  valTreeToAllProgramEIdInterpretations_ True expFilter val


valTreeToAllProgramEIdInterpretations_ : Bool -> (Exp -> Bool) -> Val -> List (Set EId)
valTreeToAllProgramEIdInterpretations_ ignoreUninterpretedSubtress expFilter val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  let perhapsThisExp = if FastParser.isProgramEId exp.val.eid && expFilter exp then [Set.singleton exp.val.eid] else [] in
  basedOnVals
  |> List.map (valTreeToAllProgramEIdInterpretations_ ignoreUninterpretedSubtress expFilter)
  |> (if ignoreUninterpretedSubtress then List.filter (not << List.isEmpty) else identity)
  |> Utils.oneOfEach
  |> List.map Utils.unionAll
  |> (++) perhapsThisExp
  |> Utils.dedupByEquality


-- Returns [] if val cannot be interpreted all inside vals.
-- Otherwise, returns the different ways it could be interpreted.
valInterpretationsAllInside : List Val -> Val -> List (List Val)
valInterpretationsAllInside vals val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  let perhapsThisVal = if List.member val vals then [[val]] else [] in
  let childrenInterps =
    -- We do want poisoning here: a child that can't be interpreted inside vals should break the entire interpretation.
    basedOnVals
    |> List.map (valInterpretationsAllInside vals)
    |> Utils.oneOfEach -- This will kill this interpretation and all ancestors if any value is not interpreted.
    |> List.map Utils.unionAllAsSet
  in
  perhapsThisVal ++ childrenInterps


proximalValInterpretationsAllInside : List Val -> Val -> List (List Val)
proximalValInterpretationsAllInside vals val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  if List.member val vals then
    [[val]]
  else
    -- We do want poisoning here: a child that can't be interpreted inside vals should break the entire interpretation.
    basedOnVals
    |> List.map (proximalValInterpretationsAllInside vals)
    |> Utils.oneOfEach -- This will kill this interpretation and all ancestors if any value is not interpreted.
    |> List.map Utils.unionAllAsSet


-- Includes given val.
flattenValBasedOnTree : Val -> List Val
flattenValBasedOnTree val =
  let (Provenance _ _ basedOnVals) = val.provenance in
  val :: List.concatMap flattenValBasedOnTree basedOnVals


-- There are two proximal and two distal intepretations.
-- Proximal/distal to final x and proximal/distal to final y.
--
-- However, haven't yet found an example where the proximal for x/y differ or the distal for x/y differ.
valsToProximalDistalPointInterpretations : (Exp -> Bool) -> Val -> Val -> (Set EId, Set EId, Set EId, Set EId)
valsToProximalDistalPointInterpretations expFilter xValTree yValTree =
  let valsWherePossiblyXCoord =
    xValTree
    |> flattenValBasedOnTree
    |> List.concatMap (\intermediateVal ->
      valParents intermediateVal
      |> List.filter (\parent ->
        case parent.v_ of
          VList [x, y] -> x == intermediateVal -- Would not be an exact match if we didn't mutate in the evaluator.
          -- VList [x, y] -> x == intermediateVal || let _ = Utils.log (LangUnparser.unparseWithIds (provenanceExp parent.provenance)) in let _ = Utils.log (LangUnparser.unparseWithIds (provenanceExp intermediateVal.provenance)) in False
          _            -> False
      )
    )
  in
  -- let _ = Debug.log "valsWherePossiblyXCoord size" (List.length valsWherePossiblyXCoord) in
  let valsWherePossiblyYCoord =
    yValTree
    |> flattenValBasedOnTree
    |> List.concatMap (\intermediateVal ->
      valParents intermediateVal
      |> List.filter (\parent ->
        case parent.v_ of
          VList [x, y] -> y == intermediateVal -- Would not be an exact match if we didn't mutate in the evaluator.
          _            -> False
      )
    )
  in
  -- let _ = Debug.log "valsWherePossiblyYCoord size" (List.length valsWherePossiblyYCoord) in
  let parentPoints = Utils.intersectAsSet valsWherePossiblyXCoord valsWherePossiblyYCoord in
  -- let _ = Debug.log "parentPoints size" (List.length parentPoints) in
  -- let _ = parentPoints |> List.map (\ptVal -> Utils.log <| LangUnparser.unparseWithIds (provenanceExp ptVal.provenance)) in
  let pointInterpretation valTreeToProgramPointEIdInterpretation preferredCoordVal otherCoordVal =
    -- A point interpretation can be multiple points (that were later combined together), these multiple points are the valsUsed.
    let (coord1Interp, valsUsed1) = valTreeToProgramPointEIdInterpretation parentPoints preferredCoordVal in
    -- Limit intepretation of second coordinate to the point val(s) of the first coordinate.
    let (coord2Interp, valsUsed2) = valTreeToProgramPointEIdInterpretation valsUsed1 otherCoordVal in
    if valsUsed1 == [] then
      Set.empty
    else if List.length valsUsed1 /= List.length valsUsed2 then
      -- Give up. Too deep down the rabbit hole to figure out how to handle this case cleanly.
      Set.empty
    else
      Set.union coord1Interp coord2Interp
  in
  let proximalToXInterpretation = pointInterpretation (valTreeToMostProximalProgramPointEIdInterpretation expFilter) xValTree yValTree in
  let proximalToYInterpretation = pointInterpretation (valTreeToMostProximalProgramPointEIdInterpretation expFilter) yValTree xValTree in
  let distalToXInterpretation   = pointInterpretation (valTreeToMostDistalProgramPointEIdInterpretation expFilter) xValTree yValTree in
  let distalToYInterpretation   = pointInterpretation (valTreeToMostDistalProgramPointEIdInterpretation expFilter) yValTree xValTree in
  ( proximalToXInterpretation
  , proximalToYInterpretation
  , distalToXInterpretation
  , distalToYInterpretation
  ) -- |> Debug.log "valsToProximalDistalPointInterpretations"


isRelevantParentPoint : (Exp -> Bool) -> List Val -> Val -> Bool
isRelevantParentPoint expFilter pointVals parent =
  let (Provenance _ parentExp _) = parent.provenance in
  -- Former condition should always be true; Eval won't add parent unless it's in the program.
  -- Leave it here though: eventually we will narrow contexts further.
  FastParser.isProgramEId parentExp.val.eid && List.member parent pointVals && expFilter parentExp


-- Provide a list of vals known to be points.
--
-- Returns an EId interpretation of val prefering those vals, and a list of the pointVals used.
-- Resulting interpretation may be a mix of EIds from pointVals and bare values
valTreeToMostProximalProgramPointEIdInterpretation : (Exp -> Bool) -> List Val -> Val -> (Set EId, List Val)
valTreeToMostProximalProgramPointEIdInterpretation expFilter pointVals val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  -- Most recent parents are at front of list.
  case valParents val |> Utils.findFirst (isRelevantParentPoint expFilter pointVals) of
    Just parent ->
      let (Provenance _ parentExp _) = parent.provenance in
      (Set.singleton parentExp.val.eid, [parent])

    Nothing ->
      let (childrenInterpretations, pointValsUsed) =
        basedOnVals
        |> List.map (valTreeToMostProximalProgramPointEIdInterpretation expFilter pointVals)
        |> List.unzip
      in
      if List.any ((/=) []) pointValsUsed then
        -- Child interpretations contain a point, use as given.
        (Utils.unionAll childrenInterpretations, Utils.unionAllAsSet pointValsUsed)
      else if FastParser.isProgramEId exp.val.eid && expFilter exp then
        -- No point intepretation yet, most proximal intepretation we can give is ourself.
        (Set.singleton exp.val.eid, [])
      else
        -- No point intepretation yet, most proximal intepretation we can give is our children's proximal interpretations.
        (Utils.unionAll childrenInterpretations, [])


-- Provide a list of vals known to be points.
--
-- Returns an EId interpretation of val prefering those vals, and a list of the pointVals used.
-- Resulting interpretation may be a mix of EIds from pointVals and bare values
valTreeToMostDistalProgramPointEIdInterpretation : (Exp -> Bool) -> List Val -> Val -> (Set EId, List Val)
valTreeToMostDistalProgramPointEIdInterpretation expFilter pointVals val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  let (childrenInterpretations, pointValsUsed) =
    basedOnVals
    |> List.map (valTreeToMostDistalProgramPointEIdInterpretation expFilter pointVals)
    |> List.unzip
  in
  if List.any ((/=) []) pointValsUsed then
    -- Child interpretations contain a point, use as given.
    (Utils.unionAll childrenInterpretations, Utils.unionAllAsSet pointValsUsed)
  else
    -- No point interpretations yet: are we part of a point?
    -- Oldest parents are at the back of the list.
    case List.reverse (valParents val) |> Utils.findFirst (isRelevantParentPoint expFilter pointVals) of
      Just parent ->
        let (Provenance _ parentExp _) = parent.provenance in
        (Set.singleton parentExp.val.eid, [parent])

      Nothing ->
        -- No point intepretation yet.
        if childrenInterpretations |> List.any (not << Set.isEmpty) then
          (Utils.unionAll childrenInterpretations, [])
        else if FastParser.isProgramEId exp.val.eid && expFilter exp then
          (Set.singleton exp.val.eid, [])
        else
          (Set.empty, [])


-- Interpretation closest to output value, i.e. furthest along in the program.
valTreeToMostProximalProgramEIdInterpretation : (Exp -> Bool) -> Val -> Set EId
valTreeToMostProximalProgramEIdInterpretation expFilter val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  if FastParser.isProgramEId exp.val.eid && expFilter exp then
    Set.singleton exp.val.eid
  else
    basedOnVals
    |> List.map (valTreeToMostProximalProgramEIdInterpretation expFilter)
    |> Utils.unionAll


-- Interpretation farthest from output value, i.e. earliest in the program.
-- Generally constants.
valTreeToMostDistalProgramEIdInterpretation : (Exp -> Bool) -> Val -> Set EId
valTreeToMostDistalProgramEIdInterpretation expFilter val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  let childrenInterpretations =
    basedOnVals
    |> List.map (valTreeToMostDistalProgramEIdInterpretation expFilter)
  in
  -- Commented-out bit is if we want to be a member of valTreeToAllProgramEIdInterpretations rather than valTreeToAllProgramEIdInterpretationsIgnoringUninterpretedSubtrees
  -- if childrenInterpretations |> List.all (not << Set.isEmpty) then
  --   Utils.unionAll childrenInterpretations
  if childrenInterpretations |> List.any (not << Set.isEmpty) then
    Utils.unionAll childrenInterpretations
  else if FastParser.isProgramEId exp.val.eid && expFilter exp then
    Set.singleton exp.val.eid
  else
    Set.empty


-- Still combinatorically explosive :/
-- Using isPossibleSingleEIdInterpretation instead.
valTreeToSingleEIdInterpretations : Exp -> (Exp -> Bool) -> Val -> List EId
valTreeToSingleEIdInterpretations program expFilter val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  let perhapsThisExp = if FastParser.isProgramEId exp.val.eid && expFilter exp then [exp.val.eid] else [] in
  basedOnVals
  |> List.map (valTreeToAllProgramEIdInterpretationsIgnoringUninterpretedSubtrees expFilter)
  |> List.filter (not << List.isEmpty)
  |> Utils.oneOfEach
  |> List.map Utils.unionAll
  |> Utils.dedupByEquality
  |> List.filterMap
      (\interpretation ->
        case Set.toList interpretation of
          [singleEId]    -> Just singleEId
          interpretationList ->
            case expandEIdInterpretationOutward program interpretationList of
              [singleEId] ->
                -- Using the exp filter this late may discard allowable interpretations, but it's not clear how to push it into expandEIdInterpretationOutward
                if singleEId |> findExpByEId program |> Maybe.map expFilter |> (==) (Just True)
                then Just singleEId
                else Nothing
              _ ->
                Nothing
      )
  |> (++) perhapsThisExp
  |> Utils.dedupByEquality


interpretationIsNonEmpty : Val -> Bool
interpretationIsNonEmpty val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  FastParser.isProgramEId exp.val.eid || List.any interpretationIsNonEmpty basedOnVals


isPossibleSingleEIdInterpretation : Exp -> EId -> Val -> Bool
isPossibleSingleEIdInterpretation program eid val =
  let (Provenance _ exp basedOnVals) = val.provenance in
  (exp.val.eid == eid && FastParser.isProgramEId exp.val.eid)
  || let relevantChildren = List.filter interpretationIsNonEmpty basedOnVals in
  case relevantChildren of
    [] -> False
    _  -> List.all (\basedOnVal -> isPossibleSingleEIdInterpretation program eid basedOnVal) basedOnVals

--
-- isPossibleEIdInterpretation : Exp -> EId -> Val -> Bool
-- isPossibleEIdInterpretation program eid val =
--
