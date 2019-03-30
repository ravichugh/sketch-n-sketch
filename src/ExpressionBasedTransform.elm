module ExpressionBasedTransform exposing -- in contrast to ValueBasedTransform
  ( passiveSynthesisSearch
  , cloneEliminationSythesisResults
  , repeatByIndexedMerge
  )

import Lang exposing (..)
import LangUnparser exposing (unparse, unparsePat)
import Info exposing (parsedThingToLocation)
import LangTools exposing (..)
import LangSimplify
import Types
import InterfaceModel exposing (Model, resultExp)
import Syntax
import Utils

import Dict exposing (Dict)
import Set
import String


--------------------------------------------------------------------------------
-- Expression Rewriting

-- x TODO: don't freeze all MathNums
-- x TODO: Split abstraction -> mapping into two steps
-- x TODO: revisit clone detection -- lists of constants are ignored if >1 constant differs
-- x TODO: abstract with n parameters
-- x TODO: don't lift variables usages into an abstraction outside of the variable's original scope (later we may add lifting)
-- x TODO: fix grouping so scaling is inserted correctly (use eids not locids for esubst)
-- x TODO: "Merge 2 rect1s by abstracting over x2 y2"
-- x TODO: multiple rewrites (either speculative or nested dropdowns)
-- TODO: make nested dropdowns scrollable
-- TODO: support 0 parameter clone removal
-- x TODO: fix width + scroll synthesis results box
-- TODO: examine examples again (including sine wave)
-- TODO: think about interactive rewrite rules DSL
-- TODO: can rewrite rules get us to the UIST steps? e.g. radial repeat?
-- TODO: think about what the story for this paper can be: maybe take one stage of workflow and make it really great (unlikely we can make all of them better) Draw? Relate? Abstract/Repeat/Rewrite?
-- TODO: naming for abstracted pts
-- TODO: write down methodology and document heuristics
-- TODO: fix lifting a variable name that ends up hiding an in-use variable
-- TODO: speed up clone detection/passive synthesis (slow on anything other than small programs; particularly slow if program has leading comments)
-- TODO: Filter out speculative expressions that error or produce a different result.
-- TODO: ensure abstracted expressions weren't sub-expressions of each other...?
-- TODO: lift dependencies (procrastinate this)
-- TODO: turn off overlapping pairs relation


passiveSynthesisSearch : Model -> Exp -> List InterfaceModel.SynthesisResult
passiveSynthesisSearch model originalExp =
  cloneEliminationSythesisResults (always True) 2 5 originalExp ++
  mapAbstractSynthesisResults originalExp ++
  rangeSynthesisResults originalExp ++
  inlineListSynthesisResults originalExp
  |> List.filter (\synthesisResult -> InterfaceModel.runAndResolve model (resultExp synthesisResult) |> Utils.resultToBool)


rangeSynthesisResults : Exp -> List InterfaceModel.SynthesisResult
rangeSynthesisResults originalExp =
  let eidAndRangeLists =
    flattenExpTree originalExp
    |> List.filterMap
        (\exp ->
          case exp.val.e__ of
            EList ws1 es ws2 Nothing ws3 ->
              let maybeNums = List.map Tuple.second es |> List.map expToMaybeNum |> Utils.projJusts in
              case maybeNums of
                Just (n1::n2::n3::nRest) -> -- At least three nums
                  let nums = n1::n2::n3::nRest in
                  let (min, max) = (List.minimum nums |> Utils.fromJust_ "ExpressionBasedTransform.rangeSynthesisResults min" |> round, List.maximum nums |> Utils.fromJust_ "ExpressionBasedTransform.rangeSynthesisResults max" |> round) in
                  let (characterization, ascending) =
                    if nums == (List.range min max |> List.map toFloat) then
                      ("ascending", nums)
                    else if List.reverse nums == (List.range min max |> List.map toFloat) then
                      ("descending", List.reverse nums)
                    else
                      ("none", [])
                  in
                  if characterization /= "none" then
                    let insertedLoc = dummyLoc_ (if List.all isFrozenNumber (List.map Tuple.second es) then frozen else unann) in
                    let maybeReverse e =
                      if characterization == "descending"
                      then eApp (eVar0 "reverse") [e]
                      else e
                    in
                    if List.head ascending == Just 0.0 then
                      Just (exp.val.eid, maybeReverse <| eApp (eVar0 "zeroTo") [withDummyExpInfo <| EConst space1 (toFloat max + 1) insertedLoc (intSlider 0 (5*(max + 1)))])
                    else if List.head ascending == Just 1.0 then
                      Just (exp.val.eid, maybeReverse <| eApp (eVar0 "list1N") [withDummyExpInfo <| EConst space1 (toFloat max) insertedLoc (intSlider 0 (5*(max + 1)-1))])
                    else
                      Just (exp.val.eid, maybeReverse <| eApp (eVar0 "range") [eConst (toFloat min) insertedLoc, eConst (toFloat max) insertedLoc])
                  else
                    Nothing

                _ ->
                  Nothing

            _ ->
              Nothing
        )
  in
  eidAndRangeLists
  |> List.map
      (\(eid, newExp) ->
        InterfaceModel.synthesisResult
            ("Replace " ++ (unparse >> Utils.squish) (justFindExpByEId originalExp eid) ++ " with " ++ unparse newExp)
            (replaceExpNodePreservingPrecedingWhitespace eid newExp originalExp)
      )


-- e.g. let [a b c] = ... in [a b c] => let list = ... in list; and other variations
inlineListSynthesisResults originalExp =
  let candidatesAndDescription =
    flattenExpTree originalExp
    |> List.concatMap
        (\exp ->
          case exp.val.e__ of
            ELet letWs1 letKind False letPat letWs2 letAssign letWs3 letBody letWs4 -> -- non-recursive lets only
              let letExp = exp in
              case letPat.val.p__ of
                PList _ (p1::p2::p3::pRest) _ Nothing _ -> -- At least three
                  let pats = p1::p2::p3::pRest in
                  let maybeIdents = pats |> List.map patToMaybePVarIdent |> Utils.projJusts in
                  case maybeIdents of
                    Just idents ->
                      let usages = identifierSetUses (Set.fromList idents) letBody in
                      if List.map expToIdent usages == idents then -- Each should be used exactly once, in order
                        -- All idents used in only in same list
                        -- Or wrapped in singleton lists in same list
                        let usagesExpanded = usages |> List.map (outerSameValueExp letBody) in
                        let usagesExpandedAncestors = findAllWithAncestors (\e -> List.member e usagesExpanded) letBody |> List.map (Utils.dropLast 1) in
                        let maybeParents = usagesExpandedAncestors |> List.map Utils.maybeLast in
                        let maybeParentSingletonParents =
                          usagesExpandedAncestors
                          |> List.map
                              (\ancestors ->
                                if Utils.maybeLast ancestors |> Maybe.map isSingletonList |> (==) (Just True) then
                                  Utils.dropLast 1 ancestors |> Utils.maybeLast
                                else
                                  Nothing
                              )
                        in
                        let maybeSingleParentAndEffectiveUsages =
                          case Utils.dedup maybeParents of
                            [Just parentExp] -> Just (parentExp, usagesExpanded)
                            _ ->
                              case maybeParentSingletonParents |> Utils.projJusts |> Maybe.map Utils.dedup of
                                Just [grandparentExp] -> Just (grandparentExp, Utils.filterJusts maybeParents) -- maybeParents are all Justs here.
                                _ -> Nothing
                        in
                        case maybeSingleParentAndEffectiveUsages of
                          Just (parentExp, effectiveUsages) ->
                            -- Single, shared parent (or grandparent of singleton lists).
                            case parentExp.val.e__ of
                              EList listWs1 heads listWs2 maybeTail listW3 ->
                                let listName =
                                  let listBaseName =
                                    let prefix = Utils.commonPrefixString idents in
                                    if prefix == "" then
                                      "list"
                                    else
                                      prefix ++ "s"
                                  in
                                  nonCollidingName listBaseName 2 (identifiersSet letBody)
                                in
                                -- In Sketch-n-Sketch there's a lot of concatenation; try it.
                                let eConcatExp listExp = eApp (eVar0 "concat") [listExp] in
                                let eConcat listExps = eApp (eVar0 "concat") [eTuple listExps] in
                                let eAppend listExpA listExpB = eApp (eVar0 "append") (List.map (replacePrecedingWhitespace " ") [listExpA, listExpB]) in
                                let usagePrecedingWhitespace = precedingWhitespace (Utils.head "ExpressionBasedTransform.inlineListSynthesisResults effectiveUsages" effectiveUsages) in
                                let useOldWs e = replacePrecedingWhitespace usagePrecedingWhitespace e in
                                let newListExpCandidates =
                                  -- Must be used in the heads of the list, in order.
                                  case List.map Tuple.second heads |> Utils.splitBy effectiveUsages of
                                    [[], []] ->
                                      -- Heads and target match exactly.
                                      case maybeTail of
                                        Nothing ->
                                          [ eVar listName
                                          , eTuple [eConcatExp (eVar listName) |> useOldWs]
                                          , eTuple [eVar listName              |> useOldWs]
                                          ]

                                        Just tail ->
                                          [ eAppend (eVar listName) tail
                                          , eList [eConcatExp (eVar listName) |> useOldWs] (Just tail)
                                          , eList [eVar listName              |> useOldWs] (Just tail)
                                          ]

                                    [[], restHeads] ->
                                      -- Target occurs at beginning of heads.
                                      case maybeTail of
                                        Nothing ->
                                          [ eAppend (eVar listName) (eTuple restHeads)
                                          , eTuple <| useOldWs (eConcatExp (eVar listName)) :: restHeads
                                          , eTuple <| useOldWs (eVar listName)              :: restHeads
                                          , eList [eConcatExp (eVar listName) |> useOldWs] (Just (eTuple restHeads))
                                          , eList [eVar listName              |> useOldWs] (Just (eTuple restHeads))
                                          ]

                                        Just tail ->
                                          [ eConcat [eVar listName, eTuple restHeads, tail]
                                          , eAppend (eVar listName) (eList restHeads (Just tail))
                                          , eList (useOldWs (eConcatExp (eVar listName)) :: restHeads) (Just tail)
                                          , eList (useOldWs (eVar listName)              :: restHeads) (Just tail)
                                          ]

                                    [restHeads, []] ->
                                      -- Target occurs at end of heads.
                                      case maybeTail of
                                        Nothing ->
                                          [ eAppend (eTuple restHeads) (eVar listName)
                                          , eList restHeads (Just (eVar listName))
                                          , eTuple (restHeads ++ [eConcatExp (eVar listName) |> useOldWs])
                                          , eTuple (restHeads ++ [eVar listName              |> useOldWs])
                                          , eList restHeads (Just (eConcatExp (eVar listName)))
                                          ]

                                        Just tail ->
                                          [ eConcat [eTuple restHeads, eVar listName, tail]
                                          , eList restHeads (Just (eAppend (eVar listName) tail))
                                          , eList (restHeads ++ [eConcatExp (eVar listName) |> useOldWs]) (Just tail)
                                          , eList (restHeads ++ [eVar listName              |> useOldWs]) (Just tail)
                                          ]

                                    [headsBefore, headsAfter] ->
                                      -- Target occurs in the middle of the heads.
                                      case maybeTail of
                                        Nothing ->
                                          [ eConcat [eTuple headsBefore, eVar listName, eTuple headsAfter]
                                          , eTuple (headsBefore ++ [eConcatExp (eVar listName) |> useOldWs] ++ headsAfter)
                                          , eTuple (headsBefore ++ [eVar listName              |> useOldWs] ++ headsAfter)
                                          ]

                                        Just tail ->
                                          [ eConcat [eTuple headsBefore, eVar listName, eTuple headsAfter, tail]
                                          , eList (headsBefore ++ [eConcatExp (eVar listName) |> useOldWs] ++ headsAfter) (Just tail)
                                          , eList (headsBefore ++ [eVar listName              |> useOldWs] ++ headsAfter) (Just tail)
                                          ]

                                    _ ->
                                      []
                                in
                                newListExpCandidates
                                |> List.map
                                    (\newListExp ->
                                      let prettyNewListExp =
                                        case newListExp.val.e__ of
                                          EList _ _ _ _ _ -> copyListWhitespace parentExp newListExp
                                          _               -> newListExp
                                      in
                                      let newLetBody = replaceExpNodePreservingPrecedingWhitespace parentExp.val.eid prettyNewListExp letBody in
                                      let newLet = replaceE__ letExp (ELet letWs1 letKind False (pVar listName) letWs2 letAssign letWs3 newLetBody letWs4) in
                                      ( replaceExpNode newLet.val.eid newLet originalExp
                                      , "Inline " ++ (unparsePat >> Utils.squish) letPat ++ " into " ++ (unparse >> Utils.squish) newListExp
                                      )
                                    )

                              _ ->
                                []

                          _ ->
                            []

                      else
                        []

                    Nothing ->
                      []

                _ ->
                  []

            _ ->
              []
        )
  in
  -- Candidates are produced speculatively.
  -- TODO: Filter out those that error or produce a different result.
  candidatesAndDescription
  |> List.map
      (\(candidateExp, description) ->
        InterfaceModel.synthesisResult description candidateExp
      )


-- Detects clones of duplication at least minCloneCount amount expressions allowed by candidateExpFilter.
-- Clones must be at least minCloneSize after subexps are replaced by argument variables.
--
-- Only returns clones resulting in argCount arguments. Call multiple times to get clones of differing arg counts.
-- (See cloneEliminationSythesisResults below for an example)
--
-- If allowCurrying is True and the clone is a simple function call with the last parameter as an argument, will try removing that last arg.
--
-- Returns List of (Sorted List of (EId, Expression to Replace, Argument Expressions), Replacing Function, Common Scope, Suggested Function Name, Argument Names)
--
-- Suggested function name has *not* been checked for collisions.
--
-- Resulting abstracted functions will evaluate correctly but may not type check (may have more polymorphism in the arguments than the type system can handle).
detectClones : Exp -> (Exp -> Bool) -> Int -> Int -> Int -> Bool -> List (List (EId, Exp, List Exp), Exp, Exp, String, List String)
detectClones originalExp candidateExpFilter minCloneCount minCloneSize argCount allowCurrying =
  let argVar = eVar "INSERT_ARGUMENT_HERE" in
  let isArgVarPlaceholder exp =
    case exp.val.e__ of
      EVar _ ident -> ident == "INSERT_ARGUMENT_HERE"
      _            -> False
  in
  -- Sister function in LangTools.extraExpsDiff
  -- This version returns the various differing subtrees replaced by argVar:
  let merge expA expB =
    case (expA.val.e__, expB.val.e__) of
      (EConst ws1A nA locA wdA,              EConst ws1B nB locB wdB)              -> if nA == nB then expA else argVar
      (EBase ws1A ebvA,                      EBase ws1B ebvB)                      -> if eBaseValsEqual ebvA ebvB then expA else argVar
      (EVar ws1A identA,                     EVar ws1B identB)                     -> if identA == identB then expA else argVar
      (EFun ws1A psA eA ws2A,                EFun ws1B psB eB ws2B)                -> if patternListsEqual psA psB then replaceE__ expA (EFun ws1A psA (merge eA eB) ws2A) else argVar
      (EOp ws1A opA esA ws2A,                EOp ws1B opB esB ws2B)                -> if opA.val == opB.val then Utils.maybeZip esA esB |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EOp ws1A opA newEs ws2A))) |> Maybe.withDefault argVar else argVar
      (EList ws1A esA ws2A Nothing ws3A,     EList ws1B esB ws2B Nothing ws3B)     -> Utils.maybeZip (List.map Tuple.second esA) (List.map Tuple.second esB)
                                                                                        |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EList ws1A (Utils.zip (List.map Tuple.first esA) newEs) ws2A Nothing ws3A)))
                                                                                        |> Maybe.withDefault argVar
      (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)   -> Utils.maybeZip (List.map Tuple.second esA) (List.map Tuple.second esB)
                                                                                        |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EList ws1A (Utils.zip (List.map Tuple.first esA) newEs) ws2A (Just (merge eA eB)) ws3A)))
                                                                                        |> Maybe.withDefault argVar
      (EApp ws1A fA esA appA ws2A,           EApp ws1B fB esB appB ws2B)           -> Utils.maybeZip esA esB |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EApp ws1A (merge fA fB) newEs appA ws2A))) |> Maybe.withDefault argVar
      (ELet ws1A kindA recA pA ws2A e1A ws3A e2A ws4A, ELet _ kindB recB pB _ e1B _ e2B _) -> if recA == recB && patternsEqual pA pB then replaceE__ expA (ELet ws1A kindA recA pA ws2A (merge e1A e1B) ws3A (merge e2A e2B) ws4A) else argVar
      (EIf ws1A e1A ws2A e2A ws3A e3A ws4A,  EIf ws1B e1B ws2B e2B ws3B e3B ws4B)  -> replaceE__ expA (EIf ws1A (merge e1A e1B) ws2A (merge e2A e2B) ws3A (merge e3A e3B) ws4A)
      (ECase ws1A eA branchesA ws2A,         ECase ws1B eB branchesB ws2B)         -> Utils.maybeZip branchesA  branchesB  |> Maybe.andThen (\branchPairs  -> let bValPairs  = branchPairs  |> List.map (\(bA, bB)   -> (bA.val,  bB.val))  in if bValPairs  |> List.all (\(Branch_  bws1A  bpatA   beA  bws2A,  Branch_  bws1B  bpatB   beB  bws2B)  -> patternsEqual bpatA bpatB)   then Just (replaceE__ expA (ECase     ws1A (merge eA eB) (Utils.zip branchPairs  bValPairs  |> List.map (\((bA,  bB),  (Branch_  bws1A  bpatA   beA  bws2A,  Branch_  bws1B  bpatB   beB  bws2B))  -> {bA  | val = Branch_  bws1A  bpatA   (merge beA  beB)  bws2A}))  ws2A)) else Nothing) |> Maybe.withDefault argVar
      (ETypeCase ws1A eA tbranchesA ws2A,    ETypeCase ws1B eB tbranchesB ws2B)    -> Utils.maybeZip tbranchesA tbranchesB |> Maybe.andThen (\tbranchPairs -> let tbValPairs = tbranchPairs |> List.map (\(tbA, tbB) -> (tbA.val, tbB.val)) in if tbValPairs |> List.all (\(TBranch_ tbws1A tbtypeA tbeA tbws2A, TBranch_ tbws1B tbtypeB tbeB tbws2B) -> Types.equal tbtypeA tbtypeB) then Just (replaceE__ expA (ETypeCase ws1A (merge eA eB) (Utils.zip tbranchPairs tbValPairs |> List.map (\((tbA, tbB), (TBranch_ tbws1A tbtypeA tbeA tbws2A, TBranch_ tbws1B tbtypeB tbeB tbws2B)) -> {tbA | val = TBranch_ tbws1A tbtypeA (merge tbeA tbeB) tbws2A})) ws2A)) else Nothing) |> Maybe.withDefault argVar
      (EComment wsA sA e1A,                  _)                                    -> replaceE__ expA (EComment wsA sA (merge e1A expB)) -- Keep only comments in expA.
      (_,                                    EComment wsB sB e1B)                  -> merge expA e1B
      (EOption ws1A s1A ws2A s2A e1A,        EOption ws1B s1B ws2B s2B e1B)        -> argVar
      (ETyp ws1A patA typeA eA ws2A,         ETyp ws1B patB typeB eB ws2B)         -> if patternsEqual patA patB && Types.equal typeA typeB then replaceE__ expA (ETyp ws1A patA typeA (merge eA eB) ws2A) else argVar
      (EColonType ws1A eA ws2A typeA ws3A,   EColonType ws1B eB ws2B typeB ws3B)   -> if Types.equal typeA typeB then replaceE__ expA (EColonType ws1A (merge eA eB) ws2A typeA ws3A) else argVar
      (ETypeAlias ws1A patA typeA eA ws2A,   ETypeAlias ws1B patB typeB eB ws2B)   -> if patternsEqual patA patB && Types.equal typeA typeB then replaceE__ expA (ETypeAlias ws1A patA typeA (merge eA eB) ws2A) else argVar
      (EParens ws1A e1A pStyleA ws2A,        EParens ws1B e1B pStyleB ws2B)        -> replaceE__ expA (EParens ws1A (merge e1A e1B) pStyleA ws2A)
      (EParens ws1A e1A pStyleA ws2A,        _)                                    -> replaceE__ expA (EParens ws1A (merge e1A expB) pStyleA ws2A)
      (_,                                    EParens ws1B e1B pStyleB ws2B)        -> replaceE__ expB (EParens ws1B (merge expA e1B) pStyleB ws2B)
      _                                                                            -> argVar
  in
  -- This version is limited to at most a single argVar: if multiple subtrees differ, their common ancestor becomes a single argVar.
  -- Returns (merged, boolean true if merged contains argVar)
  let mergeSingleArg expA expB =
    let retArgVar = (argVar, True) in
    let retSame   = (expA,   False) in
    -- If at most one child has an argVar, return an exp constructed by newE__Func and an appropriate hasArgVar bool
    -- If more than one child has an argVar, return (argVar, True)
    let generalizedMerge precondition maybeE1Pair maybeE2Pair maybeE3Pair maybePairOfEs newE__Func =
      if precondition then
        -- Ensure both lists are the same length.
        -- If "Nothing" given for lists, empty lists will produce the desired results.
        case maybePairOfEs |> Maybe.withDefault ([], []) |> (\(esA, esB) -> Utils.maybeZip esA esB) of
          Nothing ->
            retArgVar

          Just expPairs ->
            -- Now, see what happens when we merged the given pairs of children.
            let (e1Merged, e1HasArgVar) = maybeE1Pair |> Maybe.map (\(eA, eB) -> mergeSingleArg eA eB) |> Maybe.withDefault (eVar "ignored", False) in
            let (e2Merged, e2HasArgVar) = maybeE2Pair |> Maybe.map (\(eA, eB) -> mergeSingleArg eA eB) |> Maybe.withDefault (eVar "ignored", False) in
            let (e3Merged, e3HasArgVar) = maybeE3Pair |> Maybe.map (\(eA, eB) -> mergeSingleArg eA eB) |> Maybe.withDefault (eVar "ignored", False) in
            let (esMergers, esHasArgVarBools) = expPairs |> List.map (\(eA, eB) -> mergeSingleArg eA eB) |> List.unzip in
            let argVarCount = Utils.count ((==) True) (e1HasArgVar::e2HasArgVar::e3HasArgVar::esHasArgVarBools) in
            if argVarCount <= 1 then
              let newE__ = newE__Func e1Merged e2Merged e3Merged esMergers in
              (replaceE__ expA newE__, argVarCount == 1)
            else
              retArgVar

      else
        retArgVar
    in
    case (expA.val.e__, expB.val.e__) of
      (EConst ws1A nA locA wdA,              EConst ws1B nB locB wdB)              -> if nA == nB then retSame else retArgVar
      (EBase ws1A ebvA,                      EBase ws1B ebvB)                      -> if eBaseValsEqual ebvA ebvB then retSame else retArgVar
      (EVar ws1A identA,                     EVar ws1B identB)                     -> if identA == identB then retSame else retArgVar
      (EFun ws1A psA eA ws2A,                EFun ws1B psB eB ws2B)                -> generalizedMerge (patternListsEqual psA psB) (Just (eA, eB)) Nothing Nothing Nothing (\mergedBody _ _ _ -> EFun ws1A psA mergedBody ws2A)
      (EOp ws1A opA esA ws2A,                EOp ws1B opB esB ws2B)                -> generalizedMerge (opA.val == opB.val) Nothing Nothing Nothing (Just (esA, esB)) (\_ _ _ mergedEs -> EOp ws1A opA mergedEs ws2A)
      (EList ws1A esA ws2A Nothing ws3A,     EList ws1B esB ws2B Nothing ws3B)     -> generalizedMerge True Nothing Nothing Nothing
                                                                                        (Just (List.map Tuple.second esA, List.map Tuple.second esB))
                                                                                        (\_ _ _ headMergers -> EList ws1A (Utils.zip (List.map Tuple.first esA) headMergers) ws2A Nothing ws3A)
      (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)   -> generalizedMerge True (Just (eA, eB)) Nothing Nothing
                                                                                        (Just (List.map Tuple.second esA, List.map Tuple.second esB))
                                                                                        (\tailMerged _ _ headMergers -> EList ws1A (Utils.zip (List.map Tuple.first esA) headMergers) ws2A (Just tailMerged) ws3A)
      (EApp ws1A fA esA appA ws2A,           EApp ws1B fB esB appB ws2B)           -> generalizedMerge True (Just (fA, fB)) Nothing Nothing (Just (esA, esB)) (\fMerged _ _ argMergers -> EApp ws1A fMerged argMergers appA ws2A)
      (ELet ws1A kindA recA pA ws2A e1A ws3A e2A ws4A, ELet _ kindB recB pB _ e1B _ e2B _) -> generalizedMerge (recA == recB && patternsEqual pA pB) (Just (e1A, e1B)) (Just (e2A, e2B)) Nothing Nothing (\e1Merged e2Merged _ _ -> ELet ws1A kindA recA pA ws2A e1Merged ws3A e2Merged ws4A)
      (EIf ws1A e1A ws2A e2A ws3A e3A ws4A,  EIf ws1B e1B ws2B e2B ws3B e3B ws4B)  -> generalizedMerge True (Just (e1A, e1B)) (Just (e2A, e2B)) (Just (e3A, e3B)) Nothing (\e1Merged e2Merged e3Merged _ -> EIf ws1A e1Merged ws2A e2Merged ws3A e3Merged ws4A)
      (ECase ws1A eA branchesA ws2A,         ECase ws1B eB branchesB ws2B)         ->
        let precondition = Utils.listsEqualBy patternsEqual (branchPats branchesA) (branchPats branchesB) in
        generalizedMerge precondition (Just (eA, eB)) Nothing Nothing (Just (branchExps branchesA, branchExps branchesB)) (\eMerged _ _ branchExpsMerged -> ECase ws1A eMerged (List.map2 replaceBranchExp branchesA branchExpsMerged) ws2A)
      (ETypeCase ws1A eA tbranchesA ws2A,    ETypeCase ws1B eB tbranchesB ws2B)  ->
        let precondition = Utils.listsEqualBy Types.equal (tbranchTypes tbranchesA) (tbranchTypes tbranchesB) in
        generalizedMerge precondition (Just (eA, eB)) Nothing Nothing (Just (tbranchExps tbranchesA, tbranchExps tbranchesB)) (\eMerged _ _ tbranchExpsMerged -> ETypeCase ws1A eMerged (List.map2 replaceTBranchExp tbranchesA tbranchExpsMerged) ws2A)
      (EComment wsA sA e1A,                  _)                                    -> mergeSingleArg e1A expB |> (\(e1Merged, e1HasArgVar) -> (replaceE__ expA (EComment wsA sA e1Merged), e1HasArgVar))
      (_,                                    EComment wsB sB e1B)                  -> mergeSingleArg expA e1B
      (EOption ws1A s1A ws2A s2A e1A,        EOption ws1B s1B ws2B s2B e1B)        -> retArgVar
      (ETyp ws1A patA typeA eA ws2A,         ETyp ws1B patB typeB eB ws2B)         -> generalizedMerge (patternsEqual patA patB && Types.equal typeA typeB) (Just (eA, eB)) Nothing Nothing Nothing (\mergedE _ _ _ -> ETyp ws1A patA typeA mergedE ws2A)
      (EColonType ws1A eA ws2A typeA ws3A,   EColonType ws1B eB ws2B typeB ws3B)   -> generalizedMerge (Types.equal typeA typeB) (Just (eA, eB)) Nothing Nothing Nothing (\mergedE _ _ _ -> EColonType ws1A mergedE ws2A typeA ws3A)
      (ETypeAlias ws1A patA typeA eA ws2A,   ETypeAlias ws1B patB typeB eB ws2B)   -> generalizedMerge (patternsEqual patA patB && Types.equal typeA typeB) (Just (eA, eB)) Nothing Nothing Nothing (\mergedE _ _ _ -> ETypeAlias ws1A patA typeA mergedE ws2A)
      (EParens ws1A e1A pStyleA ws2A,        EParens ws1B e1B pStyleB ws2B)        -> mergeSingleArg e1A e1B  |> (\(e1Merged, e1HasArgVar) -> (replaceE__ expA (EParens ws1A e1Merged pStyleA ws2A), e1HasArgVar))
      (EParens ws1A e1A pStyleA ws2A,        _)                                    -> mergeSingleArg e1A expB |> (\(e1Merged, e1HasArgVar) -> (replaceE__ expA (EParens ws1A e1Merged pStyleA ws2A), e1HasArgVar))
      (_,                                    EParens ws1B e1B pStyleB ws2B)        -> mergeSingleArg expA e1B |> (\(e1Merged, e1HasArgVar) -> (replaceE__ expB (EParens ws1B e1Merged pStyleB ws2B), e1HasArgVar))
      _                                                                            -> retArgVar
  in
  let goThroughMergedAndMakeParentArgIfAllChildrenAreArgs merged =
    merged
    |> mapExp -- folds bottom up, so this can bubble.
        (\e ->
          case childExps e of
            []  -> e
            [_] -> e
            children -> if List.all (expEffectiveExp >> isArgVarPlaceholder) children then argVar else e
        )
  in
  let mergeFunc = if argCount == 1 then (\eA eB -> mergeSingleArg eA eB |> Tuple.first) else merge in
  let argVarCount exp =
    flattenExpTree exp |> Utils.count isArgVarPlaceholder
  in
  subExpsOfSizeAtLeast minCloneSize originalExp
  |> List.filter (\e -> not <| isComment e || isOption e) -- Comments and options should not be a base expression of a clone
  |> List.filter candidateExpFilter
  |> List.foldl
      (\exp mergeGroups ->
        let addedMergeGroups =
          mergeGroups
          |> List.filterMap
              (\(priorMerged, priorExps) ->
                let newMerged = mergeFunc priorMerged exp in
                -- Node counts can only go down on subsequent mergings, so this is safe.
                if nodeCount newMerged >= minCloneSize then
                  Just (newMerged, exp::priorExps)
                else
                  Nothing
              )
        in
        (exp, [exp])::(mergeGroups ++ addedMergeGroups)
      )
      []
  |> List.filter (\(merged, exps) -> List.length exps >= minCloneCount)
  |> List.concatMap (\(merged, exps) -> [merged, goThroughMergedAndMakeParentArgIfAllChildrenAreArgs merged] |> Utils.dedup |> List.map (\newMerged -> (newMerged, exps))) -- Step to combine e.g. `[arg, arg]` into a single arg
  |> List.filter (\(merged, exps) -> argVarCount merged == argCount)
  |> List.map (\(merged, exps) -> (merged, exps |> List.sortBy parsedThingToLocation))
  |> List.map (\(merged, sortedExps) -> (merged, sortedExps, sortedExps |> List.map (\exp -> extraExpsDiff merged exp)))
  |> List.filter (\(merged, sortedExps, parameterExpLists) -> List.all (\e -> isLiteral e || nodeCount e <= 3) (List.concat parameterExpLists)) -- Exps that will become calling arguments must have no free variables or be small.
  |> List.sortBy (\(merged, sortedExps, parameterExpLists) -> -(List.length sortedExps)) -- For each abstraction, perserve only the largest set of clones matching it
  |> Utils.dedupBy (\(merged, sortedExps, parameterExpLists) -> LangUnparser.unparseWithUniformWhitespace False False merged)
  |> List.map
      (\(merged, sortedExps, parameterExpLists) ->
        let eidsToReplace = sortedExps |> List.map (.val >> .eid) in
        let commonScope = deepestCommonAncestorWithNewline originalExp (\exp -> List.member exp.val.eid eidsToReplace) in
        let funcSuggestedName =
          let defaultName = if simpleExpName merged == "INSERT_ARGUMENT_HERE" then "thing" else simpleExpName merged in
          commonNameForEIdsWithDefault defaultName originalExp eidsToReplace
        in
        -- Number the usages of the arguments as 1 2 3 in the merged expression.
        let (mergedArgUsesEnumerated, _) =
          -- mapFoldExp and extraExpsDiff visit leaves in exactly the reserve order: so start with argCount and count down.
          merged
          |> mapFoldExp
              (\e argN ->
                case e.val.e__ of
                  EVar ws "INSERT_ARGUMENT_HERE" -> (replaceE__ e (EVar ws ("INSERT_ARGUMENT" ++ toString argN ++ "_HERE")), argN-1)
                  _                              -> (e, argN)
              )
              argCount
        in
        let funcWithPlaceholders =
          -- If you ever do more than pVars here, you will need to modify repeatByIndexedMerge
          let argList = List.map (\n -> (if n == 1 then pVar0 else pVar) ("INSERT_ARGUMENT" ++ toString n ++ "_HERE")) (List.range 1 argCount) in
          let fBody =
            mergedArgUsesEnumerated
            |> LangSimplify.changeRenamedVarsToOuter
            |> LangSimplify.removeUnusedLetPats
            |> unindent
          in
          let fBodyReflowed =
            let multipleLinesForFunction =
              -- Rather different logic than LangTools.reflowLetWhitespace :/
              not (String.contains "\n" (LangUnparser.unparse fBody))
              && longLineLength < String.length (LangUnparser.unparseWithUniformWhitespace True True (eFun argList fBody))
            in
            if multipleLinesForFunction
            then replacePrecedingWhitespace "\n" fBody
            else ensureWhitespaceExp fBody
          in
          let explicitFunc = eFun argList fBodyReflowed in
          -- Try to curry once, if body is a simple function application and flag given
          if not allowCurrying || argCount >= 2 then
            explicitFunc
          else
            case fBody.val.e__ of
              (EApp ws1 funcE args appType ws2) ->
                case Utils.takeLast 1 args of
                  [lastArg] ->
                    case lastArg.val.e__ of
                      EVar _ "INSERT_ARGUMENT1_HERE" ->
                        if List.length args >= 2 then
                          replaceE__ fBody (EApp ws1 funcE (List.take (List.length args - 1) args) appType ws2)
                          |> replacePrecedingWhitespace " " -- Presume a single line application.
                        else
                          -- funcE is almost certainly an EVar
                          replacePrecedingWhitespace " " funcE

                      _ ->
                        explicitFunc
                  _ ->
                    explicitFunc

              _ ->
                explicitFunc
        in
        -- Finally, name and replace arguments
        let (argRenamingsList, _) =
          Utils.maybeZipN parameterExpLists -- Go from lists of parameters per call to lists of parameters per arg
          |> Utils.fromJust_ "ExpressionBasedTransform.detectClones maybeZipN"
          |> Utils.foldli1
              (\(argN, parametersForArg) (renamings, usedNames) ->
                let argBaseName =
                  let defaultName = if argCount == 0 then "arg" else "arg" ++ toString argN in
                  commonNameForEIdsWithDefault defaultName originalExp (List.map (.val >> .eid) parametersForArg)
                in
                let argName = nonCollidingName argBaseName 2 usedNames in
                ( renamings ++ [("INSERT_ARGUMENT" ++ toString argN ++ "_HERE", argName)]
                , Set.insert argName usedNames
                )
              )
              ([], identifiersSet funcWithPlaceholders)
        in
        let (_, argNames) = List.unzip argRenamingsList in
        let abstractedFunc =
          renameIdentifiers (Dict.fromList argRenamingsList) funcWithPlaceholders
        in
        ( Utils.zip3 eidsToReplace sortedExps parameterExpLists
        , abstractedFunc
        , commonScope
        , funcSuggestedName
        , argNames
        )
      )


-- Ensure program won't crash or have bad behavior because we lifted a variable usage out of the variable's scope.
noExtraneousFreeVarsInRemovedClones : List Exp -> Exp -> Bool
noExtraneousFreeVarsInRemovedClones cloneExps commonScopeWhereAbstractionWillBeDefined =
  -- In the current clone detection, parameters necessarily have no free variables so
  -- we don't have to worry about allowing the new function parameters to be free.
  let freeAtAbstraction = freeVars commonScopeWhereAbstractionWillBeDefined in
  cloneExps
  |> List.all
      (\cloneExp ->
        freeVars cloneExp |> List.all (\var -> List.member var freeAtAbstraction)
      )

-- Returns List of (Sorted List of (EId, Expression to Replace, Argument Expressions), Replacing Function, Common Scope, Suggested Function Name, Argument Names)
detectClonesOfVariousSizes : (Exp -> Bool) -> Int -> Int -> List Int -> Bool -> Exp -> List (List (EId, Exp, List Exp), Exp, Exp, String, List String)
detectClonesOfVariousSizes candidateExpFilter minCloneCount minCloneSizeToArgumentRatio sizes allowCurrying originalExp =
  sizes
  |> List.concatMap
      (\size ->
        detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * size) size allowCurrying
      )
  |> List.filter
      (\(cloneEIdsAndExpsAndParameterExpLists, _, commonScope, _, _) ->
        let (_, cloneExps, _) = Utils.unzip3 cloneEIdsAndExpsAndParameterExpLists in
         noExtraneousFreeVarsInRemovedClones cloneExps commonScope
      )


cloneEliminationSythesisResults : (Exp -> Bool) -> Int -> Int -> Exp -> List InterfaceModel.SynthesisResult
cloneEliminationSythesisResults candidateExpFilter minCloneCount minCloneSizeToArgumentRatio originalExp =
  detectClonesOfVariousSizes candidateExpFilter minCloneCount minCloneSizeToArgumentRatio (List.range 1 8) False originalExp
  |> List.map
      (\(cloneEIdsAndExpsAndParameterExpLists, abstractedFunc, commonScope, funcSuggestedName, argNames) ->
        let funcName = nonCollidingName funcSuggestedName 2 (identifiersSet commonScope) in
        let oldIndentation = indentationOf commonScope in
        let abstractedFuncIndented =
          replaceIndentation ("  " ++ oldIndentation) abstractedFunc
        in
        let eidToNewE__ =
          cloneEIdsAndExpsAndParameterExpLists
          |> List.map (\(eid, _, parameterExps) -> (eid, EApp space1 (eVar0 funcName) (List.map (replacePrecedingWhitespace " ") parameterExps) SpaceApp space0))
          |> Dict.fromList
        in
        let usagesReplaced = applyESubstPreservingPrecedingWhitespace eidToNewE__ commonScope in
        let wrapped =
          newLetFancyWhitespace -1 False (pVar funcName) abstractedFuncIndented usagesReplaced originalExp
        in
        let newProgram = replaceExpNode commonScope.val.eid wrapped originalExp in
        let clonesName =
          let (eidsToReplace, _, _) = Utils.unzip3 cloneEIdsAndExpsAndParameterExpLists in
          let name = commonNameForEIds originalExp eidsToReplace in
          if name == "" then
            eidsToReplace |> List.map (expNameForEId originalExp) |> Utils.toSentence
          else
            toString (List.length cloneEIdsAndExpsAndParameterExpLists) ++ " " ++ name ++ "s"
        in
        InterfaceModel.synthesisResult
            ("Merge " ++ clonesName ++ " by abstracting over " ++ Utils.toSentence argNames)
            newProgram
      )


mapAbstractSynthesisResults : Exp -> List InterfaceModel.SynthesisResult
mapAbstractSynthesisResults originalExp =
  detectClonesOfVariousSizes (always True) 3 1 [1] True originalExp -- at least three clones, at least size 3*1 = 3, exactly 1 argument, allow currying
  |> List.map
      (\(cloneEIdsAndExpsAndParameterExpLists, abstractedFunc, commonScope, funcSuggestedName, argNames) ->
        let (eidsToReplace, sortedExps, parameterExpLists) = Utils.unzip3 cloneEIdsAndExpsAndParameterExpLists in
        let oldIndentation = indentationOf commonScope in
        let mapCall =
          -- Multiline or single line map call, depending on mapping function
          -- let _ = Debug.log "mapping func" (unparse abstractedFunc) in
          let parameterExps = List.concat parameterExpLists in -- each paramter list should only have one element (single parameter)
          if String.contains "\n" (unparse abstractedFunc) then
            let newLineIndent extraIndent exp = replacePrecedingWhitespace ("\n" ++ extraIndent ++ oldIndentation) exp in
            eApp
                (eVar0 "map")
                [ replacePrecedingWhitespace " " (indent ("      " ++ oldIndentation) abstractedFunc) -- Put arguments on same line as map call.
                , newLineIndent "    " (eTuple (setExpListWhitespace "" " " parameterExps))
                ]
            |> newLineIndent "  "
          else
            eApp (eVar0 "map") [abstractedFunc, eTuple (setExpListWhitespace "" " " parameterExps)]
        in
        let namesToAvoid = identifiersSet commonScope in
        let varNames = sortedExps |> Utils.mapi1 (\(i, _) -> nonCollidingName (funcSuggestedName ++ toString i) 2 namesToAvoid) in
        let eidToVarE__ = Utils.zip eidsToReplace (varNames |> List.map (\name -> EVar space1 name)) |> Dict.fromList in
        let usagesReplaced = applyESubstPreservingPrecedingWhitespace eidToVarE__ commonScope in
        let wrapped =
          newLetFancyWhitespace -1 False (pListOfPVars varNames) mapCall usagesReplaced originalExp
          -- let letKind = if isTopLevel commonScope originalExp then Def else Let in
          -- withDummyExpInfo <| ELet (ws <| "\n" ++ oldIndentation) letKind False (pListOfPVars varNames) space1 mapCall space1 usagesReplaced space0
        in
        let newProgram = replaceExpNode commonScope.val.eid wrapped originalExp in
        let clonesName =
          if Utils.commonPrefixString varNames /= "" then
            toString (List.length cloneEIdsAndExpsAndParameterExpLists) ++ " " ++ Utils.commonPrefixString varNames ++ "s"
          else
            eidsToReplace |> List.map (expNameForEId originalExp) |> Utils.toSentence
        in
        InterfaceModel.synthesisResult
            ("Merge " ++ clonesName ++ " by mapping over " ++ String.join " " argNames)
            newProgram
      )


-- Repeat by Indexed Merge
--
-- Naively assumes the order of expressions in the program is the same as their order in the output.
--
-- Performs three steps that previously you had to do separately: 1. map-merge 2. rename simple renamings to outer 3. inline list
--
-- Not amazing because it does whole-program transforms for the last two steps (LangSimplify.simplify |> inlineListSynthesisResults) which may impact other code.
repeatByIndexedMerge : Model -> (Exp -> Bool) -> Int -> Int -> Exp -> List InterfaceModel.SynthesisResult
repeatByIndexedMerge model candidateExpFilter minCloneCount minCloneSizeToArgumentRatio originalExp =
  let (oldListItemsCount, oldShapeTree) = -- After DrawAddShape.addShape
    case InterfaceModel.runAndResolveAtContext model originalExp of
      Ok (val, _, (root, shapeTree), _, _) -> (vListToMaybeValsExcludingPoint val |> Maybe.map List.length |> Maybe.withDefault 1, shapeTree)
      _                                    -> (0, Dict.empty)
  in
  detectClonesOfVariousSizes candidateExpFilter minCloneCount minCloneSizeToArgumentRatio (List.range 1 8) False originalExp
  |> List.concatMap
      (\(cloneEIdsAndExpsAndParameterExpLists, abstractedFunc, commonScope, funcSuggestedName, argNames) ->
        let (eidsToReplace, sortedExps, parameterExpLists) = Utils.unzip3 cloneEIdsAndExpsAndParameterExpLists in
        let oldIndentation = indentationOf commonScope in
        let itemCount = List.length eidsToReplace in
        let mapCallOptionAndDescs =
          let abstractedFuncWithPBEHoles =
            let
              freeVarsInFunc = abstractedFunc |> expToFuncBody |> freeVars
              parameterExpsTransposed = Utils.maybeZipN parameterExpLists |> Utils.fromJust_ "ExpressionBasedTransform.repeatByIndexedMerge parameterExpsTransposed = Utils.maybeZipN parameterExpLists failed"
              varSubst =
                Utils.zip (expToFuncPats abstractedFunc) parameterExpsTransposed
                |> List.map
                    (\(patVar, expressions) ->
                      let
                        targetIdent = patToMaybePVarIdent patVar |> Utils.fromJust_ "ExpressionBasedTransform.repeatByIndexedMerge expected func pats from clone detection to only be pVars"
                        pbeHole = eHolePBE (List.map (replacePrecedingWhitespace " ") expressions)
                      in
                      (targetIdent, (always pbeHole))
                    )
                |> Dict.fromList
            in
            abstractedFunc |> mapFuncBody (transformVarsUntilBound varSubst) |> mapFuncPats (always [pVar0 "i"])
          in
          let rangeExpOptions =
            let zeroToN = eCall "zeroTo" [withDummyExpInfo <| EConst space1 (toFloat itemCount ) (dummyLoc_ unann) (intSlider 0 (5*itemCount))] in
            [ zeroToN
            , eCall "reverse" [zeroToN]
            ]
          in
          let newLineIndent extraIndent exp = replacePrecedingWhitespace ("\n" ++ extraIndent ++ oldIndentation) exp in
          rangeExpOptions
          |> List.map
              (\rangeExp ->
                eApp
                    (eVar0 "map") -- or concatMap
                    [ replacePrecedingWhitespace " " (indent ("      " ++ oldIndentation) (eParens <| removePrecedingWhitespace abstractedFuncWithPBEHoles)) -- Put arguments on same line as map call.
                    , newLineIndent "    " rangeExp
                    ]
                |> newLineIndent "  "
                |> (\mapCall -> (mapCall, Syntax.unparser Syntax.Elm rangeExp |> Utils.squish) )
              )
        in
        let namesToAvoid = identifiersSet commonScope in
        let varNames = sortedExps |> Utils.mapi1 (\(i, _) -> nonCollidingName (funcSuggestedName ++ toString i) 2 namesToAvoid) in
        let eidToVarE__ = Utils.zip eidsToReplace (varNames |> List.map (\name -> EVar space1 name)) |> Dict.fromList in
        let usagesReplaced = applyESubstPreservingPrecedingWhitespace eidToVarE__ commonScope in
        mapCallOptionAndDescs
        |> List.concatMap
            (\(mapCall, rangeDesc) ->
              let wrapped =
                newLetFancyWhitespace -1 False (pListOfPVars varNames) mapCall usagesReplaced originalExp
                -- let letKind = if isTopLevel commonScope originalExp then Def else Let in
                -- withDummyExpInfo <| ELet (ws <| "\n" ++ oldIndentation) letKind False (pListOfPVars varNames) space1 mapCall space1 usagesReplaced space0
              in
              let newProgram = replaceExpNode commonScope.val.eid wrapped originalExp in
              let clonesName =
                if Utils.commonPrefixString varNames /= "" then
                  toString (List.length cloneEIdsAndExpsAndParameterExpLists) ++ " " ++ Utils.commonPrefixString varNames ++ "s"
                else
                  eidsToReplace |> List.map (expNameForEId originalExp) |> Utils.toSentence
              in
              newProgram
              |> LangSimplify.simplify
              |> inlineListSynthesisResults
              |> List.filter
                  (\result ->
                    let newProgram = resultExp result in
                    -- Filter similar to DrawAddShape.addShape
                    case InterfaceModel.runAndResolveAtContext model newProgram of
                      Ok (val, _, (root, shapeTree), _, _) ->
                        Dict.size oldShapeTree <= Dict.size shapeTree && -- Removing shapes signifies a type error
                        oldListItemsCount <= (vListToMaybeValsExcludingPoint val |> Maybe.map List.length |> Maybe.withDefault 1) -- Removing items signifies a type error
                      Err _ ->
                        False
                  )
              |> List.map (InterfaceModel.mapResultDescription (always <| "Repeat " ++ clonesName ++ " by mapping over " ++ rangeDesc ++ " producing " ++ toString (Utils.count isPBEHole <| flattenExpTree mapCall) ++ " holes"))
            )
      )

