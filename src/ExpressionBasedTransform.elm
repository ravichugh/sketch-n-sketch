module ExpressionBasedTransform exposing -- in contrast to ValueBasedTransform
  ( passiveSynthesisSearch
  , cloneEliminationSythesisResults
  , groupSelectedBlobs
  , abstractSelectedBlobs
  , replicateSelectedBlob
  -- , duplicateSelectedBlobs
  , mergeSelectedBlobs
  , deleteSelectedBlobs
  , anchorOfSelectedFeatures
  , groupSelectedBlobsAround
  )

import Lang exposing (..)
import LangUnparser exposing (unparse, unparsePat)
import LangSvg exposing (NodeId)
import ShapeWidgets exposing (PointFeature, SelectableFeature)
import Blobs exposing (..)
import LangTools exposing (..)
import LangSimplify
import Types
import InterfaceModel exposing (Model, ReplicateKind(..), resultExp)
import Utils
import Keys

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
              let maybeNums = Utils.listValues es |> List.map expToMaybeNum |> Utils.projJusts in
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
                    let insertedLoc = dummyLoc_ (if List.all isFrozenNumber (Utils.listValues es) then frozen else unann) in
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
                                  case Utils.listValues heads |> Utils.splitBy effectiveUsages of
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


-- Returns List of (Sorted List of (EId, Expression to Replace, Argument Expressions), Replacing Function, Common Scope, Suggested Function Name, Argument Names)
--
-- Suggested function name has *not* been checked for collisions.
--
-- Resulting abstracted functions will evaluate correctly but may not type check (may have more polymorphism in the arguments than the type system can handle).
detectClones : Exp -> (Exp -> Bool) -> Int -> Int -> Int -> Bool -> List (List (EId, Exp, List Exp), Exp, Exp, String, List String)
detectClones originalExp candidateExpFilter minCloneCount minCloneSize argCount allowCurrying =
  let argVar = eVar "INSERT_ARGUMENT_HERE" in
  -- Sister function in LangTools.extraExpsDiff
  -- This version returns the various differing subtrees replaced by argVar:
  let merge expA expB =
    case (expA.val.e__, expB.val.e__) of
      (EConst ws1A nA locA wdA,              EConst ws1B nB locB wdB)              -> if nA == nB then expA else argVar
      (EBase ws1A ebvA,                      EBase ws1B ebvB)                      -> if eBaseValsEqual ebvA ebvB then expA else argVar
      (EVar ws1A identA,                     EVar ws1B identB)                     -> if identA == identB then expA else argVar
      (EFun ws1A psA eA ws2A,                EFun ws1B psB eB ws2B)                -> if patternListsEqual psA psB then replaceE__ expA (EFun ws1A psA (merge eA eB) ws2A) else argVar
      (EOp ws1A opA esA ws2A,                EOp ws1B opB esB ws2B)                -> if opA.val == opB.val then Utils.maybeZip esA esB |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EOp ws1A opA newEs ws2A))) |> Maybe.withDefault argVar else argVar
      (EList ws1A esA ws2A Nothing ws3A,     EList ws1B esB ws2B Nothing ws3B)     -> Utils.maybeZip (Utils.listValues esA) (Utils.listValues esB)
                                                                                        |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EList ws1A (Utils.listValuesMake esA newEs) ws2A Nothing ws3A)))
                                                                                        |> Maybe.withDefault argVar
      (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)   -> Utils.maybeZip (Utils.listValues esA) (Utils.listValues esB)
                                                                                        |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EList ws1A (Utils.listValuesMake esA newEs) ws2A (Just (merge eA eB)) ws3A)))
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
                                                                                        (Just (Utils.listValues esA, Utils.listValues esB))
                                                                                        (\_ _ _ headMergers -> EList ws1A (Utils.listValuesMake esA headMergers) ws2A Nothing ws3A)
      (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)   -> generalizedMerge True (Just (eA, eB)) Nothing Nothing
                                                                                        (Just (Utils.listValues esA, Utils.listValues esB))
                                                                                        (\tailMerged _ _ headMergers -> EList ws1A (Utils.listValuesMake esA headMergers) ws2A (Just tailMerged) ws3A)
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
  let mergeFunc = if argCount == 1 then (\eA eB -> mergeSingleArg eA eB |> Tuple.first) else merge in
  let argVarCount exp =
    flattenExpTree exp
    |> Utils.count
        (\exp ->
          case exp.val.e__ of
            EVar _ ident -> ident == "INSERT_ARGUMENT_HERE"
            _            -> False
        )
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
  |> List.filter (\(merged, exps) -> List.length exps >= minCloneCount && argVarCount merged == argCount)
  |> List.map (\(merged, exps) -> (merged, exps |> List.sortBy (\exp -> (exp.start.line, exp.start.col))))
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

cloneEliminationSythesisResults : (Exp -> Bool) -> Int -> Int -> Exp -> List InterfaceModel.SynthesisResult
cloneEliminationSythesisResults candidateExpFilter minCloneCount minCloneSizeToArgumentRatio originalExp =
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 1) 1 False ++
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 2) 2 False ++
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 3) 3 False ++
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 4) 4 False ++
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 5) 5 False ++
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 6) 6 False ++
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 7) 7 False ++
  detectClones originalExp candidateExpFilter minCloneCount (minCloneSizeToArgumentRatio * 8) 8 False
  |> List.filter
      (\(cloneEIdsAndExpsAndParameterExpLists, _, commonScope, _, _) ->
        let (_, cloneExps, _) = Utils.unzip3 cloneEIdsAndExpsAndParameterExpLists in
         noExtraneousFreeVarsInRemovedClones cloneExps commonScope
      )
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
  detectClones originalExp (always True) 3 3 1 True
  |> List.filter
      (\(cloneEIdsAndExpsAndParameterExpLists, _, commonScope, _, _) ->
        let (_, cloneExps, _) = Utils.unzip3 cloneEIdsAndExpsAndParameterExpLists in
         noExtraneousFreeVarsInRemovedClones cloneExps commonScope
      )
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
          let letKind = if isTopLevel commonScope originalExp then Def else Let in
          withDummyExpInfo <| ELet (ws <| "\n" ++ oldIndentation) letKind False (pListOfPVars varNames) space1 mapCall space1 usagesReplaced space0
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


--------------------------------------------------------------------------------
-- Group Blobs

selectedBlobsToSelectedNiceBlobs : Model -> List BlobExp -> List (Int, Exp, NiceBlob)
selectedBlobsToSelectedNiceBlobs model blobs =
  let selectedExps =
    List.filter (flip Dict.member model.selectedBlobs << Tuple.first)
                (Utils.zip (List.range 1 (List.length blobs)) blobs)
  in
  Utils.filterJusts <|
    List.map
       (\(i, be) ->
         case be of
           NiceBlob e niceBlob -> Just (i, e, niceBlob)
           _                   -> Nothing
       )
       selectedExps

matchesAnySelectedVarBlob_ : List (Int, Exp, NiceBlob) -> TopDef -> Maybe Ident
matchesAnySelectedVarBlob_ selectedNiceBlobs def =
  let findBlobForIdent y =
    let foo (_,_,niceBlob) =
      case niceBlob of
        VarBlob x        -> x == y
        WithBoundsBlob _ -> False
        WithAnchorBlob _ -> False
        CallBlob _       -> False
    in
    case Utils.findFirst foo selectedNiceBlobs of
      Just (_, _, VarBlob x) -> Just x
      _                      -> Nothing
  in
  let (_,p,_,_) = def in
  case p.val.p__ of
    PVar _ y _  -> findBlobForIdent y
    PAs _ y _ _ -> findBlobForIdent y
    _           -> Nothing

matchesAnySelectedVarBlob selectedNiceBlobs def =
  case matchesAnySelectedVarBlob_ selectedNiceBlobs def of
    Just _  -> True
    Nothing -> False

-- TODO refactor/combine with above
matchesAnySelectedCallBlob_ : List (Int, Exp, NiceBlob) -> TopDef -> Maybe Ident
matchesAnySelectedCallBlob_ selectedNiceBlobs def =
  let findBlobForIdent y =
    let foo (_,_,niceBlob) =
      case niceBlob of
        VarBlob _                -> False
        WithBoundsBlob (_, f, _) -> f == y
        WithAnchorBlob (_, f, _) -> f == y
        CallBlob (f, _)          -> f == y
    in
    case Utils.findFirst foo selectedNiceBlobs of
      Just (_, _, CallBlob (f, _))          -> Just f
      Just (_, _, WithBoundsBlob (_, f, _)) -> Just f
      Just (_, _, WithAnchorBlob (_, f, _)) -> Just f
      _                                     -> Nothing
  in
  let (_,p,_,_) = def in
  case p.val.p__ of
    PVar _ y _  -> findBlobForIdent y
    PAs _ y _ _ -> findBlobForIdent y
    _           -> Nothing

matchesAnySelectedCallBlob selectedNiceBlobs def =
  case matchesAnySelectedCallBlob_ selectedNiceBlobs def of
    Just _  -> True
    Nothing -> False

-- TODO
matchesAnySelectedBlob selectedNiceBlobs def =
  case matchesAnySelectedVarBlob_ selectedNiceBlobs def of
    Just _  -> True
    Nothing ->
      case matchesAnySelectedCallBlob_ selectedNiceBlobs def of
        Just _  -> True
        Nothing -> False

groupAndRearrange model newGroup defs blobs selectedNiceBlobs
    groupDefs eSubst finalExpOfNewGroup =
  let (pluckedBlobs, beforeBlobs, afterBlobs) =
    let indexedBlobs = Utils.zip (List.range 1 (List.length blobs)) blobs in
    let matches (i,_) = Dict.member i model.selectedBlobs in
    let (plucked_, before_, after_) = pluckFromList matches indexedBlobs in
    (List.map Tuple.second plucked_, List.map Tuple.second before_, List.map Tuple.second after_)
  in
  let defs_ =
    let matches = matchesAnySelectedBlob selectedNiceBlobs in
    let (pluckedDefs, beforeDefs, afterDefs) =
      -- TODO make safe again
      -- let (plucked, before, after) = pluckFromList matches defs in
      let (plucked, before, after) = unsafePluckFromList matches defs in
      let getExps = List.map (\(_,_,e,_) -> e) in
      let (beforeInside, beforeOutside) =
        List.foldr
           (\beforeDef (acc1,acc2) ->
             -- if needed, could split a multi-binding into smaller chunks
             let (_,p,_,_) = beforeDef in
             let vars = varsOfPat p in
             let someVarAppearsIn e = let free = freeIdentifiers e in List.any (\ident -> Set.member ident free) vars in
             let noVarAppearsIn e   = let free = freeIdentifiers e in List.all (\ident -> not (Set.member ident free)) vars in
             if List.any someVarAppearsIn (getExps (plucked ++ acc1)) &&
                List.all noVarAppearsIn (getExps (after ++ acc2))
             then (beforeDef :: acc1, acc2)
             else (acc1, beforeDef :: acc2)
           )
           ([],[])
           before
      in
      (beforeInside ++ plucked, beforeOutside, after)
    in
    let listGroup =
      let pluckedBlobs_ =
        List.map ( replacePrecedingWhitespace " " << fromBlobExp)
                 pluckedBlobs
      in
      finalExpOfNewGroup pluckedBlobs_
    in
    let pluckedDefs_ =
      let tab = "  " in
      List.map (\(ws1,p,e,ws2) -> (ws <| ws1.val ++ tab, p,  indent tab e, ws <| ws2.val))
               pluckedDefs
    in
    let newGroupExp =
      applyESubst eSubst <|
        fuseExp (groupDefs ++ pluckedDefs_, OtherExp listGroup)
          -- TODO flag for fuseExp to insert lets instead of defs
    in
    let newDef = (newline2, pVar newGroup, newGroupExp, space0) in
    -- beforeDefs ++ [newDef] ++ afterDefs
    beforeDefs ++ afterDefs ++ [newDef]
  in
  let blobs_ =
    let newBlob = varBlob (withDummyExpInfo (EVar (ws "\n  ") newGroup)) newGroup in
    beforeBlobs ++ [newBlob] ++ afterBlobs
  in
  (defs_, blobs_)

-- TODO maybe stop using this to keep total ordering
pluckFromList pred xs =
  let foo x (plucked, before, after) =
    case (pred x, plucked) of
      (True, _)   -> (plucked ++ [x], before, after)
      (False, []) -> (plucked, before ++ [x], after)
      (False, _)  -> (plucked, before, after ++ [x])
  in
  List.foldl foo ([],[],[]) xs

unsafePluckFromList pred xs =
  let (plucked, before, after) = pluckFromList pred (List.reverse xs) in
  (List.reverse plucked, List.reverse after, List.reverse before)

scaleXY program start end startVal widthOrHeight ws (n,t) eSubst =
  case t of
    TrLoc (locid,_,_) ->
      let pct = (n - Tuple.first startVal) / widthOrHeight in
      let app =
        if pct == 0 then ws ++ start
        else if pct == 1 then ws ++ end
        else
          ws ++ Utils.parens (Utils.spaces ["scaleBetween", start, end, toString pct]) in
      case locIdToEId program locid of
        Just eid -> Dict.insert eid (eRaw__ space0 app) eSubst
        Nothing  -> eSubst
    _ ->
      eSubst

-- TODO for scaleXY and offsetXY, forgo function call when on
-- boundaries to improve readability of generated code
-- (falls back in on little prelude encoding)

offsetXY program base1 base2 baseVal1 baseVal2 ws (n,t) eSubst =
  case t of
    TrLoc (locid,_,_) ->
      let (off1, off2) = (n - Tuple.first baseVal1, n - Tuple.first baseVal2) in
      let (base, off) =
        if off1 <= abs off2 then (base1, off1) else (base2, off2) in
      let app =
        ws ++ Utils.parens (Utils.spaces
                [ "evalOffset"
                , Utils.bracks (Utils.spaces [base, toString off])]) in
      case locIdToEId program locid of
        Just eid -> Dict.insert eid (eRaw__ space0 app) eSubst
        Nothing  -> eSubst
    _ ->
      eSubst


--------------------------------------------------------------------------------
-- Rewrite and Group Blobs with Bounding Box

groupSelectedBlobs model (defs, blobs, f) =
  let selectedNiceBlobs = selectedBlobsToSelectedNiceBlobs model blobs in
  let selectedBlobsAndBounds = computeSelectedBlobsAndBounds model in
  let newGroup = "newGroup" ++ toString model.genSymCount in
  let (groupDefs, eSubst) =
    rewriteBoundingBoxesOfSelectedBlobs model selectedBlobsAndBounds in
  let (defs_, blobs_) =
    groupAndRearrange model newGroup defs blobs selectedNiceBlobs
       groupDefs eSubst
       (\pluckedBlobs_ ->
         withDummyExpInfo <| EList (ws "\n\n  ")
           [ (space0, withDummyExpInfo <| EApp space1
               (eVar0 "group")
               [ eVar "bounds"
               , withDummyExpInfo <| EApp space1
                   (eVar0 "concat")
                   [withDummyExpInfo <| EList space1 (List.map ((,) space0) pluckedBlobs_) space0 Nothing space1]
                   SpaceApp
                   space0
               ]
               SpaceApp
               space0
             )
           ]
           space0 Nothing space1
       )
  in
  let code_ = unparse (fuseExp (defs_, Blobs blobs_ f)) in
  -- upstate Run
    { model | code = code_
            , genSymCount = model.genSymCount + 1
            , selectedBlobs = Dict.empty
            }

computeSelectedBlobsAndBounds : Model -> Dict Int (NumTr, NumTr, NumTr, NumTr)
computeSelectedBlobsAndBounds model =
  let tree = Tuple.second model.slate in
  Dict.map
     (\blobId nodeId ->
       undoGroupPadding <|
       case Dict.get nodeId tree |> Maybe.map .interpreted of

         -- refactor the following cases for readability

         Just (LangSvg.SvgNode "BOX" nodeAttrs _) ->
           let get attr = LangSvg.findNumishAttr attr nodeAttrs in
           (get "LEFT", get "TOP", get "RIGHT", get "BOT")

         Just (LangSvg.SvgNode "OVAL" nodeAttrs _) ->
           let get attr = LangSvg.findNumishAttr attr nodeAttrs in
           (get "LEFT", get "TOP", get "RIGHT", get "BOT")

         Just (LangSvg.SvgNode "g" nodeAttrs _) ->
           case LangSvg.maybeFindBounds nodeAttrs of
             Just bounds -> bounds
             Nothing     -> Debug.crash "computeSelectedBlobsAndBounds"

         Just (LangSvg.SvgNode "line" nodeAttrs _) ->
           let get attr = LangSvg.findNumishAttr attr nodeAttrs in
           let (x1,y1,x2,y2) = (get "x1", get "y1", get "x2", get "y2") in
           (minNumTr x1 x2, minNumTr y1 y2, maxNumTr x1 x2, maxNumTr y1 y2)

         -- "ellipse" and "circle" aren't handled nicely by grouping

         Just (LangSvg.SvgNode "ellipse" nodeAttrs _) ->
           let get attr = LangSvg.findNumishAttr attr nodeAttrs in
           let (cx,cy,rx,ry) = (get "cx", get "cy", get "rx", get "ry") in
           (minusNumTr cx rx, minusNumTr cy ry, plusNumTr cx rx, plusNumTr cy ry)

         Just (LangSvg.SvgNode "circle" nodeAttrs _) ->
           let get attr = LangSvg.findNumishAttr attr nodeAttrs in
           let (cx,cy,r) = (get "cx", get "cy", get "r") in
           (minusNumTr cx r, minusNumTr cy r, plusNumTr cx r, plusNumTr cy r)

         Just (LangSvg.SvgNode "rect" nodeAttrs _) ->
           let get attr = LangSvg.findNumishAttr attr nodeAttrs in
           let (x,y,width,height) = (get "x", get "y", get "width", get "height") in
           (x, y, plusNumTr x width, plusNumTr y height)

         _ -> Debug.crash "computeSelectedBlobsAndBounds"
     )
     model.selectedBlobs

undoGroupPadding (left, top, right, bot) =
  let clean (n,t) =
    case t of
      -- TODO: remove this reliance on the group function in prelude.little.
      -- for example, by drawing zones for 'g' nodes that have 'BOUNDS'
      -- attributes, with extra padding as needed
      TrOp Minus [t1, TrLoc (_,_,"nGroupPad")] -> (n + 20, t1)
      TrOp Plus  [t1, TrLoc (_,_,"nGroupPad")] -> (n - 20, t1)
      TrOp Minus [t1, TrLoc (_,_,"nPolyPathPad")] -> (n + 10, t1)
      TrOp Plus  [t1, TrLoc (_,_,"nPolyPathPad")] -> (n - 10, t1)

      _ -> (n, t)
  in
  (clean left, clean top, clean right, clean bot)

rewriteBoundingBoxesOfSelectedBlobs model selectedBlobsAndBounds =
  let selectedBlobIndices = Dict.keys model.selectedBlobs in
  let (left, top, right, bot) =
    case selectedBlobIndices of
      [] -> Debug.crash "groupAndRearrange: shouldn't get here"
      i::is ->
        let init = Utils.justGet i selectedBlobsAndBounds in
        let foo j (left,top,right,bot) =
          let (a,b,c,d) = Utils.justGet j selectedBlobsAndBounds in
          (minNumTr left a, minNumTr top b, maxNumTr right c, maxNumTr bot d)
        in
        List.foldl foo init is
  in
  let (width, height) = (Tuple.first right - Tuple.first left, Tuple.first bot - Tuple.first top) in
  let scaleX  = scaleXY  model.inputExp "left" "right" left width in
  let scaleY  = scaleXY  model.inputExp "top"  "bot"   top  height in
  let offsetX = offsetXY model.inputExp "left" "right" left right in
  let offsetY = offsetXY model.inputExp "top"  "bot"   top  bot in
  let eSubst =
    -- the spaces inserted by calls to offset*/scale* work best
    -- when the source expressions being rewritten are of the form
    --   (let [a b c d] [na nb nc nd] ...)
    let foo i acc =
      let (a,b,c,d) = Utils.justGet i selectedBlobsAndBounds in
      if model.keysDown == [Keys.keyShift] then
        acc |> offsetX "" a |> offsetY " " b |> offsetX " " c |> offsetY " " d
      else
        -- acc |> scaleX "" a |> scaleY " " b |> scaleX " " c |> scaleY " " d
        acc |> scaleX " " a |> scaleY " " b |> scaleX " " c |> scaleY " " d
    in
    List.foldl foo Dict.empty selectedBlobIndices
  in
  let groupDefs =
    [ ( ws "\n  "
      , pAs "bounds" (pList (listOfPVars ["left", "top", "right", "bot"]))
      , eList (listOfNums [Tuple.first left, Tuple.first top, Tuple.first right, Tuple.first bot]) Nothing
      , space0 )
    ]
  in
  (groupDefs, eSubst)


--------------------------------------------------------------------------------
-- Rewrite and Group Blobs with Anchor

anchorOfSelectedFeatures
    : Set.Set SelectableFeature
   -> Result String (Maybe (NodeId, PointFeature))
anchorOfSelectedFeatures selectedFeatures =
  let err = Err "To group around an anchor, need to select exactly one point." in
  case Set.toList selectedFeatures of
    [selected1, selected2] ->
      case ShapeWidgets.featuresToMaybeSelectablePoint selected1 selected2 of
        Just result -> Ok (Just result)
        Nothing     -> err
    [] -> Ok Nothing
    _  -> err


groupSelectedBlobsAround model (defs, blobs, f) (anchorId, anchorPointFeature) =
  let (anchorKind, anchorAttrs, _) =
    LangSvg.justGetSvgNode "groupSelectedBlobsAround" anchorId model.slate in

  -- TODO
  -- simple approach: anchor must be a primitive point
  case ShapeWidgets.getPointEquations anchorKind anchorAttrs anchorPointFeature of
    (ShapeWidgets.EqnNum (nxBase, txBase), ShapeWidgets.EqnNum (nyBase, tyBase)) ->

      -- simple approach: anchor point must be defined by constant literals
      case (txBase, tyBase) of
        (TrLoc xBaseLoc, TrLoc yBaseLoc) ->

          let selectedNiceBlobs = selectedBlobsToSelectedNiceBlobs model blobs in
          let newGroup = "newGroup" ++ toString model.genSymCount in
          let (groupDefs, eSubst) =
            rewritePrimitivePointsOfSelectedBlobs model
               (nxBase, xBaseLoc) (nyBase, yBaseLoc)
          in
          let (defs_, blobs_) =
            groupAndRearrange model newGroup defs blobs selectedNiceBlobs
               groupDefs eSubst
               (\pluckedBlobs_ ->
                 withDummyExpInfo <| EList (ws "\n\n  ")
                   [ (space0, withDummyExpInfo <| EApp space1
                       (eVar0 "anchoredGroup")
                       [ withDummyExpInfo <| EApp space1
                           (eVar0 "concat")
                           [withDummyExpInfo <| EList space1 (List.map ((,) space0) pluckedBlobs_) space0 Nothing space1]
                           SpaceApp
                           space0
                       ]
                       SpaceApp
                       space0
                     )
                   ]
                   space0 Nothing space1
               )
          in
          let code_ = unparse (fuseExp (defs_, Blobs blobs_ f)) in
          { model | code = code_
                  , genSymCount = model.genSymCount + 1
                  , selectedBlobs = Dict.empty
                  , selectedFeatures = Set.empty
                  }

        _ ->
          let _ = Debug.log "WARN: anchor must be defined by constants" () in
          model

    _ ->
      let _ = Debug.log "WARN: for now, anchor must be a primitive point" () in
      model


rewritePrimitivePointsOfSelectedBlobs model (nxBase, xBaseLoc)
                                            (nyBase, yBaseLoc) =
  let (xId, _, xName) = xBaseLoc in
  let (yId, _, yName) = yBaseLoc in
  let (xAnchor, yAnchor) = ("xAnchor", "yAnchor") in
  let pointsOfSelectedBlobs =
     Dict.foldl
       (\_ nodeId acc -> acc ++ ShapeWidgets.getPrimitivePointEquations model.slate nodeId)
       [] model.selectedBlobs
  in
  let anchorDef =
      ( ws "\n  "
      , pAs "anchor" (pList (listOfPVars [xAnchor, yAnchor]))
      , eAsPoint (eList (listOfNums [nxBase, nyBase]) Nothing)
      , space0
      )
  in
  let eSubst =
    let locIdToEId =
      Lang.locIdToEId model.inputExp
        >> Utils.fromJust_ "rewritePrimitivePointsOfSelectedBlobs"
    in
    pointsOfSelectedBlobs |> List.foldl (\(xOther, yOther) acc ->
      -- TODO when anchor is derived point, rewrite anchor shape appropriately
      if (nxBase, nyBase) == (Tuple.first xOther, Tuple.first yOther) then
        acc |> Dict.insert (locIdToEId xId) (EVar space1 xAnchor)
            |> Dict.insert (locIdToEId yId) (EVar space1 yAnchor)
      else
        case (xOther, yOther) of
          ( (nxOther, TrLoc (xOtherId,_,_))
          , (nyOther, TrLoc (yOtherId,_,_))
          ) ->
            acc |> Dict.insert (locIdToEId xOtherId) (eBaseOffset xAnchor (nxOther - nxBase))
                |> Dict.insert (locIdToEId yOtherId) (eBaseOffset yAnchor (nyOther - nyBase))
          _ ->
            acc
      ) Dict.empty
  in
  ([anchorDef], eSubst)


eBaseOffset baseVar offsetNum =
  let allowZeroOffsets = True in -- flag for: anchor or (+ anchor 0)
  if offsetNum == 0 && not allowZeroOffsets then
    EVar space1 baseVar
  else
    ePlus (eVar baseVar) (eConst offsetNum (dummyLoc_ unann))
      |>  replacePrecedingWhitespace " "
      |> .val |> .e__


eAsPoint e =
  let insertPointAnnotations = False in -- Config param
  if not insertPointAnnotations then e
  else

  let e_ =  replacePrecedingWhitespace "" e in
  withDummyExpInfo <|
    EColonType space1 e_ space1 (withDummyRange <| TNamed space1 "Point") space0


pAsTight x p =
  let p_ =  replacePrecedingWhitespacePat "" p in
  withDummyPatInfo <| PAs space1 x space0 p_


--------------------------------------------------------------------------------
-- Abstract Blob

selectedBlobsToSelectedVarBlobs : Model -> List BlobExp -> List (Int, Exp, Ident)
selectedBlobsToSelectedVarBlobs model blobs =
  List.concatMap
     (\(i,e,niceBlob) ->
       case niceBlob of
         VarBlob x        -> [(i, e, x)]
         WithBoundsBlob _ -> []
         WithAnchorBlob _ -> []
         CallBlob _       -> []
     )
     (selectedBlobsToSelectedNiceBlobs model blobs)

abstractSelectedBlobs model =
  let (defs,mainExp) = splitExp model.inputExp in
  case mainExp of
    Blobs blobs f ->
      -- silently ignoring WithBoundsBlobs
      let selectedVars = selectedBlobsToSelectedVarBlobs model blobs in
      let (defs_,blobs_) = List.foldl abstractOne (defs, blobs) selectedVars in
      let code_ = unparse (fuseExp (defs_, Blobs blobs_ f)) in
      -- upstate Run
        { model | code = code_
                , selectedBlobs = Dict.empty
                }
    _ ->
      model

abstractOne (i, eBlob, x) (defs, blobs) =

  let (pluckedDefs, beforeDefs, afterDefs) =
    pluckFromList (matchesAnySelectedVarBlob [(i, eBlob, VarBlob x)]) defs in

  let (pluckedBlobs, beforeBlobs, afterBlobs) =
    let matches (j,_) = i == j in
    let (plucked, before, after) =
      pluckFromList matches (Utils.zip (List.range 1 (List.length blobs)) blobs) in
    (List.map Tuple.second plucked, List.map Tuple.second before, List.map Tuple.second after) in

  case (pluckedDefs, pluckedBlobs) of

    ([(ws1,p,e,ws2)], [NiceBlob _ (VarBlob x)]) ->

      let (e_, mapping) = collectUnfrozenConstants e in
      let (newDef, newBlob) =
        case findSpecialBindingsInMapping mapping of

          Just (restOfMapping, BoundsBindings left top right bot) ->
            let newFunc =
              let pBounds =
                let pVars = listOfPVars ["left", "top", "right", "bot"] in
                case restOfMapping of
                  [] -> pList0 pVars
                  _  -> pList  pVars
              in
              let params = listOfPVars (List.map Tuple.first restOfMapping) in
              withDummyExpInfo (EFun space1 (params ++ [pBounds]) e_ space0)
            in
            let eBounds = eList (listOfAnnotatedNums [left, top, right, bot]) Nothing in
            let newCall =
              let eBlah =
                case listOfAnnotatedNums1 (List.map Tuple.second restOfMapping) of
                  []   -> eVar x
                  args -> withDummyExpInfo (EApp (ws "\n    ") (eVar0 x) args SpaceApp space0)
              in
              withDummyExpInfo (EApp (ws "\n  ") (eVar0 "withBounds") [eBounds, eBlah] SpaceApp space0)
            in
            let newBlob = NiceBlob newCall (WithBoundsBlob (eBounds, x, [])) in
            ((ws1, p, newFunc, ws2), newBlob)

          -- mostly copying previous case...
          Just (restOfMapping, AnchorBindings xAnchor yAnchor) ->
            let newFunc =
              let pBounds =
                let pVars = listOfPVars ["xAnchor", "yAnchor"] in
                case restOfMapping of
                  [] -> pList0 pVars
                  _  -> pList  pVars
              in
              let params = listOfPVars (List.map Tuple.first restOfMapping) in
              withDummyExpInfo (EFun space1 (params ++ [pAsTight "anchor" pBounds]) e_ space0)
            in
            let eAnchor =
              eAsPoint (eList (listOfAnnotatedNums [xAnchor, yAnchor]) Nothing)
            in
            let newCall =
              let eBlah =
                case listOfAnnotatedNums1 (List.map Tuple.second restOfMapping) of
                  []   -> eVar x
                  args -> withDummyExpInfo (EApp space1 (eVar0 x) args SpaceApp space0)
              in
              withDummyExpInfo (EApp (ws "\n  ") (eVar0 "withAnchor") [eAnchor, eBlah] SpaceApp space0)
            in
            let newBlob = NiceBlob newCall (WithAnchorBlob (eAnchor, x, [])) in
            ((ws1, p, newFunc, ws2), newBlob)

          Nothing ->
            let newFunc =
              let params = listOfPVars (List.map Tuple.first mapping) in
              withDummyExpInfo (EFun space1 params e_ space0)
            in
            let newBlob =
              case listOfAnnotatedNums1 (List.map Tuple.second mapping) of
                []   -> varBlob (eVar x) x
                args ->
                  let newCall = withDummyExpInfo (EApp (ws "\n  ") (eVar0 x) args SpaceApp space0) in
                  callBlob newCall (x, args)
            in
            ((ws1, p, newFunc, ws2), newBlob)
      in
      let defs_ = beforeDefs ++ [newDef] ++ afterDefs in
      let blobs_ = beforeBlobs ++ [newBlob] ++ afterBlobs in
      (defs_, blobs_)

    _ ->
      let _ = Debug.log "abstractOne: multiple defs..." in
      (defs, blobs)

-- TODO handle as-patterns in a general way
--
collectUnfrozenConstants : Exp -> (Exp, List (Ident, AnnotatedNum))
collectUnfrozenConstants e =
  -- extra first pass, as a quick and simple way to approximate name clashes
  let (_, list0) = collectUnfrozenConstants_ Nothing e in
  let varCounts =
    List.foldl (\var acc ->
      case Dict.get var acc of
        Nothing    -> Dict.insert var 1 acc
        Just count -> Dict.insert var (1 + count) acc
      ) Dict.empty (List.map Tuple.first list0)
  in
  let (e_, list) = collectUnfrozenConstants_ (Just varCounts) e in
  (clean e_, List.reverse list)

collectUnfrozenConstants_
     : Maybe (Dict Ident Int) -> Exp -> (Exp, List (Ident, AnnotatedNum))
collectUnfrozenConstants_ maybeVarCounts e =
  let foo e__ =
    let default = (e__, []) in
    case e__ of
      EConst ws n (locid, ann, x) wd ->
        if ann == unann || ann == thawed then
          if x == ""
          then default -- (EVar ws x, [("k" ++ toString locid, n)])
          else
            let addVar y = (EVar ws y, [(y, (n,ann,wd))]) in
            case maybeVarCounts of
              Nothing -> addVar x
              Just varCounts ->
                if Utils.justGet x varCounts == 1
                then addVar x
                else addVar (x ++ toString locid)
        else
          default
      _ ->
       default
  in
  -- two passes for ease of implementation
  let e_ = mapExpViaExp__ (Tuple.first << foo) e in
  let mapping = foldExpViaE__ ((++) << Tuple.second << foo) [] e in
  (e_, mapping)

type SpecialBindings
  = BoundsBindings AnnotatedNum AnnotatedNum AnnotatedNum AnnotatedNum
  | AnchorBindings AnnotatedNum AnnotatedNum

findSpecialBindingsInMapping
    : List (Ident, AnnotatedNum)
   -> Maybe (List (Ident, AnnotatedNum), SpecialBindings)
findSpecialBindingsInMapping mapping =
  findBoundsInMapping mapping |> Utils.plusMaybe (findAnchorInMapping mapping)

findBoundsInMapping mapping =
  case mapping of
    ("left", left) :: ("top", top) :: ("right", right) :: ("bot", bot) :: rest ->
      Just (rest, BoundsBindings left top right bot)
    _ ->
      Nothing

findAnchorInMapping mapping =
  case mapping of
    ("xAnchor", xAnchor) :: ("yAnchor", yAnchor) :: rest ->
      Just (rest, AnchorBindings xAnchor yAnchor)
    _ ->
      Nothing

removeRedundantBindings =
  mapExp <| \e ->
    case e.val.e__ of
      ELet _ _ _ p _ e1 _ e2 _ -> if redundantBinding (p, e1) then e2 else e
      _                    -> e

redundantBinding (p, e) =
  case (p.val.p__, e.val.e__) of
    (PConst _ n, EConst _ n_ _ _) -> n == n_
    (PBase _ bv, EBase _ bv_)     -> bv == bv_
    (PVar _ x _, EVar _ x_)       -> x == x_

    (PList _ ps _ Nothing _, EList _ es _ Nothing _) ->
      List.all redundantBinding (Utils.zip ps (Utils.listValues es))
    (PList _ ps _ (Just p) _, EList _ es _ (Just e) _) ->
      List.all redundantBinding (Utils.zip (p::ps) (e :: Utils.listValues es))

    (_, EColonType _ e1 _ _ _) -> redundantBinding (p, e1)

    _ -> False

clean =
  removeRedundantBindings << LangSimplify.simplify


--------------------------------------------------------------------------------
-- Replicate Blob

replicateSelectedBlob replicateKind model (defs, blobs, f) =
  case selectedBlobsToSelectedNiceBlobs model blobs of

    [(i, _, WithAnchorBlob (anchor, g, args))] ->

      let eGroupFunc = withDummyExpInfo <| EApp (ws "\n    ") (eVar0 g) args SpaceApp space0 in
      let eAnchor =  replacePrecedingWhitespace "\n    " anchor in
      let (arrayFunction, arrayArgs) =
        case replicateKind of

          HorizontalRepeat ->
            let eNum = withDummyExpInfo <| EConst space1 3 (dummyLoc_ frozen) (intSlider 1 20) in
            let eSep = withDummyExpInfo <| EConst space1 20 dummyLoc noWidgetDecl in
            ("horizontalArray", [eNum, eSep, eGroupFunc, eAnchor])

          LinearRepeat ->
            let eNum   = withDummyExpInfo <| EConst space1 3 (dummyLoc_ frozen) (intSlider 1 20) in
            let eStart =  replacePrecedingWhitespace "\n    " anchor in
            let eEnd =
              case stripPointExp anchor of
                Nothing -> eAnchor
                Just (nx,ny) ->
                  let ex_ = eConst0 (nx + 100) dummyLoc in
                  let ey_ = eConst (ny + 50) dummyLoc in
                   replacePrecedingWhitespace "\n    " <|
                    eAsPoint (eList [ex_, ey_] Nothing)
            in
            ("linearArrayFromTo", [eNum, eGroupFunc, eStart, eEnd])

          RadialRepeat ->
            let nRadius = 100 in
            let eNum    = withDummyExpInfo <| EConst space1 3 (dummyLoc_ frozen) (intSlider 1 20) in
            let eRadius = withDummyExpInfo <| EConst space1 nRadius (dummyLoc_ unann) noWidgetDecl in
            let eRot    = withDummyExpInfo <| EConst space1 0 (dummyLoc_ frozen) (numSlider 0 6.28) in
            let eCenter =
              case stripPointExp anchor of
                Nothing -> eAnchor
                Just (nx,ny) ->
                  let ex_ = eConst0 nx dummyLoc in
                  let ey_ = eConst (ny + nRadius) dummyLoc in
                   replacePrecedingWhitespace "\n    " <|
                    eAsPoint (eList [ex_, ey_] Nothing)
            in
            ("radialArray", [ eNum, eRadius, eRot, eGroupFunc, eCenter ])
      in
      let newBlob =
        NiceBlob
           (withDummyExpInfo <| EApp (ws "\n  ") (eVar0 arrayFunction) arrayArgs SpaceApp space0)
           (CallBlob (arrayFunction, arrayArgs))
      in
      let blobs_ = Utils.replacei i newBlob blobs in
      let code_ = unparse (fuseExp (defs, Blobs blobs_ f)) in
      { model | code = code_ , selectedBlobs = Dict.empty }

    [(i, _, WithBoundsBlob (bounds, g, args))] ->

      let eGroupFunc = withDummyExpInfo <| EApp (ws "\n    ") (eVar0 g) args SpaceApp space0 in
      let eBounds =  replacePrecedingWhitespace "\n    " bounds in
      let (arrayFunction, arrayArgs) =
        case replicateKind of

          HorizontalRepeat ->
            let eNum = withDummyExpInfo <| EConst space1 3 (dummyLoc_ frozen) (intSlider 1 10) in
            let eSep = withDummyExpInfo <| EConst space1 20 dummyLoc noWidgetDecl in
            ("horizontalArrayByBounds", [eNum, eSep, eGroupFunc, eBounds])

          LinearRepeat ->
            let (nNum, nSep) = (3, 20) in
            let eNum = withDummyExpInfo <| EConst space1 nNum (dummyLoc_ frozen) (intSlider 1 20) in
            let eSep = withDummyExpInfo <| EConst space1 nSep dummyLoc noWidgetDecl in
            let eGroupBounds =
              case stripBoundsExp bounds of
                Nothing -> eBounds
                Just (nLeft, nTop, nRight, nBot) ->
                  let eLeft = eConst0 nLeft dummyLoc in
                  let eTop = eConst nTop dummyLoc in
                  let eRight = eConst (nLeft + nNum*(nRight-nLeft) + (nNum-1)*nSep) dummyLoc in
                  let eBot = eConst nBot dummyLoc in
                   replacePrecedingWhitespace "\n    " <|
                    eList [eLeft, eTop, eRight, eBot] Nothing
            in
            ("repeatInsideBounds", [eNum, eSep, eGroupFunc, eGroupBounds])

          RadialRepeat ->
            Debug.crash "replicateSelectedBlob: TODO"
      in
      let newBlob =
        NiceBlob
           (withDummyExpInfo <| EApp (ws "\n  ") (eVar0 arrayFunction) arrayArgs SpaceApp space0)
           (CallBlob (arrayFunction, arrayArgs))
      in
      let blobs_ = Utils.replacei i newBlob blobs in
      let code_ = unparse (fuseExp (defs, Blobs blobs_ f)) in
      { model | code = code_ , selectedBlobs = Dict.empty }

    _ -> model


stripPointExp e =
  case e.val.e__ of
    EList _ [(_,ex),(_,ey)] _ Nothing _ ->
      case (ex.val.e__, ey.val.e__) of
        (EConst _ nx _ _, EConst _ ny _ _) -> Just (nx, ny)
        _                                  -> Nothing
    EColonType _ e_ _ _ _ -> stripPointExp e_
    _                     -> Nothing


stripBoundsExp e =
  case e.val.e__ of
    EList _ es _ Nothing _ ->
      case List.map (.val >> .e__) (Utils.listValues es) of
        [ EConst _ nLeft _ _
        , EConst _ nTop _ _
        , EConst _ nRight _ _
        , EConst _ nBot _ _ ] -> Just (nLeft, nTop, nRight, nBot)
        _                     -> Nothing
    EColonType _ e_ _ _ _ -> stripBoundsExp e_
    _                     -> Nothing


--------------------------------------------------------------------------------
-- Delete Blobs

deleteSelectedBlobs model =
  let (defs,mainExp) = splitExp model.inputExp in
  case mainExp of
    Blobs blobs f ->
      let blobs_ =
        Utils.filteri1
           (\(i,_) -> not (Dict.member i model.selectedBlobs))
           blobs
      in
      let code_ = unparse (fuseExp (defs, Blobs blobs_ f)) in
      -- upstate Run
        { model | code = code_
              , selectedBlobs = Dict.empty
              }
    _ ->
      model


--------------------------------------------------------------------------------
-- Duplicate Blobs

-- duplicateSelectedBlobs model =
--   let (defs,mainExp) = splitExp model.inputExp in
--   case mainExp of
--     Blobs blobs f ->
--       let (nextGenSym, newDefs, newBlobs) =
--         let selectedNiceBlobs = selectedBlobsToSelectedNiceBlobs model blobs in
--         let (nextGenSym_, newDefs_, newVarBlobs_) =
--           List.foldl
--              (\def (k,acc1,acc2) ->
--                if not (matchesAnySelectedVarBlob selectedNiceBlobs def)
--                then (k, acc1, acc2)
--                else
--                  let (ws1,p,e,ws2) = def in
--                  case p.val.p__ of
--                    PVar pws x wd ->
--                      let x_ = x ++ "_copy" ++ toString k in
--                      let acc1_ = (ws1, withDummyPatInfo (PVar pws x_ wd), e, ws2) :: acc1 in
--                      let acc2_ = varBlob (withDummyExpInfo (EVar (ws "\n  ") x_)) x_ :: acc2 in
--                      (1 + k, acc1_, acc2_)
--                    _ ->
--                      let _ = Debug.log "duplicateSelectedBlobs: weird..." () in
--                      (k, acc1, acc2)
--              )
--              (model.genSymCount, [], [])
--              defs
--         in
--         let newWithAndCallBlobs =
--           List.concatMap
--              (\(_,e,niceBlob) ->
--                case niceBlob of
--                  WithBoundsBlob _ -> [NiceBlob e niceBlob]
--                  WithAnchorBlob _ -> [NiceBlob e niceBlob]
--                  CallBlob _       -> [NiceBlob e niceBlob]
--                  VarBlob _        -> []
--              )
--              selectedNiceBlobs
--         in
--         let newDefs = List.reverse newDefs_ in
--         let newBlobs = List.reverse newVarBlobs_ ++ newWithAndCallBlobs in
--         (nextGenSym_, newDefs, newBlobs)
--       in
--       let code_ =
--         let blobs_ = blobs ++ newBlobs in
--         let defs_ = defs ++ newDefs in
--         unparse (fuseExp (defs_, Blobs blobs_ f))
--       in
--       -- upstate Run
--         { model | code = code_
--                 , genSymCount = List.length newBlobs + model.genSymCount
--                 }
--     _ ->
--       model

{-
shiftNum (n, t) = (30 + n, t)

shiftDownAndRight (left, top, right, bot) =
  (shiftNum left, shiftNum top, right, bot)
-}


--------------------------------------------------------------------------------
-- Merge Blobs

mergeSelectedBlobs model =
  let (defs,mainExp) = splitExp model.inputExp in
  case mainExp of
    Blobs blobs f ->
      let selectedVarBlobs = selectedBlobsToSelectedVarBlobs model blobs in
      if List.length selectedVarBlobs /= Dict.size model.selectedBlobs then
        model -- should display error caption for remaining selected blobs...
      else
        let (defs_, blobs_) = mergeSelectedVarBlobs model defs blobs selectedVarBlobs in
        let code_ = unparse (fuseExp (defs_, Blobs blobs_ f)) in
        -- upstate Run
          { model | code = code_
                  , selectedBlobs = Dict.empty
                  }
    _ ->
      model

mergeSelectedVarBlobs model defs blobs selectedVarBlobs =

  let (pluckedDefs, beforeDefs, afterDefs) =
    let selectedNiceBlobs = List.map (\(i,e,x) -> (i, e, VarBlob x)) selectedVarBlobs in
    pluckFromList (matchesAnySelectedVarBlob selectedNiceBlobs) defs in

  let (pluckedBlobs, beforeBlobs, afterBlobs) =
    let matches (j,_) = Dict.member j model.selectedBlobs in
    let (plucked, before, after) =
      pluckFromList matches (Utils.zip (List.range 1 (List.length blobs)) blobs) in
    (List.map Tuple.second plucked, List.map Tuple.second before, List.map Tuple.second after) in

  let ((ws1,p,e,ws2),es) =
    case pluckedDefs of
      def::defs_ -> (def, List.map (\(_,_,e,_) -> e) defs_)
      []         -> Debug.crash "mergeSelectedVarBlobs: shouldn't get here" in

  case mergeExpressions e es of
    Nothing ->
      -- let _ = Debug.log "mergeExpressions Nothing" () in
      (defs, blobs)

    Just (_, []) ->
      let defs_ = beforeDefs ++ [(ws1,p,e,ws2)] ++ afterDefs in
      let blobs_ = beforeBlobs ++ [Utils.head_ pluckedBlobs] ++ afterBlobs in
      (defs_, blobs_)

    Just (eMerged, multiMapping) ->

      -- TODO treat bounds variables specially, as in abstract

      let newDef =
        let newFunc =
          let params = listOfPVars (List.map Tuple.first multiMapping) in
          withDummyExpInfo (EFun space1 params (clean eMerged) space0) in
        (ws1, p, newFunc, ws2) in

      let f =
        case p.val.p__ of
          PVar _ x _ -> x
          _          -> Debug.crash "mergeSelected: not var" in

      let newBlobs =
        case Utils.maybeZipN (List.map Tuple.second multiMapping) of
          Nothing -> Debug.crash "mergeSelected: no arg lists?"
          Just numLists ->
            -- let _ = Debug.log "numLists:" numLists in
            List.map
               (\nums ->
                  let args = listOfAnnotatedNums1 nums in
                  let e = withDummyExpInfo <| EApp (ws "\n  ") (eVar0 f) args SpaceApp space0 in
                  callBlob e (f, args)
               ) numLists in

      let defs_ = beforeDefs ++ [newDef] ++ afterDefs in
      let blobs_ = beforeBlobs ++ newBlobs ++ afterBlobs in
      (defs_, blobs_)

-- Merge 2+ expressions
mergeExpressions
    : Exp -> List Exp
   -> Maybe (Exp, List (Ident, List AnnotatedNum))
mergeExpressions eFirst eRest =
  let return e__ list =
    Just (replaceE__ eFirst e__, list) in

  case eFirst.val.e__ of

    EConst ws1 n loc wd ->
      let match eNext = case eNext.val.e__ of
        EConst _ nNext (_,annNext,_) wdNext -> Just (nNext, annNext, wdNext)
        _                                   -> Nothing
      in
      matchAllAndBind match eRest <| \restAnnotatedNums ->
        let (locid,ann,x) = loc in
        let allAnnotatedNums = (n,ann,wd) :: restAnnotatedNums in
        case Utils.dedupBy annotatedNumToComparable allAnnotatedNums of
          [_] -> return eFirst.val.e__ []
          _   ->
            let var = if x == "" then "k" ++ toString locid else x in
            -- let _ = Debug.log "var for merge: " (var, n::nums) in
            return (EVar ws1 var) [(var, allAnnotatedNums)]

    EBase _ bv ->
      let match eNext = case eNext.val.e__ of
        EBase _ bv_ -> Just bv_
        _           -> Nothing
      in
      matchAllAndBind match eRest <| \bvs ->
        if List.all ((==) bv) bvs then return eFirst.val.e__ [] else Nothing

    EVar _ x ->
      let match eNext = case eNext.val.e__ of
        EVar _ x_ -> Just x_
        _         -> Nothing
      in
      matchAllAndBind match eRest <| \xs ->
        if List.all ((==) x) xs then return eFirst.val.e__ [] else Nothing

    EFun ws1 ps eBody ws2 ->
      let match eNext = case eNext.val.e__ of
        EFun _ ps_ eBody_ _ -> Just (ps_, eBody_)
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (psList, eBodyList) = List.unzip stuff in
        Utils.bindMaybe2
          (\() (eBody_,list) -> return (EFun ws1 ps eBody_ ws2) list)
          (mergePatternLists (ps::psList))
          (mergeExpressions eBody eBodyList)

    EApp ws1 eFunc eArgs appType ws2 ->
      let match eNext = case eNext.val.e__ of
        EApp _ eFunc_ eArgs_ appType_ _ -> Just ((eFunc_, eArgs_), appType_)
        _                      -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (eFuncArgList, eAppTypeList) = List.unzip stuff in
        let (eFuncList, eArgsList) = List.unzip eFuncArgList in
        Utils.bindMaybe3
          (\(eFunc_,l1) (eArgs_,l2) appType_ ->
            return (EApp ws1 eFunc_ eArgs_ appType_ ws2) (l1 ++ l2))
          (mergeExpressions eFunc eFuncList)
          (mergeExpressionLists (eArgs::eArgsList))
          (Just appType)

    ELet ws1 letKind rec p1 ws2 e1 ws3 e2 ws4 ->
      let match eNext = case eNext.val.e__ of
        ELet _ _ _ p1_ _ e1_ _ e2_ _ -> Just ((p1_, e1_), e2_)
        _                            -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((p1List, e1List), e2List) =
          Tuple.mapFirst List.unzip (List.unzip stuff)
        in
        Utils.bindMaybe3
          (\_ (e1_,l1) (e2_,l2) ->
            return (ELet ws1 letKind rec p1 ws2 e1_ ws3 e2_ ws4) (l1 ++ l2))
          (mergePatterns p1 p1List)
          (mergeExpressions e1 e1List)
          (mergeExpressions e2 e2List)

    EList ws1 es ws2 me ws3 ->
      let match eNext = case eNext.val.e__ of
        EList _ es_ _ me_ _ -> Just (es_, me_)
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (esList, meList) = List.unzip stuff in
        Utils.bindMaybe2
          (\(es_,l1) (me_,l2) -> return (EList ws1 (Utils.listValuesMake es es_) ws2 me_ ws3) (l1 ++ l2))
          (mergeExpressionLists (Utils.listValues es :: List.map Utils.listValues esList))
          (mergeMaybeExpressions me meList)

    ERecord ws1 mi es ws2 ->
      let match eNext = case eNext.val.e__ of
        ERecord _ mi_ es_ _ -> Just (Utils.recordInitValue mi_, Utils.recordValues es_)
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (miList, esList) = List.unzip stuff in
        Utils.bindMaybe2
          (\(mi_,l1) (es_,l2) -> return (ERecord ws1 (Utils.recordInitMake mi mi_) (Utils.recordValuesMake es es_) ws2) (l1 ++ l2))
          (mergeMaybeExpressions (Utils.recordInitValue mi) miList)
          (mergeExpressionLists  (Utils.recordValues es :: esList))

    ESelect ws0 eRec ws1 ws2 m ->
      let match eNext = case eNext.val.e__ of
        ESelect _ eRec_ _ _ m2 -> if m == m2 then Just eRec_ else Nothing
        _                      -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let eRecList = stuff in
        Utils.bindMaybe
          (\(eRec_,l1) ->
            return (ESelect ws0 eRec_ ws1 ws2 m) l1)
          (mergeExpressions eRec eRecList)

    EOp ws1 op es ws2 ->
      let match eNext = case eNext.val.e__ of
        EOp _ op_ es_ _ -> Just (op_, es_)
        _               -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (opList, esList) = List.unzip stuff in
        if List.all ((==) op.val) (List.map .val opList) then
          Utils.bindMaybe
            (\(es_,l) -> return (EOp ws1 op es_ ws2) l)
            (mergeExpressionLists (es::esList))
        else
          Nothing

    EIf ws1 e1 ws2 e2 ws3 e3 ws4 ->
      let match eNext = case eNext.val.e__ of
        EIf _ e1_ _ e2_ _ e3_ _ -> Just ((e1_, e2_), e3_)
        _                       -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((e1List, e2List), e3List) = Tuple.mapFirst List.unzip (List.unzip stuff) in
        Utils.bindMaybe3
          (\(e1_,l1) (e2_,l2) (e3_,l3) ->
            return (EIf ws1 e1_ ws2 e2_ ws3 e3_ ws4) (l1 ++ l2 ++ l3))
          (mergeExpressions e1 e1List)
          (mergeExpressions e2 e2List)
          (mergeExpressions e3 e3List)

    EComment ws s e ->
      let match eNext = case eNext.val.e__ of
        EComment _ _ e_ -> Just e_
        _               -> Nothing
      in
      matchAllAndBind match eRest <| \es ->
        Utils.bindMaybe
          (\(e_,l) -> return (EComment ws s e_) l)
          (mergeExpressions e es)

    ETyp ws1 pat tipe e ws2 ->
      let match eNext = case eNext.val.e__ of
        ETyp _ pat tipe e _ -> Just ((pat, tipe), e)
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((patList, typeList), eList) =
          Tuple.mapFirst List.unzip (List.unzip stuff)
        in
        Utils.bindMaybe3
          (\_ _ (e_,l) ->
            return (ETyp ws1 pat tipe e_ ws2) l)
          (mergePatterns pat patList)
          (mergeTypes tipe typeList)
          (mergeExpressions e eList)

    EColonType ws1 e ws2 tipe ws3 ->
      let match eNext = case eNext.val.e__ of
        EColonType _ e _ tipe _ -> Just (e,tipe)
        _                       -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (eList, typeList) = List.unzip stuff in
        Utils.bindMaybe2
          (\(e_,l) _ ->
            return (EColonType ws1 e_ ws2 tipe ws3) l)
          (mergeExpressions e eList)
          (mergeTypes tipe typeList)

    ETypeAlias ws1 pat tipe e ws2 ->
      let match eNext = case eNext.val.e__ of
        ETypeAlias _ pat tipe e _ -> Just ((pat, tipe), e)
        _                         -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((patList, typeList), eList) =
          Tuple.mapFirst List.unzip (List.unzip stuff)
        in
        Utils.bindMaybe3
          (\_ _ (e_,l) ->
            return (ETypeAlias ws1 pat tipe e_ ws2) l)
          (mergePatterns pat patList)
          (mergeTypes tipe typeList)
          (mergeExpressions e eList)

    ECase _ _ _ _ ->
      let _ = Debug.log "mergeExpressions: TODO handle: " eFirst in
      Nothing

    ETypeCase _ _ _ _ ->
      let _ = Debug.log "mergeExpressions: TODO handle: " eFirst in
      Nothing

    EOption _ _ _ _ _ ->
      let _ = Debug.log "mergeExpressions: options shouldn't appear nested: " () in
      Nothing

    EParens ws1 e pStyle ws2 ->
      let match eNext = case eNext.val.e__ of
        EParens _ e_ _ _  -> Just e_
        _               -> Nothing
      in
      matchAllAndBind match eRest <| \es ->
        Utils.bindMaybe
          (\(e_,l) -> return (EParens ws1 e_ pStyle ws2) l)
          (mergeExpressions e es)

    EHole ws mv ->
      let match eNext = case eNext.val.e__ of
        EHole _ mvNext -> Just mvNext
        _              -> Nothing
      in
      matchAllAndBind match eRest <| \maybeVals ->
        if List.all ((==) mv) maybeVals then return eFirst.val.e__ [] else Nothing

matchAllAndBind : (a -> Maybe b) -> List a -> (List b -> Maybe c) -> Maybe c
matchAllAndBind f xs g = Utils.bindMaybe g (Utils.projJusts (List.map f xs))

mergeExpressionLists
    : List (List Exp)
   -> Maybe (List Exp, List (Ident, List AnnotatedNum))
mergeExpressionLists lists =
  case Utils.maybeZipN lists of
    Nothing -> Nothing
    Just listListExp ->
      let foo listExp maybeAcc =
        case (listExp, maybeAcc) of
          (e::es, Just (acc1,acc2)) ->
            case mergeExpressions e es of
              Nothing     -> Nothing
              Just (e_,l) -> Just (acc1 ++ [e_], acc2 ++ l)
          _ ->
            Nothing
      in
      List.foldl foo (Just ([],[])) listListExp

mergeMaybeExpressions
    : Maybe Exp -> List (Maybe Exp)
   -> Maybe (Maybe Exp, List (Ident, List AnnotatedNum))
mergeMaybeExpressions me mes =
  case me of
    Nothing ->
      if List.all ((==) Nothing) mes
        then Just (Nothing, [])
        else Nothing
    Just e  ->
      Utils.bindMaybe
        (Utils.mapMaybe (Tuple.mapFirst Just) << mergeExpressions e)
        (Utils.projJusts mes)


mergePatterns : Pat -> List Pat -> Maybe ()
mergePatterns pFirst pRest =
  case pFirst.val.p__ of
    PWildcard _ ->
      let match pNext = case pNext.val.p__ of
        PWildcard _ -> Just ()
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest ()
    PVar _ x _ ->
      let match pNext = case pNext.val.p__ of
        PVar _ x_ _ -> Just x_
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest x
    PConst _ n ->
      let match pNext = case pNext.val.p__ of
        PConst _ n_ -> Just n_
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest n
    PBase _ bv ->
      let match pNext = case pNext.val.p__ of
        PBase _ bv_ -> Just bv_
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest bv
    PList _ ps _ mp _ ->
      let match pNext = case pNext.val.p__ of
        PList _ ps_ _ mp_ _ -> Just (ps_, mp_)
        _                   -> Nothing
      in
      matchAllAndBind match pRest <| \stuff ->
        let (psList, mpList) = List.unzip stuff in
        Utils.bindMaybe2
          (\_ () -> Just ())
          (mergePatternLists (ps::psList))
          (mergeMaybePatterns mp mpList)
    PRecord _ ps _ ->
       let match pNext = case pNext.val.p__ of
         PRecord _ ps2 _ -> Just (Utils.recordValues ps2)
         _               -> Nothing
       in
       matchAllAndBind match pRest <|  \stuff ->
         let psList = stuff in
         mergePatternLists (Utils.recordValues ps::psList)

    PAs _ x _ p ->
      let match pNext = case pNext.val.p__ of
        PAs _ x_ _ p_ -> Just (x_, p_)
        _             -> Nothing
      in
      matchAllAndBind match pRest <| \stuff ->
        let (indentList, pList) = List.unzip stuff in
        Utils.bindMaybe
          (\() -> if List.all ((==) x) indentList then Just () else Nothing)
          (mergePatterns p pList)
    PParens _ p _ ->
      let match pNext = case pNext.val.p__ of
        PParens _ p_ _ -> Just p_
        _             -> Nothing
      in
      matchAllAndBind match pRest <| \pList ->
        Utils.bindMaybe
          (\() -> Just ())
          (mergePatterns p pList)

mergeTypes : Type -> List Type -> Maybe ()
mergeTypes tFirst tRest =
  if List.all (Types.equal tFirst) tRest
  then Just ()
  else Nothing

matchAllAndCheckEqual f xs x =
  let g ys = if List.all ((==) x) ys then Just () else Nothing in
  matchAllAndBind f xs g

mergePatternLists : List (List Pat) -> Maybe ()
mergePatternLists lists =
  case Utils.maybeZipN lists of
    Nothing -> Nothing
    Just listListPat ->
      let foo listPat maybeAcc =
        case (listPat, maybeAcc) of
          (p::ps, Just ()) -> mergePatterns p ps
          _                -> Nothing
      in
      List.foldl foo (Just ()) listListPat

mergeMaybePatterns : Maybe Pat -> List (Maybe Pat) -> Maybe ()
mergeMaybePatterns mp mps =
  case mp of
    Nothing -> if List.all ((==) Nothing) mps then Just () else Nothing
    Just p  -> Utils.bindMaybe (mergePatterns p) (Utils.projJusts mps)

annotatedNumToComparable : AnnotatedNum -> (Num, Frozen, Float, Float)
annotatedNumToComparable (n, frzn, wd) =
  case wd.val of
    IntSlider a _ b _ _ -> (n, frzn, toFloat a.val, toFloat b.val)
    NumSlider a _ b _ _ -> (n, frzn, a.val, b.val)
    NoWidgetDecl        -> (n, frzn, 1, -1)
