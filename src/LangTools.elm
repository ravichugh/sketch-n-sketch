module LangTools exposing (..)

-- Most of these methods used to be in Lang.elm
--
-- Extracted to avoid circular dependencies.
--
-- Lots of methods for dealing with identifiers and renaming.
--
-- Things in here are only used by LangSimplify and ValueBasedTransform,
-- currently. Also used in InterfaceView to find unfrozen locs to animate in our
-- (possibly defunct) relate attributes selection screen.

import Eval
import Lang exposing (..)
import FastParser exposing (prelude, isPreludeLocId, isPreludeEId)
import Utils
import LangUnparser exposing (unparseWithIds)
import Types

import Dict
import Regex
import Set


-- For ranking synthesized expressions
nodeCount : Exp -> Int
nodeCount exp =
  let expsNodeCount exps =
    exps |> List.map nodeCount |> List.sum
  in
  case exp.val.e__ of
    EConst _ _ _ _          -> 1
    EBase _ _               -> 1
    EVar _ x                -> 1
    EFun _ ps e _           -> 1 + patsNodeCount ps + nodeCount e
    EOp _ op es _           -> 1 + expsNodeCount es
    EList _ es _ (Just e) _ -> 1 + expsNodeCount es + nodeCount e
    EList _ es _ Nothing _  -> 1 + expsNodeCount es
    EIf _ e1 e2 e3 _        -> 1 + expsNodeCount [e1, e2, e3]
    -- Cases have a set of parens around each branch. I suppose each should count as a node.
    ECase _ e1 bs _         -> 1 + (List.length bs) + nodeCount e1 + patsNodeCount (branchPats bs) + expsNodeCount (branchExps bs)
    ETypeCase _ e1 tbs _    -> 1 + (List.length tbs) + nodeCount e1 + typesNodeCount (tbranchTypes tbs) + expsNodeCount (tbranchExps tbs)
    -- ETypeCase _ e1 tbranches _  ->
    EApp _ e1 es _          -> 1 + nodeCount e1 + expsNodeCount es
    ELet _ _ _ p e1 e2 _    -> 1 + patNodeCount p + nodeCount e1 + nodeCount e2
    EComment _ _ e1         -> 0 + nodeCount e1 -- Comments don't count.
    EOption _ _ _ _ e1      -> 1 + nodeCount e1
    ETyp _ p t e1 _         -> 1 + patNodeCount p + typeNodeCount t + nodeCount e1
    EColonType _ e1 _ t _   -> 1 + typeNodeCount t + nodeCount e1
    ETypeAlias _ p t e1 _   -> 1 + patNodeCount p + typeNodeCount t + nodeCount e1


-- O(n); for clone detection
subExpsOfSizeAtLeast : Int -> Exp -> List Exp
subExpsOfSizeAtLeast min exp =
  let (_, exps) = subExpsOfSizeAtLeast_ min exp in
  exps

-- Returns exact self size if < min, otherwise just some number >= min; and any ancestors of the minimum size
subExpsOfSizeAtLeast_ : Int -> Exp -> (Int, List Exp)
subExpsOfSizeAtLeast_ min exp =
  let (childrenTotal, largeSubExps) =
    childExps exp
    |> List.map (subExpsOfSizeAtLeast_ min)
    |> List.unzip
    |> (\(childrenSizes, subExpLists) -> (List.sum childrenSizes, List.concat subExpLists))
  in
  if childrenTotal >= min then
    if  not (isComment exp) then
      (childrenTotal, exp::largeSubExps)
    else
      (childrenTotal, largeSubExps)
  else
    let thisSizeWithoutChildren =
      case exp.val.e__ of
        EConst _ _ _ _          -> 1
        EBase _ _               -> 1
        EVar _ x                -> 1
        EFun _ ps e _           -> 1 + patsNodeCount ps
        EOp _ op es _           -> 1
        EList _ es _ (Just e) _ -> 1
        EList _ es _ Nothing _  -> 1
        EIf _ e1 e2 e3 _        -> 1
        -- Cases have a set of parens around each branch. I suppose each should count as a node.
        ECase _ e1 bs _         -> 1 + (List.length bs) + patsNodeCount (branchPats bs)
        ETypeCase _ e1 tbs _    -> 1 + (List.length tbs) + typesNodeCount (tbranchTypes tbs)
        EApp _ e1 es _          -> 1
        ELet _ _ _ p e1 e2 _    -> 1 + patNodeCount p
        EComment _ _ e1         -> 0 -- Comments don't count.
        EOption _ _ _ _ e1      -> 1
        ETyp _ p t e1 _         -> 1 + patNodeCount p + typeNodeCount t
        EColonType _ e1 _ t _   -> 1 + typeNodeCount t
        ETypeAlias _ p t e1 _   -> 1 + patNodeCount p + typeNodeCount t
    in
    if largeSubExps /= [] then
      Debug.crash "LangTools.thisSizeWithoutChildren bug"
    else
      -- Comment should be excluded here because size 0; if big enough we would have returned already.
      let thisSize = thisSizeWithoutChildren + childrenTotal in
      (thisSize, if thisSize >= min then [exp] else [])


-- -- Returns early. To turn O(n^2) counting into O(n*min) counting if you
-- -- count for every program node.
-- nodeCountAtLeast : Int -> Exp -> Bool
-- nodeCountAtLeast min exp =
--   nodeCountAtLeast_ min exp >= min
--
-- -- Returns exact count if count < min; otherwise returns some number >= min
-- nodeCountAtLeast_ : Int -> Exp -> Int
-- nodeCountAtLeast_ min exp =
--   let expsNodeCountAtLeast_ min exps =
--     case exps of
--       []    -> 0
--       e::es ->
--         let eCount = nodeCountAtLeast_ min e in
--         if eCount >= min
--         then eCount
--         else eCount + expsNodeCountAtLeast_ (min - eCount) es
--   in
--   if min <= 1 && not (isComment exp) then
--     1
--   else
--     case exp.val.e__ of
--       EConst _ _ _ _          -> 1
--       EBase _ _               -> 1
--       EVar _ x                -> 1
--       EFun _ ps e _           -> if 3 >= min then 3 else let pCount = patsNodeCount ps in 1 + pCount + nodeCountAtLeast_ (min - 1 - pCount) e
--       EOp _ op es _           -> 1 + expsNodeCountAtLeast_ (min - 1) es
--       EList _ es _ (Just e) _ -> 1 + expsNodeCountAtLeast_ (min - 1) (e::es)
--       EList _ es _ Nothing _  -> 1 + expsNodeCountAtLeast_ (min - 1) es
--       EIf _ e1 e2 e3 _        -> 1 + expsNodeCountAtLeast_ (min - 1) [e1, e2, e3]
--       -- Cases have a set of parens around each branch. I suppose each should count as a node.
--       ECase _ e1 bs _         -> let bCount  = List.length bs  in if 2 + 3*bCount >= min  then 2 + 3*bCount  else let pCounts  = patsNodeCount (branchPats bs)                      in 1 + bCount + pCounts + expsNodeCountAtLeast_ (min - 1 - bCount - pCounts) (e1 :: branchExps bs)
--       ETypeCase _ p tbs _     -> let tbCount = List.length tbs in if 2 + 3*tbCount >= min then 2 + 3*tbCount else let ptCounts = patNodeCount p + typesNodeCount (tbranchTypes tbs) in 1 + tbCount + ptCounts + expsNodeCountAtLeast_ (min - 1 - tbCount - ptCounts) (tbranchExps tbs)
--       -- ETypeCase _ e1 tbranches _  ->
--       EApp _ e1 es _          -> 1 + expsNodeCountAtLeast_ (min - 1) (e1::es)
--       ELet _ _ _ p e1 e2 _    -> if 4 >= min then 4 else let pCount = patNodeCount p in 1 + pCount + expsNodeCountAtLeast_ (min - 1 - pCount) [e1, e2]
--       EComment _ _ e1         -> 0 + nodeCountAtLeast_ min e1 -- Comments don't count.
--       EOption _ _ _ _ e1      -> 1 + nodeCountAtLeast_ (min - 1) e1
--       ETyp _ p t e1 _         -> if 4 >= min then 4 else let ptCount = patNodeCount p + typeNodeCount t in 1 + ptCount + nodeCountAtLeast_ (min - 1 - ptCount) e1
--       EColonType _ e1 _ t _   -> if 3 >= min then 3 else let tCount = typeNodeCount t in 1 + tCount + nodeCountAtLeast_ (min - 1 - tCount) e1
--       ETypeAlias _ p t e1 _   -> if 4 >= min then 4 else let ptCount = patNodeCount p + typeNodeCount t in 1 + ptCount + nodeCountAtLeast_ (min - 1 - ptCount) e1


patNodeCount : Pat -> Int
patNodeCount pat =
  case pat.val.p__ of
    PVar _ _ _                  -> 1
    PConst _ _                  -> 1
    PBase _ _                   -> 1
    PList _ pats _ (Just pat) _ -> 1 + patsNodeCount pats + patNodeCount pat
    PList _ pats _ Nothing    _ -> 1 + patsNodeCount pats
    PAs _ _ _ pat               -> 1 + patNodeCount pat

patsNodeCount : List Pat -> Int
patsNodeCount pats =
  pats |> List.map patNodeCount |> List.sum

typeNodeCount : Type -> Int
typeNodeCount tipe =
  case tipe.val of
    TNum _                          -> 1
    TBool _                         -> 1
    TString _                       -> 1
    TNull _                         -> 1
    TList _ t _                     -> 1 + typeNodeCount t
    TDict _ kt vt _                 -> 1 + typeNodeCount kt + typeNodeCount vt
    TTuple _ ts _ Nothing _         -> 1 + typesNodeCount ts
    TTuple _ ts _ (Just t) _        -> 1 + typesNodeCount ts + typeNodeCount t
    TArrow _ ts _                   -> 1 + typesNodeCount ts
    TUnion _ ts _                   -> 1 + typesNodeCount ts
    TNamed _ _                      -> 1
    TVar _ _                        -> 1
    TForall _ (One (_, _)) t _      -> 1 + typeNodeCount t
    TForall _ (Many _ idents _) t _ -> 1 + List.length idents + typeNodeCount t
    TWildcard _                     -> 1

typesNodeCount : List Type -> Int
typesNodeCount types =
  types |> List.map typeNodeCount |> List.sum


countNodes : (Exp -> Bool) -> Exp -> Int
countNodes pred exp =
  flattenExpTree exp
  |> Utils.count pred


patternListsEqual : List Pat -> List Pat -> Bool
patternListsEqual patsA patsB =
  Utils.maybeZip patsA patsB
  |> Maybe.map (List.all (\(patA, patB) -> patternsEqual patA patB))
  |> Maybe.withDefault False

-- More syntactically strict than "will these two patterns match/reject the same things?", i.e. identifier strings must also match exactly
patternsEqual : Pat -> Pat -> Bool
patternsEqual patA patB =
  case (patA.val.p__, patB.val.p__) of
    (PVar ws1A identA wdA,               PVar ws1B identB wdB)               -> identA == identB
    (PConst ws1A nA,                     PConst ws1B nB)                     -> nA == nB
    (PBase ws1A ebvA,                    PBase ws1B ebvB)                    -> eBaseValsEqual ebvA ebvB
    (PList ws1A psA ws2A Nothing ws3A,   PList ws1B psB ws2B Nothing ws3B)   -> patternListsEqual psA psB
    (PList ws1A psA ws2A (Just pA) ws3A, PList ws1B psB ws2B (Just pB) ws3B) -> patternListsEqual (pA::psA) (pB::psB)
    (PAs ws1A identA ws2A pA,            PAs ws1B identB ws2B pB)            -> identA == identB && patternsEqual pA pB
    _                                                                        -> False


-- Traverse baseExp and otherExp, comparing them to each other node by node.
-- When they differ, adds the differing node in otherExp to the return list.
--
-- For finding what expressions are being removed when an expression is replaced by a function.
--
-- Also suitable to compare expressions for equality, discounting whitespace and EIds etc.
-- (Though see also LangUnparser.unparseWithUniformWhitespace)
--
-- The sister functions here are ExpressionBasedTransform.passiveSynthesisSearch.(merge/mergeSingleArg)
extraExpsDiff : Exp -> Exp -> List Exp
extraExpsDiff baseExp otherExp =
  let childDiffs () =
    case Utils.maybeZip (childExps baseExp) (childExps otherExp) of
      Just childPairs -> childPairs |> List.concatMap (\(aChild, bChild) -> extraExpsDiff aChild bChild)
      Nothing         -> [otherExp]
  in
  case (baseExp.val.e__, otherExp.val.e__) of
    (EConst ws1A nA locA wdA,              EConst ws1B nB locB wdB)              -> if nA == nB then [] else [otherExp]
    (EBase ws1A ebvA,                      EBase ws1B ebvB)                      -> if eBaseValsEqual ebvA ebvB then [] else [otherExp]
    (EVar ws1A identA,                     EVar ws1B identB)                     -> if identA == identB then [] else [otherExp]
    (EFun ws1A psA eA ws2A,                EFun ws1B psB eB ws2B)                -> if patternListsEqual psA psB then extraExpsDiff eA eB else [otherExp]
    (EOp ws1A opA esA ws2A,                EOp ws1B opB esB ws2B)                -> if opA.val == opB.val then childDiffs () else [otherExp]
    (EList ws1A esA ws2A Nothing ws3A,     EList ws1B esB ws2B Nothing ws3B)     -> childDiffs ()
    (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)   -> childDiffs ()
    (EApp ws1A fA esA ws2A,                EApp ws1B fB esB ws2B)                -> childDiffs ()
    (ELet ws1A kindA recA pA e1A e2A ws2A, ELet ws1B kindB recB pB e1B e2B ws2B) -> if recA == recB && patternsEqual pA pB then extraExpsDiff e1A e1B ++ extraExpsDiff e2A e2B else [otherExp]
    (EIf ws1A e1A e2A e3A ws2A,            EIf ws1B e1B e2B e3B ws2B)            -> extraExpsDiff e1A e1B ++ extraExpsDiff e2A e2B ++ extraExpsDiff e3A e3B
    (ECase ws1A eA branchesA ws2A,         ECase ws1B eB branchesB ws2B)         -> Utils.maybeZip branchesA  branchesB  |> Maybe.andThen (\branchPairs  -> let bValPairs  = branchPairs  |> List.map (\(bA,  bB)  -> (bA.val,  bB.val))  in if bValPairs  |> List.all (\(Branch_  bws1A  bpatA   beA  bws2A,  Branch_  bws1B  bpatB   beB  bws2B)  -> patternsEqual bpatA bpatB)  then  Just (childDiffs ()) else Nothing) |> Maybe.withDefault [otherExp]
    (ETypeCase ws1A eA tbranchesA ws2A,    ETypeCase ws1B eB tbranchesB ws2B)    -> Utils.maybeZip tbranchesA tbranchesB |> Maybe.andThen (\tbranchPairs -> let tbValPairs = tbranchPairs |> List.map (\(tbA, tbB) -> (tbA.val, tbB.val)) in if tbValPairs |> List.all (\(TBranch_ tbws1A tbtypeA tbeA tbws2A, TBranch_ tbws1B tbtypeB tbeB tbws2B) -> Types.equal tbtypeA tbtypeB) then Just (childDiffs ()) else Nothing) |> Maybe.withDefault [otherExp]
    (EComment wsA sA e1A,                  _)                                    -> extraExpsDiff e1A otherExp
    (_,                                    EComment wsB sB e1B)                  -> extraExpsDiff baseExp e1B
    (EOption ws1A s1A ws2A s2A e1A,        EOption ws1B s1B ws2B s2B e1B)        -> [otherExp]
    (ETyp ws1A patA typeA eA ws2A,         ETyp ws1B patB typeB eB ws2B)         -> if patternsEqual patA patB && Types.equal typeA typeB then extraExpsDiff eA eB else [otherExp]
    (EColonType ws1A eA ws2A typeA ws3A,   EColonType ws1B eB ws2B typeB ws3B)   -> if Types.equal typeA typeB then extraExpsDiff eA eB else [otherExp]
    (ETypeAlias ws1A patA typeA eA ws2A,   ETypeAlias ws1B patB typeB eB ws2B)   -> if patternsEqual patA patB && Types.equal typeA typeB then extraExpsDiff eA eB else [otherExp]
    _                                                                            -> [otherExp]


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
  |> List.filter (\((locId, annotation, _), n) -> annotation /= "!" && not (isPreludeLocId locId))
  |> List.map (\((locId, _, _), n) -> (locId, n))


-- These should use syncOptions
-- Or we should remove the frozen by default config option
frozenLocIdsAndNumbers : Exp -> List (LocId, Num)
frozenLocIdsAndNumbers exp =
  allLocsAndNumbers exp
  |> List.filter (\((locId, annotation, _), n) -> annotation == "!" || isPreludeLocId locId)
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


justFindExpByEId : Exp -> EId -> Exp
justFindExpByEId exp eid =
  findExpByEId exp eid
  |> Utils.fromJust__ (\() -> "Couldn't find eid " ++ toString eid ++ " in " ++ unparseWithIds exp)


justFindExpWithAncestorsByEId : Exp -> EId -> List Exp
justFindExpWithAncestorsByEId root eid =
  findWithAncestorsByEId root eid
  |> Utils.fromJust__ (\() -> "justFindExpWithAncestorsByEId: Couldn't find eid " ++ toString eid ++ " in " ++ unparseWithIds root)


locationInProgram : Exp -> EId -> (Int, Int)
locationInProgram program eid =
  expToLocation (justFindExpByEId program eid)


expToLocation : Exp -> (Int, Int)
expToLocation exp =
  (exp.start.line, exp.start.col)


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


isTopLevelEId : EId -> Exp -> Bool
isTopLevelEId eid program =
  if eid == program.val.eid then
    True
  else
    case maybeTopLevelChild program of
      Just child -> isTopLevelEId eid child
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


lastExp : Exp -> Exp
lastExp exp =
  case childExps exp |> List.reverse of
    []           -> exp
    lastChild::_ -> lastExp lastChild


-- What exp actually determines the evaluated value of this exp?
expValueExp : Exp -> Exp
expValueExp exp =
  case exp.val.e__ of
    ETyp _ _ _ body _       -> expValueExp body
    EColonType _ body _ _ _ -> expValueExp body
    ETypeAlias _ _ _ body _ -> expValueExp body
    ELet _ _ _ _ _ body _   -> expValueExp body
    EComment _ _ e          -> expValueExp e
    EOption _ _ _ _ e       -> expValueExp e
    _                       -> exp


copyListWhitespace : Exp -> Exp -> Exp
copyListWhitespace templateList list =
  case (templateList.val.e__, list.val.e__) of
    (EList ws1 _ ws2 _ ws3, EList _ heads _ maybeTail _) ->
      replaceE__ list (EList ws1 heads ws2 maybeTail ws3)

    _ ->
      Debug.crash <| "Lang.copyListWs expected lists, but given " ++ unparseWithIds templateList ++ " and " ++ unparseWithIds list

longLineLength = 50 -- Not precisely 50, but roughly so. (using unparseWithUniformWhitespace to ensure convergence in one step; also don't count length of initial "(let ")

-- O(n^2) if applied recursively to children
-- O(n) if used once
reflowLetWhitespace : Exp -> Exp -> Exp
reflowLetWhitespace program letExp =
  case letExp.val.e__ of
    ELet oldLetWs letKind isRec pat boundExp body ws2 ->
      let oldIndentation = indentationOf letExp in
      let oneOrTwoNewlinesBeforeLet =
        let newlineCount = List.length (String.split "\n" oldLetWs.val) - 1 in
        if newlineCount <= 1
        then "\n"
        else "\n\n"
      in
      case boundExp.val.e__ of
        EFun fws1 fpats fbody fws2 ->
          let multipleLinesForFunction =
            String.contains "\n" (LangUnparser.unparse fbody)
            || longLineLength < String.length (LangUnparser.unparsePatWithUniformWhitespace True pat ++ LangUnparser.unparseWithUniformWhitespace True True boundExp)
          in
          let newLineForFunctionArgs =
            longLineLength < String.length (LangUnparser.unparsePatWithUniformWhitespace True pat ++ String.join " " (List.map (LangUnparser.unparsePatWithUniformWhitespace True) fpats))
          in
          let (letWs_, funcWs_, fbodyWs_, newFBody, bodyMinimalNewlineCount) =
            if isTopLevelEId letExp.val.eid program then
              let oldBodyWs = precedingWhitespace body in
              if      newLineForFunctionArgs   then ( "\n\n"                   , "\n  ", "\n    ", replaceIndentation "    " fbody, 2)
              else if multipleLinesForFunction then ( "\n\n"                   , " "   , "\n  "  , replaceIndentation "  " fbody  , 2)
              else                                  ( oneOrTwoNewlinesBeforeLet, " "   , " "     , fbody                          , 1)
            else
              let letWs = oneOrTwoNewlinesBeforeLet ++ oldIndentation in
              let (funcWs, fbodyWs, newFBody) =
                if      newLineForFunctionArgs   then ( "\n" ++ oldIndentation ++ "  ", "\n" ++ oldIndentation ++ "    ", replaceIndentation (oldIndentation ++ "    ") fbody )
                else if multipleLinesForFunction then ( " "                           , "\n" ++ oldIndentation ++ "  "  , replaceIndentation (oldIndentation ++ "  ") fbody   )
                else                                  ( " "                           , " "                             , fbody                                               )
              in
              let bodyMinimalNewlineCount =
                if multipleLinesForFunction then 2 else 1
              in
              (letWs, funcWs, fbodyWs, newFBody, bodyMinimalNewlineCount)
          in
          let (letWs, funcWs, fbodyWs) =
            (ws letWs_, ws funcWs_, ws fbodyWs_)
          in
          let newFunc =
            replaceE__ boundExp <|
              EFun
                  funcWs
                  (setPatListWhitespace "" " " fpats)
                  (replacePrecedingWhitespace fbodyWs_ newFBody)
                  space0
          in
          replaceE__ letExp <|
            ELet
                letWs
                letKind
                isRec
                (replacePrecedingWhitespacePat " " pat)
                newFunc
                (ensureNNewlinesExp bodyMinimalNewlineCount (extractIndentation letWs_) body)
                space0

        _ ->
          let minimalSurroundingNewlineCount =
            if String.contains "\n" (LangUnparser.unparse boundExp)
            then 2
            else 1
          in
          let newlinesBefore =
            if letExp.val.eid == program.val.eid -- First expression in a program does not need a newline.
            then 0
            else minimalSurroundingNewlineCount
          in
          replaceE__ letExp <|
            ELet
                (ws <| ensureNNewlines newlinesBefore oldLetWs.val oldLetWs.val)
                letKind
                isRec
                (replacePrecedingWhitespacePat " " pat)
                boundExp
                (ensureNNewlinesExp minimalSurroundingNewlineCount (extractIndentation oldLetWs.val) body)
                space0

          -- let addNewLineForBoundExp =
          --   (not <| String.contains "\n" (precedingWhitespace boundExp))
          --   && longLineLength < String.length (LangUnparser.unparsePatWithUniformWhitespace True pat ++ LangUnparser.unparseWithUniformWhitespace True True boundExp)
          -- in
          -- let (letWs, boundExpWs, bodyMinimalNewlineCount) =
          --   if isTopLevelEId letExp.val.eid program then
          --     let oldBodyWs = precedingWhitespace body in
          --     if addNewLineForBoundExp
          --     then ( "\n\n"                   , "\n  ", "\n\n" )
          --     else ( oneOrTwoNewlinesBeforeLet, " "   , if String.contains "\n" oldBodyWs then oldBodyWs else "\n" )
          --   else
          --     let letWs = oneOrTwoNewlinesBeforeLet ++ oldIndentation in
          --     let boundExpWs =
          --       if addNewLineForBoundExp
          --       then "\n" ++ oldIndentation ++ "  "
          --       else " "
          --     in
          --     let bodyMinimalNewlineCount =
          --       if multipleLinesForFunction then 2 else 1
          --     in
          --     (letWs, boundExpWs, bodyMinimalNewlineCount)
          -- in
          -- replaceE__ letExp <|
          --   ELet
          --       letWs
          --       letKind
          --       isRec
          --       (replacePrecedingWhitespacePat " " pat)
          --       (replacePrecedingWhitespace boundExpWs boundExp)
          --       (replacePrecedingWhitespace bodyWs body)
          --       ""

    _ ->
      Debug.crash <| "reflowLetWhitespace expected an ELet, got: " ++ unparseWithIds letExp


newLetFancyWhitespace : EId -> Pat -> Exp -> Exp -> Exp -> Exp
newLetFancyWhitespace insertedLetEId pat boundExp expToWrap program =
  let isTopLevel = isTopLevelEId expToWrap.val.eid program in
  let letOrDef = if isTopLevel then Def else Let in
  let newLetIndentation =
    -- If target expression is the body of a existing let, then use the indentation of the existing let.
    -- Otherwise, copy indentation of the wrapped expression.
    case parentByEId program expToWrap.val.eid of
      Just (Just parent) ->
        if (expToMaybeLetBody parent |> Maybe.map (.val >> .eid)) == Just expToWrap.val.eid
        then indentationAt parent.val.eid program
        else indentationAt expToWrap.val.eid program
      _ -> indentationAt expToWrap.val.eid program
  in
  let newlineCountAfterLet =
    let newlinesBeforeWrapped = List.length (String.split "\n" (precedingWhitespace expToWrap)) - 1 in
    if isTopLevel || newlinesBeforeWrapped >= 2
    then 2
    else 1
  in
  let newlineCountBeforeLet =
    if expToWrap.val.eid == program.val.eid then 1 else newlineCountAfterLet
  in
  let expToWrapWithNewWs =
    let wrappedExpIndent = if isLet expToWrap || isTopLevel then "" else "  " in
    if patHasNewlines pat || expHasNewlines boundExp
    then expToWrap |> ensureWhitespaceNNewlinesExp newlineCountAfterLet |> replaceIndentation wrappedExpIndent
    else expToWrap |> ensureWhitespaceSmartExp newlineCountAfterLet wrappedExpIndent
  in
  ELet space0 letOrDef False (ensureWhitespacePat pat) (replaceIndentation "  " boundExp |> ensureWhitespaceExp) expToWrapWithNewWs space0
  |> withDummyExpInfoEId insertedLetEId
  |> replacePrecedingWhitespace (String.repeat newlineCountBeforeLet "\n")
  |> indent newLetIndentation


newVariableVisibleTo : EId -> Ident -> Int -> Exp -> List EId -> Exp -> (Ident, Exp)
newVariableVisibleTo insertedLetEId suggestedName startingNumberForNonCollidingName boundExp observerEIds program =
  let
    newName =
      nonCollidingName suggestedName startingNumberForNonCollidingName (visibleIdentifiersAtEIds program (Set.fromList observerEIds))
    eidToWrap =
      deepestCommonAncestorWithNewline program (\exp -> List.member exp.val.eid observerEIds) |> .val |> .eid
    newProgram =
      program
      |> mapExpNode
          eidToWrap
          (\expToWrap ->
            newLetFancyWhitespace insertedLetEId (pVar newName) boundExp expToWrap program
          )
  in
  (newName, newProgram)


identifiersVisibleAtProgramEnd : Exp -> Set.Set Ident
identifiersVisibleAtProgramEnd program =
  let lastEId = (lastExp program).val.eid in
  visibleIdentifiersAtEIds program (Set.singleton lastEId)


-- e.g. "rect1 x" for (def rect1 (let x = ... in ...) ...)
locDescription program loc =
  String.join " " (locDescriptionParts program loc)


-- e.g. ["rect1", "x"] for (def rect1 (let x = ... in ...) ...)
locDescriptionParts program loc =
  let (locId, _, ident) = loc in
  let baseIdent = if ident == "" then "k" ++ toString locId else ident in
  let scopeNamesLiftedThrough = scopeNamesLocLiftedThrough program loc in
  scopeNamesLiftedThrough ++ [baseIdent]


defaultExpName = "thing"


-- Fallback for expDescriptionParts
simpleExpName : Exp -> String
simpleExpName exp =
  simpleExpNameWithDefault defaultExpName exp

simpleExpNameWithDefault default exp =
  case exp.val.e__ of
    EConst _ _ _ _        -> "num"
    EVar _ ident          -> ident
    EApp _ funE _ _       -> expToMaybeIdent funE |> Maybe.withDefault default
    EList _ _ _ _ _       -> "list"
    EOp _ _ es _          -> List.map (simpleExpNameWithDefault default) es |> Utils.findFirst ((/=) default) |> Maybe.withDefault default
    EBase _ ENull         -> "null"
    EBase _ (EString _ _) -> "string"
    EBase _ (EBool _)     -> "bool"
    EFun _ _ _ _          -> "func"
    _                     -> default

-- Suggest a name for the expression at eid in program
expNameForEId : Exp -> EId -> String
expNameForEId program targetEId =
  expNameForEIdWithDefault defaultExpName program targetEId

expNameForEIdWithDefault : String -> Exp -> EId -> String
expNameForEIdWithDefault default program targetEId =
  case expDescriptionParts program targetEId |> Utils.takeLast 1 of
    [name] ->
      name

    _ ->
      -- Should only hit this branch if exp not found, so really this
      -- will always return defaultExpName
      findExpByEId program targetEId
      |> Maybe.map (simpleExpNameWithDefault default)
      |> Maybe.withDefault default


-- Suggest a name for the expression exp in program
expNameForExp : Exp -> Exp -> String
expNameForExp program exp =
  expNameForExpWithDefault defaultExpName program exp

expNameForExpWithDefault : String -> Exp -> Exp -> String
expNameForExpWithDefault default program exp =
  case expDescriptionParts program exp.val.eid |> Utils.takeLast 1 of
    [name] ->
      name

    _ ->
      simpleExpNameWithDefault default exp


commonNameForEIds : Exp -> List EId -> String
commonNameForEIds program eids =
  commonNameForEIdsWithDefault defaultExpName program eids


leadingDigits   = Regex.regex "^[0-9]+"
leadingCapitals = Regex.regex "^[A-Z]+"

removeLeadingDigits : String -> String
removeLeadingDigits string =
  Regex.replace (Regex.AtMost 1) leadingDigits (\_ -> "") string

downcaseLeadingCapitals : String -> String
downcaseLeadingCapitals string =
  Regex.replace (Regex.AtMost 1) leadingCapitals (\{match} -> String.toLower match) string


-- Suggest a common name for the expressions at eids in program
--
-- yuck; common prefix of the unscoped names of the expressions
commonNameForEIdsWithDefault : String -> Exp -> List EId -> String
commonNameForEIdsWithDefault defaultName program eids =
  let expNames = eids |> List.map (expNameForEId program) in
  let prefixCandidate = Utils.commonPrefixString expNames |> removeLeadingDigits in
  let suffixCandidate = Utils.commonSuffixString expNames |> removeLeadingDigits in
  let candidate =
    let candidate =
      if prefixCandidate == "" then suffixCandidate else
      if suffixCandidate == "" then prefixCandidate else
      if prefixCandidate == defaultExpName then suffixCandidate else
      if suffixCandidate == defaultExpName then prefixCandidate else
      if prefixCandidate == "num" then suffixCandidate else
      if suffixCandidate == "num" then prefixCandidate else
      if String.length prefixCandidate < String.length suffixCandidate
      then suffixCandidate
      else prefixCandidate
    in
    downcaseLeadingCapitals candidate
  in
  if candidate == "" || candidate == "num" || candidate == defaultExpName then
    -- If candidates are all numbers, see if they look like indices and if so, use "i" as the common name.
    eids
    |> List.map (\eid -> findExpByEId program eid |> Maybe.andThen expToMaybeNum)
    |> Utils.projJusts
    |> Maybe.map
        (\nums ->
          if List.map (round >> toFloat) nums == nums && Utils.dedup nums == nums && (List.member 0 nums || List.member 1 nums) && List.all (\n -> 0 <= n && n < 10) nums
          then "i"
          else "num"
        )
    |> Maybe.withDefault (if candidate == "" then defaultName else candidate)
  else
    candidate


-- Look for targetEId in program exp.
--
-- Returns list of named scopes that contain the expression.
--
-- Last element is "name" of expression.
--
-- Could use some speeding up.
--
-- Empty list if not found.
expDescriptionParts : Exp -> EId -> List String
expDescriptionParts program targetEId =
  expDescriptionParts_ program program targetEId

expDescriptionParts_ : Exp -> Exp -> EId -> List String
expDescriptionParts_ program exp targetEId =
  let recurse e = expDescriptionParts_ program e targetEId in
  let varIdentOrDefault e default = expToMaybeIdent e |> Maybe.withDefault default in
  let searchChildren () =
    -- Recurse lazily through children.
    childExps exp
    |> Utils.mapFirstSuccess
        (\child ->
          case recurse child of
            []    -> Nothing
            parts -> Just parts
        )
    |> Maybe.withDefault []
  in
  if exp.val.eid == targetEId then
    [simpleExpName exp]
  else
    case exp.val.e__ of
      ELet _ _ _ pat assigns body _ ->
        let namedAssigns = tryMatchExpReturningList pat assigns in
        case List.filter (\(ident, e) -> findExpByEId e targetEId /= Nothing) namedAssigns of
          [] ->
            if assigns.val.eid == targetEId then
              -- e.g. want whole list, but the let only bound the individual list elements
              case identifiersListInPat pat of
                []        -> [simpleExpName assigns]
                pathedPatIdents -> [varIdentOrDefault assigns (String.join "" pathedPatIdents)]
            else
              let scopeNames =
                case pat.val.p__ of
                  PVar _ ident _  -> [ident]
                  PAs _ ident _ _ -> [ident]
                  _               ->
                    case identifiersListInPat pat of
                      []        -> []
                      pathedPatIdents -> [String.join "" pathedPatIdents]
              in
              case recurse assigns of
                []          -> recurse body
                deeperParts -> scopeNames ++ deeperParts

          identsAndMatchingExp ->
            -- Last match should be most specific.
            let (ident, matchingExp) = Utils.last "LangTools.expDescriptionParts" identsAndMatchingExp in
            let (idents, _) = identsAndMatchingExp |> List.unzip in
            if matchingExp.val.eid == targetEId then
              (Utils.dropLast 1 idents) ++ [varIdentOrDefault matchingExp ident]
            else
              case recurse matchingExp of
                []          -> Debug.crash <| "LangTools.expDescriptionParts expected to find targetEId in\n" ++ unparseWithIds matchingExp ++ "\nin\n" ++ unparseWithIds assigns
                deeperParts -> idents ++ deeperParts

      -- Try to use name of the function argument the expression is bound to.
      EApp ws1 fExp es ws2 ->
        -- Probably faster to first check for target in es (which will usually fail); avoids searching for binding of fExp
        case searchChildren () of
          [] -> []
          childrenResult ->
            expToMaybeIdent fExp
            |> Maybe.andThen
                (\funcName ->
                  case resolveIdentifierToExp funcName targetEId program of
                    Just (Bound boundExp) ->
                      case boundExp.val.e__ of
                        EFun _ pats _ _ ->
                          Utils.zip pats es
                          |> Utils.mapFirstSuccess
                              (\(pat, e) ->
                                case tryMatchExp pat e of
                                  Match bindings ->
                                    -- Not quite as complicated as the Let logic above; should be okay though.
                                    bindings
                                    |> Utils.findFirst (\(ident, e) -> e.val.eid == targetEId)
                                    |> Maybe.map (\(ident, e) -> [varIdentOrDefault e ident])
                                  _ -> Nothing
                              )

                        _ ->
                          Nothing

                    _ ->
                      Nothing

                )
            |> Maybe.withDefault childrenResult

      _ ->
        searchChildren ()


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
      Debug.crash <| "Found locId " ++ toString targetLocId ++ " more than once in the expression: " ++ unparseWithIds newLetBody


-- Returns array of matches (easiest to implement).
-- Should only be at most one match, though.
scopeNamesLocLiftedThrough_ targetLocId scopeNames exp =
  case exp.val.e__ of
    ELet _ _ _ pat assigns body _ ->
      let scopeNames_ =
        case pat.val.p__ of
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


-- If tryMatchExp is made less strict (allow partial matches), then be sure to also return unmatch identifiers (for StaticAnalysis).
type ExpMatchResult
  = Match (List (Ident, Exp))
  | NoMatch
  | CannotCompare


tryMatchExpReturningList : Pat -> Exp -> List (Ident, Exp)
tryMatchExpReturningList pat exp =
  case tryMatchExp pat exp of
    Match env -> env
    _         -> []

-- Match an expression with a pattern. (Taken from Brainstorm branch.)
--
-- For the purposes of naming, this may be more strict than needed. (No bindings unless complete match.)
tryMatchExp : Pat -> Exp -> ExpMatchResult
tryMatchExp pat exp =
  let matchMap f matchResult =
    case matchResult of
      Match env -> Match (f env)
      _         -> matchResult
  in
  -- CannotCompare overrides NoMatch overrides Match
  let projMatches resultList =
    resultList
    |> List.foldl
        (\matchResult acc ->
            case (matchResult, acc) of
              (Match env1, Match env2) -> Match (env1 ++ env2)
              (Match _, priorFailure)  -> priorFailure
              (_, CannotCompare)       -> CannotCompare
              (failure, _)             -> failure
        )
        (Match [])
  in
  -- case exp.val.e__ of
  --   EVal val ->
  --     case Eval.match (pat, val) of
  --       Nothing     -> NoMatch
  --       Just valEnv ->
  --         let expEnv =
  --           valEnv |> List.map (\(ident, val) -> (ident, eVal val))
  --         in
  --         Match expEnv
  --
  --   _ ->
  case exp.val.e__ of
    EColonType _ typedExp _ _ _ ->
      tryMatchExp pat typedExp

    _ ->
      case pat.val.p__ of
        PVar _ ident _         -> Match [(ident, exp)]
        PAs _ ident _ innerPat ->
          tryMatchExp innerPat exp
          |> matchMap (\env -> (ident, exp)::env)

        PList _ ps _ Nothing _ ->
          case exp.val.e__ of
            -- TODO: list must not have rest
            EList _ es _ Nothing _ ->
              case Utils.maybeZip ps es of
                Nothing    -> NoMatch
                Just pairs ->
                  List.map (\(p, e) -> tryMatchExp p e) pairs
                  |> projMatches

            _ ->
              CannotCompare

        PList _ ps _ (Just restPat) _ ->
          case exp.val.e__ of
            EList _ es _ Nothing _ ->
              if List.length es < List.length ps then
                NoMatch
              else
                let (headExps, tailExps) = Utils.split (List.length ps) es in
                let tryHeadMatch =
                  Utils.zip ps headExps
                  |> List.map (\(p, e) -> tryMatchExp p e)
                  |> projMatches
                in
                let tryTailMatch =
                  tryMatchExp restPat (eList tailExps Nothing)
                in
                [tryHeadMatch, tryTailMatch]
                |> projMatches

            -- TODO: must have same number of heads
            EList _ es _ (Just restExp) _ ->
              if List.length es < List.length ps then
                NoMatch
              else if List.length es /= List.length ps then
                CannotCompare
              else
                let tryHeadMatch =
                  Utils.zip ps es
                  |> List.map (\(p, e) -> tryMatchExp p e)
                  |> projMatches
                in
                let tryTailMatch =
                  tryMatchExp restPat restExp
                in
                [tryHeadMatch, tryTailMatch]
                |> projMatches

            _ ->
              CannotCompare

        PConst _ n ->
          case exp.val.e__ of
            EConst _ num _ _ -> if n == num then Match [] else NoMatch
            _                -> CannotCompare

        PBase _ bv ->
          case exp.val.e__ of
            EBase _ ev -> if eBaseValsEqual bv ev then Match [] else NoMatch
            _          -> CannotCompare


-- Returns the common ancestor just inside the deepest common scope -- the expression you want to wrap with new defintions.
-- If the nearest common ancestor is itself a scope, returns that instead.
--
-- Used to use this, now prefering deepestCommonAncestorWithNewline. (July 2017)
--
-- Remove this if it turns out that deepestCommonAncestorWithNewline is indeed better.
justInsideDeepestCommonScope : Exp -> (Exp -> Bool) -> Exp
justInsideDeepestCommonScope exp pred =
  let (maybeDeepestCommonScope, maybeAncestorJustInsideCommonScope) =
    deepestCommonScopeAndJustInside_ exp pred
  in
  let candidates =
    Utils.filterJusts [ Just exp, maybeDeepestCommonScope, maybeAncestorJustInsideCommonScope ]
  in
  Utils.last_ candidates

deepestCommonScope : Exp -> (Exp -> Bool) -> Exp
deepestCommonScope exp pred =
  let (maybeDeepestCommonScope, _) =
    deepestCommonScopeAndJustInside_ exp pred
  in
  let candidates =
    Utils.filterJusts [ Just exp, maybeDeepestCommonScope ]
  in
  Utils.last_ candidates

-- A let exp is not a scope for its boundExp, so can't just look at ancestor lists.
deepestCommonScopeAndJustInside_ : Exp -> (Exp -> Bool) -> (Maybe Exp, Maybe Exp)
deepestCommonScopeAndJustInside_ program pred =
  let allWithAncestorsScopesTagged =
    findAllWithAncestorsScopesTagged pred program
    |> List.map (Utils.dropLast 1)-- Never return an expression that the predicate matched: it will be moved/removed/replaced
  in
  let commonAncestorsScopesTagged = Utils.commonPrefix allWithAncestorsScopesTagged in
  let commonAncestors             = Utils.commonPrefix (List.map (List.map Tuple.first) allWithAncestorsScopesTagged) in
  let maybeDeepestCommonScope =
    commonAncestorsScopesTagged
    |> List.filter (\(_, isScope) -> isScope)
    |> Utils.maybeLast
    |> Maybe.map Tuple.first
  in
  -- A little wonkey to handle when a let's boundExp and body both match the predicate.
  -- The let itself should not be the scope, but it may be the expression just inside.
  let maybeAncestorJustInsideCommonScope =
    case commonAncestorsScopesTagged |> List.reverse |> Utils.takeWhile (\(_, isScope) -> not isScope) |> Utils.maybeLast of
      Nothing ->
        -- Last commonality in commonAncestorsScopesTagged is a scope. (Or no commonalities.)
        -- It is possible that there is one more common expression, but it is tagged differentialy as a scope or not.
        commonAncestors
        |> List.drop (List.length commonAncestorsScopesTagged)
        |> List.head

      Just (ancestorJustInsideCommonScope, _) ->
        Just ancestorJustInsideCommonScope
  in
  (maybeDeepestCommonScope, maybeAncestorJustInsideCommonScope)

deepestCommonAncestorWithNewline : Exp -> (Exp -> Bool) -> Exp
deepestCommonAncestorWithNewline program pred =
  commonAncestors pred program
  |> List.reverse
  |> Utils.findFirst (precedingWhitespace >> String.contains "\n")
  |> Maybe.withDefault program

deepestAncestorWithNewline : Exp -> EId -> Exp
deepestAncestorWithNewline program eid =
  let ancestors = justFindExpWithAncestorsByEId program eid |> Utils.dropLast 1 in
  let ancestorsWithNewlines =
    ancestors |> List.filter (precedingWhitespace >> String.contains "\n")
  in
  Utils.last_ (program :: ancestorsWithNewlines)

-- Given [ [("a", eConst 4 dummyLoc), ("b", eConst 5 dummyLoc)], [("c", eConst 6 dummyLoc)] ]
--
-- Wraps EId with:
--
-- (let [a c] [4 5]
-- (let [c] [6]
--   bodyExp))
--
-- Returns new whole program
wrapWithLets : List (List (String, Exp)) -> EId -> Exp -> Exp
wrapWithLets listOfListsOfNamesAndAssigns eidToWrap program =
  let nonEmptyListOfListsOfNamesAndAssigns =
    List.filter
        (not << List.isEmpty)
        listOfListsOfNamesAndAssigns
  in
  nonEmptyListOfListsOfNamesAndAssigns
  |> List.foldr
      (\letNamesAndAssigns program ->
        let (pat, boundExp) = patBoundExpOf letNamesAndAssigns in
        program
        |> mapExpNode
            eidToWrap
            (\expToWrap -> newLetFancyWhitespace -1 pat boundExp expToWrap program)
      )
      program


addFirstDef : Exp -> Pat -> Exp -> Exp
addFirstDef program pat boundExp =
  let firstNonCommentEId e =
    case e.val.e__ of
      EComment _ _ body -> firstNonCommentEId body
      _                 -> e.val.eid
  in
  program
  |> mapExpNode
      (firstNonCommentEId program)
      (\nonComment ->
        newLetFancyWhitespace -1 pat boundExp nonComment program
      )


expToMaybeNum : Exp -> Maybe Num
expToMaybeNum exp =
  case exp.val.e__ of
    EConst _ n _ _ -> Just n
    _              -> Nothing


expToMaybeVar : Exp -> Maybe Exp
expToMaybeVar exp =
  case exp.val.e__ of
    EVar _ _ -> Just exp
    _        -> Nothing


expToMaybeIdent : Exp -> Maybe Ident
expToMaybeIdent exp =
  case exp.val.e__ of
    EVar _ ident -> Just ident
    _            -> Nothing


expToIdent : Exp -> Ident
expToIdent exp =
  case exp.val.e__ of
    EVar _ ident -> ident
    _            -> Debug.crash <| "LangTools.expToIdent exp is not an EVar: " ++ unparseWithIds exp


patToMaybeIdent : Pat -> Maybe Ident
patToMaybeIdent pat =
  case pat.val.p__ of
    PVar _ ident _  -> Just ident
    PAs _ ident _ _ -> Just ident
    _               -> Nothing


patToMaybePVarIdent : Pat -> Maybe Ident
patToMaybePVarIdent pat =
  case pat.val.p__ of
    PVar _ ident _ -> Just ident
    _              -> Nothing


expToListParts : Exp -> (WS, List Exp, WS, Maybe Exp, WS)
expToListParts exp =
  case exp.val.e__ of
    EList ws1 heads ws2 maybeTail ws3 -> (ws1, heads, ws2, maybeTail, ws3)
    _                                 -> Debug.crash <| "LangTools.expToListParts exp is not an EList: " ++ unparseWithIds exp


expToLetParts : Exp -> (WS, LetKind, Bool, Pat, Exp, Exp, WS)
expToLetParts exp =
  case exp.val.e__ of
    ELet ws1 letKind rec p1 e1 e2 ws2 -> (ws1, letKind, rec, p1, e1, e2, ws2)
    _                                 -> Debug.crash <| "LangTools.expToLetParts exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetParts : Exp -> Maybe (WS, LetKind, Bool, Pat, Exp, Exp, WS)
expToMaybeLetParts exp =
  case exp.val.e__ of
    ELet ws1 letKind rec p1 e1 e2 ws2 -> Just (ws1, letKind, rec, p1, e1, e2, ws2)
    _                                 -> Nothing


expToLetKind : Exp -> LetKind
expToLetKind exp =
  case exp.val.e__ of
    ELet _ lk _ _ _ _ _ -> lk
    _                   -> Debug.crash <| "LangTools.expToLetKind exp is not an ELet: " ++ unparseWithIds exp


expToLetPat : Exp -> Pat
expToLetPat exp =
  case exp.val.e__ of
    ELet _ _ _ pat _ _ _ -> pat
    _                    -> Debug.crash <| "LangTools.expToLetPat exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetPat : Exp -> Maybe Pat
expToMaybeLetPat exp =
  case exp.val.e__ of
    ELet _ _ _ pat _ _ _ -> Just pat
    _                    -> Nothing


expToLetBoundExp : Exp -> Exp
expToLetBoundExp exp =
  case exp.val.e__ of
    ELet _ _ _ _ boundExp _ _ -> boundExp
    _                         -> Debug.crash <| "LangTools.expToLetPat exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetBoundExp : Exp -> Maybe Exp
expToMaybeLetBoundExp exp =
  case exp.val.e__ of
    ELet _ _ _ _ boundExp _ _ -> Just boundExp
    _                         -> Nothing


expToLetPatAndBoundExp : Exp -> (Pat, Exp)
expToLetPatAndBoundExp exp =
  case exp.val.e__ of
    ELet _ _ _ pat boundExp _ _ -> (pat, boundExp)
    _                           -> Debug.crash <| "LangTools.expToLetPatAndBoundExp exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetPatAndBoundExp : Exp -> Maybe (Pat, Exp)
expToMaybeLetPatAndBoundExp exp =
  case exp.val.e__ of
    ELet _ _ _ pat boundExp _ _ -> Just (pat, boundExp)
    _                           -> Nothing


expToLetBody : Exp -> Exp
expToLetBody exp =
  case exp.val.e__ of
    ELet _ _ _ _ _ body _ -> body
    _                     -> Debug.crash <| "LangTools.expToLetBody exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetBody : Exp -> Maybe Exp
expToMaybeLetBody exp =
  case exp.val.e__ of
    ELet _ _ _ _ _ body _ -> Just body
    _                     -> Nothing


expToFuncPats : Exp -> List Pat
expToFuncPats exp =
  case exp.val.e__ of
    EFun _ pats _ _ -> pats
    _               -> Debug.crash <| "LangTools.expToFuncPats exp is not an EFun: " ++ unparseWithIds exp


expToCaseScrutinee : Exp -> Exp
expToCaseScrutinee exp =
  case exp.val.e__ of
    ECase _ scrutinee _ _ -> scrutinee
    _                     -> Debug.crash <| "LangTools.expToScrutinee exp is not an ECase: " ++ unparseWithIds exp


-- This is a rather generous definition of literal.
isLiteral : Exp -> Bool
isLiteral exp =
  Set.size (freeIdentifiers exp) == 0


-------------------------------------------------------------
-- Identifier/scoping/binding functions


preludeIdentifiers = Eval.initEnv |> List.map Tuple.first |> Set.fromList


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


-- All identifiers used or bound throughout the given exp
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


allPats : Exp -> List Pat
allPats root =
  flattenExpTree root
  |> List.concatMap
      (\exp ->
        case exp.val.e__ of
          EFun _ pats _ _        -> pats
          ECase _ _ branches _   -> branchPats branches
          ELet _ _ _ pat _ _ _   -> [pat]
          ETyp _ pat _ _ _       -> [pat]
          ETypeAlias _ pat _ _ _ -> [pat]
          _                      -> []
      )


-- Look for all non-free identifiers in the expression.
identifiersSetPatsOnly : Exp -> Set.Set Ident
identifiersSetPatsOnly exp =
  identifiersListPatsOnly exp
  |> Set.fromList


identifiersListPatsOnly : Exp -> List Ident
identifiersListPatsOnly exp =
  let folder e__ acc =
    case e__ of
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
  case pat.val.p__ of
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


-- If suggestedName is not in existing names, returns it.
-- Otherwise appends a number (starting at i) that doesn't collide.
-- If the name contains "{n}", place the number there instead.
nonCollidingName : Ident -> Int -> Set.Set Ident -> Ident
nonCollidingName suggestedName i existingNames =
  nonCollidingNames [suggestedName] i existingNames |> Utils.head "LangTools.nonCollidingNames did not satisfy its invariant"


-- If suggestedName is not in existing names, returns it.
-- Otherwise appends a number (starting at i) that doesn't collide.
-- If the name contains "{n}", place the number there instead.
nonCollidingNames : List Ident -> Int -> Set.Set Ident -> List Ident
nonCollidingNames suggestedNames i existingNames =
  let plainSuggestedNames =
    suggestedNames
    |> List.map (Utils.stringReplace "{n}" "")
  in
  if not <| Utils.anyOverlap [existingNames, Set.fromList plainSuggestedNames] then
    plainSuggestedNames
  else
    let newNames =
      suggestedNames
      |> List.map
          (\name ->
            if String.contains "{n}" name then
              Utils.stringReplace "{n}" (toString i) name
            else
              name ++ (toString i)
          )
    in
    if not <| Utils.anyOverlap [existingNames, Set.fromList newNames]
    then newNames
    else nonCollidingNames suggestedNames (i+1) existingNames


renameIdentifierInPat old new pat =
  renameIdentifiersInPat (Dict.singleton old new) pat


renameIdentifiersInPat subst pat =
  let recurse = renameIdentifiersInPat subst in
  let recurseList = List.map recurse in
  let pat__ =
    case pat.val.p__ of
      PVar ws ident wd ->
        case Dict.get ident subst of
          Just new -> PVar ws new wd
          Nothing  -> pat.val.p__

      PList ws1 pats ws2 Nothing ws3 ->
        PList ws1 (recurseList pats) ws2 Nothing ws3

      PList ws1 pats ws2 (Just pRest) ws3 ->
        PList ws1 (recurseList pats) ws2 (Just (recurse pRest)) ws3

      PAs ws1 ident ws2 innerPat ->
        case Dict.get ident subst of
          Just new -> PAs ws1 new ws2 (recurse innerPat)
          Nothing  -> PAs ws1 ident ws2 (recurse innerPat)

      _ ->
        pat.val.p__
  in
  replaceP__ pat pat__


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
        let branches_ =
          List.map
              (mapValField (\(Branch_ bws1 pat ei bws2) -> Branch_ bws1 (renameIdentifiersInPat subst pat) ei bws2))
              branches
        in
        ECase ws1 e1 branches_ ws2

      ELet ws1 kind rec pat assign body ws2 ->
        ELet ws1 kind rec (renameIdentifiersInPat subst pat) assign body ws2

      _ ->
        e__
  in
  mapExpViaExp__
    exp__Renamer
    exp


setPatName : PathedPatternId -> Ident -> Exp -> Exp
setPatName ((scopeEId, branchI), path) newName exp =
  let maybeScopeExp = findExpByEId exp scopeEId in
  let maybeNewScopeExp =
    let makeNewScope e__ = replaceE__ (Utils.fromJust maybeScopeExp) e__ in
    case (Maybe.map (.val >> .e__) maybeScopeExp, path) of
      (Just (ELet ws1 letKind isRec pat boundExp body ws2), _)->
        let newPat = setPatNameInPat path newName pat in
        Just <| makeNewScope (ELet ws1 letKind isRec newPat boundExp body ws2)

      (Just (EFun ws1 pats body ws2), i::is) ->
        Utils.maybeGeti1 i pats
        |> Maybe.map
            (\pat ->
              let newPat = setPatNameInPat is newName pat in
              makeNewScope (EFun ws1 (Utils.replacei i newPat pats) body ws2)
            )

      (Just (ECase ws1 scrutinee branches ws2), _) ->
        Utils.maybeGeti1 branchI branches
        |> Maybe.map
            (\branch ->
              let (Branch_ ws1 pat exp ws2) = branch.val in
              let newPat = setPatNameInPat path newName pat in
              let newBranch = { branch | val = Branch_ ws1 newPat exp ws2 } in
              makeNewScope (ECase ws1 scrutinee (Utils.replacei branchI newBranch branches) ws2)
            )

      _ ->
        Nothing
  in
  case maybeNewScopeExp of
    Just newScopeExp -> replaceExpNode newScopeExp.val.eid newScopeExp exp
    Nothing          -> exp


setPatNameInPat : List Int -> Ident -> Pat -> Pat
setPatNameInPat path newName pat =
  case (pat.val.p__, path) of
    (PVar ws ident wd, []) ->
      replaceP__ pat (PVar ws newName wd)

    (PAs ws1 ident ws2 p, []) ->
      replaceP__ pat (PAs ws1 newName ws2 p)

    (PAs ws1 ident ws2 p, 1::is) ->
      replaceP__ pat (PAs ws1 ident ws2 (setPatNameInPat is newName p))

    (PList ws1 ps ws2 Nothing ws3, i::is) ->
      let newPs = Utils.getReplacei1 i (setPatNameInPat is newName) ps in
      replaceP__ pat (PList ws1 newPs ws2 Nothing ws3)

    (PList ws1 ps ws2 (Just tailPat) ws3, i::is) ->
      if i <= List.length ps then
        let newPs = Utils.getReplacei1 i (setPatNameInPat is newName) ps in
        replaceP__ pat (PList ws1 newPs ws2 (Just tailPat) ws3)
      else if i == List.length ps + 1 then
        replaceP__ pat (PList ws1 ps ws2 (Just (setPatNameInPat is newName tailPat)) ws3)
      else
        pat

    _ ->
      pat


-- Return the first expression(s) that can see the bound variables.
-- Returns [] if cannot find scope; letrec returns two expressions [boundExp, body]; others return singleton list.
findScopeAreas : ScopeId -> Exp -> List Exp
findScopeAreas (scopeEId, branchI) exp  =
  let maybeScopeExp = findExpByEId exp scopeEId in
  case Maybe.map (.val >> .e__) maybeScopeExp of
    Just (ELet _ _ isRec pat boundExp body _) ->
      if isRec
      then [boundExp, body]
      else [body]

    Just (EFun _ pats body _) ->
      [body]

    Just (ECase _ _ branches _) ->
      Utils.maybeGeti1 branchI (branchExps branches)
      |> Maybe.map (\branch -> [branch])
      |> Maybe.withDefault []

    _ ->
      []


-- Return the first expression(s) that can see the bound variables.
-- Probably only useful if ident is unique across the entire program (otherwise, this returns scope areas for all different bindings of the same name).
-- Returns [] if ident is not defined in any pattern; letrec returns two expressions [boundExp, body]; others return singleton list.
findScopeAreasByIdent : Ident -> Exp -> List Exp
findScopeAreasByIdent ident exp =
  exp
  |> flattenExpTree
  |> List.concatMap
      (\e ->
        case e.val.e__ of
          ELet _ _ isRec pat boundExp body _ ->
            if List.member ident (identifiersListInPat pat) then
              if isRec
              then [boundExp, body]
              else [body]
            else
              []

          EFun _ pats body _ ->
            if List.member ident (identifiersListInPats pats) then
              [body]
            else
              []

          ECase _ _ branches _ ->
            branchPatExps branches
            |> List.concatMap
                (\(bPat, bExp) ->
                  if List.member ident (identifiersListInPat bPat) then
                    [bExp]
                  else
                    []
                )

          _ ->
            []
      )


-- Nothing means not found or can't match pattern.
--
-- Only matches PVar or PAs for now
findPatAndBoundExpByPId : PId -> Exp -> Maybe (Pat, Exp)
findPatAndBoundExpByPId targetPId exp =
  case findScopeExpAndPat targetPId exp of
    Just (scopeExp, pat) ->
      case (expToMaybeLetPatAndBoundExp scopeExp, patToMaybeIdent pat) of
        (Just (letPat, letBoundExp), Just ident) ->
          tryMatchExpReturningList letPat letBoundExp
          |> Utils.maybeFind ident
          |> Maybe.map (\boundExp -> (pat, boundExp))

        _ ->
          Nothing

    Nothing ->
      Nothing


-- Nothing means not found or can't match pattern.
--
-- Only matches PVar or PAs for now
findBoundExpByPId : PId -> Exp -> Maybe Exp
findBoundExpByPId targetPId exp =
  findPatAndBoundExpByPId targetPId exp
  |> Maybe.map (\(pat, boundExp) -> boundExp)


-- Nothing means not found or can't match pattern.
findBoundExpByPathedPatternId : PathedPatternId -> Exp -> Maybe Exp
findBoundExpByPathedPatternId ((scopeEId, _), targetPath) exp =
  case findExpByEId exp scopeEId |> Maybe.andThen expToMaybeLetPatAndBoundExp of
    Just (letPat, letBoundExp) ->
        tryMatchExpPatToPaths letPat letBoundExp
        |> Utils.mapFirstSuccess (\(path, boundExp) -> if path == targetPath then Just boundExp else Nothing)

    Nothing ->
      Nothing


-- Oops I wrote this and we don't need it yet. I imagine we will though, so let's leave it be for now. (July 2017)
findScopeExpAndPat : PId -> Exp -> Maybe (Exp, Pat)
findScopeExpAndPat targetPId exp =
  exp
  |> mapFirstSuccessNode
      (\e ->
        let maybeTargetPat =
          case e.val.e__ of
            ELet _ _ _ pat _ _ _ -> findPatInPat targetPId pat
            EFun _ pats _ _      -> Utils.mapFirstSuccess (findPatInPat targetPId) pats
            ECase _ _ branches _ -> Utils.mapFirstSuccess (findPatInPat targetPId) (branchPats branches)
            _                    -> Nothing
        in
        maybeTargetPat |> Maybe.map (\pat -> (e, pat))
      )


findPatInPat : PId -> Pat -> Maybe Pat
findPatInPat targetPId pat =
  flattenPatTree pat
  |> Utils.findFirst (.val >> .pid >> (==) targetPId)


findScopeExpAndPatByPathedPatternId : PathedPatternId -> Exp -> Maybe (Exp, Pat)
findScopeExpAndPatByPathedPatternId ((scopeEId, branchI), path) exp =
  let maybeScopeExp = findExpByEId exp scopeEId in
  let maybePat =
    case (Maybe.map (.val >> .e__) maybeScopeExp, path) of
      (Just (ELet _ _ _ pat _ _ _), _) ->
        followPathInPat path pat

      (Just (EFun _ pats _ _), i::is) ->
        Utils.maybeGeti1 i pats
        |> Maybe.andThen (\pat -> followPathInPat is pat)

      (Just (ECase _ _ branches _), _) ->
        Utils.maybeGeti1 branchI (branchPats branches)
        |> Maybe.andThen (\pat -> followPathInPat path pat)

      _ ->
        Nothing
  in
  maybePat
  |> Maybe.map (\pat -> (Utils.fromJust maybeScopeExp, pat))


findPatByPathedPatternId : PathedPatternId -> Exp -> Maybe Pat
findPatByPathedPatternId pathedPatId exp =
  findScopeExpAndPatByPathedPatternId pathedPatId exp
  |> Maybe.map Tuple.second


followPathInPat : List Int -> Pat -> Maybe Pat
followPathInPat path pat =
  case (pat.val.p__, path) of
    (_, []) ->
      Just pat

    (PAs _ _ _ p, 1::is) ->
      followPathInPat is p

    (PList _ ps _ Nothing _, i::is) ->
      Utils.maybeGeti1 i ps
      |> Maybe.andThen (\p -> followPathInPat is p)

    (PList _ ps _ (Just tailPat) _, i::is) ->
      Utils.maybeGeti1 i (ps ++ [tailPat])
      |> Maybe.andThen (\p -> followPathInPat is p)

    _ ->
      Nothing


pathedPatternIdToPId : PathedPatternId -> Exp -> Maybe PId
pathedPatternIdToPId pathedPatId exp =
  findPatByPathedPatternId pathedPatId exp
  |> Maybe.map (.val >> .pid)


pathForIdentInPat : Ident -> Pat -> Maybe (List Int)
pathForIdentInPat targetIdent pat =
  indentPathsInPat pat
  |> Utils.mapFirstSuccess
      (\(ident, path) ->
        if ident == targetIdent
        then Just path
        else Nothing
      )


indentPathsInPat : Pat -> List (Ident, List Int)
indentPathsInPat pat =
  let childIdentPaths =
    childPats pat
    |> Utils.concatMapi1
        (\(i, childPat) ->
          indentPathsInPat childPat
          |> List.map (\(ident, path) -> (ident, i::path))
        )
  in
  case patToMaybeIdent pat of
    Just ident -> (ident, []) :: childIdentPaths
    Nothing    -> childIdentPaths


-- paths in expression list, e.g. function parameters
-- can recurse into list heads
expPathsInExpList : List Exp -> List (Exp, List Int)
expPathsInExpList exps =
  exps
  |> Utils.mapi1
      (\(i, exp) ->
        case exp.val.e__ of
          EList _ es _ _ _ -> [(exp, [i])] ++ List.map (\(e, path) -> (e, i::path)) (expPathsInExpList es)
          _                -> [(exp, [i])]
      )
  |> List.concat


eidPathInExpList : List Exp -> EId -> Maybe (List Int)
eidPathInExpList exps targetEId =
  expPathsInExpList exps
  |> Utils.mapFirstSuccess
      (\(e, path) -> if e.val.eid == targetEId then Just path else Nothing)


indentPIdsInPat : Pat -> List (Ident, PId)
indentPIdsInPat pat =
  flattenPatTree pat
  |> List.filterMap
      (\p ->
        case patToMaybeIdent p of
          Just ident -> Just (ident, p.val.pid)
          Nothing    -> Nothing
      )


tryMatchExpsPatsToPathsAtFunctionCall : List Pat -> List Exp -> List (List Int, Exp)
tryMatchExpsPatsToPathsAtFunctionCall pats exps =
  -- Allow partial application
  Utils.zip pats exps
  -- Not simply making a dummy pList/eList and sending that to
  -- tryMatchExpPatToPaths b/c want if we do then we will get
  -- a extra ([], dummyEList) result, which we don't want.
  |> Utils.mapi1
    (\(i, (p, e)) ->
      tryMatchExpPatToPaths_ p e
      |> Maybe.map (\pathAndExps -> List.map (\(path, e) -> (i::path, e)) pathAndExps)
    )
  |> Utils.projJusts
  |> Maybe.withDefault []
  |> List.concat


-- Match exp and pat, returning all the paths that could be matched to an expression
tryMatchExpPatToPaths : Pat -> Exp -> List (List Int, Exp)
tryMatchExpPatToPaths pat exp =
  tryMatchExpPatToPaths_ pat exp
  |> Maybe.withDefault []


-- Unlike tryMatchExp (currently), this will return partial matches
-- (For matching function calls with function arguments)
--
-- i.e. "Just ..." means partial or complete match
tryMatchExpPatToPaths_ : Pat -> Exp -> Maybe (List (List Int, Exp))
tryMatchExpPatToPaths_ pat exp =
  let thisMatch = ([], exp) in
  let addThisMatch matchResult =
    Maybe.map ((::) thisMatch) matchResult
  in
  let prependPath i (path, e) = (i::path, e) in
  let prependPathToAll i pathAndExps = List.map (prependPath i) pathAndExps in
  let matchListsAsFarAsPossible ps es =
    Utils.zip ps es
    |> Utils.mapi1
        (\(i, (p, e)) ->
          tryMatchExpPatToPaths_ p e |> Maybe.map (prependPathToAll i)
        )
    |> Utils.projJusts
    |> Maybe.map List.concat
  in
  case pat.val.p__ of
    PVar _ ident _ ->
      Just [thisMatch]

    PAs _ ident _ innerPat ->
      tryMatchExpPatToPaths_ innerPat exp
      |> Maybe.map (prependPathToAll 1)
      |> addThisMatch

    PList _ ps _ Nothing _ ->
      case exp.val.e__ of
        EList _ es _ Nothing _ ->
          if List.length ps /= List.length es then
            Nothing
          else
            matchListsAsFarAsPossible ps es
            |> addThisMatch

        EList _ es _ (Just tail) _ ->
          if List.length es > List.length ps then
            Nothing
          else
            matchListsAsFarAsPossible ps es
            |> addThisMatch

        _ ->
          Just [thisMatch]

    PList _ ps _ (Just restPat) _ ->
      case exp.val.e__ of
        EList _ es _ Nothing _ ->
          if List.length es < List.length ps then
            Nothing
          else
            -- Nothing in the tail has a definite path: skip it.
            matchListsAsFarAsPossible ps es
            |> addThisMatch

        EList _ es _ (Just restExp) _ ->
          if List.length es /= List.length ps then
            -- Nothing in the tail has a definite path: skip it.
            matchListsAsFarAsPossible ps es
            |> addThisMatch
          else
            matchListsAsFarAsPossible (ps ++ [restPat]) (es ++ [restExp])
            |> addThisMatch

        _ ->
          Just [thisMatch]

    PConst _ n ->
      case exp.val.e__ of
        EConst _ num _ _ -> if n == num then Just [thisMatch] else Nothing
        _                -> Just [thisMatch]

    PBase _ bv ->
      case exp.val.e__ of
        EBase _ ev -> if eBaseValsEqual bv ev then Just [thisMatch] else Nothing
        _          -> Just [thisMatch]


-- Given an EId, look for a name bound to it and the let scope that defined the binding.
findLetAndIdentBindingExp : EId -> Exp -> Maybe (Exp, Ident)
findLetAndIdentBindingExp targetEId program =
  program
  |> mapFirstSuccessNode
      (\exp ->
        case exp.val.e__ of
          ELet _ _ _ pat boundExp _ _ ->
            tryMatchExpReturningList pat boundExp
            |> Utils.mapFirstSuccess
                (\(ident, boundE) ->
                  if boundE.val.eid == targetEId
                  then Just (exp, ident)
                  else Nothing
                )

          _ ->
            Nothing
      )


-- All variables with the given name (no consideration for shadowing)
varsWithName : Ident -> Exp -> List Exp
varsWithName ident exp =
  flattenExpTree exp
  |> List.filter (expToMaybeIdent >> (==) (Just ident))


allVars : Exp -> List Exp
allVars root =
  flattenExpTree root
  |> List.filterMap expToMaybeVar


-- Which var idents in this exp refer to something outside this exp?
-- This is wrong for TypeCases; TypeCase scrutinee patterns not included. TypeCase scrutinee needs to turn into an expression (done on Brainstorm branch, I believe).
freeIdentifiers : Exp -> Set.Set Ident
freeIdentifiers exp =
  freeVars exp
  |> List.map expToMaybeIdent
  |> Utils.projJusts
  |> Utils.fromJust_ "LangTools.freeIdentifiers"
  |> Set.fromList


freeVars : Exp -> List Exp
freeVars exp =
  freeVars_ Set.empty exp


freeVars_ : Set.Set Ident -> Exp -> List Exp
freeVars_ boundIdentsSet exp =
  let recurse () =
    List.concatMap (freeVars_ boundIdentsSet) (childExps exp)
  in
  case exp.val.e__ of
    EConst _ i l wd             -> []
    EBase _ v                   -> []
    EVar _ x                    -> if Set.member x boundIdentsSet then [] else [exp]
    EFun _ ps e _               -> freeVars_ (Set.union (identifiersSetInPats ps) boundIdentsSet) e
    EOp _ op es _               -> recurse ()
    EList _ es _ m _            -> recurse ()
    EIf _ e1 e2 e3 _            -> recurse ()
    ECase _ e1 bs _             ->
      let freeInScrutinee = freeVars_ boundIdentsSet e1 in
      let freeInEachBranch =
        (List.map .val bs)
        |> List.concatMap (\(Branch_ _ bPat bExp _) -> freeVars_ (Set.union (identifiersSetInPat bPat) boundIdentsSet) bExp)
      in
      freeInScrutinee ++ freeInEachBranch

    ETypeCase _ e1 tbranches _  -> recurse ()
    EApp _ e1 es _              -> recurse ()
    ELet _ _ False p e1 e2 _    ->
      let freeInAssigns = freeVars_ boundIdentsSet e1 in
      let freeInBody    = freeVars_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
      freeInAssigns ++ freeInBody

    ELet _ _ True p e1 e2 _ ->
      let freeInAssigns = freeVars_ (Set.union (identifiersSetInPat p) boundIdentsSet) e1 in
      let freeInBody    = freeVars_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
      freeInAssigns ++ freeInBody

    EComment _ _ e1       -> recurse ()
    EOption _ _ _ _ e1    -> recurse ()
    ETyp _ _ _ e1 _       -> recurse ()
    EColonType _ e1 _ _ _ -> recurse ()
    ETypeAlias _ _ _ e1 _ -> recurse ()
    -- EVal _                -> Debug.crash "LangTools.freeVars_: shouldn't have an EVal in given expression"
    -- EDict _               -> Debug.crash "LangTools.freeVars_: shouldn't have an EDict in given expression"


-- Probably not useful unless program has been run through assignUniqueNames
allSimplyResolvableLetBindings : Exp -> List (Ident, Exp)
allSimplyResolvableLetBindings program =
  program
  |> flattenExpTree
  |> List.concatMap
      (\exp ->
        case exp.val.e__ of
          ELet _ _ _ pat boundExp _ _ -> tryMatchExpReturningList pat boundExp
          _                           -> []
      )

-- Precondition: program has been run through assignUniqueNames.
--
-- Identify all numeric variables in the program.
-- Not the smartest; there could be false negatives, but no false positives.
numericLetBoundIdentifiers : Exp -> Set.Set Ident
numericLetBoundIdentifiers program =
  let isSurelyNumeric numericIdents exp =
    let recurse e = isSurelyNumeric numericIdents e in
    case exp.val.e__ of
      EConst _ _ _ _ -> True
      EBase _ _      -> False
      EVar _ ident   -> Set.member ident numericIdents
      EFun _ _ _ _   -> False
      EApp _ _ _ _   -> False -- Not smart here.
      EOp _ op operands _ ->
        case op.val of
          Pi         -> True
          DictEmpty  -> False
          Cos        -> True
          Sin        -> True
          ArcCos     -> True
          ArcSin     -> True
          Floor      -> True
          Ceil       -> True
          Round      -> True
          ToStr      -> False
          Sqrt       -> True
          Explode    -> False
          DebugLog   -> List.any recurse operands
          NoWidgets  -> List.any recurse operands
          Plus       -> List.any recurse operands -- Can have string addition.
          Minus      -> True
          Mult       -> True
          Div        -> True
          Lt         -> False
          Eq         -> False
          Mod        -> True
          Pow        -> True
          ArcTan2    -> True
          DictGet    -> False
          DictRemove -> False
          DictInsert -> False

      EList _ _ _ _ _           -> False
      EIf _ _ thenExp elseExp _ -> recurse thenExp && recurse elseExp
      ECase _ _ branches _      -> List.all recurse (branchExps branches)
      ETypeCase _ _ tbranches _ -> List.all recurse (tbranchExps tbranches)
      EComment _ _ body         -> recurse body
      EOption _ _ _ _ body      -> recurse body
      ELet _ _ _ _ _ body _     -> recurse body
      ETyp _ _ _ body _         -> recurse body
      EColonType _ e _ _ _      -> recurse e
      ETypeAlias _ _ _ body _   -> recurse body
  in
  let expBindings = allSimplyResolvableLetBindings program in
  let findAllNumericIdents numericIdents =
    let moreNumericIdents =
      expBindings
      |> List.filter (\(ident, boundExp) -> isSurelyNumeric numericIdents boundExp)
      |> List.map Tuple.first
    in
    -- Have we converged?
    if List.length moreNumericIdents == Set.size numericIdents
    then numericIdents
    else findAllNumericIdents (Set.fromList moreNumericIdents)
  in
  findAllNumericIdents Set.empty


renameVarUntilBound : Ident -> Ident -> Exp -> Exp
renameVarUntilBound oldName newName exp =
  renameVarsUntilBound (Dict.singleton oldName newName) exp


-- Renames free variables only, which is great!
-- Preserves EIds (for Brainstorm)
renameVarsUntilBound : Dict.Dict Ident Ident -> Exp -> Exp
renameVarsUntilBound renamings exp =
  let renamer newName e =
    -- let _ = Debug.log ("Renaming " ++ newName ++ " on") e in
    case e.val.e__ of
      EVar ws oldName -> replaceE__ e (EVar ws newName)
      _               -> Debug.crash <| "LangTools.renameVarsUntilBound: renamer should only be passed an EVar, but given: " ++ toString e
  in
  let fnSubst =
    renamings
    |> Dict.map (\_ newName -> (renamer newName))
  in
  transformVarsUntilBound fnSubst exp


-- Transforms only free variables.
-- Preserves EIds (for Brainstorm)
-- Might be able to rewrite using freeVars or mapFoldExpTopDownWithScope
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
    -- EVal _                      -> exp
    EConst _ _ _ _              -> exp
    EBase _ _                   -> exp
    EVar _ ident                ->
      case Dict.get ident subst of
        Just f  -> f exp
        Nothing -> exp

    EFun ws1 ps e ws2           -> replaceE__ exp (EFun ws1 ps (recurseWithout (identifiersSetInPats ps) e) ws2)
    EOp ws1 op es ws2           -> replaceE__ exp (EOp ws1 op (List.map recurse es) ws2)
    EList ws1 es ws2 m ws3      -> replaceE__ exp (EList ws1 (List.map recurse es) ws2 (Maybe.map recurse m) ws3)

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

    -- EDict _                         -> Debug.crash "LangTools.transformVarsUntilBound: shouldn't have an EDict in given expression"


-- Find EVars using the given name, until name is rebound.
identifierUses : Ident -> Exp -> List Exp
identifierUses ident exp =
  freeVars exp
  |> List.filter (expToIdent >> (==) ident)


identifierUsageEIds : Ident -> Exp -> List EId
identifierUsageEIds ident exp =
  identifierUses ident exp
  |> List.map (.val >> .eid)


-- Find EVars in the set of identifiers, until name is rebound.
identifierSetUses : Set.Set Ident -> Exp -> List Exp
identifierSetUses identSet exp =
  freeVars exp
  |> List.filter (\varExp -> Set.member (expToIdent varExp) identSet)


-- Presumes ident is unique in the program (only one pattern defines it).
-- Returns [] if identifier never used or identifier is free.
identifierUsesAfterDefiningPat : Ident -> Exp -> List Exp
identifierUsesAfterDefiningPat ident exp =
  findScopeAreasByIdent ident exp
  |> List.concatMap (\scopeAreaExp -> identifierUses ident scopeAreaExp)


-- What variable names are in use at any of the given locations?
-- For help finding unused names during synthesis.
visibleIdentifiersAtEIds : Exp -> Set.Set EId -> Set.Set Ident
visibleIdentifiersAtEIds program eids =
  let programIdents = visibleIdentifiersAtPredicateNoPrelude program (\exp -> Set.member exp.val.eid eids) in
  Set.union programIdents preludeIdentifiers


visibleIdentifiersAtPredicateNoPrelude : Exp -> (Exp -> Bool) -> Set.Set Ident
visibleIdentifiersAtPredicateNoPrelude exp pred =
  visibleIdentifiersAtPredicate_ Set.empty exp pred


visibleIdentifiersAtPredicate_ : Set.Set Ident -> Exp -> (Exp -> Bool) -> Set.Set Ident
visibleIdentifiersAtPredicate_ idents exp pred =
  let ret deeperIdents =
    if (0 == Set.size deeperIdents) && pred exp then
      -- If any child was a target EId, then deeperIdents is a superset of idents,
      -- so no need to union.
      idents
    else
      deeperIdents
  in
  let recurse e =
    visibleIdentifiersAtPredicate_ idents e pred
  in
  let recurseAllChildren () =
    childExps exp |> List.map recurse |> Utils.unionAll
  in
  let recurseWithNewIdents pats e =
    visibleIdentifiersAtPredicate_ (Set.union (identifiersSetInPats pats) idents) e pred
  in
  case exp.val.e__ of
    -- EVal _           -> ret Set.empty
    EConst _ _ _ _   -> ret Set.empty
    EBase _ _        -> ret Set.empty
    EVar _ ident     -> ret Set.empty -- Referencing a var doesn't count.
    EFun _ ps e _    -> ret <| recurseWithNewIdents ps e
    EOp _ op es _    -> ret <| recurseAllChildren ()
    EList _ es _ m _ -> ret <| recurseAllChildren ()
    EIf _ e1 e2 e3 _ -> ret <| recurseAllChildren ()
    ECase _ e1 bs _  ->
      let scrutineeResult = recurse e1 in
      let branchResults =
        bs
        |> List.map .val
        |> List.map
            (\(Branch_ _ bPat bExp _) -> recurseWithNewIdents [bPat] bExp)
      in
      ret <| Utils.unionAll (scrutineeResult::branchResults)

    ETypeCase _ scrutinee tbranches _ -> ret <| recurseAllChildren ()
    EApp _ e1 es _                    -> ret <| recurseAllChildren ()
    ELet _ kind False p e1 e2 _       ->
      let assignResult = recurse e1 in
      let bodyResult   = recurseWithNewIdents [p] e2 in
      ret <| Set.union assignResult bodyResult

    ELet _ kind True p e1 e2 _ ->
      let assignResult = recurseWithNewIdents [p] e1 in
      let bodyResult   = recurseWithNewIdents [p] e2 in
      ret <| Set.union assignResult bodyResult

    EComment _ s e1           -> ret <| recurse e1
    EOption _ s1 _ s2 e1      -> ret <| recurse e1
    ETyp _ pat tipe e _       -> ret <| recurse e
    EColonType _ e _ tipe _   -> ret <| recurse e
    ETypeAlias _ pat tipe e _ -> ret <| recurse e

    -- EDict _                   -> Debug.crash "LangTools.visibleIdentifiersAtEIds_: shouldn't have an EDict in given expression"


assignUniqueNames : Exp -> (Exp, Dict.Dict Ident Ident)
assignUniqueNames program =
  let initialUsedNames =
    -- Want to rename _everything_ so that there's multiple options for how to rename back to the originals
    identifiersSetPlusPrelude program
  in
  let (newProgram, usedNames, newNameToOldName) =
    assignUniqueNames_ program initialUsedNames Dict.empty
  in
  (newProgram, newNameToOldName)


assignUniqueNames_ : Exp -> Set.Set Ident -> Dict.Dict Ident Ident -> (Exp, Set.Set Ident, Dict.Dict Ident Ident)
assignUniqueNames_ exp usedNames oldNameToNewName =
  let recurse = assignUniqueNames_ in
  let recurseExps es =
    es
    |> List.foldl
        (\e (newEs, usedNames, newNameToOldName) ->
          let (newE, usedNames_, newNameToOldName_) = recurse e usedNames oldNameToNewName in
          (newEs ++ [newE], usedNames_, Dict.union newNameToOldName_ newNameToOldName)
        )
        ([], usedNames, Dict.empty)
  in
  let recurseExp e =
    let (newEs, usedNames, newNameToOldName) = recurseExps [e] in
    (Utils.head "assignUniqueNames_ head1" newEs, usedNames, newNameToOldName)
  in
  let assignUniqueNamesToPat_ pat usedNames =
    identifiersListInPat pat
    |> List.foldl
        (\name (pat, usedNames, oldNameToNewName) ->
          if Set.member name usedNames then
            let newName = nonCollidingName name 2 usedNames in
            (renameIdentifierInPat name newName pat, Set.insert newName usedNames, Dict.insert name newName oldNameToNewName)
          else
            (pat, Set.insert name usedNames, oldNameToNewName)
        )
        (pat, usedNames, Dict.empty)
  in
  let leafUnchanged = (exp, usedNames, Dict.empty) in
  case exp.val.e__ of
    EConst _ _ _ _  -> leafUnchanged
    EBase _ _       -> leafUnchanged
    EVar ws oldName ->
      case Dict.get oldName oldNameToNewName of
        Just newName  -> (replaceE__ exp (EVar ws newName), usedNames, Dict.empty)
        Nothing       -> leafUnchanged

    EFun ws1 ps e ws2 ->
      let (newPs, usedNames_, oldNameToNewNameAdditions) =
        ps
        |> List.foldl
            (\p (newPs, usedNames, oldNameToNewNameAdditions) ->
              let (newPat, usedNames_, oldNameToNewNameAdditions_) = assignUniqueNamesToPat_ p usedNames in
              (newPs ++ [newPat], usedNames_, Dict.union oldNameToNewNameAdditions_ oldNameToNewNameAdditions)
            )
            ([], usedNames, Dict.empty)
      in
      let (newBody, usedNames__, newNameToOldName) = recurse e usedNames_ (Dict.union oldNameToNewNameAdditions oldNameToNewName) in
      let newNameToOldName_ = Dict.union (Utils.flipDict oldNameToNewNameAdditions) newNameToOldName in
      ( replaceE__ exp (EFun ws1 newPs newBody ws2)
      , usedNames__
      , newNameToOldName_
      )

    EOp ws1 op es ws2 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps es in
      ( replaceE__ exp (EOp ws1 op newEs ws2)
      , usedNames_
      , newNameToOldName
      )

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps es in
      ( replaceE__ exp (EList ws1 newEs ws2 Nothing ws3)
      , usedNames_
      , newNameToOldName
      )

    EList ws1 es ws2 (Just tail) ws3 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps (es ++ [tail]) in
      let (newHeads, newTail) = (Utils.removeLastElement newEs, Utils.last "assignUniqueNames_" newEs) in
      ( replaceE__ exp (EList ws1 newHeads ws2 (Just newTail) ws3)
      , usedNames_
      , newNameToOldName
      )

    EIf ws1 e1 e2 e3 ws2 ->
      let (newEs, usedNames_, newNameToOldName) = recurseExps [e1, e2, e3] in
      case newEs of
        [newE1, newE2, newE3] ->
          ( replaceE__ exp (EIf ws1 newE1 newE2 newE3 ws2)
          , usedNames_
          , newNameToOldName
          )

        _ ->
          Debug.crash "assignUniqueNames_ EIf"

    ECase ws1 e1 bs ws2 ->
      let (newScrutinee, usedNames_, newNameToOldName) = recurse e1 usedNames oldNameToNewName in
      let (newBranches, usedNames__, newNameToOldName_) =
        bs
        |> List.foldl
            (\branch (newBranches, usedNames, newNameToOldName) ->
              let (Branch_ bws1 bPat bExp bws2) = branch.val in
              let (newPat, usedNames_, oldNameToNewNameAdditions) = assignUniqueNamesToPat_ bPat usedNames in
              let (newBody, usedNames__, newNameToOldName_) = recurse bExp usedNames_ (Dict.union oldNameToNewNameAdditions oldNameToNewName) in
              let newBranch = { branch | val = Branch_ bws1 newPat newBody bws2 } in
              (newBranches ++ [newBranch], usedNames__, Dict.union (Utils.flipDict oldNameToNewNameAdditions) (Dict.union newNameToOldName_ newNameToOldName))
            )
            ([], usedNames_, newNameToOldName)
      in
      ( replaceE__ exp (ECase ws1 newScrutinee newBranches ws2)
      , usedNames__
      , newNameToOldName_
      )

    ETypeCase ws1 e1 tbs ws2 ->
      let (newScrutinee, usedNames_, newNameToOldName) = recurse e1 usedNames oldNameToNewName in
      let (newTBranches, usedNames__, newNameToOldName_) =
        tbs
        |> List.foldl
            (\tbranch (newTBranches, usedNames, newNameToOldName) ->
              let (TBranch_ bws1 bType bExp bws2) = tbranch.val in
              let (newBody, usedNames_, newNameToOldName_) = recurse bExp usedNames oldNameToNewName in
              let newTBranch = { tbranch | val = TBranch_ bws1 bType newBody bws2 } in
              (newTBranches ++ [newTBranch], usedNames_, Dict.union newNameToOldName_ newNameToOldName)
            )
            ([], usedNames_, newNameToOldName)
      in
      ( replaceE__ exp (ETypeCase ws1 newScrutinee newTBranches ws2)
      , usedNames__
      , newNameToOldName_
      )

    EApp ws1 e1 es ws2 ->
      let (newE1AndEs, usedNames_, newNameToOldName) = recurseExps (e1::es) in
      let (newE1, newEs) = (Utils.head "assignUniqueNames_ head" newE1AndEs, Utils.tail "assignUniqueNames_ tail" newE1AndEs) in
      ( replaceE__ exp (EApp ws1 newE1 newEs ws2)
      , usedNames_
      , newNameToOldName
      )

    ELet ws1 kind isRec p e1 e2 ws2 ->
      let (newPat, usedNames_, oldNameToNewNameAdditions) = assignUniqueNamesToPat_ p usedNames in
      let oldNameToNewNameWithAdditions = Dict.union oldNameToNewNameAdditions oldNameToNewName in
      let oldNameToNewNameForBoundExp =
        if isRec then
          oldNameToNewNameWithAdditions
        else
          oldNameToNewName
      in
      let (newE1, usedNames__, newNameToOldName)   = recurse e1 usedNames_ oldNameToNewNameForBoundExp in
      let (newE2, usedNames___, newNameToOldName_) = recurse e2 usedNames__ oldNameToNewNameWithAdditions in
      let newNameToOldName__ = Dict.union (Utils.flipDict oldNameToNewNameAdditions) (Dict.union newNameToOldName_ newNameToOldName) in
      ( replaceE__ exp (ELet ws1 kind isRec newPat newE1 newE2 ws2)
      , usedNames___
      , newNameToOldName__
      )

    EComment ws s e1 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (EComment ws s newE1)
      , usedNames_
      , newNameToOldName
      )

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (EOption ws1 s1 ws2 s2 newE1)
      , usedNames_
      , newNameToOldName
      )

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (ETyp ws1 pat tipe newE1 ws2)
      , usedNames_
      , newNameToOldName
      )

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (EColonType ws1 newE1 ws2 tipe ws3)
      , usedNames_
      , newNameToOldName
      )

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, usedNames_, newNameToOldName) = recurseExp e1 in
      ( replaceE__ exp (ETypeAlias ws1 pat tipe newE1 ws2)
      , usedNames_
      , newNameToOldName
      )

    -- EDict _                         -> Debug.crash "LangTools.transformVarsUntilBound: shouldn't have an EDict in given expression"


-- Compute the PathedPatternId that assigned the binding referenced by varExp
--
-- Uses ident of given varExp, returns that name's binding at varExp's EId in program.
--
-- Returns Nothing if free in program or not in program
bindingPathedPatternIdFor : Exp -> Exp -> Maybe PathedPatternId
bindingPathedPatternIdFor varExp program =
  let targetName = expToIdent varExp in
  let targetEId  = varExp.val.eid in
  bindingPathedPatternIdForIdentAtEId targetName targetEId program


bindingPathedPatternIdForIdentAtEId : Ident -> EId -> Exp -> Maybe PathedPatternId
bindingPathedPatternIdForIdentAtEId targetName targetEId program =
  let predMap exp maybeCurrentBindingPathedPatternId =
    case exp.val.e__ of
      EVar _ _ ->
        if exp.val.eid == targetEId then
          Just maybeCurrentBindingPathedPatternId
        else
          Nothing

      _ ->
        Nothing
  in
  bindingPathedPatternIdFor_ Nothing targetName predMap program
  |> Maybe.withDefault Nothing


-- Presuming names are unique, returns the first PathedPatternId with the given name
bindingPathedPatternIdForUniqueName : Ident -> Exp -> Maybe PathedPatternId
bindingPathedPatternIdForUniqueName targetName program =
  let predMap e maybeCurrentBindingPathedPatternId = maybeCurrentBindingPathedPatternId in
  bindingPathedPatternIdFor_ Nothing targetName predMap program


-- Compute the ScopeId that assigned the binding referenced by varExp
--
-- Uses ident of given varExp, returns that name's binding at varExp's EId in program.
--
-- Returns Nothing if free in program or not in program
bindingScopeIdFor : Exp -> Exp -> Maybe ScopeId
bindingScopeIdFor varExp program =
  bindingPathedPatternIdFor varExp program
  |> Maybe.map (\(scopeId, path) -> scopeId)


bindingScopeIdForIdentAtEId : Ident -> EId -> Exp -> Maybe ScopeId
bindingScopeIdForIdentAtEId targetName targetEId program =
  bindingPathedPatternIdForIdentAtEId targetName targetEId program
  |> Maybe.map (\(scopeId, path) -> scopeId)


-- "Nothing" means free in program
allVarEIdsToBindingPId : Exp -> Dict.Dict EId (Maybe PId)
allVarEIdsToBindingPId program =
  allVarEIdsToBindingPIdList program
  |> Dict.fromList

-- "Nothing" means free in program
-- May want this list version when you might have duplicate EIds
allVarEIdsToBindingPIdList : Exp -> List (EId, Maybe PId)
allVarEIdsToBindingPIdList program =
  let handleELet letExp identToPId =
    Dict.union
        (expToLetPat letExp |> indentPIdsInPat |> Dict.fromList)
        identToPId
  in
  let handleEFun funcExp identToPId =
    Dict.union
        (expToFuncPats funcExp |> List.concatMap indentPIdsInPat |> Dict.fromList)
        identToPId
  in
  let handleCaseBranch caseExp branch branchI identToPId =
    Dict.union
        (branchPat branch |> indentPIdsInPat |> Dict.fromList)
        identToPId
  in
  program
  |> foldExpTopDownWithScope
      (\exp eidAndMaybePId identToPId ->
        case expToMaybeIdent exp of
          Just ident -> (exp.val.eid, Dict.get ident identToPId) :: eidAndMaybePId
          Nothing    -> eidAndMaybePId
      )
      handleELet
      handleEFun
      handleCaseBranch
      []
      Dict.empty


-- Presumes program has been run through assignUniqueNames
-- "Nothing" means no matching name in program
allVarEIdsToBindingPIdBasedOnUniqueName : Exp -> Dict.Dict EId (Maybe PId)
allVarEIdsToBindingPIdBasedOnUniqueName program =
  let allIdentToPId =
    flattenExpTree program
    |> List.concatMap
        (\exp ->
          case exp.val.e__ of
            EFun _ pats _ _      -> List.concatMap indentPIdsInPat pats
            ELet _ _ _ pat _ _ _ -> indentPIdsInPat pat
            ECase _ _ branches _ -> List.concatMap indentPIdsInPat (branchPats branches)
            _                    -> []
        )
    |> Dict.fromList
  in
  flattenExpTree program
  |> List.filterMap
      (\exp ->
        case expToMaybeIdent exp of
          Nothing    -> Nothing
          Just ident ->
            case Dict.get ident allIdentToPId of
              Nothing  -> Just (exp.val.eid, Nothing)
              Just pid -> Just (exp.val.eid, Just pid)
      )
  |> Dict.fromList


-- Outer returned maybe indicates if variable found
-- Inner returned maybe is whatever you want to return from the predicateMap, presumably the pathedPatternId that bound the identifier as seen from the usage site (Nothing means free)
bindingPathedPatternIdFor_ : Maybe PathedPatternId -> Ident -> (Exp -> Maybe PathedPatternId -> Maybe a) -> Exp -> Maybe a
bindingPathedPatternIdFor_ currentBindingPathedPatternId targetName predicateMap exp =
  let recurse pathedPatternId e = bindingPathedPatternIdFor_ pathedPatternId targetName predicateMap e in
  let maybeNewBindingForRecursion pat branchI pathPrefix =
    pathForIdentInPat targetName pat
    |> Maybe.map (\path -> Just ((exp.val.eid, branchI), pathPrefix ++ path))
  in
  case predicateMap exp currentBindingPathedPatternId of
    Just result -> Just result
    Nothing ->
      case exp.val.e__ of
        EFun _ pats body _ ->
          let newBindingPathedPatternId =
            pats
            |> Utils.zipi1
            |> Utils.mapFirstSuccess (\(i, pat) -> maybeNewBindingForRecursion pat 1 [i])
            |> Maybe.withDefault currentBindingPathedPatternId
          in
          recurse newBindingPathedPatternId body

        ELet _ _ isRecursive pat boundExp body _ ->
          let newBindingPathedPatternId =
            maybeNewBindingForRecursion pat 1 []
            |> Maybe.withDefault currentBindingPathedPatternId
          in
          let pathedPatternIdForBoundExp = if isRecursive then newBindingPathedPatternId else currentBindingPathedPatternId in
          Utils.firstOrLazySecond
              (recurse pathedPatternIdForBoundExp boundExp)
              (\() -> recurse newBindingPathedPatternId body)

        ECase _ _ branches _ ->
          branchPatExps branches
          |> Utils.zipi1
          |> Utils.mapFirstSuccess
              (\(i, (pat, branchExp)) ->
                let newBindingPathedPatternId =
                  maybeNewBindingForRecursion pat i []
                  |> Maybe.withDefault currentBindingPathedPatternId
                in
                recurse newBindingPathedPatternId branchExp
              )

        _ ->
          Utils.mapFirstSuccess (recurse currentBindingPathedPatternId) (childExps exp)


-- Returns one of:
--   Just (Bound exp)   -- var bound to expression
--   Just BoundUnknown  -- var bound, not smart enough to say to which expression
--   Nothing            -- var is free
resolveIdentifierToExp : Ident -> EId -> Exp -> Maybe ExpressionBinding
resolveIdentifierToExp ident viewerEId program =
  expEnvAt program viewerEId
  |> Maybe.andThen (Dict.get ident)


maybeResolveIdentifierToExp : Ident -> EId -> Exp -> Maybe Exp
maybeResolveIdentifierToExp ident viewerEId program =
  case resolveIdentifierToExp ident viewerEId program of
    Just (Bound exp) -> Just exp
    _                -> Nothing


type ExpressionBinding
  = Bound Exp
  | BoundUnknown


-- Too much recursion here, for some reason.
preludeExpEnv = expEnvAt_ prelude (lastExp prelude).val.eid |> Utils.fromJust_ "LangTools.preludeExpEnv"

-- Return bindings to expressions (as best as possible) at EId
expEnvAt : Exp -> EId -> Maybe (Dict.Dict Ident ExpressionBinding)
expEnvAt exp targetEId =
  expEnvAt_ exp targetEId
  |> Maybe.map
      (\bindings -> Dict.union bindings preludeExpEnv)

expEnvAt_ : Exp -> EId -> Maybe (Dict.Dict Ident ExpressionBinding)
expEnvAt_ exp targetEId =
  let recurse e = expEnvAt_ e targetEId in
  let recurseAllChildren () =
    Utils.mapFirstSuccess recurse (childExps exp)
  in
  let addShallowerIdentifiers newIdents deeperBindings =
    newIdents
    |> List.foldl
        (\ident bindings ->
          if Dict.member ident bindings
          then bindings
          else Dict.insert ident BoundUnknown bindings
        )
        deeperBindings
  in
  let addShallowerBoundExps expEnv deeperBindings =
    expEnv
    |> List.foldl
        (\(ident, boundExp) bindings ->
          if Dict.member ident bindings
          then bindings
          else Dict.insert ident (Bound boundExp) bindings
        )
        deeperBindings
  in
  let addBindingsFrom pat e deeperBindings =
    case tryMatchExp pat e of
      Match newBindings -> addShallowerBoundExps newBindings deeperBindings -- tryMatchExp only returns Match if it can bind all idents; no need to worry about partial matches
      _                 -> addShallowerIdentifiers (identifiersListInPat pat) deeperBindings
  in
  if exp.val.eid == targetEId then
    Just Dict.empty
  else
    case exp.val.e__ of
      EConst _ _ _ _   -> Nothing
      EBase _ _        -> Nothing
      EVar _ ident     -> Nothing
      EFun _ ps e _    -> recurse e |> Maybe.map (addShallowerIdentifiers (identifiersListInPats ps))
      EOp _ op es _    -> recurseAllChildren ()
      EList _ es _ m _ -> recurseAllChildren ()
      EIf _ e1 e2 e3 _ -> recurseAllChildren ()
      ECase _ e1 bs _  ->
        case recurse e1 of
          Just bindings ->
            Just bindings -- Found targetEId in scrutinee

          Nothing ->
            bs
            |> List.map .val
            |> Utils.mapFirstSuccess
                (\(Branch_ _ bPat bExp _) -> recurse bExp |> Maybe.map (addBindingsFrom bPat bExp))

      ETypeCase _ scrutinee tbranches _ -> recurseAllChildren ()
      EApp _ e1 es _                    -> recurseAllChildren ()
      ELet _ kind False p e1 e2 _       ->
        case recurse e1 of
          Just bindings ->
            Just bindings -- found targetEId in assigns

          Nothing ->
            recurse e2 |> Maybe.map (addBindingsFrom p e1)

      ELet _ kind True p e1 e2 _ -> recurseAllChildren () |> Maybe.map (addBindingsFrom p e1)
      EComment _ s e1            -> recurse e1
      EOption _ s1 _ s2 e1       -> recurse e1
      ETyp _ pat tipe e _        -> recurse e
      EColonType _ e _ tipe _    -> recurse e
      ETypeAlias _ pat tipe e _  -> recurse e

--------------------------------------------------------------------------------

-- Map a selected argument at a call site to the corresponding pathedPatId in the called function.
-- Returns Nothing if corresponding function is in prelude not the program.
eidToMaybeCorrespondingArgumentPathedPatId : Exp -> EId -> Maybe PathedPatternId
eidToMaybeCorrespondingArgumentPathedPatId program targetEId =
  -- This should be more efficient than running the massive predicate over every expression in the program
  findWithAncestorsByEId program targetEId
  |> Maybe.withDefault []
  |> Utils.mapFirstSuccess
      (\exp ->
        case exp.val.e__ of
          EApp _ appFuncExp argExps _ ->
            case appFuncExp.val.e__ of
              EVar _ funcName ->
                case resolveIdentifierToExp funcName appFuncExp.val.eid program of -- This is probably slow.
                  Just (Bound funcExp) ->
                    case (funcExp.val.e__, isPreludeEId funcExp.val.eid) of
                      (EFun _ fpats _ _, False) ->
                        -- Allow partial application
                        tryMatchExpsPatsToPathsAtFunctionCall fpats argExps
                        |> Utils.mapFirstSuccess
                            (\(path, correspondingExp) ->
                              if correspondingExp.val.eid == targetEId
                              then Just ((funcExp.val.eid, 1), path)
                              else Nothing
                            )

                      _ -> Nothing

                  _ -> Nothing

              _ -> Nothing

          _ -> Nothing
      )
