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

import Lang exposing (..)
import LeoParser
import Utils
import LangUnparser exposing (unparseWithIds)
import Types
import Syntax
import LangUtils exposing (..)

import Dict exposing (Dict)
import Regex
import Set
import Record
import Info

-- For ranking synthesized expressions
nodeCount : Exp -> Int
nodeCount exp =
  let expsNodeCount exps =
     exps |> List.map nodeCount |> List.sum
  in
  case (unwrapExp exp) of
     EConst _ _ _ _           -> 1
     EBase _ _                -> 1
     EVar _ x                 -> 1
     EFun _ ps e _            -> 1 + patsNodeCount ps + nodeCount e
     EOp _ _ op es _          -> 1 + expsNodeCount es
     EList _ es _ mbt _       -> 1 + expsNodeCount (Utils.listValues es) + (Maybe.map (\e -> nodeCount e) mbt |> Maybe.withDefault 0)
     ERecord _ mbi decls _    -> 1 + declCount decls + (Maybe.map (\(init, _) -> nodeCount init) mbi |> Maybe.withDefault 0)
     ESelect _ e _ _ _        -> 1 + nodeCount e
     EIf _ e1 _ e2 _ e3 _     -> 1 + expsNodeCount [e1, e2, e3]
     -- Cases have a set of parens around each branch. I suppose each should count as a node.
     ECase _ e1 bs _          -> 1 + (List.length bs) + nodeCount e1 + patsNodeCount (branchPats bs) + expsNodeCount (branchExps bs)
     EApp _ e1 es _ _         -> 1 + nodeCount e1 + expsNodeCount es
     ELet _ _ decls _ e2      -> 1 + declCount decls  + nodeCount e2
     EColonType _ e1 _ t _    -> 1 + typeNodeCount t + nodeCount e1
     EParens _ e1 pStyle _    -> 1 + nodeCount e1
     EHole _ _                -> 1

declCount_: Bool -> (Pat -> Int) -> (Type -> Int) -> (Exp -> Int) -> Declarations -> Int
declCount_ withChildren patNodeCount typeNodeCount nodeCount (Declarations _ tpes anns exps) =
   (tpes |> elemsOf |> List.map (\(LetType _ _ _ p _ _ t) -> 1 + patNodeCount p + typeNodeCount t) |> List.sum) +
   (anns |> List.map (\(LetAnnotation _ _ p _ _ t) -> 1 + patNodeCount p + typeNodeCount t) |> List.sum) +
   (exps |> elemsOf |> List.map (\(LetExp _ _ p _ _ e) -> 1 + patNodeCount p + if withChildren then nodeCount e else 0) |> List.sum)

declCount: Declarations -> Int
declCount = declCount_ True  patNodeCount typeNodeCount nodeCount

declCountWithoutChildren: Declarations -> Int
declCountWithoutChildren = declCount_ False patNodeCount typeNodeCount nodeCount

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
    (childrenTotal, exp::largeSubExps)
  else
    let thisSizeWithoutChildren =
      case (unwrapExp exp) of
         EConst _ _ _ _           -> 1
         EBase _ _                -> 1
         EVar _ x                 -> 1
         EFun _ ps e _            -> 1 + patsNodeCount ps
         EOp _ _ op es _          -> 1
         EList _ es _ (Just e) _  -> 1
         EList _ es _ Nothing _   -> 1
         ERecord _ _ decls _      -> 1 + declCountWithoutChildren decls
         ESelect _ _ _ _ _        -> 1
         EIf _ e1 _ e2 _ e3 _     -> 1
         -- Cases have a set of parens around each branch. I suppose each should count as a node.
         ECase _ e1 bs _          -> 1 + (List.length bs) + patsNodeCount (branchPats bs)
         EApp _ e1 es _ _         -> 1
         ELet _ _ decls _ e2      -> 1 + declCountWithoutChildren decls
         EColonType _ e1 _ t _    -> 1 + typeNodeCount t
         EParens _ _ _ _          -> 1
         EHole _ _                -> 1
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
--   if min <= 1 then
--     1
--   else
--     case (unwrapExp exp) of
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
--       ETyp _ p t e1 _         -> if 4 >= min then 4 else let ptCount = patNodeCount p + typeNodeCount t in 1 + ptCount + nodeCountAtLeast_ (min - 1 - ptCount) e1
--       EColonType _ e1 _ t _   -> if 3 >= min then 3 else let tCount = typeNodeCount t in 1 + tCount + nodeCountAtLeast_ (min - 1 - tCount) e1
--       ETypeAlias _ p t e1 _   -> if 4 >= min then 4 else let ptCount = patNodeCount p + typeNodeCount t in 1 + ptCount + nodeCountAtLeast_ (min - 1 - ptCount) e1


patNodeCount : Pat -> Int
patNodeCount pat =
  case pat.val.p__ of
    PWildcard _                 -> 1
    PVar _ _ _                  -> 1
    PConst _ _                  -> 1
    PBase _ _                   -> 1
    PList _ pats _ (Just pat) _ -> 1 + patsNodeCount pats + patNodeCount pat
    PList _ pats _ Nothing    _ -> 1 + patsNodeCount pats
    PRecord _ pats _            -> 1 + patsNodeCount (Utils.recordValues pats)
    PAs _ p1 _ p2               -> 1 + patNodeCount p1 + patNodeCount p2
    PParens _ pat _             -> 1 + patNodeCount pat
    PColonType _ pat _ tp       -> 1 + patNodeCount pat + typeNodeCount tp

patsNodeCount : List Pat -> Int
patsNodeCount pats =
  pats |> List.map patNodeCount |> List.sum

typeNodeCount : Type -> Int
typeNodeCount tipe =
  case tipe.val.t__ of
    TNum _                    -> 1
    TBool _                   -> 1
    TString _                 -> 1
    TNull _                   -> 1
    TList _ t _               -> 1 + typeNodeCount t
    TDict _ kt vt _           -> 1 + typeNodeCount kt + typeNodeCount vt
    TTuple _ ts _ Nothing _   -> 1 + typesNodeCount ts
    TTuple _ ts _ (Just t) _  -> 1 + typesNodeCount ts + typeNodeCount t
    TRecord _ Nothing ts _    -> 1 + typesNodeCount (Utils.recordValues ts)
    TRecord _ (Just _) ts _   -> 2 + typesNodeCount (Utils.recordValues ts)
    TArrow _ ts _             -> 1 + typesNodeCount ts
    TUnion _ ts _             -> 1 + typesNodeCount ts
    TApp _ _ ts _             -> 1 + typesNodeCount ts
    TVar _ _                  -> 1
    TForall _ idents t _      -> 1 + List.length idents + typeNodeCount t
    TWildcard _               -> 1
    TParens _ t _             -> 1 + typeNodeCount t

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
    (PAs ws1A p1A ws2A p2A,              PAs ws1B p1B ws2B p2B)              -> patternsEqual p1A p1B && patternsEqual p2A p2B
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
  case ((unwrapExp baseExp), (unwrapExp otherExp)) of
    (EConst ws1A nA locA wdA,              EConst ws1B nB locB wdB)                      -> if nA == nB then [] else [otherExp]
    (EBase ws1A ebvA,                      EBase ws1B ebvB)                              -> if eBaseValsEqual ebvA ebvB then [] else [otherExp]
    (EVar ws1A identA,                     EVar ws1B identB)                             -> if identA == identB then [] else [otherExp]
    (EFun ws1A psA eA ws2A,                EFun ws1B psB eB ws2B)                        -> if patternListsEqual psA psB then extraExpsDiff eA eB else [otherExp]
    (EOp ws1A wsi1 opA esA ws2A,           EOp ws1B wsiB opB esB ws2B)                   -> if opA.val == opB.val then childDiffs () else [otherExp]
    (EList ws1A esA ws2A Nothing ws3A,     EList ws1B esB ws2B Nothing ws3B)             -> childDiffs ()
    (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)           -> childDiffs ()
    (EList ws1A esA ws2A _ ws3A,           EList ws1B esB ws2B _ ws3B)                   -> [otherExp]
    (EApp ws1A fA esA appTypeA ws2A,       EApp ws1B fB esB appTypeB ws2B)               -> childDiffs ()
    (ELet ws1A kindA declsA ws2A e2A,      ELet ws1B kindB declsB ws2B e2B)              -> if patternDeclsEqual declsA declsB then childDiffs () else [otherExp]
    (EIf ws1A e1A _ e2A _ e3A ws2A,        EIf ws1B e1B _ e2B _ e3B ws2B)                -> extraExpsDiff e1A e1B ++ extraExpsDiff e2A e2B ++ extraExpsDiff e3A e3B
    (ECase ws1A eA branchesA ws2A,         ECase ws1B eB branchesB ws2B)                 -> Utils.maybeZip branchesA  branchesB  |> Maybe.andThen (\branchPairs  -> let bValPairs  = branchPairs  |> List.map (\(bA,  bB)  -> (bA.val,  bB.val))  in if bValPairs  |> List.all (\(Branch_  bws1A  bpatA   beA  bws2A,  Branch_  bws1B  bpatB   beB  bws2B)  -> patternsEqual bpatA bpatB)  then  Just (childDiffs ()) else Nothing) |> Maybe.withDefault [otherExp]
    (EColonType ws1A eA ws2A typeA ws3A,   EColonType ws1B eB ws2B typeB ws3B)           -> if Types.equal typeA typeB then extraExpsDiff eA eB else [otherExp]
    (EParens ws1A e1A pStyleA ws2A,        EParens ws1B e1B pStyleB ws2B)                -> extraExpsDiff e1A e1B
    (EParens ws1A e1A pStyleA ws2A,        _)                                            -> extraExpsDiff e1A otherExp
    (_,                                    EParens ws1B e1B pStyleB ws2B)                -> extraExpsDiff baseExp e1B
    (ERecord ws1A Nothing declsA ws2A,     ERecord ws1B Nothing declsB ws2B)             -> if patternDeclsEqual declsA declsB then childDiffs () else [otherExp]
    (ERecord ws1A (Just _) declsA ws2A,    ERecord ws1B (Just _) declsB ws2B)            -> if patternDeclsEqual declsA declsB then childDiffs () else [otherExp]
    (ERecord _ _ _ _,                      ERecord _ _ _ _)                              -> [otherExp]
    (_, _) -> [otherExp]

patternDeclsEqual: Declarations -> Declarations -> Bool
patternDeclsEqual (Declarations _ tpsA annsA expsA) (Declarations _ tpsB annsB expsB) =
  List.length tpsA == List.length tpsB &&
  List.length annsA == List.length annsB &&
  List.length expsA == List.length expsB &&
  List.all (\(LetExp _ _ pA _ _ _, LetExp _ _ pB _ _ _) -> patternsEqual pA pB) (Utils.zip (elemsOf expsA) (elemsOf expsB)) &&
  List.all (\(LetType _ _ _ pA _ _ _, LetType _ _ _ pB _ _ _) -> patternsEqual pA pB) (Utils.zip (elemsOf tpsA) (elemsOf tpsB)) &&
  List.all (\(LetAnnotation _ _ pA _ _ _, LetAnnotation _ _ pB _ _ _) -> patternsEqual pA pB) (Utils.zip annsA annsB)

replaceConstsWithVars : Dict LocId Ident -> Exp -> Exp
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
  |> List.filter (\((locId, annotation, _), n) -> annotation /= "!" && not (LeoParser.isPreludeLocId locId))
  |> List.map (\((locId, _, _), n) -> (locId, n))


-- These should use syncOptions
-- Or we should remove the frozen by default config option
frozenLocIdsAndNumbers : Exp -> List (LocId, Num)
frozenLocIdsAndNumbers exp =
  allLocsAndNumbers exp
  |> List.filter (\((locId, annotation, _), n) -> annotation == "!" || LeoParser.isPreludeLocId locId)
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

allLocIds: Exp -> List LocId
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
expToLocation (Expr exp) =
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
  if eid == (expEId program) then
    True
  else
    case maybeTopLevelChild program of
      Just child -> isTopLevelEId eid child
      Nothing    -> False


maybeTopLevelChild : Exp -> Maybe Exp
maybeTopLevelChild exp =
  case (unwrapExp exp) of
    ELet _ _ _ _ body -> Just body
    _                 -> Nothing


topLevelExps : Exp -> List Exp
topLevelExps program =
  case maybeTopLevelChild program of
    Just child -> program::(topLevelExps child)
    Nothing    -> [program]


lastTopLevelExp : Exp -> Exp
lastTopLevelExp exp = maybeTopLevelChild exp |> Maybe.map lastTopLevelExp |> Maybe.withDefault exp


lastExp : Exp -> Exp
lastExp exp =
  case childExps exp |> List.reverse of
    []           -> exp
    lastChild::_ -> lastExp lastChild


-- Find outermost expression that resolves to the same value.
outerSameValueExp : Exp -> Exp -> Exp
outerSameValueExp program targetExp =
  let targetEId = expEffectiveExp targetExp |> expEId in
  program
  |> findFirstNode (expEffectiveExp >> \exp -> (expEId exp) == targetEId)
  |> Maybe.withDefault targetExp


outerSameValueExpByEId : Exp -> EId -> Exp
outerSameValueExpByEId program targetEId =
  outerSameValueExp program (justFindExpByEId program targetEId)


copyListWhitespace : Exp -> Exp -> Exp
copyListWhitespace templateList list =
  case ((unwrapExp templateList), (unwrapExp list)) of
    (EList ws1 _ ws2 _ ws3, EList _ heads _ maybeTail _) ->
      replaceE__ list (EList ws1 heads ws2 maybeTail ws3)

    _ ->
      Debug.crash <| "Lang.copyListWs expected lists, but given " ++ unparseWithIds templateList ++ " and " ++ unparseWithIds list

longLineLength = 50 -- Not precisely 50, but roughly so. (using unparseWithUniformWhitespace to ensure convergence in one step; also don't count length of initial "(let ")

-- O(n^2) if applied recursively to children
-- O(n) if used once
--- TODO: This needs reworking.
reflowLetWhitespace : Exp -> Exp -> Exp
reflowLetWhitespace program letExp =
  let _ = Debug.log "Please update LangTools.reflowLetWhitespace to execute this command" () in
  letExp
  {-case (unwrapExp letExp) of
    ELet oldLetWs letKind isRec pat wsBeforeEq boundExp wsBeforeIn body ws2 ->
      let oldIndentation = indentationOf letExp in
      let oneOrTwoNewlinesBeforeLet =
        if newlineCount oldLetWs.val <= 1
        then "\n"
        else "\n\n"
      in
      case (unwrapExp boundExp) of
        EFun fws1 fpats fbody fws2 ->
          let multipleLinesForFunction =
            String.contains "\n" (LangUnparser.unparse fbody)
            || longLineLength < String.length (LangUnparser.unparsePatWithUniformWhitespace True pat ++ LangUnparser.unparseWithUniformWhitespace True True boundExp)
          in
          let newLineForFunctionArgs =
            longLineLength < String.length (LangUnparser.unparsePatWithUniformWhitespace True pat ++ String.join " " (List.map (LangUnparser.unparsePatWithUniformWhitespace True) fpats))
          in
          let (letWs_, funcWs_, fbodyWs_, newFBody, bodyMinimalNewlineCount) =
            if isTopLevelEId (expEId letExp) program then
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
                wsBeforeEq
                newFunc
                wsBeforeIn
                (ensureNNewlinesExp bodyMinimalNewlineCount (extractIndentation letWs_) body)
                space0

        _ ->
          let minimalSurroundingNewlineCount =
            if String.contains "\n" (LangUnparser.unparse boundExp)
            then 2
            else 1
          in
          let newlinesBefore =
            if (expEId letExp) == (expEId program) -- First expression in a program does not need a newline.
            then 0
            else minimalSurroundingNewlineCount
          in
          replaceE__ letExp <|
            ELet
                (ws <| ensureNNewlines newlinesBefore oldLetWs.val oldLetWs.val)
                letKind
                isRec
                (replacePrecedingWhitespacePat " " pat)
                wsBeforeEq
                boundExp
                wsBeforeIn
                (ensureNNewlinesExp minimalSurroundingNewlineCount (extractIndentation oldLetWs.val) body)
                space0

          -- let addNewLineForBoundExp =
          --   (not <| String.contains "\n" (precedingWhitespace boundExp))
          --   && longLineLength < String.length (LangUnparser.unparsePatWithUniformWhitespace True pat ++ LangUnparser.unparseWithUniformWhitespace True True boundExp)
          -- in
          -- let (letWs, boundExpWs, bodyMinimalNewlineCount) =
          --   if isTopLevelEId (expEId letExp) program then
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
   -}

getProperIndentationIfBody : Exp -> Exp -> Maybe String
getProperIndentationIfBody root e =
  let
    eId = expEId e
    indentationAtParent e parent mbBranch =
      Just <|
        case (unwrapExp parent, mbBranch) of
          (ELet _ _ _ _ _, _) -> indentationAt (expEId parent) root
          (_, Just branch) -> indentationOfBranch branch ++ "  "
          _ -> indentationAt (expEId parent) root ++ "  "
  in
  performActionIfBody root e indentationAtParent (always Nothing) Nothing

-- Note: the isRec flag is ignored
newLetFancyWhitespace : EId ->         Bool -> Pat -> Exp ->   Exp ->    Exp -> Exp
newLetFancyWhitespace   insertedLetEId isRec   pat    boundExp expToWrap program =
--newLetFancyWhitespace : EId ->         Bool -> List (Pat, Exp) ->   Exp ->    Exp -> Exp
--newLetFancyWhitespace   insertedLetEId isRec   patBoundExps         expToWrap program =
  let toWrapEId = expEId expToWrap in
  let isTopLevel = isTopLevelEId toWrapEId program in
  let letOrDef = if isTopLevel then Def else Let in
  let newLetIndentation =
    getProperIndentationIfBody program expToWrap |>
      Maybe.withDefault (indentationAt toWrapEId program)
  in
  -- TODO This seems unnecessary - we really only blank newlines before the let, not between the let and its decls
  let newlineCountAfterLet =
    let newlinesBeforeWrapped = newlineCount <| precedingWhitespace expToWrap in
    if isTopLevel || newlinesBeforeWrapped >= 2
    then 2
    else 1
  in
  let newlineCountBeforeLet =
    if toWrapEId == expEId program then 1 else newlineCountAfterLet
  in
  let expToWrapWithNewWs =
    let wrappedExpIndent = if isLet expToWrap || isTopLevel then "" else "  " in
    -- TODO both branches of this if do nearly the same thing
    if patHasNewlines pat || expHasNewlines boundExp
    then expToWrap |> ensureWhitespaceNNewlinesExp newlineCountAfterLet |> replaceIndentation wrappedExpIndent
    else expToWrap |> ensureWhitespaceSmartExp newlineCountAfterLet wrappedExpIndent
  in
  eLet__ space0 letOrDef isRec (
    (if isTopLevel then ensureNoWhitespacePat else ensureWhitespacePat) pat) space1 (replaceIndentation "  " boundExp |> ensureWhitespaceExp) space1 expToWrapWithNewWs space0
  |> withDummyExpInfoEId insertedLetEId
  |> Expr
  |> replacePrecedingWhitespace (String.repeat newlineCountBeforeLet "\n")
  |> indentExp newLetIndentation

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
  case unwrapExp <| expEffectiveExp exp of
    EConst _ _ _ _        -> "num"
    EVar _ ident          -> ident
    EApp _ funE _ _ _     -> expToMaybeIdent funE |> Maybe.withDefault default
    EList _ _ _ _ _       -> "list"
    ERecord _ _ _ _       -> "record"
    EOp _ _ _ es _          -> List.map (simpleExpNameWithDefault default) es |> Utils.findFirst ((/=) default) |> Maybe.withDefault default
    EBase _ ENull         -> "null"
    EBase _ (EString _ _) -> "string"
    EBase _ (EBool _)     -> "bool"
    EFun _ _ _ _          -> "func"
    EHole _ _             -> "hole"
    _                     -> default

-- Suggest a name for the expression at eid in program
expNameForEId : Exp -> EId -> String
expNameForEId program targetEId =
  expNameForEIdWithDefault defaultExpName program targetEId

expNameForEIdWithDefault : String -> Exp -> EId -> String
expNameForEIdWithDefault default program targetEId =
  -- if the target is part of a tuple binding, get the name it's bound to
  let getParent eId = parentByEId program eId |> Maybe.withDefault Nothing in
  getParent targetEId
                             |> Maybe.andThen (\parentExp ->
  expEId parentExp
                             |>               (\parentEId ->
  eTupleUnapply parentExp
                             |> Maybe.andThen (\(_, tupleChildrenWithWS, _) ->
  getParent parentEId
                             |> Maybe.andThen (\grandParentExp ->
  eLetUnapply grandParentExp
                             |> Maybe.andThen (\(_, _, (Declarations _ _ _ letExps), _, _) ->
  Utils.findFirst
    (bindingOfLetExp >> eidIs parentEId)
    (elemsOf letExps)
                             |> Maybe.andThen (\letExp ->
  patOfLetExp letExp
  |> pTupleUnapply
                             |> Maybe.andThen (\(_, patsWithWS, _) ->
  Utils.maybeZip
    (List.map Tuple.second patsWithWS)
    (List.map Tuple.second tupleChildrenWithWS)
                             |> Maybe.andThen (\patsWithExps ->
  Utils.findFirst
    (Tuple.second >> eidIs targetEId)
    patsWithExps
                             |> Maybe.andThen (\(pat, _) ->
  pVarUnapply pat

  -- else
  ))))))))) |> Maybe.withDefault (
  case expDescriptionParts program targetEId |> Utils.takeLast 1 of
    [name] ->
      name

    _ ->
      -- Should only hit this branch if exp not found, so really this
      -- will always return defaultExpName
      findExpByEId program targetEId
      |> Maybe.map (simpleExpNameWithDefault default)
      |> Maybe.withDefault default
  )


-- Suggest a name for the expression exp in program
expNameForExp : Exp -> Exp -> String
expNameForExp program exp =
  expNameForExpWithDefault defaultExpName program exp

expNameForExpWithDefault : String -> Exp -> Exp -> String
expNameForExpWithDefault default program exp =
  case expDescriptionParts program (expEId exp) |> Utils.takeLast 1 of
    [name] ->
      name

    _ ->
      simpleExpNameWithDefault default exp

renamePatVar: String -> String -> Pat -> Pat
renamePatVar oldName newName pat =
  mapPat (\p -> case p.val.p__ of
    PVar ws0 name c ->
      if name == oldName then replaceP__ p <| PVar ws0 newName c else p
    _ -> p) pat

commonNameForEIds : Exp -> List EId -> String
commonNameForEIds program eids =
  commonNameForEIdsWithDefault defaultExpName program eids


leadingDigits   = Regex.regex "^[0-9]+"
trailingDigits  = Regex.regex "[0-9]+$"
leadingCapitals = Regex.regex "^[A-Z]+"

removeLeadingDigits : String -> String
removeLeadingDigits string =
  Regex.replace (Regex.AtMost 1) leadingDigits (\_ -> "") string

downcaseLeadingCapitals : String -> String
downcaseLeadingCapitals string =
  Regex.replace (Regex.AtMost 1) leadingCapitals (\{match} -> String.toLower match) string

removeTrailingDigits : String -> String
removeTrailingDigits string =
  Regex.replace (Regex.AtMost 1) trailingDigits (\_ -> "") string


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
  if (expEId exp) == targetEId then
    [simpleExpName exp]
  else
    case unwrapExp exp of
      ELet _ _ (Declarations _ _ [] [(_, [LetExp _ _ pat _ _ assigns])]) _ body ->
        let namedAssigns = tryMatchExpReturningList pat assigns in
        case List.filter (\(ident, e) -> findExpByEId e targetEId /= Nothing) namedAssigns of
          [] ->
            if (expEId assigns) == targetEId then
              -- e.g. want whole list, but the let only bound the individual list elements
              case identifiersListInPat pat of
                []        -> [simpleExpName assigns]
                pathedPatIdents -> [varIdentOrDefault assigns (String.join "" pathedPatIdents)]
            else
              let scopeNames =
                case pat.val.p__ of
                   PVar _ ident _  -> [ident]
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
            if (expEId matchingExp) == targetEId then
              (Utils.dropLast 1 idents) ++ [varIdentOrDefault matchingExp ident]
            else
              case recurse matchingExp of
                []          -> Debug.crash <| "LangTools.expDescriptionParts expected to find targetEId in\n" ++ unparseWithIds matchingExp ++ "\nin\n" ++ unparseWithIds assigns
                deeperParts -> idents ++ deeperParts

      -- Try to use name of the function argument the expression is bound to.
      EApp ws1 fExp es appType ws2 ->
        -- Probably faster to first check for target in es (which will usually fail); avoids searching for binding of fExp
        case searchChildren () of
          [] -> []
          childrenResult ->
            expToMaybeIdent fExp
            |> Maybe.andThen
                (\funcName ->
                  case resolveIdentifierToExp funcName targetEId program of
                    Just (Bound boundExp) ->
                      case (unwrapExp boundExp) of
                        EFun _ pats _ _ ->
                          Utils.zip pats es
                          |> Utils.mapFirstSuccess
                              (\(pat, e) ->
                                case tryMatchExp pat e of
                                  Match bindings ->
                                    -- Not quite as complicated as the Let logic above; should be okay though.
                                    bindings
                                    |> Utils.findFirst (\(ident, e) -> (expEId e) == targetEId)
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
    Nothing ->
      []

    Just scopeNames ->
      -- Last element may be the original identifier: if so, remove it.
      if (Utils.maybeLast scopeNames) == Just ident
      then Utils.removeLastElement scopeNames
      else scopeNames

scopeNamesLocLiftedThrough_ : LocId -> List Ident -> Exp -> Maybe (List Ident)
scopeNamesLocLiftedThrough_ targetLocId scopeNames exp =
  case unwrapExp exp of
    ELet _ _ (Declarations _ _ [] [(_, [LetExp _ _ pat _ _ assigns])]) _ body -> -- TODO: More ELet
      let scopeNames__ pat =
        case pat.val.p__ of
           PVar _ ident _  -> [ident]
           PAs _ p1 _ p2   -> scopeNames__ p1 ++ scopeNames__ p2
           _               -> []
      in
      let scopeNames_ = scopeNames ++ scopeNames__ pat in
      -- Ident should only be added to assigns. Otherwise you get lots of junk
      -- names.
      Utils.firstMaybe
          [ scopeNamesLocLiftedThrough_ targetLocId scopeNames_ assigns
          , scopeNamesLocLiftedThrough_ targetLocId scopeNames  body
          ]

    EConst _ _ (locId, _, _) _ ->
       if locId == targetLocId
       then Just scopeNames
       else Nothing

    _ ->
      let recurse exp = scopeNamesLocLiftedThrough_ targetLocId scopeNames exp in
      Utils.mapFirstSuccess recurse (childExps exp)


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
--
-- TODO: perhaps rewrite in terms of tryMatchExpPatToSomething
tryMatchExp : Pat -> Exp -> ExpMatchResult
tryMatchExp pat exp =
  let matchMap f matchResult =
    case matchResult of
       Match env -> Match (f env)
       _         -> matchResult
  in
  let matchAndThen f matchResult =
    case matchResult of
       Match env -> f env
       _ -> matchResult
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
  -- case (unwrapExp exp) of
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
  case (unwrapExp exp) of
    EColonType _ typedExp _ _ _ ->
      tryMatchExp pat typedExp

    _ ->
      case pat.val.p__ of
        PWildcard _            -> Match []
        PVar _ ident _         -> Match [(ident, exp)]
        PAs _ innerPat1 _ innerPat2 ->
          tryMatchExp innerPat1 exp
          |> matchAndThen (\env ->
            tryMatchExp innerPat2 exp |>
             matchMap (\env2 ->
               env2 ++ env))

        PList _ ps _ Nothing _ ->
          case (unwrapExp exp) of
            -- TODO: list must not have rest
            EList _ es _ Nothing _ ->
              case Utils.maybeZip ps (Utils.listValues es) of
                Nothing    -> NoMatch
                Just pairs ->
                  List.map (\(p, e) -> tryMatchExp p e) pairs
                  |> projMatches

            _ ->
              CannotCompare

        PList _ ps _ (Just restPat) _ ->
          case (unwrapExp exp) of
            EList _ es _ Nothing _ ->
              if List.length es < List.length ps then
                NoMatch
              else
                let (headExps, tailExps) = Utils.split (List.length ps) (Utils.listValues es) in
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
                  Utils.zip ps (Utils.listValues es)
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
        PRecord _ ps _ ->
          case (unwrapExp exp) of
            ERecord _ Nothing decls _ ->
              case recordEntriesFromDeclarations decls of
                Just es ->
                  let psEsMaybe = Record.getPatternMatch (Utils.recordKey) (Utils.recordKey) ps es in
                  case psEsMaybe of
                     Nothing -> NoMatch
                     Just psEs -> psEs
                      |> List.map (\(p, e) -> tryMatchExp (Utils.recordValue p) (Utils.recordValue e))
                      |> projMatches
                Nothing -> CannotCompare
            _ ->
              CannotCompare

        PConst _ n ->
          case (unwrapExp exp) of
            EConst _ num _ _ -> if n == num then Match [] else NoMatch
            _                -> CannotCompare

        PBase _ bv ->
          case (unwrapExp exp) of
            EBase _ ev -> if eBaseValsEqual bv ev then Match [] else NoMatch
            _          -> CannotCompare

        PParens _ innerPat _  ->
          tryMatchExp innerPat exp

        PColonType _ innerPat _ _ ->
          tryMatchExp innerPat exp

deepestCommonAncestorWithNewlineOrELet : Exp -> (Exp -> Bool) -> Exp
deepestCommonAncestorWithNewlineOrELet program pred =
  commonAncestors pred program
  |> List.reverse
  |> Utils.findFirst (\e -> (precedingWhitespace e |> String.contains "\n") || eLetUnapply e /= Nothing)
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
            (\expToWrap -> newLetFancyWhitespace -1 False pat boundExp expToWrap program)
      )
      program


addFirstDef : Exp -> Pat -> Exp -> Exp
addFirstDef program pat boundExp =
  program
  |> mapExpNode
      (expEId program)
      (\nonComment ->
        newLetFancyWhitespace -1 False pat boundExp nonComment program
      )

-- if e is the "body" of its parent, returns `action e (parent e) mbBranch`, otherwise returns default
performActionIfBody : Exp -> Exp -> (Exp -> Exp -> Maybe Branch -> r) -> (Exp -> r) -> r -> r
performActionIfBody root e action actionForRoot default =
  let eId = expEId e in
  case parentByEId root eId of
    Just (Just parent) ->
      let return bodies mbBranch =
        if List.member eId <| List.map expEId bodies then
          action e parent mbBranch
        else
          default
      in
      case unwrapExp parent of
        EFun _ _ body _ ->
          return [body] Nothing
        EIf _ _ _ tExp _ fExp _ ->
          return [tExp, fExp] Nothing
        ECase _ _ branches _ ->
          let mbBranch =
            branches |>
              Utils.mapFirstSuccess (\branch ->
                if eId == expEId (branchExp branch) then
                  Just branch
                else
                  Nothing
              )
          in
          case mbBranch of
            Nothing -> default
            Just _ -> action e parent mbBranch
        ELet _ _ _ _ body ->
          return [body] Nothing
        _ ->
          default
    Just Nothing ->
      actionForRoot root
    _ ->
      default


expToMaybeNum : Exp -> Maybe Num
expToMaybeNum exp =
  case (unwrapExp exp) of
    EConst _ n _ _ -> Just n
    _              -> Nothing


expToMaybeVar : Exp -> Maybe Exp
expToMaybeVar exp =
  case (unwrapExp exp) of
    EVar _ _ -> Just exp
    _        -> Nothing


expToIdent : Exp -> Ident
expToIdent exp =
  case (unwrapExp exp) of
    EVar _ ident -> ident
    _            -> Debug.crash <| "LangTools.expToIdent exp is not an EVar: " ++ unparseWithIds exp


patToMaybeIdent : Pat -> Maybe Ident
patToMaybeIdent pat =
  case pat.val.p__ of
    PVar _ ident _  -> Just ident
    PAs _ p1 _ p2   -> patToMaybeIdent p1 |> Utils.maybeOrElseLazy (\_ -> patToMaybeIdent p2)
    PParens _ p _   -> patToMaybeIdent p
    PColonType _ p _ _ -> patToMaybeIdent p
    _               -> Nothing


patToMaybePVarIdent : Pat -> Maybe Ident
patToMaybePVarIdent pat =
  case pat.val.p__ of
    PVar _ ident _ -> Just ident
    _              -> Nothing


expToListParts : Exp -> (WS, List Exp, WS, Maybe Exp, WS)
expToListParts exp =
  case (unwrapExp exp) of
    EList ws1 heads ws2 maybeTail ws3 -> (ws1, Utils.listValues heads, ws2, maybeTail, ws3)
    _                                 -> Debug.crash <| "LangTools.expToListParts exp is not an EList: " ++ unparseWithIds exp

type alias SingleLetPart
  = (WS, LetKind, WS, Pat, FunArgStyle, WS, Exp, WS, Exp)

expToLetParts : Exp -> SingleLetPart
expToLetParts exp =
  case expToMaybeLetParts exp of
    Just lp -> lp
    _       -> Debug.crash <| "LangTools.expToLetParts exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetParts : Exp -> Maybe SingleLetPart
expToMaybeLetParts exp =
  case unwrapExp exp of
    ELet ws1 letKind (Declarations _ _ _ [(_, [LetExp Nothing wsP p1 fs ws2 e1])]) ws3 e2->
      Just (ws1, letKind, wsP, p1, fs, ws2, e1, ws3, e2)
    _                                         -> Nothing


expToLetKind : Exp -> LetKind
expToLetKind exp =
  case (unwrapExp exp) of
    ELet _ lk _ _ _ -> lk
    _                   -> Debug.crash <| "LangTools.expToLetKind exp is not an ELet: " ++ unparseWithIds exp


expToLetPat : Exp -> List Pat
expToLetPat exp =
  case expToMaybeLetPat exp of
    Just pat-> pat
    _       -> Debug.crash <| "LangTools.expToLetPat exp is not an ELet: " ++ unparseWithIds exp

expToMaybeLetPat : Exp -> Maybe (List Pat)
expToMaybeLetPat exp =
  case unwrapExp exp of
    ELet _ _ (Declarations _ _ _ letexpsGroups) _ _ ->
      Just (letexpsGroups |> elemsOf |> List.map (\(LetExp _ _ p _ _ _) -> p))
    _ -> Nothing



expToLetBoundExp : Exp -> List Exp
expToLetBoundExp exp =
  case expToMaybeLetBoundExp exp of
    Just boundExp -> boundExp
    _             -> Debug.crash <| "LangTools.expToLetPat exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetBoundExp : Exp -> Maybe (List Exp)
expToMaybeLetBoundExp exp =
  case unwrapExp exp of
    ELet _ _ (Declarations _ _ _ letexpsGroups) _ _ ->
      Just (letexpsGroups |> elemsOf |> List.map (\(LetExp _ _ _ _ _ boundExp) -> boundExp))
    _                             -> Nothing

expToLetPatAndBoundExp : Exp -> List (Pat, Exp)
expToLetPatAndBoundExp exp =
  case expToMaybeLetPatAndBoundExp exp of
    Just x -> x
    _  -> Debug.crash <| "LangTools.expToLetPatAndBoundExp exp is not an ELet: " ++ unparseWithIds exp

expToMaybeLetPatAndBoundExp : Exp -> Maybe (List (Pat, Exp))
expToMaybeLetPatAndBoundExp exp =
  case unwrapExp exp of
    ELet _ _ (Declarations _ _ _ letexpsGroups) _ _ ->
      Just (letexpsGroups |> elemsOf |> List.map (\(LetExp _ _ pat _ _ boundExp) -> (pat, boundExp)))
    _                               -> Nothing

expToLetBody : Exp -> Exp
expToLetBody exp =
  case expToMaybeLetBody exp of
    Just body -> body
    _         -> Debug.crash <| "LangTools.expToLetBody exp is not an ELet: " ++ unparseWithIds exp


expToMaybeLetBody : Exp -> Maybe Exp
expToMaybeLetBody exp =
  case (unwrapExp exp) of
    ELet _ _ _ _ body -> Just body
    _                 -> Nothing

expToFuncPats : Exp -> List Pat
expToFuncPats exp =
  case (unwrapExp exp) of
    EFun _ pats _ _ -> pats
    _               -> Debug.crash <| "LangTools.expToFuncPats exp is not an EFun: " ++ unparseWithIds exp


expToCaseScrutinee : Exp -> Exp
expToCaseScrutinee exp =
  case (unwrapExp exp) of
    ECase _ scrutinee _ _ -> scrutinee
    _                     -> Debug.crash <| "LangTools.expToScrutinee exp is not an ECase: " ++ unparseWithIds exp


expToMaybeSnapHoleVal : Exp -> Maybe Val
expToMaybeSnapHoleVal exp =
  case (unwrapExp exp) of
    EHole _ (ESnapHole val) -> Just val
    _                       -> Nothing

-- This is a rather generous definition of literal.
isLiteral : Exp -> Bool
isLiteral exp =
  Set.size (freeIdentifiers exp) == 0


-------------------------------------------------------------
-- Identifier/scoping/binding functions

allPats : Exp -> List Pat
allPats root =
  flattenExpTree root
  |> List.concatMap
      (\exp ->
        case (unwrapExp exp) of
          EFun _ pats _ _          -> pats
          ECase _ _ branches _     -> branchPats branches
          ELet _ _ decls _ _       -> declarationsPats decls
          _                        -> []
      )

declarationsPats: Declarations -> List Pat
declarationsPats (Declarations _ tpes anns exps) =
  (tpes |> elemsOf |> List.map (\(LetType _ _ _ p _ _ _) -> p)) ++
  (anns |> List.map (\(LetAnnotation _ _ p _ _ _) -> p)) ++
  (exps |> elemsOf |> List.map (\(LetExp _ _ p _ _ _) -> p))

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

       ELet _ _ decls _ _ ->
         (declarationsPats decls |> List.concatMap identifiersListInPat) ++ acc

       _ ->
         acc
  in
  foldExpViaE__
    folder
    []
    exp


identifierCounts : Exp -> Dict Ident Int
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
  if not <| Utils.anyOverlapListSet plainSuggestedNames existingNames then
    plainSuggestedNames
  else
    let newNames =
      suggestedNames
      |> List.map
          (\name ->
            if String.contains "{n}" name then
              Utils.stringReplace "{n}" (toString i) name
            else
              name ++ toString i
          )
    in
    if not <| Utils.anyOverlapListSet newNames existingNames
    then newNames
    else nonCollidingNames suggestedNames (i+1) existingNames

renameIdentifiersInDecls subst (Declarations go types anns exps) =
  Declarations go
    (types |> elemsOf |> List.map (\(LetType a b c p d e f) -> LetType a b c (renameIdentifiersInPat subst p) d e f) |> regroup types)
    (anns |> List.map (\(LetAnnotation b c p d e f) -> LetAnnotation b c (renameIdentifiersInPat subst p) d e f))
    (exps |> elemsOf |> List.map (\(LetExp b c p d e f) -> LetExp b c (renameIdentifiersInPat subst p) d e f) |> regroup exps)

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

       PAs ws1 p1 ws2 p2 ->
         PAs ws1 (recurse p1) ws2 (recurse p2)

       PColonType ws1 p ws2 typ ->
         PColonType ws1 (recurse p) ws2 typ

       PRecord ws1 kv ws2 ->
         PRecord ws1 (Utils.recordValuesMap recurse kv) ws2

       PParens ws1 p ws2 ->
         PParens ws1 (recurse p) ws2

       PWildcard ws1 -> pat.val.p__

       PBase _ _ -> pat.val.p__

       PConst _ _ -> pat.val.p__
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


renameIdentifiers : Dict Ident Ident -> Exp -> Exp
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

      ELet ws1 kind decls spIn body ->
        ELet ws1 kind (renameIdentifiersInDecls subst decls) spIn body

      _ ->
        e__
  in
  mapExpViaExp__
    exp__Renamer
    exp



setPatName : PathedPatternId -> Ident -> Exp -> Exp
setPatName ((scopeEId, branchI), path) newName exp =
  let _ = Debug.log "setPatName" () in
  let maybeScopeExp = findExpByEId exp scopeEId in
  let maybeNewScopeExp =
    let makeNewScope e__ = replaceE__ (Utils.fromJust_ "setPatName" maybeScopeExp) e__ in
    case Maybe.map unwrapExp maybeScopeExp of
       Just (ELet ws1 letKind decls ws3 body)->
          let newDecls = decls |>
            mapDeclarations (\index def ->
               if index == branchI then
                case def of
                  DeclExp (LetExp wsc wsb pat fs wse boundExp) ->
                    let newPat = setPatNameInPat path newName pat in
                    DeclExp (LetExp wsc wsb newPat fs wse boundExp)
                  _ -> def
               else def
            )
          in
          Just <| makeNewScope (ELet ws1 letKind newDecls ws3 body)

       Just (EFun ws1 pats body ws2) ->
        Utils.maybeGeti0 branchI pats
        |> Maybe.map
            (\pat ->
              let newPat = setPatNameInPat path newName pat in
              makeNewScope (EFun ws1 (Utils.replacei0 branchI newPat pats) body ws2)
            )

       Just (ECase wsb scrutinee branches wsa) ->
        Utils.maybeGeti0 branchI branches
        |> Maybe.map
            (\branch ->
              let (Branch_ ws1 pat exp ws2) = branch.val in
              let newPat = setPatNameInPat path newName pat in
              let newBranch = { branch | val = Branch_ ws1 newPat exp ws2 } in
              makeNewScope (ECase wsb scrutinee (Utils.replacei0 branchI newBranch branches) wsa)
            )

       _ ->
        Nothing
  in
  case maybeNewScopeExp of
    Just newScopeExp -> replaceExpNode (expEId newScopeExp) newScopeExp exp
    Nothing          -> exp


setPatNameInPat : List Int -> Ident -> Pat -> Pat
setPatNameInPat path newName pat =
  case (pat.val.p__, path) of
    (PVar ws ident wd, []) ->
      replaceP__ pat (PVar ws newName wd)

    (PWildcard wsb, []) ->
      replaceP__ pat (PVar wsb newName <| Info.withDummyInfo NoWidgetDecl)

    (PAs ws1 p1 ws2 p2, 1::is) ->
      replaceP__ pat (PAs ws1 (setPatNameInPat is newName p1) ws2 p1)

    (PAs ws1 p1 ws2 p2, 2::is) ->
      replaceP__ pat (PAs ws1 p1 ws2 (setPatNameInPat is newName p2))

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

    (PRecord wsb fields wsa, i::is) ->
      if i <= List.length fields then
        let
          newFields =
            fields |> Utils.getReplacei1 i (\(mws, ws1, oldIdent, ws2, fieldPat) ->
              let newFieldPat = setPatNameInPat is newName fieldPat in
              (mws, ws1, oldIdent, ws2, newFieldPat)
            )
        in
        replaceP__ pat (PRecord wsb newFields wsa)
      else
        pat

    _ ->
      pat

-- Produce an expression that, if used just after the pat, references what the pat binds.
-- This does not work for records or wildcards
patToExp : Pat -> Maybe Exp
patToExp pat =
  Maybe.map withDummyExpInfo <|
  case pat.val.p__ of
    PVar _ ident _                    -> Just <| EVar space1 ident
    PAs _ p1 _ p2                     -> patToExp p1 |> Utils.maybeOrElseLazy (\_ -> patToExp p2) |> Maybe.map unwrapExp
    PList ws1 heads ws2 maybeTail ws3 ->
      case List.map patToExp heads |> Utils.projJusts of
        Nothing -> Nothing
        Just headExps -> case maybeTail of
          Just tail -> case patToExp tail of
            Just tailExp -> Just <| EList ws1 (List.map ((,) space0) headExps) ws2 (Just tailExp) ws3
            Nothing -> Nothing
          Nothing -> Just <| EList ws1 (List.map ((,) space0) headExps) ws2 Nothing ws3
    PConst ws1 n                      -> Just <| EConst ws1 n dummyLoc noWidgetDecl
    PBase ws1 bv                      -> Just <| EBase ws1 bv
    PParens ws1 p ws2                 -> patToExp p |> Maybe.map unwrapExp
    PWildcard ws1                     -> Nothing
    PRecord ws1 ps ws2                -> Nothing
    PColonType _ p _ _                -> patToExp p |> Maybe.map unwrapExp

-- Return the first expression(s) that can see the bound variables.
-- Returns [] if cannot find scope; recursive definitions returns the bound expressions as well.
findScopeAreas : ScopeId -> Exp -> String -> List Exp
findScopeAreas (scopeEId, branchI) exp oldName =
  let maybeScopeExp = findExpByEId exp scopeEId in
  case Maybe.map unwrapExp maybeScopeExp of
    Just (ELet _ _ (Declarations _ _ _ exps as decls) _ body) ->
      let offset = Lang.startBindingNumLetExp decls in
      let (res, _, found) = foldLeftGroup ([], 0, False) exps <|
        \(acc, declsBefore, canSeeBoundVariables) group isRec ->
           let maybeShadowingIdentifiers = groupIdentifiers group in
           let isShadowing = List.member oldName maybeShadowingIdentifiers in
           let newDeclsBefore = declsBefore + List.length group in
           if canSeeBoundVariables then
             (if not isShadowing || not isRec then acc ++ groupBoundExps group else acc, newDeclsBefore, not isShadowing)
           else
           let idents = groupIdentifiers group in
           if declsBefore <= (branchI - offset) && (branchI - offset) < newDeclsBefore then
             if isRec then
               (if not isShadowing || not isRec then acc ++ groupBoundExps group else acc, newDeclsBefore, True)
             else
               (acc, newDeclsBefore, True)
           else
             (acc, newDeclsBefore, False)
      in
      res ++ [body]

    Just (EFun _ pats body _) ->
      [body]

    Just (ECase _ _ branches _) ->
      Utils.maybeGeti0 branchI (branchExps branches)
      |> Maybe.map (\branch -> [branch])
      |> Maybe.withDefault []

    x ->
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
        case unwrapExp e of
          ELet _ _ (Declarations  _ _ _ exps) _ e2 ->
            let (res, found) = foldLeftGroup ([], False) exps <|
              \(acc, canSeeBoundVariables) group isRec ->
                 if canSeeBoundVariables then
                   (acc ++ groupBoundExps group, True)
                 else
                 let idents = groupIdentifiers group in
                 if List.member ident idents then
                   if isRec then
                     (acc ++ groupBoundExps group, True)
                   else
                     (acc, True)
                 else
                   (acc, False)
            in
            if found then
              res ++ [e2]
            else res

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


-- -- Only useful if program run through EvalUpdate.assignUniqueNames
-- allIdentsToScopeAreas : Exp -> Dict Ident (List Exp)
-- allIdentsToScopeAreas program =
--   program
--   |> flattenExpTree
--   |> List.concatMap -- Make list of (ident, scopeExp) pairs
--       (\e ->
--         case (unwrapExp e) of
--           ELet _ _ _ pat _ _ _ ->
--             identifiersListInPat pat
--             |> Utils.dedup -- With ppl moving stuff around everywhere, you could have duplicate names in a pat.
--             |> flip Utils.cartProd (expToLetScopeAreas e)
--
--           EFun _ pats body _ ->
--             identifiersListInPats pats
--             |> Utils.dedup
--             |> List.map (\ident -> (ident, body))
--
--           ECase _ _ branches _ ->
--             branchPatExps branches
--             |> List.concatMap
--                 (\(bPat, bExp) ->
--                   identifiersListInPat bPat
--                   |> Utils.dedup
--                   |> List.map (\ident -> (ident, bExp))
--                 )
--
--           _ ->
--             []
--       )
--   |> Utils.pairsToDictOfLists

declarationsOf: Exp -> Maybe Declarations
declarationsOf e = case unwrapExp e of
  ELet _ _ decls _ _ -> Just decls
  _ -> Nothing

-- Find a pattern by pattern ID in the given Exp.
-- If found, returns the Exp in which it was found and the patern itself.
-- Nothing means not found or can't match pattern.
--
-- Only matches PVar for now
--
-- TODO uses existing functions to relax that requirement
findPatAndBoundExpByPId : PId -> Exp -> Maybe (Pat, Exp)
findPatAndBoundExpByPId targetPId exp =
  flip Maybe.andThen (findScopeExpAndPatByPId exp targetPId) <|
   \((scopeExp, bindingNum), pat) ->
  flip Maybe.andThen (patToMaybeIdent pat) <|
   \ident ->
  flip Maybe.andThen (declarationsOf scopeExp) <|
   \decls ->
  flip Maybe.andThen (Utils.nth (getDeclarationsInOrder decls) bindingNum |> Result.toMaybe) <|
   \decl ->
     case decl of
       DeclExp (LetExp _ _ letPat _ _ letBoundExp) ->
         tryMatchExpReturningList letPat letBoundExp
         |> Utils.maybeFind ident
         |> Maybe.map (\boundExp -> (pat, boundExp))
       _ -> Nothing


-- Nothing means not found or can't match pattern.
--
-- Only matches PVar or PAs for now
findBoundExpByPId : PId -> Exp -> Maybe Exp
findBoundExpByPId targetPId exp =
  findPatAndBoundExpByPId targetPId exp
  |> Maybe.map (\(pat, boundExp) -> boundExp)


-- Nothing means not found or can't match pattern.
findBoundExpByPathedPatternId : PathedPatternId -> Exp -> Maybe Exp
findBoundExpByPathedPatternId ((scopeEId, subScopeIndex), targetPath) exp =
  case findExpByEId exp scopeEId |> Maybe.andThen expToMaybeLetPatAndBoundExp of
    Just patsBoundExps ->
      case Utils.nth patsBoundExps subScopeIndex of
        Err msg -> Nothing
        Ok (letPat, letBoundExp) ->
          tryMatchExpPatToPaths letPat letBoundExp
          |> Utils.mapFirstSuccess (\(path, boundExp) -> if path == targetPath then Just boundExp else Nothing)
    Nothing ->
      Nothing

findDeclaration: (Int -> Declaration -> Bool) -> Declarations -> Maybe Declaration
findDeclaration callback decls =
  getDeclarations decls
  |> Utils.zipWithIndex
  |> Utils.mapFirstSuccess (\(def, index) ->
    if callback index def then Just def else Nothing)

nthDeclaration: Int -> Declarations -> Maybe Declaration
nthDeclaration i decls =
  findDeclaration (\index d -> index == i) decls-- |> Debug.log ("Declaration " ++ toString i)

mapDeclarations: (Int -> Declaration -> Declaration) -> Declarations -> Declarations
mapDeclarations callback decls =
  let (ds, dBuilder) = getDeclarationsExtractors decls in
  ds |> List.indexedMap (\index def -> callback index def) |> dBuilder

findScopeExpAndPatByPathedPatternId : PathedPatternId -> Exp -> Maybe ((Exp, Int), Pat)
findScopeExpAndPatByPathedPatternId ((scopeEId, branchI), path) exp =
  let maybeScopeExp = findExpByEId exp scopeEId in
  let maybePat =
    case Maybe.map unwrapExp maybeScopeExp of
       Just (ELet _ _ decls _ _) ->
          nthDeclaration branchI decls |> Maybe.andThen (\def ->
            case def of
              DeclAnnotation (LetAnnotation _ _ pat _ _ _) ->
                followPathInPat path pat
              DeclExp (LetExp _  _ pat _ _ _) ->
                followPathInPat path pat
              DeclType (LetType _ _ _ pat _ _ _) ->
                followPathInPat path pat
            )

       Just (EFun _ pats _ _) ->
        Utils.maybeGeti0 branchI pats
        |> Maybe.andThen (\pat -> followPathInPat path pat)

       Just (ECase _ _ branches _) ->
        Utils.maybeGeti0 branchI (branchPats branches)
        |> Maybe.andThen (\pat -> followPathInPat path pat)

       _ ->
         Nothing
  in
  maybePat
  |> Maybe.map (\pat -> ((Utils.fromJust_ "findScopeExpAndPatByPathedPatternId" maybeScopeExp, branchI - 1), pat))

findPatByPathedPatternId : PathedPatternId -> Exp -> Maybe Pat
findPatByPathedPatternId pathedPatId exp =
  findScopeExpAndPatByPathedPatternId pathedPatId exp
  |> Maybe.map Tuple.second


followPathInPat : List Int -> Pat -> Maybe Pat
followPathInPat path pat =
  case (pat.val.p__, path) of
    (_, []) ->
      Just pat

    (PAs _ p1 _ p2, i::is) ->
      Utils.maybeGeti1 i [p1, p2]
      |> Maybe.andThen (\p -> followPathInPat is p)

    (PList _ ps _ Nothing _, i::is) ->
      Utils.maybeGeti1 i ps
      |> Maybe.andThen (\p -> followPathInPat is p)

    (PList _ ps _ (Just tailPat) _, i::is) ->
      Utils.maybeGeti1 i (ps ++ [tailPat])
      |> Maybe.andThen (\p -> followPathInPat is p)

    (PRecord _ ps _, i::is) ->
      Utils.maybeGeti1 i ps
      |> Maybe.andThen (\(_,_,_,_,p) -> followPathInPat is p)

    (PParens _ p _, i :: is) ->
      Utils.maybeGeti1 i [p]
      |> Maybe.andThen (\p -> followPathInPat is p)

    (PColonType _ p _ _, i :: is) ->
      Utils.maybeGeti1 i [p]
      |> Maybe.andThen (\p -> followPathInPat is p)
    _ ->
      Nothing


pathedPatternIdToPId : PathedPatternId -> Exp -> Maybe PId
pathedPatternIdToPId pathedPatId exp =
  findPatByPathedPatternId pathedPatId exp
  |> Maybe.map (.val >> .pid)


pathForIdentInPat : Ident -> Pat -> Maybe (List Int)
pathForIdentInPat targetIdent pat =
  identPathsInPat pat
  |> Utils.mapFirstSuccess
      (\(ident, path) ->
        if ident == targetIdent
        then Just path
        else Nothing
      )


identPathsInPat : Pat -> List (Ident, List Int)
identPathsInPat pat =
  let childIdentPaths =
    childPats pat
    |> Utils.concatMapi1
        (\(i, childPat) ->
          identPathsInPat childPat
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
        case (unwrapExp exp) of
          EList _ es _ _ _ -> [(exp, [i])] ++ List.map (\(e, path) -> (e, i::path)) (expPathsInExpList (Utils.listValues es))
          _                -> [(exp, [i])]
      )
  |> List.concat


eidPathInExpList : List Exp -> EId -> Maybe (List Int)
eidPathInExpList exps targetEId =
  expPathsInExpList exps
  |> Utils.mapFirstSuccess
      (\(e, path) -> if (expEId e) == targetEId then Just path else Nothing)


identPIdsInPat : Pat -> List (Ident, PId)
identPIdsInPat pat =
  identPatsInPat pat
  |> List.map (\(ident, pat) -> (ident, pat.val.pid))


identPatsInPat : Pat -> List (Ident, Pat)
identPatsInPat pat =
  flattenPatTree pat
  |> List.filterMap
      (\p ->
        case patToMaybeIdent p of
          Just ident -> Just (ident, p)
          Nothing    -> Nothing
      )


tryMatchExpsPatsToPathsAtFunctionCall : List Pat -> List Exp -> List (Int, List Int, Exp)
tryMatchExpsPatsToPathsAtFunctionCall pats exps =
  -- Allow partial application
  Utils.zip pats exps
  -- Not simply making a dummy pList/eList and sending that to
  -- tryMatchExpPatToPaths b/c want if we do then we will get
  -- a extra ([], dummyEList) result, which we don't want.
  |> List.indexedMap
    (\i (p, e) ->
      tryMatchExpPatToPaths_ p e
      |> Maybe.map (\pathAndExps -> List.map (\(path, e) -> (i, path, e)) pathAndExps)
    )
  |> Utils.projJusts
  |> Maybe.withDefault []
  |> List.concat


-- Match exp and pat, returning all the paths that could be matched to an expression
tryMatchExpPatToPaths : Pat -> Exp -> List (List Int, Exp)
tryMatchExpPatToPaths pat exp =
  tryMatchExpPatToPaths_ pat exp
  |> Maybe.withDefault []

-- Match exp and pat, returning all the pids that could be matched to an expression
tryMatchExpPatToPIds : Pat -> Exp -> List (PId, Exp)
tryMatchExpPatToPIds pat exp =
  tryMatchExpPatToPats pat exp
  |> List.map (\(p,e) -> (p.val.pid,e))

-- Match exp and pat, returning all the pats that could be matched to an expression
tryMatchExpPatToPats : Pat -> Exp -> List (Pat, Exp)
tryMatchExpPatToPats pat exp =
  tryMatchExpPatToPats_ pat exp
  |> Maybe.withDefault []

tryMatchExpPatToPaths_ : Pat -> Exp -> Maybe (List (List Int, Exp))
tryMatchExpPatToPaths_ pat exp =
  tryMatchExpPatToSomething
      (\pat exp -> [([], exp)])
      (\i (path, e) -> (i::path, e))
      pat
      exp

tryMatchExpPatToPats_ : Pat -> Exp -> Maybe (List (Pat, Exp))
tryMatchExpPatToPats_ pat exp =
  tryMatchExpPatToSomething
      (\pat exp -> [(pat, exp)])
      (\i binding -> binding)
      pat
      exp

-- Unlike tryMatchExp (currently), this will return partial matches
-- (For matching function calls with function arguments)
--
-- i.e. "Just ..." means partial or complete match
tryMatchExpPatToSomething : (Pat -> Exp -> List a) -> (Int -> a -> a) -> Pat -> Exp -> Maybe (List a)
tryMatchExpPatToSomething makeThisMatch postProcessDescendentWithPath pat exp =
  let recurse pat exp = tryMatchExpPatToSomething makeThisMatch postProcessDescendentWithPath pat exp in
  let thisMatch = makeThisMatch pat exp in -- makeThisMatch returns list in case you want 0 match entries
  let addThisMatch matchResult =
    Maybe.map ((++) thisMatch) matchResult
  in
  let postProcessDescendentsWithPath i pathAndExps = List.map (postProcessDescendentWithPath i) pathAndExps in
  let matchListsAsFarAsPossible ps es =
    Utils.zip ps es
    |> Utils.mapi1
        (\(i, (p, e)) ->
          recurse p e |> Maybe.map (postProcessDescendentsWithPath i)
        )
    |> Utils.projJusts
    |> Maybe.map List.concat
  in
  case pat.val.p__ of
    PWildcard _ ->
      Just thisMatch

    PVar _ ident _ ->
      Just thisMatch

    PAs _ innerPat1 _ innerPat2 ->
      recurse innerPat1 exp
      |> Maybe.andThen (\match ->
        recurse innerPat2 exp |> Maybe.map ((++) match)
      )
      |> Maybe.map (postProcessDescendentsWithPath 1)
      |> addThisMatch

    PList _ ps _ Nothing _ ->
      case unwrapExp <| expEffectiveExp exp of
        EList _ es _ Nothing _ ->
          if List.length ps /= List.length es then
            Nothing
          else
            matchListsAsFarAsPossible ps (Utils.listValues es)
            |> addThisMatch

        EList _ es _ (Just tail) _ ->
          if List.length es > List.length ps then
            Nothing
          else
            matchListsAsFarAsPossible ps (Utils.listValues es)
            |> addThisMatch

        _ ->
          Just thisMatch

    PList _ ps _ (Just restPat) _ ->
      case expEffectiveExp exp |> \x -> (unwrapExp x) of
        EList _ es _ Nothing _ ->
          if List.length es < List.length ps then
            Nothing
          else
            -- Nothing in the tail has a definite path: skip it.
            matchListsAsFarAsPossible ps (Utils.listValues es)
            |> addThisMatch

        EList _ es _ (Just restExp) _ ->
          if List.length es /= List.length ps then
            -- Nothing in the tail has a definite path: skip it.
            matchListsAsFarAsPossible ps (Utils.listValues es)
            |> addThisMatch
          else
            matchListsAsFarAsPossible (ps ++ [restPat]) (Utils.listValues es ++ [restExp])
            |> addThisMatch

        _ ->
          Just thisMatch

    PRecord _ ps _ ->
      case unwrapExp <| expEffectiveExp exp of
        ERecord _ Nothing decls _ ->
          case recordEntriesFromDeclarations decls of
            Just es ->
              let psEsMaybes = Record.getPatternMatch Utils.recordKey Utils.recordKey ps es in
              case psEsMaybes of
                Nothing -> Nothing
                Just psEs ->
                  let (pps, ees) = List.unzip psEs in
                  matchListsAsFarAsPossible (Utils.recordValues pps) (Utils.recordValues ees)
                  |> addThisMatch
            Nothing -> Just thisMatch
        _ ->
          Just thisMatch

    PConst _ n ->
      case unwrapExp <| expEffectiveExp exp of
        EConst _ num _ _ -> if n == num then Just thisMatch else Nothing
        _                -> Just thisMatch

    PBase _ bv ->
      case unwrapExp <| expEffectiveExp exp of
        EBase _ ev -> if eBaseValsEqual bv ev then Just thisMatch else Nothing
        _          -> Just thisMatch

    PParens ws1 innerPat ws2 ->
      recurse innerPat exp

    PColonType _ innerPat _ _ ->
      recurse innerPat exp

-- Given an EId, look for a name bound to it and the let scope that defined the binding.
findLetAndIdentBindingExp : EId -> Exp -> Maybe (Exp, Ident)
findLetAndIdentBindingExp targetEId program =
  program
  |> mapFirstSuccessNode
      (\exp ->
        case unwrapExp exp of
          ELet _ _ (Declarations _ _ _ letexps) _ _ ->
            letexps |> elemsOf |> Utils.mapFirstSuccess (\(LetExp _ _ pat _ _ boundExp) ->
            tryMatchExpReturningList pat boundExp
            |> Utils.mapFirstSuccess
                (\(ident, boundE) ->
                  if (expEId boundE) == targetEId
                  then Just (exp, ident)
                  else Nothing
                ))

          _ ->
            Nothing
      )


-- Given an EId, look for a pat matching it and the let scope that defined the binding.
--
-- Will match and return PLists even though they don't introduce variables
findLetAndPatMatchingExp : EId -> Exp -> Maybe (Exp, Pat)
findLetAndPatMatchingExp targetEId program =
  findLetAndPatMatchingExp_
      targetEId
      program
      (\letExp (pat, boundE) ->
        if (expEId boundE) == targetEId
        then Just (letExp, pat)
        else Nothing
      )


-- Given an EId, look for a pat matching it and the let scope that defined the binding.
--
-- Looser on EId matching: targetEId and bound expression just need to
-- simply resolve to the same expression (i.e. discarding type annotations etc.)
--
-- Will match and return PLists even though they don't introduce variables
findLetAndPatMatchingExpLoose : EId -> Exp -> Maybe (Exp, Pat)
findLetAndPatMatchingExpLoose targetEId program =
  findLetAndPatMatchingExp_
      targetEId
      program
      (\letExp (pat, boundE) ->
        case findExpByEId boundE targetEId of
          Just targetExp ->
            if (expEId <| expEffectiveExp targetExp) == (expEId <| expEffectiveExp boundE)
            then Just (letExp, pat)
            else Nothing
          Nothing ->
            Nothing
      )


findLetAndPatMatchingExp_ : EId -> Exp -> (Exp -> (Pat, Exp) -> Maybe a) -> Maybe a
findLetAndPatMatchingExp_ targetEId program letAndPatBoundEFindMap =
  program
  |> mapFirstSuccessNode
      (\exp ->
        case unwrapExp exp of
          ELet _ _ (Declarations _ _ _ letexpsGroups) _ _ ->
            Utils.mapFirstSuccess (\(LetExp _ _ pat _ _ boundExp) ->
              tryMatchExpPatToPats pat boundExp -- In pt@[x y], must be sure pt matches before [x y]
              |> Utils.mapFirstSuccess (letAndPatBoundEFindMap exp)
            ) <| elemsOf letexpsGroups
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

allSimplyResolvableThings : (Pat -> Exp -> List a) -> Exp -> List a
allSimplyResolvableThings tryThing program =
  program
  |> flattenExpTree
  |> List.concatMap
      (\exp ->
        case unwrapExp exp of
          ELet _ _ (Declarations _ _ _ letexpsGroups) _ _ ->
            List.concatMap (\(LetExp _ _ pat _ _ boundExp) -> tryThing pat boundExp) <| elemsOf <| letexpsGroups
          _                               -> []
      )

-- Probably not useful unless program has been run through EvalUpdate.assignUniqueNames
allSimplyResolvableLetBindings : Exp -> List (Ident, Exp)
allSimplyResolvableLetBindings =
  allSimplyResolvableThings tryMatchExpReturningList

allSimplyResolvableLetPatBindings : Exp -> List (Pat, Exp)
allSimplyResolvableLetPatBindings =
  allSimplyResolvableThings tryMatchExpPatToPats

-- Precondition: program has been run through EvalUpdate.assignUniqueNames.
--
-- Identify all numeric variables in the program.
-- Not the smartest; there could be false negatives, but no false positives.
numericLetBoundIdentifiers : Exp -> Set.Set Ident
numericLetBoundIdentifiers program =
  let isSurelyNumeric numericIdents exp =
    let recurse e = isSurelyNumeric numericIdents e in
    case (unwrapExp exp) of
      EConst _ _ _ _ -> True
      EBase _ _      -> False
      EVar _ ident   -> Set.member ident numericIdents
      EFun _ _ _ _   -> False
      EApp _ _ _ _ _ -> False -- Not smart here.
      EOp _ _ op operands _ ->
        case op.val of
          Pi         -> True
          DictEmpty  -> False
          CurrentEnv -> False
          DictFromList -> False
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
          ToStrExceptStr -> List.any recurse operands
          RegexExtractFirstIn -> False

      EList _ _ _ _ _               -> False
      ERecord _ _ _ _               -> False
      ESelect _ _ _ _ _             -> False
      EIf _ _ _ thenExp _ elseExp _ -> recurse thenExp && recurse elseExp
      ECase _ _ branches _          -> List.all recurse (branchExps branches)
      ELet _ _ _ _ body             -> recurse body
      EColonType _ e _ _ _          -> recurse e
      EParens _ e _ _               -> recurse e
      EHole _ EEmptyHole            -> False
      EHole _ (ESnapHole val)       -> valIsNum val
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
renameVarsUntilBound : Dict Ident Ident -> Exp -> Exp
renameVarsUntilBound renamings exp =
  let renamer: String -> Exp -> Exp
      renamer newName    e =
    -- let _ = Debug.log ("Renaming " ++ newName ++ " on") e in
    case (unwrapExp e) of
       EVar ws oldName -> replaceE__ e (EVar ws newName)
       _               -> Debug.crash <| "LangTools.renameVarsUntilBound: renamer should only be passed an EVar, but given: " ++ toString e
  in
  let fnSubst =
    renamings
    |> Dict.map (\_ newName -> (renamer newName))
  in
  transformVarsUntilBound fnSubst exp

removeIntroducedIdents: List Ident -> Dict Ident (Exp -> Exp) -> Dict Ident (Exp -> Exp)
removeIntroducedIdents introducedIdents subst =
     List.foldl
       Dict.remove
       subst
       introducedIdents

transformVarsUntilBoundDecls: Dict Ident (Exp -> Exp) -> Declarations -> (Dict Ident (Exp -> Exp), Declarations)
transformVarsUntilBoundDecls subst (Declarations po types anns grouppedExps) =
  let (revLetExps, newSubst) = foldLeftGroup ([], subst) grouppedExps <|
    \(revAcc, subst) group isRec ->
       let idents = groupIdentifiers group in
       let newSubst = removeIntroducedIdents idents subst in
       let localSubst = if isRec then newSubst else subst in
       ( Utils.foldLeft revAcc group <|
           \revAcc (LetExp wsC wsB p fun wsE e) ->
              (LetExp wsC wsB p fun wsE (transformVarsUntilBound localSubst e) :: revAcc)
       , newSubst)
  in
  let newLetExps = List.reverse revLetExps in
  (newSubst, Declarations po types anns (regroup grouppedExps newLetExps))

-- Transforms only free variables.
-- Preserves EIds (for Brainstorm)
-- Might be able to rewrite using freeVars or mapFoldExpTopDownWithScope
transformVarsUntilBound : Dict Ident (Exp {-EVars only-} -> Exp) -> Exp -> Exp
transformVarsUntilBound subst exp =
  let recurse e = transformVarsUntilBound subst e in
  let recurseWithout introducedIdents e =
    let newSubst = removeIntroducedIdents introducedIdents subst in
    if Dict.size newSubst == 0 then
       e
    else
       transformVarsUntilBound newSubst e
  in
  case (unwrapExp exp) of
    EConst _ _ _ _              -> exp
    EBase _ _                   -> exp
    EVar _ ident                ->
      case Dict.get ident subst of
        Just f  -> f exp
        Nothing -> exp

    EFun ws1 ps e ws2           -> replaceE__ exp (EFun ws1 ps (recurseWithout (identifiersListInPats ps) e) ws2)
    EOp ws1 wsi op es ws2       -> replaceE__ exp (EOp ws1 wsi op (List.map recurse es) ws2)
    EList ws1 es ws2 m ws3      -> replaceE__ exp (EList ws1 (Utils.listValuesMap recurse es) ws2 (Maybe.map recurse m) ws3)
    ERecord ws1 mb decls ws2    ->
      replaceE__ exp <| ERecord ws1 (Maybe.map (\(t1, t2) -> (recurse t1, t2)) mb) (Tuple.second <| transformVarsUntilBoundDecls subst decls) ws2
    ESelect ws0 e1 ws1 ws2 s    -> replaceE__ exp (ESelect ws0 (recurse e1) ws1 ws2 s)
    EIf ws1 e1 ws2 e2 ws3 e3 ws4 -> replaceE__ exp (EIf ws1 (recurse e1) ws2 (recurse e2) ws3 (recurse e3) ws4)
    ECase ws1 e1 bs ws2         ->
      let newScrutinee = recurse e1 in
      let newBranches =
        bs
        |> List.map
            (mapValField (\(Branch_ bws1 bPat bExp bws2) ->
              Branch_ bws1 bPat (recurseWithout (identifiersListInPat bPat) bExp) bws2
            ))
      in
      replaceE__ exp <| ECase ws1 newScrutinee newBranches ws2

    EApp ws1 e1 es appType ws2      -> replaceE__ exp (EApp ws1 (recurse e1) (List.map recurse es) appType ws2)
    ELet ws1 kind decls wsIn e2 ->
      let (newSubst, newDecls) = transformVarsUntilBoundDecls subst decls in
      replaceE__ exp <| ELet ws1 kind newDecls wsIn (transformVarsUntilBound newSubst e2)

    EColonType ws1 e ws2 tipe ws3   -> replaceE__ exp (EColonType ws1 (recurse e) ws2 tipe ws3)
    EParens ws1 e pStyle ws2        -> replaceE__ exp (EParens ws1 (recurse e) pStyle ws2)
    EHole _ _                       -> exp


-- Find EVars using the given name, until name is rebound.
identifierUses : Ident -> Exp -> List Exp
identifierUses ident exp =
  freeVars exp
  |> List.filter (expToIdent >> (==) ident)


identifierUsageEIds : Ident -> Exp -> List EId
identifierUsageEIds ident exp =
  identifierUses ident exp
  |> List.map expEId


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
  case (unwrapExp exp) of
    -- EVal _           -> ret Set.empty
    EConst _ _ _ _   -> ret Set.empty
    EBase _ _        -> ret Set.empty
    EVar _ ident     -> ret Set.empty -- Referencing a var doesn't count.
    EFun _ ps e _    -> ret <| recurseWithNewIdents ps e
    EOp _ _ op es _  -> ret <| recurseAllChildren ()
    EList _ es _ m _ -> ret <| recurseAllChildren ()
    ERecord _ m es _ -> ret <| recurseAllChildren ()
    ESelect _ _ _ _ _ -> ret <| recurseAllChildren ()
    EIf _ e1 _ e2 _ e3 _ -> ret <| recurseAllChildren ()
    ECase _ e1 bs _  ->
      let scrutineeResult = recurse e1 in
      let branchResults =
        bs
        |> List.map .val
        |> List.map
            (\(Branch_ _ bPat bExp _) -> recurseWithNewIdents [bPat] bExp)
      in
      ret <| Utils.unionAll (scrutineeResult::branchResults)

    EApp _ e1 es _ _                  -> ret <| recurseAllChildren ()
    ELet _ kind (Declarations _ _ _ grouppedExps) _ e2 ->
      let (assignsResults, newIdents) = foldLeftGroup (Set.empty, idents) grouppedExps <|
        \(visibleIdents, idents) group isRec ->
           let newIdents = List.concatMap (\(LetExp _ _ p _ _ _) -> identifiersListInPat p) group in
           let assignResult =
             Utils.foldLeft Set.empty group <|
              (\acc (LetExp _ _ _ _ _ e2) ->
                 Set.union acc <|
                   visibleIdentifiersAtPredicate_ (if isRec then Set.union idents (Set.fromList newIdents) else idents) e2 pred)
           in
           (Set.union visibleIdents assignResult, Set.union idents (Set.fromList newIdents))
      in
      let bodyResult = visibleIdentifiersAtPredicate_ newIdents e2 pred in
      ret <| Set.union assignsResults bodyResult

    EColonType _ e _ tipe _   -> ret <| recurse e
    EParens _ e _ _           -> ret <| recurse e
    EHole _ _                 -> ret Set.empty

-- Compute the PathedPatternId that assigned the binding referenced by varExp
--
-- Uses ident of given varExp, returns that name's binding at varExp's EId in program.
--
-- Returns Nothing if free in program or not in program
bindingPathedPatternIdFor : Exp -> Exp -> Maybe PathedPatternId
bindingPathedPatternIdFor varExp program =
  let targetName = expToIdent varExp in
  let targetEId  = (expEId varExp) in
  bindingPathedPatternIdForIdentAtEId targetName targetEId program

bindingPathedPatternIdForIdentAtEId : Ident -> EId -> Exp -> Maybe PathedPatternId
bindingPathedPatternIdForIdentAtEId targetName targetEId program =
  let predMap: Exp -> Maybe PathedPatternId -> Maybe (Maybe PathedPatternId)
      predMap exp maybeCurrentBindingPathedPatternId =
    case (unwrapExp exp) of
       EVar _ _ ->
        if (expEId exp) == targetEId then
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
allVarEIdsToBindingPId : Exp -> Dict EId (Maybe PId)
allVarEIdsToBindingPId program =
  allVarEIdsToBindingPat program
  |> Dict.map (\_ maybePat -> Maybe.map (.val >> .pid) maybePat)


-- "Nothing" means free in program
allVarEIdsToBindingPat : Exp -> Dict EId (Maybe Pat)
allVarEIdsToBindingPat program =
  allVarEIdsToBindingPatList program
  |> Dict.fromList


-- "Nothing" means free in program
-- May want this list version when you might have duplicate EIds
allVarEIdsToBindingPIdList : Exp -> List (EId, Maybe PId)
allVarEIdsToBindingPIdList program =
  allVarEIdsToBindingPatList program
  |> List.map (\(eid, maybePat) -> (eid, Maybe.map (.val >> .pid) maybePat))


{-- Returns the EId's of all variables of an expression,
    along with the pattern in which they were originally defined
    (if bound, else 'Nothing' for no pattern and the variable is free)
--}
allVarEIdsToBindingPatList : Exp -> List (EId, Maybe Pat)
allVarEIdsToBindingPatList program =
  let  handleELet: Exp -> IsRec -> List LetExp -> BindingNumber -> List (EId, Maybe Pat) -> Dict String Pat -> (List (EId, Maybe Pat), Dict String Pat)
       handleELet  _      isRec    letExpGroup    _                globalAcc                identToPId =
    (globalAcc, Dict.union
        (List.map patOfLetExp letExpGroup |> List.concatMap identPatsInPat |> Dict.fromList)
        identToPId)

       handleEFun: Exp -> Dict String Pat -> Dict String Pat
       handleEFun funcExp identToPId =
    Dict.union
        (expToFuncPats funcExp |> List.concatMap identPatsInPat |> Dict.fromList)
        identToPId

       handleCaseBranch: Exp -> Branch -> Int -> Dict String Pat -> Dict String Pat
       handleCaseBranch caseExp branch branchI identToPId =
    Dict.union
        (branchPat branch |> identPatsInPat |> Dict.fromList)
        identToPId
  in
  program
  |> foldExpTopDownWithScope
      (\exp eidAndMaybePId identToPId ->
        case expToMaybeIdent exp of
          Just ident -> ((expEId exp), Dict.get ident identToPId) :: eidAndMaybePId
          Nothing    -> eidAndMaybePId
      )
      handleELet
      handleEFun
      handleCaseBranch
      []
      Dict.empty


-- Presumes program has been run through EvalUpdate.assignUniqueNames
-- "Nothing" means no matching name in program
allVarEIdsToBindingPIdBasedOnUniqueName : Exp -> Dict EId (Maybe PId)
allVarEIdsToBindingPIdBasedOnUniqueName program =
  allVarEIdsToBindingPatsBasedOnUniqueName program
  |> Dict.map (\_ maybePat -> Maybe.map (.val >> .pid) maybePat)


allVarEIdsToBindingPatsBasedOnUniqueName : Exp -> Dict EId (Maybe Pat)
allVarEIdsToBindingPatsBasedOnUniqueName program =
  let allIdentToPat =
    flattenExpTree program
    |> List.concatMap
        (\exp ->
          case unwrapExp exp of
            EFun _ pats _ _          -> List.concatMap identPatsInPat pats
            ELet _ _ (Declarations _ _ _ letexpsGroups) _ _ ->
              letexpsGroups |> elemsOf
              |> List.concatMap (\(LetExp _ _ pat _ _ _) -> identPatsInPat pat)
            ECase _ _ branches _     -> List.concatMap identPatsInPat (branchPats branches)
            _                        -> []
        )
    |> Dict.fromList
  in
  flattenExpTree program
  |> List.filterMap
      (\exp ->
        case expToMaybeIdent exp of
          Nothing    -> Nothing
          Just ident ->
            case Dict.get ident allIdentToPat of
              Nothing  -> Just ((expEId exp), Nothing)
              Just pid -> Just ((expEId exp), Just pid)
      )
  |> Dict.fromList

-- Outer returns maybe indicates if variable found
-- Inner returns maybe is whatever you want to return from the predicateMap,
-- presumably the pathedPatternId that bound the identifier as seen from the usage site (Nothing means free)
bindingPathedPatternIdFor_ : Maybe PathedPatternId -> Ident -> (Exp -> Maybe PathedPatternId -> Maybe a) -> Exp -> Maybe a
bindingPathedPatternIdFor_ currentBindingPathedPatternId targetName predicateMap exp =
  let _ = Debug.log "bindingPathedPatternIdFor_" (currentBindingPathedPatternId, targetName, expEId exp) in
  let recurse pathedPatternId e = bindingPathedPatternIdFor_ pathedPatternId targetName predicateMap e in
  let maybeNewBindingForRecursion pat branchI pathPrefix =
    pathForIdentInPat targetName pat
    |> Maybe.map (\path -> Just (((expEId exp), branchI), pathPrefix ++ path))
  in
  case predicateMap exp currentBindingPathedPatternId of
    Just result -> Just result
    Nothing ->
      case (unwrapExp exp) of
        EFun _ pats body _ ->
          let newBindingPathedPatternId =
            pats
            |> Utils.zipWithIndex
            |> Utils.mapFirstSuccess (\(pat, i) -> maybeNewBindingForRecursion pat i [])
            |> Maybe.withDefault currentBindingPathedPatternId
          in
          recurse newBindingPathedPatternId body

        ELet _ _ (Declarations _ _ _ letexpsGroups as decls) _ body ->
          let aux: BindingNumber -> Maybe PathedPatternId ->  Maybe PathedPatternId -> GroupsOf LetExp -> Maybe a
              aux  bn               currentBindingPathedPatternId newBindingPathedPatternId groups = case groups of
            (_, []) :: (((newIsRec, newGroup) as newHead)::newTail) ->
               let nextBindingPathedPatternId =
                 Utils.foldLeft newBindingPathedPatternId newGroup <|
                 \currentBindingPathedPatternId (LetExp _ _ pat _ _ _) ->
                    maybeNewBindingForRecursion pat bn []
                    |> Maybe.withDefault currentBindingPathedPatternId
               in
               aux (bn + 1) newBindingPathedPatternId nextBindingPathedPatternId (newHead::newTail)
            (isRec, (LetExp _ _ pat _ _ boundExp)::groupTail) :: tail ->
               let pathedPatternIdForBoundExp = if isRec then newBindingPathedPatternId else currentBindingPathedPatternId in
               Utils.firstOrLazySecond
                 (recurse pathedPatternIdForBoundExp boundExp)
                 (\() -> aux (bn + 1) currentBindingPathedPatternId newBindingPathedPatternId ((isRec, groupTail)::tail))
            _ ->
               recurse newBindingPathedPatternId body
          in aux (startBindingNumLetExp decls) currentBindingPathedPatternId currentBindingPathedPatternId ((False, [])::letexpsGroups)-- isRecursive pat _ boundExp _ body

        ECase _ _ branches _ ->
          branchPatExps branches
          |> Utils.zipWithIndex
          |> Utils.mapFirstSuccess
              (\((pat, branchExp), i) ->
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
preludeExpEnv: Dict Ident ExpressionBinding
preludeExpEnv = expEnvAt_ LeoParser.prelude (expEId <| lastTopLevelExp LeoParser.prelude) |> Utils.fromJust_ "LangTools.preludeExpEnv"

-- Return bindings to expressions (as best as possible) at EId
expEnvAt : Exp -> EId -> Maybe (Dict Ident ExpressionBinding)
expEnvAt exp targetEId =
  expEnvAt_ exp targetEId
  |> Maybe.map
      (\bindings -> Dict.union bindings preludeExpEnv)

-- If the EId is found in Exp, returns a dictionary of possible binding.
-- The dictionary is built bottom-up, that is, each time this returns a Just,
-- the caller will augment it with the bindings in scope..
expEnvAt_ : Exp -> EId -> Maybe (Dict Ident ExpressionBinding)
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
  let addBindingsFrom2 listLetExp deeperBindings =
    Utils.foldLeft deeperBindings listLetExp <|
       \acc (LetExp _ _ p _ _ e1) -> addBindingsFrom p e1 acc
  in
  if expEId exp == targetEId then
    Just Dict.empty
  else
    case (unwrapExp exp) of
      EConst _ _ _ _   -> Nothing
      EBase _ _        -> Nothing
      EVar _ ident     -> Nothing
      EFun _ ps e _    -> recurse e |> Maybe.map (addShallowerIdentifiers (identifiersListInPats ps))
      EOp _ _ op es _  -> recurseAllChildren ()
      EList _ es _ m _ -> recurseAllChildren ()
      ERecord _ _ _ _ -> recurseAllChildren ()
      ESelect _ _ _ _ _ -> recurseAllChildren ()
      EIf _ e1 _ e2 _ e3 _ -> recurseAllChildren ()
      ECase _ e1 bs _  ->
        case recurse e1 of
          Just bindings ->
            Just bindings -- Found targetEId in scrutinee

          Nothing ->
            bs
            |> List.map .val
            |> Utils.mapFirstSuccess
                (\(Branch_ _ bPat bExp _) -> recurse bExp |> Maybe.map (addBindingsFrom bPat bExp))

      EApp _ e1 es _ _                  -> recurseAllChildren ()
      ELet _ kind (Declarations _ _ _ letexpsGroups) _ e2       ->
        let aux: (Dict Ident ExpressionBinding -> Dict Ident ExpressionBinding) -> List (Bool, List LetExp) -> Maybe (Dict Ident ExpressionBinding)
            aux  accAdder                                                          letexpsGroups = case letexpsGroups of
          [] -> recurse e2 |> Maybe.map accAdder
          (isRec, group) :: tail ->
             case Utils.mapFirstSuccess recurse (groupBoundExps group) of
               Nothing ->
                 aux ((\group accAdder -> accAdder >> addBindingsFrom2 group) group accAdder) tail
               Just bindings ->
                 Just <|
                    if isRec then accAdder <| addBindingsFrom2 group bindings
                    else bindings
        in aux identity letexpsGroups

      EColonType _ e _ tipe _    -> recurse e
      EParens _ e _ _            -> recurse e
      EHole _ _                  -> Nothing

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
        case (unwrapExp exp) of
          EApp _ appFuncExp argExps _ _ ->
            case (unwrapExp appFuncExp) of
              EVar _ funcName ->
                case resolveIdentifierToExp funcName (expEId appFuncExp) program of -- This is probably slow.
                  Just (Bound funcExp) ->
                    case ((unwrapExp funcExp), LeoParser.isPreludeEId (expEId funcExp)) of
                      (EFun _ fpats _ _, False) ->
                        -- Allow partial application
                        tryMatchExpsPatsToPathsAtFunctionCall fpats argExps
                        |> Utils.mapFirstSuccess
                            (\(bn, path, correspondingExp) ->
                              if (expEId correspondingExp) == targetEId
                              then Just (((expEId funcExp), bn), path)
                              else Nothing
                            )

                      _ -> Nothing

                  _ -> Nothing

              _ -> Nothing

          _ -> Nothing
      )
