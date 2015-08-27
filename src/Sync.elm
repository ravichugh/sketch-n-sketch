module Sync (Options, defaultOptions, syncOptionsOf,
             inferLocalUpdates, inferStructuralUpdate, prepareLiveUpdates,
             inferDeleteUpdate,
             inferNewRelationships,
             printZoneTable, LiveInfo, Triggers, tryToBeSmart) where

import Dict exposing (Dict)
import Set
import Utils exposing (justGet_)
import Debug
import String

import Lang exposing (..)
import LangSvg exposing (NodeId, ShapeKind, Zone, addi)
import Eval
import OurParser2 as P
import LangParser2 as Parser
import LangUnparser as Un


------------------------------------------------------------------------------
-- Sync.Options

type alias Options =
  { thawedByDefault : Bool }

defaultOptions =
  { thawedByDefault = True }

syncOptionsOf e =
  case Utils.maybeFind "unannotated-numbers" (getOptions e) of
    Nothing -> defaultOptions
    Just s -> if
      | s == "n?" -> { thawedByDefault = True }
      | s == "n!" -> { thawedByDefault = False }
      | otherwise ->
          let _ = Debug.log "invalid sync option: " s in
          defaultOptions


------------------------------------------------------------------------------
-- Value Contexts

multiLeafDiffs = True

type alias VContext = Val
  -- a VContext is a Val with exactly one VHole in single-leaf diff mode
  -- or multiple VHoles in multi-leaf diff mode

type alias HoleSubst = Dict.Dict Int (Val,Val)

fillHole : VContext -> HoleSubst -> Val
fillHole = fillHole_ True

fillHole_ new vc subst = case vc of
  VHole i          -> case Dict.get i subst of
                        Just (vOld,vNew) -> if new then vNew else vOld
  VConst _         -> vc
  VBase _          -> vc
  VClosure _ _ _ _ -> vc   -- not recursing into closures
  VList vs         -> VList (List.map (\v -> fillHole_ new v subst) vs)

type VDiff = Same Val | Diff VContext HoleSubst

diff : Val -> Val -> Maybe VDiff
diff v1 v2 =
  let res = diff_ 0 v1 v2 in
  case res of
    Just (_, Diff vc subst) ->
      let (v1',v2') = (fillHole_ False vc subst, fillHole_ True vc subst) in
      if | eqV (v1,v1') && eqV (v2,v2') -> Just (Diff vc subst)
         | otherwise ->
             let f (i,(vOld,vNew)) = [toString i, strVal vOld, strVal vNew] in
             Debug.crash <| Utils.lines <|
               ("bad diff" :: strVal vc :: List.concatMap f (Dict.toList subst))
    _ ->
      Utils.mapMaybe snd res

eqV (v1,v2) = case (v1, v2) of            -- equality modulo traces
  (VConst it, VConst jt) -> fst it == fst jt
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> False
      Just l  -> List.all eqV l
  _ -> v1 == v2

diffNoCheck v1 v2 =
  Utils.mapMaybe snd (diff_ 0 v1 v2)

-- assuming that v1 is the value resulting from eval (so it has proper locs)
-- and that v2 has dummy locs

diff_ : Int -> Val -> Val -> Maybe (Int, VDiff)
diff_ k v1 v2 = case (v1, v2) of
  (VBase Star, VConst _) -> Just (k, Same v2)
  (VConst (i,tr), VConst (j,_)) ->
    if | i == j    -> Just (k, Same (VConst (i,tr)))  -- cf. comment above
       | otherwise -> let d = Dict.singleton k (v1, (VConst (j,tr))) in
                      Just (k+1, Diff (VHole k) d)
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> Nothing
      Just l ->
        List.foldr (\(vi1,vi2) acc ->
          case acc of
            Nothing -> Nothing
            Just (k, Same (VList vs)) ->
              case diff_ k vi1 vi2 of
                Nothing                 -> Nothing
                Just (k, Same v)        -> Just (k, Same (VList (v::vs)))
                Just (k, Diff vc subst) -> Just (k, Diff (VList (vc::vs)) subst)
            Just (k, Diff (VList vs) subst) ->
              case diff_ k vi1 vi2 of
                Nothing                 -> Nothing
                Just (k, Same v)        -> Just (k, Diff (VList (v::vs)) subst)
                Just (k, Diff vc sub')  ->
                  if | not multiLeafDiffs -> Nothing
                     | otherwise ->
                         let d = Dict.union subst sub' in
                         Just (k, Diff (VList (vc::vs)) d)
            Just (_, Diff _ _) ->
              Debug.crash "diff_: error?"
        ) (Just (k, Same (VList []))) l
  _ ->
    if | v1 == v2  -> Just (k, Same v1)
       | otherwise -> Nothing


------------------------------------------------------------------------------

type alias Equation = (Num, Trace)

locsOfTrace : Options -> Trace -> Set.Set Loc
locsOfTrace opts =
  let frozenByDefault = not opts.thawedByDefault in
  let foo t = case t of
    TrLoc l ->
      let (_,b,_) = l in
      if | Parser.isPreludeLoc l         -> Set.empty
         | b == frozen                   -> Set.empty
         | b == unann && frozenByDefault -> Set.empty
         | otherwise                     -> Set.singleton l
    TrOp _ ts -> List.foldl Set.union Set.empty (List.map foo ts)
  in
  foo

solveOneLeaf : Options -> Subst -> Val -> List (LocId, Num)
solveOneLeaf opts s (VConst (i, tr)) =
  List.filterMap
    (\k -> let s' = Dict.remove k s in
           Utils.mapMaybe (\n -> (k,n)) (solve s' (i, tr)))
    (List.map Utils.fst3 <| Set.toList <| locsOfTrace opts tr)

inferSubsts : Options -> Subst -> List Val -> List Subst
inferSubsts opts s0 vs =
  List.map (solveOneLeaf opts s0) vs
    |> Utils.oneOfEach
    |> List.map combine
    |> List.map (Utils.mapMaybe (\s' -> Dict.union s' s0))  -- pref to s'
    |> List.filterMap identity

combine : List (LocId, Num) -> Maybe Subst
combine solutions =
  let f (l,n) msubst =
    let g subst =
      case Dict.get l subst of
        Nothing -> Just (Dict.insert l n subst)
        Just i  -> if | i == n    -> Just (Dict.insert l n subst)
                      | otherwise -> Nothing
    in
    Utils.bindMaybe g msubst
  in
  List.foldl f (Just Dict.empty) solutions

solve : Subst -> Equation -> Maybe Num
solve subst eqn =
  (solveTopDown subst eqn) `Utils.plusMaybe` (simpleSolve subst eqn)

  -- both solveTopDown and simpleSolve
  -- assumes that a single variable is being solved for

evalTrace : Subst -> Trace -> Maybe Num
evalTrace subst t = case t of
  TrLoc (k,_,_) -> Dict.get k subst
  TrOp op ts ->
    Utils.mapMaybe
      (Eval.evalDelta op)
      (Utils.projJusts (List.map (evalTrace subst) ts))

evalLoc : Subst -> Trace -> Maybe (Maybe Num)
  -- Just (Just i)   tr is a location bound in subst
  -- Just Nothing    tr is a location not bound (i.e. it's being solved for)
  -- Nothing         tr is not a location
evalLoc subst tr =
  case tr of
    TrOp _ _    -> Nothing
    TrLoc (k,_,_) -> Just (Dict.get k subst)

solveTopDown subst (n, t) = case t of

  TrLoc (k,_,_) ->
    case Dict.get k subst of
      Nothing -> Just n
      Just _  -> Nothing

  TrOp op [t1,t2] ->
    let left  = (evalTrace subst t1, evalLoc   subst t2) in
    let right = (evalLoc   subst t1, evalTrace subst t2) in
    case (isNumBinop op, left, right) of

      -- four cases are of the following form,
      -- where k is the single location variable being solved for:
      --
      --    1.   n =  i op k
      --    2.   n =  i op t2
      --    3.   n =  k op j
      --    4.   n = t1 op j

      (True, (Just i, Just Nothing), _) -> solveR op n i
      (True, (Just i, Nothing), _)      -> Utils.bindMaybe
                                             (\n -> solveTopDown subst (n, t2))
                                             (solveR op n i)
      (True, _, (Just Nothing, Just j)) -> solveL op n j
      (True, _, (Nothing, Just j))      -> Utils.bindMaybe
                                             (\n -> solveTopDown subst (n, t1))
                                             (solveL op n j)

      _ ->
        let _ = Debug.log "Sync.solve" <| strTrace t in
        Nothing

  TrOp Cos [t1] ->
    case evalTrace subst t1 of
      Just i  -> maybeFloat <| acos i
      Nothing -> Nothing

  TrOp Sin [t1] ->
    case evalTrace subst t1 of
      Just i  -> maybeFloat <| asin i
      Nothing -> Nothing

  _ ->
    let _ = Debug.log "TODO solveTopDown" t in
    Nothing

isNumBinop = (/=) Lt

maybeFloat n =
  let thresh = 1000 in
  if | isNaN n || isInfinite n -> Debug.log "maybeFloat Nothing" Nothing
     | abs n > thresh          -> Debug.log "maybeFloat (above thresh)" Nothing
     | otherwise               -> Just n

-- n = i op j
solveR op n i = case op of
  Plus  -> maybeFloat <| n - i
  Minus -> maybeFloat <| i - n
  Mult  -> maybeFloat <| n / i
  Div   -> maybeFloat <| i / n

  RangeOffset _ -> Nothing

-- n = i op j
solveL op n j = case op of
  Plus  -> maybeFloat <| n - j
  Minus -> maybeFloat <| j + n
  Mult  -> maybeFloat <| n / j
  Div   -> maybeFloat <| j * n

  RangeOffset _ -> Nothing


simpleSolve subst (sum, tr) =
  let walkTrace t = case t of
    TrLoc (k,_,_) ->
      case Dict.get k subst of
        Nothing -> Just (0, 1)
        Just i  -> Just (i, 0)
    TrOp Plus ts ->
      let foo mx macc =
        case (mx, macc) of
          (Just (a,b), Just (acc1,acc2)) -> Just (a+acc1, b+acc2)
          _                              -> Nothing
      in
        List.foldl foo (Just (0,0)) (List.map walkTrace ts)
    _ ->
      Nothing
  in
  Utils.mapMaybe
    (\(partialSum,n) -> (sum - partialSum) / n)
    (walkTrace tr)

compareVals : (Val, Val) -> Num
compareVals (v1, v2) = case (v1, v2) of
  (VConst it, VConst jt)   -> abs (fst it - fst jt)
  (VList vs1, VList vs2)   -> case Utils.maybeZip vs1 vs2 of
                                Nothing -> largeInt
                                Just l  -> Utils.sum (List.map compareVals l)
  _                        -> if | v1 == v2  -> 0
                                 | otherwise -> largeInt

largeInt = 99999999

------------------------------------------------------------------------------

getFillers : HoleSubst -> List Val
getFillers = List.map (snd << snd) << Dict.toList

leafToStar v = case v of {VConst _ -> VBase Star; _ -> v}

-- historically, inferLocalUpdates was called "sync"

inferLocalUpdates : Options -> Exp -> Val -> Val -> Result String (List ((Exp, Val), Num))
inferLocalUpdates opts e v v' =
  case diff v v' of
    Nothing       -> Err "bad change"
    -- Just (Same _) -> Err "no change"
    Just (Same _) -> Ok []
    Just (Diff vc holeSubst) ->
      let newNew = getFillers holeSubst in
      let subst0 = Parser.substOf e in
      let substs = inferSubsts opts subst0 newNew in
      let res =
        List.sortBy snd <|
          List.filterMap (\s ->
            let e1 = applySubst s e in
            let v1 = Eval.run e1 in
            let vcStar = mapVal leafToStar vc in
            case diffNoCheck (fillHole vcStar holeSubst) v1 of
              Nothing -> Debug.crash "sync: shouldn't happen?"
              Just (Same _) ->
                let n = compareVals (v, v1) in
                Just ((e1, v1), n)
              Just (Diff _ holeSubst') ->
                let oldNew = getFillers holeSubst' in
                if | newNew /= oldNew -> Nothing
                   | otherwise ->
                       let n = compareVals (v, v1) in
                       Just ((e1, v1), n)
          ) substs
      in
      -- TODO: is this a good idea?
      if res == [] then Err "bad change 2" else Ok res


------------------------------------------------------------------------------
-- Naive Structural Update

stripSvg (VList [VBase (String "svg"), VList vs1, VList vs2]) = (vs1, vs2)

idOldShapes  = "oldCanvas"
idNewShape i = "newShape" ++ toString i
eOldShapes   = eVar idOldShapes
eNewShape i  = eVar (idNewShape i)

addComments = False -- CONFIG

comment s e =
  if | addComments -> eComment s e
     | otherwise   -> e

inferStructuralUpdate : Exp -> Val -> Val -> (Exp, Val)
inferStructuralUpdate eOld v v' =
  let (attrs1,children1) = stripSvg v in
  let (attrs2,children2) = stripSvg v' in
  let _ = Utils.assert "Sync.inferStruct" (attrs1 == attrs2) in

  let diff =
    let foo (i,(vi,vi')) acc =
      if | vi == vi' -> acc
         | otherwise -> (i,vi') :: acc in
    List.reverse (Utils.foldli foo [] (Utils.zip children1 children2)) in

  let eNewCanvas =
    let es =
      List.map (\(i,_) ->
        let n = toFloat i in
        ePair (eConst n dummyLoc) (eNewShape i)) diff in
      eApp (eVar "updateCanvas") [eOldShapes, eList es Nothing] in

  let bindings =
    List.map (\(i,vi) ->
      -- going through parser to avoid adding EVal
      let ei = Utils.fromOk "Sync.addNew" (Parser.parseE (strVal vi)) in
      (idNewShape i, ei)) diff in

  let eNew_ =
    comment " Here's your original program..." <|
    comment "" <|
      eLets [(idOldShapes, eOld)] <|
        comment "" <|
        comment " ... and here are the hard-coded updates:" <|
        comment "" <|
          eLets bindings <|
            comment "" <|
            comment " Refactor if you'd like!" <|
            comment "" <|
              eNewCanvas in

  -- going through parser so that new location ids are assigned
  let eNew = Utils.fromOk "Sync.inferStruct" (Parser.parseE (sExp eNew_)) in
  (eNew, Eval.run eNew)


------------------------------------------------------------------------------
-- "Dead Code Elimination"

type alias IndexTrace = (Loc, Loc, Int)

indexTraces : Trace -> List IndexTrace
indexTraces t = case t of
  TrOp (RangeOffset i) [TrLoc l1, TrLoc l2] -> [(l1, l2, i)]
  TrOp _ ts -> List.concatMap indexTraces ts
  _ -> []

sortIndexTraces (l1,u1,i1) (l2,u2,i2) =
  case compare (l1,u1) (l2,u2) of
    EQ  -> compare i1 i2
    ord -> ord

indexTracesOfVal : Val -> List IndexTrace
indexTracesOfVal v =
  let f v acc = case v of
    VConst (_, t) -> indexTraces t ++ acc
    _             -> acc
  in
  List.sortWith sortIndexTraces <| foldVal f v []

removeDeadIndices e v' =
  let idxTraces = indexTracesOfVal v' in
  let foo e_ = case e_ of
    EIndList rs -> EList (List.concatMap (expandRange idxTraces) rs) Nothing
    _           -> e_
  in
  mapExp foo e

expandRange idxTraces r =
  let mem = flip List.member idxTraces in
  case r.val of
    Point e -> case e.val of
      EConst n l ->
        if | mem (l,l,0) -> [e]
           | otherwise   -> []
    Interval e1 e2 -> case (e1.val, e2.val) of
      -- TODO may be better to just put exactly one space in
      -- between each element and ignore existing positions
      (EConst n1 l1, EConst n2 l2) ->
        let d = ceiling (n2 - n1) in
        let foo i (nextStart,acc) =
          let return a =
            let end = Un.bumpCol (String.length (strNum a)) nextStart in
            let ei  = P.WithInfo (EConst a dummyLoc) nextStart end in
            (Un.incCol end, ei :: acc)
          in
          let m = n1 + toFloat i in
          if | mem (l1,l2,i) && m > n2 -> return n2
             | mem (l1,l2,i)           -> return m
             | otherwise               -> (nextStart, acc)
        in
        snd <| List.foldr foo (e1.start, []) [0..d]

type alias MaybeOne a = List a
nothing               = []
just                  = Utils.singleton
maybeToMaybeOne mx    = case mx of {Nothing -> nothing; Just x -> just x}

inferDeleteUpdate : Exp -> Val -> Val -> MaybeOne (Exp, Val)
inferDeleteUpdate eOld v v' =
  let (attrs1,children1) = stripSvg v in
  let (attrs2,children2) = stripSvg v' in
  let _ = Utils.assert "Sync.inferDeleteUpdate" (attrs1 == attrs2) in

  let onlyDeletes =
    let foo (vi,vi') =
      if | vi == vi'                  -> Nothing
         | vi' == LangSvg.dummySvgVal -> Just True
         | otherwise                  -> Just False
    in
    let l = List.map foo (Utils.zip children1 children2) in
    List.length (List.filter ((==) (Just True)) l) > 0
      && List.length (List.filter ((==) (Just False)) l) == 0
  in

  if not onlyDeletes then nothing
  else
    -- freshen is needed b/c EConsts have been added (and removed)
    let eNew = Parser.freshen <| removeDeadIndices eOld v' in
    just (eNew, Eval.run eNew)


------------------------------------------------------------------------------
-- "Relate"

stripSvgNode : Bool -> Bool -> String -> Val -> Maybe (List Val)
stripSvgNode b1 b2 k v =
  case v of
    VList [VBase (String k'), VList vs1, VList vs2] ->
      case (k == k', b1, vs1, b2, vs2) of
        (True, True, _, False, []) -> Just vs1
        (True, False, [], True, _) -> Just vs2
        _                          -> Nothing
    _ ->
      Nothing

stripAttrs    = stripSvgNode True False
stripChildren = stripSvgNode False True

justBind = flip Utils.bindMaybe

getAttr : List Val -> String -> Maybe Val
getAttr l k = case l of
  [] -> Nothing
  VList [VBase (String k'), v] :: l' -> if
    | k == k'   -> Just v
    | otherwise -> getAttr l' k

getAttrs : List String -> List Val -> Maybe (List Val)
getAttrs ks l = Utils.projJusts <| List.map (getAttr l) ks

basicRectAttrs     = ["x","y","width","height","fill"]
getBasicRectAttrs  = getAttrs basicRectAttrs

pluckOut attrLists i = List.map (Utils.geti i) attrLists

unzipBasicRectAttrs attrLists =
  List.map (pluckOut attrLists) [1 .. List.length basicRectAttrs]

sortRectsByX = List.sortBy (valToNum << Utils.fromJust << flip getAttr "x")
sortRectsByY = List.sortBy (valToNum << Utils.fromJust << flip getAttr "y")

collectExtraRectAttrs rects =
  let f attrs acc0 =
    let g (VList [VBase (String k), _]) acc1 =
      if | List.member k basicRectAttrs -> acc1
         | otherwise                    -> k :: acc1
    in
    List.foldl g acc0 attrs
  in
  Utils.removeDupes <| List.foldl f [] rects

makeExtraRectAttrDicts rects =
  let ks = collectExtraRectAttrs rects in
  let processKey k =
    let processRect (i,attrs) acc =
      case getAttr attrs k of
        Nothing -> acc
        Just v  -> (i,v) :: acc
    in
    let indexedVals = Utils.foldri processRect [] rects in
    let table = strDictOfIndexedVals indexedVals in
    (k, table)
  in
  Utils.bracks <|
  String.join "\n                 " <|
    List.map
      (\(k,strTable) -> Utils.bracks (Utils.spaces [strVal (vStr k), strTable]))
      (List.map processKey ks)

pluckOutExtra attrLists k =
  Utils.foldri <| \(i,attrs) acc ->
    case getAttr attrs k of
      Nothing -> acc
      Just v  -> (i,v) :: acc

a `nl` b = a ++ "\n" ++ b

strCall f xs = Utils.parens (Utils.spaces (f::xs))

strInferred cap x ys =
  strCall "inferred" [x, cap, Utils.bracks (Utils.spaces ys)]

-- could switch to most common element if desired
--
chooseFirst (v::vs) =
  if | Utils.allSame (v::vs) -> strVal v
     | otherwise ->
         strInferred "'first of'" (strVal v) (List.map strVal (v::vs))

-- TODO this is duplicating toNum for Val rather than AVal...
valToNum v = case v of
  VConst (n,_) -> n
  VBase (String s) ->
    case String.toFloat s of
      Ok n -> n

chooseAvg vals =
  let nums = List.map valToNum vals in
  if | Utils.allSame nums -> toString (Utils.head_ nums)
     | otherwise ->
         let avg  = round <| Utils.avg nums in
         strInferred "'average of'" (toString avg) (List.map toString nums)

{-
lookupWithDefault def vals =
  let foo (i,v) = Utils.bracks (Utils.spaces [toString (i-1), strVal v]) in
  let s = Utils.bracks (Utils.spaces (Utils.mapi foo vals)) in
  strCall "lookupWithDefault" [toString def, "i", s]
-}

strDictOf vals =
  let foo (i,v) = Utils.bracks (Utils.spaces [toString (i-1), strVal v]) in
  Utils.bracks (Utils.spaces (Utils.mapi foo vals))

strDictOfIndexedVals indexedVals =
  Utils.bracks <| Utils.spaces <|
    List.map
      (\(i,v) -> Utils.bracks <| Utils.spaces [toString (i-1), strVal v])
      indexedVals

-- returns Nothing if not sorted (either non-decreasing or non-increasing)
--
baseAndOffset vals =
  let nums   = List.map valToNum vals in
  let pairs  = Utils.adjacentPairs False nums in
  let deltas = List.map (\(a,b) -> b-a) pairs in
  if not (List.all ((<=) 0) deltas || List.all ((>=) 0) deltas) then Nothing
  else
    let
      base   = strInferred "'smallest'"
                 (toString <| Utils.head_ nums)
                 (List.map toString nums)
      offset = strInferred "'average delta between'"
                 (toString <| round <| Utils.avg <| List.reverse deltas)
                 (List.map toString nums)
     in
     Just (base, offset)

inferXY xy vals =
  let
    xyBase          = xy ++ "Base"
    xyOff           = xy ++ "Off"
    xyBasePlusOff   = "(+ " ++ xyBase ++ " (mult i " ++ xyOff ++ "))"
    xyTable         = xy ++ "Table"
    xyLookup        = strCall "lookupWithDefault" ["10", "i", xyTable]
  in
  case baseAndOffset vals of
    Just (base,off) ->
      let
        s1 = "    (let " ++ xyBase ++ "  " ++ base            `nl`
             "    (let " ++ xyOff ++ "   " ++ off             `nl` ""
        s2 = "    (let " ++ xy ++ "      " ++ xyBasePlusOff   `nl` ""
      in
      (s1, s2, ")))")
    Nothing ->
      let
        s1 = "    (let " ++ xyTable ++ " " ++ strDictOf vals  `nl` ""
        s2 = "    (let " ++ xy ++ "      " ++ xyLookup        `nl` ""
      in
      (s1, s2, "))")

inferRelatedRectsX : Exp -> Val -> Val -> Maybe (Exp, Val)
inferRelatedRectsX = inferRelatedRects sortRectsByX "'right'"

inferRelatedRectsY : Exp -> Val -> Val -> Maybe (Exp, Val)
inferRelatedRectsY = inferRelatedRects sortRectsByY "'down'"

inferRelatedRects sortRectsByXY flow _ _ v' =
  stripChildren "svg" v' `justBind` (\shapes ->
  let mRects = List.map (stripAttrs "rect") shapes in
  Utils.projJusts mRects `justBind` (\rects_ ->
  let rects = sortRectsByXY rects_ in
  Utils.projJusts (List.map getBasicRectAttrs rects) `justBind` (\attrLists ->
    let n = List.length attrLists in
    let indices = Utils.ibracks (Utils.spaces (List.map toString [0..n-1])) in
    let flowIndices = strCall "flow" [flow, "indices"] in
    let [xs, ys, widths, heights, fills] = unzipBasicRectAttrs attrLists in
    let (let_xBaseAndOff, let_x, xParens) = inferXY "x" xs in
    let (let_yBaseAndOff, let_y, yParens) = inferXY "y" ys in
    let xyParens = xParens ++ yParens in
    let extraAttrDicts = makeExtraRectAttrDicts rects in
    let theRect =
      if extraAttrDicts == "[]" then
        "      (rect fill x y width height)"
      else
        "    (let extras " ++ extraAttrDicts                    `nl`
        "      (addExtras i extras"                             `nl`
        "        (rect fill x y width height)))"
    in
    let s =
      "(def newGroup"                                           `nl`
      "  (let indices " ++ indices                              `nl`
      "  (groupMap " ++ flowIndices ++ " (\\i"                  `nl`
            let_xBaseAndOff                                      ++
            let_yBaseAndOff                                      ++
            let_x                                                ++
            let_y                                                ++
      "    (let width  " ++ chooseAvg widths                    `nl`
      "    (let height " ++ chooseAvg heights                   `nl`
      "    (let fill   " ++ chooseFirst fills                   `nl`
             theRect ++ ")))))))" ++ xyParens                   `nl`
      ""                                                        `nl`
      "(svg newGroup)"
    in
    let eNew = Utils.fromOk_ <| Parser.parseE s in
    let vNew = Eval.run eNew in
    Just (eNew, vNew)
  )))

inferNewRelationships e v v' =
     maybeToMaybeOne (inferRelatedRectsX e v v')
  ++ maybeToMaybeOne (inferRelatedRectsY e v v')


------------------------------------------------------------------------------
-- Triggers

-- NOTE: AttrNames include "fake" attributes
--   e.g. for polygons, x1,y1,x2,y2,x3,y3,...

type alias AttrName = String
type alias LocSet = Set.Set Loc
type alias Locs = List Loc

-- band-aids for extra metadata...
type ExtraInfo = None | NumPoints Int | NumsPath LangSvg.PathCounts
type alias ExtraExtraInfo = List (AttrName, (Zone, Trace))

type alias NumAttrs = Int

type alias Dict0 = Dict NodeId (ShapeKind, ExtraInfo, ExtraExtraInfo, Dict AttrName Trace)
type alias Dict1 = Dict NodeId (ShapeKind, List (Zone, (NumAttrs, List Locs)))
type alias Dict2 = Dict NodeId (ShapeKind, List (Zone, Maybe (Locs, List Locs)))

printZoneTable : Val -> String
printZoneTable v =
  let so = defaultOptions in
  nodeToAttrLocs v           -- Step 1: Val   -> Dict0
    |> shapesToZoneTable so  -- Step 2: Dict0 -> Dict1
    |> assignTriggers        -- Step 3: Dict1 -> Dict2
    |> strTable              -- Step 4: Dict2 -> String

-- Step 1 --

-- TODO: assigning IDs is now redundant with valToIndexedTree.
-- so start with IndexedTree rather than Val.

nodeToAttrLocs : Val -> Dict0
nodeToAttrLocs = snd << flip nodeToAttrLocs_ (1, Dict.empty)

nodeToAttrLocs_ : Val -> (Int, Dict0) -> (Int, Dict0)
nodeToAttrLocs_ v (nextId,dShapes) = case v of

  VList [VBase (String "TEXT"), VBase (String s)] ->
    (1 + nextId, Dict.insert 1 ("DUMMYTEXT", None, [], Dict.empty) dShapes)

  VList [VBase (String kind), VList vs', VList children] ->

    -- processing attributes of current node
    let processAttr v' (extra,extraextra,dAttrs) = case v' of

      VList [VBase (String "fill"), VConst (_,tr)] ->
        let ee = ("fill", ("FillBall", tr)) :: extraextra in
        (extra, ee, Dict.insert "fill" tr dAttrs)

      -- NOTE: requires for a single cmd, and "transformRot" is a fake attr....
      VList [VBase (String "transform"),
             VList [VList [VBase (String "rotate"), VConst (_, tr), _, _]]] ->
        let ee = ("transformRot", ("RotateBall", tr)) :: extraextra in
        (extra, ee, Dict.insert "transformRot" tr dAttrs)

      VList [VBase (String a), VConst (_,tr)] ->
        (extra, extraextra, Dict.insert a tr dAttrs)

      VList [VBase (String "points"), VList pts] ->
        let acc' =
          Utils.foldli (\(i,vPt) acc ->
            case vPt of
              VList [VConst (_,trx), VConst (_,try)] ->
                let (ax,ay) = (addi "x" i, addi "y" i) in
                acc |> Dict.insert ax trx
                    |> Dict.insert ay try) dAttrs pts in
        (NumPoints (List.length pts), extraextra, acc')

      VList [VBase (String "d"), VList vs] ->
        let addPt (mi,(xt,yt)) dict =
          case mi of
            Nothing -> dict
            Just i  -> dict |> Dict.insert (addi "x" i) (snd xt)
                            |> Dict.insert (addi "y" i) (snd yt)
        in
        let addPts pts dict = List.foldl addPt dict pts in
        let (cmds,counts) = LangSvg.valsToPath2 vs in
        let dAttrs' =
          List.foldl (\c acc -> case c of
            LangSvg.CmdZ   s              -> acc
            LangSvg.CmdMLT s pt           -> acc |> addPt pt
            LangSvg.CmdHV  s n            -> acc
            LangSvg.CmdC   s pt1 pt2 pt3  -> acc |> addPts [pt1,pt2,pt3]
            LangSvg.CmdSQ  s pt1 pt2      -> acc |> addPts [pt1,pt2]
            LangSvg.CmdA   s a b c d e pt -> acc |> addPt pt) dAttrs cmds
        in
        (NumsPath counts, extraextra, dAttrs')

      -- NOTE:
      --   string-valued and RGBA attributes are ignored.
      --   see LangSvg.valToSvg for spec of attributes.
      _ ->
        (extra, extraextra, dAttrs)
    in
    let (extra,ee,attrs) = List.foldl processAttr (None, [], Dict.empty) vs' in

    -- recursing into sub-nodes
    let (nextId',dShapes') =
      List.foldl nodeToAttrLocs_ (nextId,dShapes) children in

    (nextId' + 1, Dict.insert nextId' (kind, extra, ee, attrs) dShapes')

  _ -> Debug.crash <| "Sync.nodeToAttrLocs_: " ++ strVal v

-- Step 2 --

-- TODO
--   equations are no longer always solvable.
--   so perhaps (symbolically) take into account whether a
--   solution may be Nothing (e.g. because of a division)
--   when computing which Locs may be assigned to a zone.
--   this would go after the cartProdWithDiff...
--   would also need to take into account whether an equation
--   is "top-down solvable" w.r.t to the desired location...

shapesToZoneTable : Options -> Dict0 -> Dict1
shapesToZoneTable opts d0 =
  let foo i stuff acc =
    let (kind,_,_,_) = stuff in
    Dict.insert i (kind, shapeToZoneInfo opts stuff) acc in
  Dict.foldl foo Dict.empty d0

shapeToZoneInfo :
  Options ->
  (ShapeKind, ExtraInfo, ExtraExtraInfo, Dict AttrName Trace) ->
  List (Zone, (NumAttrs, List Locs))
shapeToZoneInfo opts (kind, extra, ee, d) =
  let zones = getZones kind extra ee in
  let f (s,l) acc =
    let numAttrs = List.length l in
    let sets =
      -- temporary way to ignore numbers specified as strings
      -- l |> List.map (\a -> locsOfTrace opts <| justGet_ "%1" a d)
      l |> List.map (\a -> case Dict.get a d of
                             Just tr -> locsOfTrace opts tr
                             Nothing -> Set.empty)
        |> createLocLists in
    (s, (numAttrs, sets)) :: acc
  in
  List.foldr f [] zones

allowOverConstrained = True -- CONFIG

createLocLists sets =
  -- let foo = Utils.cartProdWithDiff sets in
  let removeEmpties = List.filter ((/=) 0 << Utils.setCardinal) in
  let foo = Utils.cartProdWithDiff (removeEmpties sets) in
  let bar =
    if | not allowOverConstrained -> []
       | otherwise ->
           sets |> Utils.intersectMany |> Set.toList |> List.map Utils.singleton
  in
  foo ++ bar

getZones : ShapeKind -> ExtraInfo -> ExtraExtraInfo -> List (Zone, List AttrName)
getZones kind extra ee =
  let xy i = [addi "x" i, addi "y" i] in
  let pt i = (addi "Point" i, xy i) in
  let edge n i =
    if | i <  n -> (addi "Edge" i, xy i ++ xy (i+1))
       | i == n -> (addi "Edge" i, xy i ++ xy 1) in
  let interior n = ("Interior", List.concatMap xy [1..n]) in
  let basicZones =
    case (kind, extra) of
      ("polyline", NumPoints n) ->
        List.map pt [1..n] ++ List.map (edge n) [1..n-1]
      ("polygon", NumPoints n) ->
        List.map pt [1..n] ++ List.map (edge n) [1..n] ++ [interior n]
      ("path", NumsPath {numPoints}) ->
        List.map pt [1..numPoints]
      _ ->
        Utils.fromJust_
          ("Sync.getZones " ++ kind)
          (Utils.maybeFind kind LangSvg.zones)
  in
  basicZones ++ widgetZones ee

widgetZones = List.map <| \x -> case x of
  ("fill"         , ("FillBall"   , _)) -> ("FillBall"   , ["fill"])
  ("transformRot" , ("RotateBall" , _)) -> ("RotateBall" , ["transformRot"])

-- Step 3 --

-- NOTE: choosing same name setSeen for both accumulators leads
--       to JS undefined error. perhaps due to a shadowing bug?

getTriggerType numAttrs locs =
  let n = List.length locs in
  if | n == numAttrs -> ()
     | n == 1        -> ()

-- TODO now that there are singleton loc-sets, need a better
-- way to try to cover them

{-
  old approach:
    if all locsets in rankedSets have been assigned at least once,
    then just pick the first set in rankedSets.possible sets have already.

  new approach:
    evenly distribute the number of times each locset is assigned.
-}

assignTriggers : Dict1 -> Dict2
assignTriggers = assignTriggersV2

assignTriggersV2 d1 =
  let f i (kind,zoneLists) (dictSetSeen1,acc) =
    let g (zone,(numAttrs,sets)) (dictSetSeen2,acc) =
      -- let rankedSets = List.sortBy scoreOfLocs sets in
      let rankedSets = sets in
      let maybeChosenSet =
        List.foldl (\thisSet acc ->
          case acc of
            Nothing -> Just thisSet
            Just bestSet ->
            if | getCount bestSet dictSetSeen2 < getCount thisSet dictSetSeen2 -> acc
               | otherwise -> Just thisSet) Nothing rankedSets in
      case maybeChosenSet of
        Nothing -> (dictSetSeen2, (zone, Nothing) :: acc)
        Just chosenSet ->
          (updateCount chosenSet dictSetSeen2, (zone, Just (chosenSet, rankedSets)) :: acc)
    in
    let (dictSetSeen,zoneLists') = List.foldl g (dictSetSeen1,[]) zoneLists in
    (dictSetSeen, Dict.insert i (kind, List.reverse zoneLists') acc)
  in
  snd <| Dict.foldl f (Dict.empty, Dict.empty) d1

getCount set dict    = Maybe.withDefault 0 (Dict.get set dict)
updateCount set dict = Dict.insert set (1 + getCount set dict) dict

assignTriggersV1 : Dict1 -> Dict2
assignTriggersV1 d1 =
  let f i (kind,zoneLists) (setSeen1,acc) =
    let g (zone,(numAttrs,sets)) (setSeen2,acc) =
      -- let rankedSets = List.sortBy scoreOfLocs sets in
      let rankedSets = sets in
      let pred = not << flip Set.member setSeen2 in
      case (Utils.findFirst pred rankedSets, rankedSets) of
        (Nothing, [])         -> (setSeen2, (zone,Nothing)::acc)
        (Nothing, set::sets') ->
          let _ = getTriggerType numAttrs set in
          (setSeen2, (zone, Just (set, sets'))::acc)
        (Just x,  _)          ->
          let _ = getTriggerType numAttrs x in
          let setSeen3 = Set.insert x setSeen2 in
          let acc' = (zone, Just (x, Utils.removeFirst x rankedSets)) :: acc in
          (setSeen3, acc')
    in
    let (setSeen,zoneLists') = List.foldl g (setSeen1,[]) zoneLists in
    (setSeen, Dict.insert i (kind, List.reverse zoneLists') acc)
  in
  snd <| Dict.foldl f (Set.empty, Dict.empty) d1

scoreOfLocs : Locs -> Int
scoreOfLocs locs =
  let foo (_,b,mx) acc =
    let _ = Utils.assert "scoreOfLocs" (b == unann) in
    if | mx == ""  -> acc
       | otherwise -> acc + 1
  in
  -1 * (List.foldl foo 0 locs)

-- Step 4 --

strTable : Dict2 -> String
strTable d =
  Dict.toList d
    |> List.map (\(i,(kind,di)) ->
         let s1 = addi "Shape " i ++ " " ++ Utils.parens kind in
         let sRows = List.map strRow di in
         Utils.lines (s1::sRows))
    |> String.join "\n\n"

strRow (zone, m) = case m of
  Nothing -> String.padRight 18 ' ' zone
  Just (set,sets) ->
       String.padRight 18 ' ' zone
    ++ String.padRight 25 ' ' (if set == [] then "" else strLocs set)
    ++ Utils.spaces (List.map strLocs sets)

strLocs = Utils.braces << Utils.commas << List.map strLoc_

strLoc_ l =
  let (_,_,mx) = l in
  if | mx == ""  -> strLoc l
     | otherwise -> mx

------------------------------------------------------------------------------

type alias Triggers = Dict NodeId (Dict Zone (Maybe Trigger))
type alias Trigger  = List (AttrName, Num) -> (Exp, Dict NodeId (Dict AttrName Num))

type alias LiveInfo =
  { triggers    : Triggers
  , assignments : Dict NodeId (Dict Zone Locs)
  , initSubst   : Subst
  }

tryToBeSmart = False

prepareLiveUpdates : Options -> Exp -> Val -> LiveInfo
prepareLiveUpdates opts e v =
  let d0 = nodeToAttrLocs v in
  let d1 = shapesToZoneTable opts d0 in
  let d2 = assignTriggers d1 in
  let initSubst = Parser.substOf e in
    { triggers    = makeTriggers initSubst opts e d0 d2
    , assignments = zoneAssignments d2
    , initSubst   = initSubst
    }

-- TODO refactor Dict data structures above to make this more efficient

makeTriggers : Subst -> Options -> Exp -> Dict0 -> Dict2 -> Triggers
makeTriggers subst opts e d0 d2 =
  let f i (_,zones) =
    let g (zone,m) =
      Dict.insert zone <|
        case m of
          Nothing -> Nothing
          Just _  -> Just (makeTrigger opts e d0 d2 subst i zone) in
    List.foldl g Dict.empty zones in
  Dict.map f d2

makeTrigger : Options -> Exp -> Dict0 -> Dict2 -> Subst -> NodeId -> Zone -> Trigger
makeTrigger opts e d0 d2 subst i zone = \newAttrs ->
  -- TODO symbolically compute changes !!!
  -- once this is done, might be able to rank trigger sets by int/float
  let (subst',changedLocs) =
    let f (attr,newNum) (acc1,acc2) =
      {- 6/25: now that assigned locs do not appear in every attribute,
               whichLoc may return Nothing
      let k = whichLoc opts d0 d2 i zone attr in
      let subst' = Dict.remove k subst in
      let tr = justGet attr (Utils.thd3 (justGet i d0)) in
      case solve subst' (newNum, tr) of
        -- solve will no longer always return an answer, so one of
        -- the locations assigned to this trigger may not have an
        -- effect after all... (see Dict1 comment)
        Nothing -> (acc1, acc2)
        Just kSolution -> (Dict.insert k kSolution acc1, Set.insert k acc2)
      -}
      case whichLoc opts d0 d2 i zone attr of
        Nothing -> (acc1, acc2)
        Just k ->
          let subst' = Dict.remove k subst in
          let tr = justGet_ "%2" attr (Utils.fourth4 (justGet_ "%3" i d0)) in
          case solve subst' (newNum, tr) of
            -- solve will no longer always return an answer, so one of
            -- the locations assigned to this trigger may not have an
            -- effect after all... (see Dict1 comment)
            Nothing -> (acc1, acc2)
            Just kSolution -> (Dict.insert k kSolution acc1, Set.insert k acc2)
    in
    List.foldl f (subst, Set.empty) newAttrs in
    -- if using overconstrained triggers, then some of the newAttr values
    -- from the UI make be "immediately destroyed" by subsequent ones...

  let g i (_,_,_,di) acc =
    let h attr tr acc =
      let locs = Set.map Utils.fst3 (locsOfTrace opts tr) in
      if | Utils.setIsEmpty (locs `Set.intersect` changedLocs) -> acc
         | otherwise -> Dict.insert attr (evalTr subst' tr) acc

         -- updating "points" based on fake attributes would be easier
         -- if the Little representation simply kept separate attributes
         -- for each point...
         {-
         | otherwise ->
             let default () = Dict.insert attr (evalTr subst' tr) acc in
             case String.uncons attr of
               Nothing -> default ()
               Just (pre,suf) ->
                 if pre == 'x' || pre == 'y' then
                   case String.toInt suf of
                     Err _ -> default ()
                     Ok i  -> Debug.log "SYNC: change other point..." <| default ()
                 else
                   default()
        -}
    in
    -- let di' = Dict.foldl h Dict.empty di in
    let di' =
      if | tryToBeSmart -> Dict.foldl h Dict.empty di
         | otherwise    -> Dict.empty in
    if | Utils.dictIsEmpty di' -> acc
       | otherwise -> Dict.insert i di' acc in
  let e' = applySubst subst' e in
  (e', Dict.foldl g Dict.empty d0)

-- TODO sloppy way of doing this for now...
whichLoc : Options -> Dict0 -> Dict2 -> NodeId -> Zone -> AttrName -> Maybe LocId
whichLoc opts d0 d2 i z attr =
  let trLocs =
    -- temporary way to ignore numbers specified as strings
    -- justGet_ "%4" i d0 |> Utils.thd3 |> justGet_ "%5" attr |> locsOfTrace opts in
    justGet_ "%4" i d0 |> Utils.fourth4 |> Dict.get attr |>
      \m -> case m of
        Just tr -> locsOfTrace opts tr
        Nothing -> Set.empty in
  let zoneLocs =
    justGet_ "%6" i d2
      |> snd |> Utils.maybeFind z |> Utils.fromJust
      |> Utils.fromJust_ "guaranteed not to fail b/c of check in makeTriggers"
      |> fst |> Set.fromList in
  case Set.toList (trLocs `Set.intersect` zoneLocs) of
    [(k,_,_)] -> Just k
    []        -> Nothing
    _         -> Debug.crash "whichLoc"

evalTr subst tr = Utils.fromJust_ "evalTr" (evalTrace subst tr)

------------------------------------------------------------------------------

-- TODO compute this along with everything else
-- could also make this a single dictionary: Dict (NodeId, Zone) Locs
zoneAssignments : Dict2 -> Dict NodeId (Dict Zone Locs)
zoneAssignments =
  Dict.map <| \i (_,l) ->
    List.foldl (\(z,m) acc ->
      case m of
        Just (locs,_) -> Dict.insert z locs acc
        Nothing       -> acc
    ) Dict.empty l

