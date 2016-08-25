module ExpressionBasedTransform -- in contrast to ValueBasedTransform
  ( groupSelectedBlobs
  , abstractSelectedBlobs
  , duplicateSelectedBlobs
  , mergeSelectedBlobs
  , deleteSelectedBlobs
  , anchorOfSelectedFeatures
  ) where

import Lang exposing (..)
import LangUnparser exposing (unparse)
import LangSvg exposing (NodeId, ShapeFeature)
import Blobs exposing (..)
import Types
import InterfaceModel exposing (Model, SelectedType)
import InterfaceView2 as View
import Utils
import Keys

import Dict exposing (Dict)
import Set


--------------------------------------------------------------------------------
-- Group Blobs with Bounding Box

computeSelectedBlobsAndBounds : Model -> Dict Int (NumTr, NumTr, NumTr, NumTr)
computeSelectedBlobsAndBounds model =
  let tree = snd model.slate in
  Dict.map
     (\blobId nodeId ->
       case Dict.get nodeId tree of

         -- refactor the following cases for readability

         Just (LangSvg.SvgNode "BOX" nodeAttrs _) ->
           let get attr = LangSvg.maybeFindAttr nodeId "BOX" attr nodeAttrs in
           case List.map .v_ [get "LEFT", get "TOP", get "RIGHT", get "BOT"] of
             [VConst left, VConst top, VConst right, VConst bot] ->
               (left, top, right, bot)
             _ -> Debug.crash "computeSelectedBlobsAndBounds"

         Just (LangSvg.SvgNode "OVAL" nodeAttrs _) ->
           let get attr = LangSvg.maybeFindAttr nodeId "OVAL" attr nodeAttrs in
           case List.map .v_ [get "LEFT", get "TOP", get "RIGHT", get "BOT"] of
             [VConst left, VConst top, VConst right, VConst bot] ->
               (left, top, right, bot)
             _ -> Debug.crash "computeSelectedBlobsAndBounds"

         Just (LangSvg.SvgNode "g" nodeAttrs _) ->
           case LangSvg.maybeFindBounds nodeAttrs of
             Just bounds -> bounds
             Nothing     -> Debug.crash "computeSelectedBlobsAndBounds"

         Just (LangSvg.SvgNode "line" nodeAttrs _) ->
           let get attr = LangSvg.maybeFindAttr nodeId "line" attr nodeAttrs in
           case List.map .v_ [get "x1", get "y1", get "x2", get "y2"] of
             [VConst x1, VConst y1, VConst x2, VConst y2] ->
               (minNumTr x1 x2, minNumTr y1 y2, maxNumTr x1 x2, maxNumTr y1 y2)
             _ -> Debug.crash "computeSelectedBlobsAndBounds"

         -- "ellipse" and "circle" aren't handled nicely by grouping

         Just (LangSvg.SvgNode "ellipse" nodeAttrs _) ->
           let get attr = LangSvg.maybeFindAttr nodeId "ellipse" attr nodeAttrs in
           case List.map .v_ [get "cx", get "cy", get "rx", get "ry"] of
             [VConst cx, VConst cy, VConst rx, VConst ry] ->
               (cx `minusNumTr` rx, cy `minusNumTr` ry, cx `plusNumTr` rx, cy `plusNumTr` ry)
             _ -> Debug.crash "computeSelectedBlobsAndBounds"

         Just (LangSvg.SvgNode "circle" nodeAttrs _) ->
           let get attr = LangSvg.maybeFindAttr nodeId "circle" attr nodeAttrs in
           case List.map .v_ [get "cx", get "cy", get "r"] of
             [VConst cx, VConst cy, VConst r] ->
               (cx `minusNumTr` r, cy `minusNumTr` r, cx `plusNumTr` r, cy `plusNumTr` r)
             _ -> Debug.crash "computeSelectedBlobsAndBounds"

         Just (LangSvg.SvgNode "rect" nodeAttrs _) ->
           let get attr = LangSvg.maybeFindAttr nodeId "rect" attr nodeAttrs in
           case List.map .v_ [get "x", get "y", get "width", get "height"] of
             [VConst x, VConst y, VConst width, VConst height] ->
               (x, y, x `plusNumTr` width, y `plusNumTr` height)
             _ -> Debug.crash "computeSelectedBlobsAndBounds"

         _ -> Debug.crash "computeSelectedBlobsAndBounds"
     )
     model.selectedBlobs

selectedBlobsToSelectedNiceBlobs : Model -> List BlobExp -> List (Int, Exp, NiceBlob)
selectedBlobsToSelectedNiceBlobs model blobs =
  let selectedExps =
    List.filter (flip Dict.member model.selectedBlobs << fst)
                (Utils.zip [1 .. List.length blobs] blobs)
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
        CallBlob _       -> False
    in
    case Utils.findFirst foo selectedNiceBlobs of
      Just (_, _, VarBlob x) -> Just x
      _                      -> Nothing
  in
  let (_,p,_,_) = def in
  case p.val of
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
        CallBlob (f, _)          -> f == y
    in
    case Utils.findFirst foo selectedNiceBlobs of
      Just (_, _, CallBlob (f, _))          -> Just f
      Just (_, _, WithBoundsBlob (_, f, _)) -> Just f
      _                                     -> Nothing
  in
  let (_,p,_,_) = def in
  case p.val of
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

groupSelectedBlobs model defs blobs f =
  let n = List.length blobs in
  let selectedNiceBlobs = selectedBlobsToSelectedNiceBlobs model blobs in
  let newGroup = "newGroup" ++ toString model.genSymCount in
  let (defs', blobs') = groupAndRearrange model newGroup defs blobs selectedNiceBlobs in
  -- let code' = Debug.log "newGroup" <| unparse (fuseExp (defs', Blobs blobs' f)) in
  let code' = unparse (fuseExp (defs', Blobs blobs' f)) in
  -- upstate Run
    { model | code = code'
            , genSymCount = model.genSymCount + 1
            , selectedBlobs = Dict.empty
            }

groupAndRearrange model newGroup defs blobs selectedNiceBlobs =
  let selectedBlobsAndBounds = computeSelectedBlobsAndBounds model in
  let (pluckedBlobs, beforeBlobs, afterBlobs) =
    let indexedBlobs = Utils.zip [1 .. List.length blobs] blobs in
    let matches (i,_) = Dict.member i model.selectedBlobs in
    let (plucked_, before_, after_) = pluckFromList matches indexedBlobs in
    (List.map snd plucked_, List.map snd before_, List.map snd after_)
  in
  let defs' =
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
             let someVarAppearsIn e = List.any (\x -> occursFreeIn x e) vars in
             let noVarAppearsIn e = List.all (\x -> not (occursFreeIn x e)) vars in
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
    let (width, height) = (fst right - fst left, fst bot - fst top) in
    let scaleX  = scaleXY  "left" "right" left width in
    let scaleY  = scaleXY  "top"  "bot"   top  height in
    let offsetX = offsetXY "left" "right" left right in
    let offsetY = offsetXY "top"  "bot"   top  bot in
    let eSubst =
      -- the spaces inserted by calls to offset*/scale* work best
      -- when the source expressions being rewritten are of the form
      --   (let [a b c d] [na nb nc nd] ...)
      let foo i acc =
        let (a,b,c,d) = Utils.justGet i selectedBlobsAndBounds in
        if model.keysDown == Keys.shift then
          acc |> offsetX "" a |> offsetY " " b |> offsetX " " c |> offsetY " " d
        else
          -- acc |> scaleX "" a |> scaleY " " b |> scaleX " " c |> scaleY " " d
          acc |> scaleX " " a |> scaleY " " b |> scaleX " " c |> scaleY " " d
      in
      List.foldl foo Dict.empty selectedBlobIndices
    in
    let groupDefs =
      [ ( "\n  "
        , pAs "bounds" (pList (listOfPVars ["left", "top", "right", "bot"]))
        , eList (listOfNums [fst left, fst top, fst right, fst bot]) Nothing
        , "")
      ]
    in
    let listBoundedGroup =
      let pluckedBlobs' =
        List.map (LangUnparser.replacePrecedingWhitespace " " << fromBlobExp)
                 pluckedBlobs
      in
      withDummyPos <| EList "\n\n  "
         [ withDummyPos <| EApp " "
             (eVar0 "group")
             [ eVar "bounds"
             , withDummyPos <| EApp " "
                 (eVar0 "concat")
                 [withDummyPos <| EList " " pluckedBlobs' "" Nothing " "]
                 ""
             ]
             ""
         ]
         "" Nothing " "
    in
    let pluckedDefs' =
      let tab = "  " in
      List.map (\(ws1,p,e,ws2) -> (ws1 ++ tab, p, LangUnparser.indent tab e, ws2))
               pluckedDefs
    in
    let newGroupExp =
      applyESubst eSubst <|
        fuseExp (groupDefs ++ pluckedDefs', OtherExp listBoundedGroup)
          -- TODO flag for fuseExp to insert lets instead of defs
    in
    let newDef = ("\n\n", pVar newGroup, newGroupExp, "") in
    -- beforeDefs ++ [newDef] ++ afterDefs
    beforeDefs ++ afterDefs ++ [newDef]
  in
  let blobs' =
    let newBlob = varBlob (withDummyPos (EVar "\n  " newGroup)) newGroup in
    beforeBlobs ++ [newBlob] ++ afterBlobs
  in
  (defs', blobs')

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

scaleXY start end startVal widthOrHeight ws (n,t) eSubst =
  case t of
    TrLoc (locid,_,_) ->
      let pct = (n - fst startVal) / widthOrHeight in
      let app =
        if pct == 0 then ws ++ start
        else if pct == 1 then ws ++ end
        else
          ws ++ Utils.parens (Utils.spaces ["scaleBetween", start, end, toString pct]) in
      Dict.insert locid (eRaw__ "" app) eSubst
    _ ->
      eSubst

-- TODO for scaleXY and offsetXY, forgo function call when on
-- boundaries to improve readability of generated code
-- (falls back in on little prelude encoding)

offsetXY base1 base2 baseVal1 baseVal2 ws (n,t) eSubst =
  case t of
    TrLoc (locid,_,_) ->
      let (off1, off2) = (n - fst baseVal1, n - fst baseVal2) in
      let (base, off) =
        if off1 <= abs off2 then (base1, off1) else (base2, off2) in
      let app =
        ws ++ Utils.parens (Utils.spaces
                [ "evalOffset"
                , Utils.bracks (Utils.spaces [base, toString off])]) in
      Dict.insert locid (eRaw__ "" app) eSubst
    _ ->
      eSubst

-- TODO for now, just checking occursIn
occursFreeIn : Ident -> Exp -> Bool
occursFreeIn x e =
  let vars =
    foldExpViaE__ (\e__ acc ->
      case e__ of
        EVar _ x -> Set.insert x acc
        _        -> acc
      ) Set.empty e
  in
  Set.member x vars


--------------------------------------------------------------------------------
-- Group Blobs with Anchor

anchorOfSelectedFeatures
  : Set.Set (SelectedType, NodeId, ShapeFeature)
 -> Result String (Maybe (NodeId, ())) -- TODO
anchorOfSelectedFeatures selectedFeatures =
  let err = Err "To group around an anchor, need to select exactly one point." in
  case Set.toList selectedFeatures of
    [] -> Ok Nothing
    [("shapeFeature", id1, feature1), ("shapeFeature", id2, feature2)] ->
      if id1 /= id2 then err
      else if feature1 == LangSvg.rectTLX && feature2 == LangSvg.rectTLY then
        Ok (Just (id1, ()))
      else if feature2 == LangSvg.rectTLX && feature1 == LangSvg.rectTLY then
        Ok (Just (id1, ()))
      else
        err
    _ ->
      err


--------------------------------------------------------------------------------
-- Abstract Blob

selectedBlobsToSelectedVarBlobs : Model -> List BlobExp -> List (Int, Exp, Ident)
selectedBlobsToSelectedVarBlobs model blobs =
  List.concatMap
     (\(i,e,niceBlob) ->
       case niceBlob of
         VarBlob x        -> [(i, e, x)]
         WithBoundsBlob _ -> []
         CallBlob _       -> []
     )
     (selectedBlobsToSelectedNiceBlobs model blobs)

abstractSelectedBlobs model =
  let (defs,mainExp) = splitExp model.inputExp in
  case mainExp of
    Blobs blobs f ->
      -- silently ignoring WithBoundsBlobs
      let selectedVars = selectedBlobsToSelectedVarBlobs model blobs in
      let (defs',blobs') = List.foldl abstractOne (defs, blobs) selectedVars in
      let code' = unparse (fuseExp (defs', Blobs blobs' f)) in
      -- upstate Run
        { model | code = code'
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
      pluckFromList matches (Utils.zip [1 .. List.length blobs] blobs) in
    (List.map snd plucked, List.map snd before, List.map snd after) in

  case (pluckedDefs, pluckedBlobs) of

    ([(ws1,p,e,ws2)], [NiceBlob _ (VarBlob x)]) ->

      let (e', mapping) = collectUnfrozenConstants e in
      let (newDef, newBlob) =
        case findBoundsInMapping mapping of

          Just (restOfMapping, left, top, right, bot) ->
            let newFunc =
              let pBounds =
                let pVars = listOfPVars ["left", "top", "right", "bot"] in
                case restOfMapping of
                  [] -> pList0 pVars
                  _  -> pList  pVars
              in
              let params = listOfPVars (List.map fst restOfMapping) in
              withDummyPos (EFun " " (params ++ [pBounds]) e' "")
            in
            let eBounds = eList (listOfAnnotatedNums [left, top, right, bot]) Nothing in
            let newCall =
              let eBlah =
                case listOfAnnotatedNums1 (List.map snd restOfMapping) of
                  []   -> eVar x
                  args -> withDummyPos (EApp " " (eVar0 x) args "")
              in
              withDummyPos (EApp "\n  " (eVar0 "with") [eBounds, eBlah] "")
            in
            let newBlob = NiceBlob newCall (WithBoundsBlob (eBounds, x, [])) in
            ((ws1, p, newFunc, ws2), newBlob)

          Nothing ->
            let newFunc =
              let params = listOfPVars (List.map fst mapping) in
              withDummyPos (EFun " " params e' "")
            in
            let newBlob =
              case listOfAnnotatedNums1 (List.map snd mapping) of
                []   -> varBlob (eVar x) x
                args ->
                  let newCall = withDummyPos (EApp "\n  " (eVar0 x) args "") in
                  callBlob newCall (x, args)
            in
            ((ws1, p, newFunc, ws2), newBlob)
      in
      let defs' = beforeDefs ++ [newDef] ++ afterDefs in
      let blobs' = beforeBlobs ++ [newBlob] ++ afterBlobs in
      (defs', blobs')

    _ ->
      let _ = Debug.log "abstractOne: multiple defs..." in
      (defs, blobs)

collectUnfrozenConstants : Exp -> (Exp, List (Ident, AnnotatedNum))
collectUnfrozenConstants e =
  -- extra first pass, as a quick and simple way to approximate name clashes
  let (_, list0) = collectUnfrozenConstants_ Nothing e in
  let varCounts =
    List.foldl (\var acc ->
      case Dict.get var acc of
        Nothing    -> Dict.insert var 1 acc
        Just count -> Dict.insert var (1 + count) acc
      ) Dict.empty (List.map fst list0)
  in
  let (e', list) = collectUnfrozenConstants_ (Just varCounts) e in
  (removeRedundantBindings e', List.reverse list)

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
  let e' = mapExpViaExp__ (fst << foo) e in
  let mapping = foldExpViaE__ ((++) << snd << foo) [] e in
  (e', mapping)

findBoundsInMapping : List (Ident, a) -> Maybe (List (Ident, a), a, a, a, a)
findBoundsInMapping mapping =
  case mapping of
    ("left", left) :: ("top", top) :: ("right", right) :: ("bot", bot) :: rest ->
      Just (rest, left, top, right, bot)
    _ ->
      Nothing

removeRedundantBindings =
  mapExp <| \e ->
    case e.val.e__ of
      ELet _ _ _ p e1 e2 _ -> if redundantBinding (p, e1) then e2 else e
      _                    -> e

redundantBinding (p, e) =
  case (p.val, e.val.e__) of
    (PConst _ n, EConst _ n' _ _) -> n == n'
    (PBase _ bv, EBase _ bv')     -> bv == bv'
    (PVar _ x _, EVar _ x')       -> x == x'

    (PList _ ps _ Nothing _, EList _ es _ Nothing _) ->
      List.all redundantBinding (Utils.zip ps es)
    (PList _ ps _ (Just p) _, EList _ es _ (Just e) _) ->
      List.all redundantBinding (Utils.zip (p::ps) (e::es))

    _ -> False


--------------------------------------------------------------------------------
-- Delete Blobs

deleteSelectedBlobs model =
  let (defs,mainExp) = splitExp model.inputExp in
  case mainExp of
    Blobs blobs f ->
      let blobs' =
        Utils.filteri
           (\(i,_) -> not (Dict.member i model.selectedBlobs))
           blobs
      in
      let code' = unparse (fuseExp (defs, Blobs blobs' f)) in
      -- upstate Run
        { model | code = code'
              , selectedBlobs = Dict.empty
              }
    _ ->
      model


--------------------------------------------------------------------------------
-- Duplicate Blobs

duplicateSelectedBlobs model =
  let (defs,mainExp) = splitExp model.inputExp in
  case mainExp of
    Blobs blobs f ->
      let (nextGenSym, newDefs, newBlobs) =
        let selectedNiceBlobs = selectedBlobsToSelectedNiceBlobs model blobs in
        let (nextGenSym_, newDefs_, newVarBlobs_) =
          List.foldl
             (\def (k,acc1,acc2) ->
               if not (matchesAnySelectedVarBlob selectedNiceBlobs def)
               then (k, acc1, acc2)
               else
                 let (ws1,p,e,ws2) = def in
                 case p.val of
                   PVar ws x wd ->
                     let x' = x ++ "_copy" ++ toString k in
                     let acc1' = (ws1, { p | val = PVar ws x' wd }, e, ws2) :: acc1 in
                     let acc2' = varBlob (withDummyPos (EVar "\n  " x')) x' :: acc2 in
                     (1 + k, acc1', acc2')
                   _ ->
                     let _ = Debug.log "duplicateSelectedBlobs: weird..." () in
                     (k, acc1, acc2)
             )
             (model.genSymCount, [], [])
             defs
        in
        let newWithAndCallBlobs =
          List.concatMap
             (\(_,e,niceBlob) ->
               case niceBlob of
                 WithBoundsBlob _ -> [NiceBlob e niceBlob]
                 CallBlob _       -> [NiceBlob e niceBlob]
                 VarBlob _        -> []
             )
             selectedNiceBlobs
        in
        let newDefs = List.reverse newDefs_ in
        let newBlobs = List.reverse newVarBlobs_ ++ newWithAndCallBlobs in
        (nextGenSym_, newDefs, newBlobs)
      in
      let code' =
        let blobs' = blobs ++ newBlobs in
        let defs' = defs ++ newDefs in
        unparse (fuseExp (defs', Blobs blobs' f))
      in
      -- upstate Run
        { model | code = code'
                , genSymCount = List.length newBlobs + model.genSymCount
                }
    _ ->
      model

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
        let (defs', blobs') = mergeSelectedVarBlobs model defs blobs selectedVarBlobs in
        let code' = unparse (fuseExp (defs', Blobs blobs' f)) in
        -- upstate Run
          { model | code = code'
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
      pluckFromList matches (Utils.zip [1 .. List.length blobs] blobs) in
    (List.map snd plucked, List.map snd before, List.map snd after) in

  let ((ws1,p,e,ws2),es) =
    case pluckedDefs of
      def::defs' -> (def, List.map (\(_,_,e,_) -> e) defs')
      []         -> Debug.crash "mergeSelectedVarBlobs: shouldn't get here" in

  case mergeExpressions e es of
    Nothing ->
      -- let _ = Debug.log "mergeExpressions Nothing" () in
      (defs, blobs)

    Just (_, []) ->
      let defs' = beforeDefs ++ [(ws1,p,e,ws2)] ++ afterDefs in
      let blobs' = beforeBlobs ++ [Utils.head_ pluckedBlobs] ++ afterBlobs in
      (defs', blobs')

    Just (eMerged, multiMapping) ->

      -- TODO treat bounds variables specially, as in abstract

      let newDef =
        let newFunc =
          let params = listOfPVars (List.map fst multiMapping) in
          withDummyPos (EFun " " params (removeRedundantBindings eMerged) "") in
        (ws1, p, newFunc, ws2) in

      let f =
        case p.val of
          PVar _ x _ -> x
          _          -> Debug.crash "mergeSelected: not var" in

      let newBlobs =
        case Utils.maybeZipN (List.map snd multiMapping) of
          Nothing -> Debug.crash "mergeSelected: no arg lists?"
          Just numLists ->
            -- let _ = Debug.log "numLists:" numLists in
            List.map
               (\nums ->
                  let args = listOfAnnotatedNums1 nums in
                  let e = withDummyPos <| EApp "\n  " (eVar0 f) args "" in
                  callBlob e (f, args)
               ) numLists in

      let defs' = beforeDefs ++ [newDef] ++ afterDefs in
      let blobs' = beforeBlobs ++ newBlobs ++ afterBlobs in
      (defs', blobs')

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
        case Utils.dedup_ annotatedNumToComparable allAnnotatedNums of
          [_] -> return eFirst.val.e__ []
          _   ->
            let var = if x == "" then "k" ++ toString locid else x in
            -- let _ = Debug.log "var for merge: " (var, n::nums) in
            return (EVar ws1 var) [(var, allAnnotatedNums)]

    EBase _ bv ->
      let match eNext = case eNext.val.e__ of
        EBase _ bv' -> Just bv'
        _           -> Nothing
      in
      matchAllAndBind match eRest <| \bvs ->
        if List.all ((==) bv) bvs then return eFirst.val.e__ [] else Nothing

    EVar _ x ->
      let match eNext = case eNext.val.e__ of
        EVar _ x' -> Just x'
        _         -> Nothing
      in
      matchAllAndBind match eRest <| \xs ->
        if List.all ((==) x) xs then return eFirst.val.e__ [] else Nothing

    EFun ws1 ps eBody ws2 ->
      let match eNext = case eNext.val.e__ of
        EFun _ ps' eBody' _ -> Just (ps', eBody')
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (psList, eBodyList) = List.unzip stuff in
        Utils.bindMaybe2
          (\() (eBody',list) -> return (EFun ws1 ps eBody' ws2) list)
          (mergePatternLists (ps::psList))
          (mergeExpressions eBody eBodyList)

    EApp ws1 eFunc eArgs ws2 ->
      let match eNext = case eNext.val.e__ of
        EApp _ eFunc' eArgs' _ -> Just (eFunc', eArgs')
        _                      -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (eFuncList, eArgsList) = List.unzip stuff in
        Utils.bindMaybe2
          (\(eFunc',l1) (eArgs',l2) ->
            return (EApp ws1 eFunc' eArgs' ws2) (l1 ++ l2))
          (mergeExpressions eFunc eFuncList)
          (mergeExpressionLists (eArgs::eArgsList))

    ELet ws1 letKind rec p1 e1 e2 ws2 ->
      let match eNext = case eNext.val.e__ of
        ELet _ _ _ p1' e1' e2' _ -> Just ((p1', e1'), e2')
        _                        -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((p1List, e1List), e2List) =
          Utils.mapFst List.unzip (List.unzip stuff)
        in
        Utils.bindMaybe3
          (\_ (e1',l1) (e2',l2) ->
            return (ELet ws1 letKind rec p1 e1' e2' ws2) (l1 ++ l2))
          (mergePatterns p1 p1List)
          (mergeExpressions e1 e1List)
          (mergeExpressions e2 e2List)

    EList ws1 es ws2 me ws3 ->
      let match eNext = case eNext.val.e__ of
        EList _ es' _ me' _ -> Just (es', me')
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (esList, meList) = List.unzip stuff in
        Utils.bindMaybe2
          (\(es',l1) (me',l2) -> return (EList ws1 es' ws2 me' ws3) (l1 ++ l2))
          (mergeExpressionLists (es::esList))
          (mergeMaybeExpressions me meList)

    EOp ws1 op es ws2 ->
      let match eNext = case eNext.val.e__ of
        EOp _ op' es' _ -> Just (op', es')
        _               -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (opList, esList) = List.unzip stuff in
        if List.all ((==) op.val) (List.map .val opList) then
          Utils.bindMaybe
            (\(es',l) -> return (EOp ws1 op es' ws2) l)
            (mergeExpressionLists (es::esList))
        else
          Nothing

    EIf ws1 e1 e2 e3 ws2 ->
      let match eNext = case eNext.val.e__ of
        EIf _ e1' e2' e3' _ -> Just ((e1', e2'), e3')
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((e1List, e2List), e3List) = Utils.mapFst List.unzip (List.unzip stuff) in
        Utils.bindMaybe3
          (\(e1',l1) (e2',l2) (e3',l3) ->
            return (EIf ws1 e1' e2' e3' ws2) (l1 ++ l2 ++ l3))
          (mergeExpressions e1 e1List)
          (mergeExpressions e2 e2List)
          (mergeExpressions e3 e3List)

    EComment ws s e ->
      let match eNext = case eNext.val.e__ of
        EComment _ _ e' -> Just e'
        _               -> Nothing
      in
      matchAllAndBind match eRest <| \es ->
        Utils.bindMaybe
          (\(e',l) -> return (EComment ws s e') l)
          (mergeExpressions e es)

    ETyp ws1 pat tipe e ws2 ->
      let match eNext = case eNext.val.e__ of
        ETyp _ pat tipe e _ -> Just ((pat, tipe), e)
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((patList, typeList), eList) =
          Utils.mapFst List.unzip (List.unzip stuff)
        in
        Utils.bindMaybe3
          (\_ _ (e',l) ->
            return (ETyp ws1 pat tipe e' ws2) l)
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
          (\(e',l) _ ->
            return (EColonType ws1 e' ws2 tipe ws3) l)
          (mergeExpressions e eList)
          (mergeTypes tipe typeList)

    ETypeAlias ws1 pat tipe e ws2 ->
      let match eNext = case eNext.val.e__ of
        ETypeAlias _ pat tipe e _ -> Just ((pat, tipe), e)
        _                         -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((patList, typeList), eList) =
          Utils.mapFst List.unzip (List.unzip stuff)
        in
        Utils.bindMaybe3
          (\_ _ (e',l) ->
            return (ETypeAlias ws1 pat tipe e' ws2) l)
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
              Just (e',l) -> Just (acc1 ++ [e'], acc2 ++ l)
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
        (Utils.mapMaybe (Utils.mapFst Just) << mergeExpressions e)
        (Utils.projJusts mes)

mergePatterns : Pat -> List Pat -> Maybe ()
mergePatterns pFirst pRest =
  case pFirst.val of
    PVar _ x _ ->
      let match pNext = case pNext.val of
        PVar _ x' _ -> Just x'
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest x
    PConst _ n ->
      let match pNext = case pNext.val of
        PConst _ n' -> Just n'
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest n
    PBase _ bv ->
      let match pNext = case pNext.val of
        PBase _ bv' -> Just bv'
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest bv
    PList _ ps _ mp _ ->
      let match pNext = case pNext.val of
        PList _ ps' _ mp' _ -> Just (ps', mp')
        _                   -> Nothing
      in
      matchAllAndBind match pRest <| \stuff ->
        let (psList, mpList) = List.unzip stuff in
        Utils.bindMaybe2
          (\_ () -> Just ())
          (mergePatternLists (ps::psList))
          (mergeMaybePatterns mp mpList)
    PAs _ x _ p ->
      let match pNext = case pNext.val of
        PAs _ x' _ p' -> Just (x', p')
        _             -> Nothing
      in
      matchAllAndBind match pRest <| \stuff ->
        let (indentList, pList) = List.unzip stuff in
        Utils.bindMaybe
          (\() -> if List.all ((==) x) indentList then Just () else Nothing)
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
    IntSlider a _ b _ -> (n, frzn, toFloat a.val, toFloat b.val)
    NumSlider a _ b _ -> (n, frzn, a.val, b.val)
    NoWidgetDecl      -> (n, frzn, 1, -1)
