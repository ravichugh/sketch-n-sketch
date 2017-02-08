module ExpressionBasedTransform exposing -- in contrast to ValueBasedTransform
  ( passiveSynthesisSearch
  , groupSelectedBlobs
  , abstractSelectedBlobs
  , replicateSelectedBlob
  , duplicateSelectedBlobs
  , mergeSelectedBlobs
  , deleteSelectedBlobs
  , anchorOfSelectedFeatures
  , groupSelectedBlobsAround
  )

import Lang exposing (..)
import LangUnparser exposing (unparse)
import LangParser2 exposing (parseE)
import LangSvg exposing (NodeId)
import ShapeWidgets exposing (PointFeature, SelectedShapeFeature)
import Blobs exposing (..)
import LangTools exposing (..)
import LangTransform
import Types
import InterfaceModel exposing (Model, ReplicateKind(..))
import Utils
import Keys

import Dict exposing (Dict)
import Set
import String


--------------------------------------------------------------------------------
-- Expression Rewriting

passiveSynthesisSearch : Exp -> List InterfaceModel.SynthesisResult
passiveSynthesisSearch originalExp =
  let argVar = eVar "INSERT_ARGUMENT_HERE" in
  -- Sister function in LangTools.extraExpsDiff
  let merge expA expB =
    case (expA.val.e__, expB.val.e__) of
      (EConst ws1A nA locA wdA,              EConst ws1B nB locB wdB)              -> if nA == nB then expA else argVar
      (EBase ws1A (EBool True),              EBase ws1B (EBool True))              -> expA
      (EBase ws1A (EBool False),             EBase ws1B (EBool False))             -> expA
      (EBase ws1A (EString qcA strA),        EBase ws1B (EString qcB strB))        -> if strA == strB then expA else argVar
      (EBase ws1A ENull,                     EBase ws1B ENull)                     -> expA
      (EVar ws1A identA,                     EVar ws1B identB)                     -> if identA == identB then expA else argVar
      (EFun ws1A psA eA ws2A,                EFun ws1B psB eB ws2B)                -> if patternListsEqual psA psB then replaceE__ expA (EFun ws1A psA (merge eA eB) ws2A) else argVar
      (EOp ws1A opA esA ws2A,                EOp ws1B opB esB ws2B)                -> if opA.val == opB.val then Utils.maybeZip esA esB |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EOp ws1A opA newEs ws2A))) |> Maybe.withDefault argVar else argVar
      (EList ws1A esA ws2A Nothing ws3A,     EList ws1B esB ws2B Nothing ws3B)     -> Utils.maybeZip esA esB |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EList ws1A newEs ws2A Nothing ws3A))) |> Maybe.withDefault argVar
      (EList ws1A esA ws2A (Just eA) ws3A,   EList ws1B esB ws2B (Just eB) ws3B)   -> Utils.maybeZip esA esB |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EList ws1A newEs ws2A (Just (merge eA eB)) ws3A))) |> Maybe.withDefault argVar
      (EApp ws1A fA esA ws2A,                EApp ws1B fB esB ws2B)                -> Utils.maybeZip esA esB |> Maybe.map (List.map (\(eA, eB) -> merge eA eB) >> (\newEs -> replaceE__ expA (EApp ws1A (merge fA fB) newEs ws2A))) |> Maybe.withDefault argVar
      (ELet ws1A kindA recA pA e1A e2A ws2A, ELet ws1B kindB recB pB e1B e2B ws2B) -> if recA == recB && patternsEqual pA pB then replaceE__ expA (ELet ws1A kindA recA pA (merge e1A e1B) (merge e2A e2B) ws2A) else argVar
      (EIf ws1A e1A e2A e3A ws2A,            EIf ws1B e1B e2B e3B ws2B)            -> replaceE__ expA (EIf ws1A (merge e1A e1B) (merge e2A e2B) (merge e3A e3B) ws2A)
      (ECase ws1A eA branchesA ws2A,         ECase ws1B eB branchesB ws2B)         -> Utils.maybeZip branchesA branchesB |> Maybe.andThen (\branchPairs -> let bValPairs = branchPairs |> List.map (\(bA, bB) -> (bA.val, bB.val)) in if bValPairs |> List.all (\(Branch_ bws1A bpatA beA bws2A, Branch_ bws1B bpatB beB bws2B) -> patternsEqual bpatA bpatB) then Just (replaceE__ expA (ECase ws1A (merge eA eB) (Utils.zip branchPairs bValPairs |> List.map (\((bA, bB), (Branch_ bws1A bpatA beA bws2A, Branch_ bws1B bpatB beB bws2B)) -> {bA | val = Branch_ bws1A bpatA (merge beA beB) bws2A})) ws2A)) else Nothing) |> Maybe.withDefault argVar
      (ETypeCase ws1A patA tbranchesA ws2A,  ETypeCase ws1B patB tbranchesB ws2B)  -> if patternsEqual patA patB then Utils.maybeZip tbranchesA tbranchesB |> Maybe.andThen (\tbranchPairs -> let tbValPairs = tbranchPairs |> List.map (\(tbA, tbB) -> (tbA.val, tbB.val)) in if tbValPairs |> List.all (\(TBranch_ tbws1A tbtypeA tbeA tbws2A, TBranch_ tbws1B tbtypeB tbeB tbws2B) -> Types.equal tbtypeA tbtypeB) then Just (replaceE__ expA (ETypeCase ws1A patA (Utils.zip tbranchPairs tbValPairs |> List.map (\((tbA, tbB), (TBranch_ tbws1A tbtypeA tbeA tbws2A, TBranch_ tbws1B tbtypeB tbeB tbws2B)) -> {tbA | val = TBranch_ tbws1A tbtypeA (merge tbeA tbeB) tbws2A})) ws2A)) else Nothing) |> Maybe.withDefault argVar else argVar
      (EComment wsA sA e1A,                  _)                                    -> replaceE__ expA (EComment wsA sA (merge e1A expB))
      (_,                                    EComment wsB sB e1B)                  -> replaceE__ expB (EComment wsB sB (merge e1B expA))
      (EOption ws1A s1A ws2A s2A e1A,        EOption ws1B s1B ws2B s2B e1B)        -> argVar
      (ETyp ws1A patA typeA eA ws2A,         ETyp ws1B patB typeB eB ws2B)         -> if patternsEqual patA patB && Types.equal typeA typeB then replaceE__ expA (ETyp ws1A patA typeA (merge eA eB) ws2A) else argVar
      (EColonType ws1A eA ws2A typeA ws3A,   EColonType ws1B eB ws2B typeB ws3B)   -> if Types.equal typeA typeB then replaceE__ expA (EColonType ws1A (merge eA eB) ws2A typeA ws3A) else argVar
      (ETypeAlias ws1A patA typeA eA ws2A,   ETypeAlias ws1B patB typeB eB ws2B)   -> if patternsEqual patA patB && Types.equal typeA typeB then replaceE__ expA (ETypeAlias ws1A patA typeA (merge eA eB) ws2A) else argVar
      _                                                                            -> argVar
  in
  let argVarCount exp =
    flattenExpTree exp
    |> Utils.count
        (\exp ->
          case exp.val.e__ of
            EVar _ ident -> ident == "INSERT_ARGUMENT_HERE"
            _            -> False
        )
  in
  let goodMatch exp = nodeCount exp >= 3 && argVarCount exp == 1 in
  flattenExpTree originalExp
  |> List.foldl
      (\exp mergeGroups ->
        let newMergeGroups =
          mergeGroups
          |> List.filterMap
              (\(priorMerged, priorExps) ->
                let newMerged = merge priorMerged exp in
                if goodMatch newMerged then
                  Just (newMerged, exp::priorExps)
                else
                  Nothing
              )
        in
        (exp, [exp])::(mergeGroups ++ newMergeGroups)
      )
      []
  |> List.filter (\(merged, exps) -> List.length exps >= 3)
  -- |> (\filtered -> let _ = Debug.log ("merge groups >= 3 exps:\n" ++ String.join "\n" (filtered |> List.map Tuple.second |> List.map (List.map (unparse >> Utils.squish) >> String.join ", "))) () in filtered)
  |> List.filter (\(merged, exps) -> exps |> List.concatMap (\exp -> extraExpsDiff merged exp) |> List.all isLiteral) -- No free variables in the part of the expression to parameterize
  |> List.map
      (\(merged, exps) ->
        -- TODO: add [0 1 2] -> (zeroTo 3) transform
        -- TODO: add attempts to transform [a, x1, x2, x3, z] into [a, (concat xs), z]
        -- TODO: lift dependencies
        -- TODO: Split abstraction -> mapping into two steps
        -- TODO: ensure abstracted expressions weren't sub-expressions of each other...?
        let sortedExps  = exps |> List.sortBy (\exp -> (exp.start.line, exp.start.col)) in
        let eidsToReplace = sortedExps |> List.map (.val >> .eid) in
        let commonScope = justInsideDeepestCommonScope originalExp (\exp -> List.member exp.val.eid eidsToReplace) in
        let varBaseName =
          case merged.val.e__ of
            EApp _ funE _ _ ->
              case funE.val.e__ of
                EVar _ ident -> ident
                _            -> "newVar"
            _ -> "newVar"
        in
        let parameterExps =
          sortedExps
          |> List.map
              (\exp ->
                case extraExpsDiff merged exp of
                  [parameterExp] -> parameterExp
                  _              -> Debug.crash <| "ExpressionBasedTransform.passiveSynthesisSearch parameterExps\nmerged: " ++ (unparse >> Utils.squish) merged ++ "\nexps:" ++ String.join ", " (List.map (unparse >> Utils.squish) sortedExps)
              )
        in
        let mappingFunc =
          let funcExp =
            -- Curry, if body is simple enough
            let explicitFunc = eFun [pVar0 "INSERT_ARGUMENT_HERE"] (unindent merged) in
            case merged.val.e__ of
              (EApp ws1 funcE args ws2) ->
                case Utils.takeLast 1 args of
                  [lastArg] ->
                    case lastArg.val.e__ of
                      EVar _ "INSERT_ARGUMENT_HERE" ->
                        if List.length args >= 2 then
                          replaceE__ merged (EApp ws1 funcE (List.take (List.length args - 1) args) ws2)
                        else
                          replacePrecedingWhitespace ws1 funcE

                      _ ->
                        explicitFunc
                  _ ->
                    explicitFunc

              _ ->
                explicitFunc
          in
          let argBaseName =
            let maybeParamNums = parameterExps |> List.map expToMaybeNum |> Utils.projJusts in
            case maybeParamNums of
              Just paramNums -> if List.map (round >> toFloat) paramNums == paramNums && Utils.dedup paramNums == paramNums && (List.member 0 paramNums || List.member 1 paramNums) && List.all (\n -> 0 <= n && n < 10) paramNums then "i" else "n"
              Nothing        -> "arg"
          in
          let argName = nonCollidingName argBaseName 2 (identifiersSet funcExp) in
          renameIdentifier "INSERT_ARGUMENT_HERE" argName funcExp
        in
        let mapCall =
          -- Multiline or single line map call, depending on mapping function
          if String.contains "\n" (unparse mappingFunc) then
            let oldIndentation = indentationOf commonScope in
            let newLineIndent extraIndent exp = replacePrecedingWhitespace ("\n" ++ extraIndent ++ oldIndentation) exp in
            eApp
                (eVar0 "map")
                [ replacePrecedingWhitespace " " (indent ("    " ++ oldIndentation) mappingFunc) -- Put arguments on same line as map call.
                , newLineIndent "    " (eTuple (cleanupListWhitespace " " parameterExps))
                ]
            |> newLineIndent "  "
          else
            eApp (eVar0 "map") [mappingFunc, eTuple (cleanupListWhitespace " " parameterExps)]
        in
        let namesToAvoid = identifiersSet commonScope in
        let varNames = sortedExps |> Utils.mapi1 (\(i, _) -> nonCollidingName (varBaseName ++ toString i) 2 namesToAvoid) in
        let eidToVarE__ = Utils.zip eidsToReplace (varNames |> List.map (\name -> EVar " " name)) |> Dict.fromList in
        let usagesReplaced = applyESubstPreservingPrecedingWhitespace eidToVarE__ commonScope in
        let wrapped =
          let letKind = if isTopLevel commonScope originalExp then Def else Let in
          withDummyPos <| ELet "\n" letKind False (pListOfPVars varNames) mapCall usagesReplaced ""
        in
        let newProgram = replaceExpNode commonScope.val.eid wrapped originalExp in
        { description = "Abstract " ++ String.join ", " (sortedExps |> List.map (unparse >> Utils.squish >> Utils.niceTruncateString 25 "...")) ++ " into " ++ (unparse >> Utils.squish >> Utils.niceTruncateString 40 "...") mapCall
        , exp         = newProgram
        , sortKey     = []
        }
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
    let listGroup =
      let pluckedBlobs_ =
        List.map ( replacePrecedingWhitespace " " << fromBlobExp)
                 pluckedBlobs
      in
      finalExpOfNewGroup pluckedBlobs_
    in
    let pluckedDefs_ =
      let tab = "  " in
      List.map (\(ws1,p,e,ws2) -> (ws1 ++ tab, p,  indent tab e, ws2))
               pluckedDefs
    in
    let newGroupExp =
      applyESubst eSubst <|
        fuseExp (groupDefs ++ pluckedDefs_, OtherExp listGroup)
          -- TODO flag for fuseExp to insert lets instead of defs
    in
    let newDef = ("\n\n", pVar newGroup, newGroupExp, "") in
    -- beforeDefs ++ [newDef] ++ afterDefs
    beforeDefs ++ afterDefs ++ [newDef]
  in
  let blobs_ =
    let newBlob = varBlob (withDummyPos (EVar "\n  " newGroup)) newGroup in
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

scaleXY start end startVal widthOrHeight ws (n,t) eSubst =
  case t of
    TrLoc (locid,_,_) ->
      let pct = (n - Tuple.first startVal) / widthOrHeight in
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
      let (off1, off2) = (n - Tuple.first baseVal1, n - Tuple.first baseVal2) in
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
         withDummyPos <| EList "\n\n  "
           [ withDummyPos <| EApp " "
               (eVar0 "group")
               [ eVar "bounds"
               , withDummyPos <| EApp " "
                   (eVar0 "concat")
                   [withDummyPos <| EList " " pluckedBlobs_ "" Nothing " "]
                   ""
               ]
               ""
           ]
           "" Nothing " "
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
       case Dict.get nodeId tree of

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
      , eList (listOfNums [Tuple.first left, Tuple.first top, Tuple.first right, Tuple.first bot]) Nothing
      , "")
    ]
  in
  (groupDefs, eSubst)


--------------------------------------------------------------------------------
-- Rewrite and Group Blobs with Anchor

anchorOfSelectedFeatures
    : Set.Set SelectedShapeFeature
   -> Result String (Maybe (NodeId, PointFeature))
anchorOfSelectedFeatures selectedFeatures =
  let err = Err "To group around an anchor, need to select exactly one point." in
  case Set.toList selectedFeatures of
    [selected1, selected2] ->
      case ShapeWidgets.selectedPointFeatureOf selected1 selected2 of
        Just result -> Ok (Just result)
        Nothing     -> err
    [] -> Ok Nothing
    _  -> err


groupSelectedBlobsAround model (defs, blobs, f) (anchorId, anchorPointFeature) =
  let (anchorKind, anchorAttrs) =
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
                 withDummyPos <| EList "\n\n  "
                   [ withDummyPos <| EApp " "
                       (eVar0 "anchoredGroup")
                       [ withDummyPos <| EApp " "
                           (eVar0 "concat")
                           [withDummyPos <| EList " " pluckedBlobs_ "" Nothing " "]
                           ""
                       ]
                       ""
                   ]
                   "" Nothing " "
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
      ( "\n  "
      , pAs "anchor" (pList (listOfPVars [xAnchor, yAnchor]))
      , eAsPoint (eList (listOfNums [nxBase, nyBase]) Nothing)
      , ""
      )
  in
  let eSubst =
    pointsOfSelectedBlobs |> List.foldl (\(xOther, yOther) acc ->
      -- TODO when anchor is derived point, rewrite anchor shape appropriately
      if (nxBase, nyBase) == (Tuple.first xOther, Tuple.first yOther) then
        acc |> Dict.insert xId (EVar " " xAnchor)
            |> Dict.insert yId (EVar " " yAnchor)
      else
        case (xOther, yOther) of
          ( (nxOther, TrLoc (xOtherId,_,_))
          , (nyOther, TrLoc (yOtherId,_,_))
          ) ->
            acc |> Dict.insert xOtherId (eBaseOffset xAnchor (nxOther - nxBase))
                |> Dict.insert yOtherId (eBaseOffset yAnchor (nyOther - nyBase))
          _ ->
            acc
      ) Dict.empty
  in
  ([anchorDef], eSubst)


eBaseOffset baseVar offsetNum =
  let allowZeroOffsets = True in -- flag for: anchor or (+ anchor 0)
  if offsetNum == 0 && not allowZeroOffsets then
    EVar " " baseVar
  else
    ePlus (eVar baseVar) (eConst offsetNum (dummyLoc_ unann))
      |>  replacePrecedingWhitespace " "
      |> .val |> .e__


eAsPoint e =
  let insertPointAnnotations = False in -- Config param
  if not insertPointAnnotations then e
  else

  let e_ =  replacePrecedingWhitespace "" e in
  withDummyPos <|
    EColonType " " e_ " " (withDummyRange <| TNamed " " "Point") ""


pAsTight x p =
  let p_ =  replacePrecedingWhitespacePat "" p in
  withDummyRange <| PAs " " x "" p_


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
              withDummyPos (EFun " " (params ++ [pBounds]) e_ "")
            in
            let eBounds = eList (listOfAnnotatedNums [left, top, right, bot]) Nothing in
            let newCall =
              let eBlah =
                case listOfAnnotatedNums1 (List.map Tuple.second restOfMapping) of
                  []   -> eVar x
                  args -> withDummyPos (EApp "\n    " (eVar0 x) args "")
              in
              withDummyPos (EApp "\n  " (eVar0 "withBounds") [eBounds, eBlah] "")
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
              withDummyPos (EFun " " (params ++ [pAsTight "anchor" pBounds]) e_ "")
            in
            let eAnchor =
              eAsPoint (eList (listOfAnnotatedNums [xAnchor, yAnchor]) Nothing)
            in
            let newCall =
              let eBlah =
                case listOfAnnotatedNums1 (List.map Tuple.second restOfMapping) of
                  []   -> eVar x
                  args -> withDummyPos (EApp " " (eVar0 x) args "")
              in
              withDummyPos (EApp "\n  " (eVar0 "withAnchor") [eAnchor, eBlah] "")
            in
            let newBlob = NiceBlob newCall (WithAnchorBlob (eAnchor, x, [])) in
            ((ws1, p, newFunc, ws2), newBlob)

          Nothing ->
            let newFunc =
              let params = listOfPVars (List.map Tuple.first mapping) in
              withDummyPos (EFun " " params e_ "")
            in
            let newBlob =
              case listOfAnnotatedNums1 (List.map Tuple.second mapping) of
                []   -> varBlob (eVar x) x
                args ->
                  let newCall = withDummyPos (EApp "\n  " (eVar0 x) args "") in
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
      ELet _ _ _ p e1 e2 _ -> if redundantBinding (p, e1) then e2 else e
      _                    -> e

redundantBinding (p, e) =
  case (p.val, e.val.e__) of
    (PConst _ n, EConst _ n_ _ _) -> n == n_
    (PBase _ bv, EBase _ bv_)     -> bv == bv_
    (PVar _ x _, EVar _ x_)       -> x == x_

    (PList _ ps _ Nothing _, EList _ es _ Nothing _) ->
      List.all redundantBinding (Utils.zip ps es)
    (PList _ ps _ (Just p) _, EList _ es _ (Just e) _) ->
      List.all redundantBinding (Utils.zip (p::ps) (e::es))

    (_, EColonType _ e1 _ _ _) -> redundantBinding (p, e1)

    _ -> False

clean =
  removeRedundantBindings << LangTransform.simplify


--------------------------------------------------------------------------------
-- Replicate Blob

replicateSelectedBlob replicateKind model (defs, blobs, f) =
  case selectedBlobsToSelectedNiceBlobs model blobs of

    [(i, _, WithAnchorBlob (anchor, g, args))] ->

      let eGroupFunc = withDummyPos <| EApp "\n    " (eVar0 g) args "" in
      let eAnchor =  replacePrecedingWhitespace "\n    " anchor in
      let (arrayFunction, arrayArgs) =
        case replicateKind of

          HorizontalRepeat ->
            let eNum = withDummyPos <| EConst " " 3 (dummyLoc_ frozen) (intSlider 1 20) in
            let eSep = withDummyPos <| EConst " " 20 dummyLoc noWidgetDecl in
            ("horizontalArray", [eNum, eSep, eGroupFunc, eAnchor])

          LinearRepeat ->
            let eNum   = withDummyPos <| EConst " " 3 (dummyLoc_ frozen) (intSlider 1 20) in
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
            let eNum    = withDummyPos <| EConst " " 3 (dummyLoc_ frozen) (intSlider 1 20) in
            let eRadius = withDummyPos <| EConst " " nRadius (dummyLoc_ unann) noWidgetDecl in
            let eRot    = withDummyPos <| EConst " " 0 (dummyLoc_ frozen) (numSlider 0 6.28) in
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
           (withDummyPos <| EApp "\n  " (eVar0 arrayFunction) arrayArgs "")
           (CallBlob (arrayFunction, arrayArgs))
      in
      let blobs_ = Utils.replacei i newBlob blobs in
      let code_ = unparse (fuseExp (defs, Blobs blobs_ f)) in
      { model | code = code_ , selectedBlobs = Dict.empty }

    [(i, _, WithBoundsBlob (bounds, g, args))] ->

      let eGroupFunc = withDummyPos <| EApp "\n    " (eVar0 g) args "" in
      let eBounds =  replacePrecedingWhitespace "\n    " bounds in
      let (arrayFunction, arrayArgs) =
        case replicateKind of

          HorizontalRepeat ->
            let eNum = withDummyPos <| EConst " " 3 (dummyLoc_ frozen) (intSlider 1 10) in
            let eSep = withDummyPos <| EConst " " 20 dummyLoc noWidgetDecl in
            ("horizontalArrayByBounds", [eNum, eSep, eGroupFunc, eBounds])

          LinearRepeat ->
            let (nNum, nSep) = (3, 20) in
            let eNum = withDummyPos <| EConst " " nNum (dummyLoc_ frozen) (intSlider 1 20) in
            let eSep = withDummyPos <| EConst " " nSep dummyLoc noWidgetDecl in
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
           (withDummyPos <| EApp "\n  " (eVar0 arrayFunction) arrayArgs "")
           (CallBlob (arrayFunction, arrayArgs))
      in
      let blobs_ = Utils.replacei i newBlob blobs in
      let code_ = unparse (fuseExp (defs, Blobs blobs_ f)) in
      { model | code = code_ , selectedBlobs = Dict.empty }

    _ -> model


stripPointExp e =
  case e.val.e__ of
    EList _ [ex,ey] _ Nothing _ ->
      case (ex.val.e__, ey.val.e__) of
        (EConst _ nx _ _, EConst _ ny _ _) -> Just (nx, ny)
        _                                  -> Nothing
    EColonType _ e_ _ _ _ -> stripPointExp e_
    _                     -> Nothing


stripBoundsExp e =
  case e.val.e__ of
    EList _ es _ Nothing _ ->
      case List.map (.val >> .e__) es of
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
                     let x_ = x ++ "_copy" ++ toString k in
                     let acc1_ = (ws1, { p | val = PVar ws x_ wd }, e, ws2) :: acc1 in
                     let acc2_ = varBlob (withDummyPos (EVar "\n  " x_)) x_ :: acc2 in
                     (1 + k, acc1_, acc2_)
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
                 WithAnchorBlob _ -> [NiceBlob e niceBlob]
                 CallBlob _       -> [NiceBlob e niceBlob]
                 VarBlob _        -> []
             )
             selectedNiceBlobs
        in
        let newDefs = List.reverse newDefs_ in
        let newBlobs = List.reverse newVarBlobs_ ++ newWithAndCallBlobs in
        (nextGenSym_, newDefs, newBlobs)
      in
      let code_ =
        let blobs_ = blobs ++ newBlobs in
        let defs_ = defs ++ newDefs in
        unparse (fuseExp (defs_, Blobs blobs_ f))
      in
      -- upstate Run
        { model | code = code_
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
          withDummyPos (EFun " " params (clean eMerged) "") in
        (ws1, p, newFunc, ws2) in

      let f =
        case p.val of
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
                  let e = withDummyPos <| EApp "\n  " (eVar0 f) args "" in
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

    EApp ws1 eFunc eArgs ws2 ->
      let match eNext = case eNext.val.e__ of
        EApp _ eFunc_ eArgs_ _ -> Just (eFunc_, eArgs_)
        _                      -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let (eFuncList, eArgsList) = List.unzip stuff in
        Utils.bindMaybe2
          (\(eFunc_,l1) (eArgs_,l2) ->
            return (EApp ws1 eFunc_ eArgs_ ws2) (l1 ++ l2))
          (mergeExpressions eFunc eFuncList)
          (mergeExpressionLists (eArgs::eArgsList))

    ELet ws1 letKind rec p1 e1 e2 ws2 ->
      let match eNext = case eNext.val.e__ of
        ELet _ _ _ p1_ e1_ e2_ _ -> Just ((p1_, e1_), e2_)
        _                        -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((p1List, e1List), e2List) =
          Utils.mapFst List.unzip (List.unzip stuff)
        in
        Utils.bindMaybe3
          (\_ (e1_,l1) (e2_,l2) ->
            return (ELet ws1 letKind rec p1 e1_ e2_ ws2) (l1 ++ l2))
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
          (\(es_,l1) (me_,l2) -> return (EList ws1 es_ ws2 me_ ws3) (l1 ++ l2))
          (mergeExpressionLists (es::esList))
          (mergeMaybeExpressions me meList)

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

    EIf ws1 e1 e2 e3 ws2 ->
      let match eNext = case eNext.val.e__ of
        EIf _ e1_ e2_ e3_ _ -> Just ((e1_, e2_), e3_)
        _                   -> Nothing
      in
      matchAllAndBind match eRest <| \stuff ->
        let ((e1List, e2List), e3List) = Utils.mapFst List.unzip (List.unzip stuff) in
        Utils.bindMaybe3
          (\(e1_,l1) (e2_,l2) (e3_,l3) ->
            return (EIf ws1 e1_ e2_ e3_ ws2) (l1 ++ l2 ++ l3))
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
          Utils.mapFst List.unzip (List.unzip stuff)
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
          Utils.mapFst List.unzip (List.unzip stuff)
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
        (Utils.mapMaybe (Utils.mapFst Just) << mergeExpressions e)
        (Utils.projJusts mes)


mergePatterns : Pat -> List Pat -> Maybe ()
mergePatterns pFirst pRest =
  case pFirst.val of
    PVar _ x _ ->
      let match pNext = case pNext.val of
        PVar _ x_ _ -> Just x_
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest x
    PConst _ n ->
      let match pNext = case pNext.val of
        PConst _ n_ -> Just n_
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest n
    PBase _ bv ->
      let match pNext = case pNext.val of
        PBase _ bv_ -> Just bv_
        _           -> Nothing
      in
      matchAllAndCheckEqual match pRest bv
    PList _ ps _ mp _ ->
      let match pNext = case pNext.val of
        PList _ ps_ _ mp_ _ -> Just (ps_, mp_)
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
        PAs _ x_ _ p_ -> Just (x_, p_)
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
