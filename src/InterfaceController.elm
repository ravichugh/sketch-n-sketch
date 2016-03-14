module InterfaceController (upstate) where

import Lang exposing (..) --For access to what makes up the Vals
import LangParser2 exposing (parseE, parseV)
import LangUnparser exposing (unparseE)
import Sync
import Eval
import Utils
import InterfaceModel exposing (..)
import InterfaceView2 exposing (..)
import InterfaceStorage exposing (installSaveState, removeDialog)
import LangSvg exposing (toNum, toNumTr, toPoints, addi)
import ExamplesGenerated as Examples
import Config exposing (params)

import VirtualDom

import Benchmark

--Core Libraries
import List
import Dict
import Set
import String
import Char
import Graphics.Element as GE
import Graphics.Collage as GC

--Html Libraries
import Html
import Html.Attributes as Attr
import Html.Events as Events

--Svg Libraries
import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

--Error Checking Libraries
import Debug

--------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugController

--------------------------------------------------------------------------------

slateToVal : LangSvg.RootedIndexedTree -> Val
slateToVal (rootId, tree) =
  let foo n =
    case n of
      LangSvg.TextNode s -> VList [VBase (String "TEXT"), VBase (String s)]
      LangSvg.SvgNode kind l1 l2 ->
        let vs1 = List.map LangSvg.valOfAttr l1 in
        let vs2 = List.map (foo << flip Utils.justGet tree) l2 in
        VList [VBase (String kind), VList vs1, VList vs2]
  in
  foo (Utils.justGet rootId tree)

upslate : LangSvg.NodeId -> (String, LangSvg.AVal) -> LangSvg.IndexedTree -> LangSvg.IndexedTree
upslate id newattr nodes = case Dict.get id nodes of
    Nothing   -> Debug.crash "upslate"
    Just node -> case node of
        LangSvg.TextNode x -> nodes
        LangSvg.SvgNode shape attrs children ->
            let newnode = LangSvg.SvgNode shape (Utils.update newattr attrs) children
            in Dict.insert id newnode nodes

refreshMode model e =
  case model.mode of
    Live _  -> mkLive_ model.syncOptions e
    Print _ -> mkLive_ model.syncOptions e
    m       -> m

refreshMode_ model = refreshMode model (Utils.fromJust model.inputExp)

refreshHighlights id zone model =
  let codeBoxInfo = model.codeBoxInfo in
  let hi = liveInfoToHighlights id zone model in
  { model | codeBoxInfo = { codeBoxInfo | highlights = hi } }

switchOrient m = case m of
  Vertical -> Horizontal
  Horizontal -> Vertical

toggleShowZones x = (1 + x) % showZonesModes

-- may want to eventually have a maximum history length
addToHistory s h = (s :: fst h, [])

-- this is a bit redundant with View.turnOn...
maybeStuff id shape zone m =
  case m.mode of
    Live info ->
      flip Utils.bindMaybe (Dict.get id info.assignments) <| \d ->
      flip Utils.bindMaybe (Dict.get zone d) <| \(yellowLocs,_) ->
        Just (info.initSubst, yellowLocs)
    _ ->
      Nothing

highlightChanges mStuff changes codeBoxInfo =
  case mStuff of
    Nothing -> codeBoxInfo
    Just (initSubstPlus, locs) ->

      let (hi,stringOffsets) =
        -- hi : List Highlight, stringOffsets : List (Pos, Int)
        --   where Pos is start pos of a highlight to offset by Int chars
        let f loc (acc1,acc2) =
          let (locid,_,_) = loc in
          let highlight c = makeHighlight initSubstPlus c loc in
          case (Dict.get locid initSubstPlus, Dict.get locid changes) of
            (Nothing, _)             -> Debug.crash "Controller.highlightChanges"
            (Just n, Nothing)        -> (highlight yellow :: acc1, acc2)
            (Just n, Just Nothing)   -> (highlight red :: acc1, acc2)
            (Just n, Just (Just n')) ->
              if n' == n.val then
                (highlight yellow :: acc1, acc2)
              else
                let (s, s') = (strNum n.val, strNum n') in
                let x = (acePos n.start, String.length s' - String.length s) in
                (highlight green :: acc1, x :: acc2)
        in
        List.foldl f ([],[]) (Set.toList locs)
      in

      let hi' =
        let g (startPos,extraChars) (old,new) =
          let bump pos = { pos | column = pos.column + extraChars } in
          let ret new' = (old, new') in
          ret <|
            if startPos.row    /= old.start.row         then new
            else if startPos.column >  old.start.column then new
            else if startPos.column == old.start.column then { start = new.start, end = bump new.end }
            else if startPos.column <  old.start.column then { start = bump new.start, end = bump new.end }
            else
              Debug.crash "highlightChanges"
        in
        -- hi has <= 4 elements, so not worrying about the redundant processing
        flip List.map hi <| \{color,range} ->
          let (_,range') = List.foldl g (range,range) stringOffsets in
          { color = color, range = range' }
      in

      { codeBoxInfo | highlights = hi' }


--------------------------------------------------------------------------------

clickToCanvasPoint old (mx, my) =
  let (xOrigin, yOrigin) = case old.orient of
    Vertical   -> canvasOriginVertical old
    Horizontal -> canvasOriginHorizontal old
  in
  (mx - xOrigin, my - yOrigin)

-- the computations of the top-left corner of the canvas
-- are based on copying the computations from View
-- TODO: refactor these

canvasOriginVertical old =
  let
    sideGut = params.topSection.h
    wGut    = params.mainSection.vertical.wGut
    wMiddle = params.mainSection.widgets.wBtn
    wCode_  = (fst old.dimensions - sideGut - sideGut - wMiddle - wGut - wGut) // 2
    wCode   = wCode_ + old.midOffsetX
  in
    ( sideGut + wCode + 2*wGut + wMiddle
    , params.topSection.h
    )

canvasOriginHorizontal old =
  -- TODO the y-position in horizontal mode is off by a few pixels
  -- TODO in View, the height of codebox isn't the same as the canvas.
  --   hMid is calculated weirdly in View...
  let
    hGut    = params.mainSection.horizontal.hGut
    hCode_  = (snd old.dimensions - hMid - 2*hGut) // 2 + hMid
    hCode   = hCode_ + old.midOffsetY
    -- TODO consider hideCode and hideCanvas
    hMid    = params.mainSection.widgets.hBtn
  in
    ( params.wGut
    , params.topSection.h + hCode + hMid
    )


--------------------------------------------------------------------------------
-- Updating the Model

upstate : Event -> Model -> Model
upstate evt old =
  case debugLog "Event" evt of

    Noop -> old

    WindowDimensions wh -> { old | dimensions = wh }

    Edit -> { old | editingMode = Just old.code }

    Run ->
      Benchmark.logDuration "Run" <|
        \() ->
          case parseE old.code of
            Ok e ->
             let h = case old.editingMode of
               Nothing -> old.history
               Just "" -> old.history -- "" from InterfaceStorage
               Just s  -> addToHistory s old.history
             in
             let (v,ws) = Eval.run e in
             let new =
              { old | inputExp = Just e
                    , code = unparseE e
                    , slate = LangSvg.valToIndexedTree v
                    , widgets = ws
                    , history = h
                    , editingMode = Nothing
                    , caption = Nothing
                    , syncOptions = Sync.syncOptionsOf old.syncOptions e }
              in
              { new | mode = refreshMode_ new
                    , errorBox = Nothing }
            Err err ->
              { old | caption = Just (LangError ("PARSE ERROR!\n" ++ err)) }

    ToggleOutput ->
      let m = case old.mode of
        Print _ -> refreshMode_ old
        _       -> Print (LangSvg.printSvg old.showGhosts old.slate)
      in
      { old | mode = m }

    CodeUpdate newcode -> { old | code = newcode }

    StartResizingMid -> { old | mouseMode = MouseResizeMid Nothing }

    BenchmarkExample exampleName ->
      let (_, _, code)  =
        Utils.fromJust <|
          Utils.findFirst (((==) exampleName) << Utils.fst3) Examples.list
      in
      let _ = Benchmark.setProgram exampleName in
      let _ = Benchmark.setHeuristicsMode old.syncOptions.feelingLucky in
      -- Run timing tests
      let exampleLoaded = upstate Run { old | code = code, history = ([],[]) } in
      let _ = upstate ProgramStats exampleLoaded in
      -- Trying to avoid the stack overflows that occur if we run this loop in
      -- Elm:
      let _ = Benchmark.next () in
      old

    ProgramStats ->
      let (outputVal, _) =
        Benchmark.silently <|
          \() -> Eval.run (Utils.fromJust old.inputExp)
      in
      let shapeCount =
        (fst (Benchmark.silently <| \() -> LangSvg.valToIndexedTree outputVal)) - 1
      in
      let _ = Benchmark.log "shape count" shapeCount in
      let outputTraces =
        let gatherTraces val =
          case val of
            VConst (n, trace) -> [trace]
            VBase _           -> []
            VClosure _ _ _ _  -> []
            VList vals        -> List.concatMap gatherTraces vals
            VHole _           -> []
        in
        gatherTraces outputVal
      in
      let allLocsInTrace trace =
        case trace of
          TrLoc loc     -> [loc]
          TrOp _ traces -> List.concatMap allLocsInTrace traces
      in
      let locsInOutput =
        -- Sync.locsOfTrace handles freezing
        -- We want to ignore
        List.concatMap allLocsInTrace outputTraces
        |> Set.fromList
      in
      let outputTraceCount = List.length outputTraces in
      let _ = Benchmark.log "trace count" outputTraceCount in
      let _ = Benchmark.log "lexically unique trace count" <| List.length <| Utils.dedup outputTraces in
      -- d1 is nodeIdTo(ListOf(ZoneWithListOfListOfLoc))
      -- The loclist options are determined by:
      -- Non-biased modes:
      -- ([locs unique to attr1] x [locs unique to attr2] x ...) ++ ([locs
      -- shared by all attrs] x [locs shared by all attrs])
      -- Biased modes:
      -- ([locs unique to attr1] x [locs unique to attr2] x ...) ++ [[loc1
      -- shared by all attrs], [loc2 shared by all attrs], [loc3 ...], ...]
      let nodeIdTo_StuffStuffStuffAndAttrToTrace =
        Sync.nodeToAttrLocs outputVal
      in
      let nodeIdTo_ShapeKindAndListOf_ZoneWith_NumAttrsAndListOfListOfLoc =
        Sync.shapesToZoneTable
            old.syncOptions
            nodeIdTo_StuffStuffStuffAndAttrToTrace
      in
      let nodeIdTo_ShapeKindAndListOf_ZoneWithMaybe_ChosenLocListAndAllLocLists =
        Sync.assignTriggers
            old.syncOptions
            nodeIdTo_StuffStuffStuffAndAttrToTrace
            nodeIdTo_ShapeKindAndListOf_ZoneWith_NumAttrsAndListOfListOfLoc
      in
      let zoneCount =
        Dict.foldl
            (\nodeId (shapeKind, listOf_ZoneWith_NumAttrsAndListOfListOfLoc) counter ->
              counter + (List.length listOf_ZoneWith_NumAttrsAndListOfListOfLoc)
            )
            0
            nodeIdTo_ShapeKindAndListOf_ZoneWith_NumAttrsAndListOfListOfLoc
      in
      let _ = Benchmark.log "zone count" zoneCount in
      let _ =
        (Dict.toList nodeIdTo_ShapeKindAndListOf_ZoneWith_NumAttrsAndListOfListOfLoc)
        |> List.map
            (\(nodeId, (shapeKind, listOf_ZoneWith_NumAttrsAndListOfListOfLoc)) ->
              listOf_ZoneWith_NumAttrsAndListOfListOfLoc
              |> List.map
                  (\(zoneName, (numAttrs, listOfListOfLoc)) ->
                    let _ =
                      Benchmark.log
                          ("shape " ++ shapeKind ++ " " ++ (toString nodeId) ++ " " ++ zoneName ++ " loclist options count (by SnS algorithm)")
                          (List.length listOfListOfLoc)
                    in
                    (attrNamesFor old.slate nodeId shapeKind zoneName)
                    |> List.map
                        (\attrName ->
                          let maybeChosenLocId =
                            -- whichLoc crashes if we don't check the maybeChosenLocListAndAllLocLists first
                            -- So that's what the next two lets are for.
                            let (_, listOf_ZoneWithMaybe_ChosenLocListAndAllLocLists) =
                              Utils.justGet nodeId nodeIdTo_ShapeKindAndListOf_ZoneWithMaybe_ChosenLocListAndAllLocLists
                            in
                            let (_, maybeChosenLocListAndAllLocLists) =
                              Utils.fromJust_ ("shape " ++ shapeKind ++ " " ++ (toString nodeId) ++ " " ++ zoneName ++ " couldn't find maybeChosenLocListAndAllLocLists")
                              <| Utils.findFirst
                                  (\(itemZoneName, _) -> itemZoneName == zoneName)
                                  listOf_ZoneWithMaybe_ChosenLocListAndAllLocLists
                            in
                            case maybeChosenLocListAndAllLocLists of
                              Nothing -> Nothing
                              Just _  ->
                                Sync.whichLoc
                                    old.syncOptions
                                    nodeIdTo_StuffStuffStuffAndAttrToTrace
                                    nodeIdTo_ShapeKindAndListOf_ZoneWithMaybe_ChosenLocListAndAllLocLists
                                    nodeId
                                    zoneName
                                    attrName
                          in
                          let chosenLocStr =
                            case maybeChosenLocId of
                              Just locId -> toString locId
                              Nothing    -> "none"
                          in
                          let _ =
                            Benchmark.log
                                ("shape " ++ shapeKind ++ " " ++ (toString nodeId) ++ " " ++ zoneName ++ " " ++ attrName ++ " chosen loc")
                                chosenLocStr
                          in
                          ()
                        )
                  )
            )
      in
      let _ =
        Benchmark.log "locs in output count" (Set.size locsInOutput)
      in
      let nonPreludeLocsInOutput =
        Set.filter (not << LangParser2.isPreludeLoc) locsInOutput
      in
      let _ =
        Benchmark.log "non-prelude locs in output count" (Set.size nonPreludeLocsInOutput)
      in
      let frozenLocsInOutput =
        let frozenByDefault = not old.syncOptions.thawedByDefault in
        Set.filter
            (\loc ->
              -- from Sync.locsOfTrace
              let (_,b,_) = loc in
              if      LangParser2.isPreludeLoc loc  then True
              else if b == frozen                   then True
              else if b == unann && frozenByDefault then True
              else                                       False
            )
            locsInOutput
      in
      let _ =
        Benchmark.log "frozen locs in output count" (Set.size frozenLocsInOutput)
      in
      let nonPreludeFrozenLocsInOutput =
        Set.filter (not << LangParser2.isPreludeLoc) frozenLocsInOutput
      in
      let _ =
        Benchmark.log "non-prelude frozen locs in output count" (Set.size nonPreludeFrozenLocsInOutput)
      in
      let nodeIdAndShapeAndListofZoneAndAttrs =
        List.map
            (\(nodeId, (shapeKind, pointsInfo, infusedAttrZoneTraces, attrToTrace)) ->
              let listOfZoneWithAttrList =
                Sync.getZones shapeKind pointsInfo infusedAttrZoneTraces
              in
              (nodeId, shapeKind, listOfZoneWithAttrList)
            )
            (Dict.toList nodeIdTo_StuffStuffStuffAndAttrToTrace)
      in
      let nodeIdAndShapeAndListofZoneAndAttrsWithTrace =
        nodeIdAndShapeAndListofZoneAndAttrs
        |> List.map
            (\(nodeId, shapeKind, listofZoneAndAttrs) ->
              let (_, _, infusedAttrZoneTraces, attrToTrace) =
                case Dict.get nodeId nodeIdTo_StuffStuffStuffAndAttrToTrace of
                  Just tuple -> tuple
                  Nothing    -> Debug.crash <| "unknown node id " ++ (toString nodeId)
              in
              let listOfZoneAndAttrsWithTrace =
                listofZoneAndAttrs
                |> List.map
                    (\(zoneName, attrs) ->
                      let attrsWithTrace =
                        attrs
                        |> List.map
                            (\attrName ->
                              case Dict.get attrName attrToTrace of
                                Just trace ->
                                  (attrName, trace)

                                Nothing ->
                                  let infusedAttrZoneTrace =
                                    Utils.findFirst
                                        (\(infusedAttrName, (infusedZoneName, _)) ->
                                          attrName == infusedAttrName &&
                                          zoneName == infusedZoneName
                                        )
                                        infusedAttrZoneTraces
                                  in
                                  case infusedAttrZoneTrace of
                                    Just (_, (_, trace)) ->
                                      (attrName, trace)

                                    Nothing ->
                                      Debug.crash <| "couldn't find trace for " ++ (String.join " " [shapeKind, zoneName, attrName])
                            )
                      in
                      (zoneName, attrsWithTrace)
                    )
              in
              (nodeId, shapeKind, listOfZoneAndAttrsWithTrace)
            )
      in
      let _ =
        nodeIdAndShapeAndListofZoneAndAttrsWithTrace
        |> List.map
            (\(nodeId, shapeKind, listOfZoneAndAttrsWithTrace) ->
              listOfZoneAndAttrsWithTrace
              |> List.map
                  (\(zoneName, attrsWithTrace) ->
                    attrsWithTrace
                    |> List.map
                        (\(attrName, trace) ->
                          let _ =
                            Benchmark.log "" ""
                                -- ("shape " ++ shapeKind ++ " " ++ (toString nodeId) ++ " " ++ zoneName ++ " " ++ attrName ++ " trace size")
                                -- chosenLocStr
                          in
                          ()
                        )
                  )
            )
      in
      let _ =
        nonPreludeLocsInOutput
        |> Set.map
            (\loc ->
              let (locId, annotation, ident) = loc in
              let locStr =
                "loc " ++ (toString locId) ++
                  if ident == ""
                  then ""
                  else " (" ++ ident ++ ")"
              in
              let _ =
                Benchmark.log (locStr ++ " frozen") (toString <| Set.member loc frozenLocsInOutput)
              in
              let tracesContainingLoc =
                List.filter
                    ((List.member loc) << allLocsInTrace)
                    outputTraces
              in
              let _ =
                Benchmark.log (locStr ++ " trace count") (List.length tracesContainingLoc)
              in
              let numberOfZonesThatCouldAffectLoc =
                nodeIdAndShapeAndListofZoneAndAttrsWithTrace
                |> List.map
                    (\(_, _, listOfZoneAndAttrsWithTrace) ->
                      listOfZoneAndAttrsWithTrace
                      |> Utils.count
                          (\(_, attrsWithTrace) ->
                            attrsWithTrace
                            |> List.map snd
                            |> List.any ((List.member loc) << allLocsInTrace)
                          )
                    )
                |> List.sum
              in
              let _ =
                Benchmark.log ("count of zones that might be able to change " ++ locStr) numberOfZonesThatCouldAffectLoc
              in
              let numberOfZonesThatChoseLoc =
                nodeIdTo_ShapeKindAndListOf_ZoneWithMaybe_ChosenLocListAndAllLocLists
                |> Dict.toList
                |> List.concatMap
                    (\(nodeId, (shapeKind, listOf_ZoneWithMaybe_ChosenLocListAndAllLocLists)) ->
                      listOf_ZoneWithMaybe_ChosenLocListAndAllLocLists
                    )
                |> Utils.count
                    (\(_, maybeChosenLocListAndAllLocLists) ->
                      case maybeChosenLocListAndAllLocLists of
                        Just (chosenLocList, allLocLists) -> List.member loc chosenLocList
                        Nothing                           -> False
                    )
              in
              let _ =
                Benchmark.log ("count of zones that chose to affect " ++ locStr) numberOfZonesThatChoseLoc
              in
              "side effect programming dummy string!"
            )
      in
      old
      -- let _ = Native.Benchmark.log "unique trace count" (List.length outputTraces) in


      -- - total number of output traces
      -- - total number of unique output traces
      -- - total number of zones
      -- for each zone
      --   - number of choices
      --   - choice made
      --   - solvability: always or unknowable
      -- for each loc l that appears in at least one output trace
      --   - is loc frozen
      --   - number of traces that contain l
      --   - number of zones that could affect l
      --   - number of zones that do affect l


    MousePos (mx0, my0) ->
      let (mx, my) = clickToCanvasPoint old (mx0, my0) in
      case old.mouseMode of

        MouseNothing -> old

        MouseResizeMid Nothing ->
          let f =
            case old.orient of
              Vertical   -> \(mx1,_) -> (old.midOffsetX + mx1 - mx0, old.midOffsetY)
              Horizontal -> \(_,my1) -> (old.midOffsetY, old.midOffsetY + my1 - my0)
          in
          { old | mouseMode = MouseResizeMid (Just f) }

        MouseResizeMid (Just f) ->
          let (x,y) = f (mx0, my0) in
          { old | midOffsetX = x , midOffsetY = y }

        MouseObject objid kind zone Nothing ->
          Benchmark.logDuration "initial click" <|
            \() ->
              let mStuff = maybeStuff objid kind zone old in
              let blah = Just (old.code, mStuff, (mx, my)) in
              { old | mouseMode = MouseObject objid kind zone blah  }

        MouseObject objid kind zone (Just (_, mStuff, (mx0, my0))) ->
          Benchmark.logDuration "drag" <|
            \() ->
              let (dx, dy) = (mx - mx0, my - my0) in
              let (newE,changes,newSlate,newWidgets) =
                applyTrigger objid kind zone old mx0 my0 dx dy in
              { old | code = unparseE newE
                    , inputExp = Just newE
                    , slate = newSlate
                    , widgets = newWidgets
                    , codeBoxInfo = highlightChanges mStuff changes old.codeBoxInfo
                    }

        MouseSlider widget Nothing ->
          let onNewPos = createMousePosCallbackSlider mx my widget old in
          { old | mouseMode = MouseSlider widget (Just (old.code, onNewPos)) }

        MouseSlider widget (Just (_, onNewPos)) ->
          let (newE,newSlate,newWidgets) = onNewPos (mx, my) in
          { old | code = unparseE newE
                , inputExp = Just newE
                , slate = newSlate
                , widgets = newWidgets
                }

    SelectObject id kind zone ->
      case old.mode of
        AdHoc       -> { old | mouseMode = MouseObject id kind zone Nothing }
        Live info ->
          case Dict.get id info.triggers of
            Nothing -> { old | mouseMode = MouseNothing }
            Just dZones ->
              case Dict.get zone dZones of
                Just (Just _) -> { old | mouseMode = MouseObject id kind zone Nothing }
                _             -> { old | mouseMode = MouseNothing }
        SyncSelect _ _ -> old
        _ -> Debug.crash "SelectObject"

    MouseUp ->
      case (old.mode, old.mouseMode) of
        (Print _, _) -> old
        (_, MouseObject i k z (Just (s, _, _))) ->
          -- 8/10: re-parsing to get new position info after live sync-ing
          -- TODO: could update positions within highlightChanges
          let e = Utils.fromOk_ <| parseE old.code in
          let old' = { old | inputExp = Just e } in
          refreshHighlights i z
            { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
                   , history = addToHistory s old'.history }
        (_, MouseSlider _ (Just (s, _))) ->
          let e = Utils.fromOk_ <| parseE old.code in
          let old' = { old | inputExp = Just e } in
            { old' | mouseMode = MouseNothing, mode = refreshMode_ old'
                   , history = addToHistory s old'.history }
        _ ->
          { old | mouseMode = MouseNothing, mode = refreshMode_ old }

    Sync ->
      case (old.mode, old.inputExp) of
        (Live _, _) -> Debug.crash "upstate Sync: shouldn't happen anymore"
        (AdHoc, Just ip) ->
          let
            inputval  = fst <| Eval.run ip
            inputval' = inputval |> LangSvg.valToIndexedTree
                                 |> slateToVal
            newval    = slateToVal old.slate
            struct    = Sync.inferStructuralUpdate ip inputval' newval
            revert    = (ip, inputval)
          in
            case Sync.inferLocalUpdates old.syncOptions ip inputval' newval of
              Ok [] -> { old | mode = mkLive_ old.syncOptions ip  }
              Ok ls ->
                let n = debugLog "# of sync options" (List.length ls) in
                let ls' = List.map fst ls in
                let m = SyncSelect 0 (n, ls' ++ [struct, revert]) in
                upstate (TraverseOption 1) { old | mode = m }
              Err e ->
                let _ = debugLog ("bad sync: ++ " ++ e) () in
                let m = SyncSelect 0 (0, [struct, revert]) in
                upstate (TraverseOption 1) { old | mode = m }
        _ -> Debug.crash "Sync"

    SelectOption ->
      Debug.crash "SelectOption"
{-
      let (SyncSelect i options) = old.mode in
      let (_,l) = options in
      let (ei,vi) = Utils.geti i l in
      { old | code = unparseE ei
            , inputExp = Just ei
            , slate = LangSvg.valToIndexedTree vi
            , mode = mkLive old.syncOptions ei vi }
-}

    TraverseOption offset ->
      Debug.crash "TraverseOption"
{-
      let (SyncSelect i options) = old.mode in
      let (_,l) = options in
      let j = i + offset in
      let (ei,vi) = Utils.geti j l in
      { old | code = unparseE ei
            , inputExp = Just ei
            , slate = LangSvg.valToIndexedTree vi
            , mode = SyncSelect j options }
-}

    SelectExample name thunk ->
      if name == Examples.scratchName then
        upstate Run { old | exName = name, code = old.scratchCode, history = ([],[]) }
      else

      let {e,v,ws} = thunk () in
      let (so, m) =
        case old.mode of
          Live _ -> let so = Sync.syncOptionsOf old.syncOptions e in (so, mkLive so e v)
          Print _ -> let so = Sync.syncOptionsOf old.syncOptions e in (so, mkLive so e v)
          _      -> (old.syncOptions, old.mode)
      in
      let scratchCode' =
        if old.exName == Examples.scratchName then old.code else old.scratchCode
      in
      { old | scratchCode = scratchCode'
            , exName = name
            , inputExp = Just e
            , code = unparseE e
            , history = ([],[])
            , mode = m
            , syncOptions = so
            , slate = LangSvg.valToIndexedTree v
            , widgets = ws
            }

    SwitchMode m -> { old | mode = m }

    SwitchOrient -> { old | orient = switchOrient old.orient }

    ToggleZones -> { old | showZones = toggleShowZones old.showZones }

    Undo ->
      case (old.code, old.history) of
        (_, ([],_)) -> old                -- because of keyboard shortcuts
        (current, (s::past, future)) ->
          let new = { old | history = (past, current::future) } in
          upstate Run (upstate (CodeUpdate s) new)

    Redo ->
      case (old.code, old.history) of
        (_, (_,[])) -> old                -- because of keyboard shorcuts
        (current, (past, s::future)) ->
          let new = { old | history = (current::past, future) } in
          upstate Run (upstate (CodeUpdate s) new)

    KeysDown l ->
      old

      -- let _ = Debug.log "keys" (toString l) in
{-
      case old.mode of
          SaveDialog _ -> old
          _ -> case editingMode old of
            True -> if
              | l == keysMetaShift -> upstate Run old
              | otherwise -> old
            False -> if
              | l == keysE -> upstate Edit old
              | l == keysZ -> upstate Undo old
              -- | l == keysShiftZ -> upstate Redo old
              | l == keysY -> upstate Redo old
              | l == keysG || l == keysH -> -- for right- or left-handers
                  upstate ToggleZones old
              | l == keysO -> upstate ToggleOutput old
              | l == keysP -> upstate SwitchOrient old
              | l == keysS ->
                  let _ = Debug.log "TODO Save" () in
                  upstate Noop old
              | l == keysShiftS ->
                  let _ = Debug.log "TODO Save As" () in
                  upstate Noop old
              | l == keysRight -> adjustMidOffsetX old 25
              | l == keysLeft  -> adjustMidOffsetX old (-25)
              | l == keysUp    -> adjustMidOffsetY old (-25)
              | l == keysDown  -> adjustMidOffsetY old 25
              | otherwise -> old
-}

    -- Elm does not have function equivalence/pattern matching, so we need to
    -- thread these events through upstate in order to catch them to rerender
    -- appropriately (see CodeBox.elm)
    InstallSaveState -> installSaveState old
    RemoveDialog makeSave saveName -> removeDialog makeSave saveName old
    ToggleBasicCodeBox -> { old | basicCodeBox = not old.basicCodeBox }
    UpdateFieldContents fieldContents -> { old | fieldContents = fieldContents }

    UpdateModel f -> f old

    -- Lets multiple events be executed in sequence (useful for CodeBox.elm)
    MultiEvent evts -> case evts of
      [] -> old
      e1 :: es -> upstate e1 old |> upstate (MultiEvent es)

    WaitRun -> old
    WaitSave saveName -> { old | exName = saveName }
    WaitCodeBox -> old

    -- _ -> Debug.crash ("upstate, unhandled evt: " ++ toString evt)

adjustMidOffsetX old dx =
  case old.orient of
    Vertical   -> { old | midOffsetX = old.midOffsetX + dx }
    Horizontal -> upstate SwitchOrient old

adjustMidOffsetY old dy =
  case old.orient of
    Horizontal -> { old | midOffsetY = old.midOffsetY + dy }
    Vertical   -> upstate SwitchOrient old


--------------------------------------------------------------------------------
-- Key Combinations

keysMetaShift           = List.sort [keyMeta, keyShift]
keysE                   = List.sort [Char.toCode 'E']
keysZ                   = List.sort [Char.toCode 'Z']
keysY                   = List.sort [Char.toCode 'Y']
-- keysShiftZ              = List.sort [keyShift, Char.toCode 'Z']
keysG                   = List.sort [Char.toCode 'G']
keysH                   = List.sort [Char.toCode 'H']
keysO                   = List.sort [Char.toCode 'O']
keysP                   = List.sort [Char.toCode 'P']
keysS                   = List.sort [Char.toCode 'S']
keysShiftS              = List.sort [keyShift, Char.toCode 'S']
keysLeft                = [keyLeft]
keysRight               = [keyRight]
keysUp                  = [keyUp]
keysDown                = [keyDown]

keyMeta                 = 91
keyCtrl                 = 17
keyShift                = 16
keyLeft                 = 37
keyUp                   = 38
keyRight                = 39
keyDown                 = 40


--------------------------------------------------------------------------------
-- Mouse Callbacks for Zones

applyTrigger objid kind zone old mx0 my0 dx dy =
  case old.mode of
    AdHoc ->
      (Utils.fromJust (old.inputExp), Dict.empty, old.slate, old.widgets)
    Live info ->
      case Utils.justGet_ "#4" zone (Utils.justGet_ "#5" objid info.triggers) of
        Nothing -> Debug.crash "shouldn't happen due to upstate SelectObject"
        Just trigger ->
          let (newE,changes) = trigger (mx0, my0) (dx, dy) in
          let (newVal,newWidgets) = Eval.run newE in
          (newE, changes, LangSvg.valToIndexedTree newVal, newWidgets)
    _ -> Debug.crash "applyTrigger"


-- attrNames for the solver
attrNamesFor slate nodeId shapeKind zoneName =
  let polyAttrs () =
    case LangSvg.realZoneOf zoneName of
      LangSvg.Z "Interior" ->
        case Dict.get nodeId (snd slate) of
          Just (LangSvg.SvgNode _ nodeAttrs _) ->
            let pts = toPoints <| Utils.find_ nodeAttrs "points" in
            let ptAttrNames =
              Utils.foldli
                  (\(j, (xj,yj)) attrNamesAcc -> (addi "x" j) :: (addi "y" j) :: attrNamesAcc)
                  []
                  pts
            in
            List.reverse ptAttrNames
          _ ->
            Debug.crash <| "polygon Interior attrNamesFor " ++ shapeKind ++ " " ++ zoneName

      LangSvg.ZPoint i ->
        [(addi "x" i), (addi "y" i)]

      LangSvg.ZEdge i  ->
        case Dict.get nodeId (snd slate) of
          Just (LangSvg.SvgNode _ nodeAttrs _) ->
            let pts = toPoints <| Utils.find_ nodeAttrs "points" in
            let n = List.length pts in
              -- This unusual ordering is how polyEdge does it
              -- (after the final List.reverse)
              if i == n then
                (addi "y" 1) :: (addi "x" 1) :: (addi "y" i) :: (addi "x" i) :: []
              else
                (addi "y" i) :: (addi "x" i) :: (addi "y" (i+1)) :: (addi "x" (i+1)) :: []
          _ ->
            Debug.crash <| "polygon edge attrNamesFor " ++ shapeKind ++ " " ++ zoneName

      _ ->
        Debug.crash <| "polygon attrNamesFor " ++ shapeKind ++ " " ++ zoneName
  in
  case (shapeKind, zoneName) of
    (_, "FillBall")   -> ["fill"]
    (_, "RotateBall") -> ["transformRot"]

    ("rect", "Interior")       -> ["x", "y"]
    ("rect", "RightEdge")      -> ["width"]
    ("rect", "BotRightCorner") -> ["width", "height"]
    ("rect", "BotEdge")        -> ["height"]
    ("rect", "BotLeftCorner")  -> ["x", "width", "height"]
    ("rect", "LeftEdge")       -> ["x", "width"]
    ("rect", "TopLeftCorner")  -> ["x", "y", "width", "height"]
    ("rect", "TopEdge")        -> ["y", "height"]
    ("rect", "TopRightCorner") -> ["y", "width", "height"]

    ("circle", "Interior")     -> ["cx", "cy"]
    ("circle", "Edge")         -> ["r"]

    ("ellipse", "Interior")    -> ["cx", "cy"]
    ("ellipse", "Edge")        -> ["rx", "ry"]

    ("line", "Edge")           -> ["x1", "x2", "y1", "y2"]
    ("line", _) ->
      case LangSvg.realZoneOf zoneName of
        LangSvg.ZPoint i -> [(addi "x" i), (addi "y" i)]
        _                -> Debug.crash <| "attrNamesFor line " ++ zoneName

    ("polygon", _)  -> polyAttrs ()
    ("polyline", _) -> polyAttrs ()

    ("path", _) ->
      case LangSvg.realZoneOf zoneName of
        -- y before x to match pathPoint function
        LangSvg.ZPoint i -> [(addi "y" i), (addi "x" i)]
        _                -> Debug.crash <| "attrNamesFor path " ++ zoneName

    _ ->
      Debug.crash <| "attrNamesFor " ++ shapeKind ++ " " ++ zoneName


--------------------------------------------------------------------------------
-- Mouse Callbacks for UI Widgets

wSlider = params.mainSection.uiWidgets.wSlider

createMousePosCallbackSlider mx my widget old =

  let (maybeRound, minVal, maxVal, curVal, locid) =
    case widget of
      WIntSlider a b _ curVal (locid,_,_) ->
        (toFloat << round, toFloat a, toFloat b, toFloat curVal, locid)
      WNumSlider a b _ curVal (locid,_,_) ->
        (identity, a, b, curVal, locid)
  in
  let range = maxVal - minVal in

  \(mx',my') ->
    let newVal =
      curVal + (toFloat (mx' - mx) / toFloat wSlider) * range
        |> clamp minVal maxVal
        |> maybeRound
    in
    -- unlike the live triggers via Sync,
    -- this substitution only binds the location to change
    let subst = Dict.singleton locid newVal in
    let newE = applySubst subst (Utils.fromJust old.inputExp) in
    let (newVal,newWidgets) = Eval.run newE in
    let newSlate = LangSvg.valToIndexedTree newVal in
    (newE, newSlate, newWidgets)
