module Brainstorm where

import Dict
import Set

import Utils
import Lang exposing (..)
import LangTools exposing (..)
import LangSvg exposing (..)
import LangParser2
import Eval
import ValueBasedTransform
import LocEqn
import Sync


type alias BarePoint = (Num, Num)

type ConstraintType = XConstraint | YConstraint | PointConstraint

type alias Constraint = (Trace, Trace)

snapDistance = 30

-- Dear friends, I present to you: Another epic case statement.
possibleRelationsWith : Exp -> RootedIndexedTree -> NodeId -> Zone -> List Idea
possibleRelationsWith inputExp slate shapeId zoneName =
  let (_,tree) = slate in
  let unhandled = [] in
  let env = programEnv inputExp in
  let ideas () = primitiveIdeas env inputExp slate [shapeId] in
  let filterVertical targetX ideas =
    List.filter
        (\(((x,_),(_,_)), _, _) ->
          x - snapDistance <= targetX && x + snapDistance >= targetX
        )
        ideas
  in
  let filterHorizontal targetY ideas =
    List.filter
        (\(((_,_),(y,_)), _, _) ->
          y - snapDistance <= targetY && y + snapDistance >= targetY
        )
        ideas
  in
  let filterPoint (targetX, targetY) ideas =
    List.filter
        (\(((x,_),(y,_)), _, _) ->
          (y - targetY)^2 + (x - targetX)^2 <= snapDistance^2
        )
        ideas
  in
  case Dict.get shapeId tree of
    Nothing    -> Debug.crash <| "possibleRelationsWith " ++ (toString shapeId) ++ " " ++ (toString tree)
    Just shape ->
      let shapeVal = svgNodeToVal tree shape in
      let maybeZoneIdea = shapeZoneToIdea env shape shapeVal zoneName in
      case maybeZoneIdea of
        Nothing       -> []
        Just zoneIdea ->
          let (((targetX,_),(targetY,_)), _, _) = zoneIdea in
          case zoneNameToConstraintType zoneName of
            XConstraint     -> ideas () |> filterVertical targetX
            YConstraint     -> ideas () |> filterHorizontal targetY
            PointConstraint -> ideas () |> filterPoint (targetX, targetY)


zoneNameToConstraintType zoneName =
  case zoneName of
    "RightEdge" -> XConstraint
    "BotEdge"   -> YConstraint
    "LeftEdge"  -> XConstraint
    "TopEdge"   -> YConstraint
    _           -> PointConstraint


shapeZoneToFunctionName shapeName zoneName =
  let eightIdeas prefix =
    case zoneName of
      "RightEdge"      -> Just (prefix ++ "RightCenter")
      "BotEdge"        -> Just (prefix ++ "CenterBot")
      "LeftEdge"       -> Just (prefix ++ "LeftCenter")
      "TopEdge"        -> Just (prefix ++ "CenterTop")
      "BotRightCorner" -> Just (prefix ++ "RightBot")
      "TopRightCorner" -> Just (prefix ++ "RightTop")
      "TopLeftCorner"  -> Just (prefix ++ "LeftTop")
      "BotLeftCorner"  -> Just (prefix ++ "LeftBot")
      _                -> Nothing
  in
  case (shapeName, zoneName) of
    ("rect", _)        -> eightIdeas "rect"
    ("circle", _)      -> eightIdeas "circle"
    ("ellipse", _)     -> eightIdeas "ellipse"
    ("BOX",  _)        -> eightIdeas "boundedShape"
    ("OVAL", _)        -> eightIdeas "boundedShape"
    ("line", "Point1") -> Just "lineStart"
    ("line", "Point2") -> Just "lineEnd"
    _                  -> Nothing


primitiveIdeas : Env -> Exp -> RootedIndexedTree -> List NodeId -> List Idea
primitiveIdeas env inputExp slate excludeShapeIds =
  let shapes = shapesOnCanvas slate excludeShapeIds in
  shapesToPoints env inputExp slate shapes |> consolidate


brainstorm : List Idea -> Exp -> RootedIndexedTree -> List Idea
brainstorm previousIdeas inputExp slate =
  case previousIdeas of
    [] -> primitiveIdeas (programEnv inputExp) inputExp slate []
    _  -> previousIdeas ++ (pointsToMorePoints inputExp previousIdeas) |> consolidate


ideaToPoint : Idea -> BarePoint
ideaToPoint (((x,_),(y,_)), _, _) =
  (x,y)


-- Combine ideas on the same point into a single idea
-- Ordered by first appearance of each point
consolidate ideas =
  let ideasByPoint = Utils.groupBy ideaToPoint ideas in
  let (_, consolidated) =
      List.foldl
        (\idea (ideasRemaining, out) ->
          let key = ideaToPoint idea in
          case Dict.get key ideasRemaining of
            Just ideas ->
              let (point, depth, _) = Utils.head "Brainstorm.consolidate: list of equivalent ideas shouldn't be empty" ideas in
              let ideaSources = List.concatMap Utils.thd3 ideas in
              let consolidatedIdea = (point, depth, ideaSources) in
              (Dict.remove key ideasRemaining, out ++ [consolidatedIdea])
            Nothing    -> (ideasRemaining, out)
        )
        (ideasByPoint, [])
        ideas
  in
  consolidated


programEnv inputExp =
  let (_, env) = Utils.fromOk "Brainstorm.env" <| Eval.eval Eval.initEnv [] inputExp in
  env


shapesToPoints : Env -> Exp -> RootedIndexedTree -> List IndexedTreeNode -> List Idea
shapesToPoints env inputExp slate shapes =
  let (_,tree) = slate in
  let shapeVals = shapes |> List.map (svgNodeToVal tree) in
  let shapeTypeNames = (shapeTypeNamesIn LangParser2.prelude) ++ (shapeTypeNamesIn inputExp) in
  (shapeToPointFunctionsIn shapeTypeNames LangParser2.prelude) ++ (shapeToPointFunctionsIn shapeTypeNames inputExp)
  |> List.concatMap
      (\(_, funcName) ->
        shapeVals
        |> List.filterMap (\shapeVal -> shapeIdea env shapeVal funcName)
      )


shapeIdea env shapeVal functionName =
  let funcCall = eApp (eVar0 functionName) [eVar "shapeIdeaDummyVar"] in
  case Eval.eval (("shapeIdeaDummyVar", shapeVal)::env) [] funcCall of
    Ok ((outVal,_),_) ->
      case unwrapVList outVal of
        Just [VConst (x,xTr), VConst (y,yTr)] ->
          if (x,y) /= (0,0) then
            Just (((x,xTr),(y,yTr)), 1, [PrimitiveFeature shapeVal functionName])
          else
            Nothing
        _ -> Nothing
    Err s -> Nothing


shapesOnCanvas : RootedIndexedTree -> List NodeId -> List IndexedTreeNode
shapesOnCanvas (nodeId, tree) excludeShapeIds =
  if List.member nodeId excludeShapeIds then
    []
  else
    case Utils.justGet_ "shapesOnCanvas get node" nodeId tree of
      TextNode _ -> []
      SvgNode shapeKind attrs childrenIds ->
        let childShapes =
          List.concatMap
              (\childId -> shapesOnCanvas (childId, tree) excludeShapeIds)
              childrenIds
        in
        case shapeKind of
          "g"   -> childShapes
          "svg" -> childShapes
          _     -> (SvgNode shapeKind attrs childrenIds)::childShapes


shapeTypeNamesIn : Exp -> List String
shapeTypeNamesIn exp =
  LangTools.topLevelExps exp
  |> List.filterMap
      (\exp ->
        case exp.val.e__ of
          ETypeAlias _ pat tipe _ _ ->
            case (pat.val, tipe.val) of
              -- Single assignment e.g. (def Circle SVG)
              (PVar _ ident _, TNamed _ "SVG") ->
                Just [ident]

              -- Multiple assignment e.g. (def [Circle Square Rando] [SVG SVG Num])
              (PList _ pats _ Nothing _, TTuple _ types _ Nothing _) ->
                Utils.zip pats types
                |> List.filterMap
                    (\(pat, tipe) ->
                      case (pat.val, tipe.val) of
                        (PVar _ ident _, TNamed _ "SVG") -> Just ident
                        _                                -> Nothing
                    )
                |> Just
              _ -> Nothing
          _ -> Nothing
      )
  |> List.concat


-- TODO: handle shadowing (right now just fails in eval, maybe that's fine)
shapeToPointFunctionsIn : List String -> Exp -> List (Type, String)
shapeToPointFunctionsIn shapeTypeNames exp =
  LangTools.topLevelExps exp
  |> List.filterMap
      (\exp ->
        case exp.val.e__ of
          ETyp _ pat tipe exp _ ->
            case (pat.val, tipe.val) of
              (PVar _ funcName _, TArrow _ types _) ->
                case (List.map .val types) of
                  [TNamed _ argTypeName, TNamed _ "Point"] ->
                    if List.any ((==) argTypeName) shapeTypeNames
                    then Just (tipe, funcName)
                    else Nothing
                  _ -> Nothing
              _ -> Nothing
          _ -> Nothing
      )


pointToPointToPointFunctionsIn : Exp -> List String
pointToPointToPointFunctionsIn exp =
  LangTools.topLevelExps exp
  |> List.filterMap
      (\exp ->
        case exp.val.e__ of
          ETyp _ pat tipe exp _ ->
            case (pat.val, tipe.val) of
              (PVar _ funcName _, TArrow _ types _) ->
                case (List.map .val types) of
                  [TNamed _ "Point", TNamed _ "Point", TNamed _ "Point"] ->
                    Just funcName
                  _ ->
                    Nothing
              _ -> Nothing
          _ -> Nothing
      )


pointsToMorePoints : Exp -> List Idea -> List Idea
pointsToMorePoints inputExp ideas =
  let (_,lastDepth,_) = Utils.head "Brainstorm.pointsToMorePoints: ideas shouldn't be empty here" ideas in
  let depth = lastDepth + 1 in
  let env = programEnv inputExp in
  let pointVals = List.map (LangSvg.pointToVal << Utils.fst3) ideas in
  let pointValAndIdeas = Utils.zip pointVals ideas in
  (pointToPointToPointFunctionsIn LangParser2.prelude) ++ (pointToPointToPointFunctionsIn inputExp)
  |> List.concatMap
      (\funcName ->
        -- let _ = Debug.log "trying func" funcName in
        let funcCall = eApp (eVar0 funcName) [eVar "pt1", eVar "pt2"] in
        pointValAndIdeas |> List.concatMap (\(vPt1, idea1) ->
          pointValAndIdeas |> List.concatMap (\(vPt2, idea2) ->
            case Eval.eval (("pt1", vPt1)::("pt2", vPt2)::env) [] funcCall of
              Ok ((outVal,_),_) ->
                case unwrapVList outVal of
                  Just [VConst (x,xTr), VConst (y,yTr)] ->
                    -- let _ = Debug.log "func" funcName in
                    [(((x,xTr),(y,yTr)), depth, [BasedOnTwoPoints idea1 idea2 funcName])]
                  _ -> []
              Err s -> []
          )
        )
      )


shapeZoneToIdea programEnv shape shapeVal zoneName =
  let shapeName =
    case shape of
      SvgNode shapeName _ _ -> shapeName
      TextNode _            -> Debug.crash "Brainstorm.shapeZoneToIdea: given a text node instead of an SVG node"
  in
  let functionName = shapeZoneToFunctionName shapeName zoneName in
  functionName `Maybe.andThen` (shapeIdea programEnv shapeVal)


ideaSourceToExp k env ideaSource =
  case ideaSource of
    PrimitiveFeature shapeVal functionName ->
      let dummyVarName = "shapeIdeaDummyVar" ++ (toString k) in
      let exp = eApp (eVar0 functionName) [eVar dummyVarName] in
      let env' = (dummyVarName, shapeVal)::env in
      (k + 1, env', exp)

    BasedOnTwoPoints idea1 idea2 functionName ->
      let dummyVar1Name = "pointDummyVar" ++ (toString k) in
      let dummyVar2Name = "pointDummyVar" ++ (toString (k+1)) in
      let k' = k + 2 in
      let relateExp = eApp (eVar0 functionName) [eVar dummyVar1Name, eVar dummyVar2Name] in
      let (k'',  env',  pt1exp) = ideaSourceToExp k'  env  (firstIdeaSource idea1) in
      let (k''', env'', pt2exp) = ideaSourceToExp k'' env' (firstIdeaSource idea2) in
      let exp = eLets [(dummyVar1Name, pt1exp), (dummyVar2Name, pt2exp)] relateExp in
      (k''', env'', exp)


firstIdeaSource idea =
  case idea of
    (_, _, ideaSource::_) -> ideaSource
    _                     -> Debug.crash "Brainstorm.firstIdeaSource: given idea without a source!"


ideaToMaybeVal programEnv extraFunctions idea =
  -- Just use first idea source for now, though there may be other
  -- functions that resolved to the same point.
  let (_, env, ideaExp) = ideaSourceToExp 1 programEnv (firstIdeaSource idea) in
  -- Wrap with extra functions (e.g. to pull out only the x or y coordinate), if necessary
  let exp = List.foldr (\funcName argExp -> eApp (eVar funcName) [argExp]) ideaExp extraFunctions in
  case Eval.eval env [] exp of
    Ok ((outVal,_),_) -> Just outVal
      -- case unwrapVList outVal of
      --   Just [VConst (x,xTr), VConst (y,yTr)] ->
      --     -- let _ = Debug.log "func" funcName in
      --     [(((x,xTr),(y,yTr)), depth, [BasedOnTwoPoints idea1 idea2 funcName])]
      --   _ -> []
    Err s -> Nothing


ideaToMaybeTraces programEnv extraFunctions idea =
  case ideaToMaybeVal programEnv extraFunctions idea |> Maybe.map .v_ of
    Just (VConst (n,nTr)) ->
      Just [nTr]

    Just (VList [val1, val2]) ->
      case List.map .v_ [val1, val2] of
        [VConst (x,xTr), VConst (y,yTr)] -> Just [xTr, yTr]
        _                                -> Nothing

    _ ->
      Nothing


ideasToConstraints programEnv constraintType idea1 idea2 =
  let extraFunctions =
    case constraintType of
      XConstraint     -> ["x"]
      YConstraint     -> ["y"]
      PointConstraint -> []
  in
  let maybeIdea1Trace = ideaToMaybeTraces programEnv extraFunctions idea1 in
  let maybeIdea2Trace = ideaToMaybeTraces programEnv extraFunctions idea2 in

  case (maybeIdea1Trace, maybeIdea2Trace) of
    (Just [x1Tr, y1Tr], Just [x2Tr, y2Tr]) -> [(x1Tr, x2Tr), (y1Tr, y2Tr)]
    (Just [n1Tr],       Just [n2Tr])       -> [(n1Tr, n2Tr)]
    _                                      -> []


maybeMakeEqualConstraint inputExp constraint syncOptions =
  let (tr1, tr2) = constraint in
  let eqn1 = LocEqn.traceToLocEquation tr1 in
  let eqn2 = LocEqn.traceToLocEquation tr2 in
  let unfrozenLocset =
    Set.union (Sync.locsOfTrace syncOptions tr1) (Sync.locsOfTrace syncOptions tr2)
  in
  ValueBasedTransform.makeEqual___ inputExp eqn1 eqn2 unfrozenLocset syncOptions


makeEqualConstraints inputExp constraints syncOptions =
  List.foldl
      (\constraint exp ->
        case maybeMakeEqualConstraint exp constraint syncOptions of
          Just newExp -> newExp
          Nothing     -> exp
      )
      inputExp
      constraints


relate inputExp idea zoneToRelate slideNumber movieNumber movieTime syncOptions =
  let (shapeId, zoneName) = zoneToRelate in
  case Eval.eval Eval.initEnv [] inputExp of
    Err s -> let _ = Debug.log ("Brainstorm.relate eval failed: " ++ s) () in inputExp
    Ok ((val, _), programEnv) ->
      let slateRes =
        LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
      in
      case slateRes of
        Err s -> let _ = Debug.log ("Brainstorm.relate resolveToIndexedTree failed: " ++ s) () in inputExp
        Ok slate ->
          let (_,tree) = slate in
          case Dict.get shapeId tree of
            Nothing -> inputExp
            Just shape ->
              let shapeVal = svgNodeToVal tree shape in
              let maybeZoneIdea = shapeZoneToIdea programEnv shape shapeVal zoneName in
              case maybeZoneIdea of
                Nothing       -> inputExp
                Just zoneIdea ->
                  let constraints =
                    ideasToConstraints programEnv (zoneNameToConstraintType zoneName) idea zoneIdea
                  in
                  makeEqualConstraints inputExp constraints syncOptions
