-- Next time:
--
-- Allow letrec's to evaluate if in Prelu
--   - need to add them to staticEnv in staticEval
--   - need to add the cyclic dependency
--   - need to modify transitive closure calc to handle cycles

module Brainstorm where

import Dict
import Set
import String

import Eval
import Lang exposing (..)
import LangSvg exposing (..)
import LangParser2
import LangTools exposing (..)
import LangUnparser exposing (unparse, unparsePat, precedingWhitespace, replacePrecedingWhitespace)
import LocEqn
import Sync
import Types
import Utils
import ValueBasedTransform

import Native.GloriousGlobalCounter


snapDistance = 30

type alias BarePoint = (Num, Num)

type ConstraintType = XConstraint | YConstraint | PointConstraint

type alias Constraint = { independent: Exp, dependent: Exp }


-- Expressions inserted into constraints get a special dummy EId.
-- In the case of EVar, this EId says "do variable lookup at the top level".
insertedEId = -9124923


expWithEId eid e__ =
  { e__ = e__, eid = eid } |> withDummyRange


insertedExp e__ =
  expWithEId insertedEId e__


insertedApp funcExp argExps = insertedExp (EApp " " funcExp argExps "")
insertedVar0 ident          = insertedExp (EVar "" ident)


isInsertedEId eid =
  eid == insertedEId


isActualEId eid =
  eid >= 0


isProgramEId eid =
  (isActualEId eid) && not (LangParser2.isPreludeEId eid)


allActualEIds exp =
  flattenExpTree exp |> List.map (.eid << .val) |> List.filter isActualEId


possibleRelationsWith : Exp -> Val -> NodeId -> Zone -> List Idea
possibleRelationsWith program outputVal shapeId zoneName =
  let unhandled = [] in
  let env = programEnv program in
  let ideas () = primitiveIdeas env program outputVal [shapeId] in
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
  case maybeFindShape shapeId outputVal of
    Nothing    -> Debug.crash <| "possibleRelationsWith " ++ (toString shapeId)
    Just shapeVal ->
      let maybeZoneIdea = shapeZoneToIdea env shapeVal zoneName in
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


primitiveIdeas : Env -> Exp -> Val -> List NodeId -> List Idea
primitiveIdeas env program outputVal excludeShapeIds =
  let shapeVals = shapesOnCanvas outputVal excludeShapeIds in
  shapesToPoints env program shapeVals |> consolidate


brainstorm : List Idea -> Exp -> Val -> List Idea
brainstorm previousIdeas program outputVal =
  case previousIdeas of
    [] -> primitiveIdeas (programEnv program) program outputVal []
    _  -> previousIdeas ++ (pointsToMorePoints program previousIdeas) |> consolidate


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


programEnv program =
  let (_, env) = Utils.fromOk "Brainstorm.env" <| Eval.eval Eval.initEnv [] program in
  env


shapesToPoints : Env -> Exp -> List Val -> List Idea
shapesToPoints env program shapeVals =
  let shapeTypeNames = (shapeTypeNamesIn LangParser2.prelude) ++ (shapeTypeNamesIn program) in
  (shapeToPointFunctionsIn shapeTypeNames LangParser2.prelude) ++ (shapeToPointFunctionsIn shapeTypeNames program)
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


shapesOnCanvas : Val -> List NodeId -> List Val
shapesOnCanvas outputVal excludeShapeIds =
  case maybeIndexShapes outputVal of
    Nothing -> []
    Just shapeIndex ->
      shapeIndex
      |> Dict.toList
      |> List.filterMap
          (\(shapeId, shapeVal) ->
            if List.member shapeId excludeShapeIds then
              Nothing
            else
              Just shapeVal
          )


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
pointsToMorePoints program ideas =
  let (_,lastDepth,_) = Utils.head "Brainstorm.pointsToMorePoints: ideas shouldn't be empty here" ideas in
  let depth = lastDepth + 1 in
  let env = programEnv program in
  let pointVals = List.map (LangSvg.pointToVal << Utils.fst3) ideas in
  let pointValAndIdeas = Utils.zip pointVals ideas in
  (pointToPointToPointFunctionsIn LangParser2.prelude) ++ (pointToPointToPointFunctionsIn program)
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


maybeIndexShapes : Val -> Maybe (Dict.Dict Int Val)
maybeIndexShapes outputVal =
  case tryIndexShapes_ 1 [] outputVal of
    Ok (_, idsAndVals) -> Just (Dict.fromList idsAndVals)
    Err s              -> let _ = Debug.log ("Brainstorm.indexShapes: " ++ s) () in Nothing


-- Mirrors numbering of valToIndexedTree_
tryIndexShapes_ : Int -> List (Int, Val) -> Val -> Result String (Int, (List (Int, Val)))
tryIndexShapes_ nextShapeId shapeIndex val =
  case val.v_ of
    VList vs ->
      case List.map .v_ vs of
        [VBase (VString "TEXT"), VBase (VString s)] ->
          Ok (nextShapeId + 1, (nextShapeId, val)::shapeIndex)

        [VBase (VString kind), VList attrs, VList childVals] ->
          -- Id's go to children before parent
          let processChild childVal acc =
            case acc of
              Err _                   -> acc
              Ok (nextId, shapeIndex) -> tryIndexShapes_ nextId shapeIndex childVal
          in
          let childrenResult =
            childVals
            |> List.foldl processChild (Ok (nextShapeId, shapeIndex))
          in
          childrenResult
          |> Result.map
              (\(nextShapeId', shapeIndex') ->
                (nextShapeId' + 1, (nextShapeId', val)::shapeIndex')
              )

        _ -> Err <| "an SVG node" `LangSvg.expectedButGotStr` strVal val

    _ -> Err <| "an SVG node" `LangSvg.expectedButGotStr` strVal val

--
-- type ShapeSearchState
--   = Searching Int
--   | SearchFound Val
--   | SearchError String
--

maybeFindShape : Int -> Val -> Maybe Val
maybeFindShape shapeId outputVal =
  maybeIndexShapes outputVal `Maybe.andThen` (Dict.get shapeId)


-- -- Mirrors numbering of valToIndexedTree_
-- tryMaybeFindShape_ : Int -> Int -> Val -> ShapeSearchState
-- tryMaybeFindShape_ targetShapeId nextShapeId val =
--   case val.v_ of
--     VList vs ->
--       case List.map .v_ vs of
--         [VBase (VString "TEXT"), VBase (VString s)] ->
--           if nextShapeId == targetShapeId then
--             SearchFound val
--           else
--             Searching (1 + nextShapeId)
--
--         [VBase (VString kind), VList attrs, VList childVals] ->
--           -- Id's go to children before parent
--           let processChild vi acc =
--             case acc of
--               SearchError _    -> acc
--               SearchFound _    -> acc
--               Searching nextId -> tryMaybeFindShape_ targetShapeId nextId vi
--           in
--           let childrenResult =
--             childVals
--             |> List.foldl processChild (Searching nextShapeId)
--           in
--           case childrenResult of
--             SearchError _ -> childrenResult
--             SearchFound _ -> childrenResult
--             Searching nextShapeId' ->
--               if nextShapeId' == targetShapeId then
--                 SearchFound val
--               else
--                 Searching (1 + nextShapeId')
--
--         _ -> SearchError <| "an SVG node" `LangSvg.expectedButGotStr` strVal val
--
--     _ -> SearchError <| "an SVG node" `LangSvg.expectedButGotStr` strVal val


shapeZoneToIdea programEnv shapeVal zoneName =
  let shapeName =
    case shapeVal.v_ of
      VList elements ->
        case List.map .v_ elements of
          (VBase (VString shapeName))::_ -> shapeName
          _                              -> Debug.crash <| "Brainstorm.shapeZoneToIdea: can't find shape name of " ++ (strVal shapeVal)

      _ ->
        Debug.crash <| "Brainstorm.shapeZoneToIdea: can't find shape name of " ++ (strVal shapeVal)
  in
  let functionName = shapeZoneToFunctionName shapeName zoneName in
  functionName `Maybe.andThen` (shapeIdea programEnv shapeVal)


-- ideaSourceToExp k env ideaSource =
--   case ideaSource of
--     PrimitiveFeature shapeVal functionName ->
--       let dummyVarName = "shapeIdeaDummyVar" ++ (toString k) in
--       let exp = eApp (eVar0 functionName) [eVar dummyVarName] in
--       let env' = (dummyVarName, shapeVal)::env in
--       (k + 1, env', exp)
--
--     BasedOnTwoPoints idea1 idea2 functionName ->
--       let dummyVar1Name = "pointDummyVar" ++ (toString k) in
--       let dummyVar2Name = "pointDummyVar" ++ (toString (k+1)) in
--       let k' = k + 2 in
--       let relateExp = eApp (eVar0 functionName) [eVar dummyVar1Name, eVar dummyVar2Name] in
--       let (k'',  env',  pt1exp) = ideaSourceToExp k'  env  (firstIdeaSource idea1) in
--       let (k''', env'', pt2exp) = ideaSourceToExp k'' env' (firstIdeaSource idea2) in
--       let exp = eLets [(dummyVar1Name, pt1exp), (dummyVar2Name, pt2exp)] relateExp in
--       (k''', env'', exp)


firstIdeaSource idea =
  case idea of
    (_, _, ideaSource::_) -> ideaSource
    _                     -> Debug.crash "Brainstorm.firstIdeaSource: given idea without a source!"


-- ideaToMaybeVal programEnv extraFunctions idea =
--   -- Just use first idea source for now, though there may be other
--   -- functions that resolved to the same point.
--   let (_, env, ideaExp) = ideaSourceToExp 1 programEnv (firstIdeaSource idea) in
--   -- Wrap with extra functions (e.g. to pull out only the x or y coordinate), if necessary
--   let exp = List.foldr (\funcName argExp -> eApp (eVar funcName) [argExp]) ideaExp extraFunctions in
--   case Eval.eval env [] exp of
--     Ok ((outVal,_),_) -> Just outVal
--       -- case unwrapVList outVal of
--       --   Just [VConst (x,xTr), VConst (y,yTr)] ->
--       --     -- let _ = Debug.log "func" funcName in
--       --     [(((x,xTr),(y,yTr)), depth, [BasedOnTwoPoints idea1 idea2 funcName])]
--       --   _ -> []
--     Err s -> Nothing


valToExp : Val -> Exp
valToExp val =
  let eid =
    -- TODO: limit to only static exps
    case val.vtrace |> List.filter isProgramEId of
      []     -> dummyEId
      eid::_ -> eid
  in
  case val.v_ of
    VConst (n, tr)     -> expWithEId eid <| EConst " " n dummyLoc noWidgetDecl
    VBase (VBool bool) -> expWithEId eid <| EBase " " (EBool bool)
    VBase (VString s)  -> expWithEId eid <| EBase " " (EString defaultQuoteChar s)
    VBase VNull        -> expWithEId eid <| EBase " " ENull
    VList vals         -> expWithEId eid <| EList " " (List.map valToExp vals) "" Nothing ""
    VDict vDict        -> expWithEId eid <| EDict (vDict |> Dict.map (\_ v -> valToExp v))
    _                  -> Debug.crash <| "Unexpected val in valToExp" ++ (strVal val)


-- Still only using the first ideaSource
ideaToConstraintTerm : Idea -> Exp
ideaToConstraintTerm idea =
  case firstIdeaSource idea of
    PrimitiveFeature shapeVal functionName ->
      let _ = Debug.log "shape val" shapeVal in
      let _ = Debug.log "shape val as exp" (valToExp shapeVal) in
      insertedApp (insertedVar0 functionName) [valToExp shapeVal]

    BasedOnTwoPoints idea1 idea2 functionName ->
      insertedApp
          (insertedVar0 functionName)
          [ ideaToConstraintTerm idea1
          , ideaToConstraintTerm idea2
          ]


-- idea2 is the dependent idea
ideasToConstraints : ConstraintType -> Idea -> Idea -> List Constraint
ideasToConstraints constraintType indepIdea depIdea =
  let extraFunctions =
    case constraintType of
      XConstraint     -> ["x"]
      YConstraint     -> ["y"]
      PointConstraint -> []
  in
  let lhsIdeaTerm = ideaToConstraintTerm indepIdea in
  let rhsIdeaTerm = ideaToConstraintTerm depIdea in
  let wrapTerm funcName arg = insertedApp (insertedVar0 funcName) [arg] in
  let lhs = List.foldr wrapTerm lhsIdeaTerm extraFunctions in
  let rhs = List.foldr wrapTerm rhsIdeaTerm extraFunctions in
  [ { independent = lhs, dependent = rhs } ]


ideaToMaybeConstraint : ConstraintType -> Idea -> Idea -> Maybe Constraint
ideaToMaybeConstraint constraintType indepIdea depIdea =
  case ideasToConstraints constraintType indepIdea depIdea of
    []            -> Nothing
    constraint::_ -> Just constraint


-- ***
-- varLookupNoEnv dependencyAnalysis program eid =
--   case Dict.get eid dependencyAnalysis of
--     Just varDep ->
--       case varDep.exp.val.e__ of
--         EVar _ ident ->
--           case Utils.maybeFind ident varDep.staticEnv of
--             Just (Just eid) -> Just eid
--             _               -> Nothing
--
--         _ ->
--           Debug.crash "Brainstorm.varLookupNoEnv exp should be a EVar"
--
--     _ ->
--       Debug.crash <| "Brainstorm.varLookupNoEnv eid " ++ (toString eid) ++ " should be in analysis: " ++ (toString dependencyAnalysis)


justGetExpressionAnalysis programAnalysis eid =
  Utils.justGet_ ("Brainstorm.justGetExpressionAnalysis: could not find eid " ++ (toString eid)) eid programAnalysis


--- Precondition: programAnalysis has calculated the closures
resultDependenceTransitiveClosure programAnalysis eid =
  case (justGetExpressionAnalysis programAnalysis eid).dependenceClosure of
    Just closure -> closure
    Nothing      -> Debug.crash <| "Brainstorm.resultDependenceTransitiveClosure: closure not calculated for eid " ++ (toString eid)


eidToExp programAnalysis eid =
  (justGetExpressionAnalysis programAnalysis eid).exp


-- Does not check that all indep expressions are static.
-- Non-static indep expressions will fail to lift (their
-- bound vars fail a lookup)
isValidSolution programAnalysis (independent, depEId) =
  let indepEIds = Set.toList <| Set.fromList <| allActualEIds independent in
  let indepEIdsTransitiveClosure =
    indepEIds
    |> List.map (resultDependenceTransitiveClosure programAnalysis)
    |> Utils.unionAll
    |> Set.union (Set.fromList indepEIds)
  in
  let eidsBeingReplaced =
    depEId :: (allActualEIds (eidToExp programAnalysis depEId))
    |> Set.fromList
  in
  let isIndepNotDependentOnDep =
    0 == (Set.size <| Set.intersect indepEIdsTransitiveClosure eidsBeingReplaced)
  in
  isIndepNotDependentOnDep


findVarAtEId program varExp =
  let varEId = varExp.val.eid in
  if isActualEId varEId || isInsertedEId varEId then
    let searchScopeEId =
      if isInsertedEId varEId then
        -- If this is a var inserted into the constraint, resolve at the top level.
        let errorMsg = "Brainstorm.findVarAtEId: can't handle empty program" in
        (Utils.last errorMsg (topLevelExps program)).val.eid
      else
        varEId
    in
    let visibleVars = visibleVarsAt program searchScopeEId in
    case varExp.val.e__ of
      EVar _ ident ->
        case Utils.maybeFind ident visibleVars of
          Just (Just eid) -> (findExpByEId program eid) `Utils.orTry` (\() -> findExpByEId LangParser2.prelude eid)
          _               ->
            let _ = Debug.log ("findVarAtEId: " ++ ident ++ " not in env at " ++ (toString searchScopeEId)) (List.map fst visibleVars) in
            Nothing -- not found in static env, or bound

      _ ->
        let _ = Debug.log ("findVarAtEId: not given an EVar: " ++ (unparse varExp)) () in
        Nothing
  else
    let _ = Debug.log "findVarAtEId: not actual eid" varEId in
    Nothing


-- symbolicallyEvaluateAsFarAsPossible : Exp -> Exp -> Exp
-- symbolicallyEvaluateAsFarAsPossible program exp =
--   case maybeSymbolicallyExpandOneStep program exp of
--     Nothing    -> exp
--     Just child -> symbolicallyEvaluateAsFarAsPossible program child
--
--
-- maybeSymbolicallyExpandOneStep : Exp -> Exp -> Maybe Exp
-- maybeSymbolicallyExpandOneStep program exp =
--   case exp.val.e__ of
--     EConst _ i l wd             -> Nothing
--     EBase _ v                   -> Nothing
--     EVar _ x                    -> findVarAtEId program exp
--     EFun _ ps e _               -> Nothing
--     EOp _ op es _               -> Nothing -- TODO reach into dictionaries (yay)
--     EList _ es _ m _            -> Nothing
--     EIndList _ rs _             -> Nothing
--     EIf _ e1 e2 e3 _            -> Nothing -- Maybe determine which branch???
--     ECase _ e1 bs _             -> Nothing
--     ETypeCase _ e1 tbranches _  -> Nothing
--     EApp _ e1 es _              -> maybeSymbolicallyApply program exp
--     ELet _ _ False p e1 e2 _    ->
--       case trySymbolicMatch program p e1 of
--         Just env -> symbolicSubstitute (Dict.fromList env) e2
--         Nothing  -> Nothing
--
--     ELet _ _ True p e1 e2 _     -> Nothing -- can't handle recursive substitution
--     EComment _ _ e1             -> Just e1
--     EOption _ _ _ _ e1          -> Just e1
--     ETyp _ _ _ e1 _             -> Just e1
--     EColonType _ e1 _ _ _       -> Just e1
--     ETypeAlias _ _ _ e1 _       -> Just e1

-- Only expands child expressions if necessary
-- e.g. an if-statement will evaluate the predicate and possibly a branch
--      but a list literal will not try to expand each list element
symbolicallyEvaluateAsFarAsPossible : Exp -> Exp -> Exp
symbolicallyEvaluateAsFarAsPossible program exp =
  let recurse e = symbolicallyEvaluateAsFarAsPossible program e in
  case exp.val.e__ of
    EVal _             -> exp -- TODO: How should we handle EVal's here?
    EDict _            -> exp -- TODO: How should we handle EDict's here?
    EConst _ n loc wd  -> exp
    EBase _ bVal       -> exp
    EVar _ ident       ->
      case findVarAtEId program exp of
        Just newExp -> recurse newExp
        Nothing     -> exp
    EFun _ pats body _ -> exp
    EOp _ op argExps _ ->
      let op_ = op.val in
      let nullaryOp args retVal =
        case args of
          [] -> retVal
          _  -> exp
      in
      let unaryMathOp op_ args =
        case args of
          [EConst _ n _ _] ->
            case op_ of
              Cos    -> eConstNoLoc (cos n)
              Sin    -> eConstNoLoc (sin n)
              ArcCos -> eConstNoLoc (acos n)
              ArcSin -> eConstNoLoc (asin n)
              Floor  -> eConstNoLoc (toFloat <| floor n)
              Ceil   -> eConstNoLoc (toFloat <| ceiling n)
              Round  -> eConstNoLoc (toFloat <| round n)
              Sqrt   -> eConstNoLoc (sqrt n)
              _      -> exp

          _ ->
            exp
      in
      let binMathOp op_ args =
        case args of
          [EConst _ n1 _ _, EConst _ n2 _ _] ->
            case op_ of
              Plus    -> eConstNoLoc (n1 + n2)
              Minus   -> eConstNoLoc (n1 - n2)
              Mult    -> eConstNoLoc (n1 * n2)
              Div     -> eConstNoLoc (n1 / n2)
              Pow     -> eConstNoLoc (n1 ^ n2)
              Mod     -> eConstNoLoc (toFloat <| (floor n1) % (floor n2))
              ArcTan2 -> eConstNoLoc (atan2 n1 n2)
              _       -> exp

          _ ->
            exp
      in
      let evaledArgExps = argExps |> List.map recurse in
      let args          = evaledArgExps |> List.map (.e__ << .val) in
      case op_ of
        Plus ->
          case args of
            [EBase _ (EString _ s1), EBase _ (EString _ s2)] -> eStr (s1 ++ s2)
            _                                                -> binMathOp op_ args
        Minus   -> binMathOp op_ args
        Mult    -> binMathOp op_ args
        Div     -> binMathOp op_ args
        Mod     -> binMathOp op_ args
        Pow     -> binMathOp op_ args
        ArcTan2 -> binMathOp op_ args
        Lt      -> case args of
          [EConst _ n1 _ _, EConst _ n2 _ _] -> eBase (EBool (n1 < n2))
          _                                  -> exp
        Eq            -> case args of
          [EConst _ n1 _ _,        EConst _ n2 _ _]        -> eBase (EBool (n1 == n2))
          [EBase _ (EString _ s1), EBase _ (EString _ s2)] -> eBase (EBool (s1 == s2))
          _                                                -> exp
        Pi         -> nullaryOp args (eConstNoLoc pi)
        DictEmpty  -> exp
        DictInsert -> exp -- We don't have an expression language for dictionary literals :/
        DictGet    ->
          -- TODO: may be able to walk back through insertions/removals...
          exp
        DictRemove -> exp
        Cos        -> unaryMathOp op_ args
        Sin        -> unaryMathOp op_ args
        ArcCos     -> unaryMathOp op_ args
        ArcSin     -> unaryMathOp op_ args
        Floor      -> unaryMathOp op_ args
        Ceil       -> unaryMathOp op_ args
        Round      -> unaryMathOp op_ args
        Sqrt       -> unaryMathOp op_ args
        Explode    -> case args of
          [EBase _ (EString _ s)] -> eList (List.map (eStr << String.fromChar) (String.toList s)) Nothing
          _                       -> exp
        DebugLog      -> exp
        ToStr         -> exp
        RangeOffset _ -> exp

    EList _ heads _ maybeTail _              -> exp
    EIndList _ ranges _                      -> exp
    EIf _ predicate trueBranch falseBranch _ ->
      case (recurse predicate).val.e__ of
        EBase _ (EBool True)  -> recurse trueBranch
        EBase _ (EBool False) -> recurse falseBranch
        _                     -> exp

    ECase _ scrutinee branches _ ->
      case maybeSymbolicMatchBranches program scrutinee branches of
        Nothing ->
          exp

        Just (branchEnv, branchExp) ->
          symbolicSubstitute (Dict.fromList branchEnv) branchExp
          |> recurse

    ETypeCase _ scrutinee tbranches _ -> exp -- TODO: symbolic expansion of typecase
    EApp _ funcExp argExps _          -> Maybe.withDefault exp (maybeSymbolicallyApply program exp)
    ELet _ _ True pat assigns body _  -> exp -- can't handle recursive substitution
    ELet _ _ False pat assigns body _ ->
      case maybeSymbolicMatch program pat assigns of
        Just env -> recurse (symbolicSubstitute (Dict.fromList env) body)
        Nothing  -> exp

    EComment _ _ body       -> recurse body
    EOption _ _ _ _ body    -> recurse body
    ETyp _ _ _ body _       -> recurse body
    EColonType _ body _ _ _ -> recurse body
    ETypeAlias _ _ _ body _ -> recurse body


-- CannotCompare needs to be separate from NoMatch for
-- when you are iteratively examining case branch patterns.
--
-- CannotCompare means it might be a match in real execution
-- so you can't pretend the case branch was skipped, and thus
-- you shouldn't go on to test the next branch.
type AbstractMatchResult a
  = Match a
  | NoMatch
  | CannotCompare

type alias SymbolicMatchResult = AbstractMatchResult (List (Ident, Exp))


matchMap f matchResult =
  case matchResult of
    Match env -> Match (f env)
    _         -> matchResult


matchToMaybe matchResult =
  case matchResult of
    Match env -> Just env
    _         -> Nothing


-- CannotCompare overrides NoMatch overrides Match
projMatches resultList =
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


-- For use in case branch determinations.
-- Aborts early if any match returns CannotCompare.
maybeSymbolicMatchBranches : Exp -> Exp -> List Branch -> Maybe (List (Ident, Exp), Exp)
maybeSymbolicMatchBranches program scrutinee branches =
  maybeExpMatchBranches_ (trySymbolicMatch program) scrutinee branches


-- For use in case branch determinations.
-- Aborts early if any match returns CannotCompare.
maybeExpMatchBranches : Exp -> List Branch -> Maybe (List (Ident, Exp), Exp)
maybeExpMatchBranches scrutinee branches =
  maybeExpMatchBranches_ tryMatchExp scrutinee branches


maybeExpMatchBranches_ : (Pat -> Exp -> SymbolicMatchResult) -> Exp -> List Branch -> Maybe (List (Ident, Exp), Exp)
maybeExpMatchBranches_ matcherFunc scrutinee branches =
  case branches of
    [] ->
      Nothing

    branch::otherBranches ->
      let (Branch_ _ bPat bExp _) = branch.val in
      case matcherFunc bPat scrutinee of
        Match env     -> Just (env, bExp)
        NoMatch       -> maybeExpMatchBranches_ matcherFunc scrutinee otherBranches
        CannotCompare -> Nothing



maybeSymbolicMatch : Exp -> Pat -> Exp -> Maybe (List (Ident, Exp))
maybeSymbolicMatch program pat exp =
  trySymbolicMatch program pat exp |> matchToMaybe


maybeMatchExp : Pat -> Exp -> Maybe (List (Ident, Exp))
maybeMatchExp pat exp =
  tryMatchExp pat exp |> matchToMaybe


-- The pat may not match if the exp hasn't been evaluated far enough.
-- Try to evaluate just far enough to match.
trySymbolicMatch : Exp -> Pat -> Exp -> SymbolicMatchResult
trySymbolicMatch program pat exp =
  let result = tryMatchExp_ (trySymbolicMatch program) pat exp in
  let recurse () =
    let _ = Debug.log ((unparse exp) ++ " didn't match " ++ (unparsePat pat) ++ ", attempting one step of evaluation") in
    case maybeSymbolicallyEvaluateOneStep program exp of
      Nothing            -> result
      Just oneStepEvaled -> trySymbolicMatch program pat oneStepEvaled
  in
  case result of
    Match _       -> result
    NoMatch       -> recurse ()
    CannotCompare -> recurse ()


-- Returns CannotCompare if there's not enough type information about
-- the scrutinee to match a branch.
tryExpMatchTBranches : Exp -> List TBranch -> AbstractMatchResult Exp
tryExpMatchTBranches scrutinee tbranches =
  (List.map .val tbranches)
  |> List.foldl
    (\tbranch result ->
      case result of
        CannotCompare -> result
        Match _       -> result
        NoMatch       ->
          let (TBranch_ _ bType bExp _) = tbranch in
          case Types.expIsType scrutinee bType of
            Just True  -> Match bExp
            Just False -> NoMatch
            Nothing    -> CannotCompare
    )
    NoMatch

-- typedWellEnoughToMatch : Exp -> Exp -> List TBranch -> Bool
-- typedWellEnoughToMatch program scrutinee tbranches =
--   let branchTypes =
--     tbranches |> List.map .val |> List.map (\(TBranch_ _ tipe _ _) -> tipe)
--   in
--   -- Presumes typecase is exhaustive (at least one branch will match).
--   let maybeTypedWellEnough =
--     branchTypes
--     |> List.foldl
--       (\bType maybeTypedWellEnough ->
--         case maybeTypedWellEnough of
--           Just _  -> maybeTypedWellEnough
--           Nothing ->
--             case Types.expIsType scrutinee bType of
--               Just True  -> Just True  -- Definitely matched. The scrutinee is typed well enough to match. Don't look at following branches.
--               Just False -> Nothing    -- Definitely not matched. Don't know if typecase is typed well enough to match. Look at the next branch.
--               Nothing    -> Just False -- Unable to tell if the scrutinee matches this branch or not. This typecase is not yet typed well enough to match. Don't look at following branches.
--       )
--       Nothing
--   in
--   case maybeTypedWellEnough of
--     Just True  -> True
--     Just False -> False
--     Nothing    -> False


-- symbolicallyEvaluateStepwiseUntilSuccess : Exp -> List Exp -> (List Exp -> Maybe a) -> Maybe a
-- symbolicallyEvaluateStepwiseUntilSuccess program exps thingToAttempt =
--   case thingToAttempt exps of
--     Just result -> Just result
--     Nothing ->
--       -- So, we could do the cross product of each arg's evaluation steps, but
--       -- that's pretty terrible. So let's just try to evaluate each arg one step.
--       -- Overevaluation can produce bad code, but underevaluation can prematurely fail.
--       let didEvaluateAndNewExps =
--         exps
--         |> List.map
--             (\exp ->
--               case maybeSymbolicallyEvaluateOneStep program exp of
--                 Just newExp -> (True, newExp)
--                 Nothing     -> (False, exp)
--             )
--       in
--       let anySuccess = didEvaluateAndNewExps |> List.map fst |> List.any ((==) True) in
--       if anySuccess then
--         let newExps = didEvaluateAndNewExps |> List.map snd in
--         symbolicallyEvaluateStepwiseUntilSuccess program newExps thingToAttempt
--       else
--         Nothing


-- Goal here is to evaluate just enough so that e.g. patterns can match.
--
-- Returns Nothing if no progress.
maybeSymbolicallyEvaluateOneStep : Exp -> Exp -> Maybe Exp
maybeSymbolicallyEvaluateOneStep program exp =
  -- Return Just newExps if progress made on one expression.
  -- Return Nothing if no progress made on any expressions.
  let evalOneOf exps =
    case exps of
      []    -> Nothing
      e::es ->
        case maybeSymbolicallyEvaluateOneStep program e of
          Just newE -> Just (newE::es)
          Nothing   -> evalOneOf es |> Maybe.map (\rest -> e::rest)
  in
  case exp.val.e__ of
    EVal val           -> Nothing
    EDict _            -> Nothing -- TODO: How should we handle EDict's here?
    EConst _ n loc wd  -> Nothing
    EBase _ bVal       -> Nothing
    EVar _ ident       -> findVarAtEId program exp
    EFun _ pats body _ -> Nothing
    EOp ws1 op argExps ws2 ->
      case evalOneOf argExps of
        Just newArgExps -> Just <| replaceE__ exp (EOp ws1 op newArgExps ws2)
        Nothing ->
          let op_ = op.val in
          let nullaryOp args retVal =
            case args of
              [] -> Just retVal
              _  -> Nothing
          in
          let unaryMathOp op_ args =
            case args of
              [EConst _ n _ _] ->
                case op_ of
                  Cos    -> Just <| eConstNoLoc (cos n)
                  Sin    -> Just <| eConstNoLoc (sin n)
                  ArcCos -> Just <| eConstNoLoc (acos n)
                  ArcSin -> Just <| eConstNoLoc (asin n)
                  Floor  -> Just <| eConstNoLoc (toFloat <| floor n)
                  Ceil   -> Just <| eConstNoLoc (toFloat <| ceiling n)
                  Round  -> Just <| eConstNoLoc (toFloat <| round n)
                  Sqrt   -> Just <| eConstNoLoc (sqrt n)
                  _      -> Nothing

              _ ->
                Nothing
          in
          let binMathOp op_ args =
            case args of
              [EConst _ n1 _ _, EConst _ n2 _ _] ->
                case op_ of
                  Plus    -> Just <| eConstNoLoc (n1 + n2)
                  Minus   -> Just <| eConstNoLoc (n1 - n2)
                  Mult    -> Just <| eConstNoLoc (n1 * n2)
                  Div     -> Just <| eConstNoLoc (n1 / n2)
                  Pow     -> Just <| eConstNoLoc (n1 ^ n2)
                  Mod     -> Just <| eConstNoLoc (toFloat <| (floor n1) % (floor n2))
                  ArcTan2 -> Just <| eConstNoLoc (atan2 n1 n2)
                  _       -> Nothing

              _ ->
                Nothing
          in
          let evaledArgExps = argExps in
          let args          = evaledArgExps |> List.map (.e__ << .val) in
          case op_ of
            Plus ->
              case args of
                [EBase _ (EString _ s1), EBase _ (EString _ s2)] -> Just <| eStr (s1 ++ s2)
                _                                                -> binMathOp op_ args
            Minus   -> binMathOp op_ args
            Mult    -> binMathOp op_ args
            Div     -> binMathOp op_ args
            Mod     -> binMathOp op_ args
            Pow     -> binMathOp op_ args
            ArcTan2 -> binMathOp op_ args
            Lt      -> case args of
              [EConst _ n1 _ _, EConst _ n2 _ _] -> Just <| eBase (EBool (n1 < n2))
              _                                  -> Nothing
            Eq            -> case args of
              [EConst _ n1 _ _,        EConst _ n2 _ _]        -> Just <| eBase (EBool (n1 == n2))
              [EBase _ (EString _ s1), EBase _ (EString _ s2)] -> Just <| eBase (EBool (s1 == s2))
              _                                                -> Nothing
            Pi         -> nullaryOp args (eConstNoLoc pi)
            DictEmpty  -> Nothing
            DictInsert -> Nothing -- We don't have an expression language for dictionary literals :/
            DictGet    ->
              -- TODO: may be able to walk back through insertions/removals...
              Nothing
            DictRemove -> Nothing
            Cos        -> unaryMathOp op_ args
            Sin        -> unaryMathOp op_ args
            ArcCos     -> unaryMathOp op_ args
            ArcSin     -> unaryMathOp op_ args
            Floor      -> unaryMathOp op_ args
            Ceil       -> unaryMathOp op_ args
            Round      -> unaryMathOp op_ args
            Sqrt       -> unaryMathOp op_ args
            Explode    -> case args of
              [EBase _ (EString _ s)] -> Just <| eList (List.map (eStr << String.fromChar) (String.toList s)) Nothing
              _                       -> Nothing
            DebugLog      -> Nothing
            ToStr         -> Nothing
            RangeOffset _ -> Nothing

    EList ws1 heads ws2 maybeTail ws3 ->
      case evalOneOf heads of
        Just newHeads -> Just <| replaceE__ exp (EList ws1 newHeads ws2 maybeTail ws3)
        Nothing ->
          maybeTail
          |> Maybe.map (\newTail -> replaceE__ exp (EList ws1 heads ws2 (Just newTail) ws3))

    EIndList _ ranges _ ->
      Nothing

    EIf ws1 predicate trueBranch falseBranch ws2 ->
      case maybeSymbolicallyEvaluateOneStep program predicate of
        Just newPredicate -> Just <| replaceE__ exp (EIf ws1 newPredicate trueBranch falseBranch ws2)
        Nothing ->
          case predicate.val.e__ of
            EBase _ (EBool True)  -> Just trueBranch
            EBase _ (EBool False) -> Just falseBranch
            _                     -> Nothing

    ECase ws1 scrutinee branches ws2 ->
      case maybeSymbolicMatchBranches program scrutinee branches of
        Just (branchEnv, branchExp) ->
          Just <| symbolicSubstitute (Dict.fromList branchEnv) branchExp

        Nothing ->
          maybeSymbolicallyEvaluateOneStep program scrutinee
          |> Maybe.map (\newScrutinee -> replaceE__ exp (ECase ws1 scrutinee branches ws2))

    ETypeCase ws1 scrutinee tbranches ws2 ->
      case tryExpMatchTBranches scrutinee tbranches of
        CannotCompare ->
          maybeSymbolicallyEvaluateOneStep program scrutinee
          |> Maybe.map (\newScrutinee -> replaceE__ exp (ETypeCase ws1 newScrutinee tbranches ws2))

        Match branchExp ->
          Just branchExp

        NoMatch ->
          Nothing

    EApp _ funcExp argExps _ ->
      maybeSymbolicallyApply program exp -- This may skip a few steps. It's okay.

    ELet _ _ True pat assigns body _ ->
      Nothing -- can't handle recursive substitution

    ELet ws1 kind False pat assigns body ws2 ->
      case maybeSymbolicMatch program pat assigns of
        Just env -> Just <| symbolicSubstitute (Dict.fromList env) body
        Nothing  ->
          maybeSymbolicallyEvaluateOneStep program assigns
          |> Maybe.map (\newAssigns -> replaceE__ exp (ELet ws1 kind False pat newAssigns body ws2))
          -- Might also try evaluating the let body...but that's almost certainly pointless.

    EComment _ _ body       -> Just body
    EOption _ _ _ _ body    -> Just body
    ETyp _ _ _ body _       -> Just body
    EColonType _ body _ _ _ -> Just body
    ETypeAlias _ _ _ body _ -> Just body



  -- case pat.val of
  --   PVar _ ident _            -> Just [(ident, exp)]
  --   PAs _ ident _ innerPat, _ ->
  --     trySymbolicMatch program innerPat exp
  --     |> matchMap (\env -> (ident, exp)::env)
  --   PList _ ps _ Nothing _ ->
  --     let evaled = symbolicallyEvaluateAsFarAsPossible program exp in
  --     case evaled.val.e__ of
  --       -- TODO: list must not have rest
  --       EList _ es _ Nothing _ ->
  --         case Utils.maybeZip ps es of
  --           Nothing    -> NoMatch
  --           Just pairs ->
  --             List.map (\(p, e) -> trySymbolicMatch program p e) pairs
  --             |> projMatches
  --
  --       _ ->
  --         CannotCompare
  --
  --   PList _ ps _ (Just restPat) _ ->
  --     let evaled = symbolicallyEvaluateAsFarAsPossible program exp in
  --     case evaled.val.e__ of
  --       EList _ es _ Nothing _ ->
  --         if List.length es < List.length ps then
  --           NoMatch
  --         else
  --           headExps, tailExps = Utils.split (List.length ps)
  --           let tryHeadMatch =
  --             Utils.zip ps headExps
  --             |> List.map (\(p, e) -> trySymbolicMatch program p e)
  --             |> projMatches
  --           in
  --           let tryTailMatch =
  --             trySymbolicMatch program restPat tailExps
  --           in
  --           [tryHeadMatch, tryTailMatch]
  --           |> projMatches
  --
  --       -- TODO: must have same number of heads
  --       EList _ es _ Just restExp _ ->
  --         if List.length es < List.length ps then
  --           NoMatch
  --         else if List.length es /= List.length ps then
  --           CannotCompare
  --         else
  --           let tryHeadMatch =
  --             Utils.zip ps es
  --             |> List.map (\(p, e) -> trySymbolicMatch program p e)
  --             |> projMatches
  --           in
  --           let tryTailMatch =
  --             trySymbolicMatch program restPat restExp
  --           in
  --           [tryHeadMatch, tryTailMatch]
  --           |> projMatches
  --
  --       _ ->
  --         CannotCompare
  --
  --   PConst _ n ->
  --     let evaled = symbolicallyEvaluateAsFarAsPossible program exp in
  --     case evaled.val.e__ of
  --       EConst _ num _ _ -> if n == num then Match [] else NoMatch
  --       _                -> CannotCompare
  --
  --   PBase _ bv ->
  --     let evaled = symbolicallyEvaluateAsFarAsPossible program exp in
  --     case evaled.val.e__ of
  --       EBase _ ev -> if bv == ev then Match [] else NoMatch
  --       _          -> CannotCompare
  --
  --   _ ->
  --     Debug.crash <| "Brainstorm.trySymbolicMatch huh? " ++ (unparsePat pat) ++ " vs " ++ (unparse exp)


-- Unlike trySymbolicMatch above, this function presumes the exp has been
-- evaluated to base values
tryMatchExp : Pat -> Exp -> SymbolicMatchResult
tryMatchExp pat exp =
  tryMatchExp_ tryMatchExp pat exp


tryMatchExp_ : (Pat -> Exp -> SymbolicMatchResult) -> Pat -> Exp -> SymbolicMatchResult
tryMatchExp_ recurse pat exp =
  case exp.val.e__ of
    EVal val ->
      case Eval.match (pat, val) of
        Nothing     -> NoMatch
        Just valEnv ->
          let expEnv =
            valEnv |> List.map (\(ident, val) -> (ident, eVal val))
          in
          Match expEnv

    _ ->
      case pat.val of
        PVar _ ident _         -> Match [(ident, exp)]
        PAs _ ident _ innerPat ->
          recurse innerPat exp
          |> matchMap (\env -> (ident, exp)::env)

        PList _ ps _ Nothing _ ->
          case exp.val.e__ of
            -- TODO: list must not have rest
            EList _ es _ Nothing _ ->
              case Utils.maybeZip ps es of
                Nothing    -> NoMatch
                Just pairs ->
                  List.map (\(p, e) -> recurse p e) pairs
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
                  |> List.map (\(p, e) -> recurse p e)
                  |> projMatches
                in
                let tryTailMatch =
                  recurse restPat (eList tailExps Nothing)
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
                  |> List.map (\(p, e) -> recurse p e)
                  |> projMatches
                in
                let tryTailMatch =
                  recurse restPat restExp
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
            EBase _ ev -> if bv == ev then Match [] else NoMatch
            _          -> CannotCompare


-- Preserves EIds even when children are substituted...I think that's what we want...
symbolicSubstitute : Dict.Dict Ident Exp -> Exp -> Exp
symbolicSubstitute subst exp =
  let fnSubst =
    subst
    |> Dict.map (\_ e -> always e)
  in
  transformVarsUntilBound fnSubst exp


maybeSymbolicallyApply : Exp -> Exp -> Maybe Exp
maybeSymbolicallyApply program appExp =
  let _ = Debug.log ("Attempting to apply " ++ (unparse appExp)) () in
  case appExp.val.e__ of
    EApp _ funcExp argExps _ ->
      let maybeFuncToApply =
        let evaledFuncExp = symbolicallyEvaluateAsFarAsPossible program funcExp in
        case evaledFuncExp.val.e__ of
          EFun _ pats body _ -> Just evaledFuncExp
          _                  -> Nothing
      in
      maybeFuncToApply
      `Maybe.andThen`
          (\funcExp ->
            case funcExp.val.e__ of
              EFun ws1 pats body ws2 ->
                -- Only apply recursive functions in prelude
                -- This isn't a completely reliable check for if the function is recursive (e.g. passing function to itself).
                if LangParser2.isPreludeEId funcExp.val.eid || (isActualEId funcExp.val.eid && (eidsAvailableAsVarsAt program funcExp.val.eid |> Dict.member funcExp.val.eid |> not)) then
                  let patsAndArgExps = Utils.zip pats argExps in
                  if List.length argExps <= List.length pats then
                    -- Full or partial application
                    let maybeSubst =
                      patsAndArgExps
                      |> Utils.foldlMaybe
                          (\(pat, argExp) subst ->
                            case maybeSymbolicMatch program pat argExp of
                              Nothing  -> let _ = Debug.log ("pat didn't match arg") (unparsePat pat, unparse argExp) in Nothing
                              Just env -> Just <| Dict.union (Dict.fromList <| List.reverse env) subst -- reverse b/c technically early entries overwrite later entires; pats shouldn't have repeat vars though
                          )
                          (Just Dict.empty)
                    in
                    let maybeResult =
                      case (maybeSubst, List.drop (List.length argExps) pats) of
                        (Just subst, [])        -> Just <| symbolicSubstitute subst body -- Full application
                        (Just subst, unapplied) ->
                          -- Have to rename unapplied arguments to ensure they don't collide with some
                          -- other variable of the same name that was substituted in.
                          let unappliedStrSubst =
                            identifiersListInPats unapplied
                            |> List.map (\ident -> (ident, ident ++ "UNAPPLIED" ++ (toString (Native.GloriousGlobalCounter.next ()))))
                            |> Dict.fromList
                          in
                          let unappliedExpSubst =
                            unappliedStrSubst |> Dict.map (\_ newIdent -> eVar newIdent)
                          in
                          let substWithUnapplied = Dict.union unappliedExpSubst subst in
                          let pats' = renameIdentifiersInPats unappliedStrSubst unapplied in
                          let body' = symbolicSubstitute substWithUnapplied body in
                          Just <| replaceE__ funcExp (EFun ws1 pats' body' ws2)

                        _ ->
                          Nothing
                    in
                    case maybeResult of
                      Just exp -> let _ = Debug.log ("symbolic result: " ++ unparse exp) () in maybeResult
                      Nothing  -> let _ = Debug.log "application failed" () in maybeResult
                  else
                    let _ = Debug.log ("arg count doesn't match pattern count") (pats, argExps) in
                    Nothing

                else
                  Nothing

              _ ->
                Debug.crash <| "Brainstorm.maybeSymbolicallyApply: this branch should be unreachable"
          )

    _ ->
      let _ = Debug.log ("not a func call on a variable") appExp.val.e__ in
      Nothing

-- TODO:
--   - Allow expansion into a list of constraints that must be satisfied simultaneously (e.g. [x1, y1] = [x2, y2]  =>  x1 = x2 and y1 = y2)
--   - Simple simplification of unary functions / unary ops i.e. myFunc arg1 = myFunc arg2 => arg1 = arg2 (this can actaully lead to undesirabl solutions... e.g. (x pt1) = (x pt2) => pt1 = pt2)
--   - Simplification of functions/ops with >1 arg
--      -- e.g. myFunc arg1 arg2 = myFunc arg1 arg3 => arg2 = arg3
--      -- e.g. myFunc arg1 arg2 = myFunc arg3 arg4 => arg1 = arg3 and arg2 = arg4 (this may be less desirable)
--   - Allow expansions of independent side
--     - Allow solving for locs (eids??) that initially appear on both sides, to match the power
--       of the original loc-based solver
solutionsForDependentProgramLocation : Exp -> Constraint -> List (Exp, EId)
solutionsForDependentProgramLocation program constraint =
  let { independent, dependent } = constraint in
  let recurse newConstraint      = solutionsForDependentProgramLocation program newConstraint in
  let recurseSimple newDependent = recurse (Constraint independent newDependent) in
  -- let eidToExpAnalysis eid       = Utils.justGet_ "Brainstorm.solutionsForDependentProgramLocation" eid dependencyAnalysis in
  -- let eidToCTerm eid             = expToCTerm (eidToExpAnalysis eid).exp in
  let depEId = dependent.val.eid in
  let solutionsIfInProgram eid =
    let candidateSolution = (independent, eid) in
    if isProgramEId eid
    then [candidateSolution]
    else []
  in
  let _ = Debug.log ("Exploring " ++ (unparse independent) ++ " = " ++ (unparse dependent)) () in
  case dependent.val.e__ of
    -- EVal val ->
    --   -- TODO: filter out dynamic/nonprogram EIds here
    --   let directSolutions = val.vtrace |> List.filter isProgramEId |> List.map (\eid -> (independent, eid)) in -- may or may not be redundant with expandedSolutions: revisit
    --   let expandedSolutions = val.vtrace |> List.concatMap (\eid -> recurseSimple (justFindExpByEId program eid)) in
    --   directSolutions ++ expandedSolutions
    --
    -- EDict _ -> []
    --
    -- EConst _ _ _ _ -> solutionsIfInProgram depEId
    --
    -- EBase _ _ -> solutionsIfInProgram depEId
    --
    -- EVar _ ident ->
    --   let solutionsOnThisTerm = solutionsIfInProgram depEId in
    --   case findVarAtEId program dependent of
    --     Just exp -> solutionsOnThisTerm ++ (recurseSimple exp)
    --     Nothing  -> solutionsOnThisTerm
    --
    -- EFun _ ps e _ ->
    --   -- Not sure when we'd end up solving for a function.
    --   -- Removing the function could cause the program to crash. I guess we'll check for
    --   -- that later in the pipeline and discard if so.
    --   solutionsIfInProgram depEId
    --
    -- EApp _ e1 es _ ->
    --   let solutionsOnThisTerm = solutionsIfInProgram depEId in
    --   -- Hmm, if the function is recursive this might not know when to stop applying...
    --   case maybeSymbolicallyApply program dependent of
    --     Just applied -> solutionsOnThisTerm ++ (recurseSimple applied)
    --     Nothing      -> solutionsOnThisTerm

    -- There's more algebra opportunities here
    -- Be dumb for now
    EOp _ op args _ ->
      let solutionsOnThisTerm = solutionsIfInProgram depEId in
      let op_ = op.val in
      let logBadOp ret =
        let _ = Debug.log "Bad op" (unparse dependent) in
        ret
      in
      let binOpInverseConstraints binOp lhs leftArg rightArg =
        List.map (\(indep, dep) -> { independent = indep, dependent = dep })
        <| case binOp of
          Plus  -> [ (eOp Minus [lhs, rightArg], leftArg), (eOp Minus [lhs, leftArg], rightArg) ]
          Minus -> [ (eOp Plus  [lhs, rightArg], leftArg), (eOp Minus [leftArg, lhs], rightArg) ]
          Mult  -> [ (eOp Div   [lhs, rightArg], leftArg), (eOp Div   [lhs, leftArg], rightArg) ]
          Div   -> [ (eOp Mult  [lhs, rightArg], leftArg), (eOp Div   [leftArg, lhs], rightArg) ]
          Pow   -> [ (eOp Pow [lhs, (eOp Div [eConstNoLoc 1, rightArg])], leftArg) ] --  lhs^(1/r) = l; but need log op to solve for r
          _     -> []
      in
      let binOpSolutions binOp args =
        case args of
          [leftArg, rightArg] ->
            binOpInverseConstraints binOp independent leftArg rightArg
            |> List.concatMap recurse

          _ ->
            logBadOp []
      in
      let unOpInverseMaybeConstraint unOp lhs arg =
        case unOp of
          Cos    -> Just { independent = eOp ArcCos [lhs],             dependent = arg }
          Sin    -> Just { independent = eOp ArcSin [lhs],             dependent = arg }
          ArcCos -> Just { independent = eOp Cos [lhs],                dependent = arg }
          ArcSin -> Just { independent = eOp Sin [lhs],                dependent = arg }
          Sqrt   -> Just { independent = eOp Pow [lhs, eConstNoLoc 2], dependent = arg }
          _      -> Nothing
      in
      let unOpSolutions unOp args =
        case args of
          [arg] ->
            case unOpInverseMaybeConstraint unOp independent arg of
              Just constraint -> recurse constraint
              Nothing         -> []

          _ ->
            logBadOp []
      in
      solutionsOnThisTerm ++
      case op_ of
        Plus       -> binOpSolutions op_ args -- need type checker here to differentiate from string plus; just rely on the crash check for now
        Minus      -> binOpSolutions op_ args
        Mult       -> binOpSolutions op_ args
        Div        -> binOpSolutions op_ args
        Mod        -> []
        Pow        -> binOpSolutions op_ args
        ArcTan2    -> []
        Lt         -> [] -- heh; handling inequalities could be fun
        Eq         -> []
        Pi         -> []
        DictEmpty  -> []
        DictInsert -> []
        DictGet    -> [] -- Ignoring DictGet is going to limit us when we switch SVG attrs to be a dict
          -- -- If we can symbolically evaluate the lookup, do so.
          -- case args of
          --   [keyExp, dictExp] ->
          --     case (cTermToVal dependencyAnalysis program keyCTerm, cTermToVal dependencyAnalysis program dictCTerm) of
          --       (Just keyVal, Just dictVal) ->
          --         case (Eval.valToDictKey [] keyVal.v_, dictVal.v_) of
          --           (Ok dictKey, VDict dict) ->
          --             case Dict.get dictKey dict of
          --               Just val -> recurseSimple (CTermVal val)
          --               Nothing  -> []
          --
          --           _ ->
          --             []
          --
          --       _ ->
          --         []
          --
          --   _ ->
          --     logBadOp []

        DictRemove -> []
        Cos        -> unOpSolutions op_ args
        Sin        -> unOpSolutions op_ args
        ArcCos     -> unOpSolutions op_ args
        ArcSin     -> unOpSolutions op_ args
        Floor      -> []
        Ceil       -> []
        Round      -> []
        Sqrt       -> unOpSolutions op_ args
        Explode    -> []
        DebugLog   ->
          case args of
            [arg] -> recurseSimple arg
            _     -> logBadOp []

        ToStr         -> []
        RangeOffset _ -> logBadOp []

    _ ->
      case maybeSymbolicallyEvaluateOneStep program dependent of
        Just newDependent -> (solutionsIfInProgram depEId) ++ (recurseSimple newDependent)
        Nothing           -> (solutionsIfInProgram depEId)


    -- EList _ es _ m _ -> -- TODO: both sides need to be destructured
    --   solutionsIfInProgram depEId
    --
    -- EIndList _ ranges _ ->
    --   solutionsIfInProgram depEId
    --
    -- EIf _ predicate trueBranch falseBranch _ ->
    --   (solutionsIfInProgram depEId) ++
    --   case (symbolicallyEvaluateAsFarAsPossible program predicate).val.e__ of
    --     EBase _ (EBool True)  -> recurseSimple trueBranch
    --     EBase _ (EBool False) -> recurseSimple falseBranch
    --     _                     -> []
    --
    -- ECase _ scrutinee branches _ ->
    --   (solutionsIfInProgram depEId) ++
    --   case maybeSymbolicMatchBranches program scrutinee branches of
    --     Nothing ->
    --       []
    --
    --     Just (branchEnv, branchExp) ->
    --       symbolicSubstitute (Dict.fromList branchEnv) branchExp
    --       |> recurseSimple
    --
    -- ETypeCase _ pat tbranches _ ->
    --   (solutionsIfInProgram depEId) ++
    --   [] -- TODO: Will need to talk to type checker etc
    --
    -- ELet _ letKind rec pat assign body _ ->
    --   (solutionsIfInProgram depEId) ++ (recurseSimple body)
    --
    -- EComment _ _ body ->
    --   recurseSimple body
    --
    -- EOption _ _ _ _ body ->
    --   recurseSimple body
    --
    -- ETyp _ pat tipe body _ ->
    --   (solutionsIfInProgram depEId) ++ -- If body is replaced the type annotation might be invalid, so do try replacing the annotation
    --   recurseSimple body
    --
    -- EColonType _ body _ tipe _ ->
    --   (solutionsIfInProgram depEId) ++ -- If body is replaced the type annotation might be invalid, so do try replacing the annotation
    --   recurseSimple body
    --
    -- ETypeAlias _ pat tipe body _ ->
    --   recurseSimple body


-- TODO: change StaticEnv/neededEId into set of EIds instead of single (multiple exps may resolve the same)
visibleVarsAt : Exp -> EId -> StaticEnv
visibleVarsAt program observerEId =
  let (_, _, programAnalysis) = staticEvalWithPrelude program in
  let errorMsg = "Brainstorm.visibleVarsAt: can't find eid " ++ (toString observerEId) ++ " in program analysis" in
  let observerExpAn = Utils.justGet_ errorMsg observerEId programAnalysis in
  observerExpAn.staticExpEnv
  |> List.map (\(ident, (maybeEId, maybeExp)) -> (ident, maybeEId))


eidsAvailableAsVarsAt : Exp -> EId -> Dict.Dict EId Ident
eidsAvailableAsVarsAt program observerEId =
  let (_, _, programAnalysis) = staticEvalWithPrelude program in
  let errorMsg = "Brainstorm.eidsAvailableAsVarsAt: can't find eid " ++ (toString observerEId) ++ " in program analysis" in
  let observerExpAn = Utils.justGet_ errorMsg observerEId programAnalysis in
  let (resultDict, _) =
    observerExpAn.staticExpEnv
    |> List.foldl
        (\(ident, (maybeEId, maybeExp)) (dict, seenVars) ->
          -- Make sure var isn't shadowed by something of higher precedence.
          if not <| Set.member ident seenVars then
            case maybeEId of
              Just eid ->
                let dict' =
                  (justGetExpressionAnalysis programAnalysis eid).equivalentEIds
                  |> Set.foldl (\equivEId d -> Dict.insert equivEId ident d) dict
                in
                (dict', Set.insert ident seenVars)
              Nothing ->
                (dict, Set.insert ident seenVars)
          else
            (dict, seenVars)
        )
        (Dict.empty, Set.empty)
  in
  resultDict


-- Need (Maybe EId) so that we can put function bound vars in the env but
-- they resolve to nothing, statically. If we simply leave them out the
-- lookup might wrongly find a shadowed variable higher up.
type alias StaticEnv = List (Ident, Maybe EId)
type alias StaticExpEnv = List (Ident, (Maybe EId, Maybe Exp))


findNameForEIdInStaticExpEnv : EId -> StaticExpEnv -> Maybe Ident
findNameForEIdInStaticExpEnv targetEId env =
  if isActualEId targetEId then
    let predicate (_, (maybeEId, maybeExp)) =
      case (maybeEId, maybeExp) of
        (Just envEId, _) -> targetEId == envEId
        (_, Just envExp) -> targetEId == envExp.val.eid
        _                -> False
    in
    Utils.findFirst predicate env |> Maybe.map (\(ident, _) -> ident)
  else
    Nothing


expEnvToStaticExpEnv : List (Ident, Exp) -> StaticExpEnv
expEnvToStaticExpEnv expEnv =
  expEnv
  |> List.map
      (\(ident, exp) ->
        if isActualEId exp.val.eid then
          (ident, (Just exp.val.eid, Just exp))
        else
          (ident, (Nothing, Just exp))
      )


patToDeadStaticExpEnv : Pat -> StaticExpEnv
patToDeadStaticExpEnv pat =
  identifiersListInPat pat
  |> List.map (\ident -> (ident, (Nothing, Nothing)))


expToDictKey : Exp -> Maybe (String, String)
expToDictKey exp =
  case exp.val.e__ of
    EConst _ n _ _            -> Just (toString n, "num")
    EBase _ (EBool b)         -> Just (toString b, "bool")
    EBase _ (EString _ s)     -> Just (toString s, "string")
    EBase _ ENull             -> Just ("", "null")
    EList _ heads _ Nothing _ ->
      heads
      |> List.map expToDictKey
      |> Utils.projJusts
      |> Maybe.map (\keyStrings -> (toString keyStrings, "list"))
    _                         -> Nothing


type alias ExpressionAnalysis =
  { exp                    : Exp
  , staticExpEnv           : StaticExpEnv
  , equivalentEIds         : Set.Set EId -- Which other expressions are guaranteed to resolve to this value?
  , immediatelyDependentOn : Set.Set EId
  , dependenceClosure      : Maybe (Set.Set EId)
  }


type alias ProgramAnalysis = Dict.Dict EId ExpressionAnalysis


-- Tarjan's strongly connected components algorithm.
--
-- This would be more efficient in a language that allowed mutation.
stronglyConnectedComponents programAnalysis =
  let eids = Dict.keys programAnalysis in
  let (_, _, sccs) =
    eids
    |> List.foldl
        (\eid acc ->
          let (i, allVisited, sccs) = acc in
          if Set.member eid allVisited then
            acc
          else
            let (i', stack', stackMap', allVisited', _, sccs') =
              tarjansSCC_ eid programAnalysis i [] Dict.empty allVisited sccs
            in
            if stack' /= [] || (Dict.size stackMap' > 0) then
              Debug.crash "Brainstorm.stronglyConnectedComponents: BUG"
            else
              (i', allVisited', sccs')

        )
        (0, Set.empty, [])
  in
  let _ =
    if List.sort eids /= (List.concat sccs |> List.sort) then
      Debug.crash <| "Missing EIds in SCCs: " ++ (toString <| Utils.listDiff eids (List.concat sccs))
    else
      ()
  in
  sccs


-- Where's a for loop when you need it?
tarjansSCC_ eid programAnalysis i stack stackMap allVisited sccs =
  -- returns (newStack, newStackMap)
  let pushStack eid i stack stackMap =
    (eid::stack, Dict.insert eid i stackMap)
  in
  -- returns (eid, newStack, newStackMap)
  let popStack stack stackMap =
    case stack of
      headEId::rest ->
        (headEId, rest, Dict.remove headEId stackMap)

      [] ->
        Debug.crash "Brainstorm.tarjansSCC_: Should never pop empty stack but did!"
  in
  -- returns (poppedList, newStack, newStackMap)
  let popUntil eid stack stackMap =
    let (poppedEId, newStack, newStackMap) = popStack stack stackMap in
    if poppedEId == eid then
      ([eid], newStack, newStackMap)
    else
      let (poppedList, retStack, retStackMap) = popUntil eid newStack newStackMap in
      (poppedEId::poppedList, retStack, retStackMap)
  in
  let i'                  = i + 1 in
  let (stack', stackMap') = pushStack eid i stack stackMap in
  let allVisited'         = Set.insert eid allVisited in
  let sccRootI'           = i in
  let expAn        = justGetExpressionAnalysis programAnalysis eid in
  let childrenEIds = Set.toList expAn.immediatelyDependentOn in
  let (retI, retStack, retStackMap, retAllVisited, lowestChildSccRootI, retSccs) =
    childrenEIds
    |> List.foldl
        (\childEId acc ->
          let (i'', stack'', stackMap'', allVisited'', sccRootI'', sccs'') = acc in
          case Dict.get childEId stackMap'' of
            Just childI ->
              let sccRootI''' = min childI sccRootI'' in
              (i'', stack'', stackMap'', allVisited'', sccRootI''', sccs'')

            Nothing ->
              if Set.member childEId allVisited'' then
                acc -- Visited on a previous pass. Already in an SCC that we're not in.
              else
                let (childI, childStack, childStackMap, childAllVisited, childSccRootI, childSccs) =
                  tarjansSCC_ childEId programAnalysis i'' stack'' stackMap'' allVisited'' sccs''
                in
                let sccRootI''' = min childSccRootI sccRootI'' in
                (childI, childStack, childStackMap, childAllVisited, sccRootI''', childSccs)
        )
        (i', stack', stackMap', allVisited', sccRootI', sccs)
  in
  if i == lowestChildSccRootI then
    let (newScc, retStack', retStackMap') = popUntil eid retStack retStackMap in
    (retI, retStack', retStackMap', retAllVisited, lowestChildSccRootI, newScc::retSccs)
  else
    (retI, retStack, retStackMap, retAllVisited, lowestChildSccRootI, retSccs)


type alias SCCId = EId -- Representative EId for each strongly connected component.

type alias SCCAnalysis =
  { eids                   : List EId
  , immediatelyDependentOn : Set.Set SCCId
  , dependenceClosure      : Maybe (Set.Set SCCId)
  , dependenceClosureEIds  : Maybe (Set.Set EId)
  }

type alias SCCsAnalysis = Dict.Dict SCCId SCCAnalysis


computeTransitiveDependenceClosure : ProgramAnalysis -> ProgramAnalysis
computeTransitiveDependenceClosure programAnalysis =
  let sccsAsEIdLists = stronglyConnectedComponents programAnalysis in
  let blankSccsAnalysis =
    sccsAsEIdLists
    |> List.map
        (\eids ->
          let sccId = Utils.head "Brainstorm.computeTransitiveDependenceClosure0: SCC should not be empty but was!" eids in
          let blankSccAnalysis =
            { eids                   = eids
            , immediatelyDependentOn = Set.empty
            , dependenceClosure      = Nothing
            , dependenceClosureEIds  = Nothing
            }
          in
          (sccId, blankSccAnalysis)
        )
    |> Dict.fromList
  in
  let eidToSccId =
    blankSccsAnalysis
    |> Dict.toList
    |> List.concatMap
        (\(sccId, sccAn) ->
          List.map (\eid -> (eid, sccId)) sccAn.eids
        )
    |> Dict.fromList
  in
  -- Fill in immediate dependence.
  let initialSccsAnalysis =
    blankSccsAnalysis
    |> Dict.map
        (\sccId sccAn ->
          let immediateDependenceEIds =
            sccAn.eids
            |> List.map (\eid -> (justGetExpressionAnalysis programAnalysis eid).immediatelyDependentOn)
            |> Utils.unionAll
            |> ((flip Set.diff) (Set.fromList sccAn.eids)) -- Remove EIds in this SCC.
          in
          let immediateDependentSccIds =
            immediateDependenceEIds
            |> Set.map (\eid -> Utils.justGet_ "Brainstorm.computeDAGTransitiveDependenceClosure1: Couldn't find SCC for eid!" eid eidToSccId)
          in
          { sccAn | immediatelyDependentOn = immediateDependentSccIds }
        )
  in
  -- Compute SCC transitive closure.
  let closedSccsAnalysis =
    computeSCCDAGTransitiveDependenceClosure initialSccsAnalysis
  in
  let closedProgramAnalysis =
    programAnalysis
    |> Dict.map
        (\eid expAn ->
          let expSccId = Utils.justGet_ ("Brainstorm.computeDAGTransitiveDependenceClosure2: Couldn't find SCC for eid! " ++ (toString eid)) eid eidToSccId in
          let expSccAn = Utils.justGet_ ("Brainstorm.computeDAGTransitiveDependenceClosure2: Couldn't find SCCAnalysis for sccId! " ++ (toString expSccId)) expSccId closedSccsAnalysis in
          { expAn | dependenceClosure = expSccAn.dependenceClosureEIds }
        )
  in
  -- Note that all nodes are now dependent on themselves.
  closedProgramAnalysis


-- Better be a DAG or this won't terminate.
computeSCCDAGTransitiveDependenceClosure : SCCsAnalysis -> SCCsAnalysis
computeSCCDAGTransitiveDependenceClosure sccsAnalysis =
  Dict.keys sccsAnalysis
  |> List.foldl
      ensureSCCDAGDependenceClosure
      sccsAnalysis


ensureSCCDAGDependenceClosure : SCCId -> SCCsAnalysis -> SCCsAnalysis
ensureSCCDAGDependenceClosure sccId sccsAnalysis =
  let sccAn = Utils.justGet_ "Brainstorm.ensureSCCDAGDependenceClosure: missing sccId" sccId sccsAnalysis in
  case sccAn.dependenceClosure of
    Just closure ->
      sccsAnalysis

    Nothing ->
      let depSccIds = Set.toList sccAn.immediatelyDependentOn in
      let sccsAnalysis' =
        depSccIds
        |> List.foldl
            ensureSCCDAGDependenceClosure
            sccsAnalysis
      in
      let depSccAns =
        depSccIds
        |> List.map
            (\depSccId ->
              Utils.justGet_ "Brainstorm.ensureSCCDAGDependenceClosure: missing dep sccId" depSccId sccsAnalysis'
            )
      in
      let dependenceClosure =
        depSccAns
        |> List.map
            (\depSccAn ->
              Utils.fromJust_ "Brainstorm.ensureSCCDAGDependenceClosure: closure not computed" depSccAn.dependenceClosure
            )
        |> Utils.unionAll
      in
      let dependenceClosureEIds =
        depSccAns
        |> List.map
            (\depSccAn ->
              Utils.fromJust_ "Brainstorm.ensureSCCDAGDependenceClosure: closure not computed" depSccAn.dependenceClosureEIds
            )
        |> Utils.unionAll
      in
      Dict.insert
        sccId
        { sccAn | dependenceClosure = Just dependenceClosure, dependenceClosureEIds = Just dependenceClosureEIds }
        sccsAnalysis'


-- Better be a DAG or this won't terminate.
-- computeDAGTransitiveDependenceClosure : ProgramAnalysis -> ProgramAnalysis
-- computeDAGTransitiveDependenceClosure programAnalysis =
--   Dict.keys programAnalysis
--   |> List.foldl -- left or right could have serious performance implications. I think right starts with deepest nodes. TODO: test
--       ensureDAGDependenceClosure
--       programAnalysis
--
--
-- ensureDAGDependenceClosure : EId -> ProgramAnalysis -> ProgramAnalysis
-- ensureDAGDependenceClosure eid programAnalysis =
--   let expAn = Utils.justGet_ "Brainstorm.ensureDependenceClosure: missing eid" eid programAnalysis in
--   case expAn.dependenceClosure of
--     Just closure ->
--       programAnalysis
--
--     Nothing ->
--       let depEIds = Set.toList expAn.immediatelyDependentOn in
--       let programAnalysis' =
--         depEIds
--         |> List.foldl
--             ensureDAGDependenceClosure
--             programAnalysis
--       in
--       let dependenceClosure =
--         depEIds
--         |> List.map
--             (\depEId ->
--               let depExpAn = Utils.justGet_ "Brainstorm.ensureDAGDependenceClosure: missing dep eid" depEId programAnalysis' in
--               Utils.fromJust_ "Brainstorm.ensureDAGDependenceClosure: closure not computed" depExpAn.dependenceClosure
--             )
--         |> Utils.unionAll
--       in
--       Dict.insert
--         eid
--         { expAn | dependenceClosure = Just dependenceClosure }
--         programAnalysis'



-- computeTransitiveDependenceClosure : ProgramAnalysis -> ProgramAnalysis
-- computeTransitiveDependenceClosure programAnalysis =
--   Dict.keys programAnalysis
--   |> List.foldl -- left or right could have serious performance implications. I think right starts with deepest nodes. TODO: test
--       ensureDependenceClosure
--       programAnalysis


-- ensureDependenceClosure : EId -> ProgramAnalysis -> ProgramAnalysis
-- ensureDependenceClosure eid programAnalysis =
--   let _ = Debug.log "computing closure of" eid in
--   let expAn = Utils.justGet_ "Brainstorm.ensureDependenceClosure: missing eid" eid programAnalysis in
--   case expAn.dependenceClosure of
--     Just closure ->
--       programAnalysis
--
--     Nothing ->
--       Dict.insert
--         eid
--         { expAn | dependenceClosure = Just <| dependenceClosure programAnalysis expAn.immediatelyDependentOn Set.empty }
--         programAnalysis


-- Wouldn't it be great if Elm had a method for plucking a single item from a set?
-- dependenceClosure : ProgramAnalysis -> Set.Set EId -> Set.Set EId -> Set.Set EId
-- dependenceClosure programAnalysis toVisit visited =
--   case toVisit |> Set.toList of
--     [] ->
--       visited
--
--     eid::_ ->
--       let expAn = Utils.justGet_ "Brainstorm.dependenceClosure: missing eid" eid programAnalysis in
--       case expAn.dependenceClosure of
--         Just closure ->
--           let visited'  = Set.insert eid visited in
--           let visited'' = Set.union closure visited' in
--           let toVisit'  = Set.diff toVisit visited'' in
--           dependenceClosure programAnalysis toVisit' visited''
--
--         Nothing ->
--           let visited'  = Set.insert eid visited in
--           let toVisit'  = Set.union expAn.immediatelyDependentOn toVisit in
--           let toVisit'' = Set.diff toVisit' visited' in
--           dependenceClosure programAnalysis toVisit'' visited'


(_, preludeStaticExpEnv, preludeProgramAnalysis) =
  staticEval_ True [] Dict.empty LangParser2.prelude


-- Also computes dependence closure
staticEvalWithPrelude : Exp -> (Maybe Exp, StaticExpEnv, ProgramAnalysis)
staticEvalWithPrelude program =
  let _ = Debug.log "static evaling program" () in
  let (maybeResult, staticEnvExp, programAnalysis) =
    staticEval_ True preludeStaticExpEnv preludeProgramAnalysis program
  in
  let _ = Debug.log "gathering equivalent EIds" () in
  let programAnalysis' = gatherEquivalentEIds programAnalysis in
  let _ = Debug.log "computing transitive closure" () in
  let programAnalysis'' = computeTransitiveDependenceClosure programAnalysis' in
  (maybeResult, staticEnvExp, programAnalysis'')


-- Need to do better than n^2 because all the top-level def's are equivalent to each other.
gatherEquivalentEIds : ProgramAnalysis -> ProgramAnalysis
gatherEquivalentEIds programAnalysis =
  -- Visit all connected EIds.
  -- Works no matter where we start because we ensured all connections were bidirectional.
  let equivalentEIds_ progAn visited toVisit =
    case Set.toList toVisit of
      []     -> visited
      eid::_ ->
        let visited'  = Set.insert eid visited in
        let toVisit'  = Set.union toVisit (justGetExpressionAnalysis progAn eid).equivalentEIds in
        let toVisit'' = Set.diff toVisit' visited' in
        equivalentEIds_ progAn visited' toVisit''
  in
  let (programAnalysis', equivSets, _) =
    programAnalysis
    |> Dict.foldl
        (\rootEId rootExpAn (progAn, equivSets, gathered) ->
          if Set.size rootExpAn.equivalentEIds == 1 || Set.member rootEId gathered then
            (progAn, rootExpAn.equivalentEIds::equivSets, gathered)
          else
            let equivSet = equivalentEIds_ progAn Set.empty rootExpAn.equivalentEIds in
            let gathered' = Set.union equivSet gathered in
            let progAn' =
              -- Change the set on all the equivalent expressions.
              equivSet
              |> Set.foldl
                  (\eid progAn' ->
                    let expAn = (justGetExpressionAnalysis progAn' eid) in
                    Dict.insert eid { expAn | equivalentEIds = equivSet } progAn'
                  )
                  progAn
            in
            (progAn', equivSet::equivSets, gathered')
        )
        (programAnalysis, [], Set.empty)
  in
  let _ = Debug.log "Equivalent sets" (List.map Set.toList equivSets) in
  programAnalysis'


-- Does not compute dependence closure or full equivalent EId sets.
-- isStaticEnv is a failed attempt to prevent recursion from diverging, left in here
-- because it saves some cycles during non-recursive function application.
-- (Problem: staticEval_ always visits all branches to do analysis; but for recursive functions a branch may diverge)
-- (Can't simply handle branches differently based on having isStaticEnv flag set to false during application; the very first function application could be taken in error)
-- (Turn off recursive evaluation, for now.)
-- If (Just exp) is returned, exp has an actual eid if that eid in the program is known to be statically equivalent to the returned "value".
staticEval_ : Bool -> StaticExpEnv -> ProgramAnalysis -> Exp -> (Maybe Exp, StaticExpEnv, ProgramAnalysis)
staticEval_ isStaticEnv env programAnalysis exp =

  -- let ret v_                         = ((Val v_ [e.val.eid], []), env) in
  -- let retAdd eid (v,envOut)          = ((Val v.v_ (eid::v.vtrace), []), envOut) in
  -- let retAddWs eid ((v,ws),envOut)   = ((Val v.v_ (eid::v.vtrace), ws), envOut) in
  -- let retAddThis_ (v,envOut)         = retAdd e.val.eid (v,envOut) in
  -- let retAddThis v                   = retAddThis_ (v, env) in
  -- let retBoth (v,w)                  = (({v | vtrace = e.val.eid :: v.vtrace},w), env) in
  -- let replaceEnv envOut (v,_)        = (v, envOut) in
  -- let addWidgets ws1 ((v1,ws2),env1) = ((v1, ws1 ++ ws2), env1) in

  let replaceExp newE__ =
    if isStaticEnv then -- This is a litlle conservative. More precise is to verify that exp doesn't depend on bound variables.
      replaceE__ exp newE__ -- Preserve EId for later extraction.
    else
      withDummyPos <| newE__
  in
  let thisEId = exp.val.eid in
  let programAnalysis' =
    if isStaticEnv && isActualEId thisEId then
      let expressionAnalysis =
        { exp                    = exp
        , staticExpEnv           = env
        , equivalentEIds         = Set.singleton thisEId
        , immediatelyDependentOn = childExps exp |> List.map (.eid << .val) |> List.filter isActualEId |> Set.fromList
        , dependenceClosure      = Nothing -- calculated separately
        }
      in
      Dict.insert thisEId expressionAnalysis programAnalysis
    else
      programAnalysis
  in
  let addDependencies progAn dependentEId dependencyEIds =
    let dependencyEIds' = Set.filter isActualEId dependencyEIds in
    if isStaticEnv && isActualEId dependentEId && (Set.size dependencyEIds' >= 0) then
      let expAn = justGetExpressionAnalysis progAn dependentEId in
      let expAn' = { expAn | immediatelyDependentOn = Set.union dependencyEIds' expAn.immediatelyDependentOn } in
      Dict.insert dependentEId expAn' progAn
    else
      progAn
  in
  let makeSubtreesDependent progAn subtrees dependencyEIds =
    if isStaticEnv then
      subtrees
      |> List.concatMap allActualEIds
      |> List.foldl
          (\descendantEId progAn ->
            addDependencies progAn descendantEId dependencyEIds
          )
          progAn
    else
      progAn
  in
  let addDependency progAn eid =
    addDependencies progAn thisEId (Set.singleton eid)
  in
  let addEquivalentEId msg progAn eid1 eid2 =
    if isStaticEnv && isActualEId eid1 && isActualEId eid2 then
      -- Just need to make sure connection is bi-directional to ensure we
      -- can later do a spanning tree search of an equivalent eid group.
      let expAn1 =
        Utils.justGet_ ("Couldn't find eid1 " ++ (toString eid1) ++ " with eid1 " ++ (toString eid1) ++ " and eid2 " ++ (toString eid2) ++ ". " ++ msg ++ " Line: " ++ (toString exp.start.line)) eid1 progAn
      in
      let expAn2 =
        Utils.justGet_ ("Couldn't find eid2 " ++ (toString eid2) ++ " with eid1 " ++ (toString eid1) ++ " and eid2 " ++ (toString eid2) ++ ". " ++ msg ++ " Line: " ++ (toString exp.start.line)) eid2 progAn
      in
      let progAn'  = Dict.insert eid1 { expAn1 | equivalentEIds = Set.insert eid2 expAn1.equivalentEIds } progAn in
      let progAn'' = Dict.insert eid2 { expAn2 | equivalentEIds = Set.insert eid1 expAn2.equivalentEIds } progAn' in
      progAn''
    else
      progAn
  in
  let retDict retProgramAnalysis e      = (Just e, env, retProgramAnalysis) in
  let retDictNothing retProgramAnalysis = (Nothing, env, retProgramAnalysis) in
  let ret e                             = retDict programAnalysis' e in
  let retNothing                        = retDictNothing programAnalysis' in
  let retRecurseEquivalentExpression msg env progAn exp =
    let (maybeResult, resultEnv, progAn') = staticEval_ isStaticEnv env progAn exp in
    let progAn'' = addEquivalentEId msg progAn' thisEId exp.val.eid in
    (maybeResult, resultEnv, progAn'')
  in
  let recurseAll env programAnalysis exps =
    let (resultMaybes, _, programAnalyses) =
      exps |> List.map (staticEval_ isStaticEnv env programAnalysis) |> Utils.unzip3
    in
    let mergedProgramAnalysis = List.foldl Dict.union programAnalysis programAnalyses in
    (resultMaybes, mergedProgramAnalysis)
  in
  case exp.val.e__ of
    EVal _                 -> Debug.crash "Brainstorm.staticEval_: figure out whether to handle EVal"
    EDict _                -> Debug.crash "Brainstorm.staticEval_: figure out whether to handle EDict"
    EConst _ n loc wd      -> ret exp
    EBase _ bVal           -> ret exp
    EVar _ ident           ->
      let _ =
        if ident == "rect1" then
          Debug.log "rect1" (Utils.maybeFind ident env)
        else
          Nothing
      in
      case Utils.maybeFind ident env of
        Just (maybeEId, maybeExp) ->
          -- We could say that a variable reference is dependent on the the expression with the pattern
          -- that defined it (e.g. dependent on the let statement rather than just the assign exp in the
          -- let, or the function that introduced this variable as an argument) but that's not needed in
          -- our case.
          --
          -- The dependence graph is used only to ensure that replacing the dependent expression will
          -- not change the value of the independent expression (that is, the indep expression really
          -- is independent of the dependent expression). In such a case, e.g. the assign of a let is
          -- a child of the let and so the let cannot be replaced.
          let programAnalysis'' =
            case maybeEId of
              Just eid -> addEquivalentEId ("EVar " ++ ident) (addDependency programAnalysis' eid) thisEId eid
              Nothing  -> programAnalysis'
          in
          (maybeExp, env, programAnalysis'')

        Nothing ->
          (Nothing, env, programAnalysis')

    EFun _ pats body _     ->
      -- Log static envs of func body.
      let (_, _, programAnalysis'') =
        let patsEnv =
          identifiersSetInPats pats
          |> Set.toList
          |> List.map (\ident -> (ident, (Nothing, Nothing)))
        in
        let bodyEnv = patsEnv ++ env in
        staticEval_ isStaticEnv bodyEnv programAnalysis' body
      in
      retDict programAnalysis'' exp

    EOp ws1 op argExps ws2 ->
      let op_ = op.val in
      let (evaledArgExpMaybes, programAnalysis'') = recurseAll env programAnalysis' argExps in
      let ret'        = retDict programAnalysis'' in
      let retNothing' = retDictNothing programAnalysis'' in
      let retConst n  = ret' <| replaceExp (EConst ws1 n dummyLoc noWidgetDecl) in
      let nullaryOp args n =
        case args of
          [] -> retConst n
          _  -> retNothing'
      in
      let unaryMathOp op_ args =
        case args of
          [EConst _ n _ _] ->
            case op_ of
              Cos    -> retConst (cos n)
              Sin    -> retConst (sin n)
              ArcCos -> retConst (acos n)
              ArcSin -> retConst (asin n)
              Floor  -> retConst (toFloat <| floor n)
              Ceil   -> retConst (toFloat <| ceiling n)
              Round  -> retConst (toFloat <| round n)
              Sqrt   -> retConst (sqrt n)
              _      -> retNothing'

          _ ->
            retNothing'
      in
      let binMathOp op_ args =
        case args of
          [EConst _ n1 _ _, EConst _ n2 _ _] ->
            case op_ of
              Plus    -> retConst (n1 + n2)
              Minus   -> retConst (n1 - n2)
              Mult    -> retConst (n1 * n2)
              Div     -> retConst (n1 / n2)
              Pow     -> retConst (n1 ^ n2)
              Mod     -> retConst (toFloat <| (floor n1) % (floor n2))
              ArcTan2 -> retConst (atan2 n1 n2)
              _       -> retNothing'

          _ ->
            retNothing'
      in
      case evaledArgExpMaybes |> Utils.projJusts of
        Nothing ->
          retNothing'

        Just evaledArgExps ->
          let args = evaledArgExps |> List.map (.e__ << .val) in
          case op_ of
            Plus ->
              case args of
                [EBase _ (EString _ s1), EBase _ (EString _ s2)] -> ret' <| replaceExp (EBase ws1 (EString defaultQuoteChar (s1 ++ s2)))
                _                                                -> binMathOp op_ args
            Minus   -> binMathOp op_ args
            Mult    -> binMathOp op_ args
            Div     -> binMathOp op_ args
            Mod     -> binMathOp op_ args
            Pow     -> binMathOp op_ args
            ArcTan2 -> binMathOp op_ args
            Lt      -> case args of
              [EConst _ n1 _ _, EConst _ n2 _ _] -> ret' <| replaceExp (EBase ws1 (EBool (n1 < n2)))
              _                                  -> retNothing'
            Eq            -> case args of
              [EConst _ n1 _ _,        EConst _ n2 _ _]        -> ret' <| replaceExp (EBase ws1 (EBool (n1 == n2)))
              [EBase _ (EString _ s1), EBase _ (EString _ s2)] -> ret' <| replaceExp (EBase ws1 (EBool (s1 == s2)))
              _                                                -> retNothing'
            Pi         -> nullaryOp args pi
            DictEmpty  -> ret' <| replaceExp (EDict Dict.empty)
            DictInsert ->
              case evaledArgExps of
                [eKey, eVal, dict] ->
                  case (dict.val.e__, expToDictKey eKey) of
                    (EDict d, Just dKey) -> ret' <| replaceExp (EDict (Dict.insert dKey eVal d))
                    _                    -> retNothing'
                _ ->
                  retNothing'
            DictGet ->
              case evaledArgExps of
                [eKey, dict] ->
                  case (dict.val.e__, expToDictKey eKey) of
                    (EDict d, Just dKey) ->
                      let retVal = Utils.getWithDefault dKey (replaceExp (EBase ws1 ENull)) d in
                      let programAnalysis''' = addEquivalentEId ("DictGet " ++ (toString dKey)) programAnalysis'' thisEId retVal.val.eid in -- In case we return null, thisEId == retVal.val.eid, but it works because being equivalent to yourself is fine.
                      retDict programAnalysis''' retVal
                    _ ->
                      retNothing'
                _ ->
                  retNothing'
            DictRemove ->
              case evaledArgExps of
                [eKey, dict] ->
                  case (dict.val.e__, expToDictKey eKey) of
                    (EDict d, Just dKey) -> ret' <| replaceExp (EDict (Dict.remove dKey d))
                    _                    -> retNothing'
                _ ->
                  retNothing'
            Cos        -> unaryMathOp op_ args
            Sin        -> unaryMathOp op_ args
            ArcCos     -> unaryMathOp op_ args
            ArcSin     -> unaryMathOp op_ args
            Floor      -> unaryMathOp op_ args
            Ceil       -> unaryMathOp op_ args
            Round      -> unaryMathOp op_ args
            Sqrt       -> unaryMathOp op_ args
            Explode    ->
              case args of
                [EBase _ (EString _ s)] -> ret' <| replaceExp (EList ws1 (List.map (eStr << String.fromChar) (String.toList s)) "" Nothing ws2)
                _                       -> retNothing'
            DebugLog ->
              case argExps of
                [childExp] -> retRecurseEquivalentExpression "DebugLog" env programAnalysis' childExp
                _          -> retNothing'
            ToStr         -> retNothing'
            RangeOffset _ -> retNothing'

    EList ws1 heads ws2 maybeRest ws3 ->
      let (evaledHeadMaybes, programAnalysis'') = recurseAll env programAnalysis' heads in
      let (evaledRestMaybe, programAnalysis''') =
        case maybeRest of
          Just restExp ->
            let (evaledRestMaybe, _, programAnalysis''') = staticEval_ isStaticEnv env programAnalysis'' restExp in
            (evaledRestMaybe, programAnalysis''')

          Nothing ->
            (Nothing, programAnalysis'')
      in
      let ret'        = retDict programAnalysis''' in
      let retNothing' = retDictNothing programAnalysis''' in
      case evaledHeadMaybes |> Utils.projJusts of
        Nothing -> retNothing'
        Just headsEvaled ->
          case (maybeRest, evaledRestMaybe) of
            (Nothing, _)                 -> ret' <| replaceExp (EList ws1 headsEvaled ws2 Nothing ws3)
            (Just rest, Nothing)         -> retNothing'
            (Just rest, Just restEvaled) ->
              case restEvaled.val.e__ of
                EList _ restElements _ Nothing _ -> ret' <| replaceExp (EList ws1 (headsEvaled ++ restElements) ws2 Nothing ws3)
                _                                -> retNothing'

    EIndList ws1 ranges ws2 ->
      let (evaledChildMaybes, programAnalysis'') = recurseAll env programAnalysis' (childExps exp) in
      retDictNothing programAnalysis'' -- TODO: I don't want to deal with ranges

    EIf ws1 predicate trueBranch falseBranch ws2 ->
      let (evaledPartMaybes, programAnalysis'') = recurseAll env programAnalysis' [predicate, trueBranch, falseBranch] in
      -- Conservatively make the branch descendants dependent on the predicate.
      let programAnalysis''' =
        makeSubtreesDependent programAnalysis'' [trueBranch, falseBranch] (Set.singleton predicate.val.eid)
      in
      case evaledPartMaybes of
        [maybeEvaledPredicate, maybeEvaledTrueBranch, maybeEvaledFalseBranch] ->
          case maybeEvaledPredicate |> Maybe.map (.e__ << .val) of
            Just (EBase _ (EBool True)) ->
              let programAnalysis'''' = addEquivalentEId ("EIf true ") programAnalysis''' thisEId trueBranch.val.eid in
              (maybeEvaledTrueBranch,  env, programAnalysis'''')

            Just (EBase _ (EBool False)) ->
              let programAnalysis'''' = addEquivalentEId ("EIf false ") programAnalysis''' thisEId falseBranch.val.eid in
              (maybeEvaledFalseBranch, env, programAnalysis'''')

            _ ->
              (Nothing, env, programAnalysis''')

        _ ->
          Debug.crash "Brainstorm.staticEval_ if: this branch should be unreachable"

    ECase ws1 scrutinee branches ws2 ->
      let (maybeScrutineeEvaled, _, programAnalysis'') = staticEval_ isStaticEnv env programAnalysis' scrutinee in
      let programAnalysis''' =
        branches
        |> List.map .val
        |> List.foldl
            (\(Branch_ _ bPat bExp _) progAn ->
              let patEnv = patToDeadStaticExpEnv bPat in
              let branchEnv = patEnv ++ env in
              let (_, _, progAn') = staticEval_ isStaticEnv branchEnv progAn bExp in
              progAn'
            )
            programAnalysis''
      in
      let (maybeResult, resultEnv, resultProgAn) =
        case maybeScrutineeEvaled of
          Nothing ->
            retDictNothing programAnalysis'''

          Just scrutineeEvaled ->
            case maybeExpMatchBranches scrutineeEvaled branches of
              Nothing ->
                retDictNothing programAnalysis'''

              Just (branchEnv, branchExp) ->
                let branchStaticeExpEnv = expEnvToStaticExpEnv branchEnv in
                -- Yes, this is a re-evaluation. May get more specific results with
                -- a more specific env.
                retRecurseEquivalentExpression ("ECase ") (branchStaticeExpEnv ++ env) programAnalysis''' branchExp
      in
      -- Conservatively make all branch expression descendants dependent on the scrutinee.
      let resultProgAn' =
        makeSubtreesDependent resultProgAn (branchExps branches) (Set.singleton scrutinee.val.eid)
      in
      (maybeResult, resultEnv, resultProgAn')

    ETypeCase _ scrutinee tbranches _ ->
      let (_, programAnalysis'') = recurseAll env programAnalysis' (childExps exp) in
      -- Conservatively make all branch expression descendants dependent on the scrutinee.
      let programAnalysis''' =
        makeSubtreesDependent programAnalysis'' (tbranchExps tbranches) (Set.singleton scrutinee.val.eid)
      in
      retDictNothing programAnalysis''' -- TODO handle typecase evaluation...probably need to at least handle null

    EApp ws1 funcExp argExps ws2 ->
      -- Only full application for now.
      let (funcExpEvaledMaybe, _, programAnalysis'') = staticEval_ isStaticEnv env programAnalysis' funcExp in
      let (evaledArgExpMaybes, programAnalysis''')   = recurseAll env programAnalysis'' argExps in
      retDictNothing programAnalysis'''
      -- case (funcExpEvaledMaybe, evaledArgExpMaybes |> Utils.projJusts) of
      --   (Just funcExpEvaled, Just evaledArgExps) ->
      --     case (funcExpEvaled.val.e__, Dict.get funcExpEvaled.val.eid programAnalysis''') of
      --       (EFun _ pats body _, Just expAnalysisAtFuncDefinition) ->
      --         -- Recursive evaluation turned off right now. See comment at top of function.
      --         -- TODO: This isn't completely robust against recursion (other vars assigned, function itself passed as arg).
      --         if Nothing == findNameForEIdInStaticExpEnv funcExpEvaled.val.eid expAnalysisAtFuncDefinition.staticExpEnv then
      --           case Utils.maybeZip pats evaledArgExps of
      --             Nothing ->
      --               retDictNothing programAnalysis'''
      --
      --             Just pairs ->
      --               let patEnvMaybes = pairs |> List.map (\(pat, arg) -> maybeMatchExp pat arg) in
      --               case patEnvMaybes |> Utils.projJusts of
      --                 Nothing ->
      --                   retDictNothing programAnalysis'''
      --
      --                 Just patEnvs ->
      --                   let patStaticExpEnv = List.concatMap expEnvToStaticExpEnv (List.reverse patEnvs) in -- reverse hardly matters, shouldn't have duplicate names in patterns
      --                   -- Discard returned staticExpEnv dict because the body is not
      --                   -- being evaluated in a static environment.
      --                   -- let _ = Debug.log ("Applying " ++ unparse funcExpEvaled ++ " " ++ (String.join " " <| List.map unparse evaledArgExps)) () in
      --                   let (maybeApplied, _, _) = staticEval_ False (patStaticExpEnv ++ expAnalysisAtFuncDefinition.staticExpEnv) programAnalysis''' body in
      --                   (maybeApplied, env, programAnalysis''')
      --         else
      --           retDictNothing programAnalysis'''
      --
      --       _ ->
      --         retDictNothing programAnalysis'''
      --
      --   _ ->
      --     retDictNothing programAnalysis'''

    ELet _ _ isRecursive pat assign body _ ->
      let assignEnv =
        case (isRecursive, pat.val) of
          (True, PVar _ ident _)  -> (expEnvToStaticExpEnv [(ident, assign)]) ++ env
          (True, PList _ _ _ _ _) -> env -- mutually recursive functions not supported
          _                       -> env
      in
      let (assignExpEvaledMaybe, _, programAnalysis'') = staticEval_ isStaticEnv assignEnv programAnalysis' assign in
      let patStaticExpEnv =
        case assignExpEvaledMaybe of
          Just assignExpEvaled ->
            case maybeMatchExp pat assignExpEvaled of
              Nothing  -> patToDeadStaticExpEnv pat
              Just env -> expEnvToStaticExpEnv env

          Nothing ->
            -- Try to match unsimplified expression.
            case maybeMatchExp pat assign of
              Nothing  -> patToDeadStaticExpEnv pat
              Just env -> expEnvToStaticExpEnv env
      in
      retRecurseEquivalentExpression ("ELet " ++ (toString isRecursive)) (patStaticExpEnv ++ env) programAnalysis'' body

    EComment _ _ body       -> retRecurseEquivalentExpression "EComment"   env programAnalysis' body
    EOption _ _ _ _ body    -> retRecurseEquivalentExpression "EOption"    env programAnalysis' body
    ETyp _ _ _ body _       -> retRecurseEquivalentExpression "ETyp"       env programAnalysis' body
    EColonType _ body _ _ _ -> retRecurseEquivalentExpression "EColonType" env programAnalysis' body
    ETypeAlias _ _ _ body _ -> retRecurseEquivalentExpression "ETypeAlias" env programAnalysis' body


-- staticEnvEval : StaticEnv -> Exp -> StaticEnv
-- staticEnvEval sEnv exp =
--     let ret v_                         = ((Val v_ [e.val.eid], []), env) in
--     let retAdd eid (v,envOut)          = ((Val v.v_ (eid::v.vtrace), []), envOut) in
--     let retAddWs eid ((v,ws),envOut)   = ((Val v.v_ (eid::v.vtrace), ws), envOut) in
--     let retAddThis_ (v,envOut)         = retAdd e.val.eid (v,envOut) in
--     let retAddThis v                   = retAddThis_ (v, env) in
--     let retBoth (v,w)                  = (({v | vtrace = e.val.eid :: v.vtrace},w), env) in
--     let replaceEnv envOut (v,_)        = (v, envOut) in
--     let addWidgets ws1 ((v1,ws2),env1) = ((v1, ws1 ++ ws2), env1) in
--
--     let bt' =
--       if e.start.line >= 1 -- Ignore desugared internal expressions
--       then e::bt
--       else bt
--     in
--
--     case e.val.e__ of
--
--   let eid = exp.val.eid in
--   -- Returns augmented environment (for let/def)
--   let staticEnvSimpleApp sEnv funcExp singleArgExp =
--     let addWidgets ws1 ((v1,ws2),env1) = ((v1, ws1 ++ ws2), env1) in
--     case eval_ env bt' funcExp of
--       Err s       -> Err s
--       Ok (v1,ws1) ->
--         case eval_ env bt' singleArgExp of
--           Err s       -> Err s
--           Ok (v2,ws2) ->
--             let ws = ws1 ++ ws2 in
--             case v1.v_ of
--               VClosure Nothing p eBody env' ->
--                 case (p, v2) `cons` Just env' of
--                   Just env'' -> Result.map (addWidgets ws) <| eval env'' bt' eBody -- TODO add eid to vTrace
--                   _          -> errorWithBacktrace bt' <| strPos funcExp.start ++ "bad environment"
--               VClosure (Just f) p eBody env' ->
--                 case (pVar f, v1) `cons` ((p, v2) `cons` Just env') of
--                   Just env'' -> Result.map (addWidgets ws) <| eval env'' bt' eBody -- TODO add eid to vTrace
--                   _          -> errorWithBacktrace bt' <| strPos funcExp.start ++ "bad environment"
--               _ ->
--                 errorWithBacktrace bt' <| strPos funcExp.start ++ " not a function"
--   case exp.val.e__ of
--     EConst _ i l wd             -> sEnv
--     EBase _ v                   -> sEnv
--     EVar _ x                    -> sEnv
--     EFun _ [p] e _              -> sEnv
--     EOp _ op es _               -> sEnv
--     EList _ es _ m _            -> sEnv
--     EIndList _ rs _             -> sEnv
--     EIf _ e1 e2 e3 _            -> sEnv
--     ECase _ e1 bs _             -> sEnv
--     ETypeCase _ pat tbranches _ -> sEnv
--     EApp _ e1 [e2] _            -> sEnv
--
--     ELet _ _ False p e1 e2 _ ->
--       -- Return env that the let body returns (so that programs return their final top-level environment)
--       Result.map (retAddWs e2.val.eid) <| evalSimpleApp env bt bt' (eFun [p] e2) e1
--
--     ELet _ _ True p e1 e2 _ ->
--       case eval_ env bt' e1 of
--         Err s       -> Err s
--         Ok (v1,ws1) ->
--           case (p.val, v1.v_) of
--             (PVar _ f _, VClosure Nothing x body env') ->
--               let _   = Utils.assert "eval letrec" (env == env') in
--               let v1' = Val (VClosure (Just f) x body env) v1.vtrace in
--               case (pVar f, v1') `cons` Just env of
--                 Just env' -> Result.map (addWidgets ws1) <| eval env' bt' e2
--                 _         -> errorWithBacktrace (e::bt) <| strPos e.start ++ "bad ELet"
--             (PList _ _ _ _ _, _) ->
--               errorWithBacktrace (e::bt) <|
--                 strPos e1.start ++
--                 "mutually recursive functions (i.e. letrec [...] [...] e) \
--                  not yet implemented"
--                  -- Implementation also requires modifications to LangTransform.simply
--                  -- so that clean up doesn't prune the funtions.
--             _ ->
--               errorWithBacktrace (e::bt) <| strPos e.start ++ "bad ELet"
--
--     EComment _ _ e1       -> eval env bt e1
--     EOption _ _ _ _ e1    -> eval env bt e1
--     ETyp _ _ _ e1 _       -> eval env bt e1
--     EColonType _ e1 _ _ _ -> eval env bt e1
--     ETypeAlias _ _ _ e1 _ -> eval env bt e1
--
--     -- abstract syntactic sugar
--
--     EFun _ ps e1 _           -> Result.map (retAddWs e1.val.eid) <| eval env bt' (eFun ps e1)
--     EApp _ e1 [] _           -> errorWithBacktrace (e::bt) <| strPos e1.start ++ " application with no arguments"
--     EApp _ e1 es _           -> Result.map (retAddWs e.val.eid)  <| eval env bt' (eApp e1 es)


findWithAncestors : Exp -> EId -> List Exp
findWithAncestors program eid =
  findAllWithAncestors (\exp -> eid == exp.val.eid) program
  |> Utils.head ("Brainstorm.findWithAncestors: can't find EId " ++ (toString eid))


findExpByEId : Exp -> EId -> Maybe Exp
findExpByEId exp eid =
  if exp.val.eid == eid then
    Just exp
  else
    Utils.mapFirstSuccess (\child -> findExpByEId child eid) (childExps exp)


justFindExpByEId : Exp -> EId -> Exp
justFindExpByEId exp eid =
  findExpByEId exp eid
  |> Utils.fromJust_ ("Couldn't find eid " ++ (toString eid))


-- Which EIds do we need in scope were we to lift this expression?
-- This is where bound variables fail the transformation.
-- TODO: make sure recursive vars fail the transformation.
freeVarEIdsInExp : Exp -> EId -> Maybe (List (Ident, EId))
freeVarEIdsInExp program mobileEId =
  let mobileExp   = justFindExpByEId program mobileEId in
  let freeIdents  = freeIdentifiers mobileExp |> Set.toList in
  let visibleVars = visibleVarsAt program mobileEId in
  freeIdents
  |> Utils.foldlMaybe
      (\freeIdent freeVarEIds ->
        case Utils.maybeFind freeIdent visibleVars of
          Just (Just eid) -> Just ((freeIdent, eid)::freeVarEIds)
          _               -> Nothing -- not found in static env, or bound
      )
      (Just [])


-- Replace the given EIds with given variable names
turnExpsToVars : Exp -> Dict.Dict EId Ident -> Exp
turnExpsToVars exp renamings =
  let varRenamer e =
    case Dict.get e.val.eid renamings of
      Nothing ->
        e

      Just newIdent ->
        let ws = precedingWhitespace e in
        replaceE__ e (EVar ws newIdent)
  in
  mapExp varRenamer exp


-- Performs renamings in program, but returns renaming on mobileEId
-- so caller can use it on "off-program" expressions. TODO: revisit, may not need renamings
liftSoVisbleTo : Exp -> EId -> EId -> Maybe (Exp, Dict.Dict EId Ident)
liftSoVisbleTo originalProgram mobileEId observerEId =
  let _ = Debug.log "looking for eid" mobileEId in
  case Dict.get mobileEId (eidsAvailableAsVarsAt originalProgram observerEId) of
    Just ident ->
      let _ = Debug.log "found as" ident in
      let renamings = Dict.singleton mobileEId ident in
      Just (originalProgram, renamings)

    Nothing ->
      let deepestCommonAncestorEId =
        let deepestCommonAncestor =
          let ancestorsAndMobileExp   = findWithAncestors originalProgram mobileEId in
          let ancestorsAndObserverExp = findWithAncestors originalProgram observerEId in
          let commonAncestors = Utils.commonPrefix [ancestorsAndMobileExp, ancestorsAndObserverExp] in
          let errorMsg = "couldn't find common ancestor of " ++ (toString mobileEId) ++ " and " ++ (toString observerEId) in
          Utils.last errorMsg commonAncestors
        in
        deepestCommonAncestor.val.eid
      in
      let maybeDependenciesLifted =
        case freeVarEIdsInExp originalProgram mobileEId of
          Nothing ->
            let _ = Debug.log "not found, can't lift" () in
            Nothing -- need to lift a bound var. TODO: allow lifting if we don't exceed its scope

          Just varEidsToLift ->
            let _ = Debug.log "found, trying to lift dependencies" varEidsToLift in
            varEidsToLift
            |> Utils.foldlMaybe
                (\(varName, eidToLift) program ->
                  liftSoVisbleTo program eidToLift deepestCommonAncestorEId
                  |> Maybe.map
                      (\(newProgram, renamings) ->
                        -- MobileEId is going to be moved, so it needs to use the lifted name, not the original.
                        let mobileExp = justFindExpByEId program mobileEId in
                        let mobileExp' =
                          renameVarsUntilBound (Dict.singleton varName (Utils.justGet_ "Brainstorm.liftSoVisbleTo: recursion shoulda worked" eidToLift renamings)) mobileExp
                        in
                        replaceExpNode mobileExp.val.eid mobileExp' newProgram
                      )
                )
                (Just originalProgram)
      in
      maybeDependenciesLifted
      |> Maybe.map
          (\dependenciesLifted ->
            -- TODO: better names
            let liftedName = ValueBasedTransform.nonCollidingName "lifted" (identifiersSet dependenciesLifted) in
            let deepestCommonAncestor = justFindExpByEId dependenciesLifted deepestCommonAncestorEId in
            let expToLift             = justFindExpByEId deepestCommonAncestor mobileEId in
            let renamings             = Dict.singleton mobileEId liftedName in
            let deepestCommonAncestorTargetExpRenamed = turnExpsToVars deepestCommonAncestor renamings in
            -- c.f. ValueBasedTransform.wrapWithLets for possible code deduplication
            -- we need to preserve EIds here
            -- TODO: pretty whitespace
            let liftedSubtree = eLets [(liftedName, (replacePrecedingWhitespace " " expToLift))] deepestCommonAncestorTargetExpRenamed in
            let lifted = replaceExpNode deepestCommonAncestorEId liftedSubtree dependenciesLifted in
            let _ = Debug.log (unparse lifted) () in
            (lifted, Dict.singleton mobileEId liftedName)
          )


-- Recurse down to EId's in program/prelude
-- (eid's not in program/prelude are inserted expressions)
neededEIds : Env -> Exp -> List EId
neededEIds programEnv exp =
  let recurse = neededEIds programEnv in
  let eid = exp.val.eid in
  if isActualEId eid then
    [eid]
  else
    case exp.val.e__ of
      EVar _ x ->
        case Utils.maybeFind x programEnv of
          Just val -> [val.vtrace |> Utils.head ("expected var " ++ x ++ " to resolve to val with non-empty vtrace")]
          Nothing  -> Debug.crash <| "independent side has reference to unknown var " ++ x

      _ ->
        List.concatMap recurse (childExps exp)


redefineExp programEnv originalProgram indep depEId =
  let maybeLiftedAndRenamings =
    (Debug.log "neededEIDs" <| neededEIds programEnv indep)
    |> Utils.foldlMaybe
        (\eid (program, renamings) ->
          liftSoVisbleTo program eid depEId
          |> Maybe.map (\(newProgram, newestRenamings) -> (newProgram, Dict.union newestRenamings renamings))
        )
        (Just (originalProgram, Dict.empty))
  in
  maybeLiftedAndRenamings
  |> Maybe.map
      (\(lifted, renamings) ->
        let _ = Debug.log "renamings" renamings in
        let dependent = turnExpsToVars indep renamings in
        replaceExpNode depEId dependent lifted
      )


-- type alias ExpressionDependency =
--   { exp : Exp
--   , immediatelyDependentOn : Set.Set EId
--   }
--
--
-- type alias DependencyAnalysis = Dict.Dict EId ExpressionDependency

-- sValToV_ (SV v) = v.v_
-- sValToV  (SV v) = v
--
-- sVal v_ = SV { v_ = v_, vtrace = [dummyEId] }
--
-- sVStr = sVal << VBase << VString
--
-- sVUnknown = sVal (VCustom SVUnknown)
--
-- sValToDictKey v_ =
--   Eval.valToDictKey_ v_ sValToV_

-- lookupStaticEnv : StaticEnv -> Ident -> Maybe EId
-- lookupStaticEnv staticEnv ident =
--   Utils.maybeFind ident staticEnv
--   -- case Utils.maybeFind ident staticEnv of
--   --   Just (Just eid) -> Just eid
--   --   _               -> Nothing
--
-- analyzeDependence : Exp -> DependencyAnalysis
-- analyzeDependence program =
--   let (_, dependencyAnalysis, _) =
--     analyzeDependence_ [] Dict.empty program
--   in
--   dependencyAnalysis

-- analyzeDependence_ : StaticEnv -> DependencyAnalysis -> Exp -> (StaticEnv, DependencyAnalysis, StaticVal)
-- analyzeDependence_ env depAn exp =
--   let eid = exp.val.eid in
--   let expDepTemplate =
--     { eid                    = eid
--     , exp                    = exp
--     , staticEnv              = env
--     , staticVal              = sVUnknown
--     , immediatelyDependentOn = Set.empty
--     , dependentOnClosure     = Set.empty
--     }
--   in
--   let depsBasedOn exps =
--     let newDepAn =
--       exps
--       |> List.foldl
--         (\exp depAnAcc ->
--           let (_, newDepAnAcc, _) = analyzeDependence_ env depAnAcc argExp in
--           newDepAnAcc
--         )
--         depAn
--     in
--     let depEIds = Set.fromList <| List.map (.eid << .val) allExps in
--     let depEIdsClosure =
--       List.foldl
--           Set.union
--           depEIds
--           (List.map (\depEId -> (Utils.justGet_ "Brainstorm.analyzeDependence EList" varEId depAn).dependentOnClosure) depEIds)
--     in
--     let expDep =
--       { expDepTemplate | immediatelyDependentOn = depEIds
--                        , dependentOnClosure     = depEIdsClosure }
--     in
--     (newDepAn, expDep)
--   in
--   -- May (or may not) want to change this so each sVal returned has a vtrace of [eid] rather than [dummyEId]
--   let (newEnv, newDependencyAnalysis, retVal) =
--     case exp.val.e__ of
--       EConst _ num loc wd ->
--         let sv = sVal <| VConst (num, TrLoc loc) in
--         (env, Dict.insert eid { expDepTemplate | staticVal = sv } depAn, sv)
--
--       EBase _ v ->
--         let sv = sVal <| VBase (eBaseToVBase v) in
--         (env, Dict.insert eid { expDepTemplate | staticVal = sv } depAn, sv)
--
--       EVar _ x ->
--         let maybeVarEId = lookupStaticEnv env x in
--         case maybeVarEId of
--           Just (Just varEId) ->
--             let errorMsg = "eid in env should exist in dependency analysis.\nvar: " ++ x ++ "\neid: " ++ varEId ++ "\ndependencyAnalysis: " ++ (toString depAn) in
--             let varDep = Utils.justGet_ errorMsg varEId depAn in
--             let sv = varDep.staticVal in
--             let expDep =
--               { expDepTemplate | staticVal              = sv
--                                , immediatelyDependentOn = Set.singleton varEId
--                                , dependentOnClosure     = Set.insert varEId varDep.dependentOnClosure }
--             in
--             (env, Dict.insert eid expDep depAn, sv)
--
--           _ ->
--             (env, Dict.insert eid expDepTemplate depAn, sVUnknown)
--
--       EFun _ pats body _ ->
--         let sv = sVal <| VClosure pat body env in
--         let expDep = { expDepTemplate | staticVal = sv } in
--         let depAn' = Dict.insert eid expDep depAn in
--         let (_, depAn'', _) =
--           let env' =
--             let boundEnv =
--               List.concatMap identifiersListInPat pats
--               |> List.map (\boundIdent -> (boundIdent, Nothing))
--             in
--             boundEnv ++ env
--           in
--           -- add child deps to closure deps (if closure is lifted, vars inside need to stay in scope)
--           analyzeDependence_ env' depAn' body
--         in
--         let bodyEId = body.val.eid in
--         let bodyExpDep = Utils.justGet_ "Brainstorm.analyzeDependence fun" bodyEId depAn'' in
--         let expDep' = { expDep | immediatelyDependentOn = Set.singleton bodyEId
--                                , dependentOnClosure     = Set.insert bodyEId bodyExpDep.dependentOnClosure }
--         in
--         (env, Dict.insert eid expDep' depAn'', sv)
--
--       EOp _ op exps _ ->
--         let (newDepAn, argSVals) =
--           exps
--           |> List.foldl
--             (\argExp (depAnAcc, sVals) ->
--               let (_, newDepAnAcc, argSVal) = analyzeDependence_ env depAnAcc argExp in
--               (newDepAnAcc, sVals ++ [argSVal])
--             )
--             (depAn, [])
--         in
--         let args = List.map sValToV_ argSVals in
--         let depEIds = Set.fromList <| List.map (.eid << .val) exps in
--         let depEIdsClosure =
--           List.foldl
--               Set.union
--               depEIds
--               (List.map (\depEId -> (Utils.justGet_ "Brainstorm.analyzeDependence op" varEId depAn).dependentOnClosure) depEIds)
--         in
--         let expDepTemplate' =
--           { expDepTemplate | immediatelyDependentOn = depEIds
--                            , dependentOnClosure     = depEIdsClosure }
--         in
--         let returnSVal sv =
--           (env, Dict.insert eid { expDepTemplate' | staticVal = sv } newDepAn, sv)
--         in
--         let return val_ = returnSVal (sVal val_) in
--         let returnUnknown =
--           (env, Dict.insert eid expDepTemplate' newDepAn, sVUnknown)
--         in
--         let nullaryOp args retVal =
--           case args of
--             [] -> return retVal
--             _  -> returnUnknown
--         in
--         let unaryMathOp op args =
--           case args of
--             [VConst (n,t)] ->
--               return <| VConst (Eval.evalDelta bt op [n], TrOp op [t])
--             _ -> returnUnknown
--         in
--         let binMathOp op args =
--           case args of
--             [VConst (i,it), VConst (j,jt)] ->
--               return <| VConst (Eval.evalDelta [] op [i,j], TrOp op [it,jt])
--
--             _ -> returnUnknown
--         in
--         case op of
--           Plus ->
--             case args of
--               [VBase (VString s1), VBase (VString s2)] ->
--                 return <| VBase (VString (s1 ++ s2))
--
--               _ ->
--                 binMathOp op args
--
--           Minus     -> binMathOp op args
--           Mult      -> binMathOp op args
--           Div       -> binMathOp op args
--           Mod       -> binMathOp op args
--           Pow       -> binMathOp op args
--           ArcTan2   -> binMathOp op args
--           Lt        -> case args of
--             [VConst (i,it), VConst (j,jt)] -> return <| VBase (VBool (i < j))
--             _                              -> returnUnknown
--           Eq        -> case args of
--             [VConst (i,it), VConst (j,jt)]           -> return <| VBase (VBool (i == j))
--             [VBase (VString s1), VBase (VString s2)] -> return <| VBase (VBool (s1 == s2))
--             [VCustom SVUnknown, _]                   -> returnUnknown
--             [_, VCustom SVUnknown]                   -> returnUnknown
--             [_, _]                                   -> return <| VBase (VBool False) -- polymorphic inequality, added for Prelude.addExtras
--             _                                        -> returnUnknown
--           Pi         -> nullaryOp args (VConst (pi, TrOp op []))
--           DictEmpty  -> nullaryOp args (VDict Dict.empty)
--           DictInsert -> case argSVals of
--             [vkey, sval, dictVal] -> case (sValToV_ vkey, sValToV_ dictVal) of
--               (VCustom SVUnknown, _) -> returnUnknown -- inserting with unknown key could overwrite any existing key
--               (keyV_, VDict d) ->
--                   case sValToDictKey keyV_ of
--                     Err _   -> returnUnknown
--                     Ok dkey -> return <| VDict (Dict.insert dkey sval d)
--               _ -> returnUnknown
--             _  -> returnUnknown
--           DictGet    -> case args of
--             [keyV_, VDict d] ->
--               case sValToDictKey keyV_ of
--                 Err _   -> returnUnknown
--                 Ok dkey -> returnSVal <| Utils.getWithDefault dkey (sVal <| VBase VNull) d
--             _ -> returnUnknown
--           DictRemove -> case args of
--             [keyV_, VDict d] ->
--               case sValToDictKey keyV_ of
--                 Err _   -> returnUnknown
--                 Ok dkey -> return <| VDict (Dict.remove dkey d)
--             _ -> returnUnknown
--           Cos     -> unaryMathOp op args
--           Sin     -> unaryMathOp op args
--           ArcCos  -> unaryMathOp op args
--           ArcSin  -> unaryMathOp op args
--           Floor   -> unaryMathOp op args
--           Ceil    -> unaryMathOp op args
--           Round   -> unaryMathOp op args
--           Sqrt    -> unaryMathOp op args
--           Explode -> case args of
--             [VBase (VString s)] -> return <| VList (List.map (sVStr << String.fromChar) (String.toList s))
--             _                   -> returnUnknown
--           DebugLog -> case argSVals of
--             [sval] -> returnSVal sval
--             _      -> returnUnknown
--           ToStr -> case argSVals of
--             [sval] ->
--               if sValToV_ sval == VCustom SVUnknown
--               then returnUnknown
--               else return <| VBase (VString (strVal__ False sValToV sval))
--             _ -> returnUnknown
--           RangeOffset _ -> returnUnknown
--
--       --
--       -- Below here does not yet compute staticVal
--       --
--       EList _ _ _ _ _ ->
--         let (newDepAn, expDep) = depsBasedOn (childExps exp) in
--         (env, Dict.insert eid expDep newDepAn, sVUnknown)
--
--       EIndList _ rs _ ->
--         let (newDepAn, expDep) = depsBasedOn (childExps exp) in
--         (env, Dict.insert eid expDep newDepAn, sVUnknown)
--
--       EIf _ e1 e2 e3 _ ->
--         let (newDepAn, expDep) = depsBasedOn (childExps exp) in
--         (env, Dict.insert eid expDep newDepAn, sVUnknown)
--
--       ECase _ e1 bs _ ->
--         let expDep = { expDepTemplate | immediatelyDependentOn = Set.singleton bodyEId
--                                       , dependentOnClosure     = Set.insert bodyEId bodyExpDep.dependentOnClosure }
--         case eval_ env (e::bt) e1 of
--           Err s -> Err s
--           Ok (v1,ws1) ->
--             case evalBranches env (e::bt) v1 bs of
--               Ok (Just (v2,ws2)) -> Ok <| retBoth (v2, ws1 ++ ws2)
--               Err s              -> Err s
--               _                  -> errorWithBacktrace (e::bt) <| strPos e1.start ++ " non-exhaustive case statement"
--
--       ETypeCase _ pat tbranches _ ->
--         case evalTBranches env (e::bt) pat tbranches of
--           Ok (Just (v,ws)) -> Ok <| retBoth (v, ws)
--           Err s            -> Err s
--           _                -> errorWithBacktrace (e::bt) <| strPos pat.start ++ " non-exhaustive typecase statement"
--
--       EApp _ e1 [e2] _ ->
--         -- Return env of the call site
--         Result.map (replaceEnv env) <| evalSimpleApp env bt bt' e1 e2
--
--       ELet _ _ False p e1 e2 _ ->
--         -- Return env that the let body returns (so that programs return their final top-level environment)
--         Result.map (retAddWs e2.val.eid) <| evalSimpleApp env bt bt' (eFun [p] e2) e1
--
--       ELet _ _ True p e1 e2 _ ->
--         case eval_ env bt' e1 of
--           Err s       -> Err s
--           Ok (v1,ws1) ->
--             case (p.val, v1.v_) of
--               (PVar _ f _, VClosure Nothing x body env') ->
--                 let _   = Utils.assert "eval letrec" (env == env') in
--                 let v1' = Val (VClosure (Just f) x body env) v1.vtrace in
--                 case (pVar f, v1') `cons` Just env of
--                   Just env' -> Result.map (addWidgets ws1) <| eval env' bt' e2
--                   _         -> errorWithBacktrace (e::bt) <| strPos e.start ++ "bad ELet"
--               (PList _ _ _ _ _, _) ->
--                 errorWithBacktrace (e::bt) <|
--                   strPos e1.start ++
--                   "mutually recursive functions (i.e. letrec [...] [...] e) \
--                    not yet implemented"
--                    -- Implementation also requires modifications to LangTransform.simply
--                    -- so that clean up doesn't prune the funtions.
--               _ ->
--                 errorWithBacktrace (e::bt) <| strPos e.start ++ "bad ELet"
--
--       EComment _ _ e1       -> eval env bt e1
--       EOption _ _ _ _ e1    -> eval env bt e1
--       ETyp _ _ _ e1 _       -> eval env bt e1
--       EColonType _ e1 _ _ _ -> eval env bt e1
--       ETypeAlias _ _ _ e1 _ -> eval env bt e1
--
--       -- abstract syntactic sugar
--
--       EFun _ ps e1 _           -> Result.map (retAddWs e1.val.eid) <| eval env bt' (eFun ps e1)
--       EApp _ e1 [] _           -> errorWithBacktrace (e::bt) <| strPos e1.start ++ " application with no arguments"
--       EApp _ e1 es _           -> Result.map (retAddWs e.val.eid)  <| eval env bt' (eApp e1 es)


maybeMakeEqualConstraint programEnv originalProgram constraint =
  let _ = Debug.log "statically evaluating" () in
  let (_, _, programAnalysis) = staticEvalWithPrelude originalProgram in
  let _ = Debug.log "finding solutions" () in
  let solutions =
    solutionsForDependentProgramLocation originalProgram constraint
    |> List.filter (isValidSolution programAnalysis)
  in
  let _ = Debug.log "solutions" solutions in
  solutions
  |> Utils.mapFirstSuccess
      (\(indep, depEId) ->
        let _ = Debug.log "trying solution" (indep, depEId) in
        redefineExp programEnv originalProgram indep depEId
      )


relate originalProgram idea zoneToRelate slideNumber movieNumber movieTime =
  let (shapeId, zoneName) = zoneToRelate in
  let _ =
    let (_, _, progAn) = staticEvalWithPrelude originalProgram in
    Debug.log (progAn |> Dict.toList |> List.map (\(eid, expAn) -> (toString eid) ++ " " ++ (unparse expAn.exp |> String.trimLeft |> String.left 33)) |> String.join "\n") ()
  in
  case Eval.eval Eval.initEnv [] originalProgram of
    Err s -> let _ = Debug.log ("Brainstorm.relate eval failed: " ++ s) () in originalProgram
    Ok ((val, _), programEnv) ->
      let movieFrameValRes =
        LangSvg.fetchEverything_ slideNumber movieNumber movieTime val
        |> Result.map (\(_, _, _, _, movieFrameVal) -> movieFrameVal)
      in
      case movieFrameValRes of
        Err s -> let _ = Debug.log ("Brainstorm.relate fetchEverything_ failed: " ++ s) () in originalProgram
        Ok outputVal ->
          case maybeFindShape shapeId outputVal of
            Nothing -> originalProgram
            Just shapeVal ->
              let maybeZoneIdea = shapeZoneToIdea programEnv shapeVal zoneName in
              case maybeZoneIdea of
                Nothing       -> originalProgram
                Just zoneIdea ->
                  case ideaToMaybeConstraint (zoneNameToConstraintType zoneName) idea zoneIdea of
                    Nothing         -> originalProgram
                    Just constraint ->
                      case maybeMakeEqualConstraint programEnv originalProgram constraint of
                        Nothing         -> originalProgram
                        Just newProgram -> newProgram
