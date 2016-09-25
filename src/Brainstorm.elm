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


------------------------------------------------------
-- EId/Exp Functions                                --
------------------------------------------------------

-- Expressions inserted into constraints get a special dummy EId.
-- In the case of EVar, this EId says "do variable lookup at the top level".
insertedEId = -9124923

expWithEId eid e__ =
  { e__ = e__, eid = eid } |> withDummyRange


independentDummyEId = -8273412
independentDummyExp = expWithEId independentDummyEId (EVar "" "__insert_independent_expression_here__")


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
  flattenExpTree exp |> List.map (.val >> .eid) |> List.filter isActualEId


------------------------------------------------------
-- Idea Discovery                                   --
------------------------------------------------------

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
  let pointVals = List.map (Utils.fst3 >> LangSvg.pointToVal) ideas in
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


maybeFindShape : Int -> Val -> Maybe Val
maybeFindShape shapeId outputVal =
  maybeIndexShapes outputVal `Maybe.andThen` (Dict.get shapeId)


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


firstIdeaSource idea =
  case idea of
    (_, _, ideaSource::_) -> ideaSource
    _                     -> Debug.crash "Brainstorm.firstIdeaSource: given idea without a source!"


valToExp : Set.Set EId -> Val -> Exp
valToExp staticEIds val =
  let eid =
    case val.vtrace |> List.filter isProgramEId |> List.filter (\eid -> Set.member eid staticEIds) of
      []     -> dummyEId
      eid::_ -> eid
  in
  case val.v_ of
    VConst (n, tr)     -> expWithEId eid <| EConst " " n dummyLoc noWidgetDecl
    VBase (VBool bool) -> expWithEId eid <| EBase " " (EBool bool)
    VBase (VString s)  -> expWithEId eid <| EBase " " (EString defaultQuoteChar s)
    VBase VNull        -> expWithEId eid <| EBase " " ENull
    VList vals         -> expWithEId eid <| EList " " (List.map (valToExp staticEIds) vals) "" Nothing ""
    VDict vDict        -> expWithEId eid <| EDict (vDict |> Dict.map (\_ v -> valToExp staticEIds v))
    _                  -> Debug.crash <| "Unexpected val in valToExp" ++ (strVal val)


-- Still only using the first ideaSource
ideaToConstraintTerm : Set.Set EId -> Idea -> Exp
ideaToConstraintTerm staticEIds idea =
  case firstIdeaSource idea of
    PrimitiveFeature shapeVal functionName ->
      let _ = Debug.log "shape val" shapeVal in
      let _ = Debug.log "shape val as exp" (valToExp staticEIds shapeVal) in
      insertedApp (insertedVar0 functionName) [valToExp staticEIds shapeVal]

    BasedOnTwoPoints idea1 idea2 functionName ->
      insertedApp
          (insertedVar0 functionName)
          [ ideaToConstraintTerm staticEIds idea1
          , ideaToConstraintTerm staticEIds idea2
          ]


-- idea2 is the dependent idea
ideasToConstraints : Exp -> ConstraintType -> Idea -> Idea -> List Constraint
ideasToConstraints program constraintType indepIdea depIdea =
  let extraFunctions =
    case constraintType of
      XConstraint     -> ["x"]
      YConstraint     -> ["y"]
      PointConstraint -> []
  in
  let staticEIds =
    staticAnalyzeWithPrelude program
    |> Dict.toList
    |> List.filter (\(eid, expAn) -> expAn.isStatic)
    |> List.map fst
    |> Set.fromList
  in
  let lhsIdeaTerm = ideaToConstraintTerm staticEIds indepIdea in
  let rhsIdeaTerm = ideaToConstraintTerm staticEIds depIdea in
  let wrapTerm funcName arg = insertedApp (insertedVar0 funcName) [arg] in
  let lhs = List.foldr wrapTerm lhsIdeaTerm extraFunctions in
  let rhs = List.foldr wrapTerm rhsIdeaTerm extraFunctions in
  [ { independent = lhs, dependent = rhs } ]


ideaToMaybeConstraint : Exp -> ConstraintType -> Idea -> Idea -> Maybe Constraint
ideaToMaybeConstraint program constraintType indepIdea depIdea =
  case ideasToConstraints program constraintType indepIdea depIdea of
    []            -> Nothing
    constraint::_ -> Just constraint


------------------------------------------------------
-- Relate Computation Helpers                       --
------------------------------------------------------

justGetExpressionAnalysis programAnalysis eid =
  Utils.justGet_ ("Brainstorm.justGetExpressionAnalysis: could not find eid " ++ (toString eid)) eid programAnalysis


eidToExp programAnalysis eid =
  (justGetExpressionAnalysis programAnalysis eid).exp


-- TODO: change StaticEnv/neededEId into set of EIds instead of single (multiple exps may resolve the same)
visibleVarsAt : ProgramAnalysis -> EId -> StaticEnv
visibleVarsAt programAnalysis observerEId =
  let errorMsg = "Brainstorm.visibleVarsAt: can't find eid " ++ (toString observerEId) ++ " in program analysis" in
  let observerExpAn = Utils.justGet_ errorMsg observerEId programAnalysis in
  observerExpAn.staticEnv


eidsAvailableAsVarsAt : ProgramAnalysis -> EId -> Dict.Dict EId Ident
eidsAvailableAsVarsAt programAnalysis observerEId =
  let errorMsg = "Brainstorm.eidsAvailableAsVarsAt: can't find eid " ++ (toString observerEId) ++ " in program analysis" in
  let observerExpAn = Utils.justGet_ errorMsg observerEId programAnalysis in
  let (resultDict, _) =
    observerExpAn.staticEnv
    |> List.foldl
        (\(ident, staticVarRef) (dict, seenVars) ->
          -- Make sure var isn't shadowed by something of higher precedence.
          if not <| Set.member ident seenVars then
            case staticVarRef of
              Known eid ->
                let dict' =
                  (justGetExpressionAnalysis programAnalysis eid).equivalentEIds
                  |> Set.foldl (\equivEId d -> Dict.insert equivEId ident d) dict
                in
                (dict', Set.insert ident seenVars)
              _ ->
                (dict, Set.insert ident seenVars)
          else
            (dict, seenVars)
        )
        (Dict.empty, Set.empty)
  in
  resultDict


findVarAtEId : ProgramAnalysisWithTLEId -> Exp -> Maybe Exp
findVarAtEId programAnalysisTLE varExp =
  let varEId = varExp.val.eid in
  if isActualEId varEId || isInsertedEId varEId then
    let searchScopeEId =
      if isInsertedEId varEId then
        -- If this is a var inserted into the constraint, resolve at the top level.
        let errorMsg = "Brainstorm.findVarAtEId: can't handle empty program" in
        programAnalysisTLE.lastTopLevelEId
      else
        varEId
    in
    let visibleVars = visibleVarsAt programAnalysisTLE.programAnalysis searchScopeEId in
    case varExp.val.e__ of
      EVar _ ident ->
        case Utils.maybeFind ident visibleVars of
          Just (Known eid) -> Just <| eidToExp programAnalysisTLE.programAnalysis eid
          _                ->
            let _ = Debug.log ("findVarAtEId: " ++ ident ++ " not in env at " ++ (toString searchScopeEId)) (List.map fst visibleVars) in
            Nothing -- not found in static env, not resolvable to an exp, or bound

      _ ->
        let _ = Debug.log ("findVarAtEId: not given an EVar: " ++ (unparse varExp)) () in
        Nothing
  else
    let _ = Debug.log "findVarAtEId: not actual eid" varEId in
    Nothing


------------------------------------------------------
-- Symbolic Evaluation                              --
------------------------------------------------------

-- Don't evaluate list heads when the root expression is a list.
maybeSymbolicallyEvaluateOneStepForConstraintEnumeration : ProgramAnalysisWithTLEId -> Exp -> Maybe Exp
maybeSymbolicallyEvaluateOneStepForConstraintEnumeration programAnalysisTLE exp =
  case exp.val.e__ of
    EList ws1 heads ws2 maybeTail ws3 ->
      let tailMaybeEvaluated =
        case Maybe.map (.e__ << .val) maybeTail of
          Just (EList _ tailHeads _ tailMaybeTail _) ->
            -- Graft tail into list.
            Just <| replaceE__ exp (EList ws1 (heads ++ tailHeads) ws2 tailMaybeTail ws3)

          Just _ ->
            let tailExp = Utils.fromJust maybeTail in
            maybeSymbolicallyEvaluateOneStep programAnalysisTLE tailExp
            |> Maybe.map (\newTail -> replaceE__ exp (EList ws1 heads ws2 (Just newTail) ws3))

          _ ->
            Nothing
      in
      tailMaybeEvaluated

    _ ->
      maybeSymbolicallyEvaluateOneStep programAnalysisTLE exp


-- Goal here is to evaluate just enough so that e.g. patterns can match.
--
-- Returns Nothing if no progress.
maybeSymbolicallyEvaluateOneStep : ProgramAnalysisWithTLEId -> Exp -> Maybe Exp
maybeSymbolicallyEvaluateOneStep programAnalysisTLE exp =
  -- Return Just newExps if progress made on one expression.
  -- Return Nothing if no progress made on any expressions.
  let evalOneOf exps =
    case exps of
      []    -> Nothing
      e::es ->
        case maybeSymbolicallyEvaluateOneStep programAnalysisTLE e of
          Just newE -> Just (newE::es)
          Nothing   -> evalOneOf es |> Maybe.map (\rest -> e::rest)
  in
  case exp.val.e__ of
    EVal val           -> Nothing
    EDict _            -> Nothing -- TODO: How should we handle EDict's here?
    EConst _ n loc wd  -> Nothing
    EBase _ bVal       -> Nothing
    EVar _ ident       -> findVarAtEId programAnalysisTLE exp
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
          let args          = evaledArgExps |> List.map (.val >> .e__) in
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
      let tailMaybeEvaluated =
        case Maybe.map (.e__ << .val) maybeTail of
          Just (EList _ tailHeads _ tailMaybeTail _) ->
            -- Graft tail into list.
            Just <| replaceE__ exp (EList ws1 (heads ++ tailHeads) ws2 tailMaybeTail ws3)

          Just _ ->
            let tailExp = Utils.fromJust maybeTail in
            maybeSymbolicallyEvaluateOneStep programAnalysisTLE tailExp
            |> Maybe.map (\newTail -> replaceE__ exp (EList ws1 heads ws2 (Just newTail) ws3))

          _ ->
            Nothing
      in
      case tailMaybeEvaluated of
        Just newExp -> Just newExp
        Nothing ->
          case evalOneOf heads of
            Just newHeads -> Just <| replaceE__ exp (EList ws1 newHeads ws2 maybeTail ws3)
            Nothing       -> Nothing

    EIndList _ ranges _ ->
      Nothing

    EIf ws1 predicate trueBranch falseBranch ws2 ->
      case maybeSymbolicallyEvaluateOneStep programAnalysisTLE predicate of
        Just newPredicate -> Just <| replaceE__ exp (EIf ws1 newPredicate trueBranch falseBranch ws2)
        Nothing ->
          case predicate.val.e__ of
            EBase _ (EBool True)  -> Just trueBranch
            EBase _ (EBool False) -> Just falseBranch
            _                     -> Nothing

    ECase ws1 scrutinee branches ws2 ->
      case maybeSymbolicMatchBranches programAnalysisTLE scrutinee branches of
        Just (branchEnv, branchExp) ->
          Just <| symbolicSubstitute (Dict.fromList branchEnv) branchExp

        Nothing ->
          maybeSymbolicallyEvaluateOneStep programAnalysisTLE scrutinee
          |> Maybe.map (\newScrutinee -> replaceE__ exp (ECase ws1 scrutinee branches ws2))

    ETypeCase ws1 scrutinee tbranches ws2 ->
      case tryExpMatchTBranches scrutinee tbranches of
        CannotCompare ->
          maybeSymbolicallyEvaluateOneStep programAnalysisTLE scrutinee
          |> Maybe.map (\newScrutinee -> replaceE__ exp (ETypeCase ws1 newScrutinee tbranches ws2))

        Match branchExp ->
          Just branchExp

        NoMatch ->
          Nothing

    EApp _ funcExp argExps _ ->
      maybeSymbolicallyApply programAnalysisTLE exp -- This may skip a few steps. It's okay.

    ELet _ _ True pat assigns body _ ->
      Nothing -- can't handle recursive substitution

    ELet ws1 kind False pat assigns body ws2 ->
      case maybeSymbolicMatch programAnalysisTLE pat assigns of
        Just env -> Just <| symbolicSubstitute (Dict.fromList env) body
        Nothing  ->
          maybeSymbolicallyEvaluateOneStep programAnalysisTLE assigns
          |> Maybe.map (\newAssigns -> replaceE__ exp (ELet ws1 kind False pat newAssigns body ws2))
          -- Might also try evaluating the let body...but that's almost certainly pointless.

    EComment _ _ body       -> Just body
    EOption _ _ _ _ body    -> Just body
    ETyp _ _ _ body _       -> Just body
    EColonType _ body _ _ _ -> Just body
    ETypeAlias _ _ _ body _ -> Just body


-- Only expands child expressions if necessary
-- e.g. an if-statement will evaluate the predicate and possibly a branch
--      but a list literal will not try to expand each list element
symbolicallyEvaluateAsFarAsPossible : ProgramAnalysisWithTLEId -> Exp -> Exp
symbolicallyEvaluateAsFarAsPossible programAnalysisTLE exp =
  let recurse e = symbolicallyEvaluateAsFarAsPossible programAnalysisTLE e in
  case exp.val.e__ of
    EVal _             -> exp -- TODO: How should we handle EVal's here?
    EDict _            -> exp -- TODO: How should we handle EDict's here?
    EConst _ n loc wd  -> exp
    EBase _ bVal       -> exp
    EVar _ ident       ->
      case findVarAtEId programAnalysisTLE exp of
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
      let args          = evaledArgExps |> List.map (.val >> .e__) in
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
      case maybeSymbolicMatchBranches programAnalysisTLE scrutinee branches of
        Nothing ->
          exp

        Just (branchEnv, branchExp) ->
          symbolicSubstitute (Dict.fromList branchEnv) branchExp
          |> recurse

    ETypeCase _ scrutinee tbranches _ -> exp -- TODO: symbolic expansion of typecase
    EApp _ funcExp argExps _          -> Maybe.withDefault exp (maybeSymbolicallyApply programAnalysisTLE exp)
    ELet _ _ True pat assigns body _  -> exp -- can't handle recursive substitution
    ELet _ _ False pat assigns body _ ->
      case maybeSymbolicMatch programAnalysisTLE pat assigns of
        Just env -> recurse (symbolicSubstitute (Dict.fromList env) body)
        Nothing  -> exp

    EComment _ _ body       -> recurse body
    EOption _ _ _ _ body    -> recurse body
    ETyp _ _ _ body _       -> recurse body
    EColonType _ body _ _ _ -> recurse body
    ETypeAlias _ _ _ body _ -> recurse body


-- Preserves EIds even when children are substituted...I think that's what we want...
symbolicSubstitute : Dict.Dict Ident Exp -> Exp -> Exp
symbolicSubstitute subst exp =
  let fnSubst =
    subst
    |> Dict.map (\_ e -> always e)
  in
  transformVarsUntilBound fnSubst exp


maybeSymbolicallyApply : ProgramAnalysisWithTLEId -> Exp -> Maybe Exp
maybeSymbolicallyApply programAnalysisTLE appExp =
  -- let _ = Debug.log ("Attempting to apply " ++ (unparse appExp)) () in
  case appExp.val.e__ of
    EApp _ funcExp argExps _ ->
      let maybeFuncToApply =
        let evaledFuncExp = symbolicallyEvaluateAsFarAsPossible programAnalysisTLE funcExp in
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
                if LangParser2.isPreludeEId funcExp.val.eid || (isActualEId funcExp.val.eid && (eidsAvailableAsVarsAt programAnalysisTLE.programAnalysis funcExp.val.eid |> Dict.member funcExp.val.eid |> not)) then
                  let patsAndArgExps = Utils.zip pats argExps in
                  if List.length argExps <= List.length pats then
                    -- Full or partial application
                    let maybeSubst =
                      patsAndArgExps
                      |> Utils.foldlMaybe
                          (\(pat, argExp) subst ->
                            case maybeSymbolicMatch programAnalysisTLE pat argExp of
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
                    maybeResult
                    -- case maybeResult of
                    --   Just exp -> let _ = Debug.log ("symbolic result: " ++ unparse exp) () in maybeResult
                    --   Nothing  -> let _ = Debug.log "application failed" () in maybeResult
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
maybeSymbolicMatchBranches : ProgramAnalysisWithTLEId -> Exp -> List Branch -> Maybe (List (Ident, Exp), Exp)
maybeSymbolicMatchBranches programAnalysisTLE scrutinee branches =
  maybeExpMatchBranches_ (trySymbolicMatch programAnalysisTLE) scrutinee branches


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


maybeSymbolicMatch : ProgramAnalysisWithTLEId -> Pat -> Exp -> Maybe (List (Ident, Exp))
maybeSymbolicMatch programAnalysisTLE pat exp =
  trySymbolicMatch programAnalysisTLE pat exp |> matchToMaybe


maybeMatchExp : Pat -> Exp -> Maybe (List (Ident, Exp))
maybeMatchExp pat exp =
  tryMatchExp pat exp |> matchToMaybe


-- The pat may not match if the exp hasn't been evaluated far enough.
-- Try to evaluate just far enough to match.
trySymbolicMatch : ProgramAnalysisWithTLEId -> Pat -> Exp -> SymbolicMatchResult
trySymbolicMatch programAnalysisTLE pat exp =
  let result = tryMatchExp_ (trySymbolicMatch programAnalysisTLE) pat exp in
  let recurse () =
    let _ = Debug.log ((unparse exp) ++ " didn't match " ++ (unparsePat pat) ++ ", attempting one step of evaluation") in
    case maybeSymbolicallyEvaluateOneStep programAnalysisTLE exp of
      Nothing            -> result
      Just oneStepEvaled -> trySymbolicMatch programAnalysisTLE pat oneStepEvaled
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


-- -- May need to support dictionaries in static analysis
-- expToDictKey : Exp -> Maybe (String, String)
-- expToDictKey exp =
--   case exp.val.e__ of
--     EConst _ n _ _            -> Just (toString n, "num")
--     EBase _ (EBool b)         -> Just (toString b, "bool")
--     EBase _ (EString _ s)     -> Just (toString s, "string")
--     EBase _ ENull             -> Just ("", "null")
--     EList _ heads _ Nothing _ ->
--       heads
--       |> List.map expToDictKey
--       |> Utils.projJusts
--       |> Maybe.map (\keyStrings -> (toString keyStrings, "list"))
--     _                         -> Nothing


------------------------------------------------------
-- Staic Analysis                                   --
------------------------------------------------------

type alias ExpressionAnalysis =
  { exp                    : Exp
  , staticEnv              : StaticEnv
  , isStatic               : Bool
  , equivalentEIds         : Set.Set EId -- Which other expressions are guaranteed to resolve to this value?
  , immediatelyDependentOn : Set.Set EId -- If these other expressions change, the value at this expression could change.
  }

type alias StaticEnv = List (Ident, StaticVarRef)

type StaticVarRef
  = Known EId     -- Var references a known EId. Var is static if the referenced expression is static.
  | StaticUnknown -- Var references a static value, but can't say exactly what. (E.g. var came from the deconstruction of a static value, but we're too imprecise to work out the deconstruction exactly)
  | Bound         -- Var references a bound argument, not static.

type alias ProgramAnalysis          = Dict.Dict EId ExpressionAnalysis
type alias ProgramAnalysisWithTLEId = { programAnalysis : ProgramAnalysis, lastTopLevelEId : EId }


(_, preludeStaticEnv, preludeProgramAnalysis) =
  staticAnalyze_ [] Dict.empty LangParser2.prelude


staticAnalyzeWithPrelude : Exp -> ProgramAnalysis
staticAnalyzeWithPrelude program =
  let _ = Debug.log "analyzing program" () in
  let (_, _, programAnalysis) =
    staticAnalyze_ preludeStaticEnv preludeProgramAnalysis program
  in
  let _ = Debug.log "gathering equivalent EIds" () in
  let programAnalysis' = gatherEquivalentEIds programAnalysis in
  programAnalysis'


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
  -- let _ = Debug.log "Equivalent sets" (List.map Set.toList equivSets) in
  programAnalysis'


-- Does not compute dependence closure or full equivalent EId sets.
staticAnalyze_ : StaticEnv -> ProgramAnalysis -> Exp -> (Bool, StaticEnv, ProgramAnalysis)
staticAnalyze_ env programAnalysis exp =
  let eidIsStatic progAn eid =
    (justGetExpressionAnalysis progAn eid).isStatic
  in
  let replaceExp newE__ = replaceE__ exp newE__ in
  let thisEId = exp.val.eid in
  let programAnalysis' =
    let expressionAnalysis =
      { exp                    = exp
      , staticEnv              = env
      , isStatic               = True
      , equivalentEIds         = Set.singleton thisEId
      , immediatelyDependentOn = childExps exp |> List.map (.val >> .eid) |> Set.fromList
      }
    in
    Dict.insert thisEId expressionAnalysis programAnalysis
  in
  let addDependencies progAn dependentEId dependencyEIds =
    let dependencyEIds' = Set.filter isActualEId dependencyEIds in
    let expAn = justGetExpressionAnalysis progAn dependentEId in
    let expAn' = { expAn | immediatelyDependentOn = Set.union dependencyEIds' expAn.immediatelyDependentOn } in
    Dict.insert dependentEId expAn' progAn
  in
  let makeSubtreesDependent progAn subtrees dependencyEIds =
    subtrees
    |> List.concatMap allActualEIds
    |> List.foldl
        (\descendantEId progAn ->
          addDependencies progAn descendantEId dependencyEIds
        )
        progAn
  in
  let addDependency progAn eid =
    addDependencies progAn thisEId (Set.singleton eid)
  in
  let addEquivalentEId msg progAn eid1 eid2 =
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
  in
  let retEnvDict isStatic retEnv retProgramAnalysis =
    if isStatic then
      (True, retEnv, retProgramAnalysis)
    else
      let thisExpAn = justGetExpressionAnalysis retProgramAnalysis thisEId in
      let retProgramAnalysis' = Dict.insert thisEId { thisExpAn | isStatic = False } retProgramAnalysis in
      (False, retEnv, retProgramAnalysis')
  in
  let retDict isStatic retProgramAnalysis = retEnvDict isStatic env retProgramAnalysis in
  let ret isStatic                        = retDict isStatic programAnalysis' in
  let retRecurseEquivalentExpression msg env progAn exp =
    let (isStatic, resultEnv, progAn') = staticAnalyze_ env progAn exp in
    let progAn'' = addEquivalentEId msg progAn' thisEId exp.val.eid in
    retEnvDict isStatic resultEnv progAn''
  in
  let recurseAll env programAnalysis exps =
    let (allStatic, programAnalysis') =
      exps
      |> List.foldl
          (\e (allStatic, progAn) ->
            let (expStatic, _, progAn') = staticAnalyze_ env progAn e in
            (expStatic && allStatic, progAn')
          )
          (True, programAnalysis)
    in
    (allStatic, programAnalysis')
  in
  case exp.val.e__ of
    EVal _                 -> Debug.crash "Brainstorm.staticAnalyze_: figure out whether to handle EVal"
    EDict _                -> Debug.crash "Brainstorm.staticAnalyze_: figure out whether to handle EDict"
    EConst _ n loc wd      -> ret True
    EBase _ bVal           -> ret True
    EVar _ ident           ->
      case Utils.maybeFind ident env of
        Just (Known eid) ->
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
            addEquivalentEId ("EVar " ++ ident) (addDependency programAnalysis' eid) thisEId eid
          in
          retDict (eidIsStatic programAnalysis'' eid) programAnalysis''

        Just StaticUnknown ->
          -- Variable exists and is static but can't be resolved.
          ret True

        Just Bound ->
          -- Variable exists but is bound. Non-static.
          ret False

        Nothing ->
          -- Variable not found. Normal Eval.run will produce a runtime error. Presume non-static, I guess.
          let _ = Debug.log ("Warning: variable " ++ ident ++ " not found on line " ++ (toString exp.start.line)) () in
          ret False

    EFun _ pats body _     ->
      -- Log static envs of func body.
      let (_, _, programAnalysis'') =
        let patsEnv =
          identifiersSetInPats pats
          |> Set.toList
          |> List.map (\ident -> (ident, Bound))
        in
        let bodyEnv = patsEnv ++ env in
        staticAnalyze_ bodyEnv programAnalysis' body
      in
      -- Function is static if all free variables in its body are static.
      let isStatic =
        (freeIdentifiers exp)
        |> Set.toList
        |> List.all
            (\ident ->
              case Utils.maybeFind ident env of
                Just (Known eid)   -> eidIsStatic programAnalysis'' eid
                Just StaticUnknown -> True
                Just Bound         -> False
                Nothing            -> False -- Var not found; will be a runtime error on Eval.run.
            )
      in
      retDict isStatic programAnalysis''

    EOp ws1 op argExps ws2 ->
      let ret' () =
        let (allArgsStatic, programAnalysis'') =
          recurseAll env programAnalysis' argExps
        in
        retDict allArgsStatic programAnalysis''
      in
      case op.val of
        DebugLog ->
          case argExps of
            [childExp] -> retRecurseEquivalentExpression "DebugLog" env programAnalysis' childExp
            _          -> ret' ()
        _  -> ret' ()

    EList ws1 heads ws2 maybeRest ws3 ->
      let (allChildrenStatic, programAnalysis'') = recurseAll env programAnalysis' (childExps exp) in
      retDict allChildrenStatic programAnalysis''

    EIndList ws1 ranges ws2 ->
      let (allChildrenStatic, programAnalysis'') = recurseAll env programAnalysis' (childExps exp) in
      retDict allChildrenStatic programAnalysis''

    EIf ws1 predicate trueBranch falseBranch ws2 ->
      let (allPartsStatic, programAnalysis'') = recurseAll env programAnalysis' [predicate, trueBranch, falseBranch] in
      -- Conservatively make the branch descendants dependent on the predicate.
      let programAnalysis''' =
        makeSubtreesDependent programAnalysis'' [trueBranch, falseBranch] (Set.singleton predicate.val.eid)
      in
      retDict allPartsStatic programAnalysis'''

    ECase ws1 scrutinee branches ws2 ->
      let (isScrutineeStatic, _, programAnalysis'') = staticAnalyze_ env programAnalysis' scrutinee in
      let (allBranchesStatic, programAnalysis''') =
        branches
        |> List.map .val
        |> List.foldl
            (\(Branch_ _ bPat bExp _) (allBranchesStatic, progAn) ->
              let patEnv =
                identifiersListInPat bPat
                |> List.map (\ident -> if isScrutineeStatic then (ident, StaticUnknown) else (ident, Bound))
              in
              let branchEnv = patEnv ++ env in
              let (isBranchStatic, _, progAn') = staticAnalyze_ branchEnv progAn bExp in
              (isBranchStatic && allBranchesStatic, progAn')
            )
            (True, programAnalysis'')
      in
      -- Conservatively make all branch expression descendants dependent on the scrutinee.
      let programAnalysis'''' =
        makeSubtreesDependent programAnalysis''' (branchExps branches) (Set.singleton scrutinee.val.eid)
      in
      retDict (isScrutineeStatic && allBranchesStatic) programAnalysis''''

    ETypeCase _ scrutinee tbranches _ ->
      let (allPartsStatic, programAnalysis'') = recurseAll env programAnalysis' (childExps exp) in
      -- Conservatively make all branch expression descendants dependent on the scrutinee.
      let programAnalysis''' =
        makeSubtreesDependent programAnalysis'' (tbranchExps tbranches) (Set.singleton scrutinee.val.eid)
      in
      retDict allPartsStatic programAnalysis'''

    EApp ws1 funcExp argExps ws2 ->
      let (allChildrenStatic, programAnalysis'') = recurseAll env programAnalysis' (childExps exp) in
      retDict allChildrenStatic programAnalysis''

    ELet _ _ isRecursive pat assign body _ ->
      let assignEnv =
        case (isRecursive, pat.val) of
          (True, PVar _ ident _)  -> (ident, Known assign.val.eid)::env
          (True, PList _ _ _ _ _) -> env -- mutually recursive functions not supported
          _                       -> env
      in
      let (isAssignStatic, _, programAnalysis'') = staticAnalyze_ assignEnv programAnalysis' assign in
      let patStaticEnv =
        -- let _ =
        --   if isProgramEId thisEId then
        --     Debug.log ("Static pat match " ++ unparsePat pat) (maybeMatchExp pat assign)
        --   else
        --     Nothing
        -- in
        case maybeMatchExp pat assign of
          Just env -> env |> List.map (\(ident, exp) -> (ident, Known exp.val.eid))
          Nothing  -> identifiersListInPat pat |> List.map (\ident -> if isAssignStatic then (ident, StaticUnknown) else (ident, Bound))
      in
      retRecurseEquivalentExpression ("ELet " ++ (toString isRecursive)) (patStaticEnv ++ env) programAnalysis'' body

    EComment _ _ body       -> retRecurseEquivalentExpression "EComment"   env programAnalysis' body
    EOption _ _ _ _ body    -> retRecurseEquivalentExpression "EOption"    env programAnalysis' body
    ETyp _ _ _ body _       -> retRecurseEquivalentExpression "ETyp"       env programAnalysis' body
    EColonType _ body _ _ _ -> retRecurseEquivalentExpression "EColonType" env programAnalysis' body
    ETypeAlias _ _ _ body _ -> retRecurseEquivalentExpression "ETypeAlias" env programAnalysis' body


programAnalysisWithTopLevelEId : Exp -> ProgramAnalysisWithTLEId
programAnalysisWithTopLevelEId program =
  let programAnalysis = staticAnalyzeWithPrelude program in
  let lastTopLevelEId =
    (Utils.last "Brainstorm.programAnalysisWithTopLevelEId: can't handle empty program" (topLevelExps program)).val.eid
  in
  { programAnalysis = programAnalysis
  , lastTopLevelEId = lastTopLevelEId
  }


------------------------------------------------------
-- Solution Enumeration / Program Rewriting         --
------------------------------------------------------

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
freeVarEIdsInExp : ProgramAnalysis -> EId -> Maybe (List (Ident, EId))
freeVarEIdsInExp programAnalysis mobileEId =
  let mobileExpAn = justGetExpressionAnalysis programAnalysis mobileEId in
  let mobileExp   = mobileExpAn.exp in
  let freeIdents  = freeIdentifiers mobileExp |> Set.toList in
  let visibleVars = mobileExpAn.staticEnv in
  freeIdents
  |> Utils.foldlMaybe
      (\freeIdent freeVarEIds ->
        case Utils.maybeFind freeIdent visibleVars of
          Just (Known eid) -> Just ((freeIdent, eid)::freeVarEIds)
          _                -> Nothing -- not found in static env, not resolvable to an expression, or bound
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


-- Simple expression name suggestion.
-- TODO: If exp used as an argument, can use the function's argument names
nameForEId : Exp -> EId -> Maybe Ident
nameForEId exp eid =
  let recurseChildren () =
    childExps exp
    |> Utils.mapFirstSuccess (\child -> nameForEId child eid)
  in
  case exp.val.e__ of
    EVar _ ident ->
      if exp.val.eid == eid then
        Just ident
      else
        Nothing

    ELet _ _ _ pat assign body _ ->
      case maybeMatchExp pat assign of
        Just env ->
          case env |> Utils.findFirst (\(ident, e) -> e.val.eid == eid) of
            Just (ident, _) -> Just ident
            Nothing         -> recurseChildren ()

        Nothing  ->
          recurseChildren ()

    _ ->
      recurseChildren ()


nameForLift : Exp -> EId -> Set.Set EId -> Set.Set Ident -> Ident
nameForLift program mobileEId usageEIds extraNamesToAvoid =
  let _ = Debug.log "extraNamesToAvoid" extraNamesToAvoid in
  let _ = Debug.log "visible at eids" (visibleIdentifiersAtEIds program (Set.insert mobileEId usageEIds) |> Set.toList) in
  -- Needs to be a name free at both the needed location and the lifted location
  let usedNames =
    visibleIdentifiersAtEIds program (Set.insert mobileEId usageEIds)
    |> Set.union extraNamesToAvoid
  in
  let suggestedName = Maybe.withDefault "lifted" <| nameForEId program mobileEId in
  ValueBasedTransform.nonCollidingName suggestedName usedNames


-- Performs renamings in program, but returns renaming on mobileEId
-- so caller can use it on "off-program" expressions. TODO: revisit, may not need renamings
liftSoVisbleTo : Exp -> EId -> EId -> Set.Set Ident -> Maybe (Exp, Dict.Dict EId Ident)
liftSoVisbleTo originalProgram mobileEId observerEId extraNamesToAvoid =
  -- let _ = Debug.log "looking for eid" mobileEId in
  let programAnalysis = staticAnalyzeWithPrelude originalProgram in
  -- If there are equivalent expressions, lift the highest one.
  let mobileEId' =
    if isProgramEId mobileEId then
      let equivalentEIds = (justGetExpressionAnalysis programAnalysis mobileEId).equivalentEIds in
      let findFirst_ e =
        if Set.member e.val.eid equivalentEIds then
          Just e.val.eid
        else
          Utils.mapFirstSuccess findFirst_ (childExps e)
      in
      findFirst_ originalProgram |> Utils.fromJust_ "expected mobileEId (or equivalent) to exist in program"
    else
      mobileEId
  in
  case Dict.get mobileEId' (eidsAvailableAsVarsAt programAnalysis observerEId) of
    Just ident ->
      let _ = Debug.log "found as" ident in
      let renamings = Dict.singleton mobileEId' ident in
      Just (originalProgram, renamings)

    Nothing ->
      let deepestCommonAncestorEId =
        let deepestCommonAncestor =
          let ancestorsAndMobileExp   = findWithAncestors originalProgram mobileEId' in
          let ancestorsAndObserverExp = findWithAncestors originalProgram observerEId in
          let commonAncestors = Utils.commonPrefix [ancestorsAndMobileExp, ancestorsAndObserverExp] in
          let errorMsg = "couldn't find common ancestor of " ++ (toString mobileEId') ++ " and " ++ (toString observerEId) in
          Utils.last errorMsg commonAncestors
        in
        deepestCommonAncestor.val.eid
      in
      let maybeDependenciesLifted =
        case freeVarEIdsInExp programAnalysis mobileEId' of
          Nothing ->
            -- let _ = Debug.log "not found, can't lift" () in
            Nothing -- need to lift a bound var. TODO: allow lifting if we don't exceed its scope

          Just varEIdsToLift ->
            let _ = Debug.log "found, trying to lift dependencies" varEIdsToLift in
            varEIdsToLift
            |> Utils.foldlMaybe
                (\(varName, eidToLift) program ->
                  liftSoVisbleTo program eidToLift deepestCommonAncestorEId extraNamesToAvoid
                  |> Maybe.map
                      (\(newProgram, renamings) ->
                        -- MobileEId is going to be moved, so it needs to use the lifted name, not the original.
                        let mobileExp = justFindExpByEId program mobileEId' in
                        let mobileExp' =
                          renameVarsUntilBound (Dict.singleton varName (Utils.justGet_ "Brainstorm.liftSoVisbleTo: recursion shoulda worked" eidToLift renamings)) mobileExp
                        in
                        let prog' = replaceExpNode mobileExp.val.eid mobileExp' newProgram in
                        let _ = Debug.log (unparse prog') () in
                        prog'
                      )
                )
                (Just originalProgram)
      in
      maybeDependenciesLifted
      |> Maybe.map
          (\dependenciesLifted ->
            -- TODO: better names
            let liftedName            = nameForLift dependenciesLifted mobileEId' (Set.singleton observerEId) extraNamesToAvoid in
            let deepestCommonAncestor = justFindExpByEId dependenciesLifted deepestCommonAncestorEId in
            let expToLift             = justFindExpByEId deepestCommonAncestor mobileEId' in
            let renamings             = Dict.singleton mobileEId' liftedName in
            let deepestCommonAncestorTargetExpRenamed = turnExpsToVars deepestCommonAncestor renamings in
            -- c.f. ValueBasedTransform.wrapWithLets for possible code deduplication
            -- we need to preserve EIds here
            -- TODO: pretty whitespace
            let liftedSubtree = eLets [(liftedName, (replacePrecedingWhitespace " " expToLift))] deepestCommonAncestorTargetExpRenamed in
            let lifted = replaceExpNode deepestCommonAncestorEId liftedSubtree dependenciesLifted in
            -- let _ = Debug.log (unparse lifted) () in
            (lifted, Dict.singleton mobileEId' liftedName)
          )


-- EId's not in program/prelude are inserted expressions)
--
-- Different from freeVarEIdsInExp b/c we expressions tagged as
-- already in the program are denoted as needed and will be assigned
-- to variables and so we don't need to recurse through them.
neededEIds : ProgramAnalysisWithTLEId -> Exp -> Set.Set EId
neededEIds programAnalysisTLE exp =
  neededEIds_ Set.empty programAnalysisTLE exp


neededEIds_ : Set.Set Ident -> ProgramAnalysisWithTLEId -> Exp -> Set.Set EId
neededEIds_ boundIdentsSet programAnalysisTLE exp =
  let recurse = neededEIds_ boundIdentsSet programAnalysisTLE in
  let recurseChildren () =
    List.map recurse (childExps exp)
    |> Utils.unionAll
  in
  let eid = exp.val.eid in
  if isProgramEId eid then
    Set.singleton eid
  else
    case exp.val.e__ of
      EConst _ i l wd             -> Set.empty
      EBase _ v                   -> Set.empty
      EVar _ x                    ->
        if Set.member x boundIdentsSet then
          Set.empty
        else
          case findVarAtEId programAnalysisTLE exp of
            Just assignedExp ->
              if isActualEId assignedExp.val.eid then
                Set.singleton assignedExp.val.eid
              else
                Debug.crash <| "independent side has reference to var " ++ x ++ " which refers to a non-actual EId"

            Nothing ->
              Debug.crash <| "independent side has reference to unknown var " ++ x

      EFun _ ps e _               -> neededEIds_ (Set.union (identifiersSetInPats ps) boundIdentsSet) programAnalysisTLE e
      EOp _ op es _               -> recurseChildren ()
      EList _ es _ m _            -> recurseChildren ()
      EIndList _ rs _             -> recurseChildren ()
      EIf _ e1 e2 e3 _            -> recurseChildren ()
      ECase _ e1 bs _             ->
        let neededInScrutinee = recurse e1 in
        let neededInEachBranch =
          (List.map .val bs)
          |> List.map (\(Branch_ _ bPat bExp _) -> neededEIds_ (Set.union (identifiersSetInPat bPat) boundIdentsSet) programAnalysisTLE bExp)
        in
        Utils.unionAll (neededInScrutinee::neededInEachBranch)

      ETypeCase _ e1 tbranches _  -> recurseChildren ()
      EApp _ e1 es _              -> recurseChildren ()
      ELet _ _ False p e1 e2 _    ->
        let neededInAssigns = recurse e1 in
        let neededInBody    = neededEIds_ (Set.union (identifiersSetInPat p) boundIdentsSet) programAnalysisTLE e2 in
        Set.union neededInAssigns neededInBody

      ELet _ _ True p e1 e2 _ ->
        let neededInAssigns = neededEIds_ (Set.union (identifiersSetInPat p) boundIdentsSet) programAnalysisTLE e1 in
        let neededInBody    = neededEIds_ (Set.union (identifiersSetInPat p) boundIdentsSet) programAnalysisTLE e2 in
        Set.union neededInAssigns neededInBody

      EComment _ _ e1       -> recurseChildren ()
      EOption _ _ _ _ e1    -> recurseChildren ()
      ETyp _ _ _ e1 _       -> recurseChildren ()
      EColonType _ e1 _ _ _ -> recurseChildren ()
      ETypeAlias _ _ _ e1 _ -> recurseChildren ()
      EVal _                -> Debug.crash "Brainstorm.neededEIds_: shouldn't have an EVal in given expression"
      EDict _               -> Debug.crash "Brainstorm.neededEIds_: shouldn't have an EDict in given expression"


redefineExp originalProgram indep depEId =
  let programAnalysisTLE = programAnalysisWithTopLevelEId originalProgram in
  let boundIdentsInIndep =
    -- Names to avoid for lifted renamings
    identifiersSetPatsOnly indep
  in
  let maybeLiftedAndRenamings =
    (Debug.log "neededEIDs" <| neededEIds programAnalysisTLE indep)
    |> Set.toList
    |> Utils.foldlMaybe
        (\eid (program, renamings) ->
          liftSoVisbleTo program eid depEId boundIdentsInIndep
          |> Maybe.map (\(newProgram, newestRenamings) -> (newProgram, Dict.union newestRenamings renamings))
        )
        (Just (originalProgram, Dict.empty))
  in
  maybeLiftedAndRenamings
  |> Maybe.map
      (\(lifted, renamings) ->
        -- let _ = Debug.log "renamings" renamings in
        let dependent = turnExpsToVars indep renamings in
        let maxEId = List.maximum ([LangParser2.initK] ++ allActualEIds originalProgram ++ allLocIds originalProgram) |> Utils.fromJust in
        let (freshenedDependent, _) = LangParser2.freshen_ (maxEId + 10) dependent in
        let dependentReplaced = replaceExpNode depEId freshenedDependent lifted in
        dependentReplaced
      )


-- TODO:
--   - Allow expansion into a list of constraints that must be satisfied simultaneously (e.g. [x1, y1] = [x2, y2]  =>  x1 = x2 and y1 = y2)
--   - Simple simplification of unary functions / unary ops i.e. myFunc arg1 = myFunc arg2 => arg1 = arg2 (this can actaully lead to undesirabl solutions... e.g. (x pt1) = (x pt2) => pt1 = pt2)
--   - Simplification of functions/ops with >1 arg
--      -- e.g. myFunc arg1 arg2 = myFunc arg1 arg3 => arg2 = arg3
--      -- e.g. myFunc arg1 arg2 = myFunc arg3 arg4 => arg1 = arg3 and arg2 = arg4 (this may be less desirable)
--   - Allow expansions of independent side
--     - Allow solving for locs (eids??) that initially appear on both sides, to match the power
--       of the original loc-based solver
solutionsForDependentProgramLocation : ProgramAnalysisWithTLEId -> Constraint -> List (List (Exp, EId))
solutionsForDependentProgramLocation programAnalysisTLE constraint =
  let { independent, dependent } = constraint in
  -- let recurse newConstraint      = solutionsForDependentProgramLocation program newConstraint in
  -- let recurseSimple newDependent = recurse (Constraint independent newDependent) in
  -- let eidToExpAnalysis eid       = Utils.justGet_ "Brainstorm.solutionsForDependentProgramLocation" eid dependencyAnalysis in
  -- let eidToCTerm eid             = expToCTerm (eidToExpAnalysis eid).exp in
  -- let depEId = dependent.val.eid in
  let _ = Debug.log "generating independent expression options" () in
  let independentOptions =
    let generateOptions_ indep =
      case maybeSymbolicallyEvaluateOneStepForConstraintEnumeration programAnalysisTLE indep of
        Just newIndep -> indep::(generateOptions_ newIndep)
        Nothing       -> [indep]
    in
    generateOptions_ independent
  in
  let _ = Debug.log "independent options count" (List.length independentOptions) in
  let dependentOptions =
    let generateOptions_ indep dep =
      let optionsViaEquationRearrangement =
        case dep.val.e__ of
          -- There's more algebra opportunities here
          -- Be dumb for now
          EOp _ op args _ ->
            let op_ = op.val in
            let logBadOp ret =
              let _ = Debug.log "Bad op" (unparse dep) in
              ret
            in
            let binOpInverseConstraints binOp leftArg rightArg =
             case binOp of
                Plus  -> [ (eOp Minus [indep, rightArg], leftArg), (eOp Minus [indep, leftArg], rightArg) ]
                Minus -> [ (eOp Plus  [indep, rightArg], leftArg), (eOp Minus [leftArg, indep], rightArg) ]
                Mult  -> [ (eOp Div   [indep, rightArg], leftArg), (eOp Div   [indep, leftArg], rightArg) ]
                Div   -> [ (eOp Mult  [indep, rightArg], leftArg), (eOp Div   [leftArg, indep], rightArg) ]
                Pow   -> [ (eOp Pow [indep, (eOp Div [eConstNoLoc 1, rightArg])], leftArg) ] --  lhs^(1/r) = l; but need log op to solve for r
                _     -> []
            in
            let binOpSolutions binOp args =
              case args of
                [leftArg, rightArg] ->
                  binOpInverseConstraints binOp leftArg rightArg

                _ ->
                  logBadOp []
            in
            let unOpInverseMaybeConstraint unOp arg =
              case unOp of
                Cos    -> [ (eOp ArcCos [indep],             arg) ]
                Sin    -> [ (eOp ArcSin [indep],             arg) ]
                ArcCos -> [ (eOp Cos [indep],                arg) ]
                ArcSin -> [ (eOp Sin [indep],                arg) ]
                Sqrt   -> [ (eOp Pow [indep, eConstNoLoc 2], arg) ]
                _      -> []
            in
            let unOpSolutions unOp args =
              case args of
                [arg] ->
                  unOpInverseMaybeConstraint unOp arg

                _ ->
                  logBadOp []
            in
            case op_ of
              Plus          -> binOpSolutions op_ args -- need type checker here to differentiate from string plus; just rely on the crash check for now
              Minus         -> binOpSolutions op_ args
              Mult          -> binOpSolutions op_ args
              Div           -> binOpSolutions op_ args
              Mod           -> []
              Pow           -> binOpSolutions op_ args
              ArcTan2       -> []
              Lt            -> [] -- heh; handling inequalities could be fun
              Eq            -> []
              Pi            -> []
              DictEmpty     -> []
              DictInsert    -> []
              DictGet       -> [] -- Ignoring DictGet will limit us when we switch SVG attrs to be a dict
              DictRemove    -> []
              Cos           -> unOpSolutions op_ args
              Sin           -> unOpSolutions op_ args
              ArcCos        -> unOpSolutions op_ args
              ArcSin        -> unOpSolutions op_ args
              Floor         -> []
              Ceil          -> []
              Round         -> []
              Sqrt          -> unOpSolutions op_ args
              Explode       -> []
              DebugLog      -> []
              ToStr         -> []
              RangeOffset _ -> logBadOp []

          _ ->
            []
      in
      let optionsOnThisIteration = (indep, dep)::optionsViaEquationRearrangement in
      let evaluatedOneStep =
        optionsOnThisIteration
        |> List.filterMap (\(i, d) -> maybeSymbolicallyEvaluateOneStepForConstraintEnumeration programAnalysisTLE d |> Maybe.map ((,) i))
      in
      optionsOnThisIteration ++ (evaluatedOneStep |> List.concatMap (\(i, d) -> generateOptions_ i d))
    in
    generateOptions_ independentDummyExp dependent
  in
  let multiConstraintSolutions =
    let dependentWithBareIndependent =
      dependentOptions |> List.filter (\(i, d) -> i == independentDummyExp)
    in
    Utils.cartProd independentOptions dependentWithBareIndependent
    |> List.filterMap
        (\(indep, (_, dep)) ->
          case (indep.val.e__, dep.val.e__) of
            (EList _ indepHeads _ Nothing _, EList _ depHeads _ Nothing _) -> Utils.maybeZip indepHeads depHeads
            _                                                              -> Nothing
        )
    |> List.map (List.map (\(indep, dep) -> { independent = indep, dependent = dep }))
    |> List.concatMap
        (\constraints ->
          let solutionsForEachConstraint =
            constraints |> List.map (solutionsForDependentProgramLocation programAnalysisTLE)
          in
          let solutionCombos =
            Utils.oneOfEach solutionsForEachConstraint |> List.map List.concat
          in
          solutionCombos
        )
  in
  let _ = Debug.log "multi-constraint solutions count" (List.length multiConstraintSolutions) in
  let singleConstraintSolutions =
    let dependentOptionsInProgram =
      dependentOptions |> List.filter (\(i, d) -> isProgramEId d.val.eid)
    in
    let _ = Debug.log "dependent options in program count" (List.length dependentOptionsInProgram) in
    Utils.cartProd independentOptions dependentOptionsInProgram
    |> List.map
        (\(indep, (indepWrapped, dep)) ->
          [(replaceExpNode independentDummyEId indep indepWrapped, dep.val.eid)]
        )
  in
  singleConstraintSolutions ++ multiConstraintSolutions


isValidSolution programAnalysis constraintList =
  let allIndependentsIndependent =
    List.all (isIndependentActuallyIndependent programAnalysis) constraintList
  in
  let allDependentsIndependentlyReplaceable =
    constraintList
    |> List.map (\(_, depEId) -> allActualEIds (eidToExp programAnalysis depEId) |> Set.fromList)
    |> Utils.anyOverlap
    |> not
  in
  allIndependentsIndependent && allDependentsIndependentlyReplaceable


-- Does not check that all indep expressions are static.
-- Non-static indep expressions will fail to lift (their
-- bound vars fail a lookup)
isIndependentActuallyIndependent programAnalysis (independent, depEId) =
  -- Look for targets in the transitive dependencies of the toVisit list.
  let findDependencyBFS_ visited toVisit targets =
    -- You have to do some set/list conversions here. My guess is this route below is fastest.
    case toVisit of
      []             -> False
      eid::remaining ->
        if Set.member eid visited then
          findDependencyBFS_ visited remaining targets
        else if Set.member eid targets then
          True
        else
          let visited' = Set.insert eid visited in
          let toVisit' = (Set.toList (justGetExpressionAnalysis programAnalysis eid).immediatelyDependentOn) ++ remaining in
          findDependencyBFS_ visited' toVisit' targets
  in
  let indepEIds = Set.toList <| Set.fromList <| allActualEIds independent in
  let eidsBeingReplaced =
    allActualEIds (eidToExp programAnalysis depEId) |> Set.fromList
  in
  let isIndepDependentOnDep =
    findDependencyBFS_ Set.empty indepEIds eidsBeingReplaced
  in
  not isIndepDependentOnDep


maybeMakeEqualConstraint originalProgram constraint =
  let _ = Debug.log "statically evaluating" () in
  let programAnalysisTLE = programAnalysisWithTopLevelEId originalProgram in
  let _ = Debug.log "finding solutions" () in
  let solutions =
    solutionsForDependentProgramLocation programAnalysisTLE constraint
    |> List.filter (isValidSolution programAnalysisTLE.programAnalysis)
  in
  let _ = Debug.log "solution count" (List.length solutions) in
  solutions
  |> Utils.mapFirstSuccess
      (\solutionParts ->
        let _ = Debug.log "trying solution" <| List.map (\(indep, depEId) -> (unparse indep, depEId)) solutionParts in
        solutionParts
        |> Utils.foldlMaybe
            (\(indep, depEId) prog ->
              let _ = Debug.log "trying to transform" (unparse indep, depEId) in
              redefineExp prog indep depEId
            )
            (Just originalProgram)
        |> Maybe.map (\newProgram -> let _ = Debug.log (unparse newProgram) () in newProgram)
      )
  -- |> Maybe.oneOf


relate originalProgram idea zoneToRelate slideNumber movieNumber movieTime =
  let (shapeId, zoneName) = zoneToRelate in
  -- let _ =
  --   let progAn = staticAnalyzeWithPrelude originalProgram in
  --   Debug.log (progAn |> Dict.toList |> List.map (\(eid, expAn) -> (toString eid) ++ " " ++ (unparse expAn.exp |> String.trimLeft |> String.left 33)) |> String.join "\n") ()
  -- in
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
                  case ideaToMaybeConstraint originalProgram (zoneNameToConstraintType zoneName) idea zoneIdea of
                    Nothing         -> originalProgram
                    Just constraint ->
                      case maybeMakeEqualConstraint originalProgram constraint of
                        Nothing         -> originalProgram
                        Just newProgram -> newProgram
