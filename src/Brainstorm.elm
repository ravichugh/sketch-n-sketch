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


justGetExpressionAnalysis programAnalysis eid =
  Utils.justGet_ ("Brainstorm.justGetExpressionAnalysis: could not find eid " ++ (toString eid)) eid programAnalysis


--- Precondition: programAnalysis has calculated the closures
resultDependenceTransitiveClosure programAnalysis eid =
  case (justGetExpressionAnalysis programAnalysis eid).dependenceClosure of
    Just closure -> closure
    Nothing      -> Debug.crash <| "Brainstorm.resultDependenceTransitiveClosure: closure not calculated for eid " ++ (toString eid)


eidToExp programAnalysis eid =
  (justGetExpressionAnalysis programAnalysis eid).exp


eidIsStatic programAnalysis eid =
  (justGetExpressionAnalysis programAnalysis eid).isStatic


-- Does not check that all indep expressions are static.
-- Non-static indep expressions will fail to lift (their
-- bound vars fail a lookup)
isValidSolution programAnalysis (independent, depEId) =
  let indepEIds = Set.toList <| Set.fromList <| allActualEIds independent in
  -- let _ = Debug.log "indepEIds immediate dependence" () in
  -- let _ =
  --   Debug.log
  --       (programAnalysis
  --       |> Dict.toList
  --       |> List.filter (\(eid, _) -> List.member eid indepEIds)
  --       |> List.map
  --           (\(eid, expAn) ->
  --             (unparse expAn.exp |> Utils.squish |> String.left 33)
  --             ++ "\n    " ++ (
  --               expAn.immediatelyDependentOn
  --               |> Set.toList
  --               |> List.map (\depEId -> eidToExp programAnalysis depEId |> unparse |> Utils.squish |> String.left 33)
  --               |> String.join "\n    "
  --             )
  --           )
  --       |> String.join "\n") ()
  -- in
  -- let _ = Debug.log "indepEIds dependence closures" () in
  -- let _ =
  --   Debug.log
  --       (programAnalysis
  --       |> Dict.toList
  --       |> List.filter (\(eid, _) -> List.member eid indepEIds)
  --       |> List.map
  --           (\(eid, expAn) ->
  --             (unparse expAn.exp |> Utils.squish |> String.left 33)
  --             ++ "\n    " ++ (
  --               expAn.dependenceClosure
  --               |> Utils.fromJust
  --               |> Set.toList
  --               |> List.map (\depEId -> eidToExp programAnalysis depEId |> unparse |> Utils.squish |> String.left 33)
  --               |> String.join "\n    "
  --             )
  --           )
  --       |> String.join "\n") ()
  -- in
  let indepEIdsTransitiveClosure =
    indepEIds
    |> List.map (resultDependenceTransitiveClosure programAnalysis)
    |> Utils.unionAll
    |> Set.union (Set.fromList indepEIds)
  in
  -- let _ = Debug.log "indepEIdsTransitiveClosure" (Set.toList indepEIdsTransitiveClosure) in
  -- let _ =
  --   Debug.log
  --       ("\n    " ++ (
  --           indepEIdsTransitiveClosure
  --           |> Set.toList
  --           |> List.map (\depEId -> eidToExp programAnalysis depEId |> unparse |> Utils.squish |> String.left 33)
  --           |> String.join "\n    "
  --         )
  --       ) ()
  -- in
  let eidsBeingReplaced =
    depEId :: (allActualEIds (eidToExp programAnalysis depEId))
    |> Set.fromList
  in
  let _ = Debug.log "eidsBeingReplaced" (Set.toList eidsBeingReplaced) in
  let isIndepNotDependentOnDep =
    0 == Set.size (Set.intersect indepEIdsTransitiveClosure eidsBeingReplaced)
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
          Just (Known eid) -> (findExpByEId program eid) `Utils.orTry` (\() -> findExpByEId LangParser2.prelude eid)
          _                ->
            let _ = Debug.log ("findVarAtEId: " ++ ident ++ " not in env at " ++ (toString searchScopeEId)) (List.map fst visibleVars) in
            Nothing -- not found in static env, not resolvable to an exp, or bound

      _ ->
        let _ = Debug.log ("findVarAtEId: not given an EVar: " ++ (unparse varExp)) () in
        Nothing
  else
    let _ = Debug.log "findVarAtEId: not actual eid" varEId in
    Nothing


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


-- TODO: change StaticEnv/neededEId into set of EIds instead of single (multiple exps may resolve the same)
visibleVarsAt : Exp -> EId -> StaticEnv
visibleVarsAt program observerEId =
  let programAnalysis = staticAnalyzeWithPrelude program in
  let errorMsg = "Brainstorm.visibleVarsAt: can't find eid " ++ (toString observerEId) ++ " in program analysis" in
  let observerExpAn = Utils.justGet_ errorMsg observerEId programAnalysis in
  observerExpAn.staticEnv


eidsAvailableAsVarsAt : Exp -> EId -> Dict.Dict EId Ident
eidsAvailableAsVarsAt program observerEId =
  let programAnalysis = staticAnalyzeWithPrelude program in
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


type StaticVarRef
  = Known EId     -- Var references a known EId. Var is static if the referenced expression is static.
  | StaticUnknown -- Var references a static value, but can't say exactly what. (E.g. var came from the deconstruction of a static value, but we're too imprecise to work out the deconstruction exactly)
  | Bound         -- Var references a bound argument, not static.

type alias StaticEnv = List (Ident, StaticVarRef)


patToDeadStaticEnv : Pat -> StaticEnv
patToDeadStaticEnv pat =
  identifiersListInPat pat
  |> List.map (\ident -> (ident, Bound))


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
  , staticEnv              : StaticEnv
  , isStatic               : Bool
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
  let _ =
    let sanityCheck =
      closedProgramAnalysis
      |> Dict.toList
      |> List.all
          (\(_, expAn) ->
            0 == Set.size (Set.diff expAn.immediatelyDependentOn (expAn.dependenceClosure |> Utils.fromJust))
          )
    in
    if sanityCheck then () else Debug.crash "Immediate dependencies not in closure!!"
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
      let nonSelfDepSccIds = Set.toList (Set.remove sccId sccAn.immediatelyDependentOn) in
      let sccsAnalysis' =
        nonSelfDepSccIds
        |> List.foldl
            ensureSCCDAGDependenceClosure
            sccsAnalysis
      in
      let initialEIds =
        if Set.member sccId sccAn.immediatelyDependentOn then
          Set.fromList sccAn.eids
        else
          Set.empty
      in
      let (dependenceClosure, dependenceClosureEIds) =
        nonSelfDepSccIds
        |> List.foldl
            (\depSccId (depSccIds, depEIds) ->
              let depExpAn =
                Utils.justGet_ "Brainstorm.ensureSCCDAGDependenceClosure: missing dep sccId" depSccId sccsAnalysis'
              in
              let depClosure =
                depExpAn.dependenceClosure |> Utils.fromJust_ "Brainstorm.ensureSCCDAGDependenceClosure: closure not computed"
              in
              let depClosureEIds =
                depExpAn.dependenceClosureEIds |> Utils.fromJust_ "Brainstorm.ensureSCCDAGDependenceClosure: eid closure not computed"
                |> Set.union (Set.fromList depExpAn.eids)
              in
              (Set.union depClosure depSccIds, Set.union depEIds depClosureEIds)
            )
            (sccAn.immediatelyDependentOn, initialEIds)
      in
      Dict.insert
        sccId
        { sccAn | dependenceClosure = Just dependenceClosure, dependenceClosureEIds = Just dependenceClosureEIds }
        sccsAnalysis'


(_, preludeStaticEnv, preludeProgramAnalysis) =
  staticAnalyze_ [] Dict.empty LangParser2.prelude


-- Also computes dependence closure
staticAnalyzeWithPrelude : Exp -> ProgramAnalysis
staticAnalyzeWithPrelude program =
  let _ = Debug.log "static evaling program" () in
  let (_, _, programAnalysis) =
    staticAnalyze_ preludeStaticEnv preludeProgramAnalysis program
  in
  let _ = Debug.log "gathering equivalent EIds" () in
  let programAnalysis' = gatherEquivalentEIds programAnalysis in
  -- let _ =
  --   Debug.log
  --       (programAnalysis'
  --       |> Dict.toList
  --       |> List.filter (\(eid, _) -> isProgramEId eid)
  --       |> List.map
  --           (\(eid, expAn) ->
  --             (unparse expAn.exp |> Utils.squish |> String.left 33)
  --             ++ "\n    " ++ (
  --               expAn.immediatelyDependentOn
  --               |> Set.toList
  --               |> List.map (\depEId -> eidToExp programAnalysis' depEId |> unparse |> Utils.squish |> String.left 33)
  --               |> String.join "\n    "
  --             )
  --           )
  --       |> String.join "\n") ()
  -- in
  let _ = Debug.log "computing transitive closure" () in
  let programAnalysis'' = computeTransitiveDependenceClosure programAnalysis' in
  programAnalysis''


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
  let replaceExp newE__ = replaceE__ exp newE__ in
  let thisEId = exp.val.eid in
  let programAnalysis' =
    let expressionAnalysis =
      { exp                    = exp
      , staticEnv              = env
      , isStatic               = True
      , equivalentEIds         = Set.singleton thisEId
      , immediatelyDependentOn = childExps exp |> List.map (.eid << .val) |> Set.fromList
      , dependenceClosure      = Nothing -- calculated separately
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
    let (staticBools, _, programAnalyses) =
      exps |> List.map (staticAnalyze_ env programAnalysis) |> Utils.unzip3
    in
    let mergedProgramAnalysis = Utils.mergeAll (programAnalysis::programAnalyses) in
    let allStatic = List.all ((==) True) staticBools in
    (allStatic, mergedProgramAnalysis)
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


maybeMakeEqualConstraint programEnv originalProgram constraint =
  let _ = Debug.log "statically evaluating" () in
  let programAnalysis = staticAnalyzeWithPrelude originalProgram in
  let _ = Debug.log "finding solutions" () in
  let solutions =
    solutionsForDependentProgramLocation originalProgram constraint
    |> List.filter (isValidSolution programAnalysis)
  in
  let _ = Debug.log "solutions" solutions in
  solutions
  |> Utils.mapFirstSuccess
      (\(indep, depEId) ->
        let _ = Debug.log "trying solution" (unparse indep, depEId) in
        redefineExp programEnv originalProgram indep depEId
      )


relate originalProgram idea zoneToRelate slideNumber movieNumber movieTime =
  let (shapeId, zoneName) = zoneToRelate in
  let _ =
    let progAn = staticAnalyzeWithPrelude originalProgram in
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
                  case ideaToMaybeConstraint originalProgram (zoneNameToConstraintType zoneName) idea zoneIdea of
                    Nothing         -> originalProgram
                    Just constraint ->
                      case maybeMakeEqualConstraint programEnv originalProgram constraint of
                        Nothing         -> originalProgram
                        Just newProgram -> newProgram
