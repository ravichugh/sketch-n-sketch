module Brainstorm where

import Dict
import Set

import Utils
import Lang exposing (..)
import LangTools exposing (..)
import LangSvg exposing (..)
import LangParser2
import LangUnparser
import Eval
import ValueBasedTransform
import LocEqn
import Sync


snapDistance = 30

type alias BarePoint = (Num, Num)

type ConstraintType = XConstraint | YConstraint | PointConstraint

type alias Constraint = { independent: Exp, dependent: Exp }

-- ConstraintTerm mirrors Lang.Exp__, with the notable addition of CTermVal
--
-- For now, the execution environment is implicit:
--   - we resolve variables that are constants in scope at the expression
--   - we give up if a variable is non-constant
--
-- Can't merely wrap Exp because we need to allow children to be CTermVal's.
-- type ConstraintTerm
--   = CTermVal Val
--   | CTermConst EId Num Loc
--   | CTermBase EId EBaseVal
--   | CTermVar EId Ident
--   | CTermFun EId (List Pat) ConstraintTerm
--   -- TODO remember paren whitespace for multiple pats, like TForall
--   -- | CTermFun EId (OneOrMany Pat) ConstraintTerm
--   | CTermApp EId ConstraintTerm (List ConstraintTerm)
--   | CTermOp EId Op (List ConstraintTerm)
--   | CTermList EId (List ConstraintTerm) (Maybe ConstraintTerm)
--   | CTermIndList EId (List ConstraintRange)
--   | CTermIf EId ConstraintTerm ConstraintTerm ConstraintTerm
--   | CTermCase EId ConstraintTerm (List CBranch)
--   | CTermTypeCase EId Pat (List CTBranch)
--   | CTermLet EId LetKind Rec Pat ConstraintTerm ConstraintTerm
--   | CTermComment EId String ConstraintTerm
--   | CTermOption EId String String ConstraintTerm
--   | CTermTyp EId Pat Type ConstraintTerm
--   | CTermColonType EId ConstraintTerm Type
--   | CTermTypeAlias EId Pat Type ConstraintTerm
--
--
-- type ConstraintBranch  = CBranch Pat ConstraintTerm
-- type ConstraintTBranch = CTBranch Type ConstraintTerm
-- type ConstraintRange   = CRInterval ConstraintTerm ConstraintTerm | CRPoint ConstraintTerm


-- cTermFuncCall funcName args =
--   CTermApp dummyEId (CTermVar dummyEId funcName) args
--
--
-- cTermOp op children =
--   CTermOp dummyEId op children
--
--
-- cTermConst n =
--   CTermConst dummyEId n dummyLoc


-- eids cTerm =
--   case cTerm of
--     CTermVal val             -> val.vtrace
--     CTermConst eid _ _       -> [eid]
--     CTermBase eid _          -> [eid]
--     CTermVar eid _           -> [eid]
--     CTermFun eid _ _         -> [eid]
--     -- CTermFun eid _ _         -> [eid]
--     CTermApp eid _ _         -> [eid]
--     CTermOp eid _ _          -> [eid]
--     CTermList eid _ _        -> [eid]
--     CTermIndList eid _       -> [eid]
--     CTermIf eid _ _ _        -> [eid]
--     CTermCase eid _ _        -> [eid]
--     CTermTypeCase eid _ _    -> [eid]
--     CTermLet eid _ _ _ _ _   -> [eid]
--     CTermComment eid _ _     -> [eid]
--     CTermOption eid _ _ _    -> [eid]
--     CTermTyp eid _ _ _       -> [eid]
--     CTermColonType eid _ _   -> [eid]
--     CTermTypeAlias eid _ _ _ -> [eid]


-- expToCTerm exp =
--   let eid = exp.val.eid in
--   case exp.val.e__ of
--     EConst ws n loc wd -> CTermConst eid n loc
--     EBase ws baseV     -> CTermBase eid baseV
--     EVar ws ident      -> CTermVar eid ident
--     ELet ws1 letKind rec pat assign body ws2 ->
--       CTermLet eid letKind rec pat (expToCTerm assign) (expToCTerm body)
--
--     EFun ws1 pats body ws2  ->
--       CTermFun eid pats (expToCTerm body)
--
--     EApp ws1 e1 es ws2     -> CTermApp eid (expToCTerm e1) (List.map expToCTerm es)
--     EOp ws1 op es ws2      -> CTermOp eid op (List.map expToCTerm es)
--     EList ws1 es ws2 m ws3 -> EList eid (List.map expToCTerm es) (Maybe.map expToCTerm m)
--     EIndList ws1 rs ws2    ->
--       let rangeRecurse r = case r.val of
--         RInterval e1 ws e2 -> CRInterval (expToCTerm e1) (expToCTerm e2)
--         RPoint e1          -> CRPoint (expToCTerm e1)
--       in
--       CTermIndList eid (List.map rangeRecurse rs)
--     EIf ws1 e1 e2 e3 ws2      -> CTermIf eid (expToCTerm e1) (expToCTerm e2) (expToCTerm e3)
--     ECase ws1 e1 branches ws2 ->
--       let cBranches =
--         List.map
--             (\b ->
--               let Branch_ bws1 branchPat branchBody bws2 = b.val in
--               CBranch branchPat (expToCTerm branchBody)
--             )
--             branches
--       in
--       CTermCase eid (expToCTerm e1) newBranches
--     ETypeCase ws1 pat tbranches ws2 ->
--       let ctBranches =
--         List.map
--             (\b ->
--               let TBranch_ bws1 branchTipe branchBody bws2 = b.val in
--               CTBranch branchTipe (expToCTerm branchBody)
--             )
--             tbranches
--       in
--       CTermTypeCase eid pat newBranches
--     EComment ws s e1              -> CTermComment eid s (expToCTerm e1)
--     EOption ws1 s1 ws2 s2 e1      -> CTermOption eid s1 s2 (expToCTerm e1)
--     ETyp ws1 pat tipe e ws2       -> CTermTyp eid pat tipe (expToCTerm e)
--     EColonType ws1 e ws2 tipe ws3 -> CTermColonType eid (expToCTerm e) tipe
--     ETypeAlias ws1 pat tipe e ws2 -> CTermTypeAlias eid pat tipe (expToCTerm e)
--

-- For determining dependency graph
-- cTermChildren cTerm =
--   case cTerm of
--     CTermVal _             -> []
--     CTermConst _ _ _       -> []
--     CTermBase _ _          -> []
--     CTermVar _ _           -> []
--     CTermFun _ _ cTermBody -> [cTermBody]
--     CTermApp _ ConstraintTerm (List ConstraintTerm)
--     CTermOp _ Op (List ConstraintTerm)
--     CTermList _ (List ConstraintTerm) (Maybe ConstraintTerm)
--     CTermIndList _ (List Range)
--     CTermIf _ ConstraintTerm ConstraintTerm ConstraintTerm
--     CTermCase _ ConstraintTerm (List Branch)
--     CTermTypeCase _ Pat (List TBranch)
--     CTermLet _ LetKind Rec Pat ConstraintTerm ConstraintTerm
--     CTermComment _ String ConstraintTerm
--     CTermOption _ String String ConstraintTerm
--     CTermTyp _ Pat Type ConstraintTerm
--     CTermColonType _ ConstraintTerm Type
--     CTermTypeAlias _ Pat Type ConstraintTerm


isProgramEId eid =
  eid >= 0 && not (LangParser2.isPreludeEId eid)


allActualEIds exp =
  flattenExpTree exp |> List.map (.eid << .val) |> List.filter ((>=) 0)


possibleRelationsWith : Exp -> RootedIndexedTree -> NodeId -> Zone -> List Idea
possibleRelationsWith program slate shapeId zoneName =
  let (_,tree) = slate in
  let unhandled = [] in
  let env = programEnv program in
  let ideas () = primitiveIdeas env program slate [shapeId] in
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
primitiveIdeas env program slate excludeShapeIds =
  let shapes = shapesOnCanvas slate excludeShapeIds in
  shapesToPoints env program slate shapes |> consolidate


brainstorm : List Idea -> Exp -> RootedIndexedTree -> List Idea
brainstorm previousIdeas program slate =
  case previousIdeas of
    [] -> primitiveIdeas (programEnv program) program slate []
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


shapesToPoints : Env -> Exp -> RootedIndexedTree -> List IndexedTreeNode -> List Idea
shapesToPoints env program slate shapes =
  let (_,tree) = slate in
  let shapeVals = shapes |> List.map (svgNodeToVal tree) in
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


shapeZoneToIdea programEnv shape shapeVal zoneName =
  let shapeName =
    case shape of
      SvgNode shapeName _ _ -> shapeName
      TextNode _            -> Debug.crash "Brainstorm.shapeZoneToIdea: given a text node instead of an SVG node"
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



-- Still only using the first ideaSource
ideaToConstraintTerm idea =
  case firstIdeaSource idea of
    PrimitiveFeature shapeVal functionName ->
      eApp (eVar0 functionName) [eVal shapeVal]

    BasedOnTwoPoints idea1 idea2 functionName ->
      eApp
          (eVar0 functionName)
          [ ideaToConstraintTerm (firstIdeaSource idea1)
          , ideaToConstraintTerm (firstIdeaSource idea2)
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
  let wrapTerm funcName arg = eApp (eVar0 funcName) [arg] in
  let lhs = List.foldlr wrapTerm lhsIdeaTerm extraFunctions in
  let rhs = List.foldlr wrapTerm rhsIdeaTerm extraFunctions in
  [ { independent = lhs, dependent = rhs } ]



ideaToMaybeConstraint : ConstraintType -> Idea -> Idea -> Constraint
ideaToMaybeConstraint constraintType idea1 idea2 =
  Just ideasToConstraints
  let extraFunctions =
    case constraintType of
      XConstraint     -> ["x"]
      YConstraint     -> ["y"]
      PointConstraint -> []
  in
  let cTerm1 = ideaToConstraintTerm idea1 in
  let cTerm2 = ideaToConstraintTerm idea2 in


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


--- ***
resultDependenceTransitiveClosure dependencyAnalysis eid =
  []


-- Does not check that all indep expressions are static.
-- Non-static indep expressions will fail to lift (their
-- bound vars fail a lookup)
isValidSolution dependencyAnalysis (independent, depEId) =
  let indepEIds = Set.toList <| Set.fromList <| allActualEIds independent in
  let indepEIdsTransitiveClosure =
    indepEIds
    |> List.map (resultDependenceTransitiveClosure dependencyAnalysis)
    |> Set.fromList
  in
  let eidsBeingReplaced = Set.fromList <| depEId ++ (allActualEIds (eidToExp dependencyAnalysis depEid)) in
  let isIndepNotDependentOnDep =
    0 == (Set.size <| Set.intersect indepEIdsClosure eidsBeingReplaced)
  in
  isIndepNotDependentOnDep && allIndepEIdsStatic


findVarAtEId program varExp =
  let varEId = varEId.val.eid in
  let visibleVars = visibleVarsAt program varEId in
  case Utils.maybeFind ident visibleVars of
    Just (Just eid) -> findExpByEId program eid
    _               -> Nothing -- not found in static env, or bound


findVarAtEIdOrTopLevel program varExp =
  let varEId = varEId.val.eid in
  case varEId.val.e__ of
    EVar _ ident ->
      if varEId >= 0 then
        -- If this is a var in the program, resolve at its location.
        findVarAtEId program varExp
      else
        -- If this is a var inserted into the constraint, resolve at the top level.
        let lastTopLevelEId =
          let errorMsg = "Brainstorm.solutionsForDependentProgramLocation: can't handle empty program" in
          (Utils.tail errorMsg (topLevelExps program)).val.eid
        in
        let visibleVars = visibleVarsAt program lastTopLevelEId in
        case Utils.maybeFind ident visibleVars of
          Just (Just eid) -> findExpByEId program eid
          _               -> Nothing -- not found in static env, or bound

    _ ->
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
--     ETypeCase _ pat tbranches _ -> Nothing
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
    EConst _ n loc wd  -> exp
    EBase _ bVal       -> exp
    EVar _ indent      ->
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
      case (recuse predicate).val.e__ of
        EBase _ (EBool True)  -> recurse trueBranch
        EBase _ (EBool False) -> recurse falseBranch
        _                     -> []

    ECase _ scrutinee branches _ ->
      case maybeSymbolicMatchBranches program scrutinee branches of
        Nothing ->
          exp

        Just (branchEnv, branchExp) ->
          symbolicSubstitute (Dict.fromList branchEnv) branchExp
          |> recurse

    ETypeCase _ pat tbranches _ -> exp -- TODO: symbolic expansion of typecase
    EApp _ funcExp argExps _    -> Maybe.withDefault exp (maybeSymbolicallyApply False program exp)

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
type SymbolicMatchResult
  = Match (List (Ident, Exp))
  | NoMatch
  | CannotCompare


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
  -- case branches of
  --   [] ->
  --     Nothing
  --
  --   branch::otherBranches ->
  --     let (Branch_ _ bPat bExp _) = branch.val in
  --     case trySymbolicMatch program bPat scrutinee of
  --       Match env     -> Just (env, bExp)
  --       NoMatch       -> maybeSymbolicMatchBranches program scrutinee otherBranches
  --       CannotCompare -> Nothing


-- For use in case branch determinations.
-- Aborts early if any match returns CannotCompare.
maybeExpMatchBranches : Exp -> List Branch -> Maybe (List (Ident, Exp), Exp)
maybeExpMatchBranches scrutinee branches =
  maybeExpMatchBranches_ tryMatchExp scrutinee branches
  -- case branches of
  --   [] ->
  --     Nothing
  --
  --   branch::otherBranches ->
  --     let (Branch_ _ bPat bExp _) = branch.val in
  --     case tryMatchExp bPat scrutinee of
  --       Match env     -> Just (env, bExp)
  --       NoMatch       -> maybeExpMatchBranches scrutinee otherBranches
  --       CannotCompare -> Nothing


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


trySymbolicMatch : Exp -> Pat -> Exp -> SymbolicMatchResult
trySymbolicMatch program pat exp =
  let evaled = symbolicallyEvaluateAsFarAsPossible program exp in
  tryMatchExp_ (trySymbolicMatch program) pat evaled

  -- case pat.val of
  --   PVar _ indent _           -> Just [(ident, exp)]
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
  --     Debug.crash <| "Brainstorm.trySymbolicMatch huh? " ++ (LangUnparser.unparsePat pat) ++ " vs " ++ (LangUnparser.unparse exp)


-- Unlike trySymbolicMatch above, this function presumes the exp has been
-- evaluated to base values
tryMatchExp : Pat -> Exp -> SymbolicMatchResult
tryMatchExp pat exp =
  tryMatchExp_ tryMatchExp pat exp


tryMatchExp_ : (Pat -> Exp -> SymbolicMatchResult) -> Pat -> Exp -> SymbolicMatchResult
tryMatchExp_ recurse pat exp =
  case pat.val of
    PVar _ indent _           -> Just [(ident, exp)]
    PAs _ ident _ innerPat, _ ->
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
            headExps, tailExps = Utils.split (List.length ps)
            let tryHeadMatch =
              Utils.zip ps headExps
              |> List.map (\(p, e) -> recurse p e)
              |> projMatches
            in
            let tryTailMatch =
              recurse restPat tailExps
            in
            [tryHeadMatch, tryTailMatch]
            |> projMatches

        -- TODO: must have same number of heads
        EList _ es _ Just restExp _ ->
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

    _ ->
      Debug.crash <| "Brainstorm.tryMatchExp_ huh? " ++ (LangUnparser.unparsePat pat) ++ " vs " ++ (LangUnparser.unparse exp)


-- Preserves EIds even when children are substituted...I think that's what we want...
symbolicSubstitute : Dict.Dict Ident Exp -> Exp -> Exp
symbolicSubstitute subst exp =
  let recurse e = symbolicSubstitute subst e in
  let recurseWithout introducedIdents e =
    let newSubst =
      List.foldl
          Dict.remove
          subst
          (Set.toList introducedIdents)
    in
    if newSubst.size == 0 then
      e
    else
      symbolicSubstitute newSubst e
  in
  case exp.val.e__ of
    EVal val                    -> exp
    EConst _ i l wd             -> exp
    EBase _ v                   -> exp
    EVar _ x                    -> Utils.getWithDefault x exp subst
    EFun ws1 ps e ws2           -> replaceE__ exp (EFun ws1 ps (recurseWithout (identifiersSetInPats ps) e) ws2)
    EOp ws1 op es ws2           -> replaceE__ exp (EOp ws1 op.val (List.map recurse es) ws2)
    EList ws1 es ws2 m ws3      -> replaceE__ exp (EList ws1 (List.map recurse es) ws2 (Maybe.map recurse m) ws3)
    EIndList ws1 rs ws2         ->
      let newRanges =
        rs
        |> List.map
            (mapValField (\range ->
              case range of
                RInterval e1 rws e2 -> RInterval (recurse e1) rws (recurse e2)
                RPoint e1           -> RPoint (recurse e1)
            ))
      in
      replaceE__ exp (EIndList ws1 newRanges ws2)
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
    ETypeCase _ pat tbranches _ -> Nothing
      let newTBranches =
        tbranches
        |> List.map
            (mapValField (\(TBranch_ bws1 bType bExp bws2) ->
              TBranch_ bws1 bType (recurse bExp) bws2
            ))
      in
      replaceE__ exp (ETypeCase ws1 pat newTBranches ws2)
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


maybeSymbolicallyApply : Bool -> Exp -> Exp -> Maybe Exp
maybeSymbolicallyApply lookAtTopLevel program appExp =
  case appExp.val.e__ of
    EApp _ funcExp argExps _ ->
      let maybeFuncToApply =
        let evaledFuncExp = symbolicallyEvaluateAsFarAsPossible program funcExp in
        case evaledFuncExp.val.e__ of
          EVar _ ident ->
            if lookAtTopLevel then
              case findVarAtEIdOrTopLevel program evaledFuncExp of
                Just funcExp ->
                  case funcExp.val.e__ of
                    EFun _ pats body _ -> Just funcExp
                    _                  -> Nothing
                Nothing -> Nothing
            else
              Nothing

          EFun _ pats body _ -> Just evaledFuncExp
          _                  -> Nothing
      in
      maybeFuncToApply
      |> Maybe.map
          (\funcExp ->
            case funcExp.val.e__ of
              EFun _ pats body _ ->
                -- TODO: Only handles full application at the moment
                case Utils.maybeZip pats argExps of
                  Nothing -> Nothing
                  Just patsAndArgExps ->
                    let maybeSubst =
                      patsAndArgExps
                      |> List.foldlMaybe
                          (\(pat, argExp) subst ->
                            case maybeSymbolicMatch program pat argExp of
                              Nothing  -> Nothing
                              Just env -> Just <| Dict.union (Dict.fromList <| List.reverse env) subst -- reverse b/c technically early entries overwrite later entires; pats shouldn't have repeat vars though
                          )
                          Just Dict.empty
                    in
                    maybeSubst
                    |> Maybe.map (\subst -> symbolicSubstitute subst body)

              _ ->
                Debug.crash <| "Brainstorm.maybeSymbolicallyApply: this branch should be unreachable"
          )

    _ ->
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
solutionsForDependentProgramLocation : DependencyAnalysis -> Exp -> List (ConstraintTerm, EId)
solutionsForDependentProgramLocation dependencyAnalysis program constraint =
  let { independent, dependent } = constraint in
  let recurse newConstraint      = solutionsForDependentProgramLocation dependencyAnalysis program newConstraint in
  let recurseSimple newDependent = recurse (Constraint independent newDependent) in
  -- let eidToExpAnalysis eid       = Utils.justGet_ "Brainstorm.solutionsForDependentProgramLocation" eid dependencyAnalysis in
  -- let eidToCTerm eid             = expToCTerm (eidToExpAnalysis eid).exp in
  let depEId = dependent.val.eid in
  let solutionsIfInProgram eid =
    let candidateSolution = (independent, eid)
    if isProgramEId eid
    then [candidateSolution]
    else []
  in
  List.filter (isValidSolution dependencyAnalysis) -- redundantly checks recursive solutions :/
  <| case dependent.val.e__ of
    -- EEId eid ->
    --   -- TODO: filter out dynamic EIds here
    --   case findExpByEId program eid ->
    --     Just exp -> recurseSimple exp
    --     Nothing  -> []

    EVal val ->
      -- TODO: filter out dynamic/nonprogram EIds here
      let directSolutions = val.vtrace |> List.filter isProgramEId |> List.map (\eid -> (independent, eid))  -- may or may not be redundant with expandedSolutions: revisit
      let expandedSolutions = val.vtrace |> List.concatMap (\eid -> recurseSimple (justFindExpByEId program eid))
      directSolutions ++ expandedSolutions

    EConst _ _ _ _ -> solutionsIfInProgram depEId

    EBase _ _ -> solutionsIfInProgram depEId

    EVar _ ident ->
      let solutionsOnThisTerm = solutionsIfInProgram depEId in
      case findVarAtEIdOrTopLevel program dependent of
        Just exp -> solutionsOnThisTerm ++ (recurseSimple exp)
        Nothing  -> solutionsOnThisTerm

    EFun _ ps e _ ->
      -- Not sure when we'd end up solving for a function.
      -- Removing the function could cause the program to crash. I guess we'll check for
      -- that later in the pipeline and discard if so.
      [(independent, depEId)]

    EApp _ e1 es _ ->
      let solutionsOnThisTerm = solutionsIfInProgram depEId in
      -- Hmm, if the function is recursive this might not know when to stop applying...
      case maybeSymbolicallyApply True program dependent of
        Just applied -> solutionsOnThisTerm ++ (recurseSimple applied)
        Nothing      -> solutionsOnThisTerm

    -- There's more algebra opportunities here
    -- Be dumb for now
    EOp _ op args _ ->
      let solutionsOnThisTerm = solutionsIfInProgram depEId in
      let op_ = op.val in
      let logBadOp ret =
        let _ = Debug.log "Bad op" (LangUnparser.unparse dependent) in
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
            case unOpInverseMaybeConstraint binOp independent arg of
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
            [arg] -> recurse arg
             _    -> logBadOp []

        ToStr         -> []
        RangeOffset _ -> logBadOp []

    EList _ es _ m _ -> -- TODO: both sides need to be destructured
      solutionsIfInProgram depEId

    EIndList _ ranges _ ->
      solutionsIfInProgram depEId

    EIf _ predicate trueBranch falseBranch _ ->
      (solutionsIfInProgram depEId) ++
      case (symbolicallyEvaluateAsFarAsPossible program predicate).val.e__ of
        EBase _ (EBool True)  -> recurse trueBranch
        EBase _ (EBool False) -> recurse falseBranch
        _                     -> []

    ECase _ scrutinee branches _ ->
      (solutionsIfInProgram depEId) ++
      case maybeSymbolicMatchBranches program scrutinee branches of
        Nothing ->
          []

        Just (branchEnv, branchExp) ->
          symbolicSubstitute (Dict.fromList branchEnv) branchExp
          |> recurse

    ETypeCase _ pat tbranches _ ->
      (solutionsIfInProgram depEId) ++
      [] -- TODO: Will need to talk to type checker etc

    ELet _ letKind rec pat assign body _ ->
      (solutionsIfInProgram depEId) ++ (recurse body)

    EComment _ _ body ->
      recurse body

    EOption _ _ _ _ body ->
      recurse body

    ETyp _ pat tipe body _ ->
      (solutionsIfInProgram depEId) ++ -- If body is replaced the type annotation might be invalid, so do try replacing the annotation
      recurse body

    EColonType _ body _ tipe _ ->
      (solutionsIfInProgram depEId) ++ -- If body is replaced the type annotation might be invalid, so do try replacing the annotation
      recuse body

    ETypeAlias _ pat tipe body _ ->
      recurse body


-- TODO: change StaticEnv/neededEId into set of EIds instead of single (multiple exps may resolve the same)
visibleVarsAt : Exp -> EId -> StaticEnv
visibleVarsAt program observerEId =
  let (_, preludeStaticExpEnv, eidToStaticExpEnv) = staticEval [] Dict.empty LangParser2.prelude in
  let (_, _, eidToStaticExpEnv')                  = staticEval preludeStaticExpEnv eidToStaticExpEnv program in
  let eidToStaticEnv                              = Dict.map (\_ (maybeEId, maybeExp) -> maybeEId) eidToStaticExpEnv'
  let errorMsg = "Brainstorm.visibleVarsAt: can't find eid " ++ (toString observerEId) ++ " in static env dict" in
  Utils.justGet_ errorMsg observerEId eidToStaticEnv


-- Need (Maybe EId) so that we can put function bound vars in the env but
-- they resolve to nothing, statically. If we simply leave them out the
-- lookup might wrongly find a shadowed variable higher up.
type alias StaticEnv = List (Ident, Maybe EId)
type alias StaticExpEnv = List (Ident, (Maybe EId, Maybe Exp))


expEnvToStaticExpEnv : List (Ident, Exp) -> StaticExpEnv
expEnvToStaticExpEnv expEnv =
  expEnv
  |> List.map
      (\(ident, exp) ->
        if exp.val.eid >= 0 then
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
    EBase _ (EString s)       -> Just (toString s, "string")
    EBase _ ENull             -> Just ("", "null")
    EList _ heads _ Nothing _ ->
      heads
      |> List.map expToDictKey
      |> Utils.projJusts
      |> Maybe.map (\keyStrings -> (toString keyStrings, "list"))
    _                         -> Nothing


--- NEXT UP: ADD DEPENDENCE TO STATIC EVAL ---

staticEval : StaticExpEnv -> Dict.Dict EId StaticExpEnv -> Exp -> (Maybe Exp, StaticExpEnv, Dict.Dict EId StaticExpEnv)
staticEval env eidToStaticExpEnv exp =

  -- let ret v_                         = ((Val v_ [e.val.eid], []), env) in
  -- let retAdd eid (v,envOut)          = ((Val v.v_ (eid::v.vtrace), []), envOut) in
  -- let retAddWs eid ((v,ws),envOut)   = ((Val v.v_ (eid::v.vtrace), ws), envOut) in
  -- let retAddThis_ (v,envOut)         = retAdd e.val.eid (v,envOut) in
  -- let retAddThis v                   = retAddThis_ (v, env) in
  -- let retBoth (v,w)                  = (({v | vtrace = e.val.eid :: v.vtrace},w), env) in
  -- let replaceEnv envOut (v,_)        = (v, envOut) in
  -- let addWidgets ws1 ((v1,ws2),env1) = ((v1, ws1 ++ ws2), env1) in

  let replaceExp newE__ = replaceE__ exp new__ in -- preserve EId for later extraction
  let thisEId = exp.val.eid in
  let eidToStaticExpEnv' =
    if thisEId >= 0 then
      Dict.insert thisEId env eidToStaticExpEnv
    else
      eidToStaticExpEnv
  in
  let retDict retEIdToStaticExpEnv e      = (Just e, env, retEIdToStaticExpEnv') in
  let retDictNothing retEIdToStaticExpEnv = (Nothing,  env, retEIdToStaticExpEnv') in
  let ret e                               = retDict eidToStaticExpEnv' e in
  let retNothing                          = retDictNothing eidToStaticExpEnv' in
  let recurseAll env eidToStaticExpEnv exps =
    let evaled             = exps |> List.map (staticEval env eidToStaticExpEnv) in
    let resultMaybes       = evaled |> List.map Utils.fst3 in
    let eidToStaticExpEnv' = evaled |> List.map Utils.thd3 |> List.foldl Dict.union eidToStaticExpEnv in
    (resultMaybes, eidToStaticExpEnv')
  in
  case exp.val.e__ of
    EVal _                 -> Debug.crash "Brainstorm.staticEval: figure out how to handle EVal"
    EDict _                -> Debug.crash "Brainstorm.staticEval: figure out how to handle EDict"
    EConst _ n loc wd      -> ret exp
    EBase _ bVal           -> ret exp
    EVar _ ident           -> (Maybe.withDefault Nothing (Utils.maybeFind ident env |> Maybe.map snd), env)
    EFun _ pats body _     ->
      -- Log static envs of func body.
      let (_, _, eidToStaticExpEnv'') =
        let patsEnv =
          identifiersSetInPats pats
          |> Set.toList
          |> List.map (\ident -> (ident, (Nothing, Nothing)))
        in
        let bodyEnv = patsEnv ++ env in
        staticEval bodyEnv eidToStaticExpEnv' body
      in
      retDict eidToStaticExpEnv'' exp

    EOp ws1 op argExps ws2 ->
      let op_ = op.val in
      let (evaledArgExpMaybes, eidToStaticExpEnv'') = recurseAll env eidToStaticExpEnv' argExps in
      let ret'                                      = retDict eidToStaticExpEnv'' in
      let retNothing'                               = retDictNothing eidToStaticExpEnv'' in
      let retConst n                                = ret' <| replaceExp (Const ws1 n dummyLoc noWidgetDecl)
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
                [EBase _ (EString _ s1), EBase _ (EString _ s2)] -> ret' <| replaceExp (EBase ws1 (EString (s1 ++ s2)))
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
            Pi         -> nullaryOp args (eConstNoLoc pi)
            DictEmpty  -> ret' <| replaceExp (EDict Dict.empty)
            DictInsert ->
              case evaledArgExps of
                [eKey, eVal, dict] ->
                  case (dict.val.e__, expToDictKey eKey) of
                    (EDict d, Just dKey) -> ret' <| replaceExp (EDict (Dict.insert dkey eVal d))
                    _                    -> retNothing'
                _ ->
                  retNothing'
            DictGet ->
              case evaledArgExps of
                [eKey, dict] ->
                  case (dict.val.e__, expToDictKey eKey) of
                    (EDict d, Just dKey) -> ret' <| Utils.getWithDefault dkey (replaceExp (EBase ws1 ENull)) d
                    _                    -> retNothing'
                _ ->
                  retNothing'
            DictRemove ->
              case evaledArgExps of
                [eKey, dict] ->
                  case (dict.val.e__, expToDictKey eKey) of
                    (EDict d, Just dKey) -> ret' <| replaceExp (EDict (Dict.remove dkey d))
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
            DebugLog      -> retNothing'
            ToStr         -> retNothing'
            RangeOffset _ -> retNothing'

    EList ws1 heads ws2 maybeRest ws3 ->
      let (evaledHeadMaybes, eidToStaticExpEnv'') = recurseAll env eidToStaticExpEnv' heads in
      let (evaledRestMaybe, eidToStaticExpEnv''') =
        case maybeRest of
          Just restExp ->
            let (evaledRestMaybe, _, eidToStaticExpEnv''') = staticEnvEval env eidToStaticExpEnv'' restExp in
            (evaledRestMaybe, eidToStaticExpEnv''')

          Nothing ->
            (Nothing, eidToStaticExpEnv'')
      in
      let ret'        = retDict eidToStaticExpEnv''' in
      let retNothing' = retDictNothing eidToStaticExpEnv''' in
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
      retNothing -- TODO: I don't want to deal with ranges

    EIf ws1 predicate trueBranch falseBranch ws2 ->
      let (evaledHeadMaybes, eidToStaticExpEnv'') = recurseAll env eidToStaticExpEnv' [predicate, trueBranch, falseBranch] in
      case evaledHeadMaybes of
        [maybeEvaledPredicate, maybeEvaledTrueBranch, maybeEvaledFalseBranch] ->
          case maybeEvaledPredicate |> Maybe.map (.e__ << .val) of
            Just (EBase _ (EBool True))  -> (maybeEvaledTrueBranch,  env, eidToStaticExpEnv'')
            Just (EBase _ (EBool False)) -> (maybeEvaledFalseBranch, env, eidToStaticExpEnv'')
            _                            -> (Nothing,                env, eidToStaticExpEnv'')

        _ ->
          Debug.crash "Brainstorm.staticEval if: this branch should be unreachable"

    ECase ws1 scrutinee branches ws2 ->
      let (maybeScrutineeEvaled, _, eidToStaticExpEnv'') = staticEval env eidToStaticExpEnv' scrutinee in
      let eidToStaticExpEnv''' =
        branches
        |> List.map .val
        |> List.map
            (\(Branch_ _ bPat bExp _) ->
              let patEnv = patToDeadStaticExpEnv bPat in
              let branchEnv = patEnv ++ env in
              staticEval branchEnv eidToStaticExpEnv'' bExp
            )
        |> Utils.unzip3
        |> (\(_, _, dicts) -> dicts)
        |> Utils.mergeAll
      in
      case maybeScrutineeEvaled of
        Nothing ->
          retDictNothing eidToStaticExpEnv'''

        Just scrutineeEvaled ->
          case maybeExpMatchBranches scrutineeEvaled branches of
            Nothing ->
              retDictNothing eidToStaticExpEnv'''

            Just (branchEnv, branchExp) ->
              let branchStaticeExpEnv = expEnvToStaticExpEnv branchEnv in
              -- Yes, this is a re-evaluation. May get more specific results with
              -- a more specific env.
              staticEval (branchStaticeExpEnv ++ env) eidToStaticExpEnv''' branchExp


    ETypeCase _ pat tbranches _ ->
      retNothing -- TODO handle typecase...probably need to at least handle null

    EApp ws1 funcExp argExps ws2 ->
      -- Effectively, we apply if non-recursive. So, guarenteed to terminate.
      -- (Recursive definitions are added to the static env as Nothing.)
      --
      -- Only full application for now.
      let (funcExpEvaledMaybe, _, eidToStaticExpEnv'') = staticEval env eidToStaticExpEnv' funcExp in
      let (evaledArgExpMaybes, eidToStaticExpEnv''')   = recurseAll env eidToStaticExpEnv'' argExps in
      case (funcExpEvaledMaybe, evaledArgExpMaybes |> Utils.projJusts) of
        (Just funcExpEvaled, Just evaledArgExps) ->
          case (funcExpEvaled.val.e__, Dict.get funcExpEvaled.val.eid eidToStaticExpEnv'') of
            (EFun _ pats body _, Just staticExpEnvAtFuncDefinition) ->
              case Utils.maybeZip pats evaledArgExps of
                Nothing ->
                  retDictNothing eidToStaticExpEnv'''

                Just pairs ->
                  let patEnvMaybes = pairs |> List.map (\(pat, arg) -> maybeMatchExp pat arg) in
                  case patEnvMaybes |> Utils.projJust of
                    Nothing ->
                      retDictNothing eidToStaticExpEnv'''

                    Just patEnvs ->
                      let patStaticExpEnv = List.concatMap expEnvToStaticExpEnv (List.reverse patEnvs) in -- reverse hardly matters, shouldn't have duplicate names in patterns
                      -- Discard returned staticExpEnv dict because the body is not
                      -- being evaluated in a static environment.
                      let (maybeApplied, _, _) = staticEval (patStaticExpEnv ++ staticExpEnvAtFuncDefinition) Dict.empty body in
                      retDict eidToStaticExpEnv''' maybeApplied

            _ ->
              retDictNothing eidToStaticExpEnv'''

        _ ->
          retDictNothing eidToStaticExpEnv'''

    ELet _ _ False pat assign body _ ->
      let (assignExpEvaledMaybe, _, eidToStaticExpEnv'') = staticEval env eidToStaticExpEnv' assign in
      let patStaticExpEnv =
        case assignExpEvaledMaybe of
          Nothing -> patToDeadStaticExpEnv pat
          Just assignExpEvaled ->
            case maybeMatchExp pat arg ->
              Nothing  -> patToDeadStaticExpEnv pat
              Just env -> expEnvToStaticExpEnv env
      in
      staticEval (patStaticExpEnv ++ env) eidToStaticExpEnv'' body

    ELet _ _ True pat assign body _ ->
      -- We don't handle recursion.
      let (_, _, eidToStaticExpEnv'') = staticEval env eidToStaticExpEnv' assign in
      staticEval ((patToDeadStaticExpEnv pat) ++ env) eidToStaticExpEnv'' body

    EComment _ _ body       -> staticEval env eidToStaticExpEnv' body
    EOption _ _ _ _ body    -> staticEval env eidToStaticExpEnv' body
    ETyp _ _ _ body _       -> staticEval env eidToStaticExpEnv' body
    EColonType _ body _ _ _ -> staticEval env eidToStaticExpEnv' body
    ETypeAlias _ _ _ body _ -> staticEval env eidToStaticExpEnv' body


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


-- Which vars in this exp refer to something outside this exp?
freeIdentifiers : Exp -> Set.Set Ident
freeIdentifiers exp =
  freeIdentifiers_ Set.empty exp


freeIdentifiers_ : Set.Set Ident -> Exp -> Set.Set Ident
freeIdentifiers_ boundIdentsSet exp =
  let recurse () =
    List.map (freeIdentifiers_ boundIdentsSet) (childExps exp)
    |> Utils.unionAll
  in
  case exp.val.e__ of
    EConst _ i l wd             -> Set.empty
    EBase _ v                   -> Set.empty
    EVar _ x                    -> if Set.member x boundIdentsSet then Set.empty else Set.singleton x
    EFun _ ps e _               -> freeIdentifiers_ (Set.union (identifiersSetInPats ps) boundIdentsSet) e
    EOp _ op es _               -> recurse ()
    EList _ es _ m _            -> recurse ()
    EIndList _ rs _             -> recurse ()
    EIf _ e1 e2 e3 _            -> recurse ()
    ECase _ e1 bs _             ->
      let freeInScrutinee = freeIdentifiers_ boundIdentsSet e1 in
      let freeInEachBranch =
        (List.map .val bs)
        |> List.map (\(Branch_ _ bPat bExp _) -> freeIdentifiers_ (Set.union (identifiersSetInPat bPat) boundIdentsSet) bExp)
      in
      List.foldl
          Set.union
          freeInScrutinee
          freeInEachBranch

    ETypeCase _ pat tbranches _ -> recurse ()
    EApp _ e1 es _              -> recurse ()
    ELet _ _ False p e1 e2 _    ->
      let freeInAssigns = freeIdentifiers_ boundIdentsSet e1 in
      let freeInBody    = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
      Set.union freeInAssigns freeInBody

    ELet _ _ True p e1 e2 _ ->
      let freeInAssigns = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e1 in
      let freeInBody    = freeIdentifiers_ (Set.union (identifiersSetInPat p) boundIdentsSet) e2 in
      Set.union freeInAssigns freeInBody

    EComment _ _ e1       -> recurse ()
    EOption _ _ _ _ e1    -> recurse ()
    ETyp _ _ _ e1 _       -> recurse ()
    EColonType _ e1 _ _ _ -> recurse ()
    ETypeAlias _ _ _ e1 _ -> recurse ()
    EVal _                -> Debug.crash "Brainstorm.freeIdentifiers_: shouldn't have an EVal in given expression"


-- Which EIds do we need in scope were we to lift this expression?
-- This is where bound variables fail the transformation.
varEIds : Exp -> EId -> List EId
varEIds program mobileEId =
  let mobileExp = justFindExpByEId program mobileEId in
  let freeIdents = freeIdentifiers mobileExp |> Set.toList in
  let visibleVars = visibleVarsAt program mobileEId in
  freeIdents
  |> Utils.foldlMaybe
      (\freeIdent freeEIds ->
        case Utils.maybeFind freeIdent visibleVars of
          Just (Just eid) -> eid::freeEIds
          _               -> Nothing -- not found in static env, or bound
      )
      Just []


-- Replace the given EIds with given variable names
turnExpsToVars : Exp -> Dict.Dict EId Ident -> Exp
turnExpsToVars exp renamings =
  let varRenamer e =
    case Dict.get e.val.eid renamings of
      Nothing ->
        e

      Just newIdent ->
        let ws = LangUnparser.precedingWhitespace e in
        replaceE__ e (Var ws newIdent)
  in
  mapExp varRenamer exp


-- Performs renamings in program, but returns renaming on mobileEId
-- so caller can use it on "off-program" expressions. TODO: revisit, may not need renamings
liftSoVisbleTo originalProgram mobileEId observerEId =
  case Utils.maybeFind mobileEId (visibleVarsAt originalProgram observerEId |> List.map flipTuple) of
    Just (_, var) ->
      let renamings = Dict.singleton mobileEId var in
      Just (originalProgram, renamings)

    Nothing       ->
      let deepestCommonAncestorEId =
        let deepestCommonAncestor =
          let ancestorsAndMobileEId   = findWithAncestors originalProgram mobileEId in
          let ancestorsAndObserverEId = findWithAncestors originalProgram observerEId in
          let commonAncestors = Utils.commonPrefix ancestorsAndMobileEId ancestorsAndObserverEId in
          let errorMsg = "couldn't find common ancestor of " ++ (toString mobileEId) ++ " and " ++ (toString observerEId) in
          Utils.tail errorMsg commonAncestors
        in
        deepestCommonAncestor.val.eid
      in
      let deepestCommonAncestorEId = deepestCommonAncestor.val.eid in
      let maybeDependenciesLifted =
        case varEIds originalProgram mobileEId of
          Nothing ->
            Nothing -- need to lift a bound var. TODO: allow lifting if we don't exceed its scope

          Just eidsToLift ->
            eidsToLift
            |> List.foldlMaybe
                (\eidToLift program ->
                  liftSoVisbleTo originalProgram eidToLift deepestCommonAncestorEId
                  |> Maybe.map (\(newProgram, renamings) -> newProgram)
                )
                Just originalProgram
      in
      maybeDependenciesLifted
      |> Maybe.map
          (\dependenciesLifted ->
            -- TODO: better names
            let liftedName = nonCollidingName "lifted" (identifiersSet dependenciesLifted) in
            let deepestCommonAncestor = justFindExpByEId dependenciesLifted deepestCommonAncestorEId in
            let expToLift             = justFindExpByEId deepestCommonAncestor mobileEId in
            let renamings             = Set.singleton mobileEId liftedName in
            let deepestCommonAncestorTargetExpRenamed = turnExpsToVars deepestCommonAncestor renamings in
            -- c.f. ValueBasedTransform.wrapWithLets for possible code deduplication
            -- we need to preserver EIds here
            -- TODO: pretty whitespace
            let liftedSubtree = eLets [(liftedName, expToLift)] deepestCommonAncestorTargetExpRenamed in
            let lifted = applyESubst (Dict.singleton deepestCommonAncestorEId liftedSubtree) dependenciesLifted in
            (lifted, Set.singleton mobileEId liftedName)
          )


-- Recurse down to EId's in program/prelude
-- (eid's not in program/prelude are inserted expressions)
neededEIds programEnv exp =
  let recurse = neededEIds programEnv in
  let eid = exp.val.eid in
  if eid >= 0 then
    [eid]
  else
    case exp.val.e__ of
      EVar _ x ->
        case Utils.maybeFind x programEnv of
          Just val -> [val.val.vtrace |> Utils.head ("expected var " ++ x ++ " to resolve to val with non-empty vtrace")]
          Nothing  -> Debug.crash <| "independent side has reference to unknown var " ++ x

      _ ->
        List.concatMap recurse (childExps exp)


redefineExp programEnv originalProgram indep depEId =
  let maybeLiftedAndRenamings =
    (neededEIds programEnv indep)
    |> Utils.foldlMaybe
        (\eid (program, renamings) ->
          liftSoVisbleTo program eid depEId
          |> Maybe.map (\(newProgram, newestRenamings) -> (newProgram, Dict.union newestRenamings renamings))
        )
        Just (originalProgram, Dict.empty)
  in
  maybeLiftedAndRenamings
  |> Maybe.map
      (\(lifted, renamings) ->
        let dependent = turnExpsToVars indep renamings in
        applyESubst (Dict.singleton depEId dependent) lifted
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

analyzeDependence : Exp -> DependencyAnalysis
analyzeDependence program =
  let (_, dependencyAnalysis, _) =
    analyzeDependence_ [] Dict.empty program
  in
  dependencyAnalysis

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
--           (List.map (\depEId -> (Dict.justGet_ "Brainstorm.analyzeDependence EList" varEId depAn).dependentOnClosure) depEIds)
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
--             let varDep = Dict.justGet_ errorMsg varEId depAn in
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
--         let bodyExpDep = Dict.justGet_ "Brainstorm.analyzeDependence fun" bodyEId depAn'' in
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
--               (List.map (\depEId -> (Dict.justGet_ "Brainstorm.analyzeDependence op" varEId depAn).dependentOnClosure) depEIds)
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
  let dependencyAnalysis = analyzeDependence originalProgram in
  let solutions = solutionsForDependentProgramLocation dependencyAnalysis originalProgram constraint in
  solutions
  |> Utils.mapFirstSuccess
      (\(indepCTerm, eid) ->
        redefineExp programEnv originalProgram indepCTerm eid)
      )


relate originalProgram idea zoneToRelate slideNumber movieNumber movieTime =
  let (shapeId, zoneName) = zoneToRelate in
  case Eval.eval Eval.initEnv [] originalProgram of
    Err s -> let _ = Debug.log ("Brainstorm.relate eval failed: " ++ s) () in originalProgram
    Ok ((val, _), programEnv) ->
      let slateRes =
        LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
      in
      case slateRes of
        Err s -> let _ = Debug.log ("Brainstorm.relate resolveToIndexedTree failed: " ++ s) () in originalProgram
        Ok slate ->
          let (_,tree) = slate in
          case Dict.get shapeId tree of
            Nothing -> originalProgram
            Just shape ->
              let shapeVal = svgNodeToVal tree shape in
              let maybeZoneIdea = shapeZoneToIdea programEnv shape shapeVal zoneName in
              case maybeZoneIdea of
                Nothing       -> originalProgram
                Just zoneIdea ->
                  let constraint =
                     ideaToMaybeConstraint programEnv (zoneNameToConstraintType zoneName) idea zoneIdea
                  in
                  case maybeMakeEqualConstraint programEnv originalProgram constraint of
                    Nothing         -> originalProgram
                    Just newProgram -> newProgram
