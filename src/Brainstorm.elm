module Brainstorm where

import Utils
import Lang exposing (..)
import LangTools exposing (..)
import LangSvg exposing (..)
import LangParser2
import Eval

-- Require ideas to be at least this far from other ideas
minInterIdeaDistance = 10

type alias Idea = (Point, Int) -- Int is which Brainstorm depth generated the point

brainstorm : List Idea -> Exp -> RootedIndexedTree -> List Idea
brainstorm previousIdeas inputExp slate =
  case previousIdeas of
    [] -> shapesToPoints inputExp slate
    _  -> pointsToMorePoints inputExp previousIdeas


programEnv inputExp =
  -- Wrapper ensures the program returns the top-level environment when the root
  -- expression of the program is a function (usually eval would return the
  -- environment inside that function)
  -- let wrappedProgram = eLets [("internalProgramWrapper", eBase ENull)] inputExp in
  let (_, env) = Utils.fromOk "Brainstorm.env" <| Eval.eval Eval.initEnv [] inputExp in
  -- let _ = Debug.log "env" (List.map fst env) in
  env


shapesToPoints : Exp -> RootedIndexedTree -> List Idea
shapesToPoints inputExp slate =
  let (_,tree) = slate in
  let shapes = shapesOnCanvas slate in
  -- let _ = Debug.log "shape count" <| List.length shapes in
  let shapeVals = shapes |> List.map (svgNodeToVal tree) in
  let shapeTypeNames = (shapeTypeNamesIn LangParser2.prelude) ++ (shapeTypeNamesIn inputExp) in
  let env = programEnv inputExp in
  (shapeToPointFunctionsIn shapeTypeNames LangParser2.prelude) ++ (shapeToPointFunctionsIn shapeTypeNames inputExp)
  |> List.concatMap
      (\(_, funcName) ->
        shapeVals
        |> List.concatMap (\shapeVal ->
          let funcCall = eApp (eVar0 funcName) [eVar "shapeToPointsShapeDummy"] in
          case Eval.eval (("shapeToPointsShapeDummy", shapeVal)::env) [] funcCall of
            Ok ((outVal,_),_) ->
              case unwrapVList outVal of
                Just [VConst (x,xTr), VConst (y,yTr)] -> if (x,y) /= (0,0) then let _ = Debug.log "func" funcName in [(((x,xTr),(y,yTr)), 1)] else []
                _ -> []
            Err s -> []
        )
      )


shapesOnCanvas : RootedIndexedTree -> List IndexedTreeNode
shapesOnCanvas (nodeId, tree) =
  case Utils.justGet_ "shapesOnCanvas get node" nodeId tree of
    TextNode _ -> []
    SvgNode shapeKind attrs childrenIds ->
      let childShapes =
        List.concatMap
            (\childId -> shapesOnCanvas (childId, tree))
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
  let (_,lastDepth) = Utils.head "Brainstorm.pointsToMorePoints: ideas shouldn't be empty here" ideas in
  let depth = lastDepth + 1 in
  let env = programEnv inputExp in
  let pointVals = List.map (LangSvg.pointToVal << fst) ideas in
  (pointToPointToPointFunctionsIn LangParser2.prelude) ++ (pointToPointToPointFunctionsIn inputExp)
  |> List.foldl
      (\funcName retIdeas ->
        -- let _ = Debug.log "trying func" funcName in
        let funcCall = eApp (eVar0 funcName) [eVar "pt1", eVar "pt2"] in
        pointVals |> List.foldl (\vPt1 retIdeas ->
          pointVals |> List.foldl (\vPt2 retIdeas ->
            case Eval.eval (("pt1", vPt1)::("pt2", vPt2)::env) [] funcCall of
              Ok ((outVal,_),_) ->
                case unwrapVList outVal of
                  Just [VConst (x,xTr), VConst (y,yTr)] ->
                    -- let _ = Debug.log "func" funcName in
                    if List.any (\(((xOther,_),(yOther,_)), _) -> (xOther-x)^2 + (yOther-y)^2 < minInterIdeaDistance^2) retIdeas
                    then retIdeas
                    else (((x,xTr),(y,yTr)), depth)::retIdeas
                  _ -> retIdeas
              Err s -> retIdeas
          ) retIdeas
        ) retIdeas
      ) ideas