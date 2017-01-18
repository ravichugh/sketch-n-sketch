port module DependenceGraph exposing
  ( PatternId(..), BeforeAfter(..), TargetPosition, ScopeGraph
  , compute, printHtml, render, receiveImage
  )

import Lang exposing (..)
import LangParser2 as Parser
import Utils

import String
import Set exposing (Set)
import Dict exposing (Dict)
import Regex exposing (regex, HowMany(..))
import Html exposing (Html)


------------------------------------------------------------------------------

type PatternId
  = LetId EId
  | ListIndex PatternId Int

type BeforeAfter = Before | After

type alias TargetPosition = (BeforeAfter, PatternId)

type alias Nodes = Dict EId Ident -- TODO change EId to PatternId

type alias ParentTree = Dict EId EId

type alias ScopeGraph =
  { letVars    : Nodes
  , lambdaVars : Nodes
  , tree       : ParentTree
  }

combine x y =
  { letVars    = Dict.union x.letVars y.letVars
  , lambdaVars = Dict.union x.lambdaVars y.lambdaVars
  , tree       = Dict.union x.tree y.tree
  }

combineMany scopeGraphs = case scopeGraphs of
  hd :: tl -> List.foldl combine hd tl
  []       -> Debug.crash "combineMany scopeGraphs: []"


------------------------------------------------------------------------------

rootId = 0

compute : Exp -> ScopeGraph
compute =
  computeScopeGraph rootId
    { letVars = Dict.singleton rootId "ROOT"
    , lambdaVars = Dict.empty
    , tree = Dict.empty
    }

computeScopeGraph currentId acc e =
  let recurse = computeScopeGraph currentId acc in
  let addVar x acc =
    { acc | letVars = Dict.insert e.val.eid x acc.letVars
          , tree = Dict.insert e.val.eid currentId acc.tree
          }
  in
  let traversePat p acc =
    case p.val of
      PVar _ x _               -> addVar x acc
      PList _ ps _ Nothing _   -> List.foldl traversePat acc ps
      PList _ ps _ (Just p) _  -> List.foldl traversePat acc (ps++[p])
      PAs _ x _ p              -> acc -- TODO
      _                        -> acc
  in
  case e.val.e__ of

    EConst _ _ _ _  -> acc
    EBase _ _       -> acc
    EVar _ _        -> acc

    -- TODO
    EFun _ ps e _ -> recurse e
    EApp _ e es _ -> recurse e
    EOp _ _ es _ -> acc
    EList _ es _ me _ -> acc
    EIf _ e1 e2 e3 _ -> acc
    ECase _ e branches _ -> acc
    ETypeCase _ _ tbranches _ -> acc

    ELet _ _ _ p e1 e2 _ ->
      let acc1 = recurse e1 in
      computeScopeGraph e.val.eid (traversePat p acc1) e2

    EComment _ _ e        -> recurse e
    EOption _ _ _ _ e     -> recurse e
    ETyp _ _ _ e _        -> recurse e
    EColonType _ e _ _ _  -> recurse e
    ETypeAlias _ _ _ e _  -> recurse e


------------------------------------------------------------------------------

-- Outgoing

port renderDotGraph : String -> Cmd msg

render : ScopeGraph -> Cmd msg
render = renderDotGraph << printString

-- Incoming

port receiveImage : (String -> msg) -> Sub msg


------------------------------------------------------------------------------

printHtml : ScopeGraph -> Html msg
printHtml = print (Html.div []) (List.intersperse (Html.br [] [])) Html.text

printString : ScopeGraph -> String
printString = print identity Utils.lines identity

print : (b -> c) -> (List a -> b) -> (String -> a) -> ScopeGraph -> c
print overall combine f scopeGraph =
  let letVarNodes = nodes f scopeGraph in
  let treeEdges = edges f scopeGraph in
  overall <|
    combine <|
      List.concat <|
        [ [f "digraph scopeGraph {"]
        , letVarNodes
        , treeEdges
        , [f "}"]
        ]

nodes f scopeGraph =
  flip List.map (Dict.toList scopeGraph.letVars) <| \(i,x) ->
    f (x ++ " [label=\"" ++ x ++ "\", style=filled, fillcolor=gray]")

edges f scopeGraph =
  flip List.map (Dict.toList scopeGraph.tree) <| \(i,j) ->
    let var i = Maybe.withDefault "???" <| Dict.get i scopeGraph.letVars in
    f (var i ++ " -> " ++ var j)
