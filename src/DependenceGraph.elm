port module DependenceGraph exposing
  ( ScopeGraph, BeforeAfter(..), TargetPosition
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

type alias ScopeId = EId   -- from an ELet or EFun expression
type alias LetOrFun = Bool -- True for ELet, False for EFun

type alias PatternId = (ScopeId, List Int)

type alias Nodes = Dict PatternId Ident

type alias ParentTree = Dict ScopeId ScopeId

type alias ScopeGraph =
  { scopeNodes : Dict ScopeId (LetOrFun, Set PatternId)
  , idents     : Nodes
  , tree       : ParentTree
  }

type BeforeAfter = Before | After

type alias TargetPosition = (BeforeAfter, PatternId)


------------------------------------------------------------------------------
-- Ports

-- Outgoing

port renderDotGraph : String -> Cmd msg

render : ScopeGraph -> Cmd msg
render = renderDotGraph << printString

-- Incoming

port receiveImage : (String -> msg) -> Sub msg


------------------------------------------------------------------------------
-- Helpers

strPatId (scopeId, path) =
  String.join "_" (List.map toString (scopeId :: path))

updateScopeNodes patId scopeNodes =
  let (scopeId, _) = patId in
  let (letOrFun, set) = Utils.justGet_ "updateScopeNodes" scopeId scopeNodes in
  Dict.insert scopeId (letOrFun, Set.insert patId set) scopeNodes


------------------------------------------------------------------------------
-- Computing Scope Tree and Dependency Graph

rootId = 0
rootPatId = (rootId, [])

compute : Exp -> ScopeGraph
compute =
  computeScopeGraph rootId
    { scopeNodes = Dict.singleton rootId (True, Set.empty)
    , idents = Dict.empty
    , tree = Dict.empty
    }

computeScopeGraph currentScopeId acc e =

  let recurse es =
    List.foldl (\e acc -> computeScopeGraph currentScopeId acc e) acc es in

  let newScopeId = e.val.eid in -- only if e is ELet or EFun

  let addScopeNode letOrFun acc =
    { acc
       | scopeNodes = Dict.insert newScopeId (letOrFun, Set.empty) acc.scopeNodes
       } in

  let addScopeEdge acc =
    { acc
       | tree = Dict.insert newScopeId currentScopeId acc.tree
       } in

  let addVar x path acc =
    let patId = (newScopeId, path) in
    { acc
       | scopeNodes = updateScopeNodes patId acc.scopeNodes
       , idents = Dict.insert patId x acc.idents
       } in

  let
    traversePat (pat, path) acc =
      case pat.val of
        PConst _ _  -> acc
        PBase _ _   -> acc
        PVar _ x _  -> addVar x path acc
        PAs _ x _ p -> traversePat (p, path) (addVar x path acc)
        PList _ ps _ Nothing _  -> traversePats ps path acc
        PList _ ps _ (Just p) _ -> traversePats (ps ++ [p]) path acc

    traversePats pats path acc =
      Utils.foldli (\(i, pi) -> traversePat (pi, path ++ [i])) acc pats
  in

  case e.val.e__ of

    ELet _ _ _ p e1 e2 _ ->
      let acc0 = traversePat (p, []) (addScopeEdge (addScopeNode True acc)) in
      let acc1 = computeScopeGraph currentScopeId acc0 e1 in
      computeScopeGraph newScopeId acc1 e2

    EFun _ ps eBody _ ->
      let acc0 = addScopeEdge (addScopeNode False acc) in
      let acc1 = traversePats ps [] acc0 in
      computeScopeGraph newScopeId acc1 eBody

    EConst _ _ _ _  -> acc
    EBase _ _       -> acc
    EVar _ _        -> acc

    EApp _ e es _     -> recurse (e::es)
    EOp _ _ es _      -> recurse es
    EIf _ e1 e2 e3 _  -> recurse [e1, e2, e3]

    EList _ es _ (Just eRest) _  -> recurse (es++[eRest])
    EList _ es _ Nothing _       -> recurse es

    ECase _ e branches _ ->
      let _ = Debug.log "TODO: scope tree ECase" () in acc

    ETypeCase _ _ tbranches _ ->
      let _ = Debug.log "TODO: scope tree ETypeCase" () in acc

    EComment _ _ e        -> recurse [e]
    EOption _ _ _ _ e     -> recurse [e]
    ETyp _ _ _ e _        -> recurse [e]
    EColonType _ e _ _ _  -> recurse [e]
    ETypeAlias _ _ _ e _  -> recurse [e]


------------------------------------------------------------------------------
-- DOT Conversion

printHtml : ScopeGraph -> Html msg
printHtml = print (Html.div []) (List.intersperse (Html.br [] [])) Html.text

printString : ScopeGraph -> String
printString = print identity Utils.lines identity

print : (b -> c) -> (List a -> b) -> (String -> a) -> ScopeGraph -> c
print overall combine f scopeGraph =
  let scopeClusters = clusters f scopeGraph in
  let treeEdges = edges f scopeGraph in
  overall <|
    combine <|
      List.concat <|
        [ [f "digraph scopeGraph {"]
        , scopeClusters
        , treeEdges
        , [f "}"]
        ]

defineNode color name label =
  name ++ " [label=\"" ++ label ++ "\", style=filled, fillcolor=" ++ color ++ "]"

-- http://www.graphviz.org/doc/info/colors.html

defineVarNode = defineNode "lemonchiffon1"

defineClusterNode letOrFun =
  case letOrFun of
    True  -> defineNode "white"
    False -> defineNode "lightblue1"

clusterNode i =
  "scope" ++ toString i

varNode patId =
  "var" ++ strPatId patId

clusters f scopeGraph =
  flip List.concatMap (Dict.toList scopeGraph.scopeNodes) <|
    \(scopeId, (letOrFun, patIds)) ->
      let s = toString scopeId in
      List.concat <|
        [ [f ("subgraph cluster" ++ s ++ " {")]
        , [f (defineClusterNode letOrFun (clusterNode scopeId) s)]
        , flip List.map (Set.toList patIds) <| \patId ->
            let x = Maybe.withDefault "???" (Dict.get patId scopeGraph.idents) in
            f (defineVarNode (varNode patId) x)
        , [f "}"]
        ]

edges f scopeGraph =
  flip List.map (Dict.toList scopeGraph.tree) <| \(i,j) ->
    f (clusterNode i ++ " -> " ++ clusterNode j)
