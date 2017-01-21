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

type alias ScopeGraph =
  { scopeNodes   : Dict ScopeId (LetOrFun, Set PatternId)
  , idents       : Dict PatternId Ident
  , tree         : Dict ScopeId ScopeId
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

foldPatternsWithIds
  : (PatternId -> Ident -> a -> a) -> ScopeId -> List Pat -> a -> a
foldPatternsWithIds f scopeId pats init =
  let
    doOne f patId pat acc =
      case pat.val of
        PConst _ _  -> acc
        PBase _ _   -> acc
        PVar _ x _  -> f patId x acc
        PAs _ x _ p -> f patId x (doOne f patId p acc)
        PList _ ps _ Nothing _  -> doMany f patId ps acc
        PList _ ps _ (Just p) _ -> doMany f patId (ps ++ [p]) acc

    doMany f (scopeId, path) pats acc =
      Utils.foldli (\(i, pi) -> doOne f (scopeId, path ++ [i]) pi) acc pats
  in
  case pats of
    [pat] -> doOne  f (scopeId, []) pat init     -- ELet case
    _     -> doMany f (scopeId, []) pats init    -- EFun case

addVarToEnv : PatternId -> Ident -> Env -> Env
addVarToEnv patId x env =
  let xBindings = Maybe.withDefault [] (Dict.get x env.varBindings) in
  { env
     | varBindings = Dict.insert x (patId :: xBindings) env.varBindings
     }

addVarNode : PatternId -> Ident -> ScopeGraph -> ScopeGraph
addVarNode patId x acc =
  let (scopeId, _) = patId in
  let (letOrFun, set) = Utils.justGet_ "addVarNode" scopeId acc.scopeNodes in
  { acc
     | scopeNodes = Dict.insert scopeId (letOrFun, Set.insert patId set) acc.scopeNodes
     , idents = Dict.insert patId x acc.idents
     }

addScopeNode newScopeId letOrFun acc =
  { acc
     | scopeNodes = Dict.insert newScopeId (letOrFun, Set.empty) acc.scopeNodes
     }

addScopeEdge newScopeId currentScopeId acc =
  { acc
     | tree = Dict.insert newScopeId currentScopeId acc.tree
     }

resolve x env =
  case Dict.get x env.varBindings of
    Just (patId :: _) -> Just patId
    _                 -> Nothing -- should only be for library funcs


------------------------------------------------------------------------------
-- Computing Scope Tree and Dependency Graph

type alias Env =
  { currentScope  : ScopeId
  , currentScopes : Set ScopeId
  , varBindings   : Dict Ident (List PatternId)
  }

rootId : ScopeId
rootId = 0

rootPatId : PatternId
rootPatId = (rootId, [])

initEnv : Env
initEnv =
  { currentScope = rootId
  , currentScopes = Set.empty
  , varBindings = Dict.empty
  }

compute : Exp -> ScopeGraph
compute e =
  computeScopeGraph initEnv e
    { scopeNodes = Dict.singleton rootId (True, Set.empty)
    , idents = Dict.empty
    , tree = Dict.empty
    }

computeScopeGraph : Env -> Exp -> ScopeGraph -> ScopeGraph
computeScopeGraph env exp acc =

  let recurse es = List.foldl (computeScopeGraph env) acc es in

  let newScopeId = exp.val.eid in -- only if e is ELet or EFun

  let updateEnv newScopeId pats env =
    foldPatternsWithIds addVarToEnv newScopeId pats <|
      { env | currentScope = newScopeId
            , currentScopes = Set.insert newScopeId env.currentScopes
            }
  in
  case exp.val.e__ of

    ELet _ _ _ p e1 e2 _ ->
      let acc1 =
        acc |> addScopeNode newScopeId True
            |> addScopeEdge newScopeId env.currentScope
            |> foldPatternsWithIds addVarNode newScopeId [p]
            |> computeScopeGraph env e1
      in
      let env1 = updateEnv newScopeId [p] env in
      computeScopeGraph env1 e2 acc1

    EFun _ ps eBody _ ->
      let acc1 =
        acc |> addScopeNode newScopeId False
            |> addScopeEdge newScopeId env.currentScope
            |> foldPatternsWithIds addVarNode newScopeId ps
      in
      let env1 = updateEnv newScopeId ps env in
      computeScopeGraph env1 eBody acc1

    -- NOTE: may want to put resolved PatternIds into AST (annotated EVars)
    --
    EVar _ x ->
      let resolvedId = resolve x env in
      -- let _ = Debug.log "resolve" (x, resolvedId) in
      acc

    EConst _ _ _ _  -> acc
    EBase _ _       -> acc

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
