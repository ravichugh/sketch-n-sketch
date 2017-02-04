port module DependenceGraph exposing
  ( ScopeGraph, ScopeId, PatternId
  , BeforeAfter, PatTargetPosition, ExpTargetPosition
  , ScopeOrder(..), scopeOrder
  , parentScopeOf, childScopesOf
  , lookupIdent
  , compute, printHtml, render, receiveImage
  , checkVisible
  , patternTransitivelyDependsOnScope
  , usedOnTheWayDownTo
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

type alias VarBindings = Dict Ident (List PatternId)

type alias ScopeGraph =
  { scopeNodes   : Dict ScopeId (LetOrFun, Set PatternId)
  , idents       : Dict PatternId Ident

  , parents  : Dict ScopeId ScopeId
  , children : Dict ScopeId (Set ScopeId)
      -- pre-computing children

  , dependencies   : Dict PatternId (Set PatternId)
  , dependenciesPS : Dict PatternId (Set ScopeId)
  , dependenciesSP : Dict ScopeId (Set PatternId)
  , dependenciesSS : Dict ScopeId (Set ScopeId)
      -- pre-computing the last three (don't necessarily need the last...)

  , varBindingsBefore : Dict ScopeId VarBindings
  , varBindingsAfter  : Dict ScopeId VarBindings

  , usedVars     : Set PatternId
      -- used temporarily while computing dependencies
  }

type alias BeforeAfter = Int -- 0 for Before, 1 for After

type alias PatTargetPosition = (BeforeAfter, PatternId)

type alias ExpTargetPosition = (BeforeAfter, EId)


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

parentScopeOf : ScopeId -> ScopeGraph -> ScopeId
parentScopeOf i sg =
  Utils.justGet_ ("parentScopeOf " ++ toString i) i sg.parents

childScopesOf : ScopeId -> ScopeGraph -> Set ScopeId
childScopesOf i sg =
  Utils.dictGetSet i sg.children

strPatId (scopeId, path) =
  String.join "_" (List.map toString (scopeId :: path))

foldPatternsWithIds
  : (PatternId -> Ident -> a -> a) -> PatternId -> List Pat -> a -> a
foldPatternsWithIds f (scopeId, path) pats init =
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
    [pat] -> doOne  f (scopeId, path) pat init     -- ELet case
    _     -> doMany f (scopeId, path) pats init    -- EFun case

addVarToEnv : PatternId -> Ident -> Env -> Env
addVarToEnv patId x env =
  let xBindings = Maybe.withDefault [] (Dict.get x env.varBindings) in
  { env
     | varBindings = Dict.insert x (patId :: xBindings) env.varBindings
     }

updateEnv newScopeId pats env =
  foldPatternsWithIds addVarToEnv (newScopeId, []) pats <|
    { env | currentScope = newScopeId
          , currentScopes = Set.insert newScopeId env.currentScopes
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
     | parents = Dict.insert newScopeId currentScopeId acc.parents
     , children = Utils.dictAddToSet currentScopeId newScopeId acc.children
     }

addVarBindingsBefore scopeId varBindings acc =
  { acc
     | varBindingsBefore = Dict.insert scopeId varBindings acc.varBindingsBefore
     }

addVarBindingsAfter scopeId varBindings acc =
  { acc
     | varBindingsAfter = Dict.insert scopeId varBindings acc.varBindingsAfter
     }

resolve x varBindings =
  case Dict.get x varBindings of
    Just (patId :: _) -> Just patId
    _                 -> Nothing -- should only be for library funcs

lookupIdent : PatternId -> ScopeGraph -> Ident
lookupIdent patId scopeGraph =
  Maybe.withDefault "?" (Dict.get patId scopeGraph.idents)


------------------------------------------------------------------------------
-- Scope Comparison

type ScopeOrder
  = SameScope
  | ParentScope
  | ChildScope
  | NearestCommonAncestor ScopeId

pathToRoot : ScopeGraph -> ScopeId -> List ScopeId
pathToRoot sg i =
  if i == rootId then
    [rootId]
  else
    i :: pathToRoot sg (parentScopeOf i sg)

pathFromRoot : ScopeGraph -> ScopeId -> List ScopeId
pathFromRoot sg i =
  List.reverse (pathToRoot sg i)

scopeOrder : ScopeGraph -> ScopeId -> ScopeId -> ScopeOrder
scopeOrder sg i j =
  let walk lastCommonAncestor path1 path2 =
    -- let _ = Debug.log (toString ("walk", lastCommonAncestor, path1, path2)) () in
    case (path1, path2) of
      (head1 :: tail1, head2 :: tail2) ->
        if head1 == head2 then walk head1 tail1 tail2
        else if lastCommonAncestor == i then ParentScope
        else if lastCommonAncestor == j then ChildScope
        else NearestCommonAncestor lastCommonAncestor
      (head1 :: _, []) ->
        if lastCommonAncestor == j then ChildScope
        else NearestCommonAncestor lastCommonAncestor
      ([], head2 :: _) ->
        if lastCommonAncestor == i then ParentScope
        else NearestCommonAncestor lastCommonAncestor
      ([], []) ->
        Debug.log "WARN: SameScope should've been detected already" SameScope
  in
  if i == j then SameScope
  else
    let
      iPath = pathFromRoot sg i |> List.drop 1 -- drop rootId
      jPath = pathFromRoot sg j |> List.drop 1 -- drop rootId
    in
    walk rootId iPath jPath

{-

-- "LT" means "below" (or "later") in terms of linear structure of code
--
compareScope : ScopeGraph -> ScopeId -> ScopeId -> Order
compareScope sg i j =
  case scopeOrder sg i j of
    ChildScope -> LT
    NearestCommonAncestor _ -> LT
    SameScope -> EQ
    ParentScope -> GT

-}


------------------------------------------------------------------------------
-- Computing Scope Tree and Dependency Graph

type alias Env =
  { currentScope  : ScopeId
  , currentScopes : Set ScopeId
  , varBindings   : VarBindings
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
  traverse initEnv e
    { scopeNodes = Dict.singleton rootId (True, Set.empty)
    , idents = Dict.empty
    , parents = Dict.empty
    , children = Dict.empty
    , dependencies = Dict.empty
    , dependenciesPS = Dict.empty
    , dependenciesSP = Dict.empty
    , dependenciesSS = Dict.empty
    , varBindingsBefore = Dict.empty
    , varBindingsAfter = Dict.empty
    , usedVars = Set.empty
    }

traverse : Env -> Exp -> ScopeGraph -> ScopeGraph
traverse env exp acc =

  let recurse es = List.foldl (traverse env) acc es in

  let newScopeId = exp.val.eid in -- only if e is ELet or EFun

  case exp.val.e__ of

    ELet _ _ _ p e1 e2 _ ->
      let env1 = updateEnv newScopeId [p] env in
      acc
        |> addScopeNode newScopeId True
        |> addScopeEdge newScopeId env.currentScope
        |> foldPatternsWithIds addVarNode (newScopeId, []) [p]
        |> traverseAndAddDependencies newScopeId env p e1
        |> addVarBindingsBefore newScopeId env.varBindings
        |> addVarBindingsAfter newScopeId env1.varBindings
        |> traverse env1 e2

    EFun _ ps eBody _ ->
      let env1 = updateEnv newScopeId ps env in
      acc
        |> addScopeNode newScopeId False
        |> addScopeEdge newScopeId env.currentScope
        |> foldPatternsWithIds addVarNode (newScopeId, []) ps
        |> addVarBindingsBefore newScopeId env.varBindings
        |> addVarBindingsAfter newScopeId env1.varBindings
        |> traverse env1 eBody

    -- NOTE: may want to put resolved PatternIds into AST (annotated EVars).
    -- then it would be easier to break up analyses into multiple passes.
    --
    EVar _ x ->
      case resolve x env.varBindings of
        Nothing -> acc
        Just id -> { acc | usedVars = Set.insert id acc.usedVars }

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


traverseAndAddDependencies newScopeId =
  traverseAndAddDependencies_ (newScopeId, [])

traverseAndAddDependencies_ patId env pat exp acc =

  let addDependencies somePatId acc =
    let used =
      Set.filter
         (\(scopeId,_) -> Set.member scopeId env.currentScopes)
         acc.usedVars in
    let update dict foo = Set.foldl foo dict used in

    { acc
       | dependencies =
           update acc.dependencies
             (\usedPat -> Utils.dictAddToSet somePatId usedPat)
       , dependenciesPS =
           update acc.dependenciesPS
             (\usedPat -> Utils.dictAddToSet somePatId (Tuple.first usedPat))
       , dependenciesSP =
           update acc.dependenciesSP
             (\usedPat -> Utils.dictAddToSet (Tuple.first somePatId) usedPat)
       , dependenciesSS =
           update acc.dependenciesSS
             (\usedPat -> Utils.dictAddToSet (Tuple.first somePatId) (Tuple.first usedPat))
       }
  in

  let addConservativeDependencies acc =
    foldPatternsWithIds
       (\innerPatId _ acc -> addDependencies innerPatId acc)
       patId [pat] acc
  in

  let clearUsed acc = { acc | usedVars = Set.empty } in

  case (pat.val, exp.val.e__) of

    (PConst _ _, _) -> acc |> traverse env exp

    (PBase _ _, _) -> acc |> traverse env exp

    (PVar _ x _, _) ->
      acc |> clearUsed
          |> traverse env exp
          |> addDependencies patId

    (PAs _ x _ p, _) ->
      acc |> clearUsed
          |> traverse env exp
          |> addDependencies patId
          |> traverseAndAddDependencies_ patId env p exp
               -- NOTE: exp gets traversed twice...

    (PList _ ps_ _ pMaybe _, EList _ es_ _ eMaybe _) ->

      let ps = Utils.snocMaybe ps_ pMaybe in
      let es = Utils.snocMaybe es_ eMaybe in

      if List.length ps == List.length es then
        let (scopeId, basePath) = patId in
        Utils.foldli (\(i,(pi,ei)) acc ->
          let patId = (scopeId, basePath ++ [i]) in
          traverseAndAddDependencies_ patId env pi ei acc
        ) acc (Utils.zip ps es)

      -- could be more precise here by traversing as many pairwise
      -- patterns and expressions as possible
      else
        acc |> clearUsed
            |> traverse env exp
            |> addConservativeDependencies

    (PList _ _ _ _ _, _) ->
      acc |> clearUsed
          |> traverse env exp
          |> addConservativeDependencies


------------------------------------------------------------------------------
-- Scope Dependencies

-- TODO look for capture, in addition to shadowing
-- TODO handle case when sourcePat is above targetId
-- TODO handle order in PLists

-- TODO combine scope and transitive dependencies in one place here,
--   and expose single safety check to CodeMotion. then scopeOrder
--   can even remain internal.

checkVisible scopeGraph x sourcePat targetId =
  let (sourceId, _) = sourcePat in
  let xBindingsAfterSource =
    scopeGraph.varBindingsAfter
      |> Dict.get sourceId
      |> Maybe.withDefault Dict.empty
      |> Dict.get x
      |> Maybe.withDefault []
  in
  let xBindingsBeforeTarget =
    scopeGraph.varBindingsBefore
      |> Dict.get targetId
      |> Maybe.withDefault Dict.empty
      |> Dict.get x
      |> Maybe.withDefault []
  in
  let capturedBy1 =
    case xBindingsAfterSource of
      [] -> Debug.crash "blah"
      xHead :: xRest ->
        if xHead /= sourcePat then
          -- let _ = Debug.log "blah 2" (x, sourceId, xHead) in
          Utils.nothing
        else case xRest of
          [] -> Utils.nothing
          xNext :: _ ->
            case scopeOrder scopeGraph targetId (Tuple.first xNext) of
              ChildScope -> Utils.nothing
              ParentScope ->
                -- let _ = Debug.log "issue 1" (x, sourcePat, "captured by", xRest) in
                Utils.just (sourcePat, xNext)
              SameScope ->
                -- let _ = Debug.log "issue 1" (x, sourcePat, "captured by", xRest) in
                Utils.just (sourcePat, xNext)
              order ->
                -- let _ =  Debug.log "checkVisible 1 TODO" (targetId, xNext, order) in
                Utils.nothing
  in
  let capturedBy2 =
    case xBindingsBeforeTarget of
      [] -> Utils.nothing
      j::js ->
        if sourcePat == j
        then Utils.nothing
        else
          -- let _ = Debug.log "issue 2" (x, sourcePat, "captures another", j::js) in
          Utils.just (j, sourcePat)
  in
  let scopeIssues = capturedBy1 ++ capturedBy2 in
  if scopeIssues == [] then ""
  else
    -- let _ = Debug.log "scopeIssues" scopeIssues in
    "[WARN shadowing] "


------------------------------------------------------------------------------
-- Transitive Dependencies

-- the following x_transitivelyDependsOn_y function assumes
-- scopeOrder sourceScope targetScope == ChildScope

patternTransitivelyDependsOnScope : ScopeGraph -> PatternId -> ScopeId -> Bool
patternTransitivelyDependsOnScope scopeGraph sourcePat targetScope =
  let directDependency =
     Set.member
       targetScope
       (Utils.dictGetSet sourcePat scopeGraph.dependenciesPS)
  in
  let transitiveDependency () =
     List.any
       (\pat -> patternTransitivelyDependsOnScope scopeGraph pat targetScope)
       (Set.toList (Utils.dictGetSet sourcePat scopeGraph.dependencies))
  in
  directDependency || transitiveDependency ()

-- this checks that sourcePat is used somewhere between its
-- definition and targetScope. this also requires going down
-- any branches in the scope tree that occur above targetScope.
--
-- assumes that scopeOrder sourceScope targetScope == ParentScope
--
usedOnTheWayDownTo : ScopeGraph -> PatternId -> EId -> Bool -> Bool
usedOnTheWayDownTo scopeGraph sourcePat targetScope includingTarget =
  let traverseDown currentScope =
    let usedHere () =
       Set.member
         sourcePat
         (Utils.dictGetSet currentScope scopeGraph.dependenciesSP)
    in

    if currentScope == targetScope && not includingTarget then
      False

    else if currentScope == targetScope {- && includingTarget -} then
      usedHere ()

    else if {- currentScope /= targetScope && -} usedHere () then
      True -- not recursing into children, since already used here

    else {- currentScope /= targetScope && not (usedHere ()) -}
      let children = Set.toList (childScopesOf currentScope scopeGraph) in
      List.any traverseDown children
  in
  let sourceScope = Tuple.first sourcePat in
  traverseDown sourceScope


------------------------------------------------------------------------------
-- DOT Conversion

printHtml : ScopeGraph -> Html msg
printHtml = print (Html.div []) (List.intersperse (Html.br [] [])) Html.text

printString : ScopeGraph -> String
printString = print identity Utils.lines identity

print : (b -> c) -> (List a -> b) -> (String -> a) -> ScopeGraph -> c
print overall combine f scopeGraph =
  let nodes = clusters f scopeGraph in
  let edges1 = scopeEdges f scopeGraph in
  let edges2 = dependencyEdges f scopeGraph in
  overall <|
    combine <|
      List.concat <|
        [ [f "digraph scopeGraph {"]
        , nodes
        , edges1
        , edges2
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

scopeEdges f scopeGraph =
  flip List.map (Dict.toList scopeGraph.parents) <| \(i,j) ->
    f (clusterNode i ++ " -> " ++ clusterNode j)

dependencyEdges f scopeGraph =
  flip List.concatMap (Dict.toList scopeGraph.dependencies) <| \(pSource, pTargets) ->
    flip List.map (Set.toList pTargets) <| \pTarget ->
      f (varNode pSource ++ " -> " ++ varNode pTarget)
