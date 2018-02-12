port module DependenceGraph exposing
  ( ScopeGraph, ScopeOrder(..), scopeOrder
  , parentScopeOf, childScopesOf
  , lookupIdent
  , compute, printHtml, render, receiveImage
  , checkVisible
  , patternTransitivelyDependsOnScope
  , usedOnTheWayDownTo
  )

import Lang exposing (..)
import Utils

import String
import Set exposing (Set)
import Dict exposing (Dict)
import Regex exposing (regex, HowMany(..))
import Html exposing (Html)


------------------------------------------------------------------------------

type alias LetOrFun = Bool -- True for ELet, False for EFun

type alias VarBindings = Dict Ident (List PathedPatternId)

type alias ScopeGraph =
  { scopeNodes   : Dict ScopeId (LetOrFun, Set PathedPatternId)
  , idents       : Dict PathedPatternId Ident

  , parents  : Dict ScopeId ScopeId
  , children : Dict ScopeId (Set ScopeId)
      -- pre-computing children

  , dependencies   : Dict PathedPatternId (Set PathedPatternId)
  , dependenciesPS : Dict PathedPatternId (Set ScopeId)
  , dependenciesSP : Dict ScopeId (Set PathedPatternId)
  , dependenciesSS : Dict ScopeId (Set ScopeId)
      -- pre-computing the last three (don't necessarily need the last...)

  , varBindingsBefore : Dict ScopeId VarBindings
  , varBindingsAfter  : Dict ScopeId VarBindings

  , usedVars     : Set PathedPatternId
      -- used temporarily while computing dependencies
  }


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

strPathedPatId : PathedPatternId -> String
strPathedPatId (scopeId, path) =
   String.join "_" (strScopeId scopeId :: List.map toString path)

foldPatternsWithIds
  : (PathedPatternId -> Ident -> a -> a) -> PathedPatternId -> List Pat -> a -> a
foldPatternsWithIds f (scopeId, path) pats init =
  let
    doOne f pathedPatId pat acc =
      case pat.val.p__ of
        PConst _ _  -> acc
        PBase _ _   -> acc
        PWildcard _ -> acc
        PVar _ x _  -> f pathedPatId x acc
        PAs _ x _ p -> f pathedPatId x (doOne f pathedPatId p acc)
        PList _ ps _ Nothing _  -> doMany f pathedPatId ps acc
        PList _ ps _ (Just p) _ -> doMany f pathedPatId (ps ++ [p]) acc
        PParens _ p _ -> doOne f pathedPatId p acc

    doMany f (scopeId, path) pats acc =
      Utils.foldli1 (\(i, pi) -> doOne f (scopeId, path ++ [i]) pi) acc pats
  in
  case pats of
    [pat] -> doOne  f (scopeId, path) pat init     -- ELet case
    _     -> doMany f (scopeId, path) pats init    -- EFun case

addVarToEnv : PathedPatternId -> Ident -> Env -> Env
addVarToEnv pathedPatId x env =
  let xBindings = Maybe.withDefault [] (Dict.get x env.varBindings) in
  { env
     | varBindings = Dict.insert x (pathedPatId :: xBindings) env.varBindings
     }

updateEnv newScopeId pats env =
  foldPatternsWithIds addVarToEnv (newScopeId, []) pats <|
    { env | currentScope = newScopeId
          , currentScopes = Set.insert newScopeId env.currentScopes
          }

addVarNode : PathedPatternId -> Ident -> ScopeGraph -> ScopeGraph
addVarNode pathedPatId x acc =
  let (scopeId, _) = pathedPatId in
  let (letOrFun, set) = Utils.justGet_ "addVarNode" scopeId acc.scopeNodes in
  { acc
     | scopeNodes = Dict.insert scopeId (letOrFun, Set.insert pathedPatId set) acc.scopeNodes
     , idents = Dict.insert pathedPatId x acc.idents
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
    Just (pathedPatId :: _) -> Just pathedPatId
    _                 -> Nothing -- should only be for library funcs

lookupIdent : PathedPatternId -> ScopeGraph -> Ident
lookupIdent pathedPatId scopeGraph =
  Maybe.withDefault "?" (Dict.get pathedPatId scopeGraph.idents)


------------------------------------------------------------------------------
-- Scope Comparison

type ScopeOrder
  = SameScope
  | ParentScope
  | ChildScope
  | NearestCommonAncestor ScopeId

pathToRoot : ScopeGraph -> ScopeId -> List ScopeId
pathToRoot sg scopeId =
  if scopeId == rootId then
    [rootId]
  else
    scopeId :: pathToRoot sg (parentScopeOf scopeId sg)

pathFromRoot : ScopeGraph -> ScopeId -> List ScopeId
pathFromRoot sg scopeId =
  List.reverse (pathToRoot sg scopeId)

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
rootId = (0, 1)

rootPathedPatId : PathedPatternId
rootPathedPatId = (rootId, [])

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

  let newScopeId = (exp.val.eid, 1) in -- only if e is ELet or EFun; ECase not handled yet

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

    -- NOTE: may want to put resolved PathedPatternIds into AST (annotated EVars).
    -- then it would be easier to break up analyses into multiple passes.
    --
    EVar _ x ->
      case resolve x env.varBindings of
        Nothing -> acc
        Just id -> { acc | usedVars = Set.insert id acc.usedVars }

    EConst _ _ _ _  -> acc
    EBase _ _       -> acc

    EApp _ e es _ _     -> recurse (e::es)
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
    EParens _ e _         -> recurse [e]
    EHole _ _             -> let _ = Utils.log "DependenceGraph.traverse: EHole in exp!!" in acc


traverseAndAddDependencies newScopeId =
  traverseAndAddDependencies_ (newScopeId, [])

traverseAndAddDependencies_ pathedPatId env pat exp acc =

  let addDependencies somePathedPatId acc =
    let used =
      Set.filter
         (\(scopeId,_) -> Set.member scopeId env.currentScopes)
         acc.usedVars in
    let update dict foo = Set.foldl foo dict used in

    { acc
       | dependencies =
           update acc.dependencies
             (\usedPat -> Utils.dictAddToSet somePathedPatId usedPat)
       , dependenciesPS =
           update acc.dependenciesPS
             (\usedPat -> Utils.dictAddToSet somePathedPatId (Tuple.first usedPat))
       , dependenciesSP =
           update acc.dependenciesSP
             (\usedPat -> Utils.dictAddToSet (Tuple.first somePathedPatId) usedPat)
       , dependenciesSS =
           update acc.dependenciesSS
             (\usedPat -> Utils.dictAddToSet (Tuple.first somePathedPatId) (Tuple.first usedPat))
       }
  in

  let addConservativeDependencies acc =
    foldPatternsWithIds
       (\innerPathedPatId _ acc -> addDependencies innerPathedPatId acc)
       pathedPatId [pat] acc
  in

  let clearUsed acc = { acc | usedVars = Set.empty } in

  case (pat.val.p__, exp.val.e__) of

    (PConst _ _, _) -> acc |> traverse env exp

    (PBase _ _, _) -> acc |> traverse env exp

    (PWildcard _, _) -> acc |> traverse env exp

    (PVar _ x _, _) ->
      acc |> clearUsed
          |> traverse env exp
          |> addDependencies pathedPatId

    (PAs _ x _ p, _) ->
      acc |> clearUsed
          |> traverse env exp
          |> addDependencies pathedPatId
          |> traverseAndAddDependencies_ pathedPatId env p exp
               -- NOTE: exp gets traversed twice...

    (PParens _ p _, _) ->
      acc |> clearUsed
          |> traverse env exp
          |> addDependencies pathedPatId
          |> traverseAndAddDependencies_ pathedPatId env p exp

    (PList _ ps_ _ pMaybe _, EList _ es_ _ eMaybe _) ->

      let ps = Utils.snocMaybe ps_ pMaybe in
      let es = Utils.snocMaybe es_ eMaybe in

      if List.length ps == List.length es then
        let (scopeId, basePath) = pathedPatId in
        Utils.foldli1 (\(i,(pi,ei)) acc ->
          let pathedPatId = (scopeId, basePath ++ [i]) in
          traverseAndAddDependencies_ pathedPatId env pi ei acc
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
  let nothing = [] in
  let just x = [x] in
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
          nothing
        else case xRest of
          [] -> nothing
          xNext :: _ ->
            case scopeOrder scopeGraph targetId (Tuple.first xNext) of
              ChildScope -> nothing
              ParentScope ->
                -- let _ = Debug.log "issue 1" (x, sourcePat, "captured by", xRest) in
                just (sourcePat, xNext)
              SameScope ->
                -- let _ = Debug.log "issue 1" (x, sourcePat, "captured by", xRest) in
                just (sourcePat, xNext)
              order ->
                -- let _ =  Debug.log "checkVisible 1 TODO" (targetId, xNext, order) in
                nothing
  in
  let capturedBy2 =
    case xBindingsBeforeTarget of
      [] -> nothing
      j::js ->
        if sourcePat == j
        then nothing
        else
          -- let _ = Debug.log "issue 2" (x, sourcePat, "captures another", j::js) in
          just (j, sourcePat)
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

patternTransitivelyDependsOnScope : ScopeGraph -> PathedPatternId -> ScopeId -> Bool
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
usedOnTheWayDownTo : ScopeGraph -> PathedPatternId -> ScopeId -> Bool -> Bool
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

strScopeId : ScopeId -> String
strScopeId (eid, branchi) =
  toString eid ++ "-" ++ toString branchi

clusterNode : ScopeId -> String
clusterNode scopeId =
  "scope" ++ strScopeId scopeId

varNode : PathedPatternId -> String
varNode pathedPatId =
  "var" ++ strPathedPatId pathedPatId

clusters : (String -> a) -> ScopeGraph -> List a
clusters f scopeGraph =
  flip List.concatMap (Dict.toList scopeGraph.scopeNodes) <|
    \(scopeId, (letOrFun, pathedPatIds)) ->
      let s = toString scopeId in
      List.concat <|
        [ [f ("subgraph cluster" ++ s ++ " {")]
        , [f (defineClusterNode letOrFun (clusterNode scopeId) s)]
        , flip List.map (Set.toList pathedPatIds) <| \pathedPatId ->
            let x = Maybe.withDefault "???" (Dict.get pathedPatId scopeGraph.idents) in
            f (defineVarNode (varNode pathedPatId) x)
        , [f "}"]
        ]

scopeEdges f scopeGraph =
  flip List.map (Dict.toList scopeGraph.parents) <| \(i,j) ->
    f (clusterNode i ++ " -> " ++ clusterNode j)

dependencyEdges f scopeGraph =
  flip List.concatMap (Dict.toList scopeGraph.dependencies) <| \(pSource, pTargets) ->
    flip List.map (Set.toList pTargets) <| \pTarget ->
      f (varNode pSource ++ " -> " ++ varNode pTarget)
