module SlowTypeInference exposing (..)

import FastParser
import Lang exposing (..)
import LangTools
import Types
import Utils

import Dict exposing (Dict)
import Set exposing (Set)


type Constraint
  = EIdIsType EId TypeConstraint
  | PIdIsType PId TypeConstraint
  | PIdIsEId PId EId
  | EIdVar EId Ident
  | PIdVar PId Ident
  | EIdIsEmpty EId String
  | PIdIsEmpty PId String
  | TypeAlias Ident TypeConstraint


-- TypeConstraint ---------------------------------------------------------

type TypeConstraint
  = TCEId EId
  | TCPId PId
  | TCApp TypeConstraint (List TypeConstraint)
  | TCNum
  | TCBool
  | TCString
  | TCNull
  | TCList TypeConstraint
  | TCTuple (List TypeConstraint) (Maybe TypeConstraint)
  | TCArrow (List TypeConstraint)
  | TCUnion (List TypeConstraint)
  | TCNamed Ident
  | TCVar Ident
  | TCForall (List Ident) TypeConstraint
  | TCWildcard


-- childTCs : TypeConstraint -> List TypeConstraint
-- childTCs tc =
--   case tc of
--     TCEId _             -> []
--     TCPId _             -> []
--     TCApp tc1 tcs       -> tc1::tcs
--     TCNum               -> []
--     TCBool              -> []
--     TCString            -> []
--     TCNull              -> []
--     TCList tc1          -> [tc1]
--     TCTuple tcs mtc     -> tcs ++ (mtc |> Maybe.map List.singleton |> Maybe.withDefault [])
--     TCArrow tcs         -> tcs
--     TCUnion tcs         -> tcs
--     TCNamed _           -> []
--     TCVar _             -> []
--     TCForall idents tc1 -> [tc1]
--     TCWildcard          -> []
--
--
-- flattenTCTree : TypeConstraint -> List TypeConstraint
-- flattenTCTree tc =
--   tc :: List.concatMap flattenTCTree (childTCs tc)


-- Bottom up
mapTC : (TypeConstraint -> TypeConstraint) -> TypeConstraint -> TypeConstraint
mapTC f tc =
  case tc of
    TCEId _             -> f tc
    TCPId _             -> f tc
    TCApp tc1 tcs       -> f <| TCApp (f tc1) (List.map f tcs)
    TCNum               -> f tc
    TCBool              -> f tc
    TCString            -> f tc
    TCNull              -> f tc
    TCList tc1          -> f <| TCList (f tc1)
    TCTuple tcs mtc     -> f <| TCTuple (List.map f tcs) (Maybe.map f mtc)
    TCArrow tcs         -> f <| TCArrow (List.map f tcs)
    TCUnion tcs         -> f <| TCUnion (List.map f tcs)
    TCNamed _           -> f tc
    TCVar _             -> f tc
    TCForall idents tc1 -> f <| TCForall idents (f tc1)
    TCWildcard          -> f tc


typeToTC : Type -> TypeConstraint
typeToTC tipe =
  case tipe.val of
    TNum _                           -> TCNum
    TBool _                          -> TCBool
    TString _                        -> TCString
    TNull _                          -> TCNull
    TList _ tipe _                   -> TCList (typeToTC tipe)
    TDict _ tipe1 tipe2 _            -> TCWildcard -- Dict will be removed shortly
    TTuple _ heads _ maybeRestType _ -> TCTuple (List.map typeToTC heads) (Maybe.map typeToTC maybeRestType)
    TArrow _ typeList _              -> TCArrow (List.map typeToTC typeList)
    TUnion _ typeList _              -> TCUnion (List.map typeToTC typeList)
    TNamed _ ident                   -> TCNamed ident
    TVar _ ident                     -> TCVar ident
    TWildcard _                      -> TCWildcard
    TForall _ typeVars tipe1 _       ->
      let idents =
        case typeVars of
          One (_, ident) -> [ident]
          Many _ inner _ -> inner |> List.map (\(_, ident) -> ident)
      in
      TCForall idents (typeToTC tipe1)


-- TypeConstraint2 ---------------------------------------------------------

-- Graph of Dict TC2Id (Set TC2)
-- (Set TC2) represents constraints to unify
-- Fixpoint when, after propagating TC2SameAs, each (Set TC2) is either
--   (a) all TC2SameAs
--   (b) a singleton

type alias TC2Id    = Int  -- PIds, EIds, and various shared types
type alias TC2Graph = Dict TC2Id (Set TC2)

type TC2
  = TC2SameAs TC2Id      -- Graph edge
  | TC2Empty String      -- Type error
  | TC2Unify TC2Id TC2Id (Just TC2) -- Deferred unification, with most recently computed unification (Nothing means not computed yet i.e. Unknown)
  | TC2UnifyOne TC2Id TC2 -- In-place constraint (can be narrowed)
  | TC2App TC2Id TC2Id
  | TC2Num
  | TC2Bool
  | TC2String
  | TC2Null
  -- | TC2List TC2Id
  | TC2Tuple (List TC2Id) (Maybe TC2Id)
  | TC2Arrow TC2Id TC2Id -- Binary applications
  | TC2Union (List TC2Id)
  -- Forall should introduce + resolve type vars to a new node
  -- | TC2Var Ident -- TC2Id -- TC2Id is context
  -- | TC2Forall (List Ident) TC2Id
  -- | TC2Wildcard


tc2IsASameAs : TC2 -> Bool
tc2IsASameAs tc2 =
  case tc2 of
    TC2SameAs _ -> True
    _           -> False

tc2IsEmpty : TC2 -> Bool
tc2IsEmpty tc2 =
  case tc2 of
    TC2Empty _ -> True
    _          -> False


tc2IsUnifyOrUnifyOne : TC2 -> Bool
tc2IsUnifyOrUnifyOne tc2 =
  case tc2 of
    TC2Unify _ _ _  -> True
    TC2UnifyOne _ _ -> True
    _               -> False


tc2ToMaybeSameAsId : TC2 -> Maybe TC2Id
tc2ToMaybeSameAsId tc2 =
  case tc2 of
    TC2SameAs tc2Id -> Just tc2Id
    _               -> Nothing


tc2IdToSameAsIds : TC2Id -> TC2Graph -> List TC2Id
tc2IdToSameAsIds tc2Id graph =
  case Dict.get tc2Id graph of
    Just tc2set -> Set.toList tc2set |> List.filterMap tc2IdToSameAsIds
    Nothing     -> []


addTC2ToGraph : TC2Id -> TC2 -> TC2Graph -> TC2Graph
addTC2ToGraph = Utils.dictAddToSet
-- addTC2ToGraph id tc2 graph =
--   graph
--   |> Dict.update id
--       (\maybeTC2Set ->
--         let tc2Set = maybeTC2Set |> Maybe.withDefault Set.empty in
--         Just (Set.insert tc2 tc2Set)
--       )

addIdsEdgeToGraph : TC2Id -> TC2Id -> TC2Graph -> TC2Graph
addIdsEdgeToGraph id1 id2 graph =
  if id1 == id2 then
    graph
  else
    graph
    |> addTC2ToGraph id1 (TC2SameAs id2)
    |> addTC2ToGraph id2 (TC2SameAs id1)


-- 1. Add transitively connected edges to the graph.
-- 2. For each such "component", put all non-SameAs constraints on a single node.
-- "component" converges to a strongly connected component.
propagateGraphConstraints : TC2Graph -> TC2Graph
propagateGraphConstraints graph =
  let
    equivalentIds : Set TC2Id -> List TC2Id -> TC2Graph -> Set TC2Id
    equivalentIds visited toVisit graph =
      case toVisit of
        []            -> visited
        id::remaining ->
          if Set.member id visited then
            equivalentIds visited remaining graph
          else
            let newVisited = Set.insert id visited in
            let newToVisit = tc2IdToSameAsIds id graph ++ remaining in
            equivalentIds newVisited newToVisit graph

    (_, newGraph) =
      graph
      |> Dict.foldl
          (id tc2set (visited, graph) ->
            if Set.member id visited then
              (visited, graph)
            else
              -- Gather a "component".
              -- After several iterations of propagateGraphConstraints this will converge to a connected component.
              let
                equivIdSet = equivalentIds Set.empty [id] graph
                equivIds   = Set.toList equivIdSet in
                -- Gather all the constraints to unify
                thisComponentConstraints =
                  equivIds
                  |> List.concatMap (\id -> Utils.getWithDefault id Set.empty graph |> Set.toList)
                  |> List.filter (not << tc2IsASameAs)
                  |> Set.fromList

                -- Point all nodes to this one
                tc2setPointToThisNode = Set.singleton (TC2SameAs id)
                newGraph = equivIds |> List.foldl (\id graph -> Dict.insert id tc2setPointToThisNode graph) graph

                -- Put all the constraints on this node
                -- As well as pointers to all same nodes
                tc2SameAsSet = equivIds |> List.map TC2SameAs |> Set.fromList in
                newGraph2 = Dict.insert id (Set.union thisComponentConstraints tc2SameAsSet) graph
              in
              (Set.union equivIdSet visited, newGraph2)
          )
          (Set.empty, graph)
  in
  newGraph


-- Only unifies a pair on a node at a time. Least likely to produce bugs with new nodes appearing.
unifyImmediatesStep : TC2Graph -> TC2Graph
unifyImmediatesStep graph =
  graph
  |> Dict.foldl
      (\id tc2set graph ->
        let (constraintsToUnify, otherConstraints) =
          Set.toList tc2set |> List.partition tc2IsImmediatelyUnifiable
        in
        case constraintsToUnify of
          tc2A::tc2B::rest ->
            let
              (unifiedConstraint, newGraphNodes) = unifyImmediate tc2A tc2B graph
              newTC2Set = (unifiedConstraint::rest) ++ otherConstraints |> Set.fromList
              newGraph  = Utils.insertAll ((id, newTC2Set) :: newGraphNodes) graph
            in
            newGraph

          _ ->
            graph
      )
      graph


-- Is this a constraint we can unify pair-wise?
tc2IsImmediatelyUnifiable : TC2 -> Bool
tc2IsImmediatelyUnifiable tc2 =
  case tc2 of
    TC2SameAs _     -> False
    TC2Empty _      -> False
    TC2App _ _      -> False
    TC2Unify _ _ _  -> False
    TC2UnifyOne _ _ -> False
    _               -> True


-- Unifications that will always result in one fewer TC2 on this node, at the cost of perhaps extra graph nodes.
-- Returns unified TC2 and any new graph nodes (deferred unification).
-- Assumes not given any: TC2SameAs, TC2Empty, TC2App, TC2Unify, or TC2UnifyOne
unifyImmediate : TC2 -> TC2 -> TC2Graph -> (TC2, List [(TC2Id, Set TC2)])
unifyImmediate tc2A tc2B graph =
  let typeMismatch () =
    (TC2Empty "Types don't match: " ++ toString tc2A ++ " vs. " ++ toString tc2B, [])
  in
  case tc2A tc2B of
    (TC2SameAs _, _)     -> Debug.crash "Shouldn't have TC2SameAs in unifyImmediate"
    (_, TC2SameAs _)     -> Debug.crash "Shouldn't have TC2SameAs in unifyImmediate"
    (TC2Empty _, _)      -> Debug.crash "Shouldn't have TC2Empty in unifyImmediate"
    (_, TC2Empty _)      -> Debug.crash "Shouldn't have TC2Empty in unifyImmediate"
    (TC2App _ _, _)      -> Debug.crash "Shouldn't have TC2App in unifyImmediate"
    (_, TC2App _ _)      -> Debug.crash "Shouldn't have TC2App in unifyImmediate"
    (TC2Unify _ _, _)    -> Debug.crash "Shouldn't have TC2Unify in unifyImmediate"
    (_, TC2Unify _ _)    -> Debug.crash "Shouldn't have TC2Unify in unifyImmediate"
    (TC2UnifyOne _ _, _) -> Debug.crash "Shouldn't have TC2UnifyOne in unifyImmediate"
    (_, TC2UnifyOne _ _) -> Debug.crash "Shouldn't have TC2UnifyOne in unifyImmediate"

    (TC2Union tc2AIds, tc2B) ->
      let
        currentId = 1 + (Dict.keys graph |> List.maximum |> Maybe.withDefault 0)
        unificationNodes =
          tc2AIds
          |> Utils.mapi0 (\(i, tc2AId)-> (currentId + i, Set.singleton <| TC2UnifyOne tc2AId tc2B))
        (newIds, _) = List.unzip unificationNodes
      in
      ( TC2Union newIds
      , unificationNodes
      )

    (_, TC2Union _) ->
      unifyImmediate tc2B tc2A graph

    (TC2Num,    TC2Num)    -> (TC2Num,    [])
    (TC2Bool,   TC2Bool)   -> (TC2Bool,   [])
    (TC2String, TC2String) -> (TC2String, [])
    (TC2Null,   TC2Null)   -> (TC2Null,   [])

    -- Explicit to let exhaustiveness checker help us ensure we didn't miss anything.
    (TC2Num,    _)         -> typeMismatch ()
    (TC2Bool,   _)         -> typeMismatch ()
    (TC2String, _)         -> typeMismatch ()
    (TC2Null,   _)         -> typeMismatch ()

    ( TC2Tuple tc2AIds Nothing
    , TC2Tuple tc2BIds Nothing ) ->
      case maybeZip tc2AIds tc2BIds of
        Just headsMatched ->
          let
            currentId = 1 + (Dict.keys graph |> List.maximum |> Maybe.withDefault 0)
            headUnificationNodes =
              headsMatched
              |> Utils.mapi0 (\(i, (tc2AId, tc2BId)) -> (currentId + i, Set.singleton <| TC2Unify tc2AId tc2BId Nothing))
            (headIds, _) = List.unzip headUnificationNodes
          in
          ( TC2Tuple headIds Nothing
          , headUnificationNodes
          )

        Nothing ->
          ( TC2Empty "Tuples differ in length"
          , []
          )

    ( TC2Tuple tc2AIds Nothing
    , TC2Tuple tc2BIds (Just tc2BTailId) ) ->
      case Utils.zipAndLeftovers tc2AIds tc2BIds of
        (_, _, _::_) ->
          ( TC2Empty "Tuple too short to match heads of list"
          , []
          )

        (headsMatched, leftoverAIds, _) ->
          let
            currentId = 1 + (Dict.keys graph |> List.maximum |> Maybe.withDefault 0)
            headUnificationNodes =
              headsMatched
              |> Utils.mapi0 (\(i, (tc2AId, tc2BId)) -> (currentId + i, Set.singleton <| TC2Unify tc2AId tc2BId Nothing))
            (headIds, _) = List.unzip headUnificationNodes
            newCurrentId = currentId + List.length headUnificationNodes
            moreHeadUnificationNodes =
              leftoverAIds
              |> Utils.mapi0 (\(i, tc2AId)-> (newCurrentId + i, Set.singleton <| TC2Unify tc2AId tc2BTailId Nothing))
            (moreHeadIds, _) = List.unzip moreHeadUnificationNodes
          in
          ( TC2Tuple (headIds ++ moreHeadIds) Nothing
          , headUnificationNodes ++ moreHeadUnificationNodes
          )

    ( TC2Tuple _ (Just _)
    , TC2Tuple _ Nothing ) ->
      unifyImmediate tc2B tc2A graph

    ( TC2Tuple tc2AIds (Just tc2ATailId)
    , TC2Tuple tc2BIds (Just tc2BTailId) ) ->
      let
        currentId = 1 + (Dict.keys graph |> List.maximum |> Maybe.withDefault 0)
        (headsMatched, leftoverAIds, leftoverBIds) = Utils.zipAndLeftovers tc2AIds tc2BIds
        headUnificationNodes =
          headsMatched
          |> Utils.mapi0 (\(i, (tc2AId, tc2BId)) -> (currentId + i, Set.singleton <| TC2Unify tc2AId tc2BId Nothing))
        (headIds, _)        = List.unzip headUnificationNodes
        tailId              = currentId + List.length headUnificationNodes
        tailUnificationNode = (tailId, Set.singleton <| TC2Unify tc2ATailId tc2BTailId Nothing)
        newCurrentId        = 1 + tailId
        moreHeadUnificationNodes =
          case (leftoverAIds, leftoverBIds) of
            (_, []) -> leftoverAIds |> Utils.mapi0 (\(i, tc2AId)-> (newCurrentId + i, Set.singleton <| TC2Unify tc2AId tc2BTailId Nothing))
            ([], _) -> leftoverBIds |> Utils.mapi0 (\(i, tc2BId)-> (newCurrentId + i, Set.singleton <| TC2Unify tc2BId tc2ATailId Nothing))
            _       -> Debug.crash "zipAndLeftovers violated its invariant that one leftovers list should be empty!"
        (moreHeadIds, _) = List.unzip moreHeadUnificationNodes
      in
      ( TC2Tuple (headIds ++ moreHeadIds) (Just tailId)
      , headUnificationNodes ++ moreHeadUnificationNodes ++ [tailUnificationNode]
      )

    (TC2Tuple _ _, _) -> typeMismatch ()

    ( TC2Arrow tc2ALeftId tc2ARightId
    , TC2Arrow tc2BLeftId tc2BRightId ) ->
      let
        currentId = 1 + (Dict.keys graph |> List.maximum |> Maybe.withDefault 0)
        leftUnificatioNode  = (currentId,     Set.singleton <| TC2Unify tc2ALeftId  tc2BLeftId Nothing)
        rightUnificatioNode = (currentId + 1, Set.singleton <| TC2Unify tc2ARightId tc2BRightId Nothing)
        ((leftId, _), (rightId, )) = (leftUnificatioNode, rightUnificatioNode)
      in
      ( TC2Arrow leftId rightId
      , [leftUnificatioNode, rightUnificatioNode]
      )

    (TC2Arrow _ _, _) -> typeMismatch ()


-- Take only one step at a node at a time.
--
-- Will only narrow a type at a node.
unifyAcrossNodesStep : TC2Graph -> TC2Graph
unifyAcrossNodesStep graph =
  graph
  |> Dict.foldl
      (\id tc2set graph ->
        let (constraintsToUnify, otherConstraints) =
          Set.toList tc2set |> List.partition tc2IsCrossNodeUnification
        in
        constraintsToUnify
        |> List.foldl
            (\tc2 graph ->
              let
                (newTC2s, graph2) = perhapsUnifyAcrossNodes id tc2 graph
                newTC2Set         = tc2set |> Set.remove tc2 |> Utils.insertAllIntoSet newTC2s
                newGraph          = Utils.insert id newTC2Set graph2
              in
              newGraph
            )
            graph

          _ ->
            tc2set
      )
      graph


tc2IsCrossNodeUnification : TC2 -> Bool
tc2IsCrossNodeUnification tc2 =
  case tc2 of
    TC2App _ _      -> True
    TC2Unify _ _ _  -> True
    TC2UnifyOne _ _ -> True
    _               -> False


-- tc2IsUnifiable : TC2 -> Bool
-- tc2IsUnifiable tc2 =
--   case tc2 of
--     TC2Empty _  -> False
--     TC2SameAs _ -> False
--     _           -> True


-- Gather all constraints on the graph reachable from id.
constraintsOnSubgraph : TC2Id -> TC2Graph -> List TC2
constraintsOnSubgraph id graph =
  constraintsOnSubgraph_ Set.empty [id] graph


constraintsOnSubgraph_ : Set TC2Id -> List TC2Id -> TC2Id -> TC2Graph -> List TC2
constraintsOnSubgraph_ visited toVisit id graph =
  case toVisit of
    []            -> []
    id::remaining ->
      if Set.member id visited then
        constraintsOnSubgraph_ visited remaining graph
      else
        case Dict.get id graph of
          Just tc2set ->
            let
              newVisited     = Set.insert id visited
              tc2list        = Set.toList tc2set
              newToVisit     = (tc2list |> List.filterMap tc2IdToSameAsIds) ++ remaining
              newConstraints = List.filter (not << tc2IsASameAs) tc2list
            in
            newConstraints ++ constraintsOnSubgraph_ newVisited newToVisit graph

          Nothing ->
            constraintsOnSubgraph_ (Set.insert id visited) remaining graph


-- Returns TC2s to replace this TC2 on the node, plus a possibly modified graph.
perhapsUnifyAcrossNodes : TC2Id -> TC2 -> TC2Graph -> (List TC2, TC2Graph)
perhapsUnifyAcrossNodes thisId tc2 graph =
  let noChange = ([tc2], graph) in
  case tc2 of
    TC2App leftId rightId -> noChange
    TC2Unify aId bId ->
      let aConstraints = constraintsOnSubgraph aId graph in
      let bConstraints = constraintsOnSubgraph bId graph in
      let allConstraints = aConstraints ++ bConstraints in
      if List.any tc2IsEmpty allConstraints then
        -- If empty, simply halt computation.
        noChange
      else if List.any tc2IsUnifyOrUnifyOne then
        -- Wait for downstream computation.
        noChange
      else
        case (aConstraints, bConstraints) of
          ([], []) ->
            -- Any added connections need to be bidirectional.
            ( [TC2SameAs aId, TC2SameAs bId]
            , graph |> addTC2ToGraph aId (TC2SameAs thisId) |> addTC2ToGraph bId (TC2SameAs thisId)
            )
          ([aConstraint], [bConstraint]) ->

          (aConstraint, [])
          []            -> tc2
          _::_::_       -> tc2
          [aConstraint] ->
            case constraintsOnSubgraph bId graph ->
              []            -> tc2
              _::_::_       -> tc2
              [bConstraint] ->

    _ -> tc2


unifyConstraintsUntilFixpoint : Int -> TC2Graph -> TC2Graph
unifyConstraintsUntilFixpoint maxIterations graph =
  let buildConnectedComponents graph =
    let newGraph = propagateGraphConstraints graph in
    if graph == newGraph
    then graph
    else buildConnectedComponents newGraph
  in
  -- After this point, if need to add a SameAs node be sure to re-run buildConnectedComponents UNLESS
  -- (a) You are sure there are no constraints on at least one of the components you are connecting AND
  -- (b) You make sure the connecting edge is bidirectional
  unifyConstraintsUntilFixpoint_ maxIterations


unifyConstraintsUntilFixpoint_ : Int -> TC2Graph -> TC2Graph
unifyConstraintsUntilFixpoint_ maxIterations graph
  if maxIterations <= 0 then
    graph
  else
    let newGraph =
      graph
      |> unifyImmediatesStep
      |> unifyAcrossNodesStep
    in
    if graph == newGraph then
      graph
    else
      unifyConstraintsUntilFixpoint_ (maxIterations - 1) newGraph


---------------------------------------------------------------------------


-- Send program in just because it's the easiest way to find the max id.
constraintsToTypeConstraints2 : Exp -> List Constraint -> TC2Graph
constraintsToTypeConstraints2 program constraints =
  let
    currentId : TC2Id
    currentId = 1 + FastParser.maxId program

    -- Returns (newCurrentId, tc2Ids of added nodes, newGraph)
    addTCSToGraph : List (Ident, TC2Id) -> TC2Id -> List TypeConstraint -> TC2Graph -> (TC2Id, List TC2Id, TC2Graph)
    addTCSToGraph typeVarToTC2Id currentId tcs graph =
      tcs
      |> List.foldl
          (\tc (currentId, tc2Ids, graph) ->
            let (newCurrentId, tc2Id, newGraph) = addTCToGraph typeVarToTC2Id currentId tc graph in
            (newCurrentId, tc2Ids ++ [tc2Id], newGraph)
          )
          (currentId, [], graph)

    -- Returns (newCurrentId, tc2Id of added node, newGraph)
    addTCToGraph : List (Ident, TC2Id) -> TC2Id -> TypeConstraint -> TC2Graph -> (TC2Id, TC2Id, TC2Graph)
    addTCToGraph typeVarToTC2Id currentId tc graph =
      case tc of
        TCEId eid            -> (currentId, eid, graph)
        TCPId pid            -> (currentId, pid, graph)
        TCApp tc1 []         -> addTCToGraph typeVarToTC2Id currentId tc1 graph -- Shouldn't happen, but this is correct if it does.
        TCApp tc1 [tc2]      ->
          let
            (currentId2, tc1_tc2Id, graph2) = addTCToGraph typeVarToTC2Id currentId tc1 graph
            (currentId3, tc2_tc2Id, graph3) = addTCToGraph typeVarToTC2Id currentId tc2 graph2
            finalGraph = addTC2ToGraph currentId3 (TC2App tc1_tc2Id tc2_tc2Id) graph3
          in
          (currentId3 + 1, currentId3, finalGraph)
        TCApp tc1 tcs        ->
          let desugarTCApp tc1 tcs =
            case tcs of
              []       -> tc1
              tc2::tcs -> desugarTCApp (TCApp tc1 [tc2]) tcs
          in
          addTCToGraph typeVarToTC2Id currentId (desugarTCApp tc1 tcs) graph

        -- TCApp tc1 tcs       ->
        --   let
        --     (currentId2, tc1_tc2Id,  graph2) = addTCToGraph typeVarToTC2Id currentId tc1 graph
        --     (currentId3, tcs_tc2Ids, graph3) = addTCSToGraph typeVarToTC2Id currentId2 tcs graph2
        --     finalGraph = addTC2ToGraph currentId3 (TC2App tc1_tc2Id tcs_tc2Ids) graph3
        --   in
        --   (currentId3 + 1, currentId3, finalGraph)
        TCNum                -> (currentId + 1, currentId, addTC2ToGraph currentId TC2Num    graph)
        TCBool               -> (currentId + 1, currentId, addTC2ToGraph currentId TC2Bool   graph)
        TCString             -> (currentId + 1, currentId, addTC2ToGraph currentId TC2String graph)
        TCNull               -> (currentId + 1, currentId, addTC2ToGraph currentId TC2Null   graph)
        TCList tc1           ->
          let
            (currentId2, tc1_tc2Id, graph2) = addTCToGraph typeVarToTC2Id currentId tc1 graph
            finalGraph = addTC2ToGraph currentId2 (TC2Tuple [] (Just tc1_tc2Id)) graph2
          in
          (currentId2 + 1,  currentId2, finalGraph)
        TCTuple tcs mtc     ->
          let
            (currentId2, tcs_tc2Ids, graph2) = addTCSToGraph typeVarToTC2Id currentId tcs graph
            (currentId3, maybe_tc2Id, graph3) =
              case mtc of
                Nothing     -> (currentId2,     Nothing,         graph2)
                Just tailTC -> addTCToGraph typeVarToTC2Id currentId2 tailTC graph2 |> Utils.mapSnd3 Just

            finalGraph = addTC2ToGraph currentId3 (TC2Tuple tcs_tc2Ids maybe_tc2Id) graph3
          in
          (currentId3 + 1, currentId3, finalGraph)
        TCArrow []           -> let _ = Utils.log <| "WAT: Why is there an empty TCArrow in addTCToGraph?" in (currentId + 1, currentId, addTC2ToGraph currentId (TC2Empty "Empty arrow") graph)
        TCArrow [tc1]        -> addTCToGraph typeVarToTC2Id currentId [tc1] graph
        TCArrow (tc1::tcs)   ->
          let
            (currentId2, tc1_tc2Id, graph2) = addTCToGraph typeVarToTC2Id currentId tc1 graph
            (currentId3, tcs_tc2Id, graph3) = addTCToGraph typeVarToTC2Id currentId2 (TCArrow tcs) graph2
            finalGraph = addTC2ToGraph currentId3 (TC2Arrow tc1_tc2Id tcs_tc2Id) graph3
          in
          (currentId3 + 1, currentId3, finalGraph)
        TCUnion tcs          ->
          let
            (currentId2, tcs_tc2Ids, graph2) = addTCSToGraph typeVarToTC2Id currentId tcs graph
            finalGraph = addTC2ToGraph currentId2 (TC2Union tcs_tc2Ids) graph2
          in
          (currentId2 + 1, currentId2, finalGraph)
        TCNamed _            -> let _ = Utils.log <| "BUG: All type aliases should be resolved by constraintsToTypeConstraints2 but encountered " ++ toString tc ++ "!" in (currentId + 1, currentId, graph)
        TCVar ident          ->
          case Utils.maybeFind ident typeVarToTC2Id of
            Just tc2Id -> (currentId, tc2Id, graph)
            Nothing    -> let _ = Utils.log <| "MALFORMED TYPE: type var " ++ ident ++ " not found in type env " ++ toString typeVarToTC2Id ++ ". All types with vars should be wrapped in forall!!!" in (currentId + 1, currentId, graph)
        TCForall idents tc1  ->
          let newTypeVarToTC2Id =
            idents
            |> Utils.zipi_ currentId
            |> List.map Utils.flip
          in
          addTCToGraph (newTypeVarToTC2Id ++ typeVarToTC2Id) (currentId + List.length newTypeVarToTC2Id) tc1 graph
        TCWildcard           -> (currentId + 1, currentId, graph)
  in
  let (_, _, finalGraph) =
    constraints
    |> List.foldl
        (\constraint (currentId, uniqueNameToTC2Id, graph) ->
          case constraint of
            EIdIsType eid tc ->
              let (newCurrentId, tc2Id, graphWithTC2) = addTCToGraph [] currentId tc graph in
              ( newCurrentId
              , uniqueNameToTC2Id
              , graphWithTC2 |> addIdsEdgeToGraph eid tc2Id
              )

            PIdIsType pid tc ->
              let (newCurrentId, tc2Id, graphWithTC2) = addTCToGraph [] currentId tc graph in
              ( newCurrentId
              , uniqueNameToTC2Id
              , graphWithTC2 |> addIdsEdgeToGraph pid tc2Id
              )

            PIdIsEId pid eid ->
              ( currentId
              , uniqueNameToTC2Id
              , graph |> addIdsEdgeToGraph pid eid
              )

            EIdVar eid ident ->
              case Dict.get ident uniqueNameToTC2Id of
                Just tc2Id ->
                  ( currentId
                  , uniqueNameToTC2Id
                  , graph |> addIdsEdgeToGraph eid tc2Id
                  )

                Nothing ->
                  ( currentId + 1
                  , Dict.insert ident currentId uniqueNameToTC2Id
                  , graph |> addIdsEdgeToGraph eid currentId
                  )

            PIdVar pid ident ->
              case Dict.get ident uniqueNameToTC2Id of
                Just tc2Id ->
                  ( currentId
                  , uniqueNameToTC2Id
                  , graph |> addIdsEdgeToGraph pid tc2Id
                  )

                Nothing ->
                  ( currentId + 1
                  , Dict.insert ident currentId uniqueNameToTC2Id
                  , graph |> addIdsEdgeToGraph pid currentId
                  )

            EIdIsEmpty eid str ->
              ( currentId
              , uniqueNameToTC2Id
              , graph |> addTC2ToGraph eid (TC2Empty str)
              )

            PIdIsEmpty pid str ->
              ( currentId
              , uniqueNameToTC2Id
              , graph |> addTC2ToGraph pid (TC2Empty str)
              )

            TypeAlias ident tc ->
              let _ = Utils.log <| "BUG: Type aliases should be gone by constraintsToTypeConstraints2 but encountered constraint " ++ toString constraint in
              ( currentId
              , uniqueNameToTC2Id
              , graph
              )
        )
        (currentId, Dict.empty, Dict.empty)
  in
  finalGraph


maxIterations : Int
maxIterations = 100


typecheck : Exp -> TC2Graph
typecheck program =
  let (programUniqueNames, uniqueNameToOldName) = LangTools.assignUniqueNames program in
  programUniqueNames
  |> gatherConstraints
  |> expandTypeAliases
  |> constraintsToTypeConstraints2 program
  |> unifyConstraintsUntilFixpoint maxIterations


-- Assumes type aliases are uniquely named.
expandTypeAliases : List Constraint -> List Constraint
expandTypeAliases constraints =
  let
    constraintToMaybeAlias tc =
      case tc of
        TypeAlias ident tc -> Just (ident, tc)
        _                  -> Nothing

    expandAliasInTC aliasName aliasTC tc =
      tc
      |> mapTC
          (\tc ->
            case tc of
              TCNamed ident -> if ident == aliasName then aliasTC else tc
              _             -> tc
          )

    expandAliasInConstraint aliasName aliasTC constraint =
      case constraint of
        EIdIsType eid tc       -> EIdIsType eid (expandAliasInTC aliasName aliasTC tc)
        PIdIsType pid tc       -> PIdIsType pid (expandAliasInTC aliasName aliasTC tc)
        PIdIsEId pid eid       -> constraint
        EIdVar eid ident       -> constraint
        PIdVar pid ident       -> constraint
        EIdIsEmpty eid message -> constraint
        PIdIsEmpty pid message -> constraint
        TypeAlias ident tc     -> TypeAlias ident (expandAliasInTC aliasName aliasTC tc)

  in
  case constraints |> Utils.maybeFindAndRemoveFirst (constraintToMaybeAlias >> Utils.maybeToBool) |> Maybe.map (Tuple.mapFirst constraintToMaybeAlias) of
    Just (Just (aliasName, aliasTC), remainingConstraints) ->
      remainingConstraints
      |> List.map (expandAliasInConstraint aliasName aliasTC)
      |> expandTypeAliases

    _ ->
      constraints


gatherConstraints : Exp -> List Constraint
gatherConstraints exp =
  let eidIs typeConstraint = [EIdIsType exp.val.eid typeConstraint] in
  let expToTC = .val >> .eid >> TCEId in
  let expsToTCs = List.map expToTC in
  let childConstraints = childExps exp |> List.concatMap gatherConstraints in
  childConstraints ++
  case exp.val.e__ of
    EBase _ (EBool _)      -> eidIs TCBool
    EBase _ (EString _ _)  -> eidIs TCString
    EBase _ ENull          -> eidIs TCNull
    EConst _ _ _ _         -> eidIs TCNum
    EVar _ ident           -> [EIdVar exp.val.eid ident]
    EFun _ argPats fBody _ ->
      gatherPatsConstraints argPats ++
      eidIs (TCArrow <| (argPats |> List.map (.val >> .pid >> TCPId)) ++ [TCEId fBody.val.eid])
    EApp _ fExp argExps _        -> eidIs <| TCApp (expToTC fExp) (expsToTCs argExps)
    EList _ heads _ maybeTail _  -> eidIs <| TCTuple (expsToTCs heads) (Maybe.map expToTC maybeTail)
    EOp _ op operands _          ->
      case (op.val, operands |> List.map (.val >> .eid)) of
        (Pi,         [])           -> eidIs TCNum
        (ToStr,      [_])          -> eidIs TCString
        (DebugLog,   [eid])        -> eidIs (TCEId eid)
        (Eq,         [eid1, eid2]) -> eidIs TCBool -- (a -> b -> Bool), see Eval.eval
        (Cos,        [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (Sin,        [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (ArcCos,     [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (ArcSin,     [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (ArcTan2,    [eid1, eid2]) -> eidIs TCNum  ++ [EIdIsType eid1 TCNum, EIdIsType eid2 TCNum]
        (Floor,      [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (Ceil,       [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (Round,      [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (Sqrt,       [eid])        -> eidIs TCNum  ++ [EIdIsType eid TCNum]
        (Plus,       [eid1, eid2]) -> eidIs (TCEId eid1) ++ eidIs (TCEId eid2) ++ eidIs (TCUnion [TCNum, TCString]) -- (a -> a -> a) where a is String or Num
        (Minus,      [eid1, eid2]) -> eidIs TCNum  ++ [EIdIsType eid1 TCNum, EIdIsType eid2 TCNum]
        (Mult,       [eid1, eid2]) -> eidIs TCNum  ++ [EIdIsType eid1 TCNum, EIdIsType eid2 TCNum]
        (Div,        [eid1, eid2]) -> eidIs TCNum  ++ [EIdIsType eid1 TCNum, EIdIsType eid2 TCNum]
        (Lt,         [eid1, eid2]) -> eidIs TCBool ++ [EIdIsType eid1 TCNum, EIdIsType eid2 TCNum]
        (Mod,        [eid1, eid2]) -> eidIs TCNum  ++ [EIdIsType eid1 TCNum, EIdIsType eid2 TCNum]
        (Pow,        [eid1, eid2]) -> eidIs TCNum  ++ [EIdIsType eid1 TCNum, EIdIsType eid2 TCNum]
        (NoWidgets,  [eid])        -> eidIs (TCEId eid)
        (Explode,    [eid])        -> eidIs (TCList TCString) ++ [EIdIsType eid TCString] -- (String -> List String)
        _                          -> [EIdIsEmpty exp.val.eid "Bad operation"]
    EIf _ condExp thenExp elseExp _ ->
      eidIs (expToTC thenExp) ++ eidIs (expToTC elseExp) ++ [EIdIsType condExp.val.eid TCBool]
    ELet _ _ _ pat boundExp letBody _ ->
      -- tryMatchExpPatToPIds : Pat -> Exp -> List (PId, Exp)
      -- let
      --   pidToExp           = LangTools.tryMatchExpPatToPIds pat boundExp
      --   (matchedPIds, _)   = List.unzip pidToExp
      --   unmatchedPIds      = Utils.diffAsSet (allPIds pat) matchedPIds
      --   matchedConstraints = pidToExp |> List.map (\(pid, boundExp) -> PIdIsEId pid boundExp.val.eid)
      --   unmatchedErrors    = unmatchedPIds |> List.map (\pid -> PIdIsEmpty pid "PId didn't match in let exp")
      -- in
      gatherPatConstraints pat ++
      [PIdIsEId pat.val.pid boundExp.val.eid] ++
      eidIs (expToTC letBody)
    ECase _ scrutinee bs _ ->
      gatherPatsConstraints (branchPats bs) ++
      (branchPats bs |> List.map (\bPat -> PIdIsEId bPat.val.pid scrutinee.val.eid)) ++
      (branchExps bs |> List.concatMap (eidIs << expToTC))
    ETypeCase _ scrutinee bs _ ->
      [EIdIsType scrutinee.val.eid <| TCUnion (tbranchTypes bs |> List.map typeToTC)] ++
      (tbranchExps bs |> List.concatMap (eidIs << expToTC))
    EComment _ _ e1     -> eidIs (expToTC e1)
    EOption _ _ _ _ e1  -> eidIs (expToTC e1)
    ETyp _ pat tipe e _ ->
      gatherPatConstraints pat ++
      [PIdIsType pat.val.pid (typeToTC tipe)] ++
      eidIs (expToTC e)
    EColonType _ e _ tipe _ ->
      eidIs (typeToTC tipe) ++
      eidIs (expToTC e)
    ETypeAlias _ pat tipe e _ ->
      let aliasConstraints =
      case Types.matchTypeAlias pat tipe of
        Just identToType -> identToType |> List.map (\(ident, tipe) -> TypeAlias ident (typeToTC tipe))
        Nothing          -> [PIdIsEmpty pat.val.pid "Type alias malformed"]
      in
      aliasConstraints ++
      eidIs (expToTC e)
    EParens _ e _ -> eidIs (expToTC e)
    EHole _ _     -> []


gatherPatsConstraints : List Pat -> List Constraint
gatherPatsConstraints pats = List.concatMap gatherPatConstraints pats


gatherPatConstraints : Pat -> List Constraint
gatherPatConstraints pat =
  let childConstraints = gatherPatsConstraints (childPats pat) in
  childConstraints ++
  case pat.val.p__ of
    PVar _ ident _              -> [PIdVar pat.val.pid ident]
    PList _ heads _ maybeTail _ -> [PIdIsType pat.val.pid <| TCTuple (heads |> List.map (.val >> .pid >> TCPId)) (maybeTail |> Maybe.map (.val >> .pid >> TCPId))]
    PConst _ n                  -> [PIdIsType pat.val.pid TCNum]
    PBase _ (EBool _)           -> [PIdIsType pat.val.pid TCBool]
    PBase _ (EString _ _)       -> [PIdIsType pat.val.pid TCString]
    PBase _ ENull               -> [PIdIsType pat.val.pid TCNull]
    PAs _ ident _ child         -> [PIdVar pat.val.pid ident, PIdIsType pat.val.pid (TCPId child.val.pid)]

