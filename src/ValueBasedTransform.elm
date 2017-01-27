--
-- ValueBasedTransform
--
-- Code transformations based on values selected in the output.
--

module ValueBasedTransform exposing (..)

import Lang exposing (..)
import LangTools exposing (..)
import LangParser2 exposing (parseE, freshen, substOf)
import LangUnparser exposing (unparse)
import InterfaceModel
import Eval
import Sync
import LocEqn exposing (..)
import Utils
import LangSvg exposing (NodeId, ShapeKind, Attr)
import ShapeWidgets exposing (FeatureEquation)
import Config

import Dict
import Set
import String
import Regex


debugLog = Config.debugLog Config.debugSync



digHole originalExp selectedFeatures slate syncOptions =
  let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
  let selectedFeatureEquationsNamed =
    debugLog "selectedFeatureEquations" <|
      pluckSelectedFeatureEquationsNamed selectedFeatures slate locIdToNumberAndLoc
  in
  -- If any locs are annotated with "?", only dig those.
  let locset =
    let selectedVals =
      debugLog "selectedVals" <|
        pluckSelectedVals selectedFeatures slate locIdToNumberAndLoc
    in
    let tracesLocsets =
      List.map ((Sync.locsOfTrace syncOptions) << Tuple.second) selectedVals
    in
    let allLocs = List.foldl Set.union Set.empty tracesLocsets in
    let (thawed, others) =
      allLocs
      |> Set.partition (\(_, annotation, _) -> annotation == Lang.thawed)
    in
    if Set.isEmpty thawed
    then others
    else thawed
  in
  let locsetList =
    Set.toList locset
  in
  let subst = substOf originalExp in
  let commonScope =
    deepestCommonScope originalExp locset syncOptions
  in
  let existingNames = identifiersSet originalExp in
  let locIdNameOrigNamePrime =
    let (_, result) =
      List.foldr
          (\(locId, frozen, ident) (usedNames, result) ->
            let baseIdent = if ident == "" then "k"++(toString locId) else ident in
            let scopeNamesLiftedThrough = scopeNamesLocLiftedThrough commonScope (locId, frozen, ident) in
            let scopesAndBaseIdent = String.join "_" (scopeNamesLiftedThrough ++ [baseIdent]) in
            let baseIdentOrig  =
              if scopesAndBaseIdent == baseIdent
              then baseIdent ++ "_orig"
              else scopesAndBaseIdent
            in
            let baseIdentPrime = scopesAndBaseIdent ++ "'" in
            let identOrig  = nonCollidingName baseIdentOrig  2 usedNames in
            let identPrime = nonCollidingName baseIdentPrime 2 usedNames in
            (
              Set.union usedNames (Set.fromList [identOrig, identPrime]),
              (locId, identOrig, identPrime)::result
            )
          )
          (existingNames, [])
          locsetList
    in
    result
  in
  let newNames = List.concatMap (\(_, n1, n2) -> [n1, n2]) locIdNameOrigNamePrime in
  let namesToAvoid = Set.union existingNames (Set.fromList newNames) in
  let locIdToNewName = debugLog "locIdToNewName" <|
    Dict.fromList
      <| List.map (\(locId, nameOrig, namePrime) -> (locId, namePrime))
      <| locIdNameOrigNamePrime
  in
  let origNames  = List.reverse <| List.map Utils.snd3 locIdNameOrigNamePrime in
  let primeNames = List.reverse <| List.map Utils.thd3 locIdNameOrigNamePrime in
  let valueExps =
    List.reverse locsetList
    |> List.map
        (\(locId, _, _) -> findExpByLocId locId commonScope |> Utils.fromJust_ "ValueBasedTransform.digHole valueExps")
  in
  let selectedFeatureEquationsNamedWithScopes =
    List.map
        (\(featureName, eqn) ->
          let featureLocs = equationLocs syncOptions eqn in
          let scopeNamesLocsLiftedThrough =
            List.map
                (scopeNamesLocLiftedThrough commonScope)
                featureLocs
          in
          let commonScopeNamesLocsLiftedThrough =
            Utils.commonPrefix scopeNamesLocsLiftedThrough
          in
          let featureName_ =
            String.join "_" (commonScopeNamesLocsLiftedThrough ++ [featureName])
          in
          (featureName_, eqn)
        )
        selectedFeatureEquationsNamed
  in
  let featureNamesWithExpressionExps =
    let locIdToOrigName =
      Dict.fromList
        <| List.map (\(locId, nameOrig, namePrime) -> (locId, nameOrig))
        <| locIdNameOrigNamePrime
    in
    List.map (Utils.mapSnd <| equationToExp subst locIdToOrigName) selectedFeatureEquationsNamedWithScopes
  in
  -- Remove expressions of only one term
  let significantFeatureNamesWithExpressionExps =
    List.filter
        (\(name, exp) -> nodeCount exp > 1)
        featureNamesWithExpressionExps
  in
  let featureNames          = List.map Tuple.first significantFeatureNamesWithExpressionExps in
  let featureExpressionExps = List.map Tuple.second significantFeatureNamesWithExpressionExps in
  let nonCollidingFeatureNames =
    let (newNamesToAvoid, result) =
      List.foldr
          (\featureName (usedNames, result) ->
            let featureName_ = nonCollidingName featureName 2 usedNames in
            (
              Set.insert featureName_ usedNames,
              featureName_::result
            )
          )
          (namesToAvoid, [])
          featureNames
    in
    result
  in
  let listOfListsOfNamesAndAssigns =
    [ Utils.zip origNames valueExps
    , Utils.zip nonCollidingFeatureNames featureExpressionExps
    , Utils.zip primeNames (listOfVars origNames)
    ]
  in
  let newExp =
    variableifyConstantsAndWrapTargetExpWithLets
        locIdToNewName
        listOfListsOfNamesAndAssigns
        commonScope
        originalExp
  in
  newExp


type RelateType
  = Equalize
  | Relate

makeEqual = synthesizeRelation Equalize
relate    = synthesizeRelation Relate

-- Rank synthesis results by:
--
-- 1. Distance between locs removed (less is better)
-- 2. Position in program of locs removed (later is better)
--
rankComparedTo originalExp synthesisResults =
  let isLocId targetLocId exp =
    case exp.val.e__ of
      EConst ws n (locId, frozen, ident) wd -> targetLocId == locId
      _                                     -> False
  in
  synthesisResults
  |> List.map
      (\{description, exp, sortKey, dependentLocIds} ->
        let locLineNums =
          dependentLocIds
          |> List.map
              (\locId ->
                case findFirstNode (isLocId locId) originalExp of
                  Just constExp -> toFloat constExp.start.line
                  Nothing       -> -Utils.infinity
              )
        in
        let removedLocDistance =
          if List.length locLineNums <= 1 || List.any isInfinite locLineNums then
            Utils.infinity
          else
            (List.maximum locLineNums |> Utils.fromJust) - (List.minimum locLineNums |> Utils.fromJust)
        in
        { description = description
        , exp         = exp
        , sortKey     = [removedLocDistance] ++ (locLineNums |> List.map negate |> List.reverse)
        }
      )

-- Returns list of synthesis results
synthesizeRelation relateType originalExp selectedFeatures slideNumber movieNumber movieTime syncOptions =
  let relateByPairs priorResults features =
    relateOverlappingPairs relateType priorResults features slideNumber movieNumber movieTime syncOptions
  in
  let selectedPoints =
    featurePoints (Set.toList selectedFeatures)
  in
  -- let _ = Debug.log ("Original:\n" ++ LangUnparser.unparseWithIds originalExp) () in
  let startingResult = { description = "Original", exp = originalExp, sortKey = [], dependentLocIds = [] } in
  if 2 * (List.length selectedPoints) == (Set.size selectedFeatures) then
    -- We have only selected x&y of several points.
    -- Make all the selected points overlap, that is: make all the x's equal to
    -- each other and all the y's equal to each other.
    let xFeatures = List.map Tuple.first selectedPoints in
    let yFeatures = List.map Tuple.second selectedPoints in
    let xsEqualized  = relateByPairs [startingResult] xFeatures in
    let xysEqualized = relateByPairs xsEqualized yFeatures in
    xysEqualized
    |> rankComparedTo originalExp
  else
    -- We have not selected only x&y of different points.
    -- Equalize all selected attributes naively.
    relateByPairs [startingResult] (Set.toList selectedFeatures)
    |> rankComparedTo originalExp


-- If given more than two features, run relate_ on each overlapping pair.
relateOverlappingPairs relateType priorResults features slideNumber movieNumber movieTime syncOptions =
  let relateMore results =
    case features of
      _::remainingFeatues ->
        relateOverlappingPairs relateType results remainingFeatues slideNumber movieNumber movieTime syncOptions

      _ ->
        -- Shouldn't happen.
        Debug.crash "relateOverlappingPairs relateMore"
  in
  case List.take 2 features of
    [featureA, featureB] ->
      priorResults
      |> List.concatMap
          (\{description, exp, sortKey, dependentLocIds} ->
            let priorExp = exp in
            let slateRes =
              Eval.run priorExp |>
              Result.andThen (\(val, _) ->
                  LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
                )
            in
            case slateRes of
              Err s -> []
              Ok slate ->
                let newResults =
                  relate_ relateType priorExp featureA featureB slate syncOptions
                in
                case newResults of
                  [] ->
                    relateMore [{description = description, exp = priorExp, sortKey = sortKey, dependentLocIds = dependentLocIds}]

                  _ ->
                    newResults
                    |> List.map (InterfaceModel.prependDescription (description ++ " -> "))
                    |> List.map (\result -> { result | dependentLocIds = dependentLocIds ++ result.dependentLocIds })
                    -- |> List.map (\result -> let _ = if True then Debug.log ("Before:\n" ++ LangUnparser.unparseWithIds priorExp ++ "\nAfter:\n" ++ LangUnparser.unparseWithIds result.exp) () else () in result)
                    |> relateMore

          )

    _ ->
      priorResults


-- makeEquidistant originalExp selectedFeatures slideNumber movieNumber movieTime slate syncOptions =
--   let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
--   let features = Set.toList selectedFeatures in
--   let evaluatedFeatures =
--     features
--     |> List.map (\feature -> evaluateFeature feature slate locIdToNumberAndLoc)
--   in
--   if List.all ((/=) Nothing) evaluatedFeatures then
--     let sortedFeatures =
--       features
--       |> List.sortBy (\feature -> Utils.fromJust <| evaluateFeature feature slate locIdToNumberAndLoc)
--     in
--     makeEquidistantOverlappingTriples originalExp sortedFeatures slideNumber movieNumber movieTime slate syncOptions locIdToNumberAndLoc
--   else
--     originalExp
--
--
-- makeEquidistantOverlappingTriples originalExp sortedFeatures slideNumber movieNumber movieTime slate syncOptions locIdToNumberAndLoc =
--   let relateMore exp =
--     case sortedFeatures of
--       -- If there's at least 3 more features...
--       _::featureB::featureC::featureD::otherFeatures ->
--         let newSlateRes =
--           Eval.run exp |>
--           Result.andThen (\(val, _) ->
--               LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
--             )
--         in
--         case newSlateRes of
--           Err s -> exp
--           Ok newSlate ->
--             let newLocIdToNumberAndLoc = locIdToNumberAndLocOf exp in
--             makeEquidistantOverlappingTriples
--                 exp
--                 (featureB::featureC::featureD::otherFeatures)
--                 slideNumber
--                 movieNumber
--                 movieTime
--                 newSlate
--                 syncOptions
--                 newLocIdToNumberAndLoc
--
--       _ ->
--         exp
--   in
--   case List.take 3 sortedFeatures of
--     [featureA, featureB, featureC] ->
--       let maybeNewExp =
--         let (_, tree) = slate in
--         let maybeAEqn = typeAndNodeIdAndFeatureToEquation featureA tree locIdToNumberAndLoc in
--         let maybeBEqn = typeAndNodeIdAndFeatureToEquation featureB tree locIdToNumberAndLoc in
--         let maybeCEqn = typeAndNodeIdAndFeatureToEquation featureC tree locIdToNumberAndLoc in
--         case (maybeAEqn, maybeBEqn, maybeCEqn) of
--           (Just aEqn, Just bEqn, Just cEqn) ->
--             let distanceAB = ShapeWidgets.EqnOp Minus [bEqn, aEqn] in
--             let distanceBC = ShapeWidgets.EqnOp Minus [cEqn, bEqn] in
--             relate__ Equalize originalExp distanceAB distanceBC syncOptions
--
--           _ -> Nothing
--       in
--       case maybeNewExp of
--         Just newExp ->
--           relateMore newExp
--
--         Nothing ->
--           relateMore originalExp
--
--     _ ->
--       originalExp


relate_ relateType originalExp featureA featureB slate syncOptions =
  let (_, tree) = slate in
  let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
  let featureDescription (selectedType, nodeId, featureName) tree = featureName in
  case (typeAndNodeIdAndFeatureToEquation featureA tree locIdToNumberAndLoc,
        typeAndNodeIdAndFeatureToEquation featureB tree locIdToNumberAndLoc) of
    (Nothing, _) ->
      []

    (_, Nothing) ->
      []

    (Just featureAEqn,
     Just featureBEqn) ->
       let descriptionPrefix =
         case relateType of
           Equalize -> (featureDescription featureA tree) ++ " = " ++ (featureDescription featureB tree) ++ " "
           Relate   -> ""
       in
       relate__ relateType originalExp featureAEqn featureBEqn syncOptions
       |> List.map (InterfaceModel.prependDescription descriptionPrefix)


relate__ relateType originalExp featureAEqn featureBEqn syncOptions =
  let frozenLocIdToNum =
    ((frozenLocIdsAndNumbers originalExp) ++
     (frozenLocIdsAndNumbers LangParser2.prelude))
    |> Dict.fromList
  in
  let aUnfrozenLocset = equationLocs syncOptions featureAEqn |> Set.fromList in
  let bUnfrozenLocset = equationLocs syncOptions featureBEqn |> Set.fromList in
  let unfrozenLocset = Set.union aUnfrozenLocset bUnfrozenLocset in
  -- Ignore locations multiplied by 0, etc.
  let aSignificantUnfrozenLocIdSet =
    featureEquationToLocEquation featureAEqn
    |> constantifyLocs frozenLocIdToNum
    |> normalizeSimplify
    |> locEqnLocIdSet
  in
  let bSignificantUnfrozenLocIdSet =
    featureEquationToLocEquation featureBEqn
    |> constantifyLocs frozenLocIdToNum
    |> normalizeSimplify
    |> locEqnLocIdSet
  in
  let sharedSignificantUnfrozenLocIdSet =
    Set.intersect aSignificantUnfrozenLocIdSet bSignificantUnfrozenLocIdSet
  in
  let subst = substOf originalExp in
  -- Prefer to solve for ?-annotated locs
  -- This code is pointless now that all synth results are shown.
  -- May want to incorporate thawing into the result ranking.
  let thawedLocsFirst =
    let (thawed, others) =
      unfrozenLocset
      |> Set.toList
      |> List.partition (\(_, annotation, _) -> annotation == Lang.thawed)
    in
    thawed ++ others
  in
  let solutionsForLoc dependentLoc =
    let (dependentLocId, dependentFrozen, dependentIdent) = dependentLoc in
    let dependentIdentDesc =
      let baseIdent = if dependentIdent == "" then "k"++(toString dependentLocId) else dependentIdent in
      let scopeNamesLiftedThrough = scopeNamesLocLiftedThrough originalExp dependentLoc in
      String.join " " (scopeNamesLiftedThrough ++ [baseIdent])
    in
    case relateType of
      Equalize ->
        case solveForLoc dependentLocId frozenLocIdToNum subst featureAEqn featureBEqn of
          Nothing ->
            []

          Just resultLocEqn ->
            [(resultLocEqn, "by removing " ++ dependentIdentDesc)]

      Relate ->
        -- Solve for a location that *doesn't* appear in other equation.
        -- In some cases such cases you could get a meaningful relation but that
        -- requires smarts that we don't have yet.
        let independentLocIdSet =
          if Set.member dependentLocId sharedSignificantUnfrozenLocIdSet then
            -- Loc appears in both equations; do not try to replace it.
            Set.empty
          else if Set.member dependentLocId aSignificantUnfrozenLocIdSet then
            bSignificantUnfrozenLocIdSet
          else if Set.member dependentLocId bSignificantUnfrozenLocIdSet then
            aSignificantUnfrozenLocIdSet
          else
            -- Dependent loc is insignficant (e.g. multiplied by 0)
            -- Replacing it is futile.
            Set.empty
        in
        let targetValue = Utils.justGet_ "ValueBasedTransform.relate__ targetValue" dependentLocId subst in
        -- let indepLocs = Set.remove dependentLoc unfrozenLocset in
        let isGoodEnough locEqn =
          if Set.size (locEqnLocIdSet locEqn) == 0 then
            False
          else
            let diff = locEqnEval subst locEqn - targetValue in
            if targetValue == 0
            then diff == 0
            else abs (diff / targetValue) < 0.2
        in
        -- Template must have at least one place for a variable
        -- and at most one place for a constant
        let isGoodShape locEqn =
          Set.size (locEqnLocIdSet locEqn) > 0 && List.length (locEqnConsts locEqn) <= 1
        in
        let maxResults = 10 in
        let synthesizeMore astSize results =
          if List.length results >= maxResults then
            results
          else
            let eqnTemplates = locEqnsTemplatesOfSize astSize |> List.filter isGoodShape in
            -- let newEqns = locEqnsOfSize astSize indepLocs |> List.filter isGoodEnough in
            let newEqns =
              eqnTemplates
              |> List.concatMap (\template -> locEqnTemplateFillings targetValue subst independentLocIdSet template)
              |> List.filter isGoodEnough
              |> List.map normalizeSimplify
              |> List.filter (\locEqn -> locEqnSize locEqn >= astSize) -- Equation was not simplified. Still need to handle subtraction well.
            in
            results ++ newEqns
            |> Utils.equalityDedup
        in
        let resultEqns = List.foldl synthesizeMore [] (List.range 1 5) in
        resultEqns
        |> List.map (\resultLocEqn -> (resultLocEqn, dependentIdentDesc ++ " = "))
  in
  thawedLocsFirst
  |> List.concatMap
      (\dependentLoc ->
        let (dependentLocId, dependentFrozen, dependentIdent) = dependentLoc in
        solutionsForLoc dependentLoc
        |> List.map (\(resultLocEqn, description) ->
            let locIdSet = Set.insert dependentLocId <| locEqnLocIdSet resultLocEqn in
            -- Consequently, we don't need to dig out higher than the frozen locs.
            let locsetToDig = Set.filter (\(locId, _, _) -> Set.member locId locIdSet) unfrozenLocset in
            let commonScope =
              deepestCommonScope originalExp locsetToDig syncOptions
            in
            let existingNames = identifiersSet originalExp in
            let independentLocs =
              locsetToDig
              |> Set.toList
              |> List.filter (\(locId, _, _) -> locId /= dependentLocId)
            in
            let independentLocIds = List.map Utils.fst3 independentLocs in
            let locIdToNewName =
              let (_, result) =
                List.foldr
                    (\(locId, frozen, ident) (usedNames, result) ->
                      let baseIdent = if ident == "" then "k"++(toString locId) else ident in
                      let scopeNamesLiftedThrough = scopeNamesLocLiftedThrough commonScope (locId, frozen, ident) in
                      let scopesAndBaseIdent = String.join "_" (scopeNamesLiftedThrough ++ [baseIdent]) in
                      let ident =
                        if locId == dependentLocId then
                          nonCollidingName (baseIdent ++ "'") 2 usedNames
                        else
                          if scopesAndBaseIdent == baseIdent
                          then nonCollidingName (baseIdent ++ "_orig") 2 usedNames
                          else nonCollidingName scopesAndBaseIdent 2 usedNames
                      in
                      (
                        Set.insert ident usedNames,
                        (locId, ident)::result
                      )
                    )
                    (existingNames, [])
                    (dependentLoc::independentLocs)
              in
              Dict.fromList result
            in
            let independentLocNames =
              List.map
                  (\locId ->
                    Utils.justGet_ "ValueBasedTransform.relate__ independentLocNames" locId locIdToNewName
                  )
                  independentLocIds
            in
            let independentLocExps =
              independentLocs
              |> List.map
                  (\(locId, _, _) -> findExpByLocId locId commonScope |> Utils.fromJust_ "ValueBasedTransform.relate__ independentLocValues")
            in
            let dependentLocNameStr  =
              Utils.justGet_ "ValueBasedTransform.relate__ dependentLocNameStr" dependentLocId locIdToNewName
            in
            let dependentLocExp =
              locEqnToExp frozenLocIdToNum locIdToNewName resultLocEqn
            in
            let listOfListsOfNamesAndAssigns =
              [ Utils.zip independentLocNames independentLocExps
              , [(dependentLocNameStr, dependentLocExp)]
              ]
            in
            let newExp =
              variableifyConstantsAndWrapTargetExpWithLets
                  locIdToNewName
                  listOfListsOfNamesAndAssigns
                  commonScope
                  originalExp
            in
            case relateType of
              Equalize -> {description = description, exp = newExp, sortKey = [], dependentLocIds = [dependentLocId]}
              Relate   -> {description = description ++ unparse dependentLocExp, exp = newExp, sortKey = [], dependentLocIds = [dependentLocId]}
          )
      )

deepestCommonScope : Exp -> LocSet -> Sync.Options -> Exp
deepestCommonScope exp locset syncOptions =
  let isLocsetNode exp =
    case exp.val.e__ of
      EConst ws n loc wd -> Set.member loc locset
      _                  -> False
  in
  let locsAncestors = -- debugLog "locsAncestors" <|
    findAllWithAncestors isLocsetNode exp
  in
  -- isScope needs to see the node's parent...because case statements
  -- produce many scopes out of one expression
  -- The below adds a maybe parent to each node, so we get List (List
  -- (Maybe Exp, Exp))
  let locsAncestorsWithParents = -- debugLog "locsAncestorsWithParents" <|
    List.map
        (\locAncestors ->
          Utils.zip (Nothing :: (List.map Just locAncestors)) locAncestors
        )
        locsAncestors
  in
  let locsAncestorScopesWithParents = -- debugLog "locsAncestorScopesWithParents" <|
    List.map
        (List.filter (\(parent, node) -> isScope parent node))
        locsAncestorsWithParents
  in
  let locsAncestorScopes = List.map (List.map Tuple.second) locsAncestorScopesWithParents in
  let deepestCommonScope =
    Utils.last_
    <| exp :: (Utils.commonPrefix locsAncestorScopes)
  in
  deepestCommonScope


-- If suggestedName is not in existing names, returns it.
-- Otherwise appends a number (starting at i) that doesn't collide.
nonCollidingName : Ident -> Int -> Set.Set Ident -> Ident
nonCollidingName suggestedName i existingNames =
  if not (Set.member suggestedName existingNames) then
    suggestedName
  else
    let newName = suggestedName ++ (toString i) in
    if not (Set.member newName existingNames)
    then newName
    else nonCollidingName suggestedName (i+1) existingNames


-- Replace consts in targetExp with given variable names
-- Wrap targetExp with given lets
-- Replace targetExp with wrapped version in the program
variableifyConstantsAndWrapTargetExpWithLets locIdToNewName listOfListsOfNamesAndAssigns targetExp program =
  let targetExpReplaced =
    replaceConstsWithVars locIdToNewName targetExp
  in
  let wrappedTargetExp =
    wrapWithLets
        listOfListsOfNamesAndAssigns
        (isTopLevel targetExp program)
        targetExpReplaced
  in
  -- Debug only:
  -- let _ = debugLog "wrappedTargetExp" <| unparse wrappedTargetExp in
  let newProgram =
    replaceExpNode targetExp wrappedTargetExp program
    |> freshen
  in
  newProgram


-- Given [ [("a", eConst 4 dummyLoc), ("b", eConst 5 dummyLoc)], [("c", eConst 6 dummyLoc)] ] bodyExp
--
-- Produces an Exp of:
--
-- (let [a c] [4 5]
-- (let [c] [6]
--   bodyExp))
--
wrapWithLets : List (List (String, Exp)) -> Bool -> Exp -> Exp
wrapWithLets listOfListsOfNamesAndAssigns isTopLevel bodyExp =
  let nonEmptyListOfListsOfNamesAndAssigns =
    List.filter
        (not << List.isEmpty)
        listOfListsOfNamesAndAssigns
  in
  case nonEmptyListOfListsOfNamesAndAssigns of
    [] ->
      bodyExp

    firstLetNamesAndAssigns::laterLetNamesAndAssigns ->
      let oldPrecedingWhitespace = precedingWhitespace bodyExp in
      -- Insure one newline after first let
      let extraWhitespace =
        if String.contains "\n" oldPrecedingWhitespace then "" else "\n"
      in
      -- Limit to one newline for all lets
      let limitedOldPrecedingWhitespace =
        case String.split "\n" oldPrecedingWhitespace |> List.reverse of
          indentation::_ -> "\n" ++ indentation
          []             -> oldPrecedingWhitespace
      in
      let preceedingWs = extraWhitespace ++ limitedOldPrecedingWhitespace in
      let letOrDef = if isTopLevel then Def else Let in
      let wrappedWithLets =
        listOfListsOfNamesAndAssigns
        |> List.foldr
            (\letNamesAndAssigns innerExp ->
              eLetOrDef letOrDef letNamesAndAssigns innerExp
              |> replacePrecedingWhitespace preceedingWs
            )
            (addPrecedingWhitespace extraWhitespace bodyExp)
      in
      wrappedWithLets


pluckFeatureEquationNamed (selectedType, nodeId, featureName) slate locIdToNumberAndLoc =
  let (_, tree) = slate in
  case typeAndNodeIdAndFeatureToEquation (selectedType, nodeId, featureName) tree locIdToNumberAndLoc of
    Just eqn -> Just (featureName, eqn)
    Nothing  -> Nothing


pluckSelectedFeatureEquationsNamed selectedFeatures slate locIdToNumberAndLoc =
  let accumulator typeAndNodeIdAndFeature acc =
    case pluckFeatureEquationNamed typeAndNodeIdAndFeature slate locIdToNumberAndLoc of
      Just (feature, eqn) -> (feature, eqn) :: acc
      Nothing             -> acc
  in
  Set.foldr accumulator [] selectedFeatures


pluckSelectedFeatureEquations selectedFeatures slate locIdToNumberAndLoc =
  List.map Tuple.second <| pluckSelectedFeatureEquationsNamed selectedFeatures slate locIdToNumberAndLoc


locIdToNumberAndLocOf : Exp -> Dict.Dict LocId (Num, Loc)
locIdToNumberAndLocOf exp =
  exp
  |> foldExpViaE__
      (\e__ dict ->
        case e__ of
          EConst _ n (locId, annotation, ident) wd ->
            Dict.insert locId (n, (locId, annotation, ident)) dict
          _ ->
            dict
      )
      Dict.empty


locIdToWidgetDeclOf : Exp -> Dict.Dict LocId WidgetDecl
locIdToWidgetDeclOf exp =
  exp
  |> foldExpViaE__
      (\e__ dict ->
        case e__ of
          EConst _ _ (locId, _, _) wd -> Dict.insert locId wd dict
          _                           -> dict
      )
      Dict.empty


locIdToWidgetDeclLittleOf : Exp -> Dict.Dict LocId String
locIdToWidgetDeclLittleOf exp =
  (locIdToWidgetDeclOf exp)
  |> Dict.map (\locId wd -> LangUnparser.unparseWD wd)


pluckSelectedVals selectedFeatures slate locIdToNumberAndLoc =
  let featureEquations = pluckSelectedFeatureEquations selectedFeatures slate locIdToNumberAndLoc in
  List.concatMap equationVals featureEquations


evaluateFeature typeAndNodeIdAndFeatureName slate locIdToNumberAndLoc =
  let (_, tree) = slate in
  case (typeAndNodeIdAndFeatureToEquation typeAndNodeIdAndFeatureName tree locIdToNumberAndLoc) of
    Just eqn -> ShapeWidgets.evaluateFeatureEquation eqn
    Nothing  -> Nothing


typeAndNodeIdAndFeatureToEquation (selectedType, nodeId, featureName) tree locIdToNumberAndLoc =
  if selectedType == ShapeWidgets.selectedTypeShapeFeature then
    case Dict.get nodeId tree of
      Just (LangSvg.SvgNode kind nodeAttrs _) ->
        Just (ShapeWidgets.featureEquation nodeId kind featureName nodeAttrs)

      Just (LangSvg.TextNode _) ->
        Nothing

      Nothing ->
        Debug.crash <| "typeAndNodeIdAndFeatureToEquation " ++ (toString nodeId) ++ " " ++ (toString tree)
  else if selectedType == ShapeWidgets.selectedTypeWidget then
    -- parse locId from "widget123" feature name
    let locIdStr =
      String.dropLeft (String.length "widget") featureName
    in
    let locId =
      String.toInt locIdStr
      |> Utils.fromOk ("Couldn't parse locId out of " ++ featureName)
    in
    let (n, loc) =
      Utils.justGet_
          ("Couldn't find locId " ++ (toString locId) ++ " in " ++ (toString locIdToNumberAndLoc))
          locId
          locIdToNumberAndLoc
    in
    Just (ShapeWidgets.EqnNum <| (n, TrLoc loc))
  else
    Debug.crash <| "Unknown selected feature type: " ++ selectedType


equationVals eqn =
  case eqn of
    ShapeWidgets.EqnNum val   -> [val]
    ShapeWidgets.EqnOp _ eqns -> List.concatMap equationVals eqns


equationLocs syncOptions eqn =
  List.concatMap (Set.toList << (Sync.locsOfTrace syncOptions) << Tuple.second) (equationVals eqn)


-- Will abort if any op other than + - * /
--
-- Must be linear in the locId solved for.
--
-- Convert to just locIds (variables) and constants
solveForLoc : LocId -> Dict.Dict LocId Num -> Subst -> FeatureEquation -> FeatureEquation -> Maybe LocEquation
solveForLoc locId locIdToNum subst lhs rhs =
  -- Feature equation contains feature operations and trace operations.
  -- Normalize to simple equations on locIds (variables).
  let
    lhs_ = featureEquationToLocEquation lhs
    rhs_ = featureEquationToLocEquation rhs
  in
  let maybeEqn =
    -- Help out the silly simplifier.
    case maybeExtractUnsharedExpression rhs_ lhs_ of
      Nothing ->
        Nothing

      Just (lhs__, rhs__) ->
        -- We will duplicate frozen constants into the little equation
        -- string. Otherwise, math values like 0, 1, 2 get assigned to
        -- variable names.
        let
          lhs___ = constantifyLocs locIdToNum lhs__
          rhs___ = constantifyLocs locIdToNum rhs__
        in
        -- Transform   rhs_ - lhs_ = 0
        -- to          coeff*x^pow + rest = 0
        -- where x is our target loc
        case locEqnTerms locId (LocEqnOp Minus [lhs___, rhs___]) of
          Just (locPow, locCoeff, rest) ->
            if locPow == 0 || locCoeff == LocEqnConst 0 then
              Nothing
            else if locPow == 1 then
              -- We have: coeff*x + rest = 0
              -- We want: x = something
              Just <|
              normalizeSimplify <|
                LocEqnOp Div
                    [ LocEqnOp Minus [LocEqnConst 0, rest]
                    , locCoeff]

            else if locPow == -1 then
              -- We have: coeff/x + rest = 0
              -- We want: x = something
              Just <|
              normalizeSimplify <|
                LocEqnOp Div
                    [ locCoeff
                    , LocEqnOp Minus [LocEqnConst 0, rest]]
            else
              -- Just need to add a pow op and then we can handle more pows.
              Nothing

          Nothing ->
            Nothing
  in
  -- Now check that equation doesn't produce NaN or similar...
  case maybeEqn of
    Just eqn ->
      -- Need the full subst, not just frozen constants.
      let evaled = locEqnEval subst eqn in
      if (isNaN evaled) || (isInfinite evaled)
      then Nothing
      else Just eqn

    Nothing ->
      Nothing


-- Help out our not-so-smart simplifier.
-- If lhs and rhs are identical but for some sub-expression,
-- return just the differing sub-expressions.
maybeExtractUnsharedExpression : LocEquation -> LocEquation -> Maybe (LocEquation, LocEquation)
maybeExtractUnsharedExpression lhs rhs =
  case (lhs, rhs) of
    (LocEqnConst ln, LocEqnConst rn) ->
      if ln == rn
      then Nothing
      else Just (lhs, rhs)

    (LocEqnLoc lLocId, LocEqnLoc rLocId) ->
      if lLocId == rLocId
      then Nothing
      else Just (lhs, rhs)

    (LocEqnOp lOp lChildren, LocEqnOp rOp rChildren) ->
      if lOp /= rOp then
        Just (lhs, rhs)
      else
        if lChildren == rChildren then
          Nothing
        else if List.length(lChildren) /= List.length(rChildren) then -- Not possible in current grammar, but no reason that, say, addition couldn't take 3 or more arguments.
          Just (lhs, rhs)
        else
          let unsharedSubexpressions =
            Utils.zip lChildren rChildren
            |> List.map (\(lChild, rChild) -> maybeExtractUnsharedExpression lChild rChild)
          in
          if List.all ((==) Nothing) unsharedSubexpressions then
            Nothing
          else if Utils.count ((/=) Nothing) unsharedSubexpressions > 1 then
            Just (lhs, rhs)
          else
            -- All but one child is identical between the lhs and rhs
            let justUnsharedSubexpressionPair =
              Utils.findFirst ((/=) Nothing) unsharedSubexpressions
              |> Utils.fromJust_ "extractUnsharedExpression this is logically impossible"
            in
            justUnsharedSubexpressionPair

    _ ->
      Just (lhs, rhs)


-- Turns all traces in the equation into equations on the locs
featureEquationToLocEquation : FeatureEquation -> LocEquation
featureEquationToLocEquation featureEqn =
  case featureEqn of

    -- locId of 0 means it's a constant that's part of the feature equation,
    -- not the program
    ShapeWidgets.EqnNum (n, TrLoc (0, _, _)) ->
      LocEqnConst n

    ShapeWidgets.EqnNum (n, TrLoc (locId, _, _)) ->
      LocEqnLoc locId

    ShapeWidgets.EqnNum (n, TrOp op traces) ->
      LocEqnOp op (List.map traceToLocEquation traces)

    ShapeWidgets.EqnOp op featureEqns ->
      LocEqnOp op (List.map featureEquationToLocEquation featureEqns)


-- Explicitly exclude ellipseRX/ellipseRX
xFeatureNameRegex = Regex.regex "^(?!ellipseR)(.*)X(\\d*)$"
yFeatureNameRegex = Regex.regex "^(?!ellipseR)(.*)Y(\\d*)$"
xOrYFeatureNameRegex = Regex.regex "^(?!ellipseR)(.*)[XY](\\d*)$"

featureNameIsX featureName =
  Regex.contains xFeatureNameRegex featureName

featureNameIsY featureName =
  Regex.contains yFeatureNameRegex featureName

featureNameIsXOrY featureName =
  Regex.contains xOrYFeatureNameRegex featureName

featurePointAndNumber featureName =
  Regex.find (Regex.AtMost 1) xOrYFeatureNameRegex featureName
  |> Utils.head_
  |> (.submatches)

-- Assuming features are already on the same nodeId...
featuresNamesAreXYPairs featureNameA featureNameB =
  (featureNameIsXOrY featureNameA) &&
  (featureNameIsXOrY featureNameB) &&
  (featureNameA /= featureNameB) && -- Not the same feature
  (featurePointAndNumber featureNameA) ==
    (featurePointAndNumber featureNameB) -- But the same point


-- Extract all point x,y features pairs
featurePoints features =
  case features of
    [] ->
      []

    typeAndNodeIdAndFeatureName::otherFeatures ->
      let (selectedType, nodeId, featureName) = typeAndNodeIdAndFeatureName in
      if not <| featureNameIsXOrY featureName then
        featurePoints otherFeatures
      else
        let nodeFeatures = List.filter (((==) nodeId) << Utils.snd3) otherFeatures in
        let maybePairedFeature =
          Utils.findFirst ((featuresNamesAreXYPairs featureName) << Utils.thd3) nodeFeatures
        in
        case maybePairedFeature of
          Just pairedFeature ->
            let pairToReturn =
              if featureNameIsX featureName
              then (typeAndNodeIdAndFeatureName, pairedFeature)
              else (pairedFeature, typeAndNodeIdAndFeatureName)
            in
            let remainingFeatures =
              Utils.removeFirst pairedFeature otherFeatures
            in
            pairToReturn::(featurePoints remainingFeatures)

          Nothing ->
            featurePoints otherFeatures


traceToLittle : SubstStr -> Trace -> String
traceToLittle substStr trace =
  case trace of
    TrLoc (locId, _, _) ->
      case Dict.get locId substStr of
        Just str -> str
        Nothing  -> "?"
    TrOp op childTraces ->
      let childLittleStrs = List.map (traceToLittle substStr) childTraces in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"


traceToExp : Dict.Dict LocId Num -> Dict.Dict LocId Ident -> Trace -> Exp
traceToExp locIdToFrozenNum locIdToIdent trace =
  case trace of
    TrLoc (locId, _, _) ->
      case Dict.get locId locIdToIdent of
        Just ident -> eVar ident
        Nothing    ->
          case Dict.get locId locIdToFrozenNum of
            Just n  -> eConst n (dummyLoc_ frozen)
            Nothing -> eVar ("couldNotFindLocId" ++ toString locId)

    TrOp op childTraces ->
      let childExps = List.map (traceToExp locIdToFrozenNum locIdToIdent) childTraces in
      eOp op childExps


equationToLittle : SubstStr -> FeatureEquation -> String
equationToLittle substStr eqn =
  case eqn of
    ShapeWidgets.EqnNum (n, trace) ->
      let littlizedTrace = traceToLittle substStr trace in
      if littlizedTrace /= "?" then
        littlizedTrace
      else
        -- Constants introduced by the equation (e.g. /2 for midpoint) won't
        -- have a value in subst.
        -- Also, they should be frozen.
        toString n ++ "!"

    ShapeWidgets.EqnOp op childEqns ->
      let childLittleStrs = List.map (equationToLittle substStr) childEqns in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"


equationToExp : Dict.Dict LocId Num -> Dict.Dict LocId Ident -> FeatureEquation -> Exp
equationToExp locIdToFrozenNum locIdToIdent eqn =
  case eqn of
    ShapeWidgets.EqnNum (n, trace) ->
      traceToExp locIdToFrozenNum locIdToIdent trace

    ShapeWidgets.EqnOp op childEqns ->
      let childExps = List.map (equationToExp locIdToFrozenNum locIdToIdent) childEqns in
      eOp op childExps
