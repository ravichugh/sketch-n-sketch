--
-- ValueBasedTransform
--
-- Code transformations based on values selected in the output.
--

module ValueBasedTransform exposing (..)

import Lang exposing (..)
import LangTools exposing (..)
import FastParser exposing (prelude, freshen, substOf)
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



digHole originalExp selectedFeatures slate widgets syncOptions =
  let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
  let selectedFeatureEquationsNamed =
    debugLog "selectedFeatureEquations" <|
      pluckSelectedFeatureEquationsNamed selectedFeatures slate widgets locIdToNumberAndLoc
  in
  -- If any locs are annotated with "?", only dig those.
  let locset =
    let selectedVals =
      debugLog "selectedVals" <|
        pluckSelectedVals selectedFeatures slate widgets locIdToNumberAndLoc
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
  let commonScope = deepestCommonAncestorWithNewlineByLocSet originalExp locset in
  let existingNames = identifiersSet originalExp in
  let locIdNameOrigNamePrime =
    let (_, result) =
      List.foldr
          (\(locId, frozen, ident) (usedNames, result) ->
            let baseIdent = locIdToEId originalExp locId |> Maybe.map (expNameForEId originalExp) |> Maybe.withDefault (if ident == "" then "num" else ident) in
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
        (\(locId, _, _) -> findExpByLocId commonScope locId |> Utils.fromJust_ "ValueBasedTransform.digHole valueExps")
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
    List.map (Tuple.mapSecond <| equationToExp subst locIdToOrigName) selectedFeatureEquationsNamedWithScopes
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



evalToSlateAndWidgetsResult : Exp -> Int -> Int -> Float -> Result String (LangSvg.RootedIndexedTree, Widgets)
evalToSlateAndWidgetsResult exp slideNumber movieNumber movieTime =
  Eval.run exp |>
  Result.andThen
    (\(val, widgets) ->
      LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
      |> Result.map (\tree -> (tree, widgets))
    )


getIndexedLocIdsWithTarget originalExp locsToRevolutionize =
  let subst = substOf originalExp in
  locsToRevolutionize
  |> List.map (\(locId, frozen, ident) -> (locId, Utils.justGet_ "ValueBasedTransform.stormTheBastille sortedLocs" locId subst))
  |> List.sortBy Tuple.second
  |> Utils.mapi0 (\(i, (locId, targetNum))-> (i, locId, targetNum))


indexLocId = -2


indexedRelateDistanceScore : Subst -> List (Int, LocId, Num) -> LocEquation -> Num
indexedRelateDistanceScore subst indexedLocIdsWithTarget locEqn =
  let (_, _, targets) = Utils.unzip3 indexedLocIdsWithTarget in
  let meanAbsoluteDeviation =
    let absDevs =
      Utils.zip targets (List.drop 1 targets)
      |> List.map (\(a, b) -> b - a)
    in
    List.sum absDevs / toFloat (List.length absDevs)
  in
  let sumOfSquares =
    indexedLocIdsWithTarget
    |> List.map (\(i, _, target) -> locEqnEval (Dict.insert indexLocId (toFloat i) subst) locEqn - target)
    |> List.map (\distance -> (distance / meanAbsoluteDeviation)^2)
    |> List.sum
  in
  sumOfSquares / toFloat (List.length indexedLocIdsWithTarget)


indexedRelate : Exp -> Set.Set ShapeWidgets.SelectedShapeFeature -> Set.Set NodeId -> Int -> Int -> Float -> Sync.Options -> List InterfaceModel.SynthesisResult
indexedRelate originalExp selectedFeatures selectedShapes slideNumber movieNumber movieTime syncOptions =
  case evalToSlateAndWidgetsResult originalExp slideNumber movieNumber movieTime of
    Err _    -> []
    Ok (slate, widgets) ->
      let (_, tree) = slate in
      let featuresToRevolutionize =
        if Set.size selectedFeatures > 0 then
          Set.toList selectedFeatures
        else
          selectedShapes
          |> Set.toList
          |> List.concatMap
              (\nodeId ->
                let (kind, attrs) = LangSvg.justGetSvgNode "ValueBasedTransform.indexedRelate" nodeId slate in
                ShapeWidgets.featuresOfShape kind attrs
                |> List.concatMap ShapeWidgets.featureNumsOfFeature
                |> List.map (ShapeWidgets.strFeatureNum kind)
                |> List.take 1
                |> List.map (\featureString -> (nodeId, featureString))
              )
      in
      let locsToRevolutionize =
        let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
        let featureEqns =
          featuresToRevolutionize
          |> List.map (\feature -> nodeIdAndFeatureNameToEquation feature tree widgets locIdToNumberAndLoc)
          |> Utils.projJusts
          |> Maybe.withDefault []
        in
        let isRevolutionizable featureEqn =
          List.length (equationLocs syncOptions featureEqn) == 1
        in
        if List.all isRevolutionizable featureEqns then
          let locs =
            featureEqns
            |> List.concatMap (equationLocs syncOptions)
          in
          if locs == Utils.dedupByEquality locs then
            locs
          else
            []
        else
          []
      in
      let subst = substOf originalExp in
      let indexedLocIdsWithTarget = getIndexedLocIdsWithTarget originalExp locsToRevolutionize in
      let possibleEqns = stormTheBastille subst indexedLocIdsWithTarget in
      let (_, locIds, targets) = Utils.unzip3 indexedLocIdsWithTarget in
      let locEIds =
        locIds
        |> List.map (\locId -> locIdToEId originalExp locId |> Utils.fromJust_ "ValueBasedTransform.liftLocsSoVisibleTo locEIds")
      in
      possibleEqns
      |> List.map
          (\eqn ->
            let eqnLocIds = locEqnLocIdSet eqn in
            let locsToLift =
              locsToRevolutionize
              |> List.filter (\(locId, _, _) -> Set.member locId eqnLocIds)
            in
            let (locsLifted, locIdToNewName) = liftLocsSoVisibleTo originalExp (Set.fromList locsToLift) (Set.fromList locEIds) in
            let description =
              let eqnDesc = unparse <| locEqnToExp unann Dict.empty (Dict.insert indexLocId "i" locIdToNewName) eqn in
              let locDescs = locsToRevolutionize |> List.map (locDescription originalExp) in
              "compute " ++ String.join ", " locDescs ++ " by " ++ eqnDesc
            in
            let newExp =
              locEIds
              |> Utils.foldli0
                  (\(i, locEId) priorExp ->
                    let eqnExp = locEqnToExp unann (Dict.singleton indexLocId (toFloat i)) locIdToNewName eqn in
                    replaceExpNodeE__ByEId locEId eqnExp.val.e__ priorExp
                  )
                  locsLifted
            in
            let distanceScore = indexedRelateDistanceScore subst indexedLocIdsWithTarget eqn in
            InterfaceModel.SynthesisResult <|
              { description = description
              , exp         = newExp
              , isSafe      = True
              , sortKey     = [distanceScore]
              , children    = Nothing
              }
          )


-- Generate loc eqns that, given 0 1 2 3 etc, approximate the numbers at the given locations
stormTheBastille : Subst -> List (Int, LocId, Num) -> List LocEquation
stormTheBastille subst indexedLocIdsWithTarget =
  let (_, locIds, _) = Utils.unzip3 indexedLocIdsWithTarget in
  let locIdsAndIndex = indexLocId::locIds in
  let distanceScore locEqn = indexedRelateDistanceScore subst indexedLocIdsWithTarget locEqn in
  let eqnsOfSize astSize =
    locEqnsTemplatesOfSize 1 1 astSize -- allowing two constants is taking too long :(
    -- locEqnsTemplatesOfSize 1 2 astSize
    |> List.concatMap (locEqnTemplateLocFillings locIdsAndIndex)
    |> List.map
        (\locsFilledTemplate ->
          -- if atMostNConstants 0 locsFilledTemplate then
          --   locsFilledTemplate
          -- else
          locEqnTemplateConstantFillings littleConstants locsFilledTemplate
          |> List.sortBy distanceScore
          |> Utils.head "ValueBasedTransform.stormTheBastille constantFillingRanking"
        )
    |> List.filter (\locEqn -> distanceScore locEqn < 0.2^2)
    |> List.map normalizeSimplify
    |> List.filter (\locEqn -> locEqnSize locEqn >= astSize) -- Equation was not simplified.
  in
  List.concatMap eqnsOfSize (List.range 1 5)


type alias PartialSynthesisResult =
  { description : String
  , dependentLocIds : List LocId
  , maybeTermShape : Maybe LocEquation
  , exp : Exp
  }


type RelationToSynthesize a
  = Equalize a a
  | Relate (List a)


-- Rank synthesis results by:
--
-- 1. Distance between locs removed (less is better)
-- 2. Position in program of locs removed (later is better)
--
-- May want to incorporate thawing into the result ranking.
rankComparedTo : Exp -> List PartialSynthesisResult -> List InterfaceModel.SynthesisResult
rankComparedTo originalExp synthesisResults =
  let isLocId targetLocId exp =
    case exp.val.e__ of
      EConst ws n (locId, frozen, ident) wd -> targetLocId == locId
      _                                     -> False
  in
  synthesisResults
  |> List.map
      (\{description, exp, dependentLocIds} ->
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
        InterfaceModel.SynthesisResult <|
          { description = description
          , exp         = exp
          , isSafe      = True
          , sortKey     = [removedLocDistance] ++ (locLineNums |> List.map negate |> List.reverse)
          , children    = Nothing
          }
      )


makeEqual originalExp selectedFeatures slideNumber movieNumber movieTime syncOptions =
  let relateByPairs priorResults features =
    equalizeOverlappingPairs priorResults features slideNumber movieNumber movieTime syncOptions
  in
  synthesizeRelationCoordinateWiseAndSortResults
      relateByPairs
      originalExp
      selectedFeatures
      slideNumber
      movieNumber
      movieTime
      syncOptions


relate originalExp selectedFeatures slideNumber movieNumber movieTime syncOptions =
  let relateOneInTermsOfAllOthers priorResults features =
    priorResults
    |> List.concatMap
        (\{description, exp, maybeTermShape, dependentLocIds} ->
          let priorExp = exp in
          case evalToSlateAndWidgetsResult priorExp slideNumber movieNumber movieTime of
            Err s -> []
            Ok (slate, widgets) ->
              relate_ (Relate features) priorExp maybeTermShape slate widgets syncOptions
              |> List.map (InterfaceModel.prependDescription (description ++ " → "))
              |> List.map (\result -> { result | dependentLocIds = dependentLocIds ++ result.dependentLocIds })
        )
  in
  synthesizeRelationCoordinateWiseAndSortResults
      relateOneInTermsOfAllOthers
      originalExp
      selectedFeatures
      slideNumber
      movieNumber
      movieTime
      syncOptions

-- Returns list of synthesis results
-- When points selected, relates x's and y's separately.
-- Ranks all results
synthesizeRelationCoordinateWiseAndSortResults
  :  (List PartialSynthesisResult -> List (Int, String) -> List PartialSynthesisResult)
  -> Exp
  -> Set.Set (Int, String)
  -> Int
  -> Int
  -> Num
  -> Sync.Options
  -> List InterfaceModel.SynthesisResult
synthesizeRelationCoordinateWiseAndSortResults doSynthesis originalExp selectedFeatures slideNumber movieNumber movieTime syncOptions =
  let selectedPoints =
    featurePoints (Set.toList selectedFeatures)
  in
  let startingResult = { description = "Original", exp = originalExp, maybeTermShape = Nothing, dependentLocIds = [] } in
  if 2 * (List.length selectedPoints) == (Set.size selectedFeatures) then
    -- We have only selected x&y of several points.
    -- Make all the selected points overlap, that is: make all the x's equal to
    -- each other and all the y's equal to each other.
    let xFeatures = List.map Tuple.first selectedPoints in
    let yFeatures = List.map Tuple.second selectedPoints in
    let xsRelated  = doSynthesis [startingResult] xFeatures in
    let xysRelated = doSynthesis xsRelated yFeatures in
    xysRelated
    |> rankComparedTo originalExp
  else
    -- We have not selected only x&y of different points.
    -- Equalize all selected attributes naively.
    doSynthesis [startingResult] (Set.toList selectedFeatures)
    |> rankComparedTo originalExp


-- If given more than two features, run relate_ on each overlapping pair.
equalizeOverlappingPairs priorResults features slideNumber movieNumber movieTime syncOptions =
  let equalizeMore results =
    case features of
      _::remainingFeatues ->
        equalizeOverlappingPairs results remainingFeatues slideNumber movieNumber movieTime syncOptions

      _ ->
        -- Shouldn't happen.
        Debug.crash "equalizeOverlappingPairs equalizeMore"
  in
  case List.take 2 features of
    [featureA, featureB] ->
      priorResults
      |> List.concatMap
          (\{description, exp, maybeTermShape, dependentLocIds} ->
            let priorExp = exp in
            case evalToSlateAndWidgetsResult priorExp slideNumber movieNumber movieTime of
              Err s -> []
              Ok (slate, widgets) ->
                let newResults =
                  relate_ (Equalize featureA featureB) priorExp maybeTermShape slate widgets syncOptions
                in
                case newResults of
                  [] ->
                    equalizeMore [{description = description, exp = priorExp, maybeTermShape = maybeTermShape, dependentLocIds = dependentLocIds}]

                  _ ->
                    newResults
                    |> List.map (InterfaceModel.prependDescription (description ++ " → "))
                    |> List.map (\result -> { result | dependentLocIds = dependentLocIds ++ result.dependentLocIds })
                    -- |> List.map (\result -> let _ = if True then Debug.log ("Before:\n" ++ LangUnparser.unparseWithIds priorExp ++ "\nAfter:\n" ++ LangUnparser.unparseWithIds result.exp) () else () in result)
                    |> equalizeMore

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
--         let maybeAEqn = nodeIdAndFeatureNameToEquation featureA tree locIdToNumberAndLoc in
--         let maybeBEqn = nodeIdAndFeatureNameToEquation featureB tree locIdToNumberAndLoc in
--         let maybeCEqn = nodeIdAndFeatureNameToEquation featureC tree locIdToNumberAndLoc in
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


relate_
    :  RelationToSynthesize (Int, String)
    -> Exp
    -> Maybe LocEquation
    -> LangSvg.RootedIndexedTree
    -> Widgets
    -> Sync.Options
    -> List PartialSynthesisResult
relate_ relationToSynthesize originalExp maybeTermShape slate widgets syncOptions =
  let (_, tree) = slate in
  let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
  let maybeGetFeatureEquation nodeIdAndFeatureName =
    nodeIdAndFeatureNameToEquation nodeIdAndFeatureName tree widgets locIdToNumberAndLoc
  in
  let featureDescription (nodeId, featureName) tree = featureName in
  case relationToSynthesize of
    Equalize nodeIdAndFeatureNameA nodeIdAndFeatureNameB ->
      case (maybeGetFeatureEquation nodeIdAndFeatureNameA,
            maybeGetFeatureEquation nodeIdAndFeatureNameB) of
        (Just featureAEqn, Just featureBEqn) ->
           let descriptionPrefix =
             featureDescription nodeIdAndFeatureNameA tree ++ " = " ++ featureDescription nodeIdAndFeatureNameB tree ++ " "
           in
           relate__ (Equalize featureAEqn featureBEqn) originalExp maybeTermShape syncOptions
           |> List.map (InterfaceModel.prependDescription descriptionPrefix)

        _ ->
          []

    Relate nodeIdAndFeatureNamePairs ->
      case nodeIdAndFeatureNamePairs |> List.map maybeGetFeatureEquation |> Utils.projJusts of
        Just featureEqns ->
          relate__ (Relate featureEqns) originalExp maybeTermShape syncOptions

        _ ->
          []

relate__
    :  RelationToSynthesize FeatureEquation
    -> Exp
    -> Maybe LocEquation
    -> Sync.Options
    -> List PartialSynthesisResult
relate__ relationToSynthesize originalExp maybeTermShape syncOptions =
  let frozenLocIdToNum =
    ((frozenLocIdsAndNumbers originalExp) ++
     (frozenLocIdsAndNumbers prelude))
    |> Dict.fromList
  in
  let featureEqns =
    case relationToSynthesize of
      Equalize featureAEqn featureBEqn -> [featureAEqn, featureBEqn]
      Relate   featureEqns             -> featureEqns
  in
  let unfrozenLocset =
    featureEqns
    |> List.concatMap (equationLocs syncOptions)
    |> Set.fromList
  in
  let unfrozenLocIdSet = Set.map locToLocId unfrozenLocset in
  -- Each equation's unique locs.
  let featureEqnLocIds =
    featureEqns |> List.map (equationLocs syncOptions >> List.map locToLocId >> Set.fromList)
  in
  let eqnsUniqueLocIds = Utils.manySetDiffs featureEqnLocIds in -- For each set, subtract all the other sets.
  let subst = substOf originalExp in
  let solutionsForLoc dependentLoc =
    let (dependentLocId, dependentFrozen, dependentIdent) = dependentLoc in
    let dependentIdentDesc = locDescription originalExp dependentLoc in
    case relationToSynthesize of
      Equalize featureAEqn featureBEqn ->
        -- Make equal ignores termShape.
        case solveForLoc dependentLocId frozenLocIdToNum subst featureAEqn featureBEqn of
          Nothing ->
            []

          Just resultLocEqn ->
            [(resultLocEqn, "by removing " ++ dependentIdentDesc)]

      Relate _ ->
        -- Solution should be in terms of locs unique to the other equations.
        -- In some cases such cases you could relax this constraint and still get
        -- meaningful relations but that requires smarts that we don't have yet.
        let (independentLocIds, targetFeatureEqn, otherFeatureEqns) =
          case eqnsUniqueLocIds |> Utils.zipi1 |> Utils.findFirst (\(i, eqnUniqueLocs) -> Set.member dependentLocId eqnUniqueLocs) of
            Just (i, _) ->
              let independentLocIds =
                eqnsUniqueLocIds
                |> Utils.removei i
                |> List.concatMap Set.toList
              in
              (independentLocIds, Utils.geti i featureEqns, Utils.removei i featureEqns)

            Nothing ->
              -- Loc appears in more than one equation (i.e. does not appear in any equation's unique locs)
              -- Do not try to replace it.
              ([], Utils.head "ValueBasedTransform.relate__ cannot relate but featureEqns shouldn't be empty" featureEqns, [])
        in
        let targetLocValue = Utils.justGet_ "ValueBasedTransform.relate__ targetLocValue" dependentLocId subst in
        let originalLocEqn =
          featureEquationToLocEquation targetFeatureEqn
        in
        let otherReferenceValues =
          otherFeatureEqns
          |> List.map (ShapeWidgets.evaluateFeatureEquation >> Utils.fromJust_ "ValueBasedTransform.relate__ reference values")
        in
        let originalFeatureValue =
          ShapeWidgets.evaluateFeatureEquation targetFeatureEqn |> Utils.fromJust_ "ValueBasedTransform.relate__ originalFeatureValue"
        in
        let usesLocFromEachOtherEqn locEqn =
          let locEqnLocIds = locEqnLocIdSet locEqn in
          let eqnsUsedCount =
            eqnsUniqueLocIds |> Utils.count (not << Set.isEmpty << Set.intersect locEqnLocIds)
          in
          eqnsUsedCount == List.length featureEqns - 1
        in
        let isGoodEnough locEqn =
          if Set.size (locEqnLocIdSet locEqn) == 0 then
            False
          else
            -- Loc replaced must be within 20% of its original value.
            let newValueAtLoc = locEqnEval subst locEqn in
            let valueCloseEnoughToLoc =
              let diff = newValueAtLoc - targetLocValue in
              if targetLocValue == 0
              then diff == 0
              else abs (diff / targetLocValue) < 0.2
            in
            let newFeatureValue = locEqnEval (Dict.insert dependentLocId newValueAtLoc subst) originalLocEqn in
            -- And difference between evaluated equation and other equations must be within 20% of original difference.
            let equationResultRelativelyCloseEnough =
              otherReferenceValues
              |> List.all
                  (\refVal ->
                    let targetDistance = originalFeatureValue - refVal in
                    let newDistance    = newFeatureValue - refVal in
                    let diff = newDistance - targetDistance in
                    if targetDistance == 0
                    then diff == 0
                    else abs (diff / targetDistance) < 0.2
                  )
            in
            valueCloseEnoughToLoc && equationResultRelativelyCloseEnough
        in
        let possibleEquationConstants =
          if List.length featureEqns <= 2 then
            littleConstants
          else
            littleConstants |> List.filter (\n -> n <= 10)
        in
        let resultEqns =
          case maybeTermShape of
            Nothing ->
              -- let maxResults = 10 in
              let synthesizeMore astSize results =
                if False then -- List.length results >= maxResults then
                  results
                else
                  let newEqns =
                    let minLocsInEqn = List.length featureEqns - 1 in
                    locEqnsTemplatesOfSize minLocsInEqn 1 astSize
                    |> List.concatMap (\template -> locEqnTemplateLocFillings independentLocIds template)
                    |> List.filter usesLocFromEachOtherEqn
                    |> locEqnTemplateFillingsLocsFilled targetLocValue subst possibleEquationConstants
                    |> List.filter isGoodEnough
                    |> List.map normalizeSimplify
                    |> List.filter (\locEqn -> locEqnSize locEqn >= astSize) -- Equation was not simplified. Good. But normalizeSimplify still needs to handle subtraction well which is why the equation can grow in size.
                  in
                  results ++ newEqns
              in
              List.foldl synthesizeMore [] (List.range 1 7)
              |> Utils.dedupByEquality

            Just termShape ->
              let matchesTermShape termShape locEqn =
                case (termShape, locEqn) of
                  (LocEqnConst n1,          LocEqnConst n2)          -> n1 == n2
                  (LocEqnLoc featureI,      LocEqnLoc locId)         -> featureI == 0 || (featureEqnLocIds |> Utils.findi (Set.member locId) |> Maybe.withDefault 0) == featureI
                  (LocEqnOp op1_ children1, LocEqnOp op2_ children2) -> op1_ == op2_ && (Utils.maybeZip children1 children2 |> Maybe.map (List.all (uncurry matchesTermShape)) |> Maybe.withDefault False)
                  _                                                  -> False
              in
              let astSize = locEqnSize termShape in
              locEqnTemplateLocFillings independentLocIds termShape
              |> List.filter (matchesTermShape termShape)
              |> List.filter usesLocFromEachOtherEqn
              |> List.filter isGoodEnough
              |> List.map normalizeSimplify
              |> List.filter (\locEqn -> locEqnSize locEqn >= astSize) -- Equation was not simplified. Good. But normalizeSimplify still needs to handle subtraction well which is why the equation can grow in size.
              |> Utils.dedupByEquality
        in
        resultEqns
        |> List.map (\resultLocEqn -> (resultLocEqn, dependentIdentDesc ++ " = "))
  in
  unfrozenLocset
  |> Set.toList
  |> List.concatMap
      (\dependentLoc ->
        let (dependentLocId, dependentFrozen, dependentIdent) = dependentLoc in
        solutionsForLoc dependentLoc
        |> List.map (\(resultLocEqn, description) ->
            -- We don't need to dig out higher than the frozen locs.
            let independentLocIdSet = Set.intersect (locEqnLocIdSet resultLocEqn) unfrozenLocIdSet in
            let independentLocset = unfrozenLocset |> Set.filter (\(locId, _, _) -> Set.member locId independentLocIdSet) in
            let dependentEId = locIdToEId originalExp dependentLocId |> Utils.fromJust_ "relate__: dependendLocId locIdToEId" in
            let (programWithLocsLifted, locIdToNewName) = liftLocsSoVisibleTo originalExp independentLocset (Set.singleton dependentEId) in
            let dependentLocExp =
              let constantAnnotation =
                case relationToSynthesize of
                  Equalize _ _ -> frozen
                  Relate _     -> unann
              in
              locEqnToExp constantAnnotation frozenLocIdToNum locIdToNewName resultLocEqn
            in
            let newProgram =
              programWithLocsLifted
              |> replaceExpNode dependentEId dependentLocExp
              |> freshen
            in
            -- TermShape uses feature index instead of LocId. (As a go-between between the x and y coordinate of a point.)
            let termShape locEqn =
              case locEqn of
                LocEqnConst _         -> locEqn
                LocEqnLoc locId       -> LocEqnLoc (featureEqnLocIds |> Utils.findi (Set.member locId) |> Maybe.withDefault 0)
                LocEqnOp op_ children -> LocEqnOp op_ (children |> List.map termShape)
            in
            case relationToSynthesize of
              Equalize _ _ -> {description = description,                            exp = newProgram, maybeTermShape = Just (termShape resultLocEqn), dependentLocIds = [dependentLocId]}
              Relate _     -> {description = description ++ unparse dependentLocExp, exp = newProgram, maybeTermShape = Just (termShape resultLocEqn), dependentLocIds = [dependentLocId]}
          )
      )


-- TODO: replace usages of this with makeEIdVisibleToEIds
liftLocsSoVisibleTo : Exp -> Set.Set Loc -> Set.Set EId -> (Exp, Dict.Dict LocId Ident)
liftLocsSoVisibleTo originalExp mobileLocset observerEIds =
  let isPredecessor exp =
    let isMobileLoc =
      case exp.val.e__ of
        EConst ws n loc wd -> Set.member loc mobileLocset
        _                  -> False
    in
    isMobileLoc || Set.member exp.val.eid observerEIds
  in
  let commonScope = deepestCommonAncestorWithNewline originalExp isPredecessor in
  let locs = Set.toList mobileLocset in
  let locIds = List.map (\(locId, _, _) -> locId) locs in
  let locEIds =
    locIds
    |> List.map (\locId -> locIdToEId originalExp locId |> Utils.fromJust_ "ValueBasedTransform.liftLocsSoVisibleTo locEIds")
  in
  let eids = Set.union (Set.fromList locEIds) observerEIds in
  let existingNames = visibleIdentifiersAtEIds originalExp eids in
  let (_, locIdToNewName) =
    List.foldr
        (\(locId, _, ident) (usedNames, locIdToNewName) ->
          let baseIdent = locIdToEId originalExp locId |> Maybe.map (expNameForEId originalExp) |> Maybe.withDefault (if ident == "" then "num" else ident) in
          let scopeNamesLiftedThrough = scopeNamesLocLiftedThrough commonScope (locId, frozen, ident) in
          let scopesAndBaseIdent = String.join "_" (scopeNamesLiftedThrough ++ [baseIdent]) in
          let ident =
            if scopesAndBaseIdent == baseIdent
            then nonCollidingName (baseIdent ++ "_orig") 2 usedNames
            else nonCollidingName scopesAndBaseIdent 2 usedNames
          in
          (
            Set.insert ident usedNames,
            Dict.insert locId ident locIdToNewName
          )
        )
        (existingNames, Dict.empty)
        (Set.toList mobileLocset)
  in
  let locNames =
    locIds
    |> List.map
        (\locId ->
          Utils.justGet_ "ValueBasedTransform.liftLocsSoVisibleTo locNames" locId locIdToNewName
        )
  in
  let locExps =
    locIds
    |> List.map (\locId -> findExpByLocId commonScope locId |> Utils.fromJust_ "ValueBasedTransform.liftLocsSoVisibleTo locExps")
    |> List.map clearEId
  in
  let listOfListsOfNamesAndAssigns = [ Utils.zip locNames locExps ] in
  let newExp =
    variableifyConstantsAndWrapTargetExpWithLets
        locIdToNewName
        listOfListsOfNamesAndAssigns
        commonScope
        originalExp
  in
  (newExp, locIdToNewName)


deepestCommonAncestorWithNewlineByLocSet : Exp -> LocSet -> Exp
deepestCommonAncestorWithNewlineByLocSet exp locset =
  let isLocsetNode exp =
    case exp.val.e__ of
      EConst ws n loc wd -> Set.member loc locset
      _                  -> False
  in
  deepestCommonAncestorWithNewline exp isLocsetNode


-- Replace consts in targetExp with given variable names
-- Wrap targetExp with given lets
-- Replace targetExp with wrapped version in the program
variableifyConstantsAndWrapTargetExpWithLets locIdToNewName listOfListsOfNamesAndAssigns targetExp program =
  let targetExpReplaced =
    replaceConstsWithVars locIdToNewName targetExp
  in
  let newProgram =
    program
    |> replaceExpNodeE__ByEId targetExp.val.eid targetExpReplaced.val.e__
    |> wrapWithLets
        listOfListsOfNamesAndAssigns
        targetExp.val.eid
  in
  freshen newProgram



pluckFeatureEquationNamed (nodeId, featureName) slate widgets locIdToNumberAndLoc =
  let (_, tree) = slate in
  case nodeIdAndFeatureNameToEquation (nodeId, featureName) tree widgets locIdToNumberAndLoc of
    Just eqn -> Just (featureName, eqn)
    Nothing  -> Nothing


pluckSelectedFeatureEquationsNamed selectedFeatures slate widgets locIdToNumberAndLoc =
  let accumulator typeAndNodeIdAndFeature acc =
    case pluckFeatureEquationNamed typeAndNodeIdAndFeature slate widgets locIdToNumberAndLoc of
      Just (feature, eqn) -> (feature, eqn) :: acc
      Nothing             -> acc
  in
  Set.foldr accumulator [] selectedFeatures


pluckSelectedFeatureEquations selectedFeatures slate widgets locIdToNumberAndLoc =
  List.map Tuple.second <| pluckSelectedFeatureEquationsNamed selectedFeatures slate widgets locIdToNumberAndLoc


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


pluckSelectedVals selectedFeatures slate widgets locIdToNumberAndLoc =
  let featureEquations = pluckSelectedFeatureEquations selectedFeatures slate widgets locIdToNumberAndLoc in
  List.concatMap equationVals featureEquations


evaluateFeature nodeIdAndFeatureName slate widgets locIdToNumberAndLoc =
  let (_, tree) = slate in
  case (nodeIdAndFeatureNameToEquation nodeIdAndFeatureName tree widgets locIdToNumberAndLoc) of
    Just eqn -> ShapeWidgets.evaluateFeatureEquation eqn
    Nothing  -> Nothing


nodeIdAndFeatureNameToEquation : (Int, String) -> LangSvg.IndexedTree -> Widgets -> Dict.Dict LocId (Num, Loc) -> Maybe FeatureEquation
nodeIdAndFeatureNameToEquation (nodeId, featureName) tree widgets locIdToNumberAndLoc =
  if not <| nodeId < -2 then
    -- shape feature
    case Dict.get nodeId tree |> Maybe.map .interpreted of
      Just (LangSvg.SvgNode kind nodeAttrs _) ->
        Just (ShapeWidgets.featureEquation kind featureName nodeAttrs)

      Just (LangSvg.TextNode _) ->
        Nothing

      Nothing ->
        Debug.crash <| "nodeIdAndFeatureNameToEquation " ++ (toString nodeId) ++ " " ++ (toString tree)
  else
    -- widget feature
    -- change to index widgets by position in widget list; then pull feature from widget type
    let widgetId = -nodeId - 2 in -- widget nodeId's are encoded at -2 and count down. (And they are 1-indexed, so actually they start at -3)
    case Utils.maybeGeti1 widgetId widgets of
      Just widget -> Just (ShapeWidgets.widgetFeatureEquation featureName widget locIdToNumberAndLoc)
      Nothing     -> Debug.crash <| "nodeIdAndFeatureNameToEquation can't find widget " ++ (toString widgetId) ++ " " ++ (toString widgets)


equationVals eqn =
  case eqn of
    ShapeWidgets.EqnNum val   -> [val]
    ShapeWidgets.EqnOp _ eqns -> List.concatMap equationVals eqns


equationLocs syncOptions eqn =
  List.concatMap (Set.toList << (Sync.locsOfTrace syncOptions) << Tuple.second) (equationVals eqn)


solveForLoc : LocId -> Dict.Dict LocId Num -> Subst -> FeatureEquation -> FeatureEquation -> Maybe LocEquation
solveForLoc locId locIdToNum subst lhs rhs =
  -- Feature equation contains feature operations and trace operations.
  -- Normalize to simple equations on locIds (variables).
  let
    lhsLocEqn = featureEquationToLocEquation lhs
    rhsLocEqn = featureEquationToLocEquation rhs
  in
  LocEqn.solveForLoc locId locIdToNum subst lhsLocEqn rhsLocEqn


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

    nodeIdAndFeatureName::otherFeatures ->
      let (nodeId, featureName) = nodeIdAndFeatureName in
      if not <| featureNameIsXOrY featureName then
        featurePoints otherFeatures
      else
        let nodeFeatures = List.filter (Tuple.first >> (==) nodeId) otherFeatures in
        let maybePairedFeature =
          Utils.findFirst (Tuple.second >> featuresNamesAreXYPairs featureName) nodeFeatures
        in
        case maybePairedFeature of
          Just pairedFeature ->
            let pairToReturn =
              if featureNameIsX featureName
              then (nodeIdAndFeatureName, pairedFeature)
              else (pairedFeature, nodeIdAndFeatureName)
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
