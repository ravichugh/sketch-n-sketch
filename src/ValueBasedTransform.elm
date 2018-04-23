--
-- ValueBasedTransform
--
-- Code transformations based on values selected in the output.
--

module ValueBasedTransform exposing (..)

import Lang exposing (..)
import ValUnparser exposing (..)
import LangTools exposing (..)
import LangUtils exposing (..)
import ElmParser as Parser
import LangUnparser exposing (unparseWD, unparseWithIds)
import InterfaceModel
import Eval
import EvalUpdate
import Sync
import LocEqn exposing (..)
import Solver exposing (MathExp(..))
import CodeMotion
import Utils
import LangSvg exposing (NodeId, ShapeKind, Attr)
import ShapeWidgets exposing (FeatureEquation, SelectableFeature(..))
import Config
import Syntax exposing (Syntax)

import Dict
import Set
import String
import Regex


debugLog = Config.debugLog Config.debugSync



digHole : Exp -> Set.Set ShapeWidgets.SelectableFeature -> LangSvg.RootedIndexedTree -> List Widget -> Sync.Options -> Exp
digHole originalExp selectedFeatures ((_, tree) as slate) widgets syncOptions =
  let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
  let featuresWithEquation =
    selectedFeatures
    |> Set.toList
    |> List.filterMap
        (\feature ->
          ShapeWidgets.featureToEquation feature tree widgets locIdToNumberAndLoc
          |> Maybe.map (\eqn -> (feature, eqn))
        )
    |> debugLog "featuresWithEquation"
  in
  -- If any locs are annotated with "?", only dig those.
  let locset =
    let selectedTraces =
      let (_, equations) = List.unzip featuresWithEquation in
      equations
      |> List.concatMap ShapeWidgets.equationNumTrs
      |> List.map Tuple.second
      |> debugLog "selectedTraces"
    in
    let allLocs =
      selectedTraces
      |> List.map (Sync.locsOfTrace syncOptions)
      |> Utils.unionAll
    in
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
  let subst = Parser.substOf originalExp in
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
  let featureNamesWithEquationWithScopes =
    featuresWithEquation
    |> List.map
        (\(feature, eqn) ->
          let featureLocs = equationLocs syncOptions eqn in
          let scopeNamesLocsLiftedThrough =
            List.map
                (scopeNamesLocLiftedThrough commonScope)
                featureLocs
          in
          let commonScopeNamesLocsLiftedThrough =
            Utils.commonPrefix scopeNamesLocsLiftedThrough
          in
          let featureName =
            String.join "_" (commonScopeNamesLocsLiftedThrough ++ [ShapeWidgets.featureDesc feature])
          in
          (featureName, eqn)
        )
  in
  let featureNamesWithExpressionExps =
    let locIdToOrigName =
      Dict.fromList
        <| List.map (\(locId, nameOrig, namePrime) -> (locId, nameOrig))
        <| locIdNameOrigNamePrime
    in
    featureNamesWithEquationWithScopes
    |> List.map (Tuple.mapSecond <| equationToExp subst locIdToOrigName)
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
  InterfaceModel.runAndResolve_ { slideNumber = slideNumber, movieNumber = movieNumber, movieTime = movieTime, syntax = Syntax.Elm } exp -- Syntax is dummy, we throw away unparse code
  |> Result.map (\(val, widgets, slate, code) -> (slate, widgets))


getIndexedLocIdsWithTarget originalExp locsToRevolutionize =
  let subst = Parser.substOf originalExp in
  locsToRevolutionize
  |> List.map (\(locId, frozen, ident) -> (locId, Utils.justGet_ "ValueBasedTransform.stormTheBastille sortedLocs" locId subst))
  |> List.sortBy Tuple.second
  |> Utils.mapi0 (\(i, (locId, targetNum))-> (i, locId, targetNum))


indexLocId = -2


indexedRelateDistanceScore : Subst -> List (Int, LocId, Num) -> MathExp -> Num
indexedRelateDistanceScore subst indexedLocIdsWithTarget mathExp =
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
    |> List.map (\(i, _, target) -> mathExpEval (Dict.insert indexLocId (toFloat i) subst) mathExp - target)
    |> List.map (\distance -> (distance / meanAbsoluteDeviation)^2)
    |> List.sum
  in
  sumOfSquares / toFloat (List.length indexedLocIdsWithTarget)


indexedRelate : Syntax -> Exp -> Set.Set ShapeWidgets.SelectableFeature -> Set.Set NodeId -> Int -> Int -> Float -> Sync.Options -> List InterfaceModel.SynthesisResult
indexedRelate syntax originalExp selectedFeatures selectedShapes slideNumber movieNumber movieTime syncOptions =
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
                let (kind, attrs, _) = LangSvg.justGetSvgNode "ValueBasedTransform.indexedRelate" nodeId slate in
                ShapeWidgets.featuresOfShape nodeId kind attrs
                |> List.take 1
              )
      in
      let locsToRevolutionize =
        let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
        let featureEqns =
          featuresToRevolutionize
          |> List.map (\feature -> ShapeWidgets.featureToEquation feature tree widgets locIdToNumberAndLoc)
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
          if locs == Utils.dedup locs then
            locs
          else
            []
        else
          []
      in
      let subst = Parser.substOf originalExp in
      let indexedLocIdsWithTarget = getIndexedLocIdsWithTarget originalExp locsToRevolutionize in
      let possibleMathExps = stormTheBastille subst indexedLocIdsWithTarget in
      let (_, locIds, targets) = Utils.unzip3 indexedLocIdsWithTarget in
      let locEIds =
        locIds
        |> List.map (\locId -> locIdToEId originalExp locId |> Utils.fromJust_ "ValueBasedTransform.indexedRelate locEIds")
      in
      possibleMathExps
      |> List.map
          (\mathExp ->
            let mathExpLocIds = mathExpLocIdSet mathExp in
            let (locsLifted, locIdToNewName, locIdToVarEId) = CodeMotion.copyLocsSoVisibleTo originalExp mathExpLocIds (Set.fromList locEIds) in
            -- let _ = Utils.log <| "locsLifted:\n" ++ unparseWithIds locsLifted in
            let description =
              let mathExpDesc = Syntax.unparser syntax <| mathExpToExp unann Dict.empty (Dict.insert indexLocId "i" locIdToNewName) mathExp in
              let locDescs = locsToRevolutionize |> List.map (locDescription originalExp) in
              "compute " ++ String.join ", " locDescs ++ " by " ++ mathExpDesc
            in
            let newProgram =
              Utils.zip locIds locEIds
              |> Utils.foldli0
                  (\(i, (locId, originalLocEId)) priorExp ->
                    -- If loc was copied, its original location was replaced with a var, and that's the var we want to replace.
                    let locEId = Dict.get locId locIdToVarEId |> Maybe.withDefault originalLocEId in
                    let mathExpExp = mathExpToExp unann (Dict.singleton indexLocId (toFloat i)) locIdToNewName mathExp in
                    replaceExpNodeE__ByEId locEId mathExpExp.val.e__ priorExp
                  )
                  locsLifted
            in
            let distanceScore = indexedRelateDistanceScore subst indexedLocIdsWithTarget mathExp in
            InterfaceModel.SynthesisResult <|
              { description = description
              , exp         = newProgram
              , isSafe      = True
              , diffs       = []
              , sortKey     = [distanceScore]
              , children    = Nothing
              }
          )


-- Generate loc mathExps that, given 0 1 2 3 etc, approximate the numbers at the given locations
stormTheBastille : Subst -> List (Int, LocId, Num) -> List MathExp
stormTheBastille subst indexedLocIdsWithTarget =
  let (_, locIds, _) = Utils.unzip3 indexedLocIdsWithTarget in
  let locIdsAndIndex = indexLocId::locIds in
  let distanceScore mathExp = indexedRelateDistanceScore subst indexedLocIdsWithTarget mathExp in
  let mathExpsOfSize astSize =
    mathExpsTemplatesOfSize 1 1 astSize -- allowing two constants is taking too long :(
    -- mathExpsTemplatesOfSize 1 2 astSize
    |> List.concatMap (mathExpTemplateLocFillings locIdsAndIndex)
    |> List.map
        (\locsFilledTemplate ->
          -- if atMostNConstants 0 locsFilledTemplate then
          --   locsFilledTemplate
          -- else
          mathExpTemplateConstantFillings littleConstants locsFilledTemplate
          |> List.sortBy distanceScore
          |> Utils.head "ValueBasedTransform.stormTheBastille constantFillingRanking"
        )
    |> List.filter (\mathExp -> distanceScore mathExp < 0.2^2)
    |> List.map normalizeSimplify
    |> List.filter (\mathExp -> mathExpSize mathExp >= astSize) -- Equation was not simplified.
  in
  List.concatMap mathExpsOfSize (List.range 1 5)


type alias PartialSynthesisResult =
  { description : String
  , dependentLocIds : List LocId
  , maybeTermShape : Maybe MathExp
  , exp : Exp
  , removedLocIdToMathExp : List (LocId, MathExp)
  }

type alias SelectedFeatureAndEquation = (ShapeWidgets.SelectableFeature, FeatureEquation)

type RelationToSynthesize
  = Equalize
  | Relate


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
            (List.maximum locLineNums |> Utils.fromJust_ "rankComparedTo1") - (List.minimum locLineNums |> Utils.fromJust_ "rankComparedTo2")
        in
        InterfaceModel.SynthesisResult <|
          { description = description
          , exp         = exp
          , isSafe      = True
          , diffs       = []
          , sortKey     = [removedLocDistance] ++ (locLineNums |> List.map negate |> List.reverse)
          , children    = Nothing
          }
      )


selectedFeaturesToFeaturesAndEquations : Set.Set ShapeWidgets.SelectableFeature -> Exp -> Int -> Int -> Float -> List SelectedFeatureAndEquation
selectedFeaturesToFeaturesAndEquations selectedFeatures program slideNumber movieNumber movieTime =
  case evalToSlateAndWidgetsResult program slideNumber movieNumber movieTime of
    Err s -> []
    Ok ((rootI, tree), widgets) ->
      let locIdToNumberAndLoc = locIdToNumberAndLocOf program in
      selectedFeatures
      |> Set.toList
      |> List.filterMap
          (\selectableFeature ->
            case ShapeWidgets.featureToEquation selectableFeature tree widgets locIdToNumberAndLoc of
              Just featureEqn -> Just (selectableFeature, featureEqn)
              Nothing         -> Debug.crash "Could not generate an equation for " <| toString selectableFeature -- Could make this a Utils.log, we'll see.
          )


makeEqual syntax solutionsCache originalExp selectedFeatures slideNumber movieNumber movieTime syncOptions =
  -- Have to convert to equations early: some transformations may move or create widgets which messes up feature indexing.
  let featuresAndEquations =
    selectedFeaturesToFeaturesAndEquations selectedFeatures originalExp slideNumber movieNumber movieTime
  in
  let relateByPairs priorResults featuresAndEquations =
    -- equalizeOverlappingPairs syntax solutionsCache priorResults featuresAndEquations syncOptions
    let (features, featureEqns) = List.unzip featuresAndEquations in
    let descriptionPrefix = features |> List.map ShapeWidgets.featureDesc |> String.join " = " in
    priorResults
    |> List.concatMap
        (\({description, exp, maybeTermShape, dependentLocIds, removedLocIdToMathExp} as priorResult) ->
          let priorExp = exp in
          relate__ syntax solutionsCache Equalize featureEqns priorExp maybeTermShape removedLocIdToMathExp syncOptions
          |> List.map (InterfaceModel.prependDescription (description ++ " → " ++ descriptionPrefix ++ " "))
          |> List.map (\result -> { result | dependentLocIds = dependentLocIds ++ result.dependentLocIds })
          -- |> List.map (\result -> let _ = if True then Debug.log ("Before:\n" ++ LangUnparser.unparseWithIds priorExp ++ "\nAfter:\n" ++ LangUnparser.unparseWithIds result.exp) () else () in result)
        )

  in
  synthesizeRelationCoordinateWiseAndSortResults
      relateByPairs
      originalExp
      featuresAndEquations


relate syntax solutionsCache originalExp selectedFeatures slideNumber movieNumber movieTime syncOptions =
  -- Have to convert to equations early: some transformations may move or create widgets which messes up feature indexing.
  let featuresAndEquations =
    selectedFeaturesToFeaturesAndEquations selectedFeatures originalExp slideNumber movieNumber movieTime
  in
  let relateOneInTermsOfAllOthers priorResults featuresAndEquations =
    let (_, featureEqns) = List.unzip featuresAndEquations in
    priorResults
    |> List.concatMap
        (\({description, exp, maybeTermShape, dependentLocIds, removedLocIdToMathExp} as priorResult) ->
          let priorExp = exp in
          relate__ syntax solutionsCache Relate featureEqns priorExp maybeTermShape removedLocIdToMathExp syncOptions
          |> List.map (InterfaceModel.prependDescription (description ++ " → "))
          |> List.map (\result -> { result | dependentLocIds = dependentLocIds ++ result.dependentLocIds })
        )
  in
  synthesizeRelationCoordinateWiseAndSortResults
      relateOneInTermsOfAllOthers
      originalExp
      featuresAndEquations


-- Returns list of synthesis results
-- When points selected, relates x's and y's separately.
-- Ranks all results
synthesizeRelationCoordinateWiseAndSortResults
  :  (List PartialSynthesisResult -> List SelectedFeatureAndEquation -> List PartialSynthesisResult)
  -> Exp
  -> List SelectedFeatureAndEquation
  -> List InterfaceModel.SynthesisResult
synthesizeRelationCoordinateWiseAndSortResults doSynthesis originalExp featuresAndEquations =
  let selectedPoints = featurePoints featuresAndEquations in
  let startingResult = { description = "Original", exp = originalExp, maybeTermShape = Nothing, dependentLocIds = [], removedLocIdToMathExp = [] } in
  if 2 * (List.length selectedPoints) == List.length featuresAndEquations then
    -- We have only selected x&y of several points.
    -- Make all the selected points overlap, that is: make all the x's equal to
    -- each other and all the y's equal to each other.
    let (xFeatures, yFeatures) = List.unzip selectedPoints in
    let xsRelated  = doSynthesis [startingResult] xFeatures in
    let xysRelated = doSynthesis xsRelated yFeatures in
    xysRelated
    |> rankComparedTo originalExp
  else
    -- We have not selected only x&y of different points.
    -- Equalize all selected attributes naively.
    doSynthesis [startingResult] featuresAndEquations
    |> rankComparedTo originalExp


-- -- If given more than two features, equalizes each overlapping pair.
-- -- Terminates if given only zero or one feature.
-- equalizeOverlappingPairs : Syntax -> Solver.SolutionsCache -> List PartialSynthesisResult -> List SelectedFeatureAndEquation -> Sync.Options -> List PartialSynthesisResult
-- equalizeOverlappingPairs syntax solutionsCache priorResults featuresAndEquations syncOptions =
--   let equalizeMore results =
--     equalizeOverlappingPairs syntax solutionsCache results (List.drop 1 featuresAndEquations) syncOptions
--   in
--   case featuresAndEquations of
--     (featureA, featureAEqn)::(featureB, featureBEqn)::_ ->
--       priorResults
--       |> List.concatMap
--           (\({description, exp, maybeTermShape, dependentLocIds, removedLocIdToMathExp} as priorResult) ->
--             let priorExp = exp in
--             let descriptionPrefix = ShapeWidgets.featureDesc featureA ++ " = " ++ ShapeWidgets.featureDesc featureB ++ " " in
--             let newResults =
--               relate__ syntax solutionsCache (Equalize featureAEqn featureBEqn) priorExp maybeTermShape removedLocIdToMathExp syncOptions
--               |> List.map (InterfaceModel.prependDescription descriptionPrefix)
--             in
--             case newResults of
--               [] ->
--                 let _ = Debug.log "ValueBasedTransform.equalizeOverlappingPairs: could not equalize" ((featureA, featureAEqn), (featureB, featureBEqn)) in
--                 []
--
--               _ ->
--                 newResults
--                 |> List.map (InterfaceModel.prependDescription (description ++ " → "))
--                 |> List.map (\result -> { result | dependentLocIds = dependentLocIds ++ result.dependentLocIds })
--                 -- |> List.map (\result -> let _ = if True then Debug.log ("Before:\n" ++ LangUnparser.unparseWithIds priorExp ++ "\nAfter:\n" ++ LangUnparser.unparseWithIds result.exp) () else () in result)
--                 |> equalizeMore
--           )
--
--     _ ->
--       priorResults


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
--       |> List.sortBy (\feature -> Utils.fromJust_ "makeEquidistant" <| evaluateFeature feature slate locIdToNumberAndLoc)
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
--           EvalUpdate.run exp |>
--           Result.andThen (\(val, _) ->
--               LangSvg.resolveToRootedIndexedTree slideNumber movieNumber movieTime val
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
--         let maybeAEqn = ShapeWidgets.selectableShapeFeatureToEquation featureA tree locIdToNumberAndLoc in
--         let maybeBEqn = ShapeWidgets.selectableShapeFeatureToEquation featureB tree locIdToNumberAndLoc in
--         let maybeCEqn = ShapeWidgets.selectableShapeFeatureToEquation featureC tree locIdToNumberAndLoc in
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


relate__
    :  Syntax
    -> Solver.SolutionsCache
    -> RelationToSynthesize
    -> List FeatureEquation
    -> Exp
    -> Maybe MathExp
    -> List (LocId, MathExp)
    -> Sync.Options
    -> List PartialSynthesisResult
relate__ syntax solutionsCache relationToSynthesize featureEqns originalExp maybeTermShape removedLocIdToMathExp syncOptions =
  let removedLocIds = List.map Tuple.first removedLocIdToMathExp |> Set.fromList in
  let frozenLocIdToNum =
    ((frozenLocIdsAndNumbers originalExp) ++
     (frozenLocIdsAndNumbers Parser.prelude))
    |> Dict.fromList
  in
  let unfrozenLocset =
    featureEqns
    |> List.concatMap (equationLocs syncOptions)
    |> Set.fromList
    |> Set.filter (\(locId, _, _) -> not <| Set.member locId removedLocIds)
  in
  let unfrozenLocIdSet = Set.map locToLocId unfrozenLocset in
  -- Each equation's unique locs.
  let featureEqnLocIds =
    featureEqns |> List.map (equationLocs syncOptions >> List.map locToLocId >> Set.fromList >> (flip Set.diff) removedLocIds)
  in
  let eqnsUniqueLocIds = Utils.manySetDiffs featureEqnLocIds in -- For each set, subtract all the other sets.
  let subst = Parser.substOf originalExp in
  let featureMathExps =
    featureEqns
    |> List.map (featureEquationToMathExp removedLocIdToMathExp)
  in
  let locCombosToSolveFor =
    case relationToSynthesize of
      Equalize -> unfrozenLocset |> Set.toList |> Utils.combinationsAsSet (List.length featureEqns - 1)
      Relate   -> unfrozenLocset |> Set.toList |> List.map List.singleton
  in
  -- let _ = Utils.log (LangUnparser.unparseWithIds originalExp) in
  let solutionsForLocs dependentLocs =
    case (relationToSynthesize, dependentLocs) of
      (Equalize, _) ->
        -- Make equal ignores termShape.
        let (dependentLocIds, _, _) = Utils.unzip3 dependentLocs in
        let equations =
          featureEqns
          |> List.map (featureEquationToMathExp removedLocIdToMathExp)
          |> Utils.overlappingAdjacentPairs
        in
        let dependentIdentDescs = dependentLocs |> List.map (locDescription originalExp) in
        Solver.solve solutionsCache equations dependentLocIds
        -- |> Debug.log ("solutions for dependentLocIds " ++ toString dependentLocIds)
        |> List.map (\solution -> (solution, "by removing " ++ Utils.toSentence dependentIdentDescs))

      (Relate, [(dependentLocId, _, _) as dependentLoc]) ->
        let dependentIdentDesc = locDescription originalExp dependentLoc in
        -- Solution should be in terms of locs unique to the other equations.
        -- In some cases such cases you could relax this constraint and still get
        -- meaningful relations but that requires smarts that we don't have yet.
        let (independentLocIds, targetMathExp, otherMathExps) =
          case eqnsUniqueLocIds |> Utils.zipi1 |> Utils.findFirst (\(i, eqnUniqueLocs) -> Set.member dependentLocId eqnUniqueLocs) of
            Just (i, _) ->
              let independentLocIds =
                eqnsUniqueLocIds
                |> Utils.removei i
                |> List.concatMap Set.toList
              in
              (independentLocIds, Utils.geti i featureMathExps, Utils.removei i featureMathExps)

            Nothing ->
              -- Loc appears in more than one equation (i.e. does not appear in any equation's unique locs)
              -- Do not try to replace it.
              ([], Utils.head "ValueBasedTransform.relate__ cannot relate but featureEqns shouldn't be empty" featureMathExps, [])
        in
        let targetLocValue = Utils.justGet_ "ValueBasedTransform.relate__ targetLocValue" dependentLocId subst in
        let originalMathExp = targetMathExp in
        let otherReferenceValues =
          otherMathExps
          |> List.map (LocEqn.mathExpEval subst)
        in
        let originalFeatureValue = LocEqn.mathExpEval subst targetMathExp in
        let usesLocFromEachOtherEqn mathExp =
          let mathExpLocIds = mathExpLocIdSet mathExp in
          let eqnsUsedCount =
            eqnsUniqueLocIds |> Utils.count (not << Set.isEmpty << Set.intersect mathExpLocIds)
          in
          eqnsUsedCount == List.length featureMathExps - 1
        in
        let isGoodEnough mathExp =
          if Set.size (mathExpLocIdSet mathExp) == 0 then
            False
          else
            -- Loc replaced must be within 20% of its original value.
            let newValueAtLoc = mathExpEval subst mathExp in
            let valueCloseEnoughToLoc =
              let diff = newValueAtLoc - targetLocValue in
              if targetLocValue == 0
              then diff == 0
              else abs (diff / targetLocValue) < 0.2
            in
            let newFeatureValue = mathExpEval (Dict.insert dependentLocId newValueAtLoc subst) originalMathExp in
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
          if List.length featureMathExps <= 2 then
            littleConstants
          else
            littleConstants |> List.filter (\n -> n <= 10)
        in
        let resultMathExps =
          case maybeTermShape of
            Nothing ->
              -- let maxResults = 10 in
              let synthesizeMore astSize results =
                if False then -- List.length results >= maxResults then
                  results
                else
                  let newMathExps =
                    let minLocsInMathExp = List.length featureMathExps - 1 in
                    mathExpsTemplatesOfSize minLocsInMathExp 1 astSize
                    |> List.concatMap (\template -> mathExpTemplateLocFillings independentLocIds template)
                    |> List.filter usesLocFromEachOtherEqn
                    |> mathExpTemplateFillingsLocsFilled targetLocValue subst possibleEquationConstants
                    |> List.filter isGoodEnough
                    |> List.map normalizeSimplify
                    |> List.filter (\mathExp -> mathExpSize mathExp >= astSize) -- Equation was not simplified. Good. But normalizeSimplify still needs to handle subtraction well which is why the equation can grow in size.
                  in
                  results ++ newMathExps
              in
              List.foldl synthesizeMore [] (List.range 1 7)
              |> Utils.dedup

            Just termShape ->
              let matchesTermShape termShape mathExp =
                case (termShape, mathExp) of
                  (MathNum n1,            MathNum n2)            -> n1 == n2
                  (MathVar featureI,      MathVar locId)         -> featureI == 0 || (featureEqnLocIds |> Utils.findi (Set.member locId) |> Maybe.withDefault 0) == featureI
                  (MathOp op1_ children1, MathOp op2_ children2) -> op1_ == op2_ && (Utils.maybeZip children1 children2 |> Maybe.map (List.all (uncurry matchesTermShape)) |> Maybe.withDefault False)
                  _                                              -> False
              in
              let astSize = mathExpSize termShape in
              mathExpTemplateLocFillings independentLocIds termShape
              |> List.filter (matchesTermShape termShape)
              |> List.filter usesLocFromEachOtherEqn
              |> List.filter isGoodEnough
              |> List.map normalizeSimplify
              |> List.filter (\mathExp -> mathExpSize mathExp >= astSize) -- Equation was not simplified. Good. But normalizeSimplify still needs to handle subtraction well which is why the equation can grow in size.
              |> Utils.dedup
        in
        resultMathExps
        |> List.map (\resultMathExp -> ([(resultMathExp, dependentLocId)], dependentIdentDesc ++ " = "))

      _ ->
        Debug.crash "relate__: Relation type \"Relate\" should only look for an expression for a single loc at a time!"
  in
  locCombosToSolveFor
  |> List.concatMap solutionsForLocs
  |> List.map
      (\(resultMathExpsAndLocIds, description) ->
        let (newProgram, dependentLocExps) =
          resultMathExpsAndLocIds
          |> List.foldl
              (\(resultMathExp, dependentLocId) (programSoFar, dependentLocExpsSoFar) ->
                let independentLocIdSet = Set.intersect (mathExpLocIdSet resultMathExp) unfrozenLocIdSet in
                let dependentEId = locIdToEId originalExp dependentLocId |> Utils.fromJust_ "relate__: dependendLocId locIdToEId" in
                let (programWithLocsLifted, locIdToNewName, _) = CodeMotion.liftLocsSoVisibleTo programSoFar independentLocIdSet (Set.singleton dependentEId) in
                let dependentLocExp =
                  mathExpToExp (if relationToSynthesize == Equalize then frozen else unann) frozenLocIdToNum locIdToNewName resultMathExp
                in
                ( programWithLocsLifted |> replaceExpNode dependentEId dependentLocExp
                , dependentLocExpsSoFar ++ [dependentLocExp]
                )
              )
              (originalExp, [])
        in
        let maybeTermShape =
          case (relationToSynthesize, resultMathExpsAndLocIds) of
            (Relate, [(resultMathExp, _)]) -> -- Relate should always have only 1 dependent loc.
              -- TermShape uses feature index instead of LocId. (As a go-between between the x and y coordinate of a point.)
              let termShape mathExp =
                case mathExp of
                  MathNum _           -> mathExp
                  MathVar locId       -> MathVar (featureEqnLocIds |> Utils.findi (Set.member locId) |> Maybe.withDefault 0)
                  MathOp op_ children -> MathOp op_ (children |> List.map termShape)
              in
              Just (termShape resultMathExp)
            _ ->
              Nothing
        in
        let (_, dependentLocIds) = List.unzip resultMathExpsAndLocIds in
        { description           = description ++ if relationToSynthesize == Equalize then "" else Syntax.unparser syntax (Utils.head "relate__ description" dependentLocExps)
        , exp                   = let _ = Utils.log (Syntax.unparser syntax newProgram) in Parser.freshen newProgram
        , maybeTermShape        = maybeTermShape
        , dependentLocIds       = dependentLocIds
        , removedLocIdToMathExp = removedLocIdToMathExp ++ List.map Utils.flip resultMathExpsAndLocIds
        }
      )


-- Returns synthesis results
-- Build an abstraction where one feature is returned as function of the other selected features.
buildAbstraction syntax program selectedFeatures selectedShapes selectedBlobs slideNumber movieNumber movieTime syncOptions =
  let unparse = Syntax.unparser syntax in
  case InterfaceModel.runAndResolve_ { slideNumber = slideNumber, movieNumber = movieNumber, movieTime = movieTime, syntax = Syntax.Elm } program of -- Syntax is dummy; we ignore unparsed code
    Err s -> []
    Ok (_, widgets, slate, _) ->
      ShapeWidgets.selectionsProximalDistalEIdInterpretations program slate widgets selectedFeatures selectedShapes selectedBlobs
      |> List.map (\interp -> let _ = Utils.log <| String.join " " <| List.map (justFindExpByEId program >> unparse) interp in interp)
      |> List.concatMap (\interpretation ->
        -- 1. Choose an expression to be the output (try all)
        -- In this iteration, it is one of the selected expressions.
        -- Future: perhaps select only arguments and then infer what expression(s)
        -- are calculated based on the selected expressions?
        -- Or vice versa: select only output and infer arguments (similar to DeuceTools.createFunctionFromArgsTool, which is currently limited to converting an existing definition to a function)
        List.range 1 (List.length interpretation)
        |> List.filterMap (\eidI ->
          let (outputEId, otherEIds) = (Utils.geti eidI interpretation, Utils.removei eidI interpretation) in
          -- 2. Arguments: (a) selected patterns and (b) vars free at extracted expression but not at funcLocation
          case otherEIds |> List.map (\otherEId -> findLetAndPatMatchingExpLoose otherEId program) |> Utils.projJusts |> Maybe.map List.unzip of
            Nothing -> Nothing
            Just (_, selectedOtherPatterns) ->
              let
                funcName =
                  nonCollidingName
                      (expNameForEId program outputEId ++ "Func")
                      2
                      (EvalUpdate.visibleIdentifiersAtEIds program (Set.singleton outputEId))
                funcBody = justFindExpByEId program outputEId
                funcLocation = deepestCommonAncestorWithNewline program ((==) funcBody) -- Place function just before abstracted expression
                expEnv = expEnvAt_ program funcLocation.val.eid -- Skip prelude
                -- Assumes no renamings between selected pat and extracted expression
                -- Also assumes selected pat is not part of extracted expression
                varEIdToBindingPat = allVarEIdsToBindingPatList program
                selectedPatsAndChildren = selectedOtherPatterns |> List.concatMap flattenPatTree
                varEIdsCoveredBySelectedPatterns =
                  varEIdToBindingPat
                  |> List.filter (\(varEId, maybePat) -> Just True == (maybePat |> Maybe.map (\pat -> List.member pat selectedPatsAndChildren)))
                  |> List.map Tuple.first
                funcBodyFreeVars = freeVars funcBody
                otherFreeVarsToParameterize =
                  Utils.removeAll funcBodyFreeVars (freeVars funcLocation)
                argPats =
                  selectedOtherPatterns ++
                  ( otherFreeVarsToParameterize
                    |> List.filter (\freeVar -> not <| List.member freeVar.val.eid varEIdsCoveredBySelectedPatterns)
                    |> List.map expToIdent
                    |> Utils.dedup
                    |> List.map pVar
                  )
                call = eCall funcName (List.map (patToExp >> replacePrecedingWhitespace " ") argPats)
                programWithCall =
                  program
                  |> replaceExpNodePreservingPrecedingWhitespace
                      outputEId
                      (call |> setEId outputEId)
                funcLet =
                  newLetFancyWhitespace
                      -1 False
                      (pVar funcName)
                      (eFun (argPats |> setPatListWhitespace "" " ") (funcBody |> unindent |> replacePrecedingWhitespace "\n" |> indent "  "))
                      (justFindExpByEId programWithCall funcLocation.val.eid)
                      programWithCall
                programWithCallAndFunc =
                  program
                  |> replaceExpNode
                      funcLocation.val.eid
                      funcLet
                caption = "Build abstraction of " ++ Utils.squish (unparse call)
                funcBodyFreeIdents = funcBodyFreeVars |> List.map expToIdent |> Set.fromList
                unusedArgs = argPats |> List.filter (\pat -> not <| Utils.anyOverlapListSet (identifiersListInPat pat) funcBodyFreeIdents)
              in
              if List.length unusedArgs == List.length argPats then
                Nothing
              else
                Just (InterfaceModel.synthesisResult caption programWithCallAndFunc |> InterfaceModel.setResultSafe False)
        )
      )


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
  Parser.freshen newProgram


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
  |> Dict.map (\locId wd -> unparseWD wd)


evaluateFeature selectableFeature slate widgets locIdToNumberAndLoc =
  let (_, tree) = slate in
  case (ShapeWidgets.featureToEquation selectableFeature tree widgets locIdToNumberAndLoc) of
    Just eqn -> ShapeWidgets.evaluateFeatureEquation eqn
    Nothing  -> Nothing


equationLocs syncOptions featureEqn =
  ShapeWidgets.equationNumTrs featureEqn
  |> List.concatMap (Tuple.second >> (Sync.locsOfTrace syncOptions) >> Set.toList)
  |> Utils.dedup


allEquationLocs featureEqn =
  ShapeWidgets.equationNumTrs featureEqn
  |> List.concatMap (Tuple.second >> allTraceLocs)


-- Turns all traces in the equation into equations on the locs
-- Also, compensate for locs being removed from the program.
-- We can't remake the features from scratch each round because
-- tranforms could change some widget IDs (e.g. incidentally adding an offset widget).
featureEquationToMathExp : List (LocId, MathExp) -> FeatureEquation  -> MathExp
featureEquationToMathExp removedLocIdToMathExp featureEqn =
  featureEquationToMathExp_ featureEqn
  |> applyRemovedLocIdToMathExp removedLocIdToMathExp


-- Turns all traces in the equation into equations on the locs
featureEquationToMathExp_ : FeatureEquation -> MathExp
featureEquationToMathExp_ featureEqn =
  case featureEqn of

    -- locId of 0 means it's a constant that's part of the feature equation,
    -- not the program
    ShapeWidgets.EqnNum (n, TrLoc (0, _, _)) ->
      MathNum n

    ShapeWidgets.EqnNum (n, TrLoc (locId, _, _)) ->
      MathVar locId

    ShapeWidgets.EqnNum (n, TrOp op traces) ->
      MathOp op (List.map traceToMathExp traces)

    ShapeWidgets.EqnOp op featureEqns ->
      MathOp op (List.map featureEquationToMathExp_ featureEqns)


applyRemovedLocIdToMathExp : List (LocId, MathExp) -> MathExp -> MathExp
applyRemovedLocIdToMathExp removedLocIdToMathExp mathExp =
  -- Have to apply in order to avoid infinite loops.
  removedLocIdToMathExp
  |> List.foldl
      applyRemovedLocIdToMathExp_
      mathExp


applyRemovedLocIdToMathExp_ : (LocId, MathExp) -> MathExp -> MathExp
applyRemovedLocIdToMathExp_ (removedLocId, replacementMathExp) mathExp =
  case mathExp of
    MathNum n         -> mathExp
    MathVar locId     -> if locId == removedLocId then replacementMathExp else mathExp
    MathOp op mathExps -> MathOp op (List.map (applyRemovedLocIdToMathExp_ (removedLocId, replacementMathExp)) mathExps)


-- Extract all point x,y features pairs
featurePoints : List SelectedFeatureAndEquation -> List (SelectedFeatureAndEquation, SelectedFeatureAndEquation)
featurePoints featuresAndEquations =
  case featuresAndEquations of
    [] ->
      []

    selectableFeatureAndEquation::otherFeaturesAndEquations ->
      let (feature, _) = selectableFeatureAndEquation in
      if not <| ShapeWidgets.featureIsXOrY feature then
        featurePoints otherFeaturesAndEquations
      else
        let maybePairedFeatureAndEquation =
          otherFeaturesAndEquations
          |> Utils.findFirst (\(otherFeature, _) -> ShapeWidgets.featuresAreXYPairs feature otherFeature)
        in
        case maybePairedFeatureAndEquation of
          Just pairedFeatureAndEquation ->
            let pairToReturn =
              if ShapeWidgets.featureIsX feature
              then (selectableFeatureAndEquation, pairedFeatureAndEquation)
              else (pairedFeatureAndEquation, selectableFeatureAndEquation)
            in
            let remainingFeatures =
              Utils.removeFirst pairedFeatureAndEquation otherFeaturesAndEquations
            in
            pairToReturn::(featurePoints remainingFeatures)

          Nothing ->
            featurePoints otherFeaturesAndEquations


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
  let locIdToExp = locIdToExpFromFrozenSubstAndNewNames locIdToFrozenNum locIdToIdent in
  case eqn of
    ShapeWidgets.EqnNum (n, trace) ->
      traceToExp locIdToExp trace

    ShapeWidgets.EqnOp op childEqns ->
      let childExps = List.map (equationToExp locIdToFrozenNum locIdToIdent) childEqns in
      eOp op childExps
