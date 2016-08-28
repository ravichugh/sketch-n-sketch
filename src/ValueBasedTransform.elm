--
-- ValueBasedTransform
--
-- Code transformations based on values selected in the output.
--

module ValueBasedTransform where

import Lang exposing (..)
import LangTools exposing (..)
import LangParser2 exposing (parseE, freshen, substOf)
import LangUnparser exposing (unparse, traceToLittle, precedingWhitespace, addPrecedingWhitespace)
import InterfaceModel
import Eval
import Sync
import LocEqn exposing (..)
import Utils
import LangSvg
import Config

import Dict
import Set
import String
import Regex


debugLog = Config.debugLog Config.debugSync


-- Can't just use Trace because we need to introduce
-- constants not found in the program's Subst
type FeatureEquation
  = EqnVal Val
  | EqnOp Op_ (List FeatureEquation)


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
      List.map ((Sync.locsOfTrace syncOptions) << valToTrace) selectedVals
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
            let identOrig  = nonCollidingName baseIdentOrig usedNames in
            let identPrime = nonCollidingName baseIdentPrime usedNames in
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
  let valueStrs =
    let locIdToWidgetDeclLittle =
      locIdToWidgetDeclLittleOf originalExp
    in
    List.map
        (\(locId, annotation, _) ->
          (toString (Utils.justGet_ "ValueBasedTransform.digHole valueStrs" locId subst))
          ++ annotation
          ++ (Utils.justGet_ "ValueBasedTransform.digHole valueStrs widgetDecl" locId locIdToWidgetDeclLittle)
        )
        (List.reverse locsetList)
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
          let featureName' =
            String.join "_" (commonScopeNamesLocsLiftedThrough ++ [featureName])
          in
          (featureName', eqn)
        )
        selectedFeatureEquationsNamed
  in
  let featureNamesWithExpressionStrs =
    let locIdToOrigName =
      Dict.fromList
        <| List.map (\(locId, nameOrig, namePrime) -> (locId, nameOrig))
        <| locIdNameOrigNamePrime
    in
    -- Make sure all constants are frozen in the feature equations.
    let locIdToLittleConst =
      LangParser2.substStrOf originalExp
      |> Dict.map (\_ str -> str ++ "!")
    in
    let substStr =
      Dict.union
          locIdToOrigName
          locIdToLittleConst
    in
    List.map (Utils.mapSnd <| equationToLittle substStr) selectedFeatureEquationsNamedWithScopes
  in
  -- Remove expressions of only one term
  let significantFeatureNamesWithExpressionStrs =
    List.filter
        (\(name, expStr) -> String.contains " " expStr)
        featureNamesWithExpressionStrs
  in
  let featureNames          = List.map fst significantFeatureNamesWithExpressionStrs in
  let featureExpressionStrs = List.map snd significantFeatureNamesWithExpressionStrs in
  let nonCollidingFeatureNames =
    let (newNamesToAvoid, result) =
      List.foldr
          (\featureName (usedNames, result) ->
            let featureName' = nonCollidingName featureName usedNames in
            (
              Set.insert featureName' usedNames,
              featureName'::result
            )
          )
          (namesToAvoid, [])
          featureNames
    in
    result
  in
  let listOfListsOfNamesAndAssigns =
    [ Utils.zip origNames valueStrs
    , Utils.zip nonCollidingFeatureNames featureExpressionStrs
    , Utils.zip primeNames origNames
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


makeEqual originalExp selectedFeatures slideNumber movieNumber movieTime syncOptions =
  let equalize exp features =
    makeEqualOverlappingPairs exp features slideNumber movieNumber movieTime syncOptions
  in
  let selectedPoints =
    featurePoints (Set.toList selectedFeatures)
  in
  if 2 * (List.length selectedPoints) == (Set.size selectedFeatures) then
    -- We have only selected x&y of several points.
    -- Make all the selected points overlap, that is: make all the x's equal to
    -- each other and all the y's equal to each other.
    let xFeatures = List.map fst selectedPoints in
    let yFeatures = List.map snd selectedPoints in
    let xsEqualized  = equalize originalExp xFeatures in
    let xysEqualized = equalize xsEqualized yFeatures in
    xysEqualized
  else
    -- We have not selected only x&y of different points.
    -- Equalize all selected attributes naively.
    equalize originalExp (Set.toList selectedFeatures)


-- If given more than two features, run makeEqual_ on each overlapping pair.
makeEqualOverlappingPairs originalExp features slideNumber movieNumber movieTime syncOptions =
  let relateMore exp =
    case features of
      _::remainingFeatues ->
        makeEqualOverlappingPairs exp remainingFeatues slideNumber movieNumber movieTime syncOptions

      _ ->
        -- Shouldn't happen.
        Debug.crash "makeEqualOverlappingPairs relateMore"
  in
  case List.take 2 features of
    [featureA, featureB] ->
      let slateRes =
        Eval.run originalExp
        `Result.andThen` (\(val, _) ->
            LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
          )
      in
      case slateRes of
        Err s -> originalExp
        Ok slate ->
          let maybeNewExp =
            makeEqual_ originalExp featureA featureB slate syncOptions
          in
          case maybeNewExp of
            Just newExp ->
              relateMore newExp

            Nothing ->
              relateMore originalExp

    _ ->
      originalExp


makeEquidistant originalExp selectedFeatures slideNumber movieNumber movieTime slate syncOptions =
  let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
  let features = Set.toList selectedFeatures in
  let evaluatedFeatures =
    features
    |> List.map (\feature -> evaluateFeature feature slate locIdToNumberAndLoc)
  in
  if List.all ((/=) Nothing) evaluatedFeatures then
    let sortedFeatures =
      features
      |> List.sortBy (\feature -> Utils.fromJust <| evaluateFeature feature slate locIdToNumberAndLoc)
    in
    makeEquidistantOverlappingTriples originalExp sortedFeatures slideNumber movieNumber movieTime slate syncOptions locIdToNumberAndLoc
  else
    originalExp


makeEquidistantOverlappingTriples originalExp sortedFeatures slideNumber movieNumber movieTime slate syncOptions locIdToNumberAndLoc =
  let relateMore exp =
    case sortedFeatures of
      -- If there's at least 3 more features...
      _::featureB::featureC::featureD::otherFeatures ->
        let newSlateRes =
          Eval.run exp
          `Result.andThen` (\(val, _) ->
              LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
            )
        in
        case newSlateRes of
          Err s -> exp
          Ok newSlate ->
            let newLocIdToNumberAndLoc = locIdToNumberAndLocOf exp in
            makeEquidistantOverlappingTriples
                exp
                (featureB::featureC::featureD::otherFeatures)
                slideNumber
                movieNumber
                movieTime
                newSlate
                syncOptions
                newLocIdToNumberAndLoc

      _ ->
        exp
  in
  case List.take 3 sortedFeatures of
    [featureA, featureB, featureC] ->
      let maybeNewExp =
        let (_, tree) = slate in
        let maybeAEqn = typeAndNodeIdAndFeatureToEquation featureA tree locIdToNumberAndLoc in
        let maybeBEqn = typeAndNodeIdAndFeatureToEquation featureB tree locIdToNumberAndLoc in
        let maybeCEqn = typeAndNodeIdAndFeatureToEquation featureC tree locIdToNumberAndLoc in
        case (maybeAEqn, maybeBEqn, maybeCEqn) of
          (Just aEqn, Just bEqn, Just cEqn) ->
            let distanceAB = EqnOp Minus [bEqn, aEqn] in
            let distanceBC = EqnOp Minus [cEqn, bEqn] in
            makeEqual__ originalExp distanceAB distanceBC syncOptions

          _ -> Nothing
      in
      case maybeNewExp of
        Just newExp ->
          relateMore newExp

        Nothing ->
          relateMore originalExp

    _ ->
      originalExp


makeEqual_ originalExp featureA featureB slate syncOptions =
  let (_, tree) = slate in
  let locIdToNumberAndLoc = locIdToNumberAndLocOf originalExp in
  case (typeAndNodeIdAndFeatureToEquation featureA tree locIdToNumberAndLoc,
        typeAndNodeIdAndFeatureToEquation featureB tree locIdToNumberAndLoc) of
    (Nothing, _) ->
      Nothing

    (_, Nothing) ->
      Nothing

    (Just featureAEqn,
     Just featureBEqn) ->
       makeEqual__ originalExp featureAEqn featureBEqn syncOptions


makeEqual__ originalExp featureAEqn featureBEqn syncOptions =
  let unfrozenLocset =
    Set.fromList <|
      (equationLocs syncOptions featureAEqn) ++
      (equationLocs syncOptions featureBEqn)
  in
  let subst = substOf originalExp in
  let frozenLocIdToNum =
    ((frozenLocIdsAndNumbers originalExp) ++
     (frozenLocIdsAndNumbers LangParser2.prelude))
    |> Dict.fromList
  in
  let findSolution locs =
    case locs of
      [] ->
        Nothing

      (locId, _, _)::rest ->
        case solveForLoc locId frozenLocIdToNum subst featureAEqn featureBEqn of
          Nothing ->
            findSolution rest

          Just resultLocEqn ->
            Just (locId, resultLocEqn)
  in
  -- Prefer to solve for ?-annotated locs
  let thawedLocsFirst =
    let (thawed, others) =
      unfrozenLocset
      |> Set.toList
      |> List.partition (\(_, annotation, _) -> annotation == Lang.thawed)
    in
    thawed ++ others
  in
  case findSolution thawedLocsFirst of
    Nothing ->
      Nothing

    Just (dependentLocId, resultLocEqn) ->
      let locIdSet = Set.insert dependentLocId <| locEqnLocIds resultLocEqn in
      -- Consequently, we don't need to dig out higher than the frozen locs.
      let locsetToDig = Set.filter (\(locId, _, _) -> Set.member locId locIdSet) unfrozenLocset in
      let subst = substOf originalExp in
      let commonScope =
        deepestCommonScope originalExp locsetToDig syncOptions
      in
      let existingNames = identifiersSet originalExp in
      let (dependentLocset, independentLocset) =
        Set.partition (\(locId, _, _) -> locId == dependentLocId) locsetToDig
      in
      let dependentLoc    = dependentLocset   |> Set.toList |> Utils.head_ in
      let independentLocs = independentLocset |> Set.toList in
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
                    nonCollidingName (baseIdent ++ "'") usedNames
                  else
                    if scopesAndBaseIdent == baseIdent
                    then nonCollidingName (baseIdent ++ "_orig") usedNames
                    else nonCollidingName scopesAndBaseIdent usedNames
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
              Utils.justGet_ "ValueBasedTransform.makeEqual__ independentLocNames" locId locIdToNewName
            )
            independentLocIds
      in
      let independentLocValueStrs =
        let locIdToWidgetDeclLittle =
          locIdToWidgetDeclLittleOf originalExp
        in
        List.map
            (\(locId, annotation, _) ->
              (toString (Utils.justGet_ "ValueBasedTransform.makeEqual__ independentLocValueStrs" locId subst))
              ++ annotation
              ++ (Utils.justGet_ "ValueBasedTransform.makeEqual__ independentLocValueStrs widgetDecl" locId locIdToWidgetDeclLittle)
            )
            independentLocs
      in
      let dependentLocNameStr  =
        Utils.justGet_ "ValueBasedTransform.makeEqual__ dependentLocNameStr" dependentLocId locIdToNewName
      in
      let frozenLocIdToLittle =
        Dict.map (\locId n -> (toString n) ++ "!") frozenLocIdToNum
      in
      let locIdToLittle =
        Dict.union
            locIdToNewName
            frozenLocIdToLittle
      in
      let _ = debugLog "locIdToLittle" locIdToLittle in
      let dependentLocValueStr = locEqnToLittle locIdToLittle resultLocEqn in
      let listOfListsOfNamesAndAssigns =
        [ Utils.zip independentLocNames independentLocValueStrs
        , [(dependentLocNameStr, dependentLocValueStr)]
        ]
      in
      let newExp =
        variableifyConstantsAndWrapTargetExpWithLets
            locIdToNewName
            listOfListsOfNamesAndAssigns
            commonScope
            originalExp
      in
      Just newExp


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
  let locsAncestorScopes = List.map (List.map snd) locsAncestorScopesWithParents in
  let deepestCommonScope =
    Utils.last_
    <| exp :: (Utils.commonPrefix locsAncestorScopes)
  in
  deepestCommonScope


-- If suggestedName is not in existing names, returns it.
-- Otherwise appends a number (starting at 2) that doesn't collide.
nonCollidingName : Ident -> Set.Set Ident -> Ident
nonCollidingName suggestedName existingNames =
  if not (Set.member suggestedName existingNames) then
    suggestedName
  else
    let nonCollidingName i =
      let newName = suggestedName ++ (toString i) in
      if not (Set.member newName existingNames)
      then newName
      else nonCollidingName (i+1)
    in
    nonCollidingName 2


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
  let _ = debugLog "wrappedTargetExp" <| unparse wrappedTargetExp in
  let newProgram =
    replaceExpNode targetExp wrappedTargetExp program
    |> freshen
  in
  newProgram


-- Given [ [("a","4"), ("b","5")], [("c", "6")] ] 'body exp'
--
-- Produces an Exp of:
--
-- (let [a c] [4 5]
-- (let [c] [6]
--   'body exp'))
--
wrapWithLets : List (List (String, String)) -> Bool -> Exp -> Exp
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
      let templateStr =
        let letOrDef patsStr assignsStr body =
            if isTopLevel then
              "(def ["++patsStr++"] ["++assignsStr++"])"
              ++ body
            else
              "(let ["++patsStr++"] ["++assignsStr++"]"
              ++ body ++ ")"
        in
        let letStr precedingWs letNamesAndAssigns body =
          let patStrs    = List.map fst letNamesAndAssigns in
          let assignStrs = List.map snd letNamesAndAssigns in
          let patsStr    = String.join " " patStrs in
          let assignsStr = String.join " " assignStrs in
          precedingWs ++ (letOrDef patsStr assignsStr body)
        in
        let letsStr body =
          -- This is like a monad, right? Composing functions with foldl to
          -- make a super-function?
          let superWrapper =
            List.foldl
                (\letNamesAndAssigns letsFunc ->
                  let preceedingWs = extraWhitespace ++ limitedOldPrecedingWhitespace in
                  letsFunc << (letStr preceedingWs letNamesAndAssigns)
                )
                -- Don't require newline before first let.
                (letStr limitedOldPrecedingWhitespace firstLetNamesAndAssigns)
                laterLetNamesAndAssigns
          in
          superWrapper body
        in
        letsStr "\n  'dummy body'"
      in
      let template =
        case parseE templateStr of
          Ok templateExp -> templateExp
          Err (err, _)   -> Debug.crash <| "Dig template err: " ++ err ++ "\n\n" ++ templateStr
      in
      -- Finish by replacing the dummy body:
      let wrappedWithLets =
        mapExpViaExp__
            (\e__ ->
              case e__ of
                EBase _ (EString _ "dummy body") -> (addPrecedingWhitespace extraWhitespace bodyExp).val.e__
                _                                -> e__
            )
            template
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
  List.map snd <| pluckSelectedFeatureEquationsNamed selectedFeatures slate locIdToNumberAndLoc


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
    Just eqn -> evaluateFeatureEquation eqn
    Nothing  -> Nothing


evaluateFeatureEquation eqn =
  case eqn of
    EqnVal val ->
      case val.v_ of
        VConst (n, _) ->
          Just n

        _ ->
          Debug.crash <| "Found feature equation with a value other than a VConst: " ++ (toString val) ++ "\nin: " ++ (toString eqn)

    EqnOp op [left, right] ->
      let maybePerformBinop op =
        let maybeLeftResult = evaluateFeatureEquation left in
        let maybeRightResult = evaluateFeatureEquation right in
        case (maybeLeftResult, maybeRightResult) of
          (Just leftResult, Just rightResult) -> Just (op leftResult rightResult)
          _                                   -> Nothing
      in
      case op of
        Plus  -> maybePerformBinop (+)
        Minus -> maybePerformBinop (-)
        Mult  -> maybePerformBinop (*)
        Div   -> maybePerformBinop (/)
        _     -> Nothing

    _ -> Nothing


typeAndNodeIdAndFeatureToEquation (selectedType, nodeId, featureName) tree locIdToNumberAndLoc =
  if selectedType == LangSvg.selectedTypeShapeFeature then
    case Dict.get nodeId tree of
      Just (LangSvg.SvgNode kind nodeAttrs _) ->
        Just (featureEquation nodeId kind featureName nodeAttrs)

      Just (LangSvg.TextNode _) ->
        Nothing

      Nothing ->
        Debug.crash <| "typeAndNodeIdAndFeatureToEquation " ++ (toString nodeId) ++ " " ++ (toString tree)
  else if selectedType == LangSvg.selectedTypeWidget then
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
    Just (EqnVal <| vConst (n, TrLoc loc))
  else
    Debug.crash <| "Unknown selected feature type: " ++ selectedType


equationVals eqn =
  case eqn of
    EqnVal val   -> [val]
    EqnOp _ eqns -> List.concatMap equationVals eqns


equationLocs syncOptions eqn =
  List.concatMap (Set.toList << (Sync.locsOfTrace syncOptions) << valToTrace) (equationVals eqn)


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
    lhs' = featureEquationToLocEquation lhs
    rhs' = featureEquationToLocEquation rhs
  in
  let maybeEqn =
    -- Help out the silly simplifier.
    case maybeExtractUnsharedExpression rhs' lhs' of
      Nothing ->
        Nothing

      Just (lhs'', rhs'') ->
        -- We will duplicate frozen constants into the little equation
        -- string. Otherwise, math values like 0, 1, 2 get assigned to
        -- variable names.
        let
          lhs''' = constantifyLocs locIdToNum lhs''
          rhs''' = constantifyLocs locIdToNum rhs''
        in
        -- Transform   rhs' - lhs' = 0
        -- to          coeff*x^pow + rest = 0
        -- where x is our target loc
        case locEqnTerms locId (LocEqnOp Minus [lhs''', rhs''']) of
          Just (locPow, locCoeff, rest) ->
            if locPow == 0 || locCoeff == LocEqnConst 0 then
              Nothing
            else if locPow == 1 then
              -- We have: coeff*x + rest = 0
              -- We want: x = something
              Just <|
              locEqnSimplify <|
                LocEqnOp Div
                    [ LocEqnOp Minus [LocEqnConst 0, rest]
                    , locCoeff]

            else if locPow == -1 then
              -- We have: coeff/x + rest = 0
              -- We want: x = something
              Just <|
              locEqnSimplify <|
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
    EqnVal val ->
      case val.v_ of
        -- locId of 0 means it's a constant that's part of the feature equation,
        -- not the program
        VConst (n, TrLoc (0, _, _)) ->
          LocEqnConst n

        VConst (n, TrLoc (locId, _, _)) ->
          LocEqnLoc locId

        VConst (n, TrOp op traces) ->
          LocEqnOp op (List.map traceToLocEquation traces)

        _ -> Debug.crash <| "Found feature equation with a value other than a VConst: " ++ (toString val) ++ "\nin: " ++ (toString featureEqn)

    EqnOp op featureEqns ->
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


featureEquation nodeId kind feature nodeAttrs =
  let eqnVal attr = EqnVal <| LangSvg.maybeFindAttr nodeId kind attr nodeAttrs in
  let eqnVal2     = EqnVal <| vConst (2, dummyTrace) in
  let handleRect () =
    let
      right  = EqnOp Plus [eqnVal "x", eqnVal "width"]
      bottom = EqnOp Plus [eqnVal "y", eqnVal "height"]
      -- x + (w/2)
      cx = EqnOp Plus [eqnVal "x", EqnOp Div [eqnVal "width",  eqnVal2]]
      -- y + (h/2)
      cy = EqnOp Plus [eqnVal "y", EqnOp Div [eqnVal "height", eqnVal2]]
    in
    if feature == LangSvg.rectTLX then eqnVal "x"
    else if feature == LangSvg.rectTLY then eqnVal "y"
    else if feature == LangSvg.rectTRX then right
    else if feature == LangSvg.rectTRY then eqnVal "y"
    else if feature == LangSvg.rectBLX then eqnVal "x"
    else if feature == LangSvg.rectBLY then bottom
    else if feature == LangSvg.rectBRX then right
    else if feature == LangSvg.rectBRY then bottom
    else if feature == LangSvg.rectTCX then cx
    else if feature == LangSvg.rectTCY then eqnVal "y"
    else if feature == LangSvg.rectBCX then cx
    else if feature == LangSvg.rectBCY then bottom
    else if feature == LangSvg.rectCLX then eqnVal "x"
    else if feature == LangSvg.rectCLY then cy
    else if feature == LangSvg.rectCRX then right
    else if feature == LangSvg.rectCRY then cy
    else if feature == LangSvg.rectCX then cx
    else if feature == LangSvg.rectCY then cy
    else if feature == LangSvg.rectWidth  then eqnVal "width"
    else if feature == LangSvg.rectHeight then eqnVal "height"
    else Debug.crash <| "Rectangles do not have this feature: " ++ feature
  in
  let handleBox () =
    let
      -- (left + right)/2
      cx = EqnOp Div [EqnOp Plus [eqnVal "LEFT", eqnVal "RIGHT"], eqnVal2]
      -- (top + bottom)/2
      cy = EqnOp Div [EqnOp Plus [eqnVal "TOP", eqnVal "BOT"], eqnVal2]
    in
    if feature == LangSvg.boxTLX then eqnVal "LEFT"
    else if feature == LangSvg.boxTLY then eqnVal "TOP"
    else if feature == LangSvg.boxTRX then eqnVal "RIGHT"
    else if feature == LangSvg.boxTRY then eqnVal "TOP"
    else if feature == LangSvg.boxBLX then eqnVal "LEFT"
    else if feature == LangSvg.boxBLY then eqnVal "BOT"
    else if feature == LangSvg.boxBRX then eqnVal "RIGHT"
    else if feature == LangSvg.boxBRY then eqnVal "BOT"
    else if feature == LangSvg.boxTCX then cx
    else if feature == LangSvg.boxTCY then eqnVal "TOP"
    else if feature == LangSvg.boxBCX then cx
    else if feature == LangSvg.boxBCY then eqnVal "BOT"
    else if feature == LangSvg.boxCLX then eqnVal "LEFT"
    else if feature == LangSvg.boxCLY then cy
    else if feature == LangSvg.boxCRX then eqnVal "RIGHT"
    else if feature == LangSvg.boxCRY then cy
    else if feature == LangSvg.boxCX then cx
    else if feature == LangSvg.boxCY then cy
    else if feature == LangSvg.boxWidth  then
      EqnOp Minus [eqnVal "RIGHT", eqnVal "LEFT"] -- (right - left)
    else if feature == LangSvg.boxHeight then
      EqnOp Minus [eqnVal "BOT", eqnVal "TOP"] -- (bottom - top)
    else Debug.crash <| "Boxes do not have this feature: " ++ feature
  in
  let handleOval () = -- starting by copying handleBox...
    let
      -- (left + right)/2
      cx = EqnOp Div [EqnOp Plus [eqnVal "LEFT", eqnVal "RIGHT"], eqnVal2]
      -- (top + bottom)/2
      cy = EqnOp Div [EqnOp Plus [eqnVal "TOP", eqnVal "BOT"], eqnVal2]
    in
    if feature == LangSvg.ovalTLX then eqnVal "LEFT"
    else if feature == LangSvg.ovalTLY then eqnVal "TOP"
    else if feature == LangSvg.ovalTRX then eqnVal "RIGHT"
    else if feature == LangSvg.ovalTRY then eqnVal "TOP"
    else if feature == LangSvg.ovalBLX then eqnVal "LEFT"
    else if feature == LangSvg.ovalBLY then eqnVal "BOT"
    else if feature == LangSvg.ovalBRX then eqnVal "RIGHT"
    else if feature == LangSvg.ovalBRY then eqnVal "BOT"
    else if feature == LangSvg.ovalTCX then cx
    else if feature == LangSvg.ovalTCY then eqnVal "TOP"
    else if feature == LangSvg.ovalBCX then cx
    else if feature == LangSvg.ovalBCY then eqnVal "BOT"
    else if feature == LangSvg.ovalCLX then eqnVal "LEFT"
    else if feature == LangSvg.ovalCLY then cy
    else if feature == LangSvg.ovalCRX then eqnVal "RIGHT"
    else if feature == LangSvg.ovalCRY then cy
    else if feature == LangSvg.ovalCX then cx
    else if feature == LangSvg.ovalCY then cy
    else if feature == LangSvg.ovalRX then
      EqnOp Div [EqnOp Minus [eqnVal "RIGHT", eqnVal "LEFT"], eqnVal2] -- (right - left) / 2
    else if feature == LangSvg.ovalRY then
      EqnOp Div [EqnOp Minus [eqnVal "BOT", eqnVal "TOP"], eqnVal2] -- (bot - top) / 2
    else Debug.crash <| "Ovals do not have this feature: " ++ feature
  in
  let handleCircle () =
    let
      top    = EqnOp Minus [eqnVal "cy", eqnVal "r"]
      bottom = EqnOp Plus  [eqnVal "cy", eqnVal "r"]
      left   = EqnOp Minus [eqnVal "cx", eqnVal "r"]
      right  = EqnOp Plus  [eqnVal "cx", eqnVal "r"]
    in
    if feature == LangSvg.circleTCX then eqnVal "cx"
    else if feature == LangSvg.circleTCY then top
    else if feature == LangSvg.circleBCX then eqnVal "cx"
    else if feature == LangSvg.circleBCY then bottom
    else if feature == LangSvg.circleCLX then left
    else if feature == LangSvg.circleCLY then eqnVal "cy"
    else if feature == LangSvg.circleCRX then right
    else if feature == LangSvg.circleCRY then eqnVal "cy"
    else if feature == LangSvg.circleCX then eqnVal "cx"
    else if feature == LangSvg.circleCY then eqnVal "cy"
    else if feature == LangSvg.circleR then eqnVal "r"
    else Debug.crash <| "Circles do not have this feature: " ++ feature
  in
  let handleEllipse () =
    let
      top    = EqnOp Minus [eqnVal "cy", eqnVal "ry"]
      bottom = EqnOp Plus  [eqnVal "cy", eqnVal "ry"]
      left   = EqnOp Minus [eqnVal "cx", eqnVal "rx"]
      right  = EqnOp Plus  [eqnVal "cx", eqnVal "rx"]
    in
    if feature == LangSvg.ellipseTCX then eqnVal "cx"
    else if feature == LangSvg.ellipseTCY then top
    else if feature == LangSvg.ellipseBCX then eqnVal "cx"
    else if feature == LangSvg.ellipseBCY then bottom
    else if feature == LangSvg.ellipseCLX then left
    else if feature == LangSvg.ellipseCLY then eqnVal "cy"
    else if feature == LangSvg.ellipseCRX then right
    else if feature == LangSvg.ellipseCRY then eqnVal "cy"
    else if feature == LangSvg.ellipseCX then eqnVal "cx"
    else if feature == LangSvg.ellipseCY then eqnVal "cy"
    else if feature == LangSvg.ellipseRX then eqnVal "rx"
    else if feature == LangSvg.ellipseRY then eqnVal "ry"
    else Debug.crash <| "Ellipses do not have this feature: " ++ feature
  in
  let handleLine () =
    if feature == LangSvg.lineX1 then eqnVal "x1"
    else if feature == LangSvg.lineY1 then eqnVal "y1"
    else if feature == LangSvg.lineX2 then eqnVal "x2"
    else if feature == LangSvg.lineY2 then eqnVal "y2"
    else if feature == LangSvg.lineCX then EqnOp Div [EqnOp Plus [eqnVal "x1", eqnVal "x2"], eqnVal2] -- (x1 + x2) / 2
    else if feature == LangSvg.lineCY then EqnOp Div [EqnOp Plus [eqnVal "y1", eqnVal "y2"], eqnVal2] -- (y1 + y2) / 2
    else Debug.crash <| "Lines do not have this feature: " ++ feature
  in
  let handlePoly () =
    let ptCount = LangSvg.getPtCount nodeAttrs in
    let x i = eqnVal ("x" ++ toString i) in
    let y i = eqnVal ("y" ++ toString i) in
    if String.startsWith LangSvg.polyPtXPrefix feature then
      let iStr = String.dropLeft (String.length LangSvg.polyPtXPrefix) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      x i
    else if String.startsWith LangSvg.polyPtYPrefix feature then
      let iStr = String.dropLeft (String.length LangSvg.polyPtYPrefix) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      y i
    else if String.startsWith LangSvg.polyMidptXPrefix feature then
      let i1Str = String.dropLeft (String.length LangSvg.polyMidptXPrefix) feature in
      let i1    = Utils.fromOk_ <| String.toInt i1Str in
      let i2    = if i1 == ptCount then 1 else i1 + 1 in
      EqnOp Div [EqnOp Plus [(x i1), (x i2)], eqnVal2] -- (x1 + x2) / 2
    else if String.startsWith LangSvg.polyMidptYPrefix feature then
      let i1Str = String.dropLeft (String.length LangSvg.polyMidptYPrefix) feature in
      let i1    = Utils.fromOk_ <| String.toInt i1Str in
      let i2    = if i1 == ptCount then 1 else i1 + 1 in
      EqnOp Div [EqnOp Plus [(y i1), (y i2)], eqnVal2] -- (y1 + y2) / 2
    else Debug.crash <| "Polygons/polylines do not have this feature: " ++ feature
  in
  let handlePath () =
    let x i = eqnVal ("x" ++ toString i) in
    let y i = eqnVal ("y" ++ toString i) in
    if String.startsWith LangSvg.pathPtXPrefix feature then
      let iStr = String.dropLeft (String.length LangSvg.pathPtXPrefix) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      x i
    else if String.startsWith LangSvg.pathPtYPrefix feature then
      let iStr = String.dropLeft (String.length LangSvg.pathPtYPrefix) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      y i
    else Debug.crash <| "Paths do not have this feature: " ++ feature
  in
  if feature == LangSvg.shapeFill then eqnVal "fill"
  else if feature == LangSvg.shapeStroke then eqnVal "stroke"
  else if feature == LangSvg.shapeStrokeWidth then eqnVal "stroke-width"
  else if feature == LangSvg.shapeFillOpacity then
    case (Utils.find_ nodeAttrs "fill").av_ of
      LangSvg.AColorNum (_, Just opacity) -> EqnVal (vConst opacity)
      _                                   -> Debug.crash "featureEquation: fillOpacity"
  else if feature == LangSvg.shapeStrokeOpacity then
    case (Utils.find_ nodeAttrs "stroke").av_ of
      LangSvg.AColorNum (_, Just opacity) -> EqnVal (vConst opacity)
      _                                   -> Debug.crash "featureEquation: strokeOpacity"
  else if feature == LangSvg.shapeRotation then
    let (rot,cx,cy) = LangSvg.toTransformRot <| Utils.find_ nodeAttrs "transform" in
    EqnVal (vConst rot)
  else
    case kind of
      "rect"     -> handleRect ()
      "BOX"      -> handleBox ()
      "OVAL"     -> handleOval ()
      "circle"   -> handleCircle ()
      "ellipse"  -> handleEllipse ()
      "line"     -> handleLine ()
      "polygon"  -> handlePoly ()
      "polyline" -> handlePoly ()
      "path"     -> handlePath ()
      _          -> Debug.crash <| "Shape features not implemented yet: " ++ kind


equationToLittle : SubstStr -> FeatureEquation -> String
equationToLittle substStr eqn =
  case eqn of
    EqnVal val ->
      case val.v_ of
        VConst (n, trace) ->
          let littlizedTrace = traceToLittle substStr trace in
          if littlizedTrace /= "?" then
            littlizedTrace
          else
            -- Constants introduced by the equation (e.g. /2 for midpoint) won't
            -- have a value in subst.
            -- Also, they should be frozen.
            toString n ++ "!"

        _ ->
          "?"

    EqnOp op childEqns ->
      let childLittleStrs = List.map (equationToLittle substStr) childEqns in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"
