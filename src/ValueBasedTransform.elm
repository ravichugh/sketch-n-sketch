--
-- ValueBasedTransform
--
-- Code transformations based on values selected in the output.
--

module ValueBasedTransform where

import Lang exposing (..)
import LangParser2 exposing (parseE, freshen, substOf)
import LangUnparser exposing (unparse, traceToLittle, precedingWhitespace, addPrecedingWhitespace)
import Eval
import Sync
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


-- For solving.
-- The values at the locs are presumed to be non-zero.
type LocEquation
  = LocEqnConst Num
  | LocEqnLoc LocId
  | LocEqnOp Op_ (List LocEquation)


digHole originalExp selectedFeatures slate syncOptions =
  let selectedFeatureEquationsNamed =
    debugLog "selectedFeatureEquations" <|
      pluckSelectedFeatureEquationsNamed selectedFeatures slate
  in
  let selectedVals =
    debugLog "selectedVals" <|
      pluckSelectedVals selectedFeatures slate
  in
  let tracesLocsets =
    List.map ((Sync.locsOfTrace syncOptions) << valToTrace) selectedVals
  in
  let locset =
    List.foldl Set.union Set.empty tracesLocsets
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
    List.map
        (\(locId, _, _) ->
          toString (Utils.justGet locId subst)
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
    let substStr =
      Dict.union
          locIdToOrigName
          (LangParser2.substStrOf originalExp)
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


makeEqual originalExp selectedFeatures slideNumber movieNumber movieTime slate syncOptions =
  let equalize exp features =
    makeEqualOverlappingPairs exp features slideNumber movieNumber movieTime slate syncOptions
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
makeEqualOverlappingPairs originalExp features slideNumber movieNumber movieTime slate syncOptions =
  let relateMore exp =
    case features of
      -- If there's at least 2 more features...
      _::featureB::featureC::otherFeatures ->
        let slate =
          let (val, _) = Eval.run exp in
          LangSvg.resolveToIndexedTree slideNumber movieNumber movieTime val
        in
        makeEqualOverlappingPairs
            exp
            (featureB::featureC::otherFeatures)
            slideNumber
            movieNumber
            movieTime
            slate
            syncOptions

      _ ->
        exp
  in
  case List.take 2 features of
    [featureA, featureB] ->
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


makeEqual_ originalExp featureA featureB slate syncOptions =
  case (pluckFeatureEquationNamed featureA slate,
        pluckFeatureEquationNamed featureB slate) of
    (Nothing, _) ->
      Nothing

    (_, Nothing) ->
      Nothing

    (Just (featureAName, featureAEqn),
     Just (featureBName, featureBEqn)) ->
      let unfrozenLocset =
        Set.fromList <|
          (equationLocs syncOptions featureAEqn) ++
          (equationLocs syncOptions featureBEqn)
      in
      let findSolution locs =
        let frozenLocIdToNum = Dict.fromList (frozenLocIdsAndNumbers originalExp) in
        case locs of
          [] ->
            Nothing

          (locId, _, _)::rest ->
            case solveForLoc locId frozenLocIdToNum featureAEqn featureBEqn of
              Nothing ->
                findSolution rest

              Just resultLocEqn ->
                Just (locId, resultLocEqn)
      in
      case findSolution (Set.toList unfrozenLocset) of
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
                (\locId -> Utils.justGet locId locIdToNewName)
                independentLocIds
          in
          let independentLocValueStrs =
            List.map
                (\locId -> toString (Utils.justGet locId subst))
                independentLocIds
          in
          let dependentLocNameStr  = Utils.justGet dependentLocId locIdToNewName in
          let dependentLocValueStr = locEqnToLittle locIdToNewName resultLocEqn in
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
  let locsAncestors = debugLog "locsAncestors" <|
    findAllWithAncestors isLocsetNode exp
  in
  -- isScope needs to see the node's parent...because case statements
  -- produce many scopes out of one expression
  -- The below adds a maybe parent to each node, so we get List (List
  -- (Maybe Exp, Exp))
  let locsAncestorsWithParents = debugLog "locsAncestorsWithParents" <|
    List.map
        (\locAncestors ->
          Utils.zip (Nothing :: (List.map Just locAncestors)) locAncestors
        )
        locsAncestors
  in
  let locsAncestorScopesWithParents = debugLog "locsAncestorScopesWithParents" <|
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
          Err err        -> Debug.crash <| "Dig template err: " ++ err ++ "\n\n" ++ templateStr
      in
      -- Finish by replacing the dummy body:
      let wrappedWithLets =
        mapExpViaExp__
            (\e__ ->
              case e__ of
                EBase _ (String "dummy body") -> (addPrecedingWhitespace extraWhitespace bodyExp).val.e__
                _                             -> e__
            )
            template
      in
      wrappedWithLets


pluckFeatureEquationNamed nodeIdAndFeature slate =
  let (_, tree) = slate in
  let (nodeId, feature) = nodeIdAndFeature in
  case nodeIdAndFeatureToEquation (nodeId, feature) tree of
    Just eqn -> Just (feature, eqn)
    Nothing  -> Nothing


pluckSelectedFeatureEquationsNamed selectedFeatures slate =
  let accumulator nodeIdAndFeature acc =
    case pluckFeatureEquationNamed nodeIdAndFeature slate of
      Just (feature, eqn) -> (feature, eqn) :: acc
      Nothing             -> acc
  in
  Set.foldr accumulator [] selectedFeatures


pluckSelectedFeatureEquations selectedFeatures slate =
  List.map snd <| pluckSelectedFeatureEquationsNamed selectedFeatures slate


pluckSelectedVals selectedFeatures slate =
  let featureEquations = pluckSelectedFeatureEquations selectedFeatures slate in
  List.concatMap equationVals featureEquations


nodeIdAndFeatureToEquation (nodeId, feature) tree =
  case Dict.get nodeId tree of
    Just (LangSvg.SvgNode kind nodeAttrs _) ->
      Just (featureEquation nodeId kind feature nodeAttrs)

    Just (LangSvg.TextNode _) ->
      Nothing

    Nothing ->
      Debug.crash <| "nodeIdAndFeatureToEquation " ++ (toString nodeId) ++ " " ++ (toString tree)


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
solveForLoc : LocId -> Dict.Dict LocId Num -> FeatureEquation -> FeatureEquation -> Maybe LocEquation
solveForLoc locId locIdToNum rhs lhs =
  -- Feature equation contains feature operations and trace operations.
  -- Normalize to simple equations on locIds (variables).
  let
    rhs' = featureEquationToLocEquation rhs
    lhs' = featureEquationToLocEquation lhs
  in
  -- We will duplicate frozen constants into the little equation
  -- string. Otherwise, math values like 0, 1, 2 get assigned to
  -- variable names.
  let
    rhs'' = constantifyLocs locIdToNum rhs'
    lhs'' = constantifyLocs locIdToNum lhs'
  in
  -- Transform   rhs' - lhs' = 0
  -- to          coeff*x^pow + rest = 0
  -- where x is our target loc
  case locEqnTerms locId (LocEqnOp Minus [rhs'', lhs'']) of
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


-- Repeated perform simple simplifications:
-- Remove multiply/divide by 1
-- Remove add or subtract by 0
-- Remove multiply/divide by 0
-- Combine operations on constants
locEqnSimplify : LocEquation -> LocEquation
locEqnSimplify eqn =
  let simplified =
    case eqn of
      LocEqnConst n ->
        eqn

      LocEqnLoc locId ->
        eqn

      LocEqnOp op children ->
        let children' = List.map locEqnSimplify children in
        let eqn' = LocEqnOp op children' in
        case children' of
          [left, right] ->
            case op of
              Plus ->
                case (left, right) of
                  (LocEqnConst 0, _) -> right
                  (_, LocEqnConst 0) -> left
                  (LocEqnConst a,
                   LocEqnConst b)    -> LocEqnConst (a + b)
                  _                  -> eqn'

              Minus ->
                case (left, right) of
                  (_, LocEqnConst 0) -> left
                  -- Double minus to plus
                  (LocEqnConst 0,
                   LocEqnOp Minus [LocEqnConst 0, stuff]) -> stuff
                  (LocEqnConst a,
                   LocEqnConst b)    -> LocEqnConst (a - b)
                  _                  -> eqn'

              Mult ->
                case (left, right) of
                  (LocEqnConst 1, _) -> right
                  (_, LocEqnConst 1) -> left
                  (LocEqnConst 0, _) -> LocEqnConst 0
                  (_, LocEqnConst 0) -> LocEqnConst 0
                  (LocEqnConst a,
                   LocEqnConst b)    -> LocEqnConst (a * b)
                  _                  -> eqn'

              Div ->
                case (left, right) of
                  (_, LocEqnConst 1) -> left
                  -- Division by 0 will be handled elsewhere.
                  -- We don't want to produce infinity here.
                  (LocEqnConst a,
                   LocEqnConst b)    -> if b /= 0 then LocEqnConst (a / b) else eqn'
                  (LocEqnConst 0, _) -> LocEqnConst 0
                  _                  ->
                    -- Alas, this is syntactic equality not semantic.
                    if left == right && right /= LocEqnConst 0 then
                      LocEqnConst 1
                    else
                      eqn'

              _ ->
                eqn'

          _ ->
            Debug.crash <| "locEqnSimplify: op without 2 children " ++ (toString eqn)
  in
  if simplified == eqn then
    eqn
  else
    debugLog "double simplification"
    <| locEqnSimplify simplified


-- Returns (locPow, coefficient of targetLoc, everything else)
--
-- i.e. (coeff eqn)*targetLoc^locPow + (everything else eqn)
--
-- Becaue once in that form, we can solve for the targetLoc.
--
-- Returns Nothing if equation is not linear in LocId
locEqnTerms : LocId -> LocEquation -> Maybe (Int, LocEquation, LocEquation)
locEqnTerms targetLocId eqn =
  case eqn of
    LocEqnConst n ->
      Just (1, LocEqnConst 0, eqn)

    LocEqnLoc locId ->
      if locId == targetLocId
      then Just (1, LocEqnConst 1, LocEqnConst 0)
      else Just (1, LocEqnConst 0, eqn)

    LocEqnOp op children ->
      let children' = List.map (locEqnTerms targetLocId) children in
      let result =
        case children' of
          [Just (leftLocPow,  leftCoeff, leftRest),
           Just (rightLocPow, rightCoeff, rightRest)] ->
            case op of
              Plus ->
                if leftLocPow == rightLocPow then
                  Just (leftLocPow,
                        LocEqnOp Plus [leftCoeff, rightCoeff],
                        LocEqnOp Plus [leftRest, rightRest])
                else
                  -- Not easily solvable, powers of the target loc don't match.
                  Nothing

              Minus ->
                if leftLocPow == rightLocPow then
                  Just (leftLocPow,
                        LocEqnOp Minus [leftCoeff, rightCoeff],
                        LocEqnOp Minus [leftRest, rightRest])
                else
                  -- Not easily solvable, powers of the target loc don't match.
                  Nothing

              Mult ->
                case (leftCoeff, leftRest, rightCoeff, rightRest) of
                  -- Left side doesn't contain target loc
                  (LocEqnConst 0, _, _, _) ->
                    Just (rightLocPow,
                          LocEqnOp Mult [leftRest, rightCoeff],
                          LocEqnOp Mult [leftRest, rightRest])

                  -- Right side doesn't contain target loc
                  (_, _, LocEqnConst 0, _) ->
                    Just (leftLocPow,
                          LocEqnOp Mult [leftCoeff, rightRest],
                          LocEqnOp Mult [leftRest, rightRest])

                  -- Both sides only contain terms of the coeff
                  (_, LocEqnConst 0, _, LocEqnConst 0) ->
                    let newPow = leftLocPow + rightLocPow in
                    if newPow == 0 then
                      Just (1, LocEqnConst 0, LocEqnConst 1)
                    else
                      Just (newPow,
                            LocEqnOp Mult [leftCoeff, rightCoeff],
                            LocEqnConst 0)

                  _ ->
                    -- Equation is too difficult for us :-(
                    Nothing

              Div ->
                -- Division is problematic
                case (leftCoeff, leftRest, rightCoeff, rightRest) of
                  -- Division by 0
                  (_, _, LocEqnConst 0, LocEqnConst 0) ->
                    Nothing

                  -- Denominator doesn't contain target loc,
                  -- simple distribution.
                  (_, _, LocEqnConst 0, _) ->
                    Just (leftLocPow,
                          LocEqnOp Div [leftCoeff, rightRest],
                          LocEqnOp Div [leftRest, rightRest])

                  -- Denominator is some power and coeff of target loc,
                  -- numerator does not contain target loc
                  (LocEqnConst 0, _, LocEqnConst 1, LocEqnConst 0) ->
                    Just (-rightLocPow,
                          LocEqnOp Div [leftRest, rightCoeff],
                          LocEqnConst 0)

                  -- Numerator and denominator are both terms of the target loc
                  (_, LocEqnConst 0, _, LocEqnConst 0) ->
                    if leftLocPow == rightLocPow then
                      Just (1, LocEqnConst 0, LocEqnOp Div [leftCoeff, rightCoeff])
                    else
                      Just (rightLocPow - leftLocPow,
                            LocEqnOp Div [leftCoeff, rightCoeff],
                            LocEqnConst 0)

                  _ ->
                    -- Maybe the numerator and denominator are magically
                    -- syntactically equal.
                    -- (Not smart enough to detect multiples)
                    if leftLocPow == rightLocPow && leftCoeff == rightCoeff && leftRest == rightRest then
                      Just (1, LocEqnConst 0, LocEqnConst 1)
                    else
                      Nothing

              _ ->
                -- Not smart enough to handle anything other than + - * /
                Nothing

          _ ->
            -- Couldn't work out children
            Nothing
        in
        case result of
          Just (newPow, newCoeff, newRest) ->
            Just (newPow, locEqnSimplify newCoeff, locEqnSimplify newRest)

          Nothing ->
            Nothing


locEqnLocIds eqn =
  case eqn of
    LocEqnConst _       -> Set.empty
    LocEqnLoc locId     -> Set.singleton locId
    LocEqnOp _ children ->
      List.foldl
          (\child locs -> Set.union locs <| locEqnLocIds child)
          Set.empty
          children


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


traceToLocEquation : Trace -> LocEquation
traceToLocEquation trace =
  case trace of
    -- locId of 0 means it's a constant that's part of the feature equation,
    -- not the program. These should not be in traces produced by execution.
    TrLoc (0, _, _) ->
      Debug.crash <| "traceToLocEquation: Found locId of 0 in trace. " ++ (toString trace)

    TrLoc (locId, _, _) ->
      LocEqnLoc locId

    TrOp op traces ->
      LocEqnOp op (List.map traceToLocEquation traces)


-- For all locId's in the locIdToNum dictionary, replace
-- corresponding LocEqnLoc nodes with LocEqnConst nodes.
constantifyLocs : Dict.Dict LocId Num -> LocEquation -> LocEquation
constantifyLocs locIdToNum eqn =
  case eqn of
    LocEqnConst n ->
      eqn

    LocEqnLoc locId ->
      case Dict.get locId locIdToNum of
        Just n  -> LocEqnConst n
        Nothing -> eqn

    LocEqnOp op childEqns ->
      LocEqnOp op <| List.map (constantifyLocs locIdToNum) childEqns


locEqnToLittle : Dict.Dict LocId Ident -> LocEquation -> String
locEqnToLittle locIdToLittle eqn =
  case eqn of
    LocEqnConst n ->
      toString n ++ "!"

    LocEqnLoc locId ->
      case Dict.get locId locIdToLittle of
        Just littleStr -> littleStr
        Nothing        -> let _ = (debugLog "missing locId" locId) in "?"

    LocEqnOp op childEqns ->
      let childLittleStrs = List.map (locEqnToLittle locIdToLittle) childEqns in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"



xFeatureNameRegex = Regex.regex "^(.*)X(\\d*)$"
yFeatureNameRegex = Regex.regex "^(.*)Y(\\d*)$"
xOrYFeatureNameRegex = Regex.regex "^(.*)[XY](\\d*)$"

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
        let nodeFeatures = List.filter (((==) nodeId) << fst) otherFeatures in
        let maybePairedFeature =
          Utils.findFirst ((featuresNamesAreXYPairs featureName) << snd) nodeFeatures
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


featureEquation nodeId kind feature nodeAttrs =
  let eqnVal attr = EqnVal <| maybeFindAttr nodeId kind attr nodeAttrs in
  let eqnVal2     = EqnVal <| vConst (2, dummyTrace) in
  let handleRect () =
    if feature == LangSvg.rectTLX then eqnVal "x"
    else if feature == LangSvg.rectTLY then eqnVal "y"
    else if feature == LangSvg.rectTRX then EqnOp Plus [eqnVal "x", eqnVal "width"]
    else if feature == LangSvg.rectTRY then eqnVal "y"
    else if feature == LangSvg.rectBLX then eqnVal "x"
    else if feature == LangSvg.rectBLY then EqnOp Plus [eqnVal "y", eqnVal "height"]
    else if feature == LangSvg.rectBRX then EqnOp Plus [eqnVal "x", eqnVal "width"]
    else if feature == LangSvg.rectBRY then EqnOp Plus [eqnVal "y", eqnVal "height"]
    else if feature == LangSvg.rectCX then EqnOp Plus [eqnVal "x", EqnOp Div [eqnVal "width",  eqnVal2]]  -- x + w/2
    else if feature == LangSvg.rectCY then EqnOp Plus [eqnVal "y", EqnOp Div [eqnVal "height", eqnVal2]] -- y + h/2
    else if feature == LangSvg.rectWidth  then eqnVal "width"
    else if feature == LangSvg.rectHeight then eqnVal "height"
    else Debug.crash <| "Rectangles do not have this feature: " ++ feature
  in
  let handleBox () =
    if feature == LangSvg.boxTLX then eqnVal "LEFT"
    else if feature == LangSvg.boxTLY then eqnVal "TOP"
    else if feature == LangSvg.boxTRX then eqnVal "RIGHT"
    else if feature == LangSvg.boxTRY then eqnVal "TOP"
    else if feature == LangSvg.boxBLX then eqnVal "LEFT"
    else if feature == LangSvg.boxBLY then eqnVal "BOT"
    else if feature == LangSvg.boxBRX then eqnVal "RIGHT"
    else if feature == LangSvg.boxBRY then eqnVal "BOT"
    else if feature == LangSvg.boxCX then EqnOp Div [EqnOp Plus [eqnVal "LEFT", eqnVal "RIGHT"], eqnVal2]  -- (left + right)/2
    else if feature == LangSvg.boxCY then EqnOp Div [EqnOp Plus [eqnVal "TOP", eqnVal "BOT"], eqnVal2] -- (top + bottom)/2
    else if feature == LangSvg.boxWidth  then EqnOp Minus [eqnVal "RIGHT", eqnVal "LEFT"] -- (right - left)
    else if feature == LangSvg.boxHeight then EqnOp Minus [eqnVal "BOT", eqnVal "TOP"] -- (bottom - top)
    else Debug.crash <| "Boxes do not have this feature: " ++ feature
  in
  let handleCircle () =
    if feature == LangSvg.circleCX then eqnVal "cx"
    else if feature == LangSvg.circleCY then eqnVal "cy"
    else if feature == LangSvg.circleR then eqnVal "r"
    else Debug.crash <| "Circles do not have this feature: " ++ feature
  in
  let handleEllipse () =
    if feature == LangSvg.ellipseCX then eqnVal "cx"
    else if feature == LangSvg.ellipseCY then eqnVal "cx"
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
  let handlePolyPath () =
    let ptCount = getPtCount nodeAttrs in
    let x i = eqnVal ("x" ++ toString i) in
    let y i = eqnVal ("y" ++ toString i) in
    if String.startsWith LangSvg.polyPathPtX feature then
      let iStr = String.dropLeft (String.length LangSvg.polyPathPtX) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      x i
    else if String.startsWith LangSvg.polyPathPtY feature then
      let iStr = String.dropLeft (String.length LangSvg.polyPathPtY) feature in
      let i    = Utils.fromOk_ <| String.toInt iStr in
      y i
    else if String.startsWith LangSvg.polyPathMidptX feature then
      let i1Str = String.dropLeft (String.length LangSvg.polyPathMidptX) feature in
      let i1    = Utils.fromOk_ <| String.toInt i1Str in
      let i2    = if i1 == ptCount then 1 else i1 + 1 in
      EqnOp Div [EqnOp Plus [(x i1), (x i2)], eqnVal2] -- (x1 + x2) / 2
    else if String.startsWith LangSvg.polyPathMidptY feature then
      let i1Str = String.dropLeft (String.length LangSvg.polyPathMidptY) feature in
      let i1    = Utils.fromOk_ <| String.toInt i1Str in
      let i2    = if i1 == ptCount then 1 else i1 + 1 in
      EqnOp Div [EqnOp Plus [(y i1), (y i2)], eqnVal2] -- (y1 + y2) / 2
    else Debug.crash <| "Polygons/polylines do not have this feature: " ++ feature
  in
  case kind of
    "rect"     -> handleRect ()
    "BOX"      -> handleBox ()
    "circle"   -> handleCircle ()
    "ellipse"  -> handleEllipse ()
    "line"     -> handleLine ()
    "polygon"  -> handlePolyPath ()
    "polyline" -> handlePolyPath ()
    -- "path"     -> handlePolyPath kind
    _          -> Debug.crash <| "Shape features not implemented yet: " ++ kind


maybeFindAttr_ id kind attr attrs =
  case Utils.maybeFind attr attrs of
    Just aval -> LangSvg.valOfAVal aval
    Nothing   -> Debug.crash <| toString ("RelateAttrs 2", id, kind, attr, attrs)


getXYi attrs si fstOrSnd =
  let i = Utils.fromOk_ <| String.toInt si in
  case Utils.maybeFind "points" attrs of
    Just aval -> case aval.av_ of
      LangSvg.APoints pts -> LangSvg.valOfAVal <| LangSvg.aNum <| fstOrSnd <| Utils.geti i pts
      _                   -> Debug.crash "getXYi 2"
    _ -> Debug.crash "getXYi 1"


maybeFindAttr id kind attr attrs =
  case (kind, String.uncons attr) of
    ("polygon", Just ('x', si)) -> getXYi attrs si fst
    ("polygon", Just ('y', si)) -> getXYi attrs si snd
    _                           -> maybeFindAttr_ id kind attr attrs


getPtCount attrs =
  case Utils.maybeFind "points" attrs of
    Just aval -> case aval.av_ of
      LangSvg.APoints pts -> List.length pts
      _                   -> Debug.crash "getPtCount 2"
    _ -> Debug.crash "getPtCount 1"


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
