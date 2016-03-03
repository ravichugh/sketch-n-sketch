--
-- ValueBasedTransform
--
-- Code transformations based on values selected in the output.
--

module ValueBasedTransform where

import Lang exposing (..)
import LangParser2 exposing (parseE, freshen)
import LangUnparser exposing (unparse, traceToLittle, precedingWhitespace, addPrecedingWhitespace)
import Sync
import Utils
import LangSvg
import Config

import Dict
import Set
import String


debugLog = Config.debugLog Config.debugSync


type FeatureEquation
  = EqnVal Val
  | EqnOp Op_ (List FeatureEquation)


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
  let locToNumber =
    let accumulateLocToNumbers exp__ dict =
      case exp__ of
        EConst ws n loc wd ->
          if Set.member loc locset then
            Dict.insert loc n dict
          else
            dict
        _ -> dict
    in
    foldExpViaE__
        accumulateLocToNumbers
        Dict.empty
        originalExp
  in
  let (deepestCommonScopeParent, deepestCommonScope) =
    deepestCommonScopeWithParent originalExp locset syncOptions
  in
  -- Avoid name collisions here
  let existingNames = identifiersSet originalExp in
  let locIdNameOrigNamePrime =
    let (newUsedNames, result) =
      List.foldr
          (\(locId, frozen, ident) (usedNames, result) ->
            let baseIdent = if ident == "" then "k"++(toString locId) else ident in
            let scopeNamesLiftedThrough = scopeNamesLocLiftedThrough deepestCommonScope (locId, frozen, ident) in
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
  let replaceConstsWithVars exp__ =
    case exp__ of
      EConst ws n (locId, frozen, ident) wd ->
        case Dict.get locId locIdToNewName of
          Just newName -> EVar ws newName
          Nothing      -> exp__
      _ -> exp__
  in
  let commonScopeReplaced =
    mapExpViaExp__ replaceConstsWithVars deepestCommonScope
  in
  let newlyWrappedCommonScope =
    let origNames  = List.reverse <| List.map Utils.snd3 locIdNameOrigNamePrime in
    let primeNames = List.reverse <| List.map Utils.thd3 locIdNameOrigNamePrime in
    let valueStrs =
      List.map
          (\loc ->
            toString (Utils.justGet loc locToNumber)
          )
          (List.reverse locsetList)
    in
    let selectedFeatureEquationsNamedWithScopes =
      List.map
          (\(featureName, eqn) ->
            let featureLocs = equationLocs syncOptions eqn in
            let scopeNamesLocsLiftedThrough =
              List.map
                  (scopeNamesLocLiftedThrough deepestCommonScope)
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
    let oldPrecedingWhitespace = precedingWhitespace commonScopeReplaced in
    let extraWhitespace =
      if String.contains "\n" oldPrecedingWhitespace then "" else "\n"
    in
    -- Limit to one newline
    let limitedOldPrecedingWhitespace =
      case String.split "\n" oldPrecedingWhitespace |> List.reverse of
        indentation::_ -> "\n" ++ indentation
        []             -> oldPrecedingWhitespace
    in
    let templateStr =
      let constantOrigNamesStr  = String.join " " origNames in
      let constantPrimeNamesStr = String.join " " primeNames in
      let constantValuesStr     = String.join " " valueStrs in
      let featureNamesStr       = String.join " " nonCollidingFeatureNames in
      let featureExpressionsStr = String.join " " featureExpressionStrs in
      let includeFeatures       = (List.length featureNames) > 0 in
      let letOrDef patsStr assignsStr body =
          if isTopLevel deepestCommonScope originalExp then
            "(def ["++patsStr++"] ["++assignsStr++"])"
            ++ body
          else
            "(let ["++patsStr++"] ["++assignsStr++"]"
            ++ body ++ ")"
      in
      let originalsLet body =
        limitedOldPrecedingWhitespace
        ++ (letOrDef constantOrigNamesStr constantValuesStr body)
      in
      let tracesLet body =
        if includeFeatures then
          extraWhitespace ++ limitedOldPrecedingWhitespace
          ++ (letOrDef featureNamesStr featureExpressionsStr body)
        else
          body
      in
      let primesLet body =
        extraWhitespace ++ limitedOldPrecedingWhitespace
        ++ (letOrDef constantPrimeNamesStr constantOrigNamesStr body)
      in
      originalsLet
      <| tracesLet
      <| primesLet
      <| "\n  'dummy body'"
    in
    let template =
      case parseE templateStr of
        Ok templateExp -> templateExp
        Err err        -> Debug.crash <| "Dig template err: " ++ err
    in
    -- Now replace the dummy body:
    let newLet =
      mapExpViaExp__
          (\e__ ->
            case e__ of
              EBase _ (String "dummy body") -> (addPrecedingWhitespace extraWhitespace commonScopeReplaced).val.e__
              _                             -> e__
          )
          template
    in
    newLet
  in
  -- Debug only:
  let newSubtreeStr = debugLog "newlyWrappedCommonScope" <| unparse newlyWrappedCommonScope in
  let newExp =
    freshen <|
    replaceExpNode deepestCommonScope newlyWrappedCommonScope originalExp
  in
  newExp


makeEqual originalExp selectedFeatures slate syncOptions =
  case (Set.toList selectedFeatures) of
    [featureA, featureB] -> makeEqual_ originalExp featureA featureB slate syncOptions
    _                    -> originalExp


makeEqual_ originalExp featureA featureB slate syncOptions =
  case (pluckFeatureEquationNamed featureA slate,
        pluckFeatureEquationNamed featureB slate) of
    (Nothing, _) ->
      originalExp

    (_, Nothing) ->
      originalExp

    (Just (featureAName, featureAEqn),
     Just (featureBName, featureBEqn)) ->
      let locset =
        Set.fromList <|
          (equationLocs syncOptions featureAEqn) ++
          (equationLocs syncOptions featureBEqn)
      in
      let findSolution locs =
        case locs of
          [] ->
            Nothing

          loc::rest ->
            case solveForLoc loc featureAEqn featureBEqn of
              Nothing ->
                findSolution rest

              Just resultEqn ->
                Just (loc, resultEqn)
      in
      case findSolution (Set.toList locset) of
        Nothing ->
          originalExp

        Just (loc, resultEqn) ->
          originalExp


      -- let selectedFeatureEquationsNamed =
      --   debugLog "selectedFeatureEquations" <|
      --     pluckSelectedFeatureEquationsNamed selectedFeatures slate
      -- in
      -- let selectedVals =
      --   debugLog "selectedVals" <|
      --     pluckSelectedVals selectedFeatures slate
      -- in
      -- let tracesLocsets =
      --   List.map ((Sync.locsOfTrace syncOptions) << valToTrace) selectedVals
      -- in
      -- let locset =
      --   List.foldl Set.union Set.empty tracesLocsets
      -- in
      -- let locsetList =
      --   Set.toList locset
      -- in
      -- let locToNumber =
      --   let accumulateLocToNumbers exp__ dict =
      --     case exp__ of
      --       EConst ws n loc wd ->
      --         if Set.member loc locset then
      --           Dict.insert loc n dict
      --         else
      --           dict
      --       _ -> dict
      --   in
      --   foldExpViaE__
      --       accumulateLocToNumbers
      --       Dict.empty
      --       originalExp
      -- in
      -- let (deepestCommonScopeParent, deepestCommonScope) =
      --   deepestCommonScopeWithParent originalExp locset syncOptions
      -- in
      -- originalExp


deepestCommonScopeWithParent : Exp -> LocSet -> Sync.Options -> (Maybe Exp, Exp)
deepestCommonScopeWithParent exp locset syncOptions =
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
  let deepestCommonScopeWithParent = debugLog "deepestCommonAncestorWithParent" <|
    -- If no common scope, we will wrap the root node.
    let commonPrefix = debugLog "commonPrefix" <|
      [(Nothing, exp)] ++
      Utils.commonPrefix locsAncestorScopesWithParents
    in
    Utils.last_ commonPrefix
  in
  deepestCommonScopeWithParent


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



-- type Trace = TrLoc Loc | TrOp Op_ (List Trace)
-- type FeatureEquation
--   = EqnVal Val
--   | EqnOp Op_ (List FeatureEquation)


-- Abort if any op other than + - * /
-- Distribute
-- Combine like terms
solveForLoc loc rhs lhs =
  -- let
  --   rhs' = eqnDistribute rhs
  --   lhs' = eqnDistribute lhs
  -- in let
  --   rhs'' = eqnCombineLikeTerms rhs'
  --   lhs'' = eqnCombineLikeTerms lhs'
  -- in
  Nothing


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
