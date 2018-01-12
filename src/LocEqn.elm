module LocEqn exposing (..)

import Lang exposing (..)
import ValUnparser exposing (..)
import Config
import Utils exposing (infinity)
import Solver exposing (EqnTerm(..))

import Dict
import Set
import String

debugLog = Config.debugLog Config.debugSync

--
-- Why so many equation types?
--
-- Trace           - terminals are locs (LocId, Frozen, Ident); no plain numbers.
-- FeatureEquation - terminals are are numTrs (Num, Trace); allows introduction of constants like 0.5 for midpoint calculation.
-- LocEquation     - traces inlined, terminals are locIds and plain numbers; only Plus/Minus/Mult/Div supported.
-- Poly/NormPoly   - wider tree to ease simplification; can represent arbitrary exponents but only constant exponents supported right now.
--
-- If traces supported constants (perhaps as another field to the loc), then Trace & FeatureEquation & LocEquation could be consolidated.
--


-- Holdover until we can discard this file.
type alias LocEquation = Solver.EqnTerm
-- type LocEquation
--   = LocEqnConst Num
--   | LocEqnLoc LocId
--   | LocEqnOp Op_ (List LocEquation)


-- Repeated perform simple simplifications:
-- Remove multiply/divide by 1
-- Turn divide by -1 into multiply by -1
-- Cause multiply by -1 on subtraction to merely flip operands
-- Turn division by constant into multiplication by constant
-- Combine multiple multiplications by constants
-- Remove add or subtract by 0
-- Remove multiply/divide by 0
-- Combine operations on constants
--
-- This is superceded by normalizeSimplify.
-- Remove when it's clear that normalizeSimplify is good enough.
locEqnSimplify : LocEquation -> LocEquation
locEqnSimplify eqn =
  let simplified =
    case eqn of
      EqnConst n ->
        eqn

      EqnVar locId ->
        eqn

      EqnOp op children ->
        let children_ = List.map locEqnSimplify children in
        let eqn_ = EqnOp op children_ in
        case children_ of
          [left, right] ->
            case op of
              Plus ->
                case (left, right) of
                  (EqnConst 0, _) -> right
                  (_, EqnConst 0) -> left
                  -- (+ (- a b) b) to a
                  (EqnOp Minus [a, b], c) -> if b == c then a else eqn_
                  (c, EqnOp Minus [a, b]) -> if b == c then a else eqn_
                  (EqnConst a,
                   EqnConst b)    -> EqnConst (a + b)
                  _                  -> eqn_

              Minus ->
                case (left, right) of
                  (_, EqnConst 0) -> left
                  -- Double minus to plus
                  (EqnConst 0,
                   EqnOp Minus [EqnConst 0, stuff]) -> stuff
                  -- (- 0! (- l r)) to (- r l)
                  (EqnConst 0,
                   EqnOp Minus [subleft, subright]) -> EqnOp Minus [subright, subleft]
                  -- (- 0! (* k stuff)) to (* -k stuff)
                  (EqnConst 0,
                   EqnOp Mult [EqnConst k, stuff]) -> EqnOp Mult [EqnConst -k, stuff]
                  (EqnConst 0,
                   EqnOp Mult [stuff, EqnConst k]) -> EqnOp Mult [EqnConst -k, stuff]
                  -- (- 0! (/ k stuff)) to (/ -k stuff)
                  (EqnConst 0,
                   EqnOp Div [EqnConst k, stuff]) -> EqnOp Div [EqnConst -k, stuff]
                  -- (- a (- b c)) to (+ a (- c b))
                  -- allows (- a (- 0 c)) to become (+ a (- c 0)) which becomes
                  -- (+ a c)
                  (a, EqnOp Minus [b, c]) -> EqnOp Plus [a, EqnOp Minus [c, b]]
                  -- (- (+ a b) b) to a
                  (EqnOp Plus [a, b], c) ->
                    if b == c then a
                    else if a == c then b
                    else eqn_
                  -- (- b (+ a b)) to (- 0 a)
                  (c, EqnOp Plus [a, b]) ->
                    if b == c then EqnOp Minus [EqnConst 0, a]
                    else if a == c then EqnOp Minus [EqnConst 0, b]
                    else eqn_
                  (EqnConst a,
                   EqnConst b)    -> EqnConst (a - b)
                  _                  ->
                    -- Alas, this is syntactic equality not semantic.
                    if left == right then
                      EqnConst 0
                    else
                      eqn_

              Mult ->
                case (left, right) of
                  (EqnConst 1, _) -> right
                  (_, EqnConst 1) -> left
                  (EqnConst 0, _) -> EqnConst 0
                  (_, EqnConst 0) -> EqnConst 0
                  (EqnConst a,
                   EqnConst b)    -> EqnConst (a * b)
                  -- Multiplication by -1 of subtraction...just flip operands:
                  (EqnConst -1, EqnOp Minus  [subleft, subright]) -> EqnOp Minus  [subright, subleft]
                  (EqnOp Minus [subleft, subright], EqnConst -1)  -> EqnOp Minus  [subright, subleft]
                  -- Turn 6 * (x * 7) into 42 * x
                  (EqnConst c1, EqnOp Mult [EqnConst c2, sub]) -> EqnOp Mult [EqnConst (c1 * c2), sub]
                  (EqnConst c1, EqnOp Mult [sub, EqnConst c2]) -> EqnOp Mult [EqnConst (c1 * c2), sub]
                  (EqnOp Mult [EqnConst c2, sub], EqnConst c1) -> EqnOp Mult [EqnConst (c1 * c2), sub]
                  (EqnOp Mult [sub, EqnConst c2], EqnConst c1) -> EqnOp Mult [EqnConst (c1 * c2), sub]
                  _ -> eqn_

              Div ->
                case (left, right) of
                  (_, EqnConst 1)  -> left
                  (_, EqnConst -1) -> EqnOp Mult [(EqnConst -1), left]
                  -- Division by 0 will be handled elsewhere.
                  -- We don_t want to produce infinity here.
                  (EqnConst a,
                   EqnConst b)     -> if b /= 0 then EqnConst (a / b) else eqn_
                  (EqnConst 0, _)  -> EqnConst 0
                  (_, EqnConst b)  -> if b /= 0 then EqnOp Mult [(EqnConst (1 / b)), left] else eqn_
                  _                   ->
                    -- Alas, this is syntactic equality not semantic.
                    if left == right && right /= EqnConst 0 then
                      EqnConst 1
                    else
                      eqn_

              _ ->
                eqn_

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
-- Because once in that form, we can solve for the targetLoc.
--
-- Returns Nothing if equation is not linear in LocId
locEqnTerms : LocId -> LocEquation -> Maybe (Float, LocEquation, LocEquation)
locEqnTerms targetLocId eqn =
  case eqn of
    EqnConst n ->
      Just (1, EqnConst 0, eqn)

    EqnVar locId ->
      if locId == targetLocId
      then Just (1, EqnConst 1, EqnConst 0)
      else Just (1, EqnConst 0, eqn)

    EqnOp op children ->
      let children_ = List.map (locEqnTerms targetLocId) children in
      let result =
        case children_ of
          [Just (leftLocPow,  leftCoeff,  leftRest),
           Just (rightLocPow, rightCoeff, rightRest)] ->
            case op of
              Plus ->
                if leftLocPow == rightLocPow then
                  Just (leftLocPow,
                        EqnOp Plus [leftCoeff, rightCoeff],
                        EqnOp Plus [leftRest, rightRest])
                else
                  -- Not easily solvable, powers of the target loc don't match.
                  Nothing

              Minus ->
                if leftLocPow == rightLocPow then
                  Just (leftLocPow,
                        EqnOp Minus [leftCoeff, rightCoeff],
                        EqnOp Minus [leftRest, rightRest])
                else
                  -- Not easily solvable, powers of the target loc don't match.
                  Nothing

              Mult ->
                case (leftCoeff, leftRest, rightCoeff, rightRest) of
                  -- Left side doesn't contain target loc
                  (EqnConst 0, _, _, _) ->
                    Just (rightLocPow,
                          EqnOp Mult [leftRest, rightCoeff],
                          EqnOp Mult [leftRest, rightRest])

                  -- Right side doesn't contain target loc
                  (_, _, EqnConst 0, _) ->
                    Just (leftLocPow,
                          EqnOp Mult [leftCoeff, rightRest],
                          EqnOp Mult [leftRest, rightRest])

                  -- Both sides only contain terms of the coeff
                  (_, EqnConst 0, _, EqnConst 0) ->
                    let newPow = leftLocPow + rightLocPow in
                    if newPow == 0 then
                      Just (1, EqnConst 0, EqnConst 1)
                    else
                      Just (newPow,
                            EqnOp Mult [leftCoeff, rightCoeff],
                            EqnConst 0)

                  _ ->
                    -- Equation is too difficult for us :-(
                    Nothing

              Div ->
                -- Division is problematic
                case (leftCoeff, leftRest, rightCoeff, rightRest) of
                  -- Division by 0
                  (_, _, EqnConst 0, EqnConst 0) ->
                    Nothing

                  -- Denominator doesn't contain target loc,
                  -- simple distribution.
                  (_, _, EqnConst 0, _) ->
                    Just (leftLocPow,
                          EqnOp Div [leftCoeff, rightRest],
                          EqnOp Div [leftRest, rightRest])

                  -- Denominator is some power and coeff of target loc,
                  -- numerator does not contain target loc
                  (EqnConst 0, _, EqnConst 1, EqnConst 0) ->
                    Just (-rightLocPow,
                          EqnOp Div [leftRest, rightCoeff],
                          EqnConst 0)

                  -- Numerator and denominator are both terms of the target loc
                  (_, EqnConst 0, _, EqnConst 0) ->
                    if leftLocPow == rightLocPow then
                      Just (1, EqnConst 0, EqnOp Div [leftCoeff, rightCoeff])
                    else
                      Just (rightLocPow - leftLocPow,
                            EqnOp Div [leftCoeff, rightCoeff],
                            EqnConst 0)

                  _ ->
                    -- Maybe the numerator and denominator are magically
                    -- syntactically equal.
                    -- (Not smart enough to detect multiples)
                    if leftLocPow == rightLocPow && leftCoeff == rightCoeff && leftRest == rightRest then
                      Just (1, EqnConst 0, EqnConst 1)
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
            Just (newPow, normalizeSimplify newCoeff, normalizeSimplify newRest)

          Nothing ->
            Nothing


-- Expands all equations into a form of xy + yz^2 + y(z+1)^-1 etc
-- then packages back up into a LocEqn
--
-- Not perfect but the best we have so far and simpler than
-- incorporating a full Computer Algebra System
--
type Poly
  = PolyAdd (List Poly)
  | PolyMult Num (List Poly) -- numeric coeff, other terms; if constant, other terms is []
  | PolyPow Poly Poly
  | PolyLoc LocId

type NormPoly     = NormPolyTopLevelAdd (List NormPolyMult)
type NormPolyMult = NormPolyMult Num (List NormPolyPow)
type NormPolyPow  = NormPolyPow NormPolyAdd NormPolyAdd
type NormPolyAdd  = NormPolyAdd (List NormPolyMult)
                  | NormPolyLoc LocId

polyZero      = NormPolyAdd [NormPolyMult 0 []]
polyOne       = NormPolyAdd [NormPolyMult 1 []]
polyConst n   = NormPolyAdd [NormPolyMult n []]
polyLoc locId = NormPolyAdd [NormPolyMult 1 [NormPolyPow (NormPolyLoc locId) polyOne]]




topLevelMultTerms (NormPolyTopLevelAdd multTerms) = multTerms

multTerms normPolyAdd =
  case normPolyAdd of
    NormPolyAdd multTerms -> multTerms
    NormPolyLoc locId     -> [NormPolyMult 1 [NormPolyPow (NormPolyLoc locId) polyOne]]

-- presume terms simplified/consolidated
-- At this point, the function is equivalent to list equality discounting order.
normPolyPowTermsCompatible aPowTerms bPowTerms =
  case (aPowTerms, bPowTerms) of
    ([], []) ->
      True

    (aPowTerm::aRestTerms, _::_) ->
      let maybeMatchingTermRemoved =
        bPowTerms
        |> Utils.maybeFindAndRemoveFirst
            (\bPowTerm -> aPowTerm == bPowTerm)
      in
      case maybeMatchingTermRemoved of
        Just (_, remainingBPowTerms) -> normPolyPowTermsCompatible aRestTerms remainingBPowTerms
        Nothing                      -> False

    _ ->
      False

normPolyAddTerm : NormPolyMult -> List NormPolyMult -> List NormPolyMult
normPolyAddTerm normPolyMultTerm normPolyMultTerms =
  let (NormPolyMult aCoeff aPowTerms) = normPolyMultTerm in
  let maybeMatchingTermRemoved =
    normPolyMultTerms
    |> Utils.maybeFindAndRemoveFirst
        (\(NormPolyMult _ bPowTerms) -> normPolyPowTermsCompatible aPowTerms bPowTerms)
  in
  let results =
    case maybeMatchingTermRemoved of
      Just (NormPolyMult bCoeff bPowTerms, otherMultTerms) ->
        if aCoeff + bCoeff == 0 then
          otherMultTerms
        else
          (NormPolyMult (aCoeff + bCoeff) bPowTerms)::otherMultTerms
      Nothing ->
        normPolyMultTerm::normPolyMultTerms
  in
  -- let _ = Debug.log ("normPolyAddTerm " ++ normPolyMultToString normPolyMultTerm ++ " [" ++ String.join ", " (List.map normPolyMultToString normPolyMultTerms) ++ "] = " ++ "[" ++ String.join ", " (List.map normPolyMultToString results) ++ "]") () in
  results

normPolyMultTerm : NormPolyMult -> List NormPolyMult -> List NormPolyMult
normPolyMultTerm normPolyMultTerm normPolyMultTerms =
  let (NormPolyMult aCoeff aPowTerms) = normPolyMultTerm in
  if aCoeff == 0 then
    []
  else
    aPowTerms
    |> List.foldl normPolyMultPowTerm normPolyMultTerms
    |> List.map (normPolyMultScalarMult aCoeff)

normPolyMultPowTerm : NormPolyPow -> List NormPolyMult -> List NormPolyMult
normPolyMultPowTerm normPolyPowTerm normPolyMultTerms =
  normPolyMultTerms
  |> List.map (normPolyMultPowTermWithMultTerm normPolyPowTerm)

normPolyMultPowTermWithMultTerm : NormPolyPow -> NormPolyMult -> NormPolyMult
normPolyMultPowTermWithMultTerm normPolyPowTerm (NormPolyMult coeff normPolyPowTerms) =
  if coeff == 0 then
    NormPolyMult 0 []
  else
    normPolyPowTerms
    |> List.foldl normPolyMultPowTermWithPowTerms [normPolyPowTerm]
    |> NormPolyMult coeff

normPolyMultPowTermWithPowTerms : NormPolyPow -> List NormPolyPow -> List NormPolyPow
normPolyMultPowTermWithPowTerms normPolyPowTerm normPolyPowTerms =
  case normPolyPowTerm of
    NormPolyPow _ (NormPolyAdd [])                  -> normPolyPowTerms -- n^0 * xs = xs
    NormPolyPow _ (NormPolyAdd [NormPolyMult 0 []]) -> normPolyPowTerms -- n^0 * xs = xs
    NormPolyPow (NormPolyAdd []) _                  -> [NormPolyPow polyZero polyOne] -- 0^n * xs = 0
    NormPolyPow (NormPolyAdd [NormPolyMult 0 []]) _ -> [NormPolyPow polyZero polyOne] -- 0^n * xs = 0
    NormPolyPow (NormPolyAdd [NormPolyMult b []]) (NormPolyAdd [NormPolyMult n []]) ->
      -- b^n * xs = (b^n) * xs
      -- Search for and consolidate any other constant terms
      let (constantPowTerms, otherPowTerms) =
        normPolyPowTerms
        |> List.partition
            (\powTerm ->
              case powTerm of
                NormPolyPow (NormPolyAdd [NormPolyMult _ []]) (NormPolyAdd [NormPolyMult _ []]) -> True
                _                                                                               -> False
            )
      in
      let constant =
        normPolyPowTerm::constantPowTerms
        |> List.map
            (\powTerm ->
              case powTerm of
                (NormPolyPow (NormPolyAdd [NormPolyMult termBase []]) (NormPolyAdd [NormPolyMult termExp []])) -> termBase^termExp
                _ -> Debug.crash <| "LocEqn.normPolyMultPowTermWithPowTerms shouldn't get here, got " ++ (toString powTerm)
            )
        |> List.product
      in
      if constant == 0 then
        [NormPolyPow polyZero polyOne]
      else
        (NormPolyPow (polyConst constant) polyOne)::otherPowTerms
    _ ->
      -- Is there a term with this base?
      let (NormPolyPow termBase termExponent) = normPolyPowTerm in
      let maybeMatchingTermRemoved =
        normPolyPowTerms
        |> Utils.maybeFindAndRemoveFirst
            (\(NormPolyPow bTermBase _) -> termBase == bTermBase)
      in
      case maybeMatchingTermRemoved of
        Just (NormPolyPow bTermBase bTermExponent, otherPowTerms) ->
          let newExponent = normPolyAdd termExponent bTermExponent in
          case newExponent of
            NormPolyAdd []                  -> otherPowTerms
            NormPolyAdd [NormPolyMult 0 []] -> otherPowTerms
            _                               -> (NormPolyPow bTermBase newExponent)::otherPowTerms
        Nothing ->
          normPolyPowTerm::normPolyPowTerms

normPolyAdd aNormPolyAdd bNormPolyAdd =
  case (aNormPolyAdd, bNormPolyAdd) of
    (NormPolyLoc locId, _) -> normPolyAdd (polyLoc locId) bNormPolyAdd
    (_, NormPolyLoc locId) -> normPolyAdd aNormPolyAdd (polyLoc locId)
    (NormPolyAdd aNormPolyMultTerms,
     NormPolyAdd bNormPolyMultTerms) ->
       (aNormPolyMultTerms ++ bNormPolyMultTerms)
       |> List.foldl normPolyAddTerm []
       |> NormPolyAdd


normPolyToNormPolyAdd (NormPolyTopLevelAdd multTerms) =
  NormPolyAdd multTerms

normPolyAddToNormPoly normPolyAdd =
  case normPolyAdd of
    NormPolyLoc locId             -> normPolyAddToNormPoly (polyLoc locId)
    NormPolyAdd normPolyMultTerms -> NormPolyTopLevelAdd normPolyMultTerms


normPolyMultScalarMult coeff (NormPolyMult termCoeff powTerms) =
  NormPolyMult (coeff * termCoeff) powTerms


polyScalarMult coeff normPolyAdd =
  let result =
    case normPolyAdd of
      NormPolyAdd multTerms ->
        if coeff == 0 then
          polyZero
        else
          multTerms
          |> List.map (normPolyMultScalarMult coeff)
          |> NormPolyAdd

      NormPolyLoc locId ->
        NormPolyAdd [NormPolyMult coeff [NormPolyPow (NormPolyLoc locId) polyOne]]
  in
  -- let _ = Debug.log ("polyScalarMult " ++ toString coeff ++ " * " ++ normPolyAddToString normPolyAdd ++ " = " ++ normPolyAddToString result) () in
  result


topLevelPolyScalarMult coeff (NormPolyTopLevelAdd multTerms) =
  polyScalarMult coeff (NormPolyAdd multTerms)
  |> normPolyAddToNormPoly


topLevelPolyDistribute : NormPoly -> NormPoly -> NormPoly
topLevelPolyDistribute (NormPolyTopLevelAdd aMultTerms) (NormPolyTopLevelAdd bMultTerms) =
  let result =
    aMultTerms
    |> List.concatMap (\aMultTerm -> normPolyMultTerm aMultTerm bMultTerms)
    |> List.foldl normPolyAddTerm []
    |> NormPolyTopLevelAdd
  in
  -- let _ = Debug.log ("topLevelPolyDistribute " ++ normPolyToString (NormPolyTopLevelAdd aMultTerms) ++ " * " ++ normPolyToString (NormPolyTopLevelAdd bMultTerms) ++ " = " ++ normPolyToString result) () in
  result


normPolyToPoly (NormPolyTopLevelAdd multTerms) =
  normPolyAddToPoly (NormPolyAdd multTerms)

normPolyMultToPoly (NormPolyMult coeff powTerms) =
  PolyMult coeff (List.map normPolyPowToPoly powTerms)

normPolyPowToPoly (NormPolyPow base exponent) =
  PolyPow (normPolyAddToPoly base) (normPolyAddToPoly exponent)

normPolyAddToPoly normPolyAdd =
  case normPolyAdd of
    NormPolyAdd multTerms -> PolyAdd (List.map normPolyMultToPoly multTerms)
    NormPolyLoc locId     -> PolyLoc locId


polyToString : Poly -> String
polyToString poly =
  case poly of
    PolyAdd []              -> "0"
    PolyAdd [PolyMult n []] -> toString n
    PolyAdd multTerms       -> "(" ++ String.join " + " (List.map polyToString multTerms) ++ ")"
    PolyMult coeff []       -> toString coeff
    PolyMult 1 powTerms     -> String.join "*" (List.map polyToString powTerms)
    PolyMult coeff powTerms -> toString coeff ++ "*" ++ polyToString (PolyMult 1 powTerms)
    PolyPow base exponent   -> polyToString base ++ "^" ++ polyToString exponent
    PolyLoc locId           -> "k" ++ toString locId

normPolyToString     = normPolyToPoly >> polyToString
normPolyMultToString = normPolyMultToPoly >> polyToString
normPolyPowToString  = normPolyPowToPoly >> polyToString
normPolyAddToString  = normPolyAddToPoly >> polyToString


-- Order by:
-- 1. Highest Power
-- 2. Fewest number of term kinds
-- 3. Second highest power
-- 4. Third highest power, etc.
-- 5. "Alphabetic" by term kind locId
-- 6. Power of first term kind
-- 7. Power of second term kind, etc.


-- For sorting. Non-constant exponents should be last.
normPolyPowExponentSortNum (NormPolyPow _ exponent) =
  case exponent of
    NormPolyAdd []                  -> 0
    NormPolyAdd [NormPolyMult n []] -> n
    _                               -> -infinity


normPolyPowLocIds (NormPolyPow base exponent) =
  case base of
    NormPolyLoc locId -> [locId]
    _                 -> []


-- When non-constant exponents are supported, need to sort on that.
-- Need sorting based on complex bases.
normPolyMultTermSortKey (NormPolyMult coeff powTerms) =
  let powTermPowers = List.map normPolyPowExponentSortNum powTerms in
  let locIds = List.concatMap normPolyPowLocIds powTerms in
  ( List.maximum powTermPowers |> Maybe.withDefault -infinity |> negate
  , List.length powTerms
  , List.sort powTermPowers |> List.reverse |> List.map negate
  , locIds
  , powTermPowers |> List.map negate
  , coeff
  ) -- Elm won't let a tuple longer than 6 be comparable; could nest tuples if necessary.


-- Need sorting based on complex bases.
normPolyPowSortKey (NormPolyPow base exponent) =
  case base of
    NormPolyLoc locId -> toFloat locId
    _                 -> -infinity


normPolyAddSort : NormPolyAdd -> NormPolyAdd
normPolyAddSort normPolyAdd =
  case normPolyAdd of
    NormPolyLoc _ ->
      normPolyAdd

    NormPolyAdd multTerms ->
      multTerms
      |> List.map normPolyMultSort
      |> List.sortBy normPolyMultTermSortKey
      |> NormPolyAdd


normPolyPowSort : NormPolyPow -> NormPolyPow
normPolyPowSort (NormPolyPow base exponent) =
  NormPolyPow (normPolyAddSort base) (normPolyAddSort exponent)


normPolyMultSort : NormPolyMult -> NormPolyMult
normPolyMultSort (NormPolyMult coeff powTerms) =
  powTerms
  |> List.map normPolyPowSort
  |> List.sortBy normPolyPowSortKey
  |> NormPolyMult coeff


normPolySort : NormPoly -> NormPoly
normPolySort (NormPolyTopLevelAdd multTerms) =
  multTerms
  |> List.map normPolyMultSort
  |> List.sortBy normPolyMultTermSortKey
  |> NormPolyTopLevelAdd


-- Try to simplify/normalize.
polyNorm : Poly -> NormPoly
polyNorm poly =
  -- let _ = Debug.log "normalizing" poly in
  let result =
    case poly of
      PolyAdd [] ->
        normPolyAddToNormPoly polyZero
      PolyAdd terms ->
        terms
        |> List.map polyNorm
        |> List.concatMap topLevelMultTerms
        |> List.foldl normPolyAddTerm []
        |> NormPolyTopLevelAdd
      PolyMult coeffNum [] ->
        NormPolyTopLevelAdd [NormPolyMult coeffNum []]
      PolyMult coeffNum terms ->
        terms
        |> List.map polyNorm -- addition should bubble up
        |> List.map (topLevelPolyScalarMult coeffNum)
        |> List.foldl topLevelPolyDistribute (normPolyAddToNormPoly polyOne) -- Now, distribute.
      PolyPow base expon ->
        let (normBase, normExpon) = (polyNorm base, polyNorm expon) in
        case (normBase, normExpon) of
          (_,                                       NormPolyTopLevelAdd [])                  -> normPolyAddToNormPoly polyOne  -- n^0 = 1
          (_,                                       NormPolyTopLevelAdd [NormPolyMult 0 []]) -> normPolyAddToNormPoly polyOne  -- n^0 = 1
          (_,                                       NormPolyTopLevelAdd [NormPolyMult 1 []]) -> normBase -- n^1 = n
          (NormPolyTopLevelAdd [],                  _)                                       -> normPolyAddToNormPoly polyZero -- 0^n = 0
          (NormPolyTopLevelAdd [NormPolyMult 0 []], _)                                       -> normPolyAddToNormPoly polyZero -- 0^n = 0
          (NormPolyTopLevelAdd [NormPolyMult b []], NormPolyTopLevelAdd [NormPolyMult n []]) -> normPolyAddToNormPoly (polyConst (b^n)) -- b^n = b^n
          (NormPolyTopLevelAdd [NormPolyMult bc [NormPolyPow bt bp]], NormPolyTopLevelAdd [NormPolyMult n []]) -> -- (bc*bt^bp)^n = bc^n*bt^(bp*n)
            NormPolyTopLevelAdd [NormPolyMult (bc^n) [NormPolyPow bt (polyScalarMult n bp)]]
          (NormPolyTopLevelAdd [NormPolyMult bc [NormPolyPow bt1 bp1, NormPolyPow bt2 bp2]], NormPolyTopLevelAdd [NormPolyMult n []]) ->
            NormPolyTopLevelAdd [NormPolyMult (bc^n) [NormPolyPow bt1 (polyScalarMult n bp1), NormPolyPow bt2 (polyScalarMult n bp2)]]
          (NormPolyTopLevelAdd [NormPolyMult bc [NormPolyPow bt1 bp1, NormPolyPow bt2 bp2, NormPolyPow bt3 bp3]], NormPolyTopLevelAdd [NormPolyMult n []]) ->
            NormPolyTopLevelAdd [NormPolyMult (bc^n) [NormPolyPow bt1 (polyScalarMult n bp1), NormPolyPow bt2 (polyScalarMult n bp2), NormPolyPow bt3 (polyScalarMult n bp3)]]
          (NormPolyTopLevelAdd [NormPolyMult bc [NormPolyPow bt1 bp1, NormPolyPow bt2 bp2, NormPolyPow bt3 bp3, NormPolyPow bt4 bp4]], NormPolyTopLevelAdd [NormPolyMult n []]) ->
            NormPolyTopLevelAdd [NormPolyMult (bc^n) [NormPolyPow bt1 (polyScalarMult n bp1), NormPolyPow bt2 (polyScalarMult n bp2), NormPolyPow bt3 (polyScalarMult n bp3), NormPolyPow bt4 (polyScalarMult n bp4)]]
          (NormPolyTopLevelAdd [NormPolyMult bc [NormPolyPow bt1 bp1, NormPolyPow bt2 bp2, NormPolyPow bt3 bp3, NormPolyPow bt4 bp4, NormPolyPow bt5 bp5]], NormPolyTopLevelAdd [NormPolyMult n []]) ->
            NormPolyTopLevelAdd [NormPolyMult (bc^n) [NormPolyPow bt1 (polyScalarMult n bp1), NormPolyPow bt2 (polyScalarMult n bp2), NormPolyPow bt3 (polyScalarMult n bp3), NormPolyPow bt4 (polyScalarMult n bp4), NormPolyPow bt5 (polyScalarMult n bp5)]]
          -- Yeah...it could be generalized to handle more than 5 terms...it would probably be ugly...
          (NormPolyTopLevelAdd (_::_::_), NormPolyTopLevelAdd [NormPolyMult n []]) ->
            if n > 0 && n == toFloat (round n) then
              polyNorm (PolyMult 1 (List.repeat (round n) (normPolyToPoly normBase)))
            else if n < 0 && n == toFloat (round n) then
              let denominator = polyNorm (PolyMult 1 (List.repeat (abs (round n)) (normPolyToPoly normBase))) in
              NormPolyTopLevelAdd [NormPolyMult 1 [NormPolyPow (normPolyToNormPolyAdd denominator) (polyConst -1)]]
            else
              NormPolyTopLevelAdd [NormPolyMult 1 [NormPolyPow (normPolyToNormPolyAdd normBase) (normPolyToNormPolyAdd normExpon)]]
          _ -> NormPolyTopLevelAdd [NormPolyMult 1 [NormPolyPow (normPolyToNormPolyAdd normBase) (normPolyToNormPolyAdd normExpon)]]
      PolyLoc locId ->
        NormPolyTopLevelAdd [NormPolyMult 1 [NormPolyPow (NormPolyLoc locId) polyOne]]
  in
  -- let _ = Debug.log ("polyNorm " ++ polyToString poly ++ " = " ++ normPolyToString result) result in
  normPolySort result

locEquationToPoly : LocEquation -> Poly
locEquationToPoly locEqn =
  case locEqn of
    EqnConst n ->
      PolyMult n []

    EqnVar locId ->
      PolyLoc locId

    EqnOp Plus children ->
      PolyAdd <| List.map locEquationToPoly children

    EqnOp Minus (leftChild::otherChildren) ->
      PolyAdd <| (locEquationToPoly leftChild)::(List.map (\child -> PolyMult -1 [locEquationToPoly child]) otherChildren)

    EqnOp Mult children ->
      PolyMult 1 <| List.map locEquationToPoly children

    EqnOp Div (leftChild::otherChildren) ->
      PolyMult 1 <| (locEquationToPoly leftChild)::(List.map (\child -> PolyPow (locEquationToPoly child) (PolyMult -1 [])) otherChildren)

    EqnOp _ _ ->
      Debug.crash <| "LocEqn.locEquationToPoly can only handle Plus/Minus/Mult/Div, got " ++ (toString locEqn)


-- type NormPoly     = NormPolyTopLevelAdd (List NormPolyMult)
-- type NormPolyMult = NormPolyMult Num (List NormPolyPow)
-- type NormPolyPow  = NormPolyPow NormPolyAdd NormPolyAdd
-- type NormPolyAdd  = NormPolyAdd (List NormPolyMult)
--                   | NormPolyLoc LocId

normPolyToLocEquation : NormPoly -> LocEquation
normPolyToLocEquation (NormPolyTopLevelAdd normPolyMultTerms) =
  normPolyAddToLocEquation (NormPolyAdd normPolyMultTerms)

normPolyAddToLocEquation : NormPolyAdd -> LocEquation
normPolyAddToLocEquation normPolyAdd =
  case normPolyAdd of
    NormPolyLoc locId ->
      EqnVar locId

    NormPolyAdd normPolyMultTerms ->
      case normPolyMultTerms of
        [] ->
          EqnConst 0

        multTerm::[] ->
          normPolyMultToLocEquation multTerm

        multTerm::otherMultTerms ->
          -- Simplest factoring case only right now: all same coeff (modulo sign).
          let coeffs = normPolyMultTerms |> List.map (\(NormPolyMult coeff _) -> coeff) in
          let possibleCoeffFactor = Utils.last_ (List.sort coeffs) in
          if Utils.allSame (List.map abs coeffs) && possibleCoeffFactor /= 1 then
            let coeffFactor = possibleCoeffFactor in
            let factoredTerms =
              normPolyMultTerms
              |> List.map
                  (\(NormPolyMult coeff normPolyPowTerms) ->
                    if coeff == coeffFactor then
                      NormPolyMult 1 normPolyPowTerms
                    else
                      NormPolyMult -1 normPolyPowTerms
                  )
            in
            normPolyMultToLocEquation (NormPolyMult coeffFactor [NormPolyPow (NormPolyAdd factoredTerms) (NormPolyAdd [NormPolyMult 1 []])])
          else
            let (negativeTerms, nonNegativeTerms) =
              normPolyMultTerms
              |> List.partition (\(NormPolyMult coeff normPolyPowTerms) -> coeff < 0)
            in
            case (negativeTerms, nonNegativeTerms) of
              (_, firstNonNegTerm::secondNonNegTerm::otherNonNegTerms) ->
                -- We have at least two non-negative terms. Wait to do the subtraction.
                let remainingTerms = secondNonNegTerm::otherNonNegTerms ++ negativeTerms in
                EqnOp Plus [normPolyMultToLocEquation firstNonNegTerm, normPolyAddToLocEquation (NormPolyAdd remainingTerms)]

              (_::_, onlyNonNegTerm::[]) ->
                let negNegativeTerms = negativeTerms |> List.map (normPolyMultScalarMult -1) in
                EqnOp Minus [normPolyMultToLocEquation onlyNonNegTerm, normPolyAddToLocEquation (NormPolyAdd negNegativeTerms)]

              (_::_, []) ->
                -- Only negative terms. :(
                -- Will build a common factor extractor later.
                EqnOp Plus [normPolyMultToLocEquation multTerm, normPolyAddToLocEquation (NormPolyAdd otherMultTerms)]

              ([], _::[]) ->
                Debug.crash "LocEqn.normPolyAddToLocEquation shouldn't get here; single term handled in earlier case statement"

              ([], []) ->
                Debug.crash "LocEqn.normPolyAddToLocEquation shouldn't get here; empty term list in earlier case statement"



normPolyMultToLocEquation : NormPolyMult -> LocEquation
normPolyMultToLocEquation (NormPolyMult coeff normPolyPowTerms) =
  let isNegativeExponent (NormPolyPow _ exponentTerm) =
    case exponentTerm of
      NormPolyAdd [NormPolyMult n []] -> n < 0
      _                               -> False
  in
  -- let isPositiveExponent (NormPolyPow _ exponentTerm) =
  --   case exponentTerm of
  --     NormPolyAdd [NormPolyMult n []] -> n > 0
  --     _                               -> False
  -- in
  let isZeroExponent (NormPolyPow _ exponentTerm) =
    case exponentTerm of
      NormPolyAdd []                  -> True
      NormPolyAdd [NormPolyMult n []] -> n == 0
      _                               -> False
  in
  let (zeroPowers, remainingTerms)  = List.partition isZeroExponent normPolyPowTerms  in
  let (negativePowers, otherPowers) = List.partition isNegativeExponent remainingTerms in
  let flipExponentSign (NormPolyPow baseTerm exponentTerm) =
    case exponentTerm of
      NormPolyAdd [NormPolyMult n []] -> NormPolyPow baseTerm (polyConst -n)
      _                               -> Debug.crash <| "LocEqn.normPolyMultToLocEquation shouldn't reach here; got " ++ toString (NormPolyPow baseTerm exponentTerm)
  in
  case (negativePowers, otherPowers, coeff) of
    (_, _, 0) ->
      EqnConst 0

    ([], [], _) ->
      EqnConst coeff

    (_::_, _, _) ->
      EqnOp Div
          [ normPolyMultToLocEquation (NormPolyMult coeff otherPowers)
          , normPolyMultToLocEquation (NormPolyMult 1 (List.map flipExponentSign negativePowers))
          ]

    ([], _, 1) ->
      -- This is the main branch; other branches reduce to this.
      case otherPowers of
        term::[] ->
          normPolyPowToLocEquation term

        headTerm::otherTerms ->
          EqnOp Mult
              [ normPolyPowToLocEquation headTerm
              , normPolyMultToLocEquation (NormPolyMult 1 otherTerms)
              ]

        [] ->
          Debug.crash <| "LocEqn.normPolyMultToLocEquation shouldn't get here; should have at least some terms"

    ([], _, _) ->
      if abs coeff >= 1 then
        EqnOp Mult
          [ normPolyMultToLocEquation (NormPolyMult 1 otherPowers)
          , EqnConst coeff
          ]
      else
        EqnOp Div
          [ normPolyMultToLocEquation (NormPolyMult 1 otherPowers)
          , EqnConst (1 / coeff)
          ]


normPolyPowToLocEquation (NormPolyPow normPolyBase normPolyExponent) =
  case normPolyExponent of
    NormPolyAdd [] ->
      EqnConst 1

    NormPolyAdd [NormPolyMult 0 []] ->
      EqnConst 1

    NormPolyAdd [NormPolyMult 1 []] ->
      normPolyAddToLocEquation normPolyBase

    NormPolyAdd [NormPolyMult n []] ->
      if n > 1 && n == toFloat (round n) then
        EqnOp Mult
          [ normPolyAddToLocEquation normPolyBase
          , normPolyPowToLocEquation (NormPolyPow normPolyBase (NormPolyAdd [NormPolyMult (toFloat (round n) - 1) []]))
          ]
      else
        Debug.crash <| "LocEqn.normPolyPowToLocEquation unexpected exponent: should only be positive integers >1 by now: got " ++ (toString (NormPolyPow normPolyBase normPolyExponent))

    _ ->
      Debug.crash <| "LocEqn.normPolyPowToLocEquation can't handle non-integer, non-constant powers yet: got " ++ (toString (NormPolyPow normPolyBase normPolyExponent))



normalizeSimplify : LocEquation -> LocEquation
normalizeSimplify eqn =
  -- let _ = Debug.log ("Simplifying " ++ locEqnToLittle Dict.empty eqn) () in
  let polyEqn = locEquationToPoly eqn in
  -- let _ = Debug.log ("Poly version " ++ polyToString polyEqn) () in
  let normPolyResult = polyNorm polyEqn in
  -- let _ = Debug.log ("Got " ++ normPolyToString normPolyResult) () in
  let locEqnResult = normPolyToLocEquation normPolyResult |> correctFloatErrors in
  -- let _ = Debug.log ("As little " ++ locEqnToLittle Dict.empty littleResult) () in
  locEqnResult


-- Find e.g. 1.4999999999999 and change to 1.5.
correctFloatErrors : LocEquation -> LocEquation
correctFloatErrors eqn =
  case eqn of
    EqnConst n         -> EqnConst (Utils.correctFloatError n)
    EqnVar _           -> eqn
    EqnOp op_ children -> EqnOp op_ (List.map correctFloatErrors children)


-- locEqnsOfSize astSize locsToUse =
--   if astSize < 1 then
--     []
--   else if astSize == 1 then
--     -- From stats of all our little programs so far.
--     [ EqnConst 0
--     , EqnConst 1
--     , EqnConst 2
--     , EqnConst 10
--     , EqnConst 3
--     , EqnConst 20
--     , EqnConst 50
--     , EqnConst 4
--     , EqnConst 300
--     , EqnConst 5
--     , EqnConst 0.5
--     , EqnConst 100
--     , EqnConst 200
--     , EqnConst 30
--     , EqnConst 60
--     , EqnConst 80
--     , EqnConst 15
--     , EqnConst 360
--     , EqnConst 180
--     , EqnConst 120
--     , EqnConst 6
--     , EqnConst 150
--     , EqnConst 40
--     , EqnConst 8
--     ] ++ (Set.toList locsToUse |> List.map (\(locId, _, _) -> EqnVar locId))
--   else if astSize == 2 then
--     -- No unops in LocEqns yet
--     []
--   else
--     [Plus, Minus, Mult, Div]
--     |> List.concatMap (\op ->
--       (List.range 1 (astSize - 2))
--       |> List.concatMap (\leftSize ->
--         locEqnsOfSize leftSize locsToUse
--         |> List.concatMap (\leftEqn ->
--           let rightSize = astSize - leftSize - 1 in
--           locEqnsOfSize rightSize locsToUse
--           |> List.map (\rightEqn ->
--             EqnOp op [leftEqn, rightEqn]
--           )
--         )
--       )
--     )


solveForLocValue : LocId -> Subst -> LocEquation -> Num -> Maybe Num
solveForLocValue targetLocId subst eqn eqnTargetValue =
  let eqnEqualToZero =
    constantifyLocs (Dict.remove targetLocId subst) (EqnOp Minus [eqn, EqnConst eqnTargetValue])
  in
  case locEqnTerms targetLocId eqnEqualToZero of
    Just (pow, coeffEqn, restEqn) ->
      -- We have: coeff*x^pow + rest = 0
      -- We want: x = (-rest / coeff)^(1/pow)
      let coeffEvaled = locEqnEval Dict.empty coeffEqn in
      let restEvaled  = locEqnEval Dict.empty restEqn in
      let x = (-restEvaled / coeffEvaled)^(1/pow) in
      if (isNaN x) || (isInfinite x) then
        Nothing
      else
        Just x

    Nothing ->
      Nothing


solveForConst : Subst -> LocEquation -> Num -> Maybe Num
solveForConst subst eqn eqnTargetValue =
  let locifyConstant eqn =
    case eqn of
      EqnConst _        -> EqnVar -2
      EqnVar _          -> eqn
      EqnOp op children -> EqnOp op (List.map locifyConstant children)
  in
  solveForLocValue -2 subst (locifyConstant eqn) eqnTargetValue


-- Will abort if any op other than + - * /
--
-- Must be linear in the locId solved for.
--
-- Convert to just locIds (variables) and constants
solveForLocUnchecked : LocId -> Dict.Dict LocId Num -> LocEquation -> LocEquation -> Maybe LocEquation
solveForLocUnchecked locId locIdToNum lhs rhs =
  let maybeEqn =
    -- Help out the silly simplifier.
    case maybeExtractUnsharedExpression rhs lhs of  -- TODO: why is this backwards???
      Nothing ->
        Nothing

      Just (lhs_, rhs_) ->
        -- We will duplicate frozen constants into the little equation
        -- string. Otherwise, math values like 0, 1, 2 get assigned to
        -- variable names.
        let
          lhs__ = constantifyLocs locIdToNum lhs_
          rhs__ = constantifyLocs locIdToNum rhs_
        in
        -- Transform   rhs_ - lhs_ = 0
        -- to          coeff*x^pow + rest = 0
        -- where x is our target loc
        case locEqnTerms locId (EqnOp Minus [lhs__, rhs__]) of
          Just (locPow, locCoeff, rest) ->
            if locPow == 0 || locCoeff == EqnConst 0 then
              Nothing
            else if locPow == 1 then
              -- We have: coeff*x + rest = 0
              -- We want: x = something
              Just <|
              normalizeSimplify <|
                EqnOp Div
                    [ EqnOp Minus [EqnConst 0, rest]
                    , locCoeff]

            else if locPow == -1 then
              -- We have: coeff/x + rest = 0
              -- We want: x = something
              Just <|
              normalizeSimplify <|
                EqnOp Div
                    [ locCoeff
                    , EqnOp Minus [EqnConst 0, rest]]
            else
              -- Just need to add a pow op and then we can handle more pows.
              Nothing

          Nothing ->
            Nothing
  in
  maybeEqn


-- solveForLoc : LocId -> Dict.Dict LocId Num -> Subst -> LocEquation -> LocEquation -> Maybe LocEquation
-- solveForLoc locId locIdToNum subst lhs rhs =
--   -- Check that equation doesn't produce NaN or similar...
--   case solveForLocUnchecked locId locIdToNum lhs rhs of
--     Just eqn ->
--       -- Need the full subst, not just frozen constants.
--       let evaled = locEqnEval subst eqn in
--       if (isNaN evaled) || (isInfinite evaled)
--       then Nothing
--       else Just eqn
--
--     Nothing ->
--       Nothing


-- Help out our not-so-smart simplifier.
-- If lhs and rhs are identical but for some sub-expression,
-- return just the differing sub-expressions.
maybeExtractUnsharedExpression : LocEquation -> LocEquation -> Maybe (LocEquation, LocEquation)
maybeExtractUnsharedExpression lhs rhs =
  case (lhs, rhs) of
    (EqnConst ln, EqnConst rn) ->
      if ln == rn
      then Nothing
      else Just (lhs, rhs)

    (EqnVar lLocId, EqnVar rLocId) ->
      if lLocId == rLocId
      then Nothing
      else Just (lhs, rhs)

    (EqnOp lOp lChildren, EqnOp rOp rChildren) ->
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


littleConstants = -- From stats of all our little programs so far.
      [ 0
      , 1
      , 2
      , 10
      , 3
      , 20
      , 50
      , 4
      , 300
      , 5
      , 0.5
      , 100
      , 200
      , 30
      , 60
      , 80
      , 15
      , 360
      , 180
      , 120
      , 6
      , 150
      , 40
      , 8
      ]

-- Fill in template with all combinations of locId's in the various locations.
-- Then choose a single best number for filling in a constant.
-- (Expects no more than one constant.)
--
-- This function is not actually used, just a demo. In practice may want to filter after each step.
locEqnTemplateFillings targetValue subst locIdSet template =
  let allLocFillings = locEqnTemplateLocFillings (locIdSet |> Set.toList) template in
  locEqnTemplateFillingsLocsFilled targetValue subst littleConstants allLocFillings

locEqnTemplateFillingsLocsFilled : Num -> Subst -> List Num -> List LocEquation -> List LocEquation
locEqnTemplateFillingsLocsFilled targetValue subst constants locFillings =
  let filledWithNiceNumber =
    locFillings
    |> List.map
        (\locFilledEqn ->
          locEqnTemplateConstantFillings constants locFilledEqn
          |> List.sortBy (\eqn -> abs (locEqnEval subst eqn - targetValue))
          |> Utils.head "LocEqn.locEqnTemplateFillings ranking"
        )
  in
  let filledWithExactNumber =
    let fillInConstant const eqn =
      case eqn of
        EqnConst _        -> EqnConst const
        EqnVar _          -> eqn
        EqnOp op children -> EqnOp op (List.map (fillInConstant const) children)
    in
    locFillings
    |> List.filterMap
        (\locFilledEqn ->
          -- -- Only produce an exact solution if no locs appear more than once.
          -- -- Cuts down on *some* junk.
          -- if Set.size (locEqnLocIdSet locFilledEqn) == List.length (locEqnLocIds locFilledEqn) then
          --   solveForConst subst locFilledEqn targetValue
          --   |> Maybe.map (\const -> fillInConstant const locFilledEqn)
          -- else
          --   Nothing

          -- Only produce an exact solution if only one loc in eqn.
          if List.length (locEqnLocIds locFilledEqn) <= 1 then
            solveForConst subst locFilledEqn targetValue
            |> Maybe.map (\const -> fillInConstant const locFilledEqn)
          else
            Nothing
        )
  in
  filledWithNiceNumber ++ filledWithExactNumber


-- locIds should be unique.
locEqnTemplateLocFillings : List LocId -> LocEquation -> List LocEquation
locEqnTemplateLocFillings locIds eqn =
  case eqn of
    EqnConst _        -> [eqn]
    EqnVar _          -> List.map EqnVar locIds
    EqnOp op children ->
      children
      |> List.foldl
        (\child priorCombos ->
          let thisChildFillings = locEqnTemplateLocFillings locIds child in
          thisChildFillings
          |> List.concatMap (\childFilling -> priorCombos |> List.map (\priorArgs -> priorArgs ++ [childFilling]))
        )
        [[]]
      |> List.map (EqnOp op)


locEqnTemplateConstantFillings : List Num -> LocEquation -> List LocEquation
locEqnTemplateConstantFillings constants eqn =
  case eqn of
    EqnConst _        -> List.map EqnConst constants
    EqnVar _          -> [eqn]
    EqnOp op children ->
      children
      |> List.foldl
        (\child priorCombos ->
          let thisChildFillings = locEqnTemplateConstantFillings constants child in
          thisChildFillings
          |> List.concatMap (\childFilling -> priorCombos |> List.map (\priorArgs -> priorArgs ++ [childFilling]))
        )
        [[]]
      |> List.map (EqnOp op)


atLeastNLocs     n template = List.length (locEqnLocIds template) >= n
atMostNConstants n template = List.length (locEqnConsts template) <= n

-- Templates for synthesis:
-- Returns all terms of a certain shape.
-- Exact numberic values and variables will be filled in later.
locEqnsTemplatesOfSize minLocs maxConsts astSize =
  locEqnsTemplatesOfSize_ astSize
  |> List.filter (atLeastNLocs minLocs)
  |> List.filter (atMostNConstants maxConsts)

-- Could be more time and memory effecient with explicit memoization (build up from size 1)
-- But I don't think this part is the bottleneck
locEqnsTemplatesOfSize_ astSize =
  if astSize < 1 then
    []
  else if astSize == 1 then
    [ EqnConst -1, EqnVar -1 ]
  else if astSize == 2 then
    -- No unops in LocEqns yet
    []
  else
    [Plus, Minus, Mult, Div]
    |> List.concatMap (\op ->
      (List.range 1 (astSize - 2))
      |> List.concatMap (\leftSize ->
        locEqnsTemplatesOfSize_ leftSize
        |> List.concatMap (\leftEqn ->
          let rightSize = astSize - leftSize - 1 in
          locEqnsTemplatesOfSize_ rightSize
          |> List.map (\rightEqn ->
            EqnOp op [leftEqn, rightEqn]
          )
        )
      )
    )

locEqnSize : LocEquation -> Int
locEqnSize eqn =
  case eqn of
    EqnConst _       -> 1
    EqnVar _         -> 1
    EqnOp _ children -> 1 + List.sum (List.map locEqnSize children)

locEqnLocIdSet : LocEquation -> Set.Set LocId
locEqnLocIdSet eqn =
  locEqnLocIds eqn |> Set.fromList

locEqnConsts : LocEquation -> List Num
locEqnConsts eqn =
  case eqn of
    EqnConst n       -> [n]
    EqnVar locId     -> []
    EqnOp _ children -> List.concatMap locEqnConsts children

locEqnLocIds : LocEquation -> List LocId
locEqnLocIds eqn =
  case eqn of
    EqnConst _       -> []
    EqnVar locId     -> [locId]
    EqnOp _ children -> List.concatMap locEqnLocIds children


locEqnEval locIdToNum eqn =
  locEqnEval_ (constantifyLocs locIdToNum eqn)


locEqnEval_ eqn =
  case eqn of
    EqnConst n       -> n
    EqnVar locId     -> Debug.crash "shouldn't have locs in constantified eqn"
    EqnOp op [leftChild, rightChild] ->
      let (leftEvaled, rightEvaled) = (locEqnEval_ leftChild, locEqnEval_ rightChild) in
      case op of
        Plus  -> leftEvaled + rightEvaled
        Minus -> leftEvaled - rightEvaled
        Mult  -> leftEvaled * rightEvaled
        Div   -> leftEvaled / rightEvaled
        _     -> Debug.crash <| "Unknown loc equation op: " ++ (toString op)
    _  -> Debug.crash <| "Loc equation only supports binary operations, but got: " ++ (toString eqn)

traceToLocEquation : Trace -> LocEquation
traceToLocEquation trace =
  case trace of
    -- locId of 0 means it's a constant that's part of the feature equation,
    -- not the program. These should not be in traces produced by execution.
    TrLoc (0, _, _) ->
      Debug.crash <| "traceToLocEquation: Found locId of 0 in trace. " ++ (toString trace)

    -- HACK: see LangSvg.vNumFrozen...
    -- TODO: streamline Trace, LocEquation, etc.
    TrLoc (-999, _, numString) ->
      EqnConst (Utils.fromOkay "traceToLocEquation" (String.toFloat numString))

    TrLoc (locId, _, _) ->
      EqnVar locId

    TrOp op traces ->
      EqnOp op (List.map traceToLocEquation traces)


-- For all locId's in the locIdToNum dictionary, replace
-- corresponding EqnVar nodes with EqnConst nodes.
constantifyLocs : Dict.Dict LocId Num -> LocEquation -> LocEquation
constantifyLocs locIdToNum eqn =
  case eqn of
    EqnConst n ->
      eqn

    EqnVar locId ->
      case Dict.get locId locIdToNum of
        Just n  -> EqnConst n
        Nothing -> eqn

    EqnOp op childEqns ->
      EqnOp op <| List.map (constantifyLocs locIdToNum) childEqns


-- For debuging
locEqnToString : LocEquation -> String
locEqnToString eqn =
  case eqn of
    EqnConst n   -> toString n
    EqnVar locId -> "k" ++ toString locId
    EqnOp op [left, right] ->
      "(" ++ locEqnToString left ++ " " ++ strOp op ++ " " ++ locEqnToString right ++ ")"
    EqnOp op children ->
      "(" ++ strOp op ++ " " ++ String.join " " (List.map locEqnToString children) ++ ")"


locEqnToLittle : Dict.Dict LocId Ident -> LocEquation -> String
locEqnToLittle locIdToLittle eqn =
  case eqn of
    EqnConst n ->
      toString n ++ "!"

    EqnVar locId ->
      case Dict.get locId locIdToLittle of
        Just littleStr -> littleStr
        Nothing        -> let _ = (debugLog "missing locId" locId) in "?"

    EqnOp op childEqns ->
      let childLittleStrs = List.map (locEqnToLittle locIdToLittle) childEqns in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"


locEqnToExp : Frozen -> Dict.Dict LocId Num -> Dict.Dict LocId Ident -> LocEquation -> Exp
locEqnToExp constantAnnotation locIdToFrozenNum locIdToIdent eqn =
  case eqn of
    EqnConst n ->
      eConst n (dummyLoc_ constantAnnotation)

    EqnVar locId ->
      case Dict.get locId locIdToIdent of
        Just ident -> eVar ident
        Nothing    ->
          case Dict.get locId locIdToFrozenNum of
            Just n  -> eConst n (dummyLoc_ frozen)
            Nothing -> eVar ("couldNotFindLocId" ++ toString locId)

    EqnOp op childEqns ->
      let childExps = List.map (locEqnToExp constantAnnotation locIdToFrozenNum locIdToIdent) childEqns in
      eOp op childExps
