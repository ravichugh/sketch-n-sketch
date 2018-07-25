module LocEqn exposing (..)

import Lang exposing (..)
import ValUnparser exposing (..)
import Config
import Utils exposing (infinity)
import Solver exposing (MathExp(..))

import Dict
import Set
import String

debugLog = Config.debugLog Config.debugSync

--
-- Why so many equation types?
--
-- Trace           - terminals are locs (LocId, Frozen, Ident); no plain numbers.
-- FeatureEquation - terminals are are numTrs (Num, Trace); allows introduction of constants like 0.5 for midpoint calculation.
-- MathExp         - traces inlined, terminals are locIds and plain numbers; only Plus/Minus/Mult/Div supported in some places (that used to be LocEqn)
-- Poly/NormPoly   - wider tree to ease simplification; can represent arbitrary exponents but only constant exponents supported right now.
--
-- If traces supported constants (perhaps as another field to the loc), then Trace & FeatureEquation & MathExp could be consolidated.
--


-- The old "LocEquation" and the new "MathExp" have identical structure and are now consolidated.
-- All functions below are renamed to be "mathExp" functions. Previously they were "locEqn" functions.
--
-- The functions in this file are all candidates for deletion or for moving to a new location.
--
-- Goal is to delete a lot of code.



-- Holdover until we can discard this file.
type alias LocEquation = Solver.MathExp
-- type LocEquation
--   = LocMathNum Num
--   | LocEqnLoc LocId
--   | LocMathOp Op_ (List MathExp)


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
mathExpSimplify : MathExp -> MathExp
mathExpSimplify mathExp =
  let simplified =
    case mathExp of
      MathNum n ->
        mathExp

      MathVar locId ->
        mathExp

      MathOp op children ->
        let children_ = List.map mathExpSimplify children in
        let mathExp_ = MathOp op children_ in
        case children_ of
          [left, right] ->
            case op of
              Plus ->
                case (left, right) of
                  (MathNum 0, _) -> right
                  (_, MathNum 0) -> left
                  -- (+ (- a b) b) to a
                  (MathOp Minus [a, b], c) -> if b == c then a else mathExp_
                  (c, MathOp Minus [a, b]) -> if b == c then a else mathExp_
                  (MathNum a,
                   MathNum b)    -> MathNum (a + b)
                  _              -> mathExp_

              Minus ->
                case (left, right) of
                  (_, MathNum 0) -> left
                  -- Double minus to plus
                  (MathNum 0,
                   MathOp Minus [MathNum 0, stuff]) -> stuff
                  -- (- 0! (- l r)) to (- r l)
                  (MathNum 0,
                   MathOp Minus [subleft, subright]) -> MathOp Minus [subright, subleft]
                  -- (- 0! (* k stuff)) to (* -k stuff)
                  (MathNum 0,
                   MathOp Mult [MathNum k, stuff]) -> MathOp Mult [MathNum -k, stuff]
                  (MathNum 0,
                   MathOp Mult [stuff, MathNum k]) -> MathOp Mult [MathNum -k, stuff]
                  -- (- 0! (/ k stuff)) to (/ -k stuff)
                  (MathNum 0,
                   MathOp Div [MathNum k, stuff]) -> MathOp Div [MathNum -k, stuff]
                  -- (- a (- b c)) to (+ a (- c b))
                  -- allows (- a (- 0 c)) to become (+ a (- c 0)) which becomes
                  -- (+ a c)
                  (a, MathOp Minus [b, c]) -> MathOp Plus [a, MathOp Minus [c, b]]
                  -- (- (+ a b) b) to a
                  (MathOp Plus [a, b], c) ->
                    if b == c then a
                    else if a == c then b
                    else mathExp_
                  -- (- b (+ a b)) to (- 0 a)
                  (c, MathOp Plus [a, b]) ->
                    if b == c then MathOp Minus [MathNum 0, a]
                    else if a == c then MathOp Minus [MathNum 0, b]
                    else mathExp_
                  (MathNum a,
                   MathNum b)    -> MathNum (a - b)
                  _                  ->
                    -- Alas, this is syntactic equality not semantic.
                    if left == right then
                      MathNum 0
                    else
                      mathExp_

              Mult ->
                case (left, right) of
                  (MathNum 1, _) -> right
                  (_, MathNum 1) -> left
                  (MathNum 0, _) -> MathNum 0
                  (_, MathNum 0) -> MathNum 0
                  (MathNum a,
                   MathNum b)    -> MathNum (a * b)
                  -- Multiplication by -1 of subtraction...just flip operands:
                  (MathNum -1, MathOp Minus  [subleft, subright]) -> MathOp Minus  [subright, subleft]
                  (MathOp Minus [subleft, subright], MathNum -1)  -> MathOp Minus  [subright, subleft]
                  -- Turn 6 * (x * 7) into 42 * x
                  (MathNum c1, MathOp Mult [MathNum c2, sub]) -> MathOp Mult [MathNum (c1 * c2), sub]
                  (MathNum c1, MathOp Mult [sub, MathNum c2]) -> MathOp Mult [MathNum (c1 * c2), sub]
                  (MathOp Mult [MathNum c2, sub], MathNum c1) -> MathOp Mult [MathNum (c1 * c2), sub]
                  (MathOp Mult [sub, MathNum c2], MathNum c1) -> MathOp Mult [MathNum (c1 * c2), sub]
                  _ -> mathExp_

              Div ->
                case (left, right) of
                  (_, MathNum 1)  -> left
                  (_, MathNum -1) -> MathOp Mult [(MathNum -1), left]
                  -- Division by 0 will be handled elsewhere.
                  -- We don_t want to produce infinity here.
                  (MathNum a,
                   MathNum b)     -> if b /= 0 then MathNum (a / b) else mathExp_
                  (MathNum 0, _)  -> MathNum 0
                  (_, MathNum b)  -> if b /= 0 then MathOp Mult [(MathNum (1 / b)), left] else mathExp_
                  _                   ->
                    -- Alas, this is syntactic equality not semantic.
                    if left == right && right /= MathNum 0 then
                      MathNum 1
                    else
                      mathExp_

              _ ->
                mathExp_

          _ ->
            Debug.crash <| "mathExpSimplify: op without 2 children " ++ (toString mathExp)
  in
  if simplified == mathExp then
    mathExp
  else
    debugLog "double simplification"
    <| mathExpSimplify simplified


-- Returns (locPow, coefficient of targetLoc, everything else)
--
-- i.e. (coeff mathExp)*targetLoc^locPow + (everything else mathExp)
--
-- Because once in that form, we can solve for the targetLoc.
--
-- Returns Nothing if equation is not linear in LocId
mathExpTerms : LocId -> MathExp -> Maybe (Float, MathExp, MathExp)
mathExpTerms targetLocId mathExp =
  case mathExp of
    MathNum n ->
      Just (1, MathNum 0, mathExp)

    MathVar locId ->
      if locId == targetLocId
      then Just (1, MathNum 1, MathNum 0)
      else Just (1, MathNum 0, mathExp)

    MathOp op children ->
      let children_ = List.map (mathExpTerms targetLocId) children in
      let result =
        case children_ of
          [Just (leftLocPow,  leftCoeff,  leftRest),
           Just (rightLocPow, rightCoeff, rightRest)] ->
            case op of
              Plus ->
                if leftLocPow == rightLocPow then
                  Just (leftLocPow,
                        MathOp Plus [leftCoeff, rightCoeff],
                        MathOp Plus [leftRest, rightRest])
                else
                  -- Not easily solvable, powers of the target loc don't match.
                  Nothing

              Minus ->
                if leftLocPow == rightLocPow then
                  Just (leftLocPow,
                        MathOp Minus [leftCoeff, rightCoeff],
                        MathOp Minus [leftRest, rightRest])
                else
                  -- Not easily solvable, powers of the target loc don't match.
                  Nothing

              Mult ->
                case (leftCoeff, leftRest, rightCoeff, rightRest) of
                  -- Left side doesn't contain target loc
                  (MathNum 0, _, _, _) ->
                    Just (rightLocPow,
                          MathOp Mult [leftRest, rightCoeff],
                          MathOp Mult [leftRest, rightRest])

                  -- Right side doesn't contain target loc
                  (_, _, MathNum 0, _) ->
                    Just (leftLocPow,
                          MathOp Mult [leftCoeff, rightRest],
                          MathOp Mult [leftRest, rightRest])

                  -- Both sides only contain terms of the coeff
                  (_, MathNum 0, _, MathNum 0) ->
                    let newPow = leftLocPow + rightLocPow in
                    if newPow == 0 then
                      Just (1, MathNum 0, MathNum 1)
                    else
                      Just (newPow,
                            MathOp Mult [leftCoeff, rightCoeff],
                            MathNum 0)

                  _ ->
                    -- Equation is too difficult for us :-(
                    Nothing

              Div ->
                -- Division is problematic
                case (leftCoeff, leftRest, rightCoeff, rightRest) of
                  -- Division by 0
                  (_, _, MathNum 0, MathNum 0) ->
                    Nothing

                  -- Denominator doesn't contain target loc,
                  -- simple distribution.
                  (_, _, MathNum 0, _) ->
                    Just (leftLocPow,
                          MathOp Div [leftCoeff, rightRest],
                          MathOp Div [leftRest, rightRest])

                  -- Denominator is some power and coeff of target loc,
                  -- numerator does not contain target loc
                  (MathNum 0, _, MathNum 1, MathNum 0) ->
                    Just (-rightLocPow,
                          MathOp Div [leftRest, rightCoeff],
                          MathNum 0)

                  -- Numerator and denominator are both terms of the target loc
                  (_, MathNum 0, _, MathNum 0) ->
                    if leftLocPow == rightLocPow then
                      Just (1, MathNum 0, MathOp Div [leftCoeff, rightCoeff])
                    else
                      Just (rightLocPow - leftLocPow,
                            MathOp Div [leftCoeff, rightCoeff],
                            MathNum 0)

                  _ ->
                    -- Maybe the numerator and denominator are magically
                    -- syntactically equal.
                    -- (Not smart enough to detect multiples)
                    if leftLocPow == rightLocPow && leftCoeff == rightCoeff && leftRest == rightRest then
                      Just (1, MathNum 0, MathNum 1)
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
-- then packages back up into a MathExp
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

mathExpToPoly : MathExp -> Poly
mathExpToPoly mathExp =
  case mathExp of
    MathNum n ->
      PolyMult n []

    MathVar locId ->
      PolyLoc locId

    MathOp Plus children ->
      PolyAdd <| List.map mathExpToPoly children

    MathOp Minus (leftChild::otherChildren) ->
      PolyAdd <| (mathExpToPoly leftChild)::(List.map (\child -> PolyMult -1 [mathExpToPoly child]) otherChildren)

    MathOp Mult children ->
      PolyMult 1 <| List.map mathExpToPoly children

    MathOp Div (leftChild::otherChildren) ->
      PolyMult 1 <| (mathExpToPoly leftChild)::(List.map (\child -> PolyPow (mathExpToPoly child) (PolyMult -1 [])) otherChildren)

    MathOp _ _ ->
      Debug.crash <| "LocEqn.mathExpToPoly can only handle Plus/Minus/Mult/Div, got " ++ (toString mathExp)


-- type NormPoly     = NormPolyTopLevelAdd (List NormPolyMult)
-- type NormPolyMult = NormPolyMult Num (List NormPolyPow)
-- type NormPolyPow  = NormPolyPow NormPolyAdd NormPolyAdd
-- type NormPolyAdd  = NormPolyAdd (List NormPolyMult)
--                   | NormPolyLoc LocId

normPolyToMathExp : NormPoly -> MathExp
normPolyToMathExp (NormPolyTopLevelAdd normPolyMultTerms) =
  normPolyAddToMathExp (NormPolyAdd normPolyMultTerms)

normPolyAddToMathExp : NormPolyAdd -> MathExp
normPolyAddToMathExp normPolyAdd =
  case normPolyAdd of
    NormPolyLoc locId ->
      MathVar locId

    NormPolyAdd normPolyMultTerms ->
      case normPolyMultTerms of
        [] ->
          MathNum 0

        multTerm::[] ->
          normPolyMultToMathExp multTerm

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
            normPolyMultToMathExp (NormPolyMult coeffFactor [NormPolyPow (NormPolyAdd factoredTerms) (NormPolyAdd [NormPolyMult 1 []])])
          else
            let (negativeTerms, nonNegativeTerms) =
              normPolyMultTerms
              |> List.partition (\(NormPolyMult coeff normPolyPowTerms) -> coeff < 0)
            in
            case (negativeTerms, nonNegativeTerms) of
              (_, firstNonNegTerm::secondNonNegTerm::otherNonNegTerms) ->
                -- We have at least two non-negative terms. Wait to do the subtraction.
                let remainingTerms = secondNonNegTerm::otherNonNegTerms ++ negativeTerms in
                MathOp Plus [normPolyMultToMathExp firstNonNegTerm, normPolyAddToMathExp (NormPolyAdd remainingTerms)]

              (_::_, onlyNonNegTerm::[]) ->
                let negNegativeTerms = negativeTerms |> List.map (normPolyMultScalarMult -1) in
                MathOp Minus [normPolyMultToMathExp onlyNonNegTerm, normPolyAddToMathExp (NormPolyAdd negNegativeTerms)]

              (_::_, []) ->
                -- Only negative terms. :(
                -- Will build a common factor extractor later.
                MathOp Plus [normPolyMultToMathExp multTerm, normPolyAddToMathExp (NormPolyAdd otherMultTerms)]

              ([], _::[]) ->
                Debug.crash "LocEqn.normPolyAddToMathExp shouldn't get here; single term handled in earlier case statement"

              ([], []) ->
                Debug.crash "LocEqn.normPolyAddToMathExp shouldn't get here; empty term list in earlier case statement"



normPolyMultToMathExp : NormPolyMult -> MathExp
normPolyMultToMathExp (NormPolyMult coeff normPolyPowTerms) =
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
      _                               -> Debug.crash <| "LocEqn.normPolyMultToMathExp shouldn't reach here; got " ++ toString (NormPolyPow baseTerm exponentTerm)
  in
  case (negativePowers, otherPowers, coeff) of
    (_, _, 0) ->
      MathNum 0

    ([], [], _) ->
      MathNum coeff

    (_::_, _, _) ->
      MathOp Div
          [ normPolyMultToMathExp (NormPolyMult coeff otherPowers)
          , normPolyMultToMathExp (NormPolyMult 1 (List.map flipExponentSign negativePowers))
          ]

    ([], _, 1) ->
      -- This is the main branch; other branches reduce to this.
      case otherPowers of
        term::[] ->
          normPolyPowToMathExp term

        headTerm::otherTerms ->
          MathOp Mult
              [ normPolyPowToMathExp headTerm
              , normPolyMultToMathExp (NormPolyMult 1 otherTerms)
              ]

        [] ->
          Debug.crash <| "LocEqn.normPolyMultToMathExp shouldn't get here; should have at least some terms"

    ([], _, _) ->
      if abs coeff >= 1 then
        MathOp Mult
          [ normPolyMultToMathExp (NormPolyMult 1 otherPowers)
          , MathNum coeff
          ]
      else
        MathOp Div
          [ normPolyMultToMathExp (NormPolyMult 1 otherPowers)
          , MathNum (1 / coeff)
          ]


normPolyPowToMathExp (NormPolyPow normPolyBase normPolyExponent) =
  case normPolyExponent of
    NormPolyAdd [] ->
      MathNum 1

    NormPolyAdd [NormPolyMult 0 []] ->
      MathNum 1

    NormPolyAdd [NormPolyMult 1 []] ->
      normPolyAddToMathExp normPolyBase

    NormPolyAdd [NormPolyMult n []] ->
      if n > 1 && n == toFloat (round n) then
        MathOp Mult
          [ normPolyAddToMathExp normPolyBase
          , normPolyPowToMathExp (NormPolyPow normPolyBase (NormPolyAdd [NormPolyMult (toFloat (round n) - 1) []]))
          ]
      else
        Debug.crash <| "LocEqn.normPolyPowToMathExp unexpected exponent: should only be positive integers >1 by now: got " ++ (toString (NormPolyPow normPolyBase normPolyExponent))

    _ ->
      Debug.crash <| "LocEqn.normPolyPowToMathExp can't handle non-integer, non-constant powers yet: got " ++ (toString (NormPolyPow normPolyBase normPolyExponent))



normalizeSimplify : MathExp -> MathExp
normalizeSimplify mathExp =
  -- let _ = Debug.log ("Simplifying " ++ mathExpToLittle Dict.empty mathExp) () in
  let polyEqn = mathExpToPoly mathExp in
  -- let _ = Debug.log ("Poly version " ++ polyToString polyEqn) () in
  let normPolyResult = polyNorm polyEqn in
  -- let _ = Debug.log ("Got " ++ normPolyToString normPolyResult) () in
  let mathExpResult = normPolyToMathExp normPolyResult |> correctFloatErrors in
  -- let _ = Debug.log ("As little " ++ mathExpToLittle Dict.empty littleResult) () in
  mathExpResult


-- Find e.g. 1.4999999999999 and change to 1.5.
correctFloatErrors : MathExp -> MathExp
correctFloatErrors mathExp =
  case mathExp of
    MathNum n           -> MathNum (Utils.correctFloatError n)
    MathVar _           -> mathExp
    MathOp op_ children -> MathOp op_ (List.map correctFloatErrors children)


-- mathExpsOfSize astSize locsToUse =
--   if astSize < 1 then
--     []
--   else if astSize == 1 then
--     -- From stats of all our little programs so far.
--     [ MathNum 0
--     , MathNum 1
--     , MathNum 2
--     , MathNum 10
--     , MathNum 3
--     , MathNum 20
--     , MathNum 50
--     , MathNum 4
--     , MathNum 300
--     , MathNum 5
--     , MathNum 0.5
--     , MathNum 100
--     , MathNum 200
--     , MathNum 30
--     , MathNum 60
--     , MathNum 80
--     , MathNum 15
--     , MathNum 360
--     , MathNum 180
--     , MathNum 120
--     , MathNum 6
--     , MathNum 150
--     , MathNum 40
--     , MathNum 8
--     ] ++ (Set.toList locsToUse |> List.map (\(locId, _, _) -> MathVar locId))
--   else if astSize == 2 then
--     -- No unops in MathExps yet
--     []
--   else
--     [Plus, Minus, Mult, Div]
--     |> List.concatMap (\op ->
--       (List.range 1 (astSize - 2))
--       |> List.concatMap (\leftSize ->
--         mathExpsOfSize leftSize locsToUse
--         |> List.concatMap (\leftMathExp ->
--           let rightSize = astSize - leftSize - 1 in
--           mathExpsOfSize rightSize locsToUse
--           |> List.map (\rightMathExp ->
--             MathOp op [leftMathExp, rightMathExp]
--           )
--         )
--       )
--     )


solveForLocValue : LocId -> Subst -> MathExp -> Num -> Maybe Num
solveForLocValue targetLocId subst mathExp mathExpTargetValue =
  let mathExpEqualToZero =
    constantifyLocs (Dict.remove targetLocId subst) (MathOp Minus [mathExp, MathNum mathExpTargetValue])
  in
  case mathExpTerms targetLocId mathExpEqualToZero of
    Just (pow, coeffMathExp, restMathExp) ->
      -- We have: coeff*x^pow + rest = 0
      -- We want: x = (-rest / coeff)^(1/pow)
      let coeffEvaled = mathExpEval Dict.empty coeffMathExp in
      let restEvaled  = mathExpEval Dict.empty restMathExp in
      let x = (-restEvaled / coeffEvaled)^(1/pow) in
      if (isNaN x) || (isInfinite x) then
        Nothing
      else
        Just x

    Nothing ->
      Nothing


solveForConst : Subst -> MathExp -> Num -> Maybe Num
solveForConst subst mathExp mathExpTargetValue =
  let locifyConstant mathExp =
    case mathExp of
      MathNum _          -> MathVar -2
      MathVar _          -> mathExp
      MathOp op children -> MathOp op (List.map locifyConstant children)
  in
  solveForLocValue -2 subst (locifyConstant mathExp) mathExpTargetValue


-- Will abort if any op other than + - * /
--
-- Must be linear in the locId solved for.
--
-- Convert to just locIds (variables) and constants
solveForLocUnchecked : LocId -> Dict.Dict LocId Num -> MathExp -> MathExp -> Maybe MathExp
solveForLocUnchecked locId locIdToNum lhs rhs =
  let maybeMathExp =
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
        case mathExpTerms locId (MathOp Minus [lhs__, rhs__]) of
          Just (locPow, locCoeff, rest) ->
            if locPow == 0 || locCoeff == MathNum 0 then
              Nothing
            else if locPow == 1 then
              -- We have: coeff*x + rest = 0
              -- We want: x = something
              Just <|
              normalizeSimplify <|
                MathOp Div
                    [ MathOp Minus [MathNum 0, rest]
                    , locCoeff]

            else if locPow == -1 then
              -- We have: coeff/x + rest = 0
              -- We want: x = something
              Just <|
              normalizeSimplify <|
                MathOp Div
                    [ locCoeff
                    , MathOp Minus [MathNum 0, rest]]
            else
              -- Just need to add a pow op and then we can handle more pows.
              Nothing

          Nothing ->
            Nothing
  in
  maybeMathExp


-- solveForLoc : LocId -> Dict.Dict LocId Num -> Subst -> MathExp -> MathExp -> Maybe MathExp
-- solveForLoc locId locIdToNum subst lhs rhs =
--   -- Check that equation doesn't produce NaN or similar...
--   case solveForLocUnchecked locId locIdToNum lhs rhs of
--     Just mathExp ->
--       -- Need the full subst, not just frozen constants.
--       let evaled = mathExpEval subst mathExp in
--       if (isNaN evaled) || (isInfinite evaled)
--       then Nothing
--       else Just mathExp
--
--     Nothing ->
--       Nothing


-- Help out our not-so-smart simplifier.
-- If lhs and rhs are identical but for some sub-expression,
-- return just the differing sub-expressions.
maybeExtractUnsharedExpression : MathExp -> MathExp -> Maybe (MathExp, MathExp)
maybeExtractUnsharedExpression lhs rhs =
  case (lhs, rhs) of
    (MathNum ln, MathNum rn) ->
      if ln == rn
      then Nothing
      else Just (lhs, rhs)

    (MathVar lLocId, MathVar rLocId) ->
      if lLocId == rLocId
      then Nothing
      else Just (lhs, rhs)

    (MathOp lOp lChildren, MathOp rOp rChildren) ->
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
mathExpTemplateFillings targetValue subst locIdSet template =
  let allLocFillings = mathExpTemplateLocFillings (locIdSet |> Set.toList) template in
  mathExpTemplateFillingsLocsFilled targetValue subst littleConstants allLocFillings

mathExpTemplateFillingsLocsFilled : Num -> Subst -> List Num -> List MathExp -> List MathExp
mathExpTemplateFillingsLocsFilled targetValue subst constants locFillings =
  let filledWithNiceNumber =
    locFillings
    |> List.map
        (\locFilledMathExp ->
          mathExpTemplateConstantFillings constants locFilledMathExp
          |> List.sortBy (\mathExp -> abs (mathExpEval subst mathExp - targetValue))
          |> Utils.head "LocEqn.mathExpTemplateFillings ranking"
        )
  in
  let filledWithExactNumber =
    let fillInConstant const mathExp =
      case mathExp of
        MathNum _          -> MathNum const
        MathVar _          -> mathExp
        MathOp op children -> MathOp op (List.map (fillInConstant const) children)
    in
    locFillings
    |> List.filterMap
        (\locFilledMathExp ->
          -- -- Only produce an exact solution if no locs appear more than once.
          -- -- Cuts down on *some* junk.
          -- if Set.size (mathExpLocIdSet locFilledMathExp) == List.length (mathExpLocIds locFilledMathExp) then
          --   solveForConst subst locFilledMathExp targetValue
          --   |> Maybe.map (\const -> fillInConstant const locFilledMathExp)
          -- else
          --   Nothing

          -- Only produce an exact solution if only one loc in mathExp.
          if List.length (mathExpLocIds locFilledMathExp) <= 1 then
            solveForConst subst locFilledMathExp targetValue
            |> Maybe.map (\const -> fillInConstant const locFilledMathExp)
          else
            Nothing
        )
  in
  filledWithNiceNumber ++ filledWithExactNumber


-- locIds should be unique.
mathExpTemplateLocFillings : List LocId -> MathExp -> List MathExp
mathExpTemplateLocFillings locIds mathExp =
  case mathExp of
    MathNum _          -> [mathExp]
    MathVar _          -> List.map MathVar locIds
    MathOp op children ->
      children
      |> List.foldl
        (\child priorCombos ->
          let thisChildFillings = mathExpTemplateLocFillings locIds child in
          thisChildFillings
          |> List.concatMap (\childFilling -> priorCombos |> List.map (\priorArgs -> priorArgs ++ [childFilling]))
        )
        [[]]
      |> List.map (MathOp op)


mathExpTemplateConstantFillings : List Num -> MathExp -> List MathExp
mathExpTemplateConstantFillings constants mathExp =
  case mathExp of
    MathNum _          -> List.map MathNum constants
    MathVar _          -> [mathExp]
    MathOp op children ->
      children
      |> List.foldl
        (\child priorCombos ->
          let thisChildFillings = mathExpTemplateConstantFillings constants child in
          thisChildFillings
          |> List.concatMap (\childFilling -> priorCombos |> List.map (\priorArgs -> priorArgs ++ [childFilling]))
        )
        [[]]
      |> List.map (MathOp op)


atLeastNLocs     n template = List.length (mathExpLocIds template) >= n
atMostNConstants n template = List.length (locMathNums template) <= n

-- Templates for synthesis:
-- Returns all terms of a certain shape.
-- Exact numberic values and variables will be filled in later.
mathExpsTemplatesOfSize minLocs maxConsts astSize =
  mathExpsTemplatesOfSize_ astSize
  |> List.filter (atLeastNLocs minLocs)
  |> List.filter (atMostNConstants maxConsts)

-- Could be more time and memory effecient with explicit memoization (build up from size 1)
-- But I don't think this part is the bottleneck
mathExpsTemplatesOfSize_ astSize =
  if astSize < 1 then
    []
  else if astSize == 1 then
    [ MathNum -1, MathVar -1 ]
  else if astSize == 2 then
    -- No unops in MathExps yet
    []
  else
    [Plus, Minus, Mult, Div]
    |> List.concatMap (\op ->
      (List.range 1 (astSize - 2))
      |> List.concatMap (\leftSize ->
        mathExpsTemplatesOfSize_ leftSize
        |> List.concatMap (\leftMathExp ->
          let rightSize = astSize - leftSize - 1 in
          mathExpsTemplatesOfSize_ rightSize
          |> List.map (\rightMathExp ->
            MathOp op [leftMathExp, rightMathExp]
          )
        )
      )
    )

mathExpSize : MathExp -> Int
mathExpSize mathExp =
  case mathExp of
    MathNum _         -> 1
    MathVar _         -> 1
    MathOp _ children -> 1 + List.sum (List.map mathExpSize children)

mathExpLocIdSet : MathExp -> Set.Set LocId
mathExpLocIdSet mathExp =
  mathExpLocIds mathExp |> Set.fromList

locMathNums : MathExp -> List Num
locMathNums mathExp =
  case mathExp of
    MathNum n         -> [n]
    MathVar locId     -> []
    MathOp _ children -> List.concatMap locMathNums children

mathExpLocIds : MathExp -> List LocId
mathExpLocIds mathExp =
  case mathExp of
    MathNum _         -> []
    MathVar locId     -> [locId]
    MathOp _ children -> List.concatMap mathExpLocIds children


mathExpEval locIdToNum mathExp =
  mathExp
  |> Solver.applySubst locIdToNum
  |> Solver.evalToMaybeNum
  |> Utils.fromJust__ (\_ -> "LocEqn.mathExpEval incomplete subst " ++ toString (locIdToNum, mathExp))


traceToMathExp : Trace -> MathExp
traceToMathExp trace =
  case trace of
    -- locId of 0 means it's a constant that's part of the feature equation,
    -- not the program. These should not be in traces produced by execution.
    TrLoc (0, _, _) ->
      Debug.crash <| "traceToMathExp: Found locId of 0 in trace. " ++ (toString trace)

    -- HACK: see LangSvg.vNumFrozen...
    -- TODO: streamline Trace, MathExp, etc.
    TrLoc (-999, _, numString) ->
      MathNum (Utils.fromOkay "traceToMathExp" (String.toFloat numString))

    TrLoc (locId, _, _) ->
      MathVar locId

    TrOp op traces ->
      MathOp op (List.map traceToMathExp traces)


-- For all locId's in the locIdToNum dictionary, replace
-- corresponding MathVar nodes with MathNum nodes.
constantifyLocs : Dict.Dict LocId Num -> MathExp -> MathExp
constantifyLocs locIdToNum mathExp =
  case mathExp of
    MathNum n ->
      mathExp

    MathVar locId ->
      case Dict.get locId locIdToNum of
        Just n  -> MathNum n
        Nothing -> mathExp

    MathOp op childMathExps ->
      MathOp op <| List.map (constantifyLocs locIdToNum) childMathExps


-- For debuging
mathExpToString : MathExp -> String
mathExpToString mathExp =
  case mathExp of
    MathNum n     -> toString n
    MathVar locId -> "k" ++ toString locId
    MathOp op [left, right] ->
      "(" ++ mathExpToString left ++ " " ++ strOp op ++ " " ++ mathExpToString right ++ ")"
    MathOp op children ->
      "(" ++ strOp op ++ " " ++ String.join " " (List.map mathExpToString children) ++ ")"


mathExpToLittle : Dict.Dict LocId Ident -> MathExp -> String
mathExpToLittle locIdToLittle mathExp =
  case mathExp of
    MathNum n ->
      toString n ++ "!"

    MathVar locId ->
      case Dict.get locId locIdToLittle of
        Just littleStr -> littleStr
        Nothing        -> let _ = (debugLog "missing locId" locId) in "?"

    MathOp op childMathExps ->
      let childLittleStrs = List.map (mathExpToLittle locIdToLittle) childMathExps in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"


mathExpToExp : Frozen -> Dict.Dict LocId Num -> Dict.Dict LocId Ident -> MathExp -> Exp
mathExpToExp constantAnnotation locIdToFrozenNum locIdToIdent mathExp =
  case mathExp of
    MathNum n ->
      eConst n (dummyLoc_ constantAnnotation)

    MathVar locId ->
      case Dict.get locId locIdToIdent of
        Just ident -> eVar ident
        Nothing    ->
          case Dict.get locId locIdToFrozenNum of
            Just n  -> eConst n (dummyLoc_ frozen)
            Nothing -> eVar ("couldNotFindLocId" ++ toString locId)

    MathOp op childMathExps ->
      let childExps = List.map (mathExpToExp constantAnnotation locIdToFrozenNum locIdToIdent) childMathExps in
      eOp op childExps
