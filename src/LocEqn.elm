module LocEqn exposing (..)

import Lang exposing (..)
import Config

import Dict
import Set
import String

debugLog = Config.debugLog Config.debugSync

-- For solving.
type LocEquation
  = LocEqnConst Num
  | LocEqnLoc LocId
  | LocEqnOp Op_ (List LocEquation)

-- Repeated perform simple simplifications:
-- Remove multiply/divide by 1
-- Turn divide by -1 into multiply by -1
-- Cause multiply by -1 on subtraction to merely flip operands
-- Turn division by constant into multiplication by constant
-- Combine multiple multiplications by constants
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
                  -- (+ (- a b) b) to a
                  (LocEqnOp Minus [a, b], c) -> if b == c then a else eqn'
                  (c, LocEqnOp Minus [a, b]) -> if b == c then a else eqn'
                  (LocEqnConst a,
                   LocEqnConst b)    -> LocEqnConst (a + b)
                  _                  -> eqn'

              Minus ->
                case (left, right) of
                  (_, LocEqnConst 0) -> left
                  -- Double minus to plus
                  (LocEqnConst 0,
                   LocEqnOp Minus [LocEqnConst 0, stuff]) -> stuff
                  -- (- 0! (- l r)) to (- r l)
                  (LocEqnConst 0,
                   LocEqnOp Minus [subleft, subright]) -> LocEqnOp Minus [subright, subleft]
                  -- (- 0! (* k stuff)) to (* -k stuff)
                  (LocEqnConst 0,
                   LocEqnOp Mult [LocEqnConst k, stuff]) -> LocEqnOp Mult [LocEqnConst -k, stuff]
                  (LocEqnConst 0,
                   LocEqnOp Mult [stuff, LocEqnConst k]) -> LocEqnOp Mult [LocEqnConst -k, stuff]
                  -- (- 0! (/ k stuff)) to (/ -k stuff)
                  (LocEqnConst 0,
                   LocEqnOp Div [LocEqnConst k, stuff]) -> LocEqnOp Div [LocEqnConst -k, stuff]
                  -- (- a (- b c)) to (+ a (- c b))
                  -- allows (- a (- 0 c)) to become (+ a (- c 0)) which becomes
                  -- (+ a c)
                  (a, LocEqnOp Minus [b, c]) -> LocEqnOp Plus [a, LocEqnOp Minus [c, b]]
                  -- (- (+ a b) b) to a
                  (LocEqnOp Plus [a, b], c) ->
                    if b == c then a
                    else if a == c then b
                    else eqn'
                  -- (- b (+ a b)) to (- 0 a)
                  (c, LocEqnOp Plus [a, b]) ->
                    if b == c then LocEqnOp Minus [LocEqnConst 0, a]
                    else if a == c then LocEqnOp Minus [LocEqnConst 0, b]
                    else eqn'
                  (LocEqnConst a,
                   LocEqnConst b)    -> LocEqnConst (a - b)
                  _                  ->
                    -- Alas, this is syntactic equality not semantic.
                    if left == right then
                      LocEqnConst 0
                    else
                      eqn'

              Mult ->
                case (left, right) of
                  (LocEqnConst 1, _) -> right
                  (_, LocEqnConst 1) -> left
                  (LocEqnConst 0, _) -> LocEqnConst 0
                  (_, LocEqnConst 0) -> LocEqnConst 0
                  (LocEqnConst a,
                   LocEqnConst b)    -> LocEqnConst (a * b)
                  -- Multiplication by -1 of subtraction...just flip operands:
                  (LocEqnConst -1, LocEqnOp Minus  [subleft, subright]) -> LocEqnOp Minus  [subright, subleft]
                  (LocEqnOp Minus [subleft, subright], LocEqnConst -1)  -> LocEqnOp Minus  [subright, subleft]
                  -- Turn 6 * (x * 7) into 42 * x
                  (LocEqnConst c1, LocEqnOp Mult [LocEqnConst c2, sub]) -> LocEqnOp Mult [LocEqnConst (c1 * c2), sub]
                  (LocEqnConst c1, LocEqnOp Mult [sub, LocEqnConst c2]) -> LocEqnOp Mult [LocEqnConst (c1 * c2), sub]
                  (LocEqnOp Mult [LocEqnConst c2, sub], LocEqnConst c1) -> LocEqnOp Mult [LocEqnConst (c1 * c2), sub]
                  (LocEqnOp Mult [sub, LocEqnConst c2], LocEqnConst c1) -> LocEqnOp Mult [LocEqnConst (c1 * c2), sub]
                  _ -> eqn'

              Div ->
                case (left, right) of
                  (_, LocEqnConst 1)  -> left
                  (_, LocEqnConst -1) -> LocEqnOp Mult [(LocEqnConst -1), left]
                  -- Division by 0 will be handled elsewhere.
                  -- We don't want to produce infinity here.
                  (LocEqnConst a,
                   LocEqnConst b)     -> if b /= 0 then LocEqnConst (a / b) else eqn'
                  (LocEqnConst 0, _)  -> LocEqnConst 0
                  (_, LocEqnConst b)  -> if b /= 0 then LocEqnOp Mult [(LocEqnConst (1 / b)), left] else eqn'
                  _                   ->
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
-- Because once in that form, we can solve for the targetLoc.
--
-- Returns Nothing if equation is not linear in LocId
locEqnTerms : LocId -> LocEquation -> Maybe (Float, LocEquation, LocEquation)
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
          [Just (leftLocPow,  leftCoeff,  leftRest),
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


locEqnEval locIdToNum eqn =
  locEqnEval_ (constantifyLocs locIdToNum eqn)


locEqnEval_ eqn =
  case eqn of
    LocEqnConst n       -> n
    LocEqnLoc locId     -> Debug.crash "shouldn't have locs in constantified eqn"
    LocEqnOp op [leftChild, rightChild] ->
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
