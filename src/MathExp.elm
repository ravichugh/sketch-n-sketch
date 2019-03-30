-- MathExp and utilities used outside Solver

module MathExp exposing (..)


import Lang exposing (MathExp(..), Exp, Num, Op_(..), Ident, Exp__(..))
import Utils

import Dict exposing (Dict)


-- TODO streamline Equation/MathExp; see note in LocEqn.elm

-- MathExp is defined in Lang.elm. For reference:
--
-- type MathExp
--   = MathNum Num
--   | MathVar Int -- Variable identifiers, often locIds
--   | MathOp Op_ (List MathExp)


neg : MathExp -> MathExp
neg x = MathOp Minus [MathNum 0, x]


mathExpToVarIds : MathExp -> List Int
mathExpToVarIds mathExp =
  case mathExp of
    MathNum _           -> []
    MathVar varId       -> [varId]
    MathOp _ childTerms -> List.concatMap mathExpToVarIds childTerms


-- Assumes no variables remain, otherwise returns Nothing with a debug message.
evalToMaybeNum : MathExp -> Maybe Num
evalToMaybeNum mathExp =
  case mathExp of
    MathNum n          -> Just n
    MathVar _          -> let _ = Utils.log ("MathExp.evalToMaybeNum: Found " ++ toString mathExp ++ " in an MathExp that shouldn't have any variables.") in Nothing
    MathOp op operands ->
      -- If operation poisoned by NaN, don't care if there are variables.
      let evaledOperands = List.map evalToMaybeNum operands in
      if evaledOperands |> List.any (Maybe.map isNaN >> (==) (Just True)) then
        Just Utils.nan
      else
        case (op, evaledOperands) of
          -- If one side of multiplication is 0, don't care if the other side has variables.
          (Mult, [Just 0.0, Nothing]) -> Just 0.0
          (Mult, [Nothing, Just 0.0]) -> Just 0.0
          _                   ->
            evaledOperands
            |> Utils.projJusts
            |> Maybe.andThen (\operandNums -> Lang.maybeEvalMathOp op operandNums)


applySubst : Dict Int Num -> MathExp -> MathExp
applySubst subst mathExp =
  case mathExp of
    MathNum n     -> mathExp
    MathVar varId ->
      case Dict.get varId subst of
        Just n  -> MathNum n
        Nothing -> mathExp
    MathOp op_ childTerms -> MathOp op_ (childTerms |> List.map (applySubst subst))


-- https://en.wikipedia.org/wiki/Differentiation_rules
derivative : Int -> MathExp -> MathExp
derivative withRespectToVarId mathExp =
  let ddx mathExp = derivative withRespectToVarId mathExp in
  let failure = MathNum Utils.nan in
  case mathExp of
    MathNum n             -> MathNum 0
    MathVar varId         -> if varId == withRespectToVarId then MathNum 1 else MathNum 0
    MathOp op_ childTerms ->
      let
        chainRule df g = MathOp Mult [df g, ddx g] -- (f(g))' = f'(g) * g'
      in
      case (op_, childTerms) of
        (Plus,    [f, g]) -> MathOp Plus  [ddx f, ddx g]
        (Minus,   [f, g]) -> MathOp Minus [ddx f, ddx g]
        (Mult,    [f, g]) ->
          -- (f*g)' = f'*g + f*g'
          MathOp Plus
              [ MathOp Mult [ddx f, g]
              , MathOp Mult [f, ddx g]
              ]

        (Div,     [f, g]) ->
          -- (f/g)' = ( f'*g - f*g' ) / g^2
          MathOp Div
              [ MathOp Minus [MathOp Mult [ddx f, g], MathOp Mult [f, ddx g]]
              , MathOp Mult [g, g]
              ]

        (Pow,     [f, g]) ->
          -- Could add special cases here if desired for cleanliness.
          -- Assumes f > 0
          -- (f^g)' = f^g * (f'*g/f + g'*ln(f))
          MathOp Mult
              [ mathExp -- f^g
              , MathOp Plus
                  [ MathOp Mult [ddx f, MathOp Div [g, f]] -- f'*g/f
                  , MathOp Mult [ddx g, MathOp Ln [f]]     -- g'*ln(f)
                  ]
              ]

        (Mod,     [f, g]) ->
          case evalToMaybeNum (ddx g) of
            Just 0.0 -> ddx f -- Technically discontinuous when f mod g == 0
            _        -> failure

        (ArcTan2, [f, MathNum 1.0]) ->
          -- Assume 1st or 4th quadrant
          -- (atan x)' = 1 / (1+x^2)
          -- (atan f)' = f' / (1+f^2)
          MathOp Div
              [ ddx f -- f'
              , MathOp Plus [MathNum 1, MathOp Mult [f, f]] -- 1 + f^2
              ]

        (ArcTan2, [f, g]) ->
          -- Assume equivalent to atan(f/g)
          ddx <| MathOp ArcTan2 [MathOp Div [f, g], MathNum 1.0]

        (Cos,     [g])   -> g |> chainRule (\x -> neg (MathOp Sin [x])) -- (cos(x))' = -sin(x)
        (Sin,     [g])   -> g |> chainRule (\x -> MathOp Cos [x]) -- (sin(x))' = cos(x)
        (ArcCos,  [g])   -> g |> chainRule (\x -> MathOp Div [MathNum -1, MathOp Sqrt [MathOp Minus [MathNum 1, MathOp Mult [x, x]]]]) -- (acos(x))' = - 1/sqrt(1-x^2)
        (ArcSin,  [g])   -> g |> chainRule (\x -> MathOp Div [MathNum  1, MathOp Sqrt [MathOp Minus [MathNum 1, MathOp Mult [x, x]]]]) -- (asin(x))' =   1/sqrt(1-x^2)
        (Abs,     [g])   -> g |> chainRule (\x -> MathOp Div [x, MathOp Abs [x]]) -- (abs(x))' = x/abs(x) = sgn(x) -- Discontinuous at 0
        (Floor,   [g])   -> MathNum 0 -- Technically discontinuous when g mod 0 == 0
        (Ceil,    [g])   -> MathNum 0 -- Technically discontinuous when g mod 0 == 0
        (Round,   [g])   -> MathNum 0 -- Technically discontinuous when g mod 0 == 0.5
        (Sqrt,    [g])   -> g |> chainRule (\x -> MathOp Mult [MathNum 0.5, MathOp Pow [x, MathNum -0.5]]) -- (x^0.5)' = 0.5x^-0.5
        (Ln,      [g])   -> MathOp Div [ddx g, g] -- (ln(g))' = g'/g
        (Pi,      [])    -> MathNum 0
        _                -> let _ = Debug.log "Don't know how to differentiate" mathExp in failure


mathExpToExp : (Num -> Exp) -> (Int -> Exp) -> MathExp -> Exp
mathExpToExp numberToExp varIdToExp mathExp =
  case mathExp of
    MathNum n            -> numberToExp n
    MathVar varId        -> varIdToExp varId
    MathOp op childTerms -> Lang.eOp op (List.map (mathExpToExp numberToExp varIdToExp) childTerms) -- Unparser adds parens for us


-- Returned Dict may have extra entries
expToMaybeMathExp : Exp -> Maybe (MathExp, Dict Ident Int)
expToMaybeMathExp exp =
  let identToVarId =
    exp
    |> Lang.flattenExpTree
    |> List.filterMap
        (\e ->
          case e.val.e__ of
            EVar _ ident -> Just ident
            _            -> Nothing
        )
    |> Utils.dedup
    |> Utils.zipi1
    |> List.map Utils.flip
    |> Dict.fromList
  in
  expToMaybeMathExp_ identToVarId exp
  |> Maybe.map (\mathExp -> (mathExp, identToVarId))


expToMaybeMathExp_ : Dict Ident Int -> Exp -> Maybe MathExp
expToMaybeMathExp_ identToVarId exp =
  case (Lang.expEffectiveExp exp).val.e__ of
    EConst _ n _ _ ->
      Just (MathNum n)

    EVar _ ident ->
       Just (MathVar <| Utils.getWithDefault ident -1 identToVarId)

    EOp _ op operandExps _ ->
      if Lang.isMathOp_ op.val then
        operandExps
        |> List.map (expToMaybeMathExp_ identToVarId)
        |> Utils.projJusts
        |> Maybe.map (MathOp op.val)
      else
        Nothing

    _ ->
      Nothing


-- Based on SolverServer.mathExpToREDUCE
--
-- For debugging purposes.
mathExpToString : MathExp -> String
mathExpToString mathExp =
  case mathExp of
    MathNum n     -> toString n
    MathVar varId -> "x" ++ toString varId
    MathOp op_ children ->
      let childPerhapsParens childTerm =
        case childTerm of
          MathOp ArcTan2 _ -> mathExpToString childTerm
          MathOp _ [_, _]  -> "(" ++ mathExpToString childTerm ++ ")"
          _                -> mathExpToString childTerm
      in
      case (op_, children) of
        (Plus,    [l,r]) -> childPerhapsParens l ++ " + " ++ childPerhapsParens r
        (Minus,   [l,r]) -> childPerhapsParens l ++ " - " ++ childPerhapsParens r
        (Mult,    [l,r]) -> childPerhapsParens l ++ " * " ++ childPerhapsParens r
        (Div,     [l,r]) -> childPerhapsParens l ++ " / " ++ childPerhapsParens r
        (Pow,     [l,r]) -> "(" ++ mathExpToString l ++ ")^" ++ childPerhapsParens r  -- Extra parens to prevent misinterpreting negative signs before powers
        (Mod,     [l,r]) -> childPerhapsParens l ++ " mod " ++ childPerhapsParens r
        (ArcTan2, [l,r]) -> "atan2(" ++ mathExpToString l ++ ", " ++ mathExpToString r ++ ")"
        (Cos,     [n])   -> "cos(" ++ mathExpToString n ++ ")"
        (Sin,     [n])   -> "sin(" ++ mathExpToString n ++ ")"
        (ArcCos,  [n])   -> "acos(" ++ mathExpToString n ++ ")"
        (ArcSin,  [n])   -> "asin(" ++ mathExpToString n ++ ")"
        (Abs,     [n])   -> "abs(" ++ mathExpToString n ++ ")"
        (Floor,   [n])   -> "floor(" ++ mathExpToString n ++ ")"
        (Ceil,    [n])   -> "ceiling(" ++ mathExpToString n ++ ")"
        (Round,   [n])   -> "round(" ++ mathExpToString n ++ ")"
        (Sqrt,    [n])   -> "sqrt(" ++ mathExpToString n ++ ")"
        (Ln,      [n])   -> "ln(" ++ mathExpToString n ++ ")"
        (Pi,      [])    -> "π"
        _                -> let _ = Debug.log "MathExp.mathExpToString: Didn't know how to convert this to string" mathExp in "(" ++ toString mathExp ++ ")"
