port module SolverServer exposing (..)

import InterfaceModel exposing (Msg, Model)
import InterfaceController
import Lang
import Parser exposing (Parser, Count(..), (|.), (|=), succeed, symbol, keyword, int, float, ignore, repeat, zeroOrMore, oneOf, lazy, delayedCommit, delayedCommitMap, inContext, end)
import Solver exposing (..)

import Dict
import Task
import Utils

-- type EqnTerm
--   = EqnConst Num
--   | EqnVar Int -- Variable identifiers, often locIds
--   | EqnOp Op_ (List EqnTerm)
--
-- type alias Eqn = (EqnTerm, EqnTerm) -- LHS, RHS
--
-- type alias Problem  = (List Eqn, List Int) -- System of equations, and varIds to solve for (usually a singleton).
-- type alias Solution = List (EqnTerm, Int)
--
-- type alias SolutionsCache = Dict Problem (List Solution)
--
-- type NeedSolutionException = NeedSolution problem


-- Send:
-- solve({(x2-x1)^2 + (y2-y1)^2 = (x3-x2)^2 + (y3-y2)^2,(x3-x2)^2 + (y3-y2)^2 = (x1-x3)^2 + (y1-y3)^2,(x1-x3)^2 + (y1-y3)^2 = (x2-x1)^2 + (y2-y1)^2}, {x3,y3});
-- Receive:
-- {{x3=(sqrt(3)*y1 - sqrt(3)*y2 + x1 + x2)/2, y3=( - sqrt(3)*x1 + sqrt(3)*x2 + y1 + y2)/2}, {x3=( - sqrt(3)*y1 + sqrt(3)*y2 + x1 + x2)/2, y3=(sqrt(3)*x1 - sqrt(3)*x2 + y1 + y2)/2}}

-- All REDUCE-specific code should appear here or in the associated native JS file.


-- Outgoing port
port queryReduce : String -> Cmd msg


-- Incoming port
port reduceResponse : (String -> msg) -> Sub msg


askForSolution : Problem -> Msg -> Model -> (Model, Cmd Msg)
askForSolution ((equations, targetVarIds) as problem) failedMsg oldModel =
  let reduceQueryString =
    let eqnStrings =
      equations
      |> List.map (\(lhs, rhs) -> eqnTermToREDUCE lhs ++ "=" ++ eqnTermToREDUCE rhs)
    in
    "solve({" ++ String.join "," eqnStrings ++ "},{" ++ String.join "," (List.map varIdToREDUCE targetVarIds) ++ "})"
  in
  ( { oldModel | problemsSentToSolver = oldModel.problemsSentToSolver ++ [(problem, failedMsg)] }
  , queryReduce reduceQueryString
  )


handleReduceResponse : String -> Model -> (Model, Cmd Msg)
handleReduceResponse reduceResponse oldModel=
  case oldModel.problemsSentToSolver of
    (oldestProblem, interruptedMsg)::outstandingProblems ->
      let solutions =
        case Parser.run parseReduceResponse reduceResponse of
          Ok solutions -> solutions
          Err error    -> let _ = Utils.log ("Reduce response parse error: " ++ toString error.problem ++ "\n" ++ toString error.context) in []
      in
      let newModel =
        { oldModel | problemsSentToSolver = outstandingProblems
                   , solutionsCache       = Dict.insert oldestProblem solutions oldModel.solutionsCache
        }
      in
      (newModel, Task.perform (\_ -> interruptedMsg) (Task.succeed ()))
    [] ->
      let _ = Utils.log "SolverServer.handleReduceResponse: Shouldn't happen: got a REDUCE response but there are no outstanding problems!!!" in
      (oldModel, Cmd.none)



-- Serializing -----


varIdToREDUCE : Int -> String
varIdToREDUCE varId = "x" ++ toString varId


eqnTermToREDUCE : EqnTerm -> String
eqnTermToREDUCE eqnTerm =
  case eqnTerm of
    EqnConst n   -> toString n
    EqnVar varId -> varIdToREDUCE varId
    EqnOp op_ children ->
      let childPerhapsParensToREDUCE childTerm =
        case childTerm of
          EqnOp Lang.ArcTan2 _  -> eqnTermToREDUCE childTerm
          EqnOp _ [_, _]        -> "(" ++ eqnTermToREDUCE childTerm ++ ")"
          _                     -> eqnTermToREDUCE childTerm
      in
      case (op_, children) of
        (Lang.Plus,    [l,r]) -> childPerhapsParensToREDUCE l ++ "+" ++ childPerhapsParensToREDUCE r
        (Lang.Minus,   [l,r]) -> childPerhapsParensToREDUCE l ++ "-" ++ childPerhapsParensToREDUCE r
        (Lang.Mult,    [l,r]) -> childPerhapsParensToREDUCE l ++ "*" ++ childPerhapsParensToREDUCE r
        (Lang.Div,     [l,r]) -> childPerhapsParensToREDUCE l ++ "/" ++ childPerhapsParensToREDUCE r
        (Lang.Pow,     [l,r]) -> childPerhapsParensToREDUCE l ++ "^" ++ childPerhapsParensToREDUCE r
        (Lang.Mod,     [l,r]) -> childPerhapsParensToREDUCE l ++ " mod " ++ childPerhapsParensToREDUCE r
        (Lang.ArcTan2, [l,r]) -> "atan2(" ++ eqnTermToREDUCE l ++ "," ++ eqnTermToREDUCE r ++ ")"
        (Lang.Cos,     [n])   -> "cos(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.Sin,     [n])   -> "sin(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.ArcCos,  [n])   -> "acos(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.ArcSin,  [n])   -> "asin(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.Floor,   [n])   -> "floor(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.Ceil,    [n])   -> "ceiling(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.Round,   [n])   -> "round(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.Sqrt,    [n])   -> "sqrt(" ++ eqnTermToREDUCE n ++ ")"
        (Lang.Pi,      [])    -> "pi"
        _                     -> let _ = Debug.log "Didn't know how to convert this to REDUCE syntax" eqnTerm in "unknown"



-- Parsing ---------


(.|) : Parser ignore -> Parser keep -> Parser keep
-- (.|) ignore keep = ignore |> Parser.andThen (\_ -> keep)
(.|) = delayedCommit


-- {{x1=123,x2=123},{x1=456,x2=456}} or {x1=123}
parseReduceResponse : Parser (List Solution)
parseReduceResponse =
  inContext "parseReduceResponse" <|
    oneOf [parseCurlies parseSolutions, parseSolutions] |. end


wsSymbol : String -> Parser ()
wsSymbol str = skipSpaces .| symbol str


eatChar : Char -> Parser ()
eatChar char = ignore (Exactly 1) ((==) char)


skipSpaces : Parser ()
skipSpaces = ignore zeroOrMore (\char -> char == ' ' || char == '\t')


between : String -> String -> Parser a -> Parser a
between openStr closeStr innerParser =
  inContext ("between " ++ openStr ++ " " ++ closeStr) <|
    wsSymbol openStr .| (innerParser |. wsSymbol closeStr)


parseCurlies : Parser a -> Parser a
parseCurlies innerParser = between "{" "}" innerParser


parseParens : Parser a -> Parser a
parseParens innerParser = between "(" ")" innerParser


parseCommaSeparatedList : Parser a -> Parser (List a)
parseCommaSeparatedList itemParser =
  oneOf <|
    [ succeed (::) |= itemParser |= repeat zeroOrMore (wsSymbol "," .| itemParser)
    , succeed []
    ]


parseSolutions : Parser (List Solution)
parseSolutions = inContext "parseSolutions" <| parseCommaSeparatedList parseSolution


-- e.g. {x1=123,x2=123} to [(EqnConst 123, 1), (EqnConst 123, 2)]
parseSolution : Parser Solution
parseSolution =
  inContext "parseSolution" <|
    parseCurlies <|
      parseCommaSeparatedList <|
        succeed (\varId eqnTerm -> (eqnTerm, varId))
          |= parseVarToVarId
          |. wsSymbol "="
          |= parseEqnTerm


-- e.g. x2 to 2
parseVarToVarId : Parser Int
parseVarToVarId = inContext "parseVarToVarId" <| eatChar 'x' .| int


parseEqnTerm : Parser EqnTerm
parseEqnTerm = inContext "parseEqnTerm" <| lazy (\_ -> parseEqnAtom)


parseEqnAtom : Parser EqnTerm
parseEqnAtom =
  inContext "parseEqnAtom" <|
    oneOf <|
      [ parseEqnConst
      , parseEqnVar
      , parseEqnParens
      , parseEqnFunction
      , parseEqnPi
      ]


parseEqnConst : Parser EqnTerm
parseEqnConst = parseNumber |> Parser.map EqnConst |> inContext "parseEqnConst"


parseNumber : Parser Lang.Num
parseNumber =
  oneOf <|
    [ delayedCommitMap (\_ posFloat -> -posFloat) (symbol "-") (skipSpaces .| float)
    , float
    ]


parseEqnVar : Parser EqnTerm
parseEqnVar = parseVarToVarId |> Parser.map EqnVar |> inContext "parseEqnVar"


parseEqnParens : Parser EqnTerm
parseEqnParens = parseParens parseEqnTerm


parseEqnFunction : Parser EqnTerm
parseEqnFunction =
  let parseUnaryFunction funcName op_ =
    succeed (\argTerm -> EqnOp op_ [argTerm])
      |. wsSymbol (funcName ++ "(")
      |= parseEqnTerm
      |. wsSymbol ")"
  in
  let parseBinaryFunction funcName op_ =
    succeed (\argTerm1 argTerm2 -> EqnOp op_ [argTerm1, argTerm2])
      |. wsSymbol (funcName ++ "(")
      |= parseEqnTerm
      |. wsSymbol ","
      |= parseEqnTerm
      |. wsSymbol ")"
  in
  inContext "parseEqnFunction" <|
    oneOf <|
      [ parseBinaryFunction "atan2"   Lang.ArcTan2
      , parseUnaryFunction  "cos"     Lang.Cos
      , parseUnaryFunction  "sin"     Lang.Sin
      , parseUnaryFunction  "acos"    Lang.ArcCos
      , parseUnaryFunction  "asin"    Lang.ArcSin
      , parseUnaryFunction  "floor"   Lang.Floor
      , parseUnaryFunction  "ceiling" Lang.Ceil
      , parseUnaryFunction  "round"   Lang.Round
      , parseUnaryFunction  "sqrt"    Lang.Sqrt
      ]


parseEqnPi : Parser EqnTerm
parseEqnPi = inContext "parseEqnPi" <| wsSymbol "pi" .| succeed (EqnOp Lang.Pi [])
