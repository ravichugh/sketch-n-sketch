port module SolverServer exposing (..)

import InterfaceModel exposing (Msg, Model)
import InterfaceController
import Lang
import Parser exposing (Parser, Count(..), (|.), (|=), succeed, symbol, keyword, int, float, ignore, repeat, zeroOrMore, oneOf, lazy, delayedCommit, delayedCommitMap, inContext, end)
import BinaryOperatorParser exposing (PrecedenceTable(..), Associativity(..), Precedence, binaryOperator)
import Solver exposing (..)
import Utils
import ImpureGoodies

import Dict
import Task

-- All REDUCE-specific code should appear here or in the associated native JS file.

-- Send:
-- solve({(x2-x1)^2 + (y2-y1)^2 = (x3-x2)^2 + (y3-y2)^2,(x3-x2)^2 + (y3-y2)^2 = (x1-x3)^2 + (y1-y3)^2,(x1-x3)^2 + (y1-y3)^2 = (x2-x1)^2 + (y2-y1)^2}, {x3,y3});
-- Receive:
-- {{x3=(sqrt(3)*y1 - sqrt(3)*y2 + x1 + x2)/2, y3=( - sqrt(3)*x1 + sqrt(3)*x2 + y1 + y2)/2}, {x3=( - sqrt(3)*y1 + sqrt(3)*y2 + x1 + x2)/2, y3=(sqrt(3)*x1 - sqrt(3)*x2 + y1 + y2)/2}}

-- See Section 7.10 in the REDUCE manual http://www.reduce-algebra.com/reduce38-docs/reduce.pdf#section.7.10
--
-- If you ask reduce to solve a system of multiple equations and you get no result but
-- you think you should, you many need to ask it to solve for more variables:
--
-- solve({x=y,y=z},{x});    => {}
-- solve({x=y,y=z},{x,y});  => {{x=z,y=z}}
-- solve({x=y,y=z},{x,z});  => {{x=y,z=y}}
--
-- (The ending semicolon is optional over the websocket.)



-- Outgoing port
port queryReduce : String -> Cmd msg


-- Incoming port
port reduceResponse : (String -> msg) -> Sub msg


askForSolution : Problem -> Msg -> Model -> (Model, Cmd Msg)
askForSolution ((equations, targetVarIds) as problem) failedMsg oldModel =
  let reduceQueryString =
    let eqnStrings =
      equations
      |> List.map (\(lhs, rhs) -> mathExpToREDUCE lhs ++ "=" ++ mathExpToREDUCE rhs)
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
        -- As of v2.0.1 the elm-tools/parser library sometimes hard crashes on a bad parse rather than returning an Err value.
        case ImpureGoodies.crashToError (\_ -> Parser.run parseReduceResponse reduceResponse) of
          Ok (Ok solutions) -> solutions
          Ok (Err error)    -> let _ = Utils.log ("Reduce response parse error: " ++ toString error.problem ++ "\n" ++ toString error.context) in []
          Err errorStr      -> let _ = Utils.log ("Reduce response parse crash: " ++ errorStr) in []
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


mathExpToREDUCE : MathExp -> String
mathExpToREDUCE mathExp =
  case mathExp of
    MathNum n     -> toString n
    MathVar varId -> varIdToREDUCE varId
    MathOp op_ children ->
      let childPerhapsParensToREDUCE childTerm =
        case childTerm of
          MathOp Lang.ArcTan2 _ -> mathExpToREDUCE childTerm
          MathOp _ [_, _]       -> "(" ++ mathExpToREDUCE childTerm ++ ")"
          _                     -> mathExpToREDUCE childTerm
      in
      case (op_, children) of
        (Lang.Plus,    [l,r]) -> childPerhapsParensToREDUCE l ++ "+" ++ childPerhapsParensToREDUCE r
        (Lang.Minus,   [l,r]) -> childPerhapsParensToREDUCE l ++ "-" ++ childPerhapsParensToREDUCE r
        (Lang.Mult,    [l,r]) -> childPerhapsParensToREDUCE l ++ "*" ++ childPerhapsParensToREDUCE r
        (Lang.Div,     [l,r]) -> childPerhapsParensToREDUCE l ++ "/" ++ childPerhapsParensToREDUCE r
        (Lang.Pow,     [l,r]) -> "(" ++ mathExpToREDUCE l ++ ")^" ++ childPerhapsParensToREDUCE r  -- Extra parens to prevent misinterpreting negative signs before powers
        (Lang.Mod,     [l,r]) -> childPerhapsParensToREDUCE l ++ " mod " ++ childPerhapsParensToREDUCE r
        (Lang.ArcTan2, [l,r]) -> "atan2(" ++ mathExpToREDUCE l ++ "," ++ mathExpToREDUCE r ++ ")"
        (Lang.Cos,     [n])   -> "cos(" ++ mathExpToREDUCE n ++ ")"
        (Lang.Sin,     [n])   -> "sin(" ++ mathExpToREDUCE n ++ ")"
        (Lang.ArcCos,  [n])   -> "acos(" ++ mathExpToREDUCE n ++ ")"
        (Lang.ArcSin,  [n])   -> "asin(" ++ mathExpToREDUCE n ++ ")"
        (Lang.Floor,   [n])   -> "floor(" ++ mathExpToREDUCE n ++ ")"
        (Lang.Ceil,    [n])   -> "ceiling(" ++ mathExpToREDUCE n ++ ")"
        (Lang.Round,   [n])   -> "round(" ++ mathExpToREDUCE n ++ ")"
        (Lang.Sqrt,    [n])   -> "sqrt(" ++ mathExpToREDUCE n ++ ")"
        (Lang.Pi,      [])    -> "pi"
        _                     -> let _ = Debug.log "Didn't know how to convert this to REDUCE syntax" mathExp in "unknown"



-- Parsing ---------


binaryOperatorList : List (String, Associativity, Precedence, Lang.Op_)
binaryOperatorList =
  [ ("+",   Left,  2, Lang.Plus)
  , ("-",   Left,  2, Lang.Minus)
  , ("*",   Left,  3, Lang.Mult)
  , ("/",   Left,  3, Lang.Div)
  , ("^",   Right, 4, Lang.Pow)
  , ("mod", Left,  1, Lang.Mod)
  ]


precedenceTable : PrecedenceTable
precedenceTable =
  binaryOperatorList
  |> List.foldl
      (\(str, assoc, prec, _) pt -> BinaryOperatorParser.addOperator (str, assoc, prec) pt)
      BinaryOperatorParser.emptyPrecedenceTable


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


-- e.g. {x1=123,x2=123} to [(MathNum 123, 1), (MathNum 123, 2)]
parseSolution : Parser Solution
parseSolution =
  inContext "parseSolution" <|
    parseCurlies <|
      parseCommaSeparatedList <|
        succeed (\varId mathExp -> (mathExp, varId))
          |= parseVarToVarId
          |. wsSymbol "="
          |= parseMathExp


-- e.g. x2 to 2
parseVarToVarId : Parser Int
parseVarToVarId = inContext "parseVarToVarId" <| eatChar 'x' .| int


parseMathExp : Parser MathExp
parseMathExp =
  inContext "parseMathExp" <|
    lazy (\_ ->
      binaryOperator
        { precedenceTable   = precedenceTable
        , minimumPrecedence = 1
        , expression        = parseEqnAtom
        , operator          = parseBinaryOperatorStr
        , representation    = identity -- convert output of parseBinaryOperator to what's in the precedence table
        , combine           =
            (\left opStr right ->
              case binaryOperatorList |> Utils.findFirst (\(str, _, _, _) -> str == opStr) of
                Just (_, _, _, op_) -> MathOp op_ [left, right]
                Nothing             -> Debug.crash <| "REDUCE parsing: Should not happen: could not find binary op " ++ opStr
            )
        }
    )


parseEqnAtom : Parser MathExp
parseEqnAtom =
  inContext "parseEqnAtom" <|
    skipSpaces .|
      oneOf
        [ parseMathNum
        , parseMathVar
        , parseEqnParens
        , parseEqnFunction
        , parseEqnPi
        ]


parseBinaryOperatorStr : Parser String
parseBinaryOperatorStr =
  oneOf <|
    List.map (\(str, _, _, _) -> wsSymbol str |> Parser.map (always str)) binaryOperatorList


parseMathNum : Parser MathExp
parseMathNum = parseNumber |> Parser.map MathNum |> inContext "parseMathNum"


parseNumber : Parser Lang.Num
parseNumber =
  oneOf <|
    [ delayedCommitMap (\_ posFloat -> -posFloat) (symbol "-" |. skipSpaces) float
    , float
    ]


parseMathVar : Parser MathExp
parseMathVar = parseVarToVarId |> Parser.map MathVar |> inContext "parseMathVar"


parseEqnParens : Parser MathExp
parseEqnParens = parseParens parseMathExp


parseEqnFunction : Parser MathExp
parseEqnFunction =
  let parseUnaryFunction funcName op_ =
    succeed (\argTerm -> MathOp op_ [argTerm])
      |. wsSymbol (funcName ++ "(")
      |= parseMathExp
      |. wsSymbol ")"
  in
  let parseBinaryFunction funcName op_ =
    succeed (\argTerm1 argTerm2 -> MathOp op_ [argTerm1, argTerm2])
      |. wsSymbol (funcName ++ "(")
      |= parseMathExp
      |. wsSymbol ","
      |= parseMathExp
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


parseEqnPi : Parser MathExp
parseEqnPi = inContext "parseEqnPi" <| wsSymbol "pi" .| succeed (MathOp Lang.Pi [])
