module REDUCE exposing (..)

import Native.REDUCE

import Lang exposing (Num, Op_(..), MathExp(..))
import Parser exposing (Parser, Count(..), (|.), (|=), succeed, symbol, int, float, ignore, repeat, zeroOrMore, oneOf, lazy, delayedCommit, delayedCommitMap, inContext, end)
import BinaryOperatorParser exposing (PrecedenceTable(..), Associativity(..), Precedence, binaryOperator)
import SolverTypes exposing (..)
import MathExp
import Utils

import Dict

-- All REDUCE-specific Elm code should appear here

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



query : String -> String
query str =
  let _ = Utils.log <| "Reduce query: " ++ str in
  let responseStr = Native.REDUCE.query str in
  let _ = Utils.log <| "Reduce response: " ++ responseStr in
  responseStr


solve : Problem -> List Solution
solve problem =
  let responseStr = problem |> problemToREDUCE |> query in
  let perhapsParsedSolutions = parseReduceSolutionResponse responseStr in
  let parsedSolutions        = Utils.filterOks  perhapsParsedSolutions in
  let failedParses           = Utils.filterErrs perhapsParsedSolutions in
  case (parsedSolutions, failedParses) of
    (_::_, _)         -> parsedSolutions |> mapSolutionsExps distributeNegation
    ([], badParse::_) -> let _ = Utils.log ("Reduce solution response parse error: " ++ toString badParse) in []
    ([], [])          -> []


simplify : MathExp -> MathExp
simplify unsimplifiedMathExp =
  case unsimplifiedMathExp |> simplificationToREDUCE |> query |> Parser.run (parseMathExp |. skipSpaces |. end) of
    Ok simplifiedMathExp -> distributeNegation simplifiedMathExp
    Err badParse         -> let _ = Utils.log ("Reduce simplification response parse error: " ++ toString badParse) in unsimplifiedMathExp


-- For whatever reason, REDUCE would sometimes rather return -(x - y - z) instead of (y + z - x)
-- I can't otherwise figure out how to tell it to produce the desired output.
distributeNegation : MathExp -> MathExp
distributeNegation mathExp =
  case mathExp of
    MathNum _                                     -> mathExp
    MathVar _                                     -> mathExp
    MathOp Minus [MathNum 0, MathOp Minus [l, r]] -> distributeNegation <| MathOp Minus [r, l]
    MathOp op_ children                           -> MathOp op_ (List.map distributeNegation children)


-- solve({x=y,y=z},{x,y});  => {{x=z,y=z}}
-- solve({x=y,y=z},{x,z});  => {{x=y,z=y}}
solutionsCacheToString : SolutionsCache -> String
solutionsCacheToString solutionsCache =
  let equationSolutionsStr =
    solutionsCache.eqnSystemSolutions
    |> Dict.toList
    |> List.map
        (\(problem, solutions) ->
          let solutionStrs =
            -- ["{x1=123,x2=123}", "{x1=456,x2=456}"]
            solutions
            |> List.map
                (\solution ->
                  "{" ++ (solution |> List.map (\(mathExp, varId) -> varIdToREDUCE varId ++ "=" ++ mathExpToREDUCE mathExp) |> String.join ",") ++ "}"
                )
          in
          -- solve({x=y,y=z},{x}); => {{x1=123,x2=123},{x1=456,x2=456}}
          problemToREDUCE problem ++ ";\t=> {" ++ String.join "," solutionStrs ++ "}"
        )
    |> String.join "\n"
  in
  let simplificationsStr =
    solutionsCache.simplifications
    |> Dict.toList
    |> List.map (\(unsimplified, simplified) -> mathExpToREDUCE unsimplified ++ ";\t=> " ++ mathExpToREDUCE simplified)
    |> String.join "\n"
  in
  equationSolutionsStr ++ "\n" ++ simplificationsStr



-- Serializing ----------------



varIdToREDUCE : Int -> String
varIdToREDUCE varId = "x" ++ toString varId


eqnToREDUCE : Eqn -> String
eqnToREDUCE (lhs, rhs) = mathExpToREDUCE lhs ++ "=" ++ mathExpToREDUCE rhs


problemToREDUCE : Problem -> String
problemToREDUCE ((equations, targetVarIds) as problem) =
  -- e.g. "off nat; on combineexpt; on factor; solve({x=y,y=z},{x,z});"
  -- "off nat" tells REDUCE not to ACSCII pretty-print the results (which would be neigh unparsable!)
  -- "on combineexpt" tells REDUCE to perform more simplification of exponential terms multiplied by each other (uncommon).
  -- "on factor" tells REDUCE to try to factor the results. May or may not always want this.
  -- "trigsimp" applys trig identities to simplify (e.g. cos(x)^2 + sin(x)^2 = 1)
  "off nat; on combineexpt; on factor; trigsimp(solve({" ++ String.join "," (List.map eqnToREDUCE equations) ++ "},{" ++ String.join "," (List.map varIdToREDUCE targetVarIds) ++ "}),compact);"


simplificationToREDUCE : MathExp -> String
simplificationToREDUCE mathExp =
  -- e.g. "off nat; on combineexpt; on factor; x / x;"
  -- "off nat" tells REDUCE not to ACSCII pretty-print the results (which would be neigh unparsable!)
  -- "on combineexpt" tells REDUCE to perform more simplification of exponential terms multiplied by each other (uncommon).
  -- "on factor" tells REDUCE to try to factor the results. May or may not always want this.
  -- "trigsimp" applys trig identities to simplify (e.g. cos(x)^2 + sin(x)^2 = 1)
  "off nat; on combineexpt; on factor; trigsimp(" ++ mathExpToREDUCE mathExp ++ ",compact);"


mathExpToREDUCE : MathExp -> String
mathExpToREDUCE mathExp =
  case mathExp of
    MathNum n     -> toString n
    MathVar varId -> varIdToREDUCE varId
    MathOp op_ children ->
      let childPerhapsParensToREDUCE childTerm =
        case childTerm of
          MathOp ArcTan2 _ -> mathExpToREDUCE childTerm
          MathOp _ [_, _]  -> "(" ++ mathExpToREDUCE childTerm ++ ")"
          _                -> mathExpToREDUCE childTerm
      in
      case (op_, children) of
        (Plus,    [l,r]) -> childPerhapsParensToREDUCE l ++ "+" ++ childPerhapsParensToREDUCE r
        (Minus,   [l,r]) -> childPerhapsParensToREDUCE l ++ "-" ++ childPerhapsParensToREDUCE r
        (Mult,    [l,r]) -> childPerhapsParensToREDUCE l ++ "*" ++ childPerhapsParensToREDUCE r
        (Div,     [l,r]) -> childPerhapsParensToREDUCE l ++ "/" ++ childPerhapsParensToREDUCE r
        (Pow,     [l,r]) -> "(" ++ mathExpToREDUCE l ++ ")**" ++ childPerhapsParensToREDUCE r  -- Extra parens to prevent misinterpreting negative signs before powers
        (Mod,     [l,r]) -> childPerhapsParensToREDUCE l ++ " mod " ++ childPerhapsParensToREDUCE r
        (ArcTan2, [l,r]) -> "atan2(" ++ mathExpToREDUCE l ++ "," ++ mathExpToREDUCE r ++ ")"
        (Cos,     [n])   -> "cos(" ++ mathExpToREDUCE n ++ ")"
        (Sin,     [n])   -> "sin(" ++ mathExpToREDUCE n ++ ")"
        (ArcCos,  [n])   -> "acos(" ++ mathExpToREDUCE n ++ ")"
        (ArcSin,  [n])   -> "asin(" ++ mathExpToREDUCE n ++ ")"
        (Abs,     [n])   -> "abs(" ++ mathExpToREDUCE n ++ ")"
        (Floor,   [n])   -> "floor(" ++ mathExpToREDUCE n ++ ")"
        (Ceil,    [n])   -> "ceiling(" ++ mathExpToREDUCE n ++ ")"
        (Round,   [n])   -> "round(" ++ mathExpToREDUCE n ++ ")"
        (Sqrt,    [n])   -> "sqrt(" ++ mathExpToREDUCE n ++ ")"
        (Ln,      [n])   -> "ln(" ++ mathExpToREDUCE n ++ ")"
        (Pi,      [])    -> "pi"
        _                -> let _ = Debug.log "Didn't know how to convert this to REDUCE syntax" mathExp in "unknown"





-- Parsing ----------------



-- Because defintions are not always ordered correctly in generated code,
-- need more lazy's than should otherwise be required.
--
-- If you hit a compiler bug of any kind, add more lazy's.
-- (There were multiple manifestions of the same problem.)
--
-- (See https://github.com/elm-lang/elm-compiler/issues/873)


binaryOperatorList : List (String, Associativity, Precedence, Op_)
binaryOperatorList =
  [ ("+",   Left,  2, Plus)
  , ("-",   Left,  2, Minus)
  , ("**",  Left,  4, Pow) -- Left associative in REDUCE. Needs to appear before "*" in this list.
  , ("*",   Left,  3, Mult)
  , ("/",   Left,  3, Div)
  , ("mod", Left,  1, Mod)
  ]


precedenceTable : PrecedenceTable
precedenceTable =
  binaryOperatorList
  |> List.foldl
      (\(str, assoc, prec, _) pt -> BinaryOperatorParser.addOperator (str, assoc, prec) pt)
      BinaryOperatorParser.emptyPrecedenceTable


(.|) : Parser ignore -> Parser keep -> Parser keep
(.|) = delayedCommit


-- {{x1=123,x2=123},{x1=456,x2=456}} or {x1=123} or {x1=123,x1=-123} (which really should be {{x1=123},{x1=-123}} but hey this is the 1960s)
--
-- REDUCE will sometimes send solutions we cannot deal with (imaginary numbers), so need to be
-- able to skip those solutions without destroying the whole parse.
parseReduceSolutionResponse : String -> List (Result Parser.Error Solution)
parseReduceSolutionResponse responseStr  =
  let solutionStrs =
    if String.startsWith "{{" (String.trimLeft responseStr)
    then String.split "}," responseStr
    else String.split "," responseStr
  in
  solutionStrs
  |> List.map (Utils.stringReplace "{" "")
  |> List.map (Utils.stringReplace "}" "")
  |> List.map (Parser.run (parseSolution |. skipSpaces |. end))


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


parseParens : Parser a -> Parser a
parseParens innerParser = between "(" ")" innerParser


parseCommaSeparatedList : Parser a -> Parser (List a)
parseCommaSeparatedList itemParser =
  oneOf
    [ succeed (::) |= itemParser |= repeat zeroOrMore (wsSymbol "," .| itemParser)
    , succeed []
    ]


-- e.g. {x1=123,x2=123} to [(MathNum 123, 1), (MathNum 123, 2)]
parseSolution : Parser Solution
parseSolution =
  inContext "parseSolution" <|
    -- parseCurlies <|
      parseCommaSeparatedList <|
        parseResultEqn


parseResultEqn : Parser (MathExp, Int)
parseResultEqn =
  succeed (\varId mathExp -> (mathExp, varId))
    |. skipSpaces
    |= parseVarToVarId
    |. wsSymbol "="
    |= parseMathExp


-- e.g. x2 to 2
parseVarToVarId : Parser Int
parseVarToVarId = inContext "parseVarToVarId" <| eatChar 'x' .| int


parseMathExp : Parser MathExp
parseMathExp =
  lazy <| \_ ->
    inContext "parseMathExp" <|
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


parseEqnAtom : Parser MathExp
parseEqnAtom =
  lazy <| \_ ->
    inContext "parseEqnAtom" <|
      skipSpaces .|
        oneOf
          [ parseMathNum
          , parseMathVar
          , parseEqnParens
          , parseEqnFunction
          , parseEqnPi
          , parseNegation
          ]


parseBinaryOperatorStr : Parser String
parseBinaryOperatorStr =
  inContext "parseBinaryOperatorStr" <|
    oneOf <|
      List.map (\(str, _, _, _) -> wsSymbol str |> Parser.map (always str)) binaryOperatorList


parseMathNum : Parser MathExp
parseMathNum = parseNumber |> Parser.map MathNum |> inContext "parseMathNum"


parseNumber : Parser Num
parseNumber =
  oneOf
    [ delayedCommitMap (\_ posFloat -> -posFloat) (symbol "-" |. skipSpaces) float
    , float
    ]


parseMathVar : Parser MathExp
parseMathVar = parseVarToVarId |> Parser.map MathVar |> inContext "parseMathVar"


parseEqnParens : Parser MathExp
parseEqnParens = parseParens parseMathExp


parseUnaryFunction : String -> Op_ -> Parser MathExp
parseUnaryFunction funcName op_ =
  succeed (\argTerm -> MathOp op_ [argTerm])
    |. wsSymbol (funcName ++ "(")
    |= parseMathExp
    |. wsSymbol ")"


parseBinaryFunction : String -> Op_ -> Parser MathExp
parseBinaryFunction funcName op_ =
  succeed (\argTerm1 argTerm2 -> MathOp op_ [argTerm1, argTerm2])
    |. wsSymbol (funcName ++ "(")
    |= parseMathExp
    |. wsSymbol ","
    |= parseMathExp
    |. wsSymbol ")"


parseEqnFunction : Parser MathExp
parseEqnFunction =
  lazy <| \_ ->
    inContext "parseEqnFunction" <|
      oneOf
        [ parseBinaryFunction "atan2"   ArcTan2
        , parseUnaryFunction  "cos"     Cos
        , parseUnaryFunction  "sin"     Sin
        , parseUnaryFunction  "acos"    ArcCos
        , parseUnaryFunction  "asin"    ArcSin
        , parseUnaryFunction  "abs"     Abs
        , parseUnaryFunction  "floor"   Floor
        , parseUnaryFunction  "ceiling" Ceil
        , parseUnaryFunction  "round"   Round
        , parseUnaryFunction  "sqrt"    Sqrt
        , parseUnaryFunction  "ln"      Ln
        ]


parseEqnPi : Parser MathExp
parseEqnPi = inContext "parseEqnPi" <| wsSymbol "pi" .| succeed (MathOp Pi [])


parseNegation : Parser MathExp
parseNegation =
  lazy <| \_ ->
    delayedCommitMap (\_ mathExp -> MathExp.neg mathExp) (symbol "-") parseEqnAtom


-- test = Debug.log "REDUCE parsing test" <| parseReduceSolutionResponse "{{x3=(sqrt(3)*x1 - sqrt(3)*x2 + x1 + x2)/2, x3=( - sqrt(3)*x1 + sqrt(3)*x2 + x1 + x2)/2}, {x3=( - sqrt(3)*x1 + sqrt(3)*x2 + x1 + x2)/2, x3=(sqrt(3)*x1 - sqrt(3)*x2 + x1 + x2)/2}}"
