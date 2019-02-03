module TriEval exposing
  ( UnExp
  , Env
  , eval
  , unparse
  , showEnv
  , findHoles
  , parseExample
  )

import Dict exposing (Dict)
import Set exposing (Set)
import Char

import Parser as P exposing (Parser, (|=), (|.))
import Parser.LanguageKit as LanguageKit
import ParserUtils exposing (try, token, singleLineString)

import Utils
import LeoUnparser

import Evaluator exposing (Evaluator)
import State exposing (State)

import Lang exposing (..)

import Types2 as T exposing (..)

type alias HoleIndex =
  (HoleId, Int)

type UnExp
  = UConstructor Ident UnExp
  | UNum Num
  | UBool Bool
  | UString String
  | UTuple (List UnExp)
  | UFunClosure Env (List Ident) {- Type -} Exp
  | UHoleClosure Env HoleIndex
  | UApp UnExp (List UnExp)
  | UCase Env UnExp (List (Ident, Ident, Exp))

type alias EvalState =
  {}

type alias Env =
  List (Ident, (UnExp, () {- Type -}))

type alias UnExpEvaluator =
  Evaluator EvalState String UnExp

identifierFromPat : Pat -> Maybe Ident
identifierFromPat p =
  case unwrapPat p of
    PVar _ ident _ ->
      Just ident

    _ ->
      Nothing

bindingEval : Env -> Exp -> List Ident -> List UnExp -> UnExpEvaluator
bindingEval currentEnv body parameters arguments =
  let
    envExtension =
      arguments
        |> List.map (\u -> (u, ()))
        |> Utils.zip parameters

    newEnv =
      envExtension ++ currentEnv

    argLength =
      List.length arguments

    paramLength =
      List.length parameters
  in
    case compare argLength paramLength of
      LT ->
        Evaluator.succeed <|
          UFunClosure newEnv (List.drop argLength parameters) body

      EQ ->
        eval_ newEnv body

      GT ->
        Evaluator.fail "Supplied too many arguments"

eval_ : Env -> Exp -> UnExpEvaluator
eval_ env exp =
  let
    e =
      unwrapExp exp
  in
    case e of
      -- E-Const

      EConst _ n _ _ ->
        Evaluator.succeed <|
          UNum n

      EBase _ baseVal ->
        Evaluator.succeed <|
          case baseVal of
            EBool b ->
              UBool b

            EString _ s ->
              UString s

            ENull ->
              UString "null"

      -- E-Lambda

      EFun _ pats body _ ->
        pats
          |> List.map identifierFromPat
          |> Utils.projJusts
          |> Result.fromMaybe "Non-identifier pattern in function"
          |> Result.map (\vars -> UFunClosure env vars body)
          |> Evaluator.fromResult

      -- E-Var

      EVar _ x ->
        env
          |> Utils.maybeFind x
          |> Result.fromMaybe ("Variable not found: '" ++ x ++ "'")
          |> Result.map Tuple.first
          |> Evaluator.fromResult

      -- E-App

      EApp _ eFunction eArgs _ _ ->
        let
          uArgs =
            Evaluator.mapM (eval_ env) eArgs
        in
          eval_ env eFunction |> Evaluator.andThen (\uFunction ->
            case uFunction of
              UFunClosure functionEnv parameters body ->
                uArgs
                  |> Evaluator.andThen (bindingEval functionEnv body parameters)

              UHoleClosure _ _ ->
                Evaluator.map (UApp uFunction) uArgs

              _ ->
                Evaluator.fail "Not a proper application"
          )

      -- E-Match

      ECase _ e0 branches _ ->
        Evaluator.fail "Case not supported"

      -- E-Hole

      EHole _ hole  ->
        case hole of
          EEmptyHole (holeId, _) ->
            Evaluator.succeed <|
              UHoleClosure env (holeId, -1)

          _ ->
            Evaluator.fail "Unsupported hole type"

      -- Misc.

      EOp _ _ op args _ ->
        Evaluator.fail "Op not supported"

      EList _ args _ _ _ ->
        Evaluator.fail "List not supported"

      EIf _ condition _ trueBranch _ falseBranch _ ->
        Evaluator.fail "If not supported"

      ELet _ _ decls _ body ->
        case recordEntriesFromDeclarations decls of
          Just entries ->
            let
              (parameters, eArgs) =
                entries
                  |> List.map (\(_, _, ident, _, exp) -> (ident, exp))
                  |> List.unzip

              uArgs =
                Evaluator.mapM (eval_ env) eArgs
            in
              uArgs
                |> Evaluator.andThen (bindingEval env body parameters)

          Nothing ->
            Evaluator.fail "Could not get record entries from let"

      EColonType _ _ _ _ _ ->
        Evaluator.fail "Colon type not supported"

      EParens _ eInner _ _ ->
        eval_ env eInner

      ERecord _ _ decls _ ->
        case recordEntriesFromDeclarations decls of
          Just entries ->
            case tupleEncodingUnapply entries of
              Just tupleEntries ->
                tupleEntries
                  |> Evaluator.mapM (Tuple.second >> eval_ env)
                  |> Evaluator.map UTuple

              Nothing ->
                Evaluator.fail "Arbitrary records not supported"

          Nothing ->
            Evaluator.fail "Could not get record entries"

      ESelect _ _ _ _ _ ->
        Evaluator.fail "Select not supported"

eval : Exp -> Result String UnExp
eval =
  eval_ []
    >> Evaluator.run {}
    >> Result.map Tuple.first
    >> Result.map setHoleIndexes

showEnv : Env -> String
showEnv =
  let
    showBinding : (Ident, (UnExp, ())) -> String
    showBinding (i, (u, _)) =
      i ++ " → " ++ unparse u
  in
    List.map showBinding >> String.join ", "

unparse : UnExp -> String
unparse u =
  case u of
    UConstructor name uArg ->
      name ++ " " ++ unparse uArg

    UNum n ->
      toString n

    UBool b ->
      if b then "True" else "False"

    UString s ->
      "\"" ++ s ++ "\""

    UTuple us ->
      "("
        ++ String.join ", " (List.map unparse us)
        ++ ")"

    UFunClosure env args body ->
      let
        argsString =
          String.join ", " args
      in
        "["
          ++ showEnv env
          ++ "] λ"
          ++ argsString
          ++ " ."
          ++ LeoUnparser.unparse body

    UHoleClosure env (i, j) ->
      "[" ++ showEnv env ++ "] ??(" ++ toString i ++ ", " ++ toString j ++ ")"

    UApp uFunction uArgs ->
      let
        parens u beginning =
          "(" ++ beginning ++ ") " ++ unparse u
      in
        List.foldl parens (unparse uFunction) uArgs

    UCase env u0 branches ->
      let
        unparseBranch (constructorName, varName, body) =
          constructorName
            ++ " "
            ++ varName ++ " →"
            ++ LeoUnparser.unparse body
      in
        "["
          ++ showEnv env
          ++ "] case "
          ++ unparse u0
          ++ " of "
          ++ String.join " " (List.map unparseBranch branches)


statefulMap : (UnExp -> State s UnExp) -> UnExp -> State s UnExp
statefulMap f u =
  case u of
    UConstructor ident arg ->
      State.map (UConstructor ident) (f arg)

    UNum n ->
      State.pure <| UNum n

    UBool b ->
      State.pure <| UBool b

    UString s ->
      State.pure <| UString s

    UTuple args ->
      State.map UTuple (State.mapM f args)

    UFunClosure env params body ->
      State.pure <| UFunClosure env params body

    UHoleClosure env holeIndex ->
      State.pure <| UHoleClosure env holeIndex

    UApp uFunction uArgs ->
      flip State.andThen (f uFunction) <| \newFunction ->
        State.map (UApp newFunction) (State.mapM f uArgs)

    UCase env uScrutinee branches ->
      State.map
        (\newScrutinee -> UCase env newScrutinee branches)
        (f uScrutinee)

map : (UnExp -> UnExp) -> UnExp -> UnExp
map f =
  statefulMap (f >> State.pure) >> State.run () >> Tuple.first

children : UnExp -> List UnExp
children u =
  case u of
    UConstructor _ arg ->
      [arg]

    UNum _ ->
      []

    UBool _ ->
      []

    UString _ ->
      []

    UTuple args ->
      args

    UFunClosure _ _ _ ->
      []

    UHoleClosure _ _ ->
      []

    UApp uFunction uArgs ->
      uFunction :: uArgs

    UCase _ uScrutinee _ ->
      [uScrutinee]

flatten : UnExp -> List UnExp
flatten u =
  u :: List.concatMap flatten (children u)

findHoles : HoleId -> UnExp -> List (Int, Env)
findHoles targetHoleId =
  let
    extract u =
      case u of
        UHoleClosure env (holeId, index) ->
          if holeId == targetHoleId then
            [(index, env)]
          else
            []

        _ ->
          []
  in
    flatten
      >> List.concatMap extract
      >> List.sortBy Tuple.first

setHoleIndexes : UnExp -> UnExp
setHoleIndexes =
  let
    holeSetter : UnExp -> State (Dict HoleId Int) UnExp
    holeSetter u =
      case u of
        UHoleClosure env (holeId, holeIndex) ->
          flip State.andThen State.get <| \indexMap ->
            let
              freshHoleIndex =
                Dict.get holeId indexMap
                  |> Maybe.withDefault 0

              newIndexMap =
                Dict.insert
                  holeId
                  (freshHoleIndex + 1)
                  indexMap
            in
              flip State.map (State.put newIndexMap) <| \_ ->
                UHoleClosure env (holeId, freshHoleIndex)

        _ ->
          State.pure u
  in
    statefulMap holeSetter >> State.run Dict.empty >> Tuple.first

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

type Example
  = ExConstructor Ident Example
  | ExNum Num
  | ExBool Bool
  | ExString String
  | ExTuple (List Example)
  | ExPartialFunction (List (Example, Example)) --  TODO Should be (v, ex)

spaces : Parser ()
spaces =
  P.ignore P.zeroOrMore (\char -> char == ' ')

capitalIdentifier : Parser String
capitalIdentifier =
  P.succeed (++)
    |= P.keep (P.Exactly 1) Char.isUpper
    |= P.keep P.zeroOrMore (\c -> Char.isUpper c || Char.isLower c)

exConstructor : Parser Example
exConstructor =
  P.lazy <| \_ ->
    P.inContext "constructor example" <|
      P.succeed ExConstructor
        |= capitalIdentifier
        |= example

exNum : Parser Example
exNum =
  let
    sign =
      P.oneOf
        [ P.succeed (-1)
            |. P.symbol "-"
        , P.succeed 1
        ]
  in
    try <|
      P.inContext "number example" <|
        P.succeed (\s n -> ExNum (s * n))
          |= sign
          |= P.float

exBool : Parser Example
exBool =
  P.inContext "boolean example" <|
    P.map ExBool <|
      P.oneOf
        [ token "True" True
        , token "False" False
        ]

exString : Parser Example
exString =
  P.inContext "string example" <|
    P.map (\(_, content) -> ExString content)
      singleLineString

exTuple : Parser Example
exTuple =
  P.lazy <| \_ ->
    P.inContext "tuple example" <|
      P.map ExTuple <|
        LanguageKit.sequence
          { start = "("
          , separator = ","
          , end = ")"
          , spaces = spaces
          , item = example
          , trailing = LanguageKit.Forbidden
          }

exPartialFunction : Parser Example
exPartialFunction =
  P.lazy <| \_ ->
    let
      binding : Parser (Example, Example)
      binding =
        P.succeed (,)
          |= example
          |. spaces
          |. P.symbol "->"
          |. spaces
          |= example
    in
      P.inContext "partial function example" <|
        P.map ExPartialFunction <|
          LanguageKit.sequence
            { start = "{"
            , separator = ","
            , end = "}"
            , spaces = spaces
            , item = binding
            , trailing = LanguageKit.Forbidden
            }

example : Parser Example
example =
  P.lazy <| \_ ->
    P.oneOf
       [ exConstructor
       , exNum
       , exBool
       , exString
       , exTuple
       , exPartialFunction
       ]

parseExample : String -> Result P.Error Example
parseExample =
  P.run example

--------------------------------------------------------------------------------
-- Synthesis
--------------------------------------------------------------------------------

type alias World =
  (Env, Example)

guess : T.TypeEnv -> Type -> List Exp
guess gamma tau =
  let
    -- EGuess-Var
    variableGuesses =
      let
        typePair : Ident -> Maybe (Ident, Type)
        typePair i =
          T.lookupVar gamma i
            |> Maybe.andThen (Maybe.map <| \t -> (i, t))
      in
        gamma
          |> T.varsOfGamma
          |> List.map typePair
          |> Utils.filterJusts
          |> List.filter (Tuple.second >> T.typeEquiv gamma tau)
          |> List.map (Tuple.first >> eVar0)

    -- EGuess-App
    appGuesses =
      let
        guessApps : Type -> List Exp
        guessApps tau2 =
          let
            tau2ArrTau =
              T.rebuildArrow ([], [tau2], tau)

            e1s =
              guess gamma tau2ArrTau

            e2s =
              refine gamma [] tau2ArrTau
          in
            Utils.cartProd e1s e2s
              |> List.map (\(e1, e2) -> eApp e1 [e2])

        typesToTry =
          []
      in
        List.concatMap guessApps typesToTry
  in
    variableGuesses ++ appGuesses

refine : T.TypeEnv -> List World -> Type -> List Exp
refine gamma worlds tau =
  []
