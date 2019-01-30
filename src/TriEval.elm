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

import Parser exposing (..)
import Parser.LanguageKit as LanguageKit
import ParserUtils exposing (try, token, singleLineString)

import Utils
import LeoUnparser

import Evaluator exposing (Evaluator)

import Lang exposing (..)

type alias HoleIndex =
  (HoleId, Int)

type UnExp
  = UConstructor Ident UnExp
  | UNum Num
  | UBool Bool
  | UString String
  | UTuple (List UnExp)
  | UFunClosure Env (List Ident) {- Type -} Exp
  | UHoleClosure Env HoleIndex {- Type -}
  | UApp UnExp (List UnExp)
  | UCase UnExp (List (Ident, Ident, Exp))

type alias EvalState =
  { nextHoleIndex : Dict HoleId Int
  }

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
        eval newEnv body

      GT ->
        Evaluator.fail "Supplied too many arguments"

eval : Env -> Exp -> UnExpEvaluator
eval env exp =
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
            Evaluator.mapM (eval env) eArgs
        in
          eval env eFunction |> Evaluator.andThen (\uFunction ->
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
          EEmptyHole holeId ->
            Evaluator.get |> Evaluator.andThen (\state ->
              let
                freshHoleIndex =
                  Dict.get holeId state.nextHoleIndex
                    |> Maybe.withDefault 0

                newState =
                  { state
                      | nextHoleIndex =
                        Dict.insert
                          holeId
                          (freshHoleIndex + 1)
                          state.nextHoleIndex
                  }
              in
                Evaluator.put newState |> Evaluator.andThen (\_ ->
                  Evaluator.succeed <|
                    UHoleClosure env (holeId, freshHoleIndex)
                )
            )

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
                Evaluator.mapM (eval env) eArgs
            in
              uArgs
                |> Evaluator.andThen (bindingEval env body parameters)

          Nothing ->
            Evaluator.fail "Could not get record entries from let"

      EColonType _ _ _ _ _ ->
        Evaluator.fail "Colon type not supported"

      EParens _ eInner _ _ ->
        eval env eInner

      ERecord _ _ decls _ ->
        case recordEntriesFromDeclarations decls of
          Just entries ->
            case tupleEncodingUnapply entries of
              Just tupleEntries ->
                tupleEntries
                  |> Evaluator.mapM (Tuple.second >> eval env)
                  |> Evaluator.map UTuple

              Nothing ->
                Evaluator.fail "Arbitrary records not supported"

          Nothing ->
            Evaluator.fail "Could not get record entries"

      ESelect _ _ _ _ _ ->
        Evaluator.fail "Select not supported"

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

    UCase u0 branches ->
      let
        unparseBranch (constructorName, varName, body) =
          constructorName
            ++ " "
            ++ varName ++ " →"
            ++ LeoUnparser.unparse body
      in
        "case "
          ++ unparse u0
          ++ " of "
          ++ String.join " " (List.map unparseBranch branches)


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

    UCase uScrutinee _ ->
      [uScrutinee]

flatten : UnExp -> List UnExp
flatten u =
  u :: List.concatMap flatten (children u)

findHoles : HoleId -> UnExp -> List (Int, Env)
findHoles holeId =
  let
    extract u =
      case u of
        UHoleClosure env (holeId, index) ->
          [(index, env)]

        _ ->
          []
  in
    flatten
      >> List.concatMap extract
      >> List.sortBy Tuple.first

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
  ignore zeroOrMore (\char -> char == ' ')

capitalIdentifier : Parser String
capitalIdentifier =
  succeed (++)
    |= keep (Exactly 1) Char.isUpper
    |= keep zeroOrMore (\c -> Char.isUpper c || Char.isLower c)

exConstructor : Parser Example
exConstructor =
  lazy <| \_ ->
    inContext "constructor example" <|
      succeed ExConstructor
        |= capitalIdentifier
        |= example

exNum : Parser Example
exNum =
  let
    sign =
      oneOf
        [ succeed (-1)
            |. symbol "-"
        , succeed 1
        ]
  in
    try <|
      inContext "number example" <|
        succeed (\s n -> ExNum (s * n))
          |= sign
          |= float

exBool : Parser Example
exBool =
  inContext "boolean example" <|
    map ExBool <|
      oneOf
        [ token "True" True
        , token "False" False
        ]

exString : Parser Example
exString =
  inContext "string example" <|
    map (\(_, content) -> ExString content)
      singleLineString

exTuple : Parser Example
exTuple =
  lazy <| \_ ->
    inContext "tuple example" <|
      map ExTuple <|
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
  lazy <| \_ ->
    let
      binding : Parser (Example, Example)
      binding =
        succeed (,)
          |= example
          |. spaces
          |. symbol "->"
          |. spaces
          |= example
    in
      inContext "partial function example" <|
        map ExPartialFunction <|
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
  lazy <| \_ ->
    oneOf
       [ exConstructor
       , exNum
       , exBool
       , exString
       , exTuple
       , exPartialFunction
       ]

parseExample : String -> Result Parser.Error Example
parseExample =
  run example
