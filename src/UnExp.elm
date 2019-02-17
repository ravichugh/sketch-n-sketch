module UnExp exposing
  ( Env
  , UnExp(..)
  , UnVal(..)
  , asExp, asValue
  , unval
  , unparseEnv, unparse
  , statefulMap, map, children, flatten
  , findHoles
  )

import Char

import State exposing (State)

import Parser as P exposing (..)
import Parser.LanguageKit as LanguageKit
import ParserUtils exposing (..)

import Lang exposing (Exp, Ident, HoleId, Num)
import LeoUnparser

import Utils

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type alias HoleIndex =
  (HoleId, Int)

type alias Env =
  List (Ident, UnExp ())

-- The d is for extra data
type UnExp d
  = UConstructor d Ident (UnExp d)
  | UNum d Num
  | UBool d Bool
  | UString d String
  | UTuple d (List (UnExp d))
  | UFunClosure d Env (List Ident) {- Type -} Exp
  | UHoleClosure d Env HoleIndex
  | UApp d (UnExp d) (List (UnExp d))
  | UGet d Int Int (UnExp d)
  | UCase d Env (UnExp d) (List (Ident, Ident, Exp))

type UnVal
  = UVConstructor Ident UnVal
  | UVNum Num
  | UVBool Bool
  | UVString String
  | UVTuple (List UnVal)
  | UVFunClosure Env (List Ident) {- Type -} Exp

--------------------------------------------------------------------------------
-- Value Conversion
--------------------------------------------------------------------------------

asExp : UnVal -> UnExp ()
asExp v =
  case v of
    UVConstructor ident arg ->
      UConstructor () ident (asExp arg)

    UVNum n ->
      UNum () n

    UVBool b ->
      UBool () b

    UVString s ->
      UString () s

    UVTuple args ->
      UTuple () (List.map asExp args)

    UVFunClosure env params body ->
      UFunClosure () env params body

asValue : UnExp d -> Maybe UnVal
asValue u =
  case u of
    UConstructor _ ident arg ->
      Maybe.map (UVConstructor ident) (asValue arg)

    UNum _ n ->
      Just <|
        UVNum n

    UBool _ b ->
      Just <|
        UVBool b

    UString _ s ->
      Just <|
        UVString s

    UTuple _ args ->
      args
        |> List.map asValue
        |> Utils.projJusts
        |> Maybe.map UVTuple

    UFunClosure _ env params body ->
      Just <|
        UVFunClosure env params body

    UHoleClosure _ _ _ ->
      Nothing

    UApp _ _ _ ->
      Nothing

    UGet _ _ _ _ ->
      Nothing

    UCase _ _ _ _ ->
      Nothing

--------------------------------------------------------------------------------
-- Value Parsing
--------------------------------------------------------------------------------

spaces : Parser ()
spaces =
  ignore zeroOrMore (\char -> char == ' ')

capitalIdentifier : Parser String
capitalIdentifier =
  succeed (++)
    |= keep (Exactly 1) Char.isUpper
    |= keep zeroOrMore (\c -> Char.isUpper c || Char.isLower c)

uvConstructor : Parser UnVal
uvConstructor =
  lazy <| \_ ->
    inContext "constructor unval" <|
      succeed UVConstructor
        |= capitalIdentifier
        |= unval

uvNum : Parser UnVal
uvNum =
  let
    sign =
      oneOf
        [ succeed (-1)
            |. symbol "-"
        , succeed 1
        ]
  in
    try <|
      inContext "number unval" <|
        succeed (\s n -> UVNum (s * n))
          |= sign
          |= float

uvBool : Parser UnVal
uvBool =
  inContext "boolean unval" <|
    P.map UVBool <|
      oneOf
        [ token "True" True
        , token "False" False
        ]

uvString : Parser UnVal
uvString =
  inContext "string unval" <|
    P.map (\(_, content) -> UVString content)
      singleLineString

uvTuple : Parser UnVal
uvTuple =
  lazy <| \_ ->
    inContext "tuple unval" <|
      P.map UVTuple <|
        LanguageKit.sequence
          { start = "("
          , separator = ","
          , end = ")"
          , spaces = spaces
          , item = unval
          , trailing = LanguageKit.Forbidden
          }

uvFunClosure : Parser UnVal
uvFunClosure =
  fail "function closure unval not yet supported"

unval : Parser UnVal
unval =
  lazy <| \_ ->
    oneOf
       [ uvNum
       , uvBool
       , uvString
       , uvTuple
       , uvConstructor
       ]

parseVal : String -> Result P.Error UnVal
parseVal =
  run unval

--------------------------------------------------------------------------------
-- Unparsing
--------------------------------------------------------------------------------

unparseEnv : Env -> String
unparseEnv =
  let
    showBinding : (Ident, UnExp d) -> String
    showBinding (i, u) =
      i ++ " → " ++ unparse u
  in
    List.map showBinding >> String.join ", "

unparse : UnExp d -> String
unparse u =
  case u of
    UConstructor _ name uArg ->
      name ++ " " ++ unparse uArg

    UNum _ n ->
      toString n

    UBool _ b ->
      if b then "True" else "False"

    UString _ s ->
      "\"" ++ s ++ "\""

    UTuple _ us ->
      "("
        ++ String.join ", " (List.map unparse us)
        ++ ")"

    UFunClosure _ env args body ->
      let
        argsString =
          String.join ", " args
      in
        "["
          ++ unparseEnv env
          ++ "] λ"
          ++ argsString
          ++ " ."
          ++ LeoUnparser.unparse body

    UHoleClosure _ env (i, j) ->
      "[" ++ unparseEnv env ++ "] ??(" ++ toString i ++ ", " ++ toString j ++ ")"

    UApp _ uFunction uArgs ->
      let
        parens u beginning =
          "(" ++ beginning ++ ") " ++ unparse u
      in
        List.foldl parens (unparse uFunction) uArgs

    UGet _ n i uTuple ->
      "get_" ++ toString n ++ "_" ++ toString i ++ " " ++ unparse uTuple

    UCase _ env u0 branches ->
      let
        unparseBranch (constructorName, varName, body) =
          constructorName
            ++ " "
            ++ varName ++ " →"
            ++ LeoUnparser.unparse body
      in
        "["
          ++ unparseEnv env
          ++ "] case "
          ++ unparse u0
          ++ " of "
          ++ String.join " " (List.map unparseBranch branches)

--------------------------------------------------------------------------------
-- Generic Library
--------------------------------------------------------------------------------

statefulMap : (UnExp d -> State s (UnExp d)) -> UnExp d -> State s (UnExp d)
statefulMap f u =
  flip State.andThen (f u) <| \uNew ->
    case uNew of
      UConstructor d ident arg ->
        State.map (UConstructor d ident) (statefulMap f arg)

      UNum d n ->
        State.pure <| UNum d n

      UBool d b ->
        State.pure <| UBool d b

      UString d s ->
        State.pure <| UString d s

      UTuple d args ->
        State.map (UTuple d) (State.mapM (statefulMap f) args)

      UFunClosure d env params body ->
        State.pure <| UFunClosure d env params body

      UHoleClosure d env holeIndex ->
        State.pure <| UHoleClosure d env holeIndex

      UApp d uFunction uArgs ->
        flip State.andThen (statefulMap f uFunction) <| \newFunction ->
          State.map (UApp d newFunction) (State.mapM (statefulMap f) uArgs)

      UGet d n i uTuple ->
        State.map (UGet d n i) (statefulMap f uTuple)

      UCase d env uScrutinee branches ->
        State.map
          (\newScrutinee -> UCase d env newScrutinee branches)
          (statefulMap f uScrutinee)

map : (UnExp d -> UnExp d) -> UnExp d -> UnExp d
map f =
  statefulMap (f >> State.pure) >> State.run () >> Tuple.first

children : UnExp d -> List (UnExp d)
children u =
  case u of
    UConstructor _ _ arg ->
      [arg]

    UNum _ _ ->
      []

    UBool _ _ ->
      []

    UString _ _ ->
      []

    UTuple _ args ->
      args

    UFunClosure _ _ _ _ ->
      []

    UHoleClosure _ _ _ ->
      []

    UApp _ uFunction uArgs ->
      uFunction :: uArgs

    UGet _ _ _ uTuple ->
      [uTuple]

    UCase _ _ uScrutinee _ ->
      [uScrutinee]

flatten : UnExp d -> List (UnExp d)
flatten u =
  u :: List.concatMap flatten (children u)

--------------------------------------------------------------------------------
-- Additional Functions
--------------------------------------------------------------------------------

findHoles : HoleId -> UnExp d -> List (Int, Env)
findHoles targetHoleId =
  let
    extract u =
      case u of
        UHoleClosure d env (holeId, index) ->
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
