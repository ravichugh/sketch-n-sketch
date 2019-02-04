module UnExp exposing
  ( Env
  , UnExp(..)
  , UnVal(..)
  , asExp, asValue
  , unparseEnv, unparse
  , statefulMap, map, children, flatten
  , findHoles
  )

import State exposing (State)

import Lang exposing (Exp, Ident, HoleId, Num)
import LeoUnparser

import Utils

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type alias HoleIndex =
  (HoleId, Int)

type alias Env =
  List (Ident, (UnExp, () {- Type -}))

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

asExp : UnVal -> UnExp
asExp v =
  case v of
    UVConstructor ident arg ->
      UConstructor ident (asExp arg)

    UVNum n ->
      UNum n

    UVBool b ->
      UBool b

    UVString s ->
      UString s

    UVTuple args ->
      UTuple (List.map asExp args)

    UVFunClosure env params body ->
      UFunClosure env params body

asValue : UnExp -> Maybe UnVal
asValue u =
  case u of
    UConstructor ident arg ->
      Maybe.map (UVConstructor ident) (asValue arg)

    UNum n ->
      Just <|
        UVNum n

    UBool b ->
      Just <|
        UVBool b

    UString s ->
      Just <|
        UVString s

    UTuple args ->
      args
        |> List.map asValue
        |> Utils.projJusts
        |> Maybe.map UVTuple

    UFunClosure env params body ->
      Just <|
        UVFunClosure env params body

    UHoleClosure _ _ ->
      Nothing

    UApp _ _ ->
      Nothing

    UCase _ _ _ ->
      Nothing

--------------------------------------------------------------------------------
-- Unparsing
--------------------------------------------------------------------------------

unparseEnv : Env -> String
unparseEnv =
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
          ++ unparseEnv env
          ++ "] λ"
          ++ argsString
          ++ " ."
          ++ LeoUnparser.unparse body

    UHoleClosure env (i, j) ->
      "[" ++ unparseEnv env ++ "] ??(" ++ toString i ++ ", " ++ toString j ++ ")"

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
          ++ unparseEnv env
          ++ "] case "
          ++ unparse u0
          ++ " of "
          ++ String.join " " (List.map unparseBranch branches)

--------------------------------------------------------------------------------
-- Generic Library
--------------------------------------------------------------------------------

statefulMap : (UnExp -> State s UnExp) -> UnExp -> State s UnExp
statefulMap f u =
  flip State.andThen (f u) <| \uNew ->
    case uNew of
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

--------------------------------------------------------------------------------
-- Additional Functions
--------------------------------------------------------------------------------

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
