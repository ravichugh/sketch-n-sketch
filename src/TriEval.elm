module TriEval exposing
  (eval, unparse)

import Utils
import Lang exposing (..)

type alias HoleIndex =
  (Int, Int)

type UnExp
  = UConstructor Ident UnExp
  | UNum Num
  | UBool Bool
  | UString String
  | UTuple (List UnExp)
  | UFunClosure Env (List Ident) {- Type -} Exp
  | UHoleClosure Env HoleIndex {- Type -}
  | UApp UnExp UnExp
  | UCase UnExp (List (Ident, Ident, Exp))

type alias Env =
  List (Ident, (UnExp, () {- Type -}))

identifierFromPat : Pat -> Maybe String
identifierFromPat p =
  case unwrapPat p of
    PVar _ ident _ ->
      Just ident

    _ ->
      Nothing

eval : Env -> Exp -> Maybe UnExp
eval env expr =
  let
    e =
      unwrapExp expr
  in
    case e of
      -- E-Const

      EConst _ n _ _ ->
        Just <|
          UNum n

      EBase _ baseVal ->
        Just <|
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
          |> Maybe.map (\vars -> UFunClosure env vars body)

      -- E-Var

      EVar _ x ->
        Maybe.map Tuple.first <|
          Utils.maybeFind x env

      -- E-App

      EApp _ ef es _ _ ->
        case eval env ef of
          Just (UFunClosure functionEnv vars body) ->
            es
              |> List.map (eval env)
              |> Utils.projJusts
              |> Maybe.map
                   ( List.map (\u -> (u, ()))
                   )
              |> Maybe.map
                   ( \envBindings ->
                       Utils.zip vars envBindings ++ functionEnv
                   )
              |> Maybe.andThen (\newEnv -> eval newEnv body)

          _ ->
            Nothing

      -- E-Match

      ECase _ e0 branches _ ->
        Nothing -- TODO

      -- E-Hole

      EHole _ _ ->
        Just <|
          UHoleClosure env (-1, -1)

      -- Misc.

      EOp _ _ op args _ ->
        Nothing

      EList _ args _ _ _ ->
        Nothing

      EIf _ condition _ trueBranch _ falseBranch _ ->
        Nothing

      ELet _ _ _ _ _ ->
        Nothing

      EColonType _ _ _ _ _ ->
        Nothing

      EParens _ e0 _ _ ->
        eval env e0

      ERecord _ _ _ _ ->
        Nothing

      ESelect _ _ _ _ _ ->
        Nothing

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
        "[E] " ++ "λ" ++ argsString ++ " . " ++ "e"

    UHoleClosure env (i, j) ->
      "[E] ??(" ++ toString i ++ ", " ++ toString j ++ ")"

    UApp u1 u2 ->
      "(" ++ unparse u1 ++ ") " ++ unparse u2

    UCase u0 branches ->
      let
        unparseBranch (constructorName, varName, body) =
          constructorName ++ " " ++ varName ++ " -> " ++ "e"
      in
        "case "
          ++ unparse u0
          ++ " of "
          ++ String.join " " (List.map unparseBranch branches)
