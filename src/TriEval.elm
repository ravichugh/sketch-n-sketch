module TriEval exposing
  (eval, unparse)

import Utils
import LeoUnparser

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

identifierFromPat : Pat -> Maybe Ident
identifierFromPat p =
  case unwrapPat p of
    PVar _ ident _ ->
      Just ident

    _ ->
      Nothing

eval : Env -> Exp -> Result String UnExp
eval env expr =
  let
    e =
      unwrapExp expr
  in
    case e of
      -- E-Const

      EConst _ n _ _ ->
        Ok <|
          UNum n

      EBase _ baseVal ->
        Ok <|
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

      -- E-Var

      EVar _ x ->
        env
          |> Utils.maybeFind x
          |> Result.fromMaybe ("Variable not found: '" ++ x ++ "'")
          |> Result.map Tuple.first

      -- E-App

      EApp _ ef es _ _ ->
        case eval env ef of
          Ok (UFunClosure functionEnv vars body) ->
            if List.length vars /= List.length es then
              Err "Partial application not supported"
            else
              es
                |> List.map (eval env)
                |> Utils.projOk
                |> Result.map
                     ( List.map (\u -> (u, ()))
                     )
                |> Result.map
                     ( \envBindings ->
                         Utils.zip vars envBindings ++ functionEnv
                     )
                |> Result.andThen (\newEnv -> eval newEnv body)

          evaluated ->
            Err "Not a closure"

      -- E-Match

      ECase _ e0 branches _ ->
        Err "Case not supported" -- TODO

      -- E-Hole

      EHole _ hole  ->
        case hole of
          EEmptyHole holeId ->
            Ok <|
              UHoleClosure env (holeId, 0)

          _ ->
            Err "Unsupported hole type"

      -- Misc.

      EOp _ _ op args _ ->
        Err "Op not supported"

      EList _ args _ _ _ ->
        Err "List not supported"

      EIf _ condition _ trueBranch _ falseBranch _ ->
        Err "If not supported"

      ELet _ _ _ _ _ ->
        Err "Let not supported"

      EColonType _ _ _ _ _ ->
        Err "Colon type not supported"

      EParens _ e0 _ _ ->
        eval env e0

      ERecord _ _ decls _ ->
        case recordEntriesFromDeclarations decls of
          Just fields ->
            case tupleEncodingUnapply fields of
              Just tupleEntries ->
                tupleEntries
                  |> List.map (Tuple.second >> eval env)
                  |> Utils.projOk
                  |> Result.map UTuple

              Nothing ->
                Err "Arbitrary records not supported"

          Nothing ->
            Err "Could not get record entries"

      ESelect _ _ _ _ _ ->
        Err "Select not supported"

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

    UApp u1 u2 ->
      "(" ++ unparse u1 ++ ") " ++ unparse u2

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
