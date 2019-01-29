module TriEval exposing
  (eval, unparse)

import Utils
import LeoUnparser

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
    bindEval : Env -> Exp -> List Ident -> List UnExp -> Result String UnExp
    bindEval currentEnv body parameters arguments =
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
            Ok <|
              UFunClosure newEnv (List.drop argLength parameters) body

          EQ ->
            eval newEnv body

          GT ->
            Err "Supplied too many arguments"

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

      EApp _ eFunction eArgs _ _ ->
        let
          uArgs =
            eArgs
              |> List.map (eval env)
              |> Utils.projOk
        in
          case eval env eFunction of
            Ok (UFunClosure functionEnv parameters body) ->
              Result.andThen (bindEval functionEnv body parameters) uArgs

            Ok ((UHoleClosure _ _) as hole) ->
                Result.map (UApp hole) uArgs

            _ ->
              Err "Not a proper application"

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

      ELet _ _ decls _ body ->
        case recordEntriesFromDeclarations decls of
          Just entries ->
            let
              (parameters, eArgs) =
                entries
                  |> List.map (\(_, _, ident, _, exp) -> (ident, exp))
                  |> List.unzip

              uArgs =
                eArgs
                  |> List.map (eval env)
                  |> Utils.projOk
            in
              Result.andThen (bindEval env body parameters) uArgs

          Nothing ->
            Err "Could not get record entries from let"

      EColonType _ _ _ _ _ ->
        Err "Colon type not supported"

      EParens _ e0 _ _ ->
        eval env e0

      ERecord _ _ decls _ ->
        case recordEntriesFromDeclarations decls of
          Just entries ->
            case tupleEncodingUnapply entries of
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
