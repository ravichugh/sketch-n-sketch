module TriEval exposing
  ( evalWithEnv
  , eval
  )

import Dict exposing (Dict)
import Char

import Utils

import Evaluator exposing (Evaluator)
import State exposing (State)

import UnExp exposing (..)
import Lang exposing (..)

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type alias EvalState =
  {}

type alias UnExpEvaluator =
  Evaluator EvalState String (UnExp ())

--------------------------------------------------------------------------------
-- Core Evaluation
--------------------------------------------------------------------------------

identifierFromPat : Pat -> Maybe Ident
identifierFromPat p =
  case unwrapPat p of
    PVar _ ident _ ->
      Just ident

    _ ->
      Nothing

bindingEval :
  UnExp.Env -> Exp -> List Ident -> List (UnExp ()) -> UnExpEvaluator
bindingEval currentEnv body parameters arguments =
  let
    envExtension =
      Utils.zip parameters arguments
        |> UnExp.pairsToEnv

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
          UFunClosure () newEnv (List.drop argLength parameters) body

      EQ ->
        eval_ newEnv body

      GT ->
        Evaluator.fail "Supplied too many arguments"

eval_ : UnExp.Env -> Exp -> UnExpEvaluator
eval_ env exp =
  let
    e =
      unwrapExp exp
  in
    case e of
      -- E-Const

      EConst _ n _ _ ->
        Evaluator.succeed <|
          UNum () n

      EBase _ baseVal ->
        Evaluator.succeed <|
          case baseVal of
            EBool b ->
              UBool () b

            EString _ s ->
              UString () s

            ENull ->
              UString () "null"

      -- E-Lambda

      EFun _ pats body _ ->
        pats
          |> List.map identifierFromPat
          |> Utils.projJusts
          |> Result.fromMaybe "Non-identifier pattern in function"
          |> Result.map (\vars -> UFunClosure () env vars body)
          |> Evaluator.fromResult

      -- E-Var

      EVar _ x ->
        env
          |> UnExp.lookupVar x
          |> Result.fromMaybe ("Variable not found: '" ++ x ++ "'")
          |> Evaluator.fromResult

      -- E-App

      EApp _ eFunction eArgs _ _ ->
        let
          default () =
            let
              uArgs =
                Evaluator.mapM (eval_ env) eArgs
            in
              eval_ env eFunction |> Evaluator.andThen (\uFunction ->
                case uFunction of
                  UFunClosure _ functionEnv parameters body ->
                    uArgs
                      |> Evaluator.andThen
                           (bindingEval functionEnv body parameters)

                  UHoleClosure _ _ _ ->
                    Evaluator.map (UApp () uFunction) uArgs

                  _ ->
                    Evaluator.fail "Not a proper application"
              )

          evalGet (n, i, arg) =
            eval_ env arg |> Evaluator.andThen (\uArg ->
              case uArg of
                UTuple _ tupleArgs ->
                  case Utils.maybeGeti1 i tupleArgs of
                    Just returnValue ->
                      Evaluator.succeed returnValue

                    Nothing ->
                      Evaluator.fail "Out of bounds index for 'get'"

                UHoleClosure _ _ _ ->
                  Evaluator.succeed <|
                    UGet () n i uArg

                _ ->
                  Evaluator.fail "Not a proper 'get'"
            )
        in
          exp
            |> toTupleGet
            |> Maybe.map evalGet
            |> Utils.withLazyDefault default

      -- E-Match

      ECase _ e0 branches _ ->
        let
          noBindingName =
            "__NO_BINDING_NAME__"

          toUBranch branch =
            case branch.val of
              Branch_ _ pat body _ ->
                case unwrapPat pat of
                  PRecord _ entries _ ->
                    case
                      Lang.dataTypeEncodingUnapply
                        entries
                        Lang.getPatString
                        Lang.getPatEntries
                    of
                      Just (ctorName, args) ->
                        case args of
                          [] ->
                            Evaluator.succeed
                              (ctorName, noBindingName, body)


                          [(_, arg)] ->
                            case unwrapPat arg of
                              PVar _ argName _ ->
                                Evaluator.succeed (ctorName, argName, body)

                              _ ->
                                Evaluator.fail
                                  "Non-var pattern in constructor match"

                          _ ->
                            Evaluator.fail
                              "Multiple arguments in constructor pattern match"

                      Nothing ->
                        Evaluator.fail "Non-constructor pattern match"

                  _ ->
                    Evaluator.fail
                      "Non-record (constructor sugar) pattern match"

          evalBranch uArg (_, argName, body) =
            let
              newEnv =
                if argName == noBindingName then
                  env
                else
                  UnExp.addVar argName uArg env
            in
              eval_ newEnv body
        in
          Evaluator.mapM toUBranch branches |> Evaluator.andThen (\uBranches ->
            eval_ env e0 |> Evaluator.andThen (\u0 ->
              case u0 of
                UConstructor () ctorName uArg ->
                  uBranches
                    |> Utils.findFirst (\(c, _, _) -> c == ctorName)
                    |> Result.fromMaybe
                         ( "Non-exhaustive pattern match, could not find '"
                             ++ ctorName
                             ++ "'"
                         )
                    |> Evaluator.fromResult
                    |> Evaluator.andThen (evalBranch uArg)

                _ ->
                  Evaluator.succeed (UCase () env u0 uBranches)
            )
          )

      -- E-Hole

      EHole _ hole  ->
        case hole of
          EEmptyHole holeId ->
            Evaluator.succeed <|
              UHoleClosure () env (holeId, -1)

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
              paramArgPairs =
                List.map
                  (\(_, _, ident, _, exp) -> (ident, exp))
                  entries

              (parameters, eArgs) =
                List.unzip paramArgPairs

              uArgs =
                let
                  evalAndBind (param, arg) (us, latestEnv) =
                    Evaluator.map
                      (\u -> (u :: us, UnExp.addVar param u latestEnv))
                      (eval_ latestEnv arg)
                in
                  Evaluator.foldlM evalAndBind ([], env) paramArgPairs
                    |> Evaluator.map (Tuple.first >> List.reverse)
            in
              uArgs
                |> Evaluator.andThen (bindingEval env body parameters)

          Nothing ->
            Evaluator.fail "Could not get record entries from let"

      EColonType _ eInner _ _ _ ->
        eval_ env eInner

      EParens _ eInner _ _ ->
        eval_ env eInner

      ERecord _ _ decls _ ->
        case recordEntriesFromDeclarations decls of
          Just entries ->
            case tupleEncodingUnapply entries of
              Just tupleEntries ->
                tupleEntries
                  |> Evaluator.mapM (Tuple.second >> eval_ env)
                  |> Evaluator.map (UTuple ())

              Nothing ->
                case
                  Lang.dataTypeEncodingUnapply
                    entries
                    Lang.getExpString
                    Lang.getExpEntries
                of
                  Just (ctorName, args) ->
                    args
                      |> Evaluator.mapM (Tuple.second >> eval_ env)
                      |> Evaluator.map
                           ( \uArgs ->
                               case uArgs of
                                 [uArg] ->
                                   uArg
                                 _ ->
                                   UTuple () uArgs
                           )
                      |> Evaluator.map (UConstructor () ctorName)

                  _ ->
                    Evaluator.fail "Arbitrary records not supported"

          Nothing ->
            Evaluator.fail "Could not get record entries"

      ESelect _ _ _ _ _ ->
        Evaluator.fail "Select not supported"

--------------------------------------------------------------------------------
-- Additional Pipeline Operations
--------------------------------------------------------------------------------

setHoleIndexes : UnExp d -> UnExp d
setHoleIndexes =
  let
    holeSetter : UnExp d -> State (Dict HoleId Int) (UnExp d)
    holeSetter u =
      case u of
        UHoleClosure d env (holeId, holeIndex) ->
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
                UHoleClosure d env (holeId, freshHoleIndex)

        _ ->
          State.pure u
  in
    statefulMap holeSetter >> State.run Dict.empty >> Tuple.first

--------------------------------------------------------------------------------
-- Full Evaluation
--------------------------------------------------------------------------------

evalWithEnv : UnExp.Env -> Exp -> Result String (UnExp ())
evalWithEnv env =
  eval_ env
    >> Evaluator.run {}
    >> Result.map Tuple.first
    >> Result.map setHoleIndexes

eval : Exp -> Result String (UnExp ())
eval =
  evalWithEnv []
