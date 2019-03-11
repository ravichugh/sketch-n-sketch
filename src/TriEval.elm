module TriEval exposing
  ( evalWithEnv
  , eval
  , ensureConstraintFree
  )

import Dict exposing (Dict)
import Char

import Utils

import Evaluator exposing (Evaluator)
import State exposing (State)

import UnLang as U exposing (..)
import Constraints

import Lang exposing (..)

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type alias EvalState =
  { constraints : Maybe Constraints
  }

type alias UnExpEvaluator =
  Evaluator EvalState String (UnExp ())

--------------------------------------------------------------------------------
-- Evaluator Helper
--------------------------------------------------------------------------------

withConstraints : Maybe Constraints -> UnExp () -> UnExpEvaluator
withConstraints ks u =
  let
    addConstraints old ks =
      { old | constraints =
          Maybe.map2 (++) old.constraints ks
      }
  in
    Evaluator.do Evaluator.get <| \old ->
    Evaluator.do (Evaluator.put <| addConstraints old ks) <| \_ ->
    Evaluator.succeed u

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
  U.Env -> Exp -> List Ident -> List (UnExp ()) -> UnExpEvaluator
bindingEval currentEnv body parameters arguments =
  let
    envExtension =
      Utils.zip parameters arguments
        |> U.pairsToEnv

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

eval_ : U.Env -> Exp -> UnExpEvaluator
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
        case U.lookupVar x env of
          Just u ->
            Evaluator.succeed u

          Nothing ->
            case U.lookupCtor x env of
              Just (ctorName, uBinding) ->
                let
                  makeUnwrapper () =
                    Evaluator.succeed <|
                      UCase
                        ()
                        env
                        uBinding
                        [(ctorName, "x", eVar0 "x")]
                in
                  case uBinding of
                    UConstructor _ ctorBindingName uCtorArg ->
                      if ctorBindingName == ctorName then
                        Evaluator.succeed uCtorArg
                      else
                        makeUnwrapper ()

                    _ ->
                      makeUnwrapper ()

              _ ->
                Evaluator.fail <|
                  "Variable not found: '" ++ x ++ "'"

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

          evalConstraintsAssert (e1, e2) =
            eval_ env e1 |> Evaluator.andThen (\u1 ->
              eval_ env e2 |> Evaluator.andThen (\u2 ->
                withConstraints
                  (Constraints.assertEqual u1 u2)
                  (UTuple () [])
              )
            )
        in
          case Lang.toTupleGet exp of
            Just tupleGet ->
              evalGet tupleGet

            Nothing ->
              case Lang.toConstraintsAssertion exp of
                Just constraintsAssertion ->
                  evalConstraintsAssert constraintsAssertion

                Nothing ->
                  default ()

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
                      Lang.entriesToMaybeCtorNameAndArgPats entries
                    of
                      Just (ctorName, args) ->
                        case args of
                          [] ->
                            Evaluator.succeed
                              (ctorName, noBindingName, body)


                          [arg] ->
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
                  U.addVar argName uArg env
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
                      (\u -> (u :: us, U.addVar param u latestEnv))
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
                  Lang.entriesToMaybeCtorNameAndArgExps entries
                of
                  Just (ctorName, args) ->
                    if ctorName == "PF" then
                      case args of
                        [pfArg] ->
                          case getExpString pfArg of
                            Just pfString ->
                              case
                                U.parseUnval pfString
                                  |> Result.toMaybe
                              of
                                Just (UVPartialFunction pf) ->
                                  Evaluator.succeed <|
                                    UPartialFunction () pf

                                _ ->
                                  Evaluator.fail "PF applied to non-pf string"

                            Nothing ->
                              Evaluator.fail "PF applied to non-string"

                        _ ->
                          Evaluator.fail "PF applied to more than one argument"
                    else
                      args
                        |> Evaluator.mapM (eval_ env)
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

evalWithEnv : U.Env -> Exp -> Result String (UnExp (), Maybe Constraints)
evalWithEnv env =
  eval_ env
    >> Evaluator.run { constraints = Just [] }
    >> Result.map (Tuple.mapFirst setHoleIndexes)
    >> Result.map (Tuple.mapSecond .constraints)

eval : Exp -> Result String (UnExp (), Maybe Constraints)
eval =
  evalWithEnv []

ensureConstraintFree :
  Result String (UnExp (), Maybe Constraints) -> Maybe (UnExp ())
ensureConstraintFree =
  Result.toMaybe
    >> Maybe.andThen
         (\(u, ks) -> if ks == Just [] then Just u else Nothing)
