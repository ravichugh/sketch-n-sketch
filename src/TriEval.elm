--------------------------------------------------------------------------------
-- This module contains all the code for dynamic handling of UnExps, including:
--   - Evaluation
--   - Backpropagation
--   - Example collection (constraint collection)
--------------------------------------------------------------------------------
module TriEval exposing
  ( evalWithEnv
  , eval
  , ensureConstraintFree
  , backprop
  )

import Dict exposing (Dict)
import Char

import Utils

import Evaluator exposing (Evaluator)
import State exposing (State)

import UnLang as U exposing (..)

import Lang exposing (..)

--==============================================================================
--= Evaluation
--==============================================================================

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
  List Ident -> U.Env -> Exp -> List Ident -> List (UnExp ()) -> UnExpEvaluator
bindingEval recNames currentEnv body parameters arguments =
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
          UFunClosure () recNames newEnv (List.drop argLength parameters) body

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
          |> Result.map (\vars -> UFunClosure () [] env vars body)
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
              uArgsEvaluation =
                Evaluator.mapM (eval_ env) eArgs
            in
              eval_ env eFunction |> Evaluator.andThen (\uFunction ->
                case uFunction of
                  UFunClosure _ recNames functionEnv parameters body ->
                    let
                      recBindings =
                        case recNames of
                          [] ->
                            []

                          [recName] ->
                            U.pairsToEnv [(recName, uFunction)]

                          _ ->
                            Debug.log
                              ( "[WARN]  Mutually recursive functions not yet"
                                  ++ "supported: "
                                  ++ toString recNames
                              )
                              []
                    in
                      Evaluator.andThen
                        ( bindingEval
                            recNames
                            (functionEnv ++ recBindings)
                            body
                            parameters
                        )
                        uArgsEvaluation

                  UHoleClosure _ _ _ ->
                    Evaluator.map (UApp () uFunction) uArgsEvaluation

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
                  (assertEqual u1 u2)
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
              nameBindingPairs =
                List.map
                  (\(_, _, ident, _, exp) -> (ident, exp))
                  entries

              (names, bindings) =
                List.unzip nameBindingPairs

              bindingsEvaluation =
                let
                  addRecursiveBinding name u =
                    case u of
                      UFunClosure _ recNames env params body ->
                        UFunClosure () (name :: recNames) env params body

                      _ ->
                        u

                  accumulate name us latestEnv u =
                    (u :: us, U.addVar name u latestEnv)

                  evalAndBind (name, binding) (us, latestEnv) =
                    eval_ latestEnv binding
                      |> Evaluator.map
                           ( addRecursiveBinding name
                               >> accumulate name us latestEnv
                           )
                in
                  Evaluator.foldlM evalAndBind ([], env) nameBindingPairs
                    |> Evaluator.map (Tuple.first >> List.reverse)
            in
              Evaluator.andThen
                (bindingEval [] env body names)
                bindingsEvaluation

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

--==============================================================================
--= Constraints
--==============================================================================

assertEqual : UnExp () -> UnExp () -> Maybe Constraints
assertEqual u1 u2 =
  case (u1, u2) of
    (UConstructor _ ctorName1 uInner1, UConstructor _ ctorName2 uInner2) ->
      if ctorName1 == ctorName2 then
        assertEqual uInner1 uInner2
      else
        Nothing

    (UNum _ n1, UNum _ n2) ->
      if n1 == n2 then
        Just []
      else
        Nothing

    (UBool _ b1, UBool _ b2) ->
      if b1 == b2 then
        Just []
      else
        Nothing

    (UString _ s1, UString _ s2) ->
      if s1 == s2 then
        Just []
      else
        Nothing

    (UTuple _ us1, UTuple _ us2) ->
      if List.length us1 == List.length us2 then
        List.map2 assertEqual us1 us2
          |> Utils.projJusts
          |> Maybe.map List.concat
      else
        Nothing

    (UHoleClosure _ env (holeId, _), _) ->
      Maybe.map (\ex -> [(holeId, (env, ex))]) <|
        expToExample u2

    (_, UHoleClosure _ env (holeId, _)) ->
      Maybe.map (\ex -> [(holeId, (env, ex))]) <|
        expToExample u1

    (UApp _ u1 u1Args, UApp _ u2 u2Args) ->
      if List.length u1Args == List.length u2Args then
        let
          funcConstraints =
            assertEqual u1 u2

          argConstraints =
            List.map2 assertEqual u1Args u2Args
              |> Utils.projJusts
              |> Maybe.map List.concat
        in
          Maybe.map2 (++) funcConstraints argConstraints
      else
        Nothing

    (UGet _ n1 i1 u1, UGet _ n2 i2 u2) ->
      if n1 == n2 && i1 == i2 then
        assertEqual u1 u2
      else
        Nothing

    (UCase _ env1 u1 branches1, UCase _ env2 u2 branches2) ->
      if env1 /= env2 then
        Debug.log
          "WARN: env1 /= env2 for case statement in assertEqual"
          Nothing
      else if branches1 /= branches2 then
        Debug.log
          "WARN: branches1 /= branches2 for case statement in assertEqual"
          Nothing
      else
        assertEqual u1 u2

    _ ->
      if u1 == u2 then
        let
          _ = Debug.log "WARN: u1 == u2 case in assertEqual" ()
        in
          Just []
      else
        case expToExample u2 of
          Just ex2 ->
            backprop u1 ex2

          Nothing ->
            case expToExample u1 of
              Just ex1 ->
                backprop u2 ex1

              Nothing ->
                Nothing

--==============================================================================
--= Backpropagation
--==============================================================================

dontCareHole : Example
dontCareHole =
  ExConstructor "---dontCareHole---" (ExNum -3468801)

evalBackprop : U.Env -> Exp -> Example -> Maybe Constraints
evalBackprop env exp example =
  exp
    |> evalWithEnv env
    |> Result.toMaybe
    |> Maybe.andThen
         ( \(uResult, maybeEvalConstraints) ->
             maybeEvalConstraints |> Maybe.andThen (\evalConstraints ->
               Maybe.map
                 ((++) evalConstraints)
                 (backprop uResult example)
             )
         )

backprop : UnExp () -> Example -> Maybe Constraints
backprop u ex =
  if ex == dontCareHole then
    Just []
  else
    case (u, ex) of
      (UConstructor _ uIdent uInner, ExConstructor exIdent exInner) ->
        if uIdent == exIdent then
          backprop uInner exInner
        else
          Nothing

      (UNum _ uN, ExNum exN) ->
        if uN == exN then
          Just []
        else
          Nothing

      (UBool _ uB, ExBool exB) ->
        if uB == exB then
          Just []
        else
          Nothing

      (UString _ uS, ExString exS) ->
        if uS == exS then
          Just []
        else
          Nothing

      (UTuple _ uInners, ExTuple exInners) ->
        if List.length uInners == List.length exInners then
          exInners
            |> List.map2 backprop uInners
            |> Utils.projJusts
            |> Maybe.map List.concat
        else
          Nothing

      (UFunClosure _ recNames env params body, ExPartialFunction bindings) ->
        let
          backpropBinding (arguments, outputExample) =
            if List.length params == List.length arguments then
              evalBackprop
                (U.pairsToEnv (Utils.zip params arguments) ++ env)
                body
                outputExample
            else
              Nothing
        in
          bindings
            |> List.map backpropBinding
            |> Utils.projJusts
            |> Maybe.map List.concat

      (UHoleClosure _ env (i, j), _) ->
        Just [(i, (env, ex))]

      (UApp _ (UHoleClosure _ env (i, _)) uArgs, _) ->
        Just [(i, (env, ExPartialFunction [(uArgs, ex)]))]

      (UGet _ n i uArg, _) ->
        let
          exTuple =
            ExTuple <|
              List.repeat (i - 1) dontCareHole
                ++ [ex]
                ++ List.repeat (n - i) dontCareHole
        in
          backprop uArg exTuple

      (UCase _ env uScrutinee branches, _) ->
        branches
          |> List.map
               ( \(ctorName, argName, body) ->
                   ExConstructor ctorName dontCareHole
                     |> backprop uScrutinee
                     |> Maybe.map (\ks -> (ctorName, argName, body, ks))
               )
          |> Utils.firstMaybe
          |> Maybe.andThen
              ( \(ctorName, argName, body, ks) ->
                  let
                    newEnv =
                      U.addCtor ctorName argName uScrutinee env
                  in
                    evalBackprop newEnv body ex
              )

      _ ->
        Nothing
