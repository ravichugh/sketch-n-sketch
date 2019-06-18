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

maxStackDepth : Int
maxStackDepth =
  100

type alias EvalState =
  { constraints : Maybe Constraints
  , stackDepth : Int
  }

type alias UnExpEvaluator =
  Evaluator EvalState String (UnExp ())

--------------------------------------------------------------------------------
-- Evaluator Helper
--------------------------------------------------------------------------------

withConstraints : Maybe Constraints -> UnExp () -> UnExpEvaluator
withConstraints ks u =
  let
    addConstraints oldState ks =
      { oldState | constraints =
          Maybe.map2 (++) oldState.constraints ks
      }
  in
    Evaluator.do Evaluator.get <| \oldState ->
    Evaluator.do (Evaluator.put <| addConstraints oldState ks) <| \_ ->
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

apply :
  U.Env -> Exp -> List Ident -> List Exp -> List (UnExp ()) -> UnExpEvaluator
apply currentEnv body parameters eArguments arguments =
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

    increaseStackDepth oldState =
      { oldState | stackDepth = oldState.stackDepth + 1 }
  in
    if argLength < paramLength then
      -- Partially applied function
      Evaluator.succeed <|
        UFunClosure () newEnv (List.drop argLength parameters) body
    else
      Evaluator.do Evaluator.get <| \oldState ->
      Evaluator.do (Evaluator.put <| increaseStackDepth oldState) <| \_ ->
      Evaluator.do (eval_ newEnv body) <| \uBody ->
        if argLength == paramLength then
          -- Fully applied function
          Evaluator.succeed <|
            uBody
        else -- argLength > paramLength
          -- Applied too many arguments; try to evaluate a further application.
          -- (Would ideally use "resume" here; re-wrapping in an Exp is a
          -- workaround.)
          eval_ currentEnv <|
            eApp body (List.drop paramLength eArguments)

eval_ : U.Env -> Exp -> UnExpEvaluator
eval_ env exp =
  Evaluator.do Evaluator.get <| \oldState ->
    if oldState.stackDepth > maxStackDepth then
      Evaluator.fail
        "Maximum stack depth exceeded"
    else
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
                case U.lookupRecursiveFunction x env of
                  Just (functionEnv, functionDefs, (_, params, body)) ->
                    let
                      newFunctionEnv =
                        U.addRecursiveBinding
                          (functionEnv, functionDefs)
                          functionEnv
                    in
                      Evaluator.succeed <|
                        UFunClosure () newFunctionEnv params body

                  Nothing ->
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
                      UFunClosure _ functionEnv parameters body ->
                        Evaluator.andThen
                          (apply functionEnv body parameters eArgs)
                          uArgsEvaluation

                      UHoleClosure _ _ _ ->
                        Evaluator.map (UApp () uFunction) uArgsEvaluation

                      UPartialFunction _ partialFunction ->
                        uArgsEvaluation |> Evaluator.andThen (\uArgs ->
                          Utils.maybeFind uArgs partialFunction
                            |> Result.fromMaybe
                                 ( "Partial function applied to expression not"
                                     ++ " in domain"
                                 )
                            |> Result.andThen
                                 ( U.exampleToExp
                                     >> Result.fromMaybe
                                          "Partial function returned ?? example"
                                 )
                            |> Evaluator.fromResult
                        )

                      UApp _ head appliedArgs ->
                        uArgsEvaluation |> Evaluator.map (\newArgs ->
                          UApp () head (appliedArgs ++ newArgs)
                        )

                      _ ->
                        Evaluator.fail
                          "Not a proper application"
                  )

              evalGet (n, i, arg) =
                eval_ env arg |> Evaluator.andThen (\uArg ->
                  case uArg of
                    UTuple _ tupleArgs ->
                      case Utils.maybeGeti1 i tupleArgs of
                        Just returnValue ->
                          Evaluator.succeed returnValue

                        Nothing ->
                          Evaluator.fail
                            "Out of bounds index for 'get'"

                    UHoleClosure _ _ _ ->
                      Evaluator.succeed <|
                        UGet () n i uArg

                    _ ->
                      Evaluator.fail
                        "Not a proper 'get'"
                )

              evalConstrain e1 e2 =
                eval_ env e1 |> Evaluator.andThen (\u1 ->
                  eval_ env e2 |> Evaluator.andThen (\u2 ->
                    withConstraints
                      (assertEqual u1 u2)
                      (UTuple () [])
                  )
                )

              evalDefineHole e1 e2 =
                -- Desugars to:
                --   let _ = PBE.constrain e1 e2 in e1
                eval_ env <|
                  eLet
                    [ ( "_"
                      , eApp
                          (eSelect (eVar "PBE") "constrain")
                          [e1, e2]
                      )
                    ]
                    e1
            in
              case Lang.toTupleGet exp of
                Just tupleGet ->
                  evalGet tupleGet

                Nothing ->
                  case Lang.pbeAction exp of
                    Just (Constrain e1 e2) ->
                      evalConstrain e1 e2

                    Just (DefineHole e1 e2) ->
                      evalDefineHole e1 e2

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
                                Evaluator.fail <|
                                  "Multiple arguments in constructor pattern"
                                    ++ " match"

                          Nothing ->
                            Evaluator.fail
                              "Non-constructor pattern match"

                      _ ->
                        Evaluator.fail
                          "Non-record (constructor sugar) pattern match"

              evalBranch uArg (_, argName, body) =
                let
                  newEnv =
                    if argName == noBindingName then
                      env
                    else
                      U.addVarBinding argName uArg env
                in
                  eval_ newEnv body
            in
              Evaluator.do (Evaluator.mapM toUBranch branches) <| \uBranches ->
              Evaluator.do (eval_ env e0) <| \u0 ->
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

          -- E-Hole

          EHole _ hole  ->
            case hole of
              EEmptyHole holeId ->
                Evaluator.succeed <|
                  UHoleClosure () env (holeId, -1)

              _ ->
                Evaluator.fail
                  "Unsupported hole type"

          -- Misc.

          EOp _ _ op args _ ->
            Evaluator.fail
              "Op not supported"

          EList _ args _ _ _ ->
            args
              |> List.map Tuple.second
              |> List.foldr
                   ( \element list ->
                       eDatatype "Cons" [eTuple [element, list]]
                   )
                   ( eDatatype "Nil" [eTuple []]
                   )
              |> eval_ env

          EIf _ condition _ trueBranch _ falseBranch _ ->
            Evaluator.fail
              "If not supported"

          ELet _ _ decls _ body ->
            case recordEntriesFromDeclarations decls of
              Just entries ->
                let
                  nameBindingPairs =
                    List.map
                      (\(_, _, ident, _, exp) -> (ident, exp))
                      entries

                  (names, _) =
                    List.unzip nameBindingPairs

                  nameLen =
                    List.length names

                  addToEnv (name, binding) (nonRecEnv, functionDefs) =
                    let
                      bindingEvaluation =
                        eval_ nonRecEnv binding
                    in
                      Evaluator.do (eval_ nonRecEnv binding) <| \u ->
                      case unwrapExp binding of
                        -- Syntactic lambda; should be treated as
                        -- recursive function
                        EFun _ paramPats body _ ->
                          case
                            paramPats
                              |> List.map identifierFromPat
                              |> Utils.projJusts
                          of
                            Just params ->
                              Evaluator.succeed
                                ( nonRecEnv
                                , (name, params, body) :: functionDefs
                                )

                            Nothing ->
                              Evaluator.fail <|
                                "Non-identifier pattern in function (let"
                                  ++ " binding)"

                        _ ->
                          Evaluator.succeed
                            ( U.addVarBinding name u nonRecEnv
                            , functionDefs
                            )
                in
                  nameBindingPairs
                    |> Evaluator.foldlM addToEnv (env, [])
                    |> Evaluator.map
                         ( \(nonRecEnv, functionDefs) ->
                             U.addRecursiveBinding
                               (nonRecEnv, functionDefs)
                               nonRecEnv
                         )
                    |> Evaluator.andThen
                         ( \envExtension ->
                             eval_ (envExtension ++ env) body
                         )

              Nothing ->
                Evaluator.fail
                  "Could not get record entries from let"

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
                                      Evaluator.fail
                                        "PF applied to non-pf string"

                                Nothing ->
                                  Evaluator.fail
                                    "PF applied to non-string"

                            _ ->
                              Evaluator.fail
                                "PF applied to more than one argument"
                        else
                          case args of
                            -- -- Syntactic sugar for applying to unit
                            -- [] ->
                            --   Evaluator.succeed <|
                            --     UConstructor () ctorName (UTuple () [])

                            [arg] ->
                              Evaluator.map
                                (UConstructor () ctorName)
                                (eval_ env arg)

                            _ ->
                              Evaluator.fail <|
                                "Constructor applied to not exactly one"
                                  ++ " argument"

                      _ ->
                        Evaluator.fail
                          "Arbitrary records not supported"

              Nothing ->
                Evaluator.fail
                  "Could not get record entries"

          ESelect _ _ _ _ _ ->
            Evaluator.fail
              "Select not supported"

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
    >> Evaluator.run { constraints = Just [], stackDepth = 0 }
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
  if ex == ExDontCare then
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

      (UFunClosure _ env params body, ExPartialFunction bindings) ->
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

      (UApp _ uHead uArgs, _) ->
        let
          exHead =
            List.foldr
              (\uArg accExHead -> ExPartialFunction [([uArg], accExHead)])
              ex
              uArgs
        in
          backprop uHead exHead

      (UGet _ n i uArg, _) ->
        let
          exTuple =
            ExTuple <|
              List.repeat (i - 1) ExDontCare
                ++ [ex]
                ++ List.repeat (n - i) ExDontCare
        in
          backprop uArg exTuple

      (UConstructorInverse _ ident uArg, _) ->
        backprop uArg (ExConstructor ident ex)

      (UCase _ env uScrutinee branches, _) ->
        let
          tryBranch : UnExp () -> (Ident, Ident, Exp) -> Maybe Constraints
          tryBranch uScrutinee (ctorName, argName, body) =
            -- Cannot be applicative because we only want to continue if the
            -- first computation succeeds
            flip Maybe.andThen
              ( backprop uScrutinee (ExConstructor ctorName ExDontCare)
              )
              ( \ks1 ->
                  let
                    newEnv =
                      U.addVarBinding
                        argName
                        (UConstructorInverse () ctorName uScrutinee)
                        env
                  in
                    evalBackprop newEnv body ex
                      |> Maybe.map (\ks2 -> ks1 ++ ks2)
              )
        in
          branches
            |> List.map (tryBranch uScrutinee)
            |> Utils.firstMaybe

      _ ->
        Nothing
