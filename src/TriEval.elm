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
  { constraints : Result String Constraints
  , stackDepth : Int
  }

type alias UnExpEvaluator =
  Evaluator EvalState String (UnExp ())

--------------------------------------------------------------------------------
-- Evaluator Helper
--------------------------------------------------------------------------------

withConstraints : Result String Constraints -> UnExp () -> UnExpEvaluator
withConstraints ks u =
  let
    addConstraints oldState ks =
      { oldState | constraints =
          Result.map2 (++) oldState.constraints ks
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

apply : U.Env -> UnExp () -> List (UnExp ()) -> UnExpEvaluator
apply env head arguments =
  case arguments of
    [] ->
      Evaluator.succeed head

    firstArgument :: restArguments ->
      case head of
        UFunClosure _ functionEnv parameter body ->
          let
            newEnv =
              U.addVarBinding parameter firstArgument functionEnv

            increaseStackDepth oldState =
              { oldState | stackDepth = oldState.stackDepth + 1 }
          in
            Evaluator.do Evaluator.get <| \oldState ->
            Evaluator.do (Evaluator.put <| increaseStackDepth oldState) <| \_ ->
            Evaluator.do (eval_ newEnv body) <| \uBody ->
              apply newEnv uBody restArguments

        UPartialFunction _ partialFunction ->
          Utils.maybeFind firstArgument partialFunction
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
            |> Evaluator.andThen
                 (\uResult -> apply env uResult restArguments)

        UHoleClosure _ _ _ ->
          Evaluator.succeed <|
            List.foldl (UApp ()) head arguments

        UApp _ _ _ ->
          Evaluator.succeed <|
            List.foldl (UApp ()) head arguments

        _ ->
          Evaluator.fail
            "Not a proper application"

buildClosure : List Pat -> Exp -> Evaluator EvalState String (Ident, Exp)
buildClosure pats body =
  case pats of
    -- Impossible
    [] ->
      Evaluator.fail "Function with no parameters"

    headPat :: restPats ->
      let
        newBody =
          if List.isEmpty restPats then
            body
          else
            eFun restPats body
      in
        headPat
          |> identifierFromPat
          |> Result.fromMaybe "Non-identifier pattern in function"
          |> Result.map (\param -> (param, newBody))
          |> Evaluator.fromResult

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

          EConst _ num _ _ ->
            case Utils.intFromFloat num of
              Just n ->
                if n >= 0 then
                  Evaluator.succeed <|
                    Utils.iterate
                      n
                      (UConstructor () "S")
                      (UConstructor () "Z" (UTuple () []))
                else
                  Evaluator.fail "Negative integers not supported"

              Nothing ->
                Evaluator.fail "Floats not supported"

          EBase _ baseVal ->
            case baseVal of
              EBool b ->
                Evaluator.succeed <|
                  if b then
                    UConstructor () "T" (UTuple () [])
                  else
                    UConstructor () "F" (UTuple () [])

              EString _ s ->
                Evaluator.fail "Strings not supported"

              ENull ->
                Evaluator.fail "Null not supported"

          -- E-Lambda

          EFun _ pats body _ ->
            Evaluator.map
              (Utils.uncurry (UFunClosure () env))
              (buildClosure pats body)

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
                      "Variable not found: '" ++ x ++ "': " ++ U.unparseEnv env

          -- E-App

          EApp _ eFunction eArgs _ _ ->
            let
              default () =
                Evaluator.do (eval_ env eFunction) <| \uFunction ->
                Evaluator.do (Evaluator.mapM (eval_ env) eArgs) <| \uArgs ->
                apply env uFunction uArgs

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

                    UConstructorInverse _ _ _ ->
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
                        EFun _ pats body _ ->
                          Evaluator.map
                            ( \(param, newBody) ->
                                ( nonRecEnv
                                , (name, param, newBody) :: functionDefs
                                )
                            )
                            ( buildClosure pats body
                            )

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

          ESelect _ target _ _ selector ->
            case unwrapExp target of
              EVar _ "PBE" ->
                case Dict.get selector pbeActions of
                  Just argCount ->
                    let
                      head =
                        pbeName selector

                      argList =
                        pbeArgList argCount
                    in
                      case argList of
                        firstArg :: restArgs ->
                          Evaluator.succeed <|
                            UFunClosure () env firstArg <|
                              eFun (List.map pVar restArgs) <|
                                eApp (eVar head) (List.map eVar argList)

                        -- Impossible
                        [] ->
                          Evaluator.fail "PBE action with no arguments"

                  Nothing ->
                    Evaluator.fail <|
                      "Unknown PBE action '" ++ selector ++ "'"

              _ ->
                Evaluator.fail
                  "Arbitrary select not supported"

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

evalWithEnv :
  U.Env -> Exp -> Result String (UnExp (), Result String Constraints)
evalWithEnv env =
  eval_ env
    >> Evaluator.run { constraints = Ok [], stackDepth = 0 }
    >> Result.map (Tuple.mapFirst setHoleIndexes)
    >> Result.map (Tuple.mapSecond .constraints)

eval :
  Exp -> Result String (UnExp (), Result String Constraints)
eval =
  evalWithEnv []

ensureConstraintFree :
  Result String (UnExp (), Result String Constraints) -> Maybe (UnExp ())
ensureConstraintFree =
  Result.toMaybe
    >> Maybe.andThen
         (\(u, ks) -> if ks == Ok [] then Just u else Nothing)

--==============================================================================
--= Constraints
--==============================================================================

assertEqual : UnExp () -> UnExp () -> Result String Constraints
assertEqual u1 u2 =
  case (u1, u2) of
    (UConstructor _ ctorName1 uInner1, UConstructor _ ctorName2 uInner2) ->
      if ctorName1 == ctorName2 then
        assertEqual uInner1 uInner2
      else
        Err <|
          "Constructor names do not match: '"
            ++ ctorName1 ++ "' and '" ++ ctorName2 ++ "'"

    (UTuple _ us1, UTuple _ us2) ->
      let
        us1Length =
          List.length us1

        us2Length =
          List.length us2
      in
        if us1Length == us2Length then
          List.map2 assertEqual us1 us2
            |> Utils.projOk
            |> Result.map List.concat
        else
          Err <|
            "Tuple lengths do not match: "
              ++ toString us1Length ++ " and " ++ toString us2Length

    (UHoleClosure _ env (holeId, _), _) ->
      Result.map (\ex -> [(holeId, (env, ex))]) <|
        Result.fromMaybe
          ( "Right-hand side "
              ++ U.unExpName u2
              ++ " expression could not be converted to example"
          )
          ( expToExample u2
          )

    (_, UHoleClosure _ env (holeId, _)) ->
      Result.map (\ex -> [(holeId, (env, ex))]) <|
        Result.fromMaybe
          ( "Left-hand side "
              ++ U.unExpName u1
              ++ " expression could not be converted to example"
          )
          ( expToExample u1
          )

    (UApp _ u1 u1Arg, UApp _ u2 u2Arg) ->
      let
        funcConstraints =
          assertEqual u1 u2

        argConstraints =
          assertEqual u1Arg u2Arg
      in
        Result.map2 (++) funcConstraints argConstraints

    (UGet _ n1 i1 u1, UGet _ n2 i2 u2) ->
      if n1 == n2 && i1 == i2 then
        assertEqual u1 u2
      else
        Err <|
          "Tuple projections do not match: ("
            ++ toString n1 ++ "_" ++ toString i1
            ++ ") and ("
            ++ toString n2 ++ "_" ++ toString i2
            ++ ")"

    (UCase _ env1 u1 branches1, UCase _ env2 u2 branches2) ->
      if env1 /= env2 then
        Debug.log
          "WARN: env1 /= env2 for case statement in assertEqual"
          (Err "Case environments do not match")
      else if branches1 /= branches2 then
        Debug.log
          "WARN: branches1 /= branches2 for case statement in assertEqual"
          (Err "Case branches do not match")
      else
        assertEqual u1 u2

    _ ->
      if u1 == u2 then
        let
          _ = Debug.log "WARN: u1 == u2 case in assertEqual" ()
        in
          Ok []
      else
        case expToExample u2 of
          Just ex2 ->
            backprop u1 ex2

          Nothing ->
            case expToExample u1 of
              Just ex1 ->
                backprop u2 ex1

              Nothing ->
                Err "Incompatible examples"

--==============================================================================
--= Backpropagation
--==============================================================================

evalBackprop : U.Env -> Exp -> Example -> Result String Constraints
evalBackprop env exp example =
  exp
    |> evalWithEnv env
    |> Result.andThen
         ( \(uResult, resultEvalConstraints) ->
             resultEvalConstraints |> Result.andThen (\evalConstraints ->
               Result.map
                 ((++) evalConstraints)
                 (backprop uResult example)
             )
         )

backprop : UnExp () -> Example -> Result String Constraints
backprop u ex =
  if ex == ExDontCare then
    Ok []
  else
    case (u, ex) of
      (UConstructor _ uIdent uInner, ExConstructor exIdent exInner) ->
        if uIdent == exIdent then
          backprop uInner exInner
        else
          Err <|
            "Constructor names do not match: '"
              ++ uIdent ++ "' and '" ++ exIdent ++ "'"

      (UTuple _ uInners, ExTuple exInners) ->
        let
          uLength =
            List.length uInners

          exLength =
            List.length exInners
        in
          if List.length uInners == List.length exInners then
            exInners
              |> List.map2 backprop uInners
              |> Utils.projOk
              |> Result.map List.concat
          else
            Err <|
              "Tuple lengths do not match: "
                ++ toString uLength ++ " and " ++ toString exLength

      (UFunClosure _ env param body, ExPartialFunction bindings) ->
        let
          backpropBinding (argument, outputExample) =
            evalBackprop
              (U.addVarBinding param argument env)
              body
              outputExample
        in
          bindings
            |> List.map backpropBinding
            |> Utils.projOk
            |> Result.map List.concat

      (UHoleClosure _ env (i, j), _) ->
        Ok [(i, (env, ex))]

      (UApp _ uHead uArg, _) ->
        let
          exHead =
            ExPartialFunction [(uArg, ex)]
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
          tryBranch :
            UnExp () -> (Ident, Ident, Exp) -> Result String Constraints
          tryBranch uScrutinee (ctorName, argName, body) =
            -- Cannot be applicative because we only want to continue if the
            -- first computation succeeds
            flip Result.andThen
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
                      |> Result.map (\ks2 -> ks1 ++ ks2)
              )
        in
          branches
            |> List.map (tryBranch uScrutinee)
            |> Utils.firstOk "Could not find suitable branch in case statement"

      _ ->
        Err <|
          "Cannot backpropagate "
            ++ U.exampleName ex
            ++ " example into "
            ++ U.unExpName u
            ++ " expression."
