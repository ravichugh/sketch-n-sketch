--------------------------------------------------------------------------------
-- This module contains all the code for dynamic handling of UnExps, including:
--   - Evaluation
--   - Backpropagation
--   - Example collection (constraint collection)
--------------------------------------------------------------------------------
module TriEval exposing
  ( setNoData
  , setHoleIndexes
  , evalWithEnv
  , eval
  , ensureConstraintFree
  , backprop_
  , backprop
  )

import Dict exposing (Dict)
import Char

import Utils

import Evaluator exposing (Evaluator)
import State exposing (State)
import NonDet exposing (NonDet)

import UnLang as U exposing (..)

import Lang exposing (..)
import Types2 as T

--==============================================================================
--= Evaluation
--==============================================================================

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

maxBackpropDepth : Int
maxBackpropDepth =
  7

maxStackDepth : Int
maxStackDepth =
  100

type alias Data =
  { bindSpec : Maybe T.BindingSpecification
  }

-- "no data"
nd : Data
nd =
  { bindSpec =
      Nothing
  }

setNoData : UnExp d -> UnExp Data
setNoData =
  U.mapData (\_ -> nd)

type alias EvalState =
  { constraints : NonDet Constraints
  , stackDepth : Int
  }

type alias UnExpEvaluator =
  Evaluator EvalState String (UnExp Data)

--------------------------------------------------------------------------------
-- Evaluator Helper
--------------------------------------------------------------------------------

withConstraints : NonDet Constraints -> UnExp Data -> UnExpEvaluator
withConstraints ks u =
  let
    addConstraints oldState =
      { oldState | constraints =
          NonDet.do oldState.constraints <| \old ->
          NonDet.pureDo ks <| \new ->
            old ++ new
      }
  in
    Evaluator.do Evaluator.get <| \oldState ->
    Evaluator.do (Evaluator.put <| addConstraints oldState) <| \_ ->
    Evaluator.succeed u

--------------------------------------------------------------------------------
-- Core Evaluation
--------------------------------------------------------------------------------

apply : U.Env -> UnExp Data -> List (UnExp Data) -> UnExpEvaluator
apply env head arguments =
  case arguments of
    [] ->
      Evaluator.succeed head

    firstArgument :: restArguments ->
      case head of
        UFunClosure { bindSpec } functionEnv maybeName parameter body ->
          let
            argBindSpec =
              firstArgument
                |> getData
                |> .bindSpec
          in
            if
              True
              -- T.structurallyDecreasingBindSpec
              --   { parent = bindSpec, child = argBindSpec }
            then
              let
                newEnv =
                  U.addVarBinding
                    parameter
                    (firstArgument, Maybe.map (T.Arg << pVar0) maybeName)
                    functionEnv

                increaseStackDepth oldState =
                  { oldState | stackDepth = oldState.stackDepth + 1 }
              in
                Evaluator.do Evaluator.get <|
                  \oldState ->
                Evaluator.do (Evaluator.put <| increaseStackDepth oldState) <|
                  \_ ->
                Evaluator.do (eval_ newEnv body) <|
                  \uBody ->
                    apply newEnv uBody restArguments
            else
              Evaluator.fail "Non-structural recursion"

        UPartialFunction _ partialFunction ->
          Utils.maybeFind (U.clearData firstArgument) partialFunction
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
            |> Evaluator.map setNoData
            |> Evaluator.andThen
                 (\uResult -> apply env uResult restArguments)

        UHoleClosure _ _ _ ->
          Evaluator.succeed <|
            List.foldl (UApp nd) head arguments

        UApp _ _ _ ->
          Evaluator.succeed <|
            List.foldl (UApp nd) head arguments

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
                      (UConstructor nd "S")
                      (UConstructor nd "Z" (UTuple nd []))
                else
                  Evaluator.fail "Negative integers not supported"

              Nothing ->
                Evaluator.fail "Floats not supported"

          EBase _ baseVal ->
            case baseVal of
              EBool b ->
                Evaluator.succeed <|
                  if b then
                    UConstructor nd "T" (UTuple nd [])
                  else
                    UConstructor nd "F" (UTuple nd [])

              EString _ s ->
                Evaluator.fail "Strings not supported"

              ENull ->
                Evaluator.fail "Null not supported"

          -- E-Lambda

          EFun _ pats body _ ->
            Evaluator.map
              (Utils.uncurry (UFunClosure nd env Nothing))
              (buildClosure pats body)

          -- E-Var

          EVar _ varName ->
            -- Library functions
            case varName of
              -- assert (x == y) =
              --   _constrain x y
              "assert" ->
                Evaluator.succeed <|
                  UFunClosure nd env Nothing "x" <|
                    eApp
                      (eVar "_constrain")
                      [ eApp (eVar "get_2_1") [eVar "x"]
                      , eApp (eVar "get_2_2") [eVar "x"]
                      ]

              -- specifyFunction func ioList =
              --   let
              --     _ =
              --       _map
              --         ( \pair ->
              --             _constrain
              --               (func (get_2_1 pair))
              --               (get_2_2 pair)
              --         )
              --         ioList
              --   in
              --     ()
              "specifyFunction" ->
                Evaluator.succeed <|
                  UFunClosure nd env Nothing "func" <|
                    eFun [pVar "ioList"] <|
                      eLet
                        [ ( "_"
                          , eApp
                              (eVar "_map")
                              [ eFun [pVar "pair"] <|
                                  eApp
                                    (eVar "_constrain")
                                    [ eApp
                                        (eVar "func")
                                        [eApp (eVar "get_2_1") [eVar "pair"]]
                                    , eApp (eVar "get_2_2") [eVar "pair"]
                                    ]
                              , eVar "ioList"
                              ]
                          )
                        ]
                        (eTuple [])

              -- specifyFunction2 func ioList =
              --   let
              --     _ =
              --       _map
              --         ( \triple ->
              --             _constrain
              --               (func (get_3_1 pair) (get_3_2 triple))
              --               (get_3_3 pair)
              --         )
              --         ioList
              --   in
              --     ()
              "specifyFunction2" ->
                Evaluator.succeed <|
                  UFunClosure nd env Nothing "func" <|
                    eFun [pVar "ioList"] <|
                      eLet
                        [ ( "_"
                          , eApp
                              (eVar "_map")
                              [ eFun [pVar "triple"] <|
                                  eApp
                                    (eVar "_constrain")
                                    [ eApp
                                        (eVar "func")
                                        [ eApp (eVar "get_3_1") [eVar "triple"]
                                        , eApp (eVar "get_3_2") [eVar "triple"]
                                        ]
                                    , eApp (eVar "get_3_3") [eVar "triple"]
                                    ]
                              , eVar "ioList"
                              ]
                          )
                        ]
                        (eTuple [])

              -- defineHole hole exp =
              --   let _ = _constrain hole exp in hole
              "defineHole" ->
                Evaluator.succeed <|
                  UFunClosure nd env Nothing "hole" <|
                    eFun [pVar "exp"] <|
                      eLet
                        [ ( "_"
                          , eApp
                              (eVar "_constrain")
                              [ eVar "hole"
                              , eVar "exp"
                              ]
                          )
                        ]
                        (eVar "hole")

              _ ->
                case U.lookupVar varName env of
                  Just (u, bindSpec) ->
                    Evaluator.succeed <|
                      U.setData { bindSpec = bindSpec } u

                  Nothing ->
                    case U.lookupRecursiveFunction varName env of
                      Just
                        ( functionEnv
                        , functionDefs
                        , (_, param, body)
                        , bindSpec
                        ) ->
                          let
                            newFunctionEnv =
                              U.addRecursiveBinding
                                ( functionEnv
                                , functionDefs
                                , Just <|
                                    T.Rec
                                      ( List.map
                                          (Utils.fst3 >> pVar0)
                                          functionDefs
                                      )
                                )
                                functionEnv
                          in
                            Evaluator.succeed <|
                              UFunClosure
                                { bindSpec = bindSpec }
                                newFunctionEnv
                                ( Just varName )
                                param
                                body

                      Nothing ->
                        Evaluator.fail <|
                          "Variable not found: '"
                            ++ varName
                            ++ "': "
                            ++ U.unparseEnv env

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
                    UTuple { bindSpec } tupleArgs ->
                      case Utils.maybeGeti1 i tupleArgs of
                        Just returnValue ->
                          Evaluator.succeed <|
                            U.setData
                              { bindSpec =
                                  Maybe.andThen T.subBindSpec bindSpec
                              }
                              returnValue

                        Nothing ->
                          Evaluator.fail
                            "Out of bounds index for 'get'"

                    UHoleClosure _ _ _ ->
                      Evaluator.succeed <|
                        UGet nd n i uArg

                    UConstructorInverse _ _ _ ->
                      Evaluator.succeed <|
                        UGet nd n i uArg

                    _ ->
                      Evaluator.fail
                        "Not a proper 'get'"
                )

              evalConstrain e1 e2 =
                eval_ env e1 |> Evaluator.andThen (\u1 ->
                  eval_ env e2 |> Evaluator.andThen (\u2 ->
                    withConstraints
                      (constrain u1 u2)
                      (UTuple nd [])
                  )
                )

              evalMap eMapF eMapList =
                let
                  consMap uFunc uList =
                    case uList of
                      UConstructor _ "Nil" (UTuple _ []) ->
                        Evaluator.succeed uList

                      UConstructor _ "Cons" (UTuple _ [uHead, uRest]) ->
                        Evaluator.do (apply env uFunc [uHead]) <| \uNewHead ->
                        Evaluator.do (consMap uFunc uRest) <| \uNewRest ->
                        Evaluator.succeed <|
                          UConstructor nd
                            "Cons"
                            (UTuple nd [uNewHead, uNewRest])

                      _ ->
                        Evaluator.fail
                          "_map applied to non-list"
                in
                  Evaluator.do (eval_ env eMapF) <| \uFunc ->
                  Evaluator.do (eval_ env eMapList) <| \uList ->
                    consMap uFunc uList
            in
              case Lang.toTupleGet exp of
                Just tupleGet ->
                  evalGet tupleGet

                Nothing ->
                  case unwrapExp eFunction of
                    -- Built-ins
                    EVar _ action ->
                      case (action, eArgs) of
                        ("_constrain", [e1, e2]) ->
                          evalConstrain e1 e2

                        ("_map", [eMapF, eMapList]) ->
                          evalMap eMapF eMapList

                        _ ->
                          default ()

                    _ ->
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

              evalBranch scrutineeBindSpec uArg (_, argName, body) =
                let
                  newEnv =
                    if argName == noBindingName then
                      env
                    else
                      U.addVarBinding
                        argName
                        (uArg, Maybe.andThen T.subBindSpec scrutineeBindSpec)
                        env
                in
                  eval_ newEnv body
            in
              Evaluator.do (Evaluator.mapM toUBranch branches) <| \uBranches ->
              Evaluator.do (eval_ env e0) <| \u0 ->
                case u0 of
                  UConstructor _ ctorName uArg ->
                    let
                      scrutineeBindSpec =
                        (getData u0).bindSpec
                    in
                      uBranches
                        |> Utils.findFirst (\(c, _, _) -> c == ctorName)
                        |> Result.fromMaybe
                             ( "Non-exhaustive pattern match, could not find '"
                                 ++ ctorName
                                 ++ "'"
                             )
                        |> Evaluator.fromResult
                        |> Evaluator.andThen (evalBranch scrutineeBindSpec uArg)

                  _ ->
                    Evaluator.succeed (UCase nd env u0 uBranches)

          -- E-Hole

          EHole _ hole  ->
            case hole of
              EEmptyHole holeId ->
                Evaluator.succeed <|
                  UHoleClosure nd env (holeId, -1)

              _ ->
                Evaluator.fail
                  "Unsupported hole type"

          -- Misc.

          EOp _ _ op args _ ->
            case (op.val, args) of
              -- Used for assert: a == b ===> (a, b)
              (Eq, [e1, e2]) ->
                Evaluator.do (eval_ env e1) <| \u1 ->
                Evaluator.do (eval_ env e2) <| \u2 ->
                Evaluator.succeed <|
                  UTuple nd [u1, u2]

              _ ->
                Evaluator.fail
                  "Arbitrary operator not supported"

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
                          Evaluator.do (eval_ nonRecEnv binding) <| \u ->
                            Evaluator.succeed
                              ( U.addVarBinding name (u, Nothing) nonRecEnv
                              , functionDefs
                              )
                in
                  nameBindingPairs
                    |> Evaluator.foldlM addToEnv (env, [])
                    |> Evaluator.map
                         ( \(nonRecEnv, functionDefs) ->
                             U.addRecursiveBinding
                               (nonRecEnv, functionDefs, Nothing)
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
                      |> Evaluator.map (UTuple nd)

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
                                        UPartialFunction nd pf

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
                                (UConstructor nd ctorName)
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

additionalPipeline : UnExp Data -> UnExp ()
additionalPipeline =
  U.clearData >> setHoleIndexes

--------------------------------------------------------------------------------
-- Full Evaluation
--------------------------------------------------------------------------------

evalWithEnv :
  U.Env -> Exp -> Result String (UnExp (), NonDet Constraints)
evalWithEnv env =
  eval_ env
    >> Evaluator.run { constraints = NonDet.pure [], stackDepth = 0 }
    >> Result.map (Tuple.mapFirst additionalPipeline)
    >> Result.map (Tuple.mapSecond .constraints)

eval :
  Exp -> Result String (UnExp (), NonDet Constraints)
eval =
  evalWithEnv []

ensureConstraintFree :
  Result String (UnExp (), NonDet Constraints) -> Maybe (UnExp ())
ensureConstraintFree evalResult =
  case evalResult of
    Err _ ->
      Nothing

    Ok (u, possibleConstraintResults) ->
      if
        possibleConstraintResults
          |> NonDet.toList
          |> Utils.all1 List.isEmpty
      then
        Just u
      else
        Nothing

--==============================================================================
--= Constraints
--==============================================================================

constrain : UnExp Data -> UnExp Data -> NonDet Constraints
constrain u1 u2 =
  case (u1, u2) of
    (UConstructor _ ctorName1 uInner1, UConstructor _ ctorName2 uInner2) ->
      if ctorName1 == ctorName2 then
        constrain uInner1 uInner2
      else
        NonDet.none

    (UTuple _ us1, UTuple _ us2) ->
      let
        us1Length =
          List.length us1

        us2Length =
          List.length us2
      in
        if us1Length == us2Length then
          List.map2 constrain us1 us2
            |> NonDet.oneOfEach
            |> NonDet.map List.concat
        else
          NonDet.none

    _ ->
      if u1 == u2 then
        let
          _ = Debug.log "WARN: u1 == u2 case in constrain" ()
        in
          NonDet.pure []
      else
        case expToExample u2 of
          Just ex2 ->
            backprop u1 ex2

          Nothing ->
            case expToExample u1 of
              Just ex1 ->
                backprop u2 ex1

              Nothing ->
                NonDet.none

--==============================================================================
--= Backpropagation
--==============================================================================

evalBackprop :
  Int
    -> { allowEvaluationConstraints : Bool, compareExp : Maybe (UnExp ()) }
    -> U.Env -> Exp -> Example -> NonDet Constraints
evalBackprop depth { allowEvaluationConstraints, compareExp } env exp example =
  let
    postPass1 =
      if allowEvaluationConstraints then
        Result.toMaybe
      else
        ensureConstraintFree >> Maybe.map (\u -> (u, NonDet.pure []))

    postPass2 ((uResult, _) as input) =
      case compareExp of
        Just uCompare ->
          if U.equalModuloEnv uCompare uResult then
            Nothing
          else
            Just input

        Nothing ->
          Just input

    postPass =
      postPass1 >> Maybe.andThen postPass2
  in
    case evalWithEnv env exp |> postPass of
      Just (uResult, possibleEvalConstraints) ->
        NonDet.map List.concat <|
          NonDet.oneOfEach
            [ possibleEvalConstraints
            , backprop_ (depth + 1) (setNoData uResult) example
            ]

      Nothing ->
        NonDet.none

backprop_ : Int -> UnExp Data -> Example -> NonDet Constraints
backprop_ depth u ex =
  if depth > maxBackpropDepth then
    NonDet.none
  else if ex == ExDontCare then
    NonDet.pure []
  else
    case (u, ex) of
      (UConstructor _ uIdent uInner, ExConstructor exIdent exInner) ->
        if uIdent == exIdent then
          backprop_ depth uInner exInner
        else
          NonDet.none

      (UTuple _ uInners, ExTuple exInners) ->
        let
          uLength =
            List.length uInners

          exLength =
            List.length exInners
        in
          if List.length uInners == List.length exInners then
            List.map2 (backprop_ depth) uInners exInners
              |> NonDet.oneOfEach
              |> NonDet.map List.concat
          else
            NonDet.none

      ( UFunClosure { bindSpec } env maybeName param body
      , ExPartialFunction bindings
      ) ->
        let
          backpropBinding (argument, outputExample) =
            evalBackprop
              depth
              { allowEvaluationConstraints = False
              , compareExp = Nothing
              }
              ( U.addVarBinding
                  param
                  (argument, Maybe.map (T.Arg << pVar0) maybeName)
                  env
              )
              body
              outputExample
        in
          bindings
            |> List.map backpropBinding
            |> NonDet.oneOfEach
            |> NonDet.map List.concat

      (UHoleClosure _ env (i, _), _) ->
        NonDet.pure [(i, (env, ex))]

      (UApp _ uHead uArg, _) ->
        let
          exHead =
            ExPartialFunction [(U.clearData uArg, ex)]
        in
          backprop_ depth uHead exHead

      (UGet _ n i uArg, _) ->
        let
          exTuple =
            ExTuple <|
              List.repeat (i - 1) ExDontCare
                ++ [ex]
                ++ List.repeat (n - i) ExDontCare
        in
          backprop_ depth uArg exTuple

      (UConstructorInverse _ ident uArg, _) ->
        backprop_ depth uArg (ExConstructor ident ex)

      (UCase _ env uScrutinee branches, _) ->
        let
          tryBranch : UnExp Data -> (Ident, Ident, Exp) -> NonDet Constraints
          tryBranch uScrutinee (ctorName, argName, body) =
            -- Cannot be applicative because we only want to continue if the
            -- first computation succeeds
            NonDet.do
              ( backprop_ depth uScrutinee (ExConstructor ctorName ExDontCare)
              )
              ( \k1 ->
                  let
                    scrutineeBindSpec =
                      (getData uScrutinee).bindSpec

                    newEnv =
                      U.addVarBinding
                        argName
                        ( UConstructorInverse nd ctorName uScrutinee
                        , Maybe.andThen T.subBindSpec scrutineeBindSpec
                        )
                        env
                  in
                    NonDet.map (\k23 -> k1 ++ k23) <|
                      evalBackprop
                        depth
                        { allowEvaluationConstraints = False
                        , compareExp = Just (clearData u)
                        }
                        newEnv
                        body
                        ex
              )
        in
          branches
            |> NonDet.fromList
            |> NonDet.andThen (tryBranch uScrutinee)

      _ ->
        NonDet.none

backprop : UnExp Data -> Example -> NonDet Constraints
backprop =
  backprop_ 0
