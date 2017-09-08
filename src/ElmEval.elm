module ElmEval exposing
  ( eval
  , showError
  )

import Dict exposing (Dict)

import Utils
import Range exposing (Ranged)

import ElmLang exposing (..)

--==============================================================================
--= Data Structures
--==============================================================================

--------------------------------------------------------------------------------
-- Environment
--------------------------------------------------------------------------------

-- If a binding is Nothing, then it the variable exists in scope but isn't
-- assigned yet via function application (currying).
type alias Binding =
  Maybe ETerm

type alias Environment =
  Dict Identifier (Maybe ETerm)

-- Extend an environment with new patterns. All the previous variables are kept
-- in scope, but preference is given to the new bindings (shadowing).
extendEnvironment : Environment -> List PTerm -> Environment
extendEnvironment previousEnvironment pterms =
  pterms
    |> List.concatMap (.pattern >> getIdentifiers)
    |> List.map (flip (,) Nothing)
    |> Dict.fromList
    |> flip Dict.union previousEnvironment

updateEnvironment : Environment -> List (PTerm, ETerm) -> Environment
updateEnvironment =
  List.foldl <|
    \(pterm, eterm) environment ->
      let
        identifiers =
          getIdentifiers pterm.pattern
      in
        List.foldl
          (flip Dict.insert (Just eterm))
          environment
          identifiers

--------------------------------------------------------------------------------
-- Evaluation Context
--------------------------------------------------------------------------------

type alias Context =
  { environment : Environment
  }

--------------------------------------------------------------------------------
-- Evaluation Output
--------------------------------------------------------------------------------

type EvalErrorType
  = ConditionNotBool
  | NotAFunction
  | NoSuchVariable Identifier
  | TooManyArguments
  | UnimplError

type alias EvalError =
  Ranged
    { error : EvalErrorType
    , context : Context
    }

type alias Output =
  { value : Result (List EvalError) ETerm
  }

evalError : Context -> EvalErrorType -> Ranged a -> EvalError
evalError context error { start, end } =
  { start = start
  , end = end
  , error = error
  , context = context
  }

showError : String -> EvalError -> String
showError source { start, end, error, context } =
  let
    relevantLines =
      source
        |> String.lines
        |> List.take end.row
        |> List.drop (start.row - 1)
    prettyError =
      String.join "\n" relevantLines
    prettyEnvironment =
      context.environment
        |> Dict.toList
        |> List.map
             ( \(identifier, binding) ->
                 "\n" ++ identifier ++ ":\n  " ++ toString binding
             )
        |> String.join "\n"
  in
    "[Evaluator Error]\n\n" ++
      (toString error) ++ ":\n\n" ++
      prettyError ++ "\n\n" ++
    "Environment\n" ++
    "===========\n" ++
      prettyEnvironment ++ "\n\n" ++
    "Position\n" ++
    "========\n" ++
    "  Start Row: " ++ (toString start.row) ++ "\n" ++
    "  Start Col: " ++ (toString start.col) ++ "\n" ++
    "    End Row: " ++ (toString end.row) ++ "\n" ++
    "    End Col: " ++ (toString end.col) ++ "\n\n"


--==============================================================================
--= Evaluation
--==============================================================================

type alias EvalHelper r a =
  Context -> Ranged r -> a -> Output

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

evalBaseValue : EvalHelper r ETerm
evalBaseValue _ _ eterm =
  { value =
      Ok eterm
  }

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

evalVariable : EvalHelper r EVariableInfo
evalVariable context range info =
  { value =
      let
        bindingLookup =
          Dict.get info.identifier context.environment
      in
        case bindingLookup of
          Just binding ->
            case binding of
              Just bindingTerm ->
                eval context bindingTerm
                  |> .value

              Nothing ->
                Ok << eterm_ <|
                  EVariable info

          Nothing ->
            Err [evalError context (NoSuchVariable info.identifier) range]
  }

--------------------------------------------------------------------------------
-- Lambdas
--------------------------------------------------------------------------------

evalLambda : EvalHelper r ELambdaInfo
evalLambda context _ { parameters, body } =
  let
    newEnvironment =
      extendEnvironment context.environment parameters

    newContext =
      { context | environment = newEnvironment }

    evaluatedBody =
      eval newContext body
        |> .value
  in
    { value =
        case evaluatedBody of
          Ok bodyTerm ->
            Ok << eterm_ <|
              ELambda
                { parameters =
                    parameters
                , body =
                    bodyTerm
                }

          Err bodyErrors ->
            Err bodyErrors
    }

--------------------------------------------------------------------------------
-- Parentheses
--------------------------------------------------------------------------------

evalParen : EvalHelper r EParenInfo
evalParen context _ { inside } =
  eval context inside

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

evalList : EvalHelper r EListInfo
evalList context _ { members } =
  { value =
      let
        collapsedEvaluatedMembers =
          members
            |> List.map (eval context >> .value)
            |> Utils.collapseResults
            |> Result.mapError List.concat
      in
        case collapsedEvaluatedMembers of
          Ok evaluatedMemberTerms ->
            Ok << eterm_ <|
              EList
                { members =
                    evaluatedMemberTerms
                }
          Err memberErrors ->
            Err memberErrors
  }

--------------------------------------------------------------------------------
-- Records
--------------------------------------------------------------------------------

evalRecord : EvalHelper r ERecordInfo
evalRecord context _ { base, entries } =
  { value =
      let
        evaluatedBase =
          Maybe.map (eval context >> .value) base

        collapsedEvaluatedBase =
          Utils.collapseMaybeResult evaluatedBase

        (entryKeys, entryValues) =
          List.unzip entries

        collapsedEvaluatedEntryValues =
          entryValues
            |> List.map (eval context >> .value)
            |> Utils.collapseResults
            |> Result.mapError List.concat
      in
        case collapsedEvaluatedBase of
          Ok evaluatedBaseTerm ->
            case collapsedEvaluatedEntryValues of
              Ok evaluatedEntryValueTerms ->
                Ok << eterm_ <|
                  ERecord
                    { base =
                        evaluatedBaseTerm
                    , entries =
                        Utils.zip entryKeys evaluatedEntryValueTerms
                    }

              Err entryValueErrors ->
                Err entryValueErrors

          Err baseErrors ->
            Err baseErrors
  }

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

evalConditional : EvalHelper r EConditionalInfo
evalConditional context range { condition, trueBranch, falseBranch } =
  { value =
      case
        eval context condition
          |> .value
      of
        Ok conditionTerm ->
          case conditionTerm.expression of
            EBool { bool } ->
              if bool then
                Ok trueBranch
              else
                Ok falseBranch

            _ ->
              Err [evalError context ConditionNotBool range]

        Err errors ->
          Err errors
  }

--------------------------------------------------------------------------------
-- Function Applications
--------------------------------------------------------------------------------

evalFunctionApplication : EvalHelper r EFunctionApplicationInfo
evalFunctionApplication context range { function, arguments } =
  { value =
      let
        evaluatedFunction =
          eval context function
            |> .value

        -- Evaluated arguments (strict)
        collapsedEvaluatedArguments =
          arguments
            |> List.map (eval context >> .value)
            |> Utils.collapseResults
            |> Result.mapError List.concat
      in
        case collapsedEvaluatedArguments of
          Ok evaluatedArgumentTerms ->
            case evaluatedFunction of
              Ok evaluatedFunctionTerm ->
                case evaluatedFunctionTerm.expression of
                  ELambda { parameters, body } ->
                    let
                      argCount =
                        List.length arguments

                      paramCount =
                        List.length parameters

                      evaluatedBody =
                        let
                          -- The parameter-argument pairs
                          pairs =
                            Utils.zip parameters evaluatedArgumentTerms

                          newEnvironment =
                            context.environment
                              |> flip extendEnvironment parameters
                              |> flip updateEnvironment pairs

                          -- The new context
                          newContext =
                            { context | environment = newEnvironment }
                        in
                          eval newContext body
                            |> .value
                    in
                      -- If fully applied, return the evaluated body.
                      -- Else, curry.
                      if argCount == paramCount then
                        evaluatedBody
                      else if argCount < paramCount then
                        case evaluatedBody of
                          Ok bodyTerm ->
                            let
                              remainingParameters =
                                List.drop argCount parameters
                            in
                              Ok << eterm_ <|
                                ELambda
                                  { parameters =
                                      remainingParameters
                                  , body =
                                      bodyTerm
                                  }

                          Err errors ->
                            Err errors
                      else
                        Err [evalError context TooManyArguments function]

                  -- Don't reduce this term futher if the function is a variable
                  -- (for now). The variable may actually be a lambda when looked
                  -- up later.
                  EVariable _ ->
                    Ok << eterm_ <|
                      EFunctionApplication
                        { function =
                            evaluatedFunctionTerm
                        , arguments =
                            evaluatedArgumentTerms
                        }

                  _ ->
                    Err [evalError context NotAFunction function]

              Err functionErrors ->
                Err functionErrors

          Err argumentErrors ->
            Err argumentErrors
  }

--------------------------------------------------------------------------------
-- Binary Operators
--------------------------------------------------------------------------------

evalBinaryOperator : EvalHelper r EBinaryOperatorInfo
evalBinaryOperator context range info =
  { value =
      Err [evalError context UnimplError range ]
  }

  -- TODO
  --EBinaryOperator { operator, left, right } ->
  --  eval context <|
  --    EFunctionApplication
  --      { function = operator
  --      , arguments = [left, right]
  --      }

--------------------------------------------------------------------------------
-- General Evaluation
--------------------------------------------------------------------------------

eval : Context -> ETerm -> Output
eval context eterm  =
  case eterm.expression of
    ELineComment _ ->
      evalBaseValue context eterm eterm

    EBlockComment _ ->
      evalBaseValue context eterm eterm

    EBool _ ->
      evalBaseValue context eterm eterm

    EInt _ ->
      evalBaseValue context eterm eterm

    EFloat _ ->
      evalBaseValue context eterm eterm

    EChar _ ->
      evalBaseValue context eterm eterm

    EString _ ->
      evalBaseValue context eterm eterm

    EMultiLineString _ ->
      evalBaseValue context eterm eterm

    EEmptyList _ ->
      evalBaseValue context eterm eterm

    EEmptyRecord _ ->
      evalBaseValue context eterm eterm

    EVariable info ->
      evalVariable context eterm info

    ELambda info ->
      evalLambda context eterm info

    EParen info  ->
      evalParen context eterm info

    EList info ->
      evalList context eterm info

    ERecord info ->
      evalRecord context eterm info

    EConditional info ->
      evalConditional context eterm info

    EFunctionApplication info ->
      evalFunctionApplication context eterm info

    EBinaryOperator info ->
      evalBinaryOperator context eterm info
