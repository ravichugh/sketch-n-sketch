module ElmEval exposing
  ( eval
  , showError
  )

import Dict exposing (Dict)
import Set exposing (Set)

import Utils
import Range exposing (Ranged)

import ElmLang exposing (..)
import ElmPrettyPrint

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
  Dict Identifier Binding

extendEnvironment : Identifier -> Binding -> Environment -> Environment
extendEnvironment =
  Dict.insert

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
  = ConditionNotBool ETerm
  | NotAFunction ETerm
  | NoSuchVariable Identifier
  | TooManyArguments
  | SpecialError

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
      case error of
        ConditionNotBool eTerm ->
          "Condition not bool: " ++ ElmPrettyPrint.prettyPrint eTerm
        NotAFunction eTerm ->
          "Not a function: " ++ ElmPrettyPrint.prettyPrint eTerm
        NoSuchVariable identifier ->
          "No such variable: " ++ identifier
        TooManyArguments ->
          "Too many arguments"
        SpecialError ->
          "Special error"

    prettyErrorLines =
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
      prettyError ++ "\n\n" ++
      prettyErrorLines ++ "\n\n" ++
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
evalBaseValue _ _ eTerm =
  { value =
      Ok eTerm
  }

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

evalVariable : EvalHelper r EVariableInfo
evalVariable context range info =
  { value =
      case specialify info.identifier of
        Just special ->
          Ok << eTerm_ <|
            ESpecial
              { special =
                  special
              , arguments =
                  []
              }

        Nothing ->
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
                    Ok << eTerm_ <|
                      EVariable info

              Nothing ->
                Err [evalError context (NoSuchVariable info.identifier) range]
  }

--------------------------------------------------------------------------------
-- Lambdas
--------------------------------------------------------------------------------

evalLambda : EvalHelper r ELambdaInfo
evalLambda context _ { parameter, body } =
  let
    identifier =
      getIdentifier parameter.pattern

    newContext =
      { context
          | environment =
              extendEnvironment identifier Nothing context.environment
      }

    evaluatedBody =
      eval newContext body
        |> .value
  in
    { value =
        case evaluatedBody of
          Ok evaluatedBodyTerm ->
            Ok << eTerm_ <|
              ELambda
                { parameter =
                    parameter
                , body =
                    evaluatedBodyTerm
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
            Ok << eTerm_ <|
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
                Ok << eTerm_ <|
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
              Err [evalError context (ConditionNotBool condition) range]

        Err errors ->
          Err errors
  }

--------------------------------------------------------------------------------
-- Function Applications
--------------------------------------------------------------------------------

handleVariableApplication
  :  Context -> EVariableInfo -> ETerm
  -> Result (List EvalError) ETerm
handleVariableApplication context ({ identifier } as info) argument =
  case specialify identifier of
    Just special ->
      Ok << eTerm_ <|
        ESpecial
          { special =
              special
          , arguments =
              [ argument ]
          }

    -- This variable is not a special function, but do not reduce this term
    -- further (for now). The variable may actually be a lambda when looked up
    -- later.
    Nothing ->
      Ok << eTerm_ <|
        EFunctionApplication
          { function =
              eTerm_ <|
                EVariable info
          , argument =
              argument
          }

handleLambdaApplication
  :  Context -> ELambdaInfo -> ETerm
  -> Result (List EvalError) ETerm
handleLambdaApplication context { parameter, body } argument =
  let
    identifier =
      getIdentifier parameter.pattern

    newContext =
      { context
          | environment =
              extendEnvironment
                identifier
                (Just argument)
                context.environment
      }
  in
    eval newContext body
      |> .value

handleSpecialApplication
  :  Context -> ESpecialInfo -> ETerm
  -> Result (List EvalError) ETerm
handleSpecialApplication context info argument =
  let
    newSpecial =
      eTerm_ <|
        ESpecial
          { info
              | arguments =
                  info.arguments ++ [ argument ]
          }
  in
    eval context newSpecial
      |> .value

handleFunctionApplicationApplication
  :  Context -> EFunctionApplicationInfo -> ETerm
  -> Result (List EvalError) ETerm
handleFunctionApplicationApplication context info argument =
  Ok << eTerm_ <|
    EFunctionApplication
      { function =
          eTerm_ <|
            EFunctionApplication info
      , argument =
          argument
      }

evalFunctionApplication : EvalHelper r EFunctionApplicationInfo
evalFunctionApplication context _ { function, argument } =
  { value =
      let
        evaluatedFunction =
          eval context function
            |> .value

        -- Evaluated argument (strict)
        evaluatedArgument =
          eval context argument
            |> .value
      in
        case evaluatedArgument of
          Ok evaluatedArgumentTerm ->
            case evaluatedFunction of
              Ok evaluatedFunctionTerm ->
                case evaluatedFunctionTerm.expression of
                  EVariable info ->
                    handleVariableApplication
                      context info evaluatedArgumentTerm

                  ELambda info ->
                    handleLambdaApplication
                      context info evaluatedArgumentTerm

                  -- Unreduced function application even after evaluation
                  EFunctionApplication info ->
                    handleFunctionApplicationApplication
                      context info evaluatedArgumentTerm

                  ESpecial info ->
                    handleSpecialApplication
                      context info evaluatedArgumentTerm

                  _ ->
                    Err [evalError context (NotAFunction function) function]

              Err functionErrors ->
                Err functionErrors

          Err argumentErrors ->
            Err argumentErrors
  }

--------------------------------------------------------------------------------
-- Binary Operators
--------------------------------------------------------------------------------
-- a + b == ((+) a) b

evalBinaryOperator : EvalHelper r EBinaryOperatorInfo
evalBinaryOperator context _ { operator, left, right } =
  let
    function =
      eTerm_ <|
        EVariable
          { identifier =
              prefixifyOperator operator
          }

    firstApplication =
      eTerm_ <|
        EFunctionApplication
          { function =
              function
          , argument =
              left
          }

    secondApplication =
      eTerm_ <|
        EFunctionApplication
          { function =
              firstApplication
          , argument =
              right
          }
  in
    eval context secondApplication

--------------------------------------------------------------------------------
-- Specials
--------------------------------------------------------------------------------

intSpecial2 : (Int -> Int -> Int) -> ETerm -> ETerm -> Maybe ETerm
intSpecial2 intFunction left right =
  case (left.expression, right.expression) of
    (EInt leftInfo, EInt rightInfo) ->
      Just << eTerm_ <|
        EInt
          { int =
              intFunction leftInfo.int rightInfo.int
          }

    _ ->
      Nothing

evalSpecial : EvalHelper r ESpecialInfo
evalSpecial context range { special, arguments } =
  let
    collapsedEvaluatedArguments =
      arguments
        |> List.map (eval context >> .value)
        |> Utils.collapseResults
        |> Result.mapError List.concat
  in
    { value =
        case collapsedEvaluatedArguments of
          Ok evaluatedArgumentTerms ->
            let
              result =
                case special of
                  Add ->
                    case evaluatedArgumentTerms of
                      [ left, right ] ->
                        intSpecial2 (+) left right
                      _ ->
                        Nothing
            in
              case result of
                Just resultTerm ->
                  Ok resultTerm

                Nothing ->
                  Ok << eTerm_ <|
                    ESpecial
                      { special =
                          special
                      , arguments =
                          evaluatedArgumentTerms
                      }

          Err argumentErrors ->
            Err argumentErrors
    }

--------------------------------------------------------------------------------
-- General Evaluation
--------------------------------------------------------------------------------

eval : Context -> ETerm -> Output
eval context eTerm  =
  case eTerm.expression of
    ELineComment _ ->
      evalBaseValue context eTerm eTerm

    EBlockComment _ ->
      evalBaseValue context eTerm eTerm

    EBool _ ->
      evalBaseValue context eTerm eTerm

    EInt _ ->
      evalBaseValue context eTerm eTerm

    EFloat _ ->
      evalBaseValue context eTerm eTerm

    EChar _ ->
      evalBaseValue context eTerm eTerm

    EString _ ->
      evalBaseValue context eTerm eTerm

    EMultiLineString _ ->
      evalBaseValue context eTerm eTerm

    EEmptyList _ ->
      evalBaseValue context eTerm eTerm

    EEmptyRecord _ ->
      evalBaseValue context eTerm eTerm

    EVariable info ->
      evalVariable context eTerm info

    ELambda info ->
      evalLambda context eTerm info

    EParen info  ->
      evalParen context eTerm info

    EList info ->
      evalList context eTerm info

    ERecord info ->
      evalRecord context eTerm info

    EConditional info ->
      evalConditional context eTerm info

    EFunctionApplication info ->
      evalFunctionApplication context eTerm info

    EBinaryOperator info ->
      evalBinaryOperator context eTerm info

    ESpecial info ->
      evalSpecial context eTerm info
