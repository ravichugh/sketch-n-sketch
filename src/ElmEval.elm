module ElmEval exposing
  ( eval
  , evalProgram
  , showError
  )

import Dict exposing (Dict)
import Set exposing (Set)

import Utils
import Position exposing (dummyPosition)
import Range exposing (Ranged)

import ElmLang exposing (..)
import ElmPrettyPrint

--==============================================================================
--= Data Structures
--==============================================================================

--------------------------------------------------------------------------------
-- Evaluation Context
--------------------------------------------------------------------------------

type alias Context =
  {}

emptyContext : Context
emptyContext =
  {}

--------------------------------------------------------------------------------
-- Evaluation Output
--------------------------------------------------------------------------------

type EvalErrorType
  = ConditionNotBool ETerm
  | NotAFunction ETerm
  | UnboundVariable Identifier
  | TooManyArguments
  | SpecialError
  | MainHasParameters
  | NoMainFunction

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
        UnboundVariable identifier ->
          "Unbound variable: " ++ identifier
        TooManyArguments ->
          "Too many arguments"
        SpecialError ->
          "Special error"
        MainHasParameters ->
          "Main function has parameters"
        NoMainFunction ->
          "No main function"

    prettyErrorLines =
      String.join "\n" relevantLines
  in
    "[Evaluator Error]\n\n" ++
      prettyError ++ "\n\n" ++
      prettyErrorLines ++ "\n\n" ++
    "Position\n" ++
    "========\n" ++
    "  Start Row: " ++ (toString start.row) ++ "\n" ++
    "  Start Col: " ++ (toString start.col) ++ "\n" ++
    "    End Row: " ++ (toString end.row) ++ "\n" ++
    "    End Col: " ++ (toString end.col) ++ "\n\n"

--==============================================================================
--= Pattern Matching
--==============================================================================

type alias Binding =
  (Identifier, ETerm)

match : PTerm -> ETerm -> Result (List EvalError) (List Binding)
match pTerm eTerm =
  case pTerm.pattern of
    PNamed { identifier } ->
      Ok [(identifier, eTerm)]

--==============================================================================
--= Beta Reduction
--==============================================================================

type alias Substitution =
  (PTerm, ETerm)

bind : Binding -> ETerm -> ETerm
bind (identifier, value) body =
  let
    subst =
      bind (identifier, value)
  in
    case body.expression of
      ELineComment _ ->
        body

      EBlockComment _ ->
        body

      EBool _ ->
        body

      EInt _ ->
        body

      EFloat _ ->
        body

      EChar _ ->
        body

      EString _ ->
        body

      EMultiLineString _ ->
        body

      EEmptyList _ ->
        body

      EEmptyRecord _ ->
        body

      ELambda info ->
        let
          newBody =
            -- Shadowing
            if
              List.member
                identifier
                (getIdentifiers info.parameter.pattern)
            then
              info.body

            else
              subst info.body
        in
          eTerm_ <|
            ELambda
              { parameter =
                  info.parameter
              , body =
                  newBody
              }

      EVariable info ->
        if info.identifier == identifier then
          value

        else
          body

      EParen { inside } ->
        subst inside

      EList { members } ->
        eTerm_ <|
          EList
            { members =
                List.map subst members
            }

      ERecord { base, entries } ->
        eTerm_ <|
          ERecord
            { base =
                Maybe.map subst base
            , entries =
                List.map
                  (\(p, e) -> (p, subst e))
                  entries
            }

      EConditional { condition, trueBranch, falseBranch } ->
        eTerm_ <|
          EConditional
            { condition =
                subst condition
            , trueBranch =
                subst trueBranch
            , falseBranch =
                subst falseBranch
            }

      EFunctionApplication { function, argument } ->
        eTerm_ <|
          EFunctionApplication
            { function =
                subst function
            , argument =
                subst argument
            }

      EBinaryOperator { operator, left, right } ->
        eTerm_ <|
          EBinaryOperator
            { operator =
                operator
            , left =
                subst left
            , right =
                subst right
            }

      ESpecial { special, arguments } ->
        eTerm_ <|
          ESpecial
            { special =
                special
            , arguments =
                List.map subst arguments
            }

bindAll : List Binding -> ETerm -> ETerm
bindAll bindings body =
  List.foldl bind body bindings

substitute : Substitution -> ETerm -> Result (List EvalError) ETerm
substitute (pTerm, eTerm) body =
  match pTerm eTerm
    |> Result.map (flip bindAll body)

--==============================================================================
--= E-Term Evaluation
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
evalVariable context range { identifier } =
  case specialify identifier of
    Just special ->
      let
        newSpecial =
          eTerm_ <|
            ESpecial
              { special =
                  special
              , arguments =
                  []
              }
      in
        eval context newSpecial

    Nothing ->
      { value =
          Err [evalError context (UnboundVariable identifier) range]
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
                eval context trueBranch
                  |> .value

              else
                eval context falseBranch
                  |> .value

            _ ->
              Err [evalError context (ConditionNotBool condition) range]

        Err errors ->
          Err errors
  }

--------------------------------------------------------------------------------
-- Function Applications
--------------------------------------------------------------------------------

-- Helpers

applyLambda : Context -> ELambdaInfo -> ETerm -> Output
applyLambda context { parameter, body } argument =
  { value =
      substitute (parameter, argument) body
        |> Result.andThen (eval context >> .value)
  }

applySpecial : Context -> ESpecialInfo -> ETerm -> Output
applySpecial context info argument =
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

-- Evaluator

evalFunctionApplication : EvalHelper r EFunctionApplicationInfo
evalFunctionApplication context _ { function, argument } =
  { value =
      let
        evaluatedFunction =
          eval context function
            |> .value

        -- Evaluate argument (be strict)
        evaluatedArgument =
          eval context argument
            |> .value
      in
        case evaluatedArgument of
          Ok evaluatedArgumentTerm ->
            case evaluatedFunction of
              Ok evaluatedFunctionTerm ->
                case evaluatedFunctionTerm.expression of
                  ELambda info ->
                    applyLambda context info evaluatedArgumentTerm
                      |> .value

                  ESpecial info ->
                    applySpecial context info evaluatedArgumentTerm
                      |> .value

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

specialInt2 : (Int -> Int -> Int) -> ETerm -> ETerm -> Maybe ETerm
specialInt2 intFunction left right =
  case (left.expression, right.expression) of
    (EInt leftInfo, EInt rightInfo) ->
      Just << eTerm_ <|
        EInt
          { int =
              intFunction leftInfo.int rightInfo.int
          }

    _ ->
      Nothing

specialIntComparision
  : (Int -> Int -> Bool) -> ETerm -> ETerm -> Maybe ETerm
specialIntComparision comparator left right =
  case (left.expression, right.expression) of
    (EInt leftInfo, EInt rightInfo) ->
      Just << eTerm_ <|
        EBool
          { bool =
              comparator leftInfo.int rightInfo.int
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
                        specialInt2 (+) left right
                      _ ->
                        Nothing
                  Sub ->
                    case evaluatedArgumentTerms of
                      [ left, right ] ->
                        specialInt2 (-) left right
                      _ ->
                        Nothing

                  Eq ->
                    case evaluatedArgumentTerms of
                      [ left, right ] ->
                        specialIntComparision (==) left right
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
-- General E-Term Evaluation
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

    ELambda _ ->
      evalBaseValue context eTerm eTerm

    EVariable info ->
      evalVariable context eTerm info

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

--==============================================================================
--= Program Evaluation
--==============================================================================

-- Helpers

definitionsFromProgram : ElmLang.Program -> List Definition
definitionsFromProgram =
  List.filterMap <|
    \sTerm ->
      case sTerm.statement of
        SDefinition definition ->
          Just definition

        SLineComment _ ->
          Nothing

        SBlockComment _ ->
          Nothing

findAndRemoveMainTerm
  : List Definition -> Result (List EvalError) (ETerm, List Definition)
findAndRemoveMainTerm definitions =
  let
    mainName =
      PNamed { identifier = "main" }
  in
    case
      Utils.maybeFindAndRemoveFirst
        (.name >> .pattern >> (==) mainName)
        definitions
    of
      Just (mainDefinition, otherDefinitions) ->
        if List.isEmpty mainDefinition.parameters then
          Ok (mainDefinition.body, otherDefinitions)

        else
          Err
            [ evalError
                emptyContext
                MainHasParameters
                { start = dummyPosition
                , end = dummyPosition
                }
            ]

      Nothing ->
        Err
          [ evalError
              emptyContext
              NoMainFunction
              { start = dummyPosition
              , end = dummyPosition
              }
          ]

bindingsFromDefinition : Definition -> Result (List EvalError) (List Binding)
bindingsFromDefinition { name, parameters, body } =
  let
    value =
      if List.isEmpty parameters then
        body

      else
        buildLambda parameters body

    evaluatedValue =
      eval emptyContext value
        |> .value
  in
    case evaluatedValue of
      Ok evaluatedValueTerm ->
        match name evaluatedValueTerm

      Err valueErrors ->
        Err valueErrors

bindingsFromDefinitions
  : List Definition -> Result (List EvalError) (List Binding)
bindingsFromDefinitions definitions =
  definitions
    |> List.map bindingsFromDefinition
    |> Utils.collapseResults
    |> Result.mapError List.concat
    |> Result.map List.concat

evalMain : (ETerm, List Binding) -> Output
evalMain (mainTerm, bindings) =
  { value =
      mainTerm
        |> bindAll bindings
        |> eval emptyContext
        |> .value
  }

-- Evaluator

evalProgram : ElmLang.Program -> Output
evalProgram program =
  { value =
      program
        |> definitionsFromProgram
        |> findAndRemoveMainTerm
        |> Result.andThen
             ( Tuple.mapSecond bindingsFromDefinitions
                 >> Utils.collapseSecondResult
             )
        |> Result.andThen
             ( evalMain >> .value
             )
  }
