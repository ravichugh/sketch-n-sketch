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
  | NoSuchVariable
  | TooManyArguments
  | UnimplError

type alias EvalError =
  Ranged { error : EvalErrorType }

type alias Output =
  { value : Result (List EvalError) ETerm
  }

evalError : EvalErrorType -> Ranged a -> EvalError
evalError error { start, end } =
  { start = start
  , end = end
  , error = error
  }

showError : String -> EvalError -> String
showError source { start, end, error } =
  let
    relevantLines =
      source
        |> String.lines
        |> List.take end.row
        |> List.drop (start.row - 1)
    prettyError =
      String.join "\n" relevantLines
  in
    "[Evaluator Error]\n\n" ++
      (toString error) ++ ":\n\n" ++
      prettyError ++ "\n\n" ++
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
            Err [evalError NoSuchVariable range]
  }

--------------------------------------------------------------------------------
-- Parentheses
--------------------------------------------------------------------------------

evalParen : EvalHelper r EParenInfo
evalParen context _ { inside } =
  { value =
      eval context inside
        |> .value
  }

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

evalList : EvalHelper r EListInfo
evalList context _ { members } =
  { value =
      let
        (evaluatedMemberErrors, evaluatedMemberOks) =
          members
            |> List.map (eval context >> .value)
            |> Utils.partitionResults
            |> Tuple.mapFirst List.concat
      in
        if List.isEmpty evaluatedMemberErrors then
          Ok << eterm_ <|
            EList
              { members =
                  evaluatedMemberOks
              }
        else
          Err evaluatedMemberErrors
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

        (evaluatedBaseErrors, evaluatedBaseOk) =
          case evaluatedBase of
            Just (Ok baseTerm) ->
              ([], Just baseTerm)

            Just (Err baseErrors) ->
              (baseErrors, Nothing)

            Nothing ->
              ([], Nothing)

        (entryKeys, entryValues) =
          List.unzip entries

        (evaluatedEntryValueErrors, evaluatedEntryValueOks) =
          entryValues
            |> List.map (eval context >> .value)
            |> Utils.partitionResults
            |> Tuple.mapFirst List.concat

        errors =
          evaluatedBaseErrors ++ evaluatedEntryValueErrors
      in
        if List.isEmpty errors then
          Ok << eterm_ <|
            ERecord
              { base =
                  evaluatedBaseOk
              , entries =
                  Utils.zip entryKeys evaluatedEntryValueOks
              }
        else
          Err errors
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
              Err [evalError ConditionNotBool range]

        Err errors ->
          Err errors
  }

--------------------------------------------------------------------------------
-- Function Applications
--------------------------------------------------------------------------------

evalFunctionApplication : EvalHelper r EFunctionApplicationInfo
evalFunctionApplication context range { function, arguments } =
  { value =
      case
        eval context function
          |> .value
      of
        Ok functionTerm ->
          case functionTerm.expression of
            ELambda { parameters, body } ->
              let
                -- Evaluated arguments (strict)
                (evaluatedArgumentErrors, evaluatedArgumentOks) =
                  arguments
                    |> List.map (eval context >> .value)
                    |> Utils.partitionResults
                    |> Tuple.mapFirst List.concat
              in
                if List.isEmpty evaluatedArgumentErrors then
                  let
                    argCount =
                      List.length arguments

                    paramCount =
                      List.length parameters

                    evaluatedBody =
                      let
                        -- Temporary helper (no pattern matching)
                        getName pterm =
                          case pterm.pattern of
                            PNamed { name } ->
                              name

                        -- The parameter-argument pairs
                        pairs =
                          Utils.zip parameters evaluatedArgumentOks

                        -- The base environment from which to build the new
                        -- environment. All the parameters are in scope, but
                        -- unassigned.
                        baseEnvironment =
                          parameters
                            |> List.map (\p -> (getName p, Nothing))
                            |> Dict.fromList

                        -- The new environment
                        newEnvironment =
                          let
                            -- How to build the new environment
                            builder (pterm, arg) env =
                              case pterm.pattern of
                                PNamed { name } ->
                                  Dict.insert name (Just arg) env
                          in
                            List.foldl builder baseEnvironment pairs

                        -- The new context
                        newContext =
                          { context | environment = newEnvironment }
                      in
                        body
                          |> eval newContext
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
                      Err [evalError TooManyArguments function]
                else
                  Err evaluatedArgumentErrors
            _ ->
              Err [evalError NotAFunction function]
        Err errors ->
          Err errors
  }

--------------------------------------------------------------------------------
-- Binary Operators
--------------------------------------------------------------------------------

evalBinaryOperator : EvalHelper r EBinaryOperatorInfo
evalBinaryOperator context range info =
  { value =
      Err [evalError UnimplError range ]
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

    ELambda _ ->
      evalBaseValue context eterm eterm

    EVariable info ->
      evalVariable context eterm info

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
