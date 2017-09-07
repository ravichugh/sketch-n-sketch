module ElmEval exposing
  ( eval
  , showError
  )

import Dict exposing (Dict)

import Utils
import Range exposing (Ranged)

import ElmLang exposing
  ( Identifier
  , Pattern(..)
  , Expression(..)
  , ETerm
  , PTerm
  )

-- If a binding is Nothing, then it the variable exists in scope but isn't
-- assigned yet via function application (currying).
type alias Binding =
  Maybe ETerm

type alias Environment =
  Dict Identifier (Maybe ETerm)

type alias Context =
  { environment : Environment
  }

type alias Output =
  { value : Result (List EvalError) ETerm
  }

type EvalErrorType
  = TypeError
  | UnimplError
  | NoSuchVariableError
  | TooManyArgumentsError

type alias EvalError =
  Ranged { error : EvalErrorType }

evalError : EvalErrorType -> Ranged a -> EvalError
evalError error { start, end } =
  { start = start
  , end = end
  , error = error
  }

eval : Context -> ETerm -> Output
eval context eterm  =
  let
    expression =
      eterm.expression
  in
    { value =
        case expression of
          -- Comments
          ELineComment _ ->
            Ok eterm
          EBlockComment _ ->
            Ok eterm

          -- Literals
          EBool _ ->
            Ok eterm
          EInt _ ->
            Ok eterm
          EFloat _ ->
            Ok eterm
          EChar _ ->
            Ok eterm
          EString _ ->
            Ok eterm
          EMultiLineString _ ->
            Ok eterm
          ELambda _ ->
            Ok eterm

          -- Lists
          EList { members } ->
            let
              (evalErrs, evalOks) =
                members
                  |> List.map (eval context >> .value)
                  |> Utils.partitionResults
                  |> Tuple.mapFirst List.concat
            in
              if List.isEmpty evalErrs then
                Ok
                  { eterm
                      | expression =
                          EList { members = evalOks }
                  }
              else
                Err evalErrs
          EEmptyList _ ->
            Ok eterm

          -- Records
          ERecord _ ->
            Ok eterm
          EEmptyRecord _ ->
            Ok eterm


          -- Variables
          EVariable { identifier } ->
            let
              maybeBinding =
                Dict.get identifier context.environment
            in
              case maybeBinding of
                Just binding ->
                  case binding of
                    Just eterm ->
                      .value <| eval context eterm
                    Nothing ->
                      Ok eterm
                Nothing ->
                  Err [evalError NoSuchVariableError eterm]

          -- Parentheses
          EParen { eterm } ->
            let
              insideOutput =
                eval context eterm
            in
              insideOutput.value

          -- Conditional
          EConditional { condition, trueBranch, falseBranch } ->
            let
              conditionOutput =
                eval context condition
            in
              case conditionOutput.value of
                Ok conditionTerm ->
                  case conditionTerm.expression of
                    EBool { bool } ->
                      if bool then
                        Ok trueBranch
                      else
                        Ok falseBranch
                    _ ->
                      Err [evalError TypeError condition]
                Err errs ->
                  Err errs

          EFunctionApplication { function, arguments } ->
            let
              functionOutput =
                eval context function
            in
              case functionOutput.value of
                Ok functionTerm ->
                  case functionTerm.expression  of
                    ELambda { parameters, body } ->
                      let
                        -- List lengths
                        argCount =
                          List.length arguments
                        paramCount =
                          List.length parameters

                        -- Temporary helper (no pattern matching)
                        getName pterm =
                          case pterm.pattern of
                            PNamed { name } ->
                              name

                        -- The base environment to build the new environment
                        -- from
                        baseEnvironment =
                          parameters
                            |> List.map (\p -> (getName p, Nothing))
                            |> Dict.fromList

                        -- The parameter-argument pairs
                        pairs =
                          Utils.zip parameters arguments

                        -- How to build the new environment
                        builder (pterm, arg) env =
                          case pterm.pattern of
                            PNamed { name } ->
                              Dict.insert name (Just arg) env

                        -- The new environment, context, and body
                        newEnvironment =
                          List.foldl builder baseEnvironment pairs
                        newContext =
                          { context | environment = newEnvironment }
                        bodyOutput =
                          body
                            |> eval newContext
                            |> .value

                        -- The end result of the evaluation
                        result =
                          -- If fully applied, return the evaluated body. Else,
                          -- curry.
                          if argCount == paramCount then
                            bodyOutput
                          else if argCount < paramCount then
                            case bodyOutput of
                              Ok bodyTerm ->
                                let
                                  remainingParameters =
                                    List.drop argCount parameters
                                in
                                  Ok
                                    { eterm
                                        | expression =
                                            ELambda
                                              { parameters =
                                                  remainingParameters
                                              , body =
                                                  bodyTerm
                                              }
                                    }
                              Err errs ->
                                Err errs
                          else
                            Err [evalError TooManyArgumentsError function]
                      in
                        result
                    _ ->
                      Err [evalError TypeError function]
                Err errs ->
                  Err errs

          -- TODO
          --EBinaryOperator { operator, left, right } ->
          --  eval context <|
          --    EFunctionApplication
          --      { function = operator
          --      , arguments = [left, right]
          --      }

          _ ->
            Err [evalError UnimplError eterm]
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
