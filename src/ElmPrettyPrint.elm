module ElmPrettyPrint exposing
  ( prettyPrint
  )

import ElmLang exposing
  ( Pattern(..)
  , Expression(..)
  , ETerm
  , PTerm
  )

prettyPrintP : PTerm -> String
prettyPrintP { pattern } =
  case pattern of
    PNamed { name } ->
      name

prettyPrint : ETerm -> String
prettyPrint { expression } =
  case expression of
    ELineComment { text, termAfter } ->
      "--"
        ++ text
        ++ (Maybe.withDefault "" <| Maybe.map prettyPrint termAfter)
    EBlockComment { text, termAfter } ->
      "{-"
        ++ text
        ++ "-}"
        ++ (Maybe.withDefault "" <| Maybe.map prettyPrint termAfter)
    EVariable { identifier } ->
      identifier
    EBool { bool } ->
      if bool then "True" else "False"
    EInt { int } ->
      toString int
    EFloat { float } ->
      toString float
    EChar { char } ->
      "'" ++ String.fromChar char ++ "'"
    EString { string } ->
      "\"" ++ string ++ "\""
    EMultiLineString { string } ->
      "\"\"\"" ++ string ++ "\"\"\""
    EList { members } ->
      members
        |> List.map prettyPrint
        |> String.join ", "
        |> String.append "["
        |> flip String.append "]"
    EEmptyList { space } ->
      "[" ++ space.ws ++ "]"
    ERecord { base, entries } ->
      entries
        |> List.map (\(p, e) -> prettyPrintP p ++ " = " ++ prettyPrint e)
        |> String.join ", "
        |> String.append "{"
        |> flip String.append "}"
    EEmptyRecord { space } ->
      "{" ++ space.ws ++ "}"
    ELambda { parameters, body } ->
      parameters
        |> List.map prettyPrintP
        |> String.join " "
        |> String.append "\\"
        |> flip String.append " -> "
        |> flip String.append (prettyPrint body)
    EParen { eterm } ->
      "(" ++ prettyPrint eterm ++ ")"
    EConditional { condition, trueBranch, falseBranch } ->
      "if" ++ prettyPrint condition
        ++ "then" ++ prettyPrint trueBranch
        ++ "else" ++ prettyPrint falseBranch
    EFunctionApplication { function, arguments } ->
      arguments
        |> List.map prettyPrint
        |> String.join " "
        |> flip String.append (prettyPrint function)
    EBinaryOperator { operator, left, right } ->
      prettyPrint left ++ operator ++ prettyPrint right
