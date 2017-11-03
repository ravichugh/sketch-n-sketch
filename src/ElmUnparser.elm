module ElmUnparser exposing
  ( unparse
  , unparsePattern
  )

import Lang exposing (..)
import ElmLang

unparseBaseValue : EBaseVal -> String
unparseBaseValue ebv =
  case ebv of
    EBool b ->
      if b then "True" else "False"

    EString quoteChar text ->
      quoteChar ++ text ++ quoteChar

    ENull ->
      "null"

unparsePattern : Pat -> String
unparsePattern p =
  case p.val.p__ of
    PVar wsBefore identifier widgetDeclaration ->
      wsBefore.val
        ++ identifier

    PConst wsBefore num ->
      wsBefore.val
        ++ toString num

    PBase wsBefore baseValue ->
      wsBefore.val
        ++ unparseBaseValue baseValue

    PList wsBefore members _ _ wsBeforeEnd ->
      wsBefore.val
        ++ "["
        ++ String.join "," (List.map unparsePattern members)
        ++ wsBeforeEnd.val
        ++ "]"

    PAs wsBefore identifier wsBeforeAs pat ->
      wsBefore.val
        ++ "("
        ++ identifier
        ++ wsBeforeAs.val
        ++ "as"
        ++ unparsePattern pat
        ++ ")"

unparseBranch : Branch -> String
unparseBranch branch =
  case branch.val of
    Branch_ wsBefore p e _ ->
      wsBefore.val
        ++ unparsePattern p
        ++ " ->"
        ++ unparse e
        ++ ";"

unparse : Exp -> String
unparse e =
  case e.val.e__ of
    EConst wsBefore num _ _ ->
      wsBefore.val
        ++ toString num

    EBase wsBefore baseValue ->
      wsBefore.val
        ++ unparseBaseValue baseValue

    EVar wsBefore identifier ->
      wsBefore.val
        ++ identifier

    EFun wsBefore parameters body _ ->
      wsBefore.val
        ++ "\\"
        ++ String.concat (List.map unparsePattern parameters)
        ++ " ->"
        ++ unparse body

    EApp _ function arguments _ ->
      case (ElmLang.isOperator function, arguments) of
        (True, [ left, right ]) ->
          unparse left ++ unparse function ++ unparse right

        _ ->
          unparse function
            ++ String.concat (List.map unparse arguments)

    EList wsBefore members _ _ wsBeforeEnd ->
      wsBefore.val
        ++ "["
        ++ String.join "," (List.map unparse members)
        ++ wsBeforeEnd.val
        ++ "]"

    EIf wsBefore condition trueBranch falseBranch _ ->
      wsBefore.val
        ++ "if"
        ++ unparse condition
        ++ " then"
        ++ unparse trueBranch
        ++ " else"
        ++ unparse falseBranch

    ECase wsBefore examinedExpression branches _ ->
      wsBefore.val
        ++ "case"
        ++ unparse examinedExpression
        ++ " of"
        ++ String.concat (List.map unparseBranch branches)

    ELet wsBefore _ _ name binding body _ ->
      wsBefore.val
        ++ "let"
        ++ unparsePattern name
        ++ " ="
        ++ unparse binding
        ++ " in"
        ++ unparse body

    EComment wsBefore text expAfter ->
      wsBefore.val
        ++ "--"
        ++ text
        ++ "\n"
        ++ unparse expAfter

    EOption wsBefore option wsMid value expAfter ->
      wsBefore.val
        ++ "# "
        ++ option.val
        ++ ":"
        ++ wsMid.val
        ++ value.val
        ++ "\n"
        ++ unparse expAfter

    _ ->
      toString e
