module ElmUnparser exposing
  ( unparse
  )

import Lang exposing (..)

unparseBaseValue : EBaseVal -> String
unparseBaseValue ebv =
  case ebv of
    EBool b ->
      "True"

    EString quoteChar text ->
      quoteChar ++ text ++ quoteChar

    ENull ->
      "null"

unparsePattern : Pat -> String
unparsePattern p =
  case p.val.p__ of
    PVar wsBefore identifier widgetDeclaration ->
      wsBefore.val ++ identifier

    PConst wsBefore num ->
      wsBefore.val ++ toString num

    PBase wsBefore baseValue ->
      wsBefore.val ++ unparseBaseValue baseValue

    _ ->
      toString p

unparse : Exp -> String
unparse e =
  case e.val.e__ of
    EBase wsBefore baseValue ->
      wsBefore.val ++ unparseBaseValue baseValue

    EVar wsBefore identifier ->
      wsBefore.val ++ identifier

    EFun wsBefore parameters body _ ->
      wsBefore.val
        ++ "\\"
        ++ String.concat (List.map unparsePattern parameters)
        ++ "->"
        ++ unparse body

    _ ->
      toString e
