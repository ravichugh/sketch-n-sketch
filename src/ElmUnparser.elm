module ElmUnparser exposing
  ( unparse
  , unparsePattern
  )

import Lang exposing (..)

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

unparseOp : Op -> String
unparseOp op =
  case op.val of
    Pi ->
      "pi"
    DictEmpty ->
      "empty"
    Cos ->
      "cos"
    Sin ->
      "sin"
    ArcCos ->
      "arccos"
    ArcSin ->
      "arcsin"
    Floor ->
      "floor"
    Ceil ->
      "ceiling"
    Round ->
      "round"
    ToStr ->
      "toString"
    Sqrt ->
      "sqrt"
    Explode ->
      "explode"
    Plus ->
      "+"
    Minus ->
      "-"
    Mult ->
      "*"
    Div ->
      "/"
    Lt ->
      "<"
    Eq ->
      "="
    Mod ->
      "mod"
    Pow ->
      "pow"
    ArcTan2 ->
      "arctan2"
    DictInsert ->
      "insert"
    DictGet ->
      "get"
    DictRemove ->
      "remove"
    DebugLog ->
      "debug"

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

    EApp wsBefore function arguments _ ->
      wsBefore.val
        ++ unparse function
        ++ String.concat (List.map unparse arguments)

    EOp wsBefore op arguments _ ->
      case arguments of
        [ left, right ] ->
          unparse left
            ++ wsBefore.val
            ++ unparseOp op
            ++ unparse right

        _ ->
          wsBefore.val
            ++ unparseOp op
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

    EParens wsBefore innerExpression wsAfter ->
      wsBefore.val
        ++ "("
        ++ unparse innerExpression
        ++ wsAfter.val
        ++ ")"

    Lang.EColonType _ _ _ _ _ ->
      Debug.crash "TODO"

    Lang.ETyp _ _ _ _ _ ->
      Debug.crash "TODO"

    Lang.ETypeAlias _ _ _ _ _ ->
      Debug.crash "TODO"

    Lang.ETypeCase _ _ _ _ ->
      Debug.crash "TODO"

