module ElmUnparser exposing
  ( unparse
  , unparsePattern
  )

import Lang exposing (..)
import ElmLang
import Utils

unparseWD : WidgetDecl -> String
unparseWD wd =
  let strHidden bool = if bool then ",\"hidden\"" else "" in
  case wd.val of
    NoWidgetDecl        -> ""
    IntSlider a tok b _ hidden ->
      "{" ++ toString a.val ++ tok.val ++ toString b.val ++ strHidden hidden ++ "}"
    NumSlider a tok b _ hidden ->
      "{" ++ toString a.val ++ tok.val ++ toString b.val ++ strHidden hidden ++ "}"

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

-- TODO
unparseType : Type -> String
unparseType tipe =
  case tipe.val of
    TNum ws                   -> ws.val ++ "Num"
    TBool ws                  -> ws.val ++ "Bool"
    TString ws                -> ws.val ++ "String"
    TNull ws                  -> ws.val ++ "Null"
    TList ws1 tipe ws2        -> ws1.val ++ "(List" ++ (unparseType tipe) ++ ws2.val ++ ")"
    TDict ws1 tipe1 tipe2 ws2 -> ws1.val ++ "(Dict" ++ (unparseType tipe1) ++ (unparseType tipe2) ++ ws2.val ++ ")"
    TTuple ws1 typeList ws2 maybeRestType ws3 ->
      case maybeRestType of
        Just restType -> ws1.val ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ "|" ++ (unparseType restType) ++ ws3.val ++ "]"
        Nothing       -> ws1.val ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws3.val ++ "]"
    TArrow ws1 typeList ws2 -> ws1.val ++ "(->" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TUnion ws1 typeList ws2 -> ws1.val ++ "(union" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TNamed ws1 "Num"        -> ws1.val ++ "Bad_NUM"
    TNamed ws1 "Bool"       -> ws1.val ++ "Bad_BOOL"
    TNamed ws1 "String"     -> ws1.val ++ "Bad_STRING"
    TNamed ws1 "Null"       -> ws1.val ++ "Bad_NULL"
    TNamed ws1 ident        -> ws1.val ++ ident
    TVar ws1 ident          -> ws1.val ++ ident
    TWildcard ws            -> ws.val ++ "_"
    TForall ws1 typeVars tipe1 ws2 ->
      let strVar (ws,x) = ws.val ++ x in
      let sVars =
        case typeVars of
          One var             -> strVar var
          Many ws1_ vars ws2_ -> ws1_.val ++ Utils.parens (String.concat (List.map strVar vars) ++ ws2_.val)
      in
      ws1.val ++ Utils.parens ("forall" ++ sVars ++ unparseType tipe1 ++ ws2.val)

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
      "=="
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
    NoWidgets ->
      "noWidgets"

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
    EConst wsBefore num (_, frozen, _) wd ->
      wsBefore.val
        ++ toString num
        ++ frozen
        ++ unparseWD wd

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
      -- NOTE: to help with converting Little to Elm
      let unparseArg e =
        case e.val.e__ of
          EApp ws1 f args ws2 ->
            ws1.val
              ++ "("
              ++ unparse (replaceE__ e (EApp { ws1 | val = "" } f args ws2))
              ++ ")"
          EOp ws1 f args ws2 ->
            ws1.val
              ++ "("
              ++ unparse (replaceE__ e (EOp { ws1 | val = "" } f args ws2))
              ++ ")"
          _ ->
            unparse e
      in
      wsBefore.val
        ++ unparse function
        -- NOTE: to help with converting Little to Elm
        -- ++ String.concat (List.map unparse arguments)
        ++ String.concat (List.map unparseArg arguments)

    EOp wsBefore op arguments _ ->
      -- TODO: help convert Little to Elm by inserting parens
      -- for nested EOps, based on precedence/associativity
      let
        default =
          wsBefore.val
            ++ unparseOp op
            ++ String.concat (List.map unparse arguments)
      in
        if ElmLang.isInfixOperator op then
          case arguments of
            [ left, right ] ->
              unparse left
                ++ wsBefore.val
                ++ unparseOp op
                ++ unparse right
            _ ->
              default
        else
          default

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

    ELet wsBefore letKind _ name binding_ body wsBeforeSemicolon ->
      let
        (parameters, binding) =
          case binding_.val.e__ of
            EFun _ parameters functionBinding _ ->
              (parameters, functionBinding)

            _ ->
              ([], binding_)

        strParametersDefault =
          String.concat (List.map unparsePattern parameters)

        -- NOTE: to help with converting Little to Elm
        strParameters =
          case (parameters, String.startsWith " " strParametersDefault) of
            (_::_, False) -> " " ++ strParametersDefault
            _             -> strParametersDefault
      in
      case letKind of
        Let ->
          wsBefore.val
            ++ "let"
            ++ unparsePattern name
            -- NOTE: to help with converting Little to Elm
            -- ++ String.concat (List.map unparsePattern parameters)
            ++ strParameters
            ++ " ="
            ++ unparse binding
            ++ " in"
            ++ unparse body

        Def ->
          wsBefore.val
            -- NOTE: to help with converting Little to Elm
            -- ++ unparsePattern name
            ++ ( let
                   strName =
                     unparsePattern name
                 in
                   if String.startsWith " " strName
                     then String.dropLeft 1 strName
                     else strName
               )
            -- NOTE: to help with converting Little to Elm
            -- ++ String.concat (List.map unparsePattern parameters)
            ++ strParameters
            ++ " ="
            ++ unparse binding
            ++ wsBeforeSemicolon.val
            ++ ";"
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

    Lang.EColonType wsBefore term wsBeforeColon typ wsAfter ->
      wsBefore.val
        ++ "("
        ++ unparse term
        ++ wsBeforeColon.val
        ++ ":"
        ++ unparseType typ
        ++ wsAfter.val
        ++ ")"

    Lang.ETyp _ name typ rest wsBeforeColon ->
      unparsePattern name
        ++ wsBeforeColon.val
        ++ ":"
        ++ unparseType typ
        ++ unparse rest

    Lang.ETypeAlias wsBefore pat typ rest _ ->
      wsBefore.val
        ++ "type alias"
        ++ unparsePattern pat
        ++ " ="
        ++ unparseType typ
        ++ unparse rest

    Lang.ETypeCase _ _ _ _ ->
      "{Error: typecase not yet implemented for Elm syntax}" -- TODO
