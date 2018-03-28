module ElmUnparser exposing
  ( unparse
  , unparsePattern
  , unparseType -- Experimental
  )

import Lang exposing (..)
import ElmLang
import ElmParser
import BinaryOperatorParser
import Utils
import Regex

--------------------------------------------------------------------------------
-- Simple Values
--------------------------------------------------------------------------------

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
      quoteChar ++ Regex.replace Regex.All (Regex.regex <| "\\\\|" ++ quoteChar ++ "|\r|\n|\t") ( -- EStrings are not multiline.
        \{match} -> if match == "\\" then "\\\\" else if match == "\n" then "\\n" else if match == "\r" then "\\r" else if match == "\t" then "\\t" else "\\" ++ quoteChar)
        text ++
      quoteChar

    ENull ->
      "null"

--------------------------------------------------------------------------------
-- Records as sugar for other types with `Lang.recordConstructorName`
--------------------------------------------------------------------------------

expName : Exp -> Maybe String
expName e =
  case e.val.e__ of
    EBase _ baseVal ->
      case baseVal of
        EString _ name ->
          Just name
        _ ->
          Nothing
    _ ->
      Nothing

patName : Pat -> Maybe String
patName p =
  case p.val.p__ of
    PBase _ baseVal ->
      case baseVal of
        EString _ name ->
          Just name
        _ ->
          Nothing
    _ ->
      Nothing

-- Tries to unparse a record as a tuple
tryUnparseTuple
  :  (t -> String) -> (t -> Maybe String)
  -> WS -> List (WS, WS, Ident, WS, t) -> WS
  -> (() -> String) -> String
tryUnparseTuple unparseTerm name wsBefore elems wsBeforeEnd default =
  let
    pairs =
      List.map (\(_, _, key, _, val) -> (key, val)) elems
  in
    if
      pairs
        |> Utils.maybeFind Lang.recordConstructorName
        |> Maybe.andThen name
        |> Maybe.map (String.startsWith "Tuple")
        |> Maybe.withDefault False
    then
      let
        inside =
          elems
            |> List.filter
                 ( \(_, _, elName, _, _) ->
                     String.startsWith "_" elName
                 )
            |> List.sortBy
                 ( \(_, _, elName, _, _) ->
                     elName
                       |> String.dropLeft 1
                       |> String.toInt
                       |> Result.withDefault -1
                 )
            |> List.map
                 ( \(wsBeforeComma, _, _, _, elBinding) ->
                     wsBeforeComma.val ++ "," ++ unparseTerm elBinding
                 )
            |> String.concat
      in
        wsBefore.val
          ++ "("
          ++ String.dropLeft 1 inside -- removes first comma
          ++ wsBeforeEnd.val
          ++ ")"
    else
      default ()

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

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

    PWildcard wsBefore ->
      wsBefore.val
        ++ "_"

    PParens wsBefore pat wsBeforeEnd ->
      wsBefore.val
        ++ "("
        ++ unparsePattern pat
        ++ wsBeforeEnd.val
        ++ ")"

    PList wsBefore members _ Nothing wsBeforeEnd ->
      wsBefore.val
        ++ "["
        ++ String.join "," (List.map unparsePattern members)
        ++ wsBeforeEnd.val
        ++ "]"

    PList wsBefore [head] wsMiddle (Just tail) wsBeforeEnd ->
      wsBefore.val -- Should be empty
        ++ wrapPatternWithParensIfLessPrecedence p head (unparsePattern head)
        ++ wsMiddle.val
        ++ "::"
        ++ wrapPatternWithParensIfLessPrecedence p tail (unparsePattern tail)
        ++ wsBeforeEnd.val -- Should be empty

    PList wsBefore (head::headTail) wsMiddle (Just tail) wsBeforeEnd ->
      unparsePattern <| replaceP__ p <| PList wsBefore [head] wsMiddle (Just (replaceP__ p <| PList wsBefore headTail wsMiddle (Just tail) wsBeforeEnd)) wsBeforeEnd

    PList _ [] wsMiddle (Just tail) _ ->
      unparsePattern tail

    PRecord wsBefore elems wsAfter ->
      tryUnparseTuple unparsePattern patName wsBefore elems wsAfter <| \_ ->
        let maybeJustKey eqSpace key value =
          let default = eqSpace ++ "=" ++ unparsePattern value in
          if eqSpace == "" then
            case value.val.p__ of
              PVar _ name _ -> if name == key then "" else default
              _ -> default
          else default
        in
        wsBefore.val
          ++ "{"
          ++ (case elems of
                [] -> ""
                (wsComma, wsKey, key, wsEq, value)::tail ->
                  wsKey.val ++ key ++ maybeJustKey wsEq.val key value ++
                  String.concat (List.map (\(wsComma, wsKey, key, wsEq, value) ->
                     (if String.contains "\n" wsComma.val && wsKey.val == "" then wsComma.val else wsComma.val ++ ",") ++
                     wsKey.val ++ key ++ maybeJustKey wsEq.val key value
                  ) tail)
          )
          ++ wsAfter.val
          ++ "}"

    PAs wsName name wsBeforeAs pat ->
      wrapPatternWithParensIfLessPrecedence p pat (unparsePattern pat)
        ++ wsBeforeAs.val
        ++ "as"
        ++ wsName.val
        ++ name

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
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
    TRecord wsBefore mb elems wsAfter ->
      wsBefore.val
        ++ "{"
        ++ (case mb of
          Just (ident, wsIdent) -> ident ++ wsIdent.val
          Nothing -> ""
        ) ++ (case elems of
              [] -> ""
              (wsComma, wsKey, key, wsEq, value)::tail ->
                wsKey.val ++ key ++ wsEq.val ++ "=" ++ unparseType value ++
                String.concat (List.map (\(wsComma, wsKey, key, wsEq, value) ->
                   (if String.contains "\n" wsComma.val && wsKey.val == "" then wsComma.val else wsComma.val ++ ",") ++
                   wsKey.val ++ key ++ wsEq.val ++ "=" ++ unparseType value
                ) tail)
        )
        ++ wsAfter.val
        ++ "}"
    TArrow ws1 typeList ws2 -> ws1.val ++ "(->" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TUnion ws1 typeList ws2 -> ws1.val ++ "(union" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TApp ws1 "Num" _        -> ws1.val ++ "Bad_NUM"
    TApp ws1 "Bool" _       -> ws1.val ++ "Bad_BOOL"
    TApp ws1 "String" _     -> ws1.val ++ "Bad_STRING"
    TApp ws1 "Null" _       -> ws1.val ++ "Bad_NULL"
    TApp ws1 ident ts       -> ws1.val ++ ident ++ String.concat (List.map unparseType ts)
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

--------------------------------------------------------------------------------
-- Operators
--------------------------------------------------------------------------------

unparseOp : Op -> String
unparseOp op =
  case op.val of
    Pi ->
      "pi"
    DictEmpty ->
      "empty"
    DictFromList ->
      "dict"
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
    ToStrExceptStr ->
      "ToStrExceptStr"
    RegexReplaceAllIn ->
      "replaceAllIn"
    RegexReplaceFirstIn ->
      "replaceFirstIn"
    RegexExtractFirstIn ->
      "extractFirstIn"

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

unparseBranch : Bool -> Branch -> String
unparseBranch   isNotFirst branch =
  case branch.val of
    Branch_ wsBefore p e wsBeforeArrow ->
      (if isNotFirst && not (String.contains "\n" wsBefore.val)
       then ";" ++ wsBefore.val
       else wsBefore.val)
        ++ unparsePattern p
        ++ wsBeforeArrow.val
        ++ "->"
        ++ unparse e

unparseBranches : List Branch -> String
unparseBranches =
  let aux isNotFirst branches =
      case branches of
        [] -> ""
        head::tail -> unparseBranch isNotFirst head ++ aux True tail
  in aux False


wrapWithTightParens : String -> String
wrapWithTightParens unparsed =
  let
    trimmed    = String.trimLeft unparsed
    lengthDiff = String.length unparsed - String.length trimmed
    ws         = String.left lengthDiff unparsed
  in
  ws ++ "(" ++ trimmed ++ ")"

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

    EFun wsBefore parameters functionBinding _ ->
      let default =
        wsBefore.val
        ++ "\\"
        ++ String.concat (List.map unparsePattern parameters)
        ++ " ->"
        ++ unparse functionBinding
      in
      case parameters of
        [p] -> case p.val.p__ of
          PVar _ " $implicitcase" _ -> case functionBinding.val.e__ of
            ECase wsBefore examinedExpression branches wsBeforeOf -> case examinedExpression.val.e__ of
              EVar _ " $implicitcase" ->
                unparse (replaceE__ functionBinding <| ECase wsBefore (replaceE__ examinedExpression <| EVar space0 "") branches wsBeforeOf)
              _ -> default
            _ -> default
          _ -> default
        _ -> default

    EApp wsBefore function arguments appType _ ->
      -- Not just for help converting Little to Elm. Allows synthesis ot not have to worry about so many cases.
      let unparseArg e =
        case e.val.e__ of
           EApp _ _ _ _ _       -> wrapWithTightParens (unparse e)
           EOp _ _ _ _          -> wrapWithTightParens (unparse e)
           EColonType _ _ _ _ _ -> wrapWithTightParens (unparse e)
           _                    -> unparse e
      in
      case appType of
        SpaceApp ->
          wsBefore.val
            ++ unparse function
            -- NOTE: to help with converting Little to Elm
            -- ++ String.concat (List.map unparse arguments)
            ++ String.concat (List.map unparseArg arguments)
        LeftApp lws ->
          case arguments of
            [arg] ->
              wsBefore.val
                ++ wrapWithParensIfLessEqPrecedence e function (unparse function)
                ++ lws.val
                ++ "<|"
                ++ unparse arg
            _ -> "?[internal error EApp LeftApp wrong number of arguments]?"
        RightApp rws ->
          case arguments of
            [arg] ->
              wsBefore.val
                ++ unparse arg
                ++ rws.val
                ++ "|>"
                ++ wrapWithParensIfLessEqPrecedence e function (unparse function)
            _ -> "?[internal error EApp RightApp did not have exactly 1 argument]?"
        InfixApp ->
          case arguments of
            [arg1, arg2] ->
              wsBefore.val
                ++ wrapWithParensIfLessEqPrecedence e arg1 (unparse arg1)
                ++ unparse function
                ++ wrapWithParensIfLessEqPrecedence e arg2 (unparse arg2)
            _ -> "?[internal error EApp InfixApp did not have exactly 2 arguments]?"

    EOp wsBefore op arguments _ ->
      let
        default =
          wsBefore.val
            ++ unparseOp op
            ++ String.concat (List.map (\x -> wrapWithParensIfLessPrecedence e x (unparse x)) arguments)
      in
        if ElmLang.isInfixOperator op then
          case arguments of
            [ left, right ] ->
              wrapWithParensIfLessPrecedence e left (unparse left)
                ++ wsBefore.val
                ++ unparseOp op
                ++ wrapWithParensIfLessPrecedence e right (unparse right)
            _ ->
              default
        else
          default

    EList wsBefore members wsmiddle Nothing wsBeforeEnd ->
      wsBefore.val
        ++ "["
        ++ ( case members of
               [] ->
                 ""
               (_,e1) :: eRest ->
                 unparse e1
                   ++ String.concat (List.map (\(wsI,eI) -> wsI.val ++ "," ++ unparse eI) eRest)
           )
        ++ wsBeforeEnd.val
        ++ "]"

    EList wsBefore [(_,head)] wsmiddle (Just tail) wsBeforeEnd ->
      wsBefore.val -- Should be empty
        ++ wrapWithParensIfLessPrecedence e head (unparse head)
        ++ wsmiddle.val
        ++ "::"
        ++ wrapWithParensIfLessPrecedence e tail (unparse tail)
        ++ wsBeforeEnd.val -- Should be empty

    -- Obsolete, remove whenever we are sure that there are no mutliple cons anymore.
    EList wsBefore (head::headtail) wsmiddle (Just tail) wsBeforeEnd ->
      unparse <| replaceE__ e <| EList wsBefore [head] wsmiddle (Just <| replaceE__ e <| EList wsBefore headtail wsmiddle (Just tail) wsBeforeEnd) wsBeforeEnd

    EList wsBefore [] wsMiddle (Just tail) wsBeforeEnd ->
      unparse tail

    ERecord wsBefore mi elems wsAfter ->
      tryUnparseTuple unparse expName wsBefore elems wsAfter <| \_ ->
        wsBefore.val
          ++ "{"
          ++ (case mi of
                Just (m, ws) -> unparse m ++ ws.val ++ "|"
                Nothing -> ""
          )
          ++ (case elems of
                [] -> ""
                (wsComma, wsKey, key, wsEq, value)::tail ->
                  let (strParameters, binding) = getParametersBinding value in
                  wsComma.val ++ wsKey.val ++ key ++ strParameters ++ wsEq.val ++ "=" ++ unparse binding ++
                  String.concat (List.map (\(wsComma, wsKey, key, wsEq, value) ->
                    let (strParameters, binding) = getParametersBinding value in
                    (if String.contains "\n" wsComma.val && wsKey.val == "" then wsComma.val else wsComma.val ++ ",") ++
                    wsKey.val ++ key ++ strParameters ++ wsEq.val ++ "=" ++ unparse binding
                  ) tail)
          )
          ++ wsAfter.val
          ++ "}"

    ESelect ws0 exp wsBeforeDot wsAfterDot id ->
      ws0.val ++ unparse exp ++ wsBeforeDot.val ++ "." ++ wsAfterDot.val ++ id

    EIf wsBefore condition wsBeforeTrue trueBranch wsBeforeElse falseBranch _ ->
      wsBefore.val
        ++ "if"
        ++ unparse condition
        ++ wsBeforeTrue.val ++ "then"
        ++ unparse trueBranch
        ++ wsBeforeElse.val ++ "else"
        ++ unparse falseBranch

    ECase wsBefore examinedExpression branches wsBeforeOf ->
      wsBefore.val
        ++ "case"
        ++ unparse examinedExpression
        ++ wsBeforeOf.val
        ++ "of"
        ++ unparseBranches branches

    ELet wsBefore letKind isRec name wsBeforeEq binding_ wsBeforeInOrSemi body _ ->
        let (strParameters, binding) = getParametersBinding binding_ in
        case letKind of
          Let ->
            case name.val.p__ of
              PVar _ "_IMPLICIT_MAIN" _ ->
                ""

              _ ->
                wsBefore.val
                  ++ (if isRec then "letrec" else "let")
                  ++ unparsePattern name
                  -- NOTE: to help with converting Little to Elm
                  -- ++ String.concat (List.map unparsePattern parameters)
                  ++ strParameters
                  ++ wsBeforeEq.val ++ "="
                  ++ unparse binding
                  ++ wsBeforeInOrSemi.val ++ "in"
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
              ++ wsBeforeEq.val ++ "="
              ++ unparse binding
              ++ wsBeforeInOrSemi.val
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

    EParens wsBefore innerExpression pStyle wsAfter ->
      case pStyle of
        Parens ->
          wsBefore.val
            ++ "("
            ++ unparse innerExpression
            ++ wsAfter.val
            ++ ")"
        LongStringSyntax ->
          wsBefore.val
            ++ "\"\"\""
            ++ multilineContentUnparse innerExpression
            ++ "\"\"\""
        ElmSyntax -> -- We just unparse the inner expression as regular parentheses
          unparse <| replaceE__ e <| EParens wsBefore innerExpression Parens wsAfter

    EHole wsBefore val ->
      wsBefore.val
        ++ "??"

    EColonType wsBefore term wsBeforeColon typ wsAfter ->
      wsBefore.val
        ++ unparse term
        ++ wsBeforeColon.val
        ++ ":"
        ++ unparseType typ
        ++ wsAfter.val

    ETyp _ name typ rest wsBeforeColon ->
      unparsePattern name
        ++ wsBeforeColon.val
        ++ ":"
        ++ unparseType typ
        ++ unparse rest

    ETypeAlias wsBefore pat typ rest _ ->
      wsBefore.val
        ++ "type alias"
        ++ unparsePattern pat
        ++ " ="
        ++ unparseType typ
        ++ unparse rest

    ETypeDef wsBefore (wsBeforeIdent, ident) vars wsBeforeEqual dcs rest _ ->
      let
        unparsedVars =
          String.concat <|
            List.map
              ( \(ws, name) ->
                  ws.val ++ name
              )
              vars
        unparsedDcs =
          String.concat <|
            List.intersperse "|" <|
              List.map
                ( \(ws1, name, ts, ws2) ->
                    ws1.val
                      ++ name
                      ++ String.concat (List.map unparseType ts)
                      ++ ws2.val
                )
                dcs
      in
        wsBefore.val
          ++ "type"
          ++ wsBeforeIdent.val
          ++ ident
          ++ unparsedVars
          ++ wsBeforeEqual.val
          ++ "="
          ++ unparsedDcs
          ++ unparse rest

    ETypeCase _ _ _ _ ->
      "{Error: typecase not yet implemented for Elm syntax}" -- TODO

-- If the expression is a EFun, prints the lists of parameters and returns its body.
getParametersBinding: Exp -> (String, Exp)
getParametersBinding binding_  =
  let (parameters, binding) =
    case binding_.val.e__ of
       EFun _ parameters functionBinding _ ->
         let default = (parameters, functionBinding) in
         case parameters of
            [p] -> case p.val.p__ of
              PVar _ " $implicitcase" _ -> case functionBinding.val.e__ of
                ECase wsBefore examinedExpression branches wsBeforeOf -> case examinedExpression.val.e__ of
                  EVar _ " $implicitcase" ->
                    ([], binding_)
                  _ -> default
                _ -> default
              _ -> default
            _ -> default
       _ ->
        ([], binding_)
  in
  let strParametersDefault =
        String.concat (List.map unparsePattern parameters)
  in
  let strParameters = --To help to convert from little to Elm
     case (parameters, String.startsWith " " strParametersDefault) of
       (_::_, False) -> " " ++ strParametersDefault
       _             -> strParametersDefault
  in
  (strParameters, binding)

multilineRegexEscape = Regex.regex <| "@"

--Parses and unparses long interpolated strings.
multilineContentUnparse : Exp -> String
multilineContentUnparse e = case e.val.e__ of
  EBase sp0 (EString _ s) ->
    Regex.replace  Regex.All multilineRegexEscape (\m -> "@@") s
  EOp sp1 op [left, right] sp2 ->
    case op.val of
      Plus ->
        let unwrapToStrExceptStr x =
          case x of
            EOp spm1 op [arg] spm2 -> case op.val of
              ToStrExceptStr -> arg.val.e__
              _ -> x
            _ -> x
        in
        case unwrapToStrExceptStr left.val.e__ of
          EBase sp0 (EString _ s) ->
            multilineContentUnparse left ++ multilineContentUnparse right
          EVar sp0 ident as left ->
            case right.val.e__ of
              EOp sp2 op2 [left2, _] sp4->
                case op2.val of
                  Plus ->
                    case left2.val.e__ of
                      EBase sp3 (EString _ s) ->
                        let varRep = case String.uncons s of
                          Nothing -> "@" ++ ident
                          Just (c, r) -> if ElmParser.isRestChar c then "@(" ++ ident ++ ")" else "@" ++ ident
                        in
                        varRep ++ multilineContentUnparse right
                      _ -> "@" ++ ident ++ multilineContentUnparse right
                  _ -> "@" ++ ident ++ multilineContentUnparse right
              EBase sp3 (EString _ s) ->
                let varRep = case String.uncons s of
                  Nothing -> "@" ++ ident
                  Just (c, r) -> if ElmParser.isRestChar c then "@(" ++ ident ++ ")" else "@" ++ ident
                in
                varRep ++ multilineContentUnparse right
              _ -> "@" ++ ident ++ multilineContentUnparse right
          EParens sp0 innerElmExp ElmSyntax sp4 ->
            "@" ++ unparse innerElmExp ++ multilineContentUnparse right
          _ -> "@(" ++ unparse left ++ ")" ++ multilineContentUnparse right
      _ -> "@(" ++ unparse e ++ ")"
  ELet ws1 kind rec p ws2 e1 ws3 e2 ws4 ->
    let remaining = multilineContentUnparse e2 in
    let definition = unparse e1 in
    let finalDefinition = if String.contains "\n" definition then
      case String.uncons (String.trim definition) of
        Just ('(', _) -> definition ++ "\n"
        _ -> "(" ++ definition ++ ")\n"
      else definition ++ "\n" in
    "@" ++ (case kind of
      Let -> "let"
      Def -> "def"
    ) ++ (if rec then "rec" else "") ++ ws1.val ++ unparsePattern p ++ ws2.val ++ "=" ++ finalDefinition ++ remaining
  anyExp -> "@(" ++ unparse e ++ ")"


getExpPrecedence: Exp -> Int
getExpPrecedence exp =
  case exp.val.e__ of
    EList _ [head] _ (Just tail) _ -> 5
    EApp _ _ _ (LeftApp _) _       -> 0
    EApp _ _ _ (RightApp _) _      -> 0
    EApp _ f _ (InfixApp) _        ->
      case f.val.e__ of
        EVar _ name ->
          case BinaryOperatorParser.getOperatorInfo name ElmParser.builtInPrecedenceTable of
            Nothing -> 10
            Just (associativity, precedence) -> precedence
        _ -> 0
    EColonType _ _ _ _ _           -> -1
    EOp _ operator _ _ ->
      case BinaryOperatorParser.getOperatorInfo (unparseOp operator) ElmParser.builtInPrecedenceTable of
        Nothing -> 10
        Just (associativity, precedence) -> precedence
    _ -> 10

wrapWithParensIfLessPrecedence: Exp -> Exp -> String -> String
wrapWithParensIfLessPrecedence outsideExp insideExp unparsedInsideExpr =
  if getExpPrecedence insideExp < getExpPrecedence outsideExp then wrapWithTightParens unparsedInsideExpr else unparsedInsideExpr

wrapWithParensIfLessEqPrecedence: Exp -> Exp -> String -> String
wrapWithParensIfLessEqPrecedence outsideExp insideExp unparsedInsideExpr =
  if getExpPrecedence insideExp <= getExpPrecedence outsideExp then wrapWithTightParens unparsedInsideExpr else unparsedInsideExpr

getPatPrecedence: Pat -> Int
getPatPrecedence pat =
  case pat.val.p__ of
    PList _ _ _ (Just tail) _ ->
      case BinaryOperatorParser.getOperatorInfo "::" ElmParser.builtInPatternPrecedenceTable of
        Nothing -> 10
        Just (associativity, precedence) -> precedence
    PAs _ _ _ _ ->
      case BinaryOperatorParser.getOperatorInfo "as" ElmParser.builtInPatternPrecedenceTable of
        Nothing -> 10
        Just (associativity, precedence) -> precedence
    _ -> 10

wrapPatternWithParensIfLessPrecedence: Pat -> Pat -> String -> String
wrapPatternWithParensIfLessPrecedence outsidePat insidePat unparsedInsidePat =
  if getPatPrecedence insidePat < getPatPrecedence outsidePat then wrapWithTightParens unparsedInsidePat else unparsedInsidePat

wrapPatternWithParensIfLessEqPrecedence: Pat -> Pat -> String -> String
wrapPatternWithParensIfLessEqPrecedence outsidePat insidePat unparsedInsidePat =
  if getPatPrecedence insidePat <= getPatPrecedence outsidePat then wrapWithTightParens unparsedInsidePat else unparsedInsidePat
