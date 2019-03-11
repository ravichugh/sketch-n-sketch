module LeoUnparser exposing
  ( unparse
  , unparseOp
  , unparsePattern
  , unparseType -- Experimental
  , unparseLongStringContent
  , unparseHtmlTextContent
  , unparseAnyHtml
  , HtmlInterpolationStyle(..)
  )

import Lang exposing (..)
import LeoLang
import LeoParser
import LangParserUtils
import BinaryOperatorParser
import Utils
import Regex
import ParserUtils exposing (unparseStringContent)
import ImpureGoodies
import HTMLParser
import Char

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
      quoteChar ++ unparseStringContent quoteChar text ++ quoteChar

    ENull ->
      "null"

--------------------------------------------------------------------------------
-- Records as sugar for other types with `Lang.recordConstructorName`
--------------------------------------------------------------------------------

-- Tries to unparse a record as a tuple
tryUnparseTuple
  :  (t -> String)
  -> WS -> List (Maybe WS, WS, String, WS, t) -> WS
  -> Maybe String
tryUnparseTuple unparseTerm wsBefore keyValues wsBeforeEnd =
  case tupleEncodingUnapply keyValues of
    Nothing -> Nothing
    Just insideValues ->
      let
        inside =
            insideValues
            |> List.map
                 (\(wsBeforeComma, elBinding) ->
                   (wsBeforeComma |> Maybe.map (\spc -> spc.val ++ ",") |> Maybe.withDefault "") ++ unparseTerm elBinding
                 )
            |> String.join ""
      in
        Just <|
          wsBefore.val
            ++ "("
            ++ inside
            ++ wsBeforeEnd.val
            ++ ")"

getKeyValuesFromDecls: Declarations -> Maybe (List (String, Exp))
getKeyValuesFromDecls (Declarations po _ _ letexpsGroups) =
  letexpsGroups |> elemsOf
  |> List.map (\(LetExp os sp pat fs spe val) ->
       case pat.val.p__ of
         PVar _ key _ -> Just (key, val)
         _ -> Nothing)
  |> Utils.projJusts

getDataConstructorNameString keyValues =
  let ctorString = stringifyCtorKind Lang.DataTypeCtor in
  keyValues |> Utils.maybeFind ctorString

getArgConstructorNameString keyValues =
  keyValues |> Utils.maybeFind ctorArgs

-- Tries to unparse a record as a data constructor
-- (Has redundancy with Lang.dataTypeEncodingUnapply)
tryUnparseDataConstructor
  :  (t -> String) -> (t -> Maybe String) -> (t -> Maybe (List (Maybe WS, WS, Ident, WS, t)))
  -> WS -> List (Maybe WS, WS, String, WS, t) -> WS
  -> Maybe String
tryUnparseDataConstructor unparseTerm name args wsBefore fields wsBeforeEnd =
  let
    keyValues =
      Utils.recordKeyValuePairs fields

    maybeNameString = getDataConstructorNameString keyValues
        |> Maybe.andThen name

    maybeArgs =
      keyValues |> Utils.maybeFind Lang.ctorArgs
      |> Maybe.andThen args
  in
    case (maybeNameString, maybeArgs) of
      (Just nameString, Just args) ->
        let
          argsString =
            args
              |> Lang.ensureRecordNumericArgEntriesInOrder
              |> List.map
                   ( \(_, _, _, _, elBinding) ->
                       unparseTerm elBinding
                   )
              |> String.concat
        in
          Just <|
            wsBefore.val
              ++ nameString
              ++ argsString

      _ ->
        Nothing

tryUnparseRecordSugars
  :  (t -> String)
  -> (t -> Maybe String)
  -> (t -> Maybe (List (Maybe WS, WS, Ident, WS, t)))
  -> WS -> List (Maybe WS, WS, String, WS, t) -> WS
  -> (() -> String) -> String
tryUnparseRecordSugars unparseTerm name args wsBefore decls wsBeforeEnd default =
  case tryUnparseTuple unparseTerm wsBefore decls wsBeforeEnd of
    Just s ->
      s
    Nothing ->
      case tryUnparseDataConstructor unparseTerm name args wsBefore decls wsBeforeEnd of
        Just s ->
          s
        Nothing ->
          default ()

--------------------------------------------------------------------------------
-- Patterns
--------------------------------------------------------------------------------

unparseTypePattern: TPat -> String
unparseTypePattern p =
  case p.val of
    TPatVar ws ident ->
      ws.val ++ ident

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
        ++ wrapPatternWithParensIfLessPrecedence OpLeft p head (unparsePattern head)
        ++ wsMiddle.val
        ++ "::"
        ++ wrapPatternWithParensIfLessPrecedence OpRight p tail (unparsePattern tail)
        ++ wsBeforeEnd.val -- Should be empty

    PList wsBefore (head::headTail) wsMiddle (Just tail) wsBeforeEnd ->
      unparsePattern <| replaceP__ p <| PList wsBefore [head] wsMiddle (Just (replaceP__ p <| PList wsBefore headTail wsMiddle (Just tail) wsBeforeEnd)) wsBeforeEnd

    PList _ [] wsMiddle (Just tail) _ ->
      unparsePattern tail

    PRecord wsBefore elems wsAfter ->
      tryUnparseRecordSugars unparsePattern Lang.getPatString Lang.getPatEntries wsBefore elems wsAfter <| \_ ->
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
          ++ String.concat (List.map (\(mbWsComma, wsKey, key, wsEq, value) ->
               (mbWsComma |> Maybe.map (\wsComma -> wsComma.val ++ ",") |> Maybe.withDefault "") ++
               wsKey.val ++ key ++ maybeJustKey wsEq.val key value) elems)
          ++ wsAfter.val
          ++ "}"

    PAs wsBefore p1 wsBeforeAs p2 ->
      wsBefore.val
        ++ wrapPatternWithParensIfLessPrecedence OpLeft p p1 (unparsePattern p1)
        ++ wsBeforeAs.val
        ++ "as"
        ++ wrapPatternWithParensIfLessPrecedence OpLeft p p2 (unparsePattern p2)
    PColonType wsBefore p1 wsBeforeColon t ->
      wsBefore.val
        ++ wrapPatternWithParensIfLessPrecedence OpLeft p p1 (unparsePattern p1)
        ++ wsBeforeColon.val
        ++ ":"
        ++ unparseType t


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- TODO

unparseType : Type -> String
unparseType tipe =
  case tipe.val.t__ of
    TNum ws                   -> ws.val ++ "Num"
    TBool ws                  -> ws.val ++ "Bool"
    TString ws                -> ws.val ++ "String"
    TNull ws                  -> ws.val ++ "Null"
    TList ws1 tipe ws2        -> ws1.val ++ "List" ++ (unparseType tipe) ++ ws2.val
    TDict ws1 tipe1 tipe2 ws2 -> ws1.val ++ "Dict" ++ (unparseType tipe1) ++ (unparseType tipe2) ++ ws2.val
    TTuple ws1 typeList ws2 maybeRestType ws3 ->
      case maybeRestType of
        Just restType -> ws1.val ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ "|" ++ (unparseType restType) ++ ws3.val ++ "]"
        Nothing       -> ws1.val ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws3.val ++ "]"
    TRecord wsBefore mb elems wsAfter ->
      tryUnparseRecordSugars unparseType Lang.getTypeString Lang.getTypeEntries wsBefore elems wsAfter <| \_ ->
      wsBefore.val
        ++ "{"
        ++ (case mb of
          Just (ident, wsIdent) -> ident ++ wsIdent.val
          Nothing -> ""
        ) ++ String.join "," (List.map (\(mbWsComma, wsKey, key, wsEq, value) ->
                (mbWsComma |> Maybe.map .val |> Maybe.withDefault "") ++ wsKey.val ++
                key ++ wsEq.val ++ ":" ++ unparseType value
             ) elems)
        ++ wsAfter.val
        ++ "}"
    TArrow ws1 typeList ws2 -> ws1.val ++ "(->" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TUnion ws1 typeList ws2 -> ws1.val ++ "(union" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TApp ws1 name ts appType->
      case (appType, ts) of
        (LeftApp ws, [tArg]) ->
          ws1.val ++ unparseType name ++ ws.val ++ "<|" ++ unparseType tArg
        (RightApp ws, [tArg]) ->
           ws1.val ++ unparseType tArg ++ ws.val ++ "|>" ++ unparseType name
        (InfixApp, [arg1, arg2]) ->
           ws1.val ++ unparseType arg1 ++ unparseType name ++ unparseType arg2
        (_, _) -> --SpaceApp
          ws1.val ++ unparseType name ++ String.concat (List.map unparseType ts)

    TVar ws1 ident          -> ws1.val ++ ident
    TWildcard ws            -> ws.val ++ "_"
    TForall ws1 vars tipe1 ws2 ->
      let sVars = String.concat (List.map unparseTypePattern vars)
      in
      ws1.val ++ "forall" ++ sVars ++ ws2.val ++ "." ++ unparseType tipe1
    TParens ws1 t ws2 ->
      ws1.val ++ "(" ++ unparseType t ++ ws2.val ++ ")"

--------------------------------------------------------------------------------
-- Operators
--------------------------------------------------------------------------------

unparseOp : Op -> String
unparseOp op =
  case op.val of
    Pi ->
      "pi"
    DictEmpty ->
      "__DictEmpty__"
    CurrentEnv ->
      "__CurrentEnv__"
    DictFromList ->
      "__DictFromList__"
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
      "^"
    ArcTan2 ->
      "arctan2"
    DictInsert ->
      "__DictInsert__"
    DictGet ->
      "__DictGet__"
    DictRemove ->
      "__DictRemove__"
    DebugLog ->
      "debug"
    NoWidgets ->
      "noWidgets"
    ToStrExceptStr ->
      "ToStrExceptStr"
    RegexExtractFirstIn ->
      "extractFirstIn"
    StrLength ->
      "__strLength__"

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
  let aux: Bool -> List Branch -> String
      aux isNotFirst branches =
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
  case (unwrapExp e) of
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

    EFun wsBeforeFun parameters functionBinding _ ->
      let default () =
        wsBeforeFun.val
        ++ "\\"
        ++ String.concat (List.map unparsePattern parameters)
        ++ " ->"
        ++ unparse functionBinding
      in
      case parameters of
        (p::_) -> if pVarUnapply p == Just LeoParser.implicitVarName then
          case (unwrapExp functionBinding) of
            ECase wsBefore examinedExpression branches wsBeforeOf -> if eVarUnapply examinedExpression == Just LeoParser.implicitVarName then
                unparse (replaceE__ functionBinding <| ECase wsBefore (replaceE__ examinedExpression <| EVar space0 "") branches wsBeforeOf)
              else default ()
            ESelect _ selected wsBeforeDot wsAfterDot name ->
              let res = unparse  functionBinding in
              if String.startsWith LeoParser.implicitVarName res then
                wsBeforeFun.val ++ String.dropLeft (String.length LeoParser.implicitVarName) res
              else
                let _ = Debug.log ("Could not find the implicit var name at the start of '" ++ res ++ "' reverting to default") () in
                default ()
            ERecord wsBefore Nothing (Declarations _ [] [] elems) wsBeforeEnd ->
              wsBeforeFun.val ++ "(" ++ String.repeat (List.length elems - 2) "," ++ ")"
            EOp wsBefore wsOp op args wsBeforeEnd ->
              {-if LeoLang.arity op == List.length args then
                wsBefore.val ++
                unparseOp op
              else-} -- Won't work correctly, e.G. (+) will be displayed as "+"
              default ()
            _ -> default ()
          else default ()
        _ -> default ()

    EApp wsBefore function arguments appType _ ->
      -- Not just for help converting Little to Leo. Allows synthesis ot not have to worry about so many cases.
      let unparseArg e =
        case (unwrapExp e) of
           EApp _ _ _ _ _       -> wrapWithTightParens (unparse e)
           EOp _ _ _ _ _        -> wrapWithTightParens (unparse e)
           EColonType _ _ _ _ _ -> wrapWithTightParens (unparse e)
           _                    -> unparse e
      in
      case appType of
        SpaceApp ->

          (++) wsBefore.val <|
               Utils.foldLeft (unparse function) arguments <|
                 \currentRendering arg ->
                   let argStr = unparseArg arg in
                   let mbWrapArg = if Utils.wouldNotRecognizeTokenSplit currentRendering argStr then
                     "(" ++ argStr ++ ")"
                     else argStr
                   in
                   currentRendering ++ mbWrapArg
            -- NOTE: to help with converting Little to Leo
            -- ++ String.concat (List.map unparse arguments)
        LeftApp lws ->
          case arguments of
            [arg] ->
              wsBefore.val
                ++ wrapWithParensIfLessPrecedence OpLeft e function (unparse function)
                ++ lws.val
                ++ "<|"
                ++ wrapWithParensIfLessPrecedence OpRight e arg (unparse arg)
            _ -> "?[internal error EApp LeftApp wrong number of arguments]?"
        RightApp rws ->
          case arguments of
            [arg] ->
              wsBefore.val
                ++ wrapWithParensIfLessPrecedence OpLeft e arg (unparse arg)
                ++ rws.val
                ++ "|>"
                ++ wrapWithParensIfLessPrecedence OpRight e function (unparse function)
            _ -> "?[internal error EApp RightApp did not have exactly 1 argument]?"
        InfixApp ->
          case arguments of
            [arg1, arg2] ->
              wsBefore.val
                ++ wrapWithParensIfLessPrecedence OpLeft e arg1 (unparse arg1)
                ++ unparse function
                ++ wrapWithParensIfLessPrecedence OpRight e arg2 (unparse arg2)
            _ -> "?[internal error EApp InfixApp did not have exactly 2 arguments]?"

    EOp wsBefore wsOp op arguments _ ->
      let
        default =
          wsBefore.val
            ++ wsOp.val
            ++ unparseOp op
            ++ String.concat (List.map (\x -> wrapWithParensIfLessPrecedence OpRight e x (unparse x)) arguments)
      in
        if LeoLang.isInfixOperator op then
          case arguments of
            [ left, right ] ->
              wsBefore.val
                ++ wrapWithParensIfLessPrecedence OpLeft e left (unparse left)
                ++ wsOp.val
                ++ unparseOp op
                ++ wrapWithParensIfLessPrecedence OpRight e right (unparse right)
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
        ++ wrapWithParensIfLessPrecedence OpLeft e head (unparse head)
        ++ wsmiddle.val
        ++ "::"
        ++ wrapWithParensIfLessPrecedence OpRight e tail (unparse tail)
        ++ wsBeforeEnd.val -- Should be empty

    -- Obsolete, remove whenever we are sure that there are no mutliple cons anymore.
    EList wsBefore (head::headtail) wsmiddle (Just tail) wsBeforeEnd ->
      unparse <| replaceE__ e <| EList wsBefore [head] wsmiddle (Just <| replaceE__ e <| EList wsBefore headtail wsmiddle (Just tail) wsBeforeEnd) wsBeforeEnd

    EList wsBefore [] wsMiddle (Just tail) wsBeforeEnd ->
      unparse tail

    ERecord wsBefore mi decls wsAfter ->
      let default = \_ ->
        wsBefore.val
          ++ "{"
          ++ (case mi of
                Just (m, ws) -> unparse m ++ ws.val ++ "|"
                Nothing -> ""
          ) ++ unparseDeclarations decls ++ wsAfter.val ++ "}"
      in
      case recordEntriesFromDeclarations decls of
        Nothing -> default ()
        Just fields ->
          tryUnparseRecordSugars (\arg -> wrapWithParensIfLessPrecedence OpRight e arg (unparse arg)) Lang.getExpString Lang.getExpEntries
              wsBefore fields wsAfter default

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

    ELet wsBefore letKind (Declarations _ _ _ letexpsGroups as decls) wsIn body ->
       if onlyImplicitMain letexpsGroups || letexpsGroups == [] && eVarUnapply body == Just "main" then ""
       else
      (case letKind of
        Let -> wsBefore.val ++ "let"
        Def -> "") ++ unparseDeclarations decls ++ wsIn.val ++
      (case letKind of
        Let -> "in"
        Def -> "") ++ unparse body ++
      (case letKind of
        Let -> ""
        Def -> wsBefore.val)

    EParens wsBefore innerExpression pStyle wsAfter ->
      case pStyle of
        CustomSyntax x -> -- only for foreign parsers/unparsers
          wsBefore.val ++ "{-CustomXyntax " ++ toString x ++ "-}(" ++
          unparse innerExpression ++
          wsAfter.val ++ ")"
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
        HtmlSyntax ->
          wsBefore.val
            ++ unparseHtmlNode Interpolated innerExpression
        LeoSyntax ->
           -- We just unparse the inner expression as regular parentheses
           -- This is normally never called from here.
          unparse innerExpression  --<| replaceE__ e <| EParens wsBefore innerExpression Parens wsAfter

    -- TODO: ESnapHole should probably cause an error instead of unparsing
    EHole wsBefore _ ->
      wsBefore.val
        ++ "??"

    EColonType wsBefore term wsBeforeColon typ wsAfter ->
      wsBefore.val
        ++ unparse term
        ++ wsBeforeColon.val
        ++ ":"
        ++ unparseType typ
        ++ wsAfter.val

onlyImplicitMain letExps = case letExps of
  [(_, [LetExp _ _ name _ _ _])] ->
     case name.val.p__ of
       PVar _ "_IMPLICIT_MAIN" _ -> True
       _ -> False
  _ -> False

unparseDeclarations: Declarations -> String
unparseDeclarations declarations =
   Utils.foldLeft "" (getDeclarationsInOrder declarations) <|
     \acc decl -> (++) acc <| case decl of
       DeclExp (LetExp wsBeforeComma wsBefore p funArgStyle wsEq e2) ->
         (case wsBeforeComma of
           Just ws -> ws.val ++ ","
           Nothing -> "") ++ wsBefore.val ++ unparsePattern p ++
             (let (params, body) = getParametersBinding funArgStyle e2 in
               params ++ wsEq.val ++ "=" ++ unparse body
             )
       DeclAnnotation (LetAnnotation wsBeforeComma wsBefore p funArgStyle wsEq t2) ->
         (case wsBeforeComma of
           Just ws -> ws.val ++ ","
           Nothing -> "") ++ wsBefore.val ++ unparsePattern p ++
             (let (params, body) = getTypeParametersBinding funArgStyle t2 in
               params ++ wsEq.val ++ ":" ++ unparseType body
             )
       DeclType (LetType wsBeforeComma wsBefore wsBeforeAlias p funArgStyle wsEq t2) ->
         (case wsBeforeComma of
           Just ws -> ws.val ++ ","
           Nothing -> "") ++ wsBefore.val ++ "type" ++ (case wsBeforeAlias of
             Just ws -> ws.val ++ "alias"
             Nothing -> "")++ unparsePattern p ++
             (let (params, body) = getTypeParametersBinding funArgStyle t2 in
               params ++ wsEq.val ++ "=" ++ unparseType body
             )


-- If the expression is a EFun, prints the lists of parameters and returns its body.
getParametersBinding: FunArgStyle -> Exp -> (String, Exp)
getParametersBinding funArgStyle binding_  =
  let (parameters, binding) =
    case (funArgStyle, (unwrapExp binding_)) of
       (FunArgAsPats, EFun _ parameters functionBinding _) ->
         let default = (parameters, functionBinding) in
         case parameters of
            [p] -> if pVarUnapply p == Just LeoParser.implicitVarName then
                    ([], binding_)
              else default
            _ -> default
       _ ->
        ([], binding_)
  in
  let strParametersDefault =
        String.concat (List.map unparsePattern parameters)
  in
  let strParameters = --To help to convert from little to Leo
     case (parameters, String.startsWith " " strParametersDefault) of
       (_::_, False) -> " " ++ strParametersDefault
       _             -> strParametersDefault
  in
  (strParameters, binding)

-- If the expression is a EFun, prints the lists of parameters and returns its body.
getTypeParametersBinding: FunArgStyle -> Type -> (String, Type)
getTypeParametersBinding funArgStyle binding_  =
  let (parameters, binding) =
    case (funArgStyle, binding_.val.t__) of
       (FunArgAsPats, TForall _ parameters functionBinding _) ->
         let default = (parameters, functionBinding) in
         case parameters of
            [p] -> if tpVarUnapply p == Just LeoParser.implicitVarName then
                  ([], binding_)
              else default
            _ -> default
       _ ->
        ([], binding_)
  in
  let strParametersDefault =
        String.concat (List.map unparseTypePattern parameters)
  in
  let strParameters = --To help to convert from little to Leo
     case (parameters, String.startsWith " " strParametersDefault) of
       (_::_, False) -> " " ++ strParametersDefault
       _             -> strParametersDefault
  in
  (strParameters, binding)


multilineRegexEscape = Regex.regex <| "@|\"\"\""

unparseLongStringContent s =
  Regex.replace  Regex.All multilineRegexEscape (\m -> case m.match of
    "@" -> "@@"
    "\"\"\"" -> "@(\"\\\"\\\"\\\"\")" -- Interpolation of triple quotes itself
    _ -> m.match) s

--Parses and unparses long interpolated strings.
multilineContentUnparse : Exp -> String
multilineContentUnparse e = case (unwrapExp e) of
  EBase sp0 (EString _ s) ->
    unparseLongStringContent s
  EOp sp1 spOp op [left, right] sp2 ->
    case op.val of
      Plus ->
        let unwrapToStrExceptStr x =
          case eOpUnapply1 ToStrExceptStr x of
             Just arg ->
               case eParensUnapplyIf LeoSyntax arg of
               Just arg ->
                 (arg, (unwrapExp arg))
               Nothing ->  (arg, (unwrapExp arg))
             _ -> (left, (unwrapExp x))
        in
        case unwrapToStrExceptStr left of
          (_, EBase sp0 (EString _ s)) ->
            multilineContentUnparse left ++ multilineContentUnparse right
          (_, EVar sp0 ident as left) ->
            case (unwrapExp right) of
              EOp sp2 spOp2 op2 [left2, _] sp4->
                case op2.val of
                  Plus ->
                    case (unwrapExp left2) of
                      EBase sp3 (EString _ s) ->
                        let varRep = case String.uncons s of
                          Nothing -> "@" ++ ident
                          Just (c, r) -> if LangParserUtils.isRestChar c then "@(" ++ ident ++ ")" else "@" ++ ident
                        in
                        varRep ++ multilineContentUnparse right
                      _ -> "@" ++ ident ++ multilineContentUnparse right
                  _ -> "@" ++ ident ++ multilineContentUnparse right
              EBase sp3 (EString _ s) ->
                let varRep = case String.uncons s of
                  Nothing -> "@" ++ ident
                  Just (c, r) -> if LangParserUtils.isRestChar c then "@(" ++ ident ++ ")" else "@" ++ ident
                in
                varRep ++ multilineContentUnparse right
              _ -> "@" ++ ident ++ multilineContentUnparse right
          (x, _) -> -- There should be parentheses
            let sx = unparse x in
            let sy = multilineContentUnparse right in
            if noInterpolationConflict sx sy then
              "@" ++ sx ++ sy
            else
              "@(" ++ sx ++ ")" ++ sy
      _ -> "@(" ++ unparse e ++ ")"
  ELet ws1 _ decls wsIn e2 ->
    "@let" ++ unparseDeclarations decls ++ wsIn.val ++ "in\n" ++ multilineContentUnparse e2
  anyExp -> "@(" ++ unparse e ++ ")"

noInterpolationConflict varString rightString =
  String.endsWith ")" varString ||
  Regex.contains (Regex.regex "[\\w_\\$]$") varString && Regex.contains (Regex.regex "^[^\\w_\\$\\.]|^$") rightString

------------------------
-- HTML Unparsing
------------------------
dummyExp = withDummyExpInfo <| EApp space0 (eVar "x") [] SpaceApp space0

unparseHtmlAttributes: HtmlInterpolationStyle -> Exp -> String
unparseHtmlAttributes interpolationStyle attrExp =
  case eListUnapply attrExp of
    Just attrs ->
      attrs |> List.map (\attr -> case unwrapExp attr of
        EList attrNameSpace [(atEncoded, attrName), (spBeforeEq, attrValue)] spAfterEq Nothing _ ->
          let atAfterEqual = if atEncoded.val == " " && interpolationStyle /= Raw then "@" else ""  in
          let beforeSpace = if attrNameSpace.val == "" then " " else attrNameSpace.val in
          case unwrapExp attrName of
            EBase _ (EString _ attrNameStr) ->
              let attrValueToConsider = case attrNameStr of -- Removes __mbstylesplit__
                "style" -> eAppUnapply1 attrValue |> Maybe.map Tuple.second |> Maybe.withDefault attrValue
                _ -> attrValue
              in
              let default () =
                let defaultValue () = unparse attrValueToConsider in
                let value = if interpolationStyle == Raw && attrNameStr == "style" then -- Get rid of lists if the name is style
                     case unwrapExp  attrValueToConsider of
                       EList _ elems _ _ _ ->
                         let resUnparsed =
                              List.map (\(ws, elem) -> case unwrapExp elem of
                                EList _ [(_, styleName), (_, styleValue)] _ _ _ ->
                                  case (eStrUnapply styleName, eStrUnapply styleValue) of
                                    (Just sName, Just sValue) -> Ok (sName, sValue)
                                    _ -> Err "not a valid style key/value pair"
                                _ -> Err "not a valid style key/value pair"
                                  ) elems |> Utils.projOk
                         in
                         case resUnparsed of
                           Err _ -> defaultValue ()
                           Ok listPairStringString-> HTMLParser.printAttrValueRaw False<|
                             LangParserUtils.implodeStyleValue listPairStringString
                       _ -> defaultValue ()
                     else defaultValue ()
                in
                beforeSpace ++ attrNameStr ++ spBeforeEq.val ++ "=" ++ spAfterEq.val ++ atAfterEqual ++
                   wrapWithParensIfLessPrecedence -- Trick to put parentheses if we have an expression that is EOp or EApp for example
                     OpRight dummyExp attrValueToConsider value
              in
              case unwrapExp attrValueToConsider of
                EBase spIfNoValue (EString _ attrValueStr) ->
                  if attrValueStr == "" && spIfNoValue.val == " " then
                    beforeSpace ++ attrNameStr
                  else
                    beforeSpace ++ attrNameStr ++ spBeforeEq.val ++ "=" ++ spAfterEq.val ++ atAfterEqual ++
                       wrapWithParensIfLessPrecedence -- Trick to put parentheses if we have an expression that is EOp or EApp for example
                         OpRight dummyExp attrValueToConsider (unparse attrValueToConsider)
                EApp precedingWs maybeHtmlAttributeWrap [elem] _ _ ->
                  case unwrapExp maybeHtmlAttributeWrap of
                    EVar _ "__htmlRawAttribute__" ->
                      let unparseHtmlStringContent elem = case unwrapExp elem of
                        EBase _ (EString _ content) -> Ok content
                        EOp _ _ maybePlus [left, right] _ ->
                           if maybePlus.val == Plus then
                             Result.map2 (++) (unparseHtmlStringContent left) (unparseHtmlStringContent right)
                           else Err "Unexpected AST. reverting to standard rendering"
                        EApp _ maybeHtmlEntity [rendered, raw] _ _ ->
                           case unwrapExp maybeHtmlEntity of
                            EVar _ "__htmlStrEntity__" ->
                              case unwrapExp raw of
                                EBase _ (EString _ content) -> Ok content
                                _ -> Err "Unexpected AST. Reverting to standard rendering"
                            _ -> Err "Unexpected AST. Reverting to standard rendering"
                        _ -> Err "Unexpected AST. Reverting to standard rendering"
                      in
                      let quoteChar = if precedingWs.val == " " then "\"" else "'" in
                      case unparseHtmlStringContent elem  of
                        Err _ -> default ()
                        Ok unparsedContent ->
                          let rawcontent = unparsedContent |> Regex.replace (Regex.All) (Regex.regex quoteChar) (\m ->
                            if m.match == "\"" then "&quot;" else "&#39;")
                          in
                          beforeSpace ++ attrNameStr ++ spBeforeEq.val ++ "=" ++ spAfterEq.val ++ quoteChar ++ rawcontent ++ quoteChar
                    _ -> default ()
                _ -> default ()
            _ -> " @[" ++ unparse attr ++"]"
        _ -> " @[" ++ unparse attr ++"]"
      ) |> String.join ""
    Nothing -> case (unwrapExp attrExp) of
      EApp sp _ [e1, e2] _ _ -> case eAppUnapply2 e1 of
        Just (_, eleft, e) ->
          unparseHtmlAttributes interpolationStyle eleft ++ sp.val ++ "@" ++ unparse e ++ unparseHtmlAttributes interpolationStyle e2
        Nothing -> " @("++unparse attrExp++")"
      _ -> " @("++unparse attrExp++")"

unparseHtmlChildList: HtmlInterpolationStyle -> Exp -> String
unparseHtmlChildList interpolationStyle childExp =
  case eListUnapply childExp of
    Just children ->
      children |> List.map (unparseHtmlNode interpolationStyle) |> String.join ""
    Nothing ->
      case unwrapExp childExp of
        EApp _ v [ex] SpaceApp _ ->
          case unwrapExp v of
            EVar _ "__mergeHtmlText__" ->
              unparseHtmlChildList interpolationStyle ex
            _ -> "@(" ++ unparse childExp ++ ")"
        EApp _ _ [e1, eRight] _ _ ->
          case unwrapExp e1 of
            EApp _ _ [eLeft, eToRenderwrapped] _ _ ->
              let rightRendered = unparseHtmlChildList interpolationStyle eRight in
              let mbEntity = case unwrapExp eToRenderwrapped of
                EApp  _ _ [entityRendered, entity] _ _ -> --  __htmlEntity__
                   case unwrapExp entity of
                     EBase _ (EString _ content) -> Just content
                     _ -> Nothing
                _ -> Nothing
              in
              case mbEntity of
                Just entity ->
                  unparseHtmlChildList interpolationStyle eLeft ++ entity ++ rightRendered -- No interpolation for entities
                Nothing ->
                  let interpolated = case unwrapExp eToRenderwrapped of
                    EApp  _ _ [eToRender] _ _ -> unparse eToRender -- __mbwraphtmlnode__
                    _ ->                         unparse eToRenderwrapped
                  in
                  if noInterpolationConflict interpolated rightRendered then
                    unparseHtmlChildList interpolationStyle eLeft ++ "@" ++ interpolated ++ rightRendered
                  else
                    unparseHtmlChildList interpolationStyle eLeft ++ "@(" ++ interpolated ++ ")" ++ rightRendered
            _  -> "@(" ++ unparse childExp ++ ")"
        _  -> "@(" ++ unparse childExp ++ ")"

htmlContentRegexEscape = Regex.regex <| "@"

regexExcape = Regex.regex "&(?=\\w+)|<(?!\\s)|>(?!\\s)"
regexEscapeScript = Regex.regex "&(?=\\w+)|</script"
regexEscapeStyle = Regex.regex "&(?=\\w+)|</style"

unparseHtmlTextContent: HtmlInterpolationStyle -> String -> String
unparseHtmlTextContent style content =
  content |>
  (if style == Raw then identity else Regex.replace  Regex.All htmlContentRegexEscape (\m -> "@@")) |>
  (if style == Raw then identity
   else if style == ScriptInterpolated then
     Regex.replace Regex.All regexEscapeScript (\m -> case m.match of
         "&" -> "&amp;"
         "</script" -> "&lt;/script"
         _ -> m.match
       )
   else if style == StyleInterpolated then
     Regex.replace Regex.All regexEscapeScript (\m -> case m.match of
         "&" -> "&amp;"
         "</style" -> "&lt;/style"
         _ -> m.match
       )
   else
      Regex.replace Regex.All regexExcape (\m -> case m.match of
        "&" -> "&amp;"
        "<" -> "&lt;"
        ">" -> "&gt;"
        _ -> m.match
      ))

type HtmlInterpolationStyle = Interpolated | Raw | ScriptInterpolated | StyleInterpolated

unparseHtmlNode: HtmlInterpolationStyle -> Exp -> String
unparseHtmlNode interpolationStyle e = case (unwrapExp e) of
  EList unparseData [(_, typ), (_, content)] _ Nothing _ ->
    case unwrapExp typ of
      EBase _ (EString _ "COMMENT") ->
        case unwrapExp content of
          EBase _ (EString _ content) ->
            let (opening, closing) = case Regex.find (Regex.AtMost 1) (Regex.regex "\\{-(.*):(.*)-\\}") unparseData.val of
              [m] ->
                 case m.submatches of
                  [Just opening, Just closing] ->
                    if opening == "<" && not (String.startsWith "?" content) then
                      ("<!--", "-->")
                    else if opening == "</" && (case String.uncons content of
                              Just (c, _) -> Char.isUpper c || Char.isLower c || c == '@'
                              _ -> False) then
                      ("<!--", "-->")
                    else if opening == "<!" && String.startsWith "--" content then
                      ("<!--", "-->")
                    else
                      (opening, closing)
                  _ -> ("<!--", "-->")
              _ -> ("<!--", "-->")
            in
             opening ++ content ++ closing
          x -> "@[" ++ unparse e ++ "]"

      _ -> -- EBase _ (EString _ "TEXT") ->
        case unwrapExp content of
          EBase _ (EString _ content) -> unparseHtmlTextContent interpolationStyle content
          EParens _ subExpr LongStringSyntax _ ->
            case unwrapExp subExpr of
               EBase _ (EString _ content) -> unparseHtmlTextContent interpolationStyle content
               x -> "@[" ++ unparse e ++ "]"
          EVar _ varname -> "@" ++ varname
          x -> "@[" ++ unparse e ++ "]"
  EList _ [(tagSpace, tagExp), (attrSpace, attrExp), (spaceBeforeEndOpeningTag, childExp)] spaceBeforeTail Nothing spaceAfterTagClosing ->
    let (tagStart, tagEnd) = case (unwrapExp tagExp) of
          EBase _ (EString _ content) -> (content, content)
          _ -> ("@" ++ unparse tagExp, "@")
    in
    let newIsRaw = if interpolationStyle == Raw then Raw else
         case tagStart of
          "raw" -> Raw
          "script" -> ScriptInterpolated
          "style" -> StyleInterpolated
          _ -> interpolationStyle
    in
    "<" ++ tagStart ++ unparseHtmlAttributes interpolationStyle attrExp ++spaceBeforeEndOpeningTag.val ++ (
      if spaceBeforeTail.val == LeoParser.encoding_autoclosing then
        "/>"
      else if spaceBeforeTail.val == LeoParser.encoding_voidclosing then
        ">"
      else if spaceBeforeTail.val == LeoParser.encoding_forgotclosing then
        ">" ++ unparseHtmlChildList newIsRaw childExp
      else  -- Normal closing if the tag is ok
        if HTMLParser.isVoidElement tagStart then
          ">"
        else
          ">" ++ unparseHtmlChildList newIsRaw childExp ++ "</" ++ tagEnd ++ spaceAfterTagClosing.val ++ ">"
    )
  _ -> "@[" ++ unparse e ++ "]"

-- Detects if there are nodes, text, attributes and call the correct unparser. Use it for displaying local difference only.
unparseAnyHtml: Exp -> String
unparseAnyHtml e =
  case (unwrapExp e) of
    EList _ [(_, text), (_, content)] _ Nothing _ ->
      case eStrUnapply text of
        Just "TEXT" -> unparseHtmlNode Interpolated e
        Just "COMMENT" -> unparseHtmlNode Interpolated e
        Just _ -> -- It's an attribute
          unparseHtmlAttributes Interpolated (eList [e] Nothing)
        Nothing -> unparse e
    EList _ [(_, tag), (_, attr), (_, children)] _ Nothing _ ->
      case eStrUnapply tag of
        Just tagStr -> -- Just to make sure the second is a list
          case eListUnapply attr of
            Just _ ->  unparseHtmlNode (case tagStr of
              "raw" -> Raw
              "script" -> ScriptInterpolated
              "style" -> StyleInterpolated
              _ -> Interpolated) e
            Nothing -> case eAppUnapply2 attr of
              Just  (fun, left, right) -> case eVarUnapply fun of
                Just "++" ->  unparseHtmlNode Interpolated e
                _ -> unparse e
              _ -> unparse e
        Nothing -> case (unwrapExp tag) of
          EParens _ inner LeoSyntax _-> unparseHtmlNode Interpolated e
          _ -> unparse e
    _ -> unparseHtmlChildList Interpolated e

-- Return an integer if the exp is an operator with a precedence, or Nothing if it is always self-contained
getExpPrecedence: Exp -> Maybe Int
getExpPrecedence exp =
  case (unwrapExp exp) of
    EList _ [head] _ (Just tail) _ -> Just 5
    EApp _ _ _ (LeftApp _) _       -> Just 0
    EApp _ _ _ (RightApp _) _      -> Just 0
    EApp _ f _ (InfixApp) _        ->
      case (unwrapExp f) of
        EVar _ name ->
          case BinaryOperatorParser.getOperatorInfo name LeoParser.builtInPrecedenceTable of
            Nothing -> Nothing
            Just (associativity, precedence) -> Just precedence
        _ -> Nothing
    EApp _ _ _ SpaceApp _ -> Just 10
    ERecord _ _ decls _ ->
      case decls |> getKeyValuesFromDecls of
        Just keyValues ->
          case getDataConstructorNameString keyValues of
            Just x ->
              case getArgConstructorNameString keyValues of
                Just ex ->
                  case unwrapExp ex |> eRecord__Unapply of
                    Just (_, [], _)  -> Just 10
                    _ -> Just 9
                _ -> Nothing
            Nothing -> Nothing
        _ -> Nothing
    EColonType _ _ _ _ _           -> Just -1
    EOp _ _ operator _ _ ->
      case BinaryOperatorParser.getOperatorInfo (unparseOp operator) LeoParser.builtInPrecedenceTable of
        Nothing -> Just 10
        Just (associativity, precedence) -> Just precedence
    EParens _ e LeoSyntax _ -> getExpPrecedence e
    _ -> Nothing

getExpAssociativity: Exp -> Maybe BinaryOperatorParser.Associativity
getExpAssociativity exp =
  case (unwrapExp exp) of
    EList _ [head] _ (Just tail) _ -> Just BinaryOperatorParser.Right
    EApp _ _ _ (LeftApp _) _       -> Just BinaryOperatorParser.Right
    EApp _ _ _ (RightApp _) _      -> Just BinaryOperatorParser.Left
    EApp _ f _ (InfixApp) _        ->
      case (unwrapExp f) of
        EVar _ name ->
          case BinaryOperatorParser.getOperatorInfo name LeoParser.builtInPrecedenceTable of
            Nothing -> Nothing
            Just (associativity, precedence) -> Just associativity
        _ -> Nothing
    EColonType _ _ _ _ _           -> Just BinaryOperatorParser.Left
    EOp _ _ operator _ _ ->
      case BinaryOperatorParser.getOperatorInfo (unparseOp operator) LeoParser.builtInPrecedenceTable of
        Nothing -> Nothing
        Just (associativity, precedence) -> Just associativity
    _ -> Nothing

type OpDir = OpLeft | OpRight

wrapWithParensIfLessPrecedence_: (a -> Maybe Int) -> (a -> Maybe BinaryOperatorParser.Associativity) -> OpDir -> a -> a -> String -> String
wrapWithParensIfLessPrecedence_ getPrecedence getAssociativity opDir outsideExp insideExp unparsedInsideExpr =
  let inPrecedenceMb  = getPrecedence insideExp in
  case inPrecedenceMb of
    Nothing -> unparsedInsideExpr
    Just inPrecedence ->
  let precedenceMb    = getPrecedence outsideExp in
  case precedenceMb of
      Nothing -> unparsedInsideExpr
      Just precedence ->
  let inAssociativity = getAssociativity insideExp in
  let associativity = getAssociativity outsideExp in
  if inPrecedence < precedence
  then wrapWithTightParens unparsedInsideExpr
  else if inPrecedence == precedence then
     if associativity == inAssociativity && (
             associativity == Just BinaryOperatorParser.Left && opDir == OpLeft
          || associativity == Just BinaryOperatorParser.Right && opDir == OpRight) then
          unparsedInsideExpr
     else wrapWithTightParens unparsedInsideExpr -- Same associativity with conflicts, we put parentheses.
  else unparsedInsideExpr

wrapWithParensIfLessPrecedence: OpDir -> Exp -> Exp -> String -> String
wrapWithParensIfLessPrecedence  =
  wrapWithParensIfLessPrecedence_ getExpPrecedence getExpAssociativity

getPatPrecedence: Pat -> Maybe Int
getPatPrecedence pat =
  case pat.val.p__ of
    PList _ _ _ (Just tail) _ ->
      case BinaryOperatorParser.getOperatorInfo "::" LeoParser.builtInPatternPrecedenceTable of
        Nothing -> Nothing
        Just (associativity, precedence) -> Just precedence
    PAs _ _ _ _ ->
      case BinaryOperatorParser.getOperatorInfo "as" LeoParser.builtInPatternPrecedenceTable of
        Nothing -> Nothing
        Just (associativity, precedence) -> Just precedence
    PColonType _ _ _ _ ->
      Just 9
    _ -> Nothing

getPatAssociativity: Pat -> Maybe BinaryOperatorParser.Associativity
getPatAssociativity pat =
  case pat.val.p__ of
    PList _ _ _ (Just tail) _ ->
      case BinaryOperatorParser.getOperatorInfo "::" LeoParser.builtInPatternPrecedenceTable of
        Nothing -> Nothing
        Just (associativity, precedence) -> Just associativity
    PAs _ _ _ _ ->
      case BinaryOperatorParser.getOperatorInfo "as" LeoParser.builtInPatternPrecedenceTable of
        Nothing -> Nothing
        Just (associativity, precedence) -> Just associativity
    _ -> Nothing

wrapPatternWithParensIfLessPrecedence: OpDir -> Pat -> Pat -> String -> String
wrapPatternWithParensIfLessPrecedence =
  wrapWithParensIfLessPrecedence_ getPatPrecedence getPatAssociativity
