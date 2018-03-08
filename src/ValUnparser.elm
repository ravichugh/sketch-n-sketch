module ValUnparser exposing
  (..)

import Dict
import Record
import Utils
import Lang exposing (..)
import Set

strBaseVal : VBaseVal -> String
strBaseVal v =
  case v of
    VBool True ->
      "true"
    VBool False ->
      "false"
    VString s ->
      "'" ++ s ++ "'"
    VNull ->
      "null"

strVal : Val -> String
strVal =
  strVal_ False

strValLocs : Val -> String
strValLocs =
  strVal_ True

strNum : Num -> String
strNum  = toString
-- strNumDot  = strNum >> (\s -> if String.contains "[.]" s then s else s ++ ".0")

strNumTrunc : Int -> Num -> String
strNumTrunc k =
  strNum >> (\s -> if String.length s > k then String.left k s ++ ".." else s)

strVal_ : Bool -> Val -> String
strVal_ showTraces v =
  let recurse = strVal_ showTraces in
  -- let sTrace = if showTraces then Utils.braces (toString v.provenance) else "" in
  -- sTrace ++
  case v.v_ of
    VConst maybeAxis (i,tr) -> strNum i ++ if showTraces then Utils.angleBracks (toString maybeAxis) ++ Utils.braces (strTrace tr) else ""
    VBase b                 -> strBaseVal b
    VClosure _ _ _ _        -> "<fun>"
    VList vs                -> Utils.bracks (String.join " " (List.map recurse vs))
    VDict d                 -> "<dict " ++ (Dict.toList d |> List.map (\(k, v) -> (toString k) ++ ":" ++ (recurse v)) |> String.join " ") ++ ">"
    VRecord d               -> "<record " ++ String.join " " (List.map (\k ->
      case Dict.get k d of
        Nothing -> ""
        Just v -> k ++ ":" ++ recurse v
      ) <| Dict.keys d) ++ ">"
    VFun name i l m         -> "<fun name=" ++ name ++  " arity=" ++ toString i ++ (case m of
        Just x -> " reversible"
        Nothing -> ""
      ) ++ ">"


strOp : Op_ -> String
strOp op = case op of
  Plus          -> "+"
  Minus         -> "-"
  Mult          -> "*"
  Div           -> "/"
  Lt            -> "<"
  Eq            -> "="
  Pi            -> "pi"
  Cos           -> "cos"
  Sin           -> "sin"
  ArcCos        -> "arccos"
  ArcSin        -> "arcsin"
  ArcTan2       -> "arctan2"
  Floor         -> "floor"
  Ceil          -> "ceiling"
  Round         -> "round"
  ToStr         -> "toString"
  Explode       -> "explode"
  Sqrt          -> "sqrt"
  Mod           -> "mod"
  Pow           -> "pow"
  DictEmpty     -> "empty"
  DictFromList  -> "dict"
  DictInsert    -> "insert"
  DictGet       -> "get"
  DictRemove    -> "remove"
  DebugLog      -> "debug"
  NoWidgets     -> "noWidgets"
  ToStrExceptStr-> "ToStrExceptStr"
  RegexReplaceAllIn -> "replaceAllIn"
  RegexReplaceFirstIn -> "replaceFirstIn"
  RegexExtractFirstIn -> "extractFirstIn"

strLoc : Loc -> String
strLoc (k, b, mx) =
  "k" ++ toString k ++ (if mx == "" then "" else "_" ++ mx) ++ b

strTrace : Trace -> String
strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])


-- Better rendering of values
