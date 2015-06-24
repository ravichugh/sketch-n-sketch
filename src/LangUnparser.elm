module LangUnparser (topLevelDiv, unparseE_, unparseE, lift) where

import Lang exposing (..)
-- TODO import Config exposing (..)
import Utils

import List
import String
import Set
import Debug

import Html exposing (Html)
import Html.Attributes as Attr

------------------------------------------------------------------------------

-- TODO Config, record type
codebox =
  { border = "none"
  , font = "Courier, monospace"
  , fontSize = "14px"
  }

styledNode f styles nodes = f [ Attr.style styles ] nodes

styledSpan : List (String, String) -> List Html -> Html
styledSpan = styledNode Html.span
styledDiv  = styledNode Html.div

styledStr : List (String, String) -> String -> Html
styledStr l s = styledSpan l [ Html.text s ]

topLevelDiv w h =
  styledDiv
    [ ("font-family", codebox.font)
    , ("font-size", codebox.fontSize)
    , ("border", codebox.border)
    , ("resize", "none")
    , ("overflow", "auto")
    , ("whiteSpace", "pre")
    , ("width", toString w ++ "px")
    , ("height", toString h ++ "px")
    ]

plainStr = styledStr []

hiliteStr =
  styledStr
    [ ("color", "white")
    , ("background-color", "darkGray")
    ]


------------------------------------------------------------------------------

type alias Thing = (String, Html)

concat f xys =
  let (xs,ys) = List.unzip xys in
  (String.concat xs, f [] ys)

concatLines : List Thing -> Thing
concatLines = concat Html.div << List.intersperse break

concatTokens : List Thing -> Thing
concatTokens = concat Html.span

joinTokens : Thing -> List Thing -> Thing
joinTokens sep things = concatTokens (List.intersperse sep things)

delimit : String -> String -> Thing -> Thing
delimit a b (x,y) =
  let x' = Utils.delimit a b x in
  let y' = styledSpan [] [ Html.text a, y, Html.text b ] in
  (x', y')

-- NOTE: use these only within a single line
parens = delimit "(" ")"
bracks = delimit "[" "]"
braces = delimit "{" "}"

delimitAndConcatTokens a b tokens =
  tokens
    |> concatTokens
    |> delimit a b

parensAroundTokens = delimitAndConcatTokens "(" ")"

delimitAroundLines : String -> String -> List (List Thing) -> List (List Thing)
delimitAroundLines a b lines =
  case lines of
    [] -> Debug.crash "delimitAroundLines [] 1"
    firstLine::rest ->
      case List.reverse rest of
        [] -> Debug.crash "delimitAroundLines [] 2"
        lastLine::middle ->
          let firstLine' = lift a :: firstLine in
          let lastLine'  = lastLine ++ [lift b] in
          [firstLine'] ++ (List.reverse middle) ++ [lastLine']

delimitAndConcatLines a b lines =
  lines
    |> delimitAroundLines a b
    |> List.map concatTokens
    |> concatLines

parensAroundLines = delimitAndConcatLines "(" ")"

-- breaks = join break
commas = joinTokens comma
spaces = joinTokens space

lift : String -> Thing
lift s = (s, plainStr s)

-- break  = lift "\n"
break  = ("\n", Html.br [] [Html.text "TODOblah"])
comma  = lift ","
space  = (" ", spaceH)
lambda = lift "\\"

-- TODO this is no good...
spaceH = 
  Html.span
    [ Attr.style [ ("color", "white") ] ]
    [ Html.text "X" ]
-- spaceH = 
--   Html.span
--     [ Attr.style [ ("display", "inline-block"), ("width", "9px")] ]
--     []

tab_ : Int -> Thing
tab_ k = lift (String.repeat k "  ")


------------------------------------------------------------------------------

type alias UnparseInfo =
  { showLocs : Bool
  , tab : Int
  , highlightedLocs : LocIdSet
  }

unparse : UnparseInfo -> Exp -> (String, Html.Html)
unparse info e =

  -- TODO remove call to fst
  -- TODO don't call lift everywhere
  let foo k' e'    = fst <| unparse { info | tab <- k' } e' in
  -- TODO remove this oldOne
  let indent    = fst << maybeIndent info in

  let goo k' e' = unparse { info | tab <- k' } e' in
  let k         = info.tab in
  let indent_   = maybeIndent info in

  case e of
    EBase v -> lift <| strBaseVal v
    EConst i l ->
      let (locid,b,_) = Debug.log "unparse EConst" <| l in
      let s =
        toString i
          ++ b
          ++ if info.showLocs then Utils.braces (strLoc l) else ""
      in
      if -- | Set.member locid info.highlightedLocs -> (s, T.bold (T.fromString s))
         | True                           -> (s, hiliteStr s)
         | otherwise                             -> lift s
    EVar x -> lift x
    EFun [p] e ->
      --lift <| Utils.parens <| "\\" ++ strPat p ++ indent e
      -- TODO indent
      parensAroundTokens [lift "\\", lift (strPat p), indent_ e]
    EFun ps e ->
      let args = Utils.spaces (List.map strPat ps) in
      -- lift <| Utils.parens <| "\\" ++ Utils.parens args ++ indent e
      parensAroundTokens [lambda, parens (lift args), indent_ e]
    EApp e1 [e2] ->
      -- lift <| Utils.parens <| foo k e1 ++ " " ++ indent e2
      parensAroundTokens [goo k e1, space, indent_ e2]
    EApp e1 es ->
      -- lift <| Utils.parens <|
      --   let s1 = foo k e1
      --       ss = List.map (foo (k+1)) es
      --       s2 = Utils.spaces ss in
      --   if fitsOnLine s2
      --   then s1 ++ " " ++ s2
      --   else String.join ("\n" ++ tab (k+1)) (s1::ss)
      let sh1 = goo k e1
          shs = List.map (goo (k+1)) es
          -- sh2 = spaces shs in
      in
      -- join (concat [break, tab_ (k+1)]) (sh1::shs)
      joinTokens space (sh1::shs)

    EOp op [e1,e2] ->
      -- lift <| Utils.parens <| String.join " " [strOp op, foo k e1, foo k e2]
      parens <| spaces [lift (strOp op), goo k e1, goo k e2]

    EIf e1 e2 e3 ->
      parens <|
        concatLines
          [ concatTokens [ lift "if ", goo k e1 ]
          , concatTokens [ tab_ (k+1), goo (k+1) e2 ]
          , concatTokens [ tab_ (k+1), goo (k+1) e3 ]
          ]

    -- TODO
    EList es Nothing ->
      bracks <| spaces <| List.map (goo k) es

    EList es mrest ->
      lift <| Utils.bracks <|
        let ss = List.map (foo k) es
            s  = Utils.spaces ss in
        if fitsOnLine s then
          case mrest of
            Nothing -> s
            Just e  -> s ++ " | " ++ foo k e
        else
          let s = String.join ("\n" ++ tab k ++ " ") ss in
          case mrest of
            Nothing -> s
            Just e  -> s ++ "\n" ++ tab k ++ "|" ++ foo k e


    ELet b p e1 e2 ->
      -- lift <| Utils.parens <|
      --   let k' = case e2 of {ELet _ _ _ _ -> k; _ -> k+1} in
      --   (if b then "letrec " else "let ") ++ strPat p ++
      --     indent e1 ++ "\n" ++
      --     tab k' ++ foo k' e2
      let k' = case e2 of {ELet _ _ _ _ -> k; _ -> k+1} in
      parensAroundLines
        [ [ lift (if b then "letrec " else "let ")
          , lift (strPat p)
          , indent_ e1
          ]
        , [ tab_ k'
          , goo k' e2
          ]
        ]

    -- TODO
    ECase e1 l ->
      let bar (pi,ei) =
        tab (k+1) ++ Utils.parens (strPat pi ++ " " ++ foo (k+1) ei) in
      lift <| Utils.parens <|
        "case " ++ foo k e1 ++ "\n" ++ Utils.lines (List.map bar l)

    -- TODO
    EComment s e1 ->
      lift <| foo k e1

maybeIndent info e =
  let k = info.tab in
  let out = unparse { info | tab <- k+1 } e in
  if | fitsOnLine (fst out) -> concatTokens [space, out]
     | otherwise            -> concatTokens [break, tab_ (k+1), out]

-- TODO take into account indent and other prefix of current line
fitsOnLine s =
  if | String.length s > 70               -> False
     | List.member '\n' (String.toList s) -> False
     | otherwise                          -> True

dummyInfo = 
  { showLocs = False
  , tab = 0
  , highlightedLocs = Set.empty
  }

unparseE_ : LocIdSet -> Exp -> Thing
unparseE_ locs e = unparse { dummyInfo | highlightedLocs <- locs} e

unparseE : Exp -> Thing
unparseE = unparseE_ Set.empty

