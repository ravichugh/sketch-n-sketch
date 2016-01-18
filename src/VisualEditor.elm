module VisualEditor where

import Html exposing (Html, Attribute)
import Html.Attributes as Attr

import ExamplesGenerated as Ex
import Lang exposing (..)
import LangParser2 as Parser
import LangUnparser as Unparser


------------------------------------------------------------------------------
-- Styles

basicStyle : Attribute
basicStyle =
  Attr.style
    [ ("font-size", "20pt")
    , ("font-family", "monospace")
    , ("line-height", "1.8")
    ]

literalStyle : Attribute
literalStyle =
  Attr.style
    -- TODO
    [ ("background", "yellow")
    , ("border", "3pt")
    , ("border-style", "solid")
    ]

varUseStyle : Attribute
varUseStyle =
  Attr.style
    [ ("background", "red")
    , ("border", "3pt")
    , ("border-style", "solid")
    ]

patUseStyle : Attribute
patUseStyle =
  Attr.style
    [ ("background", "blue")
    , ("border", "3pt")
    , ("border-style", "solid")
    ]

------------------------------------------------------------------------------
-- Expression to HTML

-- TODO:
--
--  - div for each line
--  - span for each character (or seq of adjacent characters with same style)
--  - colored bounding boxes according to start/end pos
--

-- map with space interspersed 
htmlMap : (a -> Html) -> List a -> List Html
htmlMap f xs = List.intersperse (Html.text " ") <| List.map f xs
               
htmlOfExp : Exp -> Html
htmlOfExp e = 
  let e_ = e.val in
  let e__ = e_.e__ in
  case e__ of
    EConst n _ _ ->
      Html.span [ literalStyle ] [ Html.text <| toString n ]
    EBase baseVal ->
      case baseVal of
        Bool b -> Html.span [ literalStyle ] [ Html.text <| toString b ]
        String s -> Html.span [ literalStyle ] [ Html.text s ]
        Star -> Html.span [ literalStyle ] [ Html.text <| toString Star ]
    EOp op [e1, e2] ->
      let (h1,h2) = (htmlOfExp e1, htmlOfExp e2) in
      Html.span [ basicStyle ]
        [ Html.text <| "(" ++ strOp op.val ++ " "
        , h1
        , Html.text " "
        , h2
        ]
    EVar x -> Html.span [ varUseStyle ] [ Html.text x]
    EFun xs exp ->
      let (hs, h2) = (htmlMap htmlOfPat xs, htmlOfExp exp) in
      Html.span [ basicStyle ] <| 
        [ Html.text "( \\( " ] ++ hs ++ [ Html.text ") ", h2 ]
    EApp e1 es ->
      let (h1, hs) = (htmlOfExp e1, htmlMap htmlOfExp es) in
      Html.span [ basicStyle ] <|
          [ Html.text "( ", h1, Html.text " "] ++ hs
    ELet k _ p e1 e2 ->
      let (h1, h2, h3) = (htmlOfPat p, htmlOfExp e1, htmlOfExp e2) in
      Html.span [ basicStyle ] <|
          [ Html.text <| "( " ++ toString k ++ " ", h1, h2, h3 ]
    EList xs m ->
      Html.span [ basicStyle ] <| [ Html.text "[ "] ++ htmlMap htmlOfExp xs ++
        case m of
          Nothing -> [ Html.text " ]" ]
          Just y -> [ Html.text " | ", htmlOfExp y, Html.text " ]" ]
    _ -> 
      let s = Unparser.unparseE e in
      Html.pre [] [ Html.text s ]

htmlOfPat : Pat -> Html
htmlOfPat pat =
    case pat.val of
      PVar x _ -> Html.span [ patUseStyle ] [ Html.text x ]
      PConst n -> Html.span [ literalStyle ] [ Html.text <| toString n ]
      PBase baseVal -> Html.span [ literalStyle ] [ Html.text <| toString baseVal ]
      PList xs m ->
        Html.span [ basicStyle ] <| [ Html.text "[ "] ++ htmlMap htmlOfPat xs ++
        case m of
          Nothing -> [ Html.text " ]" ]
          Just y -> [ Html.text " | ", htmlOfPat y, Html.text " ]" ]

------------------------------------------------------------------------------
-- Basic Driver

main : Html
main =
  -- NOTE: for now, toggle different examples from ExamplesGenerated.elm
  let testString = Ex.sineWaveOfBoxes in
  --let testString = "(\\(x y) (f x y))" in
  let testExp =
    case Parser.parseE testString of
      Err _ -> Debug.crash "main: bad parse"
      Ok e  -> e
  in
  let head = Html.node "head" [] [] in
  let body = Html.node "body"
               [ basicStyle, Attr.contenteditable True ]
               [ htmlOfExp testExp ]
  in
  Html.node "html" [] [head, body]

