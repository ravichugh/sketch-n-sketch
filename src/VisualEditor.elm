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
    [ -- TODO
    ]


------------------------------------------------------------------------------
-- Expression to HTML

-- TODO:
--
--  - div for each line
--  - span for each character (or seq of adjacent characters with same style)
--  - colored bounding boxes according to start/end pos
--
htmlOfExp : Exp -> Html
htmlOfExp e =
  let e_ = e.val in
  let e__ = e_.e__ in
  case e__ of
    EConst n _ _ ->
      Html.span [ literalStyle ] [ Html.text (toString n) ]
    EOp op [e1, e2] ->
      let (h1,h2) = (htmlOfExp e1, htmlOfExp e2) in
      Html.span [ basicStyle ]
        [ Html.text <| "(" ++ strOp op.val ++ " "
        , h1
        , Html.text " "
        , h2
        ]
    _ ->
      let s = Unparser.unparseE e in
      Html.pre [] [ Html.text s ]


------------------------------------------------------------------------------
-- Basic Driver

main : Html
main =
  -- NOTE: for now, toggle different examples from ExamplesGenerated.elm
  let testString = Ex.sineWaveOfBoxes in
  -- let testString = "(+ 1 2)" in
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

