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
  let s = Unparser.unparseE e in
  Html.pre [ basicStyle ] [ Html.text s ]


------------------------------------------------------------------------------
-- Basic Driver

main : Html
main =
  -- NOTE: for now, toggle different examples from ExamplesGenerated.elm
  let testString = Ex.sineWaveOfBoxes in
  let testExp =
    case Parser.parseE testString of
      Err _ -> Debug.crash "main: bad parse"
      Ok e  -> e
  in
  let head = Html.node "head" [] [] in
  let body = Html.node "body"
               [ Attr.contenteditable True ]
               [ htmlOfExp testExp ]
  in
  Html.node "html" [] [head, body]

