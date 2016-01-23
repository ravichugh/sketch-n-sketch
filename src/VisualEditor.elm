module VisualEditor where

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import String

import ExamplesGenerated as Ex
import Lang exposing (..)
import LangParser2 as Parser
import LangUnparser as Unparser
import OurParser2 exposing (WithInfo, Pos)
import Utils

------------------------------------------------------------------------------
-- Styles

basicStyle : Attribute
basicStyle =
  Attr.style
    [ ("font-size", "20pt")
    , ("font-family", "monospace")
    , ("line-height", "1.8")
    , ("white-space", "pre")
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

-- map with linebreak interspersed

htmlMap : (WithInfo a -> Html) -> List (WithInfo a) -> List Html
htmlMap f xs =
  let combine a (b,xs) =
    case (b, xs) of
      (Just b', (x :: xs')) -> (Just a, f a :: space a.end b'.start ++ xs)
      _ -> (Just a, [ f a ])
  in
  snd <| List.foldr combine (Nothing, []) xs

space : Pos -> Pos -> List Html
space endPrev startNext =
  if endPrev.line == startNext.line
  then [ Html.text <| Unparser.cols endPrev.col startNext.col ]
  else List.repeat (startNext.line - endPrev.line) (Html.br [] [])
         ++ [ Html.text <| Unparser.cols 1 startNext.col ]

delimit : String -> String -> Pos -> Pos -> Pos -> Pos -> List Html -> List Html
delimit open close startOutside startInside endInside endOutside hs =
  let olen = String.length open
      clen = String.length close 
  in
  let begin = Html.text <| open ++ Unparser.whitespace (Unparser.bumpCol olen startOutside) startInside
      end = Html.text <| Unparser.whitespace endInside (Unparser.bumpCol (-1 * clen) endOutside) ++ close
  in
    [ begin ] ++ hs ++ [ end ]

parens = delimit "(" ")"
brackets = delimit "[" "]"

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
      Html.span [ basicStyle ] <|
          parens e.start op.start e2.end e.end <|
            [ Html.text <| strOp op.val ]
            ++ space op.end e1.start
            ++ [ h1 ]
            ++ space e1.end e2.start
            ++ [ h2 ]
    EVar x -> Html.span [ varUseStyle ] [ Html.text x]
    EFun [p] e1 ->
      let (h1, h2) = (htmlOfPat p, htmlOfExp e1) in
      let tok = Unparser.makeToken (Unparser.incCol e.start) "\\" in
      Html.span [ basicStyle ] <| parens e.start tok.start e1.end e.end <|
        [ Html.text tok.val ] ++ space tok.end p.start ++ [ h1 ] ++ space p.end e1.start ++ [ h2 ]
    EFun ps e1 ->
      let tok = Unparser.makeToken (Unparser.incCol e.start) "\\" in
      let (h1, h2) = (htmlMap htmlOfPat ps, htmlOfExp e1) in
      Html.span [ basicStyle ] <| parens e.start tok.start e1.end e.end <|
          let (h,l) = (Utils.head_ ps, Utils.last_ ps) in
          [ Html.text tok.val] ++ space tok.end (Unparser.decCol h.start)
             ++ [ Html.text "("] ++ h1 ++ [ Html.text ")"]
                 ++ space (Unparser.incCol l.end) e1.start ++ [ h2 ]
    EApp e1 es ->
      let (h1, hs) = (htmlOfExp e1, htmlMap htmlOfExp es) in
      let (h,l) = (Utils.head_ es, Utils.last_ es) in
          Html.span [ basicStyle ] <| parens e.start e1.start l.end e.end <|
               [h1] ++ space e1.end h.start ++ hs
             
    ELet k r p e1 e2 ->
      let (h1, h2, h3) = (htmlOfPat p, htmlOfExp e1, htmlOfExp e2) in
      let s1 = space p.end e1.start
          s2 = space e1.end e2.start
      in
      let rest = [h1] ++ s1 ++ [ h2 ] ++ s2 ++ [ h3 ] in
      case (k,r) of
        (Let, True)  ->
          let tok = Unparser.makeToken (Unparser.incCol e.start) "letrec" in
          Html.span [ basicStyle ] <|
              parens e.start tok.start e2.end e.end <|
                       [ Html.text tok.val] ++ space tok.end p.start ++ rest
        (Let, False) ->
          let tok = Unparser.makeToken (Unparser.incCol e.start) "let" in
          Html.span [ basicStyle ] <|
              parens e.start tok.start e2.end e.end <|
                       [ Html.text tok.val] ++ space tok.end p.start ++ rest
        (Def, True)  ->
          let tok = Unparser.makeToken (Unparser.incCol e.start) "defrec" in
          Html.span [ basicStyle ] <|
              parens e.start tok.start e2.end e.end <|
                       [ Html.text <| tok.val] ++ space tok.end p.start ++ rest
        (Def, False) ->
          let tok = Unparser.makeToken (Unparser.incCol e.start) "def" in
          Html.span [ basicStyle ] <|
              parens e.start tok.start e2.end e.end <|
                    [ Html.text tok.val] ++ space tok.end p.start ++ rest
    EList xs Nothing ->
      Html.span [ basicStyle ] <|
          brackets e.start (Utils.head_ xs).start (Utils.last_ xs).end e.end <| htmlMap htmlOfExp xs
    EList xs (Just y) ->
      let (h1, h2) = (htmlMap htmlOfExp xs, htmlOfExp y) in
      let (e1,e2) = (Utils.head_ xs, Utils.last_ xs) in
      let tok1 = Unparser.makeToken e.start "["
          tok2 = Unparser.makeToken e2.end "|"
          tok3 = Unparser.makeToken y.end "]"
      in
        Html.span [ basicStyle ] <|
           brackets e.start tok1.start tok3.end e.end <|
             [ Html.text tok1.val ] ++ space tok1.end e1.start ++ h1 ++ [ Html.text tok2.val ]
                ++ space tok2.end y.start ++ [ h2 ] ++ [ Html.text tok3.val ]
    EIf e1 e2 e3 ->
      let (h1,h2,h3) = (htmlOfExp e1, htmlOfExp e2, htmlOfExp e2) in
      let tok = Unparser.makeToken (Unparser.incCol e.start) "if" in
      let s1 = space tok.end e1.start
          s2 = space e1.end e2.start
          s3 = space e2.end e3.start
      in
      Html.span [ basicStyle ] <| parens e.start e.start e3.end e.end
            [ Html.text tok.val] ++ s1 ++ [ h1 ] ++ s2 ++ [ h2 ] ++ s3 ++ [ h3 ] 
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
  let testString = "(let yi (- y0 (* amp (sin (* i (/ twoPi n))))) (rect 'lightblue' xi yi w h))" in
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

