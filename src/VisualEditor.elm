module VisualEditor where

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import String

import ExamplesGenerated as Ex
import Lang exposing (..)
import LangParser2 as Parser
import LangUnparser as Unparser
import OurParser2 exposing (WithInfo, Pos)

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
{-
-- TODO: add correpsonding strings when more styles are added
pos : Int -> Int -> String -> Attribute
pos height width styleName =
  let posInfo = [ ("position", "absolute"), ("left", toString (width*20) ++ "pt")] in --, ("top", toString ((toFloat height)*36) ++ "pt")] in
  case styleName of
    "basicStyle" -> Attr.style <|
                    [ ("font-size", "20pt")
                    , ("font-family", "monospace")
                    , ("line-height", "1.8")
                    ] ++ posInfo
    "literalStyle" -> Attr.style <|
                      [ ("background", "yellow")
                      , ("border", "3pt")
                      , ("border-style", "solid")
                      ] ++ posInfo
    "varUseStyle" -> Attr.style <|
                     [ ("background", "red")
                     , ("border", "3pt")
                     , ("border-style", "solid")
                     ] ++ posInfo
    "patUseStyle" -> Attr.style <|
                     [ ("background", "blue")
                     , ("border", "3pt")
                     , ("border-style", "solid")
                     ] ++ posInfo
    _             -> Attr.style posInfo

insertBr : WithInfo a -> WithInfo b -> List Html
insertBr e1 e2 = 
  let (pos1, pos2) = (e1.start, e2.start) in
  let diff = pos2.line - pos1.line in
  List.repeat diff <| Html.br [] []
-}
space : Pos -> Pos -> List Html
space endPrev startNext =
  if endPrev.line == startNext.line
  then [ Html.text <| String.repeat (startNext.col - endPrev.col) " " ]
  else List.repeat (startNext.line - endPrev.line) (Html.br [] [])
         ++ [ Html.text <| String.repeat (startNext.col - 1) " " ]

delimit : String -> String -> Pos -> Pos -> Pos -> Pos -> List Html -> List Html
delimit open close startOutside startInside endInside endOutside hs =
  let olen = String.length open
      clen = String.length close 
  in
  let begin = Html.text <| open ++ Unparser.whitespace (Unparser.bumpCol olen startOutside) startInside
      end = Html.text <| Unparser.whitespace (Unparser.bumpCol (-1 * clen) endOutside) endInside ++ close
  in
    [ begin ] ++ hs ++ [ end ]

parens = delimit "(" ")"

last : List a -> Maybe a
last = List.head << List.reverse
       
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
      Html.span [ basicStyle ] <| parens e.start p.start e1.end e.end <|
        [ Html.text "\\" ] ++ [ h1 ] ++ space p.end e1.start ++ [ h2 ]
    EApp e1 es ->
      let (h1, hs) = (htmlOfExp e1, htmlMap htmlOfExp es) in
      case (List.head es, last es) of
        (Just h, Just l) ->
          Html.span [ basicStyle ] <| parens e.start e1.start l.end e.end <|
               [h1] ++ space e1.end h.start ++ hs
        _ -> Debug.crash "Impossible: EApp has no argument."
             
    ELet k r p e1 e2 ->
      let (h1, h2, h3) = (htmlOfPat p, htmlOfExp e1, htmlOfExp e2) in
      let s1 = space p.start e1.start
          s2 = space e1.start e2.start
      in
      let rest = [h1] ++ s1 ++ [ h2 ] ++ s2 ++ [ h3 ] in
      case (k,r) of
        (Let, True)  -> Html.span [ basicStyle ] <|
                    [ Html.text <| "( letrec "] ++ rest
        (Let, False) -> Html.span [ basicStyle ] <|
                    [ Html.text <| "( let ", h1] ++ rest
        (Def, True)  -> Html.span [ basicStyle ] <|
                    [ Html.text <| "( defrec ", h1] ++ rest
        (Def, False) -> Html.span [ basicStyle ] <|
                    [ Html.text <| "( def ", h1 ] ++ rest
    EList xs m ->
      Html.span [ basicStyle ] <| [ Html.text "[ "] ++ htmlMap htmlOfExp xs ++
        case m of
          Nothing -> [ Html.text " ]" ]
          Just y -> [ Html.text " | ", htmlOfExp y, Html.text " ]" ]
    EIf e1 e2 e3 ->
      let (h1,h2,h3) = (htmlOfExp e1, htmlOfExp e2, htmlOfExp e2) in
      let s1 = space e1.start e2.start
          s2 = space e2.start e3.start
      in
      Html.span [ basicStyle ] <|
            [ Html.text "If ", h1] ++ s1 ++ [ h2 ] ++ s2 ++ [ h3 ]
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
  let testString = "(\\x \n  (f   x))" in
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

