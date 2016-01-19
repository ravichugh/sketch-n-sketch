module VisualEditor where

import Html exposing (Html, Attribute)
import Html.Attributes as Attr

import ExamplesGenerated as Ex
import Lang exposing (..)
import LangParser2 as Parser
import LangUnparser as Unparser
import OurParser2 exposing (WithInfo)


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

-- map with linebreak interspersed

htmlMap : (WithInfo a -> Html) -> List (WithInfo a) -> List Html
htmlMap f xs =
  let combine a (b,xs) =
    case (b, xs) of
      (Just b', (x :: xs')) -> (Just a, f a :: insertBr a b' ++ xs)
      _ -> (Just a, [ f a ])
  in
  snd <| List.foldr combine (Nothing, []) xs

-- TODO: add correpsonding strings when more styles are added
pos : Int -> Int -> String -> Attribute
pos width height styleName =
  let posInfo = [("left", toString width ++ "pt"), ("top", toString height ++ "pt")] in
  case styleName of
    "basicStyle" -> Attr.style <|
                    [ ("font-size", "20pt")
                    , ("font-family", "monospace")
                    , ("line-height", "1.8")
                    ] ++ posInfo
    "literalStyle" -> Attr.style <|
                      [ ("background", "yellow")
                      -- , ("border", "3pt")
                      -- , ("border-style", "solid")
                      ] ++ posInfo
    "varUseStyle" -> Attr.style <|
                     [ ("background", "red")
                     -- , ("border", "3pt")
                     -- , ("border-style", "solid")
                     ] ++ posInfo
    "patUseStyle" -> Attr.style <|
                     [ ("background", "blue")
                     -- , ("border", "3pt")
                     -- , ("border-style", "solid")
                     ] ++ posInfo
    _             -> Attr.style posInfo

insertBr : WithInfo a -> WithInfo b -> List Html
insertBr e1 e2 =
  let (pos1, pos2) = (e1.start, e2.start) in
  let diff = pos2.line - pos1.line in
  List.repeat diff <| Html.br [] []
      
htmlOfExp : Exp -> Html
htmlOfExp e =
  let (e_,epos) = (e.val, e.start) in
  let (line,col) = (epos.line,epos.col) in
  let e__ = e_.e__ in
  case e__ of
    EConst n _ _ ->
      Html.span [ pos line col "literalStyle" ] [ Html.text <| toString n ]
    EBase baseVal ->
      case baseVal of
        Bool b -> Html.span [ pos line col "literalStyle" ] [ Html.text <| toString b ]
        String s -> Html.span [ pos line col "literalStyle" ] [ Html.text s ]
        Star -> Html.span [ pos line col "literalStyle" ] [ Html.text <| toString Star ]
    EOp op [e1, e2] ->
      let (h1,h2) = (htmlOfExp e1, htmlOfExp e2) in
      Html.span [ basicStyle ] <|
        [ Html.text <| "(" ++ strOp op.val ] ++
        insertBr e e1 ++
        [ h1 ] ++
        insertBr e1 e2 ++
        [ h2 ]
    EVar x -> Html.span [ pos line col "varUseStyle" ] [ Html.text x]
    EFun xs exp ->
      let (hs, h2) = (htmlMap htmlOfPat xs, htmlOfExp exp) in
      Html.span [ pos line col "basicStyle" ] <| 
        [ Html.text "( \\( " ] ++ hs ++ [ Html.text ") ", h2 ]
    EApp e1 es ->
      let (h1, hs) = (htmlOfExp e1, htmlMap htmlOfExp es) in
      Html.span [ pos line col "basicStyle" ] <|
          [ Html.text "( ", h1, Html.text " "] ++ hs
    ELet k r p e1 e2 ->
      let (h1, h2, h3) = (htmlOfPat p, htmlOfExp e1, htmlOfExp e2) in
      let (br1, br2) = (insertBr p e1, insertBr e1 e2) in
      let rest = [h1] ++ br1 ++ [ h2 ] ++ br2 ++ [ h3 ] in
      case (k,r) of
        (Let, True)  -> Html.span [ pos line col "basicStyle" ] <|
                    [ Html.text <| "( letrec "] ++ rest
        (Let, False) -> Html.span [ pos line col "basicStyle" ] <|
                    [ Html.text <| "( let ", h1] ++ rest
        (Def, True)  -> Html.span [ pos line col "basicStyle" ] <|
                    [ Html.text <| "( defrec ", h1] ++ rest
        (Def, False) -> Html.span [ pos line col "basicStyle" ] <|
                    [ Html.text <| "( def ", h1 ] ++ rest
    EList xs m ->
      Html.span [ pos line col "basicStyle" ] <| [ Html.text "[ "] ++ htmlMap htmlOfExp xs ++
        case m of
          Nothing -> [ Html.text " ]" ]
          Just y -> [ Html.text " | ", htmlOfExp y, Html.text " ]" ]
    EIf e1 e2 e3 ->
      let (h1,h2,h3) = (htmlOfExp e1, htmlOfExp e2, htmlOfExp e2) in
      let (br1, br2) = (insertBr e1 e2, insertBr e2 e3) in
      Html.span [ pos line col "basicStyle" ] <|
            [ Html.text "If ", h1] ++ br1 ++ [ h2 ] ++ br2 ++ [ h3 ]
    _ -> 
      let s = Unparser.unparseE e in
      Html.pre [] [ Html.text s ]

htmlOfPat : Pat -> Html
htmlOfPat pat =
    let (line, col) = (pat.start.line, pat.start.col) in
    case pat.val of
      PVar x _ -> Html.span [ pos line col "patUseStyle" ] [ Html.text x ]
      PConst n -> Html.span [ pos line col "literalStyle" ] [ Html.text <| toString n ]
      PBase baseVal -> Html.span [ pos line col "literalStyle" ] [ Html.text <| toString baseVal ]
      PList xs m ->
        Html.span [ pos line col "basicStyle" ] <| [ Html.text "[ "] ++ htmlMap htmlOfPat xs ++
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

