module VisualEditor where

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as Decode
import String
import Signal exposing (Mailbox, mailbox)

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

opUseStyle : Attribute
opUseStyle =
  Attr.style
    [ ("background", "brown")
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
        String s -> Html.span [ literalStyle ] [ Html.text <| "\'" ++ s ++ "\'" ]
        Star -> Html.span [ literalStyle ] [ Html.text <| toString Star ]
    EOp op es ->
      let hs = htmlMap htmlOfExp es in
      let (h,l) = (Utils.head_ es, Utils.last_ es) in
      Html.span [ basicStyle ] <|
          parens e.start op.start l.end e.end <|
            [ Html.span [ opUseStyle ] <| [ Html.text <| strOp op.val ]]
            ++ space op.end h.start
            ++ hs
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
             
    ELet Let r p e1 e2 ->
      let (h1, h2, h3) = (htmlOfPat p, htmlOfExp e1, htmlOfExp e2) in
      let s1 = space p.end e1.start
          s2 = space e1.end e2.start
      in
      let rest = [h1] ++ s1 ++ [ h2 ] ++ s2 ++ [ h3 ] in
      if r then 
        let tok = Unparser.makeToken (Unparser.incCol e.start) "letrec" in
        Html.span [ basicStyle ] <|
            parens e.start tok.start e2.end e.end <|
                     [ Html.text tok.val] ++ space tok.end p.start ++ rest
      else
        let tok = Unparser.makeToken (Unparser.incCol e.start) "let" in
        Html.span [ basicStyle ] <|
            parens e.start tok.start e2.end e.end <|
                     [ Html.text tok.val] ++ space tok.end p.start ++ rest
    ELet Def r p e1 e2 ->
      let (h1, h2, h3) = (htmlOfPat p, htmlOfExp e1, htmlOfExp e2) in
      let s1 = space p.end e1.start
          s2 = space e1.end e2.start
      in
      let rest = s2 ++ [ h3 ] in
      if r then
          let tok = Unparser.makeToken (Unparser.incCol e.start) "defrec" in
          let defParen = [ Html.text tok.val ] ++ space tok.end p.start ++ [ h1 ] ++ s1 ++ [ h2 ] in
          Html.span [ basicStyle ] <|
              parens e.start tok.start e1.end e.end defParen ++ rest                     
      else
          let tok = Unparser.makeToken (Unparser.incCol e.start) "def" in
          let defParen = [ Html.text tok.val ] ++ space tok.end p.start ++ [ h1 ] ++ s1 ++ [ h2 ] in
          Html.span [ basicStyle ] <|
              parens e.start tok.start e1.end e.end defParen ++ rest
    EList xs Nothing ->
      case xs of
        [] -> Html.span [ basicStyle ] <| [ Html.text "[]" ]
        _  -> Html.span [ basicStyle ] <| brackets e.start (Utils.head_ xs).start (Utils.last_ xs).end e.end <| htmlMap htmlOfExp xs
    EList xs (Just y) ->
      let (h1, h2) = (htmlMap htmlOfExp xs, htmlOfExp y) in
      let (e1,e2) = (Utils.head_ xs, Utils.last_ xs) in
      let tok1 = Unparser.makeToken e.start "["
          tok2 = Unparser.makeToken e2.end "|"
          tok3 = Unparser.makeToken y.end "]"
      in
        Html.span [ basicStyle ] <|
           delimit tok1.val tok2.val tok1.start e1.start e2.end tok2.end (space tok1.end e1.start ++ h1)
                ++ space tok2.end y.start ++ [ h2 ] ++ space y.end tok3.start ++ [ Html.text tok3.val ]
    EIf e1 e2 e3 ->
      let (h1,h2,h3) = (htmlOfExp e1, htmlOfExp e2, htmlOfExp e3) in
      let tok = Unparser.makeToken (Unparser.incCol e.start) "if" in
      let s1 = space tok.end e1.start
      in
      Html.span [ basicStyle ] <| parens e.start tok.start e3.end e.end <|
            [ Html.text tok.val] ++ s1 ++ htmlMap htmlOfExp [ e1, e2, e3 ]
    EComment s e1 ->
      let white = space (Unparser.incLine e.start) e1.start in
      Html.span [ basicStyle] <|
            [ Html.text <| ";" ++ s] ++ [ Html.br [] [] ] ++ white ++ [ htmlOfExp e1 ] 
    ECase e1 bs ->
      let tok = Unparser.makeToken (Unparser.incCol e.start) "case" in
      let l = Utils.last_ bs in
      Html.span [ basicStyle ] <|
          parens e.start tok.start l.end e.end <| htmlMap htmlOfBranch bs
    _ -> 
      let _ = Debug.log "VisualEditor.HtmlOfExp no match :" (toString e) in
      let s = Unparser.unparseE e in
      Html.pre [] [ Html.text s ]

htmlOfPat : Pat -> Html
htmlOfPat p =
    case p.val of
      PVar x _ -> Html.span [ patUseStyle ] [ Html.text x ]
      PConst n -> Html.span [ literalStyle ] [ Html.text <| toString n ]
      PBase baseVal -> Html.span [ literalStyle ] [ Html.text <| toString baseVal ]
      PList xs Nothing ->
        case xs of
          [] -> Html.span [ basicStyle ] <| [ Html.text "[]" ] 
          _  -> Html.span [ basicStyle ] <| brackets p.start (Utils.head_ xs).start (Utils.last_ xs).end p.end <| htmlMap htmlOfPat xs
      PList xs (Just y) ->
        let (h1, h2) = (htmlMap htmlOfPat xs, htmlOfPat y) in
        let (e1,e2) = (Utils.head_ xs, Utils.last_ xs) in
        let tok1 = Unparser.makeToken p.start "["
            tok2 = Unparser.makeToken e2.end "|"
            tok3 = Unparser.makeToken y.end "]"
        in
        Html.span [ basicStyle ] <|
            delimit tok1.val tok2.val tok1.start e1.start e2.end tok2.end (space tok1.end e1.start ++ h1)
                ++ space tok2.end y.start ++ [ h2 ] ++ space y.end tok3.start ++ [ Html.text tok3.val ]

htmlOfBranch : Branch -> Html
htmlOfBranch b =
  let (p,e) = b.val in
  Html.span [ basicStyle] <|
      parens b.start p.start e.end b.end <|
             [ htmlOfPat p ] ++ space p.end e.start ++ [ htmlOfExp e ]
------------------------------------------------------------------------------
-- Basic Driver

type alias Event = Model -> Model

myMailbox : Mailbox Event
myMailbox = mailbox identity

type alias Model =
  { name : String
  , code : String
  }

initModel =
  { name = Ex.scratchName
  , code = Ex.scratch
  }

upstate : Event -> Model -> Model
upstate = (<|)

-- http://stackoverflow.com/questions/32426042/how-to-print-index-of-selected-option-in-elm
targetSelectedIndex : Decode.Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

view : Model -> Html
view model =
  let testString = model.code in
  --let testString = "[[x0 y0] [x1 y1] | rest]" in 
  let testExp =
    case Parser.parseE testString of
      Err _ -> Debug.crash "main: bad parse"
      Ok e  -> e
  in
  let break = Html.br [] [] in
  let head = Html.node "head" [] [] in
  let body =
    let options =
      flip List.map Ex.examples <| \(name,code) ->
        Html.option
           [Attr.value name, Attr.selected (name == model.name)]
           [Html.text name]
    in
    let dropdown =
      Html.select
         [ Attr.contenteditable False
         , Attr.style
              [ ("width", "500px")
              , ("font-size", "20pt")
              , ("font-family", "monospace")
              ]
         , Events.on "change" targetSelectedIndex <| \i ->
             let (name,code) = Utils.geti (i+1) Ex.examples in
             Signal.message myMailbox.address (\_ -> {name=name, code=code})
         ]
         options
    in
    Html.node "body"
       [ basicStyle, Attr.contenteditable True ]
       [ dropdown, break, htmlOfExp testExp ]
  in
  Html.node "html" [] [head, body]

events : Signal Event
events = myMailbox.signal

main : Signal Html
main = Signal.map view (Signal.foldp upstate initModel events)

