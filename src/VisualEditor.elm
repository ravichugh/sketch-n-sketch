module VisualEditor where

import Html exposing (Html, Attribute)
import Html.Attributes as Attr
import Html.Events as Events exposing (defaultOptions)
import Json.Decode as Decode
import String
import Dict
import Signal exposing (Mailbox, mailbox)
import Time exposing (Time)

import ExamplesGenerated as Examples
import Lang exposing (..)
import LangParser2 as Parser
import LangUnparser as Unparser
import OurParser2 exposing (WithInfo, Pos)
import Utils

------------------------------------------------------------------------------

stopPropagation = { defaultOptions | stopPropagation = True }

------------------------------------------------------------------------------
-- Styles

-- TODO use CSS classes/selectors eventually

border =
  [ ("border", "3pt")
  , ("border-style", "solid")
  , ("border-color", "black") ]

leftRightPadding =
  [ ("padding", "0pt 2pt 0pt 2pt") ]

basicStyle : Attribute
basicStyle =
  Attr.style
    [ ("font-size", "14pt")
    , ("font-family", "monospace")
    , ("line-height", "1.8")
    , ("white-space", "pre")
    ]

literalStyle : Attribute
literalStyle =
  Attr.style <|
    [ ("background", "yellow")
    ] ++ border ++ leftRightPadding

varUseStyle : Attribute
varUseStyle =
  Attr.style <|
    [ ("background", "red")
    ] ++ border ++ leftRightPadding

varDefStyle : Attribute
varDefStyle =
  Attr.style <|
    [ ("background", "lightblue")
    , ("cursor", "text")
    ] ++ leftRightPadding

opUseStyle : Attribute
opUseStyle =
  Attr.style <|
    [ ("background", "brown")
    ] ++ border

literalOuterStyle model padding color =
  let (l1,l2) =
    -- ( [ ("padding", "5pt")
    --   , ("border-radius", "5pt")
    ( [ ("padding", padding)
      , ("border-radius", "5pt")
      , ("background", "lightgray")
      ]
    , [ ("background", color)
      , ("cursor", "pointer")
      ])
  in
  let l =
    case model.textChangedAt of
      Nothing -> l1 ++ l2
      Just _  -> l1
  in
  Attr.style l

literalInnerStyle model =
  Attr.style <|
    [ ("background", "yellow")
    , ("cursor", "text")
    , ("word-wrap", "normal")
    ] ++ leftRightPadding


------------------------------------------------------------------------------
-- Events

updateModelMessage f =
  (\_ -> Signal.message myMailbox.address (UpdateModel f))

handleAndStop evt f =
  Events.onWithOptions evt stopPropagation Decode.value (updateModelMessage f)

-- this is a simple example, just increments current value by offset
eConstEvent : Float -> Loc -> Int -> Attribute
eConstEvent n loc offset =
  let (locid,_,_) = loc in
  handleAndStop "mousedown" <| \model ->
    let lSubst = Dict.singleton locid (n + toFloat offset) in
    let exp' = applyLocSubst lSubst model.inputExp in
    let code' = Unparser.unparse exp' in
    { model | inputExp = exp', code = code' }

eConstFlipFreeze (ws, n, loc, wd) =
  let (locid,ann,mx) = loc in
  let ann' =
    if ann == frozen then unann
    else if ann == unann then frozen
    else ann
  in
  let eSubst = Dict.singleton locid (EConst ws n (locid, ann', mx) wd) in
    -- relying on invariant that EId = LocId

  handleAndStop "mousedown" <| \model ->
    let exp' = applyESubst eSubst model.inputExp in
    let code' = Unparser.unparse exp' in
    { model | inputExp = exp', code = code' }

-- <span> doesn't have a "change" event
eTextChange =
  handleAndStop "mousedown" <| \m ->
    { m | textChangedAt = Just 0 }

{-
onClickWithoutPropagation : Signal.Address a -> a -> Attribute
onClickWithoutPropagation address a = Events.onWithOptions "click" {defaultOptions | stopPropagation = True} Decode.value (\_ -> Signal.message address a)
                                   
eVarEvent : Ident -> Int -> Attribute
eVarEvent x id =
  onClickWithoutPropagation myMailbox.address <| UpdateModel <| \model ->
    let e = Utils.fromOk_ <| Parser.parseE <| "(let " ++ x ++"1 " ++ x ++ " " ++ x ++ "1)" in
    let e__ = e.val.e__ in
    let eSubst = Dict.singleton id e__  in
    let exp' = applyESubst eSubst model.exp in
    let code' = Unparser.unparseE exp' in
    { model | exp = exp', code = code'}
-}

------------------------------------------------------------------------------

eConstOuterLeftRight model n loc padding color offset =
  case model.textChangedAt of
    Nothing -> [ literalOuterStyle model padding color, eConstEvent n loc offset ]
    Just _  -> [ literalOuterStyle model padding color ]

eConstOuterBottom model eConstInfo =
  let padding = "0 0 5pt 0" in
  let color = "lightblue" in
  case model.textChangedAt of
    Nothing -> [ literalOuterStyle model padding color, eConstFlipFreeze eConstInfo ]
    Just _  -> [ literalOuterStyle model padding color ]

eConstInnerAttrs model =
  [ Attr.contenteditable True
  , literalInnerStyle model
  , eTextChange
  ]


------------------------------------------------------------------------------
-- Expression to HTML

-- TODO:
--
--  - div for each line
--  - span for each character (or seq of adjacent characters with same style)
--  - colored bounding boxes according to start/end pos
--

-- map with linebreak/spaces interspersed

lines : Int -> Int -> String
lines i j =
  if i > j then let _ = Debug.log <| "VisualEditor: " ++ toString (i,j) in " "
  else String.repeat (j-i) "\n"

cols : Int -> Int -> String
cols i j =
  if i > j then let _ = Debug.log <| "VisualEditor: " ++ toString (i,j) in " "
  else String.repeat (j-i) " "

-- stack : List Attribute -> Html -> Html
stack layers node =
  case layers of
    []             -> node
    layer::layers' -> Html.span layer [ stack layers' node ]

htmlOfConst model ws n loc wd =
  let (_,ann,_) = loc in
  let layers =
     [ eConstOuterLeftRight model n loc "0 8pt 0 0" "gray" 1
     , eConstOuterLeftRight model n loc "0 0 0 8pt" "gray" (-1)
     , eConstOuterBottom model (ws, n, loc, wd)
     , eConstInnerAttrs model
     ]
  in
  stack layers (Html.text <| toString n ++ ann)

rewritePat : Ident -> Ident -> Pat -> Maybe Pat
rewritePat x x' =
  let foo p =
    case p.val of
      PVar ws y wd -> if x == y then { p | val = PVar ws x' wd } else p
      PList ws1 ps ws2 rest ws3 ->
        let (ps', rest') = (List.map foo ps, Utils.mapMaybe foo rest) in
        { p | val = PList ws1 ps' ws2 rest' ws3 }
      _ -> p
  in
  \p ->
    let p' = foo p in
    if p == p'
      then Nothing
      else Just p'

rebuildExp e e__' =
  let e_ = e.val in
  let e__ = e_.e__ in
  { e | val = { e_ | e__ = e__' } }

-- for now, not taking into account ids, scope, capture...
renameVar x x' model =
  let bar e =
    rebuildExp e <|
      case e.val.e__ of
        EVar ws y -> if x == y then EVar ws x' else e.val.e__
        _         -> e.val.e__
  in
  let foo e =
    rebuildExp e <|
      case e.val.e__ of
        ELet ws1 k b p e1 e2 ws2 ->
          case rewritePat x x' p of
            Nothing -> e.val.e__
            Just p' -> let e2' = mapExp bar e2 in
                       ELet ws1 k b p' e1 e2' ws2
        EFun ws1 ps e1 ws2 ->
          let ps' = List.map (rewritePat x x') ps in
          -- TODO
          let blah pi mpi' =
            case mpi' of
              Nothing -> pi
              Just pi' -> pi'
            in
          let ps'' = List.map2 blah ps ps' in
          let e1' = mapExp bar e1 in
          EFun ws1 ps'' e1' ws2
        _ -> e.val.e__
  in
  let exp' = mapExp foo model.inputExp in
  let code' = Unparser.unparse exp' in
  { model | inputExp = exp', code = code' }

-- could supply EId of ELet/EFun from htmlOfExp/Pat...
htmlOfPVar model ws x =
  let botRight =
    [ Attr.style
        [ ("padding", "0 5pt 5pt 0")
        , ("border-radius", "5pt")
        , ("background", "orange")
        , ("cursor", "pointer")
        ]
    , Attr.id x
    , Events.onClick deuceQueryMailbox.address (x)
        -- sending message to JavaScript,
        -- rather than installing UpdateModel callback...
    ] in
  let left =
    [ Attr.style
        [ ("padding", "0 0 0 5pt")
        , ("border-radius", "5pt")
        , ("background", "lightgreen")
        , ("cursor", "pointer")
        ]
    ] in
  let top =
    [ Attr.style
        [ ("padding", "5pt 0 0 0")
        , ("border-radius", "5pt")
        , ("background", "gray")
        , ("cursor", "grab")
        ]
    ] in
  let layers =
     [ botRight
     , left
     , top
     , [ Attr.contenteditable True, varDefStyle, eTextChange ]
     ]
  in
  let h = stack layers (Html.text x) in
  if ws == ""
    then h
    else hConcat [Html.text ws, h]

hConcat : List Html -> Html
hConcat hs = Html.span [] hs

htmlOfExp : Model_ a -> Exp -> Html
htmlOfExp model e =
  let recurse = htmlOfExp model in
  let e_ = e.val in
  let e__ = e_.e__ in
  case e__ of
    EConst ws n loc wd -> hConcat [ Html.text ws, htmlOfConst model ws n loc wd ]
    EBase ws baseVal ->
      let attrs = [ {- literalStyle -} ] in
      case baseVal of
        Bool b   -> Html.span attrs [ Html.text ws, Html.text <| toString b ]
        String s -> Html.span attrs [ Html.text ws, Html.text <| "\'" ++ s ++ "\'" ]
        Star     -> Html.span attrs [ Html.text ws, Html.text <| toString Star ]
    EOp ws1 op es ws2 ->
      let (pre, suf) = (ws1 ++ "(", ws2 ++ ")") in
      let hs = List.map recurse es in
      Html.span [ basicStyle ]
        (List.concat [[Html.text pre, Html.text (strOp op.val)], hs, [Html.text suf]])
    EVar ws x -> hConcat [ Html.text ws, Html.text x ]
    EFun ws1 [p] e1 ws2 ->
      let (pre, suf) = (ws1 ++ "(\\", ws2 ++ ")") in
      Html.span [ basicStyle ]
        [Html.text pre, htmlOfPat model p, recurse e1, Html.text suf]
    EFun ws1 ps e1 ws2 ->
      let (pre, suf) = (ws1 ++ "(\\", ws2 ++ ")") in
      let hs = List.map (htmlOfPat model) ps in
      Html.span [ basicStyle ]
        (List.concat [[Html.text pre, Html.text "("], hs, [Html.text ")", recurse e1, Html.text suf]])
    EApp ws1 e1 es ws2 ->
      let (pre, suf) = (ws1 ++ "(", ws2 ++ ")") in
      let (h1, hs) = (recurse e1, List.map recurse es) in
      Html.span [ basicStyle ] (Html.text pre :: h1 :: hs ++ [Html.text suf])
    ELet ws1 Let rec p e1 e2 ws2 ->
      let tok = if rec then "letrec" else "let" in
      let (pre, suf) = (ws1 ++ "(" ++ tok, ws2 ++ ")") in
      Html.span [ basicStyle ]
        [Html.text pre, htmlOfPat model p, recurse e1, recurse e2, Html.text suf]
    ELet ws1 Def rec p e1 e2 ws2 ->
      let tok = if rec then "defrec" else "def" in
      let (pre, suf) = (ws1 ++ "(" ++ tok, ws2 ++ ")") in
      Html.span [ basicStyle ]
        [Html.text pre, htmlOfPat model p, recurse e1, Html.text suf, recurse e2]
    EList ws1 es ws2 Nothing ws3 ->
      let (pre, suf) = (ws1 ++ "[", ws3 ++ "]") in
      Html.span [ basicStyle ]
        [Html.text pre, hConcat (List.map recurse es), Html.text ws2, Html.text suf]
    EList ws1 es ws2 (Just eRest) ws3 ->
      let (pre, mid, suf) = (ws1 ++ "[", ws2 ++ "|", ws3 ++ "]") in
      Html.span [ basicStyle ]
        [Html.text pre, hConcat (List.map recurse es), Html.text mid, recurse eRest, Html.text suf]
    EIf ws1 e1 e2 e3 ws2 ->
      let (pre, suf) = (ws1 ++ "(", ws2 ++ ")") in
      Html.span [ basicStyle ]
        [Html.text pre, recurse e1, recurse e2, recurse e3, Html.text suf]
    EComment ws s e1 ->
      Html.span [ basicStyle ]
        [Html.text ws, Html.text (";" ++ s ++ "\n"), recurse e1]
    ECase ws1 e bs ws2 ->
      let (pre, suf) = (ws1 ++ "(", ws2 ++ ")") in
      Html.span [ basicStyle ]
        (List.concat [[Html.text pre], List.map (htmlOfBranch model) bs, [Html.text suf]])
    _ -> 
      -- let _ = Debug.log "VisualEditor.HtmlOfExp no match :" (toString e) in
      let s = Unparser.unparse e in
      Html.pre [] [ Html.text s ]

htmlOfPat : Model_ a -> Pat -> Html
htmlOfPat model p =
  let recurse = htmlOfPat model in
  case p.val of
    PVar ws x _ -> htmlOfPVar model ws x
    PConst ws n -> Html.span [ literalStyle ] [ Html.text <| toString n ]
    PBase ws baseVal -> Html.span [ literalStyle ] [ Html.text <| toString baseVal ]
    PList ws1 ps ws2 rest ws3 ->
      let (hPre, hSuf) = ([Html.text (ws1 ++ "[")], [Html.text (ws3 ++ "]")]) in
      let hRest =
        case rest of
          Nothing    -> [Html.text ws2]
          Just pRest -> [Html.text (ws2 ++ "|"), recurse pRest]
      in
      Html.span [ basicStyle ]
        (List.concat [hPre, List.map recurse ps, hRest, hSuf])

htmlOfBranch : Model_ a -> Branch -> Html
htmlOfBranch model b =
  let (Branch_ ws1 p e ws2) = b.val in
  let (pre, suf) = (ws1 ++ "(", ws2 ++ ")") in
  Html.span [ basicStyle ] <|
    [Html.text pre, htmlOfPat model p, htmlOfExp model e, Html.text suf]


------------------------------------------------------------------------------
-- Basic Driver

type Event
  = UpdateModel (Model -> Model)
  | HtmlUpdate String
  | SpanValue SpanValue
      -- keeping SpanValue and HtmlUpdate ("theSourceCode") separate for now

type alias SpanValue = (String, String)

myMailbox : Mailbox Event
myMailbox = mailbox (UpdateModel identity)

btnMailbox : Mailbox ()
btnMailbox = mailbox ()

deuceQueryMailbox : Mailbox String
deuceQueryMailbox = mailbox "NOTHING YET"

type alias Model = Model_ {}

type alias Model_ a =
  { a
  | exName : String
  , code : String
  , inputExp : Exp
  , textChangedAt : Maybe Time
  }

{--

initModel =
  { exName = Examples.scratchName
  , code = Examples.scratch
  , inputExp = Utils.fromOk_ (Parser.parseE Examples.scratch)
  , textChangedAt = Nothing
  }

upstate : Event -> Model -> Model
upstate evt model =
  case evt of
    UpdateModel f -> f model
    HtmlUpdate code ->
      case Parser.parseE code of
        Err _  -> model
        Ok exp -> { model | inputExp = exp, code = code, textChangedAt = Nothing }
    SpanValue (x, x') ->
      if x == x' || String.contains " " x'
        then model
        else renameVar x x' model

-- http://stackoverflow.com/questions/32426042/how-to-print-index-of-selected-option-in-elm
targetSelectedIndex : Decode.Decoder Int
targetSelectedIndex = Decode.at ["target", "selectedIndex"] Decode.int

view : Model -> Html
view model =
  let testString = model.code in
  let testExp =
    case Parser.parseE testString of
      Err _ -> Debug.crash "main: bad parse"
      Ok e  -> e
  in
  let break = Html.br [] [] in
  let body =
    let options =
      flip List.map Examples.list <| \(name,code,_) ->
        Html.option
           [Attr.value name, Attr.selected (name == model.exName)]
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
             let (name,code,_) = Utils.geti (i+1) Examples.list in
             Signal.message myMailbox.address <| UpdateModel <| \_ ->
               let exp = Utils.fromOk_ (Parser.parseE code) in
               { exName = name, code = code, inputExp = exp, textChangedAt = Nothing }
         ]
         options
    in
    let hExp = Html.div [ Attr.id "theSourceCode" ] [ htmlOfExp model testExp ] in
    let btn =
      Html.button
         [ Attr.contenteditable False
         , Events.onClick btnMailbox.address () ]
         [ Html.text "Reparse" ] in
    Html.node "div"
       -- turning off contenteditable for now
       [ basicStyle ]
       [ dropdown, btn, break, hExp ]
  in
  body

events : Signal Event
events =
  Signal.merge myMailbox.signal eventsFromJS

eventsFromJS : Signal Event
eventsFromJS =
  let foo (id,s) =
    if id == "theSourceCode"
      then HtmlUpdate s
      else SpanValue (id, s)
  in
  Signal.map foo sourceCodeSignalFromJS

-- port sourceCodeSignalFromJS : Signal String
port sourceCodeSignalFromJS : Signal (String, String)

-- port sourceCodeSignalToJS : Signal ()
port sourceCodeSignalToJS : Signal String
port sourceCodeSignalToJS =
  Signal.mergeMany
    [ Signal.map (always "theSourceCode") btnMailbox.signal
    , queryMailbox.signal
    ]
  -- btnMailbox.signal
  -- Signal.sampleOn (Time.every (4 * Time.second)) (Signal.constant ())

main : Signal Html
main = Signal.map view (Signal.foldp upstate initModel events)

--}
