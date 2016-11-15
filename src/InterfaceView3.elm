module InterfaceView3 exposing (view)

-- Sketch-n-Sketch Libraries ---------------------------------------------------

import Config exposing (params)
import Utils
import ExamplesGenerated as Examples

import InterfaceModel as Model exposing
  ( Msg, Event(..), Model, Tool(..), ShapeToolKind(..), Mode(..)
  , Caption(..), MouseMode(..)
  , mkLive_
  )
import Canvas
import LangSvg
import Sync

-- Elm Libraries ---------------------------------------------------------------

import Set
import Dict

import Svg exposing (Svg)
import Svg.Events exposing (onMouseDown, onMouseUp, onMouseOver, onMouseOut)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput, on)
import Json.Decode


--------------------------------------------------------------------------------

pixels n = toString n ++ "px"

imgPath s = "img/" ++ s

-- TODO move to Config
showRawShapeTools = False


--------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
  let layout = Config.computeLayoutInfo model in
  let wLogo = params.topSection.wLogo in
  let top =
    Html.div
       [ Attr.style
           [ ("height", pixels layout.hTop)
           , ("padding", "2pt")
           ] ]
       [ Html.span [] [ Html.text ("Sketch-n-Sketch " ++ params.strVersion) ]
       , Html.img
           [ Attr.src (imgPath "../sketch-n-sketch-logo.png")
           , Attr.style
               [ ("width", pixels wLogo)
               , ("height", pixels wLogo)
               ]
           ]
           []
       , heuristicsButton model
       , orientationButton model
       , outputButton model
       , ghostsButton model
       , caption model
       ]
  in
  let bot =
    Html.div
       [ Attr.style [] ]
       [ Html.span
           [ Attr.style [ ("width", pixels layout.wCodebox)
                        , ("display", "inline-block")
                        , ("padding", "2pt")
                        ] ]
           [ dropdownExamples model
           , runButton
           , undoButton model
           , redoButton model
           , cleanButton model
           ]
       , Html.span
           [ Attr.style [ ("width", pixels layout.wCanvas)
                        , ("display", "inline-block")
                        , ("padding", "2pt")
                        ] ]
           [ toolButton model Cursor
           , toolButton model (Line Raw)
           , toolButton model (Rect Raw)
           , toolButton model (Oval Raw)
           , toolButton model (Poly Raw)
           , toolButton model (Path Raw)
           , relateButton model "Dig" DigHole
           , relateButton model "A = B" MakeEqual
           , groupButton model "Dupe" DuplicateBlobs
           , groupButton model "Merge" MergeBlobs
           , groupButton model "Group" GroupBlobs
           , groupButton model "Abs" AbstractBlobs
           ]
       ]
  in
  let codebox =
    textArea model model.code <|
      [ onInput CodeUpdate
      , Attr.style [ ("width", pixels layout.wCodebox)
                   ]
      ] in
  let outputArea =
    case (model.errorBox, model.mode) of
      (Nothing, Print svgCode) ->
        textArea model svgCode
          [ Attr.style [ ("width", pixels layout.wCodebox) ] ]
      (Nothing, _) ->
        Canvas.build layout.wCanvas layout.hCanvas model
      (Just errorMsg, _) ->
        textArea model errorMsg
          [ Attr.style [ ("width", pixels layout.wCodebox) ] ]
  in
  let canvas =
    Html.div
       [ Attr.style [ ("width", pixels layout.wCanvas)
                    , ("height", pixels layout.hCanvas)
                    , ("display", "inline-block")
                    , ("border", params.mainSection.canvas.border)
                    ] ]
       [ outputArea ]
  in
  Html.div []
    [ Html.div []
        [ top
        , Html.div
            [ Attr.style [ ("height", pixels layout.hMid) ] ]
            [ codebox, canvas ]
        , bot
        ]
    ]


--------------------------------------------------------------------------------

textArea model text attrs =
  let layout = Config.computeLayoutInfo model in
  let innerPadding = 4 in
  -- NOTE: using both Attr.value and Html.text seems to allow read/write...
  let commonAttrs =
    [ Attr.spellcheck False
    , Attr.value text
    , Attr.style
        [ ("font-family", params.mainSection.codebox.font)
        , ("font-size", params.mainSection.codebox.fontSize)
        , ("border", params.mainSection.codebox.border)
        , ("whiteSpace", "pre")
        , ("height", "100%")
        -- , ("height", pixels hMid)
        -- , ("width", "50%")
{-
        , ("width", pixels layout.wCodebox)
-}
        , ("resize", "none")
        , ("overflow", "auto")
        -- Horizontal Scrollbars in Chrome
        , ("word-wrap", "normal")
        -- , ("background-color", "whitesmoke")
        , ("background-color", "white")
        , ("padding", toString innerPadding ++ "px")
        -- Makes the 100% for width/height work as intended
        , ("box-sizing", "border-box")
        ]
    ]
  in
  Html.textarea (commonAttrs ++ attrs) [ Html.text text ]


--------------------------------------------------------------------------------

type ButtonKind = Regular | Selected | Unselected

htmlButton text onClickHandler btnKind disabled =
  let color =
    case btnKind of
      Regular    -> "white"
      Unselected -> "white"
      Selected   -> "lightgray"
  in
  let commonAttrs =
    [ Attr.disabled disabled
    , Attr.style [ ("font", params.mainSection.widgets.font)
                 , ("fontSize", params.mainSection.widgets.fontSize)
                 , ("background", color)
                 ] ]
  in
  Html.button (commonAttrs ++ [ onClick onClickHandler ]) [ Html.text text ]

runButton =
  htmlButton "Run" Run Regular False

cleanButton model =
  let disabled =
    case model.mode of
      Live _ -> False
      _      -> True
  in
  htmlButton "Clean Up" WaitClean Regular disabled

undoButton model =
  let past = Tuple.first model.history in
  htmlButton "Undo" Undo Regular (List.length past <= 1)

redoButton model =
  let future = Tuple.second model.history in
  htmlButton "Redo" Redo Regular (List.length future == 0)

heuristicsButton model =
  let foo old =
    let so = old.syncOptions in
    let so_ = { so | feelingLucky = Sync.toggleHeuristicMode so.feelingLucky } in
    case old.mode of
      Live _ ->
        case mkLive_ so_ old.slideNumber old.movieNumber old.movieTime old.inputExp of
          Ok m_ -> { old | syncOptions = so_, mode = m_ }
          Err s -> { old | syncOptions = so_, errorBox = Just s }
      _ -> { old | syncOptions = so_ }
  in
  let yesno =
    let hm = model.syncOptions.feelingLucky in
    if hm == Sync.heuristicsNone then "None"
    else if hm == Sync.heuristicsFair then "Fair"
    else "Biased"
  in
  htmlButton ("[Heuristics] " ++ yesno) (UpdateModel foo) Regular False

orientationButton model =
  let text = "[Orientation] " ++ toString model.orient in
  htmlButton text SwitchOrient Regular False

outputButton model =
  let disabled = model.mode == AdHoc in
  let cap =
     case model.mode of
       Print _ -> "[Out] SVG"
       _       -> "[Out] Canvas"
  in
  htmlButton cap ToggleOutput Regular disabled

ghostsButton model =
  let cap =
     case model.showGhosts of
       True  -> "[Ghosts] Shown"
       False -> "[Ghosts] Hidden"
  in
  let foo old =
    let showGhosts_ = not old.showGhosts in
    let mode_ =
      case old.mode of
        Print _ -> Print (LangSvg.printSvg showGhosts_ old.slate)
        _       -> old.mode
    in
    { old | showGhosts = showGhosts_, mode = mode_ }
  in
  htmlButton cap (UpdateModel foo) Regular False

toolButton model tool =
  let capStretchy s = if showRawShapeTools then "BB" else s in
  let capSticky = Utils.uniPlusMinus in -- Utils.uniDelta in
  let capRaw = "(Raw)" in
  let cap = case tool of
    Cursor        -> "Cursor"
    Line Raw      -> "Line"
    Rect Raw      -> "Rect"
    Rect Stretchy -> capStretchy "Box"
    Oval Raw      -> "Ellipse"
    Oval Stretchy -> capStretchy "Oval"
    Poly Raw      -> "Polygon"
    -- Poly Stretchy -> capStretchy "Polygon"
    Poly Stretchy -> capStretchy "Poly"
    Poly Sticky   -> capSticky
    Path Raw      -> "Path"
    Path Stretchy -> capStretchy "Path"
    Path Sticky   -> capSticky
    Text          -> "Text"
    HelperLine    -> "(Rule)"
    HelperDot     -> "(Dot)"
    Lambda        -> Utils.uniLambda
    _             -> Debug.crash ("toolButton: " ++ toString tool)
  in
  -- TODO temporarily disabling a couple tools
  let (btnKind, disabled) =
    case (model.tool == tool, tool) of
      (True, _)            -> (Selected, False)
      (False, Path Sticky) -> (Regular, True)
      (False, _)           -> (Unselected, False)
  in
  htmlButton cap (UpdateModel (\m -> { m | tool = tool })) btnKind disabled

relateButton model text handler =
  let noFeatures = Set.isEmpty model.selectedFeatures in
  htmlButton text handler Regular noFeatures

groupButton model text handler =
  let noFeatures = Set.isEmpty model.selectedFeatures in
  let noBlobs = Dict.isEmpty model.selectedBlobs in
  htmlButton text handler Regular (noBlobs || not noFeatures)


--------------------------------------------------------------------------------

dropdownExamples model =
  let options =
    List.map (\(name,_) -> Html.option [] [ Html.text name ]) Examples.list
  in
  let mapTargetValue name =
    case Utils.maybeFind name Examples.list of
      Just (_, thunk) -> SelectExample name thunk
      Nothing         -> let _ = Debug.log "WARN: not found:" name in Noop
  in
  Html.select
    [ on "change" (Json.Decode.map mapTargetValue Html.Events.targetValue)
    , Attr.style
        [ ("pointer-events", "auto")
        , ("border", "0 solid")
        -- , ("display", "block")
        -- , ("width", "120px")
        , ("width", "200px")
        , ("height", "24px")
        , ("font-family", params.mainSection.widgets.font)
        -- , ("font-size", "1em")
        , ("font-size", params.mainSection.widgets.font)
        ]
    ]
    options


--------------------------------------------------------------------------------

caption model =
  let (text, color) =
    case (model.caption, model.mode, model.mouseMode) of
      (Just (Hovering zoneKey), Live info, MouseNothing) ->
        case Sync.hoverInfo zoneKey info of
          (line1, Nothing) ->
            (line1 ++ " (INACTIVE)", "red")
          (line1, Just line2) ->
            (line1 ++ " (ACTIVE)\n" ++ line2, "green")

      (Just (LangError err), _, _) ->
        (err, "black")

      _ ->
        ("", "white")

  in
  Html.span
    [ Attr.style [ ("color", color) ] ]
    [ Html.text text ]
