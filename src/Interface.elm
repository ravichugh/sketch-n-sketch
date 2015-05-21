-- Interface.elm
-- This defines and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (parseE, parseV)
import Sync exposing (sync)
import Eval exposing (run)
import MainSvg
import Utils
import MicroTests
import InterfaceUtils exposing (..)
import LangSvg
import VirtualDom

--Core Libraries
import List 
import Dict
import String 
import Graphics.Element as GE 
import Graphics.Collage as GC

--Signaling Libraries
import Mouse 
import Window 

--Html Libraries
import Html 
import Html.Attributes as Attr
import Html.Events as Events

--Svg Libraries
import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

--Error Checking Libraries
import Debug

-- Model --
--Fields:
-- code            - Text currently in the textbox
--inputExp         - input Expression
-- objects         - The workingVal translated to manipulable SVGs
-- movingObj       - If an object is being moved, which one
-- inputVal        - The last code input parsed into a Val
--                   (changes only after picking an output of sync)
-- workingVal      - The inputVal after applying the manipulations performed on
--                   the graphics side (done on the fly)
-- possibleChanges - The possible new expressions and their associated Vals, 
--                   as from the output of sync
-- syncMode        - True if state should be non-manipulatable/sync selecting
type alias Model = { code : String
                   , inputExp : Maybe Exp
                   , objects : List Object
                   , movingObj : Maybe (LangSvg.NodeId, Float, Float)
                   , inputVal : Val
                   , workingVal : Val
                   , workingSlate : LangSvg.IndexedTree
                   , possibleChanges : List ((Exp, Val), Float)
                   , syncMode : Bool
                   }

--An Object is composed of an svg, list of attribute key/values
type alias Object = (Svg.Svg, List (String, String))

tempTest = MicroTests.test41 ()

--A Sample model for working with a given microtest
sampleModel = { code      = sExp tempTest.e
              , inputExp  = Just tempTest.e
              , objects   = buildVisual <| LangSvg.valToIndexedTree tempTest.v 
              , movingObj = Nothing
              , inputVal = tempTest.v
              , workingVal = tempTest.v
              , workingSlate = LangSvg.valToIndexedTree tempTest.v
              , possibleChanges = []
              , syncMode = False
              }

--Event
--CodeUpdate : carries updated string of code with it
--SelectObject : carries an id of an object and an identifying string for a zone
--DeselectObject : carries an id of an object which shall no longer be selected
--                  for alteration.
--MouseDown : carries a position of mouse on a down click
--Sync : signals the system to enter syncMode
--SelectOption : carries a possiblechange pane from sync to be displayed as the new
--              console
--Render : display a given val from the code
type Event = CodeUpdate String
           | SelectObject Int String
           | DeselectObject Int
           | MouseDown (Int, Int)
           | Sync
           | SelectOption ((Exp, Val), Float)
           | Render

--A mailbox for signaling the model
events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

-- Update --
upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of
    --make a val from code and display it
    Render -> case Debug.log "run" <| Eval.run <| parseE old.code of
        bare -> let vals = VList [VBase (String "svg"), VList [], bare] in 
            { old | objects      <- buildVisual 
                                    <| LangSvg.valToIndexedTree vals
                  , inputVal     <- vals
                  , workingVal   <- vals
                  , workingSlate <- LangSvg.valToIndexedTree vals
            }
    --Replace old code with new code
    CodeUpdate newcode -> { old | code <- newcode }
    --check if a mouse position is within an object when a mouse down
    --event occured.
    MouseDown (mx, my) -> case old.movingObj of
        Nothing                  -> old
        Just (objid, xdist, ydist) -> if
            | xdist == -1.0 || ydist == -1.0 -> 
                case Dict.get objid old.workingSlate of 
                    Just node ->
                        case buildSvg (objid, node) of
                            (svg, attrs) -> 
                                let xpos = case String.toFloat <| Utils.find_ attrs "x" of
                                        Ok a -> a
                                    ypos = case String.toFloat <| Utils.find_ attrs "y" of
                                        Ok a -> a
                                in { old | movingObj <- Just (objid 
                                            , xpos - Basics.toFloat mx
                                            , ypos - Basics.toFloat my) 
                                   }
            | otherwise -> 
                let newpos = [ ("x", toString <| Basics.toFloat mx + xdist)
                             , ("y", toString <| Basics.toFloat my + ydist) ]
                    newSlate = List.foldr (updateSlate objid)
                                old.workingSlate
                                [ ("x", toString <| Basics.toFloat mx + xdist)
                                , ("y", toString <| Basics.toFloat my + ydist)
                                ]
                    newobjs = buildVisual newSlate
                in  { old | objects <- newobjs 
                          , workingSlate <- newSlate
                    }
    --Selecting a given zone within an object
    SelectObject id zonetype -> { old | movingObj <- Just (id, -1.0, -1.0) }
    --wipes out selection of an object
    DeselectObject x -> { old | movingObj <- Nothing }
    --run sync function and then populate the list of possibleChanges
    Sync -> 
        case old.inputExp of
            Just ip -> 
                let inputval = Eval.run ip
                    newval = indexedTreeToVal old.workingSlate
                in case (Result.toMaybe <| sync ip inputval newval) of
                    Just ls -> { old | possibleChanges <- ls
                                     , syncMode <- True 
                               }
                    Nothing -> old
            _       -> old
    --Given possible changes, an option is selected. Vals are correspondingly
    --updated and syncMode is turned off.
    SelectOption ((e,v), f) -> { old | possibleChanges <- []
                                     , inputVal <- v
                                     , workingVal <- v
                                     , inputExp <- Just e
                                     , syncMode <- False 
                               }
    --catch all
    _ -> old
 
--TODO: fix object/zone tracking issue

--given a list of attributes and their new values, update an object and return it
updateObj : List (String, String) -> Object -> Object -> Object
updateObj newattrs (o1, a1) (o2, a2) = case Debug.log "index" (Utils.find_ a1 "index") of
  a ->
    --check for matching indices
    if | ((Utils.find_ a1 "index") == (Utils.find_ a2 "index")) ->
                --update the exsisting attrs
                let updatedattrs = updateAttrStrs newattrs a1
                    --since first 2 attrs are shape/id, remove them for svg creation
                    svgattrs = List.map (\(x,y) -> LangSvg.attr x <| y) (List.drop 2 updatedattrs)
                    --find correct shape function
                    shape = LangSvg.svg (Utils.find_ a1 "shape")
                in ((shape svgattrs []), updatedattrs) 
       | otherwise -> (o2, a2)


-- View --
codeBox : String -> Bool -> Html.Html
codeBox code switch =
    let
        --depending on switch, toggle manipulatability
        event = case switch of
            True -> []
            False ->  [(Events.on "input" Events.targetValue
                (Signal.message events.address << CodeUpdate))]
    in
        --build text area
        Html.textarea
            ([ Attr.id "codeBox"
            , Attr.style
                [ ("height", "100%")
                , ("width",  "100%")
                , ("resize", "none")
                , ("overflow", "scroll")
                ]
            , Attr.value code
            ]
            ++
            --add event, if it exsists
            event)
            []

--Build viusal pane and populate with the model's objects
visualsBox : List Object -> Float -> Bool -> Html.Html
visualsBox objects dim switch =
    Svg.svg [ Attr.style
                [ ("width", "100%")
                , ("height", "100%")
                ]
            ] <| List.map (\(f,g) -> f) objects

--Umbrella function for taking and indexed tree and calling buildSvg over it
buildVisual : LangSvg.IndexedTree -> List (Svg.Svg, List (String, String))
buildVisual valDict = List.map buildSvg (Dict.toList valDict)

--Function for handling attributes and children of an indexed tree and building them
--into Svgs with attr lists to be updated as necessary
buildSvg : (LangSvg.NodeId, LangSvg.IndexedTreeNode) -> (Svg.Svg, List (String, String))
buildSvg (nodeID, node) = case node of
    --if text, call svg text creation
    LangSvg.TextNode text -> (VirtualDom.text text, [("shape", "TEXT"), ("text", text)])
    --If svg object, make objects with appropriate zones and attributes
    LangSvg.SvgNode shape attrs childrenids ->
       let attrstrs = getAttrs attrs
           zones = makeZones shape nodeID attrstrs
           mainshape = (LangSvg.svg shape <| LangSvg.valsToAttrs attrs) []
       in (Svg.svg [] (mainshape :: zones), attrstrs)

compileAttrNum k v = LangSvg.attr k (toString v)
toAttrNum          = Utils.fromOk_ << String.toFloat

onMouseDown = Svg.Events.onMouseDown << Signal.message events.address
onMouseUp   = Svg.Events.onMouseUp   << Signal.message events.address

--Zone building function (still under construction/prone to change)                
makeZones : String -> LangSvg.NodeId -> List (String, String) -> List Svg.Svg
makeZones shape nodeID l =
  case shape of

    "rect" ->
        let [x,y,w,h] = List.map (toAttrNum << Utils.find_ l) ["x","y","width","height"] in
        let gutterPct = 0.125 in
        let zInterior =
          flip Svg.rect [] [
              compileAttrNum "x" (round <| x + w * gutterPct)
            , compileAttrNum "y" (round <| y + h * gutterPct)
            , compileAttrNum "width" (w * (1 - 2*gutterPct))
            , compileAttrNum "height" (h * (1 - 2*gutterPct))
            , LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
            , LangSvg.attr "strokeWidth" "3"
            , LangSvg.attr "fill" "rgba(0,0,0,0)"
            , onMouseDown (SelectObject nodeID "Interior")
            , onMouseUp (DeselectObject nodeID)
            ]
        in
        [zInterior]

    _ -> []

--Umbrella function for viewing a given model
view : (Int, Int) -> Model -> Html.Html
view (w,h) model = 
    let
        dim = (Basics.toFloat (Basics.min w h)) / 2
    in
        --check whether there is syncing occuring
        case model.syncMode of
            --no sync, do normal displaying of code & visuals
            False -> Html.div
                    [ Attr.style
                        [ ("width", toString w)
                        , ("height", toString h)
                        ]
                    ]
                    --display code & visuals
                    [renderView (w,h) model
                    , Html.button
                        [ Attr.style
                            [ ("position", "absolute")
                            , ("left", String.append (toString <| w // 6) "px")
                            , ("top", String.append (toString <| h - 40) "px")
                            , ("type", "button")
                            , ("width", "100px")
                            , ("height", "40px")
                            ]
                        , Events.onClick events.address Render
                        , Attr.value "Render"
                        , Attr.name "Render the Code"
                        ]
                        [Html.text "render"]
                    , Html.button
                        [ Attr.style
                            [ ("position", "absolute")
                            , ("left", String.append (toString <| w // 4) "px")
                            , ("top", String.append (toString <| h - 40) "px")
                            , ("type", "button")
                            , ("width", "100px")
                            , ("height", "40px")
                            ]
                        , Events.onClick events.address Sync
                        , Attr.value "Sync"
                        , Attr.name "Sync the code to the canvas"
                        ]
                        [Html.text "sync"]
                    ]
            --call special view for syncing
            True -> syncView (w,h) model

--When view is manipulatable, call this function for code & visuals
--to build corresponding panes
renderView : (Int, Int) -> Model -> Html.Html
renderView (w,h) model = 
    let
        dim = (Basics.toFloat (Basics.min w h)) / 2
    in
        Html.div
            [ Attr.style
                [ ("width", toString w)
                , ("height", toString h)
                ]
            ]
            [ Html.div 
                [ Attr.style
                    [ ("width", String.append (toString <| w // 2 - 1) "px")
                    , ("height", String.append (toString <| h - 60) "px")
                    , ("margin", "0")
                    , ("position", "absolute")
                    , ("left", "0px")
                    , ("top", "0px")
                    ]
                ]
                [codeBox model.code model.syncMode]
            , Html.div
                [ Attr.style
                    [ ("width", String.append (toString <| w // 2 - 1) "px")
                    , ("height", String.append (toString h) "px")
                    , ("margin", "0")
                    , ("position", "absolute")
                    , ("left", String.append (toString <| w // 2) "px")
                    , ("top", "0px")
                    ]
                ]    
                [visualsBox model.objects dim model.syncMode]
            ]

--Build an Html of the iterations of renderOption over the possibleChanges
syncView : (Int, Int) -> Model -> Html.Html
syncView (w,h) model = 
    let
        dim = (Basics.toFloat (Basics.min w h)) / 2
    in
        Html.div
        []
        --index the possible changes and render these options
        (renderOption (w, h // 4) (Utils.mapi (\x -> x) model.possibleChanges) model dim)
            
--Given a possible Change, build code from Expr and visuals from the val, rank by priority w/ mapi
renderOption : (Int, Int) -> List (Int, ((Exp, Val), Float)) -> Model -> Float -> List Html.Html
renderOption (w,h) possiblechanges model dim =
    case possiblechanges of
        --if there is a possible change remaining, display this option
        (i, ((e,v), f))::ps -> 
            (Html.div
                [ Attr.style
                    [ ("width", toString w)
                    , ("height", toString h)
                    , ("top", String.append (toString <| h * (i-1)) "px")
                    , ("position", "absolute")
                    ]
                ]
                [ Html.div 
                    [ Attr.style
                        [ ("width", String.append (toString <| w // 2 - 30) "px")
                        , ("height", String.append (toString <| h) "px")
                        , ("margin", "0")
                        , ("position", "absolute")
                        , ("left", "0px")
                        , ("top", "0px") -- String.append (toString <| h * (i-1)) "px")
                        ]
                    ]
                    [codeBox (sExpK 1 e) model.syncMode]
                , Html.div
                    [ Attr.style
                        [ ("width", String.append (toString <| w // 2 - 50) "px")
                        , ("height", String.append (toString h) "px")
                        , ("margin", "0")
                        , ("position", "absolute")
                        , ("left", String.append (toString <| w // 2) "px")
                        , ("top", "0px") --String.append (toString <| h * (i-1)) "px")
                        ]
                    ]    
                    [visualsBox (buildVisual <| LangSvg.valToIndexedTree v) dim model.syncMode] --TODO: parse val to svgs
--                , Html.button
--                    [ Attr.style
--                        [ ("position", "absolute")
--                        , ("left", String.append (toString <| w // 4) "px")
--                        , ("top", "0px) --String.append (toString <| h - 40) "px")
--                        , ("type", "button")
--                        , ("width", "100px")
--                        , ("height", "40px")
--                        ]
--                    , Events.onClick events.address (SelectOption ((e,v), f))
--                    , Attr.value "Select"
--                    , Attr.name "Select this codebox and visualbox"
--                    ]
--                    [Html.text "select"]
                --attach remaining option htmls
                ]) :: renderOption (w,h) ps model dim
        [] -> []


-- Main --
main : Signal Html.Html
main = let sigModel = Signal.foldp upstate sampleModel
                        <| Signal.mergeMany
                            [ events.signal
                            , Signal.map2 (,) Mouse.isDown Mouse.position
                                |> Signal.filter (\(x,y) -> x) (False, (0,0))
                                |> Signal.map (\(x,y) -> y)
                                |> Signal.map2 adjustCoords Window.dimensions
                                |> Signal.map MouseDown
                            ]
       in Signal.map2 view Window.dimensions sigModel
