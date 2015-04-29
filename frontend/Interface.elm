-- Interface.elm
-- This defines and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (freshen, parseE, parseV)
import Sync exposing (sync)

import List 
import Dict
import String 
import Graphics.Element as GE 
import Graphics.Collage as GC

import Mouse 
import Window 
import Html 
import Html.Attributes as Attr
import Html.Events as Events

import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

import Debug

-- Model --
--Fields:
-- code            - Text currently in the textbox
-- objects         - The workingVal translated to manipulable SVGs
-- movingObj       - If an object is being moved, which one
-- inputVal        - The last code input parsed into a Val
--                   (changes only after picking an output of sync)
-- workingVal      - The inputVal after applying the manipulations performed on
--                   the graphics side (done on the fly)
-- possibleChanges - The possible new expressions and their associated Vals, 
--                   as from the output of sync
type alias Model = { code : String
                   , objects : List Object
                   , movingObj : Maybe (Object, Float, Float)
                   , inputVal : Val
                   , workingVal : Val
                   , possibleChanges : List ((Exp, Val), Int)
                   }

type alias Object = (Svg.Svg, Int, Int)

initModel = { code = ""
            , objects = []
            , movingObj = Nothing
            , inputVal = VHole
            , workingVal = VHole
            , possibleChanges = []
            }

--Just as in microTests
sampleFields se =
    let e = freshen (parseE se)
        v = Eval.run e
    in {e=e, v=v}

sampleCode = 
    "(letrec map (\\(f xs) (case xs ([] []) ([hd|t1] [(f hd)|(map f t1)])))
     (letrec mult (\\(m n) (if (< m 1) 0 (+ n (mult (- m 1) n))))
     (let [x0 y0 sep] [10 8 30]
       (map (\\i [(+ x0 (mult i sep)) y0]) [0 1 2]))))"

sampleVals = sampleFields sampleCode

sampleModel = { code      = sampleCode
              , objects   = [] --TODO buildSVG sampleVals.v
              , movingObj = Nothing
              , inputVal = sampleVals.v
              , workingVal = sampleVals.v
              , possibleChanges = []
              }

type Event = CodeUpdate String
           | OutputUpdate String
           | SelectObject (List Int)
           | DeselectObject (List Int)
           | MouseDown (Int, Int)

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

-- Update --
upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of
    CodeUpdate newcode -> { old | code <- newcode }
    MouseDown (mx, my) -> case old.movingObj of
        Nothing                 -> old
        Just (obj, xdist, ydist) -> if
            | xdist == -1.0 || ydist == -1.0 -> case obj of
                (svg, xpos, ypos) -> 
                    { old | movingObj <- Just (obj 
                                               , Basics.toFloat <| xpos -  mx
                                               , Basics.toFloat <| ypos - my) }
            | otherwise -> 
                let newpos = (Basics.toFloat mx + xdist, Basics.toFloat my + ydist)
                    newobjs = List.map (updateObjPos newpos obj) old.objects
                    moved = updateObjPos newpos obj obj
                in  { old | objects <- newobjs 
                          , movingObj <- Just 
                                (moved, xdist, ydist)
                    }
    SelectObject [x,y] -> let match = List.filter 
                                (\(o,x2,y2) -> x == x2 && y == y2)
                                old.objects
                          in case match of
                              mat :: xs -> { old | movingObj <- Just (mat, -1.0, -1.0) }
    DeselectObject [x,y] -> { old | movingObj <- Nothing }
    _ -> old


pickObj : (Int, Int) -> List Object -> Maybe Object
pickObj (mx, my) objs = case objs of
    [] -> Nothing
    (form, x, y) :: xs -> if | abs (Basics.toFloat <| x - mx) <= 20 
                               && abs (Basics.toFloat <| y - my) <= 20 -> Just (form, x, y)
                             | otherwise -> pickObj (mx, my) xs

updateObjPos : (Float, Float) -> Object -> Object -> Object
updateObjPos (newx, newy) (o1,x1,y1) (o2,x2,y2) = if
    | (x1,y1) == (x2,y2) -> 
        case buildSquare [round newx, round newy] of
            Just sq -> sq
    | otherwise -> (o2,x2,y2)

adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)

-- View --
codeBox : String -> Html.Html
codeBox codeText =
    Html.textarea
        [ Attr.id "codeBox"
        , Attr.style
            [ ("height", "100%")
            , ("width",  "100%")
            , ("resize", "none")
            , ("overflow", "scroll")
            ]
        , Attr.value codeText
        , Events.on "input" Events.targetValue
            (Signal.message events.address << CodeUpdate)
        ]
        []

visualsBox : Model -> Float -> Html.Html
visualsBox model dim =
    let
        intdim = floor (dim/20)
    in 
        Svg.svg [ Attr.style
                    [ ("width", "100%")
                    , ("height", "100%")
                    ]
                ] <| List.map (\(f,x,y) -> f) model.objects 

buildSquare : List Int -> Maybe (Svg.Svg, Int, Int)
buildSquare coords =
    case coords of
        [x,y] -> 
                Just (Svg.rect
                    [ Svg.Attributes.x <| toString x 
                    , Svg.Attributes.y <| toString y
                    , Svg.Attributes.width "60"
                    , Svg.Attributes.height "60"
                    , Svg.Attributes.fill "blue"
                    , Svg.Events.onMouseDown (Signal.message events.address
                        (SelectObject coords)) --TODO id of some sort
                    , Svg.Events.onMouseUp (Signal.message events.address
                        (DeselectObject coords))
                    , Svg.Events.onMouseOut (Signal.message events.address
                        (DeselectObject coords))
                    ]
                    []
                , x 
                , y 
                )
        _     -> Nothing

justList : List (Maybe (Svg.Svg, Int, Int)) -> List (Svg.Svg, Int, Int)
justList l = 
    case l of
        Just v :: vs  -> v :: (justList vs)
        Nothing :: vs -> justList vs
        _             -> []


view : (Int, Int) -> Model -> Html.Html
view (w,h) model = 
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
                    , ("height", String.append (toString <| h - 20) "px")
                    , ("margin", "0")
                    , ("position", "absolute")
                    , ("left", "0px")
                    , ("top", "0px")
                    ]
                ]
                [codeBox model.code]
            , Html.div
                [ Attr.style
                    [ ("width", String.append (toString <| w // 2 - 1) "px")
                    , ("height", String.append (toString h) "px")
                    , ("margin", "0")
                    , ("position", "absolute")
                    , ("left", String.append (toString <| w // 2 - 1) "px")
                    , ("top", "0px")
                    ]
                ]    
                [visualsBox model dim]
            ]

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
