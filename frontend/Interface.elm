-- Interface.elm
-- This defines and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser exposing (freshen, parseE, parseV)
import Sync exposing (sync)
import Eval exposing (run)
import MainSvg
import Utils

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

type alias Object = (Svg.Svg, List (String, String))

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
        v = run e
    in {e=e, v=v}

sampleCode = 
    "(letrec map (\\(f xs) (case xs ([] []) ([hd|t1] [(f hd)|(map f t1)])))
     (letrec mult (\\(m n) (if (< m 1) 0 (+ n (mult (- m 1) n))))
     (let [x0 y0 sep] [10 8 30]
       (map (\\i [(+ x0 (mult i sep)) y0]) [0 1 2]))))"

sampleVals = sampleFields sampleCode

sampleModel = { code      = sampleCode
              , objects   = buildSvg sampleVals.v 
              , movingObj = Nothing
              , inputVal = sampleVals.v
              , workingVal = sampleVals.v
              , possibleChanges = []
              }

type Event = CodeUpdate String
           | OutputUpdate String
           | SelectObject String
           | DeselectObject String
           | MouseDown (Int, Int)
           | Sync

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

-- Update --
upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of
    CodeUpdate newcode -> { old | code <- newcode }
    MouseDown (mx, my) -> case old.movingObj of
        Nothing                  -> old
        Just (obj, xdist, ydist) -> if
            | xdist == -1.0 || ydist == -1.0 -> case obj of
                (svg, attrs) -> 
                    let xpos = case MainSvg.find "xpos" attrs of
                            Ok a -> case String.toFloat a of
                                Ok b -> b
                        ypos = case MainSvg.find "ypos" attrs of
                            Ok a -> case String.toFloat a of
                                Ok b -> b
                    in { old | movingObj <- Just (obj 
                                               , xpos - Basics.toFloat mx
                                               , ypos - Basics.toFloat my) }
            | otherwise -> 
                let newpos = [ ("xpos", toString <| Basics.toFloat mx + xdist)
                               ("ypos", toString <| Basics.toFloat my + ydist) ]
                    newobjs = List.map (updateObjPos newpos obj) old.objects
                    moved = updateObjPos newpos obj obj
                in  { old | objects <- newobjs 
                          , movingObj <- Just 
                                (moved, xdist, ydist)
                    }
    SelectObject x -> let match = List.filter 
                                (\(s, a) -> case MainSvg.find "xloc" a of 
                                    Ok b -> b == x)
                                old.objects
                      in case match of
                              mat :: xs -> { old | movingObj <- Just (mat, -1.0, -1.0) }
    DeselectObject x -> { old | movingObj <- Nothing }
    Sync -> old --TODO perform appropriate sync actions
    _ -> old

updateObjPos : List (String, String) -> Object -> Object -> Object
updateObjPos newattrs (o1, a1) (o2, a2) = 
    let xloc1 = case MainSvg.find "xloc" a1 of
            Ok a -> a
        xloc2 = case MainSvg.find "xloc" a2 of
            Ok a -> a
    --        Just sq -> sq
    in if | xloc1 == xloc2 ->
                let updatedattrs = updateAttrs newattrs a1
                    svgattrs = List.map MainSvg.attr updatedattrs
                in (Svg.rect svgattrs [], updatedattrs) --TODO keep track of
                                                         --the kind of shape it is
          | otherwise -> (o2, a2)

updateAttrs : List (String, String) -> List (String, String) -> 
                List (String, String)
updateAttrs newattrs oldattrs = case newattrs of
    [] -> oldattrs
    (a1, v1) :: xs -> updateAttrs xs (replace (a1,v1) oldattrs)

replace : (String, String) -> List (String, String) -> List (String, String)
replace (a1, v1) attrs = case attrs of
    [] -> [(a1,v1)]
    (a2, v2) :: xs -> if | a1 == a2 -> (a1, v1) :: xs
                         | otherwise -> (a2, v2) :: replace (a1, v1) xs

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

buildSvg : Val -> List (Svg.Svg, List (String, String))
buildSvg v = case v of
   VList vs -> flip List.map vs <| \v1 -> case v1 of
       VList (VBase (String shape) :: vs') ->
           let  firstattrs = getFirstAttrs vs'
                baseattrs = fst <| cleanAttrs (snd firstattrs) ([],[])
                attrloc = snd <| cleanAttrs (snd firstattrs) ([],[])
                xloc = case MainSvg.find attrloc "x" of
                    Ok s -> s
                attrs = List.append (fst firstattrs)
                    [ Svg.Events.onMouseDown (Signal.message events.address
                        (SelectObject xloc)) --xloc should be unique ID
                    , Svg.Events.onMouseUp (Signal.message events.address
                        (DeselectObject xloc))
                    , Svg.Events.onMouseOut (Signal.message events.address
                        (DeselectObject xloc))
                    ]
           in ((MainSvg.svg shape) attrs [], List.map (\(a,b) -> b) baseattrs)
                
getFirstAttrs : List Val -> List (Svg.Attribute, (String, String))
getFirstAttrs vals = List.map 
    (\x -> case x of
        VList [VBase (String a), VConst i pos] -> ((MainSvg.attr a) toString i
              , (a, String.concat [toString i, "|", toString pos]))
        VList [VBase (String a), VBase (String s)] -> ((MainSvg.attr a) s
              , (a,s))
        VList [VBase (String "points"), VList pts] ->
            let s = Utils.spaces <| List.map
                    (\y -> case y of
                        VList [VConst x1 _, VConst y1 _] ->
                            toString x1 ++ "," ++ toString y1)
                    pts
            in ((MainSvg.attr "points") s, ("points", s)))
    vals

--Takes a list of attributes and pulls out the location
-- information for the constants into a separate list
cleanAttrs : List (String, String) -> ( List (String, String)
                                      , List (String, Int))
                                   -> ( List (String, String)
                                      , List (String, String))
cleanAttrs = \l (acc1, acc2) -> case l of
    (key, val) :: xs -> case String.split "|" val of
        [v1, loc] -> cleanAttrs ((key, v1) :: acc1
                                , (key, loc) :: acc2)
                                xs
        _         -> cleanAttrs ((key, val) :: acc1
                                , acc2)
                                xs
    []            -> (acc1, acc2)
    
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
                    , ("height", String.append (toString <| h - 60) "px")
                    , ("margin", "0")
                    , ("position", "absolute")
                    , ("left", "0px")
                    , ("top", "0px")
                    ]
                ]
                [codeBox model.code]
            , Html.button
                [ Attr.style
                    [ ("name", "Sync the code and visual output")
                    , ("value", "Sync")
                    , ("type", "button")
                    ]
                , Events.onSubmit events.address Sync
                ]
                []
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
