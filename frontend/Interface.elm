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
tempTestCode = 
    "(let [x0 y0 xsep ysep] [10 28 30 30]
       (map (\\[i j] (square_ (+ x0 (mult i xsep)) (+ y0 (mult j ysep)) 20))
            (cartProd [0 1 2] [0 1])))"

tempTest = MicroTests.test27 ()

sampleModel = { code      = tempTestCode
              , objects   = buildSvg tempTest.v 
              , movingObj = Nothing
              , inputVal = tempTest.v
              , workingVal = tempTest.v
              , possibleChanges = []
              }

type Event = CodeUpdate String
           | OutputUpdate String
           | SelectObject String
           | DeselectObject String
           | MouseDown (Int, Int)
           | Sync
           | Render

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

-- Update --
upstate : Event -> Model -> Model
upstate evt old = case Debug.log "Event" evt of
    Render -> { old | objects <- (buildSvg << Eval.run << parseE)
                                  old.code
                    , inputVal <- (Eval.run << parseE)
                                  old.code
                    , workingVal <- (Eval.run << parseE)
                                    old.code
              }
    CodeUpdate newcode -> { old | code <- newcode }
    MouseDown (mx, my) -> case old.movingObj of
        Nothing                  -> old
        Just (obj, xdist, ydist) -> if
            | xdist == -1.0 || ydist == -1.0 -> case obj of
                (svg, attrs) -> 
                    let xpos = case String.toFloat <| find attrs "xpos" of
                            Ok a -> a
                        ypos = case String.toFloat <| find attrs "ypos" of
                            Ok a -> a
                    in { old | movingObj <- Just (obj 
                                               , xpos - Basics.toFloat mx
                                               , ypos - Basics.toFloat my) }
            | otherwise -> 
                let newpos = [ ("xpos", toString <| Basics.toFloat mx + xdist)
                             , ("ypos", toString <| Basics.toFloat my + ydist) ]
                    newobjs = List.map (updateObjPos newpos obj) old.objects
                    moved = updateObjPos newpos obj obj
                in  { old | objects <- newobjs 
                          , movingObj <- Just 
                                (moved, xdist, ydist)
                    }
    SelectObject x -> let match = List.filter 
                                (\(s, a) -> x == (find a "xloc"))
                                old.objects
                      in case match of
                              mat :: xs -> { old | movingObj <- Just (mat, -1.0, -1.0) }
    DeselectObject x -> { old | movingObj <- Nothing }
    Sync -> old --TODO perform appropriate sync actions
    _ -> old

--- Borrowed from LangSvg.elm ---
funcsSvg = [
    ("circle", Svg.circle)
  , ("line", Svg.line)
  , ("polygon", Svg.polygon)
  , ("rect", Svg.rect)
  ]

funcsAttr = [
    ("cx", Svg.Attributes.cx)
  , ("cy", Svg.Attributes.cy)
  , ("fill", Svg.Attributes.fill)
  , ("height", Svg.Attributes.height)
  , ("points", Svg.Attributes.points)
  , ("r", Svg.Attributes.r)
  , ("stroke", Svg.Attributes.stroke)
  , ("strokeWidth", Svg.Attributes.strokeWidth)
  , ("width", Svg.Attributes.width)
  , ("x", Svg.Attributes.x)
  , ("x1", Svg.Attributes.x1)
  , ("x2", Svg.Attributes.x2)
  , ("y", Svg.Attributes.y)
  , ("y1", Svg.Attributes.y1)
  , ("y2", Svg.Attributes.y2)
  ]

find d s =
  case Utils.maybeFind s d of
    Just f  -> f
    Nothing -> Debug.crash <| "find: " ++ s

attr = find funcsAttr
svg  = find funcsSvg
--- ---

updateObjPos : List (String, String) -> Object -> Object -> Object
updateObjPos newattrs (o1, a1) (o2, a2) = 
    let xloc1 = find a1 "xloc"
        xloc2 = find a2 "xloc"
    in if | xloc1 == xloc2 ->
                let updatedattrs = updateAttrs newattrs a1
                    svgattrs = List.map (\(x,y) -> attr x <| y) updatedattrs
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
                ] <| List.map (\(f,g) -> f) model.objects 

buildSvg : Val -> List (Svg.Svg, List (String, String))
buildSvg v = case Debug.log "v" v of
   VList vs -> flip List.map vs <| \v1 -> case v1 of
       VList (VBase (String shape) :: vs') ->
           let  firstattrs = getFirstAttrs vs'
                baseattrs = fst <| cleanAttrs (List.map snd firstattrs) ([],[])
                attrloc = snd <| cleanAttrs (List.map snd firstattrs) ([],[])
--                xloc = find attrloc "xloc"
                attrs = List.map fst firstattrs
--                attrs = List.append (List.map fst firstattrs)
--                    [ Svg.Events.onMouseDown (Signal.message events.address
--                        (SelectObject xloc)) --xloc should be unique ID
--                    , Svg.Events.onMouseUp (Signal.message events.address
--                        (DeselectObject xloc))
--                    , Svg.Events.onMouseOut (Signal.message events.address
--                        (DeselectObject xloc))
--                    ]
           in ((svg shape) attrs [], List.append attrloc baseattrs)
                
getFirstAttrs : List Val -> List (Svg.Attribute, (String, String))
getFirstAttrs vals = List.map 
    (\x -> case x of
        VList [VBase (String a), VConst i pos] -> ((attr a) <| toString i
              , (a, String.concat [toString i, "|", toString pos]))
        VList [VBase (String a), VBase (String s)] -> ((attr a) s
              , (a,s))
        VList [VBase (String "points"), VList pts] ->
            let s = Utils.spaces <| List.map
                    (\y -> case y of
                        VList [VConst x1 _, VConst y1 _] ->
                            toString x1 ++ "," ++ toString y1)
                    pts
            in ((attr "points") s, ("points", s)))
    vals

--Takes a list of attributes and pulls out the location
-- information for the constants into a separate list
cleanAttrs : List (String, String) -> ( List (String, String)
                                      , List (String, String))
                                   -> ( List (String, String)
                                      , List (String, String))
cleanAttrs = \l (acc1, acc2) -> case l of
    (key, val) :: xs -> case String.split "|" val of
        [v1, loc] -> cleanAttrs xs
                                ((key, v1) :: acc1
                                , (String.append key "loc", loc) :: acc2)
        _         -> cleanAttrs xs
                                ((key, val) :: acc1, acc2)
    []        -> (acc1, acc2)
    
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
                    [ ("position", "absolute")
                    , ("left", String.append (toString <| w // 4) "px")
                    , ("top", String.append (toString <| h - 30) "px")
                    , ("type", "button")
                    , ("width", "50px")
                    , ("height", "20px")
                    ]
                , Events.onClick events.address Render
                , Attr.value "Render"
                , Attr.name "Render the Code"
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
