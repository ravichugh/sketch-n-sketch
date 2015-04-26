-- Interface.elm
-- This defines and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the language and its parsing utilities
--import Lang
--import LangParser
--import Sync
--import Utils

import List exposing (..)
import Dict exposing (..)
import String exposing (..)
import Graphics.Element as GE exposing (..)
import Graphics.Collage as GC exposing (..)
import Result exposing (..)
import Signal exposing (..)

import Window exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..) 
import Html.Events as Events exposing (..) 


-- Model --
--code is the text from the codebox
--output: the parsed representation
--of the code (this type will change later)
--objects: a dictionary of the objects
--selected: the possible key of a selected object
type alias Model = { code : String
                    , output : List (List Int) --temporary
                    , objects : Dict Int Object
                    , selected : Maybe Int
                   }

type alias Object = (Html.Html, Int, Int)

initModel = { code = ""
            , output = []
            , objects = Dict.empty
            , selected = Nothing
            }

sampleModel = { code = "[[100,100],[100,200],[100,300]]"
            , output = [[100,100],[100,200],[100,300]] 
            , objects = Dict.fromList
                        <| justList
                        <| List.map (\s -> buildSquare s)
                        <| List.indexedMap (,) 
                            [[100,100],[100,200],[100,300]]
            , selected = Nothing
            }

type Event = CodeUpdate String
           | OutputUpdate String
           | SelectObject Int
           | MoveObject (Int, Int)

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

-- Update --
upstate : Event -> Model -> Model
upstate evt old = case evt of
    CodeUpdate newcode -> { old | code <- newcode }
    SelectObject i -> { old | selected <- Just i }
    _ -> old

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



visualsBox : Model -> Float -> List GC.Form
visualsBox model dim =
    let
        objs = List.map (\s -> snd s)
            <| Dict.toList model.objects
        intdim = floor (dim/20)
        elements = List.map (\j -> 
            case j of
                (h,x,y) -> ((x,y), Html.toElement intdim intdim h)
                _       -> ((0,0), GE.empty)
            ) objs
    in 
        List.map (\f -> GC.move (Basics.toFloat <| fst <| fst f, 
                                Basics.toFloat <| snd <| fst f) 
                                            <| GC.toForm 
                                            <| snd f) 
                                            elements



buildSquare : (Int, List Int) -> Maybe (Int, Object)
buildSquare (i, coords) =
    case coords of
        [x,y] -> 
                Just (i, (Html.div
                    [ Attr.style
                        [ ("bacgroundColor", "lightGreen")
                        , ("height", "60 px")
                        , ("width", "60 px")
                        , ("border", "2 px solid black")
                        ]
                    , Events.onClick events.address (SelectObject i)
                    , Events.onMouseMove events.address 
                        (MoveObject (x, y))
                    ]
                    []
                , x
                , y
                ))
        _     -> Nothing

justList : List (Maybe (Int, Object)) -> List (Int, Object)
justList l = 
    case l of
        Just v :: vs  -> v :: (justList vs)
        Nothing :: vs -> justList vs
        _             -> []


view : (Int, Int) -> Model -> GE.Element
view (w,h) model = 
    let
        dim = (Basics.toFloat (Basics.min w h)) / 2
    in
        GE.flow GE.right [ Html.toElement (w // 2) h
                                        <| codeBox model.code
                                    , GC.collage (w // 2) h
                                        <| visualsBox model dim
                                    ]



-- Main --
main : Signal Element
main = let sigModel = Signal.foldp upstate sampleModel
                        <| Signal.mergeMany
                            [ events.signal
                            ]
       in Signal.map2 view Window.dimensions sigModel
