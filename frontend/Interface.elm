-- Interface.elm
-- This defines and renders and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

module Main where

--Import the language and its parsing utilities
--import Lang
--import LangParser
--import Sync
--import Utils

import String exposing (..)
import Graphics.Element as GE exposing (..)
import Signal exposing (..)

import Window exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model --
type alias Model = { code : String
                   }

initModel = { code = ""
            }

type Event = CodeUpdate String
           | OutputUpdate String

events : Signal.Mailbox Event
events = Signal.mailbox <| CodeUpdate ""

-- Update --
upstate : Event -> Model -> Model
upstate evt old = case evt of
    CodeUpdate newcode -> { old | code <- newcode }
    _ -> old

-- View --
codeBox : String -> Html.Html
codeBox codeText =
    Html.textarea
        [ Html.Attributes.id "codeBox"
        , Html.Attributes.style
            [ ("height", "100%")
            , ("width",  "50%")
            , ("resize", "none")
            , ("overflow", "scroll")
            ]
        , Html.Attributes.value codeText
        , Html.Events.on "input" Html.Events.targetValue
            (Signal.message events.address << CodeUpdate)
        ]
        []

view : (Int, Int) -> Model -> GE.Element
view (w,h) model = GE.flow GE.right [ Html.toElement (w // 2) h
                                        <| codeBox model.code
                                    ]

-- Main --
main : Signal Element
main = let sigModel = Signal.foldp upstate initModel
                        <| Signal.mergeMany
                            [ events.signal
                            ]
       in Signal.map2 view Window.dimensions sigModel
