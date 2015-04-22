-- Interface.elm
-- This defines and renders and renders an interactive interface for editing the
-- program and output of the language as defined in int-trees.

--Import the language and its parsing utilities
import Lang
import LangParser
import Sync
import Utils

import String
import Graphics.Element as GE 
import Signal

import Window
import Html
import Html.Attributes
import Html.Events


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
    UpdateCode newcode -> { old | code <- newcode }
    _ -> old

-- View --
codeBox : String -> Html.Html
codeBox codeText =
    Html.textarea
        [ Html.Attributes.id "codeBox"
        , Html.Attributes.value codeText
        , Html.Events.on "input" Html.Events.targetValue
            (Signal.message events << CodeUpdate)
        ]
        []

view : (Int, Int) -> Model -> GE.Element
view (w,h) model = GE.flow GE.right [ Html.toElement (w // 2) h
                                        <| codeBox model.code
                                    ]

-- Main --
main : Signal Element
main = let sigModel = Signal.foldp upstate initState
                        <| Signal.mergeMany
                            [ events.signal
                            ]
       in Signal.map2 view Window.dimensions sigModel
