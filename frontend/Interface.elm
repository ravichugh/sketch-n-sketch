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

-- Model --
type alias Model = { code : String
                   }

initModel = { code = ""
            }

type Event = CodeUpdate String
           | OutputUpdate String

-- Update --
upstate : Event -> Model -> Model
upstate evt old = initModel

-- View --
view : (Int, Int) -> Model -> GE.Element
view (w,h) model = GE.spacer w h

-- Main --
main : Signal Element
main = Singal.constant <| GE.spacer 10 10
