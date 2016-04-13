module Config where

import Debug

--------------------------------------------------------------------------------

debugParser = False
debugController = False
debugSync = False
debugStorage = False

debugLog b s x =
  if b
    then Debug.log s x
    else x

--------------------------------------------------------------------------------
-- User Interface Layout

params =
  { strVersion = "v0.4.2"
  , debugLayout = False    -- displays colors for high-level layout structure
  , wGut = 10              -- width of left/right side gutters (spans entire height)
  , topSection =
     { h = 38              -- height of top space
     , wLogo = 25          -- width/height of logo
     , wBtnO = 210         -- width...
     , hBtnO = 25          -- ... and height of orientation button
     , wJunk = 250         -- gap between title and orientation button
     }
  , botSection =
     { h = 15              -- height of bot space
     }
  , mainSection =
     { widgets =           -- Render/Sync buttons; Mode/Tests dropdowns
        { wBtn = 130
        , wBtnWide = 150
        , hBtn = 25
        , font = "Helvetica, sans-serif"
        , fontSize = "10pt"
        }
     , vertical =
        { hExtra = 15      -- extra vertical space around widgets
        , wGut = 10        -- width of gutters in between code/widgets/canvas
        }
     , horizontal =
        { wExtra = 15      -- extra horizontal space around widgets
        , hGut = 10        -- height of gutters in between code/widgets/canvas
        }
     , canvas =
        { border = "0px solid darkGray"
        , hZoneInfo = 40   -- height of area to display hover zone info
        }
     , codebox =
        { border = "none"
        , font = "Courier, monospace"
        , fontSize = "12pt"
        }
     , uiWidgets =
        { pad = 5
        , wSlider = 100
        , hSlider = 25
        , wCaption = 100
        , rBall = "10px"
        , fontSize = "11pt"
        , font = "Tahoma, sans-serif"
        }
     }
  }

