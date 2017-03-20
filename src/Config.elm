module Config exposing (..)

import Debug

--------------------------------------------------------------------------------

debugParser = False
debugView = False
debugController = False
debugSync = False
debugStorage = False
debugTypeChecker = True

debugLog b s x =
  if b
    then Debug.log s x
    else x

--------------------------------------------------------------------------------
-- User Interface Layout

params =
  { strVersion = "v0.6.0"
  , debugLayout = False    -- displays colors for high-level layout structure
  , wGut = 10              -- width of left/right side gutters (spans entire height)
  , topSection =
     { h = 38              -- height of top space
     , wLogo = 25          -- width/height of logo
     , wBtnO = 180         -- width...
     , hBtnO = 25          -- ... and height of orientation button
     , wJunk = 250         -- gap between title and orientation button
     }
  , botSection =
     -- { h = 15              -- height of bot space
     { h = 60              -- height of bot space
     }
  , mainSection =
     { widgets =           -- Render/Sync buttons; Mode/Tests dropdowns
        { wBtn = 120
        , wBtnWide = 140
        , hBtn = 25
        , font = "Helvetica, sans-serif"
        , fontSize = "10pt"
        }
     , vertical =
        -- { hExtra = 15      -- extra vertical space around widgets
        { hExtra = 6      -- extra vertical space around widgets
        , wGut = 10        -- width of gutters in between code/widgets/canvas
        }
     , horizontal =
        { wExtra = 15      -- extra horizontal space around widgets
        , hGut = 10        -- height of gutters in between code/widgets/canvas
        }
     , canvas =
        -- { border = "0px solid darkGray"
        { border = "2px solid darkGray"
        , hZoneInfo = 40   -- height of area to display hover zone info
        }
     , codebox =
        -- { border = "none"
        { border = "2px solid darkGray"
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

computeLayoutInfo model =
  let
    wSideGutter = params.wGut
    wWindow     = model.dimensions.width
    hWindow     = model.dimensions.height
    hTop        = params.topSection.h
    hBot        = params.botSection.h
    wCodebox    = wWindow // 2 - 10           -- TODO
    hCodebox    = hWindow - (hTop + hBot) - 3 -- TODO
    wCanvas     = wCodebox
    hCanvas     = hCodebox
  in
  { hTop        = hTop
  , hBot        = hBot
  , hMid        = hCodebox
  , wCodebox    = wCodebox
  , hCodebox    = hCodebox
  , wCanvas     = wCanvas
  , hCanvas     = hCanvas
  , xCanvas     = wCodebox
  , yCanvas     = hCodebox
  }
