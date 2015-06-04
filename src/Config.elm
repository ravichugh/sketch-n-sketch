module Config where

--------------------------------------------------------------------------------
-- User Interface Layout

params =
  { strVersion = "v0.0"
  , debugLayout = False    -- displays colors for high-level layout structure
  , wGut = 10              -- width of left/right side gutters (spans entire height)
  , topSection =
     { h = 40              -- height of top space
     , wBtnO = 180         -- width...
     , hBtnO = 25          -- ... and height of orientation button
     , wJunk = 225         -- gap between title and orientation button
     }
  , botSection =
     { h = 30              -- height of bot space
     }
  , mainSection =
     { widgets =           -- Render/Sync buttons; Mode/Tests dropdowns
        { wBtn = 100
        , hBtn = 25
        , font = "Tahoma, sans-serif"
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
        }
     , codebox =
        { border = "none"
        , font = "Courier, monospace"
        , fontSize = "12pt"
        }
     }
  }

