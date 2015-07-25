module Config where

--------------------------------------------------------------------------------

debugParser = False

--------------------------------------------------------------------------------
-- User Interface Layout

params =
  { strVersion = "v0.0"
  , debugLayout = False    -- displays colors for high-level layout structure
  , wGut = 10              -- width of left/right side gutters (spans entire height)
  , topSection =
     { h = 38              -- height of top space
     , wLogo = 25          -- width/height of logo
     , wBtnO = 180         -- width...
     , hBtnO = 25          -- ... and height of orientation button
     , wJunk = 230         -- gap between title and orientation button
     }
  , botSection =
     { h = 15              -- height of bot space
     }
  , mainSection =
     { widgets =           -- Render/Sync buttons; Mode/Tests dropdowns
        { wBtn = 120
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
        , hZoneInfo = 40   -- height of area to display hover zone info
        }
     , codebox =
        { border = "none"
        , font = "Courier, monospace"
        , fontSize = "12pt"
        }
     }
  }

