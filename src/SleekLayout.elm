--------------------------------------------------------------------------------
-- This module is for all CSS constants that must be computed dynamically. When
-- in doubt, try to keep it as pure CSS (much simpler), but if necessary, add
-- a constant here and dynamically set the CSS in the view. Dynamically adding
-- in the CSS introduces an additional layer of complexity, so it is preferred
-- to keep the CSS static. Most of the time, unless the CSS is dealing with the
-- overall layout of the entire app, it can remain static.
--
-- NOTE: If CSS is added dynamically in the view, please make a note in the
--       main.css file next to the selector that is modified saying which
--       properties are modified.
--
-- NOTE: If an item is marked "descriptive", it describes the CSS rather than
--       modifies it. If you want to modify this property, the styles MUST also
--       be MANUALLY MODIFIED.
--------------------------------------------------------------------------------

module SleekLayout exposing
  ( px
  , half
  , clickToCanvasPoint
  , panelBorderWidth
  , spacing
  , menuBar
  , toolPanel
  , iconButton
  , synthesisPanel
  , codePanel
  , outputPanel
  , outputCanvas
  , codeBox
  )

import InterfaceModel as Model exposing (Model)

--------------------------------------------------------------------------------
-- General Descriptions (descriptive)
--------------------------------------------------------------------------------

panelBorderWidth : Int
panelBorderWidth = 1

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

px : Int -> String
px n =
  (toString n) ++ "px"

half : Int -> Int
half x =
  x // 2

clickToCanvasPoint : Model -> { x : Int, y : Int } -> (Bool, (Int, Int))
clickToCanvasPoint model {x, y} =
  let
    box =
      outputCanvas model
    canvasX =
      x - box.x
    canvasY =
      y - box.y
    isOnCanvas =
      0 <= canvasX && canvasX <= box.width &&
      0 <= canvasY && canvasY <= box.height
  in
    (isOnCanvas, (canvasX, canvasY))

--==============================================================================
--= Parameters
--==============================================================================

--------------------------------------------------------------------------------
-- Spacing
--------------------------------------------------------------------------------

spacing =
  { width = 10
  , height = 10
  }

-------------------------------------------------------------------------------
-- Menu Bar
--------------------------------------------------------------------------------

-- Note: the menu bar does not have "box-sizing: border-box"

menuBar =
  { height = 30
  , borderWidth = 1
  }

menuBarTotalHeight =
  menuBar.height + menuBar.borderWidth

--------------------------------------------------------------------------------
-- Tool Panel
--------------------------------------------------------------------------------

toolPanel =
  { width = 50
  , right = spacing.width
  , marginLeft = spacing.width
  }

iconButton =
  { width =
      toolPanel.width - 2 * panelBorderWidth
  , height =
      toolPanel.width - 2 * panelBorderWidth
  }

--------------------------------------------------------------------------------
-- Synthesis Panel Wrapper
--------------------------------------------------------------------------------

synthesisPanel model =
  { bottom =
      spacing.height
  , height =
      if Model.synthesisResultsNotEmpty model &&
           (not model.viewState.menuActive) then
         300
      else
        0
  }

--------------------------------------------------------------------------------
-- Main Panels
--------------------------------------------------------------------------------

type alias BoundingBox =
  { x : Int
  , y : Int
  , width : Int
  , height : Int
  }

box : Int -> Int -> Int -> Int -> BoundingBox
box x y width height =
  { x = x
  , y = y
  , width = width
  , height = height
  }

numMainPanels : Int
numMainPanels = 2

staticContentWidth : Int
staticContentWidth =
  numMainPanels * spacing.width +
    toolPanel.marginLeft + toolPanel.width + spacing.width

staticContentHeight : Model -> Int
staticContentHeight model =
  menuBarTotalHeight + 2 * spacing.height + (synthesisPanel model).height

mainPanel : Int -> Model -> BoundingBox
mainPanel panelNumber model =
  let
    width =
      (model.dimensions.width - staticContentWidth) // numMainPanels
    height =
      model.dimensions.height - staticContentHeight model
    x =
      spacing.width + (width + spacing.width) * panelNumber
    y =
      menuBarTotalHeight + spacing.height
  in
    box x y width height

codePanel : Model -> BoundingBox
codePanel = mainPanel 0

outputPanel : Model -> BoundingBox
outputPanel = mainPanel 1

--------------------------------------------------------------------------------
-- Output Dimensions (descriptive)
--------------------------------------------------------------------------------

outputCanvas : Model -> BoundingBox
outputCanvas model =
  let
    op =
      outputPanel model
  in
    { x =
        op.x + panelBorderWidth
    , y =
        op.y + panelBorderWidth
    , width =
        op.width - 2 * panelBorderWidth
    , height =
        op.height - 2 * panelBorderWidth
    }

--------------------------------------------------------------------------------
-- Code Box (descriptive)
--------------------------------------------------------------------------------

codeBox =
  { x =
      -- spacing + codePanel.borderBorderWidth
      spacing.width + panelBorderWidth
  , y =
      --   codePanel.y
      -- + codePanel.borderWidth
      -- + actionBar.height
      -- + statusBar.height
      (menuBarTotalHeight + spacing.height) + panelBorderWidth + 35 + 35
  }
