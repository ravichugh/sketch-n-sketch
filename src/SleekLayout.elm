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
--------------------------------------------------------------------------------

module SleekLayout exposing
  ( px
  , half
  , clickToCanvasPoint
  , spacing
  , menuBar
  , toolPanel
  , codePanelBox
  , outputPanelBox
  )

import InterfaceModel as Model exposing (Model)

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
      outputPanelBox model
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

--------------------------------------------------------------------------------
-- Menu Bar
--------------------------------------------------------------------------------

menuBar =
  { height = 30
  }

--------------------------------------------------------------------------------
-- Tool Panel
--------------------------------------------------------------------------------

toolPanel =
  { width = 40
  , marginLeft = 10
  }

--==============================================================================
--= Computed Information
--==============================================================================

--------------------------------------------------------------------------------
-- Layout Boxes
--------------------------------------------------------------------------------

type alias LayoutBox =
  { x : Int
  , y : Int
  , width : Int
  , height : Int
  }

box : Int -> Int -> Int -> Int -> LayoutBox
box x y width height =
  { x = x
  , y = y
  , width = width
  , height = height
  }

--------------------------------------------------------------------------------
-- Panels
--------------------------------------------------------------------------------

numPanels : Int
numPanels = 2

panelBox : Int -> Model -> LayoutBox
panelBox panelNumber model =
  let
    staticContentWidth =
      numPanels * spacing.width +
        toolPanel.marginLeft + toolPanel.width + spacing.width
    staticContentHeight =
      menuBar.height + 2 * spacing.height
    width =
      (model.dimensions.width - staticContentWidth) // numPanels
    height =
      model.dimensions.height - staticContentHeight
    x =
      spacing.width + (width + spacing.width) * panelNumber
    y =
      menuBar.height + spacing.height
  in
    box x y width height

codePanelBox : Model -> LayoutBox
codePanelBox = panelBox 0

outputPanelBox : Model -> LayoutBox
outputPanelBox = panelBox 1
