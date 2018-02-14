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
  ( BoundingBox
  , px
  , half
  , canvasPosition
  , clickToCanvasPoint
  , panelBorderWidth
  , spacing
  , deuceOverlayBleed
  , menuBar
  , toolPanel
  , iconButton
  , deucePopupPanelMouseOffset
  , deuceRightClickMenuMouseOffset
  , synthesisPanel
  , mainResizer
  , mainResizerLeftBound
  , mainResizerRightBound
  , codePanel
  , outputPanel
  , outputCanvas
  )

import InterfaceModel as Model exposing (Model)

--------------------------------------------------------------------------------
-- Bounding Box
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

--------------------------------------------------------------------------------
-- General Descriptions (descriptive)
--------------------------------------------------------------------------------

panelBorderWidth : Int
panelBorderWidth = 1

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

px : number -> String
px n =
  (toString n) ++ "px"

half : Int -> Int
half x =
  x // 2

canvasPosition : Model -> { x : Int, y : Int } -> { x : Int, y : Int }
canvasPosition model {x, y} =
  let box = outputCanvas model in
  { x = x - box.x
  , y = y - box.y
  }

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
-- DeuceOverlayBleed
--------------------------------------------------------------------------------

deuceOverlayBleed : Float
deuceOverlayBleed =
  10

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

iconButton =
  { width  = 49
  , height = 49
  }

toolPanel =
  { width = 2 * iconButton.width
  , right = spacing.width
  , marginLeft = spacing.width
  }

--------------------------------------------------------------------------------
-- Deuce Popup Panel Mouse Offset
--------------------------------------------------------------------------------

deucePopupPanelMouseOffset =
  { x = -16
  , y = -16
  }

--------------------------------------------------------------------------------
-- Deuce Right Click Menu Mouse Offset
--------------------------------------------------------------------------------

deuceRightClickMenuMouseOffset =
  { x = 16
  , y = 16
  }

--------------------------------------------------------------------------------
-- Synthesis Panel Wrapper
--------------------------------------------------------------------------------

synthesisPanel model =
  { bottom =
      spacing.height
  , height =
      if Model.synthesisResultsNotEmpty model "Auto-Synthesis" &&
           (not model.viewState.menuActive) then
         250
      else
        0
  }

--------------------------------------------------------------------------------
-- Main Panels
--------------------------------------------------------------------------------

staticContentWidth : Int
staticContentWidth =
  2 * spacing.width + toolPanel.width + toolPanel.marginLeft

staticContentHeight : Model -> Int
staticContentHeight model =
  menuBarTotalHeight + 2 * spacing.height + (synthesisPanel model).height

dynamicContentWidth : Model -> Int
dynamicContentWidth model =
  model.dimensions.width - staticContentWidth

dynamicContentHeight : Model -> Int
dynamicContentHeight model =
  model.dimensions.height - staticContentHeight model

mainPanelY : Int
mainPanelY =
  menuBarTotalHeight + spacing.height

codePanel : Model -> BoundingBox
codePanel model =
  let
    mainResizerBB =
      mainResizer model
    x =
      spacing.width
    y =
      mainPanelY
    width =
      mainResizerBB.x - x
    height =
      dynamicContentHeight model
  in
    box x y width height

outputPanel : Model -> BoundingBox
outputPanel model =
  let
    mainResizerBB =
      mainResizer model
    x =
      mainResizerBB.x + mainResizerBB.width
    y =
      mainPanelY
    width =
      model.dimensions.width
        - toolPanel.right
        - toolPanel.width
        - toolPanel.marginLeft
        - x
    height =
      dynamicContentHeight model
  in
    box x y width height

--------------------------------------------------------------------------------
-- Main Resizer
--------------------------------------------------------------------------------

mainResizerWidth : Int
mainResizerWidth =
  20

-- Position without moving the resizer
defaultMainResizerX : Model -> Int
defaultMainResizerX model =
  let
    ratio =
      0.6
    spaceToDivide =
      toFloat <|
        dynamicContentWidth model - mainResizerWidth
    left =
      spacing.width
  in
    left + round (ratio * spaceToDivide)

mainResizer : Model -> BoundingBox
mainResizer model =
  let
    width =
      mainResizerWidth
    height =
      dynamicContentHeight model
    x =
      Maybe.withDefault
        (defaultMainResizerX model)
        model.mainResizerX
    y =
      mainPanelY
  in
    box x y width height

mainResizerBoundMargin : Int
mainResizerBoundMargin =
  100

mainResizerLeftBound : Model -> Int
mainResizerLeftBound model =
  let
    codePanelBB =
      codePanel model
  in
    codePanelBB.x + mainResizerBoundMargin

mainResizerRightBound : Model -> Int
mainResizerRightBound model =
  let
    outputPanelBB =
      outputPanel model
  in
    outputPanelBB.x + outputPanelBB.width - mainResizerBoundMargin

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
