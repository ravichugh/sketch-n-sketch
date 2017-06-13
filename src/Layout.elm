module Layout exposing (..)

import InterfaceModel as Model exposing (Model)
import Either exposing (..)


--------------------------------------------------------------------------------
-- Configuration Parameters

windowPadding = 10
buttonHeight = 25
iconButtonHeight = 40
rowGap = 3
initialCodeBoxWidth = 815  -- 500
initialCanvasGapFromTop = 50
extraBotPadding = 40
extraRightPadding = 15
middlePadding = 20

strInterfaceColor = "rgba(52,73,94,1.0)"
strButtonTopColor = "rgba(231,76,60,1.0)" -- from InterfaceButtons example


--------------------------------------------------------------------------------

pixels n = toString n ++ "px"

-- Returns (isOnCanvas, (x, y))
-- Lets you handle off canvas clicks in their own manner.
clickToCanvasPoint : Model -> { x : Int, y : Int } -> (Bool, (Int, Int))
clickToCanvasPoint model {x,y} =
  let layout = computeLayout model in
  let (xOrigin, yOrigin) = (layout.canvas.left, layout.canvas.top) in
  let (xOnCanvas, yOnCanvas) = (x - xOrigin, y - yOrigin) in
  let isOnCanvas =
    xOnCanvas >= 0 && xOnCanvas < layout.canvas.width &&
    yOnCanvas >= 0 && yOnCanvas < layout.canvas.height
  in
  (isOnCanvas, (xOnCanvas, yOnCanvas))


--------------------------------------------------------------------------------
-- CSS Fixed Positions

type alias LeftRight = Either Int Int

type TopBottom = Top Int | Bottom Int

type alias FixedPosition = { leftRight: LeftRight, topBottom: TopBottom }

styleLeftRight x =
  case x of
    Left n  -> ("left",  pixels n)
    Right n -> ("right", pixels n)

styleTopBottom x =
  case x of
    Top n    -> ("top",    pixels n)
    Bottom n -> ("bottom", pixels n)

fixedPosition record =
  [styleLeftRight record.leftRight, styleTopBottom record.topBottom]

offsetLeftRight leftRight d =
  case leftRight of
    Left n  -> Left  (n + d)
    Right n -> Right (n - d)

offsetTopBottom topBottom d =
  case topBottom of
    Top n    -> Top    (n + d)
    Bottom n -> Bottom (n - d)

offset model get record =
  let {dx,dy} = get model in
  { leftRight = offsetLeftRight record.leftRight dx
  , topBottom = offsetTopBottom record.topBottom dy
  }


--------------------------------------------------------------------------------
-- "Lenses" for getting/putting Model.LayoutOffets

getPutFileToolBox      = (getFileToolBox,      putFileToolBox)
getPutCodeToolBox      = (getCodeToolBox,      putCodeToolBox)
getPutDrawToolBox      = (getDrawToolBox,      putDrawToolBox)
getPutAttributeToolBox = (getAttributeToolBox, putAttributeToolBox)
getPutBlobToolBox      = (getBlobToolBox,      putBlobToolBox)
getPutMoreBlobToolBox  = (getMoreBlobToolBox,  putMoreBlobToolBox)
getPutOutputToolBox    = (getOutputToolBox,    putOutputToolBox)
getPutAnimationToolBox = (getAnimationToolBox, putAnimationToolBox)
getPutSynthesisResultsSelectBox = (getSynthesisResultsSelectBox, putSynthesisResultsSelectBox)
getPutCodeBox          = (getCodeBox,          putCodeBox)
getPutCanvas           = (getCanvas,           putCanvas)
getPutTextToolBox      = (getTextToolBox,      putTextToolBox)
getPutDeuceToolBox     = (getDeuceToolBox,     putDeuceToolBox)

getFileToolBox      = .layoutOffsets >> .fileToolBox
getCodeToolBox      = .layoutOffsets >> .codeToolBox
getDrawToolBox      = .layoutOffsets >> .drawToolBox
getAttributeToolBox = .layoutOffsets >> .attributeToolBox
getBlobToolBox      = .layoutOffsets >> .blobToolBox
getMoreBlobToolBox  = .layoutOffsets >> .moreBlobToolBox
getOutputToolBox    = .layoutOffsets >> .outputToolBox
getAnimationToolBox = .layoutOffsets >> .animationToolBox
getSynthesisResultsSelectBox = .layoutOffsets >> .synthesisResultsSelectBox
getCodeBox          = .layoutOffsets >> .codeBox
getCanvas           = .layoutOffsets >> .canvas
getTextToolBox      = .layoutOffsets >> .textToolBox
getDeuceToolBox     = .layoutOffsets >> .deuceToolBox >> .offsets

putFileToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | fileToolBox = { dx = dx, dy = dy } } }

putCodeToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | codeToolBox = { dx = dx, dy = dy } } }

putDrawToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | drawToolBox = { dx = dx, dy = dy } } }

putAttributeToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | attributeToolBox = { dx = dx, dy = dy } } }

putBlobToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | blobToolBox = { dx = dx, dy = dy } } }

putMoreBlobToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | moreBlobToolBox = { dx = dx, dy = dy } } }

putOutputToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | outputToolBox = { dx = dx, dy = dy } } }

putAnimationToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | animationToolBox = { dx = dx, dy = dy } } }

putSynthesisResultsSelectBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | synthesisResultsSelectBox = { dx = dx, dy = dy } } }

putCodeBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | codeBox = { dx = dx, dy = dy } } }

putCanvas dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | canvas = { dx = dx, dy = dy } } }

putTextToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  { model | layoutOffsets =
    { layoutOffsets | textToolBox = { dx = dx, dy = dy } } }

putDeuceToolBox dx dy model =
  let layoutOffsets = model.layoutOffsets in
  let deuceToolBox = layoutOffsets.deuceToolBox in
  { model | layoutOffsets =
    { layoutOffsets | deuceToolBox =
      { deuceToolBox | offsets = { dx = dx, dy = dy } } } }


--------------------------------------------------------------------------------
-- Handler for Resize Widgets

dragLayoutWidgetTrigger {dx,dy} put =
  Model.Msg "Drag Layout Widget Trigger" <| \m ->
    let {x,y} = Tuple.second m.mouseState in
    let f (x2,y2) = put (dx + x2 - x) (dy + y2 - y) in
    { m | mouseMode = Model.MouseDragLayoutWidget f }


--------------------------------------------------------------------------------
-- UI Layout Info

type alias Info =
  { codeBox :
     { width : Int
     , height : Int
     , left : Int
     , top : Int
     }
  , canvas :
     { width : Int
     , height : Int
     , left : Int
     , top : Int
     , initialLeft : Int
     }
  , fileTools : FixedPosition
  , codeTools : FixedPosition
  , drawTools : FixedPosition
  , stretchyDrawTools : FixedPosition
  , lambdaDrawTools : FixedPosition
  , attributeTools : FixedPosition
  , blobTools : FixedPosition
  , moreBlobTools : FixedPosition
  , outputTools : FixedPosition
  , animationTools : FixedPosition
  , synthesisResultsSelect : FixedPosition
  , captionArea : FixedPosition
  , textTools : FixedPosition
  , deuceToolsPinnedAnchor : FixedPosition
  }

computeLayout : Model -> Info
computeLayout m =
  let dimCodeBox =
    let offsetDownFromCodeTools = 2 * buttonHeight + 18 in
    let initialHeight =
      m.dimensions.height - 2 * windowPadding - offsetDownFromCodeTools
    in
    let width = initialCodeBoxWidth + m.layoutOffsets.codeBox.dx in
    let height = initialHeight + m.layoutOffsets.codeBox.dy - extraBotPadding in
    { width = width
    , height = height
    , left = windowPadding
    , top = windowPadding + offsetDownFromCodeTools
    }
  in
  let dimCanvas =
    let initialX = windowPadding + initialCodeBoxWidth + middlePadding in
    let initialY = windowPadding + initialCanvasGapFromTop in
    let x = initialX + m.layoutOffsets.canvas.dx in
    let y = initialY + m.layoutOffsets.canvas.dy in
    { left = x
    , top = y
    , initialLeft = initialX
    , width = m.dimensions.width - windowPadding - x - extraRightPadding
    , height = m.dimensions.height - windowPadding - y - extraBotPadding
    }
  in
  { codeBox = dimCodeBox
  , canvas = dimCanvas
  , fileTools = offset m getFileToolBox
     { leftRight = Left   <| dimCodeBox.left
     , topBottom = Top    <| windowPadding + 0 * (rowGap + buttonHeight)
     }
  , codeTools = offset m getCodeToolBox
     { leftRight = Left   <| dimCodeBox.left
     , topBottom = Top    <| windowPadding + round (1 * (rowGap + buttonHeight))
     }
  , drawTools = offset m getDrawToolBox
     { leftRight = Right  <| windowPadding
     , topBottom = Top    <| windowPadding + round (0.0 * (rowGap + iconButtonHeight))
     }
  , stretchyDrawTools = offset m getDrawToolBox -- reusing drawTools offset
     { leftRight = Right  <| windowPadding
     , topBottom = Top    <| windowPadding + round (1.0 * (rowGap + iconButtonHeight))
     }
  , lambdaDrawTools = offset m getDrawToolBox -- reusing drawTools offset
     { leftRight = Right  <| windowPadding
     , topBottom = Top    <| windowPadding + round (2.0 * (rowGap + iconButtonHeight))
     }
  , attributeTools = offset m getAttributeToolBox
     { leftRight = Right  <| windowPadding
     , topBottom = Top    <| windowPadding + round (3.0 * (rowGap + iconButtonHeight) + 0.0 * (rowGap + buttonHeight))
     }
  , blobTools = offset m getBlobToolBox
     { leftRight = Right  <| windowPadding
     , topBottom = Top    <| windowPadding + round (3.0 * (rowGap + iconButtonHeight) + 1.0 * (rowGap + buttonHeight))
     }
  , moreBlobTools = offset m getMoreBlobToolBox
     { leftRight = Right  <| windowPadding
     , topBottom = Top    <| windowPadding + round (3.0 * (rowGap + iconButtonHeight) + 2.0 * (rowGap + buttonHeight))
     }
  , outputTools = offset m getOutputToolBox
     { leftRight = Right  <| windowPadding
     , topBottom = Bottom <| windowPadding + 0 * (rowGap + buttonHeight)
     }
  , animationTools = offset m getAnimationToolBox
     { leftRight = Right  <| windowPadding
     , topBottom = Bottom <| windowPadding + 1 * (rowGap + buttonHeight)
     }
  , synthesisResultsSelect = offset m getSynthesisResultsSelectBox
     { leftRight = Right  <| windowPadding
     , topBottom = Top    <| windowPadding + round (6.3 * (rowGap + buttonHeight) + 2.3 * (rowGap + iconButtonHeight))
     }
  , captionArea =
     { leftRight = Left   <| windowPadding
     , topBottom = Bottom <| windowPadding
     }
  , textTools = offset m getTextToolBox
     { leftRight = Left   <| dimCodeBox.left
     , topBottom = Bottom <| windowPadding + (0 * (rowGap + buttonHeight))
     }
  , deuceToolsPinnedAnchor =
     { leftRight = Left   <| dimCodeBox.left + 250 -- to separate codeTools and deuceTools
     , topBottom = Top    <| windowPadding + round (1 * (rowGap + buttonHeight))
     }
  }
