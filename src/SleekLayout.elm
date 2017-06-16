module SleekLayout exposing
  ( outputPanelDimensions
  , clickToCanvasPoint
  )

import InterfaceModel as Model exposing (Model)

-- TODO make constants instead of magic numbers and set CSS programmatically

outputPanelDimensions : Model -> (Int, Int)
outputPanelDimensions model =
  ( (model.dimensions.width - 140) // 2
  , model.dimensions.height - 90
  )

clickToCanvasPoint : Model -> { x : Int, y : Int} -> (Int, Int)
clickToCanvasPoint model {x, y} =
  ( x - 60 - (model.dimensions.width - 140) // 2
  , y - 60
    )
