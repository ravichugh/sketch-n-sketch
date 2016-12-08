module HtmlUtils exposing (..)

import Svg
import Svg.Attributes
import Html.Events exposing (onWithOptions, defaultOptions)
import Json.Decode

handleEventAndStop : String -> msg -> Svg.Attribute msg
handleEventAndStop eventName eventHandler =
  onWithOptions eventName { defaultOptions | stopPropagation = True}
    (Json.Decode.map (always eventHandler) Json.Decode.value)
