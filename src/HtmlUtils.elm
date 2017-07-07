module HtmlUtils exposing (..)

import Svg
import Svg.Attributes
import Html.Events exposing (onWithOptions, defaultOptions)
import Json.Decode

handleEventAndStop : String -> msg -> Svg.Attribute msg
handleEventAndStop eventName eventHandler =
  onWithOptions eventName { defaultOptions | stopPropagation = True}
    (Json.Decode.map (always eventHandler) Json.Decode.value)

styleListToString : List (String, String) -> String
styleListToString =
  let
    stylePairToString (attr, val) =
      attr ++ ": " ++ val ++ "; "
  in
    String.concat << List.map stylePairToString
