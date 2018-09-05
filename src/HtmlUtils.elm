module HtmlUtils exposing (..)

import Svg
import Svg.Attributes
import Html
import Html.Events as E exposing (defaultOptions)
import Json.Decode

handleEventAndStop : String -> msg -> Svg.Attribute msg
handleEventAndStop eventName eventHandler =
  E.onWithOptions eventName { defaultOptions | stopPropagation = True}
    (Json.Decode.map (always eventHandler) Json.Decode.value)

onClickWithoutPropagation : msg -> Html.Attribute msg
onClickWithoutPropagation handler =
  E.onWithOptions
    "click"
    { stopPropagation = True
    , preventDefault = False
    }
    (Json.Decode.succeed handler)

onRightClick : msg -> Html.Attribute msg
onRightClick handler =
  onRightClickPreventDefault True handler

onRightClickPreventDefault : Bool -> msg -> Html.Attribute msg
onRightClickPreventDefault preventDefault handler =
  E.onWithOptions
    "contextmenu"
    { stopPropagation = True
    , preventDefault = preventDefault
    }
    (Json.Decode.succeed handler)

-- Derived from (under "keyCode"):
--   http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html-Events
onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  E.on "keydown" (Json.Decode.map tagger E.keyCode)

enterKeyCode : Int
enterKeyCode =
  13

styleListToString : List (String, String) -> String
styleListToString =
  let
    stylePairToString (attr, val) =
      attr ++ ": " ++ val ++ "; "
  in
    String.concat << List.map stylePairToString
