module DeuceParameters exposing (..)

import Model exposing
  ( ColorScheme(..)
  )

import DeuceColor exposing (..)

strokeWidth : ColorScheme -> String
strokeWidth colorScheme =
  "2px"

polygonOpacity : ColorScheme -> Float
polygonOpacity colorScheme =
  0.2

diffColor : ColorScheme -> String -> Color
diffColor colorScheme tag =
  case colorScheme of
    Light ->
      case tag of
        "+" -> { r = 0, g = 255, b = 0}
        "-" -> { r = 255, g = 0, b = 0}
        _ ->   { r = 255, g = 165, b = 0}
    Dark ->
      case tag of
        "+" -> { r = 0, g = 200, b = 0}
        "-" -> { r = 200, g = 0, b = 0}
        _ ->   { r = 200, g = 200, b = 100}

objectColor : ColorScheme -> Color
objectColor colorScheme =
  case colorScheme of
    Light ->
      { r = 255
      , g = 165
      , b = 0
      }
    Dark ->
      { r = 200
      , g = 200
      , b = 100
      }

objectErrorColor : ColorScheme -> Color
objectErrorColor colorScheme =
  case colorScheme of
    Light -> { r = 255 , g = 0 , b = 0 }
    Dark  -> { r = 255 , g = 0 , b = 0 }

objectInfoColor : ColorScheme -> Color
objectInfoColor colorScheme =
  case colorScheme of
    Light -> { r = 144, g = 238, b = 144 }
    Dark  -> { r = 144, g = 238, b = 144 }

keyboardFocusedColor : ColorScheme -> Color
keyboardFocusedColor colorScheme =
  case colorScheme of
    Light -> { r = 20, g = 200, b = 40 }
    Dark  -> { r = 20, g = 200, b = 40 }

typeColor : ColorScheme -> Color
typeColor colorScheme =
  case colorScheme of
    Light -> { r = 220, g = 60, b = 255 }
    Dark  -> { r = 220, g = 60, b = 255 }

whitespaceColor : ColorScheme -> Color
whitespaceColor colorScheme =
  case colorScheme of
    Light ->
      { r = 0
      , g = 100
      , b = 255
      }
    Dark ->
      { r = 0
      , g = 200
      , b = 200
      }
