port module ColorScheme exposing
  ( updateColorScheme
  )

import InterfaceModel exposing (ColorScheme(..))

-- Internal

port updateColorSchemeByName : String -> Cmd msg

encodeToName : ColorScheme -> String
encodeToName colorScheme =
  case colorScheme of
    Light ->
      "light"
    Dark ->
      "dark"

-- Exports

updateColorScheme : ColorScheme -> Cmd msg
updateColorScheme =
  encodeToName >> updateColorSchemeByName
