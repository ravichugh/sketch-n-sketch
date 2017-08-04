port module DeucePopupPanelInfo exposing
  ( DeucePopupPanelInfo
  , requestDeucePopupPanelInfo
  , receiveDeucePopupPanelInfo
  )

type alias DeucePopupPanelInfo =
  { height : Int
  }

port requestDeucePopupPanelInfo : () -> Cmd msg

port receiveDeucePopupPanelInfo : (DeucePopupPanelInfo -> msg) -> Sub msg
