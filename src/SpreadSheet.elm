port module SpreadSheet exposing (SpreadSheet, render, valToSpreadSheet, updateCell)

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Encode as Encode
import Lang exposing (..)
import InterfaceModel as Model
import Utils

type alias SpreadSheet
  = { columns: List String
    , rows: List String
    }

port render : SpreadSheet -> Cmd msg
              
makeCol id name field =
  let col = Encode.object
            [ ("id", Encode.string id)
            , ("name", Encode.string name)
            , ("field", Encode.string field)
            ]
  in
    Encode.encode 0 col

makeRow col data =
  let row = Encode.object
            <| Utils.zip (List.map strVal col) (List.map (Encode.string << strVal) data)
  in
    Encode.encode 0 row
          
valToSpreadSheet : List (List Val) -> SpreadSheet
valToSpreadSheet vss =
  case vss of
    vs::vss_ ->
      let columns = List.map (\v -> makeCol (strVal v) (strVal v) (strVal v)) vs in
      let rows = List.map (makeRow vs) vss_ in
      { columns = columns, rows = rows }
    _        -> Debug.crash "TODO: no header"

type alias Pos = (Int, Int)
               
type alias CellInfo
  = { pos : Pos
    , value : String
    }
  
port updateCell : (CellInfo -> msg) -> Sub msg
