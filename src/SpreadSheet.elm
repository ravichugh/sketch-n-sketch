port module SpreadSheet exposing
       (SpreadSheet,
        CellInfo,
        render,
        valToSpreadSheet,
        updateCell,
        cellSelection
       )

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Encode as Encode
import Char

import Lang exposing (..)
import Utils

type alias SpreadSheet
  = { header: List String
    , data: List (List String)
    }

makeRow data = List.map strVal1 data
                                
valToSpreadSheet : List Val -> List (List Val) -> SpreadSheet
valToSpreadSheet header vss =
  let cols = List.map strVal1 header in
  let rows = List.map makeRow vss in
  { header = cols, data = rows }

type alias Pos = (Int, Int)
               
type alias CellInfo
  = { pos : Pos
    , value : String
    }

port render : SpreadSheet -> Cmd msg

port updateCell : (CellInfo -> msg) -> Sub msg

port cellSelection : (CellInfo -> msg) -> Sub msg
