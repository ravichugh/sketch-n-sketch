port module SpreadSheet exposing
       (SpreadSheet,
        CellInfo,
        render,
        valToStyledSheet,
        updateCell,
        cellSelection
       )

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Encode as Encode
import Dict

import Lang exposing (..)
import Utils

type alias SpreadSheet
  = { header: List String
    , data: List (List String)
    }

makeRow data = List.map strVal1 data
                                
valToRawSheet : List Val -> List (List Val) -> SpreadSheet
valToRawSheet header vss =
  let cols = List.map strVal1 header in
  let rows = List.map makeRow vss in
  { header = cols, data = rows }


--type alias StyledCell = Dict.Dict String String
  {-                   
type alias StyledSheet
  = { header : List String
    , dataWithStyle : List (List StyledCell)
    } -}

encodeCell : Cell -> String
encodeCell cell =
  let cellToObj cell =
        Dict.foldl (\k v acc -> (k, Encode.string <| strVal1 v) :: acc) [] cell
  in
    cellToObj cell |> Encode.object |> Encode.encode 0
      
valToStyledSheet : List Val -> List (List Cell) -> SpreadSheet
valToStyledSheet header vss =
  let convertRow cells = List.map encodeCell cells in 
  let cols = List.map strVal1 header in
  let rows = List.map convertRow vss in
  { header = cols, data = rows }

type alias Pos = (Int, Int)
               
type alias CellInfo
  = { pos : Pos
    , value : String
    }

port render : SpreadSheet -> Cmd msg

port updateCell : (CellInfo -> msg) -> Sub msg

port cellSelection : (CellInfo -> msg) -> Sub msg
