port module SpreadSheet exposing
       (SpreadSheet,
        CellInfo,
        render,
        valToSpreadSheet,
        updateCell,
        cellSelection,
        gotoCell
       )

import Html exposing (Html)
import Html.Attributes as Attr
import Json.Encode as Encode
import Char

import Lang exposing (..)
import Utils

type alias SpreadSheet
  = { columns: List String
    , rows: List (List String)
    }

              
makeCol id name field =
  let col = Encode.object
            [ ("id", Encode.string id)
            , ("name", Encode.string name)
            , ("field", Encode.string field)
            ]
  in
    Encode.encode 0 col

makeRow data = List.map strVal1 data
                          
genHeader : Int -> List String
genHeader len =
  let allNum = List.range 1 len in
  let numToChar n = Char.fromCode (65 + n - 1) in
  List.map (String.fromList << (List.map numToChar) << Utils.toBase 26) allNum
      
valToSpreadSheet : List (List Val) -> SpreadSheet
valToSpreadSheet vss =
  case vss of
    vs::vss_ ->
      if vs == []
      then
        let columns = []
            rows = List.map makeRow vss_
        in
          { columns = columns, rows = rows }
      else
      let columns = List.map strVal1 vs in
      let rows = List.map makeRow vss_ in
      { columns = columns, rows = rows }
    _        -> Debug.crash "ill formatted sheet"

type alias Pos = (Int, Int)
               
type alias CellInfo
  = { pos : Pos
    , value : String
    }

port render : SpreadSheet -> Cmd msg

port gotoCell : CellInfo -> Cmd msg

port updateCell : (CellInfo -> msg) -> Sub msg

port cellSelection : (CellInfo -> msg) -> Sub msg
