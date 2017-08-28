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
    , rows: List String
    }

              
makeCol id name field =
  let col = Encode.object
            [ ("id", Encode.string id)
            , ("name", Encode.string name)
            , ("field", Encode.string field)
            ]
  in
    Encode.encode 0 col


makeRow header data =
  let row = Encode.object
            <| Utils.zip
                 header
                 (List.map (Encode.string << strVal1) data)
  in
    Encode.encode 0 row

makeRowWithVal col data = makeRow (List.map strVal1 col) data
                          
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
        let header =
              case Maybe.map genHeader (List.maximum <| List.map List.length vss_) of
                Just header -> header
                _           -> []
        in
          let columns = List.map (\v -> makeCol v v v) header in
          let rows = List.map (makeRow header) vss_ in
          { columns = columns, rows = rows }
      else
      let columns = List.map (\v -> makeCol (strVal1 v) (strVal1 v) (strVal1 v)) vs in
      let rows = List.map (makeRowWithVal vs) vss_ in
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
