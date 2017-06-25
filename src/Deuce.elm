module Deuce exposing (overlay)

import List
import String
import Array exposing (Array)

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SE

import InterfaceModel as Model exposing
  ( Model
  , Msg(..)
  , Code
  )

import InterfaceController as Controller

import Lang exposing
  ( Exp
  , foldExp
  )

import DeuceWidgets exposing
  ( DeuceState
  , DeuceWidget(..)
  )

--==============================================================================
--= INDEXED
--==============================================================================

type alias Indexed a =
  (Int, a)

index : List a -> List (Indexed a)
index = List.indexedMap (,)

--==============================================================================
--= GENERAL DATA TYPES
--==============================================================================

--------------------------------------------------------------------------------
-- Hulls
--------------------------------------------------------------------------------

type alias CodePos =
  (Int, Int)

type alias AbsolutePos =
  (Float, Float)

type alias Hull =
  List AbsolutePos

-- CodePos to AbsolutePos
c2a : DisplayInfo -> CodePos -> AbsolutePos
c2a di (cx, cy) =
  ( di.characterWidth * toFloat cx
  , di.lineHeight * toFloat cy
  )

--------------------------------------------------------------------------------
-- Lines
--------------------------------------------------------------------------------

type alias Line =
  { startCol : Int
  , endCol : Int
  , val : String
  }

type alias LineHulls =
  Array Hull

--------------------------------------------------------------------------------
-- Information
--------------------------------------------------------------------------------

type alias DisplayInfo =
  { lineHeight : Float
  , characterWidth : Float
  }

type alias CodeInfo =
  { displayInfo : DisplayInfo
  , lineHulls : LineHulls
  , deuceState : DeuceState
  }

--==============================================================================
--= LINE FUNCTIONS
--==============================================================================

trimLine : String -> Line
trimLine s =
  let
    trimmed =
      String.trim s
    trimmedLen =
      String.length trimmed
    trimmedLeftLen =
      (String.length << String.trimLeft) s
    trimmedRightLen =
      (String.length << String.trimRight) s
    startCol =
      trimmedRightLen - trimmedLeftLen
    endCol =
      startCol + trimmedLen
  in
    { startCol =
        startCol
    , endCol =
        endCol
    , val =
        trimmed
    }

lines : String -> List Line
lines =
  List.map trimLine << String.split "\n"

lineHull : DisplayInfo -> Indexed Line -> Hull
lineHull di (row, line) =
  List.map (c2a di)
    [ (line.startCol, row)
    , (line.startCol, row + 1)
    , (line.endCol, row + 1)
    , (line.endCol, row)
    ]

lineHullsFromCode : DisplayInfo -> Code -> LineHulls
lineHullsFromCode di =
  Array.fromList << List.map (lineHull di) << index << lines

--==============================================================================
--= HULL FUNCTIONS
--==============================================================================

expHull : CodeInfo -> Exp -> Hull
expHull codeInfo e =
  let
    -- Use 0-indexing
    startCol =
      e.start.col - 1
    startRow =
      e.start.line - 1
    endCol =
      e.end.col - 1
    endRow =
      e.end.line - 1
    -- Get relevant part of line array
    relevantLines =
      Array.toList <| Array.slice (startRow + 1) endRow codeInfo.lineHulls
  in
    if startRow /= endRow then
      -- Left of first line
      ( List.map (c2a codeInfo.displayInfo)
          [ (startCol, startRow)
          , (startCol, startRow + 1)
          ]
      ) ++

      -- Left of middle lines
      ( List.concat <|
          List.map (List.take 2)
            relevantLines
      ) ++

      -- Left of last line
      ( List.take 2 <|
          Maybe.withDefault [] <|
            Array.get endRow codeInfo.lineHulls
      ) ++

      -- Right of last line
      ( List.map (c2a codeInfo.displayInfo)
          [ (endCol, endRow + 1)
          , (endCol, endRow)
          ]
      ) ++

      -- Right of middle lines
      ( List.concat <|
          List.map (List.drop 2) <|
            List.reverse relevantLines
      ) ++

      -- Right of first line
      ( List.drop 2 <|
          Maybe.withDefault [] <|
            Array.get startRow codeInfo.lineHulls
      )
    else
      List.map (c2a codeInfo.displayInfo)
        [ (startCol, startRow)
        , (startCol, startRow + 1)
        , (endCol, startRow + 1)
        , (endCol, startRow)
        ]

hullPoints : Hull -> String
hullPoints =
  let
    pairToString (x, y) =
      (toString x) ++ "," ++ (toString y) ++ " "
  in
    String.concat << List.map pairToString

boundingHullPoints : CodeInfo -> Exp -> String
boundingHullPoints ci e =
  hullPoints <| expHull ci e

--==============================================================================
--= POLYGONS
--==============================================================================

expPolygon : CodeInfo -> Exp -> Svg Msg
expPolygon ci e =
  let
    r = toString <| 100 * (e.start.col % 3)
    g = toString <| 50 * (e.end.col % 5)
    b = toString <| 50 * (e.start.col % 7)
    deuceWidget =
      DeuceExp e.val.eid
    onMouseOver =
      Controller.msgMouseEnterDeuceWidget deuceWidget
    onMouseOut =
      Controller.msgMouseLeaveDeuceWidget deuceWidget
    active =
      List.member deuceWidget ci.deuceState.hoveredWidgets
    strokeWidth =
      if active then "2px" else "0"
  in
    Svg.polygon
      [ SAttr.points <| boundingHullPoints ci e
      , SAttr.fill "rgba(0,0,0,0)"
      , SAttr.strokeWidth strokeWidth
      , SAttr.stroke <| "rgb(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"
      , SE.onMouseOver onMouseOver
      , SE.onMouseOut onMouseOut
      ]
      []

polygons : CodeInfo -> Exp -> (List (Svg Msg))
polygons ci ast =
  List.reverse <|
    foldExp
      ( \e acc ->
          expPolygon ci e :: acc
      )
      []
      ast

--==============================================================================
--= EXPORTS
--==============================================================================

overlay : Model -> Svg Msg
overlay model =
  let
    ast =
      model.inputExp
    displayInfo =
      { lineHeight =
          model.codeBoxInfo.lineHeight
      , characterWidth =
          model.codeBoxInfo.characterWidth
      }
    lineHulls =
      lineHullsFromCode displayInfo model.code
    codeInfo =
      { displayInfo =
          displayInfo
      , lineHulls =
          lineHulls
      , deuceState =
          model.deuceState
      }
  in
    Svg.g
      [ SAttr.transform <|
          "translate(" ++ toString model.codeBoxInfo.contentLeft ++ ", 0)"
      ]
      ( polygons codeInfo ast
      )
