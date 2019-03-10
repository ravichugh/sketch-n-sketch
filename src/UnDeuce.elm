module UnDeuce exposing
  ( overlay
  )

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SE

import DeuceColor
import DeuceParameters
import DeuceGeometry as DG

import Model exposing (ColorScheme(..))

import UnLang exposing (UnExp)

import Info exposing (WithInfo)
import Utils

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- (startCol, startRow, endCol, endRow)
-- NOTE: 0-indexed.
startEnd :
  DG.LineHulls -> Int -> WithInfo String -> (Int, Int, Int, Int)
startEnd lineHulls maxLineLength info =
  let
    (startCol, startLine, endCol, endLine) =
      ( info.start.col
      , info.start.line
      , info.end.col
      , info.end.line
      )
    -- Special cases
    (realStartLine, realStartCol, realEndLine, realEndCol) =
      -- Manual extension to max col
      if endCol == 0 then
        ( startLine
        , startCol
        , endLine - 1
        , maxLineLength + 1
        )
      -- Removal
      else if Info.hasDummyInfo info then
        (-100, -100, -100, -100)
      else
        (startLine, startCol, endLine, endCol)
  in
    ( realStartCol - 1
    , realStartLine - 1
    , realEndCol - 1
    , realEndLine - 1
    )

--------------------------------------------------------------------------------
-- Geometric Parameters
--------------------------------------------------------------------------------

c2a : DG.CodePos -> DG.AbsolutePos
c2a (cx, cy) =
  ( 7.19 * toFloat cx
  , 15.0 * toFloat cy
  )

--------------------------------------------------------------------------------
-- Polygons
--------------------------------------------------------------------------------

type alias Handlers msg =
  { onClick : UnExp (WithInfo String) -> msg
  , onMouseOver : UnExp (WithInfo String) -> msg
  , onMouseOut : UnExp (WithInfo String) -> msg
  }

polygon :
  Handlers msg -> DG.LineHulls -> Int -> UnExp (WithInfo String) -> Svg msg
polygon handlers lineHulls maxLineLength unExp =
  let
    info =
      UnLang.getData unExp

    infoHullPoints =
      info
        |> startEnd lineHulls maxLineLength
        |> Utils.uncurry4 (DG.hull c2a 0 0 lineHulls False False)
        |> DG.hullPoints
  in
    Svg.polygon
      [ SAttr.class "deuce-unexp-poly"

      , SAttr.points infoHullPoints

      , SE.onClick <| handlers.onClick unExp

      , SAttr.strokeWidth <|
          DeuceParameters.strokeWidth Light
      , SAttr.stroke <|
          DeuceColor.rgbaString (DeuceParameters.objectColor Light) 1
      , SAttr.fill <|
          DeuceColor.rgbaString
            (DeuceParameters.objectColor Light)
            (DeuceParameters.polygonOpacity Light)
      ]
      []

--------------------------------------------------------------------------------
-- Exports
--------------------------------------------------------------------------------

overlay : Handlers msg -> UnExp (WithInfo String) -> List (Html msg)
overlay handlers root =
  let
    info =
      UnLang.getData root

    (_, trimmedLineHulls, maxLineLength) =
      DG.lineHulls c2a info.val
  in
    root
      |> UnLang.flatten
      |> List.map (polygon handlers trimmedLineHulls maxLineLength)
