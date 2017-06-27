module Deuce exposing (overlay)

import List
import String
import Tuple

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SE

import Utils
import HtmlUtils exposing (styleListToString)

import InterfaceModel as Model exposing
  ( Model
  , Msg(..)
  , Code
  )

import InterfaceController as Controller

import Lang exposing
  ( Exp
  , Exp__(..)
  , Pat
  , LetKind(..)
  , CodeObject(..)
  , foldCode
  )

import DeuceWidgets exposing
  ( DeuceState
  , DeuceWidget(..)
  )

--==============================================================================
--= HELPER FUNCTIONS
--==============================================================================

slice : Int -> Int -> List a -> List a
slice start end list =
  list
    |> List.drop start
    |> List.take (end - start)

get : Int -> List a -> Maybe a
get i list =
  list
    |> List.drop i
    |> List.head

rgba : Int -> Int -> Int -> Float -> String
rgba r g b a =
  "rgba("
    ++ (toString r) ++ ","
    ++ (toString g) ++ ","
    ++ (toString b) ++ ","
    ++ (toString a) ++ ")"

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
  List Hull

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

emptyLine : Line
emptyLine =
  { startCol = 0
  , endCol = 0
  , val = ""
  }

isBlankLine : Line -> Bool
isBlankLine line =
  line.startCol == line.endCol

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

-- TODO Not quite right; possibly need to pad based on expression position
--      rather than lines.
--
-- padLines : List Line -> List Line
-- padLines lines =
--   let
--     paddingFolder line (lastLine, newList) =
--       let
--         newLine =
--           if isBlankLine line then
--             { lastLine | val = line.val }
--           else
--             line
--       in
--         (newLine, newLine :: newList)
--   in
--     Tuple.second <|
--       List.foldr paddingFolder (emptyLine, []) lines

lineHull : DisplayInfo -> Indexed Line -> Hull
lineHull di (row, line) =
  List.map (c2a di)
    [ (line.startCol, row)
    , (line.startCol, row + 1)
    , (line.endCol, row + 1)
    , (line.endCol, row)
    ]

lineHullsFromCode : DisplayInfo -> Code -> LineHulls
lineHullsFromCode di code =
  code
    |> lines
    |> index
    |> List.map (lineHull di)

--==============================================================================
--= HULL FUNCTIONS
--==============================================================================

-- NOTE: Use 0-indexing for columns and rows.
hull : CodeInfo -> Int -> Int -> Int -> Int -> Hull
hull codeInfo startCol startRow endCol endRow =
  let
    relevantLines =
      slice (startRow + 1) endRow codeInfo.lineHulls
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
            get endRow codeInfo.lineHulls
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
            get startRow codeInfo.lineHulls
      )
    else
      List.map (c2a codeInfo.displayInfo)
        [ (startCol, startRow)
        , (startCol, startRow + 1)
        , (endCol, startRow + 1)
        , (endCol, startRow)
        ]

codeObjectHull : CodeInfo -> CodeObject -> Hull
codeObjectHull codeInfo codeObject =
  let
    (startCol, startRow, endCol, endRow) =
      case codeObject of
        E e ->
          let
            endExp =
              case e.val.e__ of
                ELet _ Let _ _ binding _ _ ->
                  binding
                _ ->
                  e
          in
            ( e.start.col - 1
            , e.start.line - 1
            , endExp.end.col - 1
            , endExp.end.line - 1
            )
        P p ->
            ( p.start.col - 1
            , p.start.line - 1
            , p.end.col - 1
            , p.end.line - 1
            )
        T t ->
            ( t.start.col - 1
            , t.start.line - 1
            , t.end.col - 1
            , t.end.line - 1
            )
  in
    hull codeInfo startCol startRow endCol endRow

hullPoints : Hull -> String
hullPoints =
  let
    pairToString (x, y) =
      (toString x) ++ "," ++ (toString y) ++ " "
  in
    String.concat << List.map pairToString

codeObjectHullPoints : CodeInfo -> CodeObject -> String
codeObjectHullPoints codeInfo codeObject =
  hullPoints <| codeObjectHull codeInfo codeObject

--==============================================================================
--= POLYGONS
--==============================================================================

codeObjectPolygon : CodeInfo -> CodeObject -> DeuceWidget -> Svg Msg
codeObjectPolygon codeInfo codeObject deuceWidget =
  let
    -- Eventually move style into additional parameter (like "extraAttrs")
    (r, g, b) =
      case codeObject of
        E e ->
          ( 50 * (e.start.col % 5) + 50
          , 30
          , 30
          )
        P p ->
          ( 30
          , 50 * (p.start.col % 5) + 50
          , 30
          )
        T t ->
          ( 30
          , 30
          , 50 * (t.start.col % 5) + 50
          )
    onMouseOver =
      Controller.msgMouseEnterDeuceWidget deuceWidget
    onMouseOut =
      Controller.msgMouseLeaveDeuceWidget deuceWidget
    onClick =
      Controller.msgMouseClickDeuceWidget deuceWidget
    hovered =
      List.member deuceWidget codeInfo.deuceState.hoveredWidgets
    active =
      List.member deuceWidget codeInfo.deuceState.selectedWidgets
    (strokeWidth, a, cursorStyle) =
      if hovered || active then
        ("2px", 0.2, "pointer")
      else
        ("0", 0, "default")
  in
    Svg.polygon
      [ SAttr.points <| codeObjectHullPoints codeInfo codeObject
      , SAttr.strokeWidth strokeWidth
      , SAttr.stroke <| rgba r g b 1
      , SAttr.fill <| rgba r g b a
      , SAttr.style << styleListToString <|
          [ ("cursor", cursorStyle)
          ]
      , SE.onMouseOver onMouseOver
      , SE.onMouseOut onMouseOut
      , SE.onClick onClick
      ]
      []

expPolygon : CodeInfo -> Exp -> Svg Msg
expPolygon codeInfo e =
  let
    codeObject =
      E e
    deuceWidget =
      case e.val.e__ of
        ELet _ _ _ _ _ _ _ ->
          DeuceLetBindingEquation e.val.eid
        _ ->
          DeuceExp e.val.eid
  in
    codeObjectPolygon codeInfo codeObject deuceWidget

patPolygon : CodeInfo -> Pat -> Svg Msg
patPolygon codeInfo p =
  let
    codeObject =
      P p
    deuceWidget =
      -- TODO PathedPatternId
      DeucePat ((p.val.pid, 1), [])
  in
    codeObjectPolygon codeInfo codeObject deuceWidget

polygons : CodeInfo -> Exp -> (List (Svg Msg))
polygons codeInfo ast =
  List.reverse <|
    foldCode
      { expFolder =
          \e acc ->
            expPolygon codeInfo e :: acc
      , patFolder =
          \p acc ->
            patPolygon codeInfo p :: acc
      , typeFolder =
          flip always
      }
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
