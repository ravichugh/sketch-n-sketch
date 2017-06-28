module Deuce exposing (overlay)

import List
import String
import Tuple
import Dict exposing (Dict)

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
  ( WS
  , BeforeAfter
  , Exp
  , Exp__(..)
  , Pat
  , LetKind(..)
  , PId
  , PathedPatternId
  , CodeObject(..)
  , extractInfoFromCodeObject
  , isTarget
  , startEnd
  , foldCode
  , computePatMap
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

type alias Color =
  { r : Int
  , g : Int
  , b : Int
  }

type alias Opacity =
  Float

rgbaString : Color -> Opacity -> String
rgbaString c a =
  "rgba("
    ++ (toString c.r) ++ ","
    ++ (toString c.g) ++ ","
    ++ (toString c.b) ++ ","
    ++ (toString a) ++ ")"

-- (startCol, startRow, endCol, endRow) but 0-indexed
startEndZeroIndexed : CodeObject -> (Int, Int, Int, Int)
startEndZeroIndexed codeObject =
  let
    (startCol, startRow, endCol, endRow) =
      startEnd codeObject
  in
    ( startCol - 1
    , startRow - 1
    , endCol - 1
    , endRow - 1
    )

mapBoth : (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) =
  (f x, f y)

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
  , untrimmedLineHulls : LineHulls
  , trimmedLineHulls : LineHulls
  , deuceState : DeuceState
  , patMap : Dict PId PathedPatternId
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

untrimmedLine : String -> Line
untrimmedLine s =
  { startCol =
      0
  , endCol =
      String.length s
  , val =
      s
  }

trimmedLine : String -> Line
trimmedLine s =
  let
    trimmed =
      String.trim s
    trimmedLen =
      String.length trimmed
    trimmedRightLen =
      (String.length << String.trimRight) s
    startCol =
      trimmedRightLen - trimmedLen
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

-- Returns: (untrimmed, trimmed)
lineHullsFromCode : DisplayInfo -> Code -> (LineHulls, LineHulls)
lineHullsFromCode di code =
  let
    pipeline lineKind =
      code
        |> String.lines
        |> List.map lineKind
        |> index
        |> List.map (lineHull di)
  in
    ( pipeline untrimmedLine
    , pipeline trimmedLine
    )

--==============================================================================
--= HULL FUNCTIONS
--==============================================================================

-- NOTE: Use 0-indexing for columns and rows.
hull : CodeInfo -> Bool -> Int -> Int -> Int -> Int -> Hull
hull codeInfo useTrimmed startCol startRow endCol endRow =
  let
    lineHulls =
      if useTrimmed then
        codeInfo.trimmedLineHulls
      else
        codeInfo.untrimmedLineHulls
    relevantLines =
      slice (startRow + 1) endRow lineHulls
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
            get endRow lineHulls
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
            get startRow lineHulls
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
    useTrimmed =
      (not << isTarget) codeObject
    (startCol, startRow, endCol, endRow) =
      startEndZeroIndexed codeObject
  in
    hull codeInfo useTrimmed startCol startRow endCol endRow

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

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

strokeWidth : String
strokeWidth = "2px"

--------------------------------------------------------------------------------
-- Handles
--------------------------------------------------------------------------------

-- Looks not as good but easier to click
circleHandles
  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
circleHandles codeInfo codeObject color opacity radius =
  let
    (startCol, startRow, endCol, endRow) =
      startEndZeroIndexed codeObject
    (cx1, cy1) =
      (startCol, startRow)
        |> c2a codeInfo.displayInfo
        |> \(x, y) -> (x, y - radius)
        |> mapBoth toString
    (cx2, cy2) =
      (endCol, endRow + 1)
        |> c2a codeInfo.displayInfo
        |> \(x, y) -> (x, y + radius)
        |> mapBoth toString
    radiusString =
      toString radius
  in
    Svg.g
      [ SAttr.fill <| rgbaString color opacity
      , SAttr.strokeWidth strokeWidth
      , SAttr.stroke <| rgbaString color opacity
      ]
      [ Svg.circle
          [ SAttr.cx cx1
          , SAttr.cy cy1
          , SAttr.r radiusString
          ]
          []
      , Svg.circle
          [ SAttr.cx cx2
          , SAttr.cy cy2
          , SAttr.r radiusString
          ]
          []
      ]

-- Looks better but is harder to click
fancyHandles
  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
fancyHandles codeInfo codeObject color opacity radius =
  let
    (startCol, startRow, endCol, endRow) =
      startEndZeroIndexed codeObject
    (xTip1, yTip1) =
      (startCol, startRow)
        |> c2a codeInfo.displayInfo
        |> mapBoth toString
    (xTip2, yTip2) =
      (endCol, endRow + 1)
        |> c2a codeInfo.displayInfo
        |> mapBoth toString
    radiusString =
      toString radius
  in
    Svg.g
      [ SAttr.fill <| rgbaString color opacity
      , SAttr.strokeWidth strokeWidth
      , SAttr.stroke <| rgbaString color opacity
      ]
      [ Svg.path
          [ SAttr.d <|
              "M " ++ xTip1 ++ " " ++ yTip1 ++ "\n"
                ++ "l 0 -" ++ radiusString ++ "\n"
                ++ "a " ++ radiusString ++ " " ++ radiusString
                  ++ ", 0, 1, 0, -" ++ radiusString
                  ++ " " ++ radiusString ++ "\n"
                ++ "Z"
          ]
          []
      , Svg.path
          [ SAttr.d <|
              "M " ++ xTip2 ++ " " ++ yTip2 ++ "\n"
                ++ "l 0 " ++ radiusString ++ "\n"
                ++ "a " ++ radiusString ++ " " ++ radiusString
                  ++ ", 0, 1, 0, " ++ radiusString
                  ++ " -" ++ radiusString ++ "\n"
                ++ "Z"
          ]
          []
      ]

--------------------------------------------------------------------------------
-- Polygons
--------------------------------------------------------------------------------

codeObjectPolygon
  : CodeInfo -> CodeObject -> DeuceWidget -> Color -> Int -> Svg Msg
codeObjectPolygon codeInfo codeObject deuceWidget color zIndex =
  let
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
    (baseAlpha, cursorStyle) =
      if hovered || active then
        (1, "pointer")
      else
        (0, "default")
  in
    Svg.g
      [ SAttr.z <| toString zIndex
      , SAttr.style << styleListToString <|
          [ ("cursor", cursorStyle)
          ]
      , SE.onMouseOver onMouseOver
      , SE.onMouseOut onMouseOut
      , SE.onClick onClick
      ]
      [ circleHandles codeInfo codeObject color baseAlpha 3
      , Svg.polygon
          [ SAttr.points <| codeObjectHullPoints codeInfo codeObject
          , SAttr.strokeWidth strokeWidth
          , SAttr.stroke <| rgbaString color baseAlpha
          , SAttr.fill <| rgbaString color (0.2 * baseAlpha)
          ]
          []
      ]

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
    color =
      { r = 200
      , g = 0
      , b = 0
      }
    zIndex =
      0
  in
    codeObjectPolygon codeInfo codeObject deuceWidget color zIndex

patPolygon : CodeInfo -> Exp -> Pat -> Svg Msg
patPolygon codeInfo e p =
  let
    codeObject =
      P e p
    deuceWidget =
      case Dict.get p.val.pid codeInfo.patMap of
        Just ppid ->
          DeucePat ppid
        Nothing ->
          Debug.crash <|
            "Cannot find ppid of pattern with pid "
              ++ toString p.val.pid
              ++ ". The pattern in question looks like this:\n"
              ++ toString p
    color =
      { r = 0
      , g = 200
      , b = 0
      }
    zIndex = 0
  in
    codeObjectPolygon codeInfo codeObject deuceWidget color zIndex

expTargetPolygon : CodeInfo -> BeforeAfter -> WS -> Exp -> Svg Msg
expTargetPolygon codeInfo ba ws et =
  let
    codeObject =
      ET ba ws et
    deuceWidget =
      DeuceExpTarget (ba, et.val.eid)
    color =
      { r = 200
      , g = 0
      , b = 200
      }
    zIndex =
      1
  in
    codeObjectPolygon codeInfo codeObject deuceWidget color zIndex

patTargetPolygon : CodeInfo -> BeforeAfter -> WS -> Exp -> Pat -> Svg Msg
patTargetPolygon codeInfo ba ws e pt =
  let
    codeObject =
      PT ba ws e pt
    deuceWidget =
      case Dict.get pt.val.pid codeInfo.patMap of
        Just ppid ->
          DeucePatTarget (ba, ppid)
        Nothing ->
          Debug.crash <|
            "Cannot find ppid of targeted pattern with pid "
              ++ toString pt.val.pid
              ++ ". The targeted pattern in question looks like this:\n"
              ++ toString pt
    color =
      { r = 0
      , g = 200
      , b = 200
      }
    zIndex =
      1
  in
    codeObjectPolygon codeInfo codeObject deuceWidget color zIndex

polygons : CodeInfo -> Exp -> (List (Svg Msg))
polygons codeInfo ast =
  List.reverse <|
    foldCode
      ( \codeObject acc ->
          case codeObject of
            E e ->
              expPolygon codeInfo e :: acc
            P e p ->
              patPolygon codeInfo e p :: acc
            T t ->
              acc
            ET ba ws et ->
              expTargetPolygon codeInfo ba ws et :: acc
            PT ba ws e pt ->
              patTargetPolygon codeInfo ba ws e pt :: acc
            TT _ _ _ ->
              acc
      )
      []
      (E ast)

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
    (untrimmedLineHulls, trimmedLineHulls) =
      lineHullsFromCode displayInfo model.code
    patMap =
      computePatMap ast
    codeInfo =
      { displayInfo =
          displayInfo
      , untrimmedLineHulls =
          untrimmedLineHulls
      , trimmedLineHulls =
          trimmedLineHulls
      , deuceState =
          model.deuceState
      , patMap =
          patMap
      }
  in
    Svg.g
      [ SAttr.transform <|
          "translate(" ++ toString model.codeBoxInfo.contentLeft ++ ", 0)"
      ]
      ( polygons codeInfo ast
      )
