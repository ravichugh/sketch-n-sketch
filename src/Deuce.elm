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
  ( WS
  , BeforeAfter
  , Exp
  , Exp__(..)
  , Pat
  , LetKind(..)
  , CodeObject(..)
  , extractInfoFromCodeObject
  , isTarget
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
        _ ->
          let
            info =
              extractInfoFromCodeObject codeObject
          in
            ( info.start.col - 1
            , info.start.line - 1
            , info.end.col - 1
            , info.end.line - 1
            )
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
    (strokeWidth, alpha, cursorStyle) =
      if hovered || active then
        ("2px", 0.2, "pointer")
      else
        ("0", 0, "default")
  in
    Svg.polygon
      [ SAttr.points <| codeObjectHullPoints codeInfo codeObject
      , SAttr.strokeWidth strokeWidth
      , SAttr.stroke <| rgbaString color 1
      , SAttr.fill <| rgbaString color alpha
      , SAttr.z <| toString zIndex
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
    color =
      { r = 200
      , g = 0
      , b = 0
      }
    zIndex =
      0
  in
    codeObjectPolygon codeInfo codeObject deuceWidget color zIndex

patPolygon : CodeInfo -> Pat -> Svg Msg
patPolygon codeInfo p =
  let
    codeObject =
      P p
    deuceWidget =
      -- TODO PathedPatternId
      DeucePat ((p.val.pid, 1), [])
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

patTargetPolygon : CodeInfo -> BeforeAfter -> WS -> Pat -> Svg Msg
patTargetPolygon codeInfo ba ws pt =
  let
    codeObject =
      PT ba ws pt
    deuceWidget =
      -- TODO PathedPatternId
      DeucePatTarget (ba, ((pt.val.pid, 1), []))
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
      { expFolder =
          \e acc ->
            expPolygon codeInfo e :: acc
      , patFolder =
          \p acc ->
            patPolygon codeInfo p :: acc
      , typeFolder =
          \_ acc ->
            acc
      , expTargetFolder =
          \ba ws et acc ->
            expTargetPolygon codeInfo ba ws et :: acc
      , patTargetFolder =
          \ba ws pt acc ->
            patTargetPolygon codeInfo ba ws pt :: acc
      , typeTargetFolder =
          \_ _ _ acc ->
            acc
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
    (untrimmedLineHulls, trimmedLineHulls) =
      lineHullsFromCode displayInfo model.code
    codeInfo =
      { displayInfo =
          displayInfo
      , untrimmedLineHulls =
          untrimmedLineHulls
      , trimmedLineHulls =
          trimmedLineHulls
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
