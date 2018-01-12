--------------------------------------------------------------------------------
-- This modules provides the Deuce overlay for the View.
--------------------------------------------------------------------------------

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
  , ColorScheme(..)
  )

import InterfaceController as Controller

import Lang exposing
  ( WithInfo
  , WS
  , BeforeAfter(..)
  , Exp
  , Exp__(..)
  , Pat
  , LetKind(..)
  , EId
  , PId
  , PathedPatternId
  , CodeObject(..)
  , extractInfoFromCodeObject
  , isTarget
  , isSelectable
  , foldCode
  , computePatMap
  , firstNestedExp
  )

import DeuceWidgets exposing
  ( DeuceState
  , DeuceWidget(..)
  , toDeuceWidget
  )

import SleekLayout

--==============================================================================
--= HELPER FUNCTIONS
--==============================================================================

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

-- (startCol, startRow, endCol, endRow)
-- NOTE: 0-indexed.
startEnd : CodeInfo -> CodeObject -> (Int, Int, Int, Int)
startEnd codeInfo codeObject =
  let
    info =
      extractInfoFromCodeObject codeObject
    infoTuple =
      ( info.start.col
      , info.start.line
      , info.end.col
      , info.end.line
      )
    (startCol, startLine, endCol, endLine) =
      case codeObject of
        E e ->
          let
            endExp =
              firstNestedExp e
          in
            ( e.start.col
            , e.start.line
            , endExp.end.col
            , endExp.end.line
            )
        _ ->
          infoTuple
    -- Special cases
    (realStartLine, realStartCol, realEndLine, realEndCol) =
      -- Manual extension to max col
      if endCol == 0 then
        ( startLine
        , startCol
        , endLine - 1
        , codeInfo.maxLineLength + 1
        )
      -- Removal
      else if Lang.hasDummyInfo info then
        (-100, -100, -100, -100)
      else
        (startLine, startCol, endLine, endCol)
  in
    ( realStartCol - 1
    , realStartLine - 1
    , realEndCol - 1
    , realEndLine - 1
    )

--==============================================================================
--= DATA TYPES
--==============================================================================

--------------------------------------------------------------------------------
-- Indexed
--------------------------------------------------------------------------------

type alias Indexed a =
  (Int, a)

index : List a -> List (Indexed a)
index = List.indexedMap (,)

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
  , colorScheme : ColorScheme
  }

type alias CodeInfo =
  { displayInfo : DisplayInfo
  , untrimmedLineHulls : LineHulls
  , trimmedLineHulls : LineHulls
  , deuceState : DeuceState
  , previewDeuceWidgets : Maybe (List DeuceWidget)
  , patMap : Dict PId PathedPatternId
  , maxLineLength : Int
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

computeMaxLineLength : List String -> Int
computeMaxLineLength strings =
  let
    lens =
      List.map String.length strings
  in
    Maybe.withDefault 0 <|
      List.maximum lens

untrimmedLine : Int -> List String -> List Line
untrimmedLine maxLen strings =
  let
    startCol =
      0
    endCol =
      maxLen + 1
    lineMapper s =
      { startCol =
          startCol
      , endCol =
          endCol
      , val =
          s
      }
  in
    List.map lineMapper strings

trimmedLine : List String -> List Line
trimmedLine =
  List.map <|
    \s ->
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

lineHull : DisplayInfo -> Indexed Line -> Hull
lineHull di (row, line) =
  List.map (c2a di)
    [ (line.startCol, row)
    , (line.startCol, row + 1)
    , (line.endCol, row + 1)
    , (line.endCol, row)
    ]

-- Returns: (untrimmed, trimmed, max line length)
lineHullsFromCode : DisplayInfo -> Code -> (LineHulls, LineHulls, Int)
lineHullsFromCode di code =
  let
    lines =
      String.lines code
    maxLineLength =
      computeMaxLineLength lines
    pipeline lineKind =
      lines
        |> lineKind
        |> index
        |> List.map (lineHull di)
  in
    ( pipeline <| untrimmedLine maxLineLength
    , pipeline trimmedLine
    , maxLineLength
    )

--==============================================================================
--= HULL FUNCTIONS
--==============================================================================

zeroWidthPadding : Float
zeroWidthPadding = 2

affectedByBleed : CodeObject -> Bool
affectedByBleed =
  isTarget

specialEndFlag : Float
specialEndFlag =
  -123456789

addBleed : AbsolutePos -> AbsolutePos
addBleed (x, y) =
  if x == specialEndFlag then
    ( 0
    , y
    )
  else if x <= 0 then
    ( -SleekLayout.deuceOverlayBleed
    , y
    )
  else
    (x, y)

addFinalEndBleed : AbsolutePos -> AbsolutePos
addFinalEndBleed (x, y) =
  if x <= 0 then
    ( specialEndFlag
    , y
    )
  else
    (x, y)

-- NOTE: Use 0-indexing for columns and rows.
hull : CodeInfo -> Bool -> Bool -> Int -> Int -> Int -> Int -> Hull
hull codeInfo useTrimmed shouldAddBleed startCol startRow endCol endRow =
  let
    lineHulls =
      if useTrimmed then
        codeInfo.trimmedLineHulls
      else
        codeInfo.untrimmedLineHulls
    relevantLines =
      Utils.slice (startRow + 1) endRow lineHulls
    (modifier, finalEndModifier) =
      if shouldAddBleed then
        ( List.map addBleed
        , addFinalEndBleed
        )
      else
        ( identity
        , identity
        )
  in
    modifier <|
      -- Multi-line
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
              Utils.maybeGeti0 endRow lineHulls
        ) ++

        -- Right of last line
        ( List.map (finalEndModifier << c2a codeInfo.displayInfo)
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
              Utils.maybeGeti0 startRow lineHulls
        )
      -- Zero-width
      else if startCol == endCol then
        let
          (x, yTop) =
            c2a codeInfo.displayInfo (startCol, startRow)
          (_, yBottom) =
            c2a codeInfo.displayInfo (startCol, startRow + 1)
        in
          [ (x - zeroWidthPadding, yTop)
          , (x - zeroWidthPadding, yBottom)
          , (x + zeroWidthPadding, yBottom)
          , (x + zeroWidthPadding, yTop)
          ]
      -- Single-line, nonzero-width
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
    shouldAddBleed =
      affectedByBleed codeObject
    (startCol, startRow, endCol, endRow) =
      startEnd codeInfo codeObject
  in
    hull codeInfo useTrimmed shouldAddBleed startCol startRow endCol endRow

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

strokeWidth : ColorScheme -> String
strokeWidth colorScheme =
  "2px"

polygonOpacity : ColorScheme -> Float
polygonOpacity colorScheme =
  0.2

objectColor : ColorScheme -> Color
objectColor colorScheme =
  case colorScheme of
    Light ->
      { r = 255
      , g = 165
      , b = 0
      }
    Dark ->
      { r = 200
      , g = 200
      , b = 100
      }

whitespaceColor : ColorScheme -> Color
whitespaceColor colorScheme =
  case colorScheme of
    Light ->
      { r = 0
      , g = 100
      , b = 255
      }
    Dark ->
      { r = 0
      , g = 200
      , b = 200
      }

--------------------------------------------------------------------------------
-- Handles
--------------------------------------------------------------------------------
-- The following functions are a couple different options for different handle
-- styles.
--------------------------------------------------------------------------------

circleHandles
  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
circleHandles codeInfo codeObject color opacity radius =
  let
    radiusString =
      toString radius
    accountForBleed mightBleed (x, y) =
      if
        x <= 0 &&
        mightBleed &&
        affectedByBleed codeObject
      then
        ( -SleekLayout.deuceOverlayBleed
        , y
        )
      else
        (x, y)
    handle mightBleed col row =
      let
        (cx, cy) =
          (col, row)
            |> c2a codeInfo.displayInfo
            |> accountForBleed mightBleed
            |> Utils.mapBoth toString
      in
        Svg.circle
          [ SAttr.cx cx
          , SAttr.cy cy
          , SAttr.r radiusString
          ]
          []
    (startCol, startRow, endCol, endRow) =
      startEnd codeInfo codeObject
  in
    Svg.g
      [ SAttr.fill <|
          rgbaString color opacity
      , SAttr.strokeWidth <|
          strokeWidth codeInfo.displayInfo.colorScheme
      , SAttr.stroke <|
          rgbaString color opacity
      ]
      [ handle True startCol startRow
      , handle False endCol (endRow + 1)
      ]

--oldCircleHandles
--  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
--oldCircleHandles codeInfo codeObject color opacity radius =
--  let
--    (startCol, startRow, endCol, endRow) =
--      startEnd codeInfo codeObject
--    (cx1, cy1) =
--      (startCol, startRow)
--        |> c2a codeInfo.displayInfo
--        |> \(x, y) -> (x, y - radius)
--        |> Utils.mapBoth toString
--    (cx2, cy2) =
--      (endCol, endRow + 1)
--        |> c2a codeInfo.displayInfo
--        |> \(x, y) -> (x, y + radius)
--        |> Utils.mapBoth toString
--    radiusString =
--      toString radius
--  in
--    Svg.g
--      [ SAttr.fill <| rgbaString color opacity
--      , SAttr.strokeWidth strokeWidth
--      , SAttr.stroke <| rgbaString color opacity
--      ]
--      [ Svg.circle
--          [ SAttr.cx cx1
--          , SAttr.cy cy1
--          , SAttr.r radiusString
--          ]
--          []
--      , Svg.circle
--          [ SAttr.cx cx2
--          , SAttr.cy cy2
--          , SAttr.r radiusString
--          ]
--          []
--      ]
--
--fancyHandles
--  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
--fancyHandles codeInfo codeObject color opacity radius =
--  let
--    (startCol, startRow, endCol, endRow) =
--      startEnd codeInfo codeObject
--    (xTip1, yTip1) =
--      (startCol, startRow)
--        |> c2a codeInfo.displayInfo
--        |> Utils.mapBoth toString
--    (xTip2, yTip2) =
--      (endCol, endRow + 1)
--        |> c2a codeInfo.displayInfo
--        |> Utils.mapBoth toString
--    radiusString =
--      toString radius
--  in
--    Svg.g
--      [ SAttr.fill <| rgbaString color opacity
--      , SAttr.strokeWidth strokeWidth
--      , SAttr.stroke <| rgbaString color opacity
--      ]
--      [ Svg.path
--          [ SAttr.d <|
--              "M " ++ xTip1 ++ " " ++ yTip1 ++ "\n"
--                ++ "l 0 -" ++ radiusString ++ "\n"
--                ++ "a " ++ radiusString ++ " " ++ radiusString
--                  ++ ", 0, 1, 0, -" ++ radiusString
--                  ++ " " ++ radiusString ++ "\n"
--                ++ "Z"
--          ]
--          []
--      , Svg.path
--          [ SAttr.d <|
--              "M " ++ xTip2 ++ " " ++ yTip2 ++ "\n"
--                ++ "l 0 " ++ radiusString ++ "\n"
--                ++ "a " ++ radiusString ++ " " ++ radiusString
--                  ++ ", 0, 1, 0, " ++ radiusString
--                  ++ " -" ++ radiusString ++ "\n"
--                ++ "Z"
--          ]
--          []
--      ]

--------------------------------------------------------------------------------
-- Polygons
--------------------------------------------------------------------------------

-- This polygon should be used for code objects that should not be Deuce-
-- selectable. The purpose of this polygon is to block selection of the parent
-- code object of the unwanted code object. For example, this is very useful
-- with EComments.
blockerPolygon : CodeInfo -> CodeObject -> List (Svg Msg)
blockerPolygon codeInfo codeObject =
  let
    color =
      { r = 255
      , g = 0
      , b = 0
      }
  in
    [ Svg.g
      [ SAttr.opacity "0"
      ]
      [ Svg.polygon
          [ SAttr.points <|
              codeObjectHullPoints codeInfo codeObject
          , SAttr.strokeWidth <|
              strokeWidth codeInfo.displayInfo.colorScheme
          , SAttr.stroke <|
              rgbaString color 1
          , SAttr.fill <|
              rgbaString
                color
                (polygonOpacity codeInfo.displayInfo.colorScheme)
          ]
          []
      ]
    ]

codeObjectPolygon
  : CodeInfo -> CodeObject -> Color -> List (Svg Msg)
codeObjectPolygon codeInfo codeObject color =
  case toDeuceWidget codeInfo.patMap codeObject of
    Nothing ->
      []
    Just deuceWidget ->
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
          let
            activeWidgets =
              Maybe.withDefault
                codeInfo.deuceState.selectedWidgets
                codeInfo.previewDeuceWidgets
          in
            List.member deuceWidget activeWidgets
        baseAlpha =
          if active && hovered then
            1
          else if active then
            1 -- 0.75
          else if hovered then
            1 -- 0.5
          else
            0
        cursorStyle =
          if hovered || active then
            "pointer"
          else
            "default"
      in
        [ Svg.g
            [ SAttr.style << styleListToString <|
                [ ("cursor", cursorStyle)
                ]
            , SE.onMouseOver onMouseOver
            , SE.onMouseOut onMouseOut
            , SE.onClick onClick
            , SAttr.opacity <|
                toString baseAlpha
            ]
            [ circleHandles codeInfo codeObject color 1 3
            , Svg.polygon
                [ SAttr.points <|
                    codeObjectHullPoints codeInfo codeObject
                , SAttr.strokeWidth <|
                    strokeWidth codeInfo.displayInfo.colorScheme
                , SAttr.stroke <|
                    rgbaString color 1
                , SAttr.fill <|
                    rgbaString
                      color
                      (polygonOpacity codeInfo.displayInfo.colorScheme)
                ]
                []
            ]
        ]

expPolygon
  : CodeInfo -> Exp -> List (Svg Msg)
expPolygon codeInfo e =
  let
    codeObject =
      E e
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    case e.val.e__ of
      -- Do not show def polygons (for now, at least)
      ELet _ Def _ _ _ _ _ ->
        []
      _ ->
        codeObjectPolygon codeInfo codeObject color

patPolygon
  : CodeInfo -> Exp -> Pat -> List (Svg Msg)
patPolygon codeInfo e p =
  let
    codeObject =
      P e p
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon codeInfo codeObject color

letBindingEquationPolygon
  : CodeInfo -> (WithInfo EId) -> List (Svg Msg)
letBindingEquationPolygon codeInfo eid =
  let
    codeObject =
      LBE eid
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon codeInfo codeObject color

expTargetPolygon
  : CodeInfo -> BeforeAfter -> WS -> Exp -> List (Svg Msg)
expTargetPolygon codeInfo ba ws et =
  let
    codeObject =
      ET ba ws et
    color =
      whitespaceColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon codeInfo codeObject color

patTargetPolygon
  : CodeInfo -> BeforeAfter -> WS -> Exp -> Pat -> List (Svg Msg)
patTargetPolygon codeInfo ba ws e pt =
  let
    codeObject =
      PT ba ws e pt
    color =
      whitespaceColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon codeInfo codeObject color

polygons : CodeInfo -> Exp -> List (Svg Msg)
polygons codeInfo ast =
  List.reverse <|
    foldCode
      ( \codeObject acc ->
          if isSelectable codeObject then
            case codeObject of
              E e ->
                expPolygon codeInfo e ++ acc
              P e p ->
                patPolygon codeInfo e p ++ acc
              T t ->
                acc
              LBE eid ->
                letBindingEquationPolygon codeInfo eid ++ acc
              ET ba ws et ->
                expTargetPolygon codeInfo ba ws et ++ acc
              PT ba ws e pt ->
                patTargetPolygon codeInfo ba ws e pt ++ acc
              TT _ _ _ ->
                acc
          else
            blockerPolygon codeInfo codeObject ++ acc
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
      , colorScheme =
          model.colorScheme
      }
    (untrimmedLineHulls, trimmedLineHulls, maxLineLength) =
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
      , previewDeuceWidgets =
          Maybe.map (\(_, dws, _) -> dws) model.preview
      , patMap =
          patMap
      , maxLineLength =
          maxLineLength
      }
    leftShift =
      model.codeBoxInfo.contentLeft + SleekLayout.deuceOverlayBleed
  in
    Svg.g
      [ SAttr.transform <|
          "translate(" ++ toString leftShift ++ ", 0)"
      ]
      ( polygons codeInfo ast
      )
