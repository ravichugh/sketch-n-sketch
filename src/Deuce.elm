--------------------------------------------------------------------------------
-- This modules provides the Deuce overlay for the View.
--------------------------------------------------------------------------------

module Deuce exposing (Messages, overlay, diffOverlay)

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
  , Code
  , ColorScheme(..)
  )

import Info exposing (WithInfo)

import Lang exposing
  ( WS
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
  , selectedWidgets : List DeuceWidget
  , hoveredWidgets : List DeuceWidget
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

needsDecrusting : CodeInfo -> CodeObject -> Bool
needsDecrusting codeInfo codeObject =
  let
    (startCol, _, endCol, _) =
      startEnd codeInfo codeObject
  in
  isTarget codeObject && startCol /= endCol

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

crustSize : DisplayInfo -> Float
crustSize di =
  if di.lineHeight > di.characterWidth * 1.25 then
    -- the expected case
    (di.lineHeight - di.characterWidth) / 2.0
  else
    -- a backup in case the settings are weird
    0.3 * di.lineHeight

removeUpperCrust : DisplayInfo -> AbsolutePos -> AbsolutePos
removeUpperCrust di (x, y) =
  (x, y + crustSize di)

removeLowerCrust : DisplayInfo -> AbsolutePos -> AbsolutePos
removeLowerCrust di (x, y) =
  (x, y - crustSize di)

maybeDecrust :
  DisplayInfo -> Bool -> List (DisplayInfo -> AbsolutePos -> AbsolutePos) -> Hull -> Hull
maybeDecrust di shouldDecrust decrusters =
  Utils.applyIf shouldDecrust <|
    List.map2 (\dc pos -> dc di pos) decrusters

magicDecrust :
  DisplayInfo -> Bool -> List ((DisplayInfo -> AbsolutePos -> AbsolutePos), Int, Int) -> Hull
magicDecrust di shouldDecrust posInfos =
  let modifier =
    (\(dc, col, row) ->
      Utils.applyIf shouldDecrust
        (dc di)
        (c2a di (col, row))
    )
  in
  List.map modifier posInfos

-- NOTE: Use 0-indexing for columns and rows.
hull : CodeInfo -> Bool -> Bool -> Bool -> Int -> Int -> Int -> Int -> Hull
hull codeInfo useTrimmed shouldAddBleed shouldDecrust startCol startRow endCol endRow =
  let
    di =
      codeInfo.displayInfo
    lineHulls =
      if useTrimmed then
        codeInfo.trimmedLineHulls
      else
        codeInfo.untrimmedLineHulls
    relevantLines =
      Utils.slice (startRow + 1) endRow lineHulls
    modifier = if shouldAddBleed then List.map addBleed else identity
  in
    modifier <|
      -- Multi-line
      if startRow /= endRow then
        -- Left of first line
        ( magicDecrust di shouldDecrust
            [ (removeUpperCrust, startCol, startRow)
            , (always identity, startCol, startRow + 1)
            ]
        ) ++

        -- Left of middle lines
        ( List.concat <|
            List.map (List.take 2)
              relevantLines
        ) ++

        -- Left of last line
        ( maybeDecrust di shouldDecrust [always identity, removeLowerCrust] <|
            List.take 2 <|
              Maybe.withDefault [] <|
                Utils.maybeGeti0 endRow lineHulls
        ) ++

        -- Right of last line
        (
          magicDecrust di shouldDecrust
            [ (removeLowerCrust, endCol, endRow + 1)
            , (always identity, endCol, endRow)
            ] |>
              Utils.applyIf shouldAddBleed (List.map addFinalEndBleed)
        ) ++

        -- Right of middle lines
        ( List.concat <|
            List.map (List.drop 2) <|
              List.reverse relevantLines
        ) ++

        -- Right of first line
        ( maybeDecrust di shouldDecrust [always identity, removeUpperCrust] <|
            List.drop 2 <|
              Maybe.withDefault [] <|
                Utils.maybeGeti0 startRow lineHulls
        )
      -- Zero-width
      else if startCol == endCol then
        let
          (x, yTop) =
            c2a di (startCol, startRow)
          (_, yBottom) =
            c2a di (startCol, startRow + 1)
        in
          [ (x - zeroWidthPadding, yTop)
          , (x - zeroWidthPadding, yBottom)
          , (x + zeroWidthPadding, yBottom)
          , (x + zeroWidthPadding, yTop)
          ] |>
            maybeDecrust di shouldDecrust
              [ removeUpperCrust
              , removeLowerCrust
              , removeLowerCrust
              , removeUpperCrust
              ]
      -- Single-line, nonzero-width
      else
        magicDecrust di shouldDecrust
          [ (removeUpperCrust, startCol, startRow)
          , (removeLowerCrust, startCol, startRow + 1)
          , (removeLowerCrust, endCol, startRow + 1)
          , (removeUpperCrust, endCol, startRow)
          ]

codeObjectHull : CodeInfo -> CodeObject -> Hull
codeObjectHull codeInfo codeObject =
  let
    (startCol, startRow, endCol, endRow) =
      startEnd codeInfo codeObject
    useTrimmed =
      (not << isTarget) codeObject
    shouldAddBleed =
      affectedByBleed codeObject
    shouldDecrust =
      needsDecrusting codeInfo codeObject
  in
    hull codeInfo useTrimmed shouldAddBleed shouldDecrust startCol startRow endCol endRow

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

diffColor : ColorScheme -> String -> Color
diffColor colorScheme tag =
  case colorScheme of
    Light ->
      case tag of
        "+" -> { r = 0, g = 255, b = 0}
        "-" -> { r = 255, g = 0, b = 0}
        _ ->   { r = 255, g = 165, b = 0}
    Dark ->
      case tag of
        "+" -> { r = 0, g = 200, b = 0}
        "-" -> { r = 200, g = 0, b = 0}
        _ ->   { r = 200, g = 200, b = 100}

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
  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg msg
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
    decrustAsNecessary decruster absPos =
      Utils.applyIf (needsDecrusting codeInfo codeObject)
        (decruster codeInfo.displayInfo)
        absPos
    handle mightBleed decruster col row =
      let
        (cx, cy) =
          (col, row)
            |> c2a codeInfo.displayInfo
            |> accountForBleed mightBleed
            |> decrustAsNecessary decruster
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
      [ handle True removeUpperCrust startCol startRow
      , handle False removeLowerCrust endCol (endRow + 1)
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
-- code object of the unwanted code object.
blockerPolygon : CodeInfo -> CodeObject -> List (Svg msg)
blockerPolygon codeInfo codeObject =
  [ Svg.polygon
      [ SAttr.opacity "0"
      , SAttr.points <|
          codeObjectHullPoints codeInfo codeObject
      ]
      []
  ]

codeObjectPolygon
  : Messages msg -> CodeInfo -> CodeObject -> Color -> List (Svg msg)
codeObjectPolygon msgs codeInfo codeObject color =
  case toDeuceWidget codeInfo.patMap codeObject of
    Nothing ->
      []
    Just deuceWidget ->
      let
        onMouseOver =
          msgs.onMouseOver deuceWidget
        onMouseOut =
          msgs.onMouseOut deuceWidget
        onClick =
          msgs.onClick deuceWidget
        selected =
          List.member deuceWidget codeInfo.selectedWidgets
        selectedClass =
          if selected then
            " selected"
          else
            ""
        class =
          "code-object-polygon" ++ selectedClass
      in
        [ circleHandles codeInfo codeObject color 1 3
        , Svg.polygon
            [ SAttr.class class
            , SE.onMouseOver onMouseOver
            , SE.onMouseOut onMouseOut
            , SE.onClick onClick
            , SAttr.points <|
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


diffpolygon: CodeInfo -> Exp -> Svg msg
diffpolygon codeInfo exp =
  let color = diffColor codeInfo.displayInfo.colorScheme <| Maybe.withDefault "+" <| Lang.eStrUnapply exp in
  let thehull = hullPoints <| hull codeInfo True False False exp.start.col exp.start.line exp.end.col exp.end.line in
    Svg.polygon
        [ SAttr.points thehull
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

expPolygon
  : Messages msg -> CodeInfo -> Exp -> List (Svg msg)
expPolygon msgs codeInfo e =
  let
    codeObject =
      E e
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    case e.val.e__ of
      -- Do not show def polygons (for now, at least)
      ELet _ Def _ _ _ _ _ _ _ ->
        []
      _ ->
        codeObjectPolygon msgs codeInfo codeObject color

patPolygon
  : Messages msg -> CodeInfo -> Exp -> Pat -> List (Svg msg)
patPolygon msgs codeInfo e p =
  let
    codeObject =
      P e p
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon msgs codeInfo codeObject color

letBindingEquationPolygon
  : Messages msg -> CodeInfo -> (WithInfo EId) -> List (Svg msg)
letBindingEquationPolygon msgs codeInfo eid =
  let
    codeObject =
      LBE eid
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon msgs codeInfo codeObject color

expTargetPolygon
  : Messages msg -> CodeInfo -> BeforeAfter -> WS -> Exp -> List (Svg msg)
expTargetPolygon msgs codeInfo ba ws et =
  let
    codeObject =
      ET ba ws et
    color =
      whitespaceColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon msgs codeInfo codeObject color

patTargetPolygon
  : Messages msg -> CodeInfo -> BeforeAfter -> WS -> Exp -> Pat -> List (Svg msg)
patTargetPolygon msgs codeInfo ba ws e pt =
  let
    codeObject =
      PT ba ws e pt
    color =
      whitespaceColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon msgs codeInfo codeObject color

diffpolygons: CodeInfo -> List Exp -> List (Svg msg)
diffpolygons codeInfo exps =
  List.map (diffpolygon codeInfo) exps

polygons : Messages msg -> CodeInfo -> Exp -> List (Svg msg)
polygons msgs codeInfo ast =
  List.reverse <|
    foldCode
      ( \codeObject acc ->
          if isSelectable codeObject then
            case codeObject of
              E e ->
                expPolygon msgs codeInfo e ++ acc
              P e p ->
                patPolygon msgs codeInfo e p ++ acc
              T t ->
                acc
              LBE eid ->
                letBindingEquationPolygon msgs codeInfo eid ++ acc
              ET ba ws et ->
                expTargetPolygon msgs codeInfo ba ws et ++ acc
              PT ba ws e pt ->
                patTargetPolygon msgs codeInfo ba ws e pt ++ acc
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

type alias Messages msg =
  { onMouseOver : DeuceWidget -> msg
  , onMouseOut : DeuceWidget -> msg
  , onClick : DeuceWidget -> msg
  }

overlay : Messages msg -> Model -> Svg msg
overlay msgs model =
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
      , selectedWidgets =
          model.deuceState.selectedWidgets
      , hoveredWidgets =
          model.deuceState.hoveredWidgets
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
      ( polygons msgs codeInfo ast
      )

diffOverlay : Model -> List Exp -> Svg msg
diffOverlay model exps =
  let
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
    codeInfo =
      { displayInfo =
          displayInfo
      , untrimmedLineHulls =
          untrimmedLineHulls
      , trimmedLineHulls =
          trimmedLineHulls
      , selectedWidgets =
          model.deuceState.selectedWidgets
      , hoveredWidgets =
          model.deuceState.hoveredWidgets
      , patMap =
          Dict.empty
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
      ( diffpolygons codeInfo exps)
