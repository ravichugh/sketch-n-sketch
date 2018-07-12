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

type LinePos = Top | Bottom

type alias CodePosInfo =
  (LinePos, Int, Int)

type alias AbsolutePosInfo =
  (LinePos, Float, Float)

type alias Hull =
  List AbsolutePosInfo

-- CodePosInfo to AbsolutePosInfo
c2a : DisplayInfo -> CodePosInfo -> AbsolutePosInfo
c2a di (lp, cx, cy) =
  ( lp
  , di.characterWidth * toFloat cx
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
    [ (Top, line.startCol, row)
    , (Bottom, line.startCol, row + 1)
    , (Bottom, line.endCol, row + 1)
    , (Top, line.endCol, row)
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

addBleed : AbsolutePosInfo -> AbsolutePosInfo
addBleed (lp, x, y) =
  if x == specialEndFlag then
    ( lp
    , 0
    , y
    )
  else if x <= 0 then
    ( lp
    , -SleekLayout.deuceOverlayBleed
    , y
    )
  else
    (lp, x, y)

addFinalEndBleed : AbsolutePosInfo -> AbsolutePosInfo
addFinalEndBleed (lp, x, y) =
  if x <= 0 then
    ( lp
    , specialEndFlag
    , y
    )
  else
    (lp, x, y)

shorten : DisplayInfo -> AbsolutePosInfo -> AbsolutePosInfo
shorten di (lp, x, y) =
  let margin = 0.3 * di.lineHeight in
  case lp of
    Top -> (lp, x, y + margin)
    Bottom -> (lp, x, y - margin)

theGoodIf : Bool -> a -> (a -> a) -> a
theGoodIf cond default modifier =
  if cond then
    modifier default
  else
    default

-- NOTE: Use 0-indexing for columns and rows.
hull : CodeInfo -> Bool -> Bool -> Bool -> Int -> Int -> Int -> Int -> Hull
hull codeInfo useTrimmed shouldAddBleed shouldShorten startCol startRow endCol endRow =
  let
    lineHulls =
      if useTrimmed then
        codeInfo.trimmedLineHulls
      else
        codeInfo.untrimmedLineHulls
    relevantLines =
      Utils.slice (startRow + 1) endRow lineHulls
    (modifier, finalEndModifier) =
      [
        (shouldAddBleed, addBleed, addFinalEndBleed),
        (shouldShorten, shorten codeInfo.displayInfo, identity)
      ] |> List.foldl
             (\(shouldApply, modifier, final) accum ->
               theGoodIf shouldApply accum (\(mAccum, fAccum) -> (List.map modifier << mAccum, final << fAccum))
             )
             (identity, identity)
  in
    modifier <|
      -- Multi-line
      if startRow /= endRow then
        -- Left of first line
        ( List.map (c2a codeInfo.displayInfo)
            [ (Top, startCol, startRow)
            , (Bottom, startCol, startRow + 1)
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
            [ (Bottom, endCol, endRow + 1)
            , (Top, endCol, endRow)
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
          (_, x, yTop) =
            c2a codeInfo.displayInfo (Top, startCol, startRow)
          (_, _, yBottom) =
            c2a codeInfo.displayInfo (Bottom, startCol, startRow + 1)
        in
          [ (Top, x - zeroWidthPadding, yTop)
          , (Bottom, x - zeroWidthPadding, yBottom)
          , (Bottom, x + zeroWidthPadding, yBottom)
          , (Top, x + zeroWidthPadding, yTop)
          ]
      -- Single-line, nonzero-width
      else
        List.map (c2a codeInfo.displayInfo)
          [ (Top, startCol, startRow)
          , (Bottom, startCol, startRow + 1)
          , (Bottom, endCol, startRow + 1)
          , (Top, endCol, startRow)
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
    shouldShorten = isTarget codeObject && startRow == endRow && startCol /= endCol
  in
    hull codeInfo useTrimmed shouldAddBleed shouldShorten startCol startRow endCol endRow

hullPoints : Hull -> String
hullPoints =
  let
    posInfoToString (lp, x, y) =
      (toString x) ++ "," ++ (toString y) ++ " "
  in
    String.concat << List.map posInfoToString

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
    accountForBleed mightBleed (lp, x, y) =
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
    handle mightBleed lp col row =
      let
        (cx, cy) =
          (lp, col, row)
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
      [ handle True Top startCol startRow
      , handle False Bottom endCol (endRow + 1)
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
        [ Svg.g
            [ SAttr.class class
            , SE.onMouseOver onMouseOver
            , SE.onMouseOut onMouseOut
            , SE.onClick onClick
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
      ELet _ Def _ _ _ ->
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
  : Messages msg -> CodeInfo -> (WithInfo EId) -> Int -> List (Svg msg)
letBindingEquationPolygon msgs codeInfo eid n =
  let
    codeObject =
      LBE eid n
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
          --if isSelectable codeObject then
            case codeObject of
              E e ->
                expPolygon msgs codeInfo e ++ acc
              P e p ->
                patPolygon msgs codeInfo e p ++ acc
              T t ->
                acc
              LBE eid bn ->
                letBindingEquationPolygon msgs codeInfo eid bn ++ acc
              ET ba ws et ->
                expTargetPolygon msgs codeInfo ba ws et ++ acc
              PT ba ws e pt ->
                patTargetPolygon msgs codeInfo ba ws e pt ++ acc
              TT _ _ _ ->
                acc
              LXT _ _ _ _ ->
                --TODO: Create a polygon for let exp targets.
                acc
          --else
          --  blockerPolygon codeInfo codeObject ++ acc
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
