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
  )

import InterfaceController as Controller

import Lang exposing
  ( WithInfo
  , WS
  , BeforeAfter
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
startEnd : CodeObject -> (Int, Int, Int, Int)
startEnd codeObject =
  let
    infoToTuple info =
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
          infoToTuple << extractInfoFromCodeObject <| codeObject
  in
    ( startCol - 1
    , startLine - 1
    , endCol - 1
    , endLine - 1
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

untrimmedLine : List String -> List Line
untrimmedLine strings =
  let
    lens =
      List.map String.length strings
    maxLen =
      Maybe.withDefault 0 (List.maximum lens)
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
        |> lineKind
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
      Utils.slice (startRow + 1) endRow lineHulls
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
            Utils.maybeGeti0 endRow lineHulls
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
            Utils.maybeGeti0 startRow lineHulls
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
      startEnd codeObject
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
-- The following functions are a couple different options for different handle
-- styles.
--------------------------------------------------------------------------------

circleHandles
  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
circleHandles codeInfo codeObject color opacity radius =
  let
    radiusString =
      toString radius
    handle col row =
      let
        (cx, cy) =
          (col, row)
            |> c2a codeInfo.displayInfo
            |> Utils.mapBoth toString
      in
        Svg.circle
          [ SAttr.cx cx
          , SAttr.cy cy
          , SAttr.r radiusString
          ]
          []
    (startCol, startRow, endCol, endRow) =
      startEnd codeObject
  in
    Svg.g
      [ SAttr.fill <| rgbaString color opacity
      , SAttr.strokeWidth strokeWidth
      , SAttr.stroke <| rgbaString color opacity
      ]
      [ handle startCol startRow
      , handle startCol (startRow + 1)
      , handle endCol endRow
      , handle endCol (endRow + 1)
      ]

oldCircleHandles
  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
oldCircleHandles codeInfo codeObject color opacity radius =
  let
    (startCol, startRow, endCol, endRow) =
      startEnd codeObject
    (cx1, cy1) =
      (startCol, startRow)
        |> c2a codeInfo.displayInfo
        |> \(x, y) -> (x, y - radius)
        |> Utils.mapBoth toString
    (cx2, cy2) =
      (endCol, endRow + 1)
        |> c2a codeInfo.displayInfo
        |> \(x, y) -> (x, y + radius)
        |> Utils.mapBoth toString
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

fancyHandles
  : CodeInfo -> CodeObject -> Color -> Opacity -> Float -> Svg Msg
fancyHandles codeInfo codeObject color opacity radius =
  let
    (startCol, startRow, endCol, endRow) =
      startEnd codeObject
    (xTip1, yTip1) =
      (startCol, startRow)
        |> c2a codeInfo.displayInfo
        |> Utils.mapBoth toString
    (xTip2, yTip2) =
      (endCol, endRow + 1)
        |> c2a codeInfo.displayInfo
        |> Utils.mapBoth toString
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

objectColor : Color
objectColor =
  { r = 200
  , g = 200
  , b = 100
  }

whitespaceColor : Color
whitespaceColor =
  { r = 0
  , g = 200
  , b = 200
  }

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
          List.member deuceWidget codeInfo.deuceState.selectedWidgets
        baseAlpha =
          if active && hovered then
            1
          else if active then
            1 -- 0.75
          else if hovered then
            1 -- 0.5
          else
            0
        (cursorStyle) =
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
            , SAttr.opacity <| toString baseAlpha
            ]
            [ circleHandles codeInfo codeObject color 1 3
            , Svg.polygon
                [ SAttr.points <|
                    codeObjectHullPoints codeInfo codeObject
                , SAttr.strokeWidth strokeWidth
                , SAttr.stroke <| rgbaString color 1
                , SAttr.fill <| rgbaString color 0.2
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
      objectColor
  in
    codeObjectPolygon codeInfo codeObject color

patPolygon
  : CodeInfo -> Exp -> Pat -> List (Svg Msg)
patPolygon codeInfo e p =
  let
    codeObject =
      P e p
    color =
      objectColor
  in
    codeObjectPolygon codeInfo codeObject color

letBindingEquationPolygon
  : CodeInfo -> (WithInfo EId) -> List (Svg Msg)
letBindingEquationPolygon codeInfo eid =
  let
    codeObject =
      LBE eid
    color =
      objectColor
  in
    codeObjectPolygon codeInfo codeObject color

expTargetPolygon
  : CodeInfo -> BeforeAfter -> WS -> Exp -> List (Svg Msg)
expTargetPolygon codeInfo ba ws et =
  let
    codeObject =
      ET ba ws et
    color =
      whitespaceColor
  in
    codeObjectPolygon codeInfo codeObject color

patTargetPolygon
  : CodeInfo -> BeforeAfter -> WS -> Exp -> Pat -> List (Svg Msg)
patTargetPolygon codeInfo ba ws e pt =
  let
    codeObject =
      PT ba ws e pt
    color =
      whitespaceColor
  in
    codeObjectPolygon codeInfo codeObject color

polygons : CodeInfo -> Exp -> List (Svg Msg)
polygons codeInfo ast =
  List.reverse <|
    foldCode
      ( \codeObject acc ->
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
