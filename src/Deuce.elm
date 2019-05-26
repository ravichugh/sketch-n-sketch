--------------------------------------------------------------------------------
-- This modules provides the Deuce overlay for the View.
--------------------------------------------------------------------------------

module Deuce exposing (Messages, overlay, diffOverlay, c2a)

import DeuceColor exposing (..)
import DeuceParameters exposing (..)
import DeuceGeometry as DG

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

import Model exposing
  ( Model
  , Code
  , ColorScheme(..)
  )

import Info exposing (WithInfo)

import Lang exposing
  ( WS
  , BeforeAfter(..)
  , Exp(..)
  , unExpr
  , ExpBuilder__(..)
  , Pat
  , LetKind(..)
  , EId
  , PId
  , PathedPatternId
  , CodeObject(..)
  , Type
  , ExtraDeuceTypeInfo(..)
  , extractInfoFromCodeObject
  , isTarget
  , foldCode
  , childCodeObjects
  , computePatMap
  , firstNestedExp
  , unwrapExp
  )

import DeuceWidgets exposing
  ( DeuceState
  , DeuceWidget(..)
  , toDeuceWidget
  )

import Layout

--==============================================================================
--= HELPER FUNCTIONS
--==============================================================================

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
        E (Expr e) ->
          let
            (Expr endExp) =
              firstNestedExp <| Expr e
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
--= INFORMATION
--==============================================================================

type alias DisplayInfo =
  { lineHeight : Float
  , characterWidth : Float
  , colorScheme : ColorScheme
  }

type alias CodeInfo =
  { displayInfo : DisplayInfo
  , untrimmedLineHulls : DG.LineHulls
  , trimmedLineHulls : DG.LineHulls
  , mbKeyboardFocusedWidget : Maybe DeuceWidget
  , selectedWidgets : List DeuceWidget
  , patMap : Dict PId PathedPatternId
  , maxLineLength : Int
  , needsParse : Bool
  , doTypeChecking : Bool
  }

--==============================================================================
--= GEOMETRIC PARAMETERS
--==============================================================================

-- CodePos to AbsolutePos
c2a : DisplayInfo -> DG.CodePos -> DG.AbsolutePos
c2a di (cx, cy) =
  ( di.characterWidth * toFloat cx
  , di.lineHeight * toFloat cy
  )

crustSize : DisplayInfo -> Float
crustSize di =
  if di.lineHeight > di.characterWidth * 1.25 then
    -- the expected case
    (di.lineHeight - di.characterWidth) / 2.0
  else
    -- a backup in case the settings are weird
    0.3 * di.lineHeight

bleedAmount : Float
bleedAmount =
  Layout.deuceOverlayBleed

--==============================================================================
--= HULL FUNCTIONS
--==============================================================================

affectedByBleed : CodeObject -> Bool
affectedByBleed =
  isTarget

codeObjectHull : Bool -> CodeInfo -> CodeObject -> DG.Hull
codeObjectHull shouldDecrust codeInfo codeObject =
  let
    di =
      codeInfo.displayInfo
    (startCol, startRow, endCol, endRow) =
      startEnd codeInfo codeObject
    useTrimmed =
      (not << isTarget) codeObject
    lineHulls =
      if useTrimmed then
        codeInfo.trimmedLineHulls
      else
        codeInfo.untrimmedLineHulls
    shouldAddBleed =
      affectedByBleed codeObject
  in
    DG.hull (c2a di) (crustSize di) bleedAmount lineHulls shouldAddBleed shouldDecrust startCol startRow endCol endRow

codeObjectHullPoints : Bool -> CodeInfo -> CodeObject -> String
codeObjectHullPoints shouldDecrust codeInfo codeObject =
  DG.hullPoints <| codeObjectHull shouldDecrust codeInfo codeObject

--==============================================================================
--= POLYGONS
--==============================================================================

-- This polygon should be used for code objects that should not be Deuce-
-- selectable. The purpose of this polygon is to block selection of the parent
-- code object of the unwanted code object.
blockerPolygon : CodeInfo -> CodeObject -> List (Svg msg)
blockerPolygon codeInfo codeObject =
  [ Svg.polygon
      [ SAttr.opacity "0"
      , SAttr.points <|
          codeObjectHullPoints False codeInfo codeObject
      ]
      []
  ]

-- This polygon is used to specify an invisible region around the passed
-- CodeObject which can be hovered over or clicked to select the passed
-- DeuceWidget. This allows for part of a child's polygon to be used to select
-- its parent.
hoverSelectPolygon : Messages msg -> Bool -> CodeInfo -> CodeObject -> DeuceWidget -> List (Svg msg)
hoverSelectPolygon msgs shouldDecrust codeInfo codeObject deuceWidget =
  [ Svg.polygon
      [ SAttr.class "hover-select-polygon"
      , SAttr.opacity "0"
      , SE.onMouseOver <| msgs.onMouseOver deuceWidget
      , SE.onMouseOut <| msgs.onMouseOut deuceWidget
      , SE.onClick <| msgs.onClick deuceWidget
      , SAttr.points <|
          codeObjectHullPoints shouldDecrust codeInfo codeObject
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
        isClickSelected = List.member deuceWidget codeInfo.selectedWidgets
        isKeyboardFocused = codeInfo.mbKeyboardFocusedWidget == Just deuceWidget

        codeObjectHasTypeError =
          case (codeInfo.doTypeChecking, codeInfo.needsParse, codeObject) of
            (True, False, E e) ->
              case (unExpr e).val.typ of
                Nothing -> True
                Just _  -> False
            (True, False, P _ p) ->
              case p.val.typ of
                Nothing -> True
                Just _  -> False
            _ ->
              False

        highlightError =
          codeObjectHasTypeError &&
          codeInfo.selectedWidgets == [] &&
          codeInfo.mbKeyboardFocusedWidget == Nothing

        highlightInfo =
          let
            maybeShow extraDeuceTypeInfo =
              case extraDeuceTypeInfo of
                Just (HighlightWhenSelected eId) ->
                  List.member (DeuceExp eId) codeInfo.selectedWidgets

                _ ->
                  False
          in
          case (codeInfo.doTypeChecking, codeInfo.needsParse, codeObject) of
            (True, False, E e) ->
              maybeShow (unExpr e).val.extraDeuceTypeInfo
            (True, False, P _ p) ->
              maybeShow p.val.extraDeuceTypeInfo
            (True, False, T t) ->
              maybeShow t.val.extraDeuceTypeInfo
            _ ->
              False

        errorColor =
          objectErrorColor codeInfo.displayInfo.colorScheme

        infoColor =
          objectInfoColor codeInfo.displayInfo.colorScheme

        focusedColor =
          keyboardFocusedColor codeInfo.displayInfo.colorScheme

        (classModifier, finalColor)  =
          if (isKeyboardFocused || isClickSelected) && codeObjectHasTypeError then
            (" opaque", errorColor)
          else if isKeyboardFocused then
            (" opaque", focusedColor)
          else if isClickSelected then
            (" opaque", color)
          else if highlightError then
            (" translucent", errorColor)
          else if highlightInfo then
            (" opaque", infoColor)
          else
            ("", color)

        class =
          "code-object-polygon" ++ classModifier

        -- The polygon to render for the current code object
        renderPolygon =
          [ Svg.polygon
              [ SAttr.class class
              , SAttr.points <|
                  codeObjectHullPoints False codeInfo codeObject
              , SAttr.strokeWidth <|
                  strokeWidth codeInfo.displayInfo.colorScheme
              , SAttr.stroke <|
                  rgbaString finalColor 1
              , SAttr.fill <|
                  rgbaString
                    finalColor
                    (polygonOpacity codeInfo.displayInfo.colorScheme)
              ]
              []
          ]

        -- The decrusted polygon for the current code object
        hoverPolygon =
          hoverSelectPolygon msgs True codeInfo codeObject deuceWidget

        -- The polygons (with crust) of the children of the current code object,
        -- which can also be used to select the current code object
        childHoverPolygons =
          codeObject
            |> childCodeObjects
            |> List.concatMap
                 ( \child ->
                     hoverSelectPolygon msgs False codeInfo child deuceWidget
                 )
      in
        [ Svg.g
            []
            ( hoverPolygon ++
              childHoverPolygons ++
              renderPolygon
            )
        ]


diffpolygon: CodeInfo -> Exp -> Svg msg
diffpolygon codeInfo (Expr exp) =
  let di = codeInfo.displayInfo in
  let color = diffColor codeInfo.displayInfo.colorScheme <| Maybe.withDefault "+" <| Lang.eStrUnapply <| Expr exp in
  let thehull = DG.hullPoints <| DG.hull (c2a di) (crustSize di) bleedAmount codeInfo.trimmedLineHulls False False exp.start.col exp.start.line exp.end.col exp.end.line in
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
    case (unwrapExp e) of
      -- Do not show def polygons (for now, at least)
      ELet _ Def _ _ _ ->
        []
      _ ->
        codeObjectPolygon msgs codeInfo codeObject color

patPolygon
  : Messages msg -> CodeInfo -> (Exp, Int) -> Pat -> List (Svg msg)
patPolygon msgs codeInfo e p =
  let
    codeObject =
      P e p
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon msgs codeInfo codeObject color

typePolygon
  : Messages msg -> CodeInfo -> Type -> List (Svg msg)
typePolygon msgs codeInfo t =
  let
    codeObject =
      T t
    color =
      typeColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon msgs codeInfo codeObject color

letBindingEquationPolygon
  : Messages msg -> CodeInfo -> (WithInfo EId) -> Int -> List (Svg msg)
letBindingEquationPolygon msgs codeInfo eid n =
  let
    codeObject =
      D eid n
    color =
      objectColor codeInfo.displayInfo.colorScheme
  in
    codeObjectPolygon msgs codeInfo codeObject color

targetPolygon: Messages msg -> CodeInfo -> CodeObject -> List (Svg msg)
targetPolygon msgs codeInfo codeObject =
  let
    color = whitespaceColor codeInfo.displayInfo.colorScheme
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
--                case unwrapExp e of
--                  EHole _ _ ->
--                    expPolygon msgs codeInfo e ++ acc
--
--                  _ ->
--                    acc
--              _ ->
--                acc
              P e p ->
                patPolygon msgs codeInfo e p ++ acc
              T t ->
                typePolygon msgs codeInfo t ++ acc
              D eid bn ->
                letBindingEquationPolygon msgs codeInfo eid bn ++ acc
              ET ba ws et ->
                targetPolygon msgs codeInfo codeObject ++ acc
              PT ba ws e pt ->
                targetPolygon msgs codeInfo codeObject ++ acc
              DT _ _ _ _ ->
                targetPolygon msgs codeInfo codeObject ++ acc
              TT _ _ _ ->
                targetPolygon msgs codeInfo codeObject ++ acc
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
      DG.lineHulls (c2a displayInfo) model.code
    patMap =
      computePatMap ast
    codeInfo =
      { displayInfo =
          displayInfo
      , untrimmedLineHulls =
          untrimmedLineHulls
      , trimmedLineHulls =
          trimmedLineHulls
      , mbKeyboardFocusedWidget =
          model.deuceState.mbKeyboardFocusedWidget
      , selectedWidgets =
          model.deuceState.selectedWidgets
      , patMap =
          patMap
      , maxLineLength =
          maxLineLength
      , needsParse =
          Model.needsParse model
      , doTypeChecking =
          model.doTypeChecking
      }
    leftShift =
      model.codeBoxInfo.contentLeft + Layout.deuceOverlayBleed
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
      DG.lineHulls (c2a displayInfo) model.code
    codeInfo =
      { displayInfo =
          displayInfo
      , untrimmedLineHulls =
          untrimmedLineHulls
      , trimmedLineHulls =
          trimmedLineHulls
      , mbKeyboardFocusedWidget =
          model.deuceState.mbKeyboardFocusedWidget
      , selectedWidgets =
          model.deuceState.selectedWidgets
      , patMap =
          Dict.empty
      , maxLineLength =
          maxLineLength
      , needsParse =
          Model.needsParse model
      , doTypeChecking =
          model.doTypeChecking
      }
    leftShift =
      model.codeBoxInfo.contentLeft + Layout.deuceOverlayBleed
  in
    Svg.g
      [ SAttr.transform <|
          "translate(" ++ toString leftShift ++ ", 0)"
      ]
      ( diffpolygons codeInfo exps)
