module Deuce exposing (svgElements)

import List
import String

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr

import InterfaceModel as Model exposing
  ( Model
  , Msg(..)
  )

import Lang exposing (..)

import FastParser exposing (parseE)

--------------------------------------------------------------------------------
-- Hulls
--------------------------------------------------------------------------------

type alias DisplayInfo =
  { lineHeight : Float
  , characterWidth : Float
  }

type alias CodePos =
  (Int, Int)

type alias AbsolutePos =
  (Float, Float)

type alias Hull =
  List (Float, Float)

codeToAbsolute : DisplayInfo -> CodePos -> AbsolutePos
codeToAbsolute di (cx, cy) =
  ( di.characterWidth * toFloat cx
  , di.lineHeight * toFloat cy
  )

hull : DisplayInfo -> Exp -> Hull
hull di e =
  let
    smallX =
      (min e.start.col e.end.col) - 1
    smallY =
      (min e.start.line e.end.line) - 1
    bigX =
      (max e.start.col e.end.col) - 1
    bigY =
      max e.start.line e.end.line
    codeHull =
      [ (smallX, smallY)
      , (smallX, bigY)
      , (bigX, bigY)
      , (bigX, smallY)
      ]
  in
    List.map (codeToAbsolute di) codeHull

hullPoints : Hull -> String
hullPoints =
  let
    pairToString (x, y) =
      (toString x) ++ "," ++ (toString y) ++ " "
  in
    String.concat << List.map pairToString

boundingHullPoints : DisplayInfo -> Exp -> String
boundingHullPoints di e =
  hullPoints <| hull di e

--------------------------------------------------------------------------------
-- Polygons
--------------------------------------------------------------------------------

polygon : DisplayInfo -> Exp -> Svg Msg
polygon di e =
  Svg.polygon
    [ SAttr.points <| boundingHullPoints di e
    , SAttr.fill "rgba(0,0,0,0)"
    , SAttr.strokeWidth "3px"
    , SAttr.stroke "#00c6ff"
    ]
    []

polygons : DisplayInfo -> Exp -> (List (Svg Msg))
polygons di =
  foldExp
    ( \e acc ->
        polygon di e :: acc
    )
    []

--------------------------------------------------------------------------------
-- Exports
--------------------------------------------------------------------------------

svgElements : Model -> (List (Svg Msg))
svgElements model =
  let
    ast =
      model.inputExp
      -- case (parseE model.code) of
      --   Ok e ->
      --     e
      --   Err e ->
      --     Debug.crash "testing"
    displayInfo =
      { lineHeight =
          model.codeBoxInfo.lineHeight
      , characterWidth =
          model.codeBoxInfo.characterWidth
      }
  in
    polygons displayInfo ast
