module Deuce exposing (view)

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
-- Polygon Primitive
--------------------------------------------------------------------------------

svgPolygon : Int -> Int -> Int -> Int -> Svg Msg
svgPolygon x y w h =
  Svg.rect
    [ SAttr.x <| toString x
    , SAttr.y <| toString y
    , SAttr.width <| toString w
    , SAttr.height <| toString h
    ]
    []

--------------------------------------------------------------------------------
-- Polygons
--------------------------------------------------------------------------------

polygon : Exp -> Svg Msg
polygon e =
  case e.val.e__ of
    EVar ws i ->
      svgPolygon 100 100 100 100
    _ ->
      svgPolygon 300 300 100 100

polygons : Exp -> (List (Svg Msg))
polygons =
  foldExp
    ( \e acc ->
        polygon e :: acc
    )
    []

--------------------------------------------------------------------------------
-- Exports
--------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
  let
    ast =
      case (parseE model.code) of
        Ok e ->
          e
        Err e ->
          Debug.crash "testing"
  in
    Svg.svg
      []
      (polygons ast)
