module UnDeuce exposing
  ( overlay
  )

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SE

import UnExp exposing (UnExp)

import Info exposing (WithInfo)

type alias Handlers msg =
  { onClick : UnExp (WithInfo String) -> msg
  , onMouseOver : UnExp (WithInfo String) -> msg
  , onMouseOut : UnExp (WithInfo String) -> msg
  }

polygon : Handlers msg -> UnExp (WithInfo String) -> Svg msg
polygon handlers u =
  let
    xScale =
      7

    yScale =
      15

    info =
      UnExp.getData u

    xStart =
      (info.start.col - 1) * xScale

    yStart =
      (info.start.line - 1) * yScale + 3

    xEnd =
      (info.end.col - 1) * xScale

    yEnd =
      info.end.line * yScale + 3
  in
    Svg.rect
      [ SAttr.class "deuce-unexp-poly"
      , SAttr.fill "teal"
      , SAttr.x <| toString xStart
      , SAttr.y <| toString yStart
      , SAttr.width <| toString (xEnd - xStart)
      , SAttr.height <| toString (yEnd - yStart)
      , SE.onClick <| handlers.onClick u
      ]
      []

overlay : Handlers msg -> UnExp (WithInfo String) -> List (Html msg)
overlay handlers =
  UnExp.flatten >> List.map (polygon handlers)
