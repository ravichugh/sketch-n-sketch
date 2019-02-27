module UnDeuce exposing
  ( overlay
  )

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events as SE

import Model exposing (ColorScheme(..))
import DeuceColor
import DeuceParameters

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
      7.19

    yScale =
      15.0

    info =
      UnExp.getData u

    xStart =
      (toFloat <| info.start.col - 1) * xScale

    yStart =
      (toFloat <| info.start.line - 1) * yScale

    xEnd =
      (toFloat <| info.end.col - 1) * xScale

    yEnd =
      (toFloat info.end.line) * yScale
  in
    Svg.rect
      [ SAttr.class "deuce-unexp-poly"

      , SAttr.x <| toString xStart
      , SAttr.y <| toString yStart
      , SAttr.width <| toString (xEnd - xStart)
      , SAttr.height <| toString (yEnd - yStart)

      , SE.onClick <| handlers.onClick u

      , SAttr.strokeWidth <|
          DeuceParameters.strokeWidth Light
      , SAttr.stroke <|
          DeuceColor.rgbaString (DeuceParameters.objectColor Light) 1
      , SAttr.fill <|
          DeuceColor.rgbaString
            (DeuceParameters.objectColor Light)
            (DeuceParameters.polygonOpacity Light)
      ]
      []

overlay : Handlers msg -> UnExp (WithInfo String) -> List (Html msg)
overlay handlers =
  UnExp.flatten >> List.map (polygon handlers)
