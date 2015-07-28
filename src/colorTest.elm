module Main where

import Graphics.Collage as GC
import Graphics.Element as GE
import Color exposing (rgb)
import Utils exposing (numToColor_)

width = 20

main : GE.Element
main = GC.collage 600 600
    <| List.append
        (List.map (\deg ->
            let (r,g,b) = numToColor_ deg
                color = rgb r g b
            in GC.move (-250 + 500 * (1 - (360 - toFloat deg) / 360), 200)
                 <| GC.filled color
                 <| GC.square width) [0..360])
        (List.map (\deg ->
            let (r,g,b) = numToColor_ deg
                color = rgb r g b
                rad = toFloat deg * pi / 180
            in
               GC.move (150 * cos rad, 150 * sin rad)
               <| GC.filled color
               <| GC.square width) [0..360])
