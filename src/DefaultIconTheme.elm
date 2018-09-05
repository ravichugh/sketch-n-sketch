module DefaultIconTheme exposing (..)

import Dict

icons =
  Dict.fromList
    [ ("cursor", cursor)
    , ("pointOrOffset", pointOrOffset)
    , ("text", text)
    , ("line", line)
    , ("rect", rect)
    , ("ellipse", ellipse)
    , ("polygon", polygon)
    , ("path", path)
    , ("lambda", lambda)
    ]

-- TODO switch these to Elm syntax
-- TODO maybe build them into ExamplesTemplate from standalone examples/ files

cursor = """
; To customize, Save As: __ui__cursor.little

(def polygon1
  (let pts_k4913 191
  (let [pts_k4917 pts_k4911] [114 73]
  (let k4926 (* 0.5! (+ pts_k4917 pts_k4911))
  (let pts_k4922 124
  (let pts_k4910 113
  (let pts [[30 pts_k4922] [k4926 8] [157 pts_k4922] [pts_k4917 pts_k4910] [pts_k4917 pts_k4913] [pts_k4911 pts_k4913] [pts_k4911 pts_k4910]]
  (let [color strokeColor strokeWidth] [365 470 7]
    [ (rawPolygon color strokeColor strokeWidth pts -28.996918547567233) ]))))))))

(svgViewBox 200 200 (concat [
  polygon1
]))
"""

pointOrOffset = """
; To customize, Save As: __ui__pointOrOffset.little

(def canvasSize 38)
(def arrowHeadOffset 9)
(def [x1 y1] [6 (- (- canvasSize arrowHeadOffset) 2)])
(def x2 (- canvasSize 1))
(def x11 (- x2 arrowHeadOffset))

(def line1
  (let [x1 y1 x2 y2] [x1 y1 x2 y1]
  (let [color width] ['black' 1]
    [ (addAttr (line color width x1 y1 x2 y2) ["stroke-dasharray" "1,1"]) ])))

(def line2
  (let [x1 y1 x2 y2] [x11 (+ y1 arrowHeadOffset) x2 y1]
  (let [color width] ['black' 1]
    [ (addAttr (line color width x1 y1 x2 y2) ["stroke-dasharray" "1,1"]) ])))

(def line3
  (let [x1 y1 x2 y2] [x11 (- y1 arrowHeadOffset) x2 y1]
  (let [color width] ['black' 1]
    [ (addAttr (line color width x1 y1 x2 y2) ["stroke-dasharray" "1,1"]) ])))

(def circle1
  (let [cx cy r] [19 8 5]
  (let fill 'white'
    [ (rawCircle fill 360 2 cx cy r) ])))

(def circle2
  (let [cx cy r] [x1 y1 5]
  (let fill 'white'
    [ (rawCircle fill 360 2 cx cy r) ])))

(svgViewBox canvasSize canvasSize (concat [
  circle1
  circle2
  line1
  line2
  line3
]))
"""

line = """
; To customize, Save As: __ui__line.little

(def line1
  (let [x1 y1 x2 y2] [25 25 175 175]
  (let [color width] [379 7]
    [ (line color width x1 y1 x2 y2) ])))

(svgViewBox 200 200 (concat [
  line1
]))
"""

rect = """
; To customize, Save As: __ui__rect.little

(def rect1
  (let [x y w h] [25 25 150 150]
  (let [fill stroke strokeWidth] [365 470 7]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x y w h rot) ]))))

(svgViewBox 200 200 (concat [
  rect1
]))
"""

ellipse = """
; To customize, Save As: __ui__ellipse.little

(def ellipse1
  (let bounds @ [left top right bot] [25 25 175 175]
  (let [color strokeColor strokeWidth] [365 470 7]
    [ (oval color strokeColor strokeWidth bounds) ])))

(svgViewBox 200 200 (concat [
  ellipse1
]))
"""

polygon = """
; To customize, Save As: __ui__polygon.little

(def star [(nStar 365 470 7 5 45 90 3.1415 100 110)])

(svgViewBox 200 200
  star
)
"""

path = """
; To customize, Save As: __ui__path.little

(def svgViewBox2 (\\(xMin yMin xMax yMax shapes)
  (let [smx smy sx sy] [(toString xMin) (toString yMin) (toString xMax) (toString yMax)]
    ['svg'
      [['x' '0'] ['y' '0'] ['viewBox' (joinStrings ' ' [smx smy sx sy])]]
      shapes])))

(def [rect5_h rect4_y] [131 199])
(def [rect5_w polygon1_pts_k4931] [12 139])
(def polygon1_pts_k4924 209.5)
(def rect3_w 91)
(def rect2_w 105)
(def rect1_w 129)
(def polygon1
  (let pts_k4930 283
  (let pts_k4927 256
  (let pts_k4925 (+ polygon1_pts_k4931 -35!)
  (let pts_k4913 (+ polygon1_pts_k4931 70!)
  (let pts_k4919 (+ pts_k4913 (- polygon1_pts_k4931 pts_k4925))
  (let k4922 (* 0.5! (+ pts_k4913 polygon1_pts_k4931))
  (let pts [[polygon1_pts_k4931 pts_k4930] [polygon1_pts_k4931 pts_k4927] [pts_k4925 polygon1_pts_k4924] [k4922 78] [pts_k4919 polygon1_pts_k4924] [pts_k4913 pts_k4927] [pts_k4913 pts_k4930]]
  (let [color strokeColor strokeWidth] [499 365 7]
    [ (rawPolygon color strokeColor strokeWidth pts 0) ])))))))))
(def [rect1_h rect1_copy16_y rect1_copy16_h rect2_y] [77 279 17 281])
(def rect2_h (- (+ (+ rect1_copy16_y rect1_copy16_h) rect1_h) rect2_y))

(def rect2
  (let x (- (* 0.5! (+ (+ polygon1_pts_k4931 70!) polygon1_pts_k4931)) (* 0.5! rect2_w))
  (let [fill stroke strokeWidth] [365 216 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x rect2_y rect2_w rect2_h rot) ]))))

(def rect3
  (let x (- (* 0.5! (+ (+ polygon1_pts_k4931 70!) polygon1_pts_k4931)) (* 0.5! rect3_w))
  (let [fill stroke strokeWidth] [408 418 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x rect2_y rect3_w rect2_h rot) ]))))

(def rect1_copy16
  (let x (- (* 0.5! (+ (+ polygon1_pts_k4931 70!) polygon1_pts_k4931)) (* 0.5! rect1_w))
  (let [fill stroke strokeWidth] [365 0 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x rect1_copy16_y rect1_w rect1_copy16_h rot) ]))))

(def rect1
  (let [x y w] [138 (+ rect1_copy16_y rect1_copy16_h) 15]
  (let [fill stroke strokeWidth] [499 410 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x y w rect1_h rot) ]))))

(def rect4
  (let h (* 2! (- polygon1_pts_k4924 rect4_y))
  (let x (+ (* 0.5! (+ (+ polygon1_pts_k4931 70!) polygon1_pts_k4931)) (- rect4_y polygon1_pts_k4924))
  (let [fill stroke strokeWidth] [365 38 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x rect4_y h h rot) ])))))

(def rect5
  (let [x y] [(- (* 0.5! (+ (+ polygon1_pts_k4931 70!) polygon1_pts_k4931)) (* 0.5! rect5_w)) (- rect4_y rect5_h)]
  (let [fill stroke strokeWidth] [365 266 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x y rect5_w rect5_h rot) ]))))

(svgViewBox2 60 50 250 350 (concat [
  polygon1
  rect2
  rect3
  rect1_copy16
  rect1
  rect4
  rect5]))
"""

lambda = """
; To customize, Save As: __ui__lambda.little

(let [x0 y0 w h delta] [10 10 180 180 10]
(let [xw yh w2 h2] [(+ x0 w) (+ y0 h) (div w 2) (div h 2)]
(let poly (\\pts (polygon 'black' 'none' 0 pts))
(svgViewBox 200 200 [
  (rect 'white' x0 y0 w h)
  (poly
    [[(+ x0 delta) y0]
     [xw y0]
     [xw (- yh delta)]])
  (poly
    [[x0 (+ y0 delta)]
     [x0 (- yh delta)]
     [(- (+ x0 w2) delta) (+ y0 h2)]])
  (poly
    [[(+ x0 delta) yh]
     [(- xw delta) yh]
     [(+ x0 w2) (+ (+ y0 h2) delta)]])
]))))
"""

text = """
; To customize, Save As: __ui__text.little

(def [line2_num line2_x1] [156 41])
(def line1_y1 36)
(def line1
  (let x1 (+ (/ line2_num 2!) (/ line2_x1 2!))
  (let [x1 y2] [x1 165]
  (let [color width] [379 15]
    [ (line color width x1 line1_y1 x1 y2) ]))))

(def line2
  (let [color width] [379 15]
    [ (line color width line2_x1 line1_y1 line2_num line1_y1) ]))

(svgViewBox 200 200 (concat [
  line1
  line2
]))
"""
