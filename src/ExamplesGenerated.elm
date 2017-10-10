module ExamplesGenerated exposing (list, templateCategories)

import Lang
import FastParser exposing (parseE)
import Types
import Eval
import Utils
import PreludeGenerated as Prelude
import LangSvg
import DefaultIconTheme

makeExample name s =
  let thunk () =
    -- TODO tolerate parse errors, change Select Example
    let e = Utils.fromOkay ("Error parsing example " ++ name) (parseE s) in
    -- let ati = Types.typecheck e in
    let ati = Types.dummyAceTypeInfo in
    -----------------------------------------------------
    -- if name == "*Prelude*" then
    --   {e=e, v=LangSvg.dummySvgVal, ws=[], ati=ati}
    -- else
    -----------------------------------------------------
    let (v,ws) = Utils.fromOk ("Error executing example " ++ name) <| Eval.run e in
    {e=e, v=v, ws=ws, ati=ati}
  in
  (name, (s, thunk))

threeBoxes =
 """
(def threeBoxesInt
  (let [x0 y0 w h sep] [40 28 60 130 110]
  (let boxi (\\i
    (let xi (+ x0 (mult i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi [0 1 2])))))
 
threeBoxesInt

"""

nBoxesH2 =
 """
(def [a b c] [0 0 0])
(def something (+ a (+ a (+ b (+ b (+ c c))))))

(def nBoxes
  (let [n x0 y0 w h sep] [3 40 28 60 130 110]
  (let boxi (\\i
    (let xi (+ (+ x0 something) (mult i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi (zeroTo n))))))
 
nBoxes

"""

sineWaveOfBoxes =
 """(def [x0 y0 w h sep amp] [50 120 20 90 30 60])
(def n 12!{3-30})
(def boxi (\\i
   (let xi (+ x0 (* i sep))
   (let yi (- y0 (* amp (sin (* i (/ twoPi n)))))
     (rect 'lightblue' xi yi w h)))))

(svg (map boxi (zeroTo n)))

"""

sineWaveGrid =
 """; WORKAROUND: Switch to [Heuristics] None before using sliders.
; (Better yet, toggle Heuristics before selecting this example.)
; Both heuristics modes use a naive algorithm to compute triggers,
; which runs very slowly with the number of shapes in this canvas.

  ; fix case bug:
  ; > import Eval exposing (parseAndRun)
  ; > parseAndRun \"(case [2 false] ([2 true] 'A') (_ 'B'))\"
  ; \"'A'\" : String

  ; (let color (case [i shadow]
  ;   ([2 false] 'crimson')   ([2 true] 'gray')
  ;   ([_ false] 'lightblue') ([_ true] 'transparent'))

(def sineWave (\\(x0 y0 w h sep amp n shadow)
  (let boxi (\\i
    (let xi (+ x0 (* i sep))
    (let yi (- y0 (* amp (sin (* i (/ twoPi n)))))
    (let colori
      (if (= i 2) (if shadow 'lightgray' 'crimson')
                  (if shadow 'transparent' 'lightblue'))
    (rect colori xi yi w h)))))
  (map boxi (zeroTo n)))))

; Would be even cooler if wGrid and hGrid were computed
; automatically from the sine waves in each grid box.
; Leaving this step as an exercise for another day...

(def [outerPad innerPad] [100{0-300} 20{0-50}])
(def [wGrid hGrid]       [740{0-1500} 430{0-1000}])

; Helper function (could be added to Prelude)

(def outlined (\\(stroke strokeWidth shape)
  (consAttr (consAttr shape
    ['stroke' 'black'])
    ['stroke-width' strokeWidth])))

; Call sineWave in grid box at row i, column j.

(def place (\\(i j x0 y0 w h sep amp n shadow)
  (let [iPad jPad] [(+ outerPad (* innerPad i)) (+ outerPad (* innerPad j))]
  (let gridBox
    (outlined 'black' (let border 3{0-10} border)
      (rect 'none' (+ jPad (* j wGrid)) (+ iPad (* i hGrid)) wGrid hGrid))
  (let waveBoxes
    (let [x y] [(+ x0 (* j wGrid)) (+ y0 (* i hGrid))]
    (sineWave (+ x jPad) (+ y iPad) w h sep amp n shadow))
  (cons gridBox waveBoxes))))))

; Parameters from the original sineWaveOfBoxes.little.
; If want to add sliders, then symbolically compute
; the possible solutions below. Could even add a slider
; for the index of the box being dragged... another exercise.

(def [x0 y0 w h sep0 amp0 n] [50 120 20 90 30 60 12])

; Simulate dragging of third box by (dx,dy) pixels.
; Two possible solutions for each mouse direction.

(def [dx dy] [50!{0-300} 110!{0-300}])

(def x1   (+ x0 dx))
(def sep1 (+ sep0 (/ dx 2!)))

(def y1   (+ y0 dy))
(def amp1 (/ (- y0 (+ dy 68!)) 0.867!))

; Now draw original and versions with different solutions.

(def totalPad (+ (* 2 outerPad) (* 2 innerPad)))
(def totalWidth (+ (* 3 wGrid) totalPad))
(def totalHeight (+ (* 3 hGrid) totalPad))

(svgViewBox totalWidth totalHeight
  (concat [
    (place 0 0 x0 y0 w h sep0 amp0 n false)
    (place 0 0 x1 y1 w h sep0 amp0 n true)  ; shadow box

    (place 1 1 x1 y1 w h sep0 amp0 n false) ; update  x0/y0
    (place 1 2 x1 y0 w h sep0 amp1 n false) ; update  x0/amp
    (place 2 1 x0 y1 w h sep1 amp0 n false) ; update sep/y0
    (place 2 2 x0 y0 w h sep1 amp1 n false) ; update sep/amp

    ; workaround parser design bug that the comment above
    ; needs to be followed by an expression
    []
]))

"""

-- LITTLE_TO_ELM waveOfBoxes
-- LITTLE_TO_ELM waveOfBoxesTokens
-- LITTLE_TO_ELM waveOfBoxes3
nBoxes =
 """
(def nBoxes
  (let [n x0 y0 w h sep] [3{1-10} 40 28 60 130 110{50-200}]
  (let boxi (\\i
    (let xi (+ x0 (mult i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi (range 0! (- n 1!)))))))
 
nBoxes

"""

groupOfBoxes =
 """
; Try:
;  - replacing (basicZonesTail nBoxes) with nBoxes

(def [x0 y0 w h sep] [50 200 60 130 110])
(def colorNum 100)
(def rotBox 0)
(def rotGroup 0)

(def [n slider] (hSlider true 50! 300! 30! 1! 10! 'n = '
                 3))

(def xi (\\i (+ x0 (mult i sep))))

(def nBoxes
  (let boxi (\\i
    (rotate
      (rect colorNum (xi i) y0 w h)
      rotBox (+ (xi i) (/ w 2)) (+ y0 (/ h 2))))
  (map boxi (list0N (- n 1)))))

(def groupBox
  (let [dw dh] [50 50]
  (let xg (- x0 (/ dw 2))
  (let yg (- y0 (/ dh 2))
  (let wg (+ (+ (- (xi (- n 1)) x0) w) dw)
  (let hg (+ h dh)
    (rotate
      (rect 'lightyellow' xg yg wg hg)
      rotGroup (+ xg (/ wg 2)) (+ yg (/ hg 2)))))))))

(svg (concat
  [ slider [groupBox] (basicZonesTail nBoxes) ]))

"""

-- LITTLE_TO_ELM sixBoxesA
-- LITTLE_TO_ELM sixBoxesB
basicSlides =
 """(def slideCount 5)
(def slideMovieCount 5)

[
  slideCount
  (\\slideNumber
    [
      slideMovieCount
      (\\slideMovieNumber
        [
          'Dynamic'
          1
          (\\(slideNumber movieNumber t)
            (svg [
              (rect
                'lightblue'
                (+ 100 (* 50 movieNumber))
                (+ 100 (* 50 slideNumber))
                (* t 50)
                (* t 50)
              )
            ])
          )
          true
        ]
      )
    ]
  )
]
"""

logo =
 """; sketch-n-sketch logo
;
(let [x0 y0 w h delta] [50 50 200 200 10]
(let [xw yh w2 h2] [(+ x0 w) (+ y0 h) (div w 2) (div h 2)]
(let poly (\\pts (polygon 'black' 'none' 0 pts))
(svg [
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

logo2 =
 """; sketch-n-sketch logo, v2
; better lines for non-square scaling

(def logo (\\(x0 y0 w h delta1 delta2 fg bg)
  (let [xw yh w2 h2] [(+ x0 w) (+ y0 h) (div w 2) (div h 2)]
  (let poly (\\(c pts) (polygon c 'none' 0 pts))
  (let basic (\\shape (addAttr shape ['ZONES' 'basic']))
  (svg [

    ; positive background
    ; starting with (xw,yh) to place color slider
    (poly fg [[xw yh] [xw y0] [x0 y0] [x0 yh]])

    ; negative X, part 1
    (poly bg
      [[x0 y0] [(+ x0 delta1) y0] [xw (- yh delta2)]
       [xw yh] [(- xw delta1) yh] [x0 (+ y0 delta2)]])

    ; negative X, part 2
    (basic (poly bg
      [[xw y0] [xw (+ y0 delta2)] [(+ x0 delta1) yh]
       [x0 yh] [x0 (- yh delta2)] [(- xw delta1) y0]]))

    ; positive, hiding top-right quarter of X
    (basic (poly fg
      [[(+ x0 delta1) y0] [xw y0] [xw (- yh delta2)]]))

  ]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def [x0 y0 w h delta1 delta2] [50! 50! 200 200 10 10])
(def [fg bg] [360 499])

(logo x0 y0 w h delta1 delta2 fg bg)

"""

-- TODO stack overflow for some reason?
-- LITTLE_TO_ELM logoSizes
elmLogo =
 """; Elm logo, based on:
; https://github.com/evancz/elm-svg/blob/1.0.2/examples/Logo.elm
;
; Notice how the 'viewBox' attribute puts the canvas in
; \"full screen\" mode. Also, although we don't currently handle
; rotations (i.e. 'transform's) specially, the resulting zone
; is still useful; toggle the Zones option to see.
;
(let foo (\\(color pts) (polygon color 'black' 0 pts))
(svgViewBox 323.141 322.95 [
  (foo '#F0AD00' [[161 152] [231 82] [91 82]])
  (foo '#7FD13B' [[8 0] [79 70] [232 70] [161 0]])
  (addAttr
     (rect '#7FD13B' 192 107 107 108)
     ['transform' 'matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)'])
  (foo '#60B5CC' [[323 143] [323 0] [179 0]])
  (foo '#5A6378' [[152 161] [0 8] [0 314]])
  (foo '#F0AD00' [[255 246] [323 314] [323 178]])
  (foo '#60B5CC' [[161 170] [8 323] [314 323]])
]))

"""

-- LITTLE_TO_ELM activeTrans
activeTrans2 =
 """
; Logo based on Active Transportation Alliance
; (http://activetrans.org/)
 
(def base 0)
(def grayPts
  [[  97 546           ] [  33 414           ]
   [  33 (+ base 153!) ] [  53 (+ base 128!) ]
   [  82 (+ base 135!) ] [  83 (+ base 160!) ]
   [ 114 (+ base 149!) ] [ 113 (+ base  98!) ]
   [ 143 (+ base  82!) ] [ 158 (+ base 101!) ]
   [ 160 (+ base  46!) ] [ 192 (+ base  27!) ]
   [ 221 (+ base  56!) ] [ 227 (+ base 222!) ]
   [ 245 (+ base 224!) ] [ 246 (+ base 181!) ]
   [ 288 (+ base 156!) ] [ 286 (+ base 113!) ]
   [ 312 (+ base  88!) ] [ 374 (+ base 106!) ]
   [ 375 (+ base 155!) ] [ 397 (+ base 136!) ]
   [ 424 (+ base 145!) ] [ 425 207           ]
  ])

(def greenPts
  [[247 663] [461 419] [466 230] [439 230] [178 614]])

(def [grayctrl greenctrl]
  [[47 489] [451 542]])

(def [cGreen cGray] ['#66CC66' '#505050'])
(def [b buttonShapes] (button 20! 20! '' 0.25))
(def groupBox (rect (if b 'transparent' cGreen) 0! 0! 500! 700!))

(def makePath (\\(color pts [xc yc])
  (let [[x0 y0] [x1 y1] | rest] pts
  (let commands
    (append
      (append ['M' x0 y0] ['Q' xc yc x1 y1])
      (foldr (\\([xi yi] acc) (append ['L' xi yi] acc))
             ['Z'] rest))
  (path color 'black' 0 commands)))))
 
(def grayPath (makePath (if b cGray 'white') grayPts grayctrl))
(def greenPath (makePath (if b cGreen 'white') greenPts greenctrl))

(svg (append [groupBox grayPath greenPath] buttonShapes))

"""

botanic =
 """
; Logo: Chicago Botanic Garden

; Click '[Zones]' to see the control points for
; the various Bezier curves.

(def [w h] [434! 622])
(def midline (/ w 2!))

(def [x0  y0  xc1 yc1 x1  y1  xc2 yc2]
     [185 261 59  232 0   382 28  183])

(def leaf (\\polarity
  (let [mx0 mxc1 mx1 mxc2]
       [(+ midline (* polarity x0))
        (+ midline (* polarity xc1))
        (+ midline (* polarity x1))
        (+ midline (* polarity xc2))]
    (path 'white' 'none' 0
      ['M' mx0 y0
       'Q' mxc1 yc1 mx1 y1
       'M' mx1 y1
       'Q' mxc2 yc2 mx0 y0]))))

(def [budTipY budCornerX budCornerY]
     [322     34         262       ])

(def bud
  (let [rx1 rx2]
       [(- midline budCornerX)
        (+ midline budCornerX)]
    (path 'white' 'none' 0
      ['M' midline budTipY
       'L' rx1 budCornerY
       'A' 31 31 0 0 1 rx2 budCornerY
       'L' rx2 budCornerY 'Z'])))

(def background (zones 'none' [(rect '#83F52C' 0! 0! w h)]))

(svg (concat [background [(leaf 1!) (leaf -1!) bud]]))
"""

rings =
 """
(let [x0 y0 w r dx dy] [30 30 7 20 32 20]
(let dxHalf (div dx 2)
 
(let row1
  (map (\\[i c] (ring c w (+ x0 (mult i dx)) y0 r))
       (zip [0 1 2] ['blue' 'black' 'red']))
 
(let row2
  (map (\\[i c]
         (let x (+ (+ x0 dxHalf) (mult i dx))
         (let y (+ y0 dy)
           (ring c w x y r))))
       (zip [0 1] ['yellow' 'green']))
 
(svg (append row1 row2))))))

"""

polygons =
 """(let ngon (\\(n cx cy len1 len2)
  (let dangle (/ (* 3! (pi)) 2!)
  (let anglei (\\i (+ dangle (/ (* i (* 2! (pi))) n)))
  (let xi     (\\i (+ cx (* len1 (cos (anglei i)))))
  (let yi     (\\i (+ cy (* len2 (sin (anglei i)))))
  (let pti    (\\i [(xi i) (yi i)])
  (let pts    (map pti (list0N (- n 1!)))
    (polygon 'yellow' 'maroon' 4 pts))))))))
(svg [
  (ngon 3 100 200 40 40)
  (ngon 4 200 200 30 30)
  (ngon 5 300 300 50 50)
  (ngon 7 300 100 40 40)
  (ngon 15 100 400 40 40)
]))

"""

stars =
 """ 
(let nStar (\\(fill stroke w n len1 len2 rot cx cy)
  (let pti (\\[i len]
    (let anglei (+ (- (/ (* i (pi)) n) rot) halfPi)
    (let xi (+ cx (* len (cos anglei)))
    (let yi (+ cy (neg (* len (sin anglei))))
      [xi yi]))))
  (let lengths
    (map (\\b (if b len1 len2))
         (concat (repeat n [true false])))
  (let indices (list0N (- (* 2! n) 1!))
    (polygon fill stroke w (map pti (zip indices lengths)))))))
 
(let [x0 y0 sep ni nj] [100 100 100 3! 7!]
(let [outerLen innerLen] [50 20]
(let iStar (\\i
   (let off (mult (- i ni) sep)
   (let [xi yi] [(+ x0 off) (+ y0 off)]
   (nStar 'goldenrod' 'black' 3 i outerLen innerLen 0! xi yi))))
 
(svg (map iStar (range ni nj)))))))

"""

sliders =
 """;
; The ni constants get adjusted by the sliders,
; and then clamped to fit within the [min, max] range.
; Also try changing the min and max constants below.
;
(let [min max] [0! 10!]
(let [n1 n2 n3 n4] [5 5 5 5]
(let [m1 m2 m3 m4] (map (clamp min max) [n1 n2 n3 n4])
;
; Both the horizontal and vertical slider abstractions
; below take a dropBall parameter:
;  - if true, the ball can slide off the rail;
;  - if false, the ball disappears when off the rail.
;
(let horizSlider (\\(dropBall xStart xEnd y minVal maxVal curVal)
  (let [rPoint wLine rBall] [4! 3! 10!]
  (let [xDiff valDiff] [(- xEnd xStart) (- maxVal minVal)]
  (let xBall (+ xStart (* xDiff (/ (- curVal minVal) valDiff)))
  (let xBall_ (clamp xStart xEnd xBall)
  (let rBall_ (if dropBall (if (= xBall_ xBall) rBall 0) rBall)
    [ (circle 'black' xStart y rPoint)
      (circle 'black' xEnd y rPoint)
      (line 'black' wLine xStart y xEnd y)
      (circle 'black' xBall y rBall_)
    ]))))))
;
(let vertSlider (\\(dropBall yStart yEnd x minVal maxVal curVal)
  (let [rPoint wLine rBall] [4! 3! 10!]
  (let [yDiff valDiff] [(- yEnd yStart) (- maxVal minVal)]
  (let yBall (+ yStart (* yDiff (/ (- curVal minVal) valDiff)))
  (let yBall_ (clamp yStart yEnd yBall)
  (let rBall_ (if dropBall (if (= yBall_ yBall) rBall 0) rBall)
    [ (circle 'black' x yStart rPoint)
      (circle 'black' x yEnd rPoint)
      (line 'black' wLine x yStart x yEnd)
      (circle 'black' x yBall rBall_)
    ]))))))
;
(let sliders
  (let s1 (horizSlider false 30! 230! 30! min max n1)
  (let s2 (horizSlider true 30! 230! 70! min max n2)
  (let s3 (vertSlider false 110! 300! 110! min max n3)
  (let s4 (vertSlider true 110! 300! 150! min max n4)
    (foldl append nil [s1 s2 s3 s4])))))
;
(let displays
  (let t1 (text 300 100 (+ 'm1 = ' (toString m1)))
  (let t2 (text 300 120 (+ 'm2 = ' (toString m2)))
  (let t3 (text 300 140 (+ 'm3 = ' (toString m3)))
  (let t4 (text 300 160 (+ 'm4 = ' (toString m4)))
    [t1 t2 t3 t4]))))
;
  (svg (append sliders displays)))))))))

"""

buttons =
 """;
(let button_ (\\(dropBall xStart y caption xCur)
  (let [rPoint wLine rBall wSlider] [4! 3! 10! 70!]
  (let xEnd (+ xStart wSlider)
  (let xBall (+ xStart (* xCur wSlider))
  (let xBall_ (clamp xStart xEnd xBall)
  (let rBall_ (if dropBall (if (= xBall_ xBall) rBall 0) rBall)
  (let val (< xCur 0.5)
  (let shapes
    [ (circle 'black' xStart y rPoint)
      (circle 'black' xEnd y rPoint)
      (line 'black' wLine xStart y xEnd y)
      (circle (if val 'darkgreen' 'darkred') xBall y rBall_)
      (text (+ xEnd 10) (+ y 5) (+ caption (toString val))) ]
  [val shapes]))))))))
;
(let [b b1] (button_ true 20! 20! 'b = ' 0.25)
  (svg b1)))

"""

widgets =
 """; library widgets
;
(let [n  s1] (hSlider false 20! 90!  20! 0! 5! 'n = ' 3.1415)
(let [i  s2] (hSlider true  20! 90!  50! 0! 5! 'i = ' 3.1415)
(let [b1 s3] (button        20!      80!       'b1 = ' 0.25)
(let [b2 s4] (button        20!     110!       'b2 = ' 0.75)
  (svg (concat [s1 s2 s3 s4]))))))

"""

xySlider =
 """; A two dimensional slider in a similar style to the other sliders
(def xySlider_
  (\\(dropBall roundInt xStart xEnd yStart yEnd minx maxx miny maxy xcaption ycaption curx cury)
    (let [rCorner wEdge rBall] [4! 3! 10!]
    (let [xDiff yDiff xValDiff yValDiff] [(- xEnd xStart) (- yEnd yStart) (- maxx minx) (- maxy miny)]
    (let ballx (+ xStart (* xDiff (/ (- curx minx) xValDiff)))
    (let bally (+ yStart (* yDiff (/ (- cury miny) yValDiff)))
    (let ballx_ (clamp xStart xEnd ballx)
    (let bally_ (clamp yStart yEnd bally)
    (let rball_ (if dropBall (if (< maxx curx) 0 rBall) rBall)
    (let rball__ (if dropBall (if (< maxy cury) 0 rball_) rBall)
    (let xval
      (let xval_ (clamp minx maxx curx)
      (if roundInt (round xval_) xval_))
    (let yval
      (let yval_ (clamp miny maxy cury)
      (if roundInt (round yval_) yval_))
    (let shapes
      [ (line 'black' wEdge xStart yStart xEnd yStart)
        (line 'black' wEdge xStart yStart xStart yEnd)
        (line 'black' wEdge xStart yEnd xEnd yEnd)
        (line 'black' wEdge xEnd yStart xEnd yEnd)
        (circle 'black' xStart yStart rCorner)
        (circle 'black' xStart yEnd rCorner)
        (circle 'black' xEnd yStart rCorner)
        (circle 'black' xEnd yEnd rCorner)
        (circle 'black' ballx_ bally_ rball__)
        (text (- (+ xStart (/ xDiff 2)) 40) (+ yEnd 20) (+ xcaption (toString xval)))
        (text (+ xEnd 10) (+ yStart (/ yDiff 2)) (+ ycaption (toString yval))) ]
  [ [ xval yval ] shapes ])))))))))))))

(def xySlider (xySlider_ false))

(def [ [ a b ] slider ]
  (xySlider false 20! 420! 20! 420! 0! 100! 0! 100! 'X Axis: ' 'Y Axis: ' 20 20))

(svg slider)

"""

offsets =
 """(def pt@[x y] ([125 150] : Point))

(def pt2x (+ x 150))
(def pt3y (+ y 100))
(def pt4x (+ pt2x 50))
(def pt5y (- y 40))

(blobs [
])
"""

rgba =
 """;
; A Color Picker
; 
; Move the sliders to change the rgba
; value of the circle!
;
(let [r_ g_ b_ a_] [22 74 237 0.5]
;
(let [r s1] (hSlider true 20! 420! 20! 0! 255! '' r_)
(let [g s2] (hSlider true 20! 420! 50! 0! 255! '' g_)
(let [b s3] (hSlider true 20! 420! 80! 0! 255! '' b_)
(let [a s4] (hSlider false 20! 420! 110! 0.0! 1.0! '' a_)
;
(let ball (circle [r g b a] 220! 300! 100!)
(let sliders (concat [s1 s2 s3 s4])
;
  (svg (cons ball sliders)))))))))

"""

-- LITTLE_TO_ELM boxGrid
boxGridTokenFilter =
 """
; Drag some \"filter tokens\" from the right over the grid.
; Toggle between positive/negative filtering.

(def [x0 y0 w h boxSize] [30! 100! 300! 300! 50!])
(def allColors [0 100 200 300 450])
(def seedRows 1.5)
(def seedCols 2.5)
(def seedNumColors 1.5)
(def seedShapeKind 0.5)
(def seedFilterKind 0.75)

; derived values
(def [xw yh] [(+ x0 w) (+ y0 h)])
(def sep (+ boxSize 10!))
(def halfBoxSize (/ boxSize 2!))

(def [ [ cols rows ] boxSlider ] 
  (let pad 10!
  (xySlider
    (- x0 pad) (+ xw pad)
    (- y0 pad) (+ yh pad)
    0! (/ w sep)
    0! (/ h sep)
    '' ''
    seedCols seedRows)))

(def [numColors numColorsSlider]
  (hSlider true 20! 100! 30! 1! 5! '#Colors = ' seedNumColors))

(def [shapeKind shapeKindSlider]
  (enumSlider 220! 300! 30! ['Box' 'Dot' 'Star'] '' seedShapeKind))

(def [posFilter filterKindSlider]
  (button 360! 30! 'PosNeg = ' seedFilterKind))

(def tokens 
  (let [x0 y0] [400! 50!]
  (let shift (\\(dx dy) [(+ x0 dx) (+ y0 dy)])
  (map (\\[x y] (ghost (circle (if posFilter 'blue' 'red') x y 10!)))
       [(shift 0  30)
        (shift 0  60)
        (shift 0  90)
        (shift 0 120)
        (shift 0 150)
        (shift 0 180)
        (shift 0 210)
        (shift 0 240)
        (shift 0 270)
        (shift 0 300)
       ]))))

(def isCovered (\\(cx cy)
  (let checkX (between (- cx halfBoxSize) (+ cx halfBoxSize))
  (let checkY (between (- cy halfBoxSize) (+ cy halfBoxSize))
  (let centers (map (\\tok [(lookupAttr tok 'cx') (lookupAttr tok 'cy')]) tokens)
  (some (\\[x y] (and (checkX x) (checkY y))) centers)
 )))))

(def shapes
  (let indices (cartProd (range 0! (- cols 1!)) (range 0! (- rows 1!)))
  (let drawShape (\\[i j]
    (let shape
      (let c (nth allColors (mod (- i j) numColors))
      (let x (+ x0 (mult i sep))
      (let y (+ y0 (mult j sep))
      (let [cx cy] [(+ x halfBoxSize) (+ y halfBoxSize)]
      ; TODO boolean patterns?
      (let covered (isCovered cx cy)
      (if (or (and posFilter (not covered))
              (and (not posFilter) covered)) (circle 'none' 0! 0! 0!)
      (case shapeKind
        ('Box'  (square c x y boxSize))
        ('Dot'  (circle c cx cy halfBoxSize))
        ('Star' (nStar c 'none' 0! 4! halfBoxSize 10! 0! cx cy))
        ( else  (circle 'none' 0! 0! 0!)))))))))
    (if (and (= i (- cols 1!)) (< j numColors))
        shape
        (addAttr shape ['ZONES' 'none']))))
  (map drawShape indices))))

(svg (concat [ 
  shapes
  boxSlider
  numColorsSlider
  shapeKindSlider
  filterKindSlider
  tokens
]))

"""

usFlag13 =
 """;
; Original flag of the United States
;
; A few ways to mainpulate this example:
; - Grab bottom right corner to increase overall size
; - Grab the edge of a red stripe to increase width
; - Grab the points of one of the stars to change 
;     the size of its points
;
(let rotate (\\a (/ (* a (pi)) 6.5!))
(let [x0 y0 ni nj pts w h] [20 20 0! 12! 5! 500 260]
(let hstripe (/ h 13!)
(let [blockw blockh] [(/ w 3!) (* 7! hstripe)]
(let min
  (if (< blockw blockh)
    (* 0.4! blockw)
    (* 0.4! blockh))
(let [outerLen innerLen] [10 4]
(let block (rect '#09096d' x0 y0 blockw blockh)
(let stripes
  (map
    (\\i (rect 'red' x0 (+ y0 (* i hstripe)) w hstripe))
    [0! 2! 4! 6! 8! 10! 12!])
(let base (append stripes [block])
;
  (svg
    (cons (rect 'white' (- x0 10!) (- y0 10!) (+ w 20!) (+ h 20!))
    (append base
      (map (\\i
         (nStar 'white' 'none' 0 pts innerLen outerLen
           (rotate  i)
           (+ (+ x0 (/ w 6!)) (* min (cos (rotate i))))
           (+ (+ y0 (* hstripe 3.5!)) (* min (sin (rotate i))))))
        (range ni nj))))))))))))))

"""

usFlag50 =
 """;
; Current Flag of the United States
; (using circles for now, since 50 stars is slow)
;
; A few ways to manipulate:
; - Grab various parts of the red stripes or
;     blue block and pull in various directions
; - Grab the edges of the circles and and
;     increase or decrease the radius
;
(let [x0 y0 ni nj pts w h rad] [20 20 0! 12! 5! 510 272 6]
(let hstripe (/ h 13!)
(let block (rect '#09096d' x0 y0 (* w (/ 2! 5!)) (* 7! hstripe))
(let stripes
  (map
    (\\i (rect 'red' x0 (+ y0 (* i hstripe)) w hstripe))
    [0! 2! 4! 6! 8! 10! 12!])
(let base (append stripes [block])
;
  (svg
    (cons (rect 'white' (- x0 10!) (- y0 10!) (+ w 20!) (+ h 20!)) 
    (append base
      (map (\\[i j]
        (let xsep (/ w 15!)
        (let ysep (* hstripe 1.3!)
          (circle 'white' (+ x0 (* i xsep)) (+ y0 (* j ysep)) rad))))
        (append (cartProd (range 0.5! 5.5!) (range 0.75! 4.75!))
                (cartProd (range 1! 5!) (range 1.2! 4.2!))))))))))))

"""

chicago =
 """ 
; The flag of Chicago
;
; Possible ways to manipulate
; - Pull stripes or stars in various directions
; - Group box in background
 
(def [x0 y0 ni nj pts w h] [40 40 0.5! 3.5! 6! 454 300])
(def [outerLen innerLen] [30 12])

(def background
  (let [d1 d2] [10! 20!]
  [ (rect 'white' (- x0 d1) (- y0 d1) (+ w d2) (+ h d2)) ]))

(def stripes
  (map (\\i
    (rect 'lightblue' x0 (+ y0 (* i h)) w (/ h 6!)))
  [(/ 1! 6!) (/ 2! 3!)]))

(def stars
  (map (\\i
    (let off (* i (/ w 4!))
    (nStar 'red' 'none' 0 pts outerLen innerLen 0
      (+ x0 off) (+ y0 (/ h 2!)))))
  (range ni nj)))

(svg (concat [background stripes stars]))

"""

-- LITTLE_TO_ELM chicagoColors
frenchSudan =
 """;
; The Flag of French Sudan, based on:
;
; A few ways to manipulate:
; - Grab any part of the stick figure and move it
;     in various directions
;
(let [x0 y0 w h] [50 30 450 300]
(let wstripe (/ w 3!)
(let xoff (+ x0 wstripe)
(let yoff (+ y0 (/ h 4!))
(let stripe (\\[color x] (rect color x y0 wstripe h))
(let minrad
  (if (< (/ wstripe 7.5!) (/ h 15!))
    (/ wstripe 7.5!)
    (/ h 15!))
(let figline (\\[[a b] [c d]] (line 'black' (/ minrad 2!) a b c d))
(let [x1 x2 x3] (map (\\n (+ x0 (* wstripe n))) [1.2! 1.5! 1.8!])
(let [y1 y2 y3 y4] (map (\\n (+ y0 (/ h n))) [4.3! 2.8! 1.9! 1.4!])
;
  (svg
    (cons (rect 'white' (- x0 10!) (- y0 10!) (+ w 20!) (+ h 20!)) 
    (append
      (map stripe [['blue' x0] ['white' (+ x0 wstripe)] ['red' (+ x0 (* 2! wstripe))]])
      (snoc
        (ellipse 'black' x2 y1 (/ wstripe 7.5!) (/ h 15!))
        (map
          figline
          [[[x1 y1] [x1 y2]]
           [[x1 y2] [x3 y2]]
           [[x3 y1] [x3 y2]]
           [[x1 y4] [x1 y3]]
           [[x1 y3] [x3 y3]]
           [[x3 y3] [x3 y4]]
           [[x2 y1] [x2 y3]]]))))))))))))))

"""

flw1 =
 """;
; A Frank Lloyd Wright design inspired by:
; http://www.glass-by-design.com/images3/skylight3.jpg
;
(let [x0 y0 w h max] [69 55 532 744 10!]
(let wbox (/ w 10!)
(let hbox (/ h 10!)
(let xoff (\\n (+ x0 (* wbox n)))
(let yoff (\\n (+ y0 (* hbox n)))
(let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
(let redpoly
  (\\[a b]
    (polygon
      'red'
      'black'
      3
      [[(xoff  a) (yoff  a)]
       [(xoff  a) (yoff  b)]
       [(xoff  b) (yoff  b)]
       [(xoff  b) (yoff  a)]]))
(let dimension
  [0! 1 2 2.9 2.4 1.5 9.1 7.9 8.2 8.7 10!]
(let verticals
  (zip
    (map (\\n [(xoff  n) y0]) dimension)
    (map (\\n [(xoff  n) (+ y0 (* hbox max))]) dimension))
(let horizontals
  (zip
    (map (\\n [x0 (yoff  n)]) dimension)
    (map (\\n [(+ x0 (* wbox max)) (yoff  n)]) dimension))
  (svg
    (cons (rect 'white' (- x0 10!) (- y0 10!) (+ w 20!) (+ h 20!)) 
    (append
      (map blkline (append verticals horizontals))
      (append
        (append
          (let [p0 p1 p2 p3 p4] [0 1 2 2.9 5]
            (map redpoly [[p0 p1] [p1 p2] [p2 p3] [p3 p4]]))
          (map (\\[x y] (ellipse 'blue' x y (* wbox 4) hbox)) [[(xoff  5) (yoff  9)]]))
        (map
          (\\[x y r] (circle 'yellow' x y r))
          [[(xoff  6) (yoff  1.75) (+ wbox hbox)]
           [(xoff  6) (yoff  7) (/ (+ wbox hbox) 4)]
           [(xoff  6) (yoff  5) (/ (+ wbox hbox) 2)]])))))))))))))))

"""

flw2 =
 """;
; A Frank Lloyd Wright design based on:
; http://www.glass-by-design.com/images3/skylight3.jpg
;
; This is a tiled version of that design
;
; Possible ways to manipulate:
; - Grab edges of red polygons, yellow circles, or
;     blue ellipses and pull in various directions
; - Grab ends of lines and move in various directions
;
(let [x0 y0 w h max] [69 55 200 320 10!]
(let wbox (/ w 10!)
(let hbox (/ h 10!)
(let xoff (\\(n x) (+ x0 (+ (* x w) (* wbox n))))
(let yoff (\\(n y) (+ y0 (+ (* y h) (* hbox n))))
(let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
(let redpoly
  (\\[a b x y]
    (polygon
      'red'
      'black'
      3
      [[(xoff a x) (yoff a y)]
       [(xoff a x) (yoff b y)]
       [(xoff b x) (yoff b y)]
       [(xoff b x) (yoff a y)]]))
(let dimension [0! 4 5 6 7 10!]
(let [p0 p1 p2 p3 p4] [0 1 2 2.9 5]
(let singletile
  (\\[x y] 
    (let verticals
      (zip
        (map (\\n [(xoff n x) (+ y0 (* y h))]) dimension)
        (map (\\n [(xoff n x) (+ y0 (* (+ y 1) h))]) dimension))
    (let horizontals
      (zip
        (map (\\n [(+ x0 (* x w)) (yoff n y)]) dimension)
        (map (\\n [(+ x0 (* (+ 1 x) w)) (yoff n y)]) dimension))
      (append
        (append
          (map blkline (append verticals horizontals))
          (map (\\[xc yc r] (circle 'yellow' xc yc r))
            [[(xoff 6 x) (yoff 1.75 y) (+ wbox hbox)]
             [(xoff 6 x) (yoff 7 y) (/ (+ wbox hbox) 4)]
             [(xoff 6 x) (yoff 5 y) (/ (+ wbox hbox) 2)]]))
        (cons
          (ellipse 'blue' (xoff 5 x) (yoff 9 y) (* wbox 4) hbox)
          (map redpoly [[p0 p1 x y] [p1 p2 x y] [p2 p3 x y] [p3 p4 x y]])
          )))))
(let grid (cartProd [0! 1! 2!] [0! 1!])
;
  (svg 
    (cons (rect 'white' (- x0 10!) (- y0 10!) (+ (* 3 w) 20!) (+ (* 2 h) 20!))
      (concatMap singletile grid))))))))))))))

"""

ferris =
 """
; Take this ferris wheel for a spin!
; Try:
;  - Stretching the passenger cars
;  - Stretching the central hub
;  - Dragging the central hub
;  - Setting showSliders to false

(def wheel (\\(cx cy rCenter wCar rCap numSpokes spokeLen rotAngle)
  (let rim      [(ring 'darkgray' 6 cx cy spokeLen)]
  (let center   [(circle 'black' cx cy rCenter)]
  (let frame    [(nStar 'goldenrod' 'darkgray' 3 numSpokes spokeLen 0 rotAngle cx cy)]
  (let spokePts (nPointsOnCircle numSpokes rotAngle cx cy spokeLen)
  (let cars     (mapi (\\[i [x y]] (squareByCenter (if (= i 0) 'pink' 'lightgray') x y wCar)) spokePts)
  (let hubcaps  (map (\\[x y] (circle 'black' x y rCap)) spokePts)
    (concat [rim cars center frame hubcaps])
))))))))

(def [cx cy spokeLen rCenter wCar rCap] [220 300 80 20 30 7])
(def [numSpokes rotAngle] [5!{3-15} 0!{-3.14-3.14}])

(svg (wheel cx cy rCenter wCar rCap numSpokes spokeLen rotAngle))

"""

ferris2 =
 """(def n 5)
(def len 80)
(def rot 0)

(def wheel
  (let [cx cy] [280 200]
  (let rim [(ring 'darkgray' 8 cx cy len)]
  (let center [(circle 'black' cx cy 20)]
  (let frame [(nStar 'goldenrod' 'darkgray' 3 n len 0 rot cx cy)]
  (let spokePts (nPointsOnCircle n rot cx cy len)
  (let caps (map (\\[x y] (circle 'black' x y 7)) spokePts)
  (let cars
    (let wCar 30
    (let wHalfCar (/ wCar 2!)
    (map (\\[x y] (squareByCenter 'lightgray' x y wCar)) spokePts)))
  (concat [rim cars center frame caps])))))))))

(svg wheel)

"""

ferris2target =
 """(def n 8)
(def len 142)
(def rot -0.13796015197333036)

(def wheel
  (let [cx cy] [280 200]
  (let rim [(ring 'darkgray' 3 cx cy len)]
  (let center [(circle 'black' cx cy 15)]
  (let frame [(nStar 'goldenrod' 'darkgray' 3 n len 0 rot cx cy)]
  (let spokePts (nPointsOnCircle n rot cx cy len)
  (let caps (map (\\[x y] (circle 'black' x y 6)) spokePts)
  (let cars
    (let wCar 27
    (let wHalfCar (/ wCar 2!)
    (mapi (\\[i [x y]] (squareByCenter (if (= 0 i) 'pink' 'lightgray') x y wCar)) spokePts)))
  (concat [rim cars center frame caps])))))))))

(svg wheel)

"""

ferrisWheelSlideshow =
 """(def [slideN slideSlider] (hSlider true 20! 400! 20! 1! 13! 'Slide ' 1))
(def [timeInSlide timeInSlideSlider] (hSlider false 20! 400! 40! 0.0! 1.0! 'Time in Slide ' 0.0))

(def rimColor [0 0 0 1])
(def beamWidth 1)
(def [centerX centerY] [300 300])
(def [smallRadius largeRadius] [50 150])
(def spokeEndSize 3)
(def hubRadius 12)
(def [carHighlightColor carColor] [[251 191 141 1] [191 191 191 1]])
(def [carWidth carHeight] [20 20])
(def spokeDuplicationOffset 12)
(def carDuplicationOffset (+ carWidth 12))
(def carDiagonalDuplicationOffset 14)
(def rotationAngle (* twoPi 0.03))

(def spoke (\\(centerX centerY rimX rimY)
  [
    (line 'black' 1 centerX centerY rimX rimY)
    (circle 'black' rimX rimY spokeEndSize)
  ]
))

(def diameterSpoke (\\(x1 y1 x2 y2)
  [
    (line 'black' 1 x1 y1 x2 y2)
    (circle 'black' x1 y1 spokeEndSize)
    (circle 'black' x2 y2 spokeEndSize)
  ]
))

(def car_ (\\(fill x y w h)
  ; Manual, so we can add stroke.
  [
    'rect'
    [ ['x' (- x (/ w 2))] ['y' (- y (/ h 2))] ['width' w] ['height' h] ['fill' fill] ['stroke' 'black'] ]
    []
  ]
))

(def hub_ (\\(fill x y r)
  ; Manual, so we can add stroke.
  [
    'circle'
    [ ['cx' x] ['cy' y] ['r' r] ['fill' fill] ['stroke' 'black'] ]
    []
  ]
))

(def rimAttachmentPoints (\\(spokeCount radius angle centerX centerY)
  (let angles (map (\\i (- (+ (* (/ i spokeCount) twoPi) angle) halfPi)) (range 0 (- spokeCount 1)))
    (map (\\angle [(+ centerX (* (cos angle) radius)) (+ centerY (* (sin angle) radius))]) angles)
  )
))

(def carsAndHub (\\(spokeCount radius angle carWidth carHeight hubRadius centerX centerY)
  (let hub [(hub_ carColor centerX centerY hubRadius)]
  (let [[highlightedCarX highlightedCarY]|otherRimAttachmentPoints] (rimAttachmentPoints spokeCount radius angle centerX centerY)
  (let highlightedCar [(car_ carHighlightColor highlightedCarX highlightedCarY carWidth carHeight)]
  (let otherCars (map (\\[x y] (car_ carColor x y carWidth carHeight)) otherRimAttachmentPoints)
    [hub highlightedCar otherCars]
  ))))
))

(def rimAndSpokes (\\(spokeCount radius angle centerX centerY)
  (let rim [(ring rimColor beamWidth centerX centerY radius)]
  (let spokes (map (\\[x y] (spoke centerX centerY x y)) (rimAttachmentPoints spokeCount radius angle centerX centerY))
    [rim (concat spokes)]
  ))
))

(def ferrisWheel (\\(spokeCount radius angle carWidth carHeight hubRadius centerX centerY)
  (concat [
    (carsAndHub spokeCount radius angle carWidth carHeight hubRadius centerX centerY)
    (rimAndSpokes spokeCount radius angle centerX centerY)
  ])
))

(def hub [(hub_ carColor centerX centerY hubRadius)])
(def smallRim [(ring rimColor beamWidth centerX centerY smallRadius)])
(def spoke1 (diameterSpoke centerX (+ centerY smallRadius) centerX (- centerY smallRadius)))
(def spoke1Duplicate1 (diameterSpoke (+ centerX spokeDuplicationOffset) (+ (+ centerY smallRadius) spokeDuplicationOffset) (+ centerX spokeDuplicationOffset) (+ (- centerY smallRadius) spokeDuplicationOffset)))
(def spoke1Duplicate2 (diameterSpoke (+ centerX (mult 2 spokeDuplicationOffset)) (+ (+ centerY smallRadius) (mult 2 spokeDuplicationOffset)) (+ centerX (mult 2 spokeDuplicationOffset)) (+ (- centerY smallRadius) (mult 2 spokeDuplicationOffset))))
(def spoke1Duplicate1HalfMoved (diameterSpoke (+ centerX spokeDuplicationOffset) (+ (+ centerY smallRadius) spokeDuplicationOffset) (- centerX smallRadius) centerY))
(def spoke2 (diameterSpoke (+ centerX smallRadius) centerY (- centerX smallRadius) centerY))

(def car (\\(x y)
  [(car_ carColor x y carWidth carHeight)]
))

(def highlightedCar (\\(x y)
  [(car_ carHighlightColor x y carWidth carHeight)]
))

(def car1 (highlightedCar centerX (- centerY smallRadius)))
(def car1Duplicates (map (\\n (car (+ centerX (* n carDuplicationOffset)) (- centerY smallRadius))) (range 1 3)))

(def ferrisSmall4
  (ferrisWheel
    4 ; number of spokes
    smallRadius
    0 ; angle
    carWidth
    carHeight
    hubRadius
    centerX
    centerY
  )
)
(def car2Duplicates (map (\\n (car (+ smallRadius (+ centerX (* n carDiagonalDuplicationOffset))) (+ centerY (* n carDiagonalDuplicationOffset)))) (range 1 3)))
(def ferrisSmall4CarsAndHub
  (carsAndHub
    4 ; number of spokes
    smallRadius
    0 ; angle
    carWidth
    carHeight
    hubRadius
    centerX
    centerY
  )
)
(def ferrisSmall8CarsAndHub
  (carsAndHub
    8 ; number of spokes
    smallRadius
    0 ; angle
    carWidth
    carHeight
    hubRadius
    centerX
    centerY
  )
)
(def ferrisSmall8RimAndSpokes
  (rimAndSpokes
    8 ; number of spokes
    smallRadius
    0 ; angle
    centerX
    centerY
  )
)
(def ferrisSmall8
  (concat [ferrisSmall8CarsAndHub ferrisSmall8RimAndSpokes])
)
(def ferrisLarge8RimAndSpokesOffset
  (rimAndSpokes
    8 ; number of spokes
    largeRadius
    0 ; angle
    (+ centerX (- largeRadius smallRadius))
    (- centerY (- largeRadius smallRadius))
  )
)
(def ferrisLarge8RimAndSpokes
  (rimAndSpokes
    8 ; number of spokes
    largeRadius
    0 ; angle
    centerX
    centerY
  )
)
(def ferrisLarge8
  (ferrisWheel
    8 ; number of spokes
    largeRadius
    0 ; angle
    carWidth
    carHeight
    hubRadius
    centerX
    centerY
  )
)
(def ferrisLarge8BadlyRotated
  [(rotate
    ['g' [] (concat ferrisLarge8)]
    (/ (* rotationAngle 360) twoPi)
    centerX
    centerY
  )]
)

(def [car7X car7Y] (hd (reverse (rimAttachmentPoints 8 largeRadius rotationAngle centerX centerY))))
(def car7 (car car7X car7Y))
(def car7Duplicates (map (\\n (car (+ car7X (* n carDiagonalDuplicationOffset)) (+ car7Y (* n carDiagonalDuplicationOffset)))) (range 1 7)))

(def ferrisLarge8RimAndSpokesRotated
  (rimAndSpokes
    8 ; number of spokes
    largeRadius
    rotationAngle
    centerX
    centerY
  )
)
(def ferrisLarge8Rotated
  (ferrisWheel
    8 ; number of spokes
    largeRadius
    (+ rotationAngle (* twoPi timeInSlide))
    carWidth
    carHeight
    hubRadius
    centerX
    centerY
  )
)

(def appearInOrder (\\shapeGroups
  (let appearanceTimeAndShapeGroups (map2 (\\(i shapeGroup) [(/ i (len shapeGroups)) shapeGroup]) (range 0 (- (len shapeGroups) 1)) shapeGroups)
    (foldr
      (\\([t shapeGroup] visible)
        (if (ge timeInSlide t)
          (let opacity (/ (- timeInSlide t) (/ 1 (len shapeGroups)))
          (let faded [['g' [['opacity' opacity]] shapeGroup]]
            [faded | visible]
          ))
          visible
        )
      )
      []
      appearanceTimeAndShapeGroups
    )
  )
))

(def elements
  (if (= slideN 1)
    (appearInOrder [smallRim spoke1 spoke1Duplicate1])
    (if (= slideN 2)
      [smallRim spoke1 spoke1Duplicate1HalfMoved]
      (if (= slideN 3)
        (concat [ [hub car1] (appearInOrder car1Duplicates) [smallRim spoke1 spoke2] ])
        (if (= slideN 4)
          ferrisSmall4
          (if (= slideN 5)
            (concat [ ferrisSmall4 (appearInOrder (concat [[spoke1Duplicate1 spoke1Duplicate2] car2Duplicates])) ])
            (if (= slideN 6)
              (concat [ ferrisSmall4CarsAndHub ferrisSmall8RimAndSpokes (appearInOrder car2Duplicates) ])
              (if (= slideN 7)
                ferrisSmall8
                (if (= slideN 8)
                  (concat [ ferrisSmall8CarsAndHub ferrisLarge8RimAndSpokesOffset ])
                  (if (= slideN 9)
                    (concat [ ferrisSmall8CarsAndHub ferrisLarge8RimAndSpokes ])
                    (if (= slideN 10)
                      ferrisLarge8
                      (if (= slideN 11)
                        [ferrisLarge8BadlyRotated]
                        (if (= slideN 12)
                          (concat [ [hub car7] ferrisLarge8RimAndSpokesRotated (appearInOrder car7Duplicates) ])
                          (if (= slideN 13)
                            ferrisLarge8Rotated
                            []
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(svg (append (concat elements) (concat [slideSlider timeInSlideSlider])))
"""

pieChart1 =
 """; A Pie Chart
;
; Move the sliders to change the size of a particular slice
;
(let [count1_ count2_ count3_ count4_ count5_] [35 31 16 10 8]
(let [color1 color2 color3 color4 color5] ['#8DEEEE' '#66CCCC' '#49E9BD' '#5EDA9E' '#00FA9A']
(let [h1 h2 h3 h4 h5] [20! 50! 80! 110! 140!]
(let [count1 s1] (hSlider true 20! 420! h1 0! 100! '' count1_)
(let [count2 s2] (hSlider true 20! 420! h2 0! 100! '' count2_)
(let [count3 s3] (hSlider true 20! 420! h3 0! 100! '' count3_)
(let [count4 s4] (hSlider true 20! 420! h4 0! 100! '' count4_)
(let [count5 s5] (hSlider true 20! 420! h5 0! 100! '' count5_)
(let total (+ count1 (+ count2 (+ count3 (+ count4 count5))))
(let p2 (+ count1 count2)
(let p3 (+ p2 count3)
(let p4 (+ p3 count4)
(let p5 (+ p4 count5)
;
(let sliders (concat [s1 s2 s3 s4 s5])
(let swatches (map (\\[h c] (square c 460! (- h 10!) 20!)) [[h1 color1] [h2 color2] [h3 color3] [h4 color4] [h5 color5]])
(let [cx cy r t border] [280! 440! 180 4 'grey']
;
(let pie
  (let pToDegrees (\\p (* 360! (/ p total)))
  (let [d1 d2 d3 d4 d5] (map pToDegrees [count1 p2 p3 p4 p5])
  (let flag (\\d (if (< 180 d) 1 0))
  (let flagged (map (\\[d fr] [d (flag fr)]) [[d1 d1] [d2 (- d2 d1)] [d3 (- d3 d2)] [d4 (- d4 d3)] [d5 (- d5 d4)]])
  (let toRadian (\\[d f] [(* (/ (pi) 180!) d) f])
  (let polarcoords (map toRadian flagged)
  (let slice (\\[ang flg] [flg (* r (cos ang)) (* r (sin ang))])
  (let [[f1 x1 y1] [f2 x2 y2] [f3 x3 y3] [f4 x4 y4] [f5 x5 y5]] (map slice polarcoords)
  (let wedge (\\[color f [sx sy] [ex ey]] (path color border t ['M' cx cy 'L' sx sy 'A' 180 180 0 f 1 ex ey 'Z']))
  (let wedges 
    (map
      wedge
        [[color1 f1 [(+ cx 180!) cy] [(+ cx x1) (+ cy y1)]]
        [color2 f2 [(+ cx x1) (+ cy y1)] [(+ cx x2) (+ cy y2)]]
        [color3 f3 [(+ cx x2) (+ cy y2)] [(+ cx x3) (+ cy y3)]]
        [color4 f4 [(+ cx x3) (+ cy y3)] [(+ cx x4) (+ cy y4)]]
        [color5 f5 [(+ cx x4) (+ cy y4)] [(+ cx x5) (+ cy y5)]]])
  wedges))))))))))
;
(svg (cons (circle 'lightgray' cx cy (* 1.1 r)) (append (append sliders swatches) pie))))))))))))))))))))
"""

solarSystem =
 """; Visualization of the solar system 
;
; The slider on top controls the \"animation.\"
; Try changing the size of a planet in one frame,
;   and see what happens in the others.
 
(def aupx 12)
(def [ox oy] [200 400])

; Relative radii of the planet orbits, in au
(def [ merorb venorb earorb marorb juporb satorb uraorb neporb ] 
     [ 0.387! 0.723! 1! 1.524! 5.203! 9.539! 19.18! 30.06! ]
)

; Relative orbital period to the Earth
(def [ meryr venyr earyr maryr jupyr satyr urayr nepyr ]
     [ 0.2409! 0.616! 1! 1.9! 12! 29.5! 84! 165! ]
)

; Function to place a body
(def planet (\\(color orb yr radius)
  (\\t (circle color  (+ ox (* aupx (* orb (cos (* t (/ 6.28318 yr))))))
                       (+ oy (* aupx (* orb (sin (* t (/ -6.28318 yr))))))
                       radius))))

; Visual for each body
; Each takes a time to be displayed at
(def sun (circle 'yellow' ox oy 10))
(def mercury (planet 'lightred'   merorb meryr 4))
(def venus   (planet 'orange'     venorb venyr 5))
(def earth   (planet 'green'      earorb earyr 5))
(def mars    (planet 'red'        marorb maryr 4))
(def jupiter (planet 'brown'      juporb jupyr 6))
(def saturn  (planet 'sandybrown' satorb satyr 6))
(def uranus  (planet 'blue'       uraorb urayr 6))
(def neptune (planet 'darkblue'   neporb nepyr 6))

; Visual for the rings
(def rings
  (reverse
    (map (\\orb (ring 'lightgrey' 2! ox oy (* aupx orb)))
         [ merorb venorb earorb marorb juporb satorb uraorb neporb ])))

(def [time timeslider] (hSlider true 20! 600! 20! 1! 1000! 'Day ' 1))
(def rev (\\(x f) (f x)))
(def planets
  (map (rev (/ time 365))
       [mercury venus earth mars jupiter saturn uranus neptune]))

(svg (concat [ rings [sun | planets] timeslider ]))

"""

fractalTree =
 """; A fractal tree
;
(def nsin (\\n (if (< n (/ 3.14159 2)) (sin n) (cos (mod n (/ 3.14159 2))))))
(def ncos (\\n (if (< n (/ 3.14159 2)) (cos n) (sin (mod n (/ 3.14159 2))))))
(def [initwd initlen] [10! 150!])
(def [steps stepslider] (hSlider true 20! 420! 550! 3! 8! 'Steps ' 4))
(def [bendn bendnslider] (hSlider false 20! 420! 580! 1! 8! 'Bend ' 1))
(def initangle (/ 3.14159! 2!))
(def bend (/ 3.14159! bendn))
(defrec exp (\\(b p)
  (if (< p 1) 1 (* b (exp b (- p 1))))))
(def mkleftx (\\(stepnum theta px) 
  (- px (* (/ initlen stepnum) (ncos (+ theta (* (exp 0.5 stepnum) bend)))))))
(def mkrightx (\\(stepnum theta px)
  (+ px (* (/ initlen stepnum) (ncos (- theta (* (exp 0.5 stepnum) bend)))))))
(def mklefty (\\(stepnum theta py)
  (- py (* (/ initlen stepnum) (nsin (+ theta (* (exp 0.5 stepnum) bend)))))))
(def mkrighty (\\(stepnum theta py)
  (- py (* (/ initlen stepnum) (nsin (- theta (* (exp 0.5 stepnum) bend)))))))
(defrec genchildren (\\(stepnum maxstep theta px2 py2) 
  (if (< maxstep stepnum) 
    [] 
    (append 
      [ (line 'black' (/ initwd stepnum) px2 py2 
          (mkleftx stepnum theta px2)
          (mklefty stepnum theta py2))
        (line 'black' (/ initwd stepnum) px2 py2
          (mkrightx stepnum theta px2)
          (mkrighty stepnum theta py2))]
      (append
        (genchildren (+ stepnum 1) maxstep (+ theta (* (exp 0.5 stepnum) bend))
          (mkleftx stepnum theta px2)
          (mklefty stepnum theta py2))
        (genchildren (+ stepnum 1) maxstep (- theta (* (exp 0.5 stepnum) bend))
          (mkrightx stepnum theta px2)
          (mkrighty stepnum theta py2)))))))
(def trunk (line 'black' initwd 210 400 210 250))
(def branches (genchildren 2 steps initangle 210 250))
(svg (concat [ [ trunk | branches ] bendnslider stepslider]))

"""

hilbertCurveAnimation =
 """; How to draw a Hilbert curve.
;
; https://thoughtstreams.io/jtauber/on-drawing-owls-and-teaching-non-beginners/
; 1. Draw a U.
; 2. Draw the rest of the curve.
;

; What fraction of the final curve should we draw?
; Time 0 to 0.5: none. Draw curve from time 0.5 to 1.
(def curveFractionToDraw (\\time
  (if (gt time 0.5)
    (- (* time 2.0) 1.0)
    0
  )
))

; For when all the Hilbert levels but the most detailed fade out together.
(def earlierLevelOpacity (\\time
  (pow (- 1.0 (curveFractionToDraw time)) 2)
))

; The basic U.
;
; We can't center this around 0,0 because of some weird clipping with the view box.
(def hilbertPart_ [
  (path 'none' 'green' 0.4 [ 'M' 10 10 'L' 10 20 'L' 20 20 'L' 20 10 ])
  (circle 'red' 10 10 1)
])

; We use SVG transforms to position, rotate, and size the U.
(def hilbertPart (\\(centerX centerY width rotation orientation opacity)
  [
    'g'
    [
      ['transform' [['translate' centerX centerY] ['rotate' rotation 0 0] ['scale' (/ width 20) (/ width 20)] ['scale' orientation 1] ['translate' -15 -15]]]
      ['opacity' opacity]
    ]
    hilbertPart_
  ]
))

; Add 90 degrees.
(def rotateAngleRight (\\a
  (if (= a 0)
    90
    (if (= a 90)
      180
      (if (= a 180)
        -90
        (if (= a -90)
          0
          'error'
        )
      )
    )
  )
))

; Angle 0 is straight up, so basically -angle.
(def flipAngleVertical (\\a
  (if (or (= a 0) (= a 180))
    a
    (- 0 a)
  )
))

; Rotates by 90 degrees around 0,0
(def rotatePointRight (\\[x y]
  (if (and (lt 0 x) (lt 0 y))
    [(- 0 x) y]
    (if (and (gt 0 x) (lt 0 y))
      [x (- 0 y)]
      (if (and (gt 0 x) (gt 0 y))
        [(- 0 x) y]
        (if (and (lt 0 x) (gt 0 y))
          [x (- 0 y)]
          'error'
        )
      )
    )
  )
))

; Want to always rotate the short way around the circle.
(defrec circularDisplacement (\\(a b)
  (let diff (- b a)
    (if (gt diff 180)
      (circularDisplacement a (- b 360))
      (if (le diff -180)
        (circularDisplacement a (+ b 360))
        diff
      )
    )
  )
))

; Clamp angle to (-180, 180]
(def normalizeRotation (\\angle
  (circularDisplacement 0 angle)
))

(def rotateChildrenRight (\\[
    [ x1 y1 rot1 or1 ]
    [ x2 y2 rot2 or2 ]
    [ x3 y3 rot3 or3 ]
    [ x4 y4 rot4 or4 ]
  ]
  [
    (append (rotatePointRight [x1 y1]) [(rotateAngleRight rot1) or1])
    (append (rotatePointRight [x2 y2]) [(rotateAngleRight rot2) or2])
    (append (rotatePointRight [x3 y3]) [(rotateAngleRight rot3) or3])
    (append (rotatePointRight [x4 y4]) [(rotateAngleRight rot4) or4])
  ]
))

(def flipChildrenVertical (\\[
    [ x1 y1 rot1 or1 ]
    [ x2 y2 rot2 or2 ]
    [ x3 y3 rot3 or3 ]
    [ x4 y4 rot4 or4 ]
  ]
  [
    [ (- 0 x1) y1 (flipAngleVertical rot1) (- 0 or1) ]
    [ (- 0 x2) y2 (flipAngleVertical rot2) (- 0 or2) ]
    [ (- 0 x3) y3 (flipAngleVertical rot3) (- 0 or3) ]
    [ (- 0 x4) y4 (flipAngleVertical rot4) (- 0 or4) ]
  ]
))

; Returns [ [relX relY rotation orientation] ... ]
(def hilbertChildParams (\\(rotation orientation)
  (let initial [
      [-1 -1  -90 -1]
      [-1  1    0  1]
      [1   1    0  1]
      [1  -1   90 -1]
    ]
  (let oriented (if (= orientation 1) initial (flipChildrenVertical initial))
    (if (= rotation 0)
      oriented
      (if (= rotation 90)
        (rotateChildrenRight oriented)
        (if (= rotation 180)
          (rotateChildrenRight (rotateChildrenRight oriented))
          (rotateChildrenRight (rotateChildrenRight (rotateChildrenRight oriented)))
        )
      )
    )
  ))
))

; Recursively draw the U's with the proper animation and opacity.
(defrec hilbertParts (\\(time depth levelPartCount partNumber opacity centerX centerY width rotation orientation)
  (let thisLevel (hilbertPart centerX centerY width rotation orientation (* (pow 0.5 depth) opacity))
    (if (le depth 0)
      (if (gt opacity 0.005)
        [thisLevel]
        []
      )
      (append
        (concat (map2
          (\\(i [relX relY rot or])
            (let [targetX targetY targetWidth targetRot targetOr]
              [
                (+ centerX (* relX (/ width 4)))
                (+ centerY (* relY (/ width 4)))
                (/ width 2)
                (+ rotation (circularDisplacement rotation rot))
                or
              ]
            (let thisLevelPartCount (* levelPartCount 4)
            (let thisPartNumber (+ (* partNumber 4) i)
            (let animationFraction
              (if (le depth 1)
                (let partAndFraction (* (* time 2) thisLevelPartCount)
                  (if (le partAndFraction thisPartNumber)
                    0
                    (if (ge partAndFraction (+ thisPartNumber 1))
                      1
                      (- partAndFraction thisPartNumber)
                    )
                  )
                )
                1
              )
            (let [movementFraction orientationFraction]
              (if (= orientation targetOr)
                [animationFraction 1]
                (if (lt animationFraction 0.5)
                  [(* animationFraction 2) 0]
                  [1 (- (* animationFraction 2) 1)]
                )
              )
            (let [aniX aniY aniWidth aniRot aniOr] [
                (+ (* centerX     (- 1 movementFraction   )) (* targetX     movementFraction))
                (+ (* centerY     (- 1 movementFraction   )) (* targetY     movementFraction))
                (+ (* width       (- 1 movementFraction   )) (* targetWidth movementFraction))
                (+ (* rotation    (- 1 movementFraction   )) (* targetRot   movementFraction))
                (+ (* orientation (- 1 orientationFraction)) (* targetOr    orientationFraction))
              ]
            (let opacity
              (if (gt (curveFractionToDraw time) 0.0)
                (if (le depth 1)
                  (let partAndFraction (* (curveFractionToDraw time) thisLevelPartCount)
                    (if (le partAndFraction thisPartNumber)
                      1
                      (if (ge partAndFraction (+ thisPartNumber 1))
                        0
                        (- 1 (- partAndFraction thisPartNumber))
                      )
                    )
                  )
                  (earlierLevelOpacity time)
                )
                1
              )
              (if (gt animationFraction 0)
                (hilbertParts time (- depth 1) thisLevelPartCount thisPartNumber opacity aniX aniY aniWidth aniRot aniOr)
                []
              )
            )))))))
          )
          (range 0 3)
          (hilbertChildParams (normalizeRotation rotation) orientation)
        ))
        [thisLevel]
      )
    )
  )
))

; Four points in a block.
(def hilbertPoints_ (\\(centerX centerY width rotation orientation)
  (let quarterWidth (/ width 4)
    (map
      (\\[relX relY _ _]
        [(+ centerX (* relX quarterWidth)) (+ centerY (* relY quarterWidth))]
      )
      (hilbertChildParams rotation orientation)
    )
  )
))

; List of points on the curve in order. [ [100 100] [150 100] ... ]
(defrec hilbertPoints (\\(depth centerX centerY width rotation orientation)
  (let thisLevel (hilbertPoints_ centerX centerY width rotation orientation)
    (if (le depth 0)
      thisLevel
      (concatMap
        (\\[relX relY childRot childOr]
          (let [childX childY childWidth]
            [
              (+ centerX (* relX (/ width 4)))
              (+ centerY (* relY (/ width 4)))
              (/ width 2)
            ]
            (hilbertPoints (- depth 1) childX childY childWidth childRot childOr)
          )
        )
        (hilbertChildParams (normalizeRotation rotation) orientation)
      )
    )
  )
))

; Returns the first n elements of the list
(def take (\\(n list)
  (map2
    always
    list
    (range 1 n)
  )
))

; Returns element i (starting from 0) from a list
(def fetch (\\(i list)
  (fst (foldl
    (\\(x [ret thisI])
      (if (= 0 thisI)
        [x (- thisI 1)]
        [ret (- thisI 1)]
      )
    )
    [nil i]
    list
  ))
))

; When drawing the final curve, which points should we draw?
;
; All the complexity here is for adding a point part-way between
; the last point and the next point based on the time.
(def hilbertPointsAnimated (\\(time depth centerX centerY width rotation orientation)
  (if (gt (curveFractionToDraw time) 0)
    (let allPoints (hilbertPoints depth centerX centerY width rotation orientation)
    (let count (len allPoints)
    (let countToDraw (floor (* (curveFractionToDraw time) count))
    (let partialLineFraction (- (* (curveFractionToDraw time) count) countToDraw)
    (let pointsToDraw (take countToDraw allPoints)
      (if (and (gt partialLineFraction 0) (gt countToDraw 0))
        (let [lastPointX lastPointY] (fetch (- countToDraw 1) allPoints)
        (let [nextPointX nextPointY] (fetch countToDraw allPoints)
        (let lastPointToDraw
          [
            (+ (* lastPointX (- 1 partialLineFraction)) (* nextPointX partialLineFraction))
            (+ (* lastPointY (- 1 partialLineFraction)) (* nextPointY partialLineFraction))
          ]
          (snoc lastPointToDraw pointsToDraw)
        )))
        (if (gt countToDraw 1)
          pointsToDraw
          [[0 0]]
        )
      )
    )))))
    [[0 0]]
  )
))

; Draw the curve as one long path at the end of the animation.
(def hilbertCurve (\\(time depth centerX centerY width rotation orientation)
  (let [[firstX firstY]|otherPoints] (hilbertPointsAnimated time depth centerX centerY width rotation orientation)
    [(path 'none' 'blue' 5
      [ 'M' firstX firstY | (concatMap (\\[x y] ['L' x y]) otherPoints) ]
    )]
  )
))

(def crossfade (\\(a b t)
  (+
    (* a (- 1.0 t))
    (* b t)
  )
))

(def maxLevels 3)

[
  maxLevels
  (\\slideNumber
    (let level slideNumber
    (let levelPartCount (pow 4 level)
    (let drawingParts (lt level maxLevels)

      [
        (if drawingParts (+ 1 levelPartCount) 1)
        (\\slideMovieNumber
          (if (and drawingParts (= 1 slideMovieNumber))
            [
              'Dynamic'
              0.5
              (\\(slideNumber movieNumber t)
                (let relT (* t 2.0)
                  ['svg' [['opacity' (crossfade 1.0 0.5 relT)]] (hilbertParts 0.5 (- level 1) 1 0 1.0 300 300 400 0 1)]
                )
              )
              (not (= slideNumber 1))
            ]
            (if drawingParts
              (let animationDuration (* 3 (pow 0.5 level))
                [
                  'Dynamic'
                  animationDuration
                  (\\(slideNumber movieNumber t)
                    (let localT (/ (+ (/ (/ t animationDuration) levelPartCount) (* (/ 1.0 levelPartCount) (- movieNumber 2))) 2)
                      (svg (hilbertParts localT level 1 0 1.0 300 300 400 0 1))
                    )
                  )
                  (and (gt slideMovieNumber 5) (le slideMovieNumber levelPartCount))
                ]
              )
              (let animationDuration 9
                [
                  'Dynamic'
                  animationDuration
                  (\\(slideNumber movieNumber t)
                    (let localT (+ 0.5 (/ (/ t animationDuration) 2))
                      (let elements [
                        (hilbertParts localT (- maxLevels 1) 1 0 (earlierLevelOpacity localT) 300 300 400 0 1)
                        (hilbertCurve localT (- maxLevels 1) 300 300 400 0 1)
                      ]
                      (svg (concat elements))
                    ))
                  )
                  false
                ]
              )
            )
          )
        )
      ]
    )))
  )
]
"""

stickFigures =
 """;
; A diagram of a sketch-n-sketch demo w/ audience
;
(let [x0 y0 w h] [60 -22 417! 915!]
(let wstripe (/ w 6!)
(let xoff (+ x0 wstripe)
(let yoff (+ y0 (/ h 4!))
(let minrad (if (< (/ wstripe 7.5!) (/ h 30!)) (/ wstripe 7.5!) (/ h 15!))
(let min (if (< w h) (* 0.6! w) (* 0.6! h))
(let figure
  (\\(x y)
    (let [x1 x2 x3] (map (\\n (+ x (* wstripe n))) [1.2! 1.5! 1.8!])
    (let [y1 yh y2 y3 y4] (map (\\n (+ y (/ (/ h 2!) n))) [4.3! 2.2! 2.8! 1.9! 1.4!])
    (let figline (\\[[a b] [c d]] (line 'black' (/ minrad 2!) a b c d))
      (snoc
        (circle 'black' x2 y1 (/ wstripe 3.75!))
        (map
          figline
          [[[x1 yh] [x1 y2]]
           [[x1 y2] [x3 y2]]
           [[x3 yh] [x3 y2]]
           [[x1 y4] [x1 y3]]
           [[x1 y3] [x3 y3]]
           [[x3 y3] [x3 y4]]
           [[x2 y1] [x2 y3]]]))))))
(let logo
  (\\(x y)
    (let [xl yl wl hl delta] [x y 90 90 6.5]
    (let [xw yh w2 h2] [(+ xl wl) (+ yl hl) (div wl 2) (div hl 2)]
    (let poly (\\pts (polygon 'black' 'none' 0 pts))
      [(rect 'white' xl yl wl hl)
       (poly  [[(+ xl delta) yl] [xw yl] [xw (- yh delta)]])
       (poly  [[xl (+ yl delta)] [xl (- yh delta)] [(- (+ xl w2) delta) (+ yl h2)]])
       (poly  [[(+ xl delta) yh] [(- xw delta) yh] [(+ xl w2) (+ (+ yl h2) delta)]])]))))
  ;
  (let textBoxes
    [ (addAttr (text 160 160 'Laptop on Table') ['font-size' 40])
      (addAttr (text 30 600 'Demonstrators') ['font-size' 40])
      (addAttr (text 550 660 'Audience') ['font-size' 40])
    ]
  ;
  (svg (append textBoxes
    (append
      [(polygon
        '#CD7F32'
        'none'
        0
        [[508 511]
         [497 347]
         [200 354]
         [188 512]
         [171 334]
         [133 287]
         [133 435]
         [110 257]
         [443 250]
         [520 324]])
       (polygon 'none' 'black' 4 [[106 256] [445 251] [524 325] [183 334]])
       (rect 'gray' 230! 184! 103 102)
       (polygon 'gray' 'black' 2 [[250 295] [357 294] [393 312] [282 312]])]
      (append
        (logo 238 190)
        (concatMap
          (\\[x y] (figure (+ x0 x) (+ y0 y)))
          [[-24 245] [-96 140] [325 321] [405 184] [474 298]]))))))))))))))

"""

-- TODO stack overflow for some reason?
--LITTLE_TO_ELM cultOfLambda
clique =
 """;
; A six node clique
;
(let node (\\[x y] (circle 'lightblue' x y 20))
(let edge (\\[[x y] [i j]] (line 'lightgreen' 5 x y i j))
(letrec genpairs
   (\\xs
     (case xs
       ([x y | xx] [[x y] | (append (genpairs (cons x xx)) (genpairs (cons y xx)))])
       ([x] [])
       ([] [])))
(let pts [[200 50] [400 50] [100 223] [200 389] [400 391] [500 223]]
(let nodes (map node pts)
(let pairs (genpairs  pts)
(let edges (map edge pairs)
  (svg (append edges nodes)))))))))

"""

miscShapes =
 """(let [x y] [200 150] (svg [
  (rect '#999999'  50 10 80 130)
  (circle 'lightblue' 300 100 50)
  (ellipse 'orange' 40 280 30 50)
  (polygon 'lightgreen' 'black' 5 [[110 110] [300 110] [x y]])
  (polygon 'lightgreen' 'black' 5 [[110 210] [300 210] [x y]])
  (line 'blue' 4 10 20 300 40)
]))

"""

paths1 =
 """(svg [
  (path_ ['M' 10 10 'H' 90 'V' 90 'H' 10 'L' 10 10 'Z'])
  (path_ ['M' 20 20 'L' 60 20 'L' 60 80 'Z'])
  (path_ ['M' 150 0 'L' 75 200 'L' 225 200 'Z'])
])

"""

paths2 =
 """; Adapted from:
; https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
;
; Turn on the zones to see the Bezier control points.
; Try replacing \"svg\" with \"svgViewBox 200 200\".
;
(svg [
  (path_ ['M' 10 10   'C' 20 20 40 20 50 10])
  (path_ ['M' 70 10   'C' 70 20 120 20 120 10])
  (path_ ['M' 130 10  'C' 120 20 180 20 170 10])
  (path_ ['M' 10 60   'C' 20 80 40 80 50 60])
  (path_ ['M' 70 60   'C' 70 80 110 80 110 60])
  (path_ ['M' 130 60  'C' 120 80 180 80 170 60])
  (path_ ['M' 10 110  'C' 20 140 40 140 50 110])
  (path_ ['M' 70 110  'C' 70 140 110 140 110 110])
  (path_ ['M' 130 110 'C' 120 140 180 140 170 110])
])

"""

paths3 =
 """(svg [
  (path_ ['M' 10 80 'C' 40 10 65 10 95 80 'S' 150 150 180 80])
  (path_ ['M' 10 80 'Q' 95 10 180 80])
  (path_ ['M' 10 80 'Q' 52.5 10 95 80 'T' 180 80])
])

"""

paths4 =
 """; Adapted from:
; https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
;
(svg [
  (addAttr
    (path 'green' 'black' 2
      ['M' 10 315
       'L' 110 215
       'A' 30 50 0 0 1 162.55 162.45
       'L' 172.55 152.45
       'A' 30 50 -45 0 1 215.1 109.9
       'L' 315 10])
    ['opacity' 0.5])
])

"""

paths5 =
 """; Adapted from:
; https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths
;
(svg [
  (path 'green' 'black' 2
    ['M' 80 80 'A' 45 45 0 0 0 125 125 'L' 125 80 'Z'])
  (path 'red' 'black' 2
    ['M' 230 80 'A' 45 45 0 1 0 275 125 'L' 275 80 'Z'])
  (path 'purple' 'black' 2
    ['M' 80 230 'A' 45 45 0 0 1 125 275 'L' 125 230 'Z'])
  (path 'blue' 'black' 2
    ['M' 230 230 'A' 45 45 0 1 1 275 275 'L' 275 230 'Z'])
])

"""

sailBoat =
 """; A sail boat on the ocean
;
; Try mainupulating:
;   - The position of the boat by dragging the sail
;   - The height of the waves by moving the path control points with zones on
;   - The frequency of the waves
;   - The sea level

(def [sealevel amplitude period boatpos] [300 40 200 400])
(def [oceancolor backgroundcolor] [[28 107 160 50] [135 206 250 100]])

(def wave (\\([sx sy] [ex ey] amplitude)
            [ (path oceancolor 'black' 0 
                [ 'M' sx sy 
                  'Q' (+ sx (/ period 4!)) (- sy amplitude) 
                  (+ sx (/ period 2!)) sy
                  'Z'])
              (path backgroundcolor 'black' 0
                  [ 'M' (+ sx (/ period 2!)) sy
                    'Q' (+ sx (* period 0.75!)) (+ sy amplitude)
                    ex ey 
                    'Z' ])]))

(def nodes
  (map2 (\\(a b) [(* a period) b])
        (range 0! (round (/ 3000 period)))
        (repeat (round (/ 4000 period)) sealevel)))

(defrec mkwaves 
  (\\l (case l 
    ([] [])
    ([x] [])
    ([a b | rest] (append (wave a b amplitude) (mkwaves [ b | rest ]))))))

(def backdrop (rect backgroundcolor -400! -400! 2400! 2400!))
(def sun (circle 'yellow' 50 0 70))
(def deepwater (rect oceancolor -400! sealevel 2400! 4000!))
(def quadraticbezier (\\(s c e t) (+ (+ 
    (* (* (- 1 t) (- 1 t)) s) 
    (* (* (* 2 (- 1 t)) t) c)) 
    (* (* t t) e))))

(def tphase (/ (mod boatpos (/ period 2)) (/ period 2)))
(def pickdir (\\(sl amp) (if 
    (< (mod boatpos period) (/ period 2))
      (- sl amp) 
      (+ sl amp))))

(def boat
  (let boaty (quadraticbezier sealevel (pickdir sealevel amplitude) sealevel tphase)
  (let hull (path 'saddlebrown' 'black' 0
    [ 'M' (- boatpos 30) (- boaty 10)
      'C' (- boatpos 30) (+ boaty 15)
      (+ boatpos 30) (+ boaty 15)
      (+ boatpos 30) (- boaty 10)
      'Z'])
  (let mast (rect 'saddlebrown' (+ boatpos 10) (- boaty 60) 5 50)
  (let sail (rect 'beige' (- boatpos 15!) (- boaty 50!) 50 30)
  [mast hull sail])))))

(svg 
  (concat [
    [ backdrop sun deepwater ]
    (mkwaves nodes)
    boat]))

"""

eyeIcon =
 """; An eye icon
; Recreation of https://commons.wikimedia.org/wiki/Category:SVG_eye_icons#/media/File:Eye_open_font_awesome.svg
;
; Try unfreezing:
;   glintr, and manipulating the glint control points
;   cornear, and manipulating the cornea radius
;   glintWidth, and manipulating the glint control points
;   Any of the outer/inner parameters, and seeing what happens!
;
(def [outerStartx outerStarty innerStartx innerStarty] [16! 240! 50 256!])
(def [outerWidth innerWidth outerHeight innerHeight sharpness] [480 412 -60 0 16])
(def [corneax corneay cornear glintr glintWidth] [256! 216! 120! 50! 24])
(def midline 256!)
(def outerBorder
  (path
    'black'
    'black'
    0
    ['M' outerStartx outerStarty
     'Q' midline outerHeight (+ outerStartx outerWidth) outerStarty
     'Q' (+ (+ outerStartx outerWidth) sharpness) 256! (+ outerStartx outerWidth) (+ outerStarty 32!)
     'Q' midline (- 512! outerHeight) outerStartx (+ outerStarty 32!)
     'Q' (- outerStartx sharpness) 256! outerStartx outerStarty
     'Z']))
(def innerBorder
  (path
    'white'
    'black'
    0
    ['M' innerStartx innerStarty
     'Q' midline innerHeight (+ innerStartx innerWidth) innerStarty
     'Q' midline (- 512! innerHeight) innerStartx innerStarty
     'Z']))
(def cornea (circle 'black' corneax corneay cornear))
(def glint
  (path
    'white'
    'black'
    0
    ['M' corneax (- corneay (+ glintr glintWidth))
     'A' (/ glintWidth 2!) (/ glintWidth 2!) 0 0 1 corneax (- corneay glintr)
     'A' glintr glintr 0 0 0 (- corneax glintr) corneay
     'A' (/ glintWidth 2!) (/ glintWidth 2!) 0 0 1 (- corneax (+ glintr glintWidth)) corneay
     'A' (+ glintr glintWidth) (+ glintr glintWidth) 0 0 1 corneax (- corneay (+ glintr glintWidth))
     'Z']))
(svg  [outerBorder innerBorder cornea glint])

"""

wikimedia =
 """; Wikimedia Logo
; Recreation of https://upload.wikimedia.org/wikipedia/commons/8/81/Wikimedia-logo.svg
 
; The white objects are an example of using masks as opposed to paths to create
; more complicated forms, such as the green 'wings' and broken ring of this logo.
 
(def [greenr innerBluer outerBluer] [110! 134! 180!])
(def [wedgeTheta barWidth barHeight] [(/ 3.14159! 4!) 32 150])
(def [dotRed wingGreen ringBlue] ['#900' '#396' '#069'])
(def [centerx centery] [256! 256!])
(def greenCirc (circle wingGreen centerx centery greenr))
(def whiteRing (circle 'white' centerx centery innerBluer))
(def blueCirc (circle ringBlue centerx centery outerBluer))
(def rightPtx (+ centerx (* outerBluer (sin wedgeTheta))))
(def leftPtx (- centery (* outerBluer (sin wedgeTheta))))
(def pty (- (- centery 16) (* outerBluer (cos wedgeTheta))))
(def whiteWedge 
    (path 'white' 'black' 0 
        [ 'M' centerx (- centery 16)
          'L' rightPtx pty
          'A' outerBluer outerBluer 0 0 0 leftPtx pty
          'Z']))
(def whiteBar (rect 'white' (- centerx (/ barWidth 2!)) (- centery 32!) barWidth barHeight))
(def redDot (circle '#900' centerx 128! 64!))

(svg [blueCirc whiteRing greenCirc whiteWedge whiteBar redDot])

"""

haskell =
 """; Haskell.org Logo
; SVG version of https://www.haskell.org/static/img/logo.png?etag=rJR84DMh
;
; Try making a slider for the bend amount to adjust that parameter indirectly.
;
(def [wedgeWidth lambdaWidth equalsWidth] [120 120 90])
(def [wedgePos lambdaPos equalsPos] [0! 170 440])
(def [totalHeight totalWidth] [512! 752!])
(def bend (/ 3.14159! 5.3))
(def equalsSep 40)
(def [wedgeColor lambdaColor equalsColor] 
        [[69 58 98 100]
         [97 82 138 100]
         [143 78 139 100]])
(def tan (\\theta (/ (sin theta) (cos theta))))
(def leftWedge (path wedgeColor 'black' 0
    [ 'M' wedgePos 0!
      'L' (+ wedgePos (* (/ totalHeight 2!) (tan bend))) (/ totalHeight 2!)
      'L' wedgePos totalHeight
      'L' (+ wedgePos wedgeWidth) totalHeight
      'L' (+ wedgePos (+ wedgeWidth (* (/ totalHeight 2!) (tan bend))))
          (/ totalHeight 2!)
      'L' (+ wedgeWidth wedgePos) 0!
      'Z']))
(def lambda (path lambdaColor 'black' 0
    [ 'M' lambdaPos 
          0!
      'L' (+ lambdaPos (* (/ totalHeight 2!) (tan bend))) 
          (/ totalHeight 2!)
      'L' lambdaPos 
          totalHeight
      'L' (+ lambdaPos lambdaWidth) 
          totalHeight
      'L' (+ (+ lambdaPos (* (/ totalHeight 2!) (tan bend))) (/ lambdaWidth 2!)) 
          (+ (/ totalHeight 2!) (/ lambdaWidth (* 2! (tan bend))))
      'L' (+ lambdaPos (* totalHeight (tan bend)))
          totalHeight
      'L' (+ lambdaPos (+ lambdaWidth (* totalHeight (tan bend))))
          totalHeight
      'L' (+ lambdaPos lambdaWidth)
          0!
      'Z']))
(def equals 
  [ (path equalsColor 'black' 0
      [ 'M' equalsPos
            (- (- (/ totalHeight 2!) (* equalsSep 0.25!)) (* equalsWidth (cos bend)))
        'L' totalWidth
            (- (- (/ totalHeight 2!) (* equalsSep 0.25!)) (* equalsWidth (cos bend)))
        'L' totalWidth
            (- (/ totalHeight 2!) (* equalsSep 0.25!))
        'L' (+ equalsPos (* equalsWidth (sin bend)))
            (- (/ totalHeight 2!) (* equalsSep 0.25!))
        'Z'])
    (path equalsColor 'black' 0
      [ 'M' (+ equalsPos (+ (* equalsWidth (sin bend)) (* equalsSep (tan bend))))
            (+ (/ totalHeight 2!) (* equalsSep 0.75!))
        'L' totalWidth
            (+ (/ totalHeight 2!) (* equalsSep 0.75!))
        'L' totalWidth
            (+ (+ (/ totalHeight 2!) (* equalsSep 0.75!)) (* equalsWidth (cos bend)))
        'L' (+ equalsPos (+ (* 2! (* equalsWidth (sin bend))) (* equalsSep (tan bend))))
            (+ (+ (/ totalHeight 2!) (* equalsSep 0.75!)) (* equalsWidth (cos bend)))
        'Z'])])
(svg (append [leftWedge lambda] equals))
"""

matrices =
 """; Definitions for 2D matrices and transform application
;
; Similar to the SVG transform operation
; See https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
;
(def [theta tx ty m s] [(/ 3.14159! 4) 75 75 1.25 2])
(defrec rmult_ (\\(r v) (case [r v]
  ([ [] x ] 0)
  ([ x [] ] 0)
  ([ [a | aa] [b | bb] ] (+ (* a b) (rmult_ aa bb))))))
(defrec mmult (\\(m v) (case m
  ([] [])
  ([r|rest] (if (= (len r) (len v)) (cons (rmult_ r v) (mmult rest v)) [])))))
(defrec toPts (\\l (case l
  ([] [])
  ([ [x y k] | rest ] (cons [x y] (toPts rest))))))
(defrec toPath_ (\\l (case l
  ([] [])
  ([ [x y] | rest ] (append [ 'L' x y ] (toPath_ rest))))))
(def toPath (\\l (case l
  ([] [])
  ([ [x1 y1] | rest ] (path 'lightblue' 'gray' 1 (append [ 'M' x1 y1 | (toPath_ rest)] ['Z']))))))
(def id [ [1! 0! 0!] [0! 1! 0!] [0! 0! 1!] ])
(def translatert (map (mmult [ [1! 0! tx] [0! 1! 0!] [0! 0! 1!]])))
(def translatedn (map (mmult [ [1! 0! 0!] [0! 1! ty] [0! 0! 1!]])))
(def rotate (map (mmult [ [(cos theta) (* -1! (sin theta)) 0!] [(sin theta) (cos theta) 0!] [0! 0! 1!]])))
(def shear (map (mmult [ [1! m 0!] [0! 1! 0!] [0! 0! 1!]])))
(def scale (map (mmult [ [s 0! 0!] [0! s 0!] [0! 0! 1!] ])))
(def ps [ [-25! -25! 1!] [-25! 25! 1!] [25! 25! 1!] [25! -25! 1!] ])
(def square1 (toPath (toPts (translatedn (translatert ps)))))
(def square2 (toPath (toPts (translatedn (translatert (translatert ps))))))
(def rotsquare (toPath (toPts (translatert (translatert (translatert (translatedn (rotate ps))))))))
(def shearsquare (toPath (toPts (translatert (translatert (translatert (translatert (translatedn (shear ps)))))))))
(def scalesquare (toPath (toPts (translatert (translatert (translatert (translatert (translatert (translatert (translatedn (scale ps)))))))))))
(svg [square1 square2 rotsquare shearsquare scalesquare])
"""

rotTest =
 """
(def [x y w h] [257 54 152 261])
(def rot 38.166250526006905)
(def colorNum 100)

(def [x_ y_] [(+ x (/ w 2)) (+ y (/ h 2))])

; thin wrapper v1
(def transform
  (spaces
     ['translate' (parens (spaces [(toString x_) (toString y_)]))
      'rotate'    (parens (toString rot))
      'translate' (parens (spaces [(toString (neg x_)) (toString (neg y_))]))
   ]))

; thin wrapper v2
(def transform2
  (+ 'rotate' (parens (spaces [(toString rot) (toString x_) (toString y_)]))))

; specific transform encoding
(def transform3 [['rotate' rot x_ y_]])

(def r (rotate (rect colorNum x y w h) rot x_ y_))

(def e (rotate (ellipse 240 x_ y_ 10 20) -10 x_ y_))

(svg [r e])

"""

interfaceButtons =
 """(def [w h depth shadow r] [120 22.5 4 0.5 2])
(def shadowcol [180 180 180 1])

(def draw (\\(topcolor botcolor offset)

  (let tbbox  (rect topcolor r offset (- w (* 2! r)) h)
  (let lrbox  (rect topcolor 0! (+ r offset) w (- h (* 2! r)))
  (let tlcirc (circle topcolor r (+ r offset) r)
  (let trcirc (circle topcolor (- w r) (+ r offset) r)
  (let blcirc (circle topcolor r (+ (- h r) offset) r)
  (let brcirc (circle topcolor (- w r) (+ (- h r) offset) r)

  (let btbbox  (rect botcolor r (- h r) (- w (* 2! r)) depth)
  (let blrbox  (rect botcolor 0! (- h r) w (- depth r))
  (let belcirc (circle botcolor r (- (+ h depth) (* 2! r)) r)
  (let bercirc (circle botcolor (- w r) (- (+ h depth) (* 2! r)) r)

  (let stbbox  (rect shadowcol r (+ shadow (- h r)) (- w (* 2! r)) depth)
  (let slrbox  (rect shadowcol 0! (+ shadow (- h r)) w (- depth r))
  (let selcirc (circle shadowcol r (+ shadow (- (+ h depth) (* 2! r))) r)
  (let sercirc (circle shadowcol (- w r) (+ shadow (- (+ h depth) (* 2! r))) r)

  (let btop    [tbbox lrbox tlcirc trcirc blcirc brcirc]
  (let bbot    [btbbox blrbox belcirc bercirc]
  (let bshadow (if (= offset 0) [stbbox slrbox selcirc sercirc] [])

    ; NOTE: not calling (addAttr ... ['preserveAspectRatio' 'none'])
    (svgViewBox 120
      (+ depth (+ shadow (- h r)))
      (concat [bshadow bbot btop]))

)))))))))))))))))))

(def hilitedBot [198 77 64 1])

(def raisedButton      (draw [231 76 60 1] [192 57 43 1] 0))
(def highlightedButton (draw [233 94 80 1] hilitedBot 0))
(def depressedButton   (draw [233 94 80 1] hilitedBot 1.1))
(def disabledButton    (draw [236 112 99 1] [205 97 85 1] 0))

(def raisedButton2      raisedButton)
(def highlightedButton2 highlightedButton)
(def depressedButton2   depressedButton)
(def disabledButton2    disabledButton)

(def raisedButton3      (draw 'gray' hilitedBot 0))
(def highlightedButton3 (draw 'lightgray' hilitedBot 0))
(def depressedButton3   (draw 'lightgray' hilitedBot 1.1))
(def disabledButton3    (draw 470 360 0))

(def kind 1{1-3})
(def state 1{1-4})

(if (= kind 1)
  (if (= state 1) raisedButton
  (if (= state 2) highlightedButton
  (if (= state 3) depressedButton
    disabledButton)))
(if (= kind 2)
  (if (= state 1) raisedButton2
  (if (= state 2) highlightedButton2
  (if (= state 3) depressedButton2
    disabledButton2)))
; else
  (if (= state 1) raisedButton3
  (if (= state 2) highlightedButton3
  (if (= state 3) depressedButton3
    disabledButton3)))
))

"""

barGraph =
 """; A Bar Graph
;
; Try:
;   - Manipulating the height of the bars
;   - Manipulating the spacing between the bars
;   - Changing the X Offset with the bottom slider
;   - Adding and removing data points
;   - Changing the captions
;
; When you're done editing, change the doneEditing variable to true to
; get an SVG that's ready to be embedded in a web page!
(def doneEditing false)

; Each data point specifies a single bar as [XValue YValue]
(def data [[1! 5!] [2! 1!] [3! 3!] [4! 2!] [5! 4!] [7! 3!]])

; Relevant variables
(def [title xcaption ycaption] ['Bar Graph' 'X Axis' 'Y Axis'])
(def [totht totwd barspacing numy] [250! 450! 60 5])
(def [barwd axiswd] [50 4])
(def [barcol axiscol] ['lightblue' 'gray'])

(def [xbegin windowslider] (hSlider true 100! 500! 500! 0! 10! 'X Offset: ' 0))

; If you would like to specify the X Offset without the slider, uncomment
; the below line and set it appropriately.
;(def xbegin 0)

(def [lowerx upperx] [xbegin (+ xbegin (/ totwd barspacing))])
(def [x0 y0] [100! 100!])

(def [textht textwd] [18! 4!])

(def bar (\\[val freq] (if (< val upperx) (if (< lowerx val)
                [(rect barcol (+ x0 (- (* barspacing (- val xbegin)) (* 0.5! barwd)))
                                    (+ y0 (- totht (* freq (/ totht numy))))
                                    barwd
                                    (* freq (/ totht numy)) )] []) []) ) )

(def xaxis (line axiscol axiswd (- x0 (* 0.5! axiswd))
                                (+ y0 totht)
                                (+ x0 totwd)
                                (+ y0 totht) ) )

(def yaxis (line axiscol axiswd x0 (- y0 (* 0.5! (/ totht numy)))
                                x0 (+ y0 totht) ) )

(def labelx (\\val (text (+ x0 (- (* barspacing (- val xbegin)) textwd))
                        (+ y0 (+ (* 1.5! textht) totht))
                        (toString val) ) ) )

(def labely (\\freq (text (- x0 20!)
                         (+ y0 (+ (* 0.5! textht) (- totht (* freq (/ totht numy)))))
                         (toString freq) ) ) )

(def titlelabel (text (+ x0 (/ totwd 2!))
                      (- y0 (/ totht numy))
                      title) )

(def xlabel (text (+ x0 (/ totwd 2!))
                  (+ y0 (+ (* 3! textht) totht))
                  xcaption) )
(def ylabel (text (- x0 80!) (+ y0 (/ totht 2!)) ycaption) )

(def xs (range lowerx (- upperx 1!)))
(def ys (range 0! numy))

(def bars (concat (map bar data)))
(def axes [xaxis yaxis])
(def xlabels (map labelx xs))
(def ylabels (map labely ys))
(def names [titlelabel xlabel ylabel])

(if doneEditing (svgViewBox (+ (* 2! x0) totwd) (+ y0 (+ (* 3! textht) totht)) (concat [bars axes xlabels ylabels names]))
                (svg (concat [bars axes xlabels ylabels names windowslider])) )
"""

thawFreeze =
 """
# unannotated-numbers: n!

; Set [unannotated-numbers] to [n?] or [n!] to declare that
; unannotated literals be thawed or frozen, respectively.
;
; By default, this option is set to [n?].

(svg [(rect 'maroon' 100? 15 200! 50)])

"""

dictionaries =
 """(def d (empty))
(def d1 (insert 'a' 3 d))
(def d2 (insert 'b' 4 d1))
(def d3 (remove 'a' d2))

(def getWithDefault (\\(key default dict)
  (let value (get key dict)
  (typecase value
    (Null default)
    (_ value)))
))

(blobs [
    [(text 50 30  (toString d2))]
    [(text 50 50  (toString d3))]
    [(text 50 70  (toString (get 'b' d3)))]
    [(text 50 90  (toString (get 'a' d3)))]
    [(text 50 110 (toString (getWithDefault 'a' 'default' d3)))]
])

"""

-- LITTLE_TO_ELM deleteBoxes
cover =
 """; Logo for Cover
; see https://github.com/florence/cover

(def size 300!)
(def line 10!)
(def h (/ size 2.6548672566371683))
(def w (- (* 2! h) (* 2! line)))

(def m (/ size 2!))

(def x (- m (/ w 2!)))
(def y (- m (+ (/ line 2!) (/ w 2!))))

(def x2 (- x (+ w line)))
(def y2 (- x (+ w (* 2.5! line))))

(def x3 (+ x (+ w line)))
(def y3 (+ x (+ w (* 1.5! line))))

(def top (\\(x y)
 (rect 'red' x y w (- h line))))

(def sw (- h (* 1.5! line)))

(def bottom (\\(x y)
  (rect 'blue' x (+ y h) sw (- h line))))

(def bottoma (\\(x y) (bottom x y)))
(def bottomb (\\(x y) (bottom (+ sw (+ x line)) y)))

(def rot 45)

['svg'
 [['viewBox' (+ (+ (+ '0 0 ' (toString size)) ' ') (toString size))]]
 [
  (square 'white' 0! 0! size)

  (rotate (top x y)   rot m m)
  (rotate (bottoma x y) rot m m)
  (rotate (bottomb x y) rot m m)

  (rotate (top x2 y)   rot m m)
  (rotate (bottoma x2 y) rot m m)
  (rotate (bottomb x2 y) rot m m)

  (rotate (top x y2)   rot m m)
  (rotate (bottoma x y2) rot m m)
  (rotate (bottomb x y2) rot m m)

  (rotate (top x3 y)   rot m m)
  (rotate (bottoma x3 y) rot m m)
  (rotate (bottomb x3 y) rot m m)

  (rotate (top x y3)   rot m m)
  (rotate (bottoma x y3) rot m m)
  (rotate (bottomb x y3) rot m m)
]]

"""

poppl =
 """; Logo for POP-PL
; see https://github.com/florence/pop-pl

(def m 'M')
(def l 'L')
(def c 'C')
(def z 'Z')

(def ltopWidth 29!)
(def ltopHeight 63!)
(def xstart 131!)
(def ystart 63!)
(def stethx 31!)
(def stethy 7!)
(def cr2Control -0.1769993052254364)
(def cr2x (* cr2Control ltopWidth))
(def cr2y (* cr2Control ltopHeight))
(def lpath
  [m (- xstart stethx) (- ystart stethy)
   c (+ xstart -12) (+ ystart -19)
     (+ cr2x xstart) (+ cr2y ystart)
     xstart ystart
   l (+ xstart ltopWidth) (+ ystart ltopHeight)
   ])

(def axstart  (+ xstart ltopWidth))
(def aystart (+ ystart ltopHeight))
(def ascale 1.9534135150166867!)
(def ax (* ascale ltopWidth))
(def ay (* ascale ltopHeight))
(def bx 18!)
(def armpath
  [m axstart aystart
   c (+ xstart 71) (+ ystart 94)
     (+ xstart 90) (+ ystart 142)
     (+ axstart ax) (+ aystart ay)
   c (+ xstart 63) (+ ystart 190)
     (+ xstart 74) (+ ystart 188)
     (- (+ axstart ax) bx) (+ aystart ay)])

(def lwidth 5)


(def nub
  (circle 'black' (- (+ axstart ax) bx) (+ aystart ay) (* lwidth 2!)))

(def small (* lwidth 2.1))
(def scope1
  (circle 'black' (- xstart stethx) (- ystart stethy) (+ small lwidth)))
(def scope2
  (circle 'white' (- xstart stethx) (- ystart stethy) small))

['svg'
 [['viewBox' '0 0 300 300']]
 [(square 'white' 0! 0! 300!)
      (path 'none' 'black' lwidth lpath)
      (path 'none' 'black' lwidth armpath)
      nub
      (addAttr (path 'white' 'black' lwidth armpath)
               ['transform' (+ (+ 'matrix(-1 0 0 1 ' (toString (* 2 axstart)))
                               ' 0)')])
      (addAttr nub
               ['transform' (+ (+ 'matrix(-1 0 0 1 ' (toString (* 2 axstart)))
                               ' 0)')])
      scope1
      scope2
]]

"""

bezier =
 """; Animated Bezier Curves
; Recreating https://www.jasondavies.com/animated-bezier/
;
; Bezier functions
;
(def linbez (\\(t p1 p2)
  (+ p1 (* t (- p2 p1))) ) )
(def quadbez (\\(t p1 p2 p3)
  (+ (* (- 1 t) (linbez t p1 p2)) (* t (linbez t p2 p3))) ) )
(def cubez (\\(t p1 p2 p3 p4)
  (+ (* (- 1 t) (quadbez t p1 p2 p3)) (* t (quadbez t p2 p3 p4))) ) )
(def quarbez (\\(t p1 p2 p3 p4 p5)
  (+ (* (- 1 t) (cubez t p1 p2 p3 p4)) (* t (cubez t p2 p3 p4 p5))) ) )
;
; Point definitions
;
(def [t tslider] (hSlider false 50! 450! 300! 0! 1! 't: ' 0.25))
(def [linx1 liny1 linx2 liny2] [50 200 100 50])
(def [quadx1 quady1 quadx2 quady2 quadx3 quady3] 
     [(+ linx1 150!) liny1 (+ linx2 150!) liny2 300 200])
(def [cux1 cuy1 cux2 cuy2 cux3 cuy3 cux4 cuy4] 
     [(+ quadx1 150!) liny1 (+ quadx2 150!) liny2 (+ quadx3 150!) quady3 500 50])
(def [qux1 quy1 qux2 quy2 qux3 quy3 qux4 quy4 qux5 quy5]
     [(+ cux1 150!) liny1 (+ cux2 150!) liny2 (+ cux3 150!) quady3 (+ cux4 150!) cuy4 700 200])
;
; Style Definitions
;
(def [linwd col1 col2 col3 col4] [3 'gray' 'lightblue' 'green' 'lightgreen'])
(def ptcirc (\\(cx cy) (circle 'red' cx cy 5!)))
;
; Curve Definitions
;
(def linCurve
  (let [midx midy] [(linbez t linx1 linx2) (linbez t liny1 liny2)]
  [ (path 'none' col1 linwd
      [ 'M' linx1 liny1
        'L' linx2 liny2 ] )
    (path 'none' 'red' linwd
      [ 'M' linx1 liny1
        'L' midx midy ] )
    (ptcirc midx midy)  ] ) )
(def quadCurve
  (let [midx1 midy1 midx2 midy2] [(linbez t quadx1 quadx2) (linbez t quady1 quady2)
                                  (linbez t quadx2 quadx3) (linbez t quady2 quady3)]
  (let [px py] [(quadbez t quadx1 quadx2 quadx3) (quadbez t quady1 quady2 quady3)]
  [ (path 'none' col1 linwd
      [ 'M' quadx1 quady1
        'L' quadx2 quady2 ] )
    (path 'none' col1 linwd
      [ 'M' quadx2 quady2
        'L' quadx3 quady3 ] )
    (path 'none' col2 linwd
      [ 'M' midx1 midy1
        'L' midx2 midy2 ] )
    (path 'none' 'red' linwd
      [ 'M' quadx1 quady1
        'Q' quadx2 quady2
            quadx3 quady3 ] )
    (ptcirc px py)    ] ) ) )
(def cuCurve
  (let [midx1 midy1 midx2 midy2 midx3 midy3]
       [(linbez t cux1 cux2) (linbez t cuy1 cuy2)
        (linbez t cux2 cux3) (linbez t cuy2 cuy3)
        (linbez t cux3 cux4) (linbez t cuy3 cuy4)]
  (let [dubmidx1 dubmidy1 dubmidx2 dubmidy2]
       [(linbez t midx1 midx2) (linbez t midy1 midy2)
        (linbez t midx2 midx3) (linbez t midy2 midy3)]
  (let [px py] [(cubez t cux1 cux2 cux3 cux4) (cubez t cuy1 cuy2 cuy3 cuy4)]
  [ (path 'none' col1 linwd
      [ 'M' cux1 cuy1
        'L' cux2 cuy2 ] )
    (path 'none' col1 linwd
      [ 'M' cux2 cuy2
        'L' cux3 cuy3 ] )
    (path 'none' col1 linwd
      [ 'M' cux3 cuy3
        'L' cux4 cuy4 ] )
    (path 'none' col2 linwd
      [ 'M' midx1 midy1
        'L' midx2 midy2 ] )
    (path 'none' col2 linwd
      [ 'M' midx2 midy2
        'L' midx3 midy3 ] )
    (path 'none' col3 linwd
      [ 'M' dubmidx1 dubmidy1
        'L' dubmidx2 dubmidy2 ] )
    (path 'none' 'red' linwd
      [ 'M' cux1 cuy1
        'C' cux2 cuy2
            cux3 cuy3
            cux4 cuy4 ] )
    (ptcirc px py)            ] ) ) ) )

(def quCurve
  (let [midx1 midy1 midx2 midy2 midx3 midy3 midx4 midy4]
       [(linbez t qux1 qux2) (linbez t quy1 quy2)
        (linbez t qux2 qux3) (linbez t quy2 quy3)
        (linbez t qux3 qux4) (linbez t quy3 quy4)
        (linbez t qux4 qux5) (linbez t quy4 quy5)]
  (let [dubmidx1 dubmidy1 dubmidx2 dubmidy2 dubmidx3 dubmidy3]
       [(linbez t midx1 midx2) (linbez t midy1 midy2)
        (linbez t midx2 midx3) (linbez t midy2 midy3)
        (linbez t midx3 midx4) (linbez t midy3 midy4)]
  (let [trimidx1 trimidy1 trimidx2 trimidy2]
       [(linbez t dubmidx1 dubmidx2) (linbez dubmidy1 dubmidy2)
        (linbez t dubmidx2 dubmidx3) (linbez dubmidy2 dubmidy3)]
  (let [px py] [(quarbez t qux1 qux2 qux3 qux4 qux5) (quarbez t quy1 quy2 quy3 quy4 quy5)]
  [ (path 'none' col1 linwd
      [ 'M' qux1 quy1
        'L' qux2 quy2 ] )
    (path 'none' col1 linwd
      [ 'M' qux2 quy2
        'L' qux3 quy3 ] )
    (path 'none' col1 linwd
      [ 'M' qux3 quy3
        'L' qux4 quy4 ] )
    (path 'none' col1 linwd
      [ 'M' qux4 quy4
        'L' qux5 quy5 ] )
    (path 'none' col2 linwd
      [ 'M' midx1 midy1
        'L' midx2 midy2 ] )
    (path 'none' col2 linwd
      [ 'M' midx2 midy2
        'L' midx3 midy3 ] )
    (path 'none' col2 linwd
      [ 'M' midx3 midy3
        'L' midx4 midy4 ] )
    (path 'none' col3 linwd
      [ 'M' dubmidx1 dubmidy1
        'L' dubmidx2 dubmidy2 ] )
    (path 'none' col3 linwd
      [ 'M' dubmidx2 dubmidy2
        'L' dubmidx3 dubmidy3 ] )
    (path 'none' col4 linwd
      [ 'M' trimidx1 trimidy1
        'L' trimidx2 trimidy2 ] )
    (ptcirc px py)            ] ) ) ) ) )
;
; Putting it all together
;
(svg (concat [linCurve quadCurve cuCurve tslider]))
"""

-- LITTLE_TO_ELM surveyResultsTriBubbles
-- LITTLE_TO_ELM surveyResultsTriHist
surveyResultsTriHist2 =
 """
; Interactive charts that show results from user study:
; http://ravichugh.github.io/sketch-n-sketch/blog/03-user-study-videos.html

; counts: [ A/B C/A C/B CI-Intervals ]

(def ferrisCounts [
  [ 3 14  2  5  1 ]
  [ 0  3  1 11 10 ]
  [ 1  3  4  9  8 ]
  [ [-0.92 0.01] [0.59 1.47] [0.25 1.23] ]
])

(def keyboardCounts [
  [ 0  5  3 10  7 ]
  [ 0  1  5 14  5 ]
  [ 0  2  2  9 12 ]
  [ [0.26 1.18] [0.59 1.21] [0.73 1.57] ]
])

(def tesselationCounts [
  [ 0  7  9  6  3 ]
  [ 1  0  8 11  5 ]
  [ 1  0  4 13  7 ]
  [ [-0.20 0.64] [0.34 1.10] [0.53 1.32] ]
])

(def maxDataPoint 14!)

(def sum (\\ns
  (foldr
    (\\([weight n] acc) (+ (* n weight) acc))
    0 (zip [0 1 2 3 4] ns))))

(def [iRot jRot kRot]                   [0! -120! 120!])
(def [iRevCounts jRevCounts kRevCounts] [id id reverse])
(def [iRevAvg jRevAvg kRevAvg]          [id id (\\n (+ (neg (- n 2!)) 2!))])
(def [iRevBound jRevBound kRevBound]    [(\\n (+ 2! n)) (\\n (+ 2! n)) (\\n (+ 2! (neg n)))])

(def numUsers 25!)
(def numBins 5!)
(def slices (- numBins 0!))
(def shift  (\\idx (+ idx 0.5!)))

(def sideLen 90{20-200})
(def sidePad 13!{0-50})
(def tickLen 5!{1-10})
(def edgeWidth 1!{0-4})
(def levelWidth 1.0!{0.0-2.1})
(def dotSize 5!{3-10})
(def barSize 16!{1-20})
(def intWidth 8!{1-30})
(def intTicks 0!{-3-15})
(def fontSize 20!{10-40})
(def showAvgs (let showAvgs_ 0.7{0.1-1.0} (< showAvgs_ 0.5!)))
(def showTicks (let showTicks_ 0.7{0.1-1.0} (< showTicks_ 0.5!)))

(def [aUp   aLeft]  [-5!{-20-50}  25!{0-50}])
(def [bUp   bRight] [aUp          98!{0-150}])
(def [cDown cLeft]  [115!{50-200} 10!{0-30}])

(def halfLen (/ sideLen 2!))

(def tri (\\(cx cy [iCounts jCounts kCounts ciIntervals])
  (let [iSum jSum kSum] (map sum [iCounts jCounts kCounts])
  (let [iAvg jAvg kAvg] [(/ iSum numUsers) (/ jSum numUsers) (/ kSum numUsers)]
  (let [iBounds jBounds kBounds] ciIntervals
  (let x0    (- cx halfLen)
  (let x1    (+ cx halfLen)
  (let y     (- cy (* (+ sideLen (* 2! sidePad)) (/ (sqrt 3!) 6!)))
  (let dx    (/ (- x1 x0) slices)
  (let xi    (\\i (+ x0 (* (shift i) dx)))
  (let yn    (\\n (- y (* n tickLen)))
  (let edge  (line 'gray' edgeWidth)
  (let tick  (\\x (circle 'gray' x y 3!))
  (let bar   (line 'lightblue' barSize)
  (let dot   (\\x (circle 'goldenrod' x y dotSize))
  (let level (\\i (let yLevel (- y (* i tickLen))
                 (line 'white' levelWidth x0 yLevel x1 yLevel)))
  (let label (\\(x y s)
    (addAttr (text x y s) ['font-size' (+ (toString fontSize) 'pt')]))
  (let edges
    [ (rotate (edge x0 y x1 y) iRot cx cy)
      (rotate (edge x0 y x1 y) jRot cx cy)
      (rotate (edge x0 y x1 y) kRot cx cy)
    ]
  (let tickmarks
    (let foo (\\rot
      (map (\\i (rotate (tick (+ (+ x0 (/ dx 2!)) (* i dx))) rot cx cy))
           (zeroTo slices)))
    (concatMap foo [iRot jRot kRot]))
  (let levels
    (let foo (\\rot
      (map (\\i (rotate (level i) rot cx cy))
           (range 1! maxDataPoint)))
    (concatMap foo [iRot jRot kRot]))
  (let averages
    [ (rotate (dot (xi (iRevAvg iAvg))) iRot cx cy)
      (rotate (dot (xi (jRevAvg jAvg))) jRot cx cy)
      (rotate (dot (xi (kRevAvg kAvg))) kRot cx cy)
    ]
  (let intervals
    (let draw (\\(revBound [ciMin ciMax] rot)
      [ (rotate (line 'red' intWidth
           (xi (revBound ciMin)) (- y (* intTicks tickLen))
           (xi (revBound ciMax)) (- y (* intTicks tickLen))) rot cx cy) ])
    (concat [
      (draw iRevBound iBounds iRot)
      (draw jRevBound jBounds jRot)
      (draw kRevBound kBounds kRot)
    ]))
  (let labels
    [ (label (- x0 aLeft) (- y aUp) 'A')
      (label (+ x0 bRight) (- y bUp) 'B')
      (label (- (+ x0 (/ (- x1 x0) 2!)) cLeft) (+ y cDown) 'C')
    ]
  (concat [
    (flip mapi (iRevCounts iCounts) (\\[i n]
      (rotate (bar (xi i) y (xi i) (yn n)) iRot cx cy)))
    (flip mapi (jRevCounts jCounts) (\\[i n]
      (rotate (bar (xi i) y (xi i) (yn n)) jRot cx cy)))
    (flip mapi (kRevCounts kCounts) (\\[i n]
      (rotate (bar (xi i) y (xi i) (yn n)) kRot cx cy)))
    levels
    edges
    intervals
    (if showTicks tickmarks [])
    (if showAvgs averages [])
    labels
  ]
))))))))))))))))))))))))

(def [cx0 cy0] [180!{0-200} 130!{0-200}])
(def sep 216!{100-300})

(svg (concat
  (mapi
    (\\[i countsi] (tri cx0 (+ cy0 (* i sep)) countsi))
    [ferrisCounts keyboardCounts tesselationCounts])))

"""

equiTri =
 """
; Equilateral Triangles
; (derived in terms of nStar, rather than nGon)

(def tri (\\(c x y sideLen rot)
  (let len1 (* sideLen (/ 2! 3!))
  (let len2 (* sideLen (/ 1! 3!))
  (nStar c 'none' 0 3! len1 len2 rot x y)))))

(svg [
  (tri 'darkblue'
    150 150
    100
    0)
  (tri 'lightblue'
    200 300
    50
    10)
])

"""

gridTile =
 """
(def grid (\\(x0 y0 w h n m)
  (let [xw yh] [(+ x0 w) (+ y0 h)]
  (let [rowH colW] [(/ h n) (/ w m)]
  (let box (rect 460 x0 y0 w h)
  (let rows
    (map (\\i (let yi (+ y0 (mult i rowH))
             (line 'black' 2 x0 yi xw yi)))
         (range 0! n))
  (let cols
    (map (\\i (let xi (+ x0 (mult i colW))
             (line 'black' 2 xi y0 xi yh)))
         (range 0! n))
  (concat [[box] rows cols]))))))))

(def [x0 y0 w h n m] [100 100 325 285 10! 10!])

(def blob
  (ellipse 196 (+ x0 (/ w 2!))
               (+ y0 (/ h 2!))
               (- (/ w 2!) (/ w m))
               (- (/ h 2!) (/ h n))))

(svg (append
  (grid x0 y0 w h n m)
  [blob]
))

"""

lilliconP =
 """
(def [x0 x1 x2 x3 x4] [20 80 150 240 380])
(def [y0 y1 y2 y3]    [20 125 296 424])

(def yRightmostPt (+ y0 (/ (- y2 y0) 2!)))
(def delta 50!) ; TODO

(def theP
  (path 'black' 'none' 0
    ['M' x0 y3
     'C' x1 376 x1 216 x1 y1
     'C' 80 60 134 y0 x3 y0
     'C' (- x4 delta) y0 x4 (+ y0 delta) x4 yRightmostPt
     'C' x4 (- y2 delta) (- x4 delta) y2 x3 y2
     'L' x2 y2
     'L' x2 y3
     'L' x0 y3
    ]))

(def highlights
  (let r 10!
  (cons
    (circle 'goldenrod' x3 y2 r)
    (map (\\[x y] (circle 'magenta' x y r))
         [[(- x4 delta) y0]
          [x4 (+ y0 delta)]
          [x4 yRightmostPt]
          [x4 (- y2 delta)]
          [(- x4 delta) y2]
          [x2 y2]
         ]))))

(svg (cons theP (if true highlights [])))

"""

lilliconP2 =
 """
(def [x0 x1 x2 x3 x4] [20 80 150 240 380])
(def [y0 y1 y2 y3]    [20 125 296 424])

(def yRightmostPt (+ y0 (/ (- y2 y0) 2!)))
(def arcR 1!) ; any non-zero, not-too-large number works

(def theP
  (path 'black' 'none' 0
    ['M' x0 y3
     'C' x1 376 x1 216 x1 y1
     'C' 80 60 134 y0 x3 y0
     'A' arcR arcR 0 0 1 x3 y2
     'L' x2 y2
     'L' x2 y3
     'L' x0 y3
    ]))

(def highlights
  (let r 10!
  [ (circle 'goldenrod' x3 y2 r)
    (circle 'magenta'   x2 y2 r) ]))

(svg (cons theP (if true highlights [])))

"""

keyboard =
 """(def scale 25)
(def keyBaseHeight scale)
(def keyBaseWidth keyBaseHeight)
(def relativeSpacing 0.3333333333333)

(def [boardLeft boardTop] [50 50])

(def key (\\(relativeLeft relativeTop relativeWidth relativeHeight)
  (rect
    'orange'
    (+ boardLeft (* relativeLeft keyBaseWidth))
    (+ boardTop (* relativeTop keyBaseWidth))
    (* relativeWidth keyBaseWidth)
    (* relativeHeight keyBaseHeight)
  )
))

; Generate a row of keys with the given relativeKeyWidths, separated by relativeKeySpacing
; Returns [keyRects relativeTotalWidth]
(def row (\\(relativeLeft relativeTop relativeHeight relativeKeySpacing relativeKeyWidths)
  (let [keys relativeWidthPlusSpacing]
    (foldl
      (\\(relativeKeyWidth [keys nextKeyRelativeLeft])
        (let newKey (key nextKeyRelativeLeft relativeTop relativeKeyWidth relativeHeight)
          [[newKey|keys] (+ nextKeyRelativeLeft (+ relativeKeySpacing relativeKeyWidth))]
        )
      )
      [[] relativeLeft]
      relativeKeyWidths
    )
  [keys (- (- relativeWidthPlusSpacing relativeKeySpacing) relativeLeft)]
  )
))

(def row1SquareKeyCount 10)
(def row2SquareKeyCount 8)
(def row3SquareKeyCount 7)

(def row2SquareKeysRelativeWidth (+ row2SquareKeyCount (* relativeSpacing (- row2SquareKeyCount 1))))
(def row3SquareKeysRelativeWidth (+ row3SquareKeyCount (* relativeSpacing (- row3SquareKeyCount 1))))

(def [row1 keysRelativeWidth] (row relativeSpacing relativeSpacing 1 relativeSpacing [1.5|(repeat row1SquareKeyCount 1)]))

; Make the first and last keys of the row the appropriate width so the other keys are center.
(def row2EdgeKeyRelativeWidth (- (* 0.5 (- keysRelativeWidth row2SquareKeysRelativeWidth)) relativeSpacing))
(def row3EdgeKeyRelativeWidth (- (* 0.5 (- keysRelativeWidth row3SquareKeysRelativeWidth)) relativeSpacing))

(def [row2 _] (row relativeSpacing (+ 1 (* 2 relativeSpacing)) 1 relativeSpacing (concat [[row2EdgeKeyRelativeWidth] (repeat row2SquareKeyCount 1) [row2EdgeKeyRelativeWidth]])))
(def [row3 _] (row relativeSpacing (+ 2 (* 3 relativeSpacing)) 1 relativeSpacing (concat [[row3EdgeKeyRelativeWidth] (repeat row3SquareKeyCount 1) [row3EdgeKeyRelativeWidth]])))

(def boardRelativeWidth  (+ keysRelativeWidth (* 2 relativeSpacing)))
(def boardRelativeHeight (+ 4 (* 5 relativeSpacing)))

(def arrowVerticalSpacing 0.1)
(def arrowHeight (* 0.5 (- 1 arrowVerticalSpacing)))
(def arrowsRelativeWidth (+ 3 (* 2 relativeSpacing)))
(def [bottomArrows _] (row (- (- boardRelativeWidth arrowsRelativeWidth) relativeSpacing) (+ arrowVerticalSpacing (+ arrowHeight (+ 3 (* 4 relativeSpacing)))) arrowHeight relativeSpacing (repeat 3 1)))

(def topArrow
  (key
    (- (- boardRelativeWidth (+ 2 relativeSpacing)) relativeSpacing)
    (+ 3 (* 4 relativeSpacing))
    1
    arrowHeight
  )
)

(def row4SquareKeyCount 2)
(def row4NextToSpaceBarKeyRelativeWidth 1.25)
(def row4NotArrowsRelativeWidth (- (- keysRelativeWidth arrowsRelativeWidth) relativeSpacing))
(def row4SquareKeysRelativeWidth (+ row4SquareKeyCount (* relativeSpacing (- row4SquareKeyCount 1))))
(def spaceBarRelativeWidth (- row4NotArrowsRelativeWidth (+ row4SquareKeysRelativeWidth (+ (* 2 row4NextToSpaceBarKeyRelativeWidth) (* 3 relativeSpacing)))))
(def row4KeyRelativeWidths (concat [(repeat row4SquareKeyCount 1) [row4NextToSpaceBarKeyRelativeWidth spaceBarRelativeWidth row4NextToSpaceBarKeyRelativeWidth]]))
(def [row4 _] (row relativeSpacing (+ 3 (* 4 relativeSpacing)) 1 relativeSpacing row4KeyRelativeWidths))

(def backBoard
  (rect 'lightblue' boardLeft boardTop (* boardRelativeWidth scale) (* boardRelativeHeight scale))
)

(svg (concat [
  [backBoard]
  row1
  row2
  row3
  bottomArrows
  [topArrow]
  row4
]))
"""

keyboard2 =
 """(def scale 40)
(def keyBaseHeight scale)
(def keyBaseWidth keyBaseHeight)
(def relativeSpacing 0.3333333333333)

(def [boardLeft boardTop] [50 50])

(def key (\\(relativeLeft relativeTop relativeWidth relativeHeight)
  (rect
    'lightgray'
    (+ boardLeft (* relativeLeft keyBaseWidth))
    (+ boardTop (* relativeTop keyBaseWidth))
    (* relativeWidth keyBaseWidth)
    (* relativeHeight keyBaseHeight)
  )
))

; Generate a row of keys with the given relativeKeyWidths, separated by relativeKeySpacing
; Returns [keyRects relativeTotalWidth]
(def row (\\(relativeLeft relativeTop relativeHeight relativeKeySpacing relativeKeyWidths)
  (let [keys relativeWidthPlusSpacing]
    (foldl
      (\\(relativeKeyWidth [keys nextKeyRelativeLeft])
        (let newKey (key nextKeyRelativeLeft relativeTop relativeKeyWidth relativeHeight)
          [[newKey|keys] (+ nextKeyRelativeLeft (+ relativeKeySpacing relativeKeyWidth))]
        )
      )
      [[] relativeLeft]
      relativeKeyWidths
    )
  [keys (- (- relativeWidthPlusSpacing relativeKeySpacing) relativeLeft)]
  )
))

(def row1RelativeKeyWidths [1 1 1 1 1 1 1 1])
(def row2RelativeKeyWidths [1 1 1 1 1 1 1 1])
(def row3RelativeKeyWidths [1 1 1 1 1 1 1 1])
(def row4RelativeKeyWidths [1 7.6667      1])

(def [row1 keysRelativeWidth] (row relativeSpacing relativeSpacing 1 relativeSpacing row1RelativeKeyWidths))
(def [row2 _] (row relativeSpacing (+ 1 (* 2 relativeSpacing)) 1 relativeSpacing row2RelativeKeyWidths))
(def [row3 _] (row relativeSpacing (+ 2 (* 3 relativeSpacing)) 1 relativeSpacing row3RelativeKeyWidths))
(def [row4 _] (row relativeSpacing (+ 3 (* 4 relativeSpacing)) 1 relativeSpacing row4RelativeKeyWidths))

(def boardRelativeWidth  (+ keysRelativeWidth (* 2 relativeSpacing)))
(def boardRelativeHeight (+ 4 (* 5 relativeSpacing)))

(def backBoard
  (rect 'green' boardLeft boardTop (* boardRelativeWidth scale) (* boardRelativeHeight scale))
)

(svg (concat [
  [backBoard]
  row1
  row2
  row3
  row4
]))
"""

keyboard2target =
 """(def scale 40)
(def keyBaseHeight scale)
(def keyBaseWidth keyBaseHeight)
(def relativeSpacing 0.3333333333333)

(def [boardLeft boardTop] [54 50])

(def key (\\(relativeLeft relativeTop relativeWidth relativeHeight)
  (rect
    'lightgray'
    (+ boardLeft (* relativeLeft keyBaseWidth))
    (+ boardTop (* relativeTop keyBaseWidth))
    (* relativeWidth keyBaseWidth)
    (* relativeHeight keyBaseHeight)
  )
))

; Generate a row of keys with the given relativeKeyWidths, separated by relativeKeySpacing
; Returns [keyRects relativeTotalWidth]
(def row (\\(relativeLeft relativeTop relativeHeight relativeKeySpacing relativeKeyWidths)
  (let [keys relativeWidthPlusSpacing]
    (foldl
      (\\(relativeKeyWidth [keys nextKeyRelativeLeft])
        (let newKey (key nextKeyRelativeLeft relativeTop relativeKeyWidth relativeHeight)
          [[newKey|keys] (+ nextKeyRelativeLeft (+ relativeKeySpacing relativeKeyWidth))]
        )
      )
      [[] relativeLeft]
      relativeKeyWidths
    )
  [keys (- (- relativeWidthPlusSpacing relativeKeySpacing) relativeLeft)]
  )
))

(def row2EndKeysWidth 1.95)
(def row3EndKeysWidth 2.625)
(def row4CommandKeysWidth 1.5)

(def row1RelativeKeyWidths [1.575 1 1 1 1 1 1 1 1 1])
(def row2RelativeKeyWidths [row2EndKeysWidth 1 1 1 1 1 1 1 row2EndKeysWidth])
(def row3RelativeKeyWidths [row3EndKeysWidth 1 1 1 1 1 1 row3EndKeysWidth])
(def row4RelativeKeyWidths [1 row4CommandKeysWidth 7.2667 row4CommandKeysWidth 1])

(def [row1 keysRelativeWidth] (row relativeSpacing relativeSpacing 1 relativeSpacing row1RelativeKeyWidths))
(def [row2        _] (row relativeSpacing (+ 1 (* 2 relativeSpacing)) 1 relativeSpacing row2RelativeKeyWidths))
(def [row3        _] (row relativeSpacing (+ 2 (* 3 relativeSpacing)) 1 relativeSpacing row3RelativeKeyWidths))
(def [row4        _] (row relativeSpacing (+ 3 (* 4 relativeSpacing)) 1 relativeSpacing row4RelativeKeyWidths))

(def boardRelativeWidth  (+ keysRelativeWidth (* 2 relativeSpacing)))
(def boardRelativeHeight (+ 4 (* 5 relativeSpacing)))

(def backBoard
  (rect 'green' boardLeft boardTop (* boardRelativeWidth scale) (* boardRelativeHeight scale))
)

(svg (concat [
  [backBoard]
  row1
  row2
  row3
  row4
]))
"""

tessellation =
 """; I believe this is set up for group p6mm
; https://en.wikipedia.org/wiki/Wallpaper_group#Group_p6mm_.28.2A632.29

; SVG transforms to flip, rotate, and position.
(def transformGroup (\\(transformCenterX transformCenterY flipVertical rotationAngle translateX translateY shapes)
  [
    'g'
    [
      ['transform' [['translate' (+ translateX transformCenterX) (+ translateY transformCenterY)] ['rotate' rotationAngle 0 0] ['scale' (if flipVertical -1 1) 1] ['translate' (- 0 transformCenterX) (- 0 transformCenterY)]]]
    ]
    shapes
  ]
))


(def sin30 0.5!)
(def cos30 (* 0.5! (sqrt 3!)))

(def [x y radius] [350 200 200])
(def innerRadius (* radius cos30))
(def [bottomY cornerX] [(+ y innerRadius) (- x (/ radius 2!))])
(def primativeBottomWidth (/ radius 2!))

(def smallInnerRadius 50)
(def bottomRectWidth (- primativeBottomWidth smallInnerRadius))
(def bottomRectHeight (/ smallInnerRadius (sqrt 3!)))

(def primitive [
  (path 'lightblue' 'none' 0 [
    'M' x y
    'L' x bottomY
    'L' cornerX bottomY
    'Z'
  ])
  (path 'green' 'none' 0 [
    'M' x y
    'L' x (+ y (/ (* smallInnerRadius 2!) (sqrt 3!)))
    'L' (- x (* smallInnerRadius sin30)) (+ y (* smallInnerRadius cos30))
    'Z'
  ])
  (path 'green' 'none' 0 [
    'M' cornerX bottomY
    'L' (+ cornerX smallInnerRadius) bottomY
    'L' (+ cornerX smallInnerRadius) (- bottomY (/ smallInnerRadius (sqrt 3!)))
    'L' (+ cornerX (* smallInnerRadius sin30)) (- bottomY (* smallInnerRadius cos30))
    'Z'
  ])
  (rect 'pink' (- x bottomRectWidth) (- bottomY bottomRectHeight) bottomRectWidth bottomRectHeight)
  (path 'pink' 'none' 0 [
    'M' x (+ y (/ (* smallInnerRadius 2!) (sqrt 3!)))
    'L' (+ cornerX smallInnerRadius) (- bottomY (/ smallInnerRadius (sqrt 3!)))
    'L' (+ cornerX (* smallInnerRadius sin30)) (- bottomY (* smallInnerRadius cos30))
    'L' (- x (* smallInnerRadius sin30)) (+ y (* smallInnerRadius cos30))
    'Z'
  ])
  (path 'red' 'none' 0 [
    'M' x (- bottomY bottomRectHeight)
    'L' x (- (- bottomY bottomRectHeight) (* bottomRectWidth cos30))
    'L' (- x (/ bottomRectWidth 2)) (- (- bottomY bottomRectHeight) (* bottomRectWidth cos30))
    'Z'
  ])
])

(def primitiveHexagon [
  (transformGroup x y false 0 0 0 primitive)
  (transformGroup x y true  0 0 0 primitive)
  (transformGroup x y false 60 0 0 primitive)
  (transformGroup x y true  60 0 0 primitive)
  (transformGroup x y false 120 0 0 primitive)
  (transformGroup x y true  120 0 0 primitive)
  (transformGroup x y false 180 0 0 primitive)
  (transformGroup x y true  180 0 0 primitive)
  (transformGroup x y false 240 0 0 primitive)
  (transformGroup x y true  240 0 0 primitive)
  (transformGroup x y false 300 0 0 primitive)
  (transformGroup x y true  300 0 0 primitive)
])

(def primitiveHexagonColumn [
  (transformGroup x y false 0 0 (* -2 innerRadius) primitiveHexagon)
  (transformGroup x y false 0 0 0 primitiveHexagon)
  (transformGroup x y false 0 0 (* 2 innerRadius) primitiveHexagon)
  (transformGroup x y false 0 0 (* 4 innerRadius) primitiveHexagon)
])

(def primitiveHexagonColumns [
  (transformGroup x y false 0 (* -1 (* radius (+ 1 sin30))) innerRadius primitiveHexagonColumn)
  (transformGroup x y false 0 0 0 primitiveHexagonColumn)
  (transformGroup x y false 0 (* 1 (* radius (+ 1 sin30))) innerRadius primitiveHexagonColumn)
  (transformGroup x y false 0 (* 3 radius) 0 primitiveHexagonColumn)
])

(svg primitiveHexagonColumns)
"""

tessellationTarget =
 """; I believe this is set up for group p6mm
; https://en.wikipedia.org/wiki/Wallpaper_group#Group_p6mm_.28.2A632.29

; SVG transforms to flip, rotate, and position.
(def transformGroup (\\(transformCenterX transformCenterY flipVertical rotationAngle translateX translateY shapes)
  [
    'g'
    [
      ['transform' [['translate' (+ translateX transformCenterX) (+ translateY transformCenterY)] ['rotate' rotationAngle 0 0] ['scale' (if flipVertical -1 1) 1] ['translate' (- 0 transformCenterX) (- 0 transformCenterY)]]]
    ]
    shapes
  ]
))


(def sin30 0.5!)
(def cos30 (* 0.5! (sqrt 3!)))

(def [x y radius] [350 200 200])
(def innerRadius (* radius cos30))
(def [bottomY cornerX] [(+ y innerRadius) (- x (/ radius 2!))])
(def primativeBottomWidth (/ radius 2!))

(def smallInnerRadius 56)
(def largeInnerRadius 69)
(def bottomRectWidth (- primativeBottomWidth smallInnerRadius))
(def bottomRectHeight (/ smallInnerRadius (sqrt 3!)))

(def primitive [
  (path 'lightblue' 'none' 0 [
    'M' x y
    'L' x bottomY
    'L' cornerX bottomY
    'Z'
  ])
  (path 'green' 'none' 0 [
    'M' x y
    'L' x (+ y (/ (* largeInnerRadius 2!) (sqrt 3!)))
    'L' (- x (* smallInnerRadius sin30)) (+ y (* smallInnerRadius cos30))
    'Z'
  ])
  (path 'green' 'none' 0 [
    'M' cornerX bottomY
    'L' (+ cornerX smallInnerRadius) bottomY
    'L' (+ cornerX largeInnerRadius) (- bottomY (/ largeInnerRadius (sqrt 3!)))
    'L' (+ cornerX (* smallInnerRadius sin30)) (- bottomY (* smallInnerRadius cos30))
    'Z'
  ])
  (path 'red' 'none' 0 [
    'M' x (+ y (/ (* largeInnerRadius 2!) (sqrt 3!)))
    'L' (+ cornerX largeInnerRadius) (- bottomY (/ largeInnerRadius (sqrt 3!)))
    'L' x (- bottomY (/ largeInnerRadius (sqrt 3!)))
    'Z'
  ])
])

(def primitiveHexagon [
  (transformGroup x y false 0 0 0 primitive)
  (transformGroup x y true  0 0 0 primitive)
  (transformGroup x y false 60 0 0 primitive)
  (transformGroup x y true  60 0 0 primitive)
  (transformGroup x y false 120 0 0 primitive)
  (transformGroup x y true  120 0 0 primitive)
  (transformGroup x y false 180 0 0 primitive)
  (transformGroup x y true  180 0 0 primitive)
  (transformGroup x y false 240 0 0 primitive)
  (transformGroup x y true  240 0 0 primitive)
  (transformGroup x y false 300 0 0 primitive)
  (transformGroup x y true  300 0 0 primitive)
])

(def primitiveHexagonColumn [
  (transformGroup x y false 0 0 (* -2 innerRadius) primitiveHexagon)
  (transformGroup x y false 0 0 0 primitiveHexagon)
  (transformGroup x y false 0 0 (* 2 innerRadius) primitiveHexagon)
  (transformGroup x y false 0 0 (* 4 innerRadius) primitiveHexagon)
])

(def primitiveHexagonColumns [
  (transformGroup x y false 0 (* -1 (* radius (+ 1 sin30))) innerRadius primitiveHexagonColumn)
  (transformGroup x y false 0 0 0 primitiveHexagonColumn)
  (transformGroup x y false 0 (* 1 (* radius (+ 1 sin30))) innerRadius primitiveHexagonColumn)
  (transformGroup x y false 0 (* 3 radius) 0 primitiveHexagonColumn)
])

(svg primitiveHexagonColumns)
"""

tessellation2 =
 """; I believe this is set up for group p6mm
; https://en.wikipedia.org/wiki/Wallpaper_group#Group_p6mm_.28.2A632.29

; SVG transforms to flip, rotate, and position.
(def transformGroup (\\(transformCenterX transformCenterY flipVertical rotationAngle translateX translateY shapes)
  [
    'g'
    [
      ['transform' [['translate' (+ translateX transformCenterX) (+ translateY transformCenterY)] ['rotate' rotationAngle 0 0] ['scale' (if flipVertical -1 1) 1] ['translate' (- 0 transformCenterX) (- 0 transformCenterY)]]]
    ]
    shapes
  ]
))


(def sin30 0.5!)
(def cos30 (* 0.5! (sqrt 3!)))

(def [x y radius] [350 200 200])
(def innerRadius (* radius cos30))
(def [bottomY cornerX] [(+ y innerRadius) (- x (/ radius 2!))])

(def smallInnerRadius 36.14359353944901)
(def smallInnerRadius2 62.143593539449)
(def largeInnerRadius 74.2487113059643)

(def primitive [
  (path 'lightblue' 'none' 0 [
    'M' x y
    'L' x bottomY
    'L' cornerX bottomY
    'Z'
  ])
  (path 'blue' 'none' 0 [
    'M' x y
    'L' x (+ y (/ (* largeInnerRadius 2!) (sqrt 3!)))
    'L' (- x (* smallInnerRadius sin30)) (+ y (* smallInnerRadius cos30))
    'Z'
  ])
  (path 'darkblue' 'none' 0 [
    'M' cornerX bottomY
    'L' (+ cornerX smallInnerRadius2) bottomY
    'L' (+ cornerX largeInnerRadius) (- bottomY (/ largeInnerRadius (sqrt 3!)))
    'L' (+ cornerX (* smallInnerRadius2 sin30)) (- bottomY (* smallInnerRadius2 cos30))
    'Z'
  ])
  (path 'white' 'none' 0 [
    'M' x (+ y (/ (* largeInnerRadius 2!) (sqrt 3!)))
    'L' (+ cornerX largeInnerRadius) (- bottomY (/ largeInnerRadius (sqrt 3!)))
    'L' x (- bottomY (/ largeInnerRadius (sqrt 3!)))
    'Z'
  ])
])

(def primitiveHexagon [
  (transformGroup x y false 0 0 0 primitive)
  (transformGroup x y true  0 0 0 primitive)
  (transformGroup x y false 60 0 0 primitive)
  (transformGroup x y true  60 0 0 primitive)
  (transformGroup x y false 120 0 0 primitive)
  (transformGroup x y true  120 0 0 primitive)
  (transformGroup x y false 180 0 0 primitive)
  (transformGroup x y true  180 0 0 primitive)
  (transformGroup x y false 240 0 0 primitive)
  (transformGroup x y true  240 0 0 primitive)
  (transformGroup x y false 300 0 0 primitive)
  (transformGroup x y true  300 0 0 primitive)
])

(def primitiveHexagonColumn [
  (transformGroup x y false 0 0 (* -2 innerRadius) primitiveHexagon)
  (transformGroup x y false 0 0 0 primitiveHexagon)
  (transformGroup x y false 0 0 (* 2 innerRadius) primitiveHexagon)
  (transformGroup x y false 0 0 (* 4 innerRadius) primitiveHexagon)
])

(def primitiveHexagonColumns [
  (transformGroup x y false 0 (* -1 (* radius (+ 1 sin30))) innerRadius primitiveHexagonColumn)
  (transformGroup x y false 0 0 0 primitiveHexagonColumn)
  (transformGroup x y false 0 (* 1 (* radius (+ 1 sin30))) innerRadius primitiveHexagonColumn)
  (transformGroup x y false 0 (* 3 radius) 0 primitiveHexagonColumn)
])

(svg primitiveHexagonColumns)
"""

floralLogo =
 """(def spiralArmCount 5)
(def ringParameters [
  ; petalSize petalRotation ringRadius ringRotation
  [ 118{0-300}  0.63{-3.2-3.2} -16{-100-300}      0{-3.2-3.2}]
  [ 76{0-300} -0.957{-3.2-3.2}  48{-100-300} -0.384{-3.2-3.2}]
  [ 59{0-300} -1.269{-3.2-3.2}  76{-100-300} -0.89{-3.2-3.2}]
  [ 36{0-300} -1.136{-3.2-3.2} 88{-100-300} -1.394{-3.2-3.2}]
])

(def rotatePointAround (\\(relX relY aroundX aroundY theta)
  (let [rotRelX rotRelY] [(- (* relX (cos theta)) (* relY (sin theta))) (+ (* relX (sin theta)) (* relY (cos theta)))]
    [(+ rotRelX aroundX) (+ rotRelY aroundY)]
  )
))

;(x + yi)(cosθ + isinθ) = xcosθ + ixsinθ + iycosθ - ysinθ
;= (xcosθ - ysinθ) + (xsinθ + ycosθ)i

(def petal (\\(x y scale theta)
  (let [[x1 y1]     [x2 y2]    ] [(rotatePointAround (* 1.0 scale) 0              x y theta) (rotatePointAround 0              0             x y theta)]
  (let [[cx1a cy1a] [cx1b cy1b]] [(rotatePointAround (* 0.7 scale) (* 0.3 scale)  x y theta) (rotatePointAround (* 0.3 scale) (* 0.3 scale)  x y theta)]
  (let [[cx2a cy2a] [cx2b cy2b]] [(rotatePointAround (* 0.4573836036582167 scale) (* -0.24276959866973943 scale) x y theta) (rotatePointAround (* 0.4710783946789573 scale) (* 0.40107241629569196 scale) x y theta)]
    (path 'orange' 'none' 0 [
      'M' x1 y1
      'C' cx1a cy1a cx1b cy1b x2 y2
      'C' cx2a cy2a cx2b cy2b x1 y1
      'Z'
    ])
  )))
))

(def [x y] [300 140])
(def flower
  (concat
    (map
      (\\[petalSize petalRotation ringRadius ringRotation]
        (map
          (\\i
            (let armTheta (+ ringRotation (* i (/ twoPi spiralArmCount)))
            (let petalX (+ x (* ringRadius (cos armTheta)))
            (let petalY (+ y (* ringRadius (sin armTheta)))
              (petal petalX petalY petalSize (+ armTheta petalRotation))
            )))
          )
          (range 0! (- spiralArmCount 1!))
        )
      )
      ringParameters
    )
  )
)

(svg flower)

"""

floralLogo2 =
 """(def ringParameters [
  ; petalSize petalRotation ringRadius ringRotation
  [ 76{0-300} -0.266{-3.2-3.2}  -12{-100-300}   0.128{-3.2-3.2}]
  [ 52{0-300} -0.317{-3.2-3.2}   60{-100-300}  -0.320{-3.2-3.2}]
  [ 38{0-300} -0.629{-3.2-3.2}  104{-100-300}  -0.570{-3.2-3.2}]
])

(def rotatePointAround (\\(relX relY aroundX aroundY theta orientation)
  (let relY (* orientation relY)
  (let [rotRelX rotRelY] [(- (* relX (cos theta)) (* relY (sin theta))) (+ (* relX (sin theta)) (* relY (cos theta)))]
    [(+ rotRelX aroundX) (+ rotRelY aroundY)]
  ))
))

;(x + yi)(cosθ + isinθ) = xcosθ + ixsinθ + iycosθ - ysinθ
;= (xcosθ - ysinθ) + (xsinθ + ycosθ)i

(def petal (\\(x y scale theta orientation)
  (let [[x1 y1]     [x2 y2]    ] [(rotatePointAround (* 1 scale) 0              x y theta orientation) (rotatePointAround 0              0             x y theta orientation)]
  (let [[cx1a cy1a] [cx1b cy1b]] [(rotatePointAround (* 0.58 scale) (* 0.305 scale)  x y theta orientation) (rotatePointAround (* 0.43 scale) (* 0.275 scale)  x y theta orientation)]
  (let [[cx2a cy2a] [cx2b cy2b]] [(rotatePointAround (* 0.4573836036582167 scale) (* -0.24276959866973943 scale) x y theta orientation) (rotatePointAround (* 0.5760783946789573 scale) (* -0.2389275837043081 scale) x y theta orientation)]
    (path 'orange' 'none' 0 [
      'M' x1 y1
      'C' cx1a cy1a cx1b cy1b x2 y2
      'C' cx2a cy2a cx2b cy2b x1 y1
      'Z'
    ])
  )))
))

(def [x y] [300 200])
(def [theta0 deltaTheta] [0.314{-3.2-3.2} -0.5{-3.2-3.2}])
(def flower
  (concat
    (map
      (\\[petalSize petalRotation ringRadius ringRotation]
        (concatMap
          (\\theta
            (let armTheta (+ ringRotation theta)
            (let rightPetalX (+ x (* ringRadius (cos armTheta)))
            (let leftPetalX  (- x (* ringRadius (cos armTheta)))
            (let petalY (+ y (* ringRadius (sin armTheta)))
              [
                (petal rightPetalX petalY petalSize (+ armTheta petalRotation) 1!)
                (petal leftPetalX petalY petalSize (- (pi) (+ armTheta petalRotation)) -1!)
              ]
            ))))
          )
          [theta0 (+ theta0 deltaTheta) (+ theta0 (* 2! deltaTheta))]
        )
      )
      ringParameters
    )
  )
)

(svg flower)
"""

zones =
 """(def ngon (\\(n cx cy len1 len2)
  (let dangle (/ (* 3! (pi)) 2!)
  (let anglei (\\i (+ dangle (/ (* i (* 2! (pi))) n)))
  (let xi     (\\i (+ cx (* len1 (cos (anglei i)))))
  (let yi     (\\i (+ cy (* len2 (sin (anglei i)))))
  (let pti    (\\i [(xi i) (yi i)])
  (let pts    (map pti (list0N (- n 1!)))
    (polygon 'goldenrod' 'none' 4 pts)))))))))

(svg [
  (rect 'goldenrod' 32 170 109 132)
  (ellipse 'goldenrod' 203 237 32 68)
  (ngon 5 464{200-600} 240{100-300} 60 60)
  (path_ ['M' 261 250 'Q' 316.5 306 307 231 'C' 317 179 341 256 366 188 'T' 380 274])
])

"""

roundedRect =
 """
(def roundedRect (\\(fill x y w h rxSeed rySeed)
  (let off 20!
  (let [wHalf hHalf] [(/ w 2!) (/ h 2!)]
  (let [rx rxSlider] (hSlider true x (+ x wHalf) (- y off) 0! wHalf '' rxSeed)
  (let [ry rySlider] (vSlider true y (+ y hHalf) (- x off) 0! hHalf '' rySeed)
  (concat [
    [ (consAttr (consAttr (rect fill x y w h) ['rx' rx]) ['ry' ry]) ]
    rxSlider
    rySlider
  ])))))))

(svg (concat [
  [(rect 'black' 0 0 0 0)] ; TODO remove dummy
  (roundedRect 150 100 100 80 200 10 20)
]))

"""

spiralSpiralGraph =
 """; Spiral Spiral-Graph
; Somewhat like the Spirograph toy, but based on a circle
; following a spiral rather than a circle in a circle.

(def n 140{1-200})
(def revs 4{0-20})
(def innerRevsRatio 2{0-20})
(def innerR 125{0-500})

(def placeCircle (\\i
  (let spiralRadius 200
  (let radius (* spiralRadius (/ i (- n 1)))
  (let theta (* (* (/ i (- n 1)) (* 2 (pi))) revs)
  (let innerTheta (* (* (* (/ i (- n 1)) (* 2 (pi))) innerRevsRatio) revs)
  (let x (+ (+ (+ spiralRadius 150) (* radius (cos theta))) (* innerR (cos innerTheta)))
  (let y (+ (+ (+ spiralRadius 250) (* radius (sin theta))) (* innerR (sin innerTheta)))
    (circle i x y 10)
  ))))))
))

(def theCircles (map placeCircle (range 1 n)))

(svg theCircles)
"""

-- LITTLE_TO_ELM relateRects0
-- LITTLE_TO_ELM relateRects1
-- LITTLE_TO_ELM relateCircles0
-- LITTLE_TO_ELM relateLines0
-- LITTLE_TO_ELM relatePoints0
-- LITTLE_TO_ELM relatePoints1
-- LITTLE_TO_ELM relatePoints2
-- LITTLE_TO_ELM relatePoints3
-- LITTLE_TO_ELM relatePoints4
blank =
 """(blobs [
])

"""

horrorFilms0 =
 """
; http://www.awwwards.com/gallery/4453/99-creative-logo-designs-for-inspiration/

(def equiTriAt (\\(cx cy color sideLen rot)
  (let len1 (* sideLen (/ 2! 3!))
  (let len2 (* sideLen (/ 1! 3!))
  (let point (circle color cx cy 15!)
  (let tri (nStar 'none' color 10! 3! len1 len2 rot cx cy)
  [tri point]
))))))

(def horror (\\(cx0 cy0 bgColor fgColor rBig rSmall sep)

  (def helper
    (ghosts (equiTriAt cx0 cy0 60 sep (pi))))

  (def [ snap3 _ snap2 _ snap1 | _ ]
    (polygonPoints (hd helper)))

  (def backgroundCircle
    [ (rawCircle bgColor 360 0 cx0 cy0 rBig) ])

  (def foregroundCircle (\\[cx cy]
    [ (rawCircle fgColor 360 0 cx cy rSmall) ]))

  (concat [
    backgroundCircle
    (foregroundCircle snap1)
    (foregroundCircle snap2)
    (foregroundCircle snap3)
    helper
  ])
))

(blobs [
  (horror 220 250 390 499 172 47 139)
])

"""

cyclingAssociation0 =
 """
; http://www.awwwards.com/gallery/4433/99-creative-logo-designs-for-inspiration/

(def equiTriAt (\\(cx cy color sideLen rot)
  (let len1 (* sideLen (/ 2! 3!))
  (let len2 (* sideLen (/ 1! 3!))
  (let point (circle color cx cy 15!)
  (let tri (nStar 'none' color 10! 3! len1 len2 rot cx cy)
  [tri (ghost point)]
))))))


(def logo (\\(cx0 cy0
             wheelRadius wheelDistance
             armPct
             wheelCapSize
             [logoColor logoStrokeWidth])

  (def helper
    (ghosts (equiTriAt cx0 cy0 60 wheelDistance 0!)))

  (def [ snap1 _ snap2 _ snap3 | _ ]
    (polygonPoints (hd helper)))

  (def onePiece (\\([cx cy] rot)

    (def wheel
      (rawCircle 'none' logoColor logoStrokeWidth cx cy wheelRadius))

    (def arm
      (let armLength (* armPct wheelDistance)
      (let dx (* armLength (cos (/ (pi) 3!)))
      (let dy (* armLength (sin (/ (pi) 3!)))
      (let d ['M' cx cy 'L' (+ cx dx) (- cy dy) 'L' (+ cx 8) (- cy dy)]
        (rawPath 'none' logoColor logoStrokeWidth d 0))))))

    (def cap
      (rawCircle logoColor 360 0 cx cy wheelCapSize))

    [wheel (rotateAround rot cx cy arm) cap]
  ))

  ; TODO use a triangle function that doesn't draw center
  (def midTriangle
    ; slightly less than 0.50 to keep room for width of stroke
    (equiTriAt cx0 cy0 logoColor (* 0.42! wheelDistance) (pi)))

  (concat [
    (onePiece snap2 0)
    (onePiece snap1 120)
    (onePiece snap3 240)
    midTriangle
    helper
  ])
))

(blobs [
  (logo 167 182 49 156.42857142857147 0.7 9 [416 9])
])

"""

snsLogoWheel =
 """
(def logo (\\(rectColor
             lineColor lineWidth
             width height
             rot
             topLeft@[left top])

  (def botRight@[right bot] [(+ left width) (+ top height)])

  (def rect1
    (rawRect rectColor 360 0 left top width height rot))

  (def line2
    (line lineColor lineWidth left top right bot))

  (def line3
    (lineBetween lineColor lineWidth
      [left bot]
      (halfwayBetween topLeft botRight)))

  [ rect1 line2 line3 ]))

(def wheel (\\(n
              spokeLen spokeColor spokeWidth
              logoSize logoColor1 logoColor2 logoLineWidth
              hubRadius
              rot center@[cx cy])

  (def cars
    (let car_i (\\[x y]
      (let cx (- x (/ logoSize 2!))
      (let cy (- y (/ logoSize 2!))
        (logo logoColor1 logoColor2 logoLineWidth logoSize logoSize 0 [cx cy]))))
    (radialArray n spokeLen rot car_i center)))

  (def spokes
    (let spoke_i (\\endpoint
      [(lineBetween spokeColor spokeWidth center endpoint)])
    (radialArray n spokeLen rot spoke_i center)))

  (def hub
    [(ring spokeColor spokeWidth cx cy hubRadius)])

  (concat [ spokes cars hub ])
))

(blobs [
  (wheel
    (let n 16{1-30} n)
    100 420 2
    30 100 200 3
    20
    0
    ([150 150] : Point)
  )
])

"""

sns_UIST =
 """
(def newGroup4 (\\(line2_width line2_color color [left top right bot])
  (def bounds [left top right bot])

  (def rect1
    (let bounds [left top right bot]
      [ (rectangle color 'black' '0' 0 bounds) ]))

  (def line2
      [ (line line2_color line2_width left top right bot) ])

  (def line3
    (let [ x2 y2] [ (* 0.5! (+ left right)) (* 0.5! (+ top bot))]
      [ (line line2_color line2_width left bot x2 y2) ]))

  [ (group bounds (concat [ rect1 line2 line3 ])) ]))

(blobs [
  (withBounds [31 100 216 269] (newGroup4 5 202 60))
])

"""

sns_revisited_UIST =
 """
; Try deleting the five helper circles from the main expression.

(def [polygon6_top polygon5_left polygon6_right] [69 92 296])
(def helper_r 27.5)
(def polygon7_bot (+ (+ (* 0.5! (+ polygon6_top polygon6_top)) (* 0.5! (- (* 0.5! (+ polygon6_right polygon6_right)) (* 0.5! (+ polygon5_left polygon5_left))))) (* 0.5! (- (* 0.5! (+ polygon6_right polygon6_right)) (* 0.5! (+ polygon5_left polygon5_left))))))
(def k3105 (/ (- (+ (- polygon6_right helper_r) (* 0.5! (+ (+ (- polygon6_right helper_r) (* 2! helper_r)) (- helper_r polygon6_right)))) (+ (+ (- polygon5_left helper_r) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left)))) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left))))) (- polygon6_right (+ (+ (- polygon5_left helper_r) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left)))) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left)))))))
(def polygon7_top (- (* 0.5! (+ (- polygon7_bot helper_r) (- polygon6_top (+ (- 0! (+ helper_r helper_r)) (* 0.5! (+ (+ (- 0! (+ helper_r helper_r)) (* 2! helper_r)) (+ helper_r helper_r))))))) (+ (- 0! (+ helper_r helper_r)) (* 0.5! (+ (+ (- 0! (+ helper_r helper_r)) (* 2! helper_r)) (+ helper_r helper_r))))))
(def [polygon5_right k3038] [(- (* 0.5! (+ (+ (+ (- polygon5_left helper_r) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left)))) helper_r) (- polygon6_right helper_r))) helper_r) (- (+ (- polygon5_left helper_r) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left)))) helper_r)])
(def k3061 (/ (- (+ polygon5_right helper_r) (+ (+ k3038 helper_r) helper_r)) (- (- polygon6_right helper_r) (+ (+ k3038 helper_r) helper_r))))
(def polygon6_bot (- (+ (- polygon7_bot helper_r) (* 0.5! (+ (+ (- polygon7_bot helper_r) (* 2! helper_r)) (- helper_r polygon7_bot)))) helper_r))
(def k3063 (/ (- (+ polygon6_bot helper_r) polygon7_top) (- polygon7_bot polygon7_top)))
(def k3034 (- polygon6_right helper_r))
(def polygon5_top (- polygon6_top (+ (- 0! (+ helper_r helper_r)) (* 0.5! (+ (+ (- 0! (+ helper_r helper_r)) (* 2! helper_r)) (+ helper_r helper_r))))))
(def k3103 (/ (- (+ (- polygon5_top (+ helper_r helper_r)) (* 0.5! (+ (+ (- polygon5_top (+ helper_r helper_r)) (* 2! helper_r)) (- (+ helper_r helper_r) polygon5_top)))) polygon6_top) (- polygon6_bot polygon6_top)))
(def [k3041 polygon5_bot] [(- polygon7_top (+ helper_r helper_r)) (- polygon7_bot helper_r)])
(def k3134 (/ (- (+ k3041 helper_r) polygon5_top) (- polygon5_bot polygon5_top)))
(def k3045 (- polygon5_top (+ helper_r helper_r)))
(def k3046 (- polygon5_left helper_r))
(def k3141 (/ (- (+ k3038 helper_r) polygon5_left) (- polygon5_right polygon5_left)))

(def helper (\\(left top)
  (let [left top] [left top]
  (let bounds [left top (+ left (* 2! helper_r)) (+ top (* 2! helper_r))]
  (let [color strokeColor strokeWidth] [394 'black' 0]
    [ (oval color strokeColor strokeWidth bounds) ])))))

(def polygon5
  (let bounds [polygon5_left polygon5_top polygon5_right polygon5_bot]
  (let [color strokeColor strokeWidth] [261 'black' 2]
  (let pcts [[k3141 1] [0 0] [1 k3134]]
    [ (stretchyPolygon bounds color strokeColor strokeWidth pcts) ]))))

(def polygon6
  (let left (+ (+ (- polygon5_left helper_r) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left)))) (* 0.5! (+ (+ (- polygon5_left helper_r) (* 2! helper_r)) (- helper_r polygon5_left))))
  (let bounds [left polygon6_top polygon6_right polygon6_bot]
  (let [color strokeColor strokeWidth] [132 'black' 2]
  (let pcts [[0 k3103] [k3105 0] [1 1]]
    [ (stretchyPolygon bounds color strokeColor strokeWidth pcts) ])))))

(def polygon7
  (let [left right] [(+ (+ k3038 helper_r) helper_r) (- polygon6_right helper_r)]
  (let bounds [left polygon7_top right polygon7_bot]
  (let [color strokeColor strokeWidth] [334 'black' 2]
  (let pcts [[0 1] [1 k3063] [k3061 0]]
    [ (stretchyPolygon bounds color strokeColor strokeWidth pcts) ])))))

(def circle8
  (let [left top r] [(* 0.5! (+ polygon5_left polygon5_left)) (* 0.5! (+ polygon6_top polygon6_top)) (* 0.5! (- (* 0.5! (+ polygon6_right polygon6_right)) (* 0.5! (+ polygon5_left polygon5_left))))]
  (let bounds [left top (+ left (* 2! r)) (+ top (* 2! r))]
  (let [color strokeColor strokeWidth] [405 'black' 0]
    [ (oval color strokeColor strokeWidth bounds) ]))))

(blobs [
  (helper k3046 k3045)
  (helper polygon5_right k3041)
  (helper k3038 polygon5_bot)
  (helper k3034 polygon6_bot)
  polygon5
  polygon6
  polygon7
  circle8
])

"""

botanic_UIST =
 """
(def newGroup4 (\\(leftLeaf_strokeWidth leftLeaf_color leftLeaf_strokeColor leftLeaf_dPcts_k3164 leftLeaf_dPcts_k3166 centerLeftPct leftRightPct leftTopPct leftMidPct x03451 y03450 x03401 y03400 y03344 dPcts_k3058 [left top right bot])
  (def bounds [left top right bot])
  (def centerRightPct (- 1! centerLeftPct))
  (def rightLeftPct (- 1! leftRightPct))
  (def rightTopPct (- 1! leftTopPct))
  (def rightMidPct (- 1! leftMidPct))

  (def leftLeaf
    (let k3186 0!
    (let [left top right bot] [ left (scaleBetween top bot k3186) (scaleBetween left right leftRightPct) bot]
    (let bounds [left top right bot]
    (let [x0 y0] [x03451 y03450]
    (let dPcts ['M' x0 y0 'Q' leftMidPct leftLeaf_dPcts_k3166 0 leftLeaf_dPcts_k3164 'Q' leftTopPct 0 x0 y0]
      [ (stretchyPath bounds leftLeaf_color leftLeaf_strokeColor leftLeaf_strokeWidth dPcts) ]))))))

  (def rightLeaf
    (let k3132 1!
    (let [left top right bot] [ (scaleBetween left right rightLeftPct) top right (scaleBetween top bot k3132)]
    (let bounds [left top right bot]
    (let [x0 y0] [x03401 y03400]
    (let dPcts ['M' x0 y0 'Q' rightMidPct leftLeaf_dPcts_k3166 1 leftLeaf_dPcts_k3164 'Q' rightTopPct 0 x0 y0]
      [ (stretchyPath bounds leftLeaf_color leftLeaf_strokeColor leftLeaf_strokeWidth dPcts) ]))))))

  (def centerLeaf
    (let [left top right bot] [ (scaleBetween left right centerLeftPct) (scaleBetween top bot 0.1166930482692767) (scaleBetween left right centerRightPct) (scaleBetween top bot 0.6433647887474833)]
    (let bounds [left top right bot]
    (let [x0 y0] [0.5! y03344]
    (let dPcts ['M' x0 y0 'L' 0 dPcts_k3058 'Q' 0.5! 0 1 dPcts_k3058 'Z']
      [ (stretchyPath bounds leftLeaf_color leftLeaf_strokeColor leftLeaf_strokeWidth dPcts) ])))))

  [ (group bounds (concat [ leftLeaf rightLeaf centerLeaf ])) ]))

(blobs [
  (withBounds [56 45.523358950223596 512 495] (newGroup4 4 100 431 0.4693710270993644? 0.3959689822652434? 0.35053185358677735 0.47858611976259036 0.44362108479755546 0.5977244375980417 1 1 0 1 1 0.3407539747046014?))
])

"""

coffee_UIST =
 """
(def mug (\\(outer_color strokeWidth3280 color strokeWidth3239 strokeColor strokeWidth3142 x0 y0 [left top right bot])
  (def bounds [left top right bot])
  (def rFrac 0.2!)
  (def [outer_right outer_left] [ (scaleBetween left right 1)(scaleBetween left right 0.3575757575757576)])
  (def outer_x_radius (/ (- outer_right outer_left) 2!))
  (def [rect3_top outer_bot outer_top] [ (scaleBetween top bot 0.3620689655172414) (scaleBetween top bot 0.9224137931034483) (scaleBetween top bot 0.4396551724137931)])
  (def outer_ellipseRY (/ (- outer_bot outer_top) 2!))

  (def outer
    (let bounds [outer_left outer_top outer_right outer_bot]
    (let [ strokeColor strokeWidth] [ 'black' strokeWidth3280]
      [ (oval outer_color strokeColor strokeWidth bounds) ])))

  (def inner
    (let [left top right bot] [(+ outer_left (* rFrac outer_x_radius)) (+ outer_top (* rFrac outer_ellipseRY)) (- outer_right (* rFrac outer_x_radius)) (- outer_bot (* rFrac outer_ellipseRY))]
    (let bounds [left top right bot]
    (let [color strokeColor strokeWidth] [color 'black' strokeWidth3239]
      [ (oval color strokeColor strokeWidth bounds) ]))))

  (def rect3
    (let [left right bot] [(scaleBetween left right 0) (+ (+ outer_left (* 0.1! (- outer_right outer_left))) (* 0.5! (- (- outer_right (* 0.1! (- outer_right outer_left))) (+ outer_left (* 0.1! (- outer_right outer_left)))))) (* 2! (- (+ (+ outer_top (* 0.1! (- outer_bot outer_top))) (* 0.5! (- (- outer_bot (* 0.1! (- outer_bot outer_top))) (+ outer_top (* 0.1! (- outer_bot outer_top)))))) (* 0.5! rect3_top)))]
    (let bounds [left rect3_top right bot]
      [ (rectangle outer_color 'black' 0 0 bounds) ])))

  (def steam (\\(left top right bot)
    (let bounds [left top right bot]
    (let [strokeColor strokeWidth color] [strokeColor strokeWidth3142 'white']
    (let dPcts ['M' x0 y0 'C' 0 0.4925373134328358? 0.8076923076923077? 0.6119402985074627? 0.4230769230769231? 1 'C' 1 0.7313432835820896? 0.4230769230769231? 0.6417910447761194? x0 y0]
      [ (stretchyPath bounds color strokeColor strokeWidth dPcts) ])))))

  [ (group bounds (concat [ outer inner rect3 (steam(scaleBetween left right 0.012121212121212121) (scaleBetween top bot 0.02586206896551724) (scaleBetween left right 0.1696969696969697) (scaleBetween top bot 0.3146551724137931)) (steam(scaleBetween left right 0.2606060606060606) (scaleBetween top bot 0) (scaleBetween left right 0.41818181818181815) (scaleBetween top bot 0.28879310344827586)) (steam(scaleBetween left right 0.5212121212121212) (scaleBetween top bot 0.02586206896551724) (scaleBetween left right 0.6787878787878788) (scaleBetween top bot 0.3146551724137931)) ])) ]))

(blobs [
  (withBounds [27 27 192 259] (mug 164 0 481 0 102 5 0.8846153846153846? 0))
  (withBounds [299 214 406 322] (mug 164 0 481 0 102 5 0.8846153846153846? 0))
  (withBounds [143 380 193 442] (mug 164 0 481 0 102 5 0.8846153846153846? 0))
])

"""

rectangleTrisection =
 """; Rectangle Trisection
;
; After Alan Turranksy p566 in \"Watch What I Do: Programming by Demonstration\" Appendix B. 1993.
;
; Straightforward with tools as of UIST 2016, but
; does require a \"Dig Hole\" to type in \"(/ rect1_w 3)\"
;
; To further abstract (as below) requires manual labor (not a bounding box)
;

(def rectTrisection (\\(x y w h)
  (def rect1
    (let [fill stroke strokeWidth] [365 365 0]
    (let rot 0
      [ (rawRect fill stroke strokeWidth x y w h rot) ])))

  (def rect2
    (let [fill stroke strokeWidth] [175 175 0]
    (let rot 0
      [ (rawRect fill stroke strokeWidth x y (/ w 3) h rot) ])))

  (concat [rect1 rect2])
))


(blobs [
  (rectTrisection 91 181 347 177)
])
"""

battery =
 """; Battery
;
; After Bernstein and Li \"Lillicon\" 2015.
;
; Can be done with tools as of UIST 2016 (minus abstaction), but
; parameterization is backward; want tip x and y to be dependent but
; the Make Equal defaults to making x and h dependent (as below).
;
; Design decisions:
;   - w/h of tip absolute or relative to body
;   - if abstracted, BB or x/y/w/h parameterization
;

(def [body_w body_x] [147 103])
(def [tip_y body_h body_y] [223 84 201])
(def fill 362)

(def body
  (let [ stroke strokeWidth] [ 250 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth body_x body_y body_w body_h rot) ])))

(def tip
  (let [x w h] [(+ body_x body_w) 19 (* 2! (- (+ body_y (* 0.5! body_h)) tip_y))]
  (let [ stroke strokeWidth] [ 182 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x tip_y w h rot) ]))))

(blobs [
  body
  tip
])
"""

batteryDynamic =
 """; Battery (Dynamic)
;
; Still need tools for reordering shapes.
;
; Design decisions:
;   - battery body made from four rectangles or one path with a hole or one polyline (below)
;   - body x/y/w/h or left/top/right/bot (below)
;   - charge bar x/y/w/h or left/top/right/bot (below)
;   - calculate inner bounds as a variable (below) or inline
;   - calculate inner width as a variable (below) or inline
;   - w/h of tip absolute (below) or relative to body
;   - if abstracted, BB or x/y/w/h parameterization
;


(def chargePercentage 20{0-100})
(def bodyTop 117)
(def bodyBot 204)
(def bodyRight 273)
(def centerY (/ (+ bodyTop bodyBot) 2!))
(def bodyLeft 90)
(def bodyOutlineWidth 18{0-50})
(def halfOutlineWidth (/ bodyOutlineWidth 2!))
(def mainColor 360)

(def bodyOutline
  (let pts [[bodyLeft bodyTop] [bodyRight bodyTop] [bodyRight bodyBot] [bodyLeft bodyBot]]
    [ (addAttr (polygon 'none' mainColor bodyOutlineWidth pts) [\"stroke-linejoin\" \"round\"]) ]))

(def tip
  (let h 52
  (let [y w] [ (- centerY (/ h 2!)) 38]
  (let [stroke strokeWidth] [368 0]
  (let rot 0
    [ (rawRect mainColor stroke strokeWidth bodyRight y w h rot) ])))))

(def [innerLeft innerTop innerRight innerBot]
  [(+ bodyLeft halfOutlineWidth)
   (+ bodyTop halfOutlineWidth)
   (- bodyRight halfOutlineWidth)
   (- bodyBot halfOutlineWidth)])
(def innerWidth (- innerRight innerLeft))

(def chargeBar
  (let bounds @ [left top right bot] [innerLeft innerTop (+ innerLeft (/ (* innerWidth chargePercentage) 100!)) innerBot]
  (let [drainedColor color fullColor] [0 mainColor 113]
  (let color (if (le chargePercentage 20) drainedColor (if (ge chargePercentage 99) fullColor mainColor))
    [ (rectangle color 360 0 0 bounds) ]))))

(blobs [
  bodyOutline
  tip
  chargeBar
])
"""

mondrianArch =
 """; Mondrian Arch
;
; After Henry Lieberman p554 in \"Watch What I Do: Programming by Demonstration\" Appendix B. 1993.
;
; Can be done with tools as of UIST 2016, but
; parameterization is not anywhere near optimal.
;
; Below is hand-rolled (some digging).
;
; Design decisions:
;   - Pillars relative to BB or relative to lintel (below: relative to lintel)
;   - x/y/w/h or BB for lintel (below: x/y/w/h)
;   - Pillar width: same as lintel or relative to BB width (below: same as lintel)
;

; Get the following with vanilla \"Make Equal\":
;
; (def [rect3_y rect2_x rect3_w rect4_x] [161 105 170 222])
; (def rect2_y (+ (- (+ rect2_x rect3_w) rect4_x) rect3_y))
; (def rect2_h 148)
;
; (def rect2
;   (let w (- rect2_y rect3_y)
;   (let [fill stroke strokeWidth] [211 381 0]
;   (let rot 0
;     [ (rawRect fill stroke strokeWidth rect2_x rect2_y w rect2_h rot) ]))))
;
; (def rect3
;   (let h (- rect2_y rect3_y)
;   (let [fill stroke strokeWidth] [344 444 0]
;   (let rot 0
;     [ (rawRect fill stroke strokeWidth rect2_x rect3_y rect3_w h rot) ]))))
;
; (def rect4
;   (let w (- (+ rect2_x rect3_w) rect4_x)
;   (let [fill stroke strokeWidth] [117 391 0]
;   (let rot 0
;     [ (rawRect fill stroke strokeWidth rect4_x rect2_y w rect2_h rot) ]))))
;
; (blobs [
;   rect2
;   rect3
;   rect4
; ])
;

(def [lintel_x lintel_y lintel_w lintel_h] [76 146 145 42])
(def lintelBot (+ lintel_y lintel_h))
(def pillarHeight 198)

(def leftPillar
  (let [x y w h] [lintel_x lintelBot lintel_h pillarHeight]
  (let [fill stroke strokeWidth] [352 352 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x y w h rot) ]))))

(def lintel
  (let [fill stroke strokeWidth] [218 218 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth lintel_x lintel_y lintel_w lintel_h rot) ])))

(def rightPillar
  (let [x y w h] [(- (+ lintel_x lintel_w) lintel_h) lintelBot lintel_h pillarHeight]
  (let [fill stroke strokeWidth] [146 146 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth x y w h rot) ]))))

(blobs [
  leftPillar
  lintel
  rightPillar
])

"""

ladder =
 """; Ladder
;
; After Cheema, Gulwani, and LaViola \"QuickDraw: Improving Drawing Experience for Geometric Diagrams\" CHI 2012.
; mentioned as originally from an math text
;
; Repetition and best implementation of vertical constraint done by hand.
;
; Design decisions:
;   - Linear repetition specification (start sep n (below); start end n; start end sep)
;   - Ladder bottom: absolute (below) or offset from last rung
;   - If last rung endpoint specified: absolute or relative to ladder height or offset from ladder bottom
;


(def leftPost_y1 121)
(def leftPost_y2 250)
(def leftPost_x1 97)
(def rightPost_x1 207)
(def color 130)
(def width 7)
(def n 4{1-15})

(def leftPost
    [ (line color width leftPost_x1 leftPost_y1 leftPost_x1 leftPost_y2) ])

(def rightPost
    [ (line color width rightPost_x1 leftPost_y1 rightPost_x1 leftPost_y2) ])

(def rungs
    (def rung (\\i
    (let y (+ leftPost_y1 (* i 27.666666666666668))
        [(line color width leftPost_x1 y rightPost_x1 y) ])))
    (concatMap rung (zeroTo n)))

(blobs [
  leftPost
  rightPost
  rungs
])
"""

rails =
 """; Rails
;
; Make Equal helped set up the rails. The rest is basically hand-rolled.
;
; Design decisions:
;   - Ties/rails encoded relative to rails (below) or to BB or to centerline
;   - If encoded relative to rails, botRail y absolute (below) or offset from topRail y
;   - Tie sep measured between left edges or between facing edges (below)
;   - Tie count absolute (below) or relative to rail length or rail length relative to tie count
;   - Tie start position absolute or based on sep (below)
;   - Tie hangout absolue (below) or relative to rail offset or BB
;


(def railLength 331)
(def railWidth 10)
(def railStart 36)
(def railFill 460)
(def topRail_y 151)
(def botRail_y 204)

(def topRail
  (let [stroke strokeWidth] [78 0]
  (let rot 0
    [ (rawRect railFill stroke strokeWidth railStart topRail_y railLength railWidth rot) ])))

(def botRail
  (let [stroke strokeWidth] [78 0]
  (let rot 0
    [ (rawRect railFill stroke strokeWidth railStart botRail_y railLength railWidth rot) ])))

(def ties
  (let tie (\\i
    (let w 12
    (let sep 20.98947368421053
    (let sepw (+ sep w)
    (let x (+ (+ railStart (/ sep 2!)) (* i sepw))
    (let hangout 12
    (let y (- topRail_y hangout)
    (let h (+ (+ (- botRail_y topRail_y) railWidth) (* 2! hangout))
    (let [fill stroke strokeWidth] [39 85 0]
    (let rot 0
      [ (rawRect fill stroke strokeWidth x y w h rot) ]))))))))))
  (concatMap tie (zeroTo 10))))

(blobs [
  ties
  topRail
  botRail
])
"""

target =
 """; Target
;
; Design decisions:
;   - BB n or BB ringWidth or cx cy r n or cx cy n ringWidth (below) or cx cy r ringWidth
;   - Inner circle radius absolute or relative to ringWidth (below)
;

(def target
  (let circleCount 4{1-10}
  (let concentricCircle (\\i
    (let ringWidth 35
    (let [cx cy r] [182 244 (+ (* 0.7 ringWidth) (* i ringWidth))]
    (let colors [470 0]
    (let color (nth colors (mod i 2))
      [ (rawCircle color 360 0 cx cy r) ])))))

  (concatMap concentricCircle (reverse (zeroTo circleCount))))))

(blobs [
  target
])
"""

xs =
 """; Xs
;
; After David Maulsby p591 in \"Watch What I Do: Programming by Demonstration\" Appendix B. 1993.
;
; Design decisions:
;   - top left n boxWidth or top left w boxWidth or top left w n or cx cy n boxWidth (below) or cx cy r boxWidth or cx cy r n or BB boxWidth or BB n
;   - boxes square or rectangular (relevant to several of the above)
;   - n ∈ {1,2,3,4,...} (below) or n ∈ {1,3,5,7,...}
;   - colors global (below) or per function call
;

(def x (\\(cx cy n boxWidth)
  (let [centerColor descendingColor ascendingColor] [438 32 240]
  (let square (\\(cx cy color)
    [ (squareByCenter color cx cy boxWidth) ])
  (let drawLevel (\\i
    (if (= i 0)
      (square cx cy centerColor)
      (let offset (* i boxWidth)
        (concat [(square (- cx offset) (- cy offset) descendingColor)
                 (square (+ cx offset) (+ cy offset) descendingColor)
                 (square (- cx offset) (+ cy offset) ascendingColor)
                 (square (+ cx offset) (- cy offset) ascendingColor)]))))
    (concatMap drawLevel (zeroTo n)))))
  ))

(blobs [
  (x 200 250 3{1-6} 50)
])

"""

conifer =
 """; Conifer
;
; This design is surprisingly difficult to code.
;
; Some notable decisions:
;   - tree parameters global (below) or passed in to each function
;   - tree height computed or given (below)
;   - leaf separation absolute (below) or relative to leaf size
;   - leaf separation measured as between respective points on consectutive leaves (below) or as space between leaves
;   - branch start/end height on trunk absolute (below) or offset from base or a ratio
;   - branch repetition start end sep (below) or start end n or start sep n
;


(defrec spaced (\\(start stop sep)
  (let direction (sgn sep)
  (if (< (* direction start) (* direction stop))
    [start|(spaced (+ start sep) stop sep)]
    []))))

(def treeX 232)
(def treeBot 375)
(def treeHeight 301{1-500})
(def treeTop (- treeBot treeHeight))
(def [branchBot branchTop] [(- treeBot 25) (+ (- treeBot treeHeight) 15)])

(def trunk
  (let [baseY baseW] [treeBot 12]
  (let halfBaseW (/ baseW 2!)
  (let pts [[(- treeX halfBaseW) baseY]
            [(+ treeX halfBaseW) baseY]
            [treeX treeTop]]
  (let [color strokeColor strokeWidth] [31 0 0]
    [ (rawPolygon color strokeColor strokeWidth pts 0) ])))))


; Draw this to maniplate how high the branches start/stop
(def branchingLine
  (let [color width] [280 5]
    [ (line color width treeX branchBot treeX branchTop) ]))

(def leaf (\\(cx cy)
  (let [w h] [14 52]
  (let [halfW halfH] [(/ w 2!) (/ h 2!)]
  (let [left top right bot] [(- cx halfW) (- cy halfH) (+ cx halfW) (+ cy halfH)]
  (let pts [[left cy] [cx bot] [right cy] [cx top]]
  (let [color strokeColor strokeWidth] [127 0 0]
    [ (rawPolygon color strokeColor strokeWidth pts 0) ])))))))

(def branch (\\(baseY baseW length)
  (let halfBaseW (/ baseW 2!)
  (let pts [[treeX (- baseY halfBaseW)]
            [treeX (+ baseY halfBaseW)]
            [(+ treeX length) baseY]]
  (let [color strokeColor strokeWidth] [31 0 0]
    [ (rawPolygon color strokeColor strokeWidth pts 0) ])))))

(def leafyBranch (\\(baseY baseW length leafSep)
  (let halfLeafSep (/ leafSep 2!)
  (let leaves (concatMap (\\x (leaf x baseY)) (spaced (- (+ treeX length) halfLeafSep) (+ treeX halfLeafSep) (neg leafSep)))
  (concat [(branch baseY baseW length) leaves])))))

(def side (\\direction
  (let branchSep 38.5
  (let leafSep (* direction 18)
  (let drawLeafyBranch (\\y
    (let lengthRatio (- 1! (/ (- treeBot y) treeHeight))
    (leafyBranch y (* lengthRatio 30) (* direction (* lengthRatio 190)) leafSep)))
  (concatMap drawLeafyBranch (spaced branchBot branchTop (neg branchSep))))))))

(blobs [
  trunk
  (side 1!)
  (side -1!)
])
"""

ferris3 =
 """; Ferris Wheel 3
;
; Design decisions:
;   - Which variables to make global vs. local
;   - Ordering of cars/rim/spokes/hub
;

(def cx 230)
(def cy 245)
(def [r n rot] [137 5{0-20} 0.11{-3.2-3.2}])
(def ferrisPoints (nPointsOnCircle n rot cx cy r))
(def spokeWidth 3)
(def [spokeColor hubColor] [0 382])
(def [carColor leadCarColor] [50 100])

(def rim
  [(ring spokeColor spokeWidth cx cy r)])

(def car (\\([x y] color)
  (let [x y w h] [x y 43 44]
    [ (rectByCenter color x y w h) ])))

(def spoke (\\[x y]
    [ (line spokeColor spokeWidth cx cy x y) ]))

(def cars
  (concat (mapi (\\[i pt] (car pt (if (= 0 i) leadCarColor carColor))) ferrisPoints)))

(def spokes
  (concatMap spoke ferrisPoints))

(def hub
  (let r 24
    [ (rawCircle hubColor 360 0 cx cy r) ]))

(blobs [
  cars
  spokes
  rim
  hub
])
"""

gear =
 """; Gear
;
; Need fancier path tools to create the
; cut out via direct manipulation.
;
; Need better DM to manipulate the design (parameter
; control and better handling of rotation)
;
; Design decisions:
;   - tip diameter absolute or offset from root diameter
;   - points defined clockwise (below) or counterclockwise
;   - how to add the lettered path commands between the points (good opportunity for PBE)
;   - scope of unshared gear parameters, either local (below, mostly) or global
;

(def [cx cy] [200 200])
(def [rootRadius tipRadius] [120 170])
(def teethCount 9!{1-50})
(def [botLandRatio topLandRatio] [0.45 0.25])
(def rot 0)

; direction controls whether points are clockwise or counterclockwise
(def circlePathCmds (\\(cx cy r direction)
  (let sweepFlag (if (= 1 direction) 1 0)
  ['M' (+ cx (* direction r)) cy
   'a' (* direction r) r 0 0 sweepFlag (* direction (neg r)) r
   'a' (* direction r) r 0 0 sweepFlag (* direction (neg r)) (neg r)
   'a' (* direction r) r 0 0 sweepFlag (* direction r) (neg r)
   'a' (* direction r) r 0 0 sweepFlag (* direction r) r
   'z'])))

(def gearPts (\\(rotAngularRatioOffset r)
  (let angularPitch (/ twoPi teethCount)
  (reverse (nPointsOnCircle teethCount (+ rot (* angularPitch rotAngularRatioOffset)) cx cy r)))))

; 0 degrees is centered on a tip
(def topLeftPts  (gearPts (/ topLandRatio -2!) tipRadius))
(def topRightPts (gearPts (/ topLandRatio  2!) tipRadius))
(def botLeftPts  (gearPts (+ 0.5! (/ botLandRatio -2!)) rootRadius))
(def botRightPts (gearPts (+ 0.5! (/ botLandRatio  2!)) rootRadius))

; Now interleave all the points
(def pathPts (concat (map4 (\\(pt1 pt2 pt3 pt4) [pt1 pt2 pt3 pt4]) topLeftPts topRightPts botLeftPts botRightPts)))

(def pathCmds
   (concat (intermingle (cons ['M'] (snoc ['Z'] (repeat (- (* 4! teethCount) 1!) ['L']))) pathPts)))

(def gear
  (let [strokeColor strokeWidth color] [372 5 352]
    [ (rawPath color strokeColor strokeWidth (append pathCmds (circlePathCmds cx cy 50 -1!)) 0) ]))


(blobs [
  gear
])
"""

kochSnowflake =
 """; Koch Snowflake
;
; Can be accomplished by graphical search and replace per
; David Kurlander p556 in \"Watch What I Do: Programming by Demonstration\" Appendix B. 1993.
;
; What would have helped the creation of the implementation below
; was some local view of a functions computation (onLine, normPt).
; Might be nice to augment with program synthesis.
;
; Design decisions:
;   - as patterns (below) or reconstructing points
;   - manual calculation of dx dy (below) or use of prelude vec2DMinus function
;   - draw as many lines or polyline or polygon (below) or path
;   - edge math calculates the three subdivistion points (below) or uses vector addition and rotation functions like turtle graphics
;   - repeated edge calls (below) or concatMap over list of points
;   - The equation for edge width: 1/3 for each successive iteration (below) or constant
;

(def iterations 3!{0-4})
; Each iteration is 1/3 the scale of the prior, so
; we divide the line width by 3 on each iteration.
(def [fill stroke width] [150 386 (/ 45 (pow 3! iterations))])

; Point on normal of line, at `ratio` distance from the line
; relative to line length.
(def normPt (\\(pt1@[x1 y1] pt2@[x2 y2] ratio)
  (let vec@[dx dy] [(- x2 x1) (- y2 y1)]
  (vec2DPlus (vec2DScalarMult ratio [(neg dy) dx]) (halfwayBetween pt1 pt2)))))

; Recursive fractal edge.
;
; Returns list of points (except the last point)
(defrec edge (\\(pt1@[x1 y1] pt2@[x2 y2] iterationsLeft)
  (if (= 0 iterationsLeft)
    [ pt1 ]
    (let [thirdPt twoThirdsPt] [(onLine pt1 pt2 (/ 1 3!)) (onLine pt1 pt2 (/ 2 3!))]
    (let outPt (normPt pt1 pt2 (* (/ 1 3!) (/ (sqrt 3!) 2!)))
    (concat [ (edge pt1         thirdPt     (- iterationsLeft 1))
              (edge thirdPt     outPt       (- iterationsLeft 1))
              (edge outPt       twoThirdsPt (- iterationsLeft 1))
              (edge twoThirdsPt pt2         (- iterationsLeft 1))
            ]))))))

; Points of initial equilateral triangle.
(def [triPt1 triPt2 triPt3] (nPointsOnCircle 3! 0 300 300 200))

(def snowflakePts
  (concat [ (edge triPt1 triPt2 iterations)
            (edge triPt2 triPt3 iterations)
            (edge triPt3 triPt1 iterations)
          ]))

(def snowflake
  [ (polygon fill stroke width snowflakePts) ])

(blobs [
  snowflake
])
"""

replaceTerminalsWithWorkstations =
 """; Replace Terminals With Workstations
;
; A Demo of Kurlander's graphical search and replace.
; David Kurlander p573,pp275-277 in \"Watch What I Do: Programming by Demonstration\" Appendix B. 1993.
;
; Design decisions:
;   - x y w h or left top right bot (below) for workstation display and pillar
;   - how to encode the equalization of the centerpoints of the keyboard, pillar, and display (used \"Make Equal\" version 1 below)
;   - how to parameterize the design into a function: x y (below) or x y size or left top right bot
;   - whether to parameterize any other details (e.g. screen color)
;   - how to implement the x y offset: map and add in each location (below) or prelude SVG translate function (not implemented yet)
;


(def terminal (\\(x y)
  (let origin [x y]
  (let offset (vec2DPlus origin) ; second point unapplied
  (let body
    (let pts [[-9 154] [76 166] [150 136] [146 14] [54 6] [36 15] [38 117]]
    (let [color strokeColor strokeWidth] [454 360 2]
      [ (rawPolygon color strokeColor strokeWidth (map offset pts) 0) ]))

  (let keyboard
    (let pts [[15 147] [46 124] [102 131] [69 154]]
    (let [color strokeColor strokeWidth] [462 360 2]
      [ (rawPolygon color strokeColor strokeWidth (map offset pts) 0) ]))

  (let screen
    (let pts [[55 88] [54 32] [108 38] [109 94]]
    (let [color strokeColor strokeWidth] [103 360 2]
      [ (rawPolygon color strokeColor strokeWidth (map offset pts) 0) ]))

  (concat [ body keyboard screen ]))))))))


(def workstation (\\(x y)
  (let origin [x y]
  (let offset (vec2DPlus origin) ; second point unapplied
  (let [polygon14_pts_k4862 rect11_bounds_left rect11_bounds_right] [71 104 148]
  (let k4859 (- (+ rect11_bounds_left rect11_bounds_right) polygon14_pts_k4862)
  (let rect12_bounds_left 46
  (let right' (- (+ rect11_bounds_left rect11_bounds_right) rect12_bounds_left)

  (let displayPillar
    (let bounds @ [left top right bot] [(+ x rect11_bounds_left) (+ y 115) (+ x rect11_bounds_right) (+ y 151)]
    (let color 364
      [ (rectangle color 360 0 0 bounds) ]))

  (let display
    (let bounds @ [left top right bot] [(+ x rect12_bounds_left) (+ y 14) (+ x right') (+ y 110)]
    (let color 110
      [ (rectangle color 360 15 0 bounds) ]))

  (let keyboard
    (let pts_k4861 150
    (let pts_k4855 178
    (let pts [[polygon14_pts_k4862 pts_k4861] [k4859 pts_k4861] [201 pts_k4855] [53 pts_k4855]]
    (let [color strokeColor strokeWidth] [460 360 5]
      [ (rawPolygon color strokeColor strokeWidth (map offset pts) 0) ]))))

  (let mouseCord
    (let [strokeColor strokeWidth color] [369 1 'none']
    (let [x0 y0] [21 159]
    (let d ['M' (+ x x0) (+ y y0) 'L' (+ x 36) (+ y 144) 'L' (+ x 69) (+ y 137) 'L' (+ x 100) (+ y 134) 'L' (+ x 133) (+ y 136)]
      [ (rawPath color strokeColor strokeWidth d 0) ])))

  (let mouseBody
    (let pts [[-5 176] [12 158] [28 158] [18 177]]
    (let [color strokeColor strokeWidth] [462 360 2]
      [ (rawPolygon color strokeColor strokeWidth (map offset pts) 0) ]))

  (let mouseButton
    (let pts [[10 165] [14 161] [23 161] [18 166]]
    (let [color strokeColor strokeWidth] [364 360 2]
      [ (rawPolygon color strokeColor strokeWidth (map offset pts) 0) ]))

  (concat [ displayPillar display keyboard mouseCord mouseBody mouseButton ])))))))))))))))

(def terminalsOrWorkstations 0!{0-1})
(def terminalOrWorkstation
  (if (< terminalsOrWorkstations 0.5!) terminal workstation))

(blobs [
  (terminalOrWorkstation 0 0)
  (terminalOrWorkstation 300 200)
  (terminalOrWorkstation 200 300)
  (terminalOrWorkstation 400 400)
])
"""

balanceScale =
 """; Balance Scale
;
; After David Kurlander p568 in \"Watch What I Do: Programming by Demonstration\" Appendix B. 1993.
;
; Had to hand-code the arc.
;
; It's a reasonable goal to support the ability to draw out the
; particular constraints that determine the placement of the weights.
;
; The mathematics for balancing the scale should probably
; not be a goal for direct manipulation (for the time being).
;
; Design decisions:
;   - Use polyline (below) or two lines for balance arms
;   - Arm pivot y absolute (below) or relative to top of pillar or pillar top relative to pivot
;   - Arms angle in degrees (below) or radians or specified as a distance to drop
;   - Tray arc radius absolute (below) or a ratio of width
;   - Each tray hung by two lines or one polyline (below)
;


(def [pillar_x base_cx] [276.5 308])
(def [pillar_y base_cy] [145 497])


(def base
  (let [ rx ry] [ 95 29]
  (let [color rot] [420 0]
    [ (rawEllipse color 360 0 base_cx base_cy rx ry rot) ])))

(def pillar
  (let [ w h] [ (* 2! (- base_cx pillar_x)) (- base_cy pillar_y)]
  (let [fill stroke strokeWidth] [41 245 0]
  (let rot 0
    [ (rawRect fill stroke strokeWidth pillar_x pillar_y w h rot) ]))))

(def pivot@[px py] [base_cx 181])
(def armsInnerAngle 2.7!{0-3.14159})
(def halfArmsInnerAngle (/ armsInnerAngle 2!))
(def armLength 200)
(def trayHangHeight 218.00000000000003)
(def [leftWeightWidth  leftWeightHeight]  [65 65{0-120}])
(def [rightWeightWidth rightWeightHeight] [65{0-120} 65])
(def leftWeightMass  (* leftWeightWidth  leftWeightHeight))
(def rightWeightMass (* rightWeightWidth rightWeightHeight))
(def totalMass (+ leftWeightMass rightWeightMass))
; Where does the center of mass fall on the invisible line between the trays?
; (0 = all the way left, 1 = all the way right)
(def centerOfMassLeftRightRatio (/ rightWeightMass totalMass))

(def threePiOverTwo (* 1.5! (pi)))
(def arctan (\\theta (arctan2 theta 1!)))
(def tan (\\theta (/ (sin theta) (cos theta))))

; Per my hand calculations
(def thetaMethod1
  (arctan (* (- 1! (* 2! centerOfMassLeftRightRatio)) (tan halfArmsInnerAngle))))
; Per http://stackoverflow.com/a/4451915 arctan[tan(phi)*(m1+m2)/(m1-m2)]
(def thetaMethod2
  (let phi (- halfPi halfArmsInnerAngle)
  (- halfPi (arctan (/ (* (tan phi) totalMass) (- leftWeightMass rightWeightMass))))))
; They give the same results, though the S.O. answer chooses the opposite arctan for some angles.
(def theta thetaMethod1)


(def [leftHangPt rightHangPt]
  (let [leftArmAngle rightArmAngle] [(+ theta (- threePiOverTwo halfArmsInnerAngle))
                                     (+ theta (+ threePiOverTwo halfArmsInnerAngle))]
  (map (compose (vec2DPlus pivot) (vec2DScalarMult armLength))
    [[(cos leftArmAngle)  (neg (sin leftArmAngle))]
     [(cos rightArmAngle) (neg (sin rightArmAngle))]])))

(def arms
  (let pts [leftHangPt [base_cx 181] rightHangPt]
  (let [color strokeColor strokeWidth] [\"none\" 430 30]
    [ (addAttr (polyline color strokeColor strokeWidth pts) [\"stroke-linecap\" \"round\"]) ])))

(def weight (\\(cx bot w h)
  (let halfWidth (/ w 2!)
  (let [left top right] [(- cx halfWidth) (- bot h) (+ cx halfWidth)]
  (let color 275
    [ (rectangle color 360 0 0 [left top right bot]) ])))))

(def hangingTray (\\(hangPoint@[hangX hangY] weightWidth weightHeight)
  (let [strokeColor strokeWidth fill] [380 4 52]
  (let w 100
  (let y (+ hangY trayHangHeight)
  (let arcRadius 300
  (let [left right] [(- hangX w) (+ hangX w)]
  (let d [\"M\" left y \"L\" right y \"A\" arcRadius arcRadius 0 0 1 left y \"Z\"]
    (concat [
      [ (path fill strokeColor strokeWidth d) ]
      [ (polyline 'none' strokeColor strokeWidth [[left y] hangPoint [right y]]) ]
      (weight hangX y weightWidth weightHeight)
    ])))))))))

(blobs [
  base
  pillar
  arms
  (hangingTray leftHangPt  leftWeightWidth  leftWeightHeight)
  (hangingTray rightHangPt rightWeightWidth rightWeightHeight)
])
"""

pencilTip =
 """; Pencil Tip
;
; Had to hand code:
;   - renamings
;   - initial eraser path arcs
;   - eraser path arc relations
;   - co-linearity of graphite tip with shaved wood wedge
;
; Design decisions:
;   - Parts horizonal parameters absolute (below) or offset from each other or relative to entire pencil width
;   - Pencil vertically paramererized as top/bottom (below) or top/width or centerY/width
;   - Pencil constituent rects left/right/top/bot (below) or x/y/w/h
;   - Eraser left x, corner bend start x, corner radius dependency: bend start x on radius and left (below), left on radius and bend start x, or radius on left and bend start x.
;   - Pencil wood left, wood top right corner, and tip x: wood top right corner on wood left, tip x, and ratio (below); tip x and wood corner top on wood left, wood right, and ratio; tip x on wood left and wood top right corner; tip x on tip angle, wood top right corner on ratio
;

(def eraserRight 134)
(def [pencilTop pencilBot] [130 266])
(def pencilCenterY (* 0.5! (+ pencilBot pencilTop)))

(def eraser
  (let [strokeColor strokeWidth color] [254 0 4]
  (let cornerRadius 13
  (let left 107
  (let bendStartX (+ left cornerRadius)
  (let d ['M' eraserRight pencilTop
          'L' eraserRight pencilBot
          'L' bendStartX pencilBot
          'A' cornerRadius cornerRadius 0 0 1 left (- pencilBot cornerRadius)
          'L' left (+ pencilTop cornerRadius)
          'A' cornerRadius cornerRadius 0 0 1 bendStartX pencilTop
          'Z']
    [ (rawPath color strokeColor strokeWidth d 0) ]))))))

(def ferrule_right 194)

(def ferrule
  (let bounds @ [left top right bot] [eraserRight pencilTop ferrule_right pencilBot]
  (let color 458
    [ (rectangle color 360 0 0 bounds) ])))

(def body_right 334)

(def body
  (let bounds @ [left top right bot] [ferrule_right pencilTop body_right pencilBot]
  (let color 43
    [ (rectangle color 360 0 0 bounds) ])))

(def tipRatio 0.3)
(def tipRight 437)

(def [woodRight tipTopY] (onLine [tipRight pencilCenterY] [body_right pencilTop] tipRatio))
(def tipBotY (- (+ pencilBot pencilTop) tipTopY))

(def wood
  (let pts [[body_right pencilBot] [body_right pencilTop] [woodRight tipTopY] [woodRight tipBotY]]
  (let [color strokeColor strokeWidth] [470 360 0]
    [ (rawPolygon color strokeColor strokeWidth pts 0) ])))

(def tip
  (let pts [[woodRight tipBotY] [woodRight tipTopY] [tipRight pencilCenterY]]
  (let [color strokeColor strokeWidth] [402 360 0]
    [ (rawPolygon color strokeColor strokeWidth pts 0) ])))

(blobs [
  eraser
  ferrule
  body
  wood
  tip
])
"""

calendarIcon =
 """; Calendar Icon
;
; After Bernstein and Li \"Lillicon\" 2015.
;
; Design Decisions:
;   - rectangles x/y/w/h or let/top/right/bot (below)
;   - day locations with margin calculated with start sep n or margin inserted per day based on point pairs from start end n (below)
;   - cell margins constant or based on number of days in a month (below)
;   - days in a month calculated inline (below) or a separate variable
;


(def daysInAWeek 4!{1-15})
(def weeksInAMonth 3!{1-12})
(def edgeMargin 10)
(def dayMargin (/ (/ 196 daysInAWeek) weeksInAMonth))

(def left 91)
(def right 468)
(def backgroundColor 325)

(def topBar
  (let bounds @ [left top right bot] [left 71 right 125]
    [ (rectangle backgroundColor 360 0 0 bounds) ]))

; end points included as part of n
(def nPointsBetween (\\(start end n)
  (let sep (/ (- end start) (- n 1!))
  (map (\\i (+ start (* i sep))) (zeroTo n)))))

(def [paperTop paperBot] [143 435])

(def xs (nPointsBetween (+ left edgeMargin) (- right edgeMargin) (+ 1! daysInAWeek)))
(def ys (nPointsBetween (+ paperTop edgeMargin) (- paperBot edgeMargin) (+ 1! weeksInAMonth)))
(def consecutiveXs (zip xs (drop xs 1)))
(def consecutiveYs (zip ys (drop ys 1)))
; Drop some days from beginning/end of month
;
; cartProd argument order is so that we drop from the first row rather than first column
(def dayBounds
  (let dropFirstN (round (* daysInAWeek 0.3!))
  (let dropLastN (round (* daysInAWeek 0.3!))
  (map reverse (dropEnd (drop (cartProd consecutiveYs consecutiveXs) dropFirstN) dropLastN)))))

(def paper
  (let bounds @ [left top right bot] [left paperTop right paperBot]
    [ (rectangle backgroundColor 360 0 0 bounds) ]))

(def days
  (map
    (\\[[left right] [top bot]]
      (rectangle 479 360 0 0 [(+ left dayMargin) (+ top dayMargin) (- right dayMargin) (- bot dayMargin)]))
    dayBounds))

(blobs [
  topBar
  paper
  days
])
"""

sns_deuce =
 """
(def logo (\\(x1 y1 w h fill stroke strokeWidth)
  (let rectangle (rect fill x1 y1 w h)
  (let line1 (line stroke strokeWidth x1 y1 (+ x1 w) (+ y1 h))
  (let line2 (line stroke strokeWidth x1 (+ y1 h) (+ x1 (/ w 2)) (+ y1 (/ h 2)))
  [rectangle line1 line2])))))

(svg (concat [
  (logo 33 21 135 120 'purple' 'orange' 15)
  (logo 180 99 72 31 'darkgreen' 'pink' 3)
  (logo 186 26 60 60 'black' 'white' 5)
]))

"""

target_deuce =
 """(def target (\\(cx cy ringWidth ringCount)
  (let ring (\\i
    (let fill (if (= 0 (mod i 2)) 'firebrick' 'lightgray')
    (circle fill cx cy (* ringWidth i))))
    
  (map ring (reverse (range 1 ringCount))))))

(svg (target 200 200 50 4))

"""

battery_deuce =
 """
(def battery (\\(topLeft bodyWidth bodyHeight capWidth capHeight strokeWidth juicePct)
  (let topRight (vec2DPlus topLeft [bodyWidth 0!])

  (let body
    (let pts [ topLeft
               (vec2DPlus topLeft [0! bodyHeight])
               (vec2DPlus topLeft [bodyWidth bodyHeight])
               topRight ]
    (addAttr
      (polygon 'none' 'black' strokeWidth pts)
      [\"stroke-linejoin\" \"round\"]))
    
  (let cap
    (let [x1 y1] (vec2DPlus topRight [0! (/ (- bodyHeight capHeight) 2!)])
    (rect 'black' x1 y1 capWidth capHeight))
  
  (let juice
    (let w (* juicePct bodyWidth)
    (let fill (if (< juicePct 0.2) 'red'
              (if (< juicePct 0.4) 'orange'
              (if (ge juicePct 1)  'green'
                                   'black')))
    (rect fill (fst topLeft) (snd topLeft) w bodyHeight)))

    [ juice body cap ]))))))
  
(svg (concat [
  (battery [160 80] 198 107 30 41 14{0-40} 1{0.001-1})
  (battery [101 253] 201 110 19 44 12{0-40} 0.8258406060606062{0.001-1})
]))

"""

coffee_deuce =
 """
(def mug

  (let fill 206
  (let [x1 y1 w h] [41 181 155 182]
  (let [handle_x handle_y] [(+ x1 w) (+ y1 63)]
  (let [rx ry] [52.899480795897965 46.77512980102551]

  (let [x02 y02] [(+ x1 43) (- y1 80)]

  (let body
    (rect fill x1 y1 w h)
    
  (let outer_handle
    (ellipse fill handle_x handle_y rx ry)
    
  (let inner_handle
    (let num 0.6201800000000001{0.001-1}
    (ellipse 'white' handle_x handle_y (* num rx) (* num ry)))
  
  (let steam (\\(x02 y02)
    (let [strokeColor strokeWidth color] [440 5 499]
    (let [x0 y0] [ x02 y02]
    (let d ['M' x0 y0
            'C' (- x0 21) (+ y0 26) (+ x0 19) (+ y0 34) (- x0 5) (+ y0 67)
            'C' (+ x0 27) (+ y0 19) (- x0 8) (+ y0 17) x0 y0 'Z']
    (rawPath color strokeColor strokeWidth d 0)))))

  (let steam1
    (steam x02 y02)
    
  (let steam2
    (steam (+ x02 59) (- y02 3))
    
  (let steam3
    (steam (+ x02 30) (- y02 8))
    
  [ outer_handle inner_handle body steam1 steam2 steam3 ])))))))))))))

(svg mug)

"""

mondrian_arch_deuce =
 """;
; Mondrian Arch
;
; After Henry Lieberman p554 in \"Watch What I Do:
; Programming by Demonstration\" Appendix B. MIT Press. 1993.
;

(def arch
  (\\(archLeft archTop archWidth pillarHeight stoneWidth)
  (let pillarTop (+ archTop stoneWidth)
  (let rightPillarLeft (- (+ archLeft archWidth) stoneWidth)
    [
      (rect 'red'   archLeft        archTop   archWidth  stoneWidth)
      (rect 'green' archLeft        pillarTop stoneWidth pillarHeight)
      (rect 'blue'  rightPillarLeft pillarTop stoneWidth pillarHeight)
    ]))))

(svg (concat [
  (arch 73  44  378 280 55)
  (arch 276 143 106 138 40)
  (arch 131 159 118 26  54)
]))

"""


--------------------------------------------------------------------------------
-- Deuce User Study Files

-- these .little files are generated from somewhere else;
-- don't change them manually

study_start =
 """
(def main
  (draw []))
; <!-- PROSE --> <p>In this study, we are going to show you some program editing features in Sketch-n-Sketch, a programming system for generating Scalable Vector Graphics images.</p><p><strong>Tutorial:</strong> In the first half, you will work through a tutorial that explains several features in Sketch-n-Sketch.</p><p><strong>Tasks:</strong> In the second half, you will work through several programming editing tasks using the features described in the tutorial.</p><p>See the Next Step button in the top-right corner of the screen? Every time you've finished a section, press Next Step to move on.</p>

"""

study_transition_1 =
 """
(def main
  (draw [(show \"That's the end of the tutorial! Let the facilitator know.\")]))
; <!-- PROSE --> <p>Great job, you've completed the tutorial! Now you're going to use Sketch-n-Sketch to perform some program editings tasks.</p><p>In the next section, you will see three example programs, each with a list of editing tasks. You will <em>not</em> be allowed to use text editing for any of the tasks. Instead, you will use the code transformation tools. You will work with each program twice:</p><ul><li>using code tools with Text-Select Mode, and</li><li>using code tools with Box-Select Mode.</li></ul><p>The order in which you are asked to perform tasks and use the different modes is random.</p><p>Before each task, you will have the opportunity to read the code to understand it. All editing features are disabled during this reading period.</p><p>Then, you will be given a list of editing tasks to perform. The top of the file will tell you to use either Text-Select Mode or Box-Select Mode. When you are done with each task, click Next Step to move on.</p><p>Let the facilitator know that you are ready to start. When given the okay, click Next Step to begin this sequence of tasks. Once you click Next Step, you will no longer be able to go the Previous Step of the tutorial.</p>

"""

study_transition_2 =
 """
(def main
  (draw []))
; <!-- PROSE --> <p>In the final section, you will see two example programs, each with a list of editing tasks. As before, you will <em>not</em> be able to use text editing. But, this time, you will be able to use <em>either</em>:</p><ul><li>code tools with Text-Select Mode, or</li><li>code tools with Box-Select Mode.</li></ul><p>It's up to you. You can mix and match Text-Select and Box-Select Modes, like in the tutorial.</p><p>And this time, you will work with each program just once.</p><p>Click Next Step to proceed.</p>

"""

study_end =
 """
(def main
  (draw [(show \"That's the end of the tasks!\")]))
; <!-- PROSE --> <p>You're all done with the programming exercises!</p><p>One last thing: please fill out the <a href=\"/survey\">exit survey</a>.</p><p>Thanks!</p>

"""


tutorial_step_01 =
 """
(def main
  (draw []))
; <!-- PROSE --> <p>Sketch-n-Sketch programs are written in a small functional language with features commonly found in other functional languages and with syntax that resembles Lisp or Racket. Significant or recent experience with functional programming is not necessary to complete the tasks.</p><p>To start, we will introduce the basics of how the language works.</p><p><strong>Note:</strong> You will <em>not</em> need to memorize all the precise details of how the language works; a basic and general understanding will be sufficient for this user study.</p>

"""

tutorial_step_02 =
 """
(def x 1)   ; var x = 1

(def y 2)   ; var y = 2

(def z 3)   ; var z = 3

(def main
  (draw []))
; <!-- PROSE --> <p>The structure of a program is a series of top-level definitions. The last definition is called <code>main</code> and describes the image to draw in the canvas area on the right. The initial <code>main</code> expression has an empty list, so nothing is drawn.</p><p>Note that any text on the same line following a semicolon (<code>; like this</code>) is a <em>comment</em> and has no effect on the output of the program.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li><p>Add the expression <code>(show &quot;hello world!&quot;)</code> to the list of drawings. Notice how the border of the canvas turns red; this indicates that the code needs to be re-run. Hit the Run button.</p></li><li><p>Now define this expression in a new variable instead, and refer to it in the <code>main</code> expression.</p></li><li><p>Remove a comment from the program. Notice how this change has no effect on the output.</p></li><li><p>Add extra parentheses somewhere and re-run. You will get a syntax error. Many expressions start and end with parentheses; extra parentheses are not allowed.</p></li></ol>

"""

tutorial_step_03 =
 """
(def str \"hello world!\")

(def num 3.14)

(def bool true)

(def list
  [1 2 3 true [false \"abc\"] str num bool])

(def main
  (draw [(show list)]))
; <!-- PROSE --> <p>The language has several kinds of data: strings, numbers, booleans, and lists.</p><p>Lists are written with square brackets and with spaces separating the elements.</p><p>The language does not have static types, so we can mix and match different types of elements within a list.</p>

"""

tutorial_step_04 =
 """
(def [one two] [1 \"two\"])

(def main
  (draw [(show [one two one two])]))
; <!-- PROSE --> <p>We use the term &quot;tuple&quot; to describe a list with a fixed number of elements. For example, the list expression <code>[1 &quot;two&quot;]</code> in this code is a 2-tuple, that is, a list with exactly two elements.</p><p>Notice how we access the components of tuple expressions with tuple patterns (instead of just plain variables) on the left-hand side of definitions. The tuple pattern <code>[one two]</code> above matches a list with exactly two elements, binding the name one to the first element and two to the second.</p><p>If the tuple pattern and tuple expression do not have the same number of elements, Sketch-n-Sketch throws a run-time error.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li><p>Add a third variable to the tuple pattern and a third value to the list expression, and re-run.</p></li><li><p>Remove one of the expressions from the list expression (so that the number of variables and expressions are unequal), and re-run.</p></li></ol>

"""

tutorial_step_05 =
 """
(def nums
  (let one 1
    [one \"two\" one \"two\"]))

(def main
  (draw [(show nums)]))
; <!-- PROSE --> <p>In addition to top-level definitions, local variables can be created using a different keyword called <code>let</code>. For example, within the <code>nums</code> definition, we can define the local variable <code>one</code> to hold the value <code>1</code>, and then refer to that value with the name <code>one</code> in the rest of the let-expression.</p><p>The syntax for a let definition is <code>(let x e1 e2)</code>, which says &quot;let <code>x</code> refer to <code>e1</code> when evaluating <code>e2</code>&quot;. Notice how parentheses surround the entire let definition.</p><p>The binding of <code>x</code> is only in scope in the expression <code>e2</code>. For example, the <code>one</code> variable in this code is not in scope outside of the <code>nums</code> definition.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li><p>Try referencing the <code>one</code> variable outside the <code>nums</code> definition.</p></li><li><p>Tuple patterns can be used in let definitions. Define the tuple pattern <code>[one two]</code> to be <code>[1 2]</code> inside <code>nums</code>.</p></li></ol>

"""

tutorial_step_06 =
 """
(def triplicate (\\x [x x x]))

(def triplicate2 (\\(x y) [x y x y x y]))

(def main
  (draw [(show (triplicate 3))]))
; <!-- PROSE --> <p>The syntax for a function expression is <code>(\\x e)</code>, which defines a function that takes an argument <code>x</code> and returns the expression <code>e</code>.</p><p>The syntax for a multi-argument function is <code>(\\(x1 x2 x3 ...) e)</code>. Notice the parentheses surrounding the list of arguments.</p><p>For example, the function <code>triplicate</code> takes a single argument <code>x</code> (and returns a list that repeats <code>x</code> three times), and <code>triplicate2</code> takes two arguments. To call any function, the function name and its argument are separated by a space, and the entire call is surrounded by parentheses.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li><p>Rename the argument of <code>triplicate</code> to something else and re-run.</p></li><li><p>Add the function call <code>(triplicate2 3 4)</code> in the <code>main</code> expression.</p></li><li><p>Call <code>triplicate2</code> with too many arguments. You'll see a run-time error.</p></li><li><p>Call <code>triplicate2</code> with too few arguments. You won't get a run-time error, but the result shown will be <code>&lt;fun&gt;</code> (a function that is waiting for the rest of its arguments).</p></li></ol>

"""

tutorial_step_07 =
 """
(def foo (\\(x y) [(+ x y) (- x y) (* x y) (/ x y)]))

(def multiply3 (\\(x y z) (* x (* y z))))

(def max (\\(x y)
  (if (< x y) y x)))

(def main
  (draw [
    (show (foo 3 4))
  ]))
; <!-- PROSE --> <p>The language has several binary operators, as well as if-then-else-expressions.</p><p>Don't worry about the syntax too much.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li><p>Replace the call to <code>foo</code> in the <code>main</code> expression with a call to <code>multiply3</code>.</p></li><li><p>Replace the call to <code>foo</code> in the <code>main</code> expression with a call to <code>max</code>.</p></li></ol>

"""

tutorial_step_08 =
 """
(def redLine
  (let [w x1 y1 x2 y2] [4 100 100 300 300]
    (line \"salmon\" w x1 y1 x2 y2)))

(def greenRect
  (let [x y w h] [100 100 150 80]
    (rect \"lightgreen\" x y w h)))

(def blueCircle
  (let [cx cy r] [100 100 20]
    (circle \"lightblue\" cx cy r)))

(def main
  (draw [redLine greenRect blueCircle]))
; <!-- PROSE --> <p>Now we'll see how to draw three basic shapes: lines, rectangles, and circles. Notice how elements later in the <code>main</code> list appear on top of earlier ones.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li><p>Make the rectangle a square.</p></li><li><p>Reorder the elements in <code>main</code> so that the red line is on top of the green rectangle.</p></li><li><p>All three shapes share the xy-point <code>(100, 100)</code>. Define variables for these two values and use them in all three shapes.</p></li></ol>

"""

tutorial_step_09 =
 """
(def main
  (draw []))
; <!-- PROSE --> <p>So far, we've seen how to write code using the text editor.</p><p>Next, we'll see some tools that Sketch-n-Sketch provides for automatically performing code transformations.</p>

"""

tutorial_step_10 =
 """
(def blueSquare
  (rect \"salmon\" 100 70 80 80))

(def main
  (draw [blueSquare]))
; <!-- PROSE --> <p>Here is a red square. Unfortunately, the name of the function that generates this red square is incorrect. To fix this, we can use one of the <em>code tools</em> that are built in to Sketch-n-Sketch.</p><p>The Code Tools menu at the top-left of the screen lists all of the tools. In this case, we want the <em>Rename Variable</em> to rename the variable at its definition on line 2 and its use on line 6.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Choose the <em>Rename Variable</em> tool from the Code Tools menu. The configuration panel that appears asks for a variable to be selected. Hover over and click <code>blueSquare</code> either on line 2 or line 6. Notice that the Code Updates section of the configuration panel now displays a text box in which to enter a new name. Type a new name (like <code>redSquare</code>) in the text box and press Enter. Notice how the variable definition and use are changed. Unlike with text edits, you do <em>not</em> need to hit the Run button after invoking a code transformation.</li></ol>

"""

tutorial_step_11 =
 """
(def blueSquare
  (rect \"salmon\" 100 70 80 80))

(def main
  (draw [blueSquare]))
; <!-- PROSE --> <p>Here is the incorrectly-named red square again. Besides selecting <em>Rename Variable</em> directly from the Code Tools menu, there are a few ways additional ways to invoke the <em>Rename</em> tool:</p><ul class=\"_123\"><li class=\"_1\"><h3>Text-Select Mode (right-click menu)</h3><p>Select the text <code>blueSquare</code>, right-click the editor, and select the <em>Rename</em> tool from the pop-up menu. Then, enter a new name (like <code>redSquare</code>) in the results list and press Enter.</p><p><strong>Note:</strong> The Escape key will make the menu or panel go away. <em>⟲ Undo</em> will let you go back.</p></li><li class=\"_2\"><h3>Text-Select Mode (Code Tools menu)</h3><p>Select the text <code>blueSquare</code>, and select the <em>Rename</em> Variable tool from the Code Tools menu at the top-left of the screen. Then enter a new name (like <code>redSquare</code>) in the results list and press Enter.</p><p><strong>Note:</strong> The Escape key will make the panel go away. <em>⟲ Undo</em> will let you go back.</p></li><li class=\"_3\"><h3>Box-Select Mode</h3><p>Hold down the Shift key, hover over <code>blueSquare</code>, click the box that pops up over <code>blueSquare</code>, and then release the Shift key. Select the <em>Rename</em> tool from the pop-up menu. Enter a new name (like <code>redSquare</code>) in the results list and press Enter.</p><p><strong>Note:</strong> The Escape key will clear any selections and make the menu go away. <em>⟲ Undo</em> will let you go back.</p></li></ul><p>Unlike with text edits, you do <em>not</em> need to hit the Run button after invoking a code transformation.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li><p>Perform the renaming with each of the three interactions. Use Undo after each to reset the program.</p></li><li><p><strong>Tip:</strong> To save time in Text-Select Mode, a right-click can be performed without a text selection for <em>variables</em> and <em>constants</em> (but not for more complicated expressions). In this case, Sketch-n-Sketch will automatically select the variable or constant that contains the click that was performed. Try this out by redoing the instructions for <em>Text-Select Mode (right-click menu)</em>, this time by right-clicking <code>blueSquare</code> without text-selecting it.</p></li><li><p><strong>Tip:</strong> The pop-up panels can be re-positioned if they are covering up relevant parts of the code. Redo any one of the interactions and, when a pop-up panel appears, click and drag the top bar to move it somewhere else.</p></li></ol>

"""

tutorial_step_12 =
 """
(def redSquare
  (rect \"salmon\" 100 70 80 80))

(def main
  (draw [redSquare]))
; <!-- PROSE --> <p>Many times, when using a function like <code>rect</code>, passing in multiple constants rather than named variables can lead to confusion as to the purpose of each constant.</p><p>To alleviate this, we can introduce variables using the <em>Introduce Variable</em> tool. This tool uses the notion of a <em>target position</em>, which is the whitespace between or above other items in the code. Target positions typically allow us to select where we want resultant transformations to appear in the code. In the case of the <em>Introduce Variable</em> tool, the target position indicates where we would like the variables to be introduced.</p><p>Let's try out the <em>Introduce Variable</em> tool on the x- and y- positions of the red square (the first two arguments of the <code>rect</code> function after the color, &quot;salmon&quot;).</p><p>We can use this code tool (and all others) in two main ways:</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select one of the <code>80</code>s, then either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Introduce Variable</em>. The configuration panel asks for one or more expressions for which variables will be introduced, as well as an optional target position. Click the other <code>80</code>, then click the whitespace (on line 3) before the call to <code>rect</code> or the whitespace (on line 1) above the <code>def</code>. Apply the transformation from the results list.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down Shift, then click on both occurrences of the number <code>80</code>, as well as the whitespace (on line 3) before the call to <code>rect</code> or the whitespace (on line 1) above the <code>def</code>. Release the Shift key and select <em>Introduce Variables</em> from the pop-up menu. Apply the transformation from the results list.</p><p><strong>Note:</strong> Remember to hold down Shift while selecting multiple arguments.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Introduce <code>x</code> and <code>y</code> variables with each of the two interactions. Use Undo after each to reset the program.</li></ol>

"""

tutorial_step_13 =
 """
(def redSquare
  (let [x y] [100 70]
    (rect \"salmon\" x y 80 80)))

(def main
  (draw [redSquare]))
; <!-- PROSE --> <p>As introduced in the previous step, a <em>target position</em> is the whitespace between or above other items in the code. Before moving on to the next tool, let's get more practice selecting target positions.</p><h3 id=\"exercises\">Exercises</h3><p>In the interactions below, notice how target positions are sometimes zero characters wide and sometimes span multiple lines.</p><ol style=\"list-style-type: decimal\"><li><p>Hold down the Shift key, and select the target position above <code>redSquare</code> either by selecting the whitespace on line 1 or at the very beginning of line 2. Notice how there are no available tools when only a target position is selected.</p></li><li><p>Hold down the Shift key, and select the target position above <code>main</code>. Notice how selecting a second target position deselects the previous one — at most one target position can be selected at a time.</p></li><li><p>Hold down the Shift key, and hover and select each of the following target positions on line 3:</p></li></ol><ul><li>between the beginning of the line and <code>(let</code>, and</li><li>between <code>let</code> and <code>[x</code>,</li><li>between <code>[</code> and <code>x</code>,</li><li>between <code>x</code> and <code>y</code>,</li><li>between <code>y</code> and <code>]</code>,</li><li>between <code>y]</code> and <code>[100</code>,</li><li>between <code>[</code> and <code>100</code>,</li><li>between <code>100</code> and <code>70</code>,</li><li>between <code>70</code> and <code>]</code>, and</li><li>between <code>70]</code> and the end of the line.</li></ul>

"""

tutorial_step_14 =
 """
(def redSquare
  (let [x y] [100 70]
    (rect \"salmon\" x y 120 80)))

(def main
  (draw [redSquare]))
; <!-- PROSE --> <p>In this code snippet, the rectangle should have sides of equal length, but the width and height (fourth and fifth) arguments of the <code>rect</code> function call are not equal. The <em>Make Equal by Copying</em> tool can be used to copy one expression to replace one or more other expressions.</p><p>As with all Sketch-n-Sketch code tools, there are two main ways that we can use the <em>Make Equal by Copying</em> tool:</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select either <code>120</code> or <code>80</code>. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Make Equal by Copying</em>. Click on the remaining number (either <code>120</code> or <code>80</code>). Click on one of the results from the list that appears to apply the transformation.</p><p><strong>Note:</strong> in Text-Select Mode, a SINGLE, primary argument is selected FIRST, then the desired tool is selected, and then the rest of the arguments are selected AFTERWARD (as required by the configuration panel).</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down Shift, then click on <code>120</code> and <code>80</code>. Release the Shift key and select <em>Make Equal by Copying</em> from the pop-up menu. Click on one of the results from the list that appears to apply the transformation.</p><p><strong>Note:</strong> in Box-Select Mode, ALL arguments are selected first, BEFORE the desired code transformation is selected.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Fix the <code>redSquare</code> definition using each of the two interactions. Use Undo after each interaction to reset the program.</li></ol>

"""

tutorial_step_15 =
 """
(def redSquare
  (let [x y] [100 70]
    (rect \"salmon\" x y 120 80)))

(def main
  (draw [redSquare]))
; <!-- PROSE --> <p>Thinking back on the previous example, we successfully turned the rectangle into a square by copying the width and height values. But now if we want to change the size of the square, we must remember to change <em>both</em> values in sync.</p><p>Instead, we may want a single variable to take the place of multiple expressions (in this case, the width and height). The <em>Make Equal with Single Variable</em> tool does just that.</p><p>We can invoke <em>Make Equal with Single Variable</em> in two main ways:</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select either <code>120</code> or <code>80</code>. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Make Equal with Single Variable</em>. Click on the remaining number (either <code>120</code> or <code>80</code>). Click on one of the results from the list that appears to apply the transformation.</p><p><strong>Reminder:</strong> in Text-Select Mode, a SINGLE, primary argument is selected FIRST, then the desired tool is selected, and then the rest of the arguments are selected AFTERWARD (as required by the configuration panel).</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down Shift, then click on <code>120</code> and <code>80</code>. Release the Shift key and select <em>Make Equal with Single Variable</em> from the pop-up menu. Click on one of the results from the list that appears to apply the transformation.</p><p><strong>Reminder:</strong> in Box-Select Mode, ALL arguments are selected first, BEFORE the desired code transformation is selected.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Fix the <code>redSquare</code> definition using each of the two interactions. Use Undo after each interaction to reset the program.</li></ol>

"""

tutorial_step_16 =
 """
(def redSquare
  (let [x y] [100 70]
  (let w 80
    (rect \"salmon\" x y w w))))

(def main
  (draw [redSquare]))
; <!-- PROSE --> <p>Next, we will use the <em>Create Function from Definition</em> tool to turn this definition into a function so that it can be called to create more red squares easily.</p><p>We can invoke <em>Create Function from Definition</em> in two main ways:</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select the entire <code>(def redSquare ...)</code> expression starting from its opening parenthesis until its closing parenthesis. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Create Function from Definition</em>.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down the Shift key, hover over the keyword <code>def</code> in <code>(def redSquare   ...)</code>, then click. Release the Shift key. Select <em>Create Function from Definition</em> from the pop-up menu.</p></li></ul><p>No matter how the <em>Create Function from Definition</em> tool is invoked, there are two options in the results list that we can select from. Because the point of this function is to draw a <em>red</em> square, we'll pick the option that does not make color one of the resulting function arguments.</p><p>Notice how the reference to this expression in <code>main</code> has now turned into a function call.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Perform the transformation with each of the two interactions. Use Undo after each to reset the program.</li></ol>

"""

tutorial_step_17 =
 """
(def redSquare (\\(x y w)
  (rect \"salmon\" x y w w)))

(def main
  (draw
    [(redSquare 100 70 80)
     (redSquare 100 160 30)
     (redSquare 100 200 150)]))
; <!-- PROSE --> <p>Let's say we've now coded up a nice piece of art using our newly-defined function. However, we now want to change how the <code>redSquare</code> function is called; instead of the size of the square being the last argument that we pass in, we'd prefer if it were the first.</p><p>To do this, we can use the <em>Reorder Arguments</em> tool.</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select the argument <code>w</code> in the list of arguments after the backslash in the <code>redSquare</code> function definition. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Reorder Arguments</em>. Click on the target position that lies before the first argument (<code>x</code>) and after the opening parenthesis of the argument list. Click on the single option from the results list.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down the Shift key and click on the argument <code>w</code> in the list of arguments after the backslash in the <code>redSquare</code> function definition. Then, click on the target position that lies before the first argument (<code>x</code>) and after the opening parenthesis of the argument list. Release the Shift key. Under the <em>Reorder Arguments</em> tool in the pop-up menu, select the single option from the results list.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Perform the transformation with each of the two interactions. Use Undo after each to reset the program.</li></ol>

"""

tutorial_step_18 =
 """
(def redSquare (\\(w x y)
  (rect \"salmon\" x y w w)))

(def main
  (draw
    [(redSquare 80 100 70)
     (redSquare 30 100 160)
     (redSquare 150 100 200)]))
; <!-- PROSE --> <p>While refactoring the function in the previous step, we notice something else we'd like to change; every single time our function is called, we always set the x-position to <code>100</code>, so we decide to just remove the x-position argument.</p><p>To do so, we can use the <em>Remove Argument</em> tool.</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select the argument <code>x</code> in the list of arguments after the backslash in the <code>redSquare</code> function definition. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Remove Argument</em>. Click on the single option from the results list.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down the Shift key and click on the argument <code>x</code> in the list of arguments after the backslash in the <code>redSquare</code> function definition. Release the Shift key. Under the <em>Remove Argument</em> tool in the pop-up menu, select the single option form the results listn.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Perform the transformation with each of the two interactions. Use Undo after each to reset the program.</li></ol>

"""

tutorial_step_19 =
 """
(def redSquare (\\(w y)
  (rect \"salmon\" 100 y w w)))

(def main
  (draw
    [(redSquare 80 70)
     (redSquare 30 160)
     (redSquare 150 200)]))
; <!-- PROSE --> <p>On second thought... we realize we'd like to change the x-position of some of the boxes after all.</p><p>To do so, we can use the <em>Add Argument</em> tool.</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select the number <code>100</code> in the <code>rect</code> function call. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Add Argument</em>. Click on the single option from the results list.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down the Shift key and click on the number <code>100</code> in the <code>rect</code> function call. Under the <em>Add Argument</em> tool in the pop-up menu, select the single option from the results list.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Perform the transformation with each of the two interactions. Use Undo after each to reset the program. Feel free to manually edit the x-position of the squares after you have performed the transformation!</li></ol>

"""

tutorial_step_20 =
 """
(def redSquare (\\(w x y)
  (rect \"salmon\" x y w w)))

(def greenSquare (\\(w x y)
  (rect \"yellowgreen\" x y w w)))

(def yellowSquare (\\(w x y)
  (rect \"gold\" x y w w)))

(def main
  (draw
    [(redSquare 50 80 50)
     (yellowSquare 70 70 110)
     (greenSquare 90 60 190)]))
; <!-- PROSE --> <p>Here are some friends for our <code>redSquare</code> function. However, a lot of the code from <code>redSquare</code> has been duplicated in the two new functions; in fact, the only thing that has changed is the color.</p><p>Sketch-n-Sketch provides a <em>Create Function by Merging Definitions</em> code tool that we can use in this situation to abstract much of the repeated code into a helper function.</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select the entire <code>(def redSquare ...)</code> expression starting from its opening parenthesis until its closing parenthesis. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Create Function by Merging Definitions</em>. Hover over the keyword <code>def</code> in <code>(def greenSquare ...)</code>, then click. Hover over the keyword <code>def</code> in <code>(def yellowSquare ...)</code>, then click. Click on the single option from the results list.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down the Shift key and hover over the keyword <code>def</code> in <code>(def   redSquare ...)</code>, then click. Hover over the keyword <code>def</code> in <code>(def   greenSquare ...)</code>, then click. Hover over the keyword <code>def</code> in <code>(def   yellowSquare ...)</code>, then click. Release the Shift key. Under the <em>Merge Expressions into Function</em> tool in the pop-up menu, select the single option from the results list.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Perform the transformation with each of the two interactions. Use Undo after each to reset the program.</li></ol>

"""

tutorial_step_21 =
 """
(def square (\\fill (\\(w x y)
  (rect fill x y w w))))

(def redSquare (square \"salmon\"))

(def greenSquare (square \"yellowgreen\"))

(def yellowSquare (square \"gold\"))

(def main
  (draw
    [(redSquare 50 80 50)
     (yellowSquare 70 70 110)
     (greenSquare 90 60 190)]))
; <!-- PROSE --> <p>A couple more things before we're done! We'd like to reorder the function definitions to match the order in which we use them, just for the sake of consistency (red, yellow, then green).</p><p>To do so, we can use the <em>Move Definition</em> tool.</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select the entire <code>(def greenSquare ...)</code> expression starting from its opening parenthesis until its closing parenthesis. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Move Definition</em>. Click on the whitespace below the <code>yellowSquare</code> definition (above the <code>main</code> definition). Click on the single option from the results list.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down the Shift key and hover over the keyword <code>def</code> in <code>(def   greenSquare ...)</code>, then click. Then, click on the whitespace below the <code>yellowSquare</code> definition (above the <code>main</code> definition). Release the Shift key. Under the Move Definition tool in the pop-up menu, select the single option from the results list.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Perform the transformation with each of the two interactions. Use Undo after each to reset the program.</li></ol>

"""

tutorial_step_22 =
 """
(def square (\\fill (\\(w x y)
  (rect fill x y w w))))

(def redSquare (square \"salmon\"))

(def yellowSquare (square \"gold\"))

(def greenSquare (square \"yellowgreen\"))

(def main
  (draw
    [(redSquare 50 80 50)
     (yellowSquare 70 70 110)
     (greenSquare 90 60 190)]))
; <!-- PROSE --> <p>Lastly, because the three definitions <code>redSquare</code>, <code>yellowSquare</code>, and <code>greenSquare</code> are short, we'd like to group them into a single tuple definition (on a single line) to save some space.</p><p>We can use the <em>Move Definition</em> tool again, this time to group definitions into a tuple. To do so, we will select only variable names (rather than entire <code>(def ...)</code> expressions) and whitespace next to variables (rather than above or below <code>(def ...)</code> expressions).</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p>Text-select <code>redSquare</code> in the <code>(def redSquare ...)</code> expression. Then, either right-click the editor or click on the Code Tools menu at the top of the screen. From either of these menus, select <em>Move Definition</em>. Click on <code>yellowSquare</code> in the <code>(def yellowSquare ...)</code> definition. Then, click the whitespace between the keyword <code>def</code> and the variable <code>greenSquare</code>. Click on the single option from the results list.</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p>Hold down the Shift key and hover over <code>redSquare</code> in <code>(def redSquare   ...)</code>, then click. Next, hover over <code>yellowSquare</code> in <code>(def yellowSquare ...)</code>, then click. Then, click on the whitespace between the keyword <code>def</code> and the variable <code>greenSquare</code>. Under the <em>Move Definition</em> tool in the pop-up menu, select the single option from the results list.</p></li></ul><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Perform the transformation with each of the two interactions. Use Undo after each to reset the program.</li></ol>

"""

tutorial_step_23 =
 """
(def main
  (draw []))
; <!-- PROSE --> <p>As a final recap, there are two general mechanisms for invoking code transformation tools:</p><ul class=\"_12\"><li class=\"_1\"><h3>Text-Select Mode</h3><p><strong>(Step 1)</strong> Text-select something in the code.</p><p><strong>(Step 2a)</strong> Right-click and select a tool from pop-up menu, or</p><p><strong>(Step 2b)</strong> Select a tool from the Code Tools menu.</p><p><strong>(Step 3)</strong> Follow any instructions and finish.</p><p>In Text-Select Mode, a SINGLE, primary argument is selected FIRST, then the desired code tool is selected, and then the rest of the arguments are selected AFTERWARD (as required by the configuration panel).</p></li><li class=\"_2\"><h3>Box-Select Mode</h3><p><strong>(Step 1)</strong> Hold down Shift, hover and click boxes for all items involved for the desired code tool (then release Shift key).</p><p><strong>(Step 2)</strong> Select a tool from pop-up menu.</p><p><strong>(Step 3)</strong> Follow any instructions and finish.</p><p>In Box-Select Mode, ALL arguments are selected first, BEFORE the desired code tool is selected.</p></li></ul><p>Note that regardless of the method of selection, the Escape key will always deselect all selected boxes and cancel any pop-up menus.</p><h3 id=\"exercises\">Exercises</h3><ol style=\"list-style-type: decimal\"><li>Try the Help menu at the top of the screen; you'll find these summary instructions there.</li></ol>

"""


task_one_rectangle =
 """
(def rect1
  (let x 20
  (let y 20
  (let height 250
  (let width 80
  (let fill \"black\"
    (rect fill x y height width)))))))

(def main
  (draw [ rect1 ]))


; The final program should look something like:
;
;   (def rect1
;     (let [fill x y width height] [\"black\" 20 20 80 250]
;       (rect fill x y width height)))
;
;   (def main
;     (draw [ rect1 ]))
; <!-- PROSE --> <p>PLACEHOLDER INSTRUCTIONS</p><p>Goals:</p><ul><li><p>The programmer intended the rectangle to be <code>250</code> pixels tall and <code>80</code> pixels wide, but the <code>height</code> and <code>width</code> arguments to <code>rect</code> are in the wrong order. Swap them.</p></li><li><p>Rearrange the five variable definitions into a single tuple definition. The order of variables should match the order of arguments to <code>rect</code>.</p></li></ul>

"""

task_two_circles =
 """
(def connectedCircles
  (let startX 50
  (let endY 50
  (let startY 70
  (let endX 150
  [(circle \"gray\" startX startY 30)
   (circle \"gray\" endX endY 30)
   (line \"gray\" 10 startX startY endX endY)
  ])))))

(def main
  (draw connectedCircles))


; The final program should look something like:
;
;   (def connectedCircles (\\(startX startY endX endY)
;     [(circle \"gray\" startX startY 30)
;      (circle \"gray\" endX endY 30)
;      (line \"gray\" 10 startX startY endX endY)
;     ]))
;
;   (def main
;     (draw (connectedCircles 50 70 150 50)))
; <!-- PROSE --> <p>PLACEHOLDER INSTRUCTIONS</p><p>Goals:</p><ul><li>Turn <code>connectedCirlces</code> into a function takes <code>startX</code>, <code>startY</code>, <code>endX</code>, and <code>endY</code> arguments, and draws two gray circles at <code>(startX, startY)</code> and <code>(endX, endY)</code> connected by a line.</li></ul>

"""

task_three_rectangles =
 """
(def rect1
  (let [fill x y w h] [\"red\" 30 30 50 70]
    (rect fill x y w h)))

(def rect2
  (let [fill x y w h] [\"green\" 109 53 50 70]
    (rect fill x y w h)))

(def rect3
  (let [fill x y w h] [\"blue\" 192 35 50 70]
    (rect fill x y w h)))

(def main
  (draw [ rect1 rect2 rect3 ]))


; The final program should look something like:
;
;   (def rect_50_70 (\\(fill x y)
;     (let [w h] [50 70]
;       (rect fill x y w h))))
;
;   (def rect1
;     (rect_50_70 \"red\" 30 30))
;
;   (def rect2
;     (rect_50_70 \"green\" 109 53))
;
;   (def rect3
;     (rect_50_70 \"blue\" 192 35))
;
;   (def main
;     (draw [ rect1 rect2 rect3 ]))
; <!-- PROSE --> <p>PLACEHOLDER INSTRUCTIONS</p><p>Goals:</p><ul><li>The three rectangle definitions share a lot of identical code. Create a function <code>rect_50_70</code> that generates a <code>50</code> x <code>70</code> rectangle given color and position arguments, and define <code>rect1</code>, <code>rect2</code>, and <code>rect3</code> in terms of <code>rect_50_70</code>.</li></ul>

"""

task_target =
 """
(def ring (\\i
  (let fill (if (= 0 (mod i 2)) \"firebrick\" \"lightgray\")
  (circle fill 150 150 (* 30 i)))))

(def target (\\(startIndex endIndex)
  (map ring (reverse (range startIndex endIndex)))))

(def main
  (draw (target 1 4)))


; The final program should look something like:
;
;   (def target (\\(numRings cx cy num)
;     (let ring (\\i
;       (let fill (if (= 0 (mod i 2)) \"firebrick\" \"lightgray\")
;       (circle fill cx cy (* num i))))
;       (map ring (reverse (range 1 numRings))))))
;
;   (def main
;     (draw (target 4 150 150 30)))
; <!-- PROSE --> <p>PLACEHOLDER INSTRUCTIONS</p><p>Goals:</p><ul><li><p>Remove the <code>startIndex</code> argument; its value should always be <code>1</code>.</p></li><li><p>Rename <code>endIndex</code> to <code>numRings</code>.</p></li><li><p>Move the <code>ring</code> function inside the <code>target</code> definition.</p></li><li><p>Add the center position and ring width as arguments to <code>target</code>.</p></li></ul>

"""

task_four_squares =
 """
(def fourSquares
  (let [x y w] [80 30 100]

    [(rect \"yellowgreen\" (+ x (* 0 w)) (+ y (* 0 w)) w w)
     (rect \"gold\"        (+ x (* 1 w)) (+ y (* 0 w)) w w)
     (rect \"royalblue\"   (+ x (* 0 w)) (+ y (* 1 w)) w w)
     (rect \"salmon\"      (+ x (* 1 w)) (+ y (* 1 w)) w w)
    ]))

(def main
  (draw fourSquares))


; The final program should look something like:
;
;   (def fourSquares (\\(x y w topLeft topRight botLeft botRight)
;
;     (let oneCorner (\\(fill num num2)
;       (rect fill (+ x (* num w)) (+ y (* num2 w)) w w))
;
;       [(oneCorner topLeft 0 0)
;        (oneCorner topRight 1 0)
;        (oneCorner botLeft 0 1)
;        (oneCorner botRight 1 1)
;       ])))
;
;   (def main
;     (draw (fourSquares 80 30 100 \"yellowgreen\" \"gold\" \"royalblue\" \"salmon\")))
; <!-- PROSE --> <p>PLACEHOLDER INSTRUCTIONS</p><p>Goals:</p><ul><li><p>Introduce a helper function called <code>oneCorner</code> that factors the code that is common to the four calls to <code>rect</code>.</p></li><li><p>Turn <code>fourSquares</code> into a function takes <code>x</code>, <code>y</code>, and <code>w</code> arguments, as well as color arguments called <code>topLeft</code>, <code>topRight</code>, <code>botLeft</code>, and <code>botRight</code>.</p></li></ul>

"""

task_lambda =
 """
(def rectangle
  (rect \"black\" 20 30 100 120))
(def line1
  (line \"white\" 5 20 30 (+ 20 100) (+ 30 120)))
(def line2
  (line \"white\" 5 20 (+ 30 120) (+ 20 (/ 100 2)) (+ 30 (/ 120 2))))

(def logo
  [rectangle line1 line2])

(def main
  (draw logo))


; The final program should look something like:
;
;   (def [x y w h] [20 30 100 120])
;   (def [fill stroke strokeWidth] [\"black\" \"white\" 5])
;
;   (def rectangle
;     (rect fill x y w h))
;   (def line1
;     (line stroke strokeWidth x y (+ x w) (+ y h)))
;   (def line2
;     (line stroke strokeWidth x (+ y h) (+ x (/ w 2)) (+ y (/ h 2))))
;
;   (def logo
;     [rectangle line1 line2])
;
;   (def main
;     (draw logo))
; <!-- PROSE --> <p>The initial program draws a <code>100</code> x <code>120</code> pixel lambda icon at xy-position <code>(20, 30)</code>, but the use of duplicated constants requires many changes if we want to draw the icon at a different position or change the style of the lines.</p><p>PLACEHOLDER INSTRUCTIONS</p><p>Goals:</p><ul><li><p>Define and use four new variables called <code>x</code>, <code>y</code>, <code>w</code>, and <code>h</code> for the x-position, y-position, width, and height, respectively, of the logo. These variables should be defined in a single 4-tuple.</p></li><li><p>Define and use two new variables called <code>stroke</code> and <code>strokeWidth</code> for the color and width, respectively, of the lines. These variables should be defined in a single 2-tuple.</p></li><li><p>Define and use a new variable called <code>fill</code> for the color of the rectangle. This variable should be defined in the same tuple as <code>stroke</code> and <code>strokeWidth</code>.</p></li></ul>

"""


--------------------------------------------------------------------------------

simpleSheet =
 """(simpleTable
 [
  [1 2]
  [3 4]
  [3 4 5]
 ])
"""

styledSheet =
 """(Table
 [
  [(styledEntry 1 [['color' 'red']]) 
   (styledEntry 2 [['color' 'blue']])
  ]
  [(styledEntry 3 [['color' 'green']])
   (styledEntry 4 [['color' 'yellow']])
  ]
 ])
"""


generalCategory =
  ( "General"
  , [ makeExample "BLANK" blank
    , makeExample "*Prelude*" Prelude.src
    ]
  )

defaultIconCategory =
  ( "Default Icons"
  , [ makeExample "Icon: Cursor" DefaultIconTheme.cursor
    , makeExample "Icon: Point Or Offset" DefaultIconTheme.pointOrOffset
    , makeExample "Icon: Text" DefaultIconTheme.text
    , makeExample "Icon: Line" DefaultIconTheme.line
    , makeExample "Icon: Rect" DefaultIconTheme.rect
    , makeExample "Icon: Ellipse" DefaultIconTheme.ellipse
    , makeExample "Icon: Polygon" DefaultIconTheme.polygon
    , makeExample "Icon: Path" DefaultIconTheme.path
    ]
  )

deuceCategory =
  ( "Deuce Examples"
  , [ makeExample "Sketch-n-Sketch Logo" sns_deuce
    , makeExample "Target" target_deuce
    , makeExample "Battery" battery_deuce
    , makeExample "Coffee Mug" coffee_deuce
    , makeExample "Mondrian Arch" mondrian_arch_deuce
    ]
  )

logoCategory =
  ( "Logos"
  , List.sortBy Tuple.first
      [ makeExample "SnS Logo (UIST)" sns_UIST
      , makeExample "SnS Logo Revisited (UIST)" sns_revisited_UIST
      , makeExample "Botanic Garden Logo (UIST)" botanic_UIST
      , makeExample "Logo" logo
      , makeExample "Botanic Garden Logo" botanic
      , makeExample "Active Trans Logo" activeTrans2
      , makeExample "SnS Logo Wheel" snsLogoWheel
      , makeExample "Haskell.org Logo" haskell
      , makeExample "Cover Logo" cover
      , makeExample "POP-PL Logo" poppl
      , makeExample "Floral Logo 1" floralLogo
      , makeExample "Floral Logo 2" floralLogo2
      , makeExample "Elm Logo" elmLogo
      , makeExample "Logo 2" logo2
      -- , makeExample "Logo Sizes" logoSizes
      ]
  )

flagCategory =
  ( "Flags"
  , List.sortBy Tuple.first
      [ makeExample "Chicago Flag" chicago
      , makeExample "US-13 Flag" usFlag13
      , makeExample "US-50 Flag" usFlag50
      , makeExample "French Sudan Flag" frenchSudan
      ]
  )


otherCategory =
  ( "Other"
  , [ makeExample "Coffee Mugs (UIST)" coffee_UIST

    , makeExample "Wave Boxes" sineWaveOfBoxes
    , makeExample "Wave Boxes Grid" sineWaveGrid

    , makeExample "Basic Slides" basicSlides
    , makeExample "Sailboat" sailBoat
    , makeExample "Sliders" sliders
    , makeExample "Buttons" buttons
    , makeExample "Widgets" widgets
    , makeExample "xySlider" xySlider
    , makeExample "Offsets" offsets
    , makeExample "Tile Pattern" boxGridTokenFilter
    , makeExample "Color Picker" rgba
    , makeExample "Ferris Wheel" ferris
    , makeExample "Ferris Task Before" ferris2
    , makeExample "Ferris Task After" ferris2target
    , makeExample "Ferris Wheel Slideshow" ferrisWheelSlideshow
    , makeExample "Survey Results" surveyResultsTriHist2
    , makeExample "Hilbert Curve Animation" hilbertCurveAnimation
    , makeExample "Bar Graph" barGraph
    , makeExample "Pie Chart" pieChart1
    , makeExample "Solar System" solarSystem
    , makeExample "Clique" clique
    , makeExample "Eye Icon" eyeIcon
    , makeExample "Horror Films" horrorFilms0
    , makeExample "Cycling Association" cyclingAssociation0
    , makeExample "Lillicon P" lilliconP
    , makeExample "Lillicon P, v2" lilliconP2
    , makeExample "Keyboard" keyboard
    , makeExample "Keyboard Task Before" keyboard2
    , makeExample "Keyboard Task After" keyboard2target
    , makeExample "Tessellation Task Before" tessellation
    , makeExample "Tessellation Task After" tessellationTarget
    , makeExample "Tessellation 2" tessellation2
    , makeExample "Spiral Spiral-Graph" spiralSpiralGraph
    , makeExample "Rounded Rect" roundedRect

    , makeExample "Thaw/Freeze" thawFreeze
    , makeExample "Dictionaries" dictionaries
    , makeExample "3 Boxes" threeBoxes

    , makeExample "N Boxes Sli" nBoxes
    , makeExample "N Boxes" groupOfBoxes

    , makeExample "Rings" rings
    , makeExample "Polygons" polygons
    , makeExample "Stars" stars
    , makeExample "Triangles" equiTri
    , makeExample "Frank Lloyd Wright" flw1
    , makeExample "Frank Lloyd Wright B" flw2
    , makeExample "Bezier Curves" bezier
    , makeExample "Fractal Tree" fractalTree
    , makeExample "Stick Figures" stickFigures
    -- , makeExample "Cult of Lambda" cultOfLambda
    , makeExample "Matrix Transformations" matrices
    , makeExample "Misc Shapes" miscShapes
    , makeExample "Interface Buttons" interfaceButtons
    , makeExample "Paths 1" paths1
    , makeExample "Paths 2" paths2
    , makeExample "Paths 3" paths3
    , makeExample "Paths 4" paths4
    , makeExample "Paths 5" paths5
    , makeExample "Sample Rotations" rotTest
    , makeExample "Grid Tile" gridTile
    , makeExample "Zones" zones
    , makeExample "Rectangle Trisection" rectangleTrisection
    , makeExample "Battery" battery
    , makeExample "Battery (Dynamic)" batteryDynamic
    , makeExample "Mondrian Arch" mondrianArch
    , makeExample "Ladder" ladder
    , makeExample "Rails" rails
    , makeExample "Target" target
    , makeExample "Xs" xs
    , makeExample "Conifer" conifer
    , makeExample "Ferris Wheel 3" ferris3
    , makeExample "Gear" gear
    , makeExample "Koch Snowflake" kochSnowflake
    , makeExample "Replace Terminals With Workstations" replaceTerminalsWithWorkstations
    , makeExample "Balance Scale" balanceScale
    , makeExample "Pencil Tip" pencilTip
    , makeExample "Calendar Icon" calendarIcon
    ]
  )

deuceUserStudyCategory =
  ( "Deuce User Study"
  , [ makeExample "Deuce Study Start" study_start
    , makeExample "Step 01" tutorial_step_01
    , makeExample "Step 02" tutorial_step_02
    , makeExample "Step 03" tutorial_step_03
    , makeExample "Step 04" tutorial_step_04
    , makeExample "Step 05" tutorial_step_05
    , makeExample "Step 06" tutorial_step_06
    , makeExample "Step 07" tutorial_step_07
    , makeExample "Step 08" tutorial_step_08
    , makeExample "Step 09" tutorial_step_09
    , makeExample "Step 10" tutorial_step_10
    , makeExample "Step 11" tutorial_step_11
    , makeExample "Step 12" tutorial_step_12
    , makeExample "Step 13" tutorial_step_13
    , makeExample "Step 14" tutorial_step_14
    , makeExample "Step 15" tutorial_step_15
    , makeExample "Step 16" tutorial_step_16
    , makeExample "Step 17" tutorial_step_17
    , makeExample "Step 18" tutorial_step_18
    , makeExample "Step 19" tutorial_step_19
    , makeExample "Step 20" tutorial_step_20
    , makeExample "Step 21" tutorial_step_21
    , makeExample "Step 22" tutorial_step_22
    , makeExample "Step 23" tutorial_step_23
    , makeExample "Deuce Study Transition 1" study_transition_1
    , makeExample "One Rectangle" task_one_rectangle
    , makeExample "Two Circles" task_two_circles
    , makeExample "Three Rectangles" task_three_rectangles
    , makeExample "Target Icon" task_target
    , makeExample "Deuce Study Transition 2" study_transition_2
    , makeExample "Four Squares" task_four_squares
    , makeExample "Lambda Icon" task_lambda
    , makeExample "Deuce Study End" study_end
    ]
  )

sheetCategory =
  ("Sheets", [ makeExample "simple sheet" simpleSheet
             , makeExample "styled sheet" styledSheet
             ]
  )
  
templateCategories =
  [ generalCategory
  , deuceCategory
  , defaultIconCategory
  , logoCategory
  , flagCategory
  , otherCategory
  , deuceUserStudyCategory
  , sheetCategory
  ]

list =
  templateCategories
    |> List.map Tuple.second
    |> List.concat
