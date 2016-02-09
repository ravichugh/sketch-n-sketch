module ExamplesGenerated (list, scratchName, scratch) where

import Lang
import LangParser2 as Parser
import Eval
import Utils
import PreludeGenerated as Prelude

makeExample name s =
  let thunk () =
    let e = Utils.fromOk_ (Parser.parseE s) in
    let (v,ws) = Eval.run e in
    {e=e, v=v, ws=ws}
  in
  (name, s, thunk)

scratchName = "*Scratch*"

scratch =
 " 
; Write a little program below.
; Or choose an example from the list.
;
; Changes to this *Scratch* example will be saved and
; restored when navigating to and from other examples.
; For the remaining named examples, changes will be
; discarded when choosing a different example.
 
(svg [(rect 'maroon' 100 15 200 50)])

"

threeBoxes =
 "
(def threeBoxesInt
  (let [x0 y0 w h sep] [40 28 60 130 110]
  (let boxi (\\i
    (let xi (+ x0 (mult i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi [0 1 2])))))
 
threeBoxesInt

"

nBoxesH2 =
 "
(def [a b c] [0 0 0])
(def something (+ a (+ a (+ b (+ b (+ c c))))))

(def nBoxes
  (let [n x0 y0 w h sep] [3 40 28 60 130 110]
  (let boxi (\\i
    (let xi (+ (+ x0 something) (mult i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi (zeroTo n))))))
 
nBoxes

"

sineWaveOfBoxes =
 "(def [x0 y0 w h sep amp] [50 120 20 90 30 60])
(def n 12!{3-30})
(def boxi (\\i
   (let xi (+ x0 (* i sep))
   (let yi (- y0 (* amp (sin (* i (/ twoPi n)))))
     (rect 'lightblue' xi yi w h)))))

(svg (map boxi (zeroTo n)))

"

-- LITTLE_TO_ELM waveOfBoxes
-- LITTLE_TO_ELM waveOfBoxesTokens
-- LITTLE_TO_ELM waveOfBoxes3
nBoxes =
 "
(def nBoxes
  (let [n x0 y0 w h sep] [3{1-10} 40 28 60 130 110{50-200}]
  (let boxi (\\i
    (let xi (+ x0 (mult i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi (range 0! (- n 1!)))))))
 
nBoxes

"

groupOfBoxes =
 "
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

"

-- LITTLE_TO_ELM sixBoxesA
-- LITTLE_TO_ELM sixBoxesB
basicSlides =
 "(def slideCount 5)
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
"

logo =
 "; sketch-n-sketch logo
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

"

logo2 =
 "; sketch-n-sketch logo, v2
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

"

logoSizes =
 "
(def logo (\\(x0 y0 w h delta1 delta2 fg bg)
  (let [xw yh w2 h2] [(+ x0 w) (+ y0 h) (div w 2) (div h 2)]
  (let poly (\\(c pts) (polygon c 'none' 0 pts))
  (let basic (\\shape (addAttr shape ['ZONES' 'basic']))
  [
  
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
  
  ])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def [w h]      [300 300])
(def [fg bg]    [360 499])
(def [p0 p1 p2] [1.0! 0.3! 0.1!])
(def offset     20!)

(def x0 offset)
(def y0 offset)
(def x1 (+ x0 (+ offset (* p0 w))))
(def x2 (+ x1 (+ offset (* p1 w))))

(def [large medium small] [
  (logo x0 y0 (* p0 w) (* p0 h) 10 10 fg bg)
  (logo x1 y0 (* p1 w) (* p1 h)  3  3 fg bg)
  (logo x2 y0 (* p2 w) (* p2 h)  1  1 fg bg)
])

(svg (concat [
  large
  (zones 'basic' medium)
  (zones 'basic' small)
]))

"

elmLogo =
 "; Elm logo, based on:
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

"

-- LITTLE_TO_ELM activeTrans
activeTrans2 =
 "
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

"

botanic =
 "
; Logo: Chicago Botanic Garden
 
; Click '[Zones]' to see the control points for
; the various Bezier curves.
  
(def [xOff yOff w h]
 [0! 0! 623 622])

(def [xOut xcOut1 ycOut1 xcOut2 ycOut2 xcOut3 ycOut3]
 [292 40 141 97 202 23 24])

(def [xMid yTip yMid xBud yBud]
 [320! 272 460 -51 272])

(def left [[xMid yMid] [(- xMid xOut) yTip]])
(def right [[xMid yMid] [(+ xMid xOut) yTip]])
(def bud [[xMid (- yMid 92)] [(+ xMid xBud) yBud] [(- xMid xBud) yBud]])
 
(def makePath
  (\\(c pts [xc1 yc1] [xc2 yc2])
    (let offsetPts (map (\\[x y] [(+ x xOff) (+ y yOff)]) pts)
    (let [[x0 y0] [x1 y1]] offsetPts
    (let commands ['M' x0 y0 'Q' xc1 yc1 x1 y1 'M' x1 y1 'Q' xc2 yc2 x0 y0]
      (path c 'black' 0 commands))))))
 
(def makeArc
  (\\(c pts [xc1 yc1] [xc2 yc2])
    (let offsetPts (map (\\[x y] [(+ x xOff) (+ y yOff)]) pts)
    (let [[x0 y0] [x1 y1] [x2 y2]] offsetPts
    (let commands ['M' x0 y0 'L' x1 y1 'A' 45 45 0 0 1 x2 y2 'L' x2 y2 'Z']
      (path c 'black' 0 commands))))))
 
(def [leftleaf rightleaf centerbud] [
  (makePath 'white' left [(- xMid xcOut1) ycOut1] [(- xMid xcOut2) ycOut2])
  (makePath 'white' right [(+ xMid xcOut1) ycOut1] [(+ xMid xcOut2) ycOut2])
  (makeArc 'white' bud [(+ xMid xcOut3) ycOut3] [(+ xMid xcOut3) ycOut3])
])

(def background (zones 'none' [(rect '#83F52C' xOff yOff w h)]))

(svg (concat [background [leftleaf rightleaf centerbud]]))

"

rings =
 "
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

"

polygons =
 "(let ngon (\\(n cx cy len1 len2)
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

"

stars =
 " 
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

"

sliders =
 ";
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

"

buttons =
 ";
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

"

widgets =
 "; library widgets
;
(let [n  s1] (hSlider false 20! 90!  20! 0! 5! 'n = ' 3.1415)
(let [i  s2] (hSlider true  20! 90!  50! 0! 5! 'i = ' 3.1415)
(let [b1 s3] (button        20!      80!       'b1 = ' 0.25)
(let [b2 s4] (button        20!     110!       'b2 = ' 0.75)
  (svg (concat [s1 s2 s3 s4]))))))

"

xySlider =
 "; A two dimensional slider in a similar style to the other sliders
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

"

rgba =
 ";
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

"

-- LITTLE_TO_ELM boxGrid
boxGridTokenFilter =
 "
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

"

usFlag13 =
 ";
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

"

usFlag50 =
 ";
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

"

chicago =
 " 
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

"

-- LITTLE_TO_ELM chicagoColors
frenchSudan =
 ";
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

"

flw1 =
 ";
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

"

flw2 =
 ";
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

"

ferris =
 ";
; Take this ferris wheel for a spin!
;
; Try:
;  - Stretching the passenger cars
;  - Stretching the central hub
;  - Dragging the central hub
;  - Setting showSliders to false
;
(let [numSpokes_ spokeLen_ rotAngle_] [5 80 0]
(let showSliders true
;
(let [numSpokes s1] (hSlider true 20! 420! 20! 3! 15! '' numSpokes_)
(let [spokeLen s2] (hSlider true 20! 420! 50! 40! 200! '' spokeLen_)
(let [rotAngle s3] (hSlider false 20! 420! 80! (neg twoPi) twoPi '' rotAngle_)
;
(let sliders (if showSliders (concat [s1 s2 s3]) [])
(let wheel
  (let [cx cy] [220 300]
  (let rim [(ring 'darkgray' 8! cx cy spokeLen)]
  (let center [(circle 'black' cx cy 20)]
  (let frame [(nStar 'goldenrod' 'darkgray' 3! numSpokes spokeLen 0! rotAngle cx cy)]
  (let spokePts (nPointsOnCircle numSpokes rotAngle cx cy spokeLen)
  (let caps (map (\\[x y] (circle 'black' x y 7!)) spokePts)
  (let cars
    (let wCar 30
    (let wHalfCar (/ wCar 2!)
    (map (\\[x y] (squareCenter 'lightgray' x y wCar)) spokePts)))
  (concat [rim cars center frame caps]))))))))
;
(svg (append sliders wheel)))))))))

"

ferris2 =
 "(def n 5)
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
    (map (\\[x y] (squareCenter 'lightgray' x y wCar)) spokePts)))
  (concat [rim cars center frame caps])))))))))

(svg wheel)

"

ferris2target =
 "(def n 8)
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
    (mapi (\\[i [x y]] (squareCenter (if (= 0 i) 'pink' 'lightgray') x y wCar)) spokePts)))
  (concat [rim cars center frame caps])))))))))

(svg wheel)
"

ferrisWheelSlideshow =
 "(def [slideN slideSlider] (hSlider true 20! 400! 20! 1! 13! 'Slide ' 1))
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
"

pieChart1 =
 "; A Pie Chart
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
"

solarSystem =
 "; Visualization of the solar system 
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

"

fractalTree =
 "; A fractal tree
;
(defrec mod (\\(x m) (if (< x m) x (mod (- x m) m))))
(def nsin (\\n (if (< n (/ 3.14159 2)) (sin n) (cos (mod n (/ 3.14159 2))))))
(def ncos (\\n (if (< n (/ 3.14159 2)) (cos n) (sin (mod n (/ 3.14159 2))))))
(def [initwd initlen] [10! 150!])
(def [steps stepslider] (hSlider true 20! 420! 550! 3! 8! 'Steps ' 4))
(def [bendn bendnslider] (hSlider false 20! 420! 580! 1! 8! 'Bend ' 1))
(def initangle (/ 3.14159! 2!))
(def bend (/ 3.14159! bendn))
(defrec exp (\\(base pow)
  (if (< pow 1) 1 (* base (exp base (- pow 1))))))
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

"

hilbertCurveAnimation =
 "; How to draw a Hilbert curve.
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
"

stickFigures =
 ";
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

"

cultOfLambda =
 ";
; Cult of Lambda
;
; Some fun 
;
(let [x0 y0 w h] [0 0 500 500]
(let wstripe (/ w 6!)
(let xoff (+ x0 wstripe)
(let yoff (+ y0 (/ h 4!))
(let minrad
  (if (< (/ wstripe 7.5!) (/ h 30!))
    (/ wstripe 7.5!)
    (/ h 15!))
(let min
  (if (< w h)
    (* 0.4! w)
    (* 0.4! h))
(let rotate (\\a (/ (* (+ 1! a) (pi)) 4!))
(let figure 
  (\\(x y) 
    (let [x1 x2 x3] (map (\\n (+ x (* wstripe n))) [1.2! 1.5! 1.8!])
    (let [y1 y2 y3 y4] (map (\\n (+ y (/ (/ h 2!) n))) [4.3! 2.8! 1.9! 1.4!])
    (let figline (\\[[a b] [c d]] (line 'orange' (/ minrad 2!) a b c d))
      (snoc
        (ellipse 'orange' x2 y1 (/ wstripe 7.5!) (/ h 30!))
        (map
          figline
            [[[x1 y1] [x1 y2]]
            [[x1 y2] [x3 y2]]
            [[x3 y1] [x3 y2]]
            [[x1 y4] [x1 y3]]
            [[x1 y3] [x3 y3]]
            [[x3 y3] [x3 y4]]
            [[x2 y1] [x2 y3]]]))))))
(let logo
  (\\(x y)
    (let [xl yl wl hl delta] [x y w h 27.5]
    (let [xw yh w2 h2] [(+ xl wl) (+ yl hl) (div wl 2) (div hl 2)]
    (let poly (\\pts (polygon 'black' 'none' 0 pts))
      [
      (rect 'white' xl yl wl hl)
      (poly
        [[(+ xl delta) yl]
         [xw yl]
         [xw (- yh delta)]])
      (poly
        [[xl (+ yl delta)]
         [xl (- yh delta)]
         [(- (+ xl w2) delta) (+ yl h2)]])
      (poly
        [[(+ xl delta) yh]
         [(- xw delta) yh]
         [(+ xl w2) (+ (+ yl h2) delta)]])
    ]))))
;
  (svg
    (append
      (logo (+ x0 80!) (+ y0 36!))
      (concatMap 
        (\\i 
          (figure 
            (+ (* 0.41! w) (+ x0 (* min (cos (rotate i)))))
            (+ (* 0.38! h) (+ y0 (* min (sin (rotate i)))))))
        (range 0! 7!)))))))))))))

"

clique =
 ";
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

"

miscShapes =
 "(let [x y] [200 150] (svg [
  (rect '#999999'  50 10 80 130)
  (circle 'lightblue' 300 100 50)
  (ellipse 'orange' 40 280 30 50)
  (polygon 'lightgreen' 'black' 5 [[110 110] [300 110] [x y]])
  (polygon 'lightgreen' 'black' 5 [[110 210] [300 210] [x y]])
  (line 'blue' 4 10 20 300 40)
]))

"

paths1 =
 "(svg [
  (path_ ['M' 10 10 'H' 90 'V' 90 'H' 10 'L' 10 10 'Z'])
  (path_ ['M' 20 20 'L' 60 20 'L' 60 80 'Z'])
  (path_ ['M' 150 0 'L' 75 200 'L' 225 200 'Z'])
])

"

paths2 =
 "; Adapted from:
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

"

paths3 =
 "(svg [
  (path_ ['M' 10 80 'C' 40 10 65 10 95 80 'S' 150 150 180 80])
  (path_ ['M' 10 80 'Q' 95 10 180 80])
  (path_ ['M' 10 80 'Q' 52.5 10 95 80 'T' 180 80])
])

"

paths4 =
 "; Adapted from:
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

"

paths5 =
 "; Adapted from:
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

"

sailBoat =
 "; A sail boat on the ocean
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

(defrec mod (\\(x m) (if (< x m) x (mod (- x m) m))))
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

"

eyeIcon =
 "; An eye icon
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

"

wikimedia =
 "; Wikimedia Logo
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

"

haskell =
 "; Haskell.org Logo
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
"

matrices =
 "; Definitions for 2D matrices and transform application
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
"

rotTest =
 "
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

"

interfaceButtons =
 "(def [w h depth shadow r] [120 22.5 4 0.5 2])
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

"

barGraph =
 "; A Bar Graph
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
"

thawFreeze =
 "
# unannotated-numbers: n!

; Set [unannotated-numbers] to [n?] or [n!] to declare that
; unannotated literals be thawed or frozen, respectively.
;
; By default, this option is set to [n?].

(svg [(rect 'maroon' 100? 15 200! 50)])

"

deleteBoxes =
 "
# unannotated-numbers: n!

; notice that need to use * instead of mult

(def deleteBoxes
  (let [x0 y0 w h sep] [21? 40? 60? 130? 100?]
  (let boxi (\\i
    (let xi (+ x0 (* i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi [| 0 1 2..4 5 |])))))

deleteBoxes

"

cover =
 "; Logo for Cover
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

"

poppl =
 "; Logo for POP-PL
; see https://github.com/florence/pop-pl

(def M 'M')
(def L 'L')
(def C 'C')
(def Z 'Z')

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
  [M (- xstart stethx) (- ystart stethy)
   C (+ xstart -12) (+ ystart -19)
     (+ cr2x xstart) (+ cr2y ystart)
     xstart ystart
   L (+ xstart ltopWidth) (+ ystart ltopHeight)
   ])

(def axstart  (+ xstart ltopWidth))
(def aystart (+ ystart ltopHeight))
(def ascale 1.9534135150166867!)
(def ax (* ascale ltopWidth))
(def ay (* ascale ltopHeight))
(def bx 18!)
(def armpath
  [M axstart aystart
   C (+ xstart 71) (+ ystart 94)
     (+ xstart 90) (+ ystart 142)
     (+ axstart ax) (+ aystart ay)
   C (+ xstart 63) (+ ystart 190)
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

"

bezier =
 "; Animated Bezier Curves
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
"

-- LITTLE_TO_ELM surveyResultsTriBubbles
-- LITTLE_TO_ELM surveyResultsTriHist
surveyResultsTriHist2 =
 "
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

"

equiTri =
 "
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

"

gridTile =
 "
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

"

lilliconP =
 "
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

"

lilliconP2 =
 "
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

"

keyboard =
 "(def scale 25)
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
"

keyboard2 =
 "(def scale 40)
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
"

keyboard2target =
 "(def scale 40)
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
"

tessellation =
 "; I believe this is set up for group p6mm
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
"

tessellationTarget =
 "; I believe this is set up for group p6mm
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
"

tessellation2 =
 "; I believe this is set up for group p6mm
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
"

floralLogo =
 "(def spiralArmCount 5)
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

"

floralLogo2 =
 "(def ringParameters [
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
"

zones =
 "(def ngon (\\(n cx cy len1 len2)
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

"

roundedRect =
 "
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

"

spiralSpiralGraph =
 "; Spiral Spiral-Graph
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
"

relateRects0 =
 "(svg [
  (rect 'maroon' 49 106 124 86)
  (rect 'blue' 214 91 125 102)
  (rect 'lightgray' 378 231 91 98)
])

"

relateRects1 =
 "(svg [
  (addAttr (rect 'maroon' 49 106 124 86) ['rx' 20])
  (addAttr (addAttr (rect 'blue' 214 91 125 102) ['rx' 10]) ['ry' 30])
  (rect 'lightgray' 378 231 91 98)
])

"

relateCircles0 =
 "(svg (map (\\i (circle 'lightblue' (+ 42 (* i 60)) 98 30)) [|0..6|]))

"

relateLines0 =
 "(svg [
  (line 'darkred' 5 109 274 301 95)
  (line 'darkgreen' 5 202 100 189 288)
  (line 'darkblue' 5 100 100 291 282)
  (line 'salmon' 5 93 178 310 206)
])

"

-- LITTLE_TO_ELM relatePoints0
-- LITTLE_TO_ELM relatePoints1
-- LITTLE_TO_ELM relatePoints2
-- LITTLE_TO_ELM relatePoints3
-- LITTLE_TO_ELM relatePoints4
blank =
 "(svg [])

"


examples =
  [ makeExample "BLANK" blank
  , makeExample scratchName scratch
  -- [ makeExample scratchName scratch
  , makeExample "*Prelude*" Prelude.src
  , makeExample "Wave Boxes" sineWaveOfBoxes

  -- up here during ad-hoc development
  , makeExample "RelateRects0" relateRects0
  , makeExample "RelateRects1" relateRects1
  , makeExample "RelateCircles0" relateCircles0
  , makeExample "RelateLines0" relateLines0
  -- , makeExample "RelatePoints0" relatePoints0
  -- , makeExample "RelatePoints1" relatePoints1
  -- , makeExample "RelatePoints2" relatePoints2
  -- , makeExample "RelatePoints3" relatePoints3
  -- , makeExample "RelatePoints4" relatePoints4
  , makeExample "Delete Boxes" deleteBoxes

  , makeExample "Basic Slides" basicSlides
  , makeExample "Logo" logo
  , makeExample "Botanic Garden Logo" botanic
  , makeExample "Active Trans Logo" activeTrans2
  , makeExample "Sailboat" sailBoat
  , makeExample "Chicago Flag" chicago
  , makeExample "Sliders" sliders
  , makeExample "Buttons" buttons
  , makeExample "Widgets" widgets
  , makeExample "xySlider" xySlider
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
  , makeExample "Wikimedia Logo" wikimedia
  , makeExample "Haskell.org Logo" haskell
  , makeExample "Cover Logo" cover
  , makeExample "POP-PL Logo" poppl
  , makeExample "Lillicon P" lilliconP
  , makeExample "Lillicon P, v2" lilliconP2
  , makeExample "Keyboard" keyboard
  , makeExample "Keyboard Task Before" keyboard2
  , makeExample "Keyboard Task After" keyboard2target
  , makeExample "Tessellation Task Before" tessellation
  , makeExample "Tessellation Task After" tessellationTarget
  , makeExample "Tessellation 2" tessellation2
  , makeExample "Floral Logo 1" floralLogo
  , makeExample "Floral Logo 2" floralLogo2
  , makeExample "Spiral Spiral-Graph" spiralSpiralGraph
  , makeExample "Rounded Rect" roundedRect

  , makeExample "Thaw/Freeze" thawFreeze
  , makeExample "3 Boxes" threeBoxes
  -- , makeExample "N Boxes H2" nBoxesH2
  , makeExample "N Boxes Sli" nBoxes
  , makeExample "N Boxes" groupOfBoxes
  -- , makeExample "6 Boxes A" sixBoxesA
  -- , makeExample "6 Boxes B" sixBoxesB
  -- , makeExample "Wave Tokens" waveOfBoxesTokens
  -- , makeExample "Wave 3" waveOfBoxes3
  -- , makeExample "Chicago Flag 2" chicagoColors
  , makeExample "Elm Logo" elmLogo
  , makeExample "Logo 2" logo2
  , makeExample "Logo Sizes" logoSizes
  , makeExample "Rings" rings
  , makeExample "Polygons" polygons
  , makeExample "Stars" stars
  , makeExample "Triangles" equiTri
  , makeExample "US-13 Flag" usFlag13
  , makeExample "US-50 Flag" usFlag50
  , makeExample "French Sudan Flag" frenchSudan
  , makeExample "Frank Lloyd Wright" flw1
  , makeExample "Frank Lloyd Wright B" flw2
  , makeExample "Bezier Curves" bezier
  , makeExample "Fractal Tree" fractalTree
  , makeExample "Stick Figures" stickFigures
  , makeExample "Cult of Lambda" cultOfLambda
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
  ]

list = examples
