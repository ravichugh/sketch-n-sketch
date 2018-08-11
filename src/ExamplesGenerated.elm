module ExamplesGenerated exposing
  ( list, templateCategories
  , blankSvgTemplate, blankHtmlTemplate, initTemplate
  , badPreludeTemplate
  , fromleo_markdown
  , fromleo_markdown_optimized
  , fromleo_markdown_optimized_lensless
  , fromleo_recipe2
  , fromleo_conference_budgetting
  , fromleo_modelviewcontroller
  , fromleo_latexeditor
  , repl
  , tableOfStatesA
  , tableOfStatesB
  , tableOfStatesC
  , fromleo_linkedtexteditor
  , fromleo_translatabledoc
  , fromleo_dixit
  , christmas_song_3_after_translation
  , mapMaybeLens
  , listAppendLens
  , Example
  )

import Lang exposing (Exp, Val, Widget, Env)
import FastParser
import ElmParser
import Types
import Eval
import Utils
import PreludeGenerated as Prelude
import DefaultIconTheme
import Syntax
import EvalUpdate
import Parser
import ParserUtils

type alias Example = {
   e: Exp,
   v: Val,
   ws: List Widget,
   ati:  Types.AceTypeInfo,
   env: Env}

--------------------------------------------------------------------------------

initTemplate = "Get Started"
blankSvgTemplate = "Blank Svg Document"
blankHtmlTemplate = "Blank Html Document"
badPreludeTemplate = "Bad Prelude"

--------------------------------------------------------------------------------

makeExample = makeExample_ FastParser.parseE Syntax.Little

makeLeoExample = makeExample_ ElmParser.parse Syntax.Elm

makeExample_: (String -> Result Parser.Error Exp) -> Syntax.Syntax -> String -> String -> (String, (String, () -> Result String Example))
makeExample_ parser syntax name s =
  let thunk () =
    -- TODO tolerate parse errors, change Select Example
    parser s |> Result.mapError (\pmsg -> "Error parsing example " ++ name ++"\n" ++ ParserUtils.showError pmsg) |> Result.map (\e ->
    -- let ati = Types.typecheck e in
    let ati = Types.dummyAceTypeInfo in
    -----------------------------------------------------
    -- if name == "*Prelude*" then
    --   {e=e, v=LangSvg.dummySvgVal, ws=[], ati=ati}
    -- else
    -----------------------------------------------------
    let ((v,ws), env) = Utils.fromOk ("Error executing example " ++ name) <| EvalUpdate.runWithEnv syntax e in
    {e=e, v=v, ws=ws, ati=ati,env=env}
    )
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
 """(svg (concat [
]))

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

cat =
 """cat catcolor x y =
  <svg x=\"\"\"@x\"\"\" y=\"\"\"@y\"\"\" height=\"410.013\" id=\"Layer_1\" inkscape:version=\"0.42\" sodipodi:docname=\"Cat03.svg\" sodipodi:version=\"0.32\" space=\"preserve\" style=\"overflow:visible;enable-background:new 0 0 411.244 410.013\" version=\"1.1\" viewBox=\"0 0 411.244 410.013\" width=\"411.244\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:cc=\"http://web.resource.org/cc/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" xmlns:sodipodi=\"http://inkscape.sourceforge.net/DTD/sodipodi-0.dtd\" xmlns:svg=\"http://www.w3.org/2000/svg\">
  <metadata>
    <rdf:RDF xmlns:cc=\"http://web.resource.org/cc/\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">
      <cc:Work rdf:about=\"\">
        <dc:title>Gatto Cat</dc:title>
        <dc:description></dc:description>
        <dc:subject>
          <rdf:Bag>
            <rdf:li>mammal</rdf:li>
            <rdf:li>cat</rdf:li>
            <rdf:li>felini</rdf:li>
            <rdf:li>animale</rdf:li>
          </rdf:Bag>
        </dc:subject>
        <dc:publisher>
          <cc:Agent rdf:about=\"http://www.openclipart.org/\">
            <dc:title>Open Clip Art Library</dc:title>
          </cc:Agent>
        </dc:publisher>
        <dc:creator>
          <cc:Agent>
            <dc:title>Architetto Francesco Rollandin</dc:title>
          </cc:Agent>
        </dc:creator>
        <dc:rights>
          <cc:Agent>
            <dc:title>Architetto Francesco Rollandin</dc:title>
          </cc:Agent>
        </dc:rights>
        <dc:date></dc:date>
        <dc:format>image/svg+xml</dc:format>
        <dc:type rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\"/>
        <cc:license rdf:resource=\"http://web.resource.org/cc/PublicDomain\"/>
        <dc:language>en</dc:language>
      </cc:Work>
      <cc:License rdf:about=\"http://web.resource.org/cc/PublicDomain\">
        <cc:permits rdf:resource=\"http://web.resource.org/cc/Reproduction\"/>
        <cc:permits rdf:resource=\"http://web.resource.org/cc/Distribution\"/>
        <cc:permits rdf:resource=\"http://web.resource.org/cc/DerivativeWorks\"/>
      </cc:License>
    </rdf:RDF>
  </metadata>
  <defs id=\"defs486\"/>
  <sodipodi:namedview bordercolor=\"#666666\" borderopacity=\"1.0\" id=\"base\" inkscape:current-layer=\"Layer_1\" inkscape:cx=\"205.62199\" inkscape:cy=\"205.00650\" inkscape:pageopacity=\"0.0\" inkscape:pageshadow=\"2\" inkscape:window-height=\"712\" inkscape:window-width=\"1024\" inkscape:window-x=\"-4\" inkscape:window-y=\"-4\" inkscape:zoom=\"0.87802094\" pagecolor=\"#ffffff\"/>
  <path d=\"M10.141,257.992c-0.481,0.959-0.963,1.92-1.444,2.881  c-6.188-26.283-14.051-54.486-3.584-85.828c12.342-36.954,39.027-58.056,68.145-81.292c52.758-42.102,157.36-24.69,208.391,11.475  c-12.109-15.802,1.387-36.769-7.221-48.999c0.963,0,1.926,0,2.889,0c-3.598-8.628-7.793-44.061-1.443-51.881  c9.42-11.6,20.74,10.608,27.439,17.293c4.457,4.446,10.457,11.985,15.885,14.411c6.137,2.742,13.703,0.433,20.648,1.817  c7.738,1.542,15.441,4.644,22.678,2.508c8.078-2.383,16.627-11.896,24.553-15.853c8.215-4.101,28.102-23.496,23.48-0.427  c-3.809,19.025-12.955,33.068-19.148,50.866c-1.881,5.401-0.725,11.698-2.887,17.296c-3.207,8.3-10.301,14.584-13,23.059  c-4.041,12.683,3.236,29.55,7.221,41.792c-0.809-1.985-2.076-3.782-2.889-5.765c0.094,5.069,0.547,9.554,1.445,14.413  c-0.48-0.961-0.963-1.922-1.445-2.883c1.225,3.23,1.74,6.798,2.889,10.088c-0.939-0.981-1.928-1.92-2.889-2.881  c0.682,3.778,0.523,7.807,1.445,11.529c-0.48-0.961-0.963-1.922-1.445-2.883c1.072,8.254-6.34,28.786-12.996,28.823  c-8.445,9.111,2.889,22.199,2.889,33.146c0,10.461-0.361,19.191,4.332,28.824c7.549,15.496,26.971,26.91,18.053,44.676  c-7.406,14.752-32.375,0.217-36.828-10.088c-4.326-10.018-5.877-19.301-11.555-28.824c-4.775-8.012-14.961-16.373-15.883-25.941  c-4.588,2.469-9.336,4.199-14.443,4.324c0.48-0.48,0.963-0.961,1.443-1.441c-7.412,2.27-15.354,1.441-23.105,1.441  c12.23,13.467-7.246,14.432-14.443,17.295c11.123,13.564,22.42,28.197,33.217,40.35c4.691,5.283,7.637,12.41,12.996,17.297  c5.469,4.986,12.928,6.824,17.332,12.969c6.58,9.182,3.879,17.729-3.609,25.221c-2.131,2.133-10.631,0.723-13.721,0.723  c-3.443,0-5.578-1.912-8.666-2.885c-0.408-0.129-6.412-0.904-7.219-1.441c-16.309-10.857-23.324-32.895-36.105-44.676  c-4.502-4.148-10.879-7.172-15.887-11.529c-6.738-5.863-12.338-15.416-19.498-20.176c-7.463-4.963-14.754-15.092-22.385-18.734  c-6.576-3.141-20.082-3.455-27.44-4.324c0.482,0.961,0.964,1.922,1.445,2.883c-3.913-2.133-7.585-2.736-11.554-4.322  c0.481,0.961,0.962,1.92,1.444,2.879c-14.43-11.76-46.904-9.967-60.656,1.443c-18.956,15.729,6.898,21.744,16.607,35.309  c7.486,10.459,17.688,20.029,27.735,28.049c9.188,7.334,20.605,11.086,29.312,18.789c8.646,7.648,13.239,25.781,23.106,25.939  c6.898,0.111,22.475-11.799,30.33-14.41c-0.482,0.961-0.965,1.92-1.445,2.881c3.047-1.295,5.059-1.066,8.666-1.441  c-0.482,0.48-0.965,0.961-1.445,1.441c7.871-0.475,6.854,7.154,4.332,12.971c0.482-0.48,0.965-0.961,1.445-1.441  c2.049,17.211-29.061,26.557-42.309,29.197c-27.875,5.557-57.112,8.525-84.782-1.813C69.447,380.018,29.025,314.529,10.141,257.992   M91.016,330.049c16.052,10.254,35.065,13.5,51.563,23.379c12.796,7.664,15.6,15.914,26.423,25.623  C137.46,370.865,110.472,358.873,91.016,330.049\" id=\"path3\" style=\"fill-rule:evenodd;clip-rule:evenodd;fill:#404040;stroke:#000000;stroke-opacity:1.0000000;stroke-width:0.40500002;stroke-miterlimit:9.8212767;stroke-dasharray:none\"/>
  <path d=\"M10.141,257.992  c-0.481,0.959-0.963,1.92-1.444,2.881c-6.188-26.283-14.051-54.486-3.584-85.828c12.342-36.954,39.027-58.056,68.145-81.292  c52.758-42.102,157.36-24.69,208.391,11.475c-12.109-15.802,1.387-36.769-7.221-48.999c0.963,0,1.926,0,2.889,0  c-3.598-8.628-7.793-44.061-1.443-51.881c9.42-11.6,20.74,10.608,27.439,17.293c4.457,4.446,10.457,11.985,15.885,14.411  c6.137,2.742,13.703,0.433,20.648,1.817c7.738,1.542,15.441,4.644,22.678,2.508c8.078-2.383,16.627-11.896,24.553-15.853  c8.215-4.101,28.102-23.496,23.48-0.427c-3.809,19.025-12.955,33.068-19.148,50.866c-1.881,5.401-0.725,11.698-2.887,17.296  c-3.207,8.3-10.301,14.584-13,23.059c-4.041,12.683,3.236,29.55,7.221,41.792c-0.809-1.985-2.076-3.782-2.889-5.765  c0.094,5.069,0.547,9.554,1.445,14.413c-0.48-0.961-0.963-1.922-1.445-2.883c1.225,3.23,1.74,6.798,2.889,10.088  c-0.939-0.981-1.928-1.92-2.889-2.881c0.682,3.778,0.523,7.807,1.445,11.529c-0.48-0.961-0.963-1.922-1.445-2.883  c1.072,8.254-6.34,28.786-12.996,28.823c-8.445,9.111,2.889,22.199,2.889,33.146c0,10.461-0.361,19.191,4.332,28.824  c7.549,15.496,26.971,26.91,18.053,44.676c-7.406,14.752-32.375,0.217-36.828-10.088c-4.326-10.018-5.877-19.301-11.555-28.824  c-4.775-8.012-14.961-16.373-15.883-25.941c-4.588,2.469-9.336,4.199-14.443,4.324c0.48-0.48,0.963-0.961,1.443-1.441  c-7.412,2.27-15.354,1.441-23.105,1.441c12.23,13.467-7.246,14.432-14.443,17.295c11.123,13.564,22.42,28.197,33.217,40.35  c4.691,5.283,7.637,12.41,12.996,17.297c5.469,4.986,12.928,6.824,17.332,12.969c6.58,9.182,3.879,17.729-3.609,25.221  c-2.131,2.133-10.631,0.723-13.721,0.723c-3.443,0-5.578-1.912-8.666-2.885c-0.408-0.129-6.412-0.904-7.219-1.441  c-16.309-10.857-23.324-32.895-36.105-44.676c-4.502-4.148-10.879-7.172-15.887-11.529c-6.738-5.863-12.338-15.416-19.498-20.176  c-7.463-4.963-14.754-15.092-22.385-18.734c-6.576-3.141-20.082-3.455-27.44-4.324c0.482,0.961,0.964,1.922,1.445,2.883  c-3.913-2.133-7.585-2.736-11.554-4.322c0.481,0.961,0.962,1.92,1.444,2.879c-14.43-11.76-46.904-9.967-60.656,1.443  c-18.956,15.729,6.898,21.744,16.607,35.309c7.486,10.459,17.688,20.029,27.735,28.049c9.188,7.334,20.605,11.086,29.312,18.789  c8.646,7.648,13.239,25.781,23.106,25.939c6.898,0.111,22.475-11.799,30.33-14.41c-0.482,0.961-0.965,1.92-1.445,2.881  c3.047-1.295,5.059-1.066,8.666-1.441c-0.482,0.48-0.965,0.961-1.445,1.441c7.871-0.475,6.854,7.154,4.332,12.971  c0.482-0.48,0.965-0.961,1.445-1.441c2.049,17.211-29.061,26.557-42.309,29.197c-27.875,5.557-57.112,8.525-84.782-1.813  C69.447,380.018,29.025,314.529,10.141,257.992\" id=\"path5\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M91.016,330.049  c16.052,10.254,35.065,13.5,51.563,23.379c12.796,7.664,15.6,15.914,26.423,25.623C137.46,370.865,110.472,358.873,91.016,330.049\" id=\"path7\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M169.001,379.051  c10.577,5.416,29.337-0.01,40.438-2.885\" id=\"path9\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M189.22,355.992  c3.357,1.346,5.867,3.85,7.222,7.203\" id=\"path11\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M177.667,351.668  c3.506,1.109,6.974,3.115,10.109,5.764\" id=\"path13\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M187.776,404.988  c-4.033-0.439-8.045-1.959-11.554-4.322\" id=\"path15\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M179.112,404.988  c-3.549-1.537-5.919-3.143-8.668-5.764\" id=\"path17\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M140.118,400.666  c-3.129-1.865-5.387-4.145-7.222-7.205\" id=\"path19\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M131.453,396.342  c-3.282-2.74-5.685-6.1-7.222-10.088\" id=\"path21\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M128.563,338.695  c-3.355-1.348-5.865-3.852-7.221-7.205\" id=\"path23\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M121.343,335.814  c-2.572-2.365-4.127-5.402-4.332-8.648\" id=\"path25\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M135.786,324.285  c-0.635-3.248-2.132-6.268-4.333-8.648\" id=\"path27\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M132.896,321.402  c-3.109-3.965-6.138-6.986-10.108-10.09\" id=\"path29\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M128.563,321.402  c-7.581-8.572-20.945-10.52-24.551-21.619\" id=\"path31\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M105.456,322.844  c-4.647-1.688-7.602-6.42-8.664-11.531\" id=\"path33\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M92.459,318.52  c-4.019-4.518-4.489-9.77-5.776-15.852\" id=\"path35\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M80.905,291.139  c-2.021,6.873,0.662,13.24,2.889,20.174\" id=\"path37\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M89.57,370.402  c-3.948-3.781-6.11-9.033-7.22-14.41\" id=\"path39\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M72.24,354.549  c-4.087-3.693-5.343-9.059-5.776-14.41\" id=\"path41\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M63.575,344.461  c-0.699-5.301-1.61-10.406-1.443-15.852\" id=\"path43\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M43.356,312.758  c-1.9-7.588-4.631-15.186-4.332-23.061\" id=\"path45\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M34.692,302.668  c-0.539-4.844-0.17-9.871,1.443-14.414\" id=\"path47\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M91.016,330.049  c-0.963-1.441-1.927-2.881-2.89-4.322c-0.48,0.961-0.962,1.922-1.443,2.883c-10.926-18.174-23.857-42.352-28.883-57.646  c-1.729-9.48-2.889-17.711-2.889-27.383\" id=\"path49\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M49.133,260.873  c-3.92-2.787-5.94-6.77-5.776-11.529\" id=\"path51\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M43.356,260.873  c-4.034-5.34-5.721-12.188-7.221-18.734\" id=\"path53\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M23.138,245.02  c-4.254,6.518-5.807,15.348-2.888,23.061\" id=\"path55\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M14.474,250.787  c-0.462-7.244,1.559-14.314,5.776-20.178\" id=\"path57\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M10.141,242.139  c0-9.607-0.132-17.43,4.333-25.939\" id=\"path59\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M57.8,227.729  c5.723,5.98,9.951,14.584,8.664,23.059\" id=\"path61\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M69.352,256.551  c1.835-7.336,2.272-14.52-1.444-21.617\" id=\"path63\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M80.905,276.725  c6.285-6.203,17.383-6.703,20.219-15.852\" id=\"path65\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M95.349,243.58  c-0.075-7.754,2.184-15.396,7.221-21.619\" id=\"path67\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M50.578,200.347  c-0.353,3.988-1.315,7.831-2.889,11.528\" id=\"path69\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M44.8,203.228  c-0.082-5.035,1.957-9.677,5.778-12.971\" id=\"path71\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M54.911,175.844  c-6.303,3.582-11.084,9.598-14.442,15.854\" id=\"path73\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M40.469,180.169  c4.882-5.53,9.191-10.912,15.886-14.411\" id=\"path75\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M8.696,198.903  c1.15-6.224,3.31-12.264,7.221-17.292\" id=\"path77\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M11.585,183.05  c0.82-6.752,5.944-11.165,11.553-14.411\" id=\"path79\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M15.917,170.082  c2.754-3.26,6.124-5.662,10.109-7.207\" id=\"path81\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M112.678,253.668  c5.343-2.734,7.544-7.697,10.11-12.971\" id=\"path83\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M127.12,237.814  c-0.33,5.494-2.26,10.609-4.332,15.854\" id=\"path85\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M127.12,252.227  c3.947-7.453,6.816-14.518,5.776-23.059\" id=\"path87\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M112.678,236.375  c-0.977-7.016,3.077-12.674,5.778-18.736\" id=\"path89\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M112.678,214.756  c5.109-4.369,7.663-10.745,8.665-17.292\" id=\"path91\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M122.788,203.228  c2.113-4.146,2.121-8.85,0-12.971\" id=\"path93\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M147.339,232.051  c4.77-2.01,7.77-6.436,8.665-11.529\" id=\"path95\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M148.782,224.846  c4.994-2.004,7.851-6.141,7.222-11.529\" id=\"path97\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M150.228,207.551  c2.396-3.494,3.365-7.339,2.888-11.529\" id=\"path99\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M157.448,206.109  c2.645-9.515,0.438-17.983-1.444-27.382\" id=\"path101\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M166.112,194.58  c0-12.098-1.489-24.357-8.664-34.588\" id=\"path103\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M78.019,145.581  c-0.008-5.539,1.689-10.942,4.332-15.853\" id=\"path105\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M72.24,141.258  c0.14-6.828,3.422-14.191,8.665-18.734\" id=\"path107\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M70.797,119.641  c4.067-4.729,9.599-7.678,15.886-7.206\" id=\"path109\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M75.13,110.995  c3.31-2.83,7.193-3.764,11.553-2.883\" id=\"path111\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M127.12,93.699  c6.609-0.592,14.685-0.049,20.219,2.883\" id=\"path113\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M144.45,92.259  c-5.258-3.969-10.973-2.883-17.33-2.883\" id=\"path115\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M131.453,99.465  c8.478,2.319,27.572,10.421,28.884,15.853c9.729,10.598,29.436,24.657,23.107,41.792c-0.482-0.48-0.963-0.961-1.445-1.442  c0,24.672-3.342,50.205-15.887,72.06c-0.48-0.961-0.963-1.922-1.443-2.883c-4.276,8.953-8.999,16.02-17.33,21.617  c0.481-0.961,0.963-1.922,1.443-2.883c-3.562,4.881-8.107,9.42-12.996,12.971c16.457-6.568,45.057-6.813,56.322-21.617\" id=\"path117\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M171.891,233.49  c4.06-6.316,8.479-12.824,10.108-20.174\" id=\"path119\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M170.444,239.258  c1.955-1.545,3.406-3.469,4.335-5.768\" id=\"path121\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M212.328,240.697  c-1.563-1.51-1.988-3.521-1.443-5.764\" id=\"path123\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M209.439,226.285  c-3.516,3.578-5.379,7.889-5.776,12.973\" id=\"path125\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M199.331,233.49  c-1.142-9.584,8.84-16.496,15.885-21.615\" id=\"path127\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M207.996,204.669  c-7.122,3.79-12.703,12.889-15.887,20.176c0.963-0.48,1.927-0.961,2.89-1.441c-2.313,4.922-3.399,10.438-2.89,15.854  c0.963-0.961,1.927-1.922,2.89-2.883c0.146,10.76,10.768,25.311,18.773,31.705\" id=\"path129\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M219.548,257.992  c-2.482-1.75-4.051-4.295-4.332-7.205\" id=\"path131\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M215.216,257.992  c-4.148-4.156-6.635-9.428-4.332-14.412\" id=\"path133\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M233.99,245.02  c-1.227,1.742-1.711,3.664-1.443,5.768\" id=\"path135\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M235.435,249.344  c-1.229,1.74-1.713,3.662-1.445,5.766\" id=\"path137\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M236.878,255.109  c0,1.922,0,3.842,0,5.764\" id=\"path139\" style=\"fill:none;stroke:#000000;stroke-width:0;stroke-linecap:square;stroke-miterlimit:10;\"/>
  <path d=\"M229.658,262.316  c3.898,4.271,8.102,8.453,12.998,11.527\" id=\"path141\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M248.433,276.725  c4.498,6.152,14.479,7.947,21.662,11.529\" id=\"path143\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M262.875,291.139  c5.281,1.852,9.381,5.697,12.998,10.09\" id=\"path145\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M316.31,347.344  c2.439,5.367,2.439,11.775-2.889,14.41\" id=\"path147\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M329.308,367.521  c5.039-4.025,5.396-10.334,2.889-15.854\" id=\"path149\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M337.972,345.902  c3.535,4.074,5.043,9,4.332,14.412\" id=\"path151\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M371.189,317.08  c4.531-3.459,6.57-8.688,5.777-14.412\" id=\"path153\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M388.521,301.229  c1.379,6.584-0.877,12.873-5.779,17.291\" id=\"path155\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M394.296,301.229  c-1.324-1.182-2.293-2.639-2.889-4.326\" id=\"path157\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M358.191,285.373  c-1.91,1.92-3.568,3.16-5.775,4.324\" id=\"path159\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M345.195,272.402  c-2.014-3.26-2.014-6.828,0-10.086\" id=\"path161\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M337.972,262.316  c0.635-3.246,2.131-6.268,4.332-8.648\" id=\"path163\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M336.529,257.992  c1.463-3.607,3.443-7.076,5.775-10.088\" id=\"path165\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M358.191,249.344  c-0.268-2.102,0.217-4.023,1.443-5.764\" id=\"path167\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M356.748,243.58  c1.229-1.74,1.713-3.664,1.443-5.766\" id=\"path169\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M355.302,237.814  c1.254-2.756,1.736-5.637,1.445-8.646\" id=\"path171\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M335.083,227.729  c0.408,3.494-0.072,6.865-1.443,10.086\" id=\"path173\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M332.197,233.49  c-1.238,2.289-2.203,4.693-2.889,7.207\" id=\"path175\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M327.865,220.521  c5.6-12.734-6.506-17.941-10.111-27.381\" id=\"path177\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M322.085,213.316  c0.479,4.189-0.49,8.035-2.889,11.529\" id=\"path179\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M307.646,223.404  c-3.363,5.332-7.662,9.615-13,12.971\" id=\"path181\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M304.757,211.875  c-0.533,4.424-2.057,8.951-4.334,12.971\" id=\"path183\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M296.091,230.609  c-3.002,3.029-6.039,5.031-8.664,5.766\" id=\"path185\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M293.203,266.639  c-4.611-1.781-9.871-0.025-12.998,2.883\" id=\"path187\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M280.205,269.521  c0.205-1.287-0.318-2.223-1.445-2.883\" id=\"path189\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M275.873,263.756  c1.18-6.863,10.006-5.416,14.441-1.439c-5.666,4.854-13.785,5.436-15.887-1.443\" id=\"path191\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M277.316,270.963  c-6.236-12.455-17.502-23.652-21.662-36.029\" id=\"path193\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M261.429,246.463  c9.516,3.242,20.291,4.445,30.33,7.205\" id=\"path195\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M288.871,255.109  c1.598,1.945,2.037,4.514,1.443,7.207\" id=\"path197\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M267.207,232.051  c2.178-7.025,3.262-18.271-4.332-21.619\" id=\"path199\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M271.541,216.199  c0,4.322,0,8.646,0,12.969\" id=\"path201\" style=\"fill:none;stroke:#000000;stroke-width:0;stroke-linecap:square;stroke-miterlimit:10;\"/>
  <path d=\"M274.427,232.051  c2.666-2.734,3.619-6.248,2.889-10.09\" id=\"path203\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M296.091,200.347  c2.588-6.235,1.121-12.641-1.445-18.736\" id=\"path205\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M298.978,204.669  c3.658-7.642,2.326-13.722,0-21.619\" id=\"path207\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M304.757,190.257  c0.268-2.103-0.217-4.025-1.445-5.765\" id=\"path209\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M268.652,172.963  c-6.734-1.943-16.303,0.44-20.219,7.206\" id=\"path211\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M248.433,151.346  c-4.127,1.988-7.705,5.031-10.111,8.646\" id=\"path213\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M226.769,168.64  c-2.332,3.013-4.313,6.481-5.775,10.088\" id=\"path215\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M226.769,174.404  c-1.4,2.193-1.873,4.61-1.443,7.207\" id=\"path217\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M219.548,185.934  c-2.373,1.833-3.814,4.239-4.332,7.207\" id=\"path219\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M213.771,185.934  c-4.254,3.277-7.186,7.862-8.665,12.969\" id=\"path221\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M187.776,211.875  c1.702-5.086,1.628-10.744,0-15.854\" id=\"path223\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M190.665,200.347  c0.291-3.01-0.191-5.893-1.445-8.648\" id=\"path225\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M190.665,180.169  c0.291-3.01-0.191-5.893-1.445-8.648\" id=\"path227\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M193.553,174.404  c-0.688-2.512-1.651-4.918-2.888-7.206\" id=\"path229\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M197.886,162.875  c2.459-6.799-0.758-13.535-2.888-20.175\" id=\"path231\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M202.218,171.521  c2.078-6.592,2.129-13.655,0-20.175\" id=\"path233\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M206.55,157.11  c-0.963-2.882-1.925-5.765-2.887-8.648\" id=\"path235\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M207.996,139.816  c-2.201-4.908-5.5-9.527-10.11-11.529\" id=\"path237\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M161.78,108.111  c4.848,1.997,9.091,3.957,12.999,7.207\" id=\"path239\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M154.561,103.789  c5.427-0.511,10.933,0.596,15.884,2.881\" id=\"path241\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M138.675,79.288  c6.061,2.016,12.057,3.522,17.329,7.204\" id=\"path243\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M156.004,83.611  c3.964,1.415,6.019,4.471,5.776,8.648\" id=\"path245\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M176.223,82.169  c4.778,1.655,9.049,4.195,12.997,7.206\" id=\"path247\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M210.884,80.729  c3.801,3.421,7.246,7.294,10.109,11.529\" id=\"path249\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M228.214,89.375  c1.926,2.882,3.852,5.764,5.775,8.646\" id=\"path251\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M233.99,89.375  c0.963,1.921,1.926,3.843,2.889,5.765\" id=\"path253\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M252.765,125.404  c2.482-5.509,6.227-10.198,11.555-12.969\" id=\"path255\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M258.541,131.17  c0.178-4.483,2.252-8.594,5.779-11.53\" id=\"path257\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M307.646,134.052  c-5.668,6.237-1.949,14.486,2.887,20.177\" id=\"path259\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M313.421,145.581  c-2.604,5.34,1.059,10.258,7.221,11.529\" id=\"path261\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M350.97,168.64  c4.252,7.333,5.529,16.013,2.889,24.5\" id=\"path263\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M358.191,198.903  c2.256-6.391,2.023-13.704,0-20.175\" id=\"path265\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M361.08,164.317  c-2.086-6.078-5.609-11.279-10.109-15.854\" id=\"path267\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M352.416,139.816  c4.254,3.277,7.186,7.863,8.664,12.971\" id=\"path269\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M362.523,145.581  c2.643,4.911,4.342,10.315,4.334,15.854\" id=\"path271\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M362.523,214.756  c-0.857,0.744-1.777,2.781-1.443,0c-1.229,5.543-6.076,11.639-8.664,11.529c-2.127,4.127-4.475,7.873-7.221,11.529  c0-0.961,0-1.922,0-2.881c-2.846,3.418-5.711,6.754-8.666,10.086c1.063-1.861,1.885-3.867,2.887-5.762  c-3.418,3.871-7.121,7.555-11.551,10.086\" id=\"path273\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M281.648,105.228  c10.549,13.584,16.85,1.664,27.441,4.323c10.33,2.594,10.785,16.544,25.994,10.089\" id=\"path275\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M320.642,123.964  c-11.648,2.481-18.086-1.289-25.996-10.088\" id=\"path277\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M290.314,105.228  c-15.09-2.491-28.488,0.307-43.324,1.442\" id=\"path279\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M236.878,100.905  c19.971-1.991,39.533-2.488,59.213,1.441\" id=\"path281\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M293.203,96.582  c-11.979-4.73-30.795-6.708-43.326-2.883\" id=\"path283\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M303.312,95.14  c10.15-8.883,22.537,8.569,11.553,4.325c-0.932,4.81-2.109,4.152-5.775,7.205c-1.02-4.596-2.789-4.906-1.443-8.648  C302.861,99.313,304.951,96.475,303.312,95.14\" id=\"path285\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M304.757,98.022  c-0.205,1.287,0.316,2.224,1.443,2.883\" id=\"path287\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M314.865,102.347  c1.287,0.205,2.227-0.316,2.889-1.441\" id=\"path289\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M310.533,99.465  c-0.752,3.397-1.203,6.542-1.443,10.086\" id=\"path291\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M375.521,136.933  c-16.697-12.502-30.346-20.198-49.102-28.822\" id=\"path293\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M330.751,113.876  c8.668,5.537,16.084,11.624,24.551,17.294\" id=\"path295\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M353.859,115.318  c-8.799-3.83-14.682-9.507-24.551-10.09\" id=\"path297\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M332.197,89.375  c-1.072-0.752-1.959,0.136-2.889-1.439c3.846-11.273,23.867-10.171,20.939-4.325c-4.148,8.289-16.561,10.112-19.496,1.442\" id=\"path299\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M339.416,86.492  c0.588-3.017-1.111-4.572,1.445-5.763C341.539,83.514,339.794,84.548,339.416,86.492\" id=\"path301\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M307.646,77.846  c-4.82,10.252-15.887,1.832-15.887-7.204c0-12.438,28.076,17.022,11.553,11.527\" id=\"path303\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M297.535,77.846  c0.197-3.057-0.502-4.838,1.443-7.206C299.671,73.78,298.035,75.538,297.535,77.846\" id=\"path305\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M281.648,46.142  c-7.088-11.299-2.889-26.132-2.889-38.911c8.775,1.112,27.119,25.041,27.441,30.265c-1.682-1.678-3.961-2.788-5.777-4.325  c1.045,2.074,1.691,3.751,2.889,5.765c-1.867-0.24-2.951-1.17-4.334-1.439c0.848,1.755,1.75,2.842,2.889,4.323  c-0.973,0.093-3.512-0.489-4.332,0c0.182,0.959,2.703,1.333,1.443,2.881c-2.305,0.267-1.256-1.135-2.887-1.441  c-0.488,0.747,0.088,3.408,0,4.323c-1.029-0.357-2.018-2.202-2.889-2.881c0.48,1.442,0.963,2.883,1.443,4.325  c-1.613-1.594-4.027-2.589-5.775-4.325c0.338,1.862-0.305,3.896,0,5.765c-1.158-1.966-2.867-3.646-4.332-5.765  c0.391,2.177,0.861,3.638,1.443,5.765c-0.734-1.543-2.621-5.497-3.611-6.485C280.33,41.941,282.486,45.526,281.648,46.142\" id=\"path307\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M280.205,63.436  c0.117,3.843,1.879,7.266,4.334,8.648\" id=\"path309\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M280.205,73.523  c1.445,1.44,2.889,2.881,4.334,4.323\" id=\"path311\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M294.646,64.877  c-1.602-2.371-2.064-5.508-1.443-8.648\" id=\"path313\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M298.978,53.348  c1.666,2.321,3.24,4.938,4.334,7.207\" id=\"path315\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M307.646,60.554  c-1.76-2.74-2.217-6.511-1.445-10.09\" id=\"path317\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M310.533,50.464  c-0.752,3.399-1.203,6.545-1.443,10.09\" id=\"path319\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M293.203,31.729  c0.291-3.009-0.191-5.891-1.443-8.646\" id=\"path321\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M294.646,27.406  c0,1.921,0,3.843,0,5.765\" id=\"path323\" style=\"fill:none;stroke:#000000;stroke-width:0;stroke-linecap:square;stroke-miterlimit:10;\"/>
  <path d=\"M313.421,44.7  c0.688-2.238,1.574-4.027,2.889-5.765\" id=\"path325\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M316.31,46.142  c-0.295-2.49,0.717-4.391,2.887-5.765\" id=\"path327\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M320.642,53.348  c0.688-2.512,1.65-4.918,2.887-7.206\" id=\"path329\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M324.976,47.581  c0.652-2.234,2.096-3.674,4.332-4.323\" id=\"path331\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M342.304,64.877  c1.373-3.222,1.852-6.594,1.443-10.089\" id=\"path333\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M349.527,57.671  c-1.549,3.98-3.957,7.344-7.223,10.088\" id=\"path335\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M345.195,69.2  c1.689-0.592,3.148-1.559,4.332-2.883\" id=\"path337\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M348.083,74.963  c1.184-1.323,2.643-2.288,4.332-2.879\" id=\"path339\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M352.416,74.963  c3.686-1.691,5.434-4.582,5.775-8.646\" id=\"path341\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M356.748,106.67  c2.262,3.994,5.363,5.747,8.666,4.325\" id=\"path343\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M355.302,100.905  c3.387,4.264,7.529,7.206,13,7.206\" id=\"path345\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M365.414,103.789  c3.076,0.074,5.891-0.402,8.664-1.442\" id=\"path347\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M378.41,80.729  c1.955-1.544,3.404-3.469,4.332-5.767\" id=\"path349\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M384.187,79.288  c1.252-2.756,1.734-5.639,1.443-8.648\" id=\"path351\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M372.634,51.906  c-1.271-15.737,21.039-27.823,33.215-33.147c1.141,14.406-4.023,34.972-17.328,41.795c0.836-2.15,0.883-3.668,1.443-5.767  c-3.125,4.186-4.086,4.197-8.666,5.767c0.268,0.659,1.451-2.984,1.443-2.883c-1.352,0.645-2.953,2.075-4.332,2.883  c0.521-2.146,0.631-3.596,1.443-5.767c-1.031,0.856-1.932,1.941-2.887,2.883c1.023-3.235,1.365-6.649,2.887-10.09  c-1.803,2.016-4.08,3.664-5.775,5.767c1.051-2.066,1.684-4.888,2.889-7.206C374.673,47.766,373.789,49.943,372.634,51.906\" id=\"path353\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M382.742,43.258  c-1.756-1.621-1.48-3.697,0-5.763\" id=\"path355\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M385.63,40.377  c-1.711-1.252-1.49-2.75,0-4.325\" id=\"path357\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M169.001,379.051c-9.879-8.861-12.484-18.539-24.551-24.502  c1.444,0,2.889,0,4.332,0c-0.506-2.188-2.023-3.467-2.889-5.764c1.268-0.143,3.139,0.418,4.334,0  c-3.945-3.34-24.613-11.971-10.109-14.412c0.018,0.004,8.746,1.703,12.997,0c-0.481-0.48-0.962-0.961-1.443-1.441  c21.587-9.656,49.044,27.164,57.767,43.234C198.886,379.539,179.015,384.178,169.001,379.051 M313.421,361.754  c-11.873,5.443-22.941-19.086-25.994-25.939c0.961-0.48,1.924-0.961,2.887-1.441c-3.688-2.279-2.549-2.402-4.332-5.764  c10.922-1.557,15.27,4.27,21.664,4.322c4.77,3.387,9.598,4.615,17.33,7.207c-0.963,0-1.928,0-2.891,0  c5.932,2.666,9.695,0.701,8.666-5.766C365.638,345.514,327.755,385.779,313.421,361.754 M271.541,259.434  c-3.561-4.178-6.6-8.627-10.111-12.971c8.926,3.041,42.002,1.793,31.773,20.176c-3.857-2.326-12.393,1.082-15.887,4.324  C275.392,267.119,273.466,263.275,271.541,259.434 M275.873,263.756c1.18-6.863,10.006-5.416,14.441-1.439  C285.58,266.371,279.14,267.051,275.873,263.756 M352.416,289.697c5.984-4.596,12.338-0.363,17.33,0  c7.855,0.574,9.695-3.732,17.33,2.883c0-0.961,0-1.922,0-2.883c13.93,10.506,9.119,26.904-7.223,28.822  C360.384,320.807,357.285,305.643,352.416,289.697 M342.304,237.814c2.236-8.699,6.018-10.041,4.334-18.734  c1.061,1.904,2.205,2.254,2.889,4.324c1.662-7.635,1.496-16.045-7.223-18.735c0,0.96,0,1.92,0,2.881  c-4.744-9.382-21.98-14.279-21.662-24.5c-4.654,0.222-14.83-5.391-14.441-12.969c-2.779,2.768-5.059,3.166-7.223,7.206  c0-1.441,0-2.883,0-4.325c-1.182,0.28-1.061,2.499-2.887,1.441c-0.639-4.689,0.316-9.018,2.887-12.969  c-0.48,0.48-0.961,0.96-1.443,1.44c-1.832-6.687-10.396-13.667-14.441-12.969c0-6.542,3.598-22.83-7.221-17.296  c0.219-1.362,1.369-0.984,1.443-2.881c-2.951,1.291-5.854,1.034-8.664,2.881c2.83-6.353,4.658-11.774,10.107-15.853  c-3.389-0.964-5.314-1.856-10.107-1.439c1.893-1.155,3.668-2.866,5.775-4.323c-0.611-0.244-4.1-1.263-4.332-1.443  c0.963-0.48,1.926-0.96,2.889-1.44c0.402,1.296-3.076-1.581-2.889-1.441c3.35-4.633,4.768-1.819,1.445-8.648  c5.822,2.779,9.922,8.34,15.887,10.089c-2.492-3.848-2.832-3.467,0-8.646c-0.963,0.48-1.926,0.96-2.889,1.44  c-0.098-4.18-1.287-6,0-10.088c-0.482,0.48-0.965,0.961-1.445,1.441c1.813-10.149,11.238-6.163,15.885-7.206  c1.17,1.388,2.209,4.074,2.889,5.765c0.482-0.96,0.963-1.921,1.445-2.881c-1.156,7.662,2.406,12.736,5.777,18.734  c4.465-3.056,6.537-4.926,11.553-5.765c-0.578-4.471-3.82-7.451-8.664-8.646c2.275-1.831,2.18-3.257,4.332-5.767  c-0.963,0.961-1.926,1.922-2.889,2.883c2.633-5.253,5.361-10.285,7.221-15.852c0.482,0.48,0.963,0.959,1.443,1.439  c1.676-5.089,1.811-10.468,1.443-15.852c0.217-0.909,4.914-9.045,10.111-11.53c-3.141,4.714-4.395,10.077-4.332,15.854  c0.48-0.48,0.961-0.961,1.443-1.442c0.58,5.205,1.303,9.041,1.445,14.413c0.48-0.481,0.961-0.962,1.443-1.444  c-0.936,1.595-1.51,4.028-2.889,5.767c-0.482-0.48-0.963-0.961-1.443-1.441c-0.666,3.361-2.018,5.512-2.889,8.648  c0-0.961,0-1.922,0-2.883c-1.898,4.375-0.225,8.228,4.332,10.087c-0.482-0.48-0.963-0.961-1.443-1.441  c5.895,2.501,10.354,0.246,13.719,3.604c3.633,3.624,3.674,9.034,9.389,12.249c-0.963,0-1.926,0-2.889,0  c14.148,7.968,13.217,15.72,28.883,1.443c-9.871,12.066,0.242,33.55,4.332,46.116c-0.809-1.985-2.076-3.782-2.889-5.765  c0.094,5.069,0.547,9.554,1.445,14.413c-0.48-0.961-0.963-1.922-1.445-2.883c1.225,3.23,1.74,6.798,2.889,10.088  c-0.939-0.981-1.928-1.92-2.889-2.881c0.682,3.778,0.523,7.807,1.445,11.529c-0.48-0.961-0.963-1.922-1.445-2.883  c1.273,9.799-6.221,26.863-12.996,28.823c-2.16,3.76-5.563,7.109-5.777,7.205c-1.229,5.543-6.076,11.639-8.664,11.529  c-2.127,4.127-4.475,7.873-7.221,11.529c0-0.961,0-1.922,0-2.881C344.232,235.893,343.267,236.854,342.304,237.814\" id=\"path359\" style=\"fill-rule:evenodd;clip-rule:evenodd;fill:#D9D9C5;stroke:#000000;stroke-opacity:1.0000000;stroke-width:0.40500002;stroke-miterlimit:9.8212767;stroke-dasharray:none\"/>
  <path d=\"M169.001,379.051  c-9.879-8.861-12.484-18.539-24.551-24.502c1.444,0,2.889,0,4.332,0c-0.506-2.188-2.023-3.467-2.889-5.764  c1.268-0.143,3.139,0.418,4.334,0c-3.945-3.34-24.613-11.971-10.109-14.412c0.018,0.004,8.746,1.703,12.997,0  c-0.481-0.48-0.962-0.961-1.443-1.441c21.587-9.656,49.044,27.164,57.767,43.234C198.886,379.539,179.015,384.178,169.001,379.051\" id=\"path361\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M313.421,361.754  c-11.873,5.443-22.941-19.086-25.994-25.939c0.961-0.48,1.924-0.961,2.887-1.441c-3.688-2.279-2.549-2.402-4.332-5.764  c10.922-1.557,15.27,4.27,21.664,4.322c4.77,3.387,9.598,4.615,17.33,7.207c-0.963,0-1.928,0-2.891,0  c5.932,2.666,9.695,0.701,8.666-5.766C365.638,345.514,327.755,385.779,313.421,361.754\" id=\"path363\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M271.541,259.434  c-3.561-4.178-6.6-8.627-10.111-12.971c8.926,3.041,42.002,1.793,31.773,20.176c-3.857-2.326-12.393,1.082-15.887,4.324  C275.392,267.119,273.466,263.275,271.541,259.434\" id=\"path365\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M275.873,263.756  c1.18-6.863,10.006-5.416,14.441-1.439C285.58,266.371,279.14,267.051,275.873,263.756\" id=\"path367\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M352.416,289.697  c5.984-4.596,12.338-0.363,17.33,0c7.855,0.574,9.695-3.732,17.33,2.883c0-0.961,0-1.922,0-2.883  c13.93,10.506,9.119,26.904-7.223,28.822C360.384,320.807,357.285,305.643,352.416,289.697\" id=\"path369\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M342.304,237.814  c2.236-8.699,6.018-10.041,4.334-18.734c1.061,1.904,2.205,2.254,2.889,4.324c1.662-7.635,1.496-16.045-7.223-18.735  c0,0.96,0,1.92,0,2.881c-4.744-9.382-21.98-14.279-21.662-24.5c-4.654,0.222-14.83-5.391-14.441-12.969  c-2.779,2.768-5.059,3.166-7.223,7.206c0-1.441,0-2.883,0-4.325c-1.182,0.28-1.061,2.499-2.887,1.441  c-0.639-4.689,0.316-9.018,2.887-12.969c-0.48,0.48-0.961,0.96-1.443,1.44c-1.832-6.687-10.396-13.667-14.441-12.969  c0-6.542,3.598-22.83-7.221-17.296c0.219-1.362,1.369-0.984,1.443-2.881c-2.951,1.291-5.854,1.034-8.664,2.881  c2.83-6.353,4.658-11.774,10.107-15.853c-3.389-0.964-5.314-1.856-10.107-1.439c1.893-1.155,3.668-2.866,5.775-4.323  c-0.611-0.244-4.1-1.263-4.332-1.443c0.963-0.48,1.926-0.96,2.889-1.44c0.402,1.296-3.076-1.581-2.889-1.441  c3.35-4.633,4.768-1.819,1.445-8.648c5.822,2.779,9.922,8.34,15.887,10.089c-2.492-3.848-2.832-3.467,0-8.646  c-0.963,0.48-1.926,0.96-2.889,1.44c-0.098-4.18-1.287-6,0-10.088c-0.482,0.48-0.965,0.961-1.445,1.441  c1.813-10.149,11.238-6.163,15.885-7.206c1.17,1.388,2.209,4.074,2.889,5.765c0.482-0.96,0.963-1.921,1.445-2.881  c-1.156,7.662,2.406,12.736,5.777,18.734c4.465-3.056,6.537-4.926,11.553-5.765c-0.578-4.471-3.82-7.451-8.664-8.646  c2.275-1.831,2.18-3.257,4.332-5.767c-0.963,0.961-1.926,1.922-2.889,2.883c2.633-5.253,5.361-10.285,7.221-15.852  c0.482,0.48,0.963,0.959,1.443,1.439c1.676-5.089,1.811-10.468,1.443-15.852c0.217-0.909,4.914-9.045,10.111-11.53  c-3.141,4.714-4.395,10.077-4.332,15.854c0.48-0.48,0.961-0.961,1.443-1.442c0.58,5.205,1.303,9.041,1.445,14.413  c0.48-0.481,0.961-0.962,1.443-1.444c-0.936,1.595-1.51,4.028-2.889,5.767c-0.482-0.48-0.963-0.961-1.443-1.441  c-0.666,3.361-2.018,5.512-2.889,8.648c0-0.961,0-1.922,0-2.883c-1.898,4.375-0.225,8.228,4.332,10.087  c-0.482-0.48-0.963-0.961-1.443-1.441c5.895,2.501,10.354,0.246,13.719,3.604c3.633,3.624,3.674,9.034,9.389,12.249  c-0.963,0-1.926,0-2.889,0c14.148,7.968,13.217,15.72,28.883,1.443c-9.871,12.066,0.242,33.55,4.332,46.116  c-0.809-1.985-2.076-3.782-2.889-5.765c0.094,5.069,0.547,9.554,1.445,14.413c-0.48-0.961-0.963-1.922-1.445-2.883  c1.225,3.23,1.74,6.798,2.889,10.088c-0.939-0.981-1.928-1.92-2.889-2.881c0.682,3.778,0.523,7.807,1.445,11.529  c-0.48-0.961-0.963-1.922-1.445-2.883c1.273,9.799-6.221,26.863-12.996,28.823c-2.16,3.76-5.563,7.109-5.777,7.205  c-1.229,5.543-6.076,11.639-8.664,11.529c-2.127,4.127-4.475,7.873-7.221,11.529c0-0.961,0-1.922,0-2.881  C344.232,235.893,343.267,236.854,342.304,237.814\" id=\"path371\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M177.667,351.668  c3.506,1.109,6.974,3.115,10.109,5.764\" id=\"path373\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M189.22,355.992  c3.376,1.139,5.332,3.613,5.778,7.203\" id=\"path375\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M313.421,361.754  c5.328-2.635,5.328-9.043,2.889-14.41\" id=\"path377\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M332.197,351.668  c2.508,5.52,2.15,11.828-2.889,15.854\" id=\"path379\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M342.304,360.314  c0.711-5.412-0.797-10.338-4.332-14.412\" id=\"path381\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M371.189,317.08  c4.531-3.459,6.57-8.688,5.777-14.412\" id=\"path383\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M388.521,301.229  c1.539,6.604-0.967,12.834-5.779,17.291\" id=\"path385\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M394.296,301.229  c-1.324-1.182-2.293-2.639-2.889-4.326\" id=\"path387\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M280.205,269.521  c0.205-1.287-0.318-2.223-1.445-2.883\" id=\"path389\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M275.873,263.756  c0.203-1.285-0.32-2.223-1.445-2.883\" id=\"path391\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M290.314,262.316  c0.666-2.697,0.24-5.297-1.443-7.207\" id=\"path393\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M350.97,168.64  c4.482,7.73,4.932,15.875,2.889,24.5\" id=\"path395\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M358.191,198.903  c2.256-6.391,2.023-13.704,0-20.175\" id=\"path397\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M366.857,161.435  c0.008-5.539-1.691-10.943-4.334-15.854\" id=\"path399\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M352.416,139.816  c4.254,3.277,7.186,7.863,8.664,12.971\" id=\"path401\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M361.08,164.317  c-2.086-6.078-5.609-11.279-10.109-15.854\" id=\"path403\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M313.421,145.581  c-2.705,5.357,1.299,10.605,7.221,11.529\" id=\"path405\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M307.646,134.052  c-5.668,6.237-1.949,14.486,2.887,20.177\" id=\"path407\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M320.642,123.964  c-11.648,2.481-18.086-1.289-25.996-10.088\" id=\"path409\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M285.982,108.111  c7.777,10.468,14.629-0.902,23.107,1.44c10.621,2.934,10.236,16.467,25.994,10.089\" id=\"path411\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M310.533,99.465  c-0.695,3.422-1.143,6.536-1.443,10.086\" id=\"path413\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <line id=\"line415\" style=\"fill:none;stroke:#C0DCC0;stroke-width:0;stroke-linecap:square;stroke-miterlimit:10;\" x1=\"290.314\" x2=\"284.539\" y1=\"105.228\" y2=\"105.228\"/>
  <line id=\"line417\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\" x1=\"281.648\" x2=\"274.427\" y1=\"105.228\" y2=\"103.789\"/>
  <line id=\"line419\" style=\"fill:none;stroke:#C0DCC0;stroke-width:0;stroke-linecap:square;stroke-miterlimit:10;\" x1=\"271.541\" x2=\"272.984\" y1=\"99.465\" y2=\"99.465\"/>
  <line id=\"line421\" style=\"fill:none;stroke:#C0DCC0;stroke-width:0;stroke-linecap:square;stroke-miterlimit:10;\" x1=\"284.539\" x2=\"285.982\" y1=\"99.465\" y2=\"99.465\"/>
  <line id=\"line423\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\" x1=\"287.427\" x2=\"296.091\" y1=\"99.465\" y2=\"102.347\"/>
  <line id=\"line425\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\" x1=\"283.093\" x2=\"293.203\" y1=\"95.14\" y2=\"96.582\"/>
  <path d=\"M330.751,113.876  c8.668,5.537,16.084,11.624,24.551,17.294\" id=\"path427\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M375.521,136.933  c-16.697-12.502-30.346-20.198-49.102-28.822\" id=\"path429\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M353.859,115.318  c-8.799-3.83-14.682-9.507-24.551-10.09\" id=\"path431\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M281.648,45.142c-7.252-11.561-2.889-25.994-2.889-38.911  c8.775,1.112,27.119,25.041,27.441,30.265c-1.682-1.678-3.961-2.788-5.777-4.325c1.045,2.074,1.691,3.751,2.889,5.765  c-1.867-0.239-2.951-1.17-4.334-1.439c0.846,1.756,1.75,2.842,2.889,4.323c-0.973,0.093-3.512-0.489-4.332,0  c0.182,0.96,2.703,1.333,1.443,2.881c-2.314,0.295-1.254-1.135-2.887-1.441c-0.383,1.236,0.125,3.028,0,4.323  c-0.482,0.076-2.438-2.53-2.889-2.881c0,1.442,0,2.883,0,4.325c-1.529-1.341-2.846-2.938-4.332-4.325  c0.309,1.707,0.111,4.03,0,5.765c-1.158-1.966-2.867-3.646-4.332-5.765c0.391,2.177,0.861,3.638,1.443,5.765  c-0.734-1.543-2.621-5.497-3.611-6.485C280.33,40.941,282.486,44.526,281.648,45.142 M372.634,50.906  c-1.271-15.737,21.039-27.823,33.215-33.147c1.145,14.46-3.787,34.945-17.328,41.795c1.035-2.208,0.861-3.592,1.443-5.767  c-2.939,4.398-3.6,4.551-8.666,5.767c1.082,0.434,1.434-2.698,1.443-2.883c-1.773,0.794-2.813,1.872-4.332,2.883  c0.539-2.219,0.43-3.541,1.443-5.767c-0.914,0.792-1.814,2.341-2.887,2.883c0.986-3.485,1.234-6.528,2.887-10.09  c-1.803,2.017-4.074,3.667-5.775,5.767c1.051-2.066,1.684-4.888,2.889-7.206C374.673,46.766,373.789,48.943,372.634,50.906\" id=\"path433\" style=\"fill-rule:evenodd;clip-rule:evenodd;fill:#996666;stroke:#000000;stroke-opacity:1.0000000;stroke-width:0.40500002;stroke-miterlimit:9.8212767;stroke-dasharray:none\"/>
  <path d=\"M281.648,45.142  c-7.252-11.561-2.889-25.994-2.889-38.911c8.775,1.112,27.119,25.041,27.441,30.265c-1.682-1.678-3.961-2.788-5.777-4.325  c1.045,2.074,1.691,3.751,2.889,5.765c-1.867-0.239-2.951-1.17-4.334-1.439c0.846,1.756,1.75,2.842,2.889,4.323  c-0.973,0.093-3.512-0.489-4.332,0c0.182,0.96,2.703,1.333,1.443,2.881c-2.314,0.295-1.254-1.135-2.887-1.441  c-0.383,1.236,0.125,3.028,0,4.323c-0.482,0.076-2.438-2.53-2.889-2.881c0,1.442,0,2.883,0,4.325  c-1.529-1.341-2.846-2.938-4.332-4.325c0.309,1.707,0.111,4.03,0,5.765c-1.158-1.966-2.867-3.646-4.332-5.765  c0.391,2.177,0.861,3.638,1.443,5.765c-0.734-1.543-2.621-5.497-3.611-6.485C280.33,40.941,282.486,44.526,281.648,45.142\" id=\"path435\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M372.634,50.906  c-1.271-15.737,21.039-27.823,33.215-33.147c1.145,14.46-3.787,34.945-17.328,41.795c1.035-2.208,0.861-3.592,1.443-5.767  c-2.939,4.398-3.6,4.551-8.666,5.767c1.082,0.434,1.434-2.698,1.443-2.883c-1.773,0.794-2.813,1.872-4.332,2.883  c0.539-2.219,0.43-3.541,1.443-5.767c-0.914,0.792-1.814,2.341-2.887,2.883c0.986-3.485,1.234-6.528,2.887-10.09  c-1.803,2.017-4.074,3.667-5.775,5.767c1.051-2.066,1.684-4.888,2.889-7.206C374.673,46.766,373.789,48.943,372.634,50.906\" id=\"path437\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M280.205,269.521c3.287-3.059,9.918-6.223,12.998-1.441  C288.982,269.277,284.671,269.758,280.205,269.521 M275.873,263.756c0.711-4.133,12.977-7.914,14.441-2.883  C292.255,267.551,277.203,265.098,275.873,263.756 M307.646,103.789c-0.074-11.354,11.523-14.257,12.996-2.883  c-2.266-1.102-4.084-0.633-5.777-1.44c-0.932,4.81-2.109,4.152-5.775,7.205C309.337,105.097,308.693,104.738,307.646,103.789\" id=\"path439\" style=\"fill-rule:evenodd;clip-rule:evenodd;fill:#F5A9A9;stroke:#000000;stroke-opacity:1.0000000;stroke-width:0.40500002;stroke-miterlimit:9.8212767;stroke-dasharray:none\"/>
  <path d=\"M280.205,269.521  c3.287-3.059,9.918-6.223,12.998-1.441C288.982,269.277,284.671,269.758,280.205,269.521\" id=\"path441\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M275.873,263.756  c0.711-4.133,12.977-7.914,14.441-2.883C292.255,267.551,277.203,265.098,275.873,263.756\" id=\"path443\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M307.646,103.789  c-0.074-11.354,11.523-14.257,12.996-2.883c-2.266-1.102-4.084-0.633-5.777-1.44c-0.932,4.81-2.109,4.152-5.775,7.205  C309.337,105.097,308.693,104.738,307.646,103.789\" id=\"path445\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M309.089,106.67  c1.4-2.192,1.873-4.609,1.443-7.205\" id=\"path447\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M303.312,81.169c-7.818,1.183-16.461-9.823-8.666-12.969  C302.191,65.156,312.136,77.066,303.312,81.169 M297.535,76.846c0.197-3.057-0.502-4.838,1.443-7.206  C299.671,72.78,298.035,74.538,297.535,76.846 M332.197,88.375c-4.904-12.153,7.139-10.087,14.441-10.087  C354.007,78.288,344,92.863,332.197,88.375 M339.416,85.492c0.588-3.017-1.111-4.572,1.445-5.763  C341.539,82.514,339.794,83.548,339.416,85.492\" id=\"path449\" style=\"fill-rule:evenodd;clip-rule:evenodd;fill:#FFBF00;stroke:#000000;stroke-opacity:1.0000000;stroke-width:0.40500002;stroke-miterlimit:9.8212767;stroke-dasharray:none\"/>
  <path d=\"M303.312,81.169  c-7.818,1.183-16.461-9.823-8.666-12.969C302.191,65.156,312.136,77.066,303.312,81.169\" id=\"path451\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M297.535,76.846  c0.197-3.057-0.502-4.838,1.443-7.206C299.671,72.78,298.035,74.538,297.535,76.846\" id=\"path453\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M332.197,88.375  c-4.904-12.153,7.139-10.087,14.441-10.087C354.007,78.288,344,92.863,332.197,88.375\" id=\"path455\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M339.416,85.492  c0.588-3.017-1.111-4.572,1.445-5.763C341.539,82.514,339.794,83.548,339.416,85.492\" id=\"path457\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M104.013,389.137c-20.697-30.977-49.927-54.453-65.578-89.594  c-13.175-29.584-23.795-69.5-14.548-101.812c9.784-34.188,30.214-74.328,67.129-85.296c31.722-9.426,71.935-4.126,89.54,25.942  c0.966-10.968-24.249-46.42-23.83-46.838c12.695-12.673,32.558,4.755,45.492,7.927c-1-5.271-4.453-9.584-8.665-12.973  c0.481-0.48,0.964-0.959,1.445-1.439c7.38,4.97,21.14,18.894,28.884,20.175c-1.533-2.154-2.539-6.703-4.334-8.646  c14.563,3.517,26.057,23.419,38.992,31.705c5.58,3.575,13.557,3.731,18.775,7.206c6.988,4.651,5.777,4.647,5.777,14.413  c0.48-0.481,0.963-0.962,1.445-1.443c4.297,4.72,10.992,7.1,12.996,14.413c0.482-0.48,0.963-0.96,1.443-1.44  c-2.57,3.951-3.525,8.28-2.887,12.969c1.826,1.057,1.705-1.161,2.887-1.441c0,1.441,0,2.883,0,4.325  c2.164-4.041,4.443-4.438,7.223-7.206c-0.389,7.578,9.787,13.191,14.441,12.969c-0.322,10.347,17.023,15.244,21.662,24.5  c0-0.961,0-1.921,0-2.881c8.779,3.038,8.93,10.895,7.223,18.735c-0.662-1.855-1.91-2.801-2.889-4.324  c1.652,7.395-2.553,19.559-10.109,25.939c1.063-1.861,1.885-3.867,2.887-5.762c-6.842,7.744-15.408,14.148-25.994,14.41  c0.48-0.48,0.963-0.961,1.443-1.441c-16.59,5.941-30.449-5.877-41.158-16.572c-10.383-10.369-22.277-9.141-14.443,6.484  c10.262,20.467,28.504,49.35,44.771,65.572c5.846,5.83,11.742,13.154,18.051,19.457c4.473,4.467,15.031,19.725,0,12.971  c0.963,0,1.928,0,2.891,0c-6.492-1.002-10.611-5.055-14.443-4.324c-2.906-1.713-7.533-1.363-10.109-7.205  c-14.162,7-52.912-50.941-63.545-60.529c-19.793-17.854-45.696-42.902-15.939-65.173c5.971-4.469,24.588-37.176,18.105-43.635  c-8.119-8.09-19.406,7.293-25.273,15.132c-4.248,5.678-20.735,36.586-27.44,31.705c-5.294-3.853,0.902-41.574-4.332-50.441  c0,27.479-8.84,46.437-15.887,72.06c-0.48-0.961-0.963-1.922-1.443-2.883c-4.276,8.953-8.999,16.02-17.33,21.617  c0.481-0.961,0.963-1.922,1.443-2.883c-8.159,13.217-42.531,20.773-38.993,36.029c2.184,9.414,20.659,17.832,26.718,25.221  c8.919,10.877,17.162,21.258,29.605,28.102c-4.968-1.109-10.023-1.896-14.44,0c0.481,0.48,0.962,0.961,1.443,1.441  c-4.251,1.703-12.979,0.004-12.997,0c-3.1,2.074-3.914,0.293-5.776,4.322c-18.988-12.41-55.826-24.469-64.268-49.719  c-6.073-18.168,7.05-23.428,14.441-38.191c4.296-8.58-2.985-13.492-3.61-15.852c-1.783-6.742-2.148-14.305,0-21.617  c-13.824,12.82-20.617-12.05-27.438-4.324c-6.781,7.682,2.343,51.064,4.332,61.971c5.025,15.295,17.957,39.473,28.883,57.646  c0.481-0.961,0.963-1.922,1.443-2.883c9.4,15.207,24.34,25.725,36.827,38.189c6.713,6.701,33.718,19.457,43.325,19.457  c19.324,0,38.836,4.426,56.325-7.207c4.551-3.027,40.754-18.236,29.605,0c0.482-0.48,0.965-0.961,1.445-1.441  c2.934,20.422-40.883,29.824-56.323,33.146C165.235,415.209,134.462,402.383,104.013,389.137 M361.08,286.814  c-7.42-9.871-23.391-30.283-10.109-41.795c0.451,2.061,3.363,5.621,4.332,8.648c3.141-5.613,0.535-12.127-0.373-18.469  c-1.424-9.936,5.611-11.535,6.15-20.443c0.607,0.385-0.16,2.41,1.443,0c5.939,16.512,7.223,28.297,7.223,45.398  c0,11.793,15.645,20.857,17.33,32.426C376.189,282.721,370.246,295.67,361.08,286.814 M275.873,49.025  c-2.27-8.167-6.375-47.542,5.775-49.001c8.676-1.042,30.91,32.254,38.994,36.028c-7.285,0.086-18.215,9.945-21.664,1.444  c1.693,0.33,1.891,1.708,4.334,1.439c-1.197-2.013-1.844-3.691-2.889-5.765c1.816,1.538,4.096,2.647,5.777,4.325  c-0.322-5.224-18.666-29.153-27.441-30.265c0,12.827-4.248,27.521,2.889,38.911c0-0.961,0-1.922,0-2.883  c1.516,2.104,3.066,4.787,4.334,7.206c-1.646-1.02-2.83-1.625-4.334-2.883c0.482,1.442,0.963,2.883,1.445,4.325  c-1.703-2.324-2.836-2.583-4.334-5.765c0.242,2.371-0.33,4.849,0,7.206C277.308,52.054,276.998,50.513,275.873,49.025   M352.416,109.551c-5.715-3.215-5.756-8.625-9.389-12.249c-3.365-3.358-7.824-1.102-13.719-3.604c0.48,0.48,0.961,0.961,1.443,1.441  c-4.557-1.86-6.23-5.712-4.332-10.087c0,0.961,0,1.922,0,2.883c0.871-3.136,2.223-5.287,2.889-8.648  c0.48,0.48,0.961,0.961,1.443,1.441c1.379-1.739,1.953-4.171,2.889-5.767c-0.482,0.481-0.963,0.962-1.443,1.444  c-0.143-5.372-0.865-9.208-1.445-14.413c-0.482,0.48-0.963,0.961-1.443,1.442c-0.063-5.777,1.191-11.14,4.332-15.854  c-5.197,2.485-9.895,10.621-10.111,11.53c0.367,5.384,0.232,10.763-1.443,15.852c-0.48-0.48-0.961-0.959-1.443-1.439  c-1.58,5.767-5.211,10.298-7.221,15.852c0.963-0.961,1.926-1.922,2.889-2.883c-2.684,6.804-8.719,8.961-8.664,17.296  c-8.455-9.334,2.752-19.925,7.941-30.266c6.672-13.292-1.541-22.036,12.998-31.705c10.975-7.3,27.367,5.111,35.383-2.882  c5.424-5.408,16.072-10.9,23.107-14.412c3.174-1.584,15.57-12.962,17.33-12.971c19.578-0.093-10.779,63.967-13.721,72.777  c-3.621,10.848-9.053,31.637-22.383,35.31C361.046,121.64,349.011,109.788,352.416,109.551 M350.97,82.169  c-3.252,10.489-14.063,9.447-21.662,5.767C332.437,78.766,343.328,73.297,350.97,82.169 M378.41,60.554  c0.537-2.208-0.035-3.669,1.443-5.767c-0.957,0.794-2.137,1.893-2.887,2.883c1.023-3.235,1.365-6.649,2.887-10.09  c-1.803,2.016-4.08,3.664-5.775,5.767c1.051-2.066,1.684-4.888,2.889-7.206c-1.465,2.119-3.176,3.797-4.332,5.765  c-1.271-15.737,21.039-27.823,33.215-33.147c1.141,14.406-4.023,34.972-17.328,41.795c0.836-2.15,0.883-3.668,1.443-5.767  c-3.125,4.186-4.086,4.197-8.666,5.767c1.096-1.437,1.293-0.906,1.443-2.883C381.458,59.106,380.042,59.598,378.41,60.554\" id=\"path459\" style=\"\"\"fill-rule:evenodd;clip-rule:evenodd;fill:@catcolor;stroke:#000000;stroke-opacity:1.0000000;stroke-width:0.40500002;stroke-miterlimit:9.8212767;stroke-dasharray:none\"\"\"/>
  <path d=\"M104.013,389.137  c-20.697-30.977-49.927-54.453-65.578-89.594c-13.175-29.584-23.795-69.5-14.548-101.812c9.784-34.188,30.214-74.328,67.129-85.296  c31.722-9.426,71.935-4.126,89.54,25.942c0.966-10.968-24.249-46.42-23.83-46.838c12.695-12.673,32.558,4.755,45.492,7.927  c-1-5.271-4.453-9.584-8.665-12.973c0.481-0.48,0.964-0.959,1.445-1.439c7.38,4.97,21.14,18.894,28.884,20.175  c-1.533-2.154-2.539-6.703-4.334-8.646c14.563,3.517,26.057,23.419,38.992,31.705c5.58,3.575,13.557,3.731,18.775,7.206  c6.988,4.651,5.777,4.647,5.777,14.413c0.48-0.481,0.963-0.962,1.445-1.443c4.297,4.72,10.992,7.1,12.996,14.413  c0.482-0.48,0.963-0.96,1.443-1.44c-2.57,3.951-3.525,8.28-2.887,12.969c1.826,1.057,1.705-1.161,2.887-1.441  c0,1.441,0,2.883,0,4.325c2.164-4.041,4.443-4.438,7.223-7.206c-0.389,7.578,9.787,13.191,14.441,12.969  c-0.322,10.347,17.023,15.244,21.662,24.5c0-0.961,0-1.921,0-2.881c8.779,3.038,8.93,10.895,7.223,18.735  c-0.662-1.855-1.91-2.801-2.889-4.324c1.652,7.395-2.553,19.559-10.109,25.939c1.063-1.861,1.885-3.867,2.887-5.762  c-6.842,7.744-15.408,14.148-25.994,14.41c0.48-0.48,0.963-0.961,1.443-1.441c-16.59,5.941-30.449-5.877-41.158-16.572  c-10.383-10.369-22.277-9.141-14.443,6.484c10.262,20.467,28.504,49.35,44.771,65.572c5.846,5.83,11.742,13.154,18.051,19.457  c4.473,4.467,15.031,19.725,0,12.971c0.963,0,1.928,0,2.891,0c-6.492-1.002-10.611-5.055-14.443-4.324  c-2.906-1.713-7.533-1.363-10.109-7.205c-14.162,7-52.912-50.941-63.545-60.529c-19.793-17.854-45.696-42.902-15.939-65.173  c5.971-4.469,24.588-37.176,18.105-43.635c-8.119-8.09-19.406,7.293-25.273,15.132c-4.248,5.678-20.735,36.586-27.44,31.705  c-5.294-3.853,0.902-41.574-4.332-50.441c0,27.479-8.84,46.437-15.887,72.06c-0.48-0.961-0.963-1.922-1.443-2.883  c-4.276,8.953-8.999,16.02-17.33,21.617c0.481-0.961,0.963-1.922,1.443-2.883c-8.159,13.217-42.531,20.773-38.993,36.029  c2.184,9.414,20.659,17.832,26.718,25.221c8.919,10.877,17.162,21.258,29.605,28.102c-4.968-1.109-10.023-1.896-14.44,0  c0.481,0.48,0.962,0.961,1.443,1.441c-4.251,1.703-12.979,0.004-12.997,0c-3.1,2.074-3.914,0.293-5.776,4.322  c-18.988-12.41-55.826-24.469-64.268-49.719c-6.073-18.168,7.05-23.428,14.441-38.191c4.296-8.58-2.985-13.492-3.61-15.852  c-1.783-6.742-2.148-14.305,0-21.617c-13.824,12.82-20.617-12.05-27.438-4.324c-6.781,7.682,2.343,51.064,4.332,61.971  c5.025,15.295,17.957,39.473,28.883,57.646c0.481-0.961,0.963-1.922,1.443-2.883c9.4,15.207,24.34,25.725,36.827,38.189  c6.713,6.701,33.718,19.457,43.325,19.457c19.324,0,38.836,4.426,56.325-7.207c4.551-3.027,40.754-18.236,29.605,0  c0.482-0.48,0.965-0.961,1.445-1.441c2.934,20.422-40.883,29.824-56.323,33.146C165.235,415.209,134.462,402.383,104.013,389.137\" id=\"path461\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M361.08,286.814  c-7.42-9.871-23.391-30.283-10.109-41.795c0.451,2.061,3.363,5.621,4.332,8.648c3.141-5.613,0.535-12.127-0.373-18.469  c-1.424-9.936,5.611-11.535,6.15-20.443c0.607,0.385-0.16,2.41,1.443,0c5.939,16.512,7.223,28.297,7.223,45.398  c0,11.793,15.645,20.857,17.33,32.426C376.189,282.721,370.246,295.67,361.08,286.814\" id=\"path463\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M275.873,49.025  c-2.27-8.167-6.375-47.542,5.775-49.001c8.676-1.042,30.91,32.254,38.994,36.028c-7.285,0.086-18.215,9.945-21.664,1.444  c1.693,0.33,1.891,1.708,4.334,1.439c-1.197-2.013-1.844-3.691-2.889-5.765c1.816,1.538,4.096,2.647,5.777,4.325  c-0.322-5.224-18.666-29.153-27.441-30.265c0,12.827-4.248,27.521,2.889,38.911c0-0.961,0-1.922,0-2.883  c1.516,2.104,3.066,4.787,4.334,7.206c-1.646-1.02-2.83-1.625-4.334-2.883c0.482,1.442,0.963,2.883,1.445,4.325  c-1.703-2.324-2.836-2.583-4.334-5.765c0.242,2.371-0.33,4.849,0,7.206C277.308,52.054,276.998,50.513,275.873,49.025\" id=\"path465\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M352.416,109.551  c-5.715-3.215-5.756-8.625-9.389-12.249c-3.365-3.358-7.824-1.102-13.719-3.604c0.48,0.48,0.961,0.961,1.443,1.441  c-4.557-1.86-6.23-5.712-4.332-10.087c0,0.961,0,1.922,0,2.883c0.871-3.136,2.223-5.287,2.889-8.648  c0.48,0.48,0.961,0.961,1.443,1.441c1.379-1.739,1.953-4.171,2.889-5.767c-0.482,0.481-0.963,0.962-1.443,1.444  c-0.143-5.372-0.865-9.208-1.445-14.413c-0.482,0.48-0.963,0.961-1.443,1.442c-0.063-5.777,1.191-11.14,4.332-15.854  c-5.197,2.485-9.895,10.621-10.111,11.53c0.367,5.384,0.232,10.763-1.443,15.852c-0.48-0.48-0.961-0.959-1.443-1.439  c-1.58,5.767-5.211,10.298-7.221,15.852c0.963-0.961,1.926-1.922,2.889-2.883c-2.684,6.804-8.719,8.961-8.664,17.296  c-8.455-9.334,2.752-19.925,7.941-30.266c6.672-13.292-1.541-22.036,12.998-31.705c10.975-7.3,27.367,5.111,35.383-2.882  c5.424-5.408,16.072-10.9,23.107-14.412c3.174-1.584,15.57-12.962,17.33-12.971c19.578-0.093-10.779,63.967-13.721,72.777  c-3.621,10.848-9.053,31.637-22.383,35.31C361.046,121.64,349.011,109.788,352.416,109.551\" id=\"path467\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M350.97,82.169  c-3.252,10.489-14.063,9.447-21.662,5.767C332.437,78.766,343.328,73.297,350.97,82.169\" id=\"path469\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M378.41,60.554  c0.537-2.208-0.035-3.669,1.443-5.767c-0.957,0.794-2.137,1.893-2.887,2.883c1.023-3.235,1.365-6.649,2.887-10.09  c-1.803,2.016-4.08,3.664-5.775,5.767c1.051-2.066,1.684-4.888,2.889-7.206c-1.465,2.119-3.176,3.797-4.332,5.765  c-1.271-15.737,21.039-27.823,33.215-33.147c1.141,14.406-4.023,34.972-17.328,41.795c0.836-2.15,0.883-3.668,1.443-5.767  c-3.125,4.186-4.086,4.197-8.666,5.767c1.096-1.437,1.293-0.906,1.443-2.883C381.458,59.106,380.042,59.598,378.41,60.554\" id=\"path471\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M164.669,363.195c-3.574-7.783-9.653-12.668-16.607-17.293  c-5.268-3.502-20.823-8.824-7.943-11.529c0.018,0.004,8.746,1.703,12.997,0c-0.481-0.48-0.962-0.961-1.443-1.441  c15.106-6.758,43.845,16.352,50.546,30.264C190.052,356.566,173.058,382.291,164.669,363.195 M307.646,363.195  c6.59-11.932-8.697-25.297-14.443-34.586c6.186-0.91,9.859,4.285,14.443,4.322c4.078,2.895,9.93,6.064,17.33,7.207  c-0.963,0-1.928,0-2.891,0c5.932,2.666,9.695,0.701,8.666-5.766c13.928,4.447,17.998,20.957,7.943,30.984  C329.578,374.449,315.367,360.836,307.646,363.195 M277.316,260.873c2.182-12.307,22.211-5.705,15.887,5.766  c-2.186-0.545-6.256-1.137-10.109-1.443c4.131-0.508,4.648-2.668,7.221-4.322C286.212,258.984,281.404,258.797,277.316,260.873   M359.634,309.873c11.422,6.455,4.656-17.254,1.445-23.059c8.74,8.443,15.104-2.902,25.996,5.766c0-0.961,0-1.922,0-2.883  C415.109,310.838,367.927,332.895,359.634,309.873 M283.093,136.933c6.563,0.428,15.43-6.211,20.941-0.72  c4.029,4.014,19.549-6.917,25.273-6.485c-2.066-0.2-22.518-5.065-23.107-5.765c-4.752-5.627-3.172-11.445,4.332-11.529  c-4.293-1.356-10.895-9.116-7.221-15.853c1.508,2.113,4.139,6.667,5.777,10.088c4.465-3.056,6.537-4.926,11.553-5.765  c-0.578-4.471-3.82-7.451-8.664-8.646c2.275-1.831,2.18-3.257,4.332-5.767c-0.963,0.961-1.926,1.922-2.889,2.883  c2.633-5.253,5.361-10.285,7.221-15.852c0.482,0.48,0.963,0.959,1.443,1.439c1.676-5.089,1.811-10.468,1.443-15.852  c0.217-0.909,4.914-9.045,10.111-11.53c-3.141,4.714-4.395,10.077-4.332,15.854c0.48-0.48,0.961-0.961,1.443-1.442  c0.58,5.205,1.303,9.041,1.445,14.413c0.48-0.481,0.961-0.962,1.443-1.444c-0.936,1.595-1.51,4.028-2.889,5.767  c-0.482-0.48-0.963-0.961-1.443-1.441c-0.666,3.361-2.018,5.512-2.889,8.648c0-0.961,0-1.922,0-2.883  c-1.898,4.375-0.225,8.228,4.332,10.087c-0.482-0.48-0.963-0.961-1.443-1.441c5.895,2.501,10.354,0.246,13.719,3.604  c3.633,3.624,3.674,9.034,9.389,12.249c-0.963,0-1.926,0-2.889,0c14.148,7.968,13.217,15.72,28.883,1.443  c-9.871,12.066,0.242,33.55,4.332,46.116c-0.809-1.985-2.076-3.782-2.889-5.765c0.094,5.069,0.547,9.554,1.445,14.413  c-0.48-0.961-0.963-1.922-1.445-2.883c1.225,3.23,1.74,6.798,2.889,10.088c-0.939-0.981-1.928-1.92-2.889-2.881  c0.682,3.778,0.523,7.807,1.445,11.529c-0.48-0.961-0.963-1.922-1.445-2.883c1.273,9.799-6.221,26.863-12.996,28.823  c-2.16,3.76-5.563,7.109-5.777,7.205c-1.229,5.543-6.076,11.639-8.664,11.529c-2.127,4.127-4.475,7.873-7.221,11.529  c0-0.961,0-1.922,0-2.881c-0.963,0.959-1.928,1.92-2.891,2.881c2.236-8.699,6.018-10.041,4.334-18.734  c1.061,1.904,2.205,2.254,2.889,4.324c1.662-7.635,1.496-16.045-7.223-18.735c0,0.96,0,1.92,0,2.881  c-4.574-9.13-21.973-14.474-21.662-24.5c-4.654,0.222-14.83-5.391-14.441-12.969c-2.779,2.768-5.059,3.166-7.223,7.206  c0-1.441,0-2.883,0-4.325c-1.182,0.28-1.061,2.499-2.887,1.441c-0.639-4.689,0.316-9.018,2.887-12.969  c-0.48,0.48-0.961,0.96-1.443,1.44c-1.832-6.687-10.396-13.667-14.441-12.969C283.093,145.582,283.093,141.257,283.093,136.933\" id=\"path473\" style=\"fill-rule:evenodd;clip-rule:evenodd;fill:#FFFFEB;stroke:#000000;stroke-opacity:1.0000000;stroke-width:0.40500002;stroke-miterlimit:9.8212767;stroke-dasharray:none\"/>
  <path d=\"M164.669,363.195  c-3.574-7.783-9.653-12.668-16.607-17.293c-5.268-3.502-20.823-8.824-7.943-11.529c0.018,0.004,8.746,1.703,12.997,0  c-0.481-0.48-0.962-0.961-1.443-1.441c15.106-6.758,43.845,16.352,50.546,30.264C190.052,356.566,173.058,382.291,164.669,363.195\" id=\"path475\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M307.646,363.195  c6.59-11.932-8.697-25.297-14.443-34.586c6.186-0.91,9.859,4.285,14.443,4.322c4.078,2.895,9.93,6.064,17.33,7.207  c-0.963,0-1.928,0-2.891,0c5.932,2.666,9.695,0.701,8.666-5.766c13.928,4.447,17.998,20.957,7.943,30.984  C329.578,374.449,315.367,360.836,307.646,363.195\" id=\"path477\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M277.316,260.873  c2.182-12.307,22.211-5.705,15.887,5.766c-2.186-0.545-6.256-1.137-10.109-1.443c4.131-0.508,4.648-2.668,7.221-4.322  C286.212,258.984,281.404,258.797,277.316,260.873\" id=\"path479\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M359.634,309.873  c11.422,6.455,4.656-17.254,1.445-23.059c8.74,8.443,15.104-2.902,25.996,5.766c0-0.961,0-1.922,0-2.883  C415.109,310.838,367.927,332.895,359.634,309.873\" id=\"path481\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
  <path d=\"M283.093,136.933  c6.563,0.428,15.43-6.211,20.941-0.72c4.029,4.014,19.549-6.917,25.273-6.485c-2.066-0.2-22.518-5.065-23.107-5.765  c-4.752-5.627-3.172-11.445,4.332-11.529c-4.293-1.356-10.895-9.116-7.221-15.853c1.508,2.113,4.139,6.667,5.777,10.088  c4.465-3.056,6.537-4.926,11.553-5.765c-0.578-4.471-3.82-7.451-8.664-8.646c2.275-1.831,2.18-3.257,4.332-5.767  c-0.963,0.961-1.926,1.922-2.889,2.883c2.633-5.253,5.361-10.285,7.221-15.852c0.482,0.48,0.963,0.959,1.443,1.439  c1.676-5.089,1.811-10.468,1.443-15.852c0.217-0.909,4.914-9.045,10.111-11.53c-3.141,4.714-4.395,10.077-4.332,15.854  c0.48-0.48,0.961-0.961,1.443-1.442c0.58,5.205,1.303,9.041,1.445,14.413c0.48-0.481,0.961-0.962,1.443-1.444  c-0.936,1.595-1.51,4.028-2.889,5.767c-0.482-0.48-0.963-0.961-1.443-1.441c-0.666,3.361-2.018,5.512-2.889,8.648  c0-0.961,0-1.922,0-2.883c-1.898,4.375-0.225,8.228,4.332,10.087c-0.482-0.48-0.963-0.961-1.443-1.441  c5.895,2.501,10.354,0.246,13.719,3.604c3.633,3.624,3.674,9.034,9.389,12.249c-0.963,0-1.926,0-2.889,0  c14.148,7.968,13.217,15.72,28.883,1.443c-9.871,12.066,0.242,33.55,4.332,46.116c-0.809-1.985-2.076-3.782-2.889-5.765  c0.094,5.069,0.547,9.554,1.445,14.413c-0.48-0.961-0.963-1.922-1.445-2.883c1.225,3.23,1.74,6.798,2.889,10.088  c-0.939-0.981-1.928-1.92-2.889-2.881c0.682,3.778,0.523,7.807,1.445,11.529c-0.48-0.961-0.963-1.922-1.445-2.883  c1.273,9.799-6.221,26.863-12.996,28.823c-2.16,3.76-5.563,7.109-5.777,7.205c-1.229,5.543-6.076,11.639-8.664,11.529  c-2.127,4.127-4.475,7.873-7.221,11.529c0-0.961,0-1.922,0-2.881c-0.963,0.959-1.928,1.92-2.891,2.881  c2.236-8.699,6.018-10.041,4.334-18.734c1.061,1.904,2.205,2.254,2.889,4.324c1.662-7.635,1.496-16.045-7.223-18.735  c0,0.96,0,1.92,0,2.881c-4.574-9.13-21.973-14.474-21.662-24.5c-4.654,0.222-14.83-5.391-14.441-12.969  c-2.779,2.768-5.059,3.166-7.223,7.206c0-1.441,0-2.883,0-4.325c-1.182,0.28-1.061,2.499-2.887,1.441  c-0.639-4.689,0.316-9.018,2.887-12.969c-0.48,0.48-0.961,0.96-1.443,1.44c-1.832-6.687-10.396-13.667-14.441-12.969  C283.093,145.582,283.093,141.257,283.093,136.933\" id=\"path483\" style=\"fill:none;stroke:#000000;stroke-width:0.40500002;stroke-linecap:square;stroke-miterlimit:9.8212767;stroke-opacity:1.0000000;stroke-dasharray:none\"/>
</svg>

<svg>
<g>@[cat \"blue\" 0 0,
     cat \"red\" 100 300]</g></svg>
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

badPrelude =
 """[\"pre\", [], [[\"TEXT\", \"\"\"
  Oops! The prelude didn't parse... sorry!
  But, you can still write programs without it. You can do it!
  If you want to see where parsing failed, go to
  File -> New From Template and select \"Standard Prelude\".
  
  ERROR_HERE
  \"\"\"]]]

"""

blankSvg =
 """main =
  svg (concat [])

"""

blankDoc =
 """main =
  [ \"div\"
  , []
  , []
  ]

"""

welcome1 =
 """title = \"Sketch-n-Sketch Docs\"

evalupdate code leoLang =
  <div class=\"code\" style=\"\"\"background:@(if leoLang then \"aliceblue\" else \"white\");
       border:2px solid black;margin-bottom:2px;border-radius:8px;\"\"\">
    <textarea onkeyup=\"\"\"if(this.getAttribute('v')!=this.value)
       this.setAttribute('v', this.value)\"\"\" v=(code)
       style=\"margin:0px;width:296px;height:52px;margin:5px;\">@code</textarea>
    <pre style=\"vertical-align:top;margin-top:0px;\">@(
      let res = evaluate <| LensLess.appendStrDef + code in
      if leoLang then toString res else res
    )</pre></div>

main =
  <div style=\"padding:20px\">
    <h2>Welcome to @title!</h2>
      <p><b>What's this?</b>
      @title provides a reversible general-purpose language.
         You can edit both the program (on the left/top) or the resulting value
         (on the right/bottom). For example:</p>
      @(evalupdate \"let x = \\\"Hello \\\" in\\nx + \\\"\\\"\\\"and @(x)world\\\"\\\"\\\"\" True)
      @(evalupdate \"\"\"let x = \"blue\" in
<h3 style=\"color:\"+x>This is @@x, change me!</h3>\"\"\" False)
      <p>Turn auto-sync on (on the right) to see the changes be applied
      immediately if they are not ambiguous !</p>
      <p>See some more examples from File -&gt; New From Template in
        the menu bar, or by pressing the Previous and Next
        buttons in the top-right corner.</p>
    @(Html.observeCopyValueToAttribute \".code > textarea\" \"v\")</div>
"""

tableOfStatesA =
 """states =
  [ [\"Alabama\", \"AL?\", \"\"]
  , [\"Alaska\", \"AL?\", \"\"]
  , [\"Arizona\", \"AR?\", \"\"]
  , [\"Arkansas\", \"AR?\", \"\"]
  , [\"California\", \"CA\", \"\"]
  , [\"Colorado\", \"CO?\", \"\"]
  , [\"Connecticut\", \"CO?\", \"\"] ]

main =
  let headers = [\"State\", \"Capital\"] in
  let rows =
    List.map
      (\\[state, abbrev, cap] -> [state, cap + \", \" + abbrev])
      states
  in
  let padding = [\"padding\", \"3px\"] in
  let headerRow =
    let styles = [padding] in
    Html.tr [] [] (List.map (Html.th styles []) headers)
  in
  let stateRows =
    let colors = [\"lightgray\", \"white\"] in
    let drawRow i row =
      let color = List.nth colors (mod i (List.length colors)) in
      let columns =
        List.map
          (Html.td [padding, [\"background-color\", color]] [])
          row
      in
      Html.tr [] [] columns
    in
    List.indexedMap drawRow rows
  in
  Html.table [padding] [] (headerRow :: stateRows)

"""

tableOfStatesB =
 """---------------------------------------------------------------------
-- Like the previous example, but with the TableWithButtons
-- module included in this file.

TableWithButtons =
  let wrapData rows =
    let blankRow =
      let numColumns =
        case rows of
          []     -> 0
          row::_ -> List.length row
      in
      List.repeat numColumns \"?\"
    in
    Update.applyLens
      { apply rows =
          List.map (\\row -> (Update.freeze False, row)) rows

      , update {outputNew = flaggedRows} =
          let processRow (flag, row) =
            if flag == True
              then [ row, blankRow ]
              else [ row ]
          in
          Ok (Inputs [List.concatMap processRow flaggedRows])
      }
      rows
  in
  let mapData f flaggedRows =
    List.map (Tuple.mapSecond f) flaggedRows
  in
  --
  -- The globalBool flag is used to determine whether to insert \"\" or \" \"
  -- before a couple attribute values. Toggling this choice in between
  -- subsequent runs helps work around our issue forcing Elm to re-render.
  --
  let tr globalBool flag styles attrs children =
    let (hasBeenClicked, nope, yep) =
      (\"has-been-clicked\", Update.softFreeze \"gray\", Update.softFreeze \"coral\")
    in
    let dummyStrPrefix =
      Update.softFreeze <| if globalBool then \"\" else \" \"
    in
    let onclick =
      \"\"\"
      var hasBeenClicked = document.createAttribute(\"@hasBeenClicked\");
      var buttonStyle = document.createAttribute(\"style\");

      if (this.parentNode.getAttribute(\"@hasBeenClicked\").endsWith(\"False\")) {
        hasBeenClicked.value = \"@(dummyStrPrefix)True\";
        buttonStyle.value = \"color: @yep;\";
      } else {
        hasBeenClicked.value = \"@(dummyStrPrefix)False\";
        buttonStyle.value = \"color: @dummyStrPrefix@nope;\";
      }

      this.parentNode.setAttributeNode(hasBeenClicked);
      this.setAttributeNode(buttonStyle);
      \"\"\"
    in
    let button = -- text-button.enabled is an SnS class
      [ \"span\"
      , [ [\"class\", \"text-button.enabled\"]
        , [\"onclick\", onclick]
        , [\"style\", [[\"color\", dummyStrPrefix + nope]]]
        ]
      , [textNode \"+\"]
      ]
    in
    Html.tr styles
      ([hasBeenClicked, dummyStrPrefix + toString flag] :: attrs)
      (snoc button children)
  in
  { wrapData = wrapData
  , mapData = mapData
  , tr = tr
  }

TableWithButtons =
  -- Toggle the global boolean flag, to workaround the force re-render issue.
  { new _ =
      { TableWithButtons | tr = TableWithButtons.tr (toggleGlobalBool []) }
  }

---------------------------------------------------------------------

states =
  [ [\"Alabama\", \"AL\", \"Montgomery\"]
  , [\"Alaska\", \"AK\", \"Juneau\"]
  , [\"Arizona\", \"AZ\", \"Phoenix\"]
  , [\"Arkansas\", \"AR\", \"Little Rock\"]
  , [\"California\", \"CA\", \"Sacramento\"]
  , [\"Colorado\", \"CO\", \"Denver\"]
  , [\"Connecticut\", \"CT\", \"Hartford\"] ]

TableWithButtons = TableWithButtons.new []

main =
  let headers = [\"State\", \"Capital\"] in
  let rows =
    TableWithButtons.mapData
      (\\[state, abbrev, cap] -> [state, cap + \", \" + abbrev])
      (TableWithButtons.wrapData states)
  in
  let padding = [\"padding\", \"3px\"] in
  let headerRow =
    let styles = [padding, [\"background-color\", \"coral\"]] in
    Html.tr [] [] (List.map (Html.th styles []) headers)
  in
  let stateRows =
    let colors = [\"lightyellow\", \"white\"] in
    let drawRow i (flag,row) =
      let color = List.nth colors (mod i (List.length colors)) in
      let columns =
        List.map
          (Html.td [padding, [\"background-color\", color]] [])
          row
      in
      TableWithButtons.tr flag [] [] columns
    in
    List.indexedMap drawRow rows
  in
  Html.table [padding] [] (headerRow :: stateRows)

"""

tableOfStatesC =
 """states =
  [ [\"Alabama\", \"AL\", \"Montgomery\"]
  , [\"Alaska\", \"AK\", \"Juneau\"]
  , [\"Arizona\", \"AZ\", \"Phoenix\"]
  , [\"Arkansas\", \"AR\", \"Little Rock\"]
  , [\"California\", \"CA\", \"Sacramento\"]
  , [\"Colorado\", \"CO\", \"Denver\"]
  , [\"Connecticut\", \"CT\", \"Hartford\"] ]

TableWithButtons = TableWithButtons.new []

main =
  let headers = [\"State\", \"Capital\"] in
  let rows =
    TableWithButtons.mapData
      (\\[state, abbrev, cap] -> [state, cap + \", \" + abbrev])
      (TableWithButtons.wrapData states)
  in
  let padding = [\"padding\", \"3px\"] in
  let headerRow =
    let styles = [padding, [\"background-color\", \"coral\"]] in
    Html.tr [] [] (List.map (Html.th styles []) headers)
  in
  let stateRows =
    let colors = [\"lightyellow\", \"white\"] in
    let drawRow i (flag,row) =
      let color = List.nth colors (mod i (List.length colors)) in
      let columns =
        List.map
          (Html.td [padding, [\"background-color\", color]] [])
          row
      in
      TableWithButtons.tr flag [] [] columns
    in
    List.indexedMap drawRow rows
  in
  Html.table [padding] [] (headerRow :: stateRows)

"""

simpleBudget =
 """main =
  h1 [] [] \"Budget\"

"""

mapMaybeLens =
 """maybeMapSimple f mx =
  case mx of [] -> []; [x] -> [f x]

maybeMapLens default =
  { apply (f, mx) = maybeMapSimple f mx

  , update {input = (f, mx), outputNew = my} =
      case my of
        []  -> Ok (Inputs [(f, [])])
        [y] ->
          let z = case mx of [x] -> x; [] -> default in
          let results =
            Update.updateApp {fun (func, arg) = func arg, input = (f, z), output = y}
          in
          Ok (Inputs (List.map (\\((newF,newZ), diffs) -> (newF, [newZ])) results.args._1.args._1))
  }

maybeMap default f mx =
  Update.applyLens (maybeMapLens default) (f, mx)

maybeMapState =
  maybeMap (\"?\", \"?\", \"?\")

displayState =
  (\\(a,b,c) -> (a, c + \", \" + b))

maybeRowA = maybeMapSimple displayState [(\"New Jersey\", \"NJ\", \"Edison\")]
maybeRowB = maybeMapSimple displayState []

maybeRow1 = maybeMapState displayState [(\"New Jersey\", \"NJ\", \"Edison\")]
maybeRow2 = maybeMapState displayState []

showValues values =
  Html.div [] [] (List.map (\\x -> [\"h3\", [], Html.text <| toString x]) values)

main =
  showValues [maybeRowA, maybeRowB, maybeRow1, maybeRow2]

"""

mapListLens_1 =
 """listMapLens =
  { apply (f,xs) = List.simpleMap f xs

  , update { input = (f, oldInputList)
           , outputOld = oldOutputList
           , outputNew = newOutputList } =

      let walk diffOps maybePreviousInput oldInputs acc =

        case (diffOps, oldInputs) of
          ([], []) ->
            acc

          (KeepValue :: moreDiffOps, oldHead :: oldTail) ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            List.simpleMap (\\newTail -> oldHead::newTail) newTails

          (DeleteValue :: moreDiffOps, oldHead :: oldTail) ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            newTails

          ((UpdateValue newVal) :: moreDiffOps, oldHead :: oldTail) ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            let newHeads = Update.updateApp {fun = f, input = oldHead, output = newVal} in
            List.cartesianProductWith List.cons newHeads.values newTails

          ((InsertValue newVal) :: moreDiffOps, _) ->
            let headOrPreviousHead =
              case (oldInputs, maybePreviousInput) of
                (oldHead :: _, _) -> oldHead
                ([], Just previousOldHead) -> previousOldHead
            in
            let newTails = walk moreDiffOps maybePreviousInput oldInputs acc in
            let newHeads = Update.updateApp {fun = f, input = headOrPreviousHead, output = newVal} in
            List.cartesianProductWith List.cons newHeads.values newTails
      in
      let newInputLists =
        walk (Update.listDiff oldOutputList newOutputList) Nothing oldInputList [[]]
      in
      let newFuncAndInputLists =
        List.simpleMap (\\newInputList -> (f, newInputList)) newInputLists
      in
      Ok (Inputs newFuncAndInputLists)
  }

listMap f xs =
  Update.applyLens listMapLens (f, xs)

-----------------------------------------------

transformedValues =
  listMap
    (\\n -> n + 1)
    [0,1,2,3]

main =
  Html.p [] [] (toString transformedValues)

"""

mapListLens_2 =
 """listMapLens =
  { apply (f,xs) = List.simpleMap f xs

  , update { input = (f, oldInputList)
           , outputOld = oldOutputList
           , outputNew = newOutputList } =
      let walk diffOps maybePreviousInput oldInputs acc =

        case (diffOps, oldInputs) of
          ([], []) ->
            acc

          (KeepValue :: moreDiffOps, oldHead :: oldTail) ->
            let tails = walk moreDiffOps (Just oldHead) oldTail acc in
            List.simpleMap (\\newTail -> (f, oldHead) :: newTail) tails

          (DeleteValue :: moreDiffOps, oldHead :: oldTail) ->
            let tails = walk moreDiffOps (Just oldHead) oldTail acc in
            tails

          ((UpdateValue newVal) :: moreDiffOps, oldHead :: oldTail) ->
            let tails = walk moreDiffOps (Just oldHead) oldTail acc in
            let heads =
              (Update.updateApp {fun (a,b) = a b, input = (f, oldHead), output = newVal}).values
            in
            List.cartesianProductWith List.cons heads tails

          ((InsertValue newVal) :: moreDiffOps, _) ->
            let headOrPreviousHead =
              case (oldInputs, maybePreviousInput) of
                (oldHead :: _, _) -> oldHead
                ([], Just oldPreviousHead) -> oldPreviousHead
            in
            let tails = walk moreDiffOps maybePreviousInput oldInputs acc in
            let heads =
              (Update.updateApp {fun (a,b) = a b, input = (f, headOrPreviousHead), output = newVal}).values
            in
            List.cartesianProductWith List.cons heads tails
      in
      let newLists =
        walk (Update.listDiff oldOutputList newOutputList) Nothing oldInputList [[]]
      in
      let newFuncAndInputLists =
        List.simpleMap (\\newList ->
          let (newFuncs, newInputList) = List.unzip newList in
          let (newFunc, _) = Update.merge f (
            List.map (\\newF -> (newF, Update.diff f newF |> .args._1)) newFuncs) in
          (newFunc, newInputList)
        ) newLists
      in
      Ok (Inputs newFuncAndInputLists)
  }

listMap f xs =
  Update.applyLens listMapLens (f, xs)

-----------------------------------------------

transformedValues =
  listMap
    (\\n -> n + 1)
    [0,1,2,3]

main =
  Html.p [] [] (toString transformedValues)
"""

listAppendLens =
 """listAppendSimple xs ys =
  case xs of
    []    -> ys
    x::xs -> x :: listAppendSimple xs ys

listAppendLens = {
  apply (xs, ys) = listAppendSimple xs ys

  update {input = (xs,ys), outputOld, outputNew} =

    -- consLefts, consRights : a -> List (List a, List a) -> List (List a, List a)
    let consLefts v list = List.simpleMap (Tuple.mapFirst (List.cons v)) list in
    let consRights v list = List.simpleMap (Tuple.mapSecond (List.cons v)) list in

    let walk insertLeft diffOps xs ys acc =
      case diffOps of
        [] ->
          case (xs, ys) of
            ([], []) -> acc

        KeepValue::diffOps ->
          case (xs, ys) of
            (x::xs, _) -> consLefts x (walk True diffOps xs ys acc)
            ([], y::ys) -> consRights y (walk False diffOps xs ys acc)

        DeleteValue::diffOps ->
          case (xs, ys) of
            (_::xs, _) -> walk True diffOps xs ys acc
            ([], _::ys) -> walk False diffOps [] ys acc

        (UpdateValue v)::diffOps ->
          case (xs, ys) of
            (_::xs, _) -> consLefts v (walk True diffOps xs ys acc)
            ([], _::ys) -> consRights v (walk False diffOps [] ys acc)

        (InsertValue v)::diffOps ->
          case (xs, ys) of
            (_::_, _) -> consLefts v (walk True diffOps xs ys acc)
            ([], _) ->
              if insertLeft then
                listAppendSimple
                  (consLefts v (walk True diffOps [] ys acc))
                  (consRights v (walk False diffOps [] ys acc))
              else
                consRights v (walk False diffOps [] ys acc)
    in
    let newLists =
      walk True (Update.listDiff outputOld outputNew) xs ys [([],[])]
    in
    Ok (Inputs newLists)
}

listAppend xs ys =
  Update.applyLens listAppendLens (xs, ys)

listConcat xss =
  case xss of
    []      -> Update.freeze []
    ys::yss -> listAppend ys (listConcat yss)

main =
  h3 [] [] (toString (listConcat [[0,1], [2,3], [4,5]]))

"""

fromleo_markdown =
 """original =
  \"\"\"#[Markdown](https://fr.wikipedia.org/wiki/Markdown) demo
This is *an **almost :\"bidirectional\":*** markdown
editor. You can **fully** edit the value of the variable `original` on the right,
and _partially_ edit the html on the right.

*Limitation*: Since this is a regex-based transformation, it cannot correctly nest italics into bold (unless you use underscores instead of stars)
#### Markdown source
* Headers of level n are prefixed with n #
* Use * or 1. to introduce lists

#### Html rendering
1. You can insert elements in lists.
2. Use SHIFT+ENTER for new lines.
3. Do not use CTRL+V

#### Anywhere
2. Add bold by wrapping with two stars.
3. Add emphasis by wrapping with underscorses.
1. Use backtick to insert `code`

>Markdown is a lightweight markup
>language with plain text formatting syntax

\"\"\"

{trim, sprintf, length} = String
{foldl = foldLeft} = List
{freeze, strDiffToConcreteDiff} = Update

-- Thanks to https://gist.github.com/jbroadway/2836900
markdown text =
  let self = {
    para regs =
      let line = nth regs.group 2 in
      let trimmed = trim line in
      if (matchIn \"^</?(ul|ol|li|h|p|bl)\" trimmed) then (
        (nth regs.group 1) + line
      ) else (
        sprintf \"%s<p>%s</p>\\n\" [nth regs.group 1, line]
      )
    ul_list regs = 
      let item = nth regs.group 1 in
      sprintf \"\\n<ul>\\n\\t<li >%s</li>\\n</ul>\" (trim item)
    ol_list regs =
      let item = nth regs.group 1 in
      sprintf \"\\n<ol>\\n\\t<li >%s</li>\\n</ol>\" (trim item)
    blockquote regs =
      let item = nth regs.group 2 in
      sprintf \"\\n<blockquote>%s</blockquote>\" (trim item)
    header regs =
      let {group= [tmp, nl, chars, header]} = regs in
      let level = toString (length chars) in
      sprintf \"<h%s>%s</h%s>\" [level, trim header, level]
  } in
  let rules = [
    [\"(\\n|^)(#+)(.*)\", self.header],                              -- headers
    [\"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\", \"<a href='$2'>$1</a>\"],    -- links
    [\"(\\\\*\\\\*|__)(?=[^\\\\s\\\\*_])(.*?)\\\\1\", \"<strong>$2</strong>\"], -- bold
    [\"(\\\\*|_)(?=[^\\\\s\\\\*_])(.*?)\\\\1\", \"<em>$2</em>\"],             -- emphasis
    [\"\\\\~\\\\~(.*?)\\\\~\\\\~\", \"<del>$1</del>\"],                       -- del
    [\"\\\\:\\\"(.*?)\\\"\\\\:\", \"<q>$1</q>\"],                             -- quote
    [\"`\\\\b(.*?)\\\\b`\", \"<code>$1</code>\"],                         -- inline code
    [\"\\r?\\n\\\\*(.*)\", self.ul_list, {
      postReverse = 
        updateReplace \"\"\"\\r?\\n<ul>\\r?\\n\\t<li>((?:(?!</li>)[\\s\\S])*)</li>\\r?\\n</ul>\"\"\" \"\\n* $1\"
    }],                                  -- ul lists
    [\"\\r?\\n[0-9]+\\\\.(.*)\", self.ol_list, {
      postReverse  = 
        updateReplace \"\"\"\\r?\\n<ol>\\r?\\n\\t<li>((?:(?!</li>)[\\s\\S])*)</li>\\r?\\n</ol>\"\"\" \"\\n1. $1\"
    }],                            -- ol lists
    [\"\\r?\\n(&gt;|\\\\>)(.*)\", self.blockquote],                        -- blockquotes
    [\"\\r?\\n-{5,}\", \"\\n<hr>\"],                                        -- horizontal rule
    [\"\\r?\\n\\r?\\n(?!<ul>|<ol>|<p>|<blockquote>)\",\"<br>\"],                -- add newlines
    [\"\\r?\\n</ul>\\\\s?<ul>\", \"\", {
      postReverse out diffs =
        updateReplace \"\"\"(<li>(?:(?!</li>)[\\s\\S])*</li>)(?=[\\s\\S]*?</(ul|ol)>)\"\"\" \"\\n</$2>\\n<$2>\\n\\t$1\" out diffs |>
        case of
          (v, VStringDiffs l) ->
            if not (v == out) then
              (v, VStringDiffs (List.map (
                \\StringUpdate a b r -> StringUpdate (a + 6) (b + 6) r) l))
            else (out, diffs)
    }],                                     -- fix extra ul
    [\"\\r?\\n</ol>\\\\s?<ol>\", \"\"],                                      -- fix extra ol, and extract blockquote
    [\"</blockquote>\\\\s?<blockquote>\", \"\\n\"]
  ] in
  let finaltext = \"\\n\" + text + \"\\n\" in
  foldLeft (\\elem acc -> case elem of
      [regex, replacement] -> Regex.replace regex replacement acc
      [regex, replacement, {postReverse = fun}] ->
        let newAcc = {
          apply acc = acc
          update {input,newOutput=out,diffs} =
            case fun out diffs of
              (value, diff) -> Ok (InputsWithDiffs [(value, Just diff)])
              value  -> case Update.diff input value of
                Ok n -> Ok (InputsWithDiffs [(value, n)])
                Err x -> Err x
          }.apply acc in
        Regex.replace regex replacement newAcc 
  ) finaltext rules 

-- Takes care of newlines inserted in the document.
newlines_lens x = {
  apply x = x
  update {outputNew,diffs} =
    let aux offset d strAcc = case d of
      [] -> strAcc
      ((ConcStringUpdate start end inserted) :: dtail) ->
        let left = String.take (start + offset) strAcc in
        let right =  String.dropLeft (start + offset + String.length inserted) strAcc in
        let newInserted =
          case Regex.extract \"\"\"^<div>([\\s\\S]*)</div>\"\"\" inserted of
            Just [content] ->
              if Regex.matchIn \"(?:^|\\n)#(.*)$\" left then -- Title, we jump only one line
                \"\\n\" + content + \"\\n\"
              else -- we jump TWO lines
                \"\\n\\n\" + content + \"\\n\"
            Nothing ->    
              if Regex.matchIn \"\"\"(?:^|\\n)(#|\\*|\\d\\.)(.*)$\"\"\" left then
                inserted
              else
                Regex.replace \"\"\"<br>\"\"\"  \"\\n\\n\" inserted
        in
        left + newInserted + right |>
        aux (offset + String.length inserted - (end - start)) dtail
    in Ok (Inputs [aux 0 (strDiffToConcreteDiff outputNew diffs) outputNew])
  }.apply x

Html.forceRefresh <|
Html.span [] [] <| html ((freeze markdown) (newlines_lens original))
"""

fromleo_markdown_optimized =
 """original =
  \"\"\"#[Markdown](https://fr.wikipedia.org/wiki/Markdown) demo
This is *an **almost :\"bidirectional\":*** markdown
editor. You can **fully** edit the value of the variable `original` on the right,
and _partially_ edit the html on the right.

*Limitation*: Since this is a regex-based transformation, it cannot correctly nest italics into bold (unless you use underscores instead of stars)
#### Markdown source
* Headers of level n are prefixed with n #
* Use * or 1. to introduce lists

#### Html rendering
1. You can insert elements in lists.
2. Use SHIFT+ENTER for new lines.
3. Do not use CTRL+V

#### Anywhere
2. Add bold by wrapping with two stars.
3. Add emphasis by wrapping with underscorses.
1. Use backtick to insert `code`

>Markdown is a lightweight markup
>language with plain text formatting syntax

\"\"\"

{trim, sprintf, length} = String
{foldl = foldLeft} = List
{freeze, strDiffToConcreteDiff} = Update

trim s = case extractFirstIn \"^\\\\s*([\\\\s\\\\S]*?)\\\\s*$\" s of
      Just [trimmed] -> trimmed
      Nothing -> s

-- Thanks to https://gist.github.com/jbroadway/2836900
markdown text =
  let para regs =
      let line = nth regs.group 2 in
      let trimmed = trim line in
      if (matchIn \"^</?(ul|ol|li|h|p|bl)\" trimmed) then
        (nth regs.group 1) + line
      else
        nth regs.group 1 + \"<p>\" + line + \"</p>\\n\"
  in
  let ul_list regs = 
      let item = nth regs.group 1 in
      \"\\n<ul>\\n\\t<li >\" + trim item + \"</li>\\n</ul>\"
  in
  let ol_list regs =
      let item = nth regs.group 1 in
      \"\\n<ol>\\n\\t<li >\" + trim item + \"</li>\\n</ol>\"
  in
  let blockquote regs =
      let item = nth regs.group 2 in
      \"\\n<blockquote>\" + trim item + \"</blockquote>\"
  in
  let header {group= [tmp, nl, chars, header]} =
      let level = toString (length chars) in
      \"<h\" + level + \">\" + trim header + \"</h\" + level + \">\"
  in
  let onUpdate fun acc = {
    apply acc = acc
    update {input,newOutput=out,diffs} =
      case fun out diffs of
        (value, diff) -> Ok (InputsWithDiffs [(value, Just diff)])
        value  -> case Update.diff input value of
          Ok n -> Ok (InputsWithDiffs [(value, n)])
          Err x -> Err x
    }.apply acc
  in
  \"\\n\" + text + \"\\n\" |>
  replaceAllIn \"(\\n|^)(#+)(.*)\" header |>
  replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" |>
  replaceAllIn \"(\\\\*\\\\*|__)(?=[^\\\\s\\\\*_])(.*?)\\\\1\" \"<strong>$2</strong>\" |>
  replaceAllIn \"(\\\\*|_)(?=[^\\\\s\\\\*_])(.*?)\\\\1\" \"<em>$2</em>\" |>
  replaceAllIn \"\\\\~\\\\~(.*?)\\\\~\\\\~\" \"<del>$1</del>\" |>
  replaceAllIn \"\\\\:\\\"(.*?)\\\"\\\\:\" \"<q>$1</q>\" |>
  replaceAllIn \"`\\\\b(.*?)\\\\b`\" \"<code>$1</code>\" |>
  replaceAllIn \"\\r?\\n\\\\*(.*)\" ul_list |>
  onUpdate (updateReplace \"\"\"\\r?\\n<ul>\\r?\\n\\t<li>((?:(?!</li>)[\\s\\S])*)</li>\\r?\\n</ul>\"\"\" \"\\n* $1\") |>
  replaceAllIn \"\\r?\\n[0-9]+\\\\.(.*)\" ol_list |>
  onUpdate (updateReplace \"\"\"\\r?\\n<ol>\\r?\\n\\t<li>((?:(?!</li>)[\\s\\S])*)</li>\\r?\\n</ol>\"\"\" \"\\n1. $1\") |>
  replaceAllIn \"\\r?\\n(&gt;|\\\\>)(.*)\" blockquote |>
  replaceAllIn \"\\r?\\n-{5,}\" \"\\n<hr>\" |>
  replaceAllIn \"\\r?\\n\\r?\\n(?!<ul>|<ol>|<p>|<blockquote>)\" \"<br>\" |>
  replaceAllIn \"\\r?\\n</ul>\\\\s?<ul>\" \"\" |>
  onUpdate (\\out diffs -> 
        updateReplace \"\"\"(<li>(?:(?!</li>)[\\s\\S])*</li>)(?=[\\s\\S]*?</(ul|ol)>)\"\"\" \"\\n</$2>\\n<$2>\\n\\t$1\" out diffs |>
        case of
          (v, VStringDiffs l) ->
            if not (v == out) then
              (v, VStringDiffs (List.map (
                \\StringUpdate a b r -> StringUpdate (a + 6) (b + 6) r) l))
            else (out, diffs)
  ) |>
  replaceAllIn \"\\r?\\n</ol>\\\\s?<ol>\" \"\" |>
  replaceAllIn \"</blockquote>\\\\s?<blockquote>\" \"\\n\"

-- Takes care of newlines inserted in the document.
newlines_lens x = {
  apply x = x
  update {outputNew,diffs} =
    let aux offset d strAcc = case d of
      [] -> strAcc
      ((ConcStringUpdate start end inserted) :: dtail) ->
        let left = String.take (start + offset) strAcc in
        let right =  String.dropLeft (start + offset + String.length inserted) strAcc in
        let newInserted =
          case Regex.extract \"\"\"^<div>([\\s\\S]*)</div>\"\"\" inserted of
            Just [content] ->
              if Regex.matchIn \"(?:^|\\n)#(.*)$\" left then -- Title, we jump only one line
                \"\\n\" + content + \"\\n\"
              else -- we jump TWO lines
                \"\\n\\n\" + content + \"\\n\"
            Nothing ->    
              if Regex.matchIn \"\"\"(?:^|\\n)(#|\\*|\\d\\.)(.*)$\"\"\" left then
                inserted
              else
                Regex.replace \"\"\"<br>\"\"\"  \"\\n\\n\" inserted
        in
        left + newInserted + right |>
        aux (offset + String.length inserted - (end - start)) dtail
    in Ok (Inputs [aux 0 (strDiffToConcreteDiff outputNew diffs) outputNew])
  }.apply x

Html.forceRefresh <|
Html.span [] [] <| html ((freeze markdown) (newlines_lens original))
"""

fromleo_markdown_optimized_lensless =
 """original =
  \"\"\"#[Markdown](https://fr.wikipedia.org/wiki/Markdown) demo
This is *an **almost :\"bidirectional\":*** markdown
editor. You can **fully** edit the value of the variable `original` on the right,
and _partially_ edit the html on the right.

*Limitation*: Since this is a regex-based transformation, it cannot correctly nest italics into bold (unless you use underscores instead of stars)
#### Markdown source
* Headers of level n are prefixed with n #
* Use * or 1. to introduce lists

#### Html rendering
1. You can insert elements in lists.
2. Use SHIFT+ENTER for new lines.
3. Do not use CTRL+V

#### Anywhere
2. Add bold by wrapping with two stars.
3. Add emphasis by wrapping with underscorses.
1. Use backtick to insert `code`

>Markdown is a lightweight markup
>language with plain text formatting syntax

\"\"\"

{trim, sprintf, length} = String
{foldl = foldLeft} = List
{freeze, strDiffToConcreteDiff} = Update

trim s = case extractFirstIn \"^\\\\s*([\\\\s\\\\S]*?)\\\\s*$\" s of
      Just [trimmed] -> trimmed
      Nothing -> s

-- Thanks to https://gist.github.com/jbroadway/2836900
markdown text =
  let para regs =
      let line = nth regs.group 2 in
      let trimmed = trim line in
      if (matchIn \"^</?(ul|ol|li|h|p|bl)\" trimmed) then
        (nth regs.group 1) + line
      else
        nth regs.group 1 + \"<p>\" + line + \"</p>\\n\"
  in
  let ul_list regs = 
      let item = nth regs.group 1 in
      \"\\n<ul>\\n\\t<li >\" + trim item + \"</li>\\n</ul>\"
  in
  let ol_list regs =
      let item = nth regs.group 1 in
      \"\\n<ol>\\n\\t<li >\" + trim item + \"</li>\\n</ol>\"
  in
  let blockquote regs =
      let item = nth regs.group 2 in
      \"\\n<blockquote>\" + trim item + \"</blockquote>\"
  in
  let header {group= [tmp, nl, chars, header]} =
      let level = toString (length chars) in
      \"<h\" + level + \">\" + trim header + \"</h\" + level + \">\"
  in
  \"\\n\" + text + \"\\n\" |>
  replaceAllIn \"(\\n|^)(#+)(.*)\" header |>
  replaceAllIn \"\\\\[([^\\\\[]+)\\\\]\\\\(([^\\\\)]+)\\\\)\" \"<a href='$2'>$1</a>\" |>
  replaceAllIn \"(\\\\*\\\\*|__)(?=[^\\\\s\\\\*_])(.*?)\\\\1\" \"<strong>$2</strong>\" |>
  replaceAllIn \"(\\\\*|_)(?=[^\\\\s\\\\*_])(.*?)\\\\1\" \"<em>$2</em>\" |>
  replaceAllIn \"\\\\~\\\\~(.*?)\\\\~\\\\~\" \"<del>$1</del>\" |>
  replaceAllIn \"\\\\:\\\"(.*?)\\\"\\\\:\" \"<q>$1</q>\" |>
  replaceAllIn \"`\\\\b(.*?)\\\\b`\" \"<code>$1</code>\" |>
  replaceAllIn \"\\r?\\n\\\\*(.*)\" ul_list |>
  replaceAllIn \"\\r?\\n[0-9]+\\\\.(.*)\" ol_list |>
  replaceAllIn \"\\r?\\n(&gt;|\\\\>)(.*)\" blockquote |>
  replaceAllIn \"\\r?\\n-{5,}\" \"\\n<hr>\" |>
  replaceAllIn \"\\r?\\n\\r?\\n(?!<ul>|<ol>|<p>|<blockquote>)\" \"<br>\" |>
  replaceAllIn \"\\r?\\n</ul>\\\\s?<ul>\" \"\" |>
  replaceAllIn \"\\r?\\n</ol>\\\\s?<ol>\" \"\" |>
  replaceAllIn \"</blockquote>\\\\s?<blockquote>\" \"\\n\"

Html.span [] [] <| html ((freeze markdown) original)
"""

fromleo_conference_budgetting =
 """-- Use of 'exactly' and 'notBelow X' to specify how values can(not) be changed.

days =  exactly 3
venue = exactly 10000 * days
lunch = (notBelow 20) 30

participants = 200
fee          = 50
sponsors     = 20000

expenses = exactly participants * lunch * days + venue
income   = exactly participants * fee + sponsors

surplus = income - expenses


-- Change surplus to 0, it changes the lunch but will stop at 20, so the surplus will be negative.
-- Change surplus to 0 again, it changes the registration fee.
main =
  <div style=\"margin:20px\">Current surplus of conference:
    <br><h3 id=\"surplus\">@(toString surplus)</h3>
    @(if surplus /= 0 then
       <button onclick=\"document.getElementById('surplus').innerText = '0'\">Set to zero</button>
      else
       <span>Hurray, the budget is coherent!</span>)
    </div>



-- Useful library definitions

exactly x = freeze x

notBelow bound x = {
  apply x = x
  update {input, outputNew} =
    if outputNew <= bound &&
       input     >  bound     then Ok (Inputs [bound])
    else if outputNew > bound then Ok (Inputs [outputNew])
    else                           Ok (Inputs [])
  }.apply x
"""

fromleo_recipe =
 """# updatedelay: 1000

base = 1000
temperature = 180

language = \"English\"
otherLanguage = if language == \"French\" then \"English\" else \"French\"

txt = \"\"\"<br><button onclick=\"this.setAttribute('x', parseInt(this.getAttribute('x'))*2 + '')\" x=\"multdivby[1024,1024]\">x2</button>
<button onclick=\"this.setAttribute('x', parseInt(this.getAttribute('x'))/2 + '')\" x=\"multdivby[1024,1024]\">/2</button>\"\"\" + (
  Dict.fromList [
  (\"French\", \"\"\"
<h1>Moelleux chocolat amandes</h1>
Recette pour multdivby[20,1000] petits gâteaux.<br>
Préchauffer le four à @(temperature)° (Celsius)
<li>multdivby[4,1000] œufifmanys[4,1000]</li>
<li>multdivby[1,2000] verre de sucre</li>
<li>multdivby[200,1000]g de chocolat fondu</li>
<li>multdivby[50,1000]g de poudre d’amande</li>
<li>multdivby[2,1000] cs d’huile de tournesol</li>
<li>Cannelle</li>
<li>Pincée de sel</li>
Au four pendant 10 minutes dans des moules à cupcakes.<br>
On peut aussi mettre en déco des amandes effilées, ou remplacer le chocolat par un citron pressé\"\"\"),
  (\"English\", \"\"\"
<h1>Soft chocolate almond cakes</h1>
Recipe for multdivby[20,1000] small cakes.<br>
Preheat the oven at @(floor (temperature * freeze 9 / freeze 5) + freeze 32)° Fahrenheit
<li>multdivby[4,1000] eggifmanys[4,1000]</li>
<li>multdivby[1,2000] cup of sugar</li>
<li>multdivby[200,1000]g of melted chocolate</li>
<li>multdivby[50,1000]g of almond powder</li>
<li>multdivby[2,1000] tbls of sunflower oil</li>
<li>Cinnamon</li>
<li>A pinch of salt</li>
In the oven for 10 minutes in cupcakes pans.<br>
One can also put as decoration sliced almonds, or replace chocolate by a squeezed lemon.\"\"\")
  ] |> flip Dict.apply language)

result = Regex.replace \"(multdivby|ifmany(\\\\w+))\\\\[(\\\\d+),(\\\\d+)\\\\]\" (\\m ->
  let mult = String.toInt <| nth m.group 3 in
  let div = String.toInt <|  nth m.group 4 in
  case nth m.group 1 of
    \"multdivby\" ->
      let res = floor (base * freeze mult / freeze div) in
      if res < 6 then -- We take into account 1/2, 1/4 and 3/4 until 5, else it makes no sense, but no more.
        { apply base =
           case floor (base * mult * 4 / div) - 4*res of
             0 -> if res == 0 then \"<¼\" else toString res
             1 -> if res == 0 then \"¼\" else if res >= 4 then toString res else toString res + \"¼\"
             2 -> if res == 0 then \"½\" else toString res + \"½\"
             3 -> if res == 0 then \"¾\" else if res >= 4 then toString res else toString res + \"¾\"
          update {outputNew, outputOriginal} =
            if outputNew == outputOriginal then Ok (Inputs [base]) else
            let quantityTimes4 = case Regex.extract \"(.*)(¼|[ +]?[13]/[24]|½|¾)\" outputNew of
              Just [i, complement] -> 
                 let addi x = if i == \"\" then x else 4 * String.toInt i + x in
                 case complement of
                   \"¼\"    -> addi 1
                   \"1/4\"  -> addi 1
                   \" 1/4\" -> addi 1
                   \"+1/4\" -> addi 1
                   \"½\"    -> addi 2
                   \"1/2\"  -> addi 2
                   \" 1/2\" -> addi 2
                   \"+1/2\" -> addi 2
                   \"¾\"    -> addi 3
                   \"3/4\"  -> addi 3
                   \" 3/4\" -> addi 3
                   \"+3/4\" -> addi 3
                   a      -> error <| \"Unexpected complement: \" + complement
              Nothing -> 4 * String.toInt outputNew
            in
            Ok (Inputs [floor (quantityTimes4 * div / mult / 4)])
        }.apply base
      else toString res
    ifmanyEnding ->
      let ending = nth m.group 2 in
      let res = floor (base * freeze mult * freeze 4 / freeze div) in
      { apply (res, ending) = if res > 4 then ending else \"\"
        update {input=(res,ending), outputNew, outputOriginal} =
          if outputNew == \"\" && outputOriginal == ending then
            Ok (Inputs [(4, ending)])
          else
            if Regex.matchIn \" \" outputNew then Ok (Inputs []) else
            Ok (Inputs [(res, outputNew)]) }.apply (res, ending)) txt

Html.div [[\"margin\", \"20px\"], [\"cursor\", \"text\"]] [] <| (\\x -> [x]) <|
Html.span [] [] <|
html <| \"\"\"<button onclick=\"this.setAttribute('v','@otherLanguage')\" v=\"@language\">To @otherLanguage</button><br>\"\"\" +
  ( Dict.fromList [(\"English\", \"\"\"<i>Hint:</i> Use _5_ for a proportional number 5, _5es_ to place an s if the quantity (5) is greater than 1.\"\"\"),
          (\"French\", \"\"\"<i>Astuce:</i> Ecrire _5_ pour un nombre proportionel 5, _5s_ pour un 's' conditionel si la quantité 5 est plus grande que 1.\"\"\")] |> flip Dict.apply language) + 
 { apply x = freeze x ,
   update {output} =
     Ok (Inputs [Regex.replace \"_(\\\\d+)(\\\\w*)_\" (\\m ->
        let amount = String.toInt (nth m.group 1) in
        let plural = nth m.group 2 in
        case plural of
           \"\" -> \"\"\"multdivby[@amount,@base]\"\"\"
           _ ->
            \"\"\"ifmany@plural[@amount,@base]\"\"\"
        ) output])
 }.apply result

"""

fromleo_recipe2 =
 """----------------------------------------------------------------------
-- Description of Scalable Recipe Editor

editorInfo = <span>
<h1>Scalable Recipe Editor</h1>
<p>
  This text editor is set up to allow <em>proportional quantities</em>
  to be defined relative to some <em>base</em> quantity.
  Whenever a proportional or base quantity is changed, all others are
  changed, too.
</p>
<p>
  <b>Defining Proportions:</b>
  Wrap a number with underscores (for example, \"_20_\") to make the
  quantity proportional to the base value.
</p>
<p>
  <b>Defining Plural Words:</b>
  To make a new word, such as \"chip\", appear in plural form,
  based on whether a proportional quantity is greater than one,
  write the word like \"chip_20s_\".
</p>
<p>
  In the example recipe below, try adding an ingredient as follows.
  Hit enter after the melted chocolate ingredient.
  Then type \"_20_g of chocolate chip_20s_\".
</p>
</span>

----------------------------------------------------------------------
-- Scalable Recipe Editor \"Library\"

introduceFractions res blah =
  case blah of
    0 -> if res == 0 then \"<¼\" else toString res
    1 -> if res == 0 then \"¼\"  else if res >= 4 then toString res else toString res + \"¼\"
    2 -> if res == 0 then \"½\"  else toString res + \"½\"
    3 -> if res == 0 then \"¾\"  else if res >= 4 then toString res else toString res + \"¾\"

updateFractions string =
  case Regex.extract \"(.*)(¼|[ +]?[13]/[24]|½|¾)\" string of
    Nothing ->
      4 * String.toInt string

    Just [i, complement] -> 
       let addi x = if i == \"\" then x else 4 * String.toInt i + x in
       case complement of
         \"¼\"    -> addi 1
         \"1/4\"  -> addi 1
         \" 1/4\" -> addi 1
         \"+1/4\" -> addi 1
         \"½\"    -> addi 2
         \"1/2\"  -> addi 2
         \" 1/2\" -> addi 2
         \"+1/2\" -> addi 2
         \"¾\"    -> addi 3
         \"3/4\"  -> addi 3
         \" 3/4\" -> addi 3
         \"+3/4\" -> addi 3
         a      -> error <| \"Unexpected complement: \" + complement

evaluateMacros base =
  Regex.replace \"(multdivby|ifmany(\\\\w+))\\\\[(\\\\d+),(\\\\d+)\\\\]\" (\\m ->

    let match = nth m.group 0 in
    let macro = nth m.group 1 in
    let mult  = String.toInt <| nth m.group 3 in
    let div   = String.toInt <| nth m.group 4 in
    case macro of

      \"multdivby\" ->
        let res = floor (base * freeze mult / freeze div) in
        if res < 6 then -- We take into account 1/2, 1/4 and 3/4 until 5, else it makes no sense, but no more.
          Update.applyLens
            { apply (base, _) =
                introduceFractions res (floor (base * mult * 4 / div) - 4*res)

              update {outputNew, outputOriginal} =
                if outputNew == outputOriginal then
                  Ok (Inputs [(base, match)])
                else
                  let newBase =
                    floor (updateFractions outputNew * div / mult / 4)
                  in
                  let newMatch =
                    let newMult = mult * String.toInt outputNew in
                    let newDiv  = div  * String.toInt outputOriginal in
                    \"\"\"multdivby[@newMult,@newDiv]\"\"\"
                  in
                  Ok (Inputs [(newBase, match), (base, newMatch)])
            }
            (base, match)
        else
          toString res

      ifmanyEnding ->
        let ending = nth m.group 2 in
        let res = floor (base * freeze mult * freeze 4 / freeze div) in
        Update.applyLens
          { apply (res, ending) =
              if res > 4 then ending else \"\"

            update {input=(res,ending), outputNew, outputOriginal} =
              if outputNew == \"\" && outputOriginal == ending then Ok (Inputs [(4, ending)])
              else if Regex.matchIn \" \" outputNew then Ok (Inputs [])
              else Ok (Inputs [(res, outputNew)])
          }
          (res, ending)
  )

defineMacros base = 
  Update.applyLens
    { apply x = x
    , update {output} =
        Ok (Inputs [Regex.replace \"_(\\\\d+)(\\\\w*)_\" (\\m ->
            let amount = String.toInt (nth m.group 1) in
            let plural = nth m.group 2 in
            case plural of
              \"\" -> \"\"\"multdivby[@amount,@base]\"\"\"
              _  -> \"\"\"ifmany@plural[@amount,@base]\"\"\"
          ) output]
        )
    }

scalableRecipe base recipe =
  recipe |> defineMacros base |> evaluateMacros base

----------------------------------------------------------------------
-- Example: Scalable Recipe

-- base = 1000 ~ servings = 20
base = 1000

ingredients = \"\"\"
<li>multdivby[4,1000] eggifmanys[4,1000]</li>
<li>multdivby[1,2000] cup of sugar</li>
<li>multdivby[200,1000]g of melted chocolate</li>
<li>multdivby[50,1000]g of almond powder</li>
<li>multdivby[2,1000] tbls of sunflower oil</li>
<li>Cinnamon</li>
<li>A pinch of salt</li>
\"\"\"

directions = \"\"\"
<li>Preheat the oven at @(floor (180 * freeze 9 / freeze 5) + freeze 32)° Fahrenheit.</li>
<li>Mix all ingredients together.</li>
<li>Bake in the oven for 10 minutes in cupcakes pans.</li>
<li>Remove from oven and top with sliced almonds.</li>
\"\"\"

cupcakeRecipe = \"\"\"

<img width=\"300px\" src=\"https://tinyurl.com/yalbg6le\" alt=\"cupcakes\">

<h1>Chocolate almond cakes</h1>

<p>
  Servings: multdivby[20,1000] small cakes.
  
  <button
      onclick=\"this.setAttribute('x', parseInt(this.getAttribute('x'))*2 + '')\"
      x=\"multdivby[1024,1024]\">
    Double
  </button>,
  <button
      onclick=\"this.setAttribute('x', parseInt(this.getAttribute('x'))/2 + '')\"
      x=\"multdivby[1024,1024]\">
    Halve
  </button>,
  or edit the number of servings directly.
</p>

<h4>Ingredients for Cake Batter</h4>

@ingredients

<h4>Directions</h4>

@directions
\"\"\"

htmlCupcakeRecipe =
  Html.span [] [] <| Html.parse (scalableRecipe base cupcakeRecipe)

main = 
  Html.div [[\"margin\", \"20px\"], [\"cursor\", \"text\"]] []
    [ Html.div [] [] [editorInfo]
    , Html.div [[\"border\", \"4px solid black\"], [\"padding\", \"20px\"]] []
        [htmlCupcakeRecipe]
    ]

"""

fromleo_modelviewcontroller =
 """--# updatedelay:0

ui model =
  let
    stringFlagForUpdate state1 state2 f model =
      Update.applyLens
        { apply _ = state1
        , update {input = model, outputNew} =
            if outputNew == state2
              then Ok (Inputs [f model])
              else Ok (InputsWithDiffs [(model, Nothing)])
        } model
  in
  { button name controller =
      [ \"button\"
      , [ [\"trigger\", stringFlagForUpdate \"\" \"#\" controller model]
        , [\"onclick\", \"this.setAttribute('trigger', '#')\"]
        ]
      , [Html.textNode name]
      ]
  }

model = {
  n = 16
  customCode = \"if n % 2 == 0 then n / 2 else 1+n*3\"
  multiplier = 2
}

controllers = {
  addOne model = { model | n = model.n + 1 }
  changeN f model = { model | n = f model.n }
  customEval model = { model | n = evaluate \"\"\"let n = @(model.n) in @(model.customCode)\"\"\" }
  changeMultiplier f model = { model | multiplier = f model.multiplier }
}

view =
  let {button} = ui model in
  let {addOne, changeN, customEval, changeMultiplier} = controllers in
  Html.div [[\"margin\",\"20px\"]] [[\"contenteditable\",\"\"]] (
    [ Html.h1 [] [] \"Model-View-Controller\"]
    ++
    Html.parse
      \"\"\"
      Using lenses, you can architect your HTML page using model-view-controller.
      <br><br>
      n = <span>@(model.n)</span>
      <br><br>
      What do you want to do?
      <br><br>
      \"\"\"
    ++
    [ button \"\"\"Increment\"\"\" (changeN <| \\n -> n + 1)
    , button \"\"\"Decrement\"\"\" (changeN <| \\n -> n - 1)
    , Html.br
    , button \"\"\"Multiply by @(model.multiplier)\"\"\" (changeN <| \\n -> n * model.multiplier)
    , button \"\"\"Divide by @(model.multiplier)\"\"\"   (changeN <| \\n -> n / model.multiplier)
    , Html.br
    , button \"\"\"Increase multiplier to @(model.multiplier + 1)\"\"\" (changeMultiplier <| \\m -> m + 1)
    , if model.multiplier > 2 then
        button \"\"\"Decrease multiplier to @(model.multiplier - 1)\"\"\" (changeMultiplier <| \\m -> m - 1)
      else
        Html.span [] [] []
    , Html.br
    , button \"\"\"Custom Code:\"\"\" customEval
    , Html.span [[\"font-family\",\"monospace\"]] [] [Html.textNode (Update.freeze \" \" + model.customCode)]
    ]
  )

main = view

"""

fromleo_linkedtexteditor =
 """editorInfo = \"\"\"
<h1>Linked-Text Editor</h1>
<p>
  This text editor is set up to allow <em>linking</em>
  ot text elements. Two portions of text can be linked
  together by defining a variable, from the output interface.
</p>
<p>
  <b>Defining a link-able word:</b>
  Prefix a word with $ (for example, \"$prove\") to create
  a variable named \"prove\". Its content is originally
  its name, so the dollar is removed.
</p>
<p>
  <b>Inserting linked words:</b>
  To insert a link of the content of a variable named \"prove\", 
  type somewhere else \"$prove\" or add a dollar to an existing \"prove\".
</p>
<p>
  <b>Modifying linked words:</b>
  If you change one of the linked occurrences of \"prove\" to \"show\",
  you will notice that all linked occurrences change. If you add a new
  \"$prove\", it will also be replaced by \"prove\".
  Replacements can be nested, but it does not replace variables in cycles, so you're safe
</p>
<p>
  In the example below, the notations are linked. If you change a P to H, all of them will be updated.
</p>
\"\"\"

variables = 
  [(\"proofbyinduction\", \"proof by induction\")
  ,(\"inductionaxiom\", \"induction axiom\")
  ,(\"P\", \"P\")
  ,(\"n\", \"n\")
  ,(\"k\",\"k\")]

variablesDict = Dict.fromList variables

replaceVariables variablesDict string =
  Regex.replace \"\\\\$(?!_)(\\\\w+)\" (\\m -> 
    let key = nth m.group 1 in
    case Dict.get key variablesDict of
      Nothing -> m.match
      Just definition -> replaceVariables (Dict.remove key variablesDict) definition
  ) string

minnum = 1
maxnum = 4
sequence = List.range minnum maxnum

content = \"\"\"<h1>Induction Hypothesis</h1>
<pre style=\"font-family:cambria;white-space:pre-line;\">
A $proofbyinduction makes use of the $inductionaxiom.
The $inductionaxiom states that, for a proposition $P depending on an integer $n, if the following precondition is satisfied:

    $P(1)   ∧   ∀$n⩾1. $P($n) ⇒ $P($n+1)

then the following result holds:

    ∀$n⩾1. $P($n)

In other words, if we want to prove a proposition $P($n) for any integer $n, we first need to prove that $P(1) holds. Then, we need to prove that if we know $P($k) for any given integer $k, we can prove $P($k + 1).

Let us consider the sum of the numbers from @minnum to @maxnum: @(sequence |> map (\\x -> toString x) |> String.join \"+\") = @(List.sum sequence). At the same time, if we multiply @maxnum by @maxnum + 1 (which is @(maxnum + 1)), and divide by 2, we get:@maxnum*(@maxnum+1)/2 = @maxnum*@(maxnum+1)/2 = @(maxnum*(maxnum+1))/2 = @(maxnum*(maxnum+1)/2) which is the same result. Hence we can conjecture that:

$P($n) = \"The sum of numbers 1+...+$n is $n*($n+1)/2\"

We prove it by using the $proofbyinduction principle.<ul
><li>$P(1) is trivially true.</li
><li>If $P($n) holds, then
  1+...+ $n + ($n+1)
= $n*($n+1)/2 + $n+1 (by invoking $P($n))
= ($n+1)(($n+1)+1)/2 (by factoring)
Therefore, $P($n+1) holds.</li></ul
>By the $inductionaxiom, we conclude that $P($n) is true for all $n.
\"\"\" |>
  replaceVariables variablesDict |>
  Regex.replace \"(\\\\$)_(\\\\w+)\" (\\m ->
    nth m.group 1 + \"<span></span>\" + nth m.group 2) |>
  (\\x ->
    { apply (x, variables) = x
      update {input = (x, variables), output} =
        Regex.find \"\\\\$(?!_)(\\\\w+)(?:=(\\\\w+))?\" output |>
        List.foldl (\\(_::name::definition::_) (variables, variablesDict) ->
          if Dict.member name variablesDict  then (variables, variablesDict)
          else 
            let vardef = if definition == \"\" then name else definition
            in (variables ++ [(name, vardef)], Dict.insert name vardef variablesDict)
        ) (variables, variablesDict) |> \\(newVariables, _) ->
          let newOutput = Regex.replace \"\\\\$(?!_)(\\\\w+)(?:=(\\\\w+))?\" (\\m -> 
            let [_, name, _] = m.group in
             if Dict.member name variablesDict then m.match else \"$\" + name
          ) output in
          Ok (Inputs [(newOutput, newVariables)])
    }.apply (x, variables)
  )

  
main = 
  Html.forceRefresh <|
  Html.div [[\"margin\", \"20px\"], [\"cursor\", \"text\"]] []
    [ Html.div [] [] <|
        Html.parse editorInfo
    , Html.div [[\"border\", \"4px solid black\"], [\"padding\", \"20px\"]] [] <|
        Html.parse content
    ]
"""

fromleo_translatabledoc =
 """editorInfo = <span>
<h1>Translation Editor</h1>
<p>
  This text editor is set up to allow you to <em>add translations for</em>
  text elements in different languages.
  It highlights texts that are available in different languages.
  You can edit translations, add translations, and add languages into which to translate.
</p>
<p>
  <b>Editing a translation</b>
  Choose a language by selecting it on the select box.
  Change the text. If the text is a translation in a language,
  it will change only for this language.
  You can display highlight around translateable text portions by enabling the option \"Highlights\".
</p>
<p>
  <b>Translating a static text</b>
  To make a sentence translatable in all languages,
  wrap the sentence it with curly braces like {a translatable sentence}.
  After update, braces disappear and you can edit its translation in different languages.
</p>
<p>
  <b>Add a language</b>
  You can add a language by entering its name in the input box below,
  and press ENTER. It duplicates the translations of the current language.
</p>
<p>
  In the example below, wrap the sentence \"This sentence is true\" with braces,
  update and afterwards translate it to the language of your choice.
</p>
</span>

content = \"\"\"<h1>$translation1</h1>
In case your printer RGB4500 is stuck, please follow these steps:
<ul>
<li>$translation2</li>
<li>$translation3</li>
</ul>
\"\"\"

translations =
  [(\"English\", 
    [(\"translation1\", \"How to fix paper jam\")
    ,(\"translation2\", \"Open the green lid using the red handle at the back\")
    ,(\"translation3\", \"Remove the paper, close the lid\")])
  ,(\"Français\", 
    [(\"translation1\", \"Réparer un bourrage papier\")
    ,(\"translation2\", \"Ouvrir le couvercle vert en tirant la poignée rouge derrière\")
    ,(\"translation3\", \"Enlever le papier bourré, fermer le couvercle\")])
  ]

languages = [\"English\", \"French\"]
languageIndex = 0
language = nth languages languageIndex
highlighttranslations = True

translate options language translations content =
  let allTranslationDicts = List.map (Tuple.mapSecond Dict.fromList) translations in
  let translationsLangDict = Dict.fromList allTranslationDicts in
  let currentTranslation = Dict.apply translationsLangDict language in
  let replaceVariables translationDict string =
    Regex.replace \"\\\\$(\\\\w+|\\\\$)\" (\\m -> 
      if m.match == \"$\" then m.match else
      let key = nth m.group 1 in
      case Dict.get key translationDict of
        Nothing -> m.match
        Just definition -> 
          let finaldefinition = 
            if case options of {highlighttranslations=x} -> x; _ -> False then
              \"\"\"<span style='outline:lightgreen 2px solid;' title='@key'>@definition</span>\"\"\"
            else definition
         in
         replaceVariables (Dict.remove key translationDict) finaldefinition
    ) string
  in
  let freshVarName name i dictionary =
    if Dict.member (name + toString i) (dictionary) then freshVarName name (i + 1) (dictionary) else name + toString i
  in
  content |> replaceVariables currentTranslation |> \\x ->
    { apply (x, _) = x
      update {input = (x, allTranslationDicts), newOutput} =
        Regex.find \"\\\\{([^\\\\}]*(?!\\\\})\\\\S[^\\\\}]*)\\\\}\" newOutput |>
        List.foldl (\\(_::definition::_) (newOutput, currentTranslation, allTranslationDicts) ->
            let name = freshVarName \"translation\" 1 currentTranslation in
            let textToFind = \"\\\\{\" + Regex.replace \"\\\\\\\\|\\\\{|\\\\}|\\\\[|\\\\]|\\\\$|\\\\.|\\\\?|\\\\+\" (\\m -> \"\\\\\" + m.match) definition + \"\\\\}\" in
            (Regex.replace textToFind (\\_ -> \"$\" + name) newOutput,
             Dict.insert name definition currentTranslation,
             List.map (\\(lang, d) -> (lang, Dict.insert name definition d)) allTranslationDicts)
        ) (newOutput, currentTranslation, allTranslationDicts) |> \\(newOutput, _, newTranslationsLangDict) ->
          Ok (Inputs [(newOutput, newTranslationsLangDict)])
    }.apply (x, allTranslationDicts)

addLang translations = {
  apply alltranslationsLangDict = \"\"
  update {input, outputNew} =
    if not (outputNew == \"\") && not (Dict.member outputNew input) then 
      let toCopy = Dict.apply input language in
      Ok (InputsWithDiffs [(Dict.insert outputNew toCopy input
                           ,Just (VDictDiffs (Dict.fromList [(outputNew, VDictElemInsert)])))])
    else
      Ok (InputsWithDiffs [(input, Nothing)])
  }.apply (Dict.fromList translations)

main = 
  Html.forceRefresh <|
  <div style=\"margin:20px;cursor:text\">
    @editorInfo
    <div style=\"border:4px solid black;padding:20px\">
      <span>@(
        Html.select [] languages languageIndex)
        <input style=\"margin-left:10px\" type=\"text\" v=(addLang translations)
           placeholder=\"New Language (e.g. German)\" title=\"Enter the name of a new language here and press ENTER\"
           onchange=\"this.setAttribute('v',this.value)\">@(
        Html.checkbox \"Highlights\" \"Highlight translatable text\" highlighttranslations)<button
          title=\"Make the selected text translatable\"
          onclick=\"\"\"
            var r = window.getSelection().getRangeAt(0);
            var t = r.cloneContents().textContent;
            r.deleteContents();
            r.insertNode(document.createTextNode(\"{\" + t + \"}\"))\"\"\"
          contenteditable=\"false\">Translatable</button>
        <br>
        @(content
          |> translate {highlighttranslations = highlighttranslations} language translations
          |> Html.parse)
      </span>
    </div>
  </div>
"""

fromleo_latexeditor =
 """latex = \"\"\"\\newcommand{\\small}{mini}

\\section{\\LaTeX{} editing in \\textsc{Html}\\label{sec:introduction}}
This \\small{} \\LaTeX{} editor is \\textit{bidirectional} and supports \\small{} \\textbf{textual} changes. Rename '\\small{}' to 'lightweight' to see\\ldots

\\section{Reference update\\label{sec:commands}}
References are supported:
Section \\ref{sec:introduction}.
Change the previous number to 2. $\\frac{b^2-4ac}{2}$\"\"\"

latexttoolong = \"\"\" or 2.1. See how it updates the source code.
\\subsection{Others\\label{others}}
Only frac, exponent and indices in math mode: $\\frac{b^2-4ac}{2}$.
%TODO support more commands.\"\"\"

-- The LaTeX tokenizer, parsers, interpreter, and linker.  
tokenize txt pos = 
  case String.uncons txt of
  Nothing -> [{tag=\"EOF\", pos = pos, origText = txt}]
  Just (first, rem) ->
    case first of
      \"\\\\\" -> case String.uncons rem of
        Just (\"\\\\\", rem) ->
          case extractFirstIn \"\"\"(\\\\\\\\)([\\s\\S]*)\"\"\" txt of
            Just [bs, rem] ->
              [{tag=\"newline\", pos=pos, origText = bs}, rem, pos + 2]
        _ ->
          case extractFirstIn \"\"\"^((\\\\\\w+\\b|\\\\.)\\s*)([\\s\\S]*)\"\"\" txt of
            Just [commandspace, command, remainder] ->
              [{tag=\"command\", name=command, pos=pos, origText = commandspace}, remainder, pos + String.length commandspace]
            _ ->
              [{tag=\"error\", pos=pos, value=\"Expected a command after \\\\, got \" + txt}, rem, pos + 1]
      \"%\" ->
        case extractFirstIn \"^(%(.*(?:\\r?\\n|$)))([\\\\s\\\\S]*)\" txt of
          Just [percentcomment, comment, remainder] ->
            [{tag=\"linecomment\", value=comment, pos=pos, origText=percentcomment}, remainder, pos + String.length percentcomment]  
      \"{\" ->
        [{tag=\"open\", pos=pos, origText=first}, rem, pos + 1]
      \"}\" ->
        [{tag=\"close\", pos=pos, origText=first}, rem, pos + 1]
      \"$\" ->
        [{tag=\"equationdelimiter\", pos=pos, origText=first}, rem, pos + 1]
      \"#\" ->
        case extractFirstIn \"\"\"^(#(\\d))([\\s\\S]*)\"\"\" txt of
          Just [original, integer, rem] ->
            [{tag=\"replacement\", pos = pos, nth = String.toInt integer, origText = original}, rem, pos + 2]
          _ -> 
            [{tag=\"error\", pos = pos + 1, value=\"Expected number after #\"}, rem, pos + 1]
      \"^\" ->
        [{tag=\"command\", name=\"^\", pos=pos, origText=first}, rem, pos + 1]
      \"_\" ->
        [{tag=\"command\", name=\"_\", pos=pos, origText=first}, rem, pos + 1]
      _ -> case extractFirstIn \"\"\"^(\\r?\\n\\r?\\n\\s*)([\\s\\S]*)\"\"\" txt of
        Just [rawspace, remainder] ->
          [{tag=\"newpar\", origText = rawspace}, remainder, pos + String.length rawspace]
        _ ->
          case extractFirstIn \"\"\"^((?:(?!\\r?\\n\\r?\\n)[^\\\\\\{\\}\\$%\\^_#])+)([\\s\\S]*)\"\"\" txt of
            Just [rawtext, remainder] ->
              [{tag=\"rawtext\", value=rawtext, pos = pos, origText = rawtext}, remainder, pos + String.length rawtext]
            res ->
              [{tag=\"error\", pos = pos, value=\"Expected text, got \" + txt}]

tokens txt =
  let aux txt revAcc pos =
    case tokenize txt pos of
      [{tag=\"EOF\"} as t] -> reverse (t::revAcc)
      [{tag=\"error\", pos = pos, value = value}] -> value + \" at pos \" + pos + \":\" + txt
      [t, rem, newPos] ->
        let newAcc = t::revAcc in
        aux rem newAcc newPos
  in
  aux txt [] 0

parse tokens = -- Group blocks together.
  let aux revAcc tokens =
    case tokens of
      [] -> [List.reverse revAcc, tokens]
      [{tag=\"EOF\"}] -> [List.reverse revAcc, []]
      ({tag=\"close\"} :: rem) -> 
        [List.reverse revAcc, tokens]
      (({tag=\"open\"} as x) :: rem) ->
        case aux [] rem of
          [res, rem] ->
            case rem of
              {tag=\"close\"}::r2 ->
                let newrevAcc = {tag=\"block\", children=res}::revAcc in
                aux newrevAcc r2
              _ ->
                [{tag=\"error\", value=\"Unclosed { at \" + x.pos }, []]
          x -> x
      (x :: rem) ->
        let newrevAcc = x :: revAcc in
        aux newrevAcc rem
  in nth (aux [] tokens) 0

incSectionCounter opts = 
  let newCounter = opts.sectionCounter  + freeze 1 in
  { opts |
    sectionCounter = newCounter
    subsectionCounter = 1
    currentLabelName = toString newCounter
  }

incSubsectionCounter opts = 
  let newCounter = opts.subsectionCounter + freeze 1 in
  { opts |
    subsectionCounter = newCounter
    currentLabelName = toString (opts.sectionCounter) + \".\" + toString newCounter
  }
 
htmlError help display = Html.span [[\"color\", \"red\"]] [[\"title\", help]] [
  case display of
    [] -> display
    head::tail -> display
    d -> [\"TEXT\", d]
  ]

htmlWrapper htmlArgsWrapper =
    { arity = 1
      toHtml toHtml opts args =
      let [argsHtml, newOpts] = toHtml opts args in
      [htmlArgsWrapper argsHtml, newOpts]}

htmlConst html =
    { arity = 0
      toHtml toHtml opts args = [html, opts] }

newcommandinstantiate args parsed =
  let aux parsed = case parsed of
    {tag=\"block\", children=c} ->
      { parsed | children = List.map aux c }
    {tag=\"replacement\", nth=n} ->
      if n <= len args then
        nth args (n - 1)
      else
        parsed
      aux head :: aux tail
    _ -> parsed
  in aux parsed

-- commandsDict: Dict String { arity: Int, toHTML: (Options -> Lists Tree -> HTMLNode) -> Options -> Trees -> (HTMLNode, Options)}
commandsDict = Dict.fromList [
  (\"\\\\label\",
    { arity = 1
    , toHtml toHtml opts args = case args of
      [{tag = \"block\", children = [{tag = \"rawtext\", value = v}]}] ->
        [[\"span\", [[\"id\", v],[\"class\", \"labellink\"], [\"title\", v]], [[\"TEXT\", \"🔗\"]]],
          let currentLabelName = opts.currentLabelName in
          { opts |
             labelToName = opts.labelToName |>
               Dict.insert v currentLabelName,
             nameToLabel = opts.nameToLabel |>
               Dict.insert currentLabelName v
          }
        ]
      _ ->
        let [argHtml, newOpts] = toHtml opts args in
        [htmlError \"\\\\label must be followed by a {name}\" argHtml, newOpts]
    }),
  (\"\\\\ref\",
    { arity = 1
    , toHtml toHtml opts args = case args of
      [{tag = \"block\", children = [{tag = \"rawtext\", value = v}]}] ->
        [[\"ref\", v], opts]
      _ -> 
        let [argHtml, newOpts] = toHtml opts args in
        [htmlError \"\\\\label must be followed by a {name}\" argHtml, newOpts]
    }),
  (\"\\\\LaTeX\",
    { arity = 0
    , toHtml toHtml opts arg =
      [[\"span\", [[\"class\", \"latex\"]], html \"\"\"L<sup>a</sup>T<sub>e</sub>X\"\"\"], opts]
    }),
  (\"\\\\section\", 
    { arity= 1
    , toHtml toHtml opts arg =
      let newOpts = { incSectionCounter opts | indent = False, newline = False } in
      let [argHtml, newOpts2] = toHtml newOpts arg in
      [[\"h1\", [], [[\"TEXT\", newOpts2.currentLabelName + \". \"]] ++ argHtml],
        {newOpts2 | indent = True }]
    }),
  (\"\\\\subsection\", 
    { arity= 1
    , toHtml toHtml opts arg =
      let newOpts = { incSubsectionCounter opts | indent = False, newline = False } in
      let [argHtml, newOpts2] = toHtml newOpts arg in
      [[\"h2\", [], [[\"TEXT\", newOpts2.currentLabelName + \". \"]] ++ argHtml],
        {newOpts2 | indent = True }]
    }),
  (\"\\\\textbf\", htmlWrapper (\\argsHtml -> [\"b\", [], argsHtml])),
  (\"\\\\textit\", htmlWrapper (\\argsHtml -> [\"i\", [], argsHtml])),
  (\"\\\\textsc\", htmlWrapper (\\argsHtml -> [\"span\", [[\"style\", [[\"font-variant\", \"small-caps\"]]]], argsHtml])),
  (\"\\\\ldots\", htmlConst [\"span\", [], [[\"TEXT\", \"…\"]]]),
  (\"\\\\textbackslash\", htmlConst [\"span\", [], [[\"TEXT\", \"\\\\\"]]]),
  (\"\\\\newcommand\",
    let extractCommand block = case block of
      {tag=\"command\", name=cmdName} -> {value = cmdName}
      {tag=\"block\", children=[{tag=\"command\", name=cmdName}]} -> {value = cmdName}
      _ -> {}
    in
    { inspect rightArgs = -- Returns the arguments to the command and the remaining that it does not parse.
        case rightArgs of
          cmdOpt::rem ->
            case extractCommand cmdOpt of
              {value= cmdName} -> case rem of
                ({tag=\"rawtext\", value=text} :: definition :: rem) ->
                  case extractFirstIn \"\"\"\\[(\\d+)\\]\"\"\" text of
                    Just [d] -> [[cmdName, String.toInt d, definition], rem]
                    _ -> [[\"Expected [number] for the number of arguments, got \" + text], rightArgs]
                (definition :: rem) ->
                  [[cmdName, 0, definition], rem]
                _ -> [[\"Expected \\\\newcommand{\"+cmdName+\"}[optional num args]{definition}\"], rightArgs]
              _ ->  [[\"No command name after \\\\newcommand, from \" + Debug.log \"\" cmdOpt], rightArgs]
          _ ->  [[\"Expacted a command name after \\\\newcommand, from \" + Debug.log \"\" cmdOpt], rightArgs]
    , toHtml toHtml opts args =
        if len args == 1 then [htmlError (nth args 0) \"???\", opts] else (
        let [cmdName, arity, definition] = args in
        let newOpts = { opts |
          customCommands = opts.customCommands |> Dict.insert cmdName {
              arity = arity,
              toHtml toHtml opts args =
                -- Perform the replacement of #1 with argument 1, #2 with argument 2, and so on.
                -- For now, just output the definition.
                let instantiatedDefinition = newcommandinstantiate args definition in
                case toHtml opts [instantiatedDefinition] of
                  [[argHtml], newOpts] ->
                    [argHtml, newOpts]
                  [argsHtml, newOpts] -> error <| \"command \" + cmdName + \" returned more than 1 arg:\" + toString argsHtml
            } }
        in
        [[\"span\", [[\"class\", \"newcommand\"]], []], newOpts]
      )
    }),
  (\"\\\\frac\",
  { arity = 2
  , toHtml toHtml opts arg =
      if opts.mathmode then
        let [arg1html, newOpts1] = toHtml opts     [nth arg 0] in
        let [arg2html, newOpts2] = toHtml newOpts1 [nth arg 1] in
        [[\"div\", [[\"class\", \"fraction\"]], [
         [\"span\", [[\"class\", \"fup\"]], arg1html],
         [\"span\", [[\"class\", \"bar\"]], [[\"TEXT\", \"/\"]]],
         [\"span\", [[\"class\", \"fdn\"]], arg2html]]], newOpts2]
      else
        [htmlError \"\\\\frac allowed only in math mode\" \"???\", opts]
  }),
  (\"\\\\_\", htmlConst [\"span\", [], [[\"TEXT\", \"_\"]]]),
  (\"_\",
  { arity = 1
  , toHtml toHtml opts args =
    if opts.mathmode then
      let [arghtml, newOpts] = toHtml opts args in
      [[\"sub\", [], arghtml], newOpts]
    else
      [htmlError \"_ allowed only in math mode\" \"???\", opts]
  }),
  (\"^\",
  { arity = 1
  , toHtml toHtml opts args =
    if opts.mathmode then
      let [arghtml, newOpts] = toHtml opts args in
      [[\"sup\", [], arghtml], newOpts]
    else
      [htmlError \"^ allowed only in math mode\" \"???\", opts]
  })]

commands x =
  Dict.get x commandsDict |> Maybe.withDefaultLazy (\\_ ->
    { arity = 0
    , toHtml toHtml opts arg =    
      [htmlError \"Unknown Command\" x, opts]})

indent opts = if opts.indent then [[\"span\", [[\"class\", \"paraindent\"]], html \"&nbsp;\"]] else []

newline opts = if opts.newline then [Html.br] else []

splitargs n array =
  let aux revAcc n array =
    if n == 0 then [reverse revAcc, array] else
    case array of
      {tag=\"rawtext\", value=text, pos = pos}:: rem ->
        case extractFirstIn \"\"\"^\\s*(\\S)(.*)\"\"\" text of
          Just [arg, other] ->
            let newAcc = {tag=\"rawtext\", value=arg, pos = pos}::revAcc in
            let newN = n - 1 in
            let newArray = {tag=\"rawtext\", value=other, pos = pos + String.length arg} :: rem in
            aux newAcc newN newArray
          _ ->
            aux revAcc n rem
      (head :: rem) ->
        let newAcc = head::revAcc in
        let newN = n - 1 in
        aux newAcc newN rem
      [] ->
        [[], array]
  in aux [] n array

escape txt = txt |>
  replaceAllIn \"\\\\\\\\\" \"\\\\textbackslash{}\" |>
  replaceAllIn \"%(\\\\w+) (\\\\w+)\" (\\{group=[_, a, b]} -> \"\\\\\" + a + \"{\" + b  + \"}\") |>
  replaceAllIn \"<[bB]>\" (\\_ -> \"\\\\textbf{\") |>
  replaceAllIn \"<[iI]>\" (\\_ -> \"\\\\textit{\") |>
  replaceAllIn \"</[bBiI]>\" (\\_ -> \"}\")

toHtmlWithoutRefs opts tree =
  let aux opts revAcc tree = case tree of
    [] -> [List.reverse revAcc, opts]
    (head::rem) -> case head of
      {tag=\"block\", children} ->
        let newTree = children ++ rem in
        aux opts revAcc newTree
      {tag=\"rawtext\", value=text, pos = pos} ->
        let finalText = {
           apply x = x,
           update {input, oldOutput, newOutput, diffs} = 
             Ok (Inputs [Update.mapInserted escape newOutput diffs])
          }.apply text in
        if opts.indent && Regex.matchIn \"\"\"^[\\s]*\\S\"\"\" text then
          let newOpts = { opts | indent = False,  newline = False } in
          let revAccWithParagraph = List.reverseInsert (newline opts ++ indent opts) revAcc in
          let newrevAcc = [\"span\", [[\"start\", toString pos]], [[\"TEXT\",finalText]]]::revAccWithParagraph in
          aux newOpts newrevAcc rem
        else
          let newrevAcc = [\"span\", [[\"start\", toString pos]], [[\"TEXT\",finalText]]]::revAcc in
          aux opts newrevAcc rem
      {tag=\"newpar\"} ->
        let newOpts = { opts | indent = True, newline = True} in
        aux newOpts revAcc rem
      {tag=\"equationdelimiter\"} -> -- Todo: Group the equation into an inline span?
        let newOpts = { opts | mathmode = if opts.mathmode then False else True } in
        aux newOpts revAcc rem
      {tag=\"replacement\", nth=n} -> -- Create a dummy tag that can be later replaced.
        let newrevAcc = [\"span\", [[\"class\", \"replacement\"]], [n]]::revAcc in
        aux opts newrevAcc rem
      {tag=\"command\", name=cmdname} ->
        let tmpOpt = if cmdname == \"\"\"\\noindent\"\"\" then { opts | indent = False } else opts in
        -- TODO: Need to not convert to html, but expand the command first.
        let cmddef = Dict.get cmdname tmpOpt.customCommands |> Maybe.withDefaultLazy (\\_ ->
          commands cmdname) in
        let [args, remainder] = case cmddef of
          {arity=n} ->
            splitargs n rem
          {inspect} ->
            inspect rem
        in
        let [toAdd, newOpts] = cmddef.toHtml toHtmlWithoutRefs tmpOpt args in
        let newrevAcc = toAdd::revAcc in
        aux newOpts newrevAcc remainder
      
      {tag=\"linecomment\", value} ->
        let newrevAcc = [\"span\", [[\"styles\", [[\"background\",\"#888\"], [\"color\", \"#FFF\"]]]], [[\"TEXT\", \"(\" + value + \")\"]]]::revAcc in
        aux opts newrevAcc rem
  in 
  aux opts [] tree

initOptions = {
  indent = False
  newline = False
  customCommands = Dict.fromList []
  currentLabelName = freeze \"0\"
  sectionCounter = freeze 0
  subsectionCounter = freeze 0
  mathmode = False
  labelToName = Dict.fromList []
  nameToLabel = Dict.fromList []
}

htmlMapOf htmlOf trees = case trees of
  [] -> \"\"
  (head::tail) -> htmlOf head + htmlMapOf htmlOf tail

htmlOf text_tree = case text_tree of
  [\"TEXT\", value] -> -- Needs some escape here.
    value 
  [m, _, children] -> \"<\"+m+\">\" + htmlMapOf htmlOf children + \"</\"+m+\">\"

toHtml x =
  let [raw, opts] = toHtmlWithoutRefs initOptions x in
  let replaceMap replaceReferences trees = case trees of
    [] -> freeze []
    (head :: tail) -> {
        apply x = x
        update {input, outputNew, outputOriginal, diffs} =
          if (len outputNew /= len outputOriginal && len outputOriginal == 1) then
            Ok (Inputs [[[\"TEXT\", htmlMapOf htmlOf outputNew outputNew]]]) else Ok (InputsWithDiffs [(outputNew, Just diffs)])
      }.apply [replaceReferences head] ++ replaceMap replaceReferences tail
  in
  let replaceReferences tree = case tree of
    [\"ref\", refname] -> Dict.get refname opts.labelToName  |> case of
      Nothing -> htmlError (\"Reference \" + refname + \" not found.\") \"???\"
      Just txt ->
        let replaceKey refNameTxt = {
           apply (refname,txt) = txt,
           update {input=(oldRefname, oldTxt), outputNew=newText} =  -- Lookup for the reference in the options.
             case Dict.get newText opts.nameToLabel of
              Just newRefname ->
                Ok (Inputs [(newRefname, oldTxt)])
              _ -> -- No solution, cancel update.
                Err (\"could not find reference\" + toString newText)
          }.apply refNameTxt
        in
        [\"span\", [
          [\"class\",\"reference\"],
          [\"onclick\", \"if(window.location.hash=='') window.location.hash = '\" + refname + \"';\"],
          [\"title\", refname]
          ], [[\"TEXT\", replaceKey (refname, txt)]]]
    [tag, attrs, c] -> [tag, attrs, replaceMap replaceReferences c]
    _ -> tree
  in
  replaceMap replaceReferences raw

latex2html latex = 
  { apply (f, latex) = f latex,
    update {input = (f, latex), outputOld, outputNew, diffs = (VListDiffs ldiffs) as diffs} = 
      let gatherDiffsChild gatherDiffs i cOld cNew childDiffs = case childDiffs of
        [] -> Ok [[]]
        ((j, ListElemUpdate d) :: diffTail) ->
          if j > i then
            gatherDiffsChild gatherDiffs j (LensLess.List.drop (j - i) cOld) (LensLess.List.drop (j - i) cNew) childDiffs
          else
            case (cOld, cNew) of
              (oldHead::oldTail, newHead::newTail) ->
                let subdiffs = gatherDiffs oldHead newHead d in
                subdiffs |> LensLess.Results.andThen (\\replacements ->
                  gatherDiffsChild gatherDiffs (i + 1) oldTail newTail diffTail |> LensLess.Results.map (\\replacementTails ->
                    replacements ++ replacementTails
                  )
                )
              _ -> error \"Unexpected size of cOld and cNew\"
        ((j, subdiff)::diffTail) -> Err (\"Insertion or deletions, cannot short-circuit at \" + toString j + \", \" + toString subdiff)
      in
      let gatherDiffs outputOld outputNew diffs = case (outputOld, outputNew, diffs) of
        ([\"span\", [[\"start\", p]], [[\"TEXT\", vOld]]], 
         [\"span\", [[\"start\", p]], [[\"TEXT\", vNew]]],
          VListDiffs [(2, ListElemUpdate (VListDiffs [(0, ListElemUpdate (VListDiffs [(1, ListElemUpdate sd)]))]))]) -> 
           Ok [[(Debug.log (\"escaped string '\" + vNew + \"' with diffs \" + toString sd) <| Update.mapInserted escape vNew sd, String.toInt p, String.toInt p + String.length vOld)]]
        ([_, _, cOld], [_, _, cNew], VListDiffs [(2, ListElemUpdate (VListDiffs childDiffs))]) ->
           gatherDiffsChild gatherDiffs 0 cOld cNew childDiffs
        _ -> Err (\"Could not find text differences \" + toString (outputOld, outputNew, diffs))
      in
      case gatherDiffsChild gatherDiffs 0 outputOld outputNew ldiffs of
        Ok [replacements] ->
          let newLatex = foldl (\\(newValue, start, end) acc ->
            String.substring 0 start acc + newValue + String.drop end acc
          ) latex replacements in
          Update.diff latex newLatex
          |> Result.map (\\mbDiff ->
            let newDiff = Maybe.map (\\d -> VRecordDiffs { _2 = d})  mbDiff in
            (InputsWithDiffs [((f, newLatex), newDiff)])
          )
        Err msg ->
          Update.updateApp {
            fun (f, x) = f x, input = (f, latex), outputOld = outputOld, output = outputNew, diffs = diffs
          }
  }.apply (\\x -> toHtml <| parse <| tokens x, latex)

Html.forceRefresh <|
<span style=\"margin:10px\">
<style type=\"text/css\">
#content {
  font-family: 'Computer Modern Serif';
}
#content h1 {
  font-size: 24px;
  margin-top: 10px;
}
#content h2 {
  font-size: 18px;
  margin-top: 10px;
}

.tex sub, .latex sub, .latex sup {
  text-transform: uppercase;
}

.tex sub, .latex sub {
  vertical-align: 0.3em;
  margin-left: -0.1667em;
  margin-right: -0.125em;
}

.tex, .latex, .tex sub, .latex sub {
  font-size: 1em;
}

.latex sup {
  font-size: 0.85em;
  vertical-align: -0.3em;
  margin-left: -0.36em;
  margin-right: -0.15em;
}
.fraction {
  display: inline-block;
  position: relative;
  vertical-align: middle; 
  letter-spacing: 0.001em;
  text-align: center;
  font-size: 12px;
  }
.fraction > span { 
  display: block; 
  padding: 0.1em; 
  }
.fraction span.fdn {border-top: thin solid black;}
.fraction span.bar {display: none;}
latex-sc {
  font-variant: small-caps;
}
.labellink {
  color: #CCC;
  font-size: 0.5em;
  opacity: 0.3;
}
.labellink:hover {
  color: blue;
  opacity: 1;
}
</style>
<textarea
   style=\"font-family:monospace;width:100%;min-height:200px\"
   onchange=\"this.textContent = this.value\"
   onkeyup=\"\"\"if(typeof timer != \"undefined\") clearTimeout(timer); timer = setTimeout((function(self){ return function() { self.textContent = self.value; } })(this), 2000);\"\"\"
>@latex</textarea>
<span>
  <button type=\"button\" class=\"btn btn-default btn-sm\" onclick=\"document.execCommand( 'bold',false,null)\" contenteditable=\"false\">
    <span class=\"glyphicon glyphicon-bold\"></span> Bold
  </button><button type=\"button\" class=\"btn btn-default btn-sm\" onclick=\"document.execCommand('italic',false,null);\" contenteditable=\"false\">
    <span class=\"glyphicon glyphicon-italic\"></span> Italic
  </button>
  <br>
  <div style=\"display:inline-block\" id=\"content\">
  @(latex2html latex)</div></span></span>
"""

fromleo_dixit =
 """--# updatedelay: 0

{select} = {
  select attributes strArray defaultSelected =
    <select @attributes>@(List.indexedMap (\\i s ->
      <option @(if i == defaultSelected then [[\"selected\", \"selected\"]] else [])>@s</option>
      )  strArray)
    </select>
}

-- Each element of betselfs is a 2-element array containing the best and the own card number
players = [
  {name=\"John\", betselfs=[], scores=[]}, 
  {name=\"Nick\", betselfs=[], scores=[]}, 
  {name=\"Pete\", betselfs=[], scores=[]}
]

displayreset = False
removeScores players =
  List.map (\\p -> {p | betselfs=[], scores=[]}) players

mkError txt = Html.span [[\"color\", \"lightgray\"]] [] (Html.text txt)

cartesDisponibles = List.range 1 (List.length players)

currentRound = List.length (nth players 0).scores

remainingbets = (List.length players - 1) - List.sum (List.map (\\j ->  if (List.length j.betselfs) == currentRound then 0 else 1) players)
allbetsdone = remainingbets == 0

scoreIfEverybodyFound = 2
scoreIfNobodyFound = 3

id = {
  currentPlayer = \"currentplayer\"
  tellPlayer = \"tellplayers\"
  cardNumber = \"cardnumberselect\"
  chooseCallback = \"chooseCard\"
  chooseCardError = \"chooseCardError\"
  betNumber = \"betnumber\"
  betNumberChoose = \"betnumberchoose\"
  betNumberNotSame = \"betnumbersame\"
  chooseBetCallback = \"selectBet\"
  buttonConfirm = \"confirmButton\"
  displayConfirmButtonCallback = \"confirmButtonCallback\"
  clickbuttoncallback = \"clickConfirmButtonCallback\"
  commitresult = \"resultsender\"
  foranotherplayer = \"foranotherplayer\"
  forcurrentplayer = \"forcurrentplayer\"
  addAnotherPlayerBet = \"addAnotherPlayerBet\"
}

commitPlayer players outputNew =
  case evaluate outputNew of
    (playerNum, card, bet) ->
      (List.take playerNum players) ++ (case List.drop playerNum players of
        player::otherPlayers ->
          {player | betselfs = player.betselfs ++ [[bet, card]]}::otherPlayers
        [] -> []
      )
    _ -> players

betself =
  <span id=\"betself\">@Html.forceRefresh<|<script>
function setTransientVisibility(element, condition) {
  element.setAttribute(\"transient-visible\", condition ? \"true\" : \"false\");
}
function fromId(id) {
  return document.querySelector(\"#\" + id);
}
function @(id.chooseCallback)() {
  var player = fromId(\"@id.currentPlayer\");
  var tellplayers = fromId(\"@id.tellPlayer\");
  if(player !== null && tellplayers !== null) {
    for(var i = 0; i < tellplayers.children.length; i++) {
      setTransientVisibility(tellplayers.children[i], player.selectedIndex == i);
    }
  }
  var card   = fromId(\"@id.cardNumber\");
  var bet    = fromId(\"@id.betNumber\");
  var card_hint = fromId(\"@id.chooseCardError\");
  var bet_hint  = fromId(\"@id.betNumberChoose\");
  var bet_error = fromId(\"@id.betNumberNotSame\");
  var button    = fromId(\"@id.buttonConfirm\");
  if(player !== null && card !== null && bet !== null && card_hint !== null && bet_hint !== null && bet_error !== null && button !== null) {
    setTransientVisibility(card_hint, card.selectedIndex == 0);
    setTransientVisibility(bet_hint, bet.selectedIndex == 0);
    setTransientVisibility(bet_error, bet.selectedIndex == card.selectedIndex && bet.selectedIndex > 0);
    setTransientVisibility(button, card.selectedIndex > 0 && bet.selectedIndex > 0 && bet.selectedIndex !== card.selectedIndex);
  }
}
function @(id.clickbuttoncallback)() {
  // Commits the bets for the given player.
  var card = fromId(\"@id.cardNumber\");
  var bet = fromId(\"@id.betNumber\");
  var player = fromId(\"@id.currentPlayer\");
  var c = fromId(\"@id.commitresult\");
  c.setAttribute(\"v\", \"(\" + player.selectedIndex + \",\" + card.selectedIndex + \",\" + bet.selectedIndex + \")\");
  var d = fromId(\"@id.forcurrentplayer\");
  var e = fromId(\"@id.foranotherplayer\");
  setTransientVisibility(d, false);
  setTransientVisibility(e, true);
}

function @(id.addAnotherPlayerBet)() {
  var d = fromId(\"@id.forcurrentplayer\");
  var e = fromId(\"@id.foranotherplayer\");
  setTransientVisibility(d, true);
  setTransientVisibility(e, false);
  var card = fromId(\"@id.cardNumber\");
  var bet = fromId(\"@id.betNumber\");
  var player = fromId(\"@id.currentPlayer\");
  if(card !== null) card.selectedIndex = 0;
  if(bet !== null) bet.selectedIndex = 0;
  if(player !== null) player.selectedIndex = (player.selectedIndex + 1) % player.children.length;
  @(id.chooseCallback)();
}
</script><style>
  .normallyNotVisible {
    display: none;
  }
  .normallyNotVisible[transient-visible=\"true\"] {
    display: inline-block;
  }
  .normallyVisible[transient-visible=\"false\"] {
    display: none;
  }
  </style
  ><span id=id.foranotherplayer class=\"normallyNotVisible\">
    <button onclick=\"\"\"@id.addAnotherPlayerBet()\"\"\" title=\"Let another player place a bet on this screen\">Place another bet</button>
  </span
  ><span id=id.forcurrentplayer class=\"normallyVisible\">Who is currently placing a bet? 
  @(select [[\"id\", id.currentPlayer], [\"onchange\", \"\"\"@id.chooseCallback()\"\"\"]] (map (\\j ->  j.name) players) 0)
  <br>
  <span id=id.tellPlayer>@(List.map (\\p -> <span class=\"normallyNotVisible\">@p.name it's your turn to bet!</span>) players)</span>
<br>Your card:
  @(select [[\"id\", id.cardNumber], [\"onchange\", \"\"\"@id.chooseCallback()\"\"\"]] 
      (\"Choose your card number...\" :: map (\\x -> toString x) cartesDisponibles) 0)
  <span class=\"normallyNotVisible\" id=id.chooseCardError> Indicate what is your card. This is confidential.</span>
<br>Your bet:
  @(select [[\"id\", id.betNumber], [\"onchange\", \"\"\"@id.chooseCallback()\"\"\"]] 
      (\"You bet that the correct card is...\" :: map (\\x -> toString x) cartesDisponibles) 0)
  <span class=\"normallyNotVisible\" id=id.betNumberChoose>What card do you think is the dealer's one.</span>
  <span class=\"normallyNotVisible\" id=id.betNumberNotSame>You cannot bet on your own card.</span>
<br>
  <button
    class=\"normallyNotVisible\"
    id=id.buttonConfirm
    title=\"By clicking this button, I confirm I know the rules of the game\"
    onclick=\"\"\"@id.clickbuttoncallback()\"\"\"
  >I confirm by bet</button>
   <span
    id=id.commitresult
    class=\"normallyNotVisible\"
    v=(Html.onChangeAttribute players commitPlayer)>
    </span>
    <script>
      @(id.chooseCallback)();
    </script>
  </span>
</span>

nomDe j = Html.li [] [] [Html.textNode j.name]
playersnoms = List.map nomDe players
playersEnCours = List.filter (\\j ->  List.length j.betselfs == currentRound ) players
playerFromName name =
  nth (List.filter (\\j ->  j.name == name) players) 0
playerIndexFromName name =
  letrec aux i = 
    if (nth players i).name == name then i else
    if i >= List.length players then -1 else
    aux (i + 1) in aux 0

<div style=\"margin:20px\">
  <span> 
  <img style=\"width:50%\" src=\"https://images-cdn.asmodee.us/filer_public/9e/7e/9e7ea2a6-d531-4f3a-b984-4119925d4c9f/dix01_feature_c85e1c.png\">
  <img style=\"float:right;width:50%;\" src=\"https://cf.geekdo-images.com/large/img/QFUbpIeEFamJbgJ_Bs5ejDtF8UA=/fit-in/1024x1024/filters:no_upscale()/pic1003159.jpg\">
  <div style=\"float:right\">@(Html.button \"Reset\" \"Display the reset button\" displayreset (\\x -> if x then False else True))
  @(if displayreset then Html.button \"Reset scores\" \"Click here to reset scores\" (displayreset, players) (\\(_, oldPlayers) -> (False, removeScores oldPlayers)) else <span></span>)
  </div>
  <h1>Dixit Score sheet</h1>,
  To play to Dixit, please enter below the name of the @(List.length players) players. You can add or remove players.
  <ul>@(map nomDe players)</ul>
  It's turn number @(currentRound+1)<br>
  Current scores:<br>
  <table>
  <tr><th>Name</th><th>Score</th>@(if currentRound >= 2 then <th style=\"font-weight:normal\"><i>by round</i>:</th> else [])@(List.map (\\i -> <th>#@i</th>) (List.range 1 (if currentRound >= 2 then currentRound else 0)))</tr>
  @(List.map (\\j -> 
    <tr>
      <td style=\"\"\"color:@(if len j.betselfs > currentRound then \"green\" else \"black\")\"\"\">@(j.name)</td>
      <td style=\"text-align:center\">@(toString (Update.freeze (List.sum (j.scores))))</td>
      @(if currentRound < 2 then [] else [<td></td>] ++
      List.map (\\(i, [b,s]) -> <td>@(Update.freeze i)@(if b==0 then \"*\" else \"\")</td>) (zip j.scores j.betselfs))
      </tr>) players)</table>
  @(if remainingbets > 0 then betself else 
  let playersWithIndex = zipWithIndex players in
  let dealerIndex = List.filter (\\(index, j) -> List.length j.betselfs == currentRound) playersWithIndex |> flip nth 0 |> Tuple.first in
  let totalNumCards = List.sum cartesDisponibles in
  let totalInCards = List.sum (List.map (\\j ->  if List.length j.betselfs == currentRound + 1 then nth (nth j.betselfs currentRound) 1 else 0) players) in
  let correctCard = totalNumCards - totalInCards in
  let guessedOk = players |> List.filterMap 
       (\\j ->  if List.length j.betselfs == currentRound + 1 then if (j.betselfs |> flip nth currentRound |> flip nth 0) == correctCard then Just j.name else Nothing else Nothing) in
  let nGuessedOk = List.length guessedOk in
  let manyguessed = if nGuessedOk > 1 then \"\" else if nGuessedOk == 1 then \" is the only one to have\" else \"Nobody\" in
  let playersAyantVotePour nCarte = List.sum (List.map (\\(i, j) ->  if i == dealerIndex then 0 else if nth (nth j.betselfs currentRound) 0 == nCarte then 1 else 0) playersWithIndex) in
  let playersAyantVotePourEux = flip List.concatMap playersWithIndex <|
    \\(i, j) -> 
      if i == dealerIndex then [] else
      let ayantparie = playersWithIndex |>
        List.filterMap (\\(i2, j2) -> if i2 == dealerIndex then Nothing else if nth (nth j.betselfs currentRound) 1 == nth (nth j2.betselfs currentRound) 0 then Just j2.name else Nothing) |>
        String.join \", \" in
      if ayantparie == \"\" then [] else
      [<br>, textNode <| j.name + \" made \" + ayantparie + \" to believe it was the card of \" + j.name]
  in
  let playersWithNewScores = playersWithIndex |>
    List.map (\\(index, player) ->
    (player,
       if index == dealerIndex then
         if nGuessedOk == 0 || nGuessedOk == List.length players - 1 then 0 else 3
       else (
         let [bet, self] = nth player.betselfs currentRound in
         (if nGuessedOk == 0 || nGuessedOk == List.length players - 1 then scoreIfEverybodyFound else (
           if bet == correctCard then scoreIfNobodyFound else 0)) +
           playersAyantVotePour self)))
  in
  <div>
     The bets are done! Here are the results of this turn:<br>
     The dealer was @((nth players dealerIndex).name), the dealer's card was the number @correctCard<br>
     @(String.join \",\" guessedOk)@manyguessed guessed it!
     <div>@(playersAyantVotePourEux)</div><br>
     <div>@(List.concatMap (\\(j, score) -> 
       [<span>@(j.name): +@score</span>, <br>]) playersWithNewScores)</div>
     @(Html.button \"Next turn\" \"Passer au tour suivant\" players <| \\oldplayers ->
          List.map (\\j -> 
                let ({betselfs=b, scores=s} as player, increment) = j in
                { player |
                  betselfs = if List.length b <= currentRound then b ++ [[0, 0]] else b
                  scores = s ++ [increment] }) playersWithNewScores
     )
  </div>)
  </span>
</div>
"""

christmas_song_3_after_translation =
 """content = \"\"\"
<h1>$phrase15</h1>
<h3>$Marion ($airdesoncouplet):</h3>
$phrase1<br>
$phrase2<br>
$phrase3 <br>
$phrase4<br>
$phrase5.
<h3>$Mikal ($airdesoncouplet):</h3>$phrase7
<h3>$Ensemble ($Mikal $airdesonrefrain, $Marion $phrase6)</h3>$phrase8
<table style=\"\\n  width:100%; \\n:\"><tbody>
<tr><td style=\"\\n  width:50%; \\n:\">
<h3>$Mikal ($airdesoncouplet)</h3>
$phrase10
</td><td style=\"\\n  width:50%; \\n:\">
<h3>$Marion ($airdesoncouplet)</h3>
$phrase1,<br>
$phrase2<br>
$phrase3 <br>
$phrase4<br>
$phrase5
</td></tr></tbody></table>
<table style=\"\\n  width:100%; \\n:\"><tbody>
<tr><td style=\"\\n  width:50%; \\n:\">
<h3>$Mikal ($airdesonrefrain)</h3>$phrase8</td>
<td style=\"\\n  width:50%; \\n:\">
<h3>$Marion ($airdesonrefrain)</h3>
$phrase9</td></tr></tbody></table>
<h3>$Ensemble ($Marion $airdesoncouplet, $Mikal $phrase6):</h3>
$phrase11
<h3>$Ensemble ($Mikal $airdesoncouplet, $Marion $airdesoncouplet)</h3>
$phrase12
<table style=\"\\n  width:100%; \\n:\"><tbody>
<tr><td style=\"\\n  width:50%; \\n:\">
<h3>$Mikal ($airdesonrefrain)</h3>$phrase8</td>
<td style=\"\\n  width:50%; \\n:\">
<h3>$Marion ($airdesonrefrain)</h3>
$phrase9
</td></tr></tbody></table>
<h3>$Mikal</h3>
$phrase13
<h3>$Marion</h3>
$phrase14\"\"\"

translations =
  [(\"English\", 
    [( \"Marion\", \"Marion\"),
     ( \"Mikal\", \"Mikaël\"),
     ( \"airdesoncouplet\", \"tune of verse\"),
     ( \"Ensemble\", \"Together\"),
     ( \"airdesonrefrain\", \"tune of chorus\"),
     ( \"phrase1\", \"What Child is, who, laid to rest\"),
     ( \"phrase2\", \"On Mary’s lap is sleeping Whom\"),
     ( \"phrase3\", \"angels greet with\"),
     ( \"phrase4\", \"anthems weet, while shep-\"),
     ( \"phrase5\", \"herds watch are keeping\"),
     ( \"phrase6\", \"2<sup>nd</sup> voice\"),
     ( \"phrase7\", \"Helpless and hungry, lowly, afraid,<br>wrapped in the chill of mid winter<br>comes now among us<br>born into poverty’s embrace,<br>new life for the world\"),
     ( \"phrase8\", \"Who is this who lives with the lowly, sharing<br>their sorrows, knowing their hunger?<br>This is Christ, revealed<br>to the world in the eyes of a child,<br>a child of the poor\"),
     ( \"phrase9\", \"This, this is the Christ the King<br>Whom shepherds guard and angels sing<br>Haste, haste<br>to bring him laud, the babe,<br>the son of Mary\"),
     ( \"phrase10\", \"Who is the stranger here in our midst,<br>Looking for shelter among us<br>Who is the outcast?<br>Who do we see amid the poor,<br>The children of God?<br>\"),
     ( \"phrase11\", \"So bring him incense, gold, and myrrh,<br>Come peasant, king, to own him;<br>The King of Kings salvation brings,<br>Let loving hearts enthrone him.\"),
     ( \"phrase12\", \"So bring all the thirsty all who seek peace;<br>bring those with nothing to offer.<br>Strengthen the feeble,<br>  ($Mikal) say to the frightened heart: “Fear not here is your God!”<br>  ($Marion) say to them, in time ''Fear not, Fear not''\"),
     ( \"phrase13\", \"a child of the poor\"),
     ( \"phrase14\", \"the babe, the son of Mary\"),
     ( \"phrase15\", \"What Child is This / Child of the Poor\")])
  ,(\"Français\", 
    [( \"Marion\", \"Marion\"),
     ( \"Mikal\", \"Mikaël\"),
     ( \"airdesoncouplet\", \"air de son couplet\"),
     ( \"Ensemble\", \"Ensemble\"),
     ( \"airdesonrefrain\", \"air de son refrain\"),
     ( \"phrase1\", \"<i>Quel</i> est l'enfant, qui est né ce soir\"),
     ( \"phrase2\", \"Inconnu des gens de la terre ?\"),
     ( \"phrase3\", \"<i>Quel</i> est l'enfant, qui est\"),
     ( \"phrase4\", \"né ce soir, Que les pauvres\"),
     ( \"phrase5\", \"ont voulu recevoir\"),
     ( \"phrase6\", \"double\"),
     ( \"phrase7\", \"Sans aucune défense, hors d'une cité,<br>enveloppé dans un lange<br>il vient parmi nous<br>pour nous sauver de nos péchés,<br>annoncent les anges\"),
     ( \"phrase8\", \"<i>Qui est</i> celui qui va vers les faibles, souffrant<br>leurs chagrins, et vivant leur faim?<br><i>C'est le</i> Christ, qui vient<br>dans le monde sous les traits d'un enfant,<br>un enfant des pauvres\"),
     ( \"phrase9\", \"Il suffit d'un enfant ce soir<br>Pour unir le ciel et la terre<br>Il suffit<br>d'un enfant ce soir, Pour changer<br>notre vie en espoir.\"),
     ( \"phrase10\", \"Qui est l'étranger, présent si tard,<br>Cherchant abri par ici<br>Qui est le paria?<br>Qui est-ce qu'on voit parmi les pauvres,<br>les enfants de Dieu?<br>\"),
     ( \"phrase11\", \"Quel est l'Enfant qui est né ce soir<br>Pour changer la nuit en lumière ?<br>Quel est l'Enfant qui est né ce soir<br>Tout joyeux comme un feu dans le noir ?\"),
     ( \"phrase12\", \"<i>Ap</i>portez les malades, <i>les</i> affligés;<br>venez ceux qui n'ont rien à donner.<br>Fortifiez le faible,<br>  ($Mikal) dites à l'appeuré: ''N'aie pas peur, voici ton Dieu!''<br>  ($Marion) et dites à chacun ''N'aie pas peur, n'aie pas peur''\"),
     ( \"phrase13\", \"un enfant des pauvres\"),
     ( \"phrase14\", \" changer notre vie en espoir\"),
     ( \"phrase15\", \"Quel est l'Enfant / L'Enfant des pauvres\")])
  ]

allTranslationDicts = List.map (Tuple.mapSecond Dict.fromList) translations
  
translationsLangDict = Dict.fromList allTranslationDicts
languages = List.map Tuple.first translations
languageIndex = 0
language = nth languages languageIndex
currentTranslation = Dict.apply translationsLangDict language

highlighttranslations = False

replaceVariables translationDict string =
  Regex.replace \"\\\\$(\\\\w+|\\\\$)\" (\\m -> 
    if m.match == \"$\" then m.match else
    let key = nth m.group 1 in
    case Dict.get key translationDict of
      Nothing -> m.match
      Just definition -> 
        let finaldefinition = 
          if highlighttranslations then
            \"\"\"<span style='outline:lightgreen 2px solid;' title='@key'>@definition</span>\"\"\"
          else definition
       in
       replaceVariables (Dict.remove key translationDict) finaldefinition
  ) string

freshVarName name i dictionary =
  if Dict.member (name + (if i == 0 then \"\" else toString i)) dictionary
  then freshVarName name (i + 1) (dictionary) else name + (if i == 0 then \"\" else toString i)
  
content = content |> replaceVariables currentTranslation |>
  (\\x ->
    { apply (x, _) = x
      update {input = (x, allTranslationDicts), newOutput} =
        Regex.find \"\\\\{([^\\\\}]*(?!\\\\})\\\\S[^\\\\}]*)\\\\}\" newOutput |>
        List.foldl (\\(_::definition::_) (newOutput, currentTranslation, allTranslationDicts) ->
            --let basename = Regex.replace \"[^\\\\w]\" \"\" definition in 
            let name = freshVarName \"phrase\" 1 currentTranslation in
            let textToFind = \"\\\\{\" + Regex.replace \"\\\\\\\\|\\\\{|\\\\}|\\\\[|\\\\]|\\\\$|\\\\.|\\\\?|\\\\+\" (\\m -> \"\\\\\" + m.match) definition + \"\\\\}\" in
            (Regex.replace textToFind (\\_ -> \"$\" + name) newOutput,
             Dict.insert name definition currentTranslation,
             List.map (\\(lang, d) -> (lang, Dict.insert name definition d)) allTranslationDicts)
        ) (newOutput, currentTranslation, allTranslationDicts) |> \\(newOutput, _, newTranslationsLangDict) ->
          Ok (Inputs [(newOutput, newTranslationsLangDict)])
    }.apply (x, allTranslationDicts)
  )

alltranslationsLangDict = Dict.fromList translations
addLang alltranslationsLangDict = {
  apply alltranslationsLangDict = \"\"
  update {input, outputNew} =
    if not (outputNew == \"\") && not (Dict.member outputNew alltranslationsLangDict) then 
      let toCopy = Dict.apply alltranslationsLangDict language in
      Ok (InputsWithDiffs [(Dict.insert outputNew toCopy alltranslationsLangDict,
                           Just (VDictDiffs (Dict.fromList [(outputNew, VDictElemInsert)])))])
    else
      Ok (InputsWithDiffs [(input, Nothing)])
  }.apply alltranslationsLangDict

main = 
  Html.forceRefresh <|
  Html.div [[\"cursor\", \"text\"]] []
    [ Html.div [[\"border\", \"4px solid black\"], [\"padding\", \"20px\"]] [] <|
        [Html.span [] [] <|
          Html.select [] languages languageIndex ::
          [\"input\", [[\"style\", [[\"margin-left\", \"10px\"]]], [\"type\",\"text\"], [\"v\", addLang alltranslationsLangDict],
            [\"placeholder\", \"New Language (e.g. German)\"], [\"title\", \"Enter the name of a language here and press ENTER\"],
            [\"onchange\",\"this.setAttribute('v',this.value)\"]], []] ::
          Html.checkbox \"Highlights\" \"Highlight translatable text\" highlighttranslations ::
          [\"br\", [], []] ::
          Html.parse content
        ]
    ]
"""

fromleo_pizzas_doodle =
 """{join} = String
{matchIn} = Regex
{indices, map, find} = List

{ onChangeAttribute } = Html

css = \"\"\"
.partTableCell{
  cursor:pointer;
}
.partTableCell:hover{
  outline: 1px solid black;
}
.hiddenAcc{
  display:block;
  position:absolute;
  top:-999em;
}
.form-control, textarea, select, input[type=\"text\"], input[type=\"password\"], input[type=\"datetime\"], input[type=\"datetime-local\"], input[type=\"date\"], input[type=\"month\"], input[type=\"time\"], input[type=\"week\"], input[type=\"number\"], input[type=\"email\"], input[type=\"url\"], input[type=\"search\"], input[type=\"tel\"], input[type=\"color\"], .uneditable-input{
  height:30px;
  padding-top:4px;
  padding-bottom:4px;
  border-radius:0;
  border-color:#b7b7b7;
  font-size:13px;
}
.textPoll th, .textPoll .foot td{
  max-width:94px;
}
label{
  font-weight:normal;
}
label{
  display:inline-block;
  margin-bottom:5px;
  font-weight:bold;
}
table.poll tr.participation td {
  text-align:center;
  background-color:#ffffff;
}
table.poll tr.participant td.n {
  background-color:#ffccca;
}
table.poll tr.participant td.y {
  background-color:#d1f3d1;
}
table.poll tr.participant td {
  text-align:center;
  vertical-align:middle;
  height:33px;
}
table.poll tr.participation.inEdit td:hover{
  background-color:#d6d6d6;
}
table.poll tr.participation td.pname,table.poll tr.participant td.pname{
  position:relative;
  min-width:182px;
  width:182px;
  border-top:1px solid #fff;
  border-bottom:2px solid #fff;
  background:url('http://doodle.com/builtstatic/1465286543/doodle/graphics/sprites/common/normal-s92f91c2182.png') -15px -972px no-repeat #fff;
  imageRendering:-moz-crisp-edges;
  imageRendering:-o-crisp-edges;
  imageRendering:-webkit-optimize-contrast;
  imageRendering:crisp-edges;
  -ms-interpolation-mode:nearest-neighbor;
  padding:0 2px 0 0;
}
table.poll tr.header.month th.xsep{
  background:url(\"http://doodle.com/graphics/polls/tick31r.png\") right 0 repeat-y #3385e4;
  padding-right:13px;
}
table.poll td.pname div.pname{
  text-align:left;
  font-size:15px;
  line-height:15px;
  padding:8px 0 5px 0;
  margin-left:3px;
  white-space:nowrap;
  overflow:hidden;
  max-width:135px;
  width:135px;
  position:relative;
}
table.poll tr.date th, table.poll tr.date td{
  background:#3385e4;
  color:#fff;
}
table.poll{
  border-collapse:separate;
}
table.poll tr.date.month th{
  padding-top:7px;
  padding-bottom:3px;
}
table.poll tr.header th, table.poll tr.header td{
  padding:0 10px 0;
}
table.poll th, table.poll td{
  font-size:13px;
  line-height:17px;
  font-weight:normal;
}
table.poll tr.participation td.pname input{
  font-size:12px;
  height:24px;
  line-height:20px;
  margin:3px 0 3px 3px;
  width:131px;
  padding:2px 6px 2px 9px;
}
table.poll tr th.nonHeader.partCount, table.poll tr td.partCount{
  background:#fff;
  padding:0 0 9px 0;
  vertical-align:bottom;
  font-size:13px;
  color:#323232;
  min-width:184px;
  width:184px;
  font-weight:bold;
  max-width:none;
}
table.poll tr.sums td{
  text-align:center;
  line-height:16px;
  padding-left:5px;
  padding-right:8px;
  padding-top:5px;
  color:#6f6f6f;
}
table.poll tr.participant td.q {
  background-color:#eaeaea;
}
pre {
  cursor:text;
}
.columnadder {
  opacity: 0.1;
}
.columnadder:hover {
  opacity: 1;
}
\"\"\"

participants = [
  \"John Doe\",
  \"Mam Butterfly\",
  \"Mysterious Man\"
  ]

choix = [
  [0, 0, True ],
  [0, 1, False],
  [0, 2, False],
  [1, 0, False],
  [1, 1, True ],
  [1, 2, False],
  [2, 0, False ],
  [2, 2, True ]
]

menus = [
  \"Margharita\",
  \"Pepperoni\",
  \"Chicken\"
]


addPerson participants choix = {
  apply (participants, choix) = \"\"
  update {input=(oldParticipants, oldChoix), outputNew = newParticipant} =
    if newParticipant /= \"\" then
      let newParticipants = oldParticipants ++ [newParticipant] in
      let newChoix = choix ++ (map (\\i -> [len oldParticipants, i, False]) (indices menus)) in
      Ok (Inputs [(newParticipants, newChoix)])
    else Ok (Inputs [(oldParticipants, oldChoix)])
}.apply (participants, choix)


total menuId =
  let n = choix |> List.map (\\[userId, mId, check] -> if check && menuId == mId then 1 else 0) |> List.sum in
  <td>@n</td>

onChangeAttribute model controller =
    { apply model = \"\"
      update {input, outputNew} = Ok (Inputs [controller input outputNew])
    }.apply model
 
wantsToEat personId menuId =
  let predicate [pId, mId, b] = pId == personId && mId == menuId in
  case find predicate choix of
    Just ([pId, mId, checked] as choix) ->
      if checked then
        <td class=(Update.softFreeze \"partTableCell xsep pok y\")
            onclick=\"this.classList.remove('y');this.classList.add('n');this.setAttribute('x', 'NO')\"
            title=((nth participants personId) + \", \" + (nth menus menuId) + \": Yes\")
            x=(onChangeAttribute checked (\\old newValue -> newValue == \"YES\"))
            >
          <span class=\"glyphicon glyphicon-ok\"></span>
        </td>
      else
        <td class=(Update.softFreeze \"partTableCell xsep pn n\")
            onclick=\"this.classList.remove('n');this.classList.add('y');this.setAttribute('y', 'YES')\"
            title=((nth participants personId) + \", \" + (nth menus menuId)+ \": No\")
            y=(onChangeAttribute checked (\\old newValue -> newValue == \"YES\"))
            > </td>
    _ ->
      <td class=\"partTableCell q sep pog\" title=((nth participants personId) + \", \" + (nth menus menuId)) onclick=\"this.setAttribute('z', 't')\"
      z=(onChangeAttribute choix (\\old _ -> [personId, menuId, True]::choix))
      >?</td>

ligne menu = <th>@menu</th>

pollInfo personId =
  <tr class=\"participant partMyself\">
    <td class=\"pname\" id=\"part1367130755\">
      <span class=\"pname\">@(nth participants personId)</span>
    </td>
    @(map (\\menusId -> wantsToEat personId menusId) (indices menus))
  </tr>

numMenus = len menus
  
page =
  <table cellpadding=\"0\" cellspacing=\"0\" class=\"poll textPoll \" summary=\"LARA Thai\">
    <tbody>
      <tr class=\"header date month\">
        <th class=\"nonHeader partCount boldText\">
          <div>@(toString (len participants) + \" participants\")</div>
        </th>
        @(map (\\menu -> <th>@menu</th>) menus)
        <th class=\"columnadder\"><button onclick=\"\"\"this.setAttribute('v@numMenus', 'T')\"\"\" @([[\"\"\"v@numMenus\"\"\", onChangeAttribute menus (\\oldMenus _ -> oldMenus++[\"\"\"Meal#@numMenus\"\"\"])]])>+</button></th>
      </tr>
      <tr></tr>
      @(map pollInfo (indices participants))
      <tr class=\"participation yesNo\">
        <td class=\"pname\">
          <label class=\"hiddenAcc\" forid=\"pname\" title=\"l10n_yourName\"></label>
          <input class=\"form-control\" id=\"pname\" maxlength=\"64\" name =\"name\" placeholder=\"Your name\"
             onkeypress=\"\"\"if(event.keyCode == 13) this.setAttribute(\"v\", this.value);\"\"\"
             type =\"text\" v=(addPerson participants choix)>
        </td>
        @(map total (indices menus) )
      </tr>
    </tbody>
  </table>

date = \"Thursday, April 26th 2018\"
urlencode txt = txt |>
  Regex.replace \":\" \"%2F5\" |>
  Regex.replace \"/\" \"%2F\" |>
  Regex.replace \" \" \"%20\" |>
  Regex.replace \"\\n\" \"%0A\"

command = String.join \",\" <| map (\\(index, menu) -> 
  let [_, _, [[_, t]]] = total index in
  let who = choix |> List.filterMap (\\[pId, mId, checked] -> if checked && mId == index then Just (nth participants pId) else Nothing) in
  if t == \"0\" then \"\" else \" \" + t + \" pizza\" + (if t==\"1\" then \"\" else \"s\") +\" \" + menu + \" (\" + String.join \", \" who + \")\"
  ) <| zipWithIndex menus

sms = \"\"\"Hello Pizzaiolo,
Always a pleaure to do business with you.
For the lunch of @date, we would like to order@command.
I'll pick up the pizzas at 1:15pm.
Best regards,
The Dream Team
\"\"\"

<div style=\"margin:20px\">
  <style>@css</style>
  <h1>Pizza meeting, @date</h1>
  <p>Choose your favorite pizzas. We will order some to satisfy everyone</p>
  @page
  <a contenteditable=\"False\" href=\"\"\"mailto:pizzaiolo@@gmail.com?subject=Pizzas for @date&body=@(urlencode sms)\"\"\">Send this email to the pizzaiolo</a>
  <br>
  <pre style=\"white-space:pre-wrap\">@sms</pre>
</div>
"""

fromleo_universal_number =
 """(lengthStr, alphabet) = (freeze \"0\" + \"000\", \"LIVE\")

(nbDigits, alphabetArray) = (toString (String.length lengthStr), explode alphabet)

universal_number lengthStr =
  let alphabetSizeStr =
    letrec aux acc a = case a of [] -> acc; head::tail -> aux (acc + \"#\") tail in
    aux \"\" alphabetArray
  in
  let stringRepeat sequence size =
    letrec aux acc sz = case String.uncons sz of
      Just (\"#\", tail) -> aux (acc + sequence) tail
      _ -> acc
    in aux \"\" size in
  let times sequence sizeBinary =
    letrec string_aux acc sizeB = case String.uncons sizeB of
      Just (\"0\", tail) ->
        let newAcc = stringRepeat acc alphabetSizeStr in
        string_aux newAcc tail
      Just (\"1\", tail) ->
        let newAcc = stringRepeat acc alphabetSizeStr + sequence in
        string_aux newAcc tail
      _ -> acc
    in
    string_aux \"\" sizeBinary in
  let numabBin = (\"1\" + replaceFirstIn \".\" \"\" lengthStr) in
  let numab = String.length (times \"#\" numabBin) in
  let init =
    letrec aux acc letterList =
      case letterList of
        [] -> acc
        head :: tail -> aux (acc + (times head numabBin)) tail
    in aux \"\" alphabetArray
  in
  let repeatedPrefix = \"^.{\" + (toString numab) + \"}\" in
  let positionNext posStr =
    letrec aux acc pos = if Regex.matchIn repeatedPrefix pos then (
        let newPos = replaceFirstIn repeatedPrefix \"\" pos in
        let newAcc = acc + \"#\" in
        aux newAcc newPos
      ) else (
        acc + (letrec k a l = case String.uncons l of Just (\"#\", tail) -> k (a + pos) tail; _ -> a in k \"\" alphabetSizeStr)
      )
    in aux \"\" posStr in
  let init0 = replaceAllIn \".\" \"0\" init in
  letrec cycle startStr currentStr state acc =
    let newCurrentStr = positionNext currentStr in
    let newCurrentStrLength = toString (String.length newCurrentStr) in
    let newAcc = acc + (
      replaceFirstIn (\"^.{\" + toString (String.length currentStr) + \"}(.).*$\") \"$1\" init
    ) in
    let newState = replaceFirstIn (\"^(.{\"+newCurrentStrLength+ \",\" +newCurrentStrLength +\"}).\")  (\\m -> (nth m.group 1) + \"1\") state in
    if (Regex.matchIn (\"^\" + startStr + \"$\") newCurrentStr) then (
      [newState, newAcc]
    ) else (
      cycle startStr newCurrentStr newState newAcc
    ) in
  letrec loop state acc =
    if (Regex.matchIn \"^1*$\" state) then
      acc
    else (
      let startPos = replaceAllIn \".\" \"#\" (replaceFirstIn \"^(1*)0.*$\" \"$1\" state) in
      let [newState, newAcc] = cycle startPos startPos state acc in
      loop newState newAcc
    ) in
  let debrujin = loop init0 \"\" in
  let lm1 = toString (String.length (replaceFirstIn \".\" \"\" lengthStr)) in
  debrujin + (replaceFirstIn (\"^(.{\"+lm1+\"}).*$\") \"$1\" debrujin)

u_number = universal_number lengthStr

<div style=\"margin:10px\">
<span>
  <h1>Finite Univeral Numbers</h1>
  This program serves as an example of creating a page
  that can be way longer than the code used to produce it.
  If you download it, you will see that its size is almost constant,
  regardless on the size of the sequence displayed below.
  <br><br>
  One of the smallest sequences on <code>@alphabet</code> containing
  all @nbDigits-digit sequences has length @(toString (String.length u_number))
  and is given below. The computation implements the idea given in the wikipedia
  article <a href=\"https://en.wikipedia.org/wiki/De_Bruijn_sequence#Construction\">De Bruijn Sequence</a>. To test the validity of this claim, press <kbd>CTRL+F</kbd> and look for any sequence of @nbDigits characters among <span>`</span>@alphabet<span>`</span>.<br>
  <code style=\"whitespace:pre-wrap;max-width:100%\">@u_number</code></span></div>
"""

repl =
 """cachedNamesList = []
cachedNames = Dict.fromList cachedNamesList
commands = [\"\\\"Hello\\\"\" , \"res1 + \\\" \\\" + \\\"world\\\"\"]
commandPrefix = \"> \"
svgTags = Set.fromList [\"rect\",\"line\"]

displayEvaluationResult x = case x of
  Err msg -> <span class=\"error\" style=\"white-space:pre\">@msg</span>
  Ok result -> 
    let displayHtml () = case result of
      [tag, _, _] ->
        if Set.member tag svgTags then
          <svg class=\"svgterminal\">@result</svg>
        else
          result
    in
    let displayText () =
      <span class=\"result\">@(toString result)</span>
    in
    case result of
      [s, a, t] -> case a of
        [] -> displayHtml ()
        head::tail -> displayHtml ()
        _ -> displayText ()
      _ -> displayText ()
  

content = 
  letrec aux i commands prevDefinitions revAcc =
    case commands of
    [] -> List.reverse revAcc
    cmd::tail ->
      let defaultName = \"\"\"res@i\"\"\" in
      let name = {
        apply x = Dict.get defaultName cachedNames |>
          case of Nothing -> defaultName; Just n -> n
        update {input=(a, b), outputNew=newName} = Ok (Inputs (
           [(a, Dict.insert defaultName newName cachedNames)]))
        }.apply (defaultName, cachedNames) in
      let res = __evaluate__ prevDefinitions cmd in
      let newPrevDefinitions = case res of
        Ok r -> (name, r)::prevDefinitions
        _ -> prevDefinitions
      in
      aux (i + 1) tail newPrevDefinitions (
        <div class=\"result\">@name: @(displayEvaluationResult res)</div> ::
        <div class=\"command\">@commandPrefix@cmd</div> :: revAcc)
  in
  aux 1 commands [(\"append\", append)] []

addCommand commands newCommand = commands ++ [newCommand]

examples = [
  (\"Reset\",  \"/!\\\\ Erases the content of the terminal\",
    ([], [])),
  (\"Line example\",  \"An example with SVG\",
    ([(\"res1\", \"x\"), (\"res2\", \"y\"), (\"res3\",\"right\")],
     [\"10\", \"100\", \"x+70\", \"<line x1=x y1=y x2=right y2=(y+10) stroke='red' stroke-width='3px'/>\"])),
  (\"Hello world\",\"String concatenations\",([], [ \"\\\"Hello\\\"\",
            \"res1 + \\\" \\\" + \\\"world\\\"\"
          ]))
]

<div style=\"margin:10px\">
<span>
<h1>Read Eval Print Loop with SVG support</h1>
 This REPL lets you:
<ul>
<li title=\"Input some characters and press ENTER on the last line\">Enter new commands</li>
<li title=\"If you do this, the commands are re-executed\">Change the commands</li>
<li title=\"The commands are updated and re-executed\">Change the results</li>
<li title=\"Just click on the variable and rename it\">Change a variable's name.</li>
<li title=\"Simply edit the quoted part of the program in the output\">Fix a parsing error from the error message</li>
</ul><div>Please give it a try! You can also save the state of the REPL and reset it using the button below.</div><br>
<div class=\"terminal\">
@content@commandPrefix<input type=\"text\" id=\"command\"
  v=(Html.onChangeAttribute commands addCommand) onchange=\"this.setAttribute('v', this.value); this.value=''\">
</div>
@(examples |> List.map (\\(name, title, (names, cmds)) ->
  Html.button name title (cachedNamesList, commands) (\\_ -> 
  (names, cmds)
)))
<br>
@(Html.button \"Save\" \"Save the current state of the REPL to examples\"
  examples (\\_ -> examples ++ [(\"Example\", \"You can modify the name and this title in the DOM\", (cachedNamesList, commands))]))
</span>
<style>
#command {
  border: none;
  outline: none;
  color: white;
  background: black;
  font-size: 1em;
}
.command {
}
.result {
}
.error {
  color: red;
}
.terminal {
  color: white;
  background: black;
  font-family: consolas, monospace;
}
.svgterminal {
  color: black;
  background: white;
}
</style>
@Html.forceRefresh<|<script>document.getElementById(\"command\").focus();</script>
</div>
"""

slides =
 """delay = \"0.5s\"

displayError msg = <span style=\"color:red;white-space:pre;\">@msg</span>

minieval x =
  <span class=\"code\">@x<div><b>⇨ </b
      >@(case __evaluate__ (__CurrentEnv__) x of
    Ok x -> toString x 
    Err msg -> displayError msg)</div></span>

minievalx x =
  <span class=\"code\">@x<br><b>⇨ </b>@(case __evaluate__ (__CurrentEnv__) x of
    Ok x -> x
    Err msg -> displayError msg
  )</span>

-- 
titleWrite = \"Write your program\"

<div>
<div class=\"slides\" id=\"slides\" contenteditable=\"true\">
  <slide ignore-position=\"current\">
    <h1 class=\"center1\">Sketch-n-Sketch 2.0</h1>
    @Html.forceRefresh<|
    <h2 class=\"center2\">Ravi Chugh and Mikaël Mayer</h2>
  </slide>
  <slide ignore-position=\"future\">
    <h1>@titleWrite</h1>
    Use standard Elm-like constructs
    <ul>
      <li>@(minieval \"let x = \\\"Hello \\\" in x + x + \\\"world\\\"\")</li>
      <li>@(minieval \"map (\\\\x -> x + 5) [1, 2, 4]\")</li>
    </ul>
  </slide>
  <slide ignore-position=\"future\">
    <h1>@titleWrite</h1>
    You can use HTML syntax, and it's <i>interpolated</i>.@Html.forceRefresh<|<ul>
      <li>@(minievalx \"let f x = <b title=\\\"I said \\\"+x>@x world</b> in f 'Hello'\")</li>
      <li>@(minievalx \"let f n = n + \\\" team. \\\" in map (\\\\x -> <i style=\\\"\\\"\\\"color:@x\\\"\\\"\\\">@f(x)</i>) [\\\"red\\\", \\\"yellow\\\", 'blue']\")</li>
    </ul>
  </slide>
  <slide ignore-position=\"future\">
    <h1>Translate slides</h1><div>By applying a single function, you can start translating slides to different languages right away!</div>
  </slide>
</div>
<style>
slide {
  color: black;
  background: none;
}
.slides {
  background: lightblue;
  font-family: \"Roboto\", \"Avenir\", sans-serif;
}
.code {
  font-family: \"Consolas\", monospace;
}
</style>
@Html.forceRefresh<|<script>
var container = document.querySelector(\"#slides\");
if(container !== null) {
  container.onscroll = function () {
    container.scrollLeft = 0;
  }
}
</script>
<script>
var container = document.querySelector(\"#slides\");
if(typeof keyDown != \"undefined\" && container !== null) {
  container.removeEventListener(\"keydown\", keyDown, false);
}

keyDown = function (e) {
  var keyCode = e.keyCode;
  var current = document.querySelector(\"slide[ignore-position=current]\");
  if(keyCode == 39 ) { // Right
    var next = current.nextElementSibling;
    while(next != null && next.tagName != \"SLIDE\"){
      next = next.nextElementSibling
    }
    if(next != null) {
      next.setAttribute(\"ignore-position\",\"current\");
      current.setAttribute(\"ignore-position\", \"past\");
    }
    return false;
  } else if(keyCode == 37) { // Left
    var prev = current.previousElementSibling;
    while(prev != null && prev.tagName != \"SLIDE\"){
      prev = prev.previousElementSibling
    }
    if(prev != null) {
      prev.setAttribute(\"ignore-position\",\"current\");
      current.setAttribute(\"ignore-position\", \"future\");
    }
    return false;
  }
  return true;
}
if(container !== null) {
  container.addEventListener(\"keydown\", keyDown, false);
}
</script>
@let center translateY = \"\"\"{
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) translateY(@translateY);
  width: 100%;
  text-align: center;
}\"\"\" in <style>
.slides {
  display: block;
  width: 100%;
  padding-bottom: 56.25%; /* 16:9 */
  position: relative;
  overflow: hidden;
}
slide {
  position: absolute;
  top: 0; bottom: 0; left: 0;
  width: 100%;
  font-size: 24px;
  padding: 20px;
  box-sizing: border-box;
}
[ignore-position=\"current\"] {
  left: 0;
  transition: @delay;
}
[ignore-position=\"future\"] {
  left: 100%;
  width: 100%;
  transition: @delay;
}
[ignore-position=\"past\"] {
  left: -100%;
  width: 100%;
  transition: @delay;
}
.slides.fullscreen {
  position: absolute;
  top: 0; left: 0; right: 0; bottom: 0;
  z-index: 1000;
}
slide h1, slide h2 {
  margin-top: 0px;
}
.center1 @center(\"-1em\")
.center2 @center(\"0em\")
</style>
</div>
"""

docs =
 """ensureNatural {minbound} n = { apply x = x, update {input, outputNew} =
  let (f, c) = (max minbound <| floor outputNew, max minbound <| ceiling outputNew) in
  if f /= outputNew || c /= outputNew then let solutions = (if f == input then [] else [f]) ++
  (if c == input || c == f then [] else [c]) in
  Ok (Inputs solutions) else Ok (Inputs [outputNew]) }.apply n

format n = { apply n = toString n |> Regex.replace \"\"\"(\\d)(?=(?:(?:\\d{3})+$))\"\"\" (\\m -> nth m.group 1 + \",\")
             update {outputNew} = Ok (Inputs [String.toInt (Regex.replace \",\" \"\" outputNew)])}.apply n

nbpostdocs     = ensureNatural {minbound=0} 2
nbgraduates    = ensureNatural {minbound=1} 4
nbyears        = ensureNatural {minbound=1} 2
postdocsalary  = ensureNatural {minbound=45000} 47000
graduatesalary = ensureNatural {minbound=25000} 28136
salariesPerYear = postdocsalary * nbpostdocs + graduatesalary * nbgraduates
years callback = List.range 1 nbyears |> List.map callback
plural x = if x > 1 then \"s\" else \"\"
pageContent = <div class=\"pagecontent\" contenteditable=\"true\">
<h1 style=\"text-align:center\">NSF Grant proposal</h1><h2>Requested funding</h2>
We want to sustain for @nbyears year@plural<|nbyears:
<ul>
  <li>@nbpostdocs postdoc@plural<|nbpostdocs paid @format<|(postdocsalary)$ per year.</li>
  <li>@nbgraduates graduate student@plural<|nbgraduates paid @format<|(graduatesalary)$ per year.</li>
</ul>
Therefore, we ask for @format<|(nbyears*salariesPerYear)$.
Detailed budget can be found in <a class=\"sectionref\" href=\"#budget\">the budget section</a>.
<h2 id=\"budget\">Detailed budget</h2>
  <table>
    <tr><th>Who</th>
    @(years <| \\i -> <th>Year #@(freeze i + 0)</th>)<th>Total</th></tr>
    <tr><td>@nbgraduates graduates</td>
      @(years <| \\i -> <td>@format<|(nbgraduates * graduatesalary)$</td>)
      <td>@(years (always <| nbgraduates * graduatesalary) |> List.sum |> format)$</td></tr>
    <tr><td>@nbpostdocs postdocs</td>
      @(years <| \\i -> <td>@format<|(nbpostdocs * postdocsalary)$</td>)
      <td>@(years (always <| nbpostdocs * postdocsalary) |> List.sum |> format)$</td></tr>
    <tr><td><b>Total</b></td>
    @(years (always <| <td>@(format salariesPerYear)$</td>))
    <td><b>@(salariesPerYear * nbyears |> format)$</b></td>
    </tr>
  </table>
  <h2>Research statement</h2>
  We can create very nice shiny documents that can embed formulas, but we would like to research how we can<br>
  <ul><li>Make it collaborative<br></li>
  <li>Ensure that we can format paragraphs that contain values properly</li>
  <li>Deal with tables nicely by back-propagating expressions</li></ul>
<span style=\"position:relative\"><div
style=\"position:absolute;left:0;width:calc(8.5in - 32mm);height:20px;background:lightgreen;text-align:right\">d</div>
</span>
@(List.range 1 50 |> List.map (\\i -> <div>More text @i</div>))
</div>

-- Very useful link: http://alistapart.com/article/boom
-- https://code.tutsplus.com/tutorials/create-a-wysiwyg-editor-with-the-contenteditable-attribute--cms-25657
<div id=\"app\">
<div id=\"menu\" style=\"padding-left:10px\">
@Html.forceRefresh<|<style>
#app {
  position: relative;
  background: #eeeeee;
  height: 100%;
  width: 100%;
  overflow: hidden;
}
#menu > a {
  text-decoration: none;
  color: black;
  padding: 4px;
  margin-left: 10px;
}
#menu > a:hover {
  background: #cccccc;
}
#menu > a ul, #menu > a ol {
 padding-left: 0px;
}
#menu {
  margin-top: 5px;
  height: 2em;
  margin-bottom-: 5px;
  border-bottom: 2px solid #cccccc;
  background: #eeeeee;
}
#body {
  position: relative;
  overflow: scroll;
  height: 100%;
}
.page {
  position: relative;
  margin: auto;
  margin-top: 10px;
  top: 0px;
  width: 8.5in;
  /*height: 11in;*/
  box-sizing: border-box;
  box-shadow: 0px 0px 5px #aaa;
  background: white;
}
.pagecontent {
   display: inline-block;
   padding: 17mm 16mm 27mm 16mm;
   cursor: text;
   width: 100%;
   height: 100%;
   box-sizing: border-box;
   word-wrap: break-word;
}
th, td {
    padding: 5px;
}
body {
  counter-reset: h2-counter;
}
h2::before {
    counter-increment: h2-counter;
    content: counter(h2-counter) \". \";
}
@@media print {
  body {
    background: white;
  }
  body * {
    visibility: hidden;
  }
  #outputCanvas {
    height: auto !important;
    overflow: visible;
  }
  #app {
    position: absolute;
    height: auto;
    width: 100%;
    overflow: visible;
  }
  #body {
    position: absolute;
    top: 0;
    overflow: visible;
  }
  .code-panel {
    display: none;
  }
  .page, .page * {
    visibility: visible;
  }
  .page {
    position: absolute;
    left: 0;
    top: 0;
    box-shadow: none;
    margin: 0;
    width: calc(8.5in - 32mm);
    /*width: calc(1.5 * calc(8.5in - 32mm));
    font-size: 1.5em;*/
    transform: scale(1.5); /* I don't know why but this is correct on my machine!\" */
    transform-origin: top left;
  }
  .pagecontent {
    padding: 0;
    /*font-size: 1.5em;*/
  }
  .pagecontent div, .pagecontent h2 {
    page-break-inside: avoid;
  }
  @@page{
    margin-top: 17mm;
    margin-right: 16mm;
    margin-bottom: 27mm;
    margin-left: 16mm;
    size: 8.5in 11in;
    @@bottom-right {
      content: counter(page);
    }
  }
  
  .output-panel {
    left: 0 !important;
    top: 0 !important;
    right: 0 !important;
    bottom: 0 !important;
  }
}
</style>
<b>Sketch-n-Sketch docs</b>
<a href=\"#\" data-command='formatBlock:h1' title=\"First-level header\">H1</a>
<a href=\"#\" data-command='formatBlock:h2' title=\"Second-level header\">H2</a>
<a href=\"#\" data-command='undo' style=\"display:none\">⟲ Undo</a>
<a href=\"#\" data-command='redo' style=\"display:none\">⟳ Redo</a>
<a href=\"#\" data-command='createlink' title=\"Create a link\"><u style=\"color:blue\">Link</u></a>
<a href=\"#\" data-command='justifyLeft' title=\"Align text to the left\">
<div style=\"font-size:0.05em;display:inline-block\">=====<br>==<br>====</div></a>
<a href=\"#\" data-command='justifyCenter' title=\"Center text\">
<div style=\"font-size:0.05em;display:inline-block;text-align:center\">=====<br>==<br>====</div></a>
<a href=\"#\" data-command='superscript' title=\"superscript\">X<sup>b</sup></a>
<a href=\"#\" data-command='subscript' title=\"subscript\">X<sub>b</sub></a>
<a href=\"#\" data-command='insertUnorderedList' title=\"bullet point list\"><ul
  style=\"font-size:0.05em;display:inline-block\"
  ><li>---</li><li>---</li><li>---</li></ul></a>
<a href=\"#\" data-command='insertOrderedList' title=\"numbered list\"><ol
  style=\"font-size:0.05em;display:inline-block\"
  ><li>---</li><li>---</li><li>---</li></ol></a>
<a href=\"#\" data-command='print' title=\"Print the document -- look for @media print for configurable options\">🖶
</a></div>
@Html.forceRefresh<|<div id=\"body\">
<div class=\"page\">
@pageContent
</div>
</div>
@Html.forceRefresh<|<script>
/*var el = document.getElementById(\"body\");
var range = document.createRange();
var sel = window.getSelection();
range.setStart(el.childNodes[0], 0);
range.collapse(true);
sel.removeAllRanges();
sel.addRange(range);*/
var as = document.querySelectorAll(\"a[data-command]\")
for(var i = 0; i < as.length; i++) {
  var a = as[i];
  var c = a.dataset.command;
  var cs = c.split(\":\")
  var command = cs[0];
  a.onclick = (function(command, argument) { return function() {
    if(command == \"createlink\") {
      argument = prompt('Enter the link here: ','http:\\/\\/');
    }
    document.execCommand(command, false, argument);
  } })(cs[0], cs[1] ? cs[1] : null);
}
</script>
</div>
"""

sync =
 """sync v1v2 = {
  apply ((s1, n1), (s2, n2)) = (
    if s1 == s2 then ((s1, n1), \"\")
    else let t = Html.freshTag () in
      ((s1, n1), <@t><script id=t mkupdate=\"\">
setTimeout(function() {
document.getElementById(\"@t\").setAttribute(\"mkupdate\",`unify`)
}, 0);
</script></@>))
  update {input = ((s1, n1), (s2, n2)), output = ((newS, _), maybeScript) } = 
    let newsn = (
      if n1.recentlyModified then s1 else
      if n2.recentlyModified then s2 else
      newS, {recentlyModified=False})
    in
    Ok (Inputs [(newsn, newsn)])
}.apply v1v2

--------------------------------------------------------------

v1 = (\"Hi big world!\", {recentlyModified=False})
v2 = (\"Hi big world!\", {recentlyModified=False})
v3 = (\"Hi big world!\", {recentlyModified=False})

(v12, script) = sync (v1, v2)
(v123, script2) = sync (v12, v3)

<h1 style=\"padding:10px\">
@v123._1
@script
@script2
</h1>
"""

foldl_reversible_join =
 """big = \"big\"
world = \" world\"
list = [\"Hello\", \"2\", \"\", big, world, \"!\"]

[color1, color2] = [\"red\", \"blue\"]
highlight color x = <span style=\"\"\"color:@color\"\"\">@x</span>

list2 = [[1, 2], [], 3, [4, 5]]

<div style=\"margin:10px\">
<h1>Reversible join and concatMap</h1>
<code>String.join</code> and <code>List.concatMap_</code> are two functions making use of <code>List.foldl2</code>. It allows to deal with insertion and deletions of elements in the original list.
<h2><code>String.join</code></h2>
<span><code style=\"font-size:1.5em\">join_ @(toString list) =<br>@highlight(color1)(toString<|String.join \"\" list)</code><br>
Try the following on @highlight(color1)(\"\"\"the result above in @color1\"\"\") to see of join is cleverly thought::
<ul>
<li>Insert 1 to the left of 2 - it first snaps with numbers</li>
<li>Insert 1 to the right of 2 - if first snaps with empty string, then number</li>
<li>Insert 'a' without quotes to the right of 2</li>
<li>Delete 'big'</li>
<li>Delete 'big w'</li>
<li>Delete ' world'</li>
<li>Delete 'g world'</li>
<li>Replace 'Hello2' by 'Hi'</li>
</ul>
</span>
<h2><code>List.concatMap_</code></h2>
<span>
<code style=\"font-size:1.5em\">concatMap wrapOrId @(toString list2) =<br> @highlight(color2)(toString (List.concatMap_ (\\x -> [x]) (\\[head] as headList -> case head of
  [] -> head
  _ :: _ -> head
  _ -> headList) list2))</code><br>

Try the following on @highlight(color2)(\"\"\"the result above in @color2\"\"\"):
<ul>
<li>Remove the elements 2, 3</li>
<li>Insert an element, e.g. 8, right before 3</li>
<li>Insert an element, e.g. 8, right after 3</li>
<li>Remove 4, 5</li>
<li>Remove all elements, and then insert one element</li>
</ul>
</span>
</div>
"""

references_in_text =
 """compareStr a b = if a == b then 0 else if a < b then -1 else 1

quicksort compare list = case list of
    [] -> []
    pivot::t -> let tBefore = List.filter (\\e -> compare e pivot < 0) t in
      let tAfter = List.filter (\\e -> compare pivot e < 0) t in
      quicksort compare tBefore ++ [pivot] ++ quicksort compare tAfter

addReferences references node =
  let collectedAddedReferences references node =
    { apply (references, node) = node
      update {input = (references, node) as input, outputNew} =
        let refAddRegex = \"\"\"\\[\\+\\s*((?:(?!\\]).)*)\\]\"\"\" in
        let addedReferences = Html.find refAddRegex outputNew in
        if addedReferences == [] then Ok (InputsWithDiffs [(input, Nothing)])
        else 
          let (newNode, (_, newReferences)) = Html.foldAndReplace refAddRegex (\\{submatches=[name]} (newRefNum, newReferences) ->
              ([[\"TEXT\", \"\"\"[@newRefNum]\"\"\"]], (newRefNum + 1, newReferences ++ [name]))
            ) (List.length references + 1, references) outputNew
          in 
          let newInput = (newReferences, newNode) in
          Ok (Inputs [newInput])
    }.apply (references, node)
  in
  let refRegex = \"\"\"\\[(\\d+)\\]\"\"\" in
  let -- returns a list of sorted references according to some criterion and an updated node.
    sortReferences references node = 
      let (newPermutation, newReferences) = List.zipWithIndex references
      |> quicksort (\\(i, ref1) (i2, ref2) -> 
         compareStr ref1 ref2)
      |> List.unzip
      in
      if newReferences == references then (references, node)
      else
        let d = List.map2 (,) newPermutation (List.range 0 (List.length newPermutation - 1))
          |> Dict.fromList in
        let newNode = Html.replace refRegex (\\{submatches=[ref],match} ->
          let nref = String.toInt ref in
          let _ = Debug.log \"permutation:\" d in
          case Dict.get (nref - 1) d of
            Just nnref  ->
              [[\"TEXT\", \"\"\"[@(nnref + 1)]\"\"\"]]
            Nothing ->
              [[\"TEXT\", match]]
          ) node
        in
        (newReferences, newNode)
  in
  let  referencesDict = Dict.fromList (
    List.range 1 (List.length references) |> List.map (toString)
    |> map2 (flip (,)) references)
  in
  let lenReferences = List.length references in
  let (sortedReferences, sortedNode, applysort) = {
    apply (references, node) = (references, node, \"\")
    update {outputNew = (newReferences, newNode, newApplySort), diffs} =
      if newApplySort /= \"\" then
        Ok (Inputs [sortReferences newReferences newNode])
      else
        Ok (InputsWithDiffs [((newReferences, newNode), Just diffs)])
    }.apply (references, node)
  in
  let finalReferences = {
    apply (references, node) = references
    update {input=(references, node), outputNew=newReferences, diffs=(VListDiffs diffs) as listDiffs} =
      let aux offset currentNode nodeHasChanged diffs = case diffs of
        [] -> if nodeHasChanged then
            case __diff__ node currentNode of
              Err msg -> Err msg
              Ok x ->
                let finalDiffs =  case x of
                  Nothing -> {_1 = listDiffs}
                  Just x ->  {_1 = listDiffs, _2 = x}
                in
                Ok (InputsWithDiffs [((newReferences, currentNode), Just (VRecordDiffs finalDiffs))])
          else
            Ok (InputsWithDiffs [((newReferences, currentNode), Just (VRecordDiffs {_1 = listDiffs}))])
        (j, d)::diffsTail ->
          case d of
          ListElemUpdate _ -> aux offset currentNode nodeHasChanged diffsTail
          ListElemInsert count ->
            let newNode = Html.replace refRegex (\\{submatches=[ref],match} ->
              let nref = String.toInt ref in
              if nref >= j + 1 + offset && nref <= lenReferences then [[\"TEXT\", \"\"\"[@(nref + count)]\"\"\"]]
              else [[\"TEXT\", match]]
              ) currentNode
            in
            aux (offset + count) newNode True diffsTail
          ListElemDelete count ->
            let newNode = Html.replace refRegex (\\{submatches=[ref],match} ->
              let nref = String.toInt ref in
              if nref >= j + 1 + offset + count then [[\"TEXT\", \"\"\"[@(nref - count)]\"\"\"]]
              else if nref >= j + 1 + offset && nref <= lenReferences then
                [[\"TEXT\", \"\"\"[0] (deleted ref to '@(Dict.get ref referencesDict |> Maybe.withDefault \"?\"))')\"\"\"]]
              else [[\"TEXT\", match]]
              ) currentNode
            in
            aux (offset - count) newNode True diffsTail
      in
      aux 0 node False diffs |> Debug.log \"aux\"
  }.apply (sortedReferences, sortedNode)
  in
  [Html.replace refRegex  (\\{submatches=[ref],match} ->
    Dict.get ref referencesDict |> 
    Maybe.map (\\name -> Update.sizeFreeze [<abbr title=name>@match</abbr>]) |>
    Maybe.withDefaultLazy (\\_ -> Update.sizeFreeze
      [<abbr style=\"color:red\" title=\"Unknown reference\">@match</abbr>]))
    (collectedAddedReferences references node)
  , Update.expressionFreeze <ul
  contenteditable=\"true\">@(List.indexedMap (\\i x -> <li>@(Html.text \"\"\"[@(i + 1)] @x\"\"\")</li>) finalReferences)</ul>
  , Html.button \"Sort references\" \"Sort the list of references alphabetically\" applysort (\\_ -> \"#\")
  ]

  
-- We should not remove references or insert them in this list,
-- instead, we should do these operations from the output to
-- keep in sync with the text
references = [
 \"Mayer\",
 \"Work that is not that relevant really\",
 \"Chugh et al. 2016\"]

example =  addReferences references <span>
  <h2>Related work</h2>
 The work in [1] both suggested that it is possible to modify
 an interface without modifying the code. However, it also
 pin-points that neglecting raw access to the code is a key
 impedance to adoption. The work of [2] could also be relevant.
 In [3], the authors demonstrate that dual editing of both code
 and view increases the productivity. [4] applies a more
 general-purpose approach to edit programmatically generated
 documents such as this one.
</span>


















 
Html.forceRefresh  <div id=\"content\" style=\"margin:20px;cursor:text\">
<style>#example { font-family: 'Computer Modern Serif'; }</style>
<span>
<h1>HTML Regex replacement</h1>
<p>
  The function <code>addReferences</code> adds to all occurrences of [number]
  a tooltip indicating the reference name, and the list of references at the end.
  It can also propagate changes in the reference list to the text.</p>
<p>
  <b>Modify references:</b>
  You can change any reference title by simply editing it,
  either in the UL, or even in the title of a citation.
  For example, change the first \"Mayer\" to \"Game Programming
  by Demonstration, Mayer et Kuncak, 2013\".
</p>
<p>
  <b>Adding or inserting references:</b>
  Position the cursor at the end of an element in the list,
  press ENTER and enter '[1] The reference name',
  and <code>addReferences</code> takes care of shifting indices.
  Try to insert \"[4] Mayer, Chugh and Kuncak 2016\".
</p>
<p>
  <b>Deleting references:</b>
  If you delete a reference, <code>addReferences</code> takes
  care of transforming any citation to [0] and inlines the name.
  Try to delete '[2] Work that is not that relevant really'
  below.
</p>
<p>
  <b>Insert, delete and modify citations</b>
  You can delete any citation that looks like [1], insert
  new ones by typing [2] for example, or change a citation by
  changing its number.
</p>
<p>
  <b>Sort references</b>
  Click on the \"Sort references\" button to sort the references.
  The references are also sorted in the text.
</p>
</span>
<div style=\"border:4px solid black;padding:20px\" id=\"example\">
@example
</div>
</div>
"""


--------------------------------------------------------------------------------

welcomeCategory =
  ( "Welcome"
  , [ makeLeoExample blankSvgTemplate blankSvg
    , makeLeoExample blankHtmlTemplate blankDoc
    , makeLeoExample initTemplate welcome1
--    , makeLeoExample "Tutorial" blankDoc
    ]
  )

docsCategory =
  ( "Examples (OOPSLA 2018 Submission)"
  , [ makeLeoExample "1a: Table of States" tableOfStatesA
    , makeLeoExample "1b: Table of States" tableOfStatesC
    , makeLeoExample "1c: Table of States" tableOfStatesB
    ] ++
    (
    List.indexedMap
      (\i (caption, program) ->
        makeLeoExample (toString (2+i) ++ ": " ++ caption) program
      )
      [ ("Conference Budget", fromleo_conference_budgetting)
      , ("Model View Controller", fromleo_modelviewcontroller)
      , ("Scalable Recipe Editor", fromleo_recipe2)
      , ("Cloning Editor", fromleo_linkedtexteditor)
      , ("Translation Editor", fromleo_translatabledoc)
      , ("Markdown Editor", fromleo_markdown)
      , ("Dixit Scoresheet", fromleo_dixit)
      , ("Doodle", fromleo_pizzas_doodle)
      , ("Universal numbers", fromleo_universal_number)
      , ("LaTeX Editor", fromleo_latexeditor)
      , ("REPL with SVG", repl)
      , ("Slides", slides)
      , ("Docs", docs)
      -- TODO maybe Conference?
      , ("Html regex replace: references", references_in_text)
      , ("String join, concatMap", foldl_reversible_join)
      , ("Lens: Maybe Map", mapMaybeLens)
      , ("Lens: List Map 1", mapListLens_1)
      , ("Lens: List Map 2", mapListLens_2)
      , ("Lens: List Append", listAppendLens)
      ]
    )
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
    , makeLeoExample "SVG Cat" cat
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

internalCategory =
 ( "(Internal Things...)"
 , [ makeLeoExample "Standard Prelude" Prelude.preludeLeo
   , makeLeoExample badPreludeTemplate badPrelude
   ]
 )

templateCategories =
  [ welcomeCategory
  , docsCategory
  , deuceCategory
  , defaultIconCategory
  , logoCategory
  , flagCategory
  , otherCategory
  , deuceUserStudyCategory
  , internalCategory
  ]

list =
  templateCategories
    |> List.map Tuple.second
    |> List.concat
