module ExamplesGenerated (list, scratchName, scratch) where

import Lang
import LangParser2 as Parser
import Eval
import Utils
import PreludeGenerated as Prelude

makeExample name s =
  let thunk () =
    let e = Utils.fromOk_ (Parser.parseE s) in
    let v = Eval.run e in
    {e=e, v=v}
  in
  (name, thunk)

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

sixBoxesA =
 "; Both x- and y-spacing is controlled by sep.

(let [x0 y0 sep] [10 28 60]
(svg
  (map (\\[i j]
    (let xi (+ x0 (mult i sep)) 
    (let yj (+ y0 (mult j sep))
    (square_ xi yj 50))))
  (cartProd [0 1 2] [0 1]))))

"

sixBoxesB =
 "; x-spacing is controlled by xsep, y-spacing by ysep.
 
(let [x0 y0 xsep ysep] [10 28 60 60]
(svg
  (map (\\[i j]
    (let xi (+ x0 (mult i xsep)) 
    (let yj (+ y0 (mult j ysep))
    (square_ xi yj 50))))
  (cartProd [0 1 2] [0 1]))))

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
  (let basic (\\shape (addAttr shape ['zones' 'basic']))
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

activeTrans =
 ";
; Logo based on Active Transportation Alliance
; (http://activetrans.org/)
;
; Possible ways to manipulate:
; - Grab a point of the city skyline and pull up and down
; - Click 'Show Zones' and mess with the curves at
;     the bottom of the logo
; - Toggle the color switch on the top left
;
(let [h] [0]
(let grayPts
  [[97 546]
   [33 414]
   [33! (+ h 153!)]
   [53! (+ h 128!)]
   [82! (+ h 135!)]
   [83! (+ h 160!)]
   [114! (+ h 149!)]
   [113! (+ h 98!)]
   [143! (+ h 82!)]
   [158! (+ h 101!)]
   [160! (+ h 46!)]
   [192! (+ h 27!)]
   [221! (+ h 56!)]
   [227! (+ h 222!)]
   [245! (+ h 224!)]
   [246! (+ h 181!)]
   [288! (+ h 156!)]
   [286! (+ h 113!)]
   [312! (+ h 88!)]
   [374! (+ h 106!)]
   [375! (+ h 155!)]
   [397! (+ h 136!)]
   [424! (+ h 145!)]
   [425 207]]
;
(let greenPts [[247 663] [461 419] [466 230] [439 230] [178 614]]
(let [grayctrl greenctrl] [[47 489] [451 542]]
;
(let [cGreen cGray] ['#66CC66' '#505050']
(let [b buttonShapes] (button 20! 20! '' 0.25)
(let [xOff yOff] [0! 0!]
;
(let groupBox (rect (if b 'transparent' cGreen) xOff yOff 500! 700!)
;
(let makePath
  (\\(color pts [xc yc])
    (let offsetPts (map (\\[x y] [(+ x xOff) (+ y yOff)]) pts)
    (let [[x0 y0] [x1 y1] | rest] offsetPts
    (let commands
      (append
        (append ['M' x0 y0] ['Q' xc yc x1 y1])
        (foldr (\\([xi yi] acc) (append ['L' xi yi] acc)) ['Z'] rest))
      (path color 'black' 0 commands)))))
;
(let grayPath (makePath (if b cGray 'white') grayPts grayctrl)
(let greenPath (makePath (if b cGreen 'white') greenPts greenctrl)
  (svg (append [groupBox grayPath greenPath] buttonShapes)))))))))))))

"

botanic =
 ";
; Logo: Chicago Botanic Garden
;
; Click '[Zones]' to see the control points for
; the various Bezier curves.
; 
(let [xOff yOff w h] [0! 0! 623 622]
(let [xOut xcOut1 ycOut1 xcOut2 ycOut2 xcOut3 ycOut3] [292 40 141 97 202 23 24]
(let [xMid yTip yMid xBud yBud] [320! 272 460 -51 272]
(let left [[xMid yMid] [(- xMid xOut) yTip]]
(let right [[xMid yMid] [(+ xMid xOut) yTip]]
(let bud [[xMid (- yMid 92)] [(+ xMid xBud) yBud] [(- xMid xBud) yBud]]
;
(let makePath
  (\\(c pts [xc1 yc1] [xc2 yc2])
    (let offsetPts (map (\\[x y] [(+ x xOff) (+ y yOff)]) pts)
    (let [[x0 y0] [x1 y1]] offsetPts
    (let commands ['M' x0 y0 'Q' xc1 yc1 x1 y1 'M' x1 y1 'Q' xc2 yc2 x0 y0]
      (path c 'black' 0 commands)))))
;
(let makeArc
  (\\(c pts [xc1 yc1] [xc2 yc2])
    (let offsetPts (map (\\[x y] [(+ x xOff) (+ y yOff)]) pts)
    (let [[x0 y0] [x1 y1] [x2 y2]] offsetPts
    (let commands ['M' x0 y0 'L' x1 y1 'A' 45 45 0 0 1 x2 y2 'L' x2 y2 'Z']
      (path c 'black' 0 commands)))))
;
(let leftleaf
  (makePath 'white' left [(- xMid xcOut1) ycOut1] [(- xMid xcOut2) ycOut2])
(let rightleaf
  (makePath 'white' right [(+ xMid xcOut1) ycOut1] [(+ xMid xcOut2) ycOut2])
(let centerbud
  (makeArc 'white' bud [(+ xMid xcOut3) ycOut3] [(+ xMid xcOut3) ycOut3])
;
  (svg  [(rect '#83F52C' xOff yOff w h) leftleaf rightleaf centerbud]))))))))))))

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

boxGrid =
 "; A grid of boxes that can be enlarged with a slider
;
; Specifies the overlaid slider
(def xySlider_
  (\\(dropBall roundInt xStart xEnd yStart yEnd minx maxx miny maxy xcaption ycaption curx cury)
    (def [rCorner wEdge rBall] [4! 3! 10!])
    (def [xDiff yDiff xValDiff yValDiff] [(- xEnd xStart) (- yEnd yStart) (- maxx minx) (- maxy miny)])
    (def ballx (+ xStart (* xDiff (/ (- curx minx) xValDiff))))
    (def bally (+ yStart (* yDiff (/ (- cury miny) yValDiff))))
    (def ballx_ (clamp xStart xEnd ballx))
    (def bally_ (clamp yStart yEnd bally))
    (def rball_ (if dropBall (if (< maxx curx) 0 rBall) rBall))
    (def rball__ (if dropBall (if (< maxy cury) 0 rball_) rBall))
    (def xval
      (def xval_ (clamp minx maxx curx))
      (if roundInt (round xval_) xval_))
    (def yval
      (def yval_ (clamp miny maxy cury))
      (if roundInt (round yval_) yval_))
    (def shapes
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
        (text (+ xEnd 10) (+ yStart (/ yDiff 2)) (+ ycaption (toString yval))) ])
  [ [ xval yval ] shapes ]))
(def xySlider (xySlider_ false))
;
; Some overall variables
(def [x0 y0 sep] [30! 30! 60!])
;
; The slider itself
(def [ [ nx ny ] boxSlider ] 
  (xySlider true 
    (- x0 10!)
    ;(+ x0 (* nx (* sep 50!))) 
    60000!
    (- y0 10!) 
    ;(+ y0 (* ny (* sep 50!)))
    60000!
    0!
    1000!
    0!
    1000!
    ''
    ''
    3
    2))
;
; Specifies the boxes in terms of the slider
(svg 
  (append
    (map
      (\\[i j] (square_ (+ x0 (mult i sep)) (+ y0 (mult j sep)) 50!))
      (cartProd (range 0! (- nx 1)) (range 0! (- ny 1))))
    boxSlider))

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

chicagoColors =
 "
; Version of Chicago Flag with color pickers.
; Toggle the Zones option.
; Notice the use of (basicZonesTail ...).

(def [x0 y0 ni nj pts w h] [40 40 0.5! 3.5! 6! 454 300])
(def [outerLen innerLen] [30 12])

(def starColor 412)
(def stripeColor 86)

(def background
  (let [d1 d2] [10! 20!]
  [ (rect 'white' (- x0 d1) (- y0 d1) (+ w d2) (+ h d2)) ]))

(def stripes
  (map (\\i
    (rect stripeColor x0 (+ y0 (* i h)) w (/ h 6!)))
  [(/ 1! 6!) (/ 2! 3!)]))

(def stars
  (map (\\i
    (let off (* i (/ w 4!))
    (nStar starColor 'none' 0 pts outerLen innerLen 0
      (+ x0 off) (+ y0 (/ h 2!)))))
  (range ni nj)))

(svg (concat
  [ background
    (basicZonesTail stripes)
    (basicZonesTail stars) ]))

"

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

(def [levels levelsSlider] (hSlider true 20! 500! 25! 1! 4! 'Levels ' 2))
(def [time timeSlider] (hSlider false 20! 500! 50! 0! 1! 'Time ' 0.0))

; What fraction of the final curve should we draw?
(def curveFractionToDraw
  (if (gt (* time 1.5) 1.0)
    (* (- (* time 1.5) 1.0) 2)
    0
  )
)

; For when all the Hilbert levels but the most detailed fade out together.
(def earlierLevelOpacity
  (if (gt curveFractionToDraw 0)
    (* (- 1.0 curveFractionToDraw) (- 1.0 curveFractionToDraw))
    1.0
  )
)

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
(defrec hilbertParts (\\(depth levelPartCount partNumber opacity centerX centerY width rotation orientation)
  (let thisLevel (hilbertPart centerX centerY width rotation orientation (* (/ 1 (+ 1 (* depth 2))) opacity))
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
                (let partAndFraction (* (* time 1.5) thisLevelPartCount)
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
              (if (gt curveFractionToDraw 0.0)
                (if (le depth 1)
                  (let partAndFraction (* curveFractionToDraw thisLevelPartCount)
                    (if (le partAndFraction thisPartNumber)
                      1
                      (if (ge partAndFraction (+ thisPartNumber 1))
                        0
                        (- 1 (- partAndFraction thisPartNumber))
                      )
                    )
                  )
                  earlierLevelOpacity
                )
                1
              )
              (if (gt animationFraction 0)
                (hilbertParts (- depth 1) thisLevelPartCount thisPartNumber opacity aniX aniY aniWidth aniRot aniOr)
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
(def hilbertPointsAnimated (\\(depth centerX centerY width rotation orientation)
  (if (gt curveFractionToDraw 0)
    (let allPoints (hilbertPoints depth centerX centerY width rotation orientation)
    (let count (len allPoints)
    (let countToDraw (floor (* curveFractionToDraw count))
    (let partialLineFraction (- (* curveFractionToDraw count) countToDraw)
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
(def hilbertCurve (\\(depth centerX centerY width rotation orientation)
  (let [[firstX firstY]|otherPoints] (hilbertPointsAnimated depth centerX centerY width rotation orientation)
    [(path 'none' 'blue' 5
      [ 'M' firstX firstY | (concatMap (\\[x y] ['L' x y]) otherPoints) ]
    )]
  )
))

(def elements [
  (hilbertParts (- levels 1) 1 0 earlierLevelOpacity 300 300 400 0 1)
  (hilbertCurve (- levels 1) 300 300 400 0 1)
])

(svg (append (concat elements) (concat [levelsSlider timeSlider])))
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
(def [topcolor botcolor shadowcol] [[231 76 60 1] [192 57 43 1] [180 180 180 1]])
(def tbbox (rect topcolor r 0! (- w (* 2! r)) h))
(def lrbox (rect topcolor 0! r w (- h (* 2! r))))
(def tlcirc (circle topcolor r r r))
(def trcirc (circle topcolor (- w r) r r))
(def blcirc (circle topcolor r (- h r) r))
(def brcirc (circle topcolor (- w r) (- h r) r))

(def btbbox (rect botcolor r (- h r) (- w (* 2! r)) depth))
(def blrbox (rect botcolor 0! (- h r) w (- depth r)))
(def belcirc (circle botcolor r (- (+ h depth) (* 2! r)) r))
(def bercirc (circle botcolor (- w r) (- (+ h depth) (* 2! r)) r))

(def stbbox (rect shadowcol r (+ shadow (- h r)) (- w (* 2! r)) depth))
(def slrbox (rect shadowcol 0! (+ shadow (- h r)) w (- depth r)))
(def selcirc (circle shadowcol r (+ shadow (- (+ h depth) (* 2! r))) r))
(def sercirc (circle shadowcol (- w r) (+ shadow (- (+ h depth) (* 2! r))) r))

(def btop    [tbbox lrbox tlcirc trcirc blcirc brcirc] )
(def bbot    [btbbox blrbox belcirc bercirc] )
(def bshadow [stbbox slrbox selcirc sercirc] )

(def raisedButton (svgViewBox 120 (+ depth (+ shadow (- h r))) (concat [bshadow bbot btop])))

(def [w h depth shadow r] [120 22.5 4 0.5 2])
(def [topcolor botcolor shadowcol] [[233 94 80 1] [198 77 64 1] [180 180 180 1]])
(def tbbox (rect topcolor r 0! (- w (* 2! r)) h))
(def lrbox (rect topcolor 0! r w (- h (* 2! r))))
(def tlcirc (circle topcolor r r r))
(def trcirc (circle topcolor (- w r) r r))
(def blcirc (circle topcolor r (- h r) r))
(def brcirc (circle topcolor (- w r) (- h r) r))

(def btbbox (rect botcolor r (- h r) (- w (* 2! r)) depth))
(def blrbox (rect botcolor 0! (- h r) w (- depth r)))
(def belcirc (circle botcolor r (- (+ h depth) (* 2! r)) r))
(def bercirc (circle botcolor (- w r) (- (+ h depth) (* 2! r)) r))

(def stbbox (rect shadowcol r (+ shadow (- h r)) (- w (* 2! r)) depth))
(def slrbox (rect shadowcol 0! (+ shadow (- h r)) w (- depth r)))
(def selcirc (circle shadowcol r (+ shadow (- (+ h depth) (* 2! r))) r))
(def sercirc (circle shadowcol (- w r) (+ shadow (- (+ h depth) (* 2! r))) r))

(def btop    [tbbox lrbox tlcirc trcirc blcirc brcirc] )
(def bbot    [btbbox blrbox belcirc bercirc] )
(def bshadow [stbbox slrbox selcirc sercirc] )

(def highlightedButton (svgViewBox 120 (+ depth (+ shadow (- h r))) (concat [bshadow bbot btop])))

(def [w h depth shadow r] [120 22.5 4 0.5 2])
(def [topcolor botcolor shadowcol] [[233 94 80 1] [198 77 64 1] [180 180 180 1]])
(def offset (* depth 0.375!))

(def tbbox (rect topcolor r offset (- w (* 2! r)) h))
(def lrbox (rect topcolor 0! (+ r offset) w (- h (* 2! r))))
(def tlcirc (circle topcolor r (+ r offset) r))
(def trcirc (circle topcolor (- w r) (+ r offset) r))
(def blcirc (circle topcolor r (+ (- h r) offset) r))
(def brcirc (circle topcolor (- w r) (+ (- h r) offset) r))

(def btbbox (rect botcolor r (- h r) (- w (* 2! r)) depth))
(def blrbox (rect botcolor 0! (- h r) w (- depth r)))
(def belcirc (circle botcolor r (- (+ h depth) (* 2! r)) r))
(def bercirc (circle botcolor (- w r) (- (+ h depth) (* 2! r)) r))

(def btop    [tbbox lrbox tlcirc trcirc blcirc brcirc] )
(def bbot    [btbbox blrbox belcirc bercirc] )

(def depressedButton (svgViewBox 120 (+ depth (+ shadow (- h r))) (concat [bbot btop])))

(def [w h depth shadow r] [120 22.5 4 0.5 2])
(def [topcolor botcolor shadowcol] [[236 112 99 1] [205 97 85 1] [180 180 180 1]])
(def tbbox (rect topcolor r 0! (- w (* 2! r)) h))
(def lrbox (rect topcolor 0! r w (- h (* 2! r))))
(def tlcirc (circle topcolor r r r))
(def trcirc (circle topcolor (- w r) r r))
(def blcirc (circle topcolor r (- h r) r))
(def brcirc (circle topcolor (- w r) (- h r) r))

(def btbbox (rect botcolor r (- h r) (- w (* 2! r)) depth))
(def blrbox (rect botcolor 0! (- h r) w (- depth r)))
(def belcirc (circle botcolor r (- (+ h depth) (* 2! r)) r))
(def bercirc (circle botcolor (- w r) (- (+ h depth) (* 2! r)) r))

(def stbbox (rect shadowcol r (+ shadow (- h r)) (- w (* 2! r)) depth))
(def slrbox (rect shadowcol 0! (+ shadow (- h r)) w (- depth r)))
(def selcirc (circle shadowcol r (+ shadow (- (+ h depth) (* 2! r))) r))
(def sercirc (circle shadowcol (- w r) (+ shadow (- (+ h depth) (* 2! r))) r))

(def btop    [tbbox lrbox tlcirc trcirc blcirc brcirc] )
(def bbot    [btbbox blrbox belcirc bercirc] )
(def bshadow [stbbox slrbox selcirc sercirc] )

(def disabledButton (svgViewBox 120 (+ depth (+ shadow (- h r))) (concat [bshadow bbot btop])))

(def [picknum pickslider] (hSlider true 20! 300! 40! 1! 4! 'Button State ' 1))

(def showbutton (\\i (if (< i 2) raisedButton (if (< i 3) highlightedButton (if (< i 4) depressedButton disabledButton)))))

(svg (cons (showbutton picknum) pickslider))

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

-- LITTLE_TO_ELM surveyResults
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


examples =
  [ makeExample scratchName scratch
  , makeExample "*Prelude*" Prelude.src
  , makeExample "3 Boxes" threeBoxes
  , makeExample "N Boxes" groupOfBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Thaw/Freeze" thawFreeze
  , makeExample "Logo" logo
  , makeExample "Logo 2" logo2
  , makeExample "Elm Logo" elmLogo
  , makeExample "Active Trans Logo" activeTrans
  , makeExample "Botanic Garden Logo" botanic
  , makeExample "Rings" rings
  , makeExample "Polygons" polygons
  , makeExample "Stars" stars
  , makeExample "Clique" clique
  , makeExample "Sliders" sliders
  , makeExample "Buttons" buttons
  , makeExample "Widgets" widgets
  , makeExample "xySlider" xySlider
  , makeExample "Color Picker" rgba
  , makeExample "Box Grid" boxGrid
  , makeExample "Bar Graph" barGraph
  , makeExample "Chicago Flag" chicago
  , makeExample "Chicago Flag 2" chicagoColors
  , makeExample "US-13 Flag" usFlag13
  , makeExample "US-50 Flag" usFlag50
  , makeExample "French Sudan Flag" frenchSudan
  , makeExample "Frank Lloyd Wright" flw1
  , makeExample "Frank Lloyd Wright B" flw2
  , makeExample "Ferris Wheel" ferris
  , makeExample "Pie Chart" pieChart1
  , makeExample "Solar System" solarSystem
  , makeExample "Bezier Curves" bezier
  , makeExample "Fractal Tree" fractalTree
  , makeExample "Hilbert Curve Animation" hilbertCurveAnimation
  , makeExample "Stick Figures" stickFigures
  , makeExample "Sailboat" sailBoat
  , makeExample "Eye Icon" eyeIcon
  , makeExample "Wikimedia Logo" wikimedia
  , makeExample "Haskell.org Logo" haskell
  , makeExample "Cover Logo" cover
  , makeExample "POP-PL Logo" poppl
  , makeExample "Matrix Transformations" matrices
  , makeExample "Cult of Lambda" cultOfLambda
  , makeExample "Misc Shapes" miscShapes
  , makeExample "Interface Buttons" interfaceButtons
  , makeExample "Paths 1" paths1
  , makeExample "Paths 2" paths2
  , makeExample "Paths 3" paths3
  , makeExample "Paths 4" paths4
  , makeExample "Paths 5" paths5
  , makeExample "Sample Rotations" rotTest
  -- , makeExample "Survey Results" surveyResults
  , makeExample "Grid Tile" gridTile
  , makeExample "Lillicon P" lilliconP
  ]

list = examples
