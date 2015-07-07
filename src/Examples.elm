module Examples (list, scratchName, scratch) where

import Lang
import LangParser
import Eval
import MicroTests
import Utils

makeExample name s =
  let thunk () =
    let e = Utils.fromOk_ (LangParser.parseE s) in
    let v = Eval.run e in
    {e=e, v=v}
  in
  (name, thunk)

--------------------------------------------------------------------------------

logo = "
  ; sketch-n-sketch logo
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

threeBoxes = "
(def threeBoxesInt
  (let [x0 y0 w h sep] [40 28 60 130 110]
  (let boxi (\\i
    (let xi (+ x0 (mult i sep))
    (rect 'lightblue' xi y0 w h)))
  (svg (map boxi [0 1 2])))))
;
threeBoxesInt
"

sixBoxesA = "
  (let [x0 y0 sep] [10 28 60]
  (svg
    (map (\\[i j] (square_ (+ x0 (mult i sep)) (+ y0 (mult j sep)) 50))
         (cartProd [0 1 2] [0 1]))))
"

sixBoxesB = "
  (let [x0 y0 xsep ysep] [10 28 60 60]
  (svg
    (map (\\[i j] (square_ (+ x0 (mult i xsep)) (+ y0 (mult j ysep)) 50))
         (cartProd [0 1 2] [0 1]))))
"

scratch = "
  ;
  ; Write a little program below.
  ; Or choose an example from the list.
  ;
  ; Changes made to this *Scratch* example will be saved and
  ; restored when navigating to and from other examples.
  ; For the remaining named examples, changes will be discarded
  ; when choosing a different example.
  ;
  (svg [(rect 'maroon' 100 15 200 50)])
"

elmLogo = "
  ; Elm logo, based on:
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

rings = "
  (let [x0 y0 w r dx dy] [30 30 7 20 32 20]
  (let dxHalf (div dx 2)
  (let row1
    (map (\\[i c] (ring c w (+ x0 (mult i dx)) y0 r))
         (zip [0 1 2] ['blue' 'black' 'red']))
  (let row2
    (map (\\[i c] (ring c w (+ (+ x0 dxHalf) (mult i dx)) (+ y0 dy) r))
         (zip [0 1] ['yellow' 'green']))
    (svg (append row1 row2))))))
"

polygons = "
  ; TODO polygons, based on tests 45,46,47
  (svg [])
"

stars = "
  ;
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
  ;
  (let [x0 y0 sep ni nj] [100 100 100 3! 7!]
  (let [outerLen innerLen] [50 20]
  (let iStar (\\i
     (let off (mult (- i ni) sep)
     (let [xi yi] [(+ x0 off) (+ y0 off)]
     (nStar 'goldenrod' 'black' 3 i outerLen innerLen 0! xi yi))))
  ;
  (svg (map iStar (range ni nj)))))))
"

sliders = "
  ;
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

buttons = "
  ;
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

widgets = "
  ; library widgets
  ;
  (let [n  s1] (hSlider false 20! 90!  20! 0! 5! 'n = ' 3.1415)
  (let [i  s2] (hSlider true  20! 90!  50! 0! 5! 'i = ' 3.1415)
  (let [b1 s3] (button        20!      80!       'b1 = ' 0.25)
  (let [b2 s4] (button        20!     110!       'b2 = ' 0.75)
    (svg (concat [s1 s2 s3 s4]))))))
"

clique = "
  ;
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

frenchSudan = "
    ;
    ; The Flag of French Sudan, based on:
    ;
    ; A few ways to manipulate:
    ; - Grab any part of the stick figure and move it
    ; in various directions
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

usFlag50 = "
    ;
    ; Current Flag of the United States
    ;
    ; A few ways to manipulate:
    ; - Grab various parts of the red stripes or blue block and pull in various directions
    ; - grab the edges of the circles and and increase or decrease the radius
    ;
    (let [x0 y0 ni nj pts w h rad] [108 20 0! 12! 5! 510 272 6]
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
        (append
          base
          (map
            (\\[i j]
              (let xsep (/ w 15!)
              (let ysep (* hstripe 1.3!)
                (circle
                  'white'
                  (+ x0 (* i xsep))
                  (+ y0 (* j ysep))
                  rad))))
          (append (cartProd (range 0.5! 5.5!) (range 0.75! 4.75!)) (cartProd (range 1! 5!) (range 1.2! 4.2!))))))))))))
"

usFlag13 = "
    ;
    ; Original flag of the United States
    ;
    ; A few ways to mainpulate this example:
    ; - Grab bottom right corner to increase overall size
    ; - Grab the edge of a red stripe to increase width
    ; - Grab the points of one of the stars to change 
    ; the size of its points
    ;
    (let rotate (\\a (/ (* a (pi)) 6.5!))
    (let [x0 y0 ni nj pts w h] [108 20 0! 12! 5! 500 260]
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
        (append
          base
          (map
            (\\i
                (nStar
                  'white'
                  'none'
                  0
                  pts
                  innerLen
                  outerLen
                  (rotate  i)
                  (+ (+ x0 (/ w 6!)) (* min (cos (rotate  i))))
                  (+ (+ y0 (* hstripe 3.5!)) (* min (sin (rotate  i))))))
          (range ni nj))))))))))))))
"

flw1 = "
    ;
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

flw2 = "
  ;
  ; A Frank Lloyd Wright design based on:
  ; http://www.glass-by-design.com/images3/skylight3.jpg
  ;
  ; This is a tiled version of that design
  ;
  ; Possible ways to manipulate:
  ; - Grab edges of red polygons, yellow circles, or blue ellipses
  ; and pull in various directions
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

chicago = "
  ;
  ; The flag of Chicago
  ;
  ; Possible ways to manipulate
  ; - Grab stripes or stars and pull in various directions
  ;
  (let [x0 y0 ni nj pts w h] [108 113 0.5! 3.5! 6! 454 300]
  (let [outerLen innerLen] [30 12]
  (let stripes
    (map
      (\\i
        (rect
          'lightblue'
          x0
          (+ y0 (* i h))
          w
          (/ h 6!)))
      [(/ 1! 6!) (/ 2! 3!)])
  ;
    (svg 
      (cons (rect 'white' (- x0 10!) (- y0 10!) (+ w 20!) (+ h 20!))
      (append
        stripes
        (map
          (\\i
            (let off (* i (/ w 4!))
              (nStar 'red' 'none' 0 pts outerLen innerLen 0 (+ x0 off) (+ y0 (/ h 2!)))))
          (range ni nj))))))))
"

ferris = "
  ;
  ; Take this ferris wheel for a spin!
  ; try: unfreezing cx and cy and translating wheel
  ;      unfreezing center radius and stretching it
  ;      unfreezing wCar and changing size of box
  ;
  (let [numSpokes_ spokeLen_ rotAngle_] [5 80 0]
  (let showSliders true
  ;
  (let [numSpokes s1] (hSlider true 20! 420! 20! 3! 10! '' numSpokes_)
  (let [spokeLen s2] (hSlider true 20! 420! 50! 40! 200! '' spokeLen_)
  (let [rotAngle s3] (hSlider false 20! 420! 80! (neg twoPi) twoPi '' rotAngle_)
  ;
  (let sliders (if showSliders (concat [s1 s2 s3]) [])
  (let wheel
    (let [cx cy] [220! 300!]
    (let rim [(ring 'darkgray' 8! cx cy spokeLen)]
    (let center [(circle 'black' cx cy 20!)]
    (let frame [(nStar 'goldenrod' 'darkgray' 3! numSpokes spokeLen 0! rotAngle cx cy)]
    (let spokePts (nPointsOnCircle numSpokes rotAngle cx cy spokeLen)
    (let caps (map (\\[x y] (circle 'black' x y 7!)) spokePts)
    (let cars
      (let wCar 30!
      (let wHalfCar (/ wCar 2!)
      (map (\\[x y] (squareCenter 'lightgray' x y wCar)) spokePts)))
    (concat [rim cars center frame caps]))))))))
  ;
  (svg (append sliders wheel)))))))))
"

activeTrans = "
  ;
  ; Logo based on Active Transportation Alliance (http://activetrans.org/)
  ;
  ; Possible ways to manipulate:
  ; - Grab edge of city skyline and pull up and down
  ; - Click 'Show Zones' and mess with the curves at the bottom of the logo
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
  ; TODO unfreeze
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

rgba = "
    ;
    ; A Color Picker
    ; 
    ; Move the sliders to change the rgba value of the circle!
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
piechart1 = "
  ;
  ; A Pie Chart
  ;
  ; Move the sliders to change the size of a particular slice
  ;
  (let [count1_ count2_ count3_ count4_ count5_] [35 31 16 10 8]
  (let [count1 s1] (hSlider true 20! 420! 20! 0! 100! '' count1_)
  (let [count2 s2] (hSlider true 20! 420! 50! 0! 100! '' count2_)
  (let [count3 s3] (hSlider true 20! 420! 80! 0! 100! '' count3_)
  (let [count4 s4] (hSlider true 20! 420! 110! 0! 100! '' count4_)
  (let [count5 s5] (hSlider true 20! 420! 140! 0! 100! '' count5_)
  (let total (+ count1 (+ count2 (+ count3 (+ count4 count5))))
  (let p2 (+ count1 count2)
  (let p3 (+ p2 count3)
  (let p4 (+ p3 count4)
  (let p5 (+ p4 count5)
  ;
  (let sliders (concat [s1 s2 s3 s4 s5])
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
          [['#8DEEEE' f1 [(+ cx 180!) cy] [(+ cx x1) (+ cy y1)]]
          ['#66CCCC' f2 [(+ cx x1) (+ cy y1)] [(+ cx x2) (+ cy y2)]]
          ['#49E9BD' f3 [(+ cx x2) (+ cy y2)] [(+ cx x3) (+ cy y3)]]
          ['#5EDA9E' f4 [(+ cx x3) (+ cy y3)] [(+ cx x4) (+ cy y4)]]
          ['#00FA9A' f5 [(+ cx x4) (+ cy y4)] [(+ cx x5) (+ cy y5)]]])
    wedges))))))))))
  ;
  (svg (cons (circle 'lightgray' cx cy (* 1.1 r)) (append sliders pie)))))))))))))))))
"

botanic = "
  ;
  ; Logo: Chicago Botanic Garden
  ;
  ; Click 'Show Zones' to see the control points for the various Bezier curves
  ; in this example.
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

solarSystem = "
  ; Visualization of the solar system 
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
  (def rings (reverse (map (\\orb (ring 'lightgrey' 2! ox oy (* aupx orb)))
                  [ merorb venorb earorb marorb juporb satorb uraorb neporb ])))
  (def [time timeslider] (hSlider true 20! 600! 20! 1! 1000! 'Day ' 1))
  (def rev (\\(x f) (f x)))
  (def planets (map (rev (/ time 365)) [mercury venus earth mars jupiter saturn uranus neptune]))
  (svg (concat [ rings [sun | planets] timeslider ]))
"

cultoflambda = "
    ;
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

stickfigures = "
  ;
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
--------------------------------------------------------------------------------

todo = "
  ; TODO
  (svg [])
"

scratchName = "*Scratch*"

examples =
  [ makeExample scratchName scratch
  , makeExample "3 Boxes" threeBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Logo" logo
  , makeExample "Elm Logo" elmLogo
  , makeExample "Active Trans Logo" activeTrans
  , makeExample "Botanic Garden Logo" botanic
  , makeExample "Rings" rings
  , makeExample "Polygons" polygons
  , makeExample "Stars" stars
  , makeExample "Sliders" sliders
  , makeExample "Buttons" buttons
  , makeExample "Widgets" widgets
  , makeExample "Color Picker" rgba
  , makeExample "US-13 Flag" usFlag13
  , makeExample "US-50 Flag" usFlag50
  , makeExample "Chicago Flag" chicago
  , makeExample "French Sudan Flag" frenchSudan
  , makeExample "Frank Lloyd Wright" flw1
  , makeExample "F.L. Wright Tiled" flw2
  , makeExample "Ferris Wheel" ferris
  , makeExample "Pie Chart" piechart1
  , makeExample "Stick Figures" stickfigures
  , makeExample "Cult of Lambda" cultoflambda 
  , makeExample "Clique" clique
  ]

list = examples ++ MicroTests.sampleTests

