module PreludeGenerated (src) where

prelude =
 "
; prelude.little
;
; This little library is accessible by every program.
; This is not an example that generates an SVG canvas,
; but we include it here for reference.

;; id : a -> a
;; The identity function - given a value, returns exactly that value
(def id (\\x x))

;; always : a -> b -> a
;; A function that always returns the same value a, regardless of b
(def always (\\(x _) x))

;; compose : (a -> b)-> (b -> c) -> (a -> c)
;; Composes two functions together
(def compose (\\(f g) (\\x (f (g x)))))

(def flip (\\(f x y) (f y x)))

;; fst : List a -> a
;; Returns the first element of a given list
(def fst (\\[x|_] x))

(def snd (\\[_ y|_] y))

;; len : List a -> Int
;; Returns the length of a given list
(defrec len (\\xs (case xs ([] 0) ([_ | xs1] (+ 1 (len xs1))))))

;; map : (a -> b) -> List a -> List b
;; Maps a function, f, over a list of values and returns the resulting list
(defrec map (\\(f xs)
  (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)]))))

;; map2 : (a -> b -> c) -> List a -> List b -> List c
;; Combines two lists with a given function, extra elements are dropped
(defrec map2 (\\(f xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] [ (f x y) | (map2 f xs1 ys1) ])
    (_                 []))))

;; foldl : (a -> b -> b) -> b -> List a -> b
;; Takes a function, an accumulator, and a list as input and reduces using the function from the left
(defrec foldl (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (foldl f (f x acc) xs1)))))

;; foldl : (a -> b -> b) -> b -> List a -> b
;; Takes a function, an accumulator, and a list as input and reduces using the function from the right
(defrec foldr (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (f x (foldr f acc xs1))))))

;; append : List a -> List a -> List a
;; Given two lists, append the second list to the end of the first
(defrec append (\\(xs ys)
  (case xs ([] ys) ([x|xs1] [ x | (append xs1 ys)]))))

;; concat : List (List a) -> List a
;; concatenate a list of lists into a single list
(def concat (foldr append []))

;; concatMap : (a -> List b) -> List a -> List b
;; Map a given function over a list and concatenate the resulting list of lists
(def concatMap (\\(f xs) (concat (map f xs))))

;; cartProd : List a -> List b -> List [a b]
;; Takes two lists and returns a list that is their cartesian product
(def cartProd (\\(xs ys)
  (concatMap (\\x (map (\\y [x y]) ys)) xs)))

;; zip : List a -> List b -> List [a b]
;; Takes elements at the same position from two input lists and returns a list of pairs of these elements
(def zip (map2 (\\(x y) [x y])))

;; nil : List a
;; The empty list
(def nil  [])

;; cons : a -> List a -> List a
;; attaches an element to the front of a list
(def cons (\\(x xs) [x | xs]))

;; snoc : a -> List a -> List a
;; attaches an element to the end of a list
(def snoc (\\(x ys) (append ys [x])))

;; hd : List a -> a
;; Returns the first element of a given list
(def hd   (\\[x|xs] x))

;; tl : List a -> a
;; Returns the last element of a given list
(def tl   (\\[x|xs] xs))

;; reverse : List a -> List a
;; Given a list, reverse its order
(def reverse (foldl cons nil))

;; range : a -> a -> List a
;; Given two numbers, creates the list between them (inclusive)
(defrec range (\\(i j)
  (if (< i (+ j 1))
      (cons i (range (+ i 1) j))
      nil)))

;; list0N : a -> List a
;; Given a number, create the list of 0 to that number inclusive (number must be > 0)
(def list0N
  (letrec foo (\\i (if (< i 0) nil (cons i (foo (- i 1)))))
  (compose reverse foo)))

;; list1N : a -> List a
;; Given a number, create the list of 1 to that number inclusive
(def list1N (\\n (range 1 n)))

(def zeroTo (\\n (range 0 (- n 1))))

;; repeat : Int -> a -> List a
;; Given a number n and some value x, return a list with x repeated n times
(def repeat (\\(n x) (map (always x) (range 1 n))))

;; intermingle : List a -> List a -> List a
;; Given two lists, return a single list that alternates between their values (first element is from first list)
(defrec intermingle (\\(xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] (cons x (cons y (intermingle xs1 ys1))))
    ([[]      []]      nil)
    (_                 (append xs ys)))))

(def mapi (\\(f xs) (map f (zip (range 0 (- (len xs) 1)) xs))))

(defrec nth (\\(xs n)
  (if (< n 0)   'ERROR: nth'
    (case xs
      ([]       'ERROR: nth')
      ([x|xs1]  (if (= n 0) x (nth xs1 (- n 1))))))))

(defrec take (\\(xs n)
  (if (= n 0) []
    (case xs
      ([]      'ERROR: take')
      ([x|xs1] [x | (take xs1 (- n 1))])))))

;; mult : Number -> Number -> Number
;; multiply two numbers and return the result
(defrec mult (\\(m n)
  (if (< m 1) 0 (+ n (mult (+ m -1) n)))))

;; minus : Number -> Number -> Number
;; Given two numbers, subtract the second from the first
(def minus (\\(x y) (+ x (mult y -1))))

;; div : Number -> Number -> Number
;; Given two numbers, divide the first by the second
(defrec div (\\(m n)
  (if (< m n) 0
  (if (< n 2) m
    (+ 1 (div (minus m n) n))))))

;; neg : Number -> Number
;; Given a number, returns the negative of that number
(def neg (\\x (- 0 x)))

;; not : Bool -> Bool
;; Given a bool, returns the opposite boolean value
(def not (\\b (if b false true)))

;; implies : Bool -> Bool -> Bool
;; Given two bools, returns a bool regarding if the first argument is true, then the second argument is as well
(def implies (\\(p q) (if p q true)))

(def or  (\\(p q) (if p true q)))
(def and (\\(p q) (if p q false)))

(defrec some (\\(p xs)
  (case xs
    ([]      false)
    ([x|xs1] (or (p x) (some p xs1))))))

(defrec all (\\(p xs)
  (case xs
    ([]      true)
    ([x|xs1] (and (p x) (all p xs1))))))

;; clamp : Number -> Number -> Number -> Number
;; Given an upper bound, lower bound, and a number, restricts that number between those bounds (inclusive)
;; Ex. clamp 1 5 4 = 4
;; Ex. clamp 1 5 6 = 5
(def clamp (\\(i j n) (if (< n i) i (if (< j n) j n))))

(def between (\\(i j n) (= n (clamp i j n))))

(def lt (\\(x y) (< x y)))
(def eq (\\(x y) (= x y)))
(def le (\\(x y) (or (lt x y) (eq x y))))
(def gt (flip lt))
(def ge (\\(x y) (or (gt x y) (eq x y))))

(def min (\\(i j) (if (lt i j) i j)))
(def max (\\(i j) (if (gt i j) i j)))

(def minimum (\\[hd|tl] (foldl min hd tl)))
(def maximum (\\[hd|tl] (foldl max hd tl)))

(def mapi (\\(f xs) (map f (zip (range 0 (- (len xs) 1)) xs))))

(defrec nth (\\(xs n)
  (if (< n 0)       'ERROR: nth'
    (case [n xs]
      ([_ []]       'ERROR: nth')
      ([0 [x|xs1]]  x)
      ([_ [x|xs1]]  (nth xs1 (- n 1)))))))

(def take
  (letrec take_ (\\(n xs)
    (case [n xs]
      ([0 _]       [])
      ([_ []]      [])
      ([_ [x|xs1]] [x | (take_ (- n 1) xs1)])))
  (compose take_ (max 0))))

(defrec elem (\\(x ys)
  (case ys
    ([]      false)
    ([y|ys1] (or (= x y) (elem x ys1))))))

;; joinStrings : String -> List String -> String
;; Combine a list of strings with a given separator
;; Ex. joinStrings ', ' ['hello' 'world'] = 'hello, world'
(def joinStrings (\\(sep ss)
  (foldr (\\(str acc) (if (= acc '') str (+ str (+ sep acc)))) '' ss)))

;; concatStrings : List String -> String
;; Concatenate a list of strings and return the resulting string
(def concatStrings (joinStrings ''))

;; spaces : List String -> String
;; Concatenates a list of strings, interspersing a single space in between each string
(def spaces (joinStrings ' '))

;; delimit : String -> String -> String -> String
;; First two arguments are appended at the front and then end of the third argument correspondingly
;; Ex. delimit '+' '+' 'plus' = '+plus+'
(def delimit (\\(a b s) (concatStrings [a s b])))

;; parens : String -> String
;; delimit a string with parentheses
(def parens (delimit '(' ')'))

;
; SVG Manipulating Functions
;

;; circle : String -> Number -> Number -> Number -> Shape
;; argument order - color, x, y, radius
;; creates a circle, center at (x,y) with given radius and color
(def circle (\\(fill x y r)
  ['circle'
     [['cx' x] ['cy' y] ['r' r] ['fill' fill]]
     []]))

;; ring : String -> Number -> Number -> Number -> Number -> Shape
;; argument order - color, width, x, y, radius
;; Just as circle, except new width parameter determines thickness of ring
(def ring (\\(c w x y r)
  ['circle'
     [ ['cx' x] ['cy' y] ['r' r] ['fill' 'none'] ['stroke' c] ['stroke-width' w] ]
     []]))

;; ellipse : String -> Number -> Number -> Number -> Number -> Shape
;; argument order - color, x, y, x-radius, y-radius
;; Just as circle, except radius is separated into x and y parameters
(def ellipse (\\(fill x y rx ry)
  ['ellipse'
     [ ['cx' x] ['cy' y] ['rx' rx] ['ry' ry] ['fill' fill] ]
     []]))

;; rect : String -> Number -> Number -> Number -> Number -> Shape
;; argument order - color, x, y, width, height
;; creates a rectangle of given width and height with (x,y) as the top left corner coordinate
(def rect (\\(fill x y w h)
  ['rect'
     [ ['x' x] ['y' y] ['width' w] ['height' h] ['fill' fill] ]
     []]))

;; square : String -> Number -> Number -> Number -> Shape
;; argument order - color, x, y, side
;; Similar to rect, but only needs one parameter for all four sides
(def square (\\(fill x y side) (rect fill x y side side)))

;; line : String -> Number -> Number -> Number -> Number -> Number -> Shape
;; argument order - color, width, x1, y1, x1, y2
;; creates a line from (x1, y1) to (x2,y2) with given color and width
(def line (\\(fill w x1 y1 x2 y2)
  ['line'
     [ ['x1' x1] ['y1' y1] ['x2' x2] ['y2' y2] ['stroke' fill] ['stroke-width' w] ]
     []]))

;; polygon: String -> String -> Number -> List (List Number) -> Shape
;; argument order - fill, stroke, width, points
;; creates a polygon following the list of points, with given fill color and a border with given width and stroke
(def polygon (\\(fill stroke w pts)
  ['polygon'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []]))

;; polyline: String -> String -> Number -> List (List Number) -> Shape
;; argument order - fill, stroke, width, points
;; See polygon
(def polyline (\\(fill stroke w pts)
  ['polyline'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []]))

;; path: String -> String -> Number -> List a -> Shape
;; argument order - fill, stroke, width, d
;; Given SVG path command d, create path with given fill color, stroke and width
;; See https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths for path command info
(def path (\\(fill stroke w d)
  ['path'
     [ ['fill' fill] ['stroke' stroke] ['stroke-width' w] ['d' d] ]
     []]))

;; text : Number -> Number -> String -> Shape
;; argument order - x, y, string
;; place a text string with top left corner at (x,y) - with default color & font
(def text (\\(x y s)
   ['text' [['x' x] ['y' y] ['style' 'fill:black']
            ['font-family' 'Tahoma, sans-serif']]
           [['TEXT' s]]]))

;; addAttr : Shape-> Attribute -> Shape
;; argument order - shape, new attribute
;; Add a new attribute to a given Shape
(def addAttr (\\([shapeKind oldAttrs children] newAttr)
  [shapeKind (snoc newAttr oldAttrs) children]))

(def consAttr (\\([shapeKind oldAttrs children] newAttr)
  [shapeKind (cons newAttr oldAttrs) children]))

;; svg : List Shape -> SVG
;; Given a list of shapes, compose into a single SVG
(def svg (\\shapes ['svg' [] shapes]))

;; svgViewBox : Number -> Number -> List Shape -> SVG
;; argument order - x-maximum, y-maximum, shapes
;; Given a list of shapes, compose into a single SVG within the x & y maxima
(def svgViewBox (\\(xMax yMax shapes)
  (let [sx sy] [(toString xMax) (toString yMax)]
  ['svg'
    [['x' '0'] ['y' '0'] ['viewBox' (joinStrings ' ' ['0' '0' sx sy])]]
    shapes])))

;; rectCenter : String -> Number -> Number -> Number -> Number -> Shape
;; As rect, except x & y represent the center of the defined rectangle
(def rectCenter (\\(fill cx cy w h)
  (rect fill (- cx (/ w 2)) (- cy (/ h 2)) w h)))

;; squareCenter : String -> Number -> Number -> Number -> Shape
;; As square, except x & y represent the center of the defined rectangle
(def square (\\(fill x y w) (rect fill x y w w)))
(def squareCenter (\\(fill cx cy w) (rectCenter fill cx cy w w)))

;; Some shapes with given default values for fill, stroke, and stroke width
; TODO remove these
(def circle_    (circle 'red'))
(def ellipse_   (ellipse 'orange'))
(def rect_      (rect '#999999'))
(def square_    (square '#999999'))
(def line_      (line 'blue' 2))
(def polygon_   (polygon 'green' 'purple' 3))
(def path_      (path 'transparent' 'goldenrod' 5))

;; updateCanvas : SVG -> SVG -> SVG
;; updates an SVG by comparing differences with another SVG
;; Note: accDiff pre-condition: indices in increasing order
;; (so can't just use foldr instead of reverse . foldl)
(def updateCanvas (\\([_ svgAttrs oldShapes] diff)
  (let oldShapesI (zip (list1N (len oldShapes)) oldShapes)
  (let initAcc [[] diff]
  (let f (\\([i oldShape] [accShapes accDiff])
    (case accDiff
      ([]
        [(cons oldShape accShapes) accDiff])
      ([[j newShape] | accDiffRest]
        (if (= i j)
          [(cons newShape accShapes) accDiffRest]
          [(cons oldShape accShapes) accDiff]))))
  (let newShapes (reverse (fst (foldl f initAcc oldShapesI)))
    ['svg' svgAttrs newShapes]))))))

(def addShapeToCanvas (\\(['svg' svgAttrs oldShapes] newShape)
  ['svg' svgAttrs (append oldShapes [newShape])]))

(def addShape (flip addShapeToCanvas))

(def addShapesToCanvas (\\(['svg' svgAttrs oldShapes] newShapes)
  ['svg' svgAttrs (append oldShapes newShapes)]))

(def addShapes (flip addShapesToCanvas))

(def groupMap (\\(xs f) (map f xs)))

(def autoChose (\\(_ x _) x))
(def inferred  (\\(x _ _) x))
(def flow (\\(_ x) x))

(defrec lookupWithDefault (\\(default k dict)
  (let foo (lookupWithDefault default k)
  (case dict
    ([]            default)
    ([[k1 v]|rest] (if (= k k1) v (foo rest)))))))

(defrec lookup (\\(k dict)
  (let foo (lookup k)
  (case dict
    ([]            'NOTFOUND')
    ([[k1 v]|rest] (if (= k k1) v (foo rest)))))))

(defrec addExtras (\\(i extras shape)
  (case extras
    ([] shape)
    ([[k table] | rest]
      (let v (lookup i table)
      (if (= v 'NOTFOUND')
          (addExtras i rest shape)
          (addExtras i rest (addAttr shape [k v]))))))))

(def lookupAttr (\\([_ attrs _] k) (lookup k attrs)))

; \"constant folding\"
(def twoPi (* 2 (pi)))
(def halfPi (/ (pi) 2))

;; nPointsOnUnitCircle : Number -> Number -> List Number
;; Helper function for nPointsOnCircle, calculates angle of points
;; Note: angles are calculated clockwise from the traditional pi/2 mark
(def nPointsOnUnitCircle (\\(n rot)
  (let off (- halfPi rot)
  (let foo (\\i
    (let ang (+ off (* (/ i n) twoPi))
    [(cos ang) (neg (sin ang))]))
  (map foo (list0N (- n 1)))))))

;; nPointsOnCircle : Number -> Number -> Number -> Number -> Number -> List Number
;; argument order - Number of points, degree of rotation, x-center, y-center, radius
;; Scales nPointsOnUnitCircle to the proper size and location with a given radius and center
(def nPointsOnCircle (\\(n rot cx cy r)
  (let pts (nPointsOnUnitCircle n rot)
  (map (\\[x y] [(+ cx (* x r)) (+ cy (* y r))]) pts))))

;; nStar : color -> color -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Shape
;; argument order -
;; fill color - interior color of star
;; stroke color - border color of star
;; width - thickness of stroke
;; points - number of star points
;; len1 - length from center to one set of star points
;; len2 - length from center to other set of star points (either inner or outer compared to len1)
;; rot - degree of rotation
;; cx - x-coordinate of center position
;; cy - y-coordinate of center position
;; Creates stars that can be modified on a number of parameters
(def nStar (\\(fill stroke w n len1 len2 rot cx cy)
  (let pti (\\[i len]
    (let anglei (+ (- (/ (* i (pi)) n) rot) halfPi)
    (let xi (+ cx (* len (cos anglei)))
    (let yi (+ cy (neg (* len (sin anglei))))
      [xi yi]))))
  (let lengths
    (map (\\b (if b len1 len2))
         (concat (repeat n [true false])))
  (let indices (list0N (- (* 2! n) 1!))
    (polygon fill stroke w (map pti (zip indices lengths))))))))

(def setZones (\\(s shape) (addAttr shape ['ZONES' s])))

;; zones : String -> List Shape -> List Shape
(def zones (\\s (map (setZones s))))

;; hideZonesTail : List Shape -> List Shape
;; Remove all zones from shapes except for the first in the list
(def hideZonesTail  (\\[hd | tl] [hd | (zones 'none'  tl)]))

;; basicZonesTail : List Shape -> List Shape
;; Turn all zones to basic for a given list of shapes except for the first shape
(def basicZonesTail (\\[hd | tl] [hd | (zones 'basic' tl)]))

(def ghost
  ; consAttr (instead of addAttr) makes internal calls to
  ; Utils.maybeRemoveFirst \"HIDDEN\" slightly faster
  (\\shape (consAttr shape ['HIDDEN' ''])))

(def ghosts (map ghost))

;; hSlider_ : Bool -> Bool -> Int -> Int -> Int -> Num -> Num -> Str -> Num
;; -> [Num (List Svg)]
;; argument order - dropBall roundInt xStart xEnd y minVal maxVal caption srcVal
;; dropBall - Determines if the slider ball continues to appear past the edges of the slider
;; roundInt - Determines whether to round to Ints or not
;; xStart - left edge of slider
;; xEnd - right edge of slider
;; y - y positioning of entire slider bar
;; minVal - minimum value of slider
;; maxVal - maximum value of slider
;; caption - text to display along with the slider
;; srcVal - the current value given by the slider ball
(def hSlider_ (\\(dropBall roundInt x0 x1 y minVal maxVal caption srcVal)
  (let preVal (clamp minVal maxVal srcVal)
  (let targetVal (if roundInt (round preVal) preVal)
  (let shapes
    (let ball
      (let [xDiff valDiff] [(- x1 x0) (- maxVal minVal)]
      (let xBall (+ x0 (* xDiff (/ (- srcVal minVal) valDiff)))
      (if (= preVal srcVal) (circle 'black' xBall y 10!)
      (if dropBall          (circle 'black' 0! 0! 0!)
                            (circle 'red' xBall y 10!)))))
    [ (line 'black' 3! x0 y x1 y)
      (text (+ x1 10) (+ y 5) (+ caption (toString targetVal)))
      (circle 'black' x0 y 4!) (circle 'black' x1 y 4!) ball ])
  [targetVal (ghosts shapes)])))))
; TODO only draw zones for ball

(def vSlider_ (\\(dropBall roundInt y0 y1 x minVal maxVal caption srcVal)
  (let preVal (clamp minVal maxVal srcVal)
  (let targetVal (if roundInt (round preVal) preVal)
  (let shapes
    (let ball
      (let [yDiff valDiff] [(- y1 y0) (- maxVal minVal)]
      (let yBall (+ y0 (* yDiff (/ (- srcVal minVal) valDiff)))
      (if (= preVal srcVal) (circle 'black' x yBall 10!)
      (if dropBall          (circle 'black' 0! 0! 0!)
                            (circle 'red' x yBall 10!)))))
    [ (line 'black' 3! x y0 x y1)
      ; (text (+ x1 10) (+ y 5) (+ caption (toString targetVal)))
      (circle 'black' x y0 4!) (circle 'black' x y1 4!) ball ])
  [targetVal (ghosts shapes)])))))
; TODO only draw zones for ball

(def hSlider (hSlider_ false))
(def vSlider (vSlider_ false))

;; button_ : Bool -> Number -> Number -> String -> Number -> SVG
;; Similar to sliders, but just has boolean values
(def button_ (\\(dropBall xStart y caption xCur)
  (let [rPoint wLine rBall wSlider] [4! 3! 10! 70!]
  (let xEnd (+ xStart wSlider)
  (let xBall (+ xStart (* xCur wSlider))
  (let xBall_ (clamp xStart xEnd xBall)
  (let val (< xCur 0.5)
  (let shapes1
    [ (circle 'black' xStart y rPoint)
      (circle 'black' xEnd y rPoint)
      (line 'black' wLine xStart y xEnd y)
      (text (+ xEnd 10) (+ y 5) (+ caption (toString val))) ]
  (let shapes2
    [ (if (= xBall_ xBall) (circle (if val 'darkgreen' 'darkred') xBall y rBall)
      (if dropBall         (circle 'black' 0! 0! 0!)
                           (circle 'red' xBall y rBall))) ]
  (let shapes (append (zones 'none' shapes1) (zones 'basic' shapes2))
  [val (ghosts shapes)]))))))))))

(def button (button_ false))

(def xySlider
  (\\(xStart xEnd yStart yEnd xMin xMax yMin yMax xCaption yCaption xCur yCur)
    (let [rCorner wEdge rBall] [4! 3! 10!]
    (let [xDiff yDiff xValDiff yValDiff] [(- xEnd xStart) (- yEnd yStart) (- xMax xMin) (- yMax yMin)]
    (let xBall (+ xStart (* xDiff (/ (- xCur xMin) xValDiff)))
    (let yBall (+ yStart (* yDiff (/ (- yCur yMin) yValDiff)))
    (let cBall (if (and (between xMin xMax xCur) (between yMin yMax yCur)) 'black' 'red')
    (let xVal (ceiling (clamp xMin xMax xCur))
    (let yVal (ceiling (clamp yMin yMax yCur))
    (let myLine (\\(x1 y1 x2 y2) (line 'black' wEdge x1 y1 x2 y2))
    (let myCirc (\\(x0 y0) (circle 'black' x0 y0 rCorner))
    (let shapes
      [ (myLine xStart yStart xEnd yStart)
        (myLine xStart yStart xStart yEnd)
        (myLine xStart yEnd xEnd yEnd)
        (myLine xEnd yStart xEnd yEnd)
        (myCirc xStart yStart)
        (myCirc xStart yEnd)
        (myCirc xEnd yStart)
        (myCirc xEnd yEnd)
        (circle cBall xBall yBall rBall)
        (text (- (+ xStart (/ xDiff 2)) 40) (+ yEnd 20) (+ xCaption (toString xVal)))
        (text (+ xEnd 10) (+ yStart (/ yDiff 2)) (+ yCaption (toString yVal))) ]
    [ [ xVal yVal ] (ghosts shapes) ]
))))))))))))

(def enumSlider (\\(x0 x1 y enum caption srcVal)
  (let n (len enum)
  (let [minVal maxVal] [0! n]
  (let preVal (clamp minVal maxVal srcVal)
  (let i (floor preVal)
  (let item (nth enum i)
  (let wrap (\\circ (addAttr circ ['SELECTED' ''])) ; TODO
  (let shapes
    (let rail [ (line 'black' 3! x0 y x1 y) ]
    (let ball
      (let [xDiff valDiff] [(- x1 x0) (- maxVal minVal)]
      (let xBall (+ x0 (* xDiff (/ (- preVal minVal) valDiff)))
      (let rBall (if (= preVal srcVal) 10! 0!)
        [ (wrap (circle 'black' xBall y rBall)) ])))
    (let endpoints
      [ (wrap (circle 'black' x0 y 4!)) (wrap (circle 'black' x1 y 4!)) ]
    (let tickpoints
      (let sep (/ (- x1 x0) n)
      (map (\\j (wrap (circle 'grey' (+ x0 (mult j sep)) y 4!)))
           (range 1! (- n 1!))))
    (let label [ (text (+ x1 10!) (+ y 5!) (+ caption (toString item))) ]
    (concat [ rail endpoints tickpoints ball label ]))))))
  [item (ghosts shapes)])))))))))

(def addSelectionSliders (\\(y0 seeds shapesCaps)
  (let shapesCapsSeeds (zip shapesCaps (take seeds (len shapesCaps)))
  (let foo (\\[i [[shape cap] seed]]
    (let [k _ _] shape
    (let enum
      (if (= k 'circle') ['' 'cx' 'cy' 'r']
      (if (= k 'line')   ['' 'x1' 'y1' 'x2' 'y2']
      (if (= k 'rect')   ['' 'x' 'y' 'width' 'height']
        [(+ 'NO SELECTION ENUM FOR KIND ' k)])))
    (let [item slider] (enumSlider 20! 170! (+ y0 (mult i 30!)) enum cap seed)
    (let shape1 (addAttr shape ['SELECTED' item]) ; TODO overwrite existing
    [shape1|slider])))))
  (concat (mapi foo shapesCapsSeeds))))))


;; rotate : Shape -> Number -> Number -> Number -> Shape
;; argument order - shape, rot, x, y
;; Takes a shape rotates it rot degrees around point (x,y)
(def rotate (\\(shape n1 n2 n3)
  (addAttr shape ['transform' [['rotate' n1 n2 n3]]])))

(def rotateAround (\\(rot x y shape)
  (addAttr shape ['transform' [['rotate' rot x y]]])))

(def radToDeg (\\rad (* (/ rad (pi)) 180!)))

; Shapes via Bounding Boxes

(def box (\\(bounds fill stroke strokeWidth)
  (let [x y xw yh] bounds
  ['BOX'
    [ ['LEFT' x] ['TOP' y] ['RIGHT' xw] ['BOT' yh]
      ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth]
    ] []
  ])))

(def simpleBoundingBox (\\bounds
  (ghost (box bounds 'transparent' 'darkblue' 1))))

(def strList
  (let foo (\\(x acc) (+ (+ acc (if (= acc '') '' ' ')) (toString x)))
  (foldl foo '')))

(def fancyBoundingBox (\\bounds
  (let [left top right bot] bounds
  (let [width height] [(- right left) (- bot top)]
  (let [c1 c2 r] ['darkblue' 'skyblue' 6]
  [ (ghost (box bounds 'transparent' c1 1))
    (ghost (setZones 'none' (circle c2 left top r)))
    (ghost (setZones 'none' (circle c2 right top r)))
    (ghost (setZones 'none' (circle c2 right bot r)))
    (ghost (setZones 'none' (circle c2 left bot r)))
    (ghost (setZones 'none' (circle c2 left (+ top (/ height 2)) r)))
    (ghost (setZones 'none' (circle c2 right (+ top (/ height 2)) r)))
    (ghost (setZones 'none' (circle c2 (+ left (/ width 2)) top r)))
    (ghost (setZones 'none' (circle c2 (+ left (/ width 2)) bot r)))
  ])))))

(def group (\\(bounds shapes)
  ['g' [['BOUNDS' bounds]]
       (concat [(fancyBoundingBox bounds) shapes])]))

(def rotatedRect (\\(fill x y w h rot)
  (let [cx cy] [(+ x (/ w 2!)) (+ y (/ h 2!))]
  (let bounds [x y (+ x w) (+ y h)]
  (let shape (rotateAround rot cx cy (rect fill x y w h))
  (group bounds [shape])
)))))

(def rectangle (\\(fill stroke strokeWidth rot bounds)
  (let [left top right bot] bounds
  (let [cx cy] [(+ left (/ (- right left) 2!)) (+ top (/ (- bot top) 2!))]
  (let shape (rotateAround rot cx cy (box bounds fill stroke strokeWidth))
  (group bounds [shape])
)))))

(def rotatedEllipse (\\(fill cx cy rx ry rot)
  (let bounds [(- cx rx) (- cy ry) (+ cx rx) (+ cy ry)]
  (let shape (rotateAround rot cx cy (ellipse fill cx cy rx ry))
  (group bounds [shape])
))))

; TODO take rot
(def oval (\\(fill stroke strokeWidth bounds)
  (let [left top right bot] bounds
  (let [rx ry] [(/ (- right left) 2!) (/ (- bot top) 2!)]
  (let [cx cy] [(+ left rx) (+ top ry)]
  (let shape ; TODO change def ellipse to take stroke/strokeWidth
    ['ellipse'
       [ ['cx' cx] ['cy' cy] ['rx' rx] ['ry' ry]
         ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth] ]
       []]
  (group bounds [shape])
))))))

(def rawPolygon (\\(fill stroke w pts)
  ['g' [] [(polygon fill stroke w pts)]]))

(def scaleBetween (\\(a b pct)
  (case pct
    (0 a)
    (1 b)
    (_ (+ a (* pct (- b a)))))))

(def stretchyPolygon (\\(bounds fill stroke strokeWidth percentages)
  (let [left top right bot] bounds
  (let [xScale yScale] [(scaleBetween left right) (scaleBetween top bot)]
  (let pts (map (\\[xPct yPct] [ (xScale xPct) (yScale yPct) ]) percentages)
  (group bounds [(polygon fill stroke strokeWidth pts)])
)))))

(def pointyPath (\\(fill stroke w d)
  (let dot (\\(x y) (ghost (circle 'orange' x y 5)))
  (letrec pointsOf (\\cmds
    (case cmds
      ([]                     [])
      (['Z']                  [])
      (['M' x y | rest]       (append [(dot x y)] (pointsOf rest)))
      (['L' x y | rest]       (append [(dot x y)] (pointsOf rest)))
      (['Q' x1 y1 x y | rest] (append [(dot x1 y1) (dot x y)] (pointsOf rest)))
      (['C' x1 y1 x2 y2 x y | rest] (append [(dot x1 y1) (dot x2 y2) (dot x y)] (pointsOf rest)))
      (_                      'ERROR')))
  ['g' []
    (cons
      (path fill stroke w d)
      (pointsOf d)) ]
))))

; can refactor to make one pass
; can also change representation/template code to pair points
(def stretchyPath (\\(bounds fill stroke w d)
  (let [left top right bot] bounds
  (let [xScale yScale] [(scaleBetween left right) (scaleBetween top bot)]
  (let dot (\\(x y) (ghost (circle 'orange' x y 5)))
  (letrec toPath (\\cmds
    (case cmds
      ([]    [])
      (['Z'] ['Z'])
      (['M' x y | rest] (append ['M' (xScale x) (yScale y)] (toPath rest)))
      (['L' x y | rest] (append ['L' (xScale x) (yScale y)] (toPath rest)))
      (['Q' x1 y1 x y | rest]
        (append ['Q' (xScale x1) (yScale y1) (xScale x) (yScale y)]
                (toPath rest)))
      (['C' x1 y1 x2 y2 x y | rest]
        (append ['C' (xScale x1) (yScale y1) (xScale x2) (yScale y2) (xScale x) (yScale y)]
                (toPath rest)))
      (_ 'ERROR')))
  (letrec pointsOf (\\cmds
    (case cmds
      ([]    [])
      (['Z'] [])
      (['M' x y | rest] (append [(dot (xScale x) (yScale y))] (pointsOf rest)))
      (['L' x y | rest] (append [(dot (xScale x) (yScale y))] (pointsOf rest)))
      (['Q' x1 y1 x y | rest]
        (append [(dot (xScale x1) (yScale y1)) (dot (xScale x) (yScale y))]
                (pointsOf rest)))
      (['C' x1 y1 x2 y2 x y | rest]
        (append [(dot (xScale x1) (yScale y1))
                 (dot (xScale x2) (yScale y2))
                 (dot (xScale x)  (yScale y))]
                (pointsOf rest)))
      (_ 'ERROR')))
  (group bounds
    (cons
      (path fill stroke w (toPath d))
      (pointsOf d)))
)))))))

(def evalOffset (\\[base off]
  (case off
    (0 base)
    (_ (+ base off)))))

(def stickyPolygon (\\(bounds fill stroke strokeWidth offsets)
  (let pts (map (\\[xOff yOff] [ (evalOffset xOff) (evalOffset yOff) ]) offsets)
  (group bounds [(polygon fill stroke strokeWidth pts)])
)))

; expects (f bounds) to be multiple SVGs
(def with (\\(bounds f) [(group bounds (f bounds))]))

(def star (\\bounds
  (let [left top right bot] bounds
  (let [width height] [(- right left) (- bot top)]
  (let [cx cy] [(+ left (/ width 2)) (+ top (/ height 2))]
  [(nStar 'lightblue' 'black' 0 6 (min (/ width 2) (/ height 2)) 10 0 cx cy)]
)))))

(def blobs (\\blobs
  (let modifyBlob (\\[i blob]
    (case blob
      ([shape] [(consAttr shape ['BLOB' (toString (+ i 1))])])
      (_       blob)))
  (svg (concat (mapi modifyBlob blobs)))
)))

; 0
['svg' [] []]

"


src = prelude

