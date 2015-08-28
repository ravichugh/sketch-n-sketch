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

;; compose : (a -> b) -> (b -> c) -> (a -> c)
;; Composes two functions
(def compose (\\(f g) (\\x (f (g x)))))

;; fst : List a -> a
;; Returns the first element of a given list
(def fst (\\[x|_] x))

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
;; Takes a function, an accumulator, and a list as input and reduces from the left with the given function
(defrec foldl (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (foldl f (f x acc) xs1)))))

;; foldl : (a -> b -> b) -> b -> List a -> b
;; Takes a function, an accumulator, and a list as input and reduces from the left with the given function
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
;; Given two bools, returns a bool regarding 'if the first argument is true, then the second argument is as well'
(def implies (\\(p q) (if p q true)))

;; clamp : Number -> Number -> Number -> Number
;; Given an upper bound, lower bound, and a number, restricts that number between those bounds (inclusive)
;; Ex. clamp 1 5 4 = 4
;; Ex. clamp 1 5 6 = 5
(def clamp (\\(i j n) (if (< n i) i (if (< j n) j n))))

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
; HTML Manipulating Functions
;

(def html (\\objects ['html' [] objects]))

(def head (\\(attrs children) ['head' attrs children]))

(def body (\\(attrs children) ['body' attrs children]))

(def p (\\(attrs children) ['p' attrs children]))

(def div (\\(attrs children) ['div' attrs children]))

;; a similar function to http://package.elm-lang.org/packages/evancz/elm-html/4.0.1/Html-Attributes#style
;; to help easily add and remove stylings
(def style (\\(attrs)
  (let boundKVs
    (map (\\s (joinStrings ': ' s)) attrs )      
  ['style'
    (joinStrings '; ' (snoc '' boundKVs))]
  )))


;; addAttr : Node-> Attribute -> Node
;; argument order - node, new attribute
;; Add a new attribute to a given node
(def addAttr (\\([nodeKind oldAttrs children] newAttr)
  [nodeKind (snoc newAttr oldAttrs) children]))

;; text : Number -> Number -> String -> Shape
;; argument order - x, y, string
;; place a text string with top left corner at (x,y) - with default color & font
(def text (\\(x y s)
   ['text' [['x' x] ['y' y] ['style' 'fill:black']
            ['font-family' 'Tahoma, sans-serif']]
           [['TEXT' s]]]))

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

;; zones : String -> Shape -> Shape
;; Add a string-specified type of zones to a given shape
(def zones (\\s (map (\\shape (addAttr shape ['zones' s])))))

;; hideZonesTail : List Shape -> List Shape
;; Remove all zones from shapes except for the first in the list
(def hideZonesTail  (\\[hd | tl] [hd | (zones 'none'  tl)]))

;; basicZonesTail : List Shape -> List Shape
;; Turn all zones to basic for a given list of shapes except for the first shape
(def basicZonesTail (\\[hd | tl] [hd | (zones 'basic' tl)]))

;; hSlider_ : Bool -> Int -> Int -> Int -> Num -> Num -> Str -> Num
;; -> [Num (List Svg)]
;; argument order - dropBall roundInt xStart xEnd y minVal maxVal caption curVal
;; dropBall - Determines if the slider ball continues to appear past the edges of the slider
;; roundInt - Determines whether to round to Ints or not
;; xStart - left edge of slider
;; xEnd - right edge of slider
;; y - y positioning of entire slider bar
;; minVal - minimum value of slider
;; maxVal - maximum value of slider
;; caption - text to display along with the slider
(def hSlider_ (\\(dropBall roundInt xStart xEnd y minVal maxVal caption curVal)
  (let [rPoint wLine rBall] [4! 3! 10!]
  (let [xDiff valDiff] [(- xEnd xStart) (- maxVal minVal)]
  (let xBall (+ xStart (* xDiff (/ (- curVal minVal) valDiff)))
  (let xBall_ (clamp xStart xEnd xBall)
  (let rBall_ (if dropBall (if (= xBall_ xBall) rBall 0) rBall)
  (let val
    (let val_ (clamp minVal maxVal curVal)
    (if roundInt (round val_) val_))
  (let shapes1
    [ (line 'black' wLine xStart y xEnd y)
      (circle 'black' xStart y rPoint)
      (circle 'black' xEnd y rPoint)
      (text (+ xEnd 10) (+ y 5) (+ caption (toString val))) ]
  (let shapes2
    [ (circle 'black' xBall y rBall_) ]
  [val (append (zones 'none' shapes1)
               (zones 'basic' shapes2))]))))))))))

;; As hSlider_, but dropBall disappears when it passes then end of a slider
(def hSlider (hSlider_ true))

;; button_ : Bool -> Number -> Number -> String -> Number -> SVG
;; Similar to sliders, but just has boolean values
(def button_ (\\(dropBall xStart y caption xCur)
  (let [rPoint wLine rBall wSlider] [4! 3! 10! 70!]
  (let xEnd (+ xStart wSlider)
  (let xBall (+ xStart (* xCur wSlider))
  (let xBall_ (clamp xStart xEnd xBall)
  (let rBall_ (if dropBall (if (= xBall_ xBall) rBall 0) rBall)
  (let val (< xCur 0.5)
  (let shapes1
    [ (circle 'black' xStart y rPoint)
      (circle 'black' xEnd y rPoint)
      (line 'black' wLine xStart y xEnd y)
      (text (+ xEnd 10) (+ y 5) (+ caption (toString val))) ]
  (let shapes2
    [ (circle (if val 'darkgreen' 'darkred') xBall y rBall_) ]
  [val (append (zones 'none' shapes1)
               (zones 'basic' shapes2))]))))))))))

;; As button, but set initially to true
(def button (button_ true))

;; rotate : Shape -> Number -> Number -> Number -> Shape
;; argument order - shape, rot, x, y
;; Takes a shape rotates it rot degrees around point (x,y)
(def rotate (\\(shape n1 n2 n3)
  (addAttr shape ['transform' [['rotate' n1 n2 n3]]])))

; 0
['svg' [] []]

"


src = prelude

