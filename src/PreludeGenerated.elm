module PreludeGenerated exposing (src, preludeLeo)

prelude =
 """
; prelude.little
;
; This little library is accessible by every program.
; This is not an example that generates an SVG canvas,
; but we include it here for reference.

;; The identity function - given a value, returns exactly that value
(typ id (forall a (-> a a)))
(def id (\\x x))

;; A function that always returns the same value a, regardless of b
(typ always (forall (a b) (-> a b a)))
(def always (\\(x _) x))

;; Composes two functions together
(typ compose (forall (a b c) (-> (-> b c) (-> a b) (-> a c))))
(def compose (\\(f g) (\\x (f (g x)))))

(typ flip (forall (a b c) (-> (-> a b c) (-> b a c))))
(def flip (\\f (\\(x y) (f y x))))
  ; TODO other version:
  ; (def flip (\\(f x y) (f y x)))

(typ fst (forall (a b) (-> [a b] a)))
(typ snd (forall (a b) (-> [a b] b)))

(def fst (\\[a _] a))
(def snd (\\[_ b] b))

;; Given a bool, returns the opposite boolean value
(typ not (-> Bool Bool))
(def not (\\b (if b false true)))

;; Given two bools, returns a bool regarding if the first argument is true, then the second argument is as well
(typ implies (-> Bool Bool Bool))
(def implies (\\(p q) (if p q true)))

(typ or  (-> Bool Bool Bool))
(typ and (-> Bool Bool Bool))

(def or  (\\(p q) (if p true q)))
(def and (\\(p q) (if p q false)))

(typ lt (-> Num Num Bool))
(typ eq (-> Num Num Bool))
(typ le (-> Num Num Bool))
(typ gt (-> Num Num Bool))
(typ ge (-> Num Num Bool))

(def lt (\\(x y) (< x y)))
(def eq (\\(x y) (= x y)))
(def le (\\(x y) (or (lt x y) (eq x y))))
(def gt (flip lt))
(def ge (\\(x y) (or (gt x y) (eq x y))))

;; Returns the length of a given list
(typ len (forall a (-> (List a) Num)))
(defrec len (\\xs (case xs ([] 0) ([_ | xs1] (+ 1 (len xs1))))))

;; Maps a function, f, over a list of values and returns the resulting list
(typ map (forall (a b) (-> (-> a b) (List a) (List b))))
(defrec map (\\(f xs)
  (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)]))))

;; Combines two lists with a given function, extra elements are dropped
(typ map2 (forall (a b c) (-> (-> a b c) (List a) (List b) (List c))))
(defrec map2 (\\(f xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] [ (f x y) | (map2 f xs1 ys1) ])
    (_                 []))))

;; Combines three lists with a given function, extra elements are dropped
(typ map3 (forall (a b c d) (-> (-> a b c d) (List a) (List b) (List c) (List d))))
(defrec map3 (\\(f xs ys zs)
  (case [xs ys zs]
    ([[x|xs1] [y|ys1] [z|zs1]] [ (f x y z) | (map3 f xs1 ys1 zs1) ])
    (_                         []))))

;; Combines four lists with a given function, extra elements are dropped
(typ map4 (forall (a b c d e) (-> (-> a b c d e) (List a) (List b) (List c) (List d) (List e))))
(defrec map4 (\\(f ws xs ys zs)
  (case [ws xs ys zs]
    ([[w|ws1] [x|xs1] [y|ys1] [z|zs1]] [ (f w x y z) | (map4 f ws1 xs1 ys1 zs1) ])
    (_                                 []))))

;; Takes a function, an accumulator, and a list as input and reduces using the function from the left
(typ foldl (forall (a b) (-> (-> a b b) b (List a) b)))
(defrec foldl (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (foldl f (f x acc) xs1)))))

;; Takes a function, an accumulator, and a list as input and reduces using the function from the right
(typ foldr (forall (a b) (-> (-> a b b) b (List a) b)))
(defrec foldr (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (f x (foldr f acc xs1))))))

;; Given two lists, append the second list to the end of the first
(typ append (forall a (-> (List a) (List a) (List a))))
(defrec append (\\(xs ys)
  (case xs ([] ys) ([x|xs1] [ x | (append xs1 ys)]))))

;; concatenate a list of lists into a single list
(typ concat (forall a (-> (List (List a)) (List a))))
(def concat (\\xss (foldr append [] xss)))
  ; TODO eta-reduced version:
  ; (def concat (foldr append []))

;; Map a given function over a list and concatenate the resulting list of lists
(typ concatMap (forall (a b) (-> (-> a (List b)) (List a) (List b))))
(def concatMap (\\(f xs) (concat (map f xs))))

;; Takes two lists and returns a list that is their cartesian product
(typ cartProd (forall (a b) (-> (List a) (List b) (List [a b]))))
(def cartProd (\\(xs ys)
  (concatMap (\\x (map (\\y [x y]) ys)) xs)))

;; Takes elements at the same position from two input lists and returns a list of pairs of these elements
(typ zip (forall (a b) (-> (List a) (List b) (List [a b]))))
(def zip (\\(xs ys) (map2 (\\(x y) [x y]) xs ys)))
  ; TODO eta-reduced version:
  ; (def zip (map2 (\\(x y) [x y])))

;; The empty list
;; (typ nil (forall a (List a)))
(typ nil [])
(def nil [])

;; attaches an element to the front of a list
(typ cons (forall a (-> a (List a) (List a))))
(def cons (\\(x xs) [x | xs]))

;; attaches an element to the end of a list
(typ snoc (forall a (-> a (List a) (List a))))
(def snoc (\\(x ys) (append ys [x])))

;; Returns the first element of a given list
(typ hd (forall a (-> (List a) a)))
(def hd (\\[x|xs] x))

(typ tl (forall a (-> (List a) (List a))))
(def tl (\\[x|xs] xs))

;; Returns the last element of a given list
(typ last (forall a (-> (List a) a)))
(defrec last (\\xs
  (case xs
    ([x]    x)
    ([_|xs] (last xs)))))

;; Given a list, reverse its order
(typ reverse (forall a (-> (List a) (List a))))
(def reverse (\\xs (foldl cons nil xs)))
  ; TODO eta-reduced version:
  ; (def reverse (foldl cons nil))

(def adjacentPairs (\\xs (zip xs (tl xs))))

;; Given two numbers, creates the list between them (inclusive)
(typ range (-> Num Num (List Num)))
(defrec range (\\(i j)
  (if (< i (+ j 1))
      (cons i (range (+ i 1) j))
      nil)))

;; Given a number, create the list of 0 to that number inclusive (number must be > 0)
(typ list0N (-> Num (List Num)))
(def list0N (\\n (range 0 n)))

;; Given a number, create the list of 1 to that number inclusive
(typ list1N (-> Num (List Num)))
(def list1N (\\n (range 1 n)))

(typ zeroTo (-> Num (List Num)))
(def zeroTo (\\n (range 0 (- n 1))))

;; Given a number n and some value x, return a list with x repeated n times
(typ repeat (forall a (-> Num a (List a))))
(def repeat (\\(n x) (map (always x) (range 1 n))))

;; Given two lists, return a single list that alternates between their values (first element is from first list)
(typ intermingle (forall a (-> (List a) (List a) (List a))))
(defrec intermingle (\\(xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] (cons x (cons y (intermingle xs1 ys1))))
    ([[]      []]      nil)
    (_                 (append xs ys)))))

(def intersperse (\\(sep xs)
  (case xs
    ([]     xs)
    ([x|xs] (reverse (foldl (\\(y acc) [ y sep | acc ]) [x] xs))))))

(typ mapi (forall (a b) (-> (-> [Num a] b) (List a) (List b))))
(def mapi (\\(f xs) (map f (zip (range 0 (- (len xs) 1)) xs))))

(def indexedMap (\\(f xs) (mapi (\\[i x] (f i x)) xs)))


(typ nth (forall a (-> (List a) Num (union Null a))))
(defrec nth (\\(xs n)
  (if (< n 0)       null
    (case [n xs]
      ([_ []]       null)
      ([0 [x|xs1]]  x)
      ([_ [x|xs1]]  (nth xs1 (- n 1)))))))

; (defrec nth (\\(xs n)
;   (if (< n 0)   'ERROR: nth'
;     (case xs
;       ([]       'ERROR: nth')
;       ([x|xs1]  (if (= n 0) x (nth xs1 (- n 1))))))))

; TODO change typ/def
; (typ take (forall a (-> (List a) Num (union Null (List a)))))

(typ take (forall a (-> (List a) Num (List (union Null a)))))
(defrec take (\\(xs n)
  (if (= n 0) []
    (case xs
      ([]      [null])
      ([x|xs1] [x | (take xs1 (- n 1))])))))

; (def take
;   (letrec take_ (\\(n xs)
;     (case [n xs]
;       ([0 _]       [])
;       ([_ []]      [])
;       ([_ [x|xs1]] [x | (take_ (- n 1) xs1)])))
;   (compose take_ (max 0))))

(typ drop (forall a (-> (List a) Num (union Null (List a)))))
(defrec drop (\\(xs n)
  (if (le n 0)
    xs
    (case xs
      ([]      null)
      ([x|xs1] (drop xs1 (- n 1)))))))

;; Drop n elements from the end of a list
(typ dropEnd (forall a (-> (List a) Num (union Null (List a)))))
(def dropEnd (\\(xs n)
  (let tryDrop (drop (reverse xs) n)
  (typecase tryDrop
    (Null null)
    (_    (reverse tryDrop))))))

(typ elem (forall a (-> a (List a) Bool)))
(defrec elem (\\(x ys)
  (case ys
    ([]      false)
    ([y|ys1] (or (= x y) (elem x ys1))))))

(def sortBy (\\(f xs)
  (letrec ins (\\(x ys)   ; insert is a keyword...
    (case ys
      ([]     [x])
      ([y|ys] (if (f x y) [x y | ys] [y | (ins x ys)]))))
  (foldl ins [] xs))))

(def sortAscending (sortBy lt))
(def sortDescending (sortBy gt))


;; multiply two numbers and return the result
(typ mult (-> Num Num Num))
(defrec mult (\\(m n)
  (if (< m 1) 0 (+ n (mult (+ m -1) n)))))

;; Given two numbers, subtract the second from the first
(typ minus (-> Num Num Num))
(def minus (\\(x y) (+ x (mult y -1))))

;; Given two numbers, divide the first by the second
(typ div (-> Num Num Num))
(defrec div (\\(m n)
  (if (< m n) 0
  (if (< n 2) m
    (+ 1 (div (minus m n) n))))))

;; Given a number, returns the negative of that number
(typ neg (-> Num Num))
(def neg (\\x (- 0 x)))

;; Absolute value
(typ abs (-> Num Num))
(def abs (\\x (if (< x 0) (neg x) x)))

;; Sign function; -1, 0, or 1 based on sign of given number
(typ sgn (-> Num Num))
(def sgn (\\x (if (= 0 x) 0 (/ x (abs x)))))

(typ some (forall a (-> (-> a Bool) (List a) Bool)))
(defrec some (\\(p xs)
  (case xs
    ([]      false)
    ([x|xs1] (or (p x) (some p xs1))))))

(typ all (forall a (-> (-> a Bool) (List a) Bool)))
(defrec all (\\(p xs)
  (case xs
    ([]      true)
    ([x|xs1] (and (p x) (all p xs1))))))

;; Given an upper bound, lower bound, and a number, restricts that number between those bounds (inclusive)
;; Ex. clamp 1 5 4 = 4
;; Ex. clamp 1 5 6 = 5
(typ clamp (-> Num Num Num Num))
(def clamp (\\(i j n) (if (< n i) i (if (< j n) j n))))

(typ between (-> Num Num Num Bool))
(def between (\\(i j n) (= n (clamp i j n))))

(typ plus (-> Num Num Num))
(def plus (\\(x y) (+ x y)))

(typ min (-> Num Num Num))
(def min (\\(i j) (if (lt i j) i j)))

(typ max (-> Num Num Num))
(def max (\\(i j) (if (gt i j) i j)))

(typ minimum (-> (List Num) Num))
(def minimum (\\[hd|tl] (foldl min hd tl)))

(typ maximum (-> (List Num) Num))
(def maximum (\\[hd|tl] (foldl max hd tl)))

(typ average (-> (List Num) Num))
(def average (\\nums
  (let sum (foldl plus 0 nums)
  (let n   (len nums)
    (/ sum n)))))

;; Combine a list of strings with a given separator
;; Ex. joinStrings ', ' ['hello' 'world'] = 'hello, world'
(typ joinStrings (-> String (List String) String))
(def joinStrings (\\(sep ss)
  (foldr (\\(str acc) (if (= acc '') str (+ str (+ sep acc)))) '' ss)))

;; Concatenate a list of strings and return the resulting string
(typ concatStrings (-> (List String) String))
(def concatStrings (joinStrings ''))

;; Concatenates a list of strings, interspersing a single space in between each string
(typ spaces (-> (List String) String))
(def spaces (joinStrings ' '))

;; First two arguments are appended at the front and then end of the third argument correspondingly
;; Ex. delimit '+' '+' 'plus' = '+plus+'
(typ delimit (-> String String String String))
(def delimit (\\(a b s) (concatStrings [a s b])))

;; delimit a string with parentheses
(typ parens (-> String String))
(def parens (delimit '(' ')'))

;
; SVG Manipulating Functions
;

; === SVG Types ===

(def Point [Num Num])
(def RGBA [Num Num Num Num])
(def Color (union String Num RGBA))
(def PathCmds (List (union String Num)))
(def Points (List Point))
(def RotationCmd [[String Num Num Num]])
(def AttrVal (union String Num Bool Color PathCmds Points RotationCmd))
(def AttrName String)
(def AttrPair [AttrName AttrVal])
(def Attrs (List AttrPair))
(def NodeKind String)
; TODO add recursive types properly
(def SVG [NodeKind Attrs (List SVG_or_Text)])
(def SVG_or_Text (union SVG [String String]))
(def Blob (List SVG))

; === Attribute Lookup ===

(typ lookupWithDefault (forall (k v) (-> v k (List [k v]) v)))
(defrec lookupWithDefault (\\(default k dict)
  (let foo (lookupWithDefault default k)
  (case dict
    ([]            default)
    ([[k1 v]|rest] (if (= k k1) v (foo rest)))))))

(typ lookup (forall (k v) (-> k (List [k v]) (union v Null))))
(defrec lookup (\\(k dict)
  (let foo (lookup k)
  (case dict
    ([]            null)
    ([[k1 v]|rest] (if (= k k1) v (foo rest)))))))

(typ addExtras (-> Num (List [String (List [Num AttrVal])]) SVG SVG))
(defrec addExtras (\\(i extras shape)
  (case extras
    ([] shape)
    ([[k table] | rest]
      (let v (lookup i table)
      (typecase v
        (Null    (addExtras i rest shape))
        (AttrVal (addExtras i rest (addAttr shape [k v])))))))))

(typ lookupAttr (-> SVG AttrName (union AttrVal Null)))
(def lookupAttr (\\([_ attrs _] k) (lookup k attrs)))

(typ lookupAttrWithDefault (-> AttrVal SVG AttrName AttrVal))
(def lookupAttrWithDefault (\\(default [_ attrs _] k) (lookupWithDefault default k attrs)))

; Pairs of Type-Specific Lookup Functions

(typ lookupNumAttr (-> SVG AttrName (union Num Null)))
(def lookupNumAttr (\\([_ attrs _] k)
  (let val (lookup k attrs)
  (typecase val (Num val) (_ null)))))

(typ lookupNumAttrWithDefault (-> Num SVG AttrName Num))
(def lookupNumAttrWithDefault (\\(default shape k)
  (let val (lookupNumAttr shape k)
  (typecase val (Num val) (Null default)))))

(typ lookupPointsAttr (-> SVG AttrName (union Points Null)))
(def lookupPointsAttr (\\([_ attrs _] k)
  (let val (lookup k attrs)
  (typecase val ((List [Num Num]) val) (_ null)))))

(typ lookupPointsAttrWithDefault (-> Points SVG AttrName Points))
(def lookupPointsAttrWithDefault (\\(default shape k)
  (let val (lookupPointsAttr shape k)
  (typecase val ((List [Num Num]) val) (Null default)))))

(typ lookupStringAttr (-> SVG AttrName (union String Null)))
(def lookupStringAttr (\\([_ attrs _] k)
  (let val (lookup k attrs)
  (typecase val (String val) (_ null)))))

(typ lookupStringAttrWithDefault (-> String SVG AttrName String))
(def lookupStringAttrWithDefault (\\(default shape k)
  (let val (lookupStringAttr shape k)
  (typecase val (String val) (Null default)))))

; === Points ===

(def Vec2D [Num Num])

(typ vec2DPlus (-> Point Vec2D Point))
(def vec2DPlus (\\(pt vec)
  [
    (+ (fst pt) (fst vec))
    (+ (snd pt) (snd vec))
  ]
))

(typ vec2DMinus (-> Point Point Vec2D))
(def vec2DMinus (\\(pt vec)
  [
    (- (fst pt) (fst vec))
    (- (snd pt) (snd vec))
  ]
))

(typ vec2DScalarMult (-> Num Vec2D Point))
(def vec2DScalarMult (\\(num vec)
  [
    (* (fst vec) num)
    (* (snd vec) num)
  ]
))

(typ vec2DScalarDiv (-> Num Vec2D Point))
(def vec2DScalarDiv (\\(num vec)
  [
    (/ (fst vec) num)
    (/ (snd vec) num)
  ]
))

(typ vec2DLength (-> Point Point Num))
(def vec2DLength (\\([x1 y1] [x2 y2])
  (let [dx dy] [(- x2 x1) (- y2 y1)]
  (sqrt (+ (* dx dx) (* dy dy))))))


; === Circles ===

(def Circle SVG)

;; argument order - color, x, y, radius
;; creates a circle, center at (x,y) with given radius and color
(typ circle (-> Color Num Num Num Circle))
(def circle (\\(fill cx cy r)
  ['circle'
     [['cx' cx] ['cy' cy] ['r' r] ['fill' fill]]
     []]))

(typ circleCenter (-> Ellipse Point))
(def circleCenter (\\circle
  [
    (lookupNumAttrWithDefault 0 circle 'cx')
    (lookupNumAttrWithDefault 0 circle 'cy')
  ]
))

(typ circleRadius (-> Circle Num))
(def circleRadius (\\circle
  (lookupNumAttrWithDefault 0 circle 'r')
))

(typ circleDiameter (-> Circle Num))
(def circleDiameter (\\circle
  (* 2 (circleRadius circle))
))

(typ circleNorth (-> Circle Point))
(def circleNorth (\\circle
  (let [cx cy] (circleCenter circle)
    [cx (- cy (circleRadius circle))]
  )
))

(typ circleEast (-> Circle Point))
(def circleEast (\\circle
  (let [cx cy] (circleCenter circle)
    [(+ cx (circleRadius circle)) cy]
  )
))

(typ circleSouth (-> Circle Point))
(def circleSouth (\\circle
  (let [cx cy] (circleCenter circle)
    [cx (+ cy (circleRadius circle))]
  )
))

(typ circleWest (-> Circle Point))
(def circleWest (\\circle
  (let [cx cy] (circleCenter circle)
    [(- cx (circleRadius circle)) cy]
  )
))


;; argument order - color, width, x, y, radius
;; Just as circle, except new width parameter determines thickness of ring
(typ ring (-> Color Num Num Num Num SVG))
(def ring (\\(c w x y r)
  ['circle'
     [ ['cx' x] ['cy' y] ['r' r] ['fill' 'none'] ['stroke' c] ['stroke-width' w] ]
     []]))


; === Ellipses ===

(def Ellipse SVG)

;; argument order - color, x, y, x-radius, y-radius
;; Just as circle, except radius is separated into x and y parameters
(typ ellipse (-> Color Num Num Num Num Ellipse))
(def ellipse (\\(fill x y rx ry)
  ['ellipse'
     [ ['cx' x] ['cy' y] ['rx' rx] ['ry' ry] ['fill' fill] ]
     []]))

(typ ellipseCenter (-> Ellipse Point))
(def ellipseCenter (\\ellipse
  [
    (lookupNumAttrWithDefault 0 ellipse 'cx')
    (lookupNumAttrWithDefault 0 ellipse 'cy')
  ]
))

(typ ellipseRadiusX (-> Ellipse Num))
(def ellipseRadiusX (\\ellipse
  (lookupNumAttrWithDefault 0 ellipse 'rx')
))

(typ ellipseRadiusY (-> Ellipse Num))
(def ellipseRadiusY (\\ellipse
  (lookupNumAttrWithDefault 0 ellipse 'ry')
))

(typ ellipseDiameterX (-> Ellipse Num))
(def ellipseDiameterX (\\ellipse
  (* 2 (ellipseRadiusX ellipse))
))

(typ ellipseDiameterY (-> Ellipse Num))
(def ellipseDiameterY (\\ellipse
  (* 2 (ellipseRadiusY ellipse))
))

(typ ellipseNorth (-> Ellipse Point))
(def ellipseNorth (\\ellipse
  (let [cx cy] (ellipseCenter ellipse)
    [cx (- cy (ellipseRadiusY ellipse))]
  )
))

(typ ellipseEast (-> Ellipse Point))
(def ellipseEast (\\ellipse
  (let [cx cy] (ellipseCenter ellipse)
    [(+ cx (ellipseRadiusX ellipse)) cy]
  )
))

(typ ellipseSouth (-> Ellipse Point))
(def ellipseSouth (\\ellipse
  (let [cx cy] (ellipseCenter ellipse)
    [cx (+ cy (ellipseRadiusY ellipse))]
  )
))

(typ ellipseWest (-> Ellipse Point))
(def ellipseWest (\\ellipse
  (let [cx cy] (ellipseCenter ellipse)
    [(- cx (ellipseRadiusX ellipse)) cy]
  )
))


; === Bounds-based shapes (Oval and Box) ===

(def BoundedShape SVG)
(def Bounds [Num Num Num Num])

(typ boundedShapeLeft (-> BoundedShape Num))
(def boundedShapeLeft (\\shape
  (lookupNumAttrWithDefault 0 shape 'LEFT')
))

(typ boundedShapeTop (-> BoundedShape Num))
(def boundedShapeTop (\\shape
  (lookupNumAttrWithDefault 0 shape 'TOP')
))

(typ boundedShapeRight (-> BoundedShape Num))
(def boundedShapeRight (\\shape
  (lookupNumAttrWithDefault 0 shape 'RIGHT')
))

(typ boundedShapeBot (-> BoundedShape Num))
(def boundedShapeBot (\\shape
  (lookupNumAttrWithDefault 0 shape 'BOT')
))

(typ boundedShapeWidth (-> BoundedShape Num))
(def boundedShapeWidth (\\shape
  (- (boundedShapeRight shape) (boundedShapeLeft shape))
))

(typ boundedShapeHeight (-> BoundedShape Num))
(def boundedShapeHeight (\\shape
  (- (boundedShapeBot shape) (boundedShapeTop shape))
))

(typ boundedShapeLeftTop (-> BoundedShape Point))
(def boundedShapeLeftTop (\\shape
  [
    (boundedShapeLeft shape)
    (boundedShapeTop shape)
  ]
))

(typ boundedShapeCenterTop (-> BoundedShape Point))
(def boundedShapeCenterTop (\\shape
  [
    (/ (+ (boundedShapeLeft shape) (boundedShapeRight shape)) 2)
    (boundedShapeTop shape)
  ]
))

(typ boundedShapeRightTop (-> BoundedShape Point))
(def boundedShapeRightTop (\\shape
  [
    (boundedShapeRight shape)
    (boundedShapeTop shape)
  ]
))

(typ boundedShapeRightCenter (-> BoundedShape Point))
(def boundedShapeRightCenter (\\shape
  [
    (boundedShapeRight shape)
    (/ (+ (boundedShapeTop shape) (boundedShapeBot shape)) 2)
  ]
))

(typ boundedShapeRightBot (-> BoundedShape Point))
(def boundedShapeRightBot (\\shape
  [
    (boundedShapeRight shape)
    (boundedShapeBot shape)
  ]
))

(typ boundedShapeCenterBot (-> BoundedShape Point))
(def boundedShapeCenterBot (\\shape
  [
    (/ (+ (boundedShapeLeft shape) (boundedShapeRight shape)) 2)
    (boundedShapeBot shape)
  ]
))

(typ boundedShapeLeftBot (-> BoundedShape Point))
(def boundedShapeLeftBot (\\shape
  [
    (boundedShapeLeft shape)
    (boundedShapeBot shape)
  ]
))

(typ boundedShapeLeftCenter (-> BoundedShape Point))
(def boundedShapeLeftCenter (\\shape
  [
    (boundedShapeLeft shape)
    (/ (+ (boundedShapeTop shape) (boundedShapeBot shape)) 2)
  ]
))

(typ boundedShapeCenter (-> BoundedShape Point))
(def boundedShapeCenter (\\shape
  [
    (/ (+ (boundedShapeLeft shape) (boundedShapeRight shape)) 2)
    (/ (+ (boundedShapeTop shape) (boundedShapeBot shape)) 2)
  ]
))


; === Rectangles ===

(def Rect SVG)

;; argument order - color, x, y, width, height
;; creates a rectangle of given width and height with (x,y) as the top left corner coordinate
(typ rect (-> Color Num Num Num Num Rect))
(def rect (\\(fill x y w h)
  ['rect'
     [ ['x' x] ['y' y] ['width' w] ['height' h] ['fill' fill] ]
     []]))

(typ square (-> Color Num Num Num Rect))
(def square (\\(fill x y side) (rect fill x y side side)))

(typ rectWidth (-> Rect Num))
(def rectWidth (\\rect
  (lookupNumAttrWithDefault 0 rect 'width')
))

(typ rectHeight (-> Rect Num))
(def rectHeight (\\rect
  (lookupNumAttrWithDefault 0 rect 'height')
))

(typ rectLeftTop (-> Rect Point))
(def rectLeftTop (\\rect
  [
    (lookupNumAttrWithDefault 0 rect 'x')
    (lookupNumAttrWithDefault 0 rect 'y')
  ]
))

(typ rectCenterTop (-> Rect Point))
(def rectCenterTop (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [ (/ (rectWidth rect) 2) 0 ]
  )
))

(typ rectRightTop (-> Rect Point))
(def rectRightTop (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [ (rectWidth rect) 0 ]
  )
))

(typ rectRightCenter (-> Rect Point))
(def rectRightCenter (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [ (rectWidth rect) (/ (rectHeight rect) 2) ]
  )
))

(typ rectRightBot (-> Rect Point))
(def rectRightBot (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [ (rectWidth rect) (rectHeight rect) ]
  )
))

(typ rectCenterBot (-> Rect Point))
(def rectCenterBot (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [ (/ (rectWidth rect) 2) (rectHeight rect) ]
  )
))

(typ rectLeftBot (-> Rect Point))
(def rectLeftBot (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [0 (rectHeight rect) ]
  )
))

(typ rectLeftCenter (-> Rect Point))
(def rectLeftCenter (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [0 (/ (rectHeight rect) 2) ]
  )
))

(typ rectCenter (-> Rect Point))
(def rectCenter (\\rect
  (vec2DPlus
    (rectLeftTop rect)
    [ (/ (rectWidth rect) 2) (/ (rectHeight rect) 2) ]
  )
))


; === Lines ===

(def Line SVG)

;; argument order - color, width, x1, y1, x1, y2
;; creates a line from (x1, y1) to (x2,y2) with given color and width
(typ line (-> Color Num Num Num Num Num Line))
(def line (\\(stroke w x1 y1 x2 y2)
  ['line'
     [ ['x1' x1] ['y1' y1] ['x2' x2] ['y2' y2] ['stroke' stroke] ['stroke-width' w] ]
     []]))

(typ lineBetween (-> Color Num Point Point Line))
(def lineBetween (\\(stroke w [x1 y1] [x2 y2])
  (line stroke w x1 y1 x2 y2)))

(typ lineStart (-> Line Point))
(def lineStart (\\line
  [
    (lookupNumAttrWithDefault 0 line 'x1')
    (lookupNumAttrWithDefault 0 line 'y1')
  ]
))

(typ lineEnd (-> Line Point))
(def lineEnd (\\line
  [
    (lookupNumAttrWithDefault 0 line 'x2')
    (lookupNumAttrWithDefault 0 line 'y2')
  ]
))

(typ lineMidPoint (-> Line Point))
(def lineMidPoint (\\line
  (halfwayBetween (lineStart line) (lineEnd line))
))


;; argument order - fill, stroke, width, points
;; creates a polygon following the list of points, with given fill color and a border with given width and stroke
(typ polygon (-> Color Color Num Points SVG))
(def polygon (\\(fill stroke w pts)
  ['polygon'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []]))

;; argument order - fill, stroke, width, points
;; See polygon
(typ polyline (-> Color Color Num Points SVG))
(def polyline (\\(fill stroke w pts)
  ['polyline'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []]))

;; argument order - fill, stroke, width, d
;; Given SVG path command d, create path with given fill color, stroke and width
;; See https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths for path command info
(typ path (-> Color Color Num PathCmds SVG))
(def path (\\(fill stroke w d)
  ['path'
     [ ['fill' fill] ['stroke' stroke] ['stroke-width' w] ['d' d] ]
     []]))

;; argument order - x, y, string
;; place a text string with top left corner at (x,y) - with default color & font
(typ text (-> Num Num String SVG))
(def text (\\(x y s)
   ['text' [['x' x] ['y' y] ['style' 'fill:black']
            ['font-family' 'Tahoma, sans-serif']]
           [['TEXT' s]]]))

;; argument order - shape, new attribute
;; Add a new attribute to a given Shape
(typ addAttr (-> SVG AttrPair SVG))
(def addAttr (\\([shapeKind oldAttrs children] newAttr)
  [shapeKind (snoc newAttr oldAttrs) children]))

(typ consAttr (-> SVG AttrPair SVG))
(def consAttr (\\([shapeKind oldAttrs children] newAttr)
  [shapeKind (cons newAttr oldAttrs) children]))

;; Given a list of shapes, compose into a single SVG
(def svg (\\shapes ['svg' [] shapes]))

;; argument order - x-maximum, y-maximum, shapes
;; Given a list of shapes, compose into a single SVG within the x & y maxima
(typ svgViewBox (-> Num Num (List SVG) SVG))
(def svgViewBox (\\(xMax yMax shapes)
  (let [sx sy] [(toString xMax) (toString yMax)]
  ['svg'
    [['x' '0'] ['y' '0'] ['viewBox' (joinStrings ' ' ['0' '0' sx sy])]]
    shapes])))

;; As rect, except x & y represent the center of the defined rectangle
(typ rectByCenter (-> Color Num Num Num Num Rect))
(def rectByCenter (\\(fill cx cy w h)
  (rect fill (- cx (/ w 2)) (- cy (/ h 2)) w h)))

;; As square, except x & y represent the center of the defined rectangle
(typ squareByCenter (-> Color Num Num Num Rect))
(def squareByCenter (\\(fill cx cy w) (rectByCenter fill cx cy w w)))

;; Some shapes with given default values for fill, stroke, and stroke width
; TODO remove these
(def circle_    (circle 'red'))
(def ellipse_   (ellipse 'orange'))
(def rect_      (rect '#999999'))
(def square_    (square '#999999'))
(def line_      (line 'blue' 2))
(def polygon_   (polygon 'green' 'purple' 3))
(def path_      (path 'transparent' 'goldenrod' 5))

;; updates an SVG by comparing differences with another SVG
;; Note: accDiff pre-condition: indices in increasing order
;; (so can't just use foldr instead of reverse . foldl)
(typ updateCanvas (-> SVG SVG SVG))
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

(def addBlob (\\(newShapes ['svg' svgAttrs oldShapes])
  ['svg' svgAttrs (append oldShapes newShapes)]))

(typ groupMap (forall (a b) (-> (List a) (-> a b) (List b))))
(def groupMap (\\(xs f) (map f xs)))

(def autoChose (\\(_ x _) x))
(def inferred  (\\(x _ _) x))
(def flow (\\(_ x) x))

; 'constant folding'
(def twoPi (* 2 (pi)))
(def halfPi (/ (pi) 2))

;; Helper function for nPointsOnCircle, calculates angle of points
;; Note: angles are calculated clockwise from the traditional pi/2 mark
(typ nPointsOnUnitCircle (-> Num Num (List Point)))
(def nPointsOnUnitCircle (\\(n rot)
  (let off (- halfPi rot)
  (let foo (\\i
    (let ang (+ off (* (/ i n) twoPi))
    [(cos ang) (neg (sin ang))]))
  (map foo (list0N (- n 1)))))))

(typ nPointsOnCircle (-> Num Num Num Num Num (List Point)))
;; argument order - Num of points, degree of rotation, x-center, y-center, radius
;; Scales nPointsOnUnitCircle to the proper size and location with a given radius and center
(def nPointsOnCircle (\\(n rot cx cy r)
  (let pts (nPointsOnUnitCircle n rot)
  (map (\\[x y] [(+ cx (* x r)) (+ cy (* y r))]) pts))))

(typ nStar (-> Color Color Num Num Num Num Num Num Num SVG))
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

(typ setZones (-> String SVG SVG))
(def setZones (\\(s shape) (addAttr shape ['ZONES' s])))

(typ zones (-> String (List SVG) (List SVG)))
(def zones (\\(s shapes) (map (setZones s) shapes)))
  ; TODO eta-reduced version:
  ; (def zones (\\s (map (setZones s))))

;; Remove all zones from shapes except for the first in the list
(typ hideZonesTail (-> (List SVG) (List SVG)))
(def hideZonesTail  (\\[hd | tl] [hd | (zones 'none' tl)]))

;; Turn all zones to basic for a given list of shapes except for the first shape
(typ basicZonesTail (-> (List SVG) (List SVG)))
(def basicZonesTail (\\[hd | tl] [hd | (zones 'basic' tl)]))

(typ ghost (-> SVG SVG))
(def ghost
  ; consAttr (instead of addAttr) makes internal calls to
  ; Utils.maybeRemoveFirst 'HIDDEN' slightly faster
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

;; button_ : Bool -> Num -> Num -> String -> Num -> SVG
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

(typ enumSlider (forall a (-> Num Num Num [a|(List a)] String Num [a (List SVG)])))
(def enumSlider (\\(x0 x1 y enum@[a|_] caption srcVal)
  (let n (len enum)
  (let [minVal maxVal] [0! n]
  (let preVal (clamp minVal maxVal srcVal)
  (let i (floor preVal)
  (let item ; using dummy first element for typechecking
    (let item_ (nth enum (if (= i n) (- n 1) i))
    (typecase item_
      (Null a)
      (_    item_)))
  (let wrap (\\circ (addAttr circ ['SELECTED' ''])) ; TODO
  (let shapes
    (let rail [ (line 'black' 3! x0 y x1 y) ]
    (let ball
      (let [xDiff valDiff] [(- x1 x0) (- maxVal minVal)]
      (let xBall (+ x0 (* xDiff (/ (- srcVal minVal) valDiff)))
      (let colorBall (if (= preVal srcVal) 'black' 'red')
        [ (wrap (circle colorBall xBall y 10!)) ])))
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

; Text Widgets

(def simpleText (\\(family color size x1 x2 y horizAlignSeed textVal)
  (let xMid (+ x1 (/ (- x2 x1) 2!))
  (let [anchor hAlignSlider]
    (let dx (/ (- x2 x1) 4!)
    (let yLine (+ 30! y)
    (enumSlider (- xMid dx) (+ xMid dx) yLine
      ['start' 'middle' 'end'] '' horizAlignSeed)))
  (let x
    (if (= anchor 'start') x1
    (if (= anchor 'middle') xMid
    (if (= anchor 'end') x2
      'CRASH')))
  (let theText
    ['text'
      [['x' x] ['y' y]
       ['style' (+ 'fill:' color)]
       ['font-family' family] ['font-size' size]
       ['text-anchor' anchor]]
      [['TEXT' textVal]]]
  (let rails
    (let pad 15!
    (let yBaseLine (+ y pad)
    (let xSideLine (- x1 pad)
    (let rail (line 'gray' 3)
    (let baseLine (rail xSideLine yBaseLine x2 yBaseLine)
    (let sideLine (rail xSideLine yBaseLine xSideLine (- y size))
    (let dragBall (circle 'black' x yBaseLine 8!)
    (ghosts [baseLine sideLine dragBall]))))))))
  (concat [[theText] hAlignSlider rails])
)))))))


(typ rotate (-> SVG Num Num Num SVG))
;; argument order - shape, rot, x, y
;; Takes a shape rotates it rot degrees around point (x,y)
(def rotate (\\(shape n1 n2 n3)
  (addAttr shape ['transform' [['rotate' n1 n2 n3]]])))

(typ rotateAround (-> Num Num Num SVG SVG))
(def rotateAround (\\(rot x y shape)
  (addAttr shape ['transform' [['rotate' rot x y]]])))

; Convert radians to degrees
(typ radToDeg (-> Num Num))
(def radToDeg (\\rad (* (/ rad (pi)) 180!)))

; Convert degrees to radians
(typ degToRad (-> Num Num))
(def degToRad (\\deg (* (/ deg 180!) (pi))))

; Polygon and Path Helpers

(typ middleOfPoints (-> (List Point) Point))
(def middleOfPoints (\\pts
  (let [xs ys] [(map fst pts) (map snd pts)]
  (let [xMin xMax] [(minimum xs) (maximum xs)]
  (let [yMin yMax] [(minimum ys) (maximum ys)]
  (let xMiddle (noWidgets (+ xMin (* 0.5 (- xMax xMin))))
  (let yMiddle (noWidgets (+ yMin (* 0.5 (- yMax yMin))))
    [xMiddle yMiddle] )))))))

(typ polygonPoints (-> SVG Points))
(def polygonPoints (\\shape@[shapeKind _ _]
  (case shapeKind
    ('polygon' (lookupPointsAttrWithDefault [] shape 'points'))
    (_         []))))

(typ allPointsOfPathCmds_ (-> PathCmds (List [(union Num String) (union Num String)])))
(defrec allPointsOfPathCmds_ (\\cmds (case cmds
  ([]    [])
  (['Z'] [])

  (['M' x y | rest] (cons [x y] (allPointsOfPathCmds_ rest)))
  (['L' x y | rest] (cons [x y] (allPointsOfPathCmds_ rest)))

  (['Q' x1 y1 x y | rest]
    (append [[x1 y1] [x y]] (allPointsOfPathCmds_ rest)))

  (['C' x1 y1 x2 y2 x y | rest]
    (append [[x1 y1] [x2 y2] [x y]] (allPointsOfPathCmds_ rest)))

  (_ [(let _ (debug \"Prelude.allPointsOfPathCmds_: not Nums...\") [-1 -1])])
)))

; (typ allPointsOfPathCmds (-> PathCmds (List Point)))
; (def allPointsOfPathCmds (\\cmds
;   (let toNum (\\numOrString
;     (typecase numOrString (Num numOrString) (String -1)))
;   (map (\\[x y] [(toNum x) (toNum y)]) (allPointsOfPathCmds_ cmds)))))

; TODO remove inner annotations and named lambda

(typ allPointsOfPathCmds (-> PathCmds (List Point)))
(def allPointsOfPathCmds (\\cmds
  (typ toNum (-> (union Num String) Num))
  (let toNum (\\numOrString
    (typecase numOrString (Num numOrString) (String -1)))
  (typ foo (-> [(union Num String) (union Num String)] Point))
  (let foo (\\[x y] [(toNum x) (toNum y)])
  (map foo (allPointsOfPathCmds_ cmds))))))


; Raw Shapes

(def rawShape (\\(kind attrs) [kind attrs []]))

(typ rawRect (-> Color Color Num Num Num Num Num Num Rect))
(def rawRect (\\(fill stroke strokeWidth x y w h rot)
  (let [cx cy] [(+ x (/ w 2!)) (+ y (/ h 2!))]
  (rotateAround rot cx cy
    (rawShape 'rect' [
      ['x' x] ['y' y] ['width' w] ['height' h]
      ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth] ])))))

(typ rawCircle (-> Color Color Num Num Num Num Circle))
(def rawCircle (\\(fill stroke strokeWidth cx cy r)
  (rawShape 'circle' [
    ['cx' cx] ['cy' cy] ['r' r]
    ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth] ])))

(typ rawEllipse (-> Color Color Num Num Num Num Num Num Ellipse))
(def rawEllipse (\\(fill stroke strokeWidth cx cy rx ry rot)
  (rotateAround rot cx cy
    (rawShape 'ellipse' [
      ['cx' cx] ['cy' cy] ['rx' rx] ['ry' ry]
      ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth] ]))))

(typ rawPolygon (-> Color Color Num Points Num SVG))
(def rawPolygon (\\(fill stroke w pts rot)
  (let [cx cy] (middleOfPoints pts)
  (rotateAround rot cx cy
    (rawShape 'polygon'
      [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ])))))

(typ rawPath (-> Color Color Num PathCmds Num SVG))
(def rawPath (\\(fill stroke w d rot)
  (let [cx cy] (middleOfPoints (allPointsOfPathCmds d))
  (rotateAround rot cx cy
    (rawShape 'path'
      [ ['fill' fill] ['d' d] ['stroke' stroke] ['stroke-width' w] ])))))


; Shapes via Bounding Boxes

(typ box (-> Bounds Color Color Num BoundedShape))
(def box (\\(bounds fill stroke strokeWidth)
  (let [x y xw yh] bounds
  ['BOX'
    [ ['LEFT' x] ['TOP' y] ['RIGHT' xw] ['BOT' yh]
      ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth]
    ] []
  ])))

; string fill/stroke/stroke-width attributes to avoid sliders
(typ hiddenBoundingBox (-> Bounds BoundedShape))
(def hiddenBoundingBox (\\bounds
  (ghost (box bounds 'transparent' 'transparent' '0'))))

(typ simpleBoundingBox (-> Bounds BoundedShape))
(def simpleBoundingBox (\\bounds
  (ghost (box bounds 'transparent' 'darkblue' 1))))

(typ strList (-> (List String) String))
(def strList
  (let foo (\\(x acc) (+ (+ acc (if (= acc '') '' ' ')) (toString x)))
  (foldl foo '')))

(typ fancyBoundingBox (-> Bounds (List SVG)))
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

(typ groupWithPad (-> Num Bounds (List SVG) SVG))
(def groupWithPad (\\(pad bounds shapes)
  (let [left top right bot] bounds
  (let paddedBounds [(- left pad) (- top pad) (+ right pad) (+ bot pad)]
  ['g' [['BOUNDS' bounds]]
       (cons (hiddenBoundingBox paddedBounds) shapes)]
))))

(typ group (-> Bounds (List SVG) SVG))
(def group (groupWithPad (let nGroupPad 20 nGroupPad)))

  ; NOTE:
  ;   keep the names nGroupPad and nPolyPathPad (and values)
  ;   in sync with ExpressionBasedTransform.elm

  ; (def group (groupWithPad 15))

(def polyPathGroup (groupWithPad (let nPolyPathPad 10 nPolyPathPad)))

; TODO make one pass over pts
(typ boundsOfPoints (-> (List Point) Bounds))
(def boundsOfPoints (\\pts
  (let left  (minimum (map fst pts))
  (let right (maximum (map fst pts))
  (let top   (minimum (map snd pts))
  (let bot   (maximum (map snd pts))
    [left top right bot]
))))))

(typ extremeShapePoints (-> SVG Points))
(def extremeShapePoints (\\shape@[kind _ _]
  (case kind

    ('line'
      (let attrs@[x1 y1 x2 y2] (map (lookupAttr shape) [\"x1\" \"y1\" \"x2\" \"y2\"])
      (typecase attrs
        ([Num Num Num Num] [[x1 y1] [x2 y2]])
        (_ []))))

    ('rect'
      (let attrs@[x y w h] (map (lookupAttr shape) [\"x\" \"y\" \"width\" \"height\"])
      (typecase attrs
        ([Num Num Num Num] [[x y] [(+ x w) (+ y h)]])
        (_ []))))

    ('circle'
      (let attrs@[cx cy r] (map (lookupAttr shape) [\"cx\" \"cy\" \"r\"])
      (typecase attrs
        ([Num Num Num] [[(- cx r) (- cy r)] [(+ cx r) (+ cy r)]])
        (_ []))))

    ('ellipse'
      (let attrs@[cx cy rx ry] (map (lookupAttr shape) [\"cx\" \"cy\" \"rx\" \"ry\"])
      (typecase attrs
        ([Num Num Num Num] [[(- cx rx) (- cy ry)] [(+ cx rx) (+ cy ry)]])
        (_ []))))

    ('polygon' (polygonPoints shape))

    ('path'
      (let pathCmds (lookupAttr shape \"d\")
      (typecase pathCmds
        ((List (union String Num)) (allPointsOfPathCmds pathCmds))
        (_ []))))

    (_ [])
)))

(typ anchoredGroup (-> (List SVG) SVG))
(def anchoredGroup (\\shapes
  (let bounds (boundsOfPoints (concat (map extremeShapePoints shapes)))
  (group bounds shapes)
)))

; (def group (\\(bounds shapes)
;   ['g' [['BOUNDS' bounds]]
;        (cons (hiddenBoundingBox bounds) shapes)]))

       ; (concat [(fancyBoundingBox bounds) shapes])]))

; TODO no longer used...
(typ rotatedRect (-> Color Num Num Num Num Num Rect))
(def rotatedRect (\\(fill x y w h rot)
  (let [cx cy] [(+ x (/ w 2!)) (+ y (/ h 2!))]
  (let bounds [x y (+ x w) (+ y h)]
  (let shape (rotateAround rot cx cy (rect fill x y w h))
  (group bounds [shape])
)))))

(typ rectangle (-> Color Color Num Num Bounds Rect))
(def rectangle (\\(fill stroke strokeWidth rot bounds)
  (let [left top right bot] bounds
  (let [cx cy] [(+ left (/ (- right left) 2!)) (+ top (/ (- bot top) 2!))]
  (let shape (rotateAround rot cx cy (box bounds fill stroke strokeWidth))
  shape
)))))
  ; (group bounds [shape])

; TODO no longer used...
(typ rotatedEllipse (-> Color Num Num Num Num Num Ellipse))
(def rotatedEllipse (\\(fill cx cy rx ry rot)
  (let bounds [(- cx rx) (- cy ry) (+ cx rx) (+ cy ry)]
  (let shape (rotateAround rot cx cy (ellipse fill cx cy rx ry))
  (group bounds [shape])
))))

; TODO take rot
(typ oval (-> Color Color Num Bounds BoundedShape))
(def oval (\\(fill stroke strokeWidth bounds)
  (let [left top right bot] bounds
  (let shape
    ['OVAL'
       [ ['LEFT' left] ['TOP' top] ['RIGHT' right] ['BOT' bot]
         ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth] ]
       []]
  shape
))))

; ; TODO take rot
; (def oval (\\(fill stroke strokeWidth bounds)
;   (let [left top right bot] bounds
;   (let [rx ry] [(/ (- right left) 2!) (/ (- bot top) 2!)]
;   (let [cx cy] [(+ left rx) (+ top ry)]
;   (let shape ; TODO change def ellipse to take stroke/strokeWidth
;     ['ellipse'
;        [ ['cx' cx] ['cy' cy] ['rx' rx] ['ry' ry]
;          ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth] ]
;        []]
;   (group bounds [shape])
; ))))))

(def scaleBetween (\\(a b pct)
  (case pct
    (0 a)
    (1 b)
    (_ (+ a (* pct (- b a)))))))

(typ stretchyPolygon (-> Bounds Color Color Num (List Num) SVG))
(def stretchyPolygon (\\(bounds fill stroke strokeWidth percentages)
  (let [left top right bot] bounds
  (let [xScale yScale] [(scaleBetween left right) (scaleBetween top bot)]
  (let pts (map (\\[xPct yPct] [ (xScale xPct) (yScale yPct) ]) percentages)
  ; (group bounds [(polygon fill stroke strokeWidth pts)])
  (polyPathGroup bounds [(polygon fill stroke strokeWidth pts)])
)))))

; TODO no longer used...
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
      [])]
))))
      ; turning off points for now
      ; (pointsOf d)) ]

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
  ; (group bounds
  (polyPathGroup bounds
    (cons
      (path fill stroke w (toPath d))
      []))
)))))))
      ; turning off points for now
      ; (pointsOf d)))

(typ evalOffset (-> [Num Num] Num))
(def evalOffset (\\[base off]
  (case off
    (0 base)
    (_ (+ base off)))))

(def stickyPolygon (\\(bounds fill stroke strokeWidth offsets)
  (let pts (map (\\[xOff yOff] [ (evalOffset xOff) (evalOffset yOff) ]) offsets)
  (group bounds [(polygon fill stroke strokeWidth pts)])
)))

(typ withBounds (-> Bounds (-> Bounds (List SVG)) (List SVG)))
(def withBounds (\\(bounds f) (f bounds)))

(typ withAnchor (-> Point (-> Point (List SVG)) (List SVG)))
(def withAnchor (\\(anchor f) (f anchor)))

(typ star (-> Bounds (List SVG)))
(def star (\\bounds
  (let [left top right bot] bounds
  (let [width height] [(- right left) (- bot top)]
  (let [cx cy] [(+ left (/ width 2)) (+ top (/ height 2))]
  [(nStar 0 'black' 0 6 (min (/ width 2) (/ height 2)) 10 0 cx cy)]
)))))

(typ blobs (-> (List Blob) SVG))
(def blobs (\\blobs
  (let modifyBlob (\\[i blob]
    (case blob
      ([['g' gAttrs [shape | shapes]]]
       [['g' gAttrs [(consAttr shape ['BLOB' (toString (+ i 1))]) | shapes]]])
      ([shape] [(consAttr shape ['BLOB' (toString (+ i 1))])])
      (_       blob)))
  (svg (concat (mapi modifyBlob blobs)))
)))


; === Relations ===

(typ halfwayBetween (-> Point Point Point))
(def halfwayBetween (\\(pt1 pt2)
  (vec2DScalarMult 0.5 (vec2DPlus pt1 pt2))
))

(typ nextInLine (-> Point Point Point))
(def nextInLine (\\(pt1 pt2)
  (vec2DPlus pt2 (vec2DMinus pt2 pt1))
))

; Point on line segment, at `ratio` location.
(typ onLine (-> Point Point Num Point))
(def onLine (\\(pt1 pt2 ratio)
  (let vec (vec2DMinus pt2 pt1)
  (vec2DPlus pt1 (vec2DScalarMult ratio vec)))))

; === Basic Replicate ===

(def horizontalArray (\\(n sep func [x y])
  (let _ ; draw point widget to control anchor
    ([x y] : Point)
  (let draw_i (\\i
    (let xi (+ x (* i sep))
    (func [xi y])))
  (concat (map draw_i (zeroTo n)))
))))

(def linearArrayFromTo (\\(n func [xStart yStart] [xEnd yEnd])
  (let xsep (/ (- xEnd xStart) (- n 1))
  (let ysep (/ (- yEnd yStart) (- n 1))
  (let draw_i (\\i
    (let xi (+ xStart (* i xsep))
    (let yi (+ yStart (* i ysep))
    (func [xi yi]))))
  (concat (map draw_i (zeroTo n)))
)))))

; To reduce size of resulting trace,
; could subtract up to M>1 at a time.
;
(defrec floorAndLocalFreeze (\\n
  (if (le n 1) 0
  ;else
    (+ 1 (floorAndLocalFreeze (- n 1))))))

  ; (let _ ; draw point widget to control anchor
  ;   ([cx cy] : Point)
(def radialArray (\\(n radius rot func [cx cy])
  (let center ; draw ghost circle to control anchor
              ; not using point widget, since it's not selectable
    (ghost (circle 'orange' cx cy 20))
  (let _ ; draw point widget to control radius
    (let xWidget (floorAndLocalFreeze cx)
    (let yWidget (- (floorAndLocalFreeze cy) radius)
      ([xWidget yWidget] : Point)))
  (let endpoints (nPointsOnCircle n rot cx cy radius)
  (let bounds
    [(- cx radius) (- cy radius) (+ cx radius) (+ cy radius)]
  [(group bounds (cons center (concat (map func endpoints))))]
))))))

(def offsetAnchor (\\(dx dy f)
  (\\[x y] (f [(+ x dx) (+ y dy)]))
))

(def horizontalArrayByBounds (\\(n sep func [left_0 top right_0 bot])
  (let w_i     (- right_0 left_0)
  (let left_i  (\\i (+ left_0 (* i (+ w_i sep))))
  (let right_i (\\i (+ (left_i i) w_i))
  (let draw_i  (\\i (func [(left_i i) top (right_i i) bot]))
  (let bounds  [left_0 top (right_i (- n 1)) bot]
    [(groupWithPad 30 bounds (concat (map draw_i (zeroTo n))))]
)))))))

(def repeatInsideBounds (\\(n sep func bounds@[left top right bot])
  (let w_i (/ (- (- right left) (* sep (- n 1))) n)
  (let draw_i (\\i
    (let left_i (+ left (* i (+ w_i sep)))
    (let right_i (+ left_i w_i)
    (func [left_i top right_i bot]))))
  [(groupWithPad 30 bounds (concat (map draw_i (zeroTo n))))]
))))


(def draw svg)

(def showOne (\\(x y val)
   ['text' [['x' x] ['y' y] ['style' 'fill:black']
            ['font-family' 'monospace']
            ['font-size' '12pt']]
           [['TEXT' (toString val)]]]))

(def show (showOne 20 30))

(def showList (\\vals
  ['g' [] (mapi (\\[i val] (showOne 20 (* (+ i 1) 30) val)) vals)]))

(def rectWithBorder (\\(stroke strokeWidth fill x y w h)
  (addAttr (addAttr
    (rect fill x y w h)
      [\"stroke\" stroke])
      [\"stroke-width\" strokeWidth])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (def columnsToRows (\\columns
;   (let numColumns (len columns)
;   (let numRows ; maxColumnSize
;     (if (= numColumns 0) 0 (maximum (map len columns)))
;   (foldr
;     (\\(col rows)
;       (let paddedCol (append col (repeat (- numRows (len col)) \".\"))
;       (map
;         (\\[datum row] [ datum | row ])
;         (zip paddedCol rows))))
;     (repeat numRows [])
;     columns)
; ))))
; 
; (def addColToRows (\\(col rows)
;   (let width (maximum (map len rows))
;   (letrec foo (\\(col rows)
;     (case [col rows]
;       ([ []     []     ] [                                          ])
;       ([ [x|xs] [r|rs] ] [ (snoc x r)                 | (foo xs rs) ])
;       ([ []     [r|rs] ] [ (snoc '' r)                | (foo [] rs) ])
;       ([ [x|xs] []     ] [ (snoc x (repeat width '')) | (foo xs []) ])
;     ))
;   (foo col rows)))))

; (def border ['border' '1px solid black'])
; (def padding ['padding' '5px'])
; (def center ['align' 'center'])
; (def style (\\list ['style' list]))
; (def onlyStyle (\\list [(style list)]))
; 
; (def td (\\text
;   ['td' (onlyStyle [border padding])
;         [['TEXT' text]]]))
; 
; (def th (\\text
;   ['th' (onlyStyle [border padding center])
;         [['TEXT' text]]]))
; 
; (def tr (\\children
;   ['tr' (onlyStyle [border])
;         children]))
; 
; ; TODO div name is already taken...
; 
; (def div_ (\\children ['div' [] children]))
; (def h1 (\\text ['h1' [] [['TEXT' text]]]))
; (def h2 (\\text ['h2' [] [['TEXT' text]]]))
; (def h3 (\\text ['h3' [] [['TEXT' text]]]))
; 
; (def table (\\children
;   ['table' (onlyStyle [border padding]) children]))

; (def table (\\children
;   (let [x y] [100 100]
;   ['table' (onlyStyle [border padding
;                       [\"position\" \"relative\"]
;                       [\"left\" (toString x)]
;                       [\"top\" (toString y)]]) children])))

; (def tableOfData (\\data
;   (let letters (explode \" ABCDEFGHIJKLMNOPQRSTUVWXYZ\")
;   (let data (mapi (\\[i row] [(+ i 1) | row]) data)
;   (let tableWidth (maximum (map len data))
;   (let headers
;     (tr (map (\\letter (th letter)) (take letters tableWidth)))
;   (let rows
;     (map (\\row (tr (map (\\col (td (toString col))) row))) data)
;   (table
;     [ headers | rows ]
; ))))))))


(def textNode (\\text
  [\"TEXT\" text]))

(def textElementHelper (\\(tag styles attrs text)
  [ tag [ [\"style\" styles] | attrs ] [ (textNode text) ] ]))

(def elementHelper (\\(tag styles attrs children)
  [ tag [ [\"style\" styles] | attrs ] children ]))

(def p (textElementHelper \"p\"))
(def th (textElementHelper \"th\"))
(def td (textElementHelper \"td\"))
(def h1 (textElementHelper \"h1\"))
(def h2 (textElementHelper \"h2\"))
(def h3 (textElementHelper \"h3\"))

(def div_ (elementHelper \"div\"))
(def tr (elementHelper \"tr\"))
(def table (elementHelper \"table\"))

; absolutePositionStyles x y = let _ = [x, y] : Point in
;   [ [\"position\", \"absolute\"]
;   , [\"left\", toString x + \"px\"]
;   , [\"top\", toString y + \"px\"]
;   ] 



(def setStyles (\\(newStyles [kind attrs children])
  (let attrs
    (let styleAttr (lookup 'style' attrs)
    (typecase styleAttr
      (Null [ ['style' []] | attrs ])
      (_    attrs)))
    ; (if (= styleAttr null)
    ;   [ ['style' []] | attrs ]
    ;   attrs)
  (let attrs
    (map (\\[key val]
      (case key
        ('style'
          (let otherStyles
            (concatMap (\\[k v]
              (case (elem k (map fst newStyles))
                (true  [])
                (false [[k v]])))
              val)
          ['style' (append newStyles otherStyles)]))
        (_
          [key val])))
      attrs)
  [kind attrs children]
))))

(def placeAt (\\([x y] node)
  (let _ ([x y] : Point)
  ; TODO px suffix should be added in LangSvg/Html translation
  (setStyles
    [ ['position' 'absolute']
      ['left' (+ (toString x) 'px')]
      ['top' (+ (toString y) 'px')]
    ]
    node))))

(def placeAtFixed (\\([x y] node)
  (let _ ([x y] : Point)
  (setStyles
    [['position' 'fixed'] ['FIXED_LEFT' x] ['FIXED_TOP' y]]
    node))))

(def placeSvgAt (\\([x y] w h shapes)
  (placeAt [x y]
    ['svg' [['width' w] ['height' h]] shapes])))

(def workspace (\\(minSize children)
  (div_
    (cons
      (placeAt minSize (h3 '</workspace>'))
      children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def nothing [\"Nothing\"])
(def just (\\x [\"Just\"  x]))

(def mapFirst (\\(f [x y]) [(f x) y]))
(def mapSecond (\\(f [x y]) [x (f y)]))

(def freeze (\\x x))


; The type checker relies on the name of this definition.
(let dummyPreludeMain ['svg' [] []] dummyPreludeMain)

"""

preludeLeo =
 """
-- This standard prelude library is accessible by every program.
--------------------------------------------------------------------------------
-- Debug --

Debug = {
  log msg value =
    -- Call Debug.log \"msg\" value
    let _ = debug (msg + \": \" + toString value) in
    value
  start msg value =
    -- Call Debug.start \"msg\" <| \\_ -> (remaining)
    let _ = debug msg in
    value []
  crash msg = error msg
  time msg callback =
    let start = getCurrentTime () in
    let res = callback () in
    let end = getCurrentTime () in
    let _ = debug (msg + \" took \" + toString (end - start) + \"ms\") in
    res
}

 -- Much simpler version, does not handle @ symbols wells.
htmlViaEval string =
  case __evaluate__ [(\"append\", append)] <| Update.freezeExcept (always \"Cannot modify raw template\") string <| \\string -> \"\"\"<raw>@string</raw>\"\"\" of
    Ok [_, _, children] -> children
    Err msg -> error msg
    _ -> error \"Parsing HTML failed:\"

-- building block for updating
freeze x = x
expressionFreeze x = Debug.log \"expressionFreeze is deprecated. Please use Update.freezeExcept instead\" x
-----------------------------------------
-- Building blocks functions

-- The following functions should be removed if they are not used (they are redundant)

--; The identity function - given a value, returns exactly that value
-- id: (forall a (-> a a))
id x = x
--; Composes two functions together
--compose: (forall (a b c) (-> (-> b c) (-> a b) (-> a c)))
compose f g = \\x -> f (g x)
--fst: (forall (a b) (-> [a b] a))
--snd: (forall (a b) (-> [a b] b))
fst [a, _] = a
snd [_, b] = b
--or:  (-> Bool Bool Bool)
--and: (-> Bool Bool Bool)
or p q = if p then True else q
and p q = if p then q else False
--lt: (-> Num Num Bool)
--eq: (-> Num Num Bool)
--le: (-> Num Num Bool)
--gt: (-> Num Num Bool)
--ge: (-> Num Num Bool)
lt x y = x < y
eq x y = x == y
le x y = or (lt x y) (eq x y)
gt x y = x > y
ge x y = or (gt x y) (eq x y)
nil = []
cons x xs = x :: xs

-- The following functions could be kept but could migrate (e.g. len to List.length)

--; Given two bools, returns a bool regarding if the first argument is true, then the second argument is as well
--implies: (-> Bool Bool Bool)
implies p q = if p then q else True

--; Returns the length of a given list
--len: (forall a (-> (List a) Num))
len xs = case xs of [] -> 0; (_ :: xs1) -> 1 + len xs1

zip xs ys =
  case (xs, ys) of
    (x::xsRest, y::ysRest) -> (x, y) :: zip xsRest ysRest
    _                      -> []

zipOld xs ys =
  case (xs, ys) of
    (x::xsRest, y::ysRest) -> [x, y] :: zipOld xsRest ysRest
    _                      -> []

range i j =
  if i < j + 1
    then cons i (range (i + 1) j)
    else nil

-----------------------------------------
--Basics = {
-- TODO: Once we have types, wrap these basics into a module.

--(==) is an Op
--(/=) is in builtinEnv
--(<) is an Op
--(>) is in builtinEnv
--(<=) is in builtinEnv
--(>=) is in builtinEnv

--max: (-> Num Num Num)
max i j = if i >= j then i else j

--min: (-> Num Num Num)
min i j = if i < j then i else j

type Order = LT | EQ | GT

compare a b = if a < b then LT else if a == b then EQ else GT

--- Booleans

--; Given a bool, returns the opposite boolean value
--not: (-> Bool Bool)
not b =  {
    apply b = if b then False else True
    unapply b = Just (if b then False else True)
  }.apply b

--(&&) is built-in
--(||) is built-in

xor a b = if a then not b else b

-- Mathematics

-- (+) is an Op
-- (-) is an Op
-- (*) is an Op
-- (/) is an Op
-- (^) is an Op
-- TODO: (//)
-- TODO: rem

negate x = 0 - x

--; Absolute value
--abs: (-> Num Num)
abs x = if x < 0 then neg x else x

-- sqrt is an Op

--; Given an upper bound, lower bound, and a number, restricts that number between those bounds (inclusive)
--; Ex. clamp 1 5 4 = 4
--; Ex. clamp 1 5 6 = 5
--clamp: (-> Num Num Num Num)
clamp i j n = if n < i then i else if j < n then j else n

-- TODO: logBase base num

e = 2.718281828459045

-- pi is an Op

tau = 2 * pi

-- cos is an op
-- sin is an op

tan x = cos x / sin x

acos = arccos

asin = arcsin

-- TODO: atan

atan2 = arctan2

-- round is an op
-- floor is an op
-- ceiling is an op
truncate x = if x > 0 then floor x else ceiling x

toFloat x = x

-- Convert radians to degrees
-- radToDeg: (-> Num Num)
radToDeg rad = rad / tau * 360!

-- Convert degrees to radians
-- degToRad: (-> Num Num)
degToRad deg = deg / 360! * tau

degrees = degToRad

radians x = x

turns x = x * tau

-- Polar coordinates

toPolar (x, y) = (sqrt (x * x + y * y), atan2 y x)
fromPolar (r, t) = (r * cos t, r * sin t)

-- Floating point check

-- TODO: isNaN
-- TODO: isInfinite

-- Strings and lists

-- toString is an op
-- (++) is interpreted as append

-- Higher-order helpers

identity x = x
  --; A function that always returns the same value a, regardless of b
  -- always: (forall (a b) (-> a b a))
always x _ = x

-- <| is defined as a left application
-- |> is defined as a right application
-- (<<) is defined in builtinEnv
-- (>>) is defined in builtinEnv

--flip: (forall (a b c) (-> (-> a b c) (-> b a c)))
flip f = \\x y -> f y x

flips = {
  moveArg n m f =
    if n == 1 && m == 1 then f
    else if n > 1 && m > 1 then
       \\x -> moveArg (n-1) (m-1) (f x)
    else if n == 1 then   -- Here m > 1
      moveArg 2 m (\\x y -> f y x)
    else -- m == 1 && n > 1
      \\x y -> (moveArg n 2 f) y x

  ab_ba = moveArg 1 2 -- same as flip
  abc_bca = moveArg 1 3
  abcd_bcda = moveArg 1 4
}

curry f a b = f (a,b)
uncurry f (a,b) = f a b

-- type Never

never x = Debug.crash \"Never can never be called\"

--------------------------------------------------------------------------------
-- LensLess modules (List, Results, String) for definitions without lenses

LensLess =
  let reverse l =
    let r acc l = case l of [] -> acc; head::tail -> r (head::acc) tail in
    r [] l
  in
  let map f l =
     case l of
       []    -> []
       x::xs -> f x :: map f xs
  in
  let append xs ys =
     case xs of
       [] -> ys
       x::xs1 -> x :: append xs1 ys
  in
  let split n l =
    let aux acc n l =
      if n == 0 then (reverse acc, l) else
      case l of
        [] -> (reverse acc, l)
        head::tail -> aux (head::acc) (n - 1) tail
    in aux [] n l in
  let take =
    let aux n l = if n == 0 then [] else
      case l of
        [] -> []
        head::tail -> head :: (aux (n - 1) tail)
    in aux in
  let drop =
    let aux n l = if n == 0 then l else
      case l of
        [] -> []
        head::tail -> aux (n - 1) tail
    in aux in
  let reverse_move n stack from = if n <= 0 then (stack, from) else case from of
    [] -> (stack, from)
    head::tail -> reverse_move (n - 1) (head::stack) tail
  in
  let filterMap f l = case l of
    [] -> []
    (head :: tail) -> case f head of
      Nothing -> filterMap f tail
      Just newHead -> newHead :: filterMap f tail
  in
  let concatMap f l = case l of
    [] -> []
    head :: tail -> f head ++ concatMap f tail
  in
  let last l = case l of
    [head] -> Just head
    _ :: tail -> last tail
    _ -> Nothing
  in
  let map2 f xs ys =
    case [xs, ys] of
      [x::xs1, y::ys1] -> f x y :: map2 f xs1 ys1
      _                -> []
  in
  let zip = map2 (,) in
  { appendStrDef = \"\"\"let append a b = case a of [] -> b; (h::t) -> h :: append t b in \"\"\"
    Maybe = {
      map f a = case a of
        Nothing -> Nothing
        Just x -> Just (f x)
    }
    List = {
      append = append
      split = split
      take = take
      drop = drop
      reverse = reverse
      reverse_move = reverse_move
      filterMap = filterMap
      map = map
      last = last
      map2 = map2
      concatMap = concatMap
      zip = zip
    },
    Result =
      let map f res = case res of
        Err msg -> res
        Ok x -> Ok (f x)
      in
      let andThen f res = case res of
        Err msg -> res
        Ok x -> f x
      in
      {
        map = map
        andThen = andThen
    },
    Results =
      let keepOks l =
        case l of
          [] -> []
          (Err _) ::tail -> keepOks tail
          (Ok ll) :: tail -> ll ++ keepOks tail
      in
      let projOks l =
        case l of
          [] -> Ok []
          (Ok []) :: tail -> projOks tail
          (Ok (vhead :: vtail)) ::tail -> Ok (vhead::(vtail ++ keepOks tail))
          (Err msg) :: tail ->
            case projOks tail of
              Err msgTail -> Err msg
              Ok []-> Err msg
              result -> result
      in
      let andThen callback results =
        --andThen : (a -> Results x b) -> Results x a -> Results x b
        case results of
          Ok ll -> ll |> map callback |> projOks
          Err msg -> results
      in
      let resultMap callback results =
        case results of
          Ok ll -> Ok (ll |> map callback)
          Err msg -> results
      in
      let andAlso otherResults results =
        case (results, otherResults) of
          (Ok ll, Ok otherLl) -> Ok (ll ++ otherLl)
          (Err msg, Err msg2) -> Err (msg + \"\\n\" + msg2)
          (Err msg, _) -> Err msg
          (_, Err msg2) -> Err msg2
      in
      {
        keepOks = keepOks
        projOks = projOks
        andThen = andThen
        andAlso = andAlso
        map = resultMap
      }
    String = {
      toInt =
        let d = __DictFromList__ [(\"0\", 0), (\"1\", 1), (\"2\", 2), (\"3\", 3), (\"4\", 4), (\"5\", 5), (\"6\", 6), (\"7\", 7), (\"8\", 8), (\"9\", 9)] in
        let aux x =
          case extractFirstIn \"^([0-9]*)([0-9])$\" x of
            Just [init, last] -> (aux init)*10 + case __DictGet__ last d of
              Just x -> x
              Nothing -> 0
            Nothing -> 0
        in
        \\x ->
          case extractFirstIn \"^-(.*)$\" x of
            Just part -> 0 - aux part
            Nothing -> aux x

      join delimiter list =
        let aux acc list = case list of
          [] -> acc
          [head] -> acc + head
          (head::tail) -> aux (acc + head + freeze delimiter) tail
        in aux \"\" list

      substring start end x =
        case extractFirstIn (\"^[\\\\s\\\\S]{0,\" + toString start + \"}([\\\\s\\\\S]{0,\" + toString (end - start) + \"})\") x of
          Just [substr] -> substr
          Nothing -> Debug.crash <| \"bad arguments to String.substring \" + toString start + \" \" + toString end + \" \" + toString x

      take length x =
          case extractFirstIn (\"^([\\\\s\\\\S]{0,\" + toString length + \"})\") x of
            Just [substr] -> substr
            Nothing -> Debug.crash <| \"bad arguments to String.take \" + toString length + \" \" + toString x

      drop length x =
        if length <= 0 then x else
        case extractFirstIn (\"^[\\\\s\\\\S]{0,\" + toString length + \"}([\\\\s\\\\S]*)\") x of
                Just [substr] -> substr
                Nothing -> Debug.crash <| \"bad arguments to String.drop \" + toString length + \" \" + toString x

      dropLeft = drop
      dropRight length x =
        if length <= 0 then x else
        case extractFirstIn \"\"\"^([\\s\\S]*?)[\\s\\S]{0,@length}$\"\"\" x of
              Just [substr] -> substr
              Nothing -> Debug.crash <| \"bad arguments to String.drop \" + toString length + \" \" + toString x

      length x = __strLength__ x

      slice = substring

      toFloat s =
        case extractFirstIn \"\"\"((-?)\\d+)\\.(\\d+)\"\"\" s of
           Just [intPart, negative, floatPart] ->
             let combine y = if negative == \"-\" then 0 - y else y in
             toInt intPart + (combine <| toInt floatPart / (10 ^ length floatPart))
           Nothing ->
        case extractFirstIn \"\"\"((-?)\\d)(?:\\.(\\d*))?(?:e|E)\\+?(-?\\d+)\"\"\" s of -- Scientific notation
           Just [beforeComma, negative, afterComma, exponent] ->
             toInt beforeComma + (combine <| toInt afterComma  / (10 ^ length afterComma)) * 10 ^ (toInt exponent)
           Nothing ->
             toInt s

      sprintf str inline = case inline of
          a::tail -> sprintf (replaceFirstIn \"%s\" a str) tail
          [] -> str
          a -> replaceFirstIn \"%s\" a str
    }
  }

Result = {
  type Result err ok = Err err | Ok ok

  map: (a -> b) -> Result err a -> Result err b
  map f res = case res of
    Err msg -> res
    Ok x -> Ok (f x)

  mapError err err2: (err -> err2) -> forall a. Result err a -> Result err2 a
  mapError f res = case res of
    Err msg -> res
    Ok x -> Ok (f x)

  andThen: (a -> Result err b) -> Result err a -> Result err b
  andThen f res = case res of
    Err msg -> res
    Ok x -> f x

  toMaybe: Result err ok -> Maybe ok
  toMaybe res = case res of
    Err msg -> Nothing
    Ok x -> Just x

  fromMaybe: err -> Maybe ok -> Result err ok
  fromMaybe err res = case res of
    Nothing -> Err err
    Just x -> Ok x

  fromMaybeLazy: (() -> err) -> Maybe ok -> Result err ok
  fromMaybeLazy msgBuilder = case of
    Just x -> Ok x
    Nothing -> Err (msgBuilder ())

  withDefault: ok -> Result err ok -> ok
  withDefault defaultValue res = case res of
    Err x -> defaultValue
    Ok x -> x

  (errn) n msg = if n == 1 then Err msg else \\_ -> errn (n - 1) msg

  --Variable arity function. mapn 1 == map, mapn 2 == map2, etc.
  --Dependently typed.
  mapn n = if n == 1 then map else
    \\f res -> case res of
      Err msg -> errn n msg
      Ok r -> mapn (n - 1) (f r)

  map2 = mapn 2
  map3 = mapn 3
  map4 = mapn 4

  andThenn n = if n == 1 then andThen else
    \\f res -> case res of
      Err msg -> errn n msg
      Ok r -> andThenn (n - 1) (f r)

  andThen2 = andThenn 2
  andThen3 = andThenn 3
  andThen4 = andThenn 4

  withDefaultMapError: (b -> a) -> Result a b -> a
  withDefaultMapError f = case of
    Ok x -> x
    Err msg -> f msg

  fold : (err -> a) -> (x -> a) -> Result err x -> a
  fold onErr onOk content =
    case content of
      Err msg -> onErr msg
      Ok c -> onOk c
}

--------------------------------------------------------------------------------
-- Update --

--type ListElemDiff a = ListElemUpdate a | ListElemInsert Int | ListElemDelete Int
--type VDictElemDiff = VDictElemDelete | VDictElemInsert | VDictElemUpdate VDiffs
--type alias EnvDiffs = TupleDiffs VDiffs
-- The environment of a closure if it was modified, the modifications of an environment else.
--type VDiffs = VClosureDiffs EnvDiffs (Maybe EDiffs)
--            | VListDiffs (ListDiffs VDiffs)
--            | VDictDiffs (Dict (String, String) VDictElemDiff)
--            | VRecordDiffs (Dict String VDiffs)
--            | VConstDiffs

--type EDiffs = EConstDiffs EWhitespaceDiffs
--            | EListDiffs (ListDiffs EDiffs)
--            | EChildDiffs (TupleDiffs EDiffs) -- Also for records

--type EWhitespaceDiffs = EOnlyWhitespaceDiffs | EAnyDiffs
  
-- The diff primitive is:
--
--   type alias DiffOp : Value -> Value -> Result String (Maybe VDiffs)
--   diff : DiffOp
--   diff ~= SnS.Update.defaultVDiffs
--

Update =
  let {String, List} = LensLess in
  let {reverse} = List in
  let freeze x =
    x
  in
  let applyLens lens x =
    lens.apply x
  in
  let softFreeze x =
    -- Update.freeze x prevents changes to x (resulting in failure),
    -- Update.softFreeze x ignores changes to x
    let constantInputLens =
      { apply x = x, update {input} = Ok (Inputs [input]) }
    in
    applyLens constantInputLens x
  in
  -- TODO: Replace this by enabling the return of Result String (Values [values...] | ValuesDiffs [(values, diffs)])
  let valuesWithDiffs valuesDiffs = Ok (InputsWithDiffs valuesDiffs) in
  let resultValuesWithDiffs valuesDiffs = case valuesDiffs of
     [] -> Ok (InputsWithDiffs [])
     (Ok vd)::tail -> case resultValuesWithDiffs tail of
       Err msg -> Ok (InputsWithDiffs [vd])
       Ok (InputsWithDiffs vds) -> Ok (InputsWithDiffs (vd :: vds))
     (Err msg)::tail -> if tail == [] then Err msg else
       case resultValuesWithDiffs tail of
       Err error-> Err (msg + \"\\n\" + error)
       x -> x
  in
  let addDiff f mbDiff (d, changed) =
        case mbDiff of
          Nothing -> (d, changed)
          Just x -> (f d x, True)
  in
  let pairDiff2 mbDiff1 mbDiff2 =
        ({}, False)
        |> addDiff (\\d x -> {d | _1 = x}) mbDiff1
        |> addDiff (\\d x -> {d | _2 = x}) mbDiff2
        |> (\\(d, changed) -> if changed then Just (VRecordDiffs d) else Nothing)
  in
  let pairDiff3 mbDiff1 mbDiff2 mbDiff3 =
        ({}, False)
        |> addDiff (\\d x -> {d | _1 = x}) mbDiff1
        |> addDiff (\\d x -> {d | _2 = x}) mbDiff2
        |> addDiff (\\d x -> {d | _3 = x}) mbDiff3
        |> (\\(d, changed) -> if changed then Just (VRecordDiffs d) else Nothing)
  in
  let pairDiff4 mbDiff1 mbDiff2 mbDiff3 mbDiff4 =
        ({}, False)
        |> addDiff (\\d x -> {d | _1 = x}) mbDiff1
        |> addDiff (\\d x -> {d | _2 = x}) mbDiff2
        |> addDiff (\\d x -> {d | _3 = x}) mbDiff3
        |> addDiff (\\d x -> {d | _4 = x}) mbDiff4
        |> (\\(d, changed) -> if changed then Just (VRecordDiffs d) else Nothing)
  in
  let
    type SimpleListDiffOp = KeepValue | DeleteValue | InsertValue Value | UpdateValue Value
  in
  let listDiffOp diffOp oldValues newValues =
   -- listDiffOp : DiffOp -> List Value -> List Value -> List SimpleListDiffOp

    -- let {Keep, Delete, Insert, Update} = SimpleListDiffOp in
     let {append} = List in
     case diffOp oldValues newValues of
        Ok (Just (VListDiffs listDiffs)) ->
          let aux i revAcc oldValues newValues listDiffs =
            case listDiffs of
              [] ->
                reverse (map1 (\\_ -> KeepValue) oldValues ++ revAcc)
              (j, listDiff)::diffTail ->
                if j > i then
                  case [oldValues, newValues] of
                    [_::oldTail, _::newTail] ->
                      aux (i + 1) (KeepValue::revAcc) oldTail newTail listDiffs
                    _ -> Debug.crash <| \"[Internal error] Expected two non-empty tails, got  \" + toString [oldValues, newValues]
                else if j == i then
                  case listDiff of
                    ListElemUpdate _ ->
                      case [oldValues, newValues] of
                        [oldHead::oldTail, newHead::newTail] ->
                          aux (i + 1) (UpdateValue newHead :: revAcc) oldTail newTail diffTail
                        _ -> Debug.crash <| \"[Internal error] update but missing element\"
                    ListElemInsert count ->
                      case newValues of
                        newHead::newTail ->
                          aux i (InsertValue newHead::revAcc) oldValues newTail (if count == 1 then diffTail else (i, ListElemInsert (count - 1))::diffTail)
                        _ -> Debug.crash <| \"[Internal error] insert but missing element\"
                    ListElemDelete count ->
                      case oldValues of
                        oldHead::oldTail ->
                          aux (i + 1) (DeleteValue::revAcc) oldTail newValues (if count == 1 then diffTail else (i + 1, ListElemDelete (count - 1))::diffTail)
                        _ -> Debug.crash <| \"[Internal error] insert but missing element\"
                else Debug.crash <| \"[Internal error] Differences not in order, got index \" + toString j + \" but already at index \" + toString i
          in aux 0 [] oldValues newValues listDiffs

        result -> Debug.crash (\"Expected Ok (Just (VListDiffs listDiffs)), got \" + toString result)
  in
  let
    type StringDiffs = StringUpdate Int Int Int
    type ConcStringDiffs = ConcStringUpdate Int Int String
  in
  -- Converts a VStringDiffs -> List ConcStringDiffs
  let strDiffToConcreteDiff newString diffs =
    case diffs of
      VStringDiffs d ->
        let aux offset d revAcc = case d of
          [] -> List.reverse revAcc
          ((StringUpdate start end replaced) :: tail) ->
             ConcStringUpdate start end (String.slice (start + offset) (start + replaced + offset) newString) :: revAcc |>
             aux (offset + replaced - (end - start)) tail
        in aux 0 d []
  in
  let affinity a b = if a == \"\" || b == \"\" then 10 else
     case extractFirstIn \"\\\\d$\" a of
       Just _ -> case extractFirstIn \"^\\\\d\" b of
         Just _ -> 8
         _ -> 5
       _ -> 5
  in
  let preferStringInsertionToLeft s1 inserted s2 = affinity s1 inserted > affinity inserted s2 in
  let offsetStr n list = case list of
      (StringUpdate start end replaced) :: tail -> StringUpdate (start + n) (end + n) replaced :: offsetStr n tail
      [] -> []
  in
  let mbConsStringUpdate start end replaced tail =
    if start == end && replaced == 0 then tail else (StringUpdate start end replaced) :: tail
  in
  let
    -- Returns all the possible ways of splitting the string differences at a particular index,
    -- at which the oldString used to be concatenated.
    -- Returns the new strings for left and for right.
    -- The old strings would simply be computed by (String.take n oldString) (String.drop n oldString)
    splitStringDiffsAt n offset oldString newString stringDiffs = case stringDiffs of
      [] -> [(String.take (n + offset) newString, [],
             String.drop (n + offset) newString, [])]
      ((StringUpdate start end replaced) as head) :: tail ->
        if end < n then
          splitStringDiffsAt n (offset + replaced - (end - start)) oldString newString tail
          |> List.map (\\(left, leftDiffs, right, rightDiffs) -> (left, head::leftDiffs, right, rightDiffs))
        else if n < start then
          [(String.take (n + offset) newString, [],
            String.drop (n + offset) newString, offsetStr (0 - n - offset) stringDiffs)]
        else if replaced == 0 then
          [(String.take (start + offset) newString, mbConsStringUpdate start n 0 [],
            String.drop (start + offset) newString, offsetStr (0 - n) <| mbConsStringUpdate n end 0 tail)]
        else
          let insertionToLeft =
           (String.take (start + offset + replaced) newString, mbConsStringUpdate start n replaced [],
            String.drop (start + offset + replaced) newString, offsetStr (0 - n) <| mbConsStringUpdate n end 0 tail)
          in
          let insertionToRight =
           (String.take (start + offset) newString, mbConsStringUpdate start n 0 [],
             String.drop (start + offset) newString, offsetStr (0 - n) <| mbConsStringUpdate n end replaced tail)
          in
          if preferStringInsertionToLeft
            (String.substring 0 start oldString)
            (String.substring (start + offset)
              (start + offset + replaced) newString)
            (String.drop end oldString)
          then [insertionToLeft, insertionToRight]
          else [insertionToRight, insertionToLeft]
  in
  let
    offsetList n list = case list of
      (i, d)::tail -> (i + n, d)::offsetList n tail
      [] -> []
  in
  -- Given a split index n (offset is zero at the beginning), split the newList that is being
  -- pushed back at the index n (n should be the original length of the left list being concatenated)
  -- Returns the new list to the left and its differences, and the new list on the right and its differences.
  let
    splitListDiffsAt n offset newList listDiffs = case listDiffs of
      [] ->
        let (left, right) = List.split (n + offset) newList in
        [(left, [], right, [])]
      (i, d) :: tail ->
        let newOffset = case d of
          ListElemInsert count -> offset + count
          ListElemDelete count -> offset - count
          ListElemUpdate _ -> offset
        in
        if i < n then
          if i + (offset - newOffset) > n then -- a deletion spanning until after the split point
            let (left, right) = List.split (n + offset) newList in
            [(left, (i, ListElemDelete (n - i))::[],
              right, offsetList (0 - n) <| (n, ListElemDelete (i + (offset - newOffset) - n))::tail)]
          else
          splitListDiffsAt n newOffset newList tail
          |> List.map (\\(left, leftDiffs, right, rightDiffs) ->
            (left, (i, d)::leftDiffs, right, rightDiffs))
        else if i > n || i == n && (case d of ListElemInsert _ -> False; _ -> True) then
          let (left, right) = List.split (n + offset) newList in
          [(left, [], right, offsetList (0 - n) listDiffs)]
        else -- i == n now, everything happens at the intersection.
          let insertionToLeft =
            let (left, right) = List.split (i + newOffset) newList in
            (left, (i, d)::[],
             right, tail)
          in
          let insertionToRight =
            let (left, right) = List.split (i + offset) newList in
            (left, [],
             right, offsetList (0 - i) <| (i, d)::tail)
          in
          [insertionToLeft, insertionToRight]
  in
  let --------------------------------------------------------------------------------
      -- Update.foldDiff

      -- type Results err ok = Result err (List ok)

      -- every onFunction should either return a Ok (List a) or an Err msg
      -- start    : a
      -- onUpdate : a -> {oldOutput: b, newOutput: b, index: Int, diffs: VDiffs} -> Results String a
      -- onInsert : a -> {newOutput: b, index: Int, diffs: VDiffs}  -> Results String a
      -- onRemove : a -> {oldOutput: b, index: Int, diffs! VDoffs}  -> Results String a
      -- onSkip   : a -> {count: Int, index: Int, oldOutputs: List b, newOutputs: List b}  -> Results String a
      -- onFinish : a -> Results String c
      -- onGather : c -> (InputWithDiff (d, Maybe VDiffs) | Input d)
      -- oldOutput: List b
      -- newOutput: List b
      -- diffs    : ListDiffs
      -- Returns  : Err String | Ok (Inputs (List d} | InputsWithDiffs (List (d, Maybe VDiffs)))
      foldDiff =
        let {List, Results} = LensLess in
        let {append, split, reverse} = List in
        \\{start, onSkip, onUpdate, onRemove, onInsert, onFinish, onGather} oldOutput newOutput diffs ->
        let listDiffs = case diffs of
          VListDiffs l -> l
          _ -> Debug.crash <| \"Expected VListDiffs, got \" + toString diffs
        in
        -- Returns either Err msg or Ok (list of values)
        --     fold: Int -> List b -> List b -> List (Int, ListElemDiff) -> a -> Results String c
        let fold  j      oldOutput  newOutput  listDiffs                    acc =
            let next i      oldOutput_ newOutput_ d newAcc =
              newAcc |> Results.andThen (\\accCase ->
                fold i oldOutput_ newOutput_ d accCase
              )
            in
            case listDiffs of
            [] ->
              let count = len newOutput in
              if count == 0 then
                onFinish acc
              else
               onSkip acc {count = count, index = j, oldOutputs = oldOutput, newOutputs = newOutput}
               |> next (j + count) [] [] listDiffs

            (i, diff)::dtail  ->
              if i > j then
                let count = i - j in
                let (previous, remainingOld) = split count oldOutput in
                let (current,  remainingNew) = split count newOutput in
                onSkip acc {count = count, index = j, oldOutputs = previous, newOutputs = current}
                |> next i remainingOld remainingNew listDiffs
              else case diff of
                ListElemUpdate d->
                  let previous::remainingOld = oldOutput in
                  let current::remainingNew = newOutput in
                  onUpdate acc {oldOutput = previous, index = i, output = current, newOutput = current, diffs = d}
                  |> next (i + 1) remainingOld remainingNew dtail
                ListElemInsert count ->
                  if count >= 1 then
                    let current::remainingNew = newOutput in
                    onInsert acc {newOutput = current, index = i}
                    |> next i oldOutput remainingNew (if count == 1 then dtail else (i, ListElemInsert (count - 1))::dtail)
                  else Debug.crash <| \"insertion count should be >= 1, got \" + toString count
                ListElemDelete count ->
                  if count >= 1 then
                    let dropped::remainingOld = oldOutput in
                    onRemove acc {oldOutput =dropped, index = i} |>
                    next (i + count) remainingOld newOutput (if count == 1 then dtail else (i + 1, ListElemDelete (count - 1))::dtail)
                  else Debug.crash <| \"deletion count should be >= 1, got \" ++ toString count
            _ -> Debug.crash <| \"Expected a list of diffs, got \" + toString diffs
        in
        case fold 0 oldOutput newOutput listDiffs start of
          Err msg -> Err msg
          Ok values -> -- values might be a pair of value and diffs. We use onGather to do the split.
            let aux revAccValues revAccDiffs values = case values of
              [] -> case revAccDiffs of
                Nothing -> Ok (Inputs (reverse revAccValues))
                Just revDiffs -> Ok (InputsWithDiffs (LensLess.List.zip (reverse revAccValues) (reverse revDiffs)))
              head::tail -> case onGather head of
                Ok (InputWithDiff (value, diff)) -> case revAccDiffs of
                  Nothing -> if len revAccValues > 0 then Err (\"Diffs not specified for all values, e.g.\" + toString value) else
                    aux [value] (Just [diff]) tail
                  Just revDiffs ->
                    aux (value :: revAccValues) (Just (diff::revDiffs)) tail
                Ok (Input value) -> case revAccDiffs of
                  Nothing -> aux (value :: revAccValues) revAccDiffs tail
                  Just revDiffs -> Err (\"Diffs not specified until \" + toString value)
            in aux [] Nothing values
  in
  let sizeFreeze l = {
         apply l = l
         update {outputNew=newL, diffs=d} =
           let lengthNotModified = case d of
             VListDiffs ds -> let aux ds = case ds of
               (_, ListElemDelete _)::tail -> False
               (_, ListElemInsert _)::tail -> False
               _::tail -> aux tail
               [] -> True
              in aux ds
             _ -> False
           in
           if lengthNotModified then Ok (InputsWithDiffs [(newL, Just d)]) else Ok (InputsWithDiffs [])
       }.apply l
  in
  let mbPairDiffs mbDiffsPair = case mbDiffsPair of
        (Nothing, Nothing) -> Nothing
        (Just d, Nothing) -> Just (VRecordDiffs {_1=d})
        (Just d, Just d2) -> Just (VRecordDiffs {_1=d, _2=d2})
        (Nothing, Just d2) -> Just (VRecordDiffs {_2=d2})
  in
  let bijection forward backward elem =
    {apply elem = forward elem
     update {outputNew} = Ok (Inputs [backward outputNew])
    }.apply elem
  in
  -- exports from Update module
  { freeze x = x
    expressionFreeze x = Debug.log \"Update.expressionFreeze is deprecated. Please use Update.freezeExcept instead\" x
    sizeFreeze = sizeFreeze
    conditionalFreeze cond = if cond then (\\x -> freeze x) else identity

    freezeWhen: Bool -> ((new_a, Diffs) -> String) -> a -> a
    freezeWhen notPermission lazyMessage x = {
      apply x = x
      update {outputNew, diffs} =
        if notPermission then
          Err (lazyMessage (outputNew, diffs))
        else
          Ok (InputsWithDiffs [(outputNew, Just diffs)])
    }.apply x

    -- Alternative to expressionFreeze. Provides a cleaner error message and enables to specify which variables to freeze on
    -- expressionFreeze F[x] <=> freezeExcept (always \"impossible\") x <| \\x -> F[x]
    freezeExcept: (Diffs -> String) -> a -> (a -> b) -> b
    freezeExcept lazyMessage arg fun = freezeWhenExcept True lazyMessage arg fun

    -- More precise version of freezeExcept that accepts a condition operator
    freezeWhenExcept: Bool -> (Diffs -> String) -> a -> (a -> b) -> b
    freezeWhenExcept notPermission lazyMessage arg fun = (freezeWhen notPermission (\\(newA, newDiffs) -> lazyMessage newDiffs) fun) arg

    foldDiff = foldDiff
    applyLens = applyLens
    lens l x = l.apply x
    lens1 = lens
    lens2 l x y = l.apply (x, y)
    lens3 l x y z = l.apply (x, y, z)
    lens4 l x y z w = l.apply (x, y, z, w)
    bijection = bijection
    vTupleDiffs_1 d = VRecordDiffs {_1=d}
    vTupleDiffs_2 d = VRecordDiffs {_2=d}
    vTupleDiffs_3 d = VRecordDiffs {_3=d}
    vTupleDiffs_4 d = VRecordDiffs {_4=d}
    vTupleDiffs_1_2 d1 d2 = VRecordDiffs {_1=d1, _2=d2}
    default apply uInput =
        __updateApp__ {uInput | fun = apply }
    -- Instead of returning a result of a lens, just returns the list or empty if there is an error.
    defaultAsListWithDiffs apply uInput =
      case default apply uInput of
        Err msg -> []
        Ok (InputsWithDiffs l) -> l
      -- \"f.apply x\" is a syntactic form for U-Lens, but eta-expanded anyway

    softFreeze = softFreeze
    splitStringDiffsAt = splitStringDiffsAt
    listDiffOp = listDiffOp
    updateApp  = __updateApp__
    diff: a -> b -> Result (Maybe VDiffs)
    diff = __diff__
    -- Instead of returning Ok (Maybe VDiffs) or error, raises the error if there is one or returns the Maybe VDiffs
    diffs: a -> b -> Maybe VDiffs
    diffs a b = case __diff__ a b of
        Err msg -> error msg
        Ok d -> d
    merge = __merge__
    listDiff = listDiffOp __diff__
    strDiffToConcreteDiff = strDiffToConcreteDiff
    splitListDiffsAt = splitListDiffsAt
    valuesWithDiffs = valuesWithDiffs
    resultValuesWithDiffs = resultValuesWithDiffs
    pairDiff2 = pairDiff2
    pairDiff3 = pairDiff3
    pairDiff4 = pairDiff4
    mbPairDiffs = mbPairDiffs
    Regex = {
        -- Performs replacements on a string with differences but also return those differences along with the old ones.
        replace: String -> (Match -> String) -> String -> Diffs -> (String, Diffs)
        replace regex replacement string diffs = updateReplace regex replacement string diffs
      }
    mapInserted fun modifiedStr diffs =
       let aux offset d strAcc = case d of
       [] -> strAcc
       ((ConcStringUpdate start end inserted) :: dtail) ->
         let left = String.take (start + offset) strAcc in
         let right =  String.dropLeft (start + offset + String.length inserted) strAcc in
         let newInserted = fun inserted in
         (if newInserted /= inserted then left + newInserted + right else strAcc) |>
         aux (offset + String.length newInserted - (end - start)) dtail
       in
       aux 0 (strDiffToConcreteDiff modifiedStr diffs) modifiedStr

    onUpdate callback = lens {
        apply = identity
        update {outputNew} = Ok (Inputs [callback outputNew])
    }

    debug msg x =
         { apply x = x, update { input, newOutput, oldOutput, diffs} =
           let _ = Debug.log (\"\"\"@msg:
oldOutput:@oldOutput
newOutput:@newOutput
diffs:@(if typeof oldOutput == \"string\" then strDiffToConcreteDiff newOutput diffs else diffs)\"\"\") \"end\" in
           Ok (InputsWithDiffs [(newOutput, Just diffs)])
         }.apply x
    debugstr msg x = Debug.log \"Update.debugstr is deprecated. Use Update.debug instead\" <| debug msg x

    debugFold msg callback value =
      callback (debug msg value)

    replaceInstead y x = {
       apply y = x
       update {outputNew,diffs} =
         Ok (InputsWithDiffs [(outputNew, Just diffs)])
     }.apply y
  }

evaluate program =
  case __evaluate__ [] program of
    Ok x -> x
    Err msg -> Debug.crash msg

--------------------------------------------------------------------------------
-- ListLenses --

-- append is defined here because it is used when we want x ++ y to be reversible.
-- Note that because this is not syntactically a lambda, the function is not recursive.
append =
  let {append,split} = LensLess.List in
  \\aas bs -> {
    apply [aas, bs] = append aas bs
    update {input = [aas, bs], outputNew, outputOld, diffs} =
      let asLength = len aas in
      Update.foldDiff {
        start = [[], [], [], [], len aas, len bs]
        onSkip [nas, nbs, diffas, diffbs, numA, numB] {count = n, newOutputs = outs} =
          if n <= numA then
            Ok [[nas ++ outs, nbs, diffas, diffbs, numA - n, numB]]
          else
            let (forA, forB) = split numA outs in
            Ok [[nas ++ forA, nbs ++ forB, diffas, diffbs, 0, numB - (n - numA)]]
        onUpdate [nas, nbs, diffas, diffbs, numA, numB] {newOutput = out, diffs, index} =
          Ok [if numA >= 1
           then [nas ++ [out],                                      nbs,
                 diffas ++ [(index, ListElemUpdate diffs)], diffbs,
                 numA - 1,                                          numB]
           else [nas,    nbs ++ [out],
                 diffas, diffbs ++ [(index - asLength, ListElemUpdate diffs)],
                 0,      numB - 1]]
        onRemove  [nas, nbs, diffas, diffbs, numA, numB] {oldOutput, index} =
          if 1 <= numA then
            Ok [[nas, nbs, diffas ++ [(index, ListElemDelete 1)], diffbs, numA - 1, numB]]
          else
            Ok [[nas, nbs, diffas, diffbs ++ [(index - asLength, ListElemDelete 1)], numA, numB - 1]]
        onInsert [nas, nbs, diffas, diffbs, numA, numB] {newOutput, index} =
          Ok (
            (if numA > 0 || len nbs == 0 then
              [[nas ++ [newOutput], nbs,
                diffas ++ [(index, ListElemInsert 1)], diffbs,
                numA, numB]]
            else []) ++
              (if len nbs > 0 || numA == 0 then
                [[nas,    nbs ++ [newOutput],
                  diffas, diffbs ++ [(index - asLength, ListElemInsert 1)],
                  numA, numB]]
              else [])
            )

        onFinish [nas, nbs, diffas, diffbs, _, _] = Ok [[[nas, nbs], (if len diffas == 0 then [] else
             [(0, ListElemUpdate (VListDiffs diffas))]) ++
                   (if len diffbs == 0 then [] else
             [(1, ListElemUpdate (VListDiffs diffbs))])]]
        onGather [[nas, nbs], diffs] = Ok (InputWithDiff ([nas, nbs],
          if len diffs == 0 then Nothing else Just (VListDiffs diffs)))
      } outputOld outputNew diffs
    }.apply [aas, bs]

--; Maps a function, f, over a list of values and returns the resulting list

--; Maps a function, f, over a list of values and returns the resulting list
--map a b: (a -> b) -> List a -> List b
map f l = {
  apply [f, l] = LensLess.List.map f l
  update {input=[f, l], oldOutput, outputNew, diffs} =
    Update.foldDiff {
      start =
        --Start: the collected functions and diffs,
        -- the collected inputs,
        -- The collected input diffs,
        -- the inputs yet to process.
        [[], [], [], l]


      onSkip [fs, insA, diffInsA, insB] {count} =
        --'outs' was the same in oldOutput and outputNew
        let (skipped, remaining) = LensLess.List.split count insB in
        Ok [[fs, insA ++ skipped, diffInsA, remaining]]

      onUpdate [fs, insA, diffInsA, insB] {oldOutput, newOutput, diffs, index} =
        let input::remaining = insB in
        case Update.updateApp {fun (f,x) = f x, input = (f, input), output = newOutput, oldOutput = oldOutput, diffs = diffs} of
          Err msg -> Err msg
          Ok (InputsWithDiffs vsds) -> Ok (
              LensLess.List.map (\\((newF, newA), d) ->
                let newFs = case d of
                  Just (VRecordDiffs {_1 = d}) -> (newF, Just d)::fs
                  _ -> fs
                in
                let newDiffsInsA = case d of
                  Just (VRecordDiffs {_2 = d}) -> diffInsA ++ [(index, ListElemUpdate d)]
                  _ -> diffInsA
                in
                [newFs, insA ++ [newA], newDiffsInsA, remaining]) vsds)

      onRemove [fs, insA, diffInsA, insB] {oldOutput, index} =
        let _::remaining = insB in
        Ok [[fs, insA, diffInsA ++ [(index, ListElemDelete 1)], remaining]]

      onInsert [fs, insA, diffInsA, insB] {newOutput, index} =
        let input =
          case insB of h::_ -> h; _ ->
          case LensLess.List.last insA of Just h -> h; Nothing -> Debug.crash \"Empty list for map, cannot insert\" in
        case Update.updateApp {fun (f,x) = f x, input = (f, input), output = newOutput} of
          Err msg -> Err msg
          Ok (InputsWithDiffs vsds) -> Ok (
              -- We disable the modification of f itself in the insertion (to prevent programmatic styling to change unexpectedly) newF::
              let aprioriResult = LensLess.List.concatMap (\\((newF, newA), diff) ->
                case diff of
                  Just (VRecordDiffs {_1}) -> []
                  _ -> [[fs, insA++[newA], diffInsA ++ [(index, ListElemInsert 1)], insB]]) <| vsds
              in -- If one of the result does not change f, that's good. Else, we take all the results.
              if aprioriResult == [] then -- Here we return all possibilities, ignoring changes to the function
                LensLess.List.concatMap (\\((newF, newA), _) ->
                   [[fs, insA++[newA], diffInsA ++ [(index, ListElemInsert 1)], insB]]) vsds
              else
                aprioriResult)

      onFinish [newFs, newIns, diffInsA, _] =
       --after we finish, we need to return the new function
       --as a merge of original functions with all other modifications
       -- and the collected new inputs
       Ok [[Update.merge f newFs, newIns, diffInsA]]

      onGather [(newF, fdiff), newIns, diffInsA] =
        let fdiffPart = case fdiff of
          Nothing -> []
          Just d -> [(0, ListElemUpdate d)]
        in
        let inPart = case diffInsA of
          [] -> []
          d -> [(1, ListElemUpdate (VListDiffs d))]
        in
        let finalDiff = case fdiffPart ++ inPart of
          [] -> Nothing
          d -> Just (VListDiffs d)
        in
        Ok (InputWithDiff ([newF, newIns], finalDiff))
    } oldOutput outputNew diffs
  }.apply [f, l]

mapWithDefault default f l = {
  apply [f, l] = LensLess.List.map f l
  update {input=[f, l], oldOutput, outputNew, diffs} =
    Update.foldDiff {
      start =
        --Start: the collected functions and diffs,
        -- the collected inputs,
        -- The collected input diffs,
        -- the inputs yet to process.
        [[], [], [], l]


      onSkip [fs, insA, diffInsA, insB] {count} =
        --'outs' was the same in oldOutput and outputNew
        let (skipped, remaining) = LensLess.List.split count insB in
        Ok [[fs, insA ++ skipped, diffInsA, remaining]]

      onUpdate [fs, insA, diffInsA, insB] {oldOutput, newOutput, diffs, index} =
        let input::remaining = insB in
        case Update.updateApp {fun (f,x) = f x, input = (f, input), output = newOutput, oldOutput = oldOutput, diffs = diffs} of
          Err msg -> Err msg
          Ok (InputsWithDiffs vsds) -> Ok (
              LensLess.List.map (\\((newF, newA), d) ->
                let newFs = case d of
                  Just (VRecordDiffs {_1 = d}) -> (newF, Just d)::fs
                  _ -> fs
                in
                let newDiffsInsA = case d of
                  Just (VRecordDiffs {_2 = d}) -> diffInsA ++ [(index, ListElemUpdate d)]
                  _ -> diffInsA
                in
                [newFs, insA ++ [newA], newDiffsInsA, remaining]) vsds)

      onRemove [fs, insA, diffInsA, insB] {oldOutput, index} =
        let _::remaining = insB in
        Ok [[fs, insA, diffInsA ++ [(index, ListElemDelete 1)], remaining]]

      onInsert [fs, insA, diffInsA, insB] {newOutput, index} =
        let input = default in
        case Update.updateApp {fun (f,x) = f x, input = (f, input), output = newOutput} of
          Err msg -> Err msg
          Ok (InputsWithDiffs vsds) -> Ok (
              -- We disable the modification of f itself in the insertion (to prevent programmatic styling to change unexpectedly) newF::
              let aprioriResult = LensLess.List.concatMap (\\((newF, newA), diff) ->
                case diff of
                  Just (VRecordDiffs {_1}) -> []
                  _ -> [[fs, insA++[newA], diffInsA ++ [(index, ListElemInsert 1)], insB]]) <| vsds
              in -- If one of the result does not change f, that's good. Else, we take all the results.
              if aprioriResult == [] then -- Here we return all possibilities, ignoring changes to the function
                LensLess.List.concatMap (\\((newF, newA), _) ->
                   [[fs, insA++[newA], diffInsA ++ [(index, ListElemInsert 1)], insB]]) vsds
              else
                aprioriResult)

      onFinish [newFs, newIns, diffInsA, _] =
       --after we finish, we need to return the new function
       --as a merge of original functions with all other modifications
       -- and the collected new inputs
       Ok [[Update.merge f newFs, newIns, diffInsA]]

      onGather [(newF, fdiff), newIns, diffInsA] =
        let fdiffPart = case fdiff of
          Nothing -> []
          Just d -> [(0, ListElemUpdate d)]
        in
        let inPart = case diffInsA of
          [] -> []
          d -> [(1, ListElemUpdate (VListDiffs d))]
        in
        let finalDiff = case fdiffPart ++ inPart of
          [] -> Nothing
          d -> Just (VListDiffs d)
        in
        Ok (InputWithDiff ([newF, newIns], finalDiff))
    } oldOutput outputNew diffs
  }.apply [f, l]

mapWithReverse reverse f l = {
  apply [f, l] = LensLess.List.map f l
  update {input=[f, l], oldOutput, outputNew, diffs} =
    Update.foldDiff {
      start =
        --Start: the collected functions and diffs,
        -- the collected inputs,
        -- The collected input diffs,
        -- the inputs yet to process.
        [[], [], [], l]


      onSkip [fs, insA, diffInsA, insB] {count} =
        --'outs' was the same in oldOutput and outputNew
        let (skipped, remaining) = LensLess.List.split count insB in
        Ok [[fs, insA ++ skipped, diffInsA, remaining]]

      onUpdate [fs, insA, diffInsA, insB] {oldOutput, newOutput, diffs, index} =
        let input::remaining = insB in
        case Update.updateApp {fun (f,x) = f x, input = (f, input), output = newOutput, oldOutput = oldOutput, diffs = diffs} of
          Err msg -> Err msg
          Ok (InputsWithDiffs vsds) -> Ok (
              LensLess.List.map (\\((newF, newA), d) ->
                let newFs = case d of
                  Just (VRecordDiffs {_1 = d}) -> (newF, Just d)::fs
                  _ -> fs
                in
                let newDiffsInsA = case d of
                  Just (VRecordDiffs {_2 = d}) -> diffInsA ++ [(index, ListElemUpdate d)]
                  _ -> diffInsA
                in
                [newFs, insA ++ [newA], newDiffsInsA, remaining]) vsds)

      onRemove [fs, insA, diffInsA, insB] {oldOutput, index} =
        let _::remaining = insB in
        Ok [[fs, insA, diffInsA ++ [(index, ListElemDelete 1)], remaining]]

      onInsert [fs, insA, diffInsA, insB] {newOutput, index} =
        let newA  = reverse newOutput in
        Ok [[fs, insA ++ [newA], diffInsA ++ [(index, ListElemInsert 1)], insB]]

      onFinish [newFs, newIns, diffInsA, _] =
       --after we finish, we need to return the new function
       --as a merge of original functions with all other modifications
       -- and the collected new inputs
       Ok [[Update.merge f newFs, newIns, diffInsA]]

      onGather [(newF, fdiff), newIns, diffInsA] =
        let fdiffPart = case fdiff of
          Nothing -> []
          Just d -> [(0, ListElemUpdate d)]
        in
        let inPart = case diffInsA of
          [] -> []
          d -> [(1, ListElemUpdate (VListDiffs d))]
        in
        let finalDiff = case fdiffPart ++ inPart of
          [] -> Nothing
          d -> Just (VListDiffs d)
        in
        Ok (InputWithDiff ([newF, newIns], finalDiff))
    } oldOutput outputNew diffs
  }.apply [f, l]

zipWithIndex xs =
  { apply x = zip (range 0 (len xs - 1)) xs
    update {output} = Ok (Inputs [map (\\(i, x) -> x) output])}.apply xs

indexedMap f l =
  map (\\(i, x) -> f i x) (zipWithIndex l)

-- TODO: Remove list lenses (lenses should be part of List)
ListLenses =
  { map = map
    append = append
    zipWithIndex = zipWithIndex
    indexedMap = indexedMap
  }

--------------------------------------------------------------------------------
-- TODO (re-)organize this section into modules
-- HEREHEREHERE

--; Combines two lists with a given function, extra elements are dropped
--map2: (forall (a b c) (-> (-> a b c) (List a) (List b) (List c)))
map2 f xs ys =
  case [xs, ys] of
    [x::xs1, y::ys1] -> f x y :: map2 f xs1 ys1
    _                -> []

--; Combines three lists with a given function, extra elements are dropped
--map3: (forall (a b c d) (-> (-> a b c d) (List a) (List b) (List c) (List d)))
map3 f xs ys zs =
  case [xs, ys, zs] of
    [x::xs1, y::ys1, z::zs1] -> f x y z :: map3 f xs1 ys1 zs1
    _                        -> []

--; Combines four lists with a given function, extra elements are dropped
--map4: (forall (a b c d e) (-> (-> a b c d e) (List a) (List b) (List c) (List d) (List e)))
map4 f ws xs ys zs =
  case [ws, xs, ys, zs]of
    [w::ws1, x::xs1, y::ys1, z::zs1] -> f w x y z :: map4 f ws1 xs1 ys1 zs1
    _                                -> []

--; Takes a function, an accumulator, and a list as input and reduces using the function from the left
--foldl: (forall (a b) (-> (-> a b b) b (List a) b))
foldl f acc xs =
  case xs of [] -> acc; x::xs1 -> foldl f (f x acc) xs1

--; Takes a function, an accumulator, and a list as input and reduces using the function from the right
--foldr: (forall (a b) (-> (-> a b b) b (List a) b))
foldr f acc xs =
  case xs of []-> acc; x::xs1 -> f x (foldr f acc xs1)

--; Given two lists, append the second list to the end of the first
--append: (forall a (-> (List a) (List a) (List a)))
-- append xs ys =
--   case xs of [] -> ys; x::xs1 -> x :: append xs1 ys

--; concatenate a list of lists into a single list
--concat: (forall a (-> (List (List a)) (List a)))
concat xss = foldr append [] xss
-- TODO eta-reduced version:
-- (def concat (foldr append []))

--; Map a given function over a list and concatenate the resulting list of lists
--concatMap: (forall (a b) (-> (-> a (List b)) (List a) (List b)))
concatMap f xs = concat (map f xs)

--; Takes two lists and returns a list that is their cartesian product
--cartProd: (forall (a b) (-> (List a) (List b) (List [a b])))
cartProd xs ys =
  concatMap (\\x -> map (\\y -> [x, y]) ys) xs

--; Takes elements at the same position from two input lists and returns a list of pairs of these elements
--zip: (forall (a b) (-> (List a) (List b) (List [a b])))
-- zip xs ys = map2 (\\x y -> [x, y]) xs ys
-- TODO eta-reduced version:
-- (def zip (map2 (\\(x y) [x y])))

--; The empty list
--; (typ nil (forall a (List a)))
--nil: []
-- nil = []

--; attaches an element to the front of a list
--cons: (forall a (-> a (List a) (List a)))
-- cons x xs = x :: xs

--; attaches an element to the end of a list
--snoc: (forall a (-> a (List a) (List a)))
snoc x ys = append ys [x]

--; Returns the first element of a given list
--hd: (forall a (-> (List a) a))
--tl: (forall a (-> (List a) (List a)))
hd (x::xs) = x
tl (x::xs) = xs

--; Returns the last element of a given list
--last: (forall a (-> (List a) a))
last xs =
  case xs of
    [x]   -> x
    _::xs -> last xs

--; Given a list, reverse its order
--reverse: (forall a (-> (List a) (List a)))
reverse xs = foldl cons nil xs
-- TODO eta-reduced version:
-- (def reverse (foldl cons nil))

adjacentPairs xs = zipOld xs (tl xs)

--; Given two numbers, creates the list between them (inclusive)
--range: (-> Num Num (List Num))
-- range i j =
--   if i < j + 1
--     then cons i (range (i + 1) j)
--     else nil

--; Given a number, create the list of 0 to that number inclusive (number must be > 0)
--list0N: (-> Num (List Num))
list0N n = range 0 n

--; Given a number, create the list of 1 to that number inclusive
--list1N: (-> Num (List Num))
list1N n = range 1 n

--zeroTo: (-> Num (List Num))
zeroTo n = range 0 (n - 1)

--; Given a number n and some value x, return a list with x repeated n times
--repeat: (forall a (-> Num a (List a)))
repeat n x = map (always x) (range 1 n)

--; Given two lists, return a single list that alternates between their values (first element is from first list)
--intermingle: (forall a (-> (List a) (List a) (List a)))
intermingle xs ys =
  case [xs, ys] of
    [x::xs1, y::ys1] -> cons x (cons y (intermingle xs1 ys1))
    [[], []]         -> nil
    _                -> append xs ys

intersperse sep xs =
  case xs of
    []    -> xs
    x::xs -> reverse (foldl (\\y acc -> y :: sep :: acc) [x] xs)

--mapi: (forall (a b) (-> (-> [Num a] b) (List a) (List b)))
mapi f xs = map f (zipWithIndex xs)

--nth: (forall a (-> (List a) Num (union Null a)))
nth xs n =
  if n < 0 then error \"index out of range. Use List.nthMaybe instead of nth to avoid this\"
  else
    case [n, xs] of
      [_, []]     -> error \"index out of range. Use List.nthMaybe instead of nth to avoid this\"
      [0, x::xs1] -> x
      [_, x::xs1] -> nth xs1 (n - 1)

-- (defrec nth (\\(xs n)
--   (if (< n 0)   \"ERROR: nth\"
--     (case xs
--       ([]       \"ERROR: nth\")
--       ([x|xs1]  (if (= n 0) x (nth xs1 (- n 1))))))))

-- TODO change typ/def
-- (typ take (forall a (-> (List a) Num (union Null (List a)))))
--take: (forall a (-> (List a) Num (List (union Null a))))
take xs n =
  if n == 0 then []
  else
    case xs of
      []     -> [null]
      x::xs1 -> x :: take xs1 (n - 1)

-- (def take
--   (let take_ (\\(n xs)
--     (case [n xs]
--       ([0 _]       [])
--       ([_ []]      [])
--       ([_ [x|xs1]] [x | (take_ (- n 1) xs1)])))
--   (compose take_ (max 0))))
--drop: (forall a (-> (List a) Num (union Null (List a))))
drop xs n =
  if le n 0 then xs
  else
    case xs of
      []     -> null
      x::xs1 -> drop xs1 (n - 1)

--; Drop n elements from the end of a list
-- dropEnd: (forall a (-> (List a) Num (union Null (List a))))
-- dropEnd xs n =
--   let tryDrop = drop (reverse xs) n in
--     Err \"typecase not yet implemented for Elm syntax\"

--elem: (forall a (-> a (List a) Bool))
elem x ys =
  case ys of
    []     -> False
    y::ys1 -> or (x == y) (elem x ys1)

sortBy f xs =
  let ins x ys =   -- insert is a keyword...
    case ys of
      []    -> [x]
      y::ys -> if f x y then x :: y :: ys else y :: ins x ys
  in
  foldl ins [] xs

sortAscending = sortBy lt
sortDescending = sortBy gt


--; multiply two numbers and return the result
--mult: (-> Num Num Num)
mult m n =
  if m < 1 then 0 else n + mult (m + -1) n

--; Given two numbers, subtract the second from the first
--minus: (-> Num Num Num)
minus x y = x + mult y -1

--; Given two numbers, divide the first by the second
--div: (-> Num Num Num)
div m n =
  if m < n then 0 else
  if n < 2 then m else 1 + div (minus m n) n

--; Given a number, returns the negative of that number
--neg: (-> Num Num)
neg x = 0 - x

--; Sign function; -1, 0, or 1 based on sign of given number
--sgn: (-> Num Num)
sgn x = if 0 == x then 0 else x / abs x

--some: (forall a (-> (-> a Bool) (List a) Bool))
some p xs =
  case xs of
    []     -> False
    x::xs1 -> or (p x) (some p xs1)

--all: (forall a (-> (-> a Bool) (List a) Bool))
all p xs =
  case xs of
    []     -> True
    x::xs1 -> and (p x) (all p xs1)

--between: (-> Num Num Num Bool)
between i j n = n == clamp i j n

--plus: (-> Num Num Num)
plus x y = x + y

--minimum: (-> (List Num) Num)
minimum (hd::tl) = foldl min hd tl

--maximum: (-> (List Num) Num)
maximum (hd::tl) = foldl max hd tl

--average: (-> (List Num) Num)
average nums =
  let sum = foldl plus 0 nums in
  let n = len nums in sum / n

--; Combine a list of strings with a given separator
--; Ex. joinStrings \", \" [\"hello\" \"world\"] = \"hello, world\"
--joinStrings: (-> String (List String) String)
joinStrings sep ss =
  foldr (\\str acc -> if acc == \"\" then str else str + sep + acc) \"\" ss

--; Concatenate a list of strings and return the resulting string
--concatStrings: (-> (List String) String)
concatStrings = joinStrings \"\"

--; Concatenates a list of strings, interspersing a single space in between each string
--spaces: (-> (List String) String)
spaces = joinStrings \" \"

--; First two arguments are appended at the front and then end of the third argument correspondingly
--; Ex. delimit \"+\" \"+\" \"plus\" = \"+plus+\"
--delimit: (-> String String String String)
delimit a b s = concatStrings [a, s, b]

--; delimit a string with parentheses
--parens: (-> String String)
parens = delimit \"(\" \")\"

--;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

-- (def columnsToRows (\\columns
--   (let numColumns (len columns)
--   (let numRows ; maxColumnSize
--     (if (= numColumns 0) 0 (maximum (map len columns)))
--   (foldr
--     (\\(col rows)
--       (let paddedCol (append col (repeat (- numRows (len col)) \".\"))
--       (map
--         (\\[datum row] [ datum | row ])
--         (zipOld paddedCol rows))))
--     (repeat numRows [])
--     columns)
-- ))))
--
-- (def addColToRows (\\(col rows)
--   (let width (maximum (map len rows))
--   (let foo (\\(col rows)
--     (case [col rows]
--       ([ []     []     ] [                                          ])
--       ([ [x|xs] [r|rs] ] [ (snoc x r)                 | (foo xs rs) ])
--       ([ []     [r|rs] ] [ (snoc \"\" r)                | (foo [] rs) ])
--       ([ [x|xs] []     ] [ (snoc x (repeat width \"\")) | (foo xs []) ])
--     ))
--   (foo col rows)))))

-- (def border [\"border\" \"1px solid black\"])
-- (def padding [\"padding\" \"5px\"])
-- (def center [\"align\" \"center\"])
-- (def style (\\list [\"style\" list]))
-- (def onlyStyle (\\list [(style list)]))
--
-- (def td (\\text
--   [\"td\" (onlyStyle [border padding])
--         [[\"TEXT\" text]]]))
--
-- (def th (\\text
--   [\"th\" (onlyStyle [border padding center])
--         [[\"TEXT\" text]]]))
--
-- (def tr (\\children
--   [\"tr\" (onlyStyle [border])
--         children]))
--
-- ; TODO div name is already taken...
--
-- (def div_ (\\children [\"div\" [] children]))
-- (def h1 (\\text [\"h1\" [] [[\"TEXT\" text]]]))
-- (def h2 (\\text [\"h2\" [] [[\"TEXT\" text]]]))
-- (def h3 (\\text [\"h3\" [] [[\"TEXT\" text]]]))
--
-- (def table (\\children
--   [\"table\" (onlyStyle [border padding]) children]))

-- (def table (\\children
--   (let [x y] [100 100]
--   [\"table\" (onlyStyle [border padding
--                       [\"position\" \"relative\"]
--                       [\"left\" (toString x)]
--                       [\"top\" (toString y)]]) children])))

-- (def tableOfData (\\data
--   (let letters (explode \" ABCDEFGHIJKLMNOPQRSTUVWXYZ\")
--   (let data (mapi (\\[i row] [(+ i 1) | row]) data)
--   (let tableWidth (maximum (map len data))
--   (let headers
--     (tr (map (\\letter (th letter)) (take letters tableWidth)))
--   (let rows
--     (map (\\row (tr (map (\\col (td (toString col))) row))) data)
--   (table
--     [ headers | rows ]
-- ))))))))



-- absolutePositionStyles x y = let _ = [x, y] : Point in
--   [ [\"position\", \"absolute\"]
--   , [\"left\", toString x + \"px\"]
--   , [\"top\", toString y + \"px\"]
--   ]

--------------------------------------------------------------------------------
-- Regex --

Regex =
  let split regex = Update.lens {
    apply s =
      case extractFirstIn (\"^([\\\\s\\\\S]*?)(\" + regex + \")([\\\\s\\\\S]*)$\") s of
        Just [before, removed, after] ->
          if before == \"\" && removed == \"\" then
            case extractFirstIn \"^([\\\\s\\\\S])([\\\\s\\\\S]*)$\" after of
              Nothing -> [after]
              Just [x, remaining] ->
                let head::tail = split regex remaining in
                (x + head) :: tail

          else before :: split regex after
        _ -> [s]
    update {input=s, outputNew} =
      case extractFirstIn (\"^([\\\\s\\\\S]*?)(\" + regex + \")([\\\\s\\\\S]*)$\") s of
        Just [before, removed, after] ->
          Ok (Inputs [outputNew |> LensLess.String.join removed])
        _ -> case outputNew of
          [x] -> Ok (Inputs [x])
          _ -> Err \"Regex.split resulted in 1 element or less, cannot update with more elements. Use Regex.splitReverse if you know the join string\"
    }
  in
  let splitReverse joinStr regex = Update.lens {
    apply s = split regex s
    update {input=s, outputNew} =
      Ok (Inputs [outputNew |> LensLess.String.join joinStr])
    }
  in
  let find regex s =
    case extractFirstIn (\"(\" + regex + \")([\\\\s\\\\S]*)\") s of
      Nothing -> []
      Just matchremaining ->
        case LensLess.List.split (len matchremaining - 1) matchremaining of
          (init, [last]) ->
            init::find regex last
  in
  {
  replace regex replacement string = replaceAllIn regex replacement string
  replaceFirst regex replacement string = replaceFirstIn regex replacement string
  extract regex string = extractFirstIn regex string
  matchIn r x = case extractFirstIn r x of
    Nothing -> False
    _ -> True
  split = split
  splitReverse = splitReverse
  find = find
  escape = replace \"\"\"\\||\\\\|\\{|\\}|\\[|\\]|\\$|\\.|\\?|\\+|\\(|\\)\"\"\" (\\m -> \"\\\\\" + m.match)
}

--------------------------------------------------------------------------------
-- Dict --

-- Interface
dictLike = {
  apply this d x = case this.get x d of
    Just x -> x
    _ -> Debug.crash (\"Expected element \" + toString x + \" in dict, got nothing\")
  member this key list = this.get key list /= Nothing
  contains this = this.member
  delete this = this.remove
  update this k f d = case f <| this.get k d of
    Nothing -> this.delete k d
    Just v -> this.insert k v d
}

-- Dictionary implementation
Dict = { dictLike |
  empty = __DictEmpty__
  fromList l = __DictFromList__ l
  get x d = __DictGet__ x d
  remove x d = __DictRemove__ x d
  insert k v d = __DictInsert__ k v d
}

-- List of pairs implementation
listDict = { dictLike |
  empty = []
  fromList = identity
  get key = Update.lens {
    apply list = case list of
      [] -> Nothing
      (k, v) :: tail -> if k == key then Just v else get key tail
    update = case of
      {input, outputOld = Nothing, outputNew = Just v} ->
        Ok (Inputs [insert key v input])
      {input, outputOld = Just _, outputNew = Nothing} ->
        Ok (Inputs [remove key input])
      uInput -> Update.default apply uInput
    }
  remove key list = case list of
    [] -> []
    ((k, v) as head) :: tail -> if k == key then tail else head :: remove key tail
  insert key value list = case list of
    [] -> [(key, value)]
    ((k, v) as head) :: tail -> if k == key then (k, value)::tail else head :: insert key value tail

  -- Enables the deletion of the key-value in reverse
  insert2 ([(key, value)] as keyValue) list = case list of
    [] -> keyValue
    _ ->
      let (([(k, v)] as head), tail)  = List.split 1 list in
      if k == key then keyValue ++ tail else head ++ insert2 keyValue tail
}


-- List of 2-element list implementation
attrDict = { dictLike |
  empty = []
  fromList = identity
  get key = Update.lens {
    apply list = case list of
      [] -> Nothing
      [k, v] :: tail -> if k == key then Just v else get key tail
    update = case of
      {input, outputOld = Nothing, outputNew = Just v} ->
        Ok (Inputs [insert key v input])
      {input, outputOld = Just _, outputNew = Nothing} ->
        Ok (Inputs [remove key input])
      uInput -> Update.default apply uInput
    }
  remove key list = case list of
    [] -> []
    ([k, v] as head) :: tail -> if k == key then tail else head :: remove key tail
  insert key value list = case list of
    [] -> [[key, value]]
    ([k, v] as head) :: tail -> if k == key then [k, value]::tail else head :: insert key value tail
}

Set = {
  empty = __DictEmpty__
  fromList l = __DictFromList__ (zip l l)
  insert k d = __DictInsert__ k k d
  remove x d = __Dict.remove__ x d
  member x d = case __DictGet__ x d of
      Just _ -> True
      _ -> False
}


--------------------------------------------------------------------------------
-- List (all operations should be lenses) --

List = {
  reverse =
    let r = LensLess.List.reverse in
    -- TODO: reverse the differences as well !
    \\l -> { apply l = r l, update {output}= Ok (Inputs [r output])}.apply l

  simpleMap = LensLess.List.map

  {-
  filterMap f l = case l of
    [] -> []
    _ ->
      let ([head] as headList, tail) = split 1 l in
      case f head of
        Nothing -> filterMap f tail
        Just newHead -> -- TODO: Correct for deletion, but not insertions!
          (List.map (always newHead) headList) ++ filterMap f tail
  -}
  filterMap f l = map f l |> filter (\\x -> x /= Nothing) |> map (\\(Just x) -> x)

  length x = len x

  nth = nth
  nthMaybe n list = case list of
    head :: tail -> if n == 0 then Just head else nthMaybe (n - 1) tail
    [] -> Nothing

  (mapi) f xs = map f (zipWithIndex xs)
  (mapiWithDefault) default f xs = mapWithDefault (0, default) f (zipWithIndex xs)
  (mapiWithReverse) reverse f xs = mapWithReverse (\\x -> (0, reverse x)) f (zipWithIndex xs)

  indexedMap f xs =
    mapi (\\(i,x) -> f i x) xs

  indexedMapWithDefault default f xs =
    mapiWithDefault default (\\(i,x) -> f i x) xs

  indexedMapWithReverse reverse f xs =
    mapiWithReverse reverse (\\(i, x) -> f i x) xs

  concatMap f l = case l of
    [] -> []
    head :: tail -> f head ++ concatMap f tail

  indexedConcatMap f l =
    let aux i l = case l of
      [] -> []
      head :: tail -> f i head ++ aux (i + 1) tail
    in aux 0 l

  cartesianProductWith f xs ys =
    concatMap (\\x -> map (\\y -> f x y) ys) xs

  unzip xys =
    case xys of
      []          -> ([], [])
      (x,y)::rest -> let (xs,ys) = unzip rest in
                     (x::xs, y::ys)

  split n l = Update.lens {
      apply l = LensLess.List.split n l
      update {output = (l1, l2), diffs} =
        let finalDiffs = case diffs of
          VRecordDiffs {_1 = VListDiffs l1, _2 = VListDiffs l2} ->
            Just (VListDiffs (l1 ++ simpleMap (\\(i, d) -> (i + n, d)) l2))
          VRecordDiffs {_1 = VListDiffs l1} ->  Just (VListDiffs l1)
          VRecordDiffs {_2 = VListDiffs l2} -> Just (VListDiffs (simpleMap (\\(i, d) -> (i + n, d)) l2))
          _ -> Nothing
        in
        Ok (InputsWithDiffs [(l1 ++ l2, finalDiffs)])
    } l

  -- This filter lens supports insertions and deletions in output
  filter f l =
        case l of
          [] -> l
          _ ->
             let ([head] as head1, tail) = split 1 l in
             let cond = f head in
             if cond then head1 ++ filter f tail
             else filter f tail

  -- In this version of foldl, the function f accepts a 1-element list instead of just the element.
  -- This enable programmers to insert or delete values from the accumulator.
  foldl2 f b l =
    let aux b l = case l of
      [] -> b
      _ ->
        let (h1, t1) = split 1 l in
        aux (f h1 b) t1
    in aux b l

  -- Pushes the elements to the head of the reverse accumulator revAcc in O(elements) time
  reverseInsert elements revAcc =
    case elements of
      [] -> revAcc
      head::tail -> reverseInsert tail (head::revAcc)

  sum l = foldl (\\x y -> x + y) 0 l
  product l = foldl (\\x y -> x * y) 1 l

  maximum l = case l of
    [] -> Nothing
    h :: t -> Just (foldl (\\x y -> if x > y then x else y) h t)

  minimum l = case l of
    [] -> Nothing
    h :: t -> Just (foldl (\\x y -> if x < y then x else y) h t)

  range min max = if min > max then [] else min :: range (min + 1) max

  mapFirstSuccess f l = case l of
    [] -> Nothing
    head :: tail -> case f head of
      Nothing -> mapFirstSuccess f tail
      x -> x

  contains n =
    let aux l = case l of
      [] -> False
      head::tail -> if head == n then True else aux tail
    in aux

  -- Contrary to concatMap, concatMap_ supports insertion and deletions of elements.
  -- It requires a function to indicate what to do when inserting in empty lists.
  -- f takes a 1-element list but can push back many elements or remove them
  concatMap_ insert_in_empty f l =
        case l of -- Insertion to an emptylist.
          [] -> { apply l = []
                  update {input=l, outputNew=lp} = Ok (Inputs [insert_in_empty lp])
                }.apply l
          _ -> Update.applyLens { -- Back-propagating a ghost boolean indicating if the next element in the computation had its first element deleted.
          apply (x, y) = x
          update {output, diffs} = Ok (InputsWithDiffs [
            ((output, True),
             Just (VRecordDiffs { _1 = diffs, _2 = VConstDiffs}))])
        } <| foldl2 (\\headList (oldAcc, dummyBool) ->
          let lengthAcc = length oldAcc in
          {apply (f, oldAcc, headList, dummyBool) = (oldAcc ++ f headList, dummyBool)
           update {input=(f, oldAcc, headList, _) as input, outputNew=(newAccFHeadList, prevDeletedOrLast), diffs=ds} =
             let handleDiffs = case ds of
                VRecordDiffs {_1=VListDiffs diffs} -> \\continuation -> continuation diffs
                _ -> \\continuation -> Ok (InputsWithDiffs [(input, Nothing)])
             in handleDiffs <| \\diffs ->
             Update.splitListDiffsAt lengthAcc 0 newAccFHeadList diffs
             |> concatMap (\\(newAcc, newAccDiffs, newFHeadList, newFHeadListDiffs) ->
               let finalAccDiffs = if newAccDiffs == [] then Nothing else Just (VListDiffs newAccDiffs) in
               let finalAccDeletedRight =
                 let aux diffs = case diffs of
                   [] -> False
                   (i, d)::tail -> case d of
                     ListElemDelete count -> if count + i == lengthAcc then True else
                       aux tail
                     _ -> aux tail
                 in aux newAccDiffs
               in
               let firstElementDeleted = case newFHeadListDiffs of
                 (0, ListElemDelete x) :: tail -> True
                 _ -> False
               in
               let deleteBoolUpdate = if firstElementDeleted then Just VConstDiffs else Nothing in
               let surroundingElementsDeleted = finalAccDeletedRight && prevDeletedOrLast in
               let headListDeletable = newFHeadList == []  && (firstElementDeleted || surroundingElementsDeleted) in
               let atLeastOneSurroundingElementDeleted = finalAccDeletedRight || prevDeletedOrLast in
               (if atLeastOneSurroundingElementDeleted && headListDeletable then [] else
               case newFHeadListDiffs of
                 [] -> [Ok ((f, newAcc, headList, firstElementDeleted),
                            Update.pairDiff4 Nothing finalAccDiffs Nothing deleteBoolUpdate)]
                 _ ->
               case Update.updateApp{fun (f, x) = f x, input=(f, headList), output=newFHeadList, diffs=VListDiffs newFHeadListDiffs} of
                 Ok (InputsWithDiffs fAndNewHeadListsWithDiffs) ->
                    LensLess.List.map (\\((newF, newHeadList), diff) ->
                      case diff of
                        Nothing ->  Ok ((f, newAcc, headList, firstElementDeleted),
                          Update.pairDiff4 Nothing finalAccDiffs Nothing deleteBoolUpdate)
                        Just (VRecordDiffs d) ->
                          let newFDiff = case d of {_1} -> Just _1; _ -> Nothing in
                          let newHeadListDiffs = case d of {_2} -> Just _2; _ -> Nothing in
                          Ok ((newF, newAcc, newHeadList, firstElementDeleted), Update.pairDiff4 newFDiff finalAccDiffs newHeadListDiffs deleteBoolUpdate)
                    ) fAndNewHeadListsWithDiffs
                 Err msg -> [Err msg]
               ) ++ (
                 if headListDeletable then
                   [Ok ((f, newAcc, [], True),
                        Update.pairDiff4 Nothing finalAccDiffs (Just (VListDiffs [(0, ListElemDelete 1)])) (Just VConstDiffs))]
                 else
                   [])
              ) |> Update.resultValuesWithDiffs
          }.apply (f, oldAcc, headList, dummyBool)) (Update.freeze [], Update.softFreeze False) l

  indices l = range 0 (length l - 1)
  isEmpty l = l == []
  head = Update.lens {
    apply l = case l of
      h :: l -> Just h
      _ -> Nothing
    update {input, outputNew, diffs} = case (input, outputNew, diffs) of
      (h :: tail, Nothing, _) ->
        Ok (InputsWithDiffs [(tail, Just (VListDiffs [(0, ListElemDelete 1)]))])
      (h :: tail, Just newH, VRecordDiffs {args= VRecordDiffs {_1=d}}) ->
        Ok (InputsWithDiffs [(newH :: tail, Just (VListDiffs [(0, ListElemUpdate d)]))])
      ([], Nothing, _) -> Ok (Inputs [input])
      ([], Just newH, _) ->
        Ok (InputsWithDiffs [([newH], Just (VListDiffs [(0, ListElemInsert 1)]))])
      (_, _, _) -> Err (\"Inconsistent diffs in List.head\" ++ toString diffs)
    }

  tail =  Update.lens {
    apply l = case l of
      h :: l -> Just l
      _ -> Nothing
    update {input, outputNew, diffs} = case (input, outputNew, diffs) of
      (h :: tail, Nothing, _) ->
        Ok (InputsWithDiffs [([], Just (VListDiffs [(0, ListElemDelete (List.length input))]))])
      (h :: tail, Just newTail, VRecordDiffs {args= VRecordDiffs {_1=VListDiffs tailDiffs}}) ->
        Ok (InputsWithDiffs [(h :: newTail, Just (VListDiffs (List.map (\\(i, d) -> (i+1, d)) tailDiffs)))])
      ([], Nothing, _) -> Ok (Inputs [input])
      ([], Just newTail, _) ->
        Err \"I don't know how to insert a new tail where there was none originally.\"
      (_, _, _) -> Err (\"Inconsistent diffs in List.head\" ++ toString diffs)
    }

  take n l =
    let (taken, remaining) = split n l in
    taken

  drop n l =
    let (taken, remaining) = split n l in
    remaining

  singleton elem = [elem]

  repeat n a =
    {apply (n, a) =
      let aux i = if i == 0 then [] else a :: aux (i - 1)
      in aux n
     update {input, outputNew, diffs = VListDiffs ds} =
       let nNew = length outputNew in
       let nNewDiffs = if nNew == n then Nothing else Just (VConstDiffs) in
       let mergeEnabled =
         let aux i diffs outputNew = case diffs of
           [] -> []
           (j, diff)::tailDiffs ->
             if i == j then
               case diff of
                 ListElemUpdate d ->
                   case outputNew of
                     o :: t -> (o, Just d) :: aux (i + 1) tailDiffs t
                 ListElemInsert count ->
                   let (inserted, remOutputNew) = split count outputNew in
                   concatMap (\\newA ->
                     case __diff__ a newA of
                       Err msg -> []
                       Ok mbDiff -> [(newA, mbDiff)]) inserted ++ aux i tailDiffs remOutputNew
                 ListElemDelete count ->
                   aux (i + count) tailDiffs outputNew
             else
               let (_, remOutputNew) = split (j - i) outputNew in
               aux j diffs remOutputNew
           k::tailDiffs -> aux i tailDiffs outputNew
         in aux 0 ds outputNew
       in
       let (nA, nDiffsA) = __merge__ a mergeEnabled in
       Ok (InputsWithDiffs [((nNew, nA), Update.mbPairDiffs (nNewDiffs, nDiffsA))])
    }.apply (n, a)

  insertAt index newElem elements =
    let (before, after) = split index elements in
    before ++ [newElem] ++ after

  removeAt index elements =
    let (before, after) = split index elements in
    case after of
      [] -> Nothing
      elem::afterTail ->
        Just (elem, before ++ afterTail)

  -- Given two converters cA and cB, a list and a target, finds an element
  -- of the list that, converted using cA, equals target. Returns cB of this element.
  -- In the reverse direction, it finds the cB of the new output and returns the cA.
  findByAReturnB: (a -> b) -> (a -> c) -> b -> List a -> Maybe c
  findByAReturnB =
        Update.lens4 {
          apply (cA, cB, target, l) =
            let aux l = case l of
              [] -> Nothing
              h :: t -> if cA h == target then Just (cB h) else aux t
            in aux l
          update {input=(converterA, converterB, target, list) as input, outputNew} as uInput =
            case outputNew of
              Nothing -> Ok (InputsWithDiffs [(input, Nothing)])
              Just outputNewTarget ->
            case apply (converterB, converterA, outputNewTarget, list) of
              Just x -> -- No rename here.
                Ok (InputsWithDiffs [
                  ((converterA, converterB, x, list),
                    Update.diffs target x |> LensLess.Maybe.map Update.vTupleDiffs_3)])
              Nothing ->
                Update.default apply uInput
            -- To ways to update: Either find the new B in the list and return A
            -- Or just regular update (to change the name)
        }

  find pred list = case list of
    [] -> Nothing
    head :: tail -> if pred head then Just head else find pred tail

  indexWhere pred list =
    let aux n list = case list of
      head :: tail -> if pred head then n else aux (n + 1) tail
      _ -> -1
    in
    aux 0 list

  indexOf value list = Update.lens {
    apply value =  indexWhere ((==) value) list
    update {input=value, outputNew=newIndex} as uInput =
      case nthMaybe newIndex list of
        Just newValue -> Ok (Inputs [newValue])
        Nothing -> Err <| \"\"\"Index @newIndex not found in @list\"\"\"
  } value

  -- TODO: Continue to insert List functions from Elm (http://package.elm-lang.org/packages/elm-lang/core/latest/List#range)
  map = map

  append = append

  concat = concatMap identity

  intersperse elem list = drop 1 (concatMap_ identity (\\headAsList -> elem :: headAsList)) list

  partition pred list = case list of
    [] -> (list, list)
    _ ->
     let ([head] as headList, tail) = split 1 list in
     let (ok, notok) = partition pred tail in
     if pred head then
       (headList ++ ok, notok)
     else
       (ok, headList ++ notok)

  mapWithDefault = mapWithDefault
  mapWithReverse = mapWithReverse
  map2 = map2 -- TOOD: Make it a lens that supports insertion?
  nil = nil
  cons = cons
  foldl = foldl
  foldr = foldr

  scanl : (a -> b -> b) -> b -> List a -> List b
  scanl f acc list = case list of
    [] -> [acc]
    head :: tail -> acc :: scanl f (f head acc) tail

  scanr : (a -> b -> b) -> b -> List a -> List b
  scanr f acc list = case list of
    [] -> [acc]
    head :: tail ->
      let headAcc::tailAcc = scanr f acc tail in
      f head headAcc :: headAcc :: tailAcc

  sort : List comparable -> List comparable
  sort = sortBy identity

  sortBy : (a -> comparable) -> List a -> List a
  sortBy f xs =
    let ins x ys =
      case ys of
        []    -> [x]
        y::ys -> if f x < f y then x :: y :: ys else y :: ins x ys
    in
    foldl ins [] xs

  sortWith : (a -> a -> Order) -> List a -> List a
  sortWith f xs =
    let ins x ys =
      case ys of
        []    -> [x]
        y::ys -> if f x y == LT then x :: y :: ys else y :: ins x ys
    in
    foldl ins [] xs

  zipWithIndex = zipWithIndex
  member = contains
  last = LensLess.List.last

  all pred list = case list of
    [] -> True
    hd :: tl -> if pred hd then all pred tl else False

  any pred list = case list of
    [] -> False
    hd :: tl -> if pred hd then True else any pred tl

  projOks list = case list of
    [] -> Ok []
    head :: tail ->
      Result.andThen2 (\\x y -> Ok (x :: y)) head (projOks tail)
}


--------------------------------------------------------------------------------
-- String --
String = {
  ({toInt=strToInt, toFloat=strToFloat, join=join__}) = LensLess.String
  (length) = LensLess.String.length -- We redefine length as a lens later in this module

  (join_) x =
    -- An example of using reversible foldl to join strings without separators
    -- Here no insertion of element is possible, but we can remove elements.
    -- We use a trick to propagate a value that is never computed, in order to know if, during update,
    -- the last string had its first char deleted
    case x of
        [] -> {apply x = \"\", update {outputNew} = Ok (Inputs [[outputNew]])}.apply x
        _ -> Update.applyLens {
        apply (x, y) = x
        update {output, diffs} = Ok (InputsWithDiffs
          [((output, True), Just (VRecordDiffs { _1 = diffs, _2 = VConstDiffs}))])
        } <| List.foldl2 (\\oldHeadList (oldAcc, dummyBool) ->
      { apply (oldAcc, [head], dummyBool) = (oldAcc + head, dummyBool)
        update {input=(oldAcc, [head], _) as input,outputNew=(newAcc, prevDeletedOrLast),diffs=ds} =
          let handleDiffs = case ds of
            VRecordDiffs {_1=VStringDiffs diffs} -> \\continuation -> continuation diffs
            _ -> \\continuation -> Ok (InputsWithDiffs [(input, Nothing)])
          in handleDiffs <| \\diffs ->
          Update.splitStringDiffsAt (length oldAcc) 0 (oldAcc + head) newAcc diffs
          |> List.concatMap (\\(leftValue, leftDiffs, rightValue, rightDiffs) ->
            let lastCharLeftDeleted =
              let aux leftDiffs = case leftDiffs of
                [StringUpdate _ end 0] -> end == length oldAcc
                head::tail -> aux tail
                _ -> leftValue == \"\"
              in aux leftDiffs
            in
            let firstCharDeleted = case rightDiffs of (StringUpdate 0 i 0) :: tail -> i > 0; _ -> False in
            let surroundingElementsDeleted = lastCharLeftDeleted && prevDeletedOrLast in
            let atLeastOneSurroundingElementDeleted = lastCharLeftDeleted || prevDeletedOrLast in
            let elemDeleted = rightValue == \"\" && (firstCharDeleted || surroundingElementsDeleted) in
            (if atLeastOneSurroundingElementDeleted && elemDeleted then [] else
              [((leftValue, [rightValue], firstCharDeleted),
                Update.pairDiff3
                  (if leftDiffs == [] then Nothing else Just (VStringDiffs leftDiffs))
                  (if rightDiffs == [] then Nothing else
                   Just (VListDiffs [(0, ListElemUpdate (VStringDiffs rightDiffs))]))
                  (if firstCharDeleted then Just VConstDiffs else Nothing)
              )]) ++
            (if elemDeleted then -- The string was deleted, one solution is to remove it.
              [((leftValue, [], True),
                Update.pairDiff3
                  (if leftDiffs == [] then Nothing else Just (VStringDiffs leftDiffs))
                  (Just (VListDiffs [(0, ListElemDelete 1)]))
                  (Just VConstDiffs)
                  )]
            else [])
          ) |> Update.valuesWithDiffs
      }.apply (oldAcc, oldHeadList, dummyBool)
      ) (freeze \"\", Update.softFreeze False) x

  {substring,
   take,
   drop,
   dropLeft,
   dropRight,
   sprintf} = LensLess.String

  -- Repeats or shrinks s until it makes the given target length
  (makeSize) s targetLength =
    let n = length s in
    if n < targetLength then makeSize (s + s) targetLength
    else if n == targetLength then s
    else take targetLength s

  repeat n s = if n <= 0 then \"\" else if n == 1 then s else s + repeat (n - 1) s
  slice = substring
  left = take
  right n x = dropLeft (max 0 ((length x) - n)) x

  uncons s = case extractFirstIn \"^([\\\\s\\\\S])([\\\\s\\\\S]*)$\" s of
    Just [x, y] -> Just (x, y)
    Nothing -> Nothing

  padLeft n c = Update.lens {
    apply s = (List.range 1 (n - length s) |> List.map (always c) |> join \"\") + s
    update {outputNew} =
       case extractFirstIn (\"^(\" + c + \")*([\\\\s\\\\S]*)$\") outputNew of
         Just [padding, str] -> Ok (Inputs [str])
         Nothing -> Err \"String.pad could not complete\"
  }

  toInt = Update.lens
    { apply x = strToInt x
      unapply output = Just (toString output) }

  toFloat = Update.lens
      { apply x = strToFloat x -- TODO: Recognize strange floats (e.g. NaN, infinity)
        unapply output = Just (toString output) }

  join delimiter x =
    if delimiter == \"\" then join_ x
    else join__ delimiter x

  -- In the forward direction, it joins the string.
  -- In the backwards direction, if the delimiter is not empty, it splits the output string with it.
  joinAndSplitBack regexSplit delimiter x = if delimiter == \"\" then join_ x else {
        apply x = join__ delimiter x
        update {output, oldOutput, diffs} =
          Ok (Inputs [Regex.split regexSplit output])
      }.apply x

  length = Update.lens {
    apply = length
    update {input, oldOutput, newOutput} =
      if newOutput < oldOutput then
        Ok (InputsWithDiffs [(take newOutput input, Just (VStringDiffs [StringUpdate newOutput oldOutput 0]))])
      else if newOutput == oldOutput then
        Ok (InputsWithDiffs [(input, Nothing)])
      else
        let increment = newOutput - oldOutput in
        let addition = makeSize (if input == \"\" then \"#\" else input) increment in
        Ok (InputsWithDiffs [(input + addition, Just (VStringDiffs [StringUpdate oldOutput oldOutput increment]))])
    }

  trim s =
    case extractFirstIn \"^\\\\s*([\\\\s\\\\S]*?)\\\\s*$\" s of
      Just [trimmed] -> trimmed
      _ -> s

  update = {
      freezeRight = Update.lens {
        apply x = x
        update {input, outputNew} =
          if take (length input) outputNew == input then
            Err <| \"Cannot add anything to the right of '\" + input + \"'\"
          else
            Ok <| Inputs [outputNew]
      }

      freezeLeft = Update.lens {
        apply x = x
        update {input, outputNew} =
          if drop (length outputNew - length input) outputNew == input then
            Err <| \"Cannot add anything to the left of '\" + input + \"'\"
          else
            Ok <| Inputs [outputNew]
      }

      debug tag text = onDeleteInsertLeftRight (\\l d i r ->
        Debug.log (tag + \" deleted: \" + toString d + \", left: \" + toString (drop (max 0 (length l - 100)) l) + \", right: \" + toString (take 100 r) + \", inserted:\") i) text

      onInsert callbackOnInserted string =
        onDeleteInsertLeftRight (\\left deleted inserted right -> callbackOnInserted inserted) string

      -- Enables to change the inserted text during back-propagation.
      onDeleteInsertLeftRight callbackOnLeftDeletedInsertedRight string = Update.lens {
        apply string = string
        update {outputOld, outputNew, diffs=(VStringDiffs sDiffs)} =
          let aux offset outputNewUpdated revDiffsUpdated oldDiffs = case oldDiffs of
            [] -> Ok (InputsWithDiffs [
              (outputNewUpdated, Just (VStringDiffs (List.reverse revDiffsUpdated)))])
            ((StringUpdate start end replaced) as headDiff) :: tailOldDiffs ->
              let inserted = substring (start + offset) (start + offset + replaced) outputNewUpdated in
              let left = take (start+offset) outputNewUpdated in
              let right = drop (start+offset+replaced) outputNewUpdated in
              let deleted = substring start end outputOld in
              let newInserted = callbackOnLeftDeletedInsertedRight left deleted inserted right in
              let lengthNewInserted = length newInserted in
              let newOffset = offset + lengthNewInserted - (end - start) in
              let (newOutputNewUpdated, newDiff) = if inserted /= newInserted then
                 ( take (start + offset) outputNewUpdated +
                   newInserted + drop (start + offset + replaced) outputNewUpdated
                 , StringUpdate start end lengthNewInserted)
                 else (outputNewUpdated, headDiff)
              in
              aux newOffset newOutputNewUpdated (newDiff::revDiffsUpdated) tailOldDiffs
           in aux 0 outputNew [] sDiffs
       } string


      -- Same version but also enables to change some state as well.
      onDeleteInsertLeftRightState callbackOnLeftDeletedInsertedRightState string state = Update.lens {
          apply (string, state) = string
          update {input=(string, originalState), outputOld, outputNew, diffs=(VStringDiffs sDiffs)} =
            let aux offset outputNewUpdated revDiffsUpdated oldDiffs state = case oldDiffs of
              [] -> Ok (InputsWithDiffs [
                ( (outputNewUpdated, state)
                , Update.mbPairDiffs (
                    Just <| VStringDiffs <| List.reverse revDiffsUpdated,
                    Update.diffs originalState state
                  )
                )])
              ((StringUpdate start end replaced) as headDiff) :: tailOldDiffs ->
                let inserted = substring (start + offset) (start + offset + replaced) outputNewUpdated in
                let left = take (start+offset) outputNewUpdated in
                let right = drop (start+offset+replaced) outputNewUpdated in
                let deleted = substring start end outputOld in
                let (newInserted, state) = callbackOnLeftDeletedInsertedRightState left deleted inserted right state in
                let lengthNewInserted = length newInserted in
                let newOffset = offset + lengthNewInserted - (end - start) in
                let (newOutputNewUpdated, newDiff) = if inserted /= newInserted then
                   ( take (start + offset) outputNewUpdated +
                     newInserted + drop (start + offset + replaced) outputNewUpdated
                   , StringUpdate start end lengthNewInserted)
                   else (outputNewUpdated, headDiff)
                in
                aux newOffset newOutputNewUpdated (newDiff::revDiffsUpdated) tailOldDiffs state
             in aux 0 outputNew [] sDiffs originalState
         } (string, state)

      fixTagUpdates string = Update.lens {
         apply string = string
         update {outputOld, outputNew, diffs=(VStringDiffs sDiffs)} =
           let aux offset revDiffsUpdated oldDiffs = case oldDiffs of
             [] -> Ok (InputsWithDiffs [
               (outputNew, Just (VStringDiffs (List.reverse revDiffsUpdated)))])
             ((StringUpdate start end replaced) as headDiff) :: tailOldDiffs ->
               let continueWith callback = case tailOldDiffs of
                 StringUpdate start2 end2 replaced2 :: tailTailOldDiffs->
                   if start2 <= end + 2 && start2 /= end then -- Second condition necessary to not merge immediate deletion/insertions
                     if not <| Regex.matchIn \"[<>]\" (substring end start2 outputOld) then
                       aux offset revDiffsUpdated (StringUpdate start end2 (replaced + replaced2 + start2 - end) :: tailTailOldDiffs)
                     else
                       callback ()
                   else
                     callback ()
                 _ ->
                 callback ()
               in
               continueWith <| \\_ ->
               let inserted = substring (start + offset) (start + offset + replaced) outputNew in
               let deleted = substring start end outputOld in
               let right = drop (start + offset + replaced) outputNew in
               let rightOld = drop end outputOld in
               let left = take (start + offset) outputNew in
               let newOffset = offset + replaced - (end - start) in
               if Regex.matchIn \"\"\"^<[\\s\\S]*>$\"\"\" inserted && Regex.matchIn \"\"\"^<[\\s\\S]*>$\"\"\" deleted then
                 -- <img> --> <br> It should be a true replacement instead, else we'll back-propagate the transformation inside the <>
                 aux offset revDiffsUpdated (StringUpdate start end 0 :: StringUpdate end end replaced :: tailOldDiffs)
               else
               let newDiff =
                    if Regex.matchIn \"^><\" inserted  && Regex.matchIn \"^>\" right then
                      StringUpdate (start + 1) (end + 1) replaced
                    else if Regex.matchIn \"<$\" left && Regex.matchIn \"><$\" inserted then
                      StringUpdate (start - 1) (end - 1) replaced
                    else if Regex.matchIn \"</$\" left && Regex.matchIn \"></$\" inserted then
                      StringUpdate (start - 2) (end - 2) replaced
                    else if Regex.matchIn \"<$\" left && Regex.matchIn \"<$\" deleted then
                      StringUpdate (start - 1) (end - 1) replaced
                    else if Regex.matchIn \"^>\" deleted && Regex.matchIn \"^>\" right then
                      StringUpdate (start + 1) (end + 1) replaced
                    else if Regex.matchIn \"<$\" left && Regex.matchIn \"^>\" right && Regex.matchIn \"^>\" rightOld then -- We expand the replacement
                      StringUpdate (start - 1) (end + 1) (replaced + 2)
                    else
                      headDiff
               in
               aux newOffset (newDiff::revDiffsUpdated) tailOldDiffs
            in aux 0 [] sDiffs
        } string
  }

  newlines = {
    isCRLF = Regex.matchIn \"\\r\\n\"
    toUnix string =
      if isCRLF string then
        Regex.split \"\\r\\n\" string
        |> joinAndSplitBack \"\\n\" \"\\n\"
      else
        string
  }

  toLowerCase string =
    Regex.replace \"[A-Z]\" (\\m ->
      Update.replaceInstead m.match <|
      case m.match of
        \"A\" -> \"a\"; \"B\" -> \"b\"; \"C\" -> \"c\"; \"D\" -> \"d\"; \"E\" -> \"e\"; \"F\" -> \"f\"; \"G\" -> \"g\"; \"H\" -> \"h\"; \"I\" -> \"i\";
        \"J\" -> \"j\"; \"K\" -> \"k\"; \"L\" -> \"l\"; \"M\" -> \"m\"; \"N\" -> \"n\"; \"O\" -> \"o\"; \"P\" -> \"p\"; \"Q\" -> \"q\"; \"R\" -> \"r\";
        \"S\" -> \"s\"; \"T\" -> \"t\"; \"U\" -> \"u\"; \"V\" -> \"v\"; \"W\" -> \"w\"; \"X\" -> \"x\"; \"Y\" -> \"y\"; \"Z\" -> \"z\"; _ -> m.match
      ) string

  toUpperCase string =
    Regex.replace \"[a-z]\" (\\m ->
      Update.replaceInstead m.match <|
      case m.match of
        \"a\" -> \"A\"; \"b\" -> \"B\"; \"c\" -> \"C\"; \"d\" -> \"D\"; \"e\" -> \"E\"; \"f\" -> \"F\"; \"g\" -> \"G\"; \"h\" -> \"H\"; \"i\" -> \"I\";
        \"j\" -> \"J\"; \"k\" -> \"K\"; \"l\" -> \"L\"; \"m\" -> \"M\"; \"n\" -> \"N\"; \"o\" -> \"O\"; \"p\" -> \"P\"; \"q\" -> \"Q\"; \"r\" -> \"R\";
        \"s\" -> \"S\"; \"t\" -> \"T\"; \"u\" -> \"U\"; \"v\" -> \"V\"; \"w\" -> \"W\"; \"x\" -> \"X\"; \"y\" -> \"Y\"; \"z\" -> \"Z\"; _ -> m.match
      ) string

  markdown text =
    let escapeHtml = Regex.replace \"[<>&]\" (case of
          {match = \"<\"} -> freeze \"&lt;\"
          {match = \">\"} -> freeze \"&gt;\"
          {match = \"&\"} -> freeze \"&amp;\")
        escapeAttribute = Regex.replace \"\\r?\\n|\\\"\" (case of
          \"\\\"\" -> \"\\\\\\\"\"
          _ -> \"\")
        notincode = \"\"\"(?!(?:(?!<code>)[\\s\\S])*</code>)\"\"\"
        notinulol = \"\"\"(?!(?:(?!<[uo]l>)[\\s\\S])*</[uo]l>)\"\"\"
        notinattr = \"\"\"(?!(?:(?!<)[\\s\\S])*>)\"\"\"
        regexFootnotes = \"\"\"\\r?\\n\\[\\^@notincode([^\\]]+)\\]:\\s*@notincode((?:(?!\\r?\\n\\r?\\n)[\\s\\S])+)@notincode\"\"\"
        regexReferences = \"\"\"\\r?\\n\\[(?!\\^)([^\\]\\\\]+)\\]:\\s*@notincode(\\S+)@notincode\"\"\"
        footnotes = Regex.find regexFootnotes text
                     |> List.map (\\m -> (nth m 1, nth m 2))
                     |> List.indexedMap (\\i (name, value) -> (name, (i + 1, value)))
        references = Regex.find regexReferences text
                     |> List.map (\\m -> (nth m 1, nth m 2))
        r  = Regex.replace
        lregex = \"\"\"(?:\\r?\\n|^)((?:(?![\\r\\n])\\s)*)(\\*|-|\\d+\\.)(\\s+)((?:@notincode.*)(?:\\r?\\n\\1  ?\\3(?:@notincode.*))*(?:\\r?\\n\\1(?:\\*|-|\\d+\\.)\\3(?:@notincode.*)(?:\\r?\\n\\1 \\3(?:@notincode.*))*)*)@notincode\"\"\"
        handleLists text  =
          flip (r lregex) text <|
            \\m -> let indent = nth m.group 1
                      afterindent = nth m.group 3
                      symbol = nth m.group 2
                      ul_ol = Update.bijection
                               (case of \"*\" -> \"ul\"; \"-\" -> \"ul\"; _ -> \"ol\")
                               (case of
                                 \"ul\" -> if symbol == \"*\" || symbol == \"-\" then symbol else \"*\"
                                 \"ol\" -> if symbol == \"*\" || symbol == \"-\" then \"1.\" else symbol
                                 x -> error \"\"\"tag name '@x' not compatible with ul or ol\"\"\")
                               symbol
                      elements =
                        Regex.split \"\"\"\\r?\\n@indent(?:\\*|-|\\d+\\.)@afterindent\"\"\" (nth m.group 4)
                      insertionPoint callback = Update.lens {
                        apply elements = \"\"
                        update {input=elements, output} =
                          case Regex.extract \"\"\"^<li>([\\s\\S]*)</li>$\"\"\" output of
                            Just [content] ->
                              Ok (Inputs [callback elements (Regex.split \"\"\"</li><li>\"\"\" content)]) -- TODO: Precompute diffs (insertions)
                            _->
                              Err (\"Can only insert <li> elements to lists, got \" + output)
                      } elements
                      insertAfter = insertionPoint (append)
                      insertBefore = insertionPoint (flip append)
                  in
                  Update.freezeExcept (\\_ ->  \"ul template\")  [ul_ol, insertBefore, handleLists, elements, notinulol, insertAfter] <|
                                                             \\[ul_ol, insertBefore, handleLists, elements, notinulol, insertAfter] ->
                  \"\"\"<@ul_ol>@insertBefore<li>@(
                    List.map handleLists elements
                    |> joinAndSplitBack \"\"\"</li><li>@notinulol\"\"\" \"</li><li>\")</li>@insertAfter</@ul_ol>\"\"\"

        handleblockquotes text =
          Regex.replace \"\"\"(?:(?:\\r?\\n|^)>(?!.*</.*).*)+@notincode\"\"\" (\\m ->
           Regex.extract \"\"\"(\\r?\\n|^)>([\\s\\S]*)\"\"\" m.match
           |> Maybe.map (\\[newline, content] ->
             let quoteContent =
                  Regex.replace \"\"\"(\\r?\\n)> *\"\"\" (\\m -> nth m.group 1) content
             in
             let recursivecontent = quoteContent |> handleblockquotes in
             Update.freezeExcept (\\_ -> \"blockquote template\") [newline, recursivecontent] <|
                                                              \\[newline, recursivecontent] ->
             \"\"\"@newline<blockquote>@recursivecontent</blockquote>\"\"\"
           ) |> Maybe.withDefault m.match
          ) text
    in let
    text =
      r \"\"\"(```)([\\s\\S]*?)\\1(?!`)|(\\r?\\n *|^ *)((?:\\r?\\n    .*)+)\"\"\" (\\m ->
      if nth m.group 1 == \"\" then
        nth m.group 4 |>
        Regex.extract \"\"\"^\\r?\\n    ([\\s\\S]*)$\"\"\" |>
        Maybe.map (\\[code] ->
                nth m.group 3 + \"\"\"<pre><code>@(Regex.split \"\"\"\\r?\\n    \"\"\" code |> join \"\\n\" |> trim |> escapeHtml)</code></pre>\"\"\")
        |> Maybe.withDefault m.match
      else
      \"\"\"<pre><code>@(nth m.group 2 |> trim |> escapeHtml)</code></pre>\"\"\") text
    text =  handleblockquotes text
    text =  r \"\"\"(`)(?=[^\\s`])(@notincode.*?)\\1@notincode\"\"\" (\\m -> \"\"\"<code>@(nth m.group 2 |> escapeHtml)</code>\"\"\") text
    text =  r \"\"\"(?:@regexReferences|@regexFootnotes)@notincode\"\"\" (\\m -> \"\") text
    text = -- Expand footnotes
      if List.length footnotes == 0 then text
      else text + \"\"\"
  <div class=\"footnotes\"><hr><ol>@(footnotes |>
              List.map (\\(name, (n, value)) ->
                \"\"\"<li id=\"fn@n\"><p>@value<a href=\"#fnref@n\">↩</a></p></li>\"\"\"
              ) |> join \"\")</ol></div>\"\"\"
    text = r \"\"\"(^|\\r?\\n)(#+)\\s*(@notincode[^\\r\\n]*)@notincode\"\"\" (\\m ->
      let hlevel = \"\"\"h@(length (nth m.group 2))\"\"\" in
      Update.freezeExcept (always \"h-template\") [m, hlevel] <| \\[m, hlevel] -> \"\"\"@(nth m.group 1)<@hlevel>@(nth m.group 3)</@hlevel>\"\"\") text
    text = handleLists text
    text = r \"\"\"(?:(\\r?\\n *\\r?\\n)(?:\\\\noindent\\r?\\n)?|^)((?=\\s*\\w|\\S)@notincode[\\s\\S]*?)(?=(\\r?\\n *\\r?\\n|\\r?\\n$|$))@notincode\"\"\" (
      \\m ->
        if nth m.group 1 == \"\" && nth m.group 3 == \"\" -- titles and images should not be paragraphs.
         || Regex.matchIn \"\"\"</?(?:h\\d|ul|ol|p|pre|center)>\"\"\" (nth m.group 2) then m.match else Update.freezeExcept (always \"p template\") m <| \\m -> \"\"\"@(nth m.group 1)<p>@(nth m.group 2)</p>\"\"\") text
    text = r \"\"\"(!?)\\[([^\\]\\[\\\\]+)\\](\\^?)(\\(|\\[)([^\\[\\)\\]\\s]+)\\s?\"?([^\\)\\]\"]+)?\"?(?:\\)|\\])|(?:http|ftp|https):\\/\\/(?:[\\w_-]+(?:(?:\\.[\\w_-]+)+))(?:[\\w.,@@?^=%&:/~+#-]*[\\w@@?^=%&/~+#-])?@notincode@notinattr\"\"\" (
      \\{group=[match, picPrefix, text, targetblank, parensStyle, url, title]} ->  -- Direct and indirect References + syntax ^ to open in external page.
      let a = if picPrefix == \"!\" then \"img\" else \"a\"
          aclose = if picPrefix == \"!\" then \"\" else text + \"</a>\"
          href = if picPrefix == \"!\" then \"src\" else \"href\"
          t = if title == \"\" then \"\" else \" title='\" + title + \"'\"
          targetblank = if targetblank == \"^\" then \"\"\" target='_blank'\"\"\" else \"\"
          alt = if picPrefix == \"!\" then \"\"\" alt='@text'\"\"\" else \"\"
      in
      case parensStyle of
        \"(\" -> (Update.freezeWhen True toString (\\a t href url targetblank alt aclose -> \"\"\"<@a @t @href=\"@url\"@targetblank@alt>@aclose\"\"\"))
                  a t href url targetblank alt aclose
        \"[\" -> listDict.get url references |> case of
              Just link ->
                Update.freezeExcept (always \"a or img relative template\")
                  [a, t, href, link, targetblank, alt, aclose] <|
                 \\[a, t, href, link, targetblank, alt, aclose] ->
               \"\"\"<@a @t @href=\"@link\"@targetblank@alt>@aclose\"\"\"
              Nothing -> match
        _ -> Update.freezeExcept (always \"full URL template\") match <| \\match -> \"\"\"<a href=\"@match\">@(escapeHtml match)</a>\"\"\"
        ) text
    text = r \"\"\"\\[\\^(@notincode[^\\]]+)\\]@notincode\"\"\" (\\m ->  -- Footnotes
      listDict.get (nth m.group 1) footnotes |> case of
        Just (n, key) ->
          Update.freezeExcept (always \"footnote template\") [n, key] <| \\[n, key] ->
          \"\"\"<a href=\"#fn@n\" title=\"@(escapeAttribute key)\" class=\"footnoteRef\" id=\"fnref@n\"><sup>@n</sup></a>\"\"\"
        Nothing -> m.match) text
    text = r \"\"\"(\\*{1,3}|_{1,3})(?=[^\\s\\*_])(@notincode(?:(?!\\\\\\*|\\_).)*?)(\\1)@notincode@notinattr\"\"\" (\\m ->
      let content = nth m.group 2 in
      let n = nth m.group 1 |> length in
      let m = nth m.group 3 |> length in
      let (start, end)= Update.lens2 {
        apply (n, m) = case n of
          1 -> (\"<em>\", \"</em>\")
          2 -> (\"<strong>\", \"</strong>\")
          3 -> (\"<em><strong>\", \"</strong></em>\")
        update {outputNew=(start,end)} = case (start, end) of
          (\"\", \"\") -> Ok (Inputs [(0, 0)])
          (\"<span>\", \"</span>\") -> Ok (Inputs [(0, 0)])
          (\"<em>\", \"</em>\") -> Ok (Inputs [(1, 1)])
          (\"<strong>\", \"</strong>\") -> Ok (Inputs [(2, 2)])
          _ -> Err \"Cannot touch em and strong tags directly. For now, just delete and replace the text without italics/bold\"
      } n m
      in start + content + end) text
    text = r \"\"\"(&mdash;|\\\\\\*|\\\\_|\\\\\\[|\\\\\\]|  \\r?\\n)@notincode\"\"\" (\\m -> case m.match of
      \"&mdash;\" -> \"—\"
      \"\\\\*\" -> drop 1 m.match
      \"\\\\_\" -> drop 1 m.match
      \"\\\\[\" -> drop 1 m.match
      \"\\\\]\" -> drop 1 m.match
      \"  \\r\\n\" -> \"<br>\"
      \"  \\n\" -> \"<br>\"
      ) text
    text = update.onDeleteInsertLeftRight (\\left deleted inserted right ->
      let
        inserted =
          Regex.replace \"\"\"<(b|i|em|strong)>(?=\\S)((?:(?!</\\1\\s*>).)*\\S)</\\1\\s*>\"\"\" (\\m ->
            let tag = nth m.group 1
                content = nth m.group 2
            in if tag == \"b\" || tag == \"strong\" then
              \"**\" + content + \"**\"
            else
              \"*\" + content + \"*\"
          ) inserted
        inserted =
          Regex.replace \"\"\"\\s*<(h([1-6]))>((?:(?!</\\1\\s*>).)*)</\\1\\s*>\\s*\"\"\" (\\m ->
              let n = String.toInt <| nth m.group 2
                  hash = String.repeat n \"#\"
              in \"\\n\\n\" + hash + \" \" + String.trim (nth m.group 3) + \"\\n\\n\"
            ) inserted
        inserted = case Regex.extract \"\"\"^([\\s\\S]*)</p>\\s*<p>([\\s\\S]*)$\"\"\" inserted of
          Just [before, after] -> before + \"\\n\\n\" + after
          _ -> inserted
        inserted = case Regex.extract \"\"\"^([\\s\\S]*)<div>([\\s\\S]*)</div>([\\s\\S]*)$\"\"\" inserted of
          Just [before, paragraph, after] ->
             before + \"\\n\\n\" + paragraph + after
          _ -> inserted
        inserted = case Regex.extract \"\"\"^([\\s\\S]*)<(/?)(b|i|strong|em)\\s*>([\\s\\S]*)$\"\"\" inserted of
          Just [before, mbSlash, bi, after] ->
            let mdtag = if bi == \"b\" || bi == \"strong\" then \"**\" else \"*\" in
            let mdtagregex = if bi == \"b\" then \"\\\\*\\\\*\" else \"\\\\*\" in
            if mbSlash == \"\" && Regex.matchIn \"\"\"^\\S(?:(?!</@bi\\s*>).)*?\\S</@bi\\s*>\"\"\" (after + right) then
              before + mdtag + after
            else if mbSlash == \"/\" && Regex.matchIn \"\"\"@mdtagregex\\S(?:(?!@mdtagregex).)*?$\"\"\" (left + before) then
              before + mdtag + after
            else inserted
          _ -> inserted
        inserted = Regex.replace \"\"\"<a href=\"((?:(?!</a>).)*)\">((?:(?!</a>).)*)</a>\"\"\" (\\{submatches=[url, text]} ->
             \"[\" + text + \"](\"+ url +\")\"
          ) inserted
        inserted = Regex.replace \"\"\"<img[^>]+\\bsrc=[^>]+>\"\"\" (\\match ->
           case htmlViaEval match.match of
              [[\"img\", attrs, _]] ->
                let title = attrDict.get \"title\" attrs |> Maybe.map (\\x -> \" \\\"\" + x + \"\\\"\") |> Maybe.withDefault \"\"
                    alt = attrDict.get \"alt\" attrs |> Maybe.map (\\x -> if x == \"\" then \"Image\" else x) |> Maybe.withDefault \"Image\"
                    url = attrDict.get \"src\" attrs |> Maybe.withDefault \"url\"
                in
                \"![\" + alt + \"](\"+ url + title + \")\"
              _ -> match.match) inserted
      in inserted
      ) text
    text = update.fixTagUpdates text
    in text

  q3 = \"\\\"\\\"\\\"\" -- To interpolate triple quotes into strings
}


--------------------------------------------------------------------------------
-- Maybe --

Maybe =
  { type Maybe a = Nothing | Just a

    -- Returns the first argument if the second is Nothing. If the second is Just x returns x
    withDefault d mb =
      case mb of
        Nothing -> d
        Just j -> j

    -- Like withDefault, but if the default was used and we push a new value,
    -- it will push it back as a \"Just\" before trying the usual behavior. Ideal for displaying placeholders.
    withDefaultReplace = Update.lens2 {
      apply (d, mb) = case mb of
        Nothing -> d
        Just j -> j
      update {input=(d,mb) as input,outputNew} as uInput =
        Ok (InputsWithDiffs ([((d, Just outputNew),
              Update.diffs mb (Just outputNew) |> LensLess.Maybe.map Update.vTupleDiffs_2)] ++ (
              if mb /= Nothing then [] else Update.defaultAsListWithDiffs apply uInput)))
    }

    -- Like withDefault, but the default value is a callback that is called if mb is empty.
    withDefaultLazy df mb =
      case mb of
        Nothing -> df ()
        Just j -> j

    -- Like withDefault, but if the default was used and we push a new value,
    -- it will push it back as a \"Just\" before trying the usual behavior. Ideal for displaying placeholders.
    withDefaultReplaceLazy = Update.lens2 {
      apply (df, mb) = case mb of
        Nothing -> df ()
        Just j -> j
      update {input=(df,mb) as input,outputNew} as uInput =
        Ok (InputsWithDiffs ([((df, Just outputNew),
              Update.diffs mb (Just outputNew) |> LensLess.Maybe.map Update.vTupleDiffs_2)] ++ (
              if mb /= Nothing then [] else Update.defaultAsListWithDiffs apply uInput)))
    }

    map = Update.lens2 {
      apply (f, a) = case a of
        Nothing -> Nothing
        Just x -> Just (f x)
      update = case of
        {outputOld=Just x, outputNew=Nothing} ->
          Ok (InputsWithDiffs [((f, Nothing), Update.mbPairDiffs (Nothing, Just VConstDiffs))])
        uInput -> Update.default apply uInput
    }

    -- Like map, but does not fail if the input was NOthing and the new output is Just, because it knows a default element.
    mapWithDefault = Update.lens2 {
      apply (default, f, a) = case a of
        Nothing -> Nothing
        Just x -> Just (f x)
      update = case of
        {outputOld=Just x, outputNew=Nothing} ->
           Ok (InputsWithDiffs [((f, Nothing), Update.mbPairDiffs (Nothing, Just VConstDiffs))])
        {outputOld=Nothing, outputNew=Just x} ->
           case Update.updateApp {fun (_, f, x) = f x, input = (default, f, default), outputNew = x} of
             Err msg -> Err msg
             Ok (InputsWithDiffs inputsWithDiffs) ->
               Ok (InputsWithDiffs (inputsWithDiffs |> List.map (\\((default, f, newX), newDiff) ->
                 ((default, f, Just newX), case newDiff of
                   Just _ -> Just (VRecordDiffs { _3=VConstDiffs})
                   Nothing -> Nothing)
               )))
        uInput -> Update.default apply uInput
    }

    andThen f a = case a of
      Nothing -> Nothing
      Just x -> f x

    orElse mb2 mb1 = case mb1 of
      Just _ -> mb1
      Nothing -> mb2

    -- like orElse, but in the reverse direction, will try to push Nothing to mb1
    orElseReplace = Update.lens2 {
      apply (mb2, mb1) = case mb1 of
        Nothing -> mb2
        Just x -> mb1
      update {input=(mb2, mb1), outputOld, outputNew} =
        case outputNew of
          Just _ -> case mb1 of
            Nothing -> Ok (Inputs [(mb2, outputNew), (outputNew, mb1)])
            Just _ -> Ok (Inputs [(mb2, outputNew)])
          Nothing -> Ok (Inputs [(mb2, Nothing)])
    }

    -- Given emptyCondition and Just x, returns x.
    -- On the reverse direction, if the new x passes the emptyCondition, propagates back Nothing
    getUnless emptyCondition = Update.lens {
      apply (Just x) = x
      update {outputNew} as uInput =
        if emptyCondition outputNew then
          Ok (Inputs [Nothing])
        else
          Update.default apply uInput
    }

    fold : a -> (x -> a) -> Maybe x -> a
    fold onNothing onJust content =
      case content of
        Nothing -> onNothing
        Just c -> onJust c

    foldLazy : (() -> a) -> (x -> a) -> Maybe x -> a
    foldLazy onNothing onJust content =
      case content of
        Nothing -> onNothing ()
        Just c -> onJust c
  }

-- if we decide to allow types to be defined within (and exported from) modules
--
-- {Nothing, Just} = Maybe
--
-- might look something more like
--
-- {Maybe(Nothing,Just)} = Maybe
-- {Maybe(..)} = Maybe
--
-- -- Sample deconstructors once generalized pattern matching works.
-- Nothing$ = {
--   unapplySeq exp = case exp of
--     {$d_ctor=\"Nothing\", args=[]} -> Just []
--     _ -> Nothing
-- }
-- Just$ = {
--   unapplySeq exp = case exp of
--     {$d_ctor=\"Just\", args=[x]} -> Just [x]
--     _ -> Nothing
-- }

--------------------------------------------------------------------------------
-- Tuple --

Tuple =
  { mapFirst f (x, y) = (f x, y)
    mapSecond f (x, y) = (x, f y)
    first (x, y) = x
    second (x, y) = y
  }

--------------------------------------------------------------------------------
-- Editor --

Editor = {}

-- TODO remove this; add as imports as needed in examples
{freeze, applyLens} = Update

-- Custom Update: List Map, List Append, ...

-- TODO

-- HTML

--------------------------------------------------------------------------------
-- Html.html

--type HTMLAttributeValue = HTMLAttributeUnquoted WS WS String | HTMLAttributeString WS WS String {-Delimiter char-}  String | HTMLAttributeNoValue
--type HTMLAttribute = HTMLAttribute WS String HTMLAttributeValue
--type HTMLCommentStyle = Less_Greater String {- The string should start with a ? -}
--                      | LessSlash_Greater {- The string should start with a space -} String
--                      | LessBang_Greater String
--                      | LessBangDashDash_DashDashGreater String

--type HTMLClosingStyle = RegularClosing WS | VoidClosing | AutoClosing | ForgotClosing
--type HTMLEndOpeningStyle = RegularEndOpening {- usually > -} | SlashEndOpening {- add a slash before the '>' of the opening, does not mark the element as ended in non-void HTML elements -}
-- HTMLInner may have unmatched closing tags inside it. You have to remove them to create a real innerHTML
-- HTMLInner may have unescaped chars (e.g. <, >, & etc.)
--type HTMLNode = HTMLInner String
--              | HTMLElement String (List HTMLAttribute) WS HTMLEndOpeningStyle (List HTMLNode) HTMLClosingStyle
--              | HTMLComment HTMLCommentStyle

updateOutputToResults c = case c of
  Ok (Inputs i) -> Ok i
  Ok (InputsWithDiffs id) -> Ok (List.unzip id |> Tuple.first)
  Err msg -> Err msg

-- Returns a list of HTML nodes parsed from a string. It uses the API for loosely parsing HTML
-- Example: html \"Hello<b>world</b>\" returns [[\"TEXT\",\"Hello\"],[\"b\",[], [[\"TEXT\", \"world\"]]]]
html string = {
  apply trees =
    let unwrapcontent elems = List.map (case of
      HTMLAttributeStringRaw raw -> raw
      HTMLAttributeEntity rendered _ -> rendered) elems |> String.join \"\" in
    let domap tree = case tree of
      HTMLInner v -> [\"TEXT\",
        replaceAllIn \"</[^>]*>\" (\\{match} -> \"\") v]
      HTMLEntity entityRendered entity -> [\"TEXT\", entityRendered]
      HTMLElement tagName attrs ws1 endOp children closing ->
        [ tagName
        , map (case of
          HTMLAttribute ws0 name value ->
            let (name, content) = case value of
              HTMLAttributeUnquoted _ _ content -> (name, unwrapcontent content)
              HTMLAttributeString _ _ _ content -> (name, unwrapcontent content)
              HTMLAttributeNoValue -> (name, \"\")
            in
            if name == \"style\" then
              let styleContent = Regex.split \"(?=;\\\\s*\\\\S)\" content |> LensLess.List.filterMap (
                  Regex.extract \"^;?([\\\\s\\\\S]*):([\\\\s\\\\S]*);?\\\\s*$\"
                )
              in
              [name, styleContent]
            else [name, content]
            ) attrs
        , map domap children]
      HTMLComment {args = {_1=content}} -> [\"comment\", [[\"display\", \"none\"]], [[\"TEXT\", content]]]
    in map domap trees

  update {input, oldOutput, newOutput, diffs} =
    let toHTMLAttribute [name, mbStyleValue] =
      let value =
        if name == \"style\" then
          LensLess.String.join \"; \" (LensLess.List.map (\\[styleName, styleValue] ->
            styleName + \":\" + styleValue
          ) mbStyleValue)
        else mbStyleValue
      in
      HTMLAttribute \" \" name (HTMLAttributeString \"\" \"\" \"\\\"\" value) in
    let toHTMLInner text = HTMLInner (replaceAllIn \"<|>|&\" (\\{match} -> case match of \"&\" -> \"&amp;\"; \"<\" -> \"&lt;\"; \">\" -> \"&gt;\"; _ -> \"\") text) in
    let toHTMLNode e = case e of
      [\"TEXT\",v2] -> toHTMLInner v2
      [tag, attrs, children] -> HTMLElement tag (map toHTMLAttribute attrs) \"\"
           RegularEndOpening (map toHTMLNode children) (RegularClosing \"\")
    in
    let reconcile oldAttrElems newString = [HTMLAttributeStringRaw newString] in
    let mergeAttrs input oldOutput newOutput diffs =
      Update.foldDiff {
        start =
          -- Accumulator of HTMLAttributes, accumulator of differences, original list of HTMLAttributes
          ([], [], input)
        onSkip (revAcc, revDiffs, input) {count} =
          --'outs' was the same in oldOutput and outputNew
          let (newRevAcc, remainingInput) = LensLess.List.reverse_move count revAcc input in
          Ok [(newRevAcc, revDiffs, remainingInput)]

        onUpdate (revAcc, revDiffs, input) {oldOutput, newOutput, diffs, index} =
          let inputElem::inputRemaining = input in
          let newInputElem = case (inputElem, newOutput) of
            (HTMLAttribute sp0 name value, [name2, value2 ]) ->
             let realValue2 =
               if name == \"style\" then -- value2
                 String.join \"\" (List.map (\\[styleName, styleValue] ->
                   styleName + \":\" + styleValue + \";\"
                 ) value2)
               else value2
             in
             case value of
               HTMLAttributeUnquoted sp1 sp2 v ->
                 case extractFirstIn \"\\\\s\" realValue2 of
                   Nothing ->
                     HTMLAttribute sp0 name2 (HTMLAttributeUnquoted sp1 sp2 (reconcile v realValue2))
                   _ ->
                     HTMLAttribute sp0 name2 (HTMLAttributeString sp1 sp2 \"\\\"\" (reconcile v realValue2))
               HTMLAttributeString sp1 sp2 delim v ->
                     HTMLAttribute sp0 name2 (HTMLAttributeString sp1 sp2 delim (reconcile v realValue2))
               HTMLAttributeNoValue ->
                  if value2 == \"\" then HTMLAttribute sp0 name2 (HTMLAttributeNoValue)
                  else toHTMLAttribute [name2, value2]
               _ -> Debug.crash <| \"expected HTMLAttributeUnquoted, HTMLAttributeString, HTMLAttributeNoValue, got \" ++ toString (inputElem, newOutput)
            _ -> Debug.crash \"Expected HTMLAttribute, got \" ++ toString (inputElem, newOutput)
          in
          let newRevDiffs = case Update.diff inputElem newInputElem of
            Ok (Just d) -> (index, ListElemUpdate d)::revDiffs
            Ok (Nothing) ->  revDiffs
            Err msg -> Debug.crash msg
          in
          Ok [(newInputElem::revAcc, newRevDiffs, inputRemaining)]

        onRemove (revAcc, revDiffs, input) {oldOutput, index} =
          let _::remainingInput = input in
          Ok [(revAcc, (index, ListElemDelete 1)::revDiffs, remainingInput)]

        onInsert (revAcc, revDiffs, input) {newOutput, index} =
          Ok [(toHTMLAttribute newOutput :: revAcc, (index, ListElemInsert 1)::revDiffs, input)]

        onFinish (revAcc, revDiffs, _) =
          Ok [(reverse revAcc, reverse revDiffs)]

        onGather (acc, diffs) =
          Ok (InputWithDiff (acc,
               (if len diffs == 0 then Nothing else Just (VListDiffs diffs))))
      } oldOutput newOutput diffs
    in
    -- Returns Ok (List (List HTMLNode)., diffs = List (Maybe ListDiff)} or Err msg
    let mergeNodes input oldOutput newOutput diffs =
      Update.foldDiff {
        start =
          -- Accumulator of values, accumulator of differences, original input
          ([], [], input)

        onSkip (revAcc, revDiffs, input) {count} =
          --'outs' was the same in oldOutput and outputNew
          let (newRevAcc, remainingInput) = LensLess.List.reverse_move count revAcc input in
          Ok [(newRevAcc, revDiffs, remainingInput)]

        onUpdate (revAcc, revDiffs, input) {oldOutput, newOutput, diffs, index} =
          let inputElem::inputRemaining = input in
          --Debug.start (\"onUpdate\" + toString (oldOutput, newOutput, diffs, index)) <| \\_ ->
          let newInputElems = case (inputElem, oldOutput, newOutput) of
            ( HTMLInner v, _, [\"TEXT\",v2]) -> Ok [toHTMLInner v2]
            ( HTMLEntity entityRendered entity, _, [\"TEXT\", v2]) -> Ok [HTMLEntity v2 v2]
            ( HTMLElement tagName attrs ws1 endOp children closing,
              [tag1, attrs1, children1], [tag2, attrs2, children2] ) ->
               if tag2 == tagName || attrs1 == attrs2 || children2 == children1  then
                 case diffs of
                   VListDiffs listDiffs ->
                     let (newAttrsMerged, otherDiffs) = case listDiffs of
                       (1, ListElemUpdate diffAttrs)::tailDiff ->
                         (mergeAttrs attrs attrs1 attrs2 diffAttrs, tailDiff)
                       _ -> (Ok (Inputs [attrs]), listDiffs)
                     in
                     let newChildrenMerged = case otherDiffs of
                       (2, ListElemUpdate diffNodes)::_ ->
                         case mergeNodes children children1 children2 diffNodes of
                           Ok (InputsWithDiffs vds) -> Ok (List.map Tuple.first vds)
                           Err msg -> Err msg
                       _ -> Ok [children]
                     in
                     newAttrsMerged |> updateOutputToResults |> LensLess.Results.andThen (\\newAttrs ->
                       newChildrenMerged |>LensLess.Results.andThen (\\newChildren ->
                         Ok [HTMLElement tag2 newAttrs ws1 endOp newChildren closing]
                       )
                     )
               else Ok [toHTMLNode newOutput]
            _ -> Ok [toHTMLNode newOutput]
          in
          newInputElems |>LensLess.Results.andThen (\\newInputElem ->
            --Debug.start (\"newInputElem:\" + toString newInputElem) <| \\_ ->
            case Update.diff inputElem newInputElem of
              Err msg -> Err msg
              Ok maybeDiff ->
                let newRevDiffs = case maybeDiff of
                  Nothing -> revDiffs
                  Just v -> (index, ListElemUpdate v)::revDiffs in
                Ok [ (newInputElem::revAcc, newRevDiffs, inputRemaining) ]
          )

        onRemove (revAcc, revDiffs, input) {oldOutput, index} =
          let _::remainingInput = input in
          Ok [(revAcc, (index, ListElemDelete 1)::revDiffs, remainingInput)]

        onInsert (revAcc, revDiffs, input) {newOutput, index} =
          Ok [(toHTMLNode newOutput :: revAcc, (index, ListElemInsert 1)::revDiffs, input)]

        onFinish (revAcc, revDiffs, _) =
          Ok [(reverse revAcc, reverse revDiffs)]

        onGather (acc, diffs) =
          Ok (InputWithDiff (acc, if len diffs == 0 then Nothing else Just (VListDiffs diffs)))
      } oldOutput newOutput diffs
    in mergeNodes input oldOutput newOutput diffs
}.apply (parseHTML string)


--------------------------------------------------------------------------------
-- Html module

Html = {
  -- Returns a list of one text element from a string, and updates by taking all the pasted text.
  text = Update.lens {
    apply s = [[\"TEXT\", s]]
    update {output} =
      let textOf = case of
        [\"TEXT\", s]::tail -> s + textOf tail
        [tag, attrs, children]::tail ->
          textOf children + textOf tail
        _ -> \"\"
      in
      Ok (Inputs [textOf output])
  }

  textNode text = [\"TEXT\", text]

  (textElementHelper) tag styles attrs textContent =
    [ tag,  [\"style\", styles] :: attrs , text textContent ]

  p = textElementHelper \"p\"
  th = textElementHelper \"th\"
  td = textElementHelper \"td\"
  h1 = textElementHelper \"h1\"
  h2 = textElementHelper \"h2\"
  h3 = textElementHelper \"h3\"
  h4 = textElementHelper \"h4\"
  h5 = textElementHelper \"h5\"
  h6 = textElementHelper \"h6\"
  pre = textElementHelper \"pre\"

  (elementHelper) tag styles attrs children =
    [ tag,  [\"style\", styles] :: attrs , children ]

  div = elementHelper \"div\"
  tr = elementHelper \"tr\"
  table = elementHelper \"table\"
  span = elementHelper \"span\"
  b= elementHelper \"b\"
  i= elementHelper \"i\"
  li = elementHelper \"li\"
  ul = elementHelper \"ul\"
  br = [\"br\", [], []]

  element = elementHelper

  parse = html
  parseViaEval = htmlViaEval

  freshTag = Update.lens {
    apply x = \"dummy\" + toString (getCurrentTime x)
    update {input} = Ok (InputsWithDiffs [(input, Nothing)]) }

  integerRefresh i node = [\"\"\"span@i\"\"\", [], [node]]

  forceRefresh node = [freshTag True, [], [node]]

  (option) value selected content =
    <option v=value @(
      Update.bijection
        (\\selected -> if selected then [[\"selected\",\"selected\"]] else [])
        ((==) []) selected
    )>@content</option>

  select attributes strArray index =
    let options = List.indexedMap (\\i opt ->
        option (toString i) (i == index) [[\"TEXT\", opt]]
      ) strArray
    in
    <select selected-index=(toString index)
      onchange=\"this.setAttribute('selected-index', this.selectedIndex)\" @attributes>@options</select>

  checkbox text title isChecked =
      let id = \"checkbox-\"+Regex.replace \"[^\\\\w]\" \"\" text in
      [\"span\", [], [
        [\"label\", [[\"class\",\"switch\"],[\"title\", title]], [
          [\"input\", [\"type\",\"checkbox\"] :: [\"id\", id] :: [\"onclick\",\"if(this.checked) { this.setAttribute('checked', ''); } else { this.removeAttribute('checked') }\"]::
            { apply checked =
                if checked then [[\"checked\", \"\"]] else []
              update {newOutput} = case newOutput of
                [] -> Ok (Inputs [ False ])
                _ ->  Ok (Inputs [ True ])
            }.apply isChecked,
            [[\"span\", [[\"class\", \"slider round\"]], []]]
          ]
        ]],
        [\"label\", [[\"for\",id], [\"title\", title]], [[\"TEXT\", text]]]
      ]]

  -- Do not use if the view DOM is not totally re-rendered.
  onChangeAttribute model controller =
    Update.lens {
      apply model = \"\"
      update {input, outputNew} = Ok (Inputs [controller input outputNew])
    } model

  onClickCallback model controller =  Update.lens {
    apply model = \"\"\"/*@getCurrentTime*/this.setAttribute('onclick', \" \" + this.getAttribute('onclick'))\"\"\"
    update {input, outputNew} =
      if String.take 1 outputNew == \" \" then
      Ok (Inputs [controller model])
      else
      Ok (InputsWithDiffs [(input, Nothing)])
    } model
  
  do = onClickCallback

  button name title model controller =
    <button title=title onclick=(onClickCallback model controller)>@name</button>

  input tpe value =
    <input type=tpe value=value v=value onchange=\"this.setAttribute('v', this.value)\">

  observeCopyValueToAttribute query attribute =
    <script>
      function handleMutation(mutations) {
        mutations.forEach(function(mutation) {
          mutation.target.value = mutation.target.getAttribute(\"@attribute\");
        }) }
      var textAreasObserver = new MutationObserver(handleMutation);
      var textAreas = document.querySelectorAll(@query);
      for (i = 0; i &lt; textAreas.length; i++)
        textAreasObserver.observe(textAreas[i], {attributes: true});
    </script>

  -- Takes a 1-element node list, and whatever this element is replaced by.
  -- In the reverse direction, modifies the element but also propagates its deletion to the list
  -- or insertions of new elements.
  mergeMatch: String -> Diffs -> Match -> String
  (mergeMatch) originalMatch mDiffs m =
     case mDiffs of
       VRecordDiffs dDiffs ->
         let matchLength = String.length originalMatch in
         let gatherDiffs groups starts groupDiffs =
           let combineDiffs i groupDiffs accDiffs =
             case groupDiffs of
               [] -> accDiffs
               (j, d)::tdgd ->
                 if i < j then combineDiffs (i + 1) groupDiffs accDiffs
                 else case d of
                     ListElemUpdate ud ->
                       let startInMatch = nth starts i - m.index in
                       case ud of
                         VStringDiffs sds ->
                           let (_, concreteUpdates) =  List.foldl (\\(StringUpdate start end replacement) (offset, accDiffs) ->
                               (offset + replacement - (end - start),
                                accDiffs ++ [ConcreteUpdate
                                               (start + startInMatch)
                                               (end + startInMatch)
                                               (String.substring (start + offset) (start + replacement + offset) (nth groups i))
                                            ])
                             ) (0, []) sds
                           in combineDiffs (i + 1) tdgd (accDiffs ++ concreteUpdates)
                         _ -> error (\"Expected string diffs, got \" + toString ud)
                     ListElemInsert _ -> error \"Not possible ot insert match groups\"
                     ListElemDelete _ -> error \"Not possible to delete match groups\"
           in
           combineDiffs 0 groupDiffs []
         in
         let totalUpdates = (case dDiffs of
           { group = VListDiffs groupDiffs} ->
             gatherDiffs m.group m.start groupDiffs
           _ -> []) ++ (case dDiffs of
           { submatches = VListDiffs groupDiffs} ->
             gatherDiffs m.submatches (List.drop 1 m.start) groupDiffs
           _ -> []) ++ (case dDiffs of
           {match = matchDiffs} ->
             gatherDiffs [m.match] [m.index] [(0, ListElemUpdate matchDiffs)]
           _ -> [])
         in
         let concreteUpdates = totalUpdates |>
              sortBy (\\(ConcreteUpdate s1 _ _) (ConcreteUpdate s2 _ _) -> s1 <= s2) |>
              List.foldl (\\((ConcreteUpdate start end replacement) as a) (accConcreteUpdates, minStartIndex) ->
                if start >= minStartIndex && end <= matchLength then
                  (a::accConcreteUpdates, end)
                else
                  (accConcreteUpdates, minStartIndex)
                ) ([], 0) |>
              Tuple.first
         in
         List.foldl (\\(ConcreteUpdate start end replacement) match ->
           String.take start match + replacement + String.drop end match)
             originalMatch concreteUpdates

  -- Split the text into Text [[\"TEXT\", unchanged text]] and Match (match)
  -- On the other direction, recombines the nodes of Text and Match
  -- Allows to back-propagate insertions and deletions of nodes
  findAugmentedInterleavings number regex ([[\"TEXT\", text]] as tNodes) =
    let interleavings = findInterleavings number regex text in
    let rev = List.foldl (\\elem acc -> case elem of
        Left text -> Text [[\"TEXT\", text]] :: acc
        Right match -> Match match :: acc
      ) [] interleavings
    in
    let augmentedInterleavings = List.reverse rev in
    let allMatches: List (Int, Match)
        allMatches = List.concatMap (case of Match match -> [(match.number, match)]; _ -> []) augmentedInterleavings in
    Update.lens {
      apply tNodes = augmentedInterleavings
      update ({outputNew, diffs} as uInput) = -- We should extract all the new matches
        case diffs of
          VListDiffs ds ->
            let rebuild i outputNew ds newInput =
               case ds of
                 [] ->
                   case outputNew of
                     [] ->
                       Ok (Inputs [__mergeHtmlText__ newInput])
                     Text nodes :: outputNewTail ->
                       newInput ++ nodes |>
                       rebuild (i + 1) outputNewTail ds
                     Match m :: outputNewTail ->
                       newInput ++ [[\"TEXT\", m.match]] |>
                       rebuild (i + 1) outputNewTail ds
                 (j, ld)::tds ->
                   if i < j then
                     case outputNew of
                     Text nodes :: outputNewTail ->
                       newInput ++ nodes |>
                       rebuild (i + 1) outputNewTail ds
                     Match m :: outputNewTail ->
                       newInput ++ [[\"TEXT\", m.match]] |>
                       rebuild (i + 1) outputNewTail ds
                     [] ->
                       Err (\"Unexpected end of findAugmentedInterleavings.outputNew\" + toString uInput)
                   else case ld of
                     ListElemUpdate u  ->
                       case outputNew of
                         Text nodes :: outputNewTail ->
                           newInput ++ nodes |>
                           rebuild (i + 1) outputNewTail tds
                         Match m :: outputNewTail ->
                           case u of
                             VRecordDiffs {args = VRecordDiffs { _1 = mDiffs } } ->
                               let originalMatch = (listDict.get m.number allMatches |> Maybe.withDefault m).match in
                               newInput ++ [[\"TEXT\", mergeMatch originalMatch mDiffs m]] |>
                               rebuild (i + 1) outputNewTail tds
                             _ -> Err (\"Unexpected findAugmentedInterleavings diffs: \" + toString u)
                         [] ->
                           Err (\"Unexpected end of findAugmentedInterleavings.outputNew\" + toString uInput)

                     ListElemInsert n  ->
                       let tailDiffs = if n == 1 then tds else (j, ListElemInsert (n - 1)) :: tds in
                       case outputNew of
                         [] ->
                           Err (\"Expected inserted elements, got nothing\")
                         Text nodes :: outputNewTail ->
                           newInput ++ nodes |>
                           rebuild i outputNewTail tailDiffs
                         Match m :: outputNewTail ->
                           newInput ++ [[\"TEXT\", m.match]] |>
                           rebuild i outputNewTail tailDiffs

                     ListElemDelete n  ->
                       rebuild (i + n) outputNew tds newInput
            in
              rebuild 0 outputNew ds []
          _ -> Err (\"Expected VListDiffs, got \" + toString diffs)
    } tNodes

  -- given a 1-element list and an element, returns the element wrapped
  -- On the way back, back-propagates insertions and deletions to the list after applying the inverse
  -- convertInserted,
  -- and changes to element itself on the other side
  insertionDeletionUpdatesTo: (b -> a) -> (List a, b) -> List b
  insertionDeletionUpdatesTo convertInserted = Update.lens2 {
      apply (node1List, node1) = [node1]
      update {input=(node1List, node1), outputNew=newNodes, diffs} =
        case diffs of
          VListDiffs listDiffs ->
            let aux i newNodes listDiffs newNode1List newNode1ListDiffs newNode1 mbNode1Diffs =
              case listDiffs of
                [] ->
                  Ok (InputsWithDiffs [(
                    (newNode1List, newNode1),
                    Update.pairDiff2
                      (if newNode1ListDiffs == [] then
                        Nothing
                       else Just (VListDiffs newNode1ListDiffs))
                      mbNode1Diffs
                  )])
                (j, d)::diffTail ->
                  if i < j then -- i == 0 and j == 1
                    let (skipped, remainingNodes) = List.split 1 newNodes in
                    aux j remainingNodes listDiffs (newNode1List ++ skipped) newNode1ListDiffs newNode1 mbNode1Diffs
                  else
                    case d of
                      ListElemInsert x ->
                        let (inserted, remainingNodes) = List.split x newNodes in
                        let insertionDiffs = newNode1ListDiffs ++ [(j, ListElemInsert x)] in
                        aux j remainingNodes diffTail
                          (newNode1List ++ convertInserted inserted) insertionDiffs newNode1 mbNode1Diffs

                      ListElemUpdate x ->
                        let newNode1_ :: remainingNodes = newNodes in
                        aux (j + 1) remainingNodes diffTail
                          (newNode1List ++ node1List) newNode1ListDiffs newNode1_ (Just x)

                      ListElemDelete 1 -> -- Only the element can be deleted.
                        let deletionDiffs = newNode1ListDiffs ++ [(0, ListElemDelete 1)] in
                        aux (j + 1) newNodes diffTail
                          newNode1List deletionDiffs node1 Nothing

                      _ -> Err <| \"Unexpected diff for insertionDeletionUpdatesTo:\" ++ toString d
            in aux 0 newNodes listDiffs [] [] node1 Nothing
          _ -> Err <| \"Expected VListDiffs for insertionDeletionUpdatesTo, got \" ++ toString diffs
      }

  -- Takes a list of nodes, returns a list of nodes.
  replaceNodesIf nodePred regex replacement nodes =
      List.concatMap_ identity (\\[node] as node1List ->
        if not (nodePred node) then node1List else
        case node of
          [\"TEXT\", text] ->
            findAugmentedInterleavings 0 regex node1List
              |> List.concatMap_ identity (\\[head] as headList ->
              case head of
                Text nodes -> nodes
                Match match -> replacement match
            ) |> __mergeHtmlText__ |> List.filter (/= [\"TEXT\", \"\"])
          [tag, attrs, children] ->
            insertionDeletionUpdatesTo identity node1List <|
              [tag, attrs, replaceNodesIf nodePred regex replacement children]
      ) nodes

  -- Takes a list of nodes, returns a list of nodes.
  replaceNodes = replaceNodesIf (\\_ -> True)

  {- Given
     * a regex
     * a replacement function that takes a string match and returns a list of Html nodes
     * a node
     This functions returns a node, (excepted if the top-level node is a [\"TEXT\", _] and is splitted.
  -}
  replaceIf nodePred regex replacement node = case replaceNodesIf nodePred regex replacement [node] of
       [x] -> x
       y -> y

  replace  = replaceIf (\\_ -> True)

  replaceNodesAsTextIf nodePred regex replacement nodes =
       if nodes == [] then nodes else
       let nodesAsText = nodes |> List.indexedMapWithReverse identity (\\i n -> case n of
         [\"TEXT\", t] -> n
         [tag, _, _] -> [\"TEXT\", \"\"\"<|#@i#@tag#|>\"\"\"]) |> __mergeHtmlText__
       in
       -- Takes a list of nodes, and replaces each <|(number)|> by the matching node in the top-level text nodes.
       -- Calls replaceNodesAsTextIf on the result.
       let reinsertNodes insideNodes = replaceNodesIf (\\_ -> True) \"\"\"<\\|#(\\d+)#\\w+#\\|>\"\"\" (\\m ->
          Update.sizeFreeze [nth nodes (String.toInt (nth m.group 1))]) insideNodes in
       let reinsertNodesInText text = reinsertNodes [[\"TEXT\", text]] in
       -- Takes a string and replaces  each <|(number)|> by the matching node in the top-level text nodes.
       let reinsertNodesInText text = reinsertNodes [[\"TEXT\", text]] in
       -- Takes a string and replace each <|(number)|> by the node in raw format (i.e. printed as HTML)
       let reinsertNodesRaw insideNodes = replaceNodesIf (\\_ -> True) \"\"\"<\\|#(\\d+)#\\w+#\\|>\"\"\" (\\m ->
         let oldNode = nth nodes (String.toInt (nth m.group 1)) in
         [[\"TEXT\", valToHTMLSource oldNode]]
       ) insideNodes in
       let reinsertNodesRawInText text = reinsertNodesRaw [[\"TEXT\", text]] in
       nodesAsText |>
       findAugmentedInterleavings 0 regex |>
       List.concatMap (\\head ->
         case head of
           Text nodes -> reinsertNodes nodes
           Match m ->  reinsertNodes (replacement { m | reinsertNodesRawInText = reinsertNodesRawInText})
       ) |> __mergeHtmlText__ |>
       List.filter (\\a -> a /= [\"TEXT\", \"\"]) |>
       List.mapWithReverse identity (\\node -> case node of
         [\"TEXT\", _] -> node
         [tag, attrs, children] ->
           if nodePred node then [tag, attrs,
             replaceNodesAsTextIf nodePred regex replacement children] else node)

  replaceNodesAsText regex replacement nodes = replaceNodesAsTextIf (\\_ -> True) regex replacement nodes

  replaceAsTextIf nodePred regex replacement node = case replaceNodesAsTextIf nodePred regex replacement [node] of
        [x] -> x
        y -> y

  replaceAsText = replaceAsTextIf (\\_ -> True)

  find regex node =
    let aux node = case node of
       [\"TEXT\", text] ->
         findInterleavings 0 regex text
         |> List.concatMap (case of
            Left _ -> []
            Right match -> [match]
          )
       [tag, attrs, children] ->
         List.concatMap aux children
    in aux node

  -- Takes a regex, a function that accepts a match and an accumulator and returns a list of nodes and the new accumulator's value.
  -- a starting accumulator and a starting node. Returns the final accumulator and the final node.
  foldAndReplace regex matchAccToNewNodesNewAcc startAcc node =
          let aux acc node = case node of
             [\"TEXT\", text] ->
               findInterleavings 0 regex text
               |> List.foldl (\\interleaving (nodes, acc) ->
                 case interleaving of
                  Left str -> (nodes ++ [[\"TEXT\", str]], acc)
                  Right match -> let (newNodes, newAcc) = matchAccToNewNodesNewAcc match acc in
                    (nodes ++ newNodes, newAcc)
                ) ([], acc) |> Tuple.mapFirst (__mergeHtmlText__ >> List.filter (/= [\"TEXT\", \"\"]))
             [tag, attrs, children] ->
               let (newChildren, newAcc) =
                 List.foldl (\\child (buildingChildren, acc) ->
                   let (nChildren, nAcc) = aux acc child in
                   (buildingChildren ++ nChildren, nAcc)
                 ) ([], acc) children in
               ([[tag, attrs, newChildren]], newAcc)
          in case aux startAcc node of
            ([node], acc)-> (node, acc)
            r -> r

  isEmptyText = case of
        [\"TEXT\", x] -> Regex.matchIn \"^\\\\s*$\" x
        _ -> False

  filter pred elemList =
        List.concatMap (case of
          [tagName, attrs, children] as elem ->
            if pred elem then [[tagName, attrs, filter pred children]]
            else []
          elem -> if pred elem then [elem] else []) elemList

  translate =
    let freshVarName name i dictionary =
      if name == \"\" then freshVarName \"translation\" i dictionary else
      let suffix = if i == 0 then \"\" else toString i in
      let potentialName = name + suffix in
      if Dict.member potentialName dictionary then freshVarName name (i + 1) dictionary else potentialName
    in
    \\translations indexLangue node ->
    let currentTranslation = nth translations indexLangue |> Tuple.second |> Dict.fromList in
    replace \"\"\"\\$(\\w+|\\$)\"\"\" (\\m ->
      if m.match == \"$\" then [[\"TEXT\", m.match]] else
        let key = nth m.group 1 in
        case Dict.get key currentTranslation of
          Nothing -> [[\"TEXT\", m.match]]
          Just definition -> [[\"TEXT\", definition]]
      ) node |>
    \\htmlNode ->
      Update.lens2 {
        apply (htmlNode, _) = htmlNode
        update {input = (_, translations), outputNew=newHtmlNode} =
          find \"\"\"\\{:([^\\}]*(?!\\})\\S[^\\}]*):\\}\"\"\" newHtmlNode
          |> List.foldl (\\matchToTranslate (updatedHtmlNode, currentTranslation, translations) ->
              let definition = nth matchToTranslate.group 1
                  name = freshVarName (Regex.replace \"[^a-zA-Z]\" \"\" definition |> String.take 16) 0 currentTranslation
                  textToFind = \"\"\"\\{:@(Regex.escape definition):\\}\"\"\"
              in
              (replace textToFind (\\_ -> [[\"TEXT\", \"$\" + name]]) updatedHtmlNode,
               Dict.insert name definition currentTranslation,
               List.map (\\(lang, d) -> (lang, d ++ [(name, definition)])) translations)
            ) (newHtmlNode, currentTranslation, translations)
          |> \\(finalHtmlNode, _, newTranslations) ->
            Ok (Inputs [(finalHtmlNode, newTranslations)])
      } htmlNode translations

  markdown node =
      let
          regexFootnotes = \"\"\"\\r?\\n\\[\\^([^\\]]+)\\]:\\s*((?:(?!\\r?\\n\\r?\\n)[\\s\\S])+)\"\"\"
          regexReferences = \"\"\"\\r?\\n\\[(?!\\^)([^\\]\\\\]+)\\]:\\s*(\\S+)\"\"\"
          footnotes = find regexFootnotes node
                       |> List.map (\\m -> (nth m.group 1, nth m.group 2))
                       |> List.indexedMap (\\i (name, value) -> (name, (i + 1, value)))
          references = find regexReferences node
                       |> List.map (\\m -> (nth m.group 1, nth m.group 2))
          notCode = case of [\"code\", _, _] -> False; _ -> True
          notTitle = case of [tag, _, _] -> not (Regex.matchIn \"\"\"h\\d\"\"\" tag); _ -> True
          notList = case of [tag, _, _] -> tag /= \"ul\" && tag /= \"ol\"; _ -> True
          notPara = case of [\"p\", _, _] -> False; _ -> True
          notA = case of [\"a\", _, _] -> False; _ -> True
          r: String -> (Match -> List HtmlNode) -> HtmlNode -> HtmlNode
          r  = replaceAsTextIf notCode
          r2 = replaceAsTextIf (\\x -> notCode x && notTitle x && notList x && notPara x)
          ra = replaceAsTextIf (\\x -> notCode x && notA x)
          lregex = \"\"\"(?:\\r?\\n|^)((?:(?![\\r\\n])\\s)*)(\\*|-|\\d+\\.)(\\s+)((?:.*)(?:\\r?\\n\\1  ?\\3(?:.*))*(?:\\r?\\n\\1(?:\\*|-|\\d+\\.)\\3(?:.*)(?:\\r?\\n\\1 \\3(?:.*))*)*)\"\"\"
          handleLists node  =
            r lregex (
              \\m -> let indent = nth m.group 1
                        afterindent = nth m.group 3
                        ul_ol = case nth m.group 2 of \"*\" -> \"ul\"; \"-\" -> \"ul\"; _ -> \"ol\"
                        elements =
                          Regex.split \"\"\"\\r?\\n@indent(?:\\*|-|\\d+\\.)@afterindent\"\"\" (nth m.group 4)
                    in
                    [<@ul_ol>@(List.map (\\elem -> <li>@elem</li>) elements)</@>]) node
      in (
      node
      |> r \"\"\"@regexReferences|@regexFootnotes\"\"\" (\\m -> [])
      |> (\\result -> -- Expand footnotes
        if List.length footnotes == 0 then result
        else case result of
          [tag, attrs, children] ->
            [tag, attrs, children ++ Update.sizeFreeze [
              <div class=\"footnotes\"><hr><ol>@(footnotes |>
                List.map (\\(name, (n, value)) ->
                  <li id=\"\"\"fn@n\"\"\"><p>@value<a href=\"\"\"#fnref@n\"\"\">↩</a></p></li>
                ))</ol></div>]
            ])
      |> r \"\"\"(```)([\\s\\S]*?)\\1(?!`)|((?:\\r?\\n    .*)+)\"\"\" (\\m ->
        if nth m.group 1 == \"\" then
          nth m.group 3 |>
          Regex.extract \"\"\"\\r?\\n    ([\\s\\S]*)\"\"\" |>
          Maybe.map (\\[code] ->
                  [<pre><code>@(Regex.split \"\"\"\\r?\\n    \"\"\" code |> String.join \"\\n\" |> String.trim |> m.reinsertNodesRawInText)</code></pre>])
          |> Maybe.withDefault [[\"TEXT\", m.match]]
        else [
        <pre><code>@(nth m.group 2 |> String.trim |> m.reinsertNodesRawInText)</code></pre>])
      |> r \"\"\"(^|\\r?\\n)(#+)\\s*([^\\r\\n]*)\"\"\" (\\m -> [[\"TEXT\", nth m.group 1], <@(\"\"\"h@(String.length (nth m.group 2))\"\"\")>@(nth m.group 3)</@>])
      |> handleLists --|> (\\x -> let _ = Debug.log (\"Paragraph phase\") () in x)
      |> r2 \"\"\"(\\r?\\n *\\r?\\n(?:\\\\noindent\\r?\\n)?|^)((?=\\s*\\w|\\S)[\\s\\S]*?)(?=(\\r?\\n *\\r?\\n|\\r?\\n$|$))\"\"\" (
        \\m ->
          --let _ = Debug.log m.match () in
          if nth m.group 1 == \"\" && nth m.group 3 == \"\" -- titles and images should not be paragraphs.
           || Regex.matchIn \"\"\"^\\s*<\\|#\\d+#(?:h\\d|ul|ol|p|pre|center)#\\|>\\s*$\"\"\" (nth m.group 2) then [[\"TEXT\", m.match]] else  [<p>@(nth m.group 2)</p>]) --|> (\\x -> let _ = Debug.log (\"End of paragraph phase:\" + valToHTMLSource x) () in x)
      |> ra \"\"\"\\[([^\\]\\\\]+)\\](\\^?)(\\(|\\[)([^\\)\\]]+)(\\)|\\])|(?:http|ftp|https)://(?:[\\w_-]+(?:(?:\\.[\\w_-]+)+))(?:[\\w.,@@?^=%&:/~+#-]*[\\w@@?^=%&/~+#-])?\"\"\" (\\m -> [ -- Direct and indirect References + syntax ^ to open in external page.
        case nth m.group 3 of
          \"(\" -> <a href=(nth m.group 4) @(if nth m.group 2 == \"^\" then [[\"target\", \"_blank\"]] else [])>@(nth m.group 1)</a>
          \"[\" -> listDict.get (nth m.group 4) references |> case of
                Just link -> <a href=link>@(nth m.group 1)</a>
                Nothing -> [\"TEXT\", m.match]
          _ -> <a href=m.match>@(m.match)</a>
          ])
      |> r \"\"\"\\[\\^([^\\]]+)\\]\"\"\" (\\m ->  -- Footnotes
        listDict.get (nth m.group 1) footnotes |> case of
          Just (n, key) -> [ <a href=\"\"\"#fn@n\"\"\" class=\"footnoteRef\" id=\"\"\"fnref@n\"\"\"><sup>@n</sup></a>]
          Nothing -> [[\"TEXT\", m.match]])
      |> r \"(`)(?=[^\\\\s`])(.*?)\\\\1\" (\\m -> [<code>@(nth m.group 2 |> m.reinsertNodesRawInText)</code>])
      |> r \"\"\"(\\*{1,3}|_{1,3})(?=[^\\s\\*_])((?:(?!\\\\\\*|\\_).)*?)\\1\"\"\" (\\m -> [
        case nth m.group 1 |> String.length of
          1 -> <em>@(nth m.group 2)</em>
          2 -> <strong>@(nth m.group 2)</strong>
          _ -> <em><strong>@(nth m.group 2)</strong></em>])
      |> r \"\"\"&mdash;|\\\\\\*|\\\\_|\\\\\\[|\\\\\\]\"\"\" (\\m -> [[\"TEXT\", case m.match of
        \"&mdash;\" -> \"—\"
        \"\\\\*\" -> String.drop 1 m.match
        \"\\\\_\" -> String.drop 1 m.match
        \"\\\\[\" -> String.drop 1 m.match
        \"\\\\]\" -> String.drop 1 m.match
        ]])
      )

  scriptFindEnclosing tagName varnameWhatToDo = \"\"\"
      var elem = this
      while(elem != null && elem.tagName.toLowerCase() != \"@tagName\")
        elem = elem.parentElement;
      if(elem == null) {
        console.log('Error: duplicate button could not find enclosing target @tagName');
      } else {
        @(varnameWhatToDo \"elem\")
      }\"\"\"

  buttonToDuplicateEnclosing tagNameToDuplicate attrs children =
      <button onclick=(scriptFindEnclosing(tagNameToDuplicate)(\\v ->
          \"\"\"@(v).parentElement.insertBefore(@(v).cloneNode(true), @(v))\"\"\"))
      @attrs>@children</button>

  buttonToDeleteEnclosing tagNameToDelete attrs children=
      <button onclick=(scriptFindEnclosing(tagNameToDelete)(\\v ->
          \"\"\"@(v).remove()\"\"\"))
      @attrs>@children</button>

  addClass name [t, attrs, c] =
    [ t
    , attrDict.update \"class\" (case of
        Just classes -> if Regex.matchIn (\"\\\\b\" + name + \"\\\\b\") classes then Just classes else Just (classes + \" \" + name)
        Nothing -> Just name) attrs
    , c]
}

--------------------------------------------------------------------------------
-- Javascript

jsCode = {
  -- Create a tuple of a list of strings. Uses v as intermediate variable.
  -- calling __jsEval__ (\"var a = 1\\n\" ++ tupleOf \"x\" [\"a\", \"2\", \"a\"]) == (1, 2, 1)
  tupleOf: String -> List String -> String
  tupleOf v list =
    \"\"\"(function() { var @v = {@(List.indexedMap (\\i x -> \"\"\"_@(i+1): @x\"\"\") list |> String.join \",\")}; @v['$t_ctor']='Tuple@(List.length list)'; return @v})()\"\"\"

  -- Creates a datatype of a list of strings. Uses v as intermediate variable.
  datatypeOf: String -> String -> List String -> String
  datatypeOf v kind args =
    \"\"\"(function() { var @v = {args: {@(List.indexedMap (\\i x -> \"\"\"_@(i+1): @x\"\"\") args |> String.join \",\")}}; @v['$d_ctor']='@kind'; return @v})()\"\"\"

  -- JS representation of a given string
  stringOf: String -> String
  stringOf content =
    \"\\\"\" + Regex.replace \"\\\\\\\\|\\\"|\\r|\\n|\\t\" (\\m -> case m.match of
      \"\\r\" -> \"\\\\r\"
      \"\\n\" -> \"\\\\n\"
      \"\\t\" -> \"\\\\t\"
      x -> \"\\\\\" + x) content + \"\\\"\"
}


nodejs = {
  (nothingJs) = jsCode.datatypeOf \"x\" \"Nothing\" []
  (justJs) arg = jsCode.datatypeOf \"x\" \"Just\" [arg]
  (fsjs) defaultValue prog = __jsEval__ \"\"\"
    (function() {
      if(!require) return @defaultValue;
      const fs = require(\"fs\");
      if(!fs) return @defaultValue;
      @prog
    })()\"\"\"

  type alias BasicFileSystemUtils = {
    read: String -> Maybe String,
    listdir: String -> List String,
    isdir: String -> Bool,
    isfile: String -> Bool
  }
  -- Reads the file system directly without instrumentation.
  nodeFS: BasicFileSystemUtils
  nodeFS = {
    read  name = fsjs nothingJs \"\"\"
      const name = @(jsCode.stringOf name);
      if(!fs) return @nothingJs;
      if(fs.existsSync(name)) { // TODO: Atomic read
        if(fs.lstatSync(name).isDirectory()) {
          return @nothingJs;
        } else {
          return @(justJs \"fs.readFileSync(name, 'utf-8')\");
        }
      } else return @nothingJs;\"\"\"
    listdir foldername = fsjs \"[]\" \"\"\"
      var name = @(jsCode.stringOf foldername);
      if(name == \"\") name = \".\";
      if(fs.existsSync(name) && fs.lstatSync(name).isDirectory()) {
        var filesfolders =
          fs.readdirSync(name);
        return filesfolders;
      } else {
        return []
      }\"\"\"
    isdir name =
      fsjs \"false\" \"\"\"
       const name = @(jsCode.stringOf name);
       return name == \"\" || fs.existsSync(name) && fs.lstatSync(@(jsCode.stringOf name)).isDirectory();\"\"\"
    isfile name =
      fsjs \"false\" \"\"\"
       return fs.existsSync(@(jsCode.stringOf name)) && fs.lstatSync(@(jsCode.stringOf name)).isFile();\"\"\"
  }


  type FileOperation = Write {-old-} String {-new-} String Diffs |
                       Rename {-newName-} String |
                       Create {-content-} String |
                       Delete |
                       CreateFolder (List String {- file names in this folder -})
  type alias ListFileOperations = List ({-filename-}String, FileOperation)

  -- Consumes all update values by writing to the file system. Use this only if you don't need to display ambiguity.
  nodeFSWrite: ListFileOperations
  nodeFSWrite = Update.lens {
    apply = identity
    update {outputNew} =
     List.foldl (\\(name, action) b ->
      let write name contentOld content =
        let mbCreateDir = case contentOld of
             Nothing -> fsjs \"0\" \"\"\"
               var pathToFile = @(jsCode.stringOf name);
               var filePathSplit = pathToFile.split('/');
               var dirName = \"\";
               for (var index = 0; index < filePathSplit.length - 1; index++) {
                  dirName += filePathSplit[index]+'/';
                  if (!fs.existsSync(dirName))
                      fs.mkdirSync(dirName);
               }
               return 1;\"\"\" -- Create the file's directory structure.
             Just oldContent -> \"\"
            written = fsjs \"0\" \"\"\"
              fs.writeFileSync(@(jsCode.stringOf name), @(jsCode.stringOf content), \"utf-8\");
              return 1;\"\"\"
        in ()
      in
      let _ = case action of
        Write oldContent newContent diffs -> write name (Just oldcontent) newContent
        Create content -> write name Nothing content
        Delete -> fsjs \"0\" \"\"\"
            fs.unlinkSync(@(jsCode.stringOf name));
            return 1;\"\"\"
        Rename newName -> fsjs \"0\" \"\"\"
            fs.renameSync(@(jsCode.stringOf name), @(jsCode.stringOf newName));
            return 1;\"\"\"
      in b) (Ok <| InputsWithDiffs [([], Nothing)]) outputNew
  } []

  type alias InlineFS = List ({-path-}String, File content | Folder (List String)) -> BasicFileSystemUtils

  inlineFS: InlineFS -> BasicFileSystemUtils
  inlineFS content = {
    read name = case listDict.get name content of
       Just (File content) -> Just content
       _ -> Nothing
    listdir foldername = case listDict.get foldername content of
       Just (Folder files) -> files
       _ -> Nothing
    isdir name = case listDict.get name content of
      Just (Folder _) -> True
      _ -> False
    isfile name = case listDict.get name content of
      Just (File _) -> True
      _ -> False
  }

  inlineFSWrite: InlineFS -> ListFileOperations
  inlineFSWrite = Update.lens {
    apply inlineFS = []
    update {input=inlineFS, outputNew} =
      List.foldl (\\(name, action) resInlineFS ->
        resInlineFS
        |> Result.andThen (\\inlineFS ->
          let prev = listDict.get name inlineFS in
          case (prev, action) of
             (Just (Folder _), Write oldContent newContent diffs) -> Err <| \"Can't write a folder as if it was a file\"
             (_,               Write oldContent newContent diffs) -> Ok <| listDict.insert name (File newContent) inlineFS
             (Nothing, Delete) -> Err <| \"Can't delete \" + name + \" from file system because it did not exist\"
             (_, Delete) -> listDict.delete name inlineFS |>
                List.map (\\(oname, ocontent) ->
                  case ocontent of
                  Folder subfiles ->
                      (oname, Folder (List.filter (\\subfile -> oname + \"/\" + subfile /= name) subfiles))
                  _ -> (oname, ocontent)
                  ) |> Ok
             (Just x, Create content) ->  Err <| \"Can't create \" + name + \" in file system because it did exist\"
             (_, Create newContent) ->  listDict.insert name (File newContent) inlineFS |>
               List.map (\\(oname, ocontent) ->
                  case ocontent of
                  Folder subfiles ->
                      if String.left (String.length oname) name == oname then -- Starts with the folder's name
                        let remaining = String.drop (String.length oname) name in
                        if not (Regex.matchIn \"./\" remaining) && String.take 1 remaining == \"/\"  then
                          (oname, Folder (subfiles ++ [String.drop 1 remaining]))
                        else
                          (oname, Folder subfiles)
                      else
                        (oname, Folder subfiles)
                  _ -> (oname, ocontent)
                  ) |> Ok
             (Just x, Rename newName) -> case listDict.get newName inlineFS of
               Just _ -> Err <| \"Can't rename \" + name + \" to \" + newName + \" because it already exists\"
               _ -> inlineFS |> listDict.remove name |> listDict.insert newName x |>
                    List.map (\\(oname, ocontent) ->
                      case ocontent of
                      Folder subfiles ->
                          (oname, Folder (List.map (\\subfile -> if oname + \"/\" + subfile == name then
                            String.drop (String.length (oname + \"/\")) newName else subfile)
                          subfiles))
                      _ -> (oname, ocontent)
                      ) |> Ok
             (_, Rename _) -> Err <| \"Can't rename \" + name + \" because it did not exist\"))  (Ok inlineFS) outputNew
      |> Result.map (\\x -> Inputs [x])
  }

  type alias FileSystemUtils =  {
    read: String -> Maybe String,
    listdir: String -> List String,
    listdircontent: String -> List (String, String),
    isdir: String -> Bool,
    isfile: String -> Bool
  }

  {- nodejs.delayedFS basicFS fileOperations   provides a file system utility that, on update,
     does not change the files on disk, but fills an array of fileOperations (create, write, delete, rename)

     basicFS:         To read files from disk using node.js, use `nodejs.nodeFS`
                      To simply read an inline content, which is a
                        list of (path, File content | Folder (list of filenames)),
                        use `(nodjs.inlineFS content)`
     fileOperations:  Pass `[]` to recover file operations on update.
                        If not empty, its operations will overwrite the content of basicFS.
                      Pass `nodejs.nodeFSWrite` if writes do not have ambiguity
                        and should be performed immediately on disk
                      Pass `nodejs.inlineFSWrite content` if the first argument is `nodejs.inlineFS content` and
                        you want the content to be immediately overriden.

     -- Sample call (put fileOperations: [] in the environment)
     fs = nodejs.delayedFS nodejs.nodeFS fileOperations

     -- Sample mock call (to locally test a file system
     fsContent = [(\"test/a.txt\", File \"Hello\"), (\"test/b.txt\", File \"World\"), (\"test\", Folder [\"a.txt\", \"b.txt\"])]
     fs = nodejs.delayedFS (nodejs.inlineFS fsContent) (nodejs.inlineFSWrite fsContent)
     fs.read \"test/a.txt\"
     ...
     -}
  delayedFS: BasicFileSystemUtils -> ListFileOperations -> FileSystemUtils
  delayedFS  basicFS                 fileOperations = {
    (fileOperationsRaw) = Update.lens {
      apply = identity
      (remove) name = List.filter (\\(otherName, _) -> otherName /= name)
      update {input=fileOperations, outputNew} =
        let process outputNew revAcc = case outputNew of
          [] -> Ok <| Inputs [List.reverse revAcc]
          (name, action) :: tail ->
            case action of
              Delete -> process (remove name tail) ((name, action) :: remove name revAcc)
              Create content -> process (remove name tail) ((name, action) :: remove name revAcc)
              Write oldContent newContent diffs ->
                let (sameName, otherNames) = List.partition (\\(otherName, _) -> name == otherName) tail in
                let extractedWrites = List.concatMap (\\(_, action) -> case action of
                     Write oldContent newContent diffs -> [(newContent, Just diffs)]
                     _ -> []) sameName in
                let (finalContent, finalDiffs) =
                      Update.merge oldContent ((newContent, Just diffs) :: extractedWrites)
                in
                let finalAction =
                  case listDict.get name fileOperations of
                    Just (Create content) -> Create finalContent
                    _ -> Write oldContent finalContent finalDiffs
                in
                process otherNames ((name, finalAction) :: revAcc)
              Rename newName ->
                if List.all (\\(_, action) -> case action of Rename _ -> True) tail then
                  process tail ((name, action)::revAcc)
                else
                  process (tail ++ [(name, action)]) revAcc
              CreateFolder content ->
                process tail ((name, action)::revAcc)
        in process outputNew []
    } fileOperations

    read: String -> Maybe String
    read filename = Update.lens {
      apply fileOperations =
        case listDict.get filename fileOperations of
          Just (Write oldContent newContent diffs) -> Just newContent
          Just (Create content) -> Just content
          Just (Delete) -> Nothing
          Just (Rename _) -> Nothing
          Just (CreateFolder _) -> Nothing
          _ -> basicFS.read filename
      update  = case of
        {input=fileOperations, outputOld = Just x , outputNew = Nothing} ->
          Ok <| InputsWithDiffs [((filename, Delete) :: fileOperations, Just <| VListDiffs [(0, ListElemInsert 1)])]
        {input=fileOperations, outputOld = Just oldContent, outputNew = Just newContent, diffs} ->
          let contentDiffs = case diffs of
            VRecordDiffs { args = VRecordDiffs { _1 = d } } -> d
            _ -> VStringDiffs []
          in
           Ok <| InputsWithDiffs [((filename, Write oldContent newContent contentDiffs) :: fileOperations, Just <| VListDiffs [(0, ListElemInsert 1)])]
        {input=fileOperations, outputOld = Nothing, outputNew = Just newContent} ->
           Ok <| InputsWithDiffs [((filename, Create newContent) :: fileOperations, Just <| VListDiffs [(0, ListElemInsert 1)])]
        {input=fileOperations} ->
          Ok (InputsWithDiffs [(fileOperations, Nothing)])
      } fileOperationsRaw

    listdir: String -> List String
    listdir foldername = Update.lens {
      apply fileOperations = case listDict.get foldername fileOperations of
        Just (CreateFolder content) -> content -- We can mock the file system by creating a folder in initial fileOperations
        _ -> basicFS.listdir foldername |>
          (if fileOperations == [] then identity else
          List.filterMap (\\name -> case listDict.get (foldername + \"/\" + name) fileOperations of
            Just Delete -> Nothing
            Just (Rename newName) ->
              let fn = foldername + \"/\" in
              let fnLenth = String.length fn in
              let possibleFolder = String.take fnLenth newName in
              let possibleName = String.drop fnLenth newName in
              if possibleFolder == fn then
                if Regex.matchIn \"/\" possibleName then Nothing
                else if possibleName == \"\" then Nothing
                else Just possibleName
              else Nothing
            _ -> Just name
          ))
      update {outputOld, outputNew, diffs} =
        let aux i outputOld outputNew diffs fo = case diffs of
          [] -> Ok (Inputs [fo])
          (j, d)::diffTail ->
            if i < j then
              aux j (List.drop (j - i) outputOld) (List.drop (j - i) outputNew) diffs fo
            else case d of
              ListElemInsert count -> Err <| \"fs.listdir cannot insert in reverse because it does not know if it should insert files or folders. Use fs.read instead\"

              ListElemDelete count ->
                let (deleted, remaining) = List.split count outputOld in
                fo ++ (List.map (\\nameDeleted -> (foldername + \"/\" + nameDeleted, Delete)) deleted) |>
                aux (i + count) remaining outputNew diffTail

              ListElemUpdate nameChange ->
                case (outputOld, outputNew) of
                  (oldName :: outputOldTail, newName :: outputNewTail) ->
                    fo ++ [(foldername + \"/\" + oldName, Rename (foldername + \"/\" + newName))] |>
                    aux (i + 1) outputOldTail outputNewTail diffTail
              _ -> Err <| \"\"\"Unknown diff for listdir @d\"\"\"
        in case diffs of
          VListDiffs diffs -> aux 0 outputOld outputNew diffs []
          _ -> Err <| \"\"\"Don't know how to handle these list differences for listdircontent : @diffs\"\"\"
    } fileOperations

    listdircontent: String -> List (String, String)
    listdircontent foldername = Update.lens {
      apply fileOperations = listdir foldername |>
          List.filter (\\name -> isfile \"\"\"@foldername/@name\"\"\") |>
          List.map (\\name ->
          let fullname = \"\"\"@foldername/@name\"\"\" in
          (name, read fullname |> Maybe.withDefault (freeze \"\"\"Unknown file @fullname\"\"\")))
      update {outputOld, outputNew, diffs} =
        -- Insertions can be treated as Create, and deletions as Delete.
        -- Changing the first component can be viewed as a Rename, whereas chaging the second component as a Write.
        let aux i outputOld outputNew diffs fo = case diffs of
          [] -> Ok (Inputs [fo])
          (j, d)::diffTail ->
            if i < j then
              aux j (List.drop (j - i) outputOld) (List.drop (j - i) outputNew) diffs fo
            else case d of
              ListElemInsert count ->
                let (inserted, remaining) = List.split count outputNew in
                fo ++ (List.map (\\(nameInserted, contentInserted) -> (foldername + \"/\" + nameInserted, Create contentInserted)) inserted) |>
                aux i outputOld remaining diffTail

              ListElemDelete count ->
                let (deleted, remaining) = List.split count outputOld in
                fo ++ (List.map (\\(nameDeleted, contentInserted) -> (foldername + \"/\" + nameDeleted, Delete)) deleted) |>
                aux (i + count) remaining outputNew diffTail

              ListElemUpdate (VRecordDiffs subd) ->
                case (outputOld, outputNew) of
                  ((oldName, oldContent) :: outputOldTail, (newName, newContent) :: outputNewTail) ->
                    let
                      fo = case subd of
                        {_2=contentChange} -> fo ++ [(foldername + \"/\" + oldName, Write oldContent newContent contentChange)]
                        _ -> fo
                      fo = case subd of
                        {_1=nameChange} -> fo ++ [(foldername + \"/\" + oldName, Rename (foldername + \"/\" + newName))]
                        _ -> fo
                    in
                    aux (i + 1) outputOldTail outputNewTail diffTail fo
              _ -> Err <| \"\"\"Unknown diff for listdircontent: @d\"\"\"
        in case diffs of
           VListDiffs diffs -> aux 0 outputOld outputNew diffs []
           _ -> Err <| \"\"\"Don't know how to handle these list differences for listdircontent : @diffs\"\"\"
    } fileOperations

    isdir: String -> Bool
    isdir name =  case listDict.get name fileOperations of
       Just (CreateFolder content) -> True
       _ -> basicFS.isdir name
    isfolder = isdir
    isdirectory = isdir

    isfile: String -> Bool
    isfile name =
        case listDict.get name fileOperations of
          Just Create -> True
          Just (Write _ _ _) -> True
          Just Delete -> True
          _ -> basicFS.isfile name

    isFile = isfile
  }
}

-- Because we base decisions on random numbers,
-- to update, it is essential that these random numbers are deterministically computed.
random = {
  -- Unsafe: it will not evaluate deterministically
  jsNum = __jsEval__ \"\"\"Math.random()\"\"\"

  type alias Seed = (Int, Int, Int, Int)
  -- Builds a seed for a generator
  seedOf: Int -> Seed
  seedOf num = (num, num* -320161540, 320161540+num, 941627624 * num)

  -- A random number generator that takes a seed and exposes a bunch of methods.
  generator: Seed -> Generator
  generator (a, b, c, d) = {
    self () = generator (a, b, c, d)

    -- Continuation version of randomFloat
    randomFloat_: (Generator -> Float -> a) -> a
    randomFloat_ withNewGeneratorResult =
      let (newABCD, result) =
        __jsEval__ \"\"\"
        var a = @a, b = @b, c = @c, d = @d;
        var t = b << 9, r = a * 5; r = (r << 7 | r >>> 25) * 9;
        c ^= a; d ^= b;
        b ^= c; a ^= d; c ^= t;
        d = d << 11 | d >>> 21;
        @(jsCode.tupleOf \"x\" [jsCode.tupleOf \"y\" [\"a\", \"b\", \"c\", \"d\"], \"(r >>> 0) / 4294967296\"]);\"\"\"
      in
      withNewGeneratorResult (generator newABCD) result

    randomFloat: () -> (Generator, Float)
    randomFloat () = randomFloat_ (,)

    randomInt_: Int -> Int -> (Generator -> Int -> a) -> a
    randomInt_ minInclusive maxExclusive withNewGeneratorInt =
      randomFloat_ <| \\newGenerator float ->
        withNewGeneratorInt newGenerator <|
          floor (float * (maxExclusive - minInclusive)) + minInclusive

    randomInt: Int -> Int -> (Generator, Int)
    randomInt minInclusive maxExclusive = generateInt_ minInclusive maxExclusive (,)

    -- Extract a random sublist of elements, but not in order
    randomSublist_: Int -> List a -> (Generator -> List a -> b) -> b
    randomSublist_ count list continuation =
      if list == [] && count > 0 then continuation (self ()) list
      else if count <= 0 then continuation (self ()) []
      else randomInt_ 0 (List.length list) <| \\g n ->
        case List.removeAt n list of
          Nothing -> (g, []) -- Don't know why it would happen.
          Just (head, remainingList) ->
            g.randomSublist_ (count - 1) remainingList <| \\g2 tail ->
            continuation g2 <| head::tail

    randomSublist: Int -> List a -> (Generator, List a)
    randomSublist count list = randomSublist_ count list (,)

    shuffleList_: List a -> (Generator -> List a -> b) -> b
    shuffleList_ list  continuation = randomSublist_ (List.length list) list continuation

    shuffleList: List a -> (Generator, List a)
    shuffleList list = shuffleList_ list (,)
  }
}

-- TODO remove this; add as imports as needed in examples
{textNode, p, th, td, h1, h2, h3, tr, table} = Html
div_ = Html.div

--------------------------------------------------------------------------------
-- Lens: Table Library

  -- Update.freeze and Update.softFreeze aren't needed below,
  -- because library definitions are implicitly frozen.
  -- But for performance it's better.

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
      { apply rows = List.map (\\row -> (False, row)) rows

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


-- Begin SVG Stuff -------------------------------------------------------------

--
-- SVG Manipulating Functions
--

-- === SVG Types ===

-- type alias Point = [Num Num]
-- type alias RGBA = [Num Num Num Num]
-- type alias Color = (union String Num RGBA)
-- type alias PathCmds = (List (union String Num))
-- type alias Points = (List Point)
-- type alias RotationCmd = [[String Num Num Num]]
-- type alias AttrVal = (union String Num Bool Color PathCmds Points RotationCmd)
-- type alias AttrName = String
-- type alias AttrPair = [AttrName AttrVal]
-- type alias Attrs = (List AttrPair)
-- type alias NodeKind = String
-- TODO add recursive types properly
-- type alias SVG = [NodeKind Attrs (List SVG_or_Text)]
-- type alias SVG_or_Text = (union SVG [String String])
-- type alias Blob = (List SVG)

-- === Attribute Lookup ===
-- lookupWithDefault: (forall (k v) (-> v k (List [k v]) v))
lookupWithDefault default k dict =
  let foo = lookupWithDefault default k in
  case dict of
    [] -> default
    [k1, v]::rest -> if k == k1 then v else foo rest

--lookup: (forall (k v) (-> k (List [k v]) (union v Null)))
lookup k dict =
  let foo = lookup k in
  case dict of
    [] -> null
    [k1, v]::rest -> if k == k1 then v else foo rest

-- addExtras: (-> Num (List [String (List [Num AttrVal])]) SVG SVG)
addExtras i extras shape =
  case extras of
    [] -> shape
    [k, table]::rest ->
      let v = lookup i table in
      \"Error: typecase not yet implemented for Elm syntax\"

-- lookupAttr: (-> SVG AttrName (union AttrVal Null))
lookupAttr [_, attrs, _] k = lookup k attrs

-- lookupAttrWithDefault: (-> AttrVal SVG AttrName AttrVal)
lookupAttrWithDefault default [_, attrs, _] k =
  lookupWithDefault default k attrs 

-- Pairs of Type-Specific Lookup Functions
-- lookupNumAttr: (-> SVG AttrName (union Num Null))
lookupNumAttr [_, attrs, _] k =
  let val = lookup k attrs in
  \"Error: typecase not yet implemented for Elm syntax\"
  
-- lookupNumAttrWithDefault: (-> Num SVG AttrName Num)
lookupNumAttrWithDefault default shape k =
  let val = lookupNumAttr shape k in
  \"Error: typecase not yet implemented for Elm syntax\"

-- lookupPointsAttr: (-> SVG AttrName (union Points Null))
lookupPointsAttr [_, attrs, _] k =
  let val = lookup k attrs in
  \"Error: typecase not yet implemented for Elm syntax\"

-- lookupPointsAttrWithDefault: (-> Points SVG AttrName Points)
lookupPointsAttrWithDefault default shape k =
  let val = lookupPointsAttr shape k in
  \"Error: typecase not yet implemented for Elm syntax\"

-- lookupStringAttr: (-> SVG AttrName (union String Null))
lookupStringAttr [_, attrs, _] k =
  let val = lookup k attrs in
  \"Error: typecase not yet implemented for Elm syntax\"
  
-- lookupStringAttrWithDefault: (-> String SVG AttrName String)
lookupStringAttrWithDefault default shape k =
  let val = lookupStringAttr shape k in
  \"Error: typecase not yet implemented for Elm syntax\"

-- === Points ===

-- type alias Vec2D = [Num Num]

-- vec2DPlus: (-> Point Vec2D Point)
vec2DPlus pt vec =
  [ fst pt
    + fst vec, snd pt
    + snd vec
  ]

-- vec2DMinus: (-> Point Point Vec2D)
vec2DMinus pt vec =
  [ fst pt
    - fst vec, snd pt
    - snd vec
  ]

-- vec2DScalarMult: (-> Num Vec2D Point)
vec2DScalarMult num vec =
  [ fst vec
    * num, snd vec
    * num
  ]

-- vec2DScalarDiv: (-> Num Vec2D Point)
vec2DScalarDiv num vec =
  [ fst vec
    / num, snd vec
    / num
  ]

-- vec2DLength: (-> Point Point Num)
vec2DLength [x1, y1] [x2, y2] =
  let [dx, dy] = [ x2- x1, y2 - y1] in
  sqrt (dx * dx + dy * dy) 


-- === Circles ===

type alias Circle = SVG

--; argument order - color, x, y, radius
--; creates a circle, center at (x,y) with given radius and color
-- circle: (-> Color Num Num Num Circle)
circle fill cx cy r =
  ['circle',
     [['cx', cx], ['cy', cy], ['r', r], ['fill', fill]],
     []]

-- circleCenter: (-> Ellipse Point)
circleCenter circle =
  [
    lookupNumAttrWithDefault 0 circle 'cx',
    lookupNumAttrWithDefault 0 circle 'cy'
  ]

-- circleRadius: (-> Circle Num)
circleRadius circle =
  lookupNumAttrWithDefault 0 circle 'r'

-- circleDiameter: (-> Circle Num)
circleDiameter circle = 2
  * circleRadius circle

-- circleNorth: (-> Circle Point)
circleNorth circle =
  let [cx, cy] = circleCenter circle in
    [cx, cy - circleRadius circle]

-- circleEast: (-> Circle Point)
circleEast circle =
  let [cx, cy] = circleCenter circle in
    [ cx+ circleRadius circle, cy]

-- circleSouth: (-> Circle Point)
circleSouth circle =
  let [cx, cy] = circleCenter circle in
    [cx, cy + circleRadius circle]

-- circleWest: (-> Circle Point)
circleWest circle =
  let [cx, cy] = circleCenter circle in
    [ cx- circleRadius circle, cy] 


--; argument order - color, width, x, y, radius
--; Just as circle, except new width parameter determines thickness of ring
-- ring: (-> Color Num Num Num Num SVG)
ring c w x y r =
  ['circle',
     [ ['cx', x], ['cy', y], ['r', r], ['fill', 'none'], ['stroke', c], ['stroke-width', w] ],
     []] 


-- === Ellipses ===

type alias Ellipse = SVG

--; argument order - color, x, y, x-radius, y-radius
--; Just as circle, except radius is separated into x and y parameters
-- ellipse: (-> Color Num Num Num Num Ellipse)
ellipse fill x y rx ry =
  ['ellipse',
     [ ['cx', x], ['cy', y], ['rx', rx], ['ry', ry], ['fill', fill] ],
     []]

-- ellipseCenter: (-> Ellipse Point)
ellipseCenter ellipse =
  [
    lookupNumAttrWithDefault 0 ellipse 'cx',
    lookupNumAttrWithDefault 0 ellipse 'cy'
  ]

-- ellipseRadiusX: (-> Ellipse Num)
ellipseRadiusX ellipse =
  lookupNumAttrWithDefault 0 ellipse 'rx'

-- ellipseRadiusY: (-> Ellipse Num)
ellipseRadiusY ellipse =
  lookupNumAttrWithDefault 0 ellipse 'ry'

-- ellipseDiameterX: (-> Ellipse Num)
ellipseDiameterX ellipse = 2
  * ellipseRadiusX ellipse

-- ellipseDiameterY: (-> Ellipse Num)
ellipseDiameterY ellipse = 2
  * ellipseRadiusY ellipse

-- ellipseNorth: (-> Ellipse Point)
ellipseNorth ellipse =
  let [cx, cy] = ellipseCenter ellipse in
    [cx, cy - ellipseRadiusY ellipse]

-- ellipseEast: (-> Ellipse Point)
ellipseEast ellipse =
  let [cx, cy] = ellipseCenter ellipse in
    [ cx+ ellipseRadiusX ellipse, cy]

-- ellipseSouth: (-> Ellipse Point)
ellipseSouth ellipse =
  let [cx, cy] = ellipseCenter ellipse in
    [cx, cy + ellipseRadiusY ellipse]

-- ellipseWest: (-> Ellipse Point)
ellipseWest ellipse =
  let [cx, cy] = ellipseCenter ellipse in
    [ cx- ellipseRadiusX ellipse, cy] 


-- === Bounds-based shapes (Oval and Box) ===

-- type alias BoundedShape = SVG
-- type alias Bounds = [Num Num Num Num]

-- boundedShapeLeft: (-> BoundedShape Num)
boundedShapeLeft shape =
  lookupNumAttrWithDefault 0 shape 'LEFT'

-- boundedShapeTop: (-> BoundedShape Num)
boundedShapeTop shape =
  lookupNumAttrWithDefault 0 shape 'TOP'

-- boundedShapeRight: (-> BoundedShape Num)
boundedShapeRight shape =
  lookupNumAttrWithDefault 0 shape 'RIGHT'

-- boundedShapeBot: (-> BoundedShape Num)
boundedShapeBot shape =
  lookupNumAttrWithDefault 0 shape 'BOT'

-- boundedShapeWidth: (-> BoundedShape Num)
boundedShapeWidth shape = boundedShapeRight shape
  - boundedShapeLeft shape

-- boundedShapeHeight: (-> BoundedShape Num)
boundedShapeHeight shape = boundedShapeBot shape
  - boundedShapeTop shape

-- boundedShapeLeftTop: (-> BoundedShape Point)
boundedShapeLeftTop shape =
  [
    boundedShapeLeft shape,
    boundedShapeTop shape
  ]

-- boundedShapeCenterTop: (-> BoundedShape Point)
boundedShapeCenterTop shape =
  [ (boundedShapeLeft shape + boundedShapeRight shape)
    / 2,
    boundedShapeTop shape
  ]

-- boundedShapeRightTop: (-> BoundedShape Point)
boundedShapeRightTop shape =
  [
    boundedShapeRight shape,
    boundedShapeTop shape
  ]

-- boundedShapeRightCenter: (-> BoundedShape Point)
boundedShapeRightCenter shape =
  [
    boundedShapeRight shape, (boundedShapeTop shape + boundedShapeBot shape)
    / 2
  ]

-- boundedShapeRightBot: (-> BoundedShape Point)
boundedShapeRightBot shape =
  [
    boundedShapeRight shape,
    boundedShapeBot shape
  ]

-- boundedShapeCenterBot: (-> BoundedShape Point)
boundedShapeCenterBot shape =
  [ (boundedShapeLeft shape + boundedShapeRight shape)
    / 2,
    boundedShapeBot shape
  ]

-- boundedShapeLeftBot: (-> BoundedShape Point)
boundedShapeLeftBot shape =
  [
    boundedShapeLeft shape,
    boundedShapeBot shape
  ]

-- boundedShapeLeftCenter: (-> BoundedShape Point)
boundedShapeLeftCenter shape =
  [
    boundedShapeLeft shape, (boundedShapeTop shape + boundedShapeBot shape)
    / 2
  ]

-- boundedShapeCenter: (-> BoundedShape Point)
boundedShapeCenter shape =
  [ (boundedShapeLeft shape + boundedShapeRight shape)
    / 2, (boundedShapeTop shape + boundedShapeBot shape)
    / 2
  ] 


-- === Rectangles ===

type alias Rect = SVG

--; argument order - color, x, y, width, height
--; creates a rectangle of given width and height with (x,y) as the top left corner coordinate
-- rect: (-> Color Num Num Num Num Rect)
rect fill x y w h =
  ['rect',
     [ ['x', x], ['y', y], ['width', w], ['height', h], ['fill', fill] ],
     []]

-- square: (-> Color Num Num Num Rect)
square fill x y side = rect fill x y side side

-- rectWidth: (-> Rect Num)
rectWidth rect =
  lookupNumAttrWithDefault 0 rect 'width'

-- rectHeight: (-> Rect Num)
rectHeight rect =
  lookupNumAttrWithDefault 0 rect 'height'

-- rectLeftTop: (-> Rect Point)
rectLeftTop rect =
  [
    lookupNumAttrWithDefault 0 rect 'x',
    lookupNumAttrWithDefault 0 rect 'y'
  ]

-- rectCenterTop: (-> Rect Point)
rectCenterTop rect =
  vec2DPlus
    (rectLeftTop rect)
    [ rectWidth rect / 2, 0 ]

-- rectRightTop: (-> Rect Point)
rectRightTop rect =
  vec2DPlus
    (rectLeftTop rect)
    [ rectWidth rect, 0 ]

-- rectRightCenter: (-> Rect Point)
rectRightCenter rect =
  vec2DPlus
    (rectLeftTop rect)
    [ rectWidth rect, rectHeight rect / 2 ]

-- rectRightBot: (-> Rect Point)
rectRightBot rect =
  vec2DPlus
    (rectLeftTop rect)
    [ rectWidth rect, rectHeight rect ]

-- rectCenterBot: (-> Rect Point)
rectCenterBot rect =
  vec2DPlus
    (rectLeftTop rect)
    [ rectWidth rect / 2, rectHeight rect ]

-- rectLeftBot: (-> Rect Point)
rectLeftBot rect =
  vec2DPlus
    (rectLeftTop rect)
    [0, rectHeight rect ]

-- rectLeftCenter: (-> Rect Point)
rectLeftCenter rect =
  vec2DPlus
    (rectLeftTop rect)
    [0, rectHeight rect / 2 ]

-- rectCenter: (-> Rect Point)
rectCenter rect =
  vec2DPlus
    (rectLeftTop rect)
    [ rectWidth rect / 2, rectHeight rect / 2 ] 


-- === Lines ===

type alias Line = SVG

--; argument order - color, width, x1, y1, x1, y2
--; creates a line from (x1, y1) to (x2,y2) with given color and width
-- line: (-> Color Num Num Num Num Num Line)
line stroke w x1 y1 x2 y2 =
  ['line',
     [ ['x1', x1], ['y1', y1], ['x2', x2], ['y2', y2], ['stroke', stroke], ['stroke-width', w] ],
     []]

-- lineBetween: (-> Color Num Point Point Line)
lineBetween stroke w [x1, y1] [x2, y2] =
  line stroke w x1 y1 x2 y2

-- lineStart: (-> Line Point)
lineStart line =
  [
    lookupNumAttrWithDefault 0 line 'x1',
    lookupNumAttrWithDefault 0 line 'y1'
  ]

-- lineEnd: (-> Line Point)
lineEnd line =
  [
    lookupNumAttrWithDefault 0 line 'x2',
    lookupNumAttrWithDefault 0 line 'y2'
  ]

-- lineMidPoint: (-> Line Point)
lineMidPoint line =
  halfwayBetween (lineStart line) (lineEnd line) 


--; argument order - fill, stroke, width, points
--; creates a polygon following the list of points, with given fill color and a border with given width and stroke
-- polygon: (-> Color Color Num Points SVG)
polygon fill stroke w pts =
  ['polygon',
     [ ['fill', fill], ['points', pts], ['stroke', stroke], ['stroke-width', w] ],
     []] 

--; argument order - fill, stroke, width, points
--; See polygon
-- polyline: (-> Color Color Num Points SVG)
polyline fill stroke w pts =
  ['polyline',
     [ ['fill', fill], ['points', pts], ['stroke', stroke], ['stroke-width', w] ],
     []] 

--; argument order - fill, stroke, width, d
--; Given SVG path command d, create path with given fill color, stroke and width
--; See https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths for path command info
-- path: (-> Color Color Num PathCmds SVG)
path fill stroke w d =
  ['path',
     [ ['fill', fill], ['stroke', stroke], ['stroke-width', w], ['d', d] ],
     []] 

--; argument order - x, y, string
--; place a text string with top left corner at (x,y) - with default color & font
-- text: (-> Num Num String SVG)
text x y s =
   ['text', [['x', x], ['y', y], ['style', 'fill:black'],
            ['font-family', 'Tahoma, sans-serif']],
           [['TEXT', s]]] 

--; argument order - shape, new attribute
--; Add a new attribute to a given Shape
-- addAttr: (-> SVG AttrPair SVG)
addAttr [shapeKind, oldAttrs, children] newAttr =
  [shapeKind, snoc newAttr oldAttrs, children]

-- consAttr: (-> SVG AttrPair SVG)
consAttr [shapeKind, oldAttrs, children] newAttr =
  [shapeKind, cons newAttr oldAttrs, children] 

--; Given a list of shapes, compose into a single SVG
svg shapes = ['svg', [], shapes] 

--; argument order - x-maximum, y-maximum, shapes
--; Given a list of shapes, compose into a single SVG within the x & y maxima
-- svgViewBox: (-> Num Num (List SVG) SVG)
svgViewBox xMax yMax shapes =
  let [sx, sy] = [toString xMax, toString yMax] in
  ['svg',
    [['x', '0'], ['y', '0'], ['viewBox', joinStrings ' ' ['0', '0', sx, sy]]],
    shapes] 

--; As rect, except x & y represent the center of the defined rectangle
-- rectByCenter: (-> Color Num Num Num Num Rect)
rectByCenter fill cx cy w h =
  rect fill (cx - w / 2) (cy - h / 2) w h 

--; As square, except x & y represent the center of the defined rectangle
-- squareByCenter: (-> Color Num Num Num Rect)
squareByCenter fill cx cy w = rectByCenter fill cx cy w w 

--; Some shapes with given default values for fill, stroke, and stroke width
-- TODO remove these
circle_ =    circle 'red' 
ellipse_ =   ellipse 'orange' 
rect_ =      rect '#999999' 
square_ =    square '#999999' 
line_ =      line 'blue' 2 
polygon_ =   polygon 'green' 'purple' 3 
path_ =      path 'transparent' 'goldenrod' 5 

--; updates an SVG by comparing differences with another SVG
--; Note: accDiff pre-condition: indices in increasing order
--; (so can't just use foldr instead of reverse . foldl)
-- updateCanvas: (-> SVG SVG SVG)
updateCanvas [_, svgAttrs, oldShapes] diff =
  let oldShapesI = zipOld (list1N (len oldShapes)) oldShapes in
  let initAcc = [[], diff] in
  let f [i, oldShape] [accShapes, accDiff] =
    case accDiff of
      []->
        [cons oldShape accShapes, accDiff]
      [j, newShape]::accDiffRest->
        if i == j then
          [cons newShape accShapes, accDiffRest]
        else
          [cons oldShape accShapes, accDiff] in
  let newShapes = reverse (fst (foldl f initAcc oldShapesI)) in
    ['svg', svgAttrs, newShapes] 

addBlob newShapes ['svg', svgAttrs, oldShapes] =
  ['svg', svgAttrs, append oldShapes newShapes]

--  groupMap: (forall (a b) (-> (List a) (-> a b) (List b)))
groupMap xs f = map f xs 

autoChose _ x _ = x 
inferred x _ _ = x 
flow _ x = x 

twoPi = 2 * pi 
halfPi = pi / 2 

--; Helper function for nPointsOnCircle, calculates angle of points
--; Note: angles are calculated clockwise from the traditional pi/2 mark
-- nPointsOnUnitCircle: (-> Num Num (List Point))
nPointsOnUnitCircle n rot =
  let off = halfPi - rot in
  let foo i =
    let ang = off + i / n * twoPi in
    [cos ang, neg (sin ang)] in
  map foo (list0N (n - 1))

-- nPointsOnCircle: (-> Num Num Num Num Num (List Point))
--; argument order - Num of points, degree of rotation, x-center, y-center, radius
--; Scales nPointsOnUnitCircle to the proper size and location with a given radius and center
nPointsOnCircle n rot cx cy r =
  let pts = nPointsOnUnitCircle n rot in
  map \\[x, y] -> [ cx+ x * r, cy + y * r] pts

-- nStar: (-> Color Color Num Num Num Num Num Num Num SVG)
--; argument order -
--; fill color - interior color of star
--; stroke color - border color of star
--; width - thickness of stroke
--; points - number of star points
--; len1 - length from center to one set of star points
--; len2 - length from center to other set of star points (either inner or outer compared to len1)
--; rot - degree of rotation
--; cx - x-coordinate of center position
--; cy - y-coordinate of center position
--; Creates stars that can be modified on a number of parameters
nStar fill stroke w n len1 len2 rot cx cy =
  let pti [i, len] =
    let anglei = i * pi / n - rot + halfPi in
    let xi = cx + len * cos anglei in
    let yi = cy + neg (len * sin anglei) in
      [xi, yi] in
  let lengths =
    map (\\b -> if b then len1 else len2)
        (concat (repeat n [True, False])) in
  let indices = list0N (2! * n - 1!) in
    polygon fill stroke w (map pti (zipOld indices lengths))

-- setZones: (-> String SVG SVG)
setZones s shape = addAttr shape ['ZONES', s]

-- zones: (-> String (List SVG) (List SVG))
zones s shapes = map (setZones s) shapes 
-- TODO eta-reduced version:
-- (def zones (\\s (map (setZones s))))

--; Remove all zones from shapes except for the first in the list
-- hideZonesTail: (-> (List SVG) (List SVG))
hideZonesTail hd :: tl = hd :: zones 'none' tl 

--; Turn all zones to basic for a given list of shapes except for the first shape
-- basicZonesTail: (-> (List SVG) (List SVG))
basicZonesTail hd :: tl = hd :: zones 'basic' tl

-- ghost: (-> SVG SVG)
ghost =
  -- consAttr (instead of addAttr) makes internal calls to
  -- Utils.maybeRemoveFirst 'HIDDEN' slightly faster
  \\shape -> consAttr shape ['HIDDEN', ''] 

ghosts = map ghost 

--; hSlider_ : Bool -> Bool -> Int -> Int -> Int -> Num -> Num -> Str -> Num
--; -> [Num (List Svg)]
--; argument order - dropBall roundInt xStart xEnd y minVal maxVal caption srcVal
--; dropBall - Determines if the slider ball continues to appear past the edges of the slider
--; roundInt - Determines whether to round to Ints or not
--; xStart - left edge of slider
--; xEnd - right edge of slider
--; y - y positioning of entire slider bar
--; minVal - minimum value of slider
--; maxVal - maximum value of slider
--; caption - text to display along with the slider
--; srcVal - the current value given by the slider ball
hSlider_ dropBall roundInt x0 x1 y minVal maxVal caption srcVal =
  let preVal = clamp minVal maxVal srcVal in
  let targetVal = if roundInt then round preVal else preVal in
  let shapes =
    let ball =
      let [xDiff, valDiff] = [ x1- x0, maxVal - minVal] in
      let xBall = x0 + xDiff * (srcVal - minVal) / valDiff in
      if preVal == srcVal then circle 'black' xBall y 10! else
      if dropBall then          circle 'black' 0! 0! 0! else
                            circle 'red' xBall y 10! in
    [ line 'black' 3! x0 y x1 y,
      text (x1 + 10) (y + 5) (caption + toString targetVal),
      circle 'black' x0 y 4!, circle 'black' x1 y 4!, ball ] in
  [targetVal, ghosts shapes] 
-- TODO only draw zones for ball

vSlider_ dropBall roundInt y0 y1 x minVal maxVal caption srcVal =
  let preVal = clamp minVal maxVal srcVal in
  let targetVal = if roundInt then round preVal else preVal in
  let shapes =
    let ball =
      let [yDiff, valDiff] = [ y1- y0, maxVal - minVal] in
      let yBall = y0 + yDiff * (srcVal - minVal) / valDiff in
      if preVal == srcVal then circle 'black' x yBall 10! else
      if dropBall then          circle 'black' 0! 0! 0! else
                            circle 'red' x yBall 10! in
    [ line 'black' 3! x y0 x y1,
      -- (text (+ x1 10) (+ y 5) (+ caption (toString targetVal)))
      circle 'black' x y0 4!, circle 'black' x y1 4!, ball ] in
  [targetVal, ghosts shapes] 
-- TODO only draw zones for ball

hSlider = hSlider_ False 
vSlider = vSlider_ False 

--; button_ : Bool -> Num -> Num -> String -> Num -> SVG
--; Similar to sliders, but just has boolean values
button_ dropBall xStart y caption xCur =
  let [rPoint, wLine, rBall, wSlider] = [4!, 3!, 10!, 70!] in
  let xEnd = xStart + wSlider in
  let xBall = xStart + xCur * wSlider in
  let xBall_ = clamp xStart xEnd xBall in
  let val = xCur < 0.5 in
  let shapes1 =
    [ circle 'black' xStart y rPoint,
      circle 'black' xEnd y rPoint,
      line 'black' wLine xStart y xEnd y,
      text (xEnd + 10) (y + 5) (caption + toString val) ] in
  let shapes2 =
    [ if xBall_ == xBall then circle (if val then 'darkgreen' else 'darkred') xBall y rBall else
      if dropBall then         circle 'black' 0! 0! 0! else
                           circle 'red' xBall y rBall ] in
  let shapes = append (zones 'none' shapes1) (zones 'basic' shapes2) in
  [val, ghosts shapes] 

button = button_ False 

xySlider xStart xEnd yStart yEnd xMin xMax yMin yMax xCaption yCaption xCur yCur =
    let [rCorner, wEdge, rBall] = [4!, 3!, 10!] in
    let [xDiff, yDiff, xValDiff, yValDiff] = [ xEnd- xStart, yEnd - yStart, xMax - xMin, yMax - yMin] in
    let xBall = xStart + xDiff * (xCur - xMin) / xValDiff in
    let yBall = yStart + yDiff * (yCur - yMin) / yValDiff in
    let cBall = if and (between xMin xMax xCur) (between yMin yMax yCur)then 'black'else 'red' in
    let xVal = ceiling clamp xMin xMax xCur in
    let yVal = ceiling clamp yMin yMax yCur in
    let myLine x1 y1 x2 y2 = line 'black' wEdge x1 y1 x2 y2 in
    let myCirc x0 y0 = circle 'black' x0 y0 rCorner in
    let shapes =
      [ myLine xStart yStart xEnd yStart,
        myLine xStart yStart xStart yEnd,
        myLine xStart yEnd xEnd yEnd,
        myLine xEnd yStart xEnd yEnd,
        myCirc xStart yStart,
        myCirc xStart yEnd,
        myCirc xEnd yStart,
        myCirc xEnd yEnd,
        circle cBall xBall yBall rBall,
        text (xStart + xDiff / 2 - 40) (yEnd + 20) (xCaption + toString xVal),
        text (xEnd + 10) (yStart + yDiff / 2) (yCaption + toString yVal) ] in
    [ [ xVal, yVal ], ghosts shapes ]
    
-- enumSlider: (forall a (-> Num Num Num [a|(List a)] String Num [a (List SVG)]))
enumSlider x0 x1 ((ya::_) as enum) caption srcVal =
  let n = len enum in
  let [minVal, maxVal] = [0!, n] in
  let preVal = clamp minVal maxVal srcVal in
  let i = floor preVal in
  let item = -- using dummy first element for typechecking
    let item_ = nth enum if i == n then n - 1 else i in
    \"Error: typecase not yet implemented for Elm syntax\" in
  let wrap circ = addAttr circ ['SELECTED', ''] in -- TODO
  let shapes =
    let rail = [ line 'black' 3! x0 y x1 y ] in
    let ball =
      let [xDiff, valDiff] = [ x1- x0, maxVal - minVal] in
      let xBall = x0 + xDiff * (srcVal - minVal) / valDiff in
      let colorBall = if preVal == srcVal then 'black' else 'red' in
        [ wrap (circle colorBall xBall y 10!) ] in
    let endpoints =
      [ wrap (circle 'black' x0 y 4!), wrap (circle 'black' x1 y 4!) ] in
    let tickpoints =
      let sep = (x1 - x0) / n in
      map (\\j -> wrap (circle 'grey' (x0 + mult j sep) y 4!))
          (range 1! (n - 1!)) in
    let label = [ text (x1 + 10!) (y + 5!) (caption + toString item) ] in
    concat [ rail, endpoints, tickpoints, ball, label ] in
  [item, ghosts shapes] 

addSelectionSliders y0 seeds shapesCaps =
  let shapesCapsSeeds = zipOld shapesCaps (take seeds (len shapesCaps)) in
  let foo [i, [[shape, cap], seed]] =
    let [k, _, _] = shape in
    let enum =
      if k == 'circle'then ['', 'cx', 'cy', 'r']else
      if k == 'line'then   ['', 'x1', 'y1', 'x2', 'y2']else
      if k == 'rect'then   ['', 'x', 'y', 'width', 'height']else
        [ 'NO SELECTION ENUM FOR KIND '+ k] in
    let [item, slider] = enumSlider 20! 170! (y0 + mult i 30!) enum cap seed in
    let shape1 = addAttr shape ['SELECTED', item] in -- TODO overwrite existing
    shape1::slider in
  concat (mapi foo shapesCapsSeeds) 

-- Text Widgets

simpleText family color size x1 x2 y horizAlignSeed textVal =
  let xMid = x1 + (x2 - x1) / 2! in
  let [anchor, hAlignSlider] =
    let dx = (x2 - x1) / 4! in
    let yLine = 30! + y in
    enumSlider (xMid - dx) (xMid + dx) yLine
      ['start', 'middle', 'end'] '' horizAlignSeed in
  let x =
    if anchor == 'start' then x1 else
    if anchor == 'middle' then xMid else
    if anchor == 'end' then x2 else
      'CRASH' in
  let theText =
    ['text',
      [['x', x], ['y', y],
       ['style', 'fill:' + color],
       ['font-family', family], ['font-size', size],
       ['text-anchor', anchor]],
      [['TEXT', textVal]]] in
  let rails =
    let pad = 15! in
    let yBaseLine = y + pad in
    let xSideLine = x1 - pad in
    let rail = line 'gray' 3 in
    let baseLine = rail xSideLine yBaseLine x2 yBaseLine in
    let sideLine = rail xSideLine yBaseLine xSideLine (y - size) in
    let dragBall = circle 'black' x yBaseLine 8! in
    ghosts [baseLine, sideLine, dragBall] in
  concat [[theText], hAlignSlider, rails]

-- rotate: (-> SVG Num Num Num SVG)
--; argument order - shape, rot, x, y
--; Takes a shape rotates it rot degrees around point (x,y)
rotate shape n1 n2 n3 =
  addAttr shape ['transform', [['rotate', n1, n2, n3]]]

-- rotateAround: (-> Num Num Num SVG SVG)
rotateAround rot x y shape =
  addAttr shape ['transform', [['rotate', rot, x, y]]]

-- Polygon and Path Helpers
-- middleOfPoints: (-> (List Point) Point)
middleOfPoints pts =
  let [xs, ys] = [map fst pts, map snd pts] in
  let [xMin, xMax] = [minimum xs, maximum xs] in
  let [yMin, yMax] = [minimum ys, maximum ys] in
  let xMiddle = noWidgets (xMin + 0.5 * (xMax - xMin)) in
  let yMiddle = noWidgets (yMin + 0.5 * (yMax - yMin)) in
    [xMiddle, yMiddle]

-- polygonPoints: (-> SVG Points)
polygonPoints [shapeKind, _, _]asshape =
  case shapeKind of
    'polygon'-> lookupPointsAttrWithDefault [] shape 'points'
    _->         []
    
-- allPointsOfPathCmds_: (-> PathCmds (List [(union Num String) (union Num String)]))
allPointsOfPathCmds_ cmds = case cmds
of
  []->    []
  ['Z']-> []

  'M'::x::y::rest-> cons [x, y] (allPointsOfPathCmds_ rest)
  'L'::x::y::rest-> cons [x, y] (allPointsOfPathCmds_ rest)

  'Q'::x1::y1::x::y::rest->
    append [[x1, y1], [x, y]] (allPointsOfPathCmds_ rest)

  'C'::x1::y1::x2::y2::x::y::rest->
    append [[x1, y1], [x2, y2], [x, y]] (allPointsOfPathCmds_ rest)

  _-> [let _ = debug \"Prelude.allPointsOfPathCmds_: not Nums...\" in [-1, -1]] 

-- (typ allPointsOfPathCmds (-> PathCmds (List Point)))
-- (def allPointsOfPathCmds (\\cmds
--   (let toNum (\\numOrString
--     (typecase numOrString (Num numOrString) (String -1)))
--   (map (\\[x y] [(toNum x) (toNum y)]) (allPointsOfPathCmds_ cmds)))))

-- TODO remove inner annotations and named lambda
-- allPointsOfPathCmds: (-> PathCmds (List Point))
allPointsOfPathCmds cmds =
  -- toNum: (-> (union Num String) Num)
  let toNum numOrString =
  \"Error: typecase not yet implemented for Elm syntax\" in
  -- foo: (-> [(union Num String) (union Num String)] Point)
  let foo [x, y] = [toNum x, toNum y] in
  map foo (allPointsOfPathCmds_ cmds) 


-- Raw Shapes

rawShape kind attrs = [kind, attrs, []]

-- rawRect: (-> Color Color Num Num Num Num Num Num Rect)
rawRect fill stroke strokeWidth x y w h rot =
  let [cx, cy] = [ x+ w / 2!, y + h / 2!] in
  rotateAround rot cx cy
    (rawShape 'rect' [
      ['x', x], ['y', y], ['width', w], ['height', h],
      ['fill', fill], ['stroke', stroke], ['stroke-width', strokeWidth] ])

-- rawCircle: (-> Color Color Num Num Num Num Circle)
rawCircle fill stroke strokeWidth cx cy r =
  rawShape 'circle' [
    ['cx', cx], ['cy', cy], ['r', r],
    ['fill', fill], ['stroke', stroke], ['stroke-width', strokeWidth] ]

-- rawEllipse: (-> Color Color Num Num Num Num Num Num Ellipse)
rawEllipse fill stroke strokeWidth cx cy rx ry rot =
  rotateAround rot cx cy
    (rawShape 'ellipse' [
      ['cx', cx], ['cy', cy], ['rx', rx], ['ry', ry],
      ['fill', fill], ['stroke', stroke], ['stroke-width', strokeWidth] ])

-- rawPolygon: (-> Color Color Num Points Num SVG)
rawPolygon fill stroke w pts rot =
  let [cx, cy] = middleOfPoints pts in
  rotateAround rot cx cy
    (rawShape 'polygon'
      [ ['fill', fill], ['points', pts], ['stroke', stroke], ['stroke-width', w] ])

-- rawPath: (-> Color Color Num PathCmds Num SVG)
rawPath fill stroke w d rot =
  let [cx, cy] = middleOfPoints (allPointsOfPathCmds d) in
  rotateAround rot cx cy
    (rawShape 'path'
      [ ['fill', fill], ['d', d], ['stroke', stroke], ['stroke-width', w] ]) 


-- Shapes via Bounding Boxes
-- box: (-> Bounds Color Color Num BoundedShape)
box bounds fill stroke strokeWidth =
  let [x, y, xw, yh] = bounds in
  ['BOX',
    [ ['LEFT', x], ['TOP', y], ['RIGHT', xw], ['BOT', yh],
      ['fill', fill], ['stroke', stroke], ['stroke-width', strokeWidth]
    ], []
  ] 

-- string fill/stroke/stroke-width attributes to avoid sliders
-- hiddenBoundingBox: (-> Bounds BoundedShape)
hiddenBoundingBox bounds =
  ghost (box bounds 'transparent' 'transparent' '0')

-- simpleBoundingBox: (-> Bounds BoundedShape)
simpleBoundingBox bounds =
  ghost (box bounds 'transparent' 'darkblue' 1)

-- strList: (-> (List String) String)
strList =
  let foo x acc = acc + if acc == ''then ''else ' ' + toString x in
  foldl foo ''

-- fancyBoundingBox: (-> Bounds (List SVG))
fancyBoundingBox bounds =
  let [left, top, right, bot] = bounds in
  let [width, height] = [ right- left, bot - top] in
  let [c1, c2, r] = ['darkblue', 'skyblue', 6] in
  [ ghost (box bounds 'transparent' c1 1),
    ghost (setZones 'none' (circle c2 left top r)),
    ghost (setZones 'none' (circle c2 right top r)),
    ghost (setZones 'none' (circle c2 right bot r)),
    ghost (setZones 'none' (circle c2 left bot r)),
    ghost (setZones 'none' (circle c2 left (top + height / 2) r)),
    ghost (setZones 'none' (circle c2 right (top + height / 2) r)),
    ghost (setZones 'none' (circle c2 (left + width / 2) top r)),
    ghost (setZones 'none' (circle c2 (left + width / 2) bot r))
  ]

-- groupWithPad: (-> Num Bounds (List SVG) SVG)
groupWithPad pad bounds shapes =
  let [left, top, right, bot] = bounds in
  let paddedBounds = [ left- pad, top - pad, right + pad, bot + pad] in
  ['g', [['BOUNDS', bounds]],
       cons (hiddenBoundingBox paddedBounds) shapes]

-- group: (-> Bounds (List SVG) SVG)
group = groupWithPad let nGroupPad = 20 in nGroupPad 

-- NOTE:
--   keep the names nGroupPad and nPolyPathPad (and values)
--   in sync with ExpressionBasedTransform.elm

-- (def group (groupWithPad 15))

polyPathGroup = groupWithPad let nPolyPathPad = 10 in nPolyPathPad 

-- TODO make one pass over pts
-- boundsOfPoints: (-> (List Point) Bounds)
boundsOfPoints pts =
  let left =  minimum (map fst pts) in
  let right = maximum (map fst pts) in
  let top =   minimum (map snd pts) in
  let bot =   maximum (map snd pts) in
    [left, top, right, bot]
    
-- extremeShapePoints: (-> SVG Points)
extremeShapePoints ([kind, _, _] as shape) =
  case kind of
    'line'->
      let [x1, y1, x2, y2]as attrs = map (lookupAttr shape) [\"x1\", \"y1\", \"x2\", \"y2\"] in
      \"Error: typecase not yet implemented for Elm syntax\"

    'rect'->
      let [x, y, w, h]as attrs = map (lookupAttr shape) [\"x\", \"y\", \"width\", \"height\"] in
      \"Error: typecase not yet implemented for Elm syntax\"

    'circle'->
      let [cx, cy, r]as attrs = map (lookupAttr shape) [\"cx\", \"cy\", \"r\"] in
      \"Error: typecase not yet implemented for Elm syntax\"

    'ellipse'->
      let [cx, cy, rx, ry]as attrs = map (lookupAttr shape) [\"cx\", \"cy\", \"rx\", \"ry\"] in
      \"Error: typecase not yet implemented for Elm syntax\"

    'polygon'-> polygonPoints shape

    'path'->
      let pathCmds = lookupAttr shape \"d\" in
      \"Error: typecase not yet implemented for Elm syntax\"

    _-> []

-- anchoredGroup: (-> (List SVG) SVG)
anchoredGroup shapes =
  let bounds = boundsOfPoints (concat (map extremeShapePoints shapes)) in
  group bounds shapes 

-- (def group (\\(bounds shapes)
--   ['g' [['BOUNDS' bounds]]
--        (cons (hiddenBoundingBox bounds) shapes)]))

       -- (concat [(fancyBoundingBox bounds) shapes])]))

-- TODO no longer used...
-- rotatedRect: (-> Color Num Num Num Num Num Rect)
rotatedRect fill x y w h rot =
  let [cx, cy] = [ x+ w / 2!, y + h / 2!] in
  let bounds = [x, y, x + w, y + h] in
  let shape = rotateAround rot cx cy (rect fill x y w h) in
  group bounds [shape]

-- rectangle: (-> Color Color Num Num Bounds Rect)
rectangle fill stroke strokeWidth rot bounds =
  let [left, top, right, bot] = bounds in
  let [cx, cy] = [ left+ (right - left) / 2!, top + (bot - top) / 2!] in
  let shape = rotateAround rot cx cy (box bounds fill stroke strokeWidth) in
  shape 
-- (group bounds [shape])

-- TODO no longer used...
-- rotatedEllipse: (-> Color Num Num Num Num Num Ellipse)
rotatedEllipse fill cx cy rx ry rot =
  let bounds = [ cx- rx, cy - ry, cx + rx, cy + ry] in
  let shape = rotateAround rot cx cy (ellipse fill cx cy rx ry) in
  group bounds [shape] 

-- TODO take rot
-- oval: (-> Color Color Num Bounds BoundedShape)
oval fill stroke strokeWidth bounds =
  let [left, top, right, bot] = bounds in
  let shape =
    ['OVAL',
       [ ['LEFT', left], ['TOP', top], ['RIGHT', right], ['BOT', bot],
         ['fill', fill], ['stroke', stroke], ['stroke-width', strokeWidth] ],
       []] in
  shape 

-- ; TODO take rot
-- (def oval (\\(fill stroke strokeWidth bounds)
--   (let [left top right bot] bounds
--   (let [rx ry] [(/ (- right left) 2!) (/ (- bot top) 2!)]
--   (let [cx cy] [(+ left rx) (+ top ry)]
--   (let shape ; TODO change def ellipse to take stroke/strokeWidth
--     ['ellipse'
--        [ ['cx' cx] ['cy' cy] ['rx' rx] ['ry' ry]
--          ['fill' fill] ['stroke' stroke] ['stroke-width' strokeWidth] ]
--        []]
--   (group bounds [shape])
-- ))))))

scaleBetween a b pct =
  case pct of
    0-> a
    1-> b
    _-> a + pct * (b - a)

-- stretchyPolygon: (-> Bounds Color Color Num (List Num) SVG)
stretchyPolygon bounds fill stroke strokeWidth percentages =
  let [left, top, right, bot] = bounds in
  let [xScale, yScale] = [scaleBetween left right, scaleBetween top bot] in
  let pts = map \\[xPct, yPct] -> [ xScale xPct, yScale yPct ] percentages in
  -- (group bounds [(polygon fill stroke strokeWidth pts)])
  polyPathGroup bounds [polygon fill stroke strokeWidth pts] 

-- TODO no longer used...
pointyPath fill stroke w d =
  let dot x y = ghost (circle 'orange' x y 5) in
  let pointsOf cmds =
    case cmds of
      []->                     []
      ['Z']->                  []
      'M'::x::y::rest->       append [dot x y] (pointsOf rest)
      'L'::x::y::rest->       append [dot x y] (pointsOf rest)
      'Q'::x1::y1::x::y::rest-> append [dot x1 y1, dot x y] (pointsOf rest)
      'C'::x1::y1::x2::y2::x::y::rest-> append [dot x1 y1, dot x2 y2, dot x y] (pointsOf rest)
      _->                      'ERROR' in
  ['g', [],
    cons
      (path fill stroke w d)
      []] 
-- turning off points for now
-- (pointsOf d)) ]

-- can refactor to make one pass
-- can also change representation/template code to pair points
stretchyPath bounds fill stroke w d =
  let [left, top, right, bot] = bounds in
  let [xScale, yScale] = [scaleBetween left right, scaleBetween top bot] in
  let dot x y = ghost (circle 'orange' x y 5) in
  let toPath cmds =
    case cmds of
      []->    []
      ['Z']-> ['Z']
      'M'::x::y::rest-> append ['M', xScale x, yScale y] (toPath rest)
      'L'::x::y::rest-> append ['L', xScale x, yScale y] (toPath rest)
      'Q'::x1::y1::x::y::rest->
        append ['Q', xScale x1, yScale y1, xScale x, yScale y]
                (toPath rest)
      'C'::x1::y1::x2::y2::x::y::rest->
        append ['C', xScale x1, yScale y1, xScale x2, yScale y2, xScale x, yScale y]
                (toPath rest)
      _-> 'ERROR' in
  let pointsOf cmds =
    case cmds of
      []->    []
      ['Z']-> []
      'M'::x::y::rest-> append [dot (xScale x) (yScale y)] (pointsOf rest)
      'L'::x::y::rest-> append [dot (xScale x) (yScale y)] (pointsOf rest)
      'Q'::x1::y1::x::y::rest->
        append [dot (xScale x1) (yScale y1), dot (xScale x) (yScale y)]
                (pointsOf rest)
      'C'::x1::y1::x2::y2::x::y::rest->
        append [dot (xScale x1) (yScale y1),
                 dot (xScale x2) (yScale y2),
                 dot (xScale x)  (yScale y)]
                (pointsOf rest)
      _-> 'ERROR' in
  -- (group bounds
  polyPathGroup bounds
    (cons
      (path fill stroke w (toPath d))
      []) 
-- turning off points for now
-- (pointsOf d)))
-- evalOffset: (-> [Num Num] Num)
evalOffset [base, off] =
  case off of
    0-> base
    _-> base + off 

stickyPolygon bounds fill stroke strokeWidth offsets =
  let pts = map \\[xOff, yOff] -> [ evalOffset xOff, evalOffset yOff ] offsets in
  group bounds [polygon fill stroke strokeWidth pts]

-- withBounds: (-> Bounds (-> Bounds (List SVG)) (List SVG))
withBounds bounds f = f bounds

-- withAnchor: (-> Point (-> Point (List SVG)) (List SVG))
withAnchor anchor f = f anchor

-- star: (-> Bounds (List SVG))
star bounds =
  let [left, top, right, bot] = bounds in
  let [width, height] = [ right- left, bot - top] in
  let [cx, cy] = [ left+ width / 2, top + height / 2] in
  [nStar 0 'black' 0 6 (min (width / 2) (height / 2)) 10 0 cx cy]

-- blobs: (-> (List Blob) SVG)
blobs blobs =
  let modifyBlob [i, blob] =
    case blob of
      [['g', gAttrs, shape :: shapes]]->
       [['g', gAttrs, consAttr shape ['BLOB', toString (i + 1)] :: shapes]]
      [shape]-> [consAttr shape ['BLOB', toString (i + 1)]]
      _->       blob in
  svg (concat (mapi modifyBlob blobs)) 


-- === Relations ===
-- halfwayBetween: (-> Point Point Point)
halfwayBetween pt1 pt2 =
  vec2DScalarMult 0.5 (vec2DPlus pt1 pt2)

-- nextInLine: (-> Point Point Point)
nextInLine pt1 pt2 =
  vec2DPlus pt2 (vec2DMinus pt2 pt1) 

-- Point on line segment, at `ratio` location.
-- onLine: (-> Point Point Num Point)
onLine pt1 pt2 ratio =
  let vec = vec2DMinus pt2 pt1 in
  vec2DPlus pt1 (vec2DScalarMult ratio vec) 

-- === Basic Replicate ===

horizontalArray n sep func [x, y] =
  let _ = -- draw point widget to control anchor
    [x, y] : Point in
  let draw_i i =
    let xi = x + i * sep in
    func [xi, y] in
  concat (map draw_i (zeroTo n)) 

linearArrayFromTo n func [xStart, yStart] [xEnd, yEnd] =
  let xsep = (xEnd - xStart) / (n - 1) in
  let ysep = (yEnd - yStart) / (n - 1) in
  let draw_i i =
    let xi = xStart + i * xsep in
    let yi = yStart + i * ysep in
    func [xi, yi] in
  concat (map draw_i (zeroTo n)) 

-- To reduce size of resulting trace,
-- could subtract up to M>1 at a time.
--
floorAndLocalFreeze n =
  if le n 1 then 0 else
  --else
  1    + floorAndLocalFreeze (n - 1) 

-- (let _ ; draw point widget to control anchor
--   ([cx cy] : Point)
radialArray n radius rot func [cx, cy] =
  let center = -- draw ghost circle to control anchor
              -- not using point widget, since it's not selectable
    ghost (circle 'orange' cx cy 20) in
  let _ = -- draw point widget to control radius
    let xWidget = floorAndLocalFreeze cx in
    let yWidget = floorAndLocalFreeze cy - radius in
      [xWidget, yWidget] : Point in
  let endpoints = nPointsOnCircle n rot cx cy radius in
  let bounds =
    [ cx- radius, cy - radius, cx + radius, cy + radius] in
  [group bounds (cons center (concat (map func endpoints)))] 

offsetAnchor dx dy f =
  \\[x, y] -> f [ x+ dx, y + dy] 

horizontalArrayByBounds n sep func [left_0, top, right_0, bot] =
  let w_i = right_0     - left_0 in
  let left_i i = left_0 + i * (w_i + sep) in
  let right_i i = left_i i + w_i in
  let draw_i i = func [left_i i, top, right_i i, bot] in
  let bounds =  [left_0, top, right_i (n - 1), bot] in
    [groupWithPad 30 bounds (concat (map draw_i (zeroTo n)))] 

repeatInsideBounds n sep func[left, top, right, bot]as bounds =
  let w_i = (right - left - sep * (n - 1)) / n in
  let draw_i i =
    let left_i = left + i * (w_i + sep) in
    let right_i = left_i + w_i in
    func [left_i, top, right_i, bot] in
  [groupWithPad 30 bounds (concat (map draw_i (zeroTo n)))] 


draw = svg 

showOne x y val =
   ['text', [['x', x], ['y', y], ['style', 'fill:black'],
            ['font-family', 'monospace'],
            ['font-size', '12pt']],
           [['TEXT', toString val]]] 

show = showOne 20 30 

showList vals =
  ['g', [], mapi \\[i, val] -> showOne 20 ((i + 1) * 30) val vals] 

rectWithBorder stroke strokeWidth fill x y w h =
  addAttr (addAttr
    (rect fill x y w h)
      [\"stroke\", stroke])
      [\"stroke-width\", strokeWidth] 

setStyles newStyles [kind, attrs, children] =
  let attrs =
    -- TODO
    if styleAttr == null
      then [\"style\", []] :: attrs
      else attrs
  in
  let attrs =
    map \\[key, val] ->
      case key of
        \"style\"->
          let otherStyles =
            concatMap \\[k, v] ->
              case elem k (map fst newStyles) of
                True  ->  []
                False -> [[k, v]]
              val in
          [\"style\", append newStyles otherStyles]
        _->
          [key, val]
      attrs
  in
  [kind, attrs, children]

placeAt [x, y] node =
  let _ = [x, y] : Point in
  -- TODO px suffix should be added in LangSvg/Html translation
  setStyles
    [ [\"position\", \"absolute\"],
      [\"left\", toString x + \"px\"],
      [\"top\", toString y + \"px\"]
    ]
    node

placeAtFixed [x, y] node =
  let _ = [x, y] : Point in
  setStyles
    [[\"position\", \"fixed\"], [\"FIXED_LEFT\", x], [\"FIXED_TOP\", y]]
    node

placeSvgAt [x, y] w h shapes =
  placeAt [x, y]
    [\"svg\", [[\"width\", w], [\"height\", h]], shapes]

workspace minSize children =
  div_
    (cons
      (placeAt minSize (h3 \"</workspace>\"))
      children)

-- End SVG Stuff ---------------------------------------------------------------

-- TODO: Refactor this in another module

tutorialUtils = {
  -- Perform basic markdown replacement (titles, newlines, italics)
  -- Do this step BEFORE the htmlpass, so that the code is not parsed.
  markdown =
    Html.replace (\"(?:^|\\n)(#+)\\\\s(.+)\") (\\match ->
      [<@(\"h\" + toString (String.length (nth match.group 1)))>@(nth match.group 2)</@>]
    ) >>
    Html.replace (\"_(?=\\\\S)(.*?)_\") (\\match ->
      [<i>@(nth match.group 1)</i>]) >>
    Html.replace \"(\\r?\\n|  )\\r?\\n\" (\\_ -> [<br>])

  ------ Functions to call during the construction of the document. ------
  ------ After the document is constructed, pass it to `htmlpass`   ------
  ------ to compute the code snippets, the line numbers, etc.       ------
  type alias Instruction = HtmlNode
  type alias Options = {production: Bool}

  -- Sets the current code to 'code'. Not visible in the final document.
  newcode: String -> Instruction
  newcode code = <newcode code=code></newcode>

  -- Replaces the 'placeHolder' by 'code' in the current code. In the final document,
  -- In the final document, displays a message \"Replace [placeHolder] (line XXX) by \"
  replace: String -> String -> Instruction
  replace placeHolder code = <replace placeholder=placeHolder code=code class=\"snippet\"></replace>

  -- Replaces the 'placeHolder' by 'code' in the current code. Not visible in the final document
  -- Can be useful to replace the code if we gave instructions to replace it from the output.
  hiddenreplace: String -> String -> Instruction
  hiddenreplace placeHolder code = <hiddenreplace placeholder=placeHolder code=code></hiddenreplace>

  -- Display the current code. You will use this perhaps only at the beginning and the end,
  -- or for checkpoints.
  displaycode: Instruction
  displaycode = <displaycode class=\"snippet\"></displaycode>

  -- Displays the given code snippet without touching the current code
  displaylocalcode: String -> Instruction
  displaylocalcode code = <displaylocalcode code=code class=\"snippet\"></displaylocalcode>

  -- Evaluates the current code and display its result.
  displayevalcode: String -> Instruction
  displayevalcode = <displayevalcode></displayevalcode>

  -- Performs a regex replacement on the current code and display its result,
  -- but does not modify the current code.
  displayevalcodeLocalReplace: String -> String -> Instruction
  displayevalcodeLocalReplace regex replacement = <displayevalcode replace=regex by=replacement></displayevalcode>

  -- Displays the line of the given snippet as it appears in the current code.
  lineof: String -> Instruction
  lineof snippet = <lineof snippet=snippet></lineof>

  -- Future: Overridable items.
  text_replace_the_code = \"Replace the code\"
  text_with = \"with\"
  text_line = \"line\"
  text_position_unknown = \"position unknown\"

  -- Interpret all 'newcode', 'displaycode', 'displaylocalcode', 'displayevalcode',
  -- 'displayevalcodeLocalReplace', 'lineof' in the given Html node.
  htmlpass: Options -> HtmlNode -> HtmlNode
  htmlpass options =
  let
    displayintermediateresult display src =
      if display then
        case __evaluate__ (__CurrentEnv__) src of
          Err msg -> <code class=\"error\">@msg</code>
          Ok evalNode ->
            <div class=\"outputwrapper\">@evalNode</div>
      else
        <div class=\"intermediateresult\">options.production is off. <button onclick=\"this.setAttribute('v', 'True')\" v=(toString options.production)>Turn it on</button> to display the intermediate result there.</div>

    localReplace src attrs = case attrs of
      [\"replace\", regex]::[\"by\", replacement]::attrs ->
        localReplace (Regex.replace (escape regex) (\\_ -> replacement) src) attrs
      attrs -> (src, attrs)
  in \\htmlnode ->
  let aux src htmlnode = case htmlnode of
    [\"newcode\", [\"code\", code]::attrs, []] ->
      (code, htmlnode)
    [\"hiddenreplace\", [\"placeholder\", placeHolder]::[\"code\", code]::attrs, []] ->
      let newSrc = Regex.replace (escape placeHolder) (\\_ -> code) src in
      (newSrc, htmlnode)
    [\"replace\", [\"placeholder\", placeHolder]::[\"code\", code]::attrs, []] ->
      let newSrc = Regex.replace (escape placeHolder) (\\_ -> code) src in
      (newSrc, <span>@text_replace_the_code <code>@placeHolder</code> (@(positionOf placeHolder src)) @text_with<code @attrs>@code</code></span>)
    [\"lineof\", [\"snippet\", snippet]::attrs, []] ->
      (src, <span>@(positionOf snippet src)</span>)
    [\"displaylocalcode\", [\"code\", code]::attrs, []] ->
      (src, [\"code\", attrs, [[\"TEXT\", code]]])
    [\"displaycode\", attrs, []] ->
      (src, [\"code\", attrs, [[\"TEXT\", src]]])
    [\"displayevalcode\", attrs, []] ->
      let (localSrc, localAttrs) = localReplace src attrs in
      (src, displayintermediateresult options.production <| localSrc + \"\\n\\nmain\")
    [tag, attrs, children] ->
      let (newSrc, newRevChildren) =
        List.foldl (\\child (tmpSrc, revChildren) ->
          let (newTmpSrc, newChild) = aux tmpSrc child in
          (newTmpSrc, newChild::revChildren)
        ) (src, []) children
      in
      (newSrc, [tag, attrs, List.reverse newRevChildren])
    _ -> (src, htmlnode)
  in Tuple.second <| aux \"\" htmlnode

  -- Escapes a string so that we can search it using regexes.
  -- Replaces ... by a regexp that parses any sequence of chars, minimally.
  escape = Regex.escape >> Regex.replace \"\"\"\\\\\\.\\\\\\.\\\\\\.\"\"\" (\\m -> \"\"\"[\\s\\S]+?\"\"\")

  -- Computes the line position of a placeholder inside a string.
  positionOf: String -> String -> String
  positionOf placeHolder code =
    case Regex.extract \"\"\"^([\\s\\S]*?)@(escape placeHolder)([\\s\\S]*)$\"\"\" code of
      Just [before, after] ->
        let line = Regex.split \"\\r?\\n\" before |> List.length |> toString in
        \"\"\"@text_line @line\"\"\"
      _ -> text_position_unknown
}

-- Utilities to invoke some of the usual browser commands
browser = {
  -- Refresh the output
  refresh: String
  refresh = \"\"\"document.querySelector(\".run\") ? document.querySelector(\".run\").click()\"\"\"

  -- Returns the value of the global javascript variable or a placeholder else
  localvar: String -> String -> String
  localvar name initContent = 
    __jsEval__ \"\"\"typeof @name == 'undefined' ? @initContent : @name\"\"\"
    
    
}

media = \"@media\" -- For compatibility with <style>
keyframes = \"@keyframes\"
font = \"@font\"
import = \"@import\"
charSet = \"@charSet\"

-- The type checker relies on the name of this definition.
let dummyPreludeMain = [\"svg\", [], []] in dummyPreludeMain

"""


src = prelude

