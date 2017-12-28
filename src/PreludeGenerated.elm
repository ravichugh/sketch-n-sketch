module PreludeGenerated exposing (src)

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

; The type checker relies on the name of this definition.
(let dummyPreludeMain ['svg' [] []] dummyPreludeMain)

"""


src = prelude

