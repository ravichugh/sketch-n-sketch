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
-- TODO (re-)organize this section into modules

--; The identity function - given a value, returns exactly that value
-- id: (forall a (-> a a))
id x = x

--; A function that always returns the same value a, regardless of b
-- always: (forall (a b) (-> a b a))
always x _ = x

--; Composes two functions together
--compose: (forall (a b c) (-> (-> b c) (-> a b) (-> a c)))
compose f g = \\x -> f (g x)

--flip: (forall (a b c) (-> (-> a b c) (-> b a c)))
flip f = \\x y -> f y x
-- TODO other version:
-- (def flip (\\(f x y) (f y x)))

--fst: (forall (a b) (-> [a b] a))
--snd: (forall (a b) (-> [a b] b))

fst [a, _] = a
snd [_, b] = b

--; Given a bool, returns the opposite boolean value
--not: (-> Bool Bool)
not b = if b then False else True

--; Given two bools, returns a bool regarding if the first argument is true, then the second argument is as well
--implies: (-> Bool Bool Bool)
implies p q = if p then q else True

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
gt = flip lt
ge x y = or (gt x y) (eq x y)

--; Returns the length of a given list
--len: (forall a (-> (List a) Num))
len xs = case xs of [] -> 0; (_ :: xs1) -> 1 + len xs1

-- TODO remove
freeze x = x

nil = []

cons x xs = x :: xs

zip xs ys =
  case [xs, ys] of
    [x::xsRest, y::ysRest] -> [x,y] :: zip xsRest ysRest
    _                      -> []

range i j =
  if i < j + 1
    then cons i (range (i + 1) j)
    else nil

reverse l =
  letrec r acc l = case l of [] -> acc; head::tail -> r (head::acc) tail in
  { apply = freeze <| r [], update {output}= {values = [r [] output]}}.apply l

map1 f l =
  case l of
    []    -> []
    x::xs -> f x :: map1 f xs

--------------------------------------------------------------------------------
-- TODO re-organize the scattered list definitions into
-- LensLess.List --

LensLess =
  letrec append xs ys =
     case xs of
       [] -> ys
       x::xs1 -> x :: append xs1 ys
  in
  let split n l =
    letrec aux acc n l =
      if n == 0 then [reverse acc, l] else
      case l of
        [] -> [reverse acc, l]
        head::tail -> aux (head::acc) (n - 1) tail
    in aux [] n l in
  let take =
    letrec aux n l = if n == 0 then [] else
      case l of
        [] -> []
        head::tail -> head :: (aux (n - 1) tail)
    in aux in
  let drop =
    letrec aux n l = if n == 0 then l else
      case l of
        [] -> []
        head::tail -> aux (n - 1) tail
    in aux in
  letrec reverse_move n stack from = if n <= 0 then (stack, from) else case from of
    [] -> (stack, from)
    head::tail -> reverse_move (n - 1) (head::stack) tail
  in
  { List = {
      append = append
      split = split
      take = take
      drop = drop
      reverse_move = reverse_move
    },
    Results =
      letrec keepOks l =
        case l of
          [] -> []
          { error }::tail -> keepOks tail
          { values =  ll }::tail -> ll ++ map1 keepOks tail
      in
      letrec projOks l =
        case l of
          [] -> {values = []}
          ({values = []}::tail) -> projOks tail
          ({values = vhead::vtail}::tail) -> {values = vhead::(vtail ++ keepOks tail)}
          ({error = msg}::tail) ->
            case projOks tail of
              {error = msgTail} -> { error = msg }
              { values = []}-> {error = msg}
              result -> result
      in
      let andThen callback results =
        --andThen : (a -> Results x b) -> Results x a -> Results x b
        case results of
          {values = ll} -> ll |> map1 callback |> projOks
          {error = msg} -> results
      in
      let resultMap callback results =
        case results of
          {values = ll} -> {values = ll |> map1 callback }
          { error = msg} -> results
      in
      {
        keepOks = keepOks
        projOks = projOks
        andThen = andThen
        map = resultMap
      }
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
      { apply x = freeze x, update {input} = { values = [input] } }
    in
    applyLens constantInputLens x
  in
  type SimpleListDiffOp = KeepValue | DeleteValue | InsertValue Value | UpdateValue Value
  let listDiffOp diffOp oldValues newValues =
   -- listDiffOp : DiffOp -> List Value -> List Value -> List SimpleListDiffOp

    -- let {Keep, Delete, Insert, Update} = SimpleListDiffOp in
     let {append} = LensLess.List in
     case diffOp oldValues newValues of
        Ok (Just (VListDiffs listDiffs)) ->
          letrec aux i revAcc oldValues newValues listDiffs =
            case listDiffs of
              [] ->
                reverse (map1 (\\_ -> KeepValue) oldValues ++ revAcc)
              (j, listDiff)::diffTail ->
                if j > i then
                  case [oldValues, newValues] of
                    [_::oldTail, _::newTail] ->
                      aux (i + 1) (KeepValue::revAcc) oldTail newTail listDiffs
                    _ -> error <| \"[Internal error] Expected two non-empty tails, got  \" + toString [oldValues, newValues]
                else if j == i then
                  case listDiff of
                    ListElemUpdate _ ->
                      case [oldValues, newValues] of
                        [oldHead::oldTail, newHead::newTail] ->
                          aux (i + 1) (UpdateValue newHead :: revAcc) oldTail newTail diffTail
                        _ -> error <| \"[Internal error] update but missing element\"
                    ListElemInsert count ->
                      case newValues of
                        newHead::newTail ->
                          aux i (InsertValue newHead::revAcc) oldValues newTail (if count == 1 then diffTail else (i, ListElemInsert (count - 1))::diffTail)
                        _ -> error <| \"[Internal error] insert but missing element\"
                    ListElemDelete count ->
                      case oldValues of
                        oldHead::oldTail ->
                          aux (i + 1) (DeleteValue::revAcc) oldTail newValues (if count == 1 then diffTail else (i + 1, ListElemDelete (count - 1))::diffTail)
                        _ -> error <| \"[Internal error] insert but missing element\"
                else error <| \"[Internal error] Differences not in order, got index \" + toString j + \" but already at index \" + toString i
          in aux 0 [] oldValues newValues listDiffs

        result -> error (\"Expected Ok (Just (VListDiffs listDiffs)), got \" + toString result)
  in
  -- exports from Update module
  { freeze x =
      -- eta-expanded because \"freeze x\" is a syntactic form for U-Freeze
      freeze x

    applyLens lens x =
      -- \"f.apply x\" is a syntactic form for U-Lens, but eta-expanded anyway
      applyLens lens x

    softFreeze = softFreeze
    listDiffOp = listDiffOp
    updateApp  = __updateApp__
    diff = __diff__
    merge = __merge__
    listDiff = listDiffOp __diff__
    mapInserted f originalStr modifiedStr =
      -- TODO: really map the insertions in the modified string by f: String -> String
      modifiedStr
  }

--------------------------------------------------------------------------------
-- Update.foldDiff

-- type Results err ok = { values: List ok } | { error: err }

-- every onFunction should either return a {values = ...} or an {error =... }
-- start    : a
-- onUpdate : a -> {oldOutput: b, newOutput: b, index: Int, diffs: VDiffs} -> Results String a
-- onInsert : a -> {newOutput: b, index: Int, diffs: VDiffs}  -> Results String a
-- onRemove : a -> {oldOutput: b, index: Int, diffs! VDoffs}  -> Results String a
-- onSkip   : a -> {count: Int, index: Int, oldOutputs: List b, newOutputs: List b}  -> Results String a
-- onFinish : a -> Results String c
-- onGather : c -> ({value: d, diff: Maybe VDiffs } | { value: d })
-- oldOutput: List b
-- newOutput: List b
-- diffs    : ListDiffs
-- Returns  : {error: String} | {values: List d} | {values: List d, diffs: List (Maybe VDiffs)}
foldDiff =
  let {List, Results} = LensLess in
  let {append, split} = List in
  \\{start, onSkip, onUpdate, onRemove, onInsert, onFinish, onGather} oldOutput newOutput diffs ->
  let listDiffs = case diffs of
    VListDiffs l -> l
    _ -> error <| \"Expected VListDiffs, got \" + toString diffs
  in
  -- Returns either {error} or {values=list of values}
  --     fold: Int -> List b -> List b -> List (Int, ListElemDiff) -> a -> Results String c
  letrec fold  j      oldOutput  newOutput  listDiffs                    acc =
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
          let [previous, remainingOld] = split count oldOutput in
          let [current,  remainingNew] = split count newOutput in
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
            else error <| \"insertion count should be >= 1, got \" + toString count
          ListElemDelete count ->
            if count >= 1 then
              let dropped::remainingOld = oldOutput in
              onRemove acc {oldOutput =dropped, index = i} |>
              next (i + count) remainingOld newOutput (if count == 1 then dtail else (i + 1, ListElemDelete (count - 1))::dtail)
            else error <| \"deletion count should be >= 1, got \" ++ toString count
      _ -> error <| \"Expected a list of diffs, got \" + toString diffs
  in
  case fold 0 oldOutput newOutput listDiffs start of
    { error = msg } -> {error = msg}
    { values = values } -> -- values might be a pair of value and diffs. We use onGather to do the split.
      letrec aux revAccValues revAccDiffs values = case values of
        [] -> case revAccDiffs of
          Nothing -> {values = reverse revAccValues}
          Just revDiffs -> {values = reverse revAccValues, diffs = reverse revDiffs}
        head::tail -> case onGather head of
          {value, diff} -> case revAccDiffs of
            Nothing -> if len revAccValues > 0 then { error = \"Diffs not specified for all values, e.g.\" + toString value } else
              aux [value] (Just [diff]) tail
            Just revDiffs ->
              aux (value :: revAccValues) (Just (diff::revDiffs)) tail
          {value} -> case revAccDiffs of
            Nothing -> aux (value :: revAccValues) revAccDiffs tail
            Just revDiffs -> { error = \"Diffs not specified until \" + toString value }
      in aux [] Nothing values

--------------------------------------------------------------------------------
-- ListLenses --

append aas bs = {
    apply [aas, bs] = freeze <| LensLess.List.append aas bs
    update {input = [aas, bs], outputNew, outputOld, diffs} =
      let asLength = len aas in
      foldDiff {
        start = [[], [], [], [], len aas, len bs]
        onSkip [nas, nbs, diffas, diffbs, numA, numB] {count = n, newOutputs = outs} =
          if n <= numA then
            {values = [[nas ++ outs, nbs, diffas, diffbs, numA - n, numB]]}
          else
            let [forA, forB] = LensLess.List.split numA outs in
            {values = [[nas ++ forA, nbs ++ forB, diffas, diffbs, 0, numB - (n - numA)]]}
        onUpdate [nas, nbs, diffas, diffbs, numA, numB] {newOutput = out, diffs, index} =
          { values = [if numA >= 1
           then [nas ++ [out],                                      nbs,
                 diffas ++ [(index, ListElemUpdate diffs)], diffbs,
                 numA - 1,                                          numB]
           else [nas,    nbs ++ [out],
                 diffas, diffbs ++ [(index - asLength, ListElemUpdate diffs)],
                 0,      numB - 1]] }
        onRemove  [nas, nbs, diffas, diffbs, numA, numB] {oldOutput, index} =
          if 1 <= numA then
            {values = [[nas, nbs, diffas ++ [(index, ListElemDelete 1)], diffbs, numA - 1, numB]] }
          else
            {values = [[nas, nbs, diffas, diffbs ++ [(index - asLength, ListElemDelete 1)], numA, numB - 1]] }
        onInsert [nas, nbs, diffas, diffbs, numA, numB] {newOutput, index} =
          {values =
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
            }

        onFinish [nas, nbs, diffas, diffbs, _, _] = {
           values = [[[nas, nbs], (if len diffas == 0 then [] else
             [(0, ListElemUpdate (VListDiffs diffas))]) ++
                   (if len diffbs == 0 then [] else
             [(1, ListElemUpdate (VListDiffs diffbs))])]]
          }
        onGather [[nas, nbs], diffs] = {value = [nas, nbs],
          diff = if len diffs == 0 then Nothing else Just (VListDiffs diffs) }
      } outputOld outputNew diffs
    }.apply [aas, bs]

--; Maps a function, f, over a list of values and returns the resulting list
--map: (forall (a b) (-> (-> a b) (List a) (List b)))
map f l =
  {
  apply [f, l] = freeze (map1 f l)
  update {input=[f, l], oldOutput, outputNew, diffs} =
    foldDiff {
      start =
        --Start: the collected functions, the collected inputs, the inputs yet to process.
        [[], [], l]


      onSkip [fs, insA, insB] {count} =
        --'outs' was the same in oldOutput and outputNew
        let [skipped, remaining] = LensLess.List.split count insB in
        {values = [[fs, insA ++ skipped, remaining]]}

      onUpdate [fs, insA, insB] {oldOutput, newOutput, diffs} =
        let input::remaining = insB in
        case Update.updateApp {fun [f,x] = f x, input = [f, input], output = newOutput, oldOutput = oldOutput, diffs = diffs} of
          { error = msg } -> {error = msg}
          { values = v } -> {values = v |>
              map1 (\\[newF, newA] -> [newF :: fs, insA ++ [newA], remaining])}

      onRemove [fs, insA, insB] {oldOutput} =
        let _::remaining = insB in
        { values = [[fs, insA, remaining]] }

      onInsert [fs, insA, insB] {newOutput} =
        let input = case insB of h::_ -> h; _ -> case insA of h::_ -> h; _ -> error \"Empty list for map, cannot insert\" in
        case Update.updateApp {fun [f,x] = f x, input = [f, input], output = newOutput} of
          { error = msg } -> {error = msg }
          { values = v} -> {values = v |>
              map (\\[newF, newA] -> [newF::fs, insA++[newA], insB])}

      onFinish [newFs, newIns, _] =
       --after we finish, we need to return the new function
       --as a merge of original functions with all other modifications
       -- and the collected new inputs
       {values = [[Update.merge f newFs, newIns]] }

      onGather result =
        -- TODO: Later, include the , diff= here.
        { value = result }
    } oldOutput outputNew diffs
  }.apply [f, l]

zipWithIndex xs =
  { apply x = freeze <| zip (range 0 (len xs - 1)) xs
    update {output} = {values = [map (\\[i, x] -> x) output]}  }.apply xs

-- TODO re-organize the scattered list definitions into
-- LensLess.List, ListLenses, and List = LensLess.List

ListLenses =
  { map = map
    append = append
    zipWithIndex = zipWithIndex
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

adjacentPairs xs = zip xs (tl xs)

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
  if n < 0 then null
  else
    case [n, xs] of
      [_, []]     -> null
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
--   (letrec take_ (\\(n xs)
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
--     {Error: typecase not yet implemented for Elm syntax}

--elem: (forall a (-> a (List a) Bool))
elem x ys =
  case ys of
    []     -> False
    y::ys1 -> or (x == y) (elem x ys1)

sortBy f xs =
  letrec ins x ys =   -- insert is a keyword...
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

--; Absolute value
--abs: (-> Num Num)
abs x = if x < 0 then neg x else x

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

--; Given an upper bound, lower bound, and a number, restricts that number between those bounds (inclusive)
--; Ex. clamp 1 5 4 = 4
--; Ex. clamp 1 5 6 = 5
--clamp: (-> Num Num Num Num)
clamp i j n = if n < i then i else if j < n then j else n

--between: (-> Num Num Num Bool)
between i j n = n == clamp i j n

--plus: (-> Num Num Num)
plus x y = x + y

--min: (-> Num Num Num)
min i j = if lt i j then i else j

--max: (-> Num Num Num)
max i j = if gt i j then i else j

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
}


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
--         (zip paddedCol rows))))
--     (repeat numRows [])
--     columns)
-- ))))
--
-- (def addColToRows (\\(col rows)
--   (let width (maximum (map len rows))
--   (letrec foo (\\(col rows)
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

-- Returns a list of HTML nodes parsed from a string. It uses the API for loosely parsing HTML
-- Example: html \"Hello<b>world</b>\" returns [[\"TEXT\",\"Hello\"],[\"b\",[], [[\"TEXT\", \"world\"]]]]
html string = {
  apply trees =
    freeze (letrec domap tree = case tree of
      HTMLInner v -> [\"TEXT\",
        replaceAllIn \"&nbsp;|&amp;|&lt;|&gt;|</[^>]*>\" (\\{match} ->
          case match of \"&nbsp;\" -> \" \"; \"&amp;\" -> \"&\"; \"&lt;\" -> \"<\"; \"&gt;\" -> \">\"; _ -> \"\") v]
      HTMLElement tagName attrs ws1 endOp children closing ->
        [ tagName
        , map (case of
          HTMLAttribute ws0 name value -> case value of
            HTMLAttributeUnquoted _ _ content -> [name, content]
            HTMLAttributeString _ _ _ content -> [name, content]
            HTMLAttributeNoValue -> [name, \"\"]) attrs
        , map domap children]
      HTMLComment {args = [content]} -> [\"comment\", [[\"display\", \"none\"]], [[\"TEXT\", content]]]
    in map domap trees)

  update {input, oldOutput, newOutput, diffs} =
    let toHTMLAttribute [name, value] = HTMLAttribute \" \" name (HTMLAttributeString \"\" \"\" \"\\\"\" value) in
    let toHTMLInner text = HTMLInner (replaceAllIn \"<|>|&\" (\\{match} -> case match of \"&\" -> \"&amp;\"; \"<\" -> \"&lt;\"; \">\" -> \"&gt;\"; _ -> \"\") text) in
    letrec toHTMLNode e = case e of
      [\"TEXT\",v2] -> toHTMLInner v2
      [tag, attrs, children] -> HTMLElement tag (map toHTMLAttribute attrs) \"\"
           RegularEndOpening (map toHTMLNode children) (RegularClosing \"\")
    in
    let mergeAttrs input oldOutput newOutput diffs =
      foldDiff {
        start = 
          -- Accumulator of HTMLAttributes, accumulator of differences, original list of HTMLAttributes
          ([], [], input)
        onSkip (revAcc, revDiffs, input) {count} =
          --'outs' was the same in oldOutput and outputNew
          let (newRevAcc, remainingInput) = LensLess.List.reverse_move count revAcc input in
          {values = [(newRevAcc, revDiffs, remainingInput)]}
        
        onUpdate (revAcc, revDiffs, input) {oldOutput, newOutput, diffs, index} =
          let inputElem::inputRemaining = input in
          let newInputElem = case (inputElem, newOutput) of
            (HTMLAttribute sp0 name value, [name2, value2 ]) ->
             case value of
               HTMLAttributeUnquoted sp1 sp2 v ->
                 case extractFirstIn \"\\\\s\" v of
                   Nothing ->
                     HTMLAttribute sp0 name2 (HTMLAttributeUnquoted sp1 sp2 value2)
                   _ ->
                     HTMLAttribute sp0 name2 (HTMLAttributeString sp1 sp2 \"\\\"\" value2)
               HTMLAttributeString sp1 sp2 delim v ->
                     HTMLAttribute sp0 name2 (HTMLAttributeString sp1 sp2 delim value2)
               HTMLAttributeNoValue ->
                  if value2 == \"\" then HTMLAttribute sp0 name2 (HTMLAttributeNoValue)
                  else toHTMLAttribute [name2, value2]
               _ -> error <| \"expected HTMLAttributeUnquoted, HTMLAttributeString, HTMLAttributeNoValue, got \" ++ toString (inputElem, newOutput)
            _ -> error \"Expected HTMLAttribute, got \" ++ toString (inputElem, newOutput)
          in
          let newRevDiffs = case Update.diff inputElem newInputElem of
            Ok (Just d) -> (index, ListElemUpdate d)::revDiffs
            Ok (Nothing) ->  revDiffs
            Err msg -> error msg
          in
          {values = [(newInputElem::revAcc, newRevDiffs, inputRemaining)]}

        onRemove (revAcc, revDiffs, input) {oldOutput, index} =
          let _::remainingInput = input in
          { values = [(revAcc, (index, ListElemDelete 1)::revDiffs, remainingInput)] }
        
        onInsert (revAcc, revDiffs, input) {newOutput, index} =
          { values = [(toHTMLNode newOutput :: revAcc, (index, ListElemInsert 1)::revDiffs, input)]}
          
        onFinish (revAcc, revDiffs, _) =
         {values = [(reverse revAcc, reverse revDiffs)] }

        onGather (acc, diffs) =
          { value = acc,
             diff = if len diffs == 0 then Nothing else Just (VListDiffs diffs) }
      } oldOutput newOutput diffs
    in
    -- Returns {values = List (List HTMLNode)., diffs = List (Maybe ListDiff)} or { error = ... }
    letrec mergeNodes input oldOutput newOutput diffs =
      foldDiff {
        start =
          -- Accumulator of values, accumulator of differences, original input
          ([], [], input)

        onSkip (revAcc, revDiffs, input) {count} =
          --'outs' was the same in oldOutput and outputNew
          let (newRevAcc, remainingInput) = LensLess.List.reverse_move count revAcc input in
          {values = [(newRevAcc, revDiffs, remainingInput)]}

        onUpdate (revAcc, revDiffs, input) {oldOutput, newOutput, diffs, index} =
          let inputElem::inputRemaining = input in
          --Debug.start (\"onUpdate\" + toString (oldOutput, newOutput, diffs, index)) <| \\_ ->
          let newInputElems = case (inputElem, oldOutput, newOutput) of
            ( HTMLInner v, _, [\"TEXT\",v2]) -> { values = [toHTMLInner v2] }
            ( HTMLElement tagName attrs ws1 endOp children closing,
              [tag1, attrs1, children1], [tag2, attrs2, children2] ) ->
               if tag2 == tagName then
                 case diffs of
                   VListDiffs listDiffs ->
                     let (newAttrsMerged, otherDiffs) = case listDiffs of
                       (1, ListElemUpdate diffAttrs)::tailDiff ->
                         (mergeAttrs attrs attrs1 attrs2 diffAttrs, tailDiff)
                       _ -> ({values = [attrs]}, listDiffs)
                     in
                     let newChildrenMerged = case otherDiffs of
                       (2, ListElemUpdate diffNodes)::_ ->
                         mergeNodes children children1 children2 diffNodes
                       _ -> {values = [children]}
                     in
                     newAttrsMerged |>LensLess.Results.andThen (\\newAttrs ->
                       newChildrenMerged |>LensLess.Results.andThen (\\newChildren ->
                         {values = [HTMLElement tag2 newAttrs ws1 endOp newChildren closing]}
                       )
                     )
               else {values = [toHTMLNode newOutput]}
            _ -> {values = [toHTMLNode newOutput]}
          in
          newInputElems |>LensLess.Results.andThen (\\newInputElem ->
            --Debug.start (\"newInputElem:\" + toString newInputElem) <| \\_ ->
            case Update.diff inputElem newInputElem of
              Err msg -> {error = msg}
              Ok maybeDiff ->
                let newRevDiffs = case maybeDiff of
                  Nothing -> revDiffs
                  Just v -> (index, ListElemUpdate v)::revDiffs in
                {values = [ (newInputElem::revAcc, newRevDiffs, inputRemaining) ]}
          )

        onRemove (revAcc, revDiffs, input) {oldOutput, index} =
          let _::remainingInput = input in
          { values = [(revAcc, (index, ListElemDelete 1)::revDiffs, remainingInput)] }

        onInsert (revAcc, revDiffs, input) {newOutput, index} =
          { values = [(toHTMLNode newOutput :: revAcc, (index, ListElemInsert 1)::revDiffs, input)]}

        onFinish (revAcc, revDiffs, _) =
         {values = [(reverse revAcc, reverse revDiffs)] }

        onGather (acc, diffs) =
          { value = acc,
             diff = if len diffs == 0 then Nothing else Just (VListDiffs diffs)}
      } oldOutput newOutput diffs
    in mergeNodes input oldOutput newOutput diffs
}.apply (parseHTML string)

--------------------------------------------------------------------------------
-- Regex --

Regex =
  letrec split regex s =
    case extractFirstIn (\"^([\\\\s\\\\S]*)(\" + regex + \")([\\\\s\\\\S]*)$\") s of
      Just [before, _, after] -> before :: split regex after
      _ -> [s]
  in
  letrec find regex s =
    case extractFirstIn (\"(\" + regex + \")([\\\\s\\\\S]*)\") s of
      Nothing -> []
      Just matchremaining ->
        case LensLess.List.split (len matchremaining - 1) matchremaining of
          [init, [last]] ->
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
  find = find
}

--------------------------------------------------------------------------------
-- Dict --

Dict = {
  remove x d = remove x d
  get x d = get x d
  apply d x = case get x d of
    Just x -> x
    _ -> error (\"Expected element \" + toString x + \" in dict, got nothing\")
  insert k v d = insert k v d
  fromList l = dict l
  member x d = case get x d of
    Just _ -> True
    _ -> False
}

--------------------------------------------------------------------------------
-- String --

String =
  let strToInt =
    let d = Dict.fromList [(\"0\", 0), (\"1\", 1), (\"2\", 2), (\"3\", 3), (\"4\", 4), (\"5\", 5), (\"6\", 6), (\"7\", 7), (\"8\", 8), (\"9\", 9)] in
    letrec aux x =
      case extractFirstIn \"^([0-9]*)([0-9])$\" x of
        Just [init, last] -> (aux init)*10 + Dict.apply d last
        Nothing -> 0
    in
    aux
  in
  let join delimiter list =
    letrec aux acc list = case list of
      [] -> acc
      [head] -> acc + head
      (head::tail) -> aux (acc + head + freeze delimiter) tail
    in aux \"\" list
  in
  { toInt x =
      { apply x = freeze <| strToInt x
      , unapply output = Just (toString output)
      }.apply x
    join delimiter x = {
        apply x = join delimiter x
        update {output, oldOutput, diffs} =
          if delimiter == \"\" then
            -- Regular update, cannot add elements
            -- TODO: We should be able to delete elements if they completely disappear !
            Update.updateApp {fun = join delimiter, oldOutput = oldOutput, output = output, diffs = diffs}
          else {values = [Regex.split delimiter output]}
      }.apply x
    length x = len (explode x)
    substring start end x =
      case Regex.extract (\"^[\\\\s\\\\S]{0,\" + toString start + \"}([\\\\s\\\\S]{0,\" + toString (end - start) + \"})\") x of
        Just [substr] -> substr
        Nothing -> error <| \"bad arguments to String.substring \" + toString start + \" \" + toString end + \" \" + toString x
    take length x =
      case Regex.extract (\"^([\\\\s\\\\S]{0,\" + toString length + \"})\") x of
        Just [substr] -> substr
        Nothing -> error <| \"bad arguments to String.take \" + toString length + \" \" + toString x
    drop length x =
      case Regex.extract (\"^[\\\\s\\\\S]{0,\" + toString length + \"}([\\\\s\\\\S]*)\") x of
        Just [substr] -> substr
        Nothing -> error <| \"bad arguments to String.drop \" + toString length + \" \" + toString x

  }


--------------------------------------------------------------------------------
-- List --
-- TODO re-organize the scattered list definitions into
-- LensLess.List, ListLenses, and List = LensLess.List

List =
  letrec simpleMap f l =
    case l of
      []    -> []
      x::xs -> f x :: simpleMap f xs
  in
  let map =
    simpleMap
  in
  -- TODO move all definitions here
  let length =
    len
  in
  let nth =
    nth
  in
  let mapi f xs = map f (zipWithIndex xs) in
  let indexedMap f xs =
    mapi (\\[i,x] -> f i x) xs
  in
  let cartesianProductWith f xs ys =
    concatMap (\\x -> map (\\y -> f x y) ys) xs
  in
  letrec unzip xys =
    case xys of
      []          -> ([], [])
      (x,y)::rest -> let (xs,ys) = unzip rest in
                     (x::xs, y::ys)
  in
  let split n l = {
      apply [n, l] = freeze (LensLess.List.split n l)
      unapply [l1, l2] = Just [n, l1 ++ l2]
    }.apply [n, l]
  in
  letrec reverseInsert elements revAcc =
    case elements of
      [] -> revAcc
      head::tail -> reverseInsert tail (head::revAcc)
  in
  { simpleMap = simpleMap
    map = map
    nil = nil
    cons = cons
    length = length
    nth = nth
    indexedMap = indexedMap
    cartesianProductWith = cartesianProductWith
    unzip = unzip
    split = split
    reverseInsert = reverseInsert
    reverse = reverse
    take = LensLess.List.take
    drop = LensLess.List.drop
    foldl = foldl
  }

--------------------------------------------------------------------------------
-- Maybe --

Maybe =
  type Maybe a = Nothing | Just a
  let withDefault x mb = case mb of
    Nothing -> x
    Just j -> j
  in
  let withDefaultLazy lazyX mb = case mb of
    Nothing -> lazyX []
    Just j -> j
  in
  { withDefault = withDefault
    withDefaultLazy = withDefaultLazy
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
  }

--------------------------------------------------------------------------------
-- Editor --

Editor = {}

-- TODO remove this; add as imports as needed in examples
{freeze, applyLens} = Update

-- Custom Update: List Map, List Append, ...

-- TODO

-- HTML


-- Returns a list of one text element from a string, and updates by taking all the pasted text.
textInner s = {
  apply s = freeze [[\"TEXT\", s]]
  update {output} =
    letrec textOf = case of
      [\"TEXT\", s]::tail -> s + textOf tail
      [tag, attrs, children]::tail ->
        textOf children + textOf tail
      _ -> \"\"
    in
    {values = [textOf output]}
}.apply s

--------------------------------------------------------------------------------
-- Html

Html =
  let textNode text =
    [\"TEXT\", text]
  in
  let textElementHelper tag styles attrs text =
    [ tag,  [\"style\", styles] :: attrs , textInner text ]
  in
  let elementHelper tag styles attrs children =
    [ tag,  [\"style\", styles] :: attrs , children ]
  in
  { textNode = textNode
    p = textElementHelper \"p\"
    th = textElementHelper \"th\"
    td = textElementHelper \"td\"
    h1 = textElementHelper \"h1\"
    h2 = textElementHelper \"h2\"
    h3 = textElementHelper \"h3\"
    h4 = textElementHelper \"h4\"
    h5 = textElementHelper \"h5\"
    h6 = textElementHelper \"h6\"
    div = elementHelper \"div\"
    tr = elementHelper \"tr\"
    table = elementHelper \"table\"
    span = elementHelper \"span\"
    b= elementHelper \"b\"
    i= elementHelper \"i\"
    element = elementHelper
    text = textInner
    br = [\"br\", [], []]
  }

-- TODO remove this; add as imports as needed in examples
{textNode, p, th, td, h1, h2, h3, tr, table} = Html
div_ = Html.div

--------------------------------------------------------------------------------
-- Lens: Table Library

  -- Update.freeze and Update.softFreeze aren't needed below,
  -- because library definitions are implicitly frozen.
  -- But for performance it's better.

  -- TODO in wrapData, use update. calculate length of rows to determine empties.

TableWithButtons =
  let wrapData =
    Update.applyLens
      { apply rows   = freeze <| (rows |> List.map (\\row -> (freeze False, row)))
      , unapply rows = rows |> concatMap (\\(flag,row) ->
                                 if flag == True
                                   then [ row, [\"?\",\"?\",\"?\"] ]
                                   else [ row ]
                               )
                            |> (\\x -> Just x)
      }
  in
  let mapData f =
    List.map (Tuple.mapSecond f)
  in
  --
  -- The globalBool flag is used to determined whether to insert \"\" or \" \"
  -- before a couple attribute values. Toggling between these two choices
  -- in subsequent runs helps work around our issue forcing Elm to re-render.
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
        , [\"style\", [[\"color\", \"\"\"@dummyStrPrefix@nope\"\"\"]]]
        ]
      , [textNode \"+\"]
      ]
    in
    Html.tr styles
      ([hasBeenClicked, \"\"\"@dummyStrPrefix@flag\"\"\"] :: attrs)
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
  let oldShapesI = zip (list1N (len oldShapes)) oldShapes in
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
    polygon fill stroke w (map pti (zip indices lengths))

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
    [ if xBall_ == xBall then circle if val then 'darkgreen' else 'darkred' xBall y rBall else
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
enumSlider x0 x1 ya::_as enum caption srcVal =
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
  let shapesCapsSeeds = zip shapesCaps (take seeds (len shapesCaps)) in
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

-- Convert radians to degrees
-- radToDeg: (-> Num Num)
radToDeg rad = rad / pi * 180! 

-- Convert degrees to radians
-- degToRad: (-> Num Num)
degToRad deg = deg / 180! * pi 

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
  letrec pointsOf cmds =
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
  letrec toPath cmds =
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
  letrec pointsOf cmds =
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


-- The type checker relies on the name of this definition.
let dummyPreludeMain = [\"svg\", [], []] in dummyPreludeMain

"""


src = prelude

