module ExamplesGenerated (list, scratchName, scratch) where

import Lang
import LangParser2 as Parser
import Eval
import MicroTests
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

(def reddiv 
  (eStyle 
    [ [ 'top'    100 ]
      [ 'left'   100 ]
      [ 'background-color' 'orange' ]
      ['padding' 20 ]
      ['margin'  40 ]
    ] (eDiv 100 100 []) ) )
(basicDoc [['background-color' 'lightblue']] [reddiv])

"

basicText =
 "; Demonstration of simple text capabilities

(def simpleText 'Dorian is swell ')
(def italics (\\str ['i' [] [(text str)]]))
(def bold    (\\str ['b' [] [(text str)]]))
(basicDoc [] 
  [ (h1 'Header 1')
    (h2 'Header 2')
    (h3 'Header 3')
    (h4 'Header 4')
    (h5 'Header 5')
    (h6 'Header 6')
    (a 'A link' 'https://ravichugh.github.io/sketch-n-sketch/')
    (ul [ (li 'Unordered list')
          (li 'Unordered list') ] )
    (ol [ (li 'Ordered list')
          (li 'Ordered list') ] )
    hr
    (text simpleText)
    (italics simpleText)
    (bold simpleText)
  ] )

"

threeDivs =
 "; Three Divs
(def threeDivsInt
  (let [left0 top0 w h sep] [40 28 60 130 110]
  (let divi (\\i
    (let lefti (+ left0 (* i sep))
    (eStyle [ [ 'top'  top0  ]
              [ 'left' lefti ]
              [ 'background-color' 'lightblue' ] ]
            (eDiv w h []) ) ) )
  (basicDoc [] (map divi [0! 1! 2!])) ) ) )

threeDivsInt

"

image =
 "; Example usage of an image

(def imgURL 'https://ravichugh.github.io/sketch-n-sketch/static/images/sketch-n-sketch-logo-gray.svg')
(basicDoc [] 
  [ (eStyle [ ['top' 100]
              ['left' 100] ]
            (eImg 200 200 imgURL) ) ] )

"

simpleNavBar =
 "; A simple navigation bar

(def [buttonwd buttonht] [120 40])
(def [menuy0 menux0 buttonspacing] [100 50 140])
(def nicelink (\\([name url] [x y])
  (eStyle [ [ 'top' y ]
            [ 'left' x] ]
  ['a' [['href' url]]
    [ (eStyle [ [ 'display' 'block' ]
                [ 'background-color' 'royalblue' ] 
                [ 'background-color:hover' 'dodgerblue' ]
                [ 'text-align' 'center'          ]
                [ 'top' y                        ] 
                [ 'left' x                       ] 
                [ 'text-decoration' 'none' ]
                [ 'color' 'white' ]
                [ 'font-family' 'sans-serif' ] ]
      (eDiv buttonwd buttonht
        [(span [ [ 'top' (+ 'calc(' (+ (toString (/ buttonht 2)) 'px - 0.5 * 1em)')) ] 
                 [ 'position' 'relative' ] ]
               [(text name)] )]) ) ] ] ) ) )
(def placeButtoni (\\i [(+ menux0 (* i buttonspacing)) menuy0]))
(def buttons 
  [ ['Home'       '.']
    ['About Us'   '.']
    ['News'       '.']
    ['Links'      '.']
    ['Contact Us' '.'] ] ) 
(def navBar (map2 nicelink
                    buttons
                    (map placeButtoni [0! 1! 2! 3! 4!]) ) )
(basicDoc [] navBar)

"

table =
 "; A simple table

(def headers
  [ 'Who' 'Entrance Survey Guess' 'Exit Survey Guess' ])
(def data
  [ ['Ravi' '25' '15']
    ['Mitch' '15' '4']
    ['Jacob' '88' '6']
  ])
(def attrs
  [ ['border' '1px solid black'] 
    ['border-collapse' 'collapse']
    ['text-align' 'center']
    ['background-color:nth-child(even)' '#fff']
    ['background-color:nth-child(odd)' '#eee']
  ])
(basicDoc [] 
  [ (eStyle [ ['top' 100]
              ['left' 100] ]
              (eTable 600 120 headers data attrs) ) ] )

"

complextable =
 "; A more complex table with specific attribute control

(def headers
  [ 'Who' 'Entrance Survey Guess' 'Exit Survey Guess' ])
(def data
  [ ['Ravi' '25' '15']
    ['Mitch' '15' '4']
    ['Jacob' '88' '6']
  ])
(def hattrs
  [ ['border' '1px solid black'] 
    ['border-collapse' 'collapse']
    ['background-color:nth-child(even)' '#fff']
    ['background-color:nth-child(odd)' '#eee']
  ])
(def rattrs
  [ ['border' '1px solid black'] 
    ['border-collapse' 'collapse']
    ['background-color:nth-child(even)' '#fff']
    ['background-color:nth-child(odd)' '#eee']
  ])
(def dattrs
  [ ['border' '1px solid black'] 
    ['border-collapse' 'collapse']
    ['text-align' 'center']
  ])
(basicDoc [] 
  [ (eStyle [ ['top' 100]
              ['left' 100] ]
              (eComplexTable 600 120 headers data hattrs rattrs dattrs) ) ] )
"

hover =
 "; An example usage of a div that changes when hovered over
(def hovdiv
  (let [top left w h col]   
       [100 100 200 200 'blue']
  (let [hovtop hovleft hovw hovh hovcol]
       [(- top 20)  (- left 20)  (+ w 40)  (+ h 40)  'lightblue'  ]
  (eStyle [ [ 'top'              top        ]
            [ 'left'             left       ]
            [ 'width'            w          ]
            [ 'height'           h          ]
            [ 'background-color' col        ] 
            [ 'position'         'absolute' ] ]
  (eStyle [ [ 'top:hover'              hovtop     ]
            [ 'left:hover'             hovleft    ]
            [ 'width:hover'            hovw       ]
            [ 'height:hover'           hovh       ]
            [ 'background-color:hover' hovcol     ] 
            [ 'position:hover'         'absolute' ] ]
  ['div' [] []] ) ) ) ) )
(basicDoc [] [hovdiv])

"

basicPage =
 "; A basic webpage, completely in little
; Based off of: http://astro.uchicago.edu/ras/
(def [spacing hrHt hrThickness pWd lMargin] [20 10 5 1000 20])
(defrec intersperseHr (\\listElems (case listElems
  ([] [])
  ([last] [last])
  ([elem | rest] 
    [elem ['hr' [ ['position' 'absolute'] 
                  ['size' hrThickness]
                  ['width' '100%'] ] []] | (intersperseHr rest)]) ) ) )
(defrec findAttrVal (\\(str list) (case list
  ([] hrHt)
  ([ [ key val ] | rest ] (if (= str key) val (findAttrVal str rest))) ) ) )
(defrec flowDown (\\(space elems offset) (case elems
  ([] [])
  ([ [name attrs children] | rest ] 
    [ (eStyle [ ['top' offset] ] [name attrs children]) | 
        (flowDown space rest 
            (+ (findAttrVal 'height' attrs) (+ space offset)) ) ] ) ) ) )
(def header (eDiv pWd 200
  [(eStyle [['top' 0] ['left' 0]] (eImg 200 200 'http://astro.uchicago.edu/ras/ras-logo-transparent.png'))
   (eStyle [['top' 0] ['left' 200]] (h1 \"Ryerson Astronomical Society')) ] ) )
(def about (eDiv pWd 100
  [(p [] [(text 'The Ryerson Astronomical Society (RAS) is the University of Chicago astronomy club. Our purpose is to have all humanity (or all University of Chicago students, for starters) observe the celestial luminaries in hushed awe with our antique 6.25\" refractor, 10\" reflector, and ~60\" radio telescope. Our website has information about upcoming club events; details of RAS history; a collection of astrophotographs and RAS photos; the Rchive, a vast index of RAS documents; useful space and astronomy links; and contact information for our members.')])] ) )
(def activities (eDiv pWd 110
  [(h2 'RAS Activities') (p [] [(text 'Information about special RAS eventes will be posted here. The RAS will also hold Monday meetings and public observation (Ryerson Wednesdays) each week (see below).')] )] ) )
(def talks (eDiv pWd 400
  [(p [] [(text 'Monday Meetings: We meet to hear presentations on astronomy-related topics from students and faculty every Monday night at 8 PM in Ryerson 358 (the classroom immediately at the top of the main staircase in Ryerson). To sign up to give a talk, email John Roberts or come to a Monday meeting and write your name and tentative subject on the talk schedule.')]) ] ) )
(def wholePage (intersperseHr (map (eStyle [['left' lMargin]]) [header about activities talks])))
(basicDoc [] (flowDown spacing wholePage 0))

"

order =
 "; An example of an ordering function
(def [boxwd boxht] [150 150])
(def box (\\(left top col)
  (eStyle [ [ 'left' left ] 
            [ 'top' top   ] 
            [ 'background-color' col ] 
            [ 'position' 'absolute'          ] ] 
          (eDiv boxwd boxht []) ) ) )
(defrec findAttrVal (\\(str list) (case list
  ([] hrHt)
  ([ [ key val ] | rest ] (if (= str key) val (findAttrVal str rest))) ) ) )
(defrec replaceAttr (\\([key val] list) (case list
  ([] [])
  ([[k1 v1] | rest] (if (= k1 key) [[key val] | rest] 
                                   [[k1 v1] | (replaceAttr [key val] rest)]) ) ) ) )
(def orderDivs (\\([name1 attrs1 children1] [name2 attrs2 children2])
  (let left1 (findAttrVal 'left' attrs1)
  (let left2 (findAttrVal 'left' attrs2)
  (if (< (- (+ left1 (findAttrVal 'width' attrs1)) left2) 10!)
      (if (< (findAttrVal 'left' attrs1) (findAttrVal 'left' attrs2) )
          [[name2 (replaceAttr ['left' (- left1 50!)] attrs2) children2]
           [name1 (replaceAttr ['left' (+ left2 50!)] attrs1) children1]]
          [[name1 (replaceAttr ['left' (- left2 50!)] attrs1) children1]
           [name2 (replaceAttr ['left' (+ left1 50!)] attrs2) children2]] )
      (if (< (- (+ left2 (findAttrVal 'width' attrs2)) left1) 10!)
          (if (< (findAttrVal 'left' attrs1) (findAttrVal 'left' attrs2) )
              [[name2 (replaceAttr ['left' (- left1 50!)] attrs2) children2]
               [name1 (replaceAttr ['left' (+ left2 50!)] attrs1) children1]]
              [[name1 (replaceAttr ['left' (- left2 50!)] attrs1) children1]
               [name2 (replaceAttr ['left' (+ left1 50!)] attrs2) children2]] )
          [[name1 attrs1 children1] [name2 attrs2 children2]] ) ) ) ) ) )
(def box1 (box 100 100! 'lightblue'))
(def box2 (box 400 100! 'tomato') )
(basicDoc [] (orderDivs box1 box2))  

"

orderWithSliders =
 "; An ordering function that has a slider to determine the ordering
(def [boxwd boxht sliderht slidercol] [150! 150! 10! 'rgba(84,84,84,0.5)'])
(def box (\\(left top col)
  (eStyle [ [ 'left' left ] 
            [ 'top' top   ] 
            [ 'background-color' col ] 
            [ 'position' 'absolute'          ] ] 
          (eDiv boxwd boxht []) ) ) )
(defrec findAttrVal (\\(str list) (case list
  ([] hrHt)
  ([ [ key val ] | rest ] (if (= str key) val (findAttrVal str rest))) ) ) )
(defrec replaceAttr (\\([key val] list) (case list
  ([] [])
  ([[k1 v1] | rest] (if (= k1 key) [[key val] | rest] 
                                   [[k1 v1] | (replaceAttr [key val] rest)]) ) ) ) )
(def abs (\\num (if (< num 0!)
                   (* -1! num)
                   num) ) )
(def pickSmaller (\\(num1 num2) (if (< num1 num2) num1 num2)))
(def orderDivs (\\([name1 attrs1 children1] [name2 attrs2 children2] curPos)
  (let left1 (findAttrVal 'left' attrs1)
  (let left2 (findAttrVal 'left' attrs2)
  (let top1 (findAttrVal 'top' attrs1)  
  (let top2 (findAttrVal 'top' attrs2)
  (let width1 (findAttrVal 'width' attrs1)
  (let width2 (findAttrVal 'width' attrs2)
  (let height1 (findAttrVal 'height' attrs1) 
  (let height2 (findAttrVal 'height' attrs2)
  (let sliderBar (eStyle [ [ 'top'  (+ top1  (/ height1 2!)) ]
                           [ 'left' (+ (pickSmaller left1 left2) (/ width1 2!)) ] 
                           [ 'position' 'absolute' ]
                           [ 'background-color' slidercol ] ]
                 (eDiv (abs (- left1 left2))
                       sliderht []) )
  (let sliderTick (eStyle [ [ 'top' (- (+ top1 (/ height1 2!)) sliderht) ]
                            [ 'left' (+ (+ (pickSmaller left1 left2) (/ width1 2!)) (/ (abs (- left1 left2)) 2!)) ]
                            [ 'position' 'absolute' ]
                            [ 'background-color' slidercol] ]
                          (eDiv sliderht (* 3! sliderht) []) )
  [[name1 attrs1 children1] [name2 attrs2 children2] sliderBar sliderTick] ) ) ) ) ) ) ) ) ) ) ) )
(def box1 (box 100 100! 'lightblue'))
(def box2 (box 400 100! 'tomato'))
(basicDoc [] (orderDivs box1 box2 0))

"


examples =
  [ makeExample scratchName scratch
  , makeExample "*Prelude*" Prelude.src
  , makeExample "Basic Text" basicText
  , makeExample "Three Divs" threeDivs
  , makeExample "Image" image
  , makeExample "Simple Navigation Bar" simpleNavBar
  , makeExample "Simple Table" table
  , makeExample "Complex Table" complextable
  , makeExample "Simple Hover" hover
  , makeExample "Basic Page" basicPage
  , makeExample "Ordering A" order
  , makeExample "Ordering B" orderWithSliders
  ]

list = examples -- ++ MicroTests.sampleTests
