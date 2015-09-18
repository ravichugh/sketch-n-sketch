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


examples =
  [ makeExample scratchName scratch
  , makeExample "*Prelude*" Prelude.src
  , makeExample "Basic Text" basicText
  , makeExample "Three Divs" threeDivs
  , makeExample "Image" image
  , makeExample "Simple Navigation Bar" simpleNavBar
  , makeExample "Simple Table" table
  , makeExample "Simple Hover" hover
  ]

list = examples -- ++ MicroTests.sampleTests
