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
  (div 
    [ (style 
        [ [ 'width' '100px' ]
          [ 'height' '100px' ] 
          [ 'background-color' 'red' ]
        ] )
    ] 
    [] ) )
(basicDoc [['bgcolor' 'lightblue']] [reddiv])

"

basicText =
 "; Demonstration of simple text capabilities

(def simpleText 'Dorian is swell')
(def italics (\\str ['i' [] [(text str)]]))
(def bold    (\\str ['b' [] [(text str)]]))
(basicDoc [] 
  [ (text simpleText)
    (italics simpleText)
    (bold simpleText)
  ] )

"

threeBoxesA =
 "
; An HTML version of our classic three threeBoxes
; example
; TODO: 
; fix spacing numbers / w&h to be mainpulatable
; add relative positioning

(def bluebox (\\s 
  (div
    [ (style 
        [ [ 'position' 'absolute']
          [ 'top' '40px']
          [ 'left' s ]
          [ 'width' '70px' ]
          [ 'height' '180px' ] 
          [ 'background-color' 'lightblue' ]
        ] )
    ] 
    [] )))
(basicDoc [] (map bluebox ['100px' '300px' '500px']))
"

sixBoxesA =
 "
; An HTML version of our classic three threeBoxes
; example
; TODO: 
; fix spacing numbers / w&h to be mainpulatable
; add relative positioning

(def greybox (\\[x y] 
  (div
    [ (style 
        [ [ 'position' 'absolute']
          [ 'top' y]
          [ 'left' x ]
          [ 'width' '20px' ]
          [ 'height' '20px' ] 
          [ 'background-color' 'grey' ]
        ] )
    ] 
    [] )))
(basicDoc [] (map greybox (cartProd ['40px' '80px' '120px'] ['20px' '60px'])))
"

elementTest =
  "
; First pass using our new, handy dandy element abstraction
; Note: all attributes added using eStyle should be
; well-formed CSS attributes

(def sampleDiv 
  (div
    [ [ 'width' '100px'  ]
      [ 'height' '100px' ]
    ]
    []
  ))
(def purpStyle (\\node
  (eStyle node 
    [ [ 'position' 'absolute']
      [ 'top' 20]
      [ 'left' 20 ]
      [ 'background-color' 'purple' ]
    ]
  )
))
(basicDoc [] (purpStyle sampleDiv))
"


examples =
  [ makeExample scratchName scratch
  , makeExample "Basic Text" basicText
  , makeExample "*Prelude*" Prelude.src
  , makeExample "Three Boxes Absolute" threeBoxesA
  , makeExample "Six Boxes Absolute" sixBoxesA
  , makeExample "Styling with eStyle" elementTest
  ]

list = examples -- ++ MicroTests.sampleTests
