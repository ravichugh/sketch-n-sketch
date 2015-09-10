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
      [ 'background-color' 'red' ]
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


examples =
  [ makeExample scratchName scratch
  , makeExample "*Prelude*" Prelude.src
  , makeExample "Basic Text" basicText
  , makeExample "Three Divs" threeDivs
  ]

list = examples -- ++ MicroTests.sampleTests
