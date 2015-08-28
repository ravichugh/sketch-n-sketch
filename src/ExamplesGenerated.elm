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


examples =
  [ makeExample scratchName scratch
  , makeExample "Basic Text" basicText
  , makeExample "*Prelude*" Prelude.src
  ]

list = examples -- ++ MicroTests.sampleTests
