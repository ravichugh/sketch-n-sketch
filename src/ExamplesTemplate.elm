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

LITTLE_TO_ELM scratch
LITTLE_TO_ELM basicText
LITTLE_TO_ELM threeDivs
LITTLE_TO_ELM image
LITTLE_TO_ELM simpleNavBar
LITTLE_TO_ELM table
LITTLE_TO_ELM hover
LITTLE_TO_ELM basicPage

examples =
  [ makeExample scratchName scratch
  , makeExample "*Prelude*" Prelude.src
  , makeExample "Basic Text" basicText
  , makeExample "Three Divs" threeDivs
  , makeExample "Image" image
  , makeExample "Simple Navigation Bar" simpleNavBar
  , makeExample "Simple Table" table
  , makeExample "Simple Hover" hover
  , makeExample "Basic Page" basicPage
  ]

list = examples -- ++ MicroTests.sampleTests
