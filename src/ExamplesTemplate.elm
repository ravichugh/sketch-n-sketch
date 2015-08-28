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

examples =
  [ makeExample scratchName scratch
  , makeExample "Basic Text" basicText
  , makeExample "*Prelude*" Prelude.src
  ]

list = examples -- ++ MicroTests.sampleTests
