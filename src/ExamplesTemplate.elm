module ExamplesGenerated (list, scratchName, scratch) where

import Lang
import LangParser
import Eval
import MicroTests
import Utils

makeExample name s =
  let thunk () =
    let e = Utils.fromOk_ (LangParser.parseE s) in
    let v = Eval.run e in
    {e=e, v=v}
  in
  (name, thunk)

scratchName = "*Scratch*"

LITTLE_TO_ELM scratch
LITTLE_TO_ELM threeBoxes
LITTLE_TO_ELM sixBoxesA
LITTLE_TO_ELM sixBoxesB
LITTLE_TO_ELM logo
LITTLE_TO_ELM elmLogo
LITTLE_TO_ELM activeTrans
LITTLE_TO_ELM botanic
LITTLE_TO_ELM rings
LITTLE_TO_ELM polygons
LITTLE_TO_ELM stars
LITTLE_TO_ELM sliders
LITTLE_TO_ELM buttons
LITTLE_TO_ELM widgets
LITTLE_TO_ELM xySlider
LITTLE_TO_ELM rgba
-- LITTLE_TO_ELM boxGrid
LITTLE_TO_ELM usFlag13
LITTLE_TO_ELM usFlag50
LITTLE_TO_ELM chicago
LITTLE_TO_ELM frenchSudan
LITTLE_TO_ELM flw1
LITTLE_TO_ELM flw2
LITTLE_TO_ELM ferris
LITTLE_TO_ELM pieChart1
LITTLE_TO_ELM solarSystem
LITTLE_TO_ELM fractalTree
LITTLE_TO_ELM stickFigures
LITTLE_TO_ELM cultOfLambda
LITTLE_TO_ELM clique
LITTLE_TO_ELM miscShapes
LITTLE_TO_ELM paths1
LITTLE_TO_ELM paths2
LITTLE_TO_ELM paths3
LITTLE_TO_ELM paths4
LITTLE_TO_ELM paths5

examples =
  [ makeExample scratchName scratch
  , makeExample "3 Boxes" threeBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Logo" logo
  , makeExample "Elm Logo" elmLogo
  , makeExample "Active Trans Logo" activeTrans
  , makeExample "Botanic Garden Logo" botanic
  , makeExample "Rings" rings
  , makeExample "Polygons" polygons
  , makeExample "Stars" stars
  , makeExample "Clique" clique
  , makeExample "Sliders" sliders
  , makeExample "Buttons" buttons
  , makeExample "Widgets" widgets
  , makeExample "xySlider" xySlider
  , makeExample "Color Picker" rgba
  -- , makeExample "Box Grid" boxGrid
  , makeExample "Chicago Flag" chicago
  , makeExample "US-13 Flag" usFlag13
  , makeExample "US-50 Flag" usFlag50
  , makeExample "French Sudan Flag" frenchSudan
  , makeExample "Frank Lloyd Wright" flw1
  , makeExample "Frank Lloyd Wright B" flw2
  , makeExample "Ferris Wheel" ferris
  , makeExample "Pie Chart" pieChart1
  , makeExample "Solar System" solarSystem
  , makeExample "Fractal Tree" fractalTree
  , makeExample "Stick Figures" stickFigures
  , makeExample "Cult of Lambda" cultOfLambda 
  , makeExample "Misc Shapes" miscShapes
  , makeExample "Paths 1" paths1
  , makeExample "Paths 2" paths2
  , makeExample "Paths 3" paths3
  , makeExample "Paths 4" paths4
  , makeExample "Paths 5" paths5
  ]

list = examples -- ++ MicroTests.sampleTests

