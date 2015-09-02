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
LITTLE_TO_ELM threeBoxes
LITTLE_TO_ELM groupOfBoxes
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
LITTLE_TO_ELM boxGrid
LITTLE_TO_ELM usFlag13
LITTLE_TO_ELM usFlag50
LITTLE_TO_ELM chicago
LITTLE_TO_ELM chicagoColors
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
LITTLE_TO_ELM sailBoat
LITTLE_TO_ELM eyeIcon
LITTLE_TO_ELM wikimedia
LITTLE_TO_ELM haskell
LITTLE_TO_ELM matrices
LITTLE_TO_ELM rotTest
LITTLE_TO_ELM interfaceButtons
LITTLE_TO_ELM barGraph
LITTLE_TO_ELM thawFreeze
LITTLE_TO_ELM cover
LITTLE_TO_ELM poppl
LITTLE_TO_ELM bezier

examples =
  [ makeExample scratchName scratch
  , makeExample "*Prelude*" Prelude.src
  , makeExample "3 Boxes" threeBoxes
  , makeExample "N Boxes" groupOfBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Thaw/Freeze" thawFreeze
  , makeExample "Logo" logo
  , makeExample "Elm Logo" elmLogo
  , makeExample "Active Trans Logo" activeTrans
  , makeExample "Botanic Garden Logo" botanic
  , makeExample "Rings" rings
  , makeExample "Polygons" polygons
  , makeExample "Bezier Curves" bezier
  , makeExample "Stars" stars
  , makeExample "Clique" clique
  , makeExample "Sliders" sliders
  , makeExample "Buttons" buttons
  , makeExample "Widgets" widgets
  , makeExample "xySlider" xySlider
  , makeExample "Color Picker" rgba
  , makeExample "Box Grid" boxGrid
  , makeExample "Bar Graph" barGraph
  , makeExample "Chicago Flag" chicago
  , makeExample "Chicago Flag 2" chicagoColors
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
  , makeExample "Sailboat" sailBoat
  , makeExample "Eye Icon" eyeIcon
  , makeExample "Wikimedia Logo" wikimedia
  , makeExample "Haskell.org Logo" haskell
  , makeExample "Cover Logo" cover
  , makeExample "POP-PL Logo" poppl
  , makeExample "Matrix Transformations" matrices
  , makeExample "Cult of Lambda" cultOfLambda 
  , makeExample "Misc Shapes" miscShapes
  , makeExample "Interface Buttons" interfaceButtons
  , makeExample "Paths 1" paths1
  , makeExample "Paths 2" paths2
  , makeExample "Paths 3" paths3
  , makeExample "Paths 4" paths4
  , makeExample "Paths 5" paths5
  , makeExample "Sample Rotations" rotTest
  ]

list = examples -- ++ MicroTests.sampleTests
