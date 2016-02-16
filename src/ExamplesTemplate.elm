module ExamplesGenerated (list, scratchName, scratch, examples) where

import Lang
import LangParser2 as Parser
import Eval
import Utils
import PreludeGenerated as Prelude

makeExample name s =
  let thunk () =
    let e = Utils.fromOk_ (Parser.parseE s) in
    let (v,ws) = Eval.run e in
    {e=e, v=v, ws=ws}
  in
  (name, s, thunk)

scratchName = "*Scratch*"

LITTLE_TO_ELM scratch
LITTLE_TO_ELM threeBoxes
LITTLE_TO_ELM nBoxesH2
LITTLE_TO_ELM sineWaveOfBoxes
-- LITTLE_TO_ELM waveOfBoxes
-- LITTLE_TO_ELM waveOfBoxesTokens
-- LITTLE_TO_ELM waveOfBoxes3
LITTLE_TO_ELM nBoxes
LITTLE_TO_ELM groupOfBoxes
-- LITTLE_TO_ELM sixBoxesA
-- LITTLE_TO_ELM sixBoxesB
LITTLE_TO_ELM basicSlides
LITTLE_TO_ELM logo
LITTLE_TO_ELM logo2
LITTLE_TO_ELM logoSizes
LITTLE_TO_ELM elmLogo
-- LITTLE_TO_ELM activeTrans
LITTLE_TO_ELM activeTrans2
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
LITTLE_TO_ELM boxGridTokenFilter
LITTLE_TO_ELM usFlag13
LITTLE_TO_ELM usFlag50
LITTLE_TO_ELM chicago
-- LITTLE_TO_ELM chicagoColors
LITTLE_TO_ELM frenchSudan
LITTLE_TO_ELM flw1
LITTLE_TO_ELM flw2
LITTLE_TO_ELM ferris
LITTLE_TO_ELM ferris2
LITTLE_TO_ELM ferris2target
LITTLE_TO_ELM ferrisWheelSlideshow
LITTLE_TO_ELM pieChart1
LITTLE_TO_ELM solarSystem
LITTLE_TO_ELM fractalTree
LITTLE_TO_ELM hilbertCurveAnimation
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
LITTLE_TO_ELM deleteBoxes
LITTLE_TO_ELM cover
LITTLE_TO_ELM poppl
LITTLE_TO_ELM bezier
-- LITTLE_TO_ELM surveyResultsTriBubbles
-- LITTLE_TO_ELM surveyResultsTriHist
LITTLE_TO_ELM surveyResultsTriHist2
LITTLE_TO_ELM equiTri
LITTLE_TO_ELM gridTile
LITTLE_TO_ELM lilliconP
LITTLE_TO_ELM lilliconP2
LITTLE_TO_ELM keyboard
LITTLE_TO_ELM keyboard2
LITTLE_TO_ELM keyboard2target
LITTLE_TO_ELM tessellation
LITTLE_TO_ELM tessellationTarget
LITTLE_TO_ELM tessellation2
LITTLE_TO_ELM floralLogo
LITTLE_TO_ELM floralLogo2
LITTLE_TO_ELM zones
LITTLE_TO_ELM roundedRect
LITTLE_TO_ELM spiralSpiralGraph
LITTLE_TO_ELM relateRects0
LITTLE_TO_ELM relateRects1
LITTLE_TO_ELM relateCircles0
LITTLE_TO_ELM relateLines0
-- LITTLE_TO_ELM relatePoints0
-- LITTLE_TO_ELM relatePoints1
-- LITTLE_TO_ELM relatePoints2
-- LITTLE_TO_ELM relatePoints3
-- LITTLE_TO_ELM relatePoints4
LITTLE_TO_ELM blank

examples =
  [ ("BLANK", blank)
  , (scratchName, scratch)
  , ("*Prelude*", Prelude.src)
  , ("Wave Boxes", sineWaveOfBoxes)

  -- up here during ad-hoc development)
  , ("RelateRects0", relateRects0)
  , ("RelateRects1", relateRects1)
  , ("RelateCircles0", relateCircles0)
  , ("RelateLines0", relateLines0)
  -- , ("RelatePoints0", relatePoints0)
  -- , ("RelatePoints1", relatePoints1)
  -- , ("RelatePoints2", relatePoints2)
  -- , ("RelatePoints3", relatePoints3)
  -- , ("RelatePoints4", relatePoints4)
  , ("Delete Boxes", deleteBoxes)

  , ("Basic Slides", basicSlides)
  , ("Logo", logo)
  , ("Botanic Garden Logo", botanic)
  , ("Active Trans Logo", activeTrans2)
  , ("Sailboat", sailBoat)
  , ("Chicago Flag", chicago)
  , ("Sliders", sliders)
  , ("Buttons", buttons)
  , ("Widgets", widgets)
  , ("xySlider", xySlider)
  , ("Tile Pattern", boxGridTokenFilter)
  , ("Color Picker", rgba)
  , ("Ferris Wheel", ferris)
  , ("Ferris Task Before", ferris2)
  , ("Ferris Task After", ferris2target)
  , ("Ferris Wheel Slideshow", ferrisWheelSlideshow)
  , ("Survey Results", surveyResultsTriHist2)
  , ("Hilbert Curve Animation", hilbertCurveAnimation)
  , ("Bar Graph", barGraph)
  , ("Pie Chart", pieChart1)
  , ("Solar System", solarSystem)
  , ("Clique", clique)
  , ("Eye Icon", eyeIcon)
  , ("Wikimedia Logo", wikimedia)
  , ("Haskell.org Logo", haskell)
  , ("Cover Logo", cover)
  , ("POP-PL Logo", poppl)
  , ("Lillicon P", lilliconP)
  , ("Lillicon P, v2", lilliconP2)
  , ("Keyboard", keyboard)
  , ("Keyboard Task Before", keyboard2)
  , ("Keyboard Task After", keyboard2target)
  , ("Tessellation Task Before", tessellation)
  , ("Tessellation Task After", tessellationTarget)
  , ("Tessellation 2", tessellation2)
  , ("Floral Logo 1", floralLogo)
  , ("Floral Logo 2", floralLogo2)
  , ("Spiral Spiral-Graph", spiralSpiralGraph)
  , ("Rounded Rect", roundedRect)

  , ("Thaw/Freeze", thawFreeze)
  , ("3 Boxes", threeBoxes)
  -- , ("N Boxes H2", nBoxesH2)
  , ("N Boxes Sli", nBoxes)
  , ("N Boxes", groupOfBoxes)
  -- , ("6 Boxes A", sixBoxesA)
  -- , ("6 Boxes B", sixBoxesB)
  -- , ("Wave Tokens", waveOfBoxesTokens)
  -- , ("Wave 3", waveOfBoxes3)
  -- , ("Chicago Flag 2", chicagoColors)
  , ("Elm Logo", elmLogo)
  , ("Logo 2", logo2)
  , ("Logo Sizes", logoSizes)
  , ("Rings", rings)
  , ("Polygons", polygons)
  , ("Stars", stars)
  , ("Triangles", equiTri)
  , ("US-13 Flag", usFlag13)
  , ("US-50 Flag", usFlag50)
  , ("French Sudan Flag", frenchSudan)
  , ("Frank Lloyd Wright", flw1)
  , ("Frank Lloyd Wright B", flw2)
  , ("Bezier Curves", bezier)
  , ("Fractal Tree", fractalTree)
  , ("Stick Figures", stickFigures)
  , ("Cult of Lambda", cultOfLambda)
  , ("Matrix Transformations", matrices)
  , ("Misc Shapes", miscShapes)
  , ("Interface Buttons", interfaceButtons)
  , ("Paths 1", paths1)
  , ("Paths 2", paths2)
  , ("Paths 3", paths3)
  , ("Paths 4", paths4)
  , ("Paths 5", paths5)
  , ("Sample Rotations", rotTest)
  , ("Grid Tile", gridTile)
  , ("Zones", zones)
  ]

list = examples
