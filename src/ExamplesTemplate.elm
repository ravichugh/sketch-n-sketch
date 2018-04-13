module ExamplesGenerated exposing
  ( list, templateCategories
  , blankSvgTemplate, blankHtmlTemplate, initTemplate
  , badPreludeTemplate
  , fromleo_markdown
  , fromleo_recipe
  , fromleo_conference_budgetting
  )

import Lang
import FastParser
import ElmParser
import Types
import Eval
import Utils
import PreludeGenerated as Prelude
import DefaultIconTheme
import Syntax
import EvalUpdate

--------------------------------------------------------------------------------

initTemplate = "Get Started"
blankSvgTemplate = "Blank Svg Document"
blankHtmlTemplate = "Blank Html Document"
badPreludeTemplate = "Bad Prelude"

--------------------------------------------------------------------------------

makeExample = makeExample_ FastParser.parseE Syntax.Little

makeLeoExample = makeExample_ ElmParser.parse Syntax.Elm

makeExample_ parser syntax name s =
  let thunk () =
    -- TODO tolerate parse errors, change Select Example
    let e = Utils.fromOkay ("Error parsing example " ++ name) (parser s) in
{-
    -- TODO why isn't this working?
    let e =
      case parser s of
        Ok exp -> exp
        Err msg ->
          -- TODO show msg in program
          let s = "Error parsing example " ++ name in
          -- case parser """main = [\"p\", [], [[\"TEXT\", \"blah\"]]]""" of
          case ElmParser.parseNoFreshen """main = [\"p\", [], [[\"TEXT\", \"blah\"]]]""" of
            Ok exp -> exp
            Err _ -> Debug.crash "ExamplesTemplate: shouldn't happen!"
    in
-}
    -- let ati = Types.typecheck e in
    let ati = Types.dummyAceTypeInfo in
    -----------------------------------------------------
    -- if name == "*Prelude*" then
    --   {e=e, v=LangSvg.dummySvgVal, ws=[], ati=ati}
    -- else
    -----------------------------------------------------
    let (v,ws) = Utils.fromOk ("Error executing example " ++ name) <| EvalUpdate.run syntax e in
    {e=e, v=v, ws=ws, ati=ati}
  in
  (name, (s, thunk))

LITTLE_TO_ELM threeBoxes
LITTLE_TO_ELM nBoxesH2
LITTLE_TO_ELM sineWaveOfBoxes
LITTLE_TO_ELM sineWaveGrid
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
-- TODO stack overflow for some reason?
-- LITTLE_TO_ELM logoSizes
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
LITTLE_TO_ELM offsets
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
-- TODO stack overflow for some reason?
--LITTLE_TO_ELM cultOfLambda
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
LITTLE_TO_ELM dictionaries
-- LITTLE_TO_ELM deleteBoxes
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
-- LITTLE_TO_ELM relateRects0
-- LITTLE_TO_ELM relateRects1
-- LITTLE_TO_ELM relateCircles0
-- LITTLE_TO_ELM relateLines0
-- LITTLE_TO_ELM relatePoints0
-- LITTLE_TO_ELM relatePoints1
-- LITTLE_TO_ELM relatePoints2
-- LITTLE_TO_ELM relatePoints3
-- LITTLE_TO_ELM relatePoints4
LITTLE_TO_ELM blank
LITTLE_TO_ELM horrorFilms0
LITTLE_TO_ELM cyclingAssociation0
LITTLE_TO_ELM snsLogoWheel
LITTLE_TO_ELM sns_UIST
LITTLE_TO_ELM sns_revisited_UIST
LITTLE_TO_ELM botanic_UIST
LITTLE_TO_ELM coffee_UIST
LITTLE_TO_ELM rectangleTrisection
LITTLE_TO_ELM battery
LITTLE_TO_ELM batteryDynamic
LITTLE_TO_ELM mondrianArch
LITTLE_TO_ELM ladder
LITTLE_TO_ELM rails
LITTLE_TO_ELM target
LITTLE_TO_ELM xs
LITTLE_TO_ELM conifer
LITTLE_TO_ELM ferris3
LITTLE_TO_ELM gear
LITTLE_TO_ELM kochSnowflake
LITTLE_TO_ELM replaceTerminalsWithWorkstations
LITTLE_TO_ELM balanceScale
LITTLE_TO_ELM pencilTip
LITTLE_TO_ELM calendarIcon
LITTLE_TO_ELM sns_deuce
LITTLE_TO_ELM target_deuce
LITTLE_TO_ELM battery_deuce
LITTLE_TO_ELM coffee_deuce
LITTLE_TO_ELM mondrian_arch_deuce

--------------------------------------------------------------------------------
-- Deuce User Study Files

-- these .little files are generated from somewhere else;
-- don't change them manually

LITTLE_TO_ELM study_start
LITTLE_TO_ELM study_transition_1
LITTLE_TO_ELM study_transition_2
LITTLE_TO_ELM study_end

LITTLE_TO_ELM tutorial_step_01
LITTLE_TO_ELM tutorial_step_02
LITTLE_TO_ELM tutorial_step_03
LITTLE_TO_ELM tutorial_step_04
LITTLE_TO_ELM tutorial_step_05
LITTLE_TO_ELM tutorial_step_06
LITTLE_TO_ELM tutorial_step_07
LITTLE_TO_ELM tutorial_step_08
LITTLE_TO_ELM tutorial_step_09
LITTLE_TO_ELM tutorial_step_10
LITTLE_TO_ELM tutorial_step_11
LITTLE_TO_ELM tutorial_step_12
LITTLE_TO_ELM tutorial_step_13
LITTLE_TO_ELM tutorial_step_14
LITTLE_TO_ELM tutorial_step_15
LITTLE_TO_ELM tutorial_step_16
LITTLE_TO_ELM tutorial_step_17
LITTLE_TO_ELM tutorial_step_18
LITTLE_TO_ELM tutorial_step_19
LITTLE_TO_ELM tutorial_step_20
LITTLE_TO_ELM tutorial_step_21
LITTLE_TO_ELM tutorial_step_22
LITTLE_TO_ELM tutorial_step_23

LITTLE_TO_ELM task_one_rectangle
LITTLE_TO_ELM task_two_circles
LITTLE_TO_ELM task_three_rectangles
LITTLE_TO_ELM task_target
LITTLE_TO_ELM task_four_squares
LITTLE_TO_ELM task_lambda

--------------------------------------------------------------------------------

LEO_TO_ELM badPrelude
LEO_TO_ELM blankSvg
LEO_TO_ELM blankDoc
LEO_TO_ELM welcome1
LEO_TO_ELM tableOfStatesA
LEO_TO_ELM tableOfStatesB
LEO_TO_ELM tableOfStatesC
LEO_TO_ELM simpleBudget
LEO_TO_ELM mapMaybeLens
LEO_TO_ELM mapListLens_1
LEO_TO_ELM mapListLens_2
LEO_TO_ELM listAppendLens
LEO_TO_ELM fromleo/markdown
LEO_TO_ELM fromleo/conference_budgetting
LEO_TO_ELM fromleo/recipe
LEO_TO_ELM fromleo/modelviewcontroller
LEO_TO_ELM fromleo/programmabledoc
LEO_TO_ELM fromleo/latexeditor

--------------------------------------------------------------------------------

welcomeCategory =
  ( "Welcome"
  , [ makeLeoExample blankSvgTemplate blankSvg
    , makeLeoExample blankHtmlTemplate blankDoc
    , makeLeoExample initTemplate welcome1
--    , makeLeoExample "Tutorial" blankDoc
    ]
  )

docsCategory =
  ( "Examples (OOPSLA 2018 Submission)"
  , [ makeLeoExample "1a: Table of States" tableOfStatesA
    , makeLeoExample "1b: Table of States" tableOfStatesC
    , makeLeoExample "1c: Table of States" tableOfStatesB
    ] ++
    (
    List.indexedMap
      (\i (caption, program) ->
        makeLeoExample (toString (2+i) ++ ": " ++ caption) program
      )
      [ ("Lens: Maybe Map", mapMaybeLens)
      , ("Lens: List Map 1", mapListLens_1)
      , ("Lens: List Map 2", mapListLens_2)
      , ("Lens: List Append", listAppendLens)
      , ("Markdown", fromleo_markdown)
      , ("Conference Budget", fromleo_conference_budgetting)
      , ("Proportional Recipe editor", fromleo_recipe)
      , ("Programmable document", fromleo_programmabledoc)
      , ("Model View Controller", fromleo_modelviewcontroller)
      , ("LaTeX editor", fromleo_latexeditor)
--      , ("TODO", blankDoc)
--      , ("Simple Budget", simpleBudget)
      ]
    )
  )

defaultIconCategory =
  ( "Default Icons"
  , [ makeExample "Icon: Cursor" DefaultIconTheme.cursor
    , makeExample "Icon: Point Or Offset" DefaultIconTheme.pointOrOffset
    , makeExample "Icon: Text" DefaultIconTheme.text
    , makeExample "Icon: Line" DefaultIconTheme.line
    , makeExample "Icon: Rect" DefaultIconTheme.rect
    , makeExample "Icon: Ellipse" DefaultIconTheme.ellipse
    , makeExample "Icon: Polygon" DefaultIconTheme.polygon
    , makeExample "Icon: Path" DefaultIconTheme.path
    ]
  )

deuceCategory =
  ( "Deuce Examples"
  , [ makeExample "Sketch-n-Sketch Logo" sns_deuce
    , makeExample "Target" target_deuce
    , makeExample "Battery" battery_deuce
    , makeExample "Coffee Mug" coffee_deuce
    , makeExample "Mondrian Arch" mondrian_arch_deuce
    ]
  )

logoCategory =
  ( "Logos"
  , List.sortBy Tuple.first
      [ makeExample "SnS Logo (UIST)" sns_UIST
      , makeExample "SnS Logo Revisited (UIST)" sns_revisited_UIST
      , makeExample "Botanic Garden Logo (UIST)" botanic_UIST
      , makeExample "Logo" logo
      , makeExample "Botanic Garden Logo" botanic
      , makeExample "Active Trans Logo" activeTrans2
      , makeExample "SnS Logo Wheel" snsLogoWheel
      , makeExample "Haskell.org Logo" haskell
      , makeExample "Cover Logo" cover
      , makeExample "POP-PL Logo" poppl
      , makeExample "Floral Logo 1" floralLogo
      , makeExample "Floral Logo 2" floralLogo2
      , makeExample "Elm Logo" elmLogo
      , makeExample "Logo 2" logo2
      -- , makeExample "Logo Sizes" logoSizes
      ]
  )

flagCategory =
  ( "Flags"
  , List.sortBy Tuple.first
      [ makeExample "Chicago Flag" chicago
      , makeExample "US-13 Flag" usFlag13
      , makeExample "US-50 Flag" usFlag50
      , makeExample "French Sudan Flag" frenchSudan
      ]
  )


otherCategory =
  ( "Other"
  , [ makeExample "Coffee Mugs (UIST)" coffee_UIST

    , makeExample "Wave Boxes" sineWaveOfBoxes
    , makeExample "Wave Boxes Grid" sineWaveGrid

    , makeExample "Basic Slides" basicSlides
    , makeExample "Sailboat" sailBoat
    , makeExample "Sliders" sliders
    , makeExample "Buttons" buttons
    , makeExample "Widgets" widgets
    , makeExample "xySlider" xySlider
    , makeExample "Offsets" offsets
    , makeExample "Tile Pattern" boxGridTokenFilter
    , makeExample "Color Picker" rgba
    , makeExample "Ferris Wheel" ferris
    , makeExample "Ferris Task Before" ferris2
    , makeExample "Ferris Task After" ferris2target
    , makeExample "Ferris Wheel Slideshow" ferrisWheelSlideshow
    , makeExample "Survey Results" surveyResultsTriHist2
    , makeExample "Hilbert Curve Animation" hilbertCurveAnimation
    , makeExample "Bar Graph" barGraph
    , makeExample "Pie Chart" pieChart1
    , makeExample "Solar System" solarSystem
    , makeExample "Clique" clique
    , makeExample "Eye Icon" eyeIcon
    , makeExample "Horror Films" horrorFilms0
    , makeExample "Cycling Association" cyclingAssociation0
    , makeExample "Lillicon P" lilliconP
    , makeExample "Lillicon P, v2" lilliconP2
    , makeExample "Keyboard" keyboard
    , makeExample "Keyboard Task Before" keyboard2
    , makeExample "Keyboard Task After" keyboard2target
    , makeExample "Tessellation Task Before" tessellation
    , makeExample "Tessellation Task After" tessellationTarget
    , makeExample "Tessellation 2" tessellation2
    , makeExample "Spiral Spiral-Graph" spiralSpiralGraph
    , makeExample "Rounded Rect" roundedRect

    , makeExample "Thaw/Freeze" thawFreeze
    , makeExample "Dictionaries" dictionaries
    , makeExample "3 Boxes" threeBoxes

    , makeExample "N Boxes Sli" nBoxes
    , makeExample "N Boxes" groupOfBoxes

    , makeExample "Rings" rings
    , makeExample "Polygons" polygons
    , makeExample "Stars" stars
    , makeExample "Triangles" equiTri
    , makeExample "Frank Lloyd Wright" flw1
    , makeExample "Frank Lloyd Wright B" flw2
    , makeExample "Bezier Curves" bezier
    , makeExample "Fractal Tree" fractalTree
    , makeExample "Stick Figures" stickFigures
    -- , makeExample "Cult of Lambda" cultOfLambda
    , makeExample "Matrix Transformations" matrices
    , makeExample "Misc Shapes" miscShapes
    , makeExample "Interface Buttons" interfaceButtons
    , makeExample "Paths 1" paths1
    , makeExample "Paths 2" paths2
    , makeExample "Paths 3" paths3
    , makeExample "Paths 4" paths4
    , makeExample "Paths 5" paths5
    , makeExample "Sample Rotations" rotTest
    , makeExample "Grid Tile" gridTile
    , makeExample "Zones" zones
    , makeExample "Rectangle Trisection" rectangleTrisection
    , makeExample "Battery" battery
    , makeExample "Battery (Dynamic)" batteryDynamic
    , makeExample "Mondrian Arch" mondrianArch
    , makeExample "Ladder" ladder
    , makeExample "Rails" rails
    , makeExample "Target" target
    , makeExample "Xs" xs
    , makeExample "Conifer" conifer
    , makeExample "Ferris Wheel 3" ferris3
    , makeExample "Gear" gear
    , makeExample "Koch Snowflake" kochSnowflake
    , makeExample "Replace Terminals With Workstations" replaceTerminalsWithWorkstations
    , makeExample "Balance Scale" balanceScale
    , makeExample "Pencil Tip" pencilTip
    , makeExample "Calendar Icon" calendarIcon
    ]
  )

deuceUserStudyCategory =
  ( "Deuce User Study"
  , [ makeExample "Deuce Study Start" study_start
    , makeExample "Step 01" tutorial_step_01
    , makeExample "Step 02" tutorial_step_02
    , makeExample "Step 03" tutorial_step_03
    , makeExample "Step 04" tutorial_step_04
    , makeExample "Step 05" tutorial_step_05
    , makeExample "Step 06" tutorial_step_06
    , makeExample "Step 07" tutorial_step_07
    , makeExample "Step 08" tutorial_step_08
    , makeExample "Step 09" tutorial_step_09
    , makeExample "Step 10" tutorial_step_10
    , makeExample "Step 11" tutorial_step_11
    , makeExample "Step 12" tutorial_step_12
    , makeExample "Step 13" tutorial_step_13
    , makeExample "Step 14" tutorial_step_14
    , makeExample "Step 15" tutorial_step_15
    , makeExample "Step 16" tutorial_step_16
    , makeExample "Step 17" tutorial_step_17
    , makeExample "Step 18" tutorial_step_18
    , makeExample "Step 19" tutorial_step_19
    , makeExample "Step 20" tutorial_step_20
    , makeExample "Step 21" tutorial_step_21
    , makeExample "Step 22" tutorial_step_22
    , makeExample "Step 23" tutorial_step_23
    , makeExample "Deuce Study Transition 1" study_transition_1
    , makeExample "One Rectangle" task_one_rectangle
    , makeExample "Two Circles" task_two_circles
    , makeExample "Three Rectangles" task_three_rectangles
    , makeExample "Target Icon" task_target
    , makeExample "Deuce Study Transition 2" study_transition_2
    , makeExample "Four Squares" task_four_squares
    , makeExample "Lambda Icon" task_lambda
    , makeExample "Deuce Study End" study_end
    ]
  )

internalCategory =
 ( "(Internal Things...)"
 , [ makeLeoExample "Standard Prelude" Prelude.preludeLeo
   , makeLeoExample badPreludeTemplate badPrelude
   ]
 )

templateCategories =
  [ welcomeCategory
  , docsCategory
  , deuceCategory
  , defaultIconCategory
  , logoCategory
  , flagCategory
  , otherCategory
  , deuceUserStudyCategory
  , internalCategory
  ]

list =
  templateCategories
    |> List.map Tuple.second
    |> List.concat
