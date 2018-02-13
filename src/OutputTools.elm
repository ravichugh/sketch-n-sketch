module OutputTools exposing
  ( OutputToolKind(..)
  , OutputTool
  , tools
  )

import Set exposing (Set)
import Dict exposing (Dict)

import InterfaceModel exposing (..)
import InterfaceController as Controller
import LangSvg exposing (NodeId)
import ShapeWidgets exposing (SelectableFeature)

--==============================================================================
--= Data Types
--==============================================================================

type alias Selections a =
  { a | selectedFeatures : Set SelectableFeature
      , selectedShapes : Set NodeId
      , selectedBlobs : Dict Int NodeId
  }

type OutputToolKind
  = Single
  | Multi

type alias OutputTransformation =
  Msg

type alias OutputTool =
  { name : String
  , shortcut : Maybe String
  , kind : OutputToolKind
  , func : Maybe OutputTransformation
  , reqs : List Predicate
  , id : String
  }

--==============================================================================
--= Requirement Helpers
--==============================================================================

nOrMore : Int -> Set a -> PredicateValue
nOrMore n xs =
  if Set.size xs >= n then
    Satisfied
  else
    Possible

atLeastOneFeature : Set SelectableFeature -> Predicate
atLeastOneFeature selectedFeatures =
  { description =
      "Select at least on feature"
  , value =
      nOrMore 1 selectedFeatures
  }

atLeastTwoFeatures : Set SelectableFeature -> Predicate
atLeastTwoFeatures selectedFeatures =
  { description =
      "Select at least two features"
  , value =
      nOrMore 2 selectedFeatures
  }

groupPredicate : Bool -> Selections a -> Predicate
groupPredicate
  disallowSelectedFeatures
  { selectedFeatures, selectedShapes, selectedBlobs } =
    let
      atLeastOneFeature =
        not <| Set.isEmpty selectedFeatures
      atLeastOneShape =
        not <| Set.isEmpty selectedShapes
      atLeastOneBlob =
        not <| Dict.isEmpty selectedBlobs
    in
      if disallowSelectedFeatures then
        { description =
            "Select at least one shape or blob (and no features)"
        , value =
            if atLeastOneFeature then
              Impossible
            else if atLeastOneShape || atLeastOneBlob then
              Satisfied
            else
              Possible
        }

      else
        { description =
            "Select at least one feature, shape, or blob"
        , value =
            if atLeastOneFeature || atLeastOneShape || atLeastOneBlob then
              Satisfied
            else
              Possible
        }

--==============================================================================
--= Tools
--==============================================================================

--------------------------------------------------------------------------------
-- Dig Hole
--------------------------------------------------------------------------------

digHoleTool : Selections a -> OutputTool
digHoleTool { selectedFeatures } =
  { name =
      "Dig Hole"
  , shortcut =
      Nothing
  , kind =
      Single
  , func =
      Just Controller.msgDigHole
  , reqs =
      [ atLeastOneFeature selectedFeatures
      ]
  , id =
      "digHole"
  }

--------------------------------------------------------------------------------
-- Make Equal
--------------------------------------------------------------------------------

makeEqualTool : Selections a -> OutputTool
makeEqualTool { selectedFeatures } =
  { name =
      "Make Equal"
  , shortcut =
      Just "E"
  , kind =
      Multi
  , func =
      Just Controller.msgMakeEqual
  , reqs =
      [ atLeastTwoFeatures selectedFeatures
      ]
  , id =
      "makeEqual"
  }

--------------------------------------------------------------------------------
-- Relate
--------------------------------------------------------------------------------

relateTool : Selections a -> OutputTool
relateTool { selectedFeatures } =
  { name =
      "Relate"
  , shortcut =
      Nothing
  , kind =
      Multi
  , func =
      Just Controller.msgRelate
  , reqs =
      [ atLeastTwoFeatures selectedFeatures
      ]
  , id =
      "relate"
  }

--------------------------------------------------------------------------------
-- Indexed Relate
--------------------------------------------------------------------------------

indexedRelateTool : Selections a -> OutputTool
indexedRelateTool { selectedFeatures } =
  { name =
      "Indexed Relate"
  , shortcut =
      Nothing
  , kind =
      Multi
  , func =
      Just Controller.msgIndexedRelate
  , reqs =
      [ atLeastTwoFeatures selectedFeatures
      ]
  , id =
      "indexedRelate"
  }

--------------------------------------------------------------------------------
-- Build Abstraction
--------------------------------------------------------------------------------

buildAbstractionTool : Selections a -> OutputTool
buildAbstractionTool { selectedFeatures } =
  { name =
      "Build Abstraction"
  , shortcut =
      Nothing
  , kind =
      Multi
  , func =
      Just Controller.msgBuildAbstraction
  , reqs =
      [ atLeastOneFeature selectedFeatures
      ]
  , id =
      "buildAbstraction"
  }

--------------------------------------------------------------------------------
-- Dupe
--------------------------------------------------------------------------------

dupeTool : Selections a -> OutputTool
dupeTool selections =
  { name =
      "Dupe"
  , shortcut =
      Just "D"
  , kind =
      Single
  , func =
      Just Controller.msgDuplicate
  , reqs =
      [ groupPredicate False selections
      ]
  , id =
      "dupe"
  }

--------------------------------------------------------------------------------
-- Merge
--------------------------------------------------------------------------------

mergeTool : Selections a -> OutputTool
mergeTool selections =
  { name =
      "Merge"
  , shortcut =
      Nothing
  , kind =
      Single
  , func =
      Just Controller.msgMergeBlobs
  , reqs =
      [ groupPredicate True selections
      ]
  , id =
      "merge"
  }

--------------------------------------------------------------------------------
-- Group
--------------------------------------------------------------------------------

groupTool : Selections a -> OutputTool
groupTool selections =
  { name =
      "Group"
  , shortcut =
      Just "G"
  , kind =
      Single
  , func =
      Just Controller.msgGroupBlobs
  , reqs =
      [ groupPredicate False selections
      ]
  , id =
      "group"
  }

--------------------------------------------------------------------------------
-- Abstract
--------------------------------------------------------------------------------

abstractTool : Selections a -> OutputTool
abstractTool selections =
  { name =
      "Abstract"
  , shortcut =
      Nothing
  , kind =
      Single
  , func =
      Just Controller.msgAbstractBlobs
  , reqs =
      [ groupPredicate True selections
      ]
  , id =
      "abstract"
  }

--------------------------------------------------------------------------------
-- Repeat Right
--------------------------------------------------------------------------------

repeatRightTool : Selections a -> OutputTool
repeatRightTool selections =
  { name =
      "Repeat Right"
  , shortcut =
      Nothing
  , kind =
      Single
  , func =
      Just <| Controller.msgReplicateBlob HorizontalRepeat
  , reqs =
      [ groupPredicate True selections
      ]
  , id =
      "repeatRight"
  }

--------------------------------------------------------------------------------
-- Repeat To
--------------------------------------------------------------------------------

repeatToTool : Selections a -> OutputTool
repeatToTool selections =
  { name =
      "Repeat To"
  , shortcut =
      Nothing
  , kind =
      Single
  , func =
      Just <| Controller.msgReplicateBlob LinearRepeat
  , reqs =
      [ groupPredicate True selections
      ]
  , id =
      "repeatTo"
  }

--------------------------------------------------------------------------------
-- Repeat Around
--------------------------------------------------------------------------------

repeatAroundTool : Selections a -> OutputTool
repeatAroundTool selections =
  { name =
      "Repeat Around"
  , shortcut =
      Nothing
  , kind =
      Single
  , func =
      Just <| Controller.msgReplicateBlob RadialRepeat
  , reqs =
      [ groupPredicate True selections
      ]
  , id =
      "repeatAround"
  }

--------------------------------------------------------------------------------
-- Update Output Value
--------------------------------------------------------------------------------

updateOutputValueTool : Selections a -> OutputTool
updateOutputValueTool selections =
  { name =
      "Update for New Output"
  , shortcut =
      Nothing
  , kind =
      Multi
  , func =
      Just Controller.msgCallUpdate
  , reqs =
      [ { description =
            "Make some edits in the value editor"
        , value =
            Satisfied
        }
      ]
  , id =
      "updateOutputValue"
  }

--------------------------------------------------------------------------------
-- All Tools
--------------------------------------------------------------------------------

tools : Selections a -> List (List OutputTool)
tools selections =
  List.map (List.map <| \tool -> tool selections) <|
    [ [ digHoleTool
      , makeEqualTool
      , relateTool
      , indexedRelateTool
      , buildAbstractionTool
      ]
    , [ dupeTool
      , mergeTool
      , groupTool
      , abstractTool
      ]
    , [ repeatRightTool
      , repeatToTool
      , repeatAroundTool
      ]
    , [ updateOutputValueTool
      ]
    ]
