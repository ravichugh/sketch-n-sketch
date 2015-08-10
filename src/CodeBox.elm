-- This is the elm file responsible for returning the completed code box given
-- the Model and the appropriate dimensions.

module CodeBox (interpretAceEvents, packageModel, tripRender,
                AceMessage, AceCodeBoxInfo, initAceCodeBoxInfo) where

import Graphics.Element as GE
import InterfaceModel as Model exposing (Event, sampleModel, events)

-- So we can check on the installSaveState update
import InterfaceStorage exposing (installSaveState)

import Task exposing (Task)

import Dict exposing (Dict)

-- So we can crash correctly
import Debug

type alias AceCodeBoxInfo = 
    { code : String 
    , cursorPos : Model.AcePos
    , manipulable : Bool
    , selections : List Model.Range
    , highlights : List Model.Highlight
    , bounce : Bool
    }

type alias AceMessage = { evt : String 
                        , strArg  : String 
                        , cursorArg : Model.AcePos
                        , selectionArg : List Model.Range
                        } 

-- An initial AceCodeBoxInfo for the foldp
-- Doesn't actually get sent over the port
initAceCodeBoxInfo =
  ( { code = sampleModel.code
    , cursorPos = sampleModel.codeBoxInfo.cursorPos
    , manipulable = True
    , selections = sampleModel.codeBoxInfo.selections
    , highlights = sampleModel.codeBoxInfo.highlights
    , bounce = True
    }
  , []
  )

interpretAceEvents : AceMessage -> Event
interpretAceEvents amsg = case amsg.evt of
    "AceCodeUpdate" -> Model.UpdateModel <|
        \m -> { m | code <- amsg.strArg
                  , codeBoxInfo <- { cursorPos = amsg.cursorArg
                                   , selections = amsg.selectionArg
                                   , highlights = m.codeBoxInfo.highlights
                                   }
              }
    "Rerender" -> Model.UpdateModel <| \m -> { m | code <- m.code }
    "LoadedFromError" -> Model.UpdateModel <| recoverFromError amsg.strArg
    "init" -> Model.Noop
    _ -> Debug.crash "Malformed update sent to Elm"

-- Puts us in the correct state if we recovered from an error, which we find out
-- about from the JS that also happens to load Ace.
-- Maybe we should split this out into a different Elm/JS file?
recoverFromError : String -> Model.Model -> Model.Model
recoverFromError offendingCode fresh = 
    { fresh | code <- offendingCode
            , editingMode <- Just offendingCode
            , caption <- Just <| Model.LangError <| "Runtime Error!\n" ++
                "You likely provided the wrong number of arguments to a\n" ++
                "function or referenced an undefined expression name."
    }

-- The number of times that we defensively rerender the codebox on codebox
-- clobbering updates. Determined experimentally.
-- We shouldn't have to do this. For some reason the Elm runtime will rerender
-- parts of the page *after* sending signals out to ports.
-- Note that each one of these won't necessarily trigger a DOM copy/replacement;
-- it only will for each of the times that Elm clobbers it.
rerenderCount : Int
rerenderCount = 3

packageModel : (Model.Model, Event) -> (AceCodeBoxInfo, List Bool) -> 
                    (AceCodeBoxInfo, List Bool)
packageModel (model, evt) (lastBox, rerenders) = 
    let manipulable = case (model.mode, model.editingMode) of
            (Model.SaveDialog _, _) -> False
            (_, Nothing) -> False
            _           -> True
        rerender = tripRender evt rerenders
    in 
      ( { code = model.code 
        , cursorPos = model.codeBoxInfo.cursorPos 
        , selections = model.codeBoxInfo.selections
        , manipulable = manipulable
        , highlights = model.codeBoxInfo.highlights
        , bounce = rerender
        }
      , rerender :: List.take (rerenderCount - 1) rerenders
      )

-- Lets a signal pass if it should triger an extra rerender
-- This is entered into a foldp so that we do not enter into an infinite
-- rerender loop. Currently, all button presses are separated by at least a
-- MouseDown event, meaning that we should never miss a rerender.
-- We have the Bool list because, experimentally, one rerender is not
-- 'enough'. Occasionally the rerender still gets clobbered. Ugh.
-- Note that the initial list population is important for fixing the blanking on
-- the page load.
tripRender : Event -> List Bool -> Bool
tripRender evt last = 
  if List.all (\a -> a) last then False else
    case (evt, last) of
      (_                 , True :: rest  ) -> True
      (Model.SwitchOrient, _             ) -> True
      (Model.InstallSaveState, _         ) -> True
      (Model.RemoveDialog _ _ , _        ) -> True
      (Model.SetBasicCodeBox _ , _       ) -> True
      _                                    -> False
