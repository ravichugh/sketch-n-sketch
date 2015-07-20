-- InterfaceStorage.elm
--
-- The necessary definitions for the in-browser and disk storage aspects of the
-- interface.
--

module InterfaceStorage (taskMailbox, saveStateLocally, loadLocalState, getLocalSaves) where

-- Storage library, for in browser storage
import Storage exposing (getItem, setItem, keys)

-- JSON encode/decode libraries, as local storage only stores values as Strings
import Json.Encode as Encode
import Json.Decode exposing (Decoder, (:=), object5, string, int, bool, customDecoder)

-- Task Library
import Task exposing (Task, succeed, andThen)

-- Signalling functions
import Signal exposing (Mailbox, mailbox, send)
-- Types for our Model
import InterfaceModel exposing (Model, Orientation, Event, sampleModel, events)
import ExamplesGenerated as Examples

-- The mailbox that recieves Tasks
taskMailbox : Mailbox (Task String ())
taskMailbox = mailbox (succeed ())

-- The necessary port for Tasks/Storage
-- **Not sure why necessary at this juncture**
-- Due to current Elm limitations, this must be in the Main module
--port taskPort : Signal (Task String ())
--port taskPort = taskMailbox.signal

-- Type for the partial object that we store in localStorage
type alias PartialObject = 
    { code : String
    , orient      : Orientation
    , showZones   : Bool
    , midOffsetX  : Int
    , midOffsetY  : Int
    }

-- JSON encoder for our Model
-- Note that this only converts the fields we care about saving:
-- * code : String
-- * orient      : Orientation (Vertical | Horizontal)
-- * showZones   : Bool
-- * midOffsetX  : Int
-- * midOffsetY  : Int
modelToValue : Model -> Encode.Value
modelToValue model =
    Encode.object <|
      [ ("code", Encode.string model.code)
      , ("orient", Encode.string 
            (case model.orient of
                InterfaceModel.Vertical -> "Vertical"
                InterfaceModel.Horizontal -> "Horizontal"
            )
        )
      , ("showZones", Encode.bool model.showZones)
      , ("midOffsetX", Encode.int model.midOffsetX)
      , ("midOffsetY", Encode.int model.midOffsetY)
      ]

-- JSON decoder for our Model
strToModel : Decoder Model
strToModel =
    let partialObjectDecoder = object5 PartialObject
            ("code" := string)
            ("orient" := customDecoder string 
                (\v -> case v of
                    "Vertical" -> Ok InterfaceModel.Vertical
                    "Horizontal" -> Ok InterfaceModel.Horizontal
                    _ -> Err "Ill-formatted orientation"
                )
            )
            ("showZones"   := bool)
            ("midOffsetX"  := int)
            ("midOffsetY"  := int)
    in customDecoder partialObjectDecoder
        (\partial -> 
            Ok { sampleModel | code <- partial.code
                             , orient <- partial.orient
                             , showZones <- partial.showZones
                             , midOffsetX <- partial.midOffsetX
                             , midOffsetY <- partial.midOffsetY
            }
        )

-- Task to save state to local browser storage
saveStateLocally : String -> Model -> Task String ()
saveStateLocally saveName model = if
    | List.all ((/=) saveName) model.localSaves
        && List.all ((/=) saveName << fst) Examples.list -> setItem saveName <| modelToValue model
    | otherwise -> send events.address <|
        InterfaceModel.UpdateModel installSaveState

-- Changes state to SaveDialog
installSaveState : Model -> Model
installSaveState oldModel = { oldModel | mode <- InterfaceModel.SaveDialog }

-- Task to load state from local browser storage
loadLocalState : Task String ()
loadLocalState = getItem "stateSave" strToModel
    `andThen` \loadedModel -> 
        send events.address <| 
            InterfaceModel.UpdateModel <| installLocalState loadedModel

-- Function to update model upon state load
installLocalState : Model -> Model -> Model
installLocalState loadedModel oldModel = 
    { loadedModel | slate <- oldModel.slate }

-- Gets the names of all of the local saves, returned in a list of strings
getLocalSaves : Task String ()
getLocalSaves = keys `andThen` \saves -> send events.address <|
    InterfaceModel.UpdateModel <| installLocalSaves saves

-- Installs the list of local saves
installLocalSaves : List String -> Model -> Model
installLocalSaves saves oldModel = { oldModel | localSaves <- saves }
