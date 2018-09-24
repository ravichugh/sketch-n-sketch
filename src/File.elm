module File exposing
  ( FileExtension(..)
  , fileExtensionToString
  , encodeFileExtension
  , iconExtensionPrecedences
  , Filename
  , encodeFilename
  , filenameDecoder
  , parseFilename
  , backupFilename
  , File
  , encodeFile
  , fileDecoder
  , FileIndex
  , fileIndexDecoder
  )

import Json.Encode as JsonE
import Json.Decode as JsonD

import Utils

--------------------------------------------------------------------------------
-- File Extensions
--------------------------------------------------------------------------------

type FileExtension
  = LittleFile
  | LeoFile
  | LittleIcon
  | LeoIcon

fileExtensionToString : FileExtension -> String
fileExtensionToString fe =
  case fe of
    LittleFile ->
      "little"

    LeoFile ->
      "elm"

    LittleIcon ->
      "licon"

    LeoIcon ->
      "eicon"

fileExtensionFromString : String -> Maybe FileExtension
fileExtensionFromString s =
  case s of
    "little" ->
      Just LittleFile

    "elm" ->
      Just LeoFile

    "licon" ->
      Just LittleIcon

    "eicon" ->
      Just LeoIcon

    _ ->
      Nothing

encodeFileExtension : FileExtension -> JsonE.Value
encodeFileExtension fe =
  JsonE.string <|
    fileExtensionToString fe

fileExtensionDecoder : JsonD.Decoder FileExtension
fileExtensionDecoder =
  flip JsonD.andThen JsonD.string <| \s ->
    case fileExtensionFromString s of
      Just fe ->
        JsonD.succeed fe

      Nothing ->
        JsonD.fail <| "Unknown file extension '." ++ s ++ "'"

iconExtensionPrecedences : List FileExtension
iconExtensionPrecedences =
  [ LeoIcon, LittleIcon ]

--------------------------------------------------------------------------------
-- Filenames
--------------------------------------------------------------------------------

type alias Filename =
  { name : String
  , extension : FileExtension
  }

encodeFilename : Filename -> JsonE.Value
encodeFilename { name, extension } =
  JsonE.object
    [ ("name", JsonE.string name)
    , ("extension", encodeFileExtension extension)
    ]

filenameDecoder : JsonD.Decoder Filename
filenameDecoder =
  JsonD.map2 Filename
    (JsonD.field "name" JsonD.string)
    (JsonD.field "extension" fileExtensionDecoder)

parseFilename : String -> Filename
parseFilename s =
  let
    default =
      { name = s, extension = LeoFile }
  in
    case Utils.maybeLast (String.indexes "." s) of
      Just i ->
        let
          name =
            String.left i s
          extensionString =
            String.dropLeft (i + 1) s
        in
          case fileExtensionFromString extensionString of
            Just extension ->
              { name = name, extension = extension }

            Nothing ->
              default

      Nothing ->
        default

backupFilename : Filename -> Filename
backupFilename filename =
  { filename
      | name = filename.name ++ "~BACKUP"
  }

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

type alias File =
  { filename : Filename
  , contents : String
  }

encodeFile : File -> JsonE.Value
encodeFile file =
  JsonE.object
    [ ("filename", encodeFilename file.filename)
    , ("contents", JsonE.string file.contents)
    ]

fileDecoder : JsonD.Decoder File
fileDecoder =
  JsonD.map2 File
    (JsonD.field "filename" filenameDecoder)
    (JsonD.field "contents" JsonD.string)

--------------------------------------------------------------------------------
-- File Index
--------------------------------------------------------------------------------

type alias FileIndex =
  List Filename

fileIndexDecoder : JsonD.Decoder FileIndex
fileIndexDecoder =
  JsonD.list filenameDecoder
