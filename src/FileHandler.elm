port module FileHandler exposing
  ( ExternalFileMessage(..)
  , InternalFileMessage(..)
  , sendMessage
  , receiveMessage
  )

import File exposing (Filename, File, FileIndex)

import Json.Encode as JsonE
import Json.Decode as JsonD

-- API structure derived from:
--   https://github.com/splodingsocks/a-very-im-port-ant-topic/

-- Sent from Elm to outside world
type ExternalFileMessage
  = Write Filename String
  | Delete Filename
  | Download String String
  | RequestFile Filename
  | RequestIcon String
  | RequestUploadedFile String
  | RequestFileIndex

-- Received from outside world for Elm
type InternalFileMessage
  = ConfirmWrite Filename
  | ConfirmDelete Filename
  | ReceiveFile File Bool
  | ReceiveIcon File
  | ReceiveFileIndex FileIndex

type alias ExternalData =
  { tag : String
  , data : JsonE.Value
  }

sendMessage : ExternalFileMessage -> Cmd msg
sendMessage em =
  case em of
    Write filename contents ->
      externalFileMessage
        { tag =
            "Write"
        , data =
            JsonE.object
              [ ("filename", File.encodeFilename filename)
              , ("contents", JsonE.string contents)
              ]
        }

    Delete filename ->
      externalFileMessage
        { tag =
            "Delete"
        , data =
            File.encodeFilename filename
        }

    Download title contents ->
      externalFileMessage
        { tag =
            "Download"
        , data =
            JsonE.object
              [ ("title", JsonE.string title)
              , ("contents", JsonE.string contents)
              ]
        }

    RequestFile filename ->
      externalFileMessage
        { tag =
            "RequestFile"
        , data =
            File.encodeFilename filename
        }

    RequestIcon iconName ->
      externalFileMessage
        { tag =
            "RequestIcon"
        , data =
            JsonE.object
              [ ( "iconName"
                , JsonE.string iconName
                )
              , ( "iconExtensionPrecedences"
                , JsonE.list <|
                    List.map
                      File.encodeFileExtension
                      File.iconExtensionPrecedences
                )
              ]
        }

    RequestUploadedFile inputId ->
      externalFileMessage
        { tag =
            "RequestUploadedFile"
        , data =
            JsonE.string inputId
        }

    RequestFileIndex ->
      externalFileMessage
        { tag =
            "RequestFileIndex"
        , data =
            JsonE.null
        }

receiveMessage : (InternalFileMessage -> msg) -> (String -> msg) -> Sub msg
receiveMessage tagger onError =
  let
    handler externalData =
      case externalData.tag of
        "ConfirmWrite" ->
          case JsonD.decodeValue File.filenameDecoder externalData.data of
            Ok filename ->
              tagger (ConfirmWrite filename)

            Err e ->
              onError e

        "ConfirmDelete" ->
          case JsonD.decodeValue File.filenameDecoder externalData.data of
            Ok filename ->
              tagger (ConfirmDelete filename)

            Err e ->
              onError e

        "ReceiveFile" ->
          case
            JsonD.decodeValue
              ( JsonD.map2 (,)
                  (JsonD.field "file" File.fileDecoder)
                  (JsonD.field "needsSave" JsonD.bool)
              )
              externalData.data
          of
            Ok (file, needsSave) ->
              tagger (ReceiveFile file needsSave)

            Err e ->
              onError e

        "ReceiveIcon" ->
          case JsonD.decodeValue File.fileDecoder externalData.data of
            Ok file ->
              tagger (ReceiveIcon file)

            Err e ->
              onError e

        "ReceiveFileIndex" ->
          case JsonD.decodeValue File.fileIndexDecoder externalData.data of
            Ok fileIndex ->
              tagger (ReceiveFileIndex fileIndex)

            Err e ->
              onError e

        _ ->
          onError <|
            "Unknown internal file message '"
              ++ externalData.tag
              ++ "' with data: "
              ++ toString externalData.data
  in
    internalFileMessage handler

port externalFileMessage : ExternalData -> Cmd msg
port internalFileMessage : (ExternalData -> msg) -> Sub msg
