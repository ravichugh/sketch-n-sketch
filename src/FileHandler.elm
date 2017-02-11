port module FileHandler exposing
  ( write, writeConfirmation
  , requestFile, receiveFile
  , requestIcon, receiveIcon
  , requestFileFromInput, receiveFileFromInput
  , requestFileIndex, receiveFileIndex
  , delete, deleteConfirmation
  , download
  )

import InterfaceModel exposing (Filename, File, IconName, Icon, FileIndex)

type alias DownloadInfo =
  { filename : Filename
  , text : String
  }

port write : File -> Cmd msg

port writeConfirmation : (Filename -> msg) -> Sub msg

port requestFile : Filename -> Cmd msg

port receiveFile : (File -> msg) -> Sub msg

port requestIcon : IconName -> Cmd msg

port receiveIcon : (Icon -> msg) -> Sub msg

port requestFileFromInput : String -> Cmd msg

port receiveFileFromInput : (File -> msg) -> Sub msg

port requestFileIndex : () -> Cmd msg

port receiveFileIndex : (FileIndex -> msg) -> Sub msg

port delete : Filename -> Cmd msg

port deleteConfirmation : (Filename -> msg) -> Sub msg

port download : DownloadInfo -> Cmd msg
