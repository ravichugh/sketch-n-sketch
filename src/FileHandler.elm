port module FileHandler exposing
  ( write, writeConfirmation
  , requestFile, receiveFile
  , requestFileIndex, receiveFileIndex
  , download
  )


import InterfaceModel exposing (Filename, File, FileIndex)

type alias DownloadInfo =
  { filename : Filename
  , text : String
  }

port write : File -> Cmd msg

port writeConfirmation : (Filename -> msg) -> Sub msg

port requestFile : Filename -> Cmd msg

port receiveFile : (File -> msg) -> Sub msg

port requestFileIndex : () -> Cmd msg

port receiveFileIndex : (FileIndex -> msg) -> Sub msg

port download : DownloadInfo -> Cmd msg
