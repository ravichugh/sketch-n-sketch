port module FileHandler exposing (
  write, writeConfirmations,
  requestFile, requestedFiles
  )

import InterfaceModel exposing (Filename, File)

port write : File -> Cmd msg

port writeConfirmations : (Filename -> msg) -> Sub msg

port requestFile : Filename -> Cmd msg

port requestedFiles : (File -> msg) -> Sub msg
