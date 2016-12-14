port module FileHandler exposing (save, hasSaved, requestLoad, receiveLoad)

import InterfaceModel exposing (Code)

port save : Code -> Cmd msg

port hasSaved : (() -> msg) -> Sub msg

port requestLoad : () -> Cmd msg

port receiveLoad : (Code -> msg) -> Sub msg
