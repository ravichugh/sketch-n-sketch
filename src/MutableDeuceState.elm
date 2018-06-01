port module MutableDeuceState exposing
  ( setHovered
  , setSelected
  , isHovered
  , isSelected
  )

import Native.MutableDeuceState

import Lang exposing (BeforeAfter(..))
import DeuceWidgets exposing (DeuceWidget(..))

port setHashesHovered : List Int -> Cmd msg
port setHashesSelected : List Int -> Cmd msg

hash : DeuceWidget -> Int
hash dw =
  let
    hashPpid ((eid, branchNum), path) =
      let
        step1 =
          branchNum
        step2 =
          List.sum <| List.indexedMap (\i x -> 10^i * x) path
        step3 =
          eid
      in
        10^12 * step3 + 10 * step2 + step1

    hashBeforeAfter ba =
      case ba of
        Before -> 1
        After  -> 2
  in
    case dw of
      DeuceExp eid ->
        10 * eid + 1
      DeucePat ppid ->
        10 * hashPpid ppid + 2
      DeuceLetBindingEquation eid ->
        10 * eid + 3
      DeuceExpTarget (ba, eid) ->
        100 * eid + 10 * hashBeforeAfter ba + 4
      DeucePatTarget (ba, ppid) ->
        100 * hashPpid ppid + 10 * hashBeforeAfter ba + 5

setHovered : List DeuceWidget -> Cmd msg
setHovered =
  setHashesHovered << List.map hash

setSelected : List DeuceWidget -> Cmd msg
setSelected =
  setHashesSelected << List.map hash

isHovered : DeuceWidget -> Bool
isHovered =
  Native.MutableDeuceState.isHovered << hash

isSelected : DeuceWidget -> Bool
isSelected =
  Native.MutableDeuceState.isSelected << hash
