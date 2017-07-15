module UserStudy exposing
  ( State(End)
  , sequence
  , getTemplate
  , getFinalCode
  , disableNextStep
  , disablePreviousStep
  )

import Utils
import Random

type State
  = Start
  | Transition1
  | Step (Phase, (String, EditorMode))
  | Transition2
  | End

type Phase
  = Tutorial
  | HeadToHeadTask
  | FullTask

type EditorMode
  = ReadOnly
  | TextEditOnly
  | TextSelectOnly
  | BoxSelectOnly
  | AllFeatures

--------------------------------------------------------------------------------

getState i =
  Utils.geti i sequence

getTemplate state =
  case state of
    Start       -> "Deuce Study Start"
    Transition1 -> "Deuce Study Transition 1"
    Transition2 -> "Deuce Study Transition 2"
    End         -> "Deuce Study End"
    Step (_, (s, mode)) ->
      if mode == ReadOnly
        then s -- TODO add task files without instructions
        else s

readOnly = """; Read and understand the code below.
; All editing features are disabled for now.
; When you are ready, press Next Step.
"""

textSelectOnly = """; Follow the instructions below.
; Use only TEXT SELECT MODE to perform the edits.
; When you are done, press Next Step.
"""

boxSelectOnly = """; Follow the instructions below.
; Use only BOX SELECT MODE to perform the edits.
; When you are done, press Next Step.
"""

allFeatures = """; Follow the instructions below.
; Use NORMAL TEXT EDITS or TEXT SELECT MODE or
; BOX SELECT MODE to perform the edits.
; When you are done, press Next Step.
"""

chopInstructions templateCode =
  templateCode
    |> String.lines
    |> List.filter (not << String.startsWith ";")
    |> String.join "\n"

getFinalCode state templateCode =
  case state of
    Step (_, (_, editorMode)) ->
      case editorMode of
        ReadOnly       -> readOnly ++ chopInstructions templateCode
        TextEditOnly   -> templateCode
        TextSelectOnly -> textSelectOnly ++ templateCode
        BoxSelectOnly  -> boxSelectOnly ++ templateCode
        AllFeatures    -> allFeatures ++ templateCode
    _ ->
      templateCode

disableNextStep i =
  case getState i of
    End -> True
    _   -> False

disablePreviousStep i =
  case getState i of
    Start              -> True
    Step (Tutorial, _) -> False
    Transition1        -> False
    _                  -> True

--------------------------------------------------------------------------------
-- User Study Configuration Parameters

participantNum = 1

--------------------------------------------------------------------------------

tutorialCount = 16

headToHeadTaskTemplates =
  [ "Three Rectangles"
  , "One Rectangle"
  , "Target Icon"
  ]

fullTaskTemplates =
  [ "Battery Icon"
  , "Lambda Icon"
  ]

shuffleList seed list =
  let
    foo remaining reordered seed =
      case remaining of
        [] -> (reordered, seed)
        _  ->
          let n = List.length remaining in
          let (i, nextSeed) = Random.step (Random.int 1 n) seed in
          let (x, nextRemaining) = (Utils.geti i remaining, Utils.removei i remaining) in
          foo nextRemaining (reordered ++ [x]) nextSeed
    initialSeed =
      Random.initialSeed participantNum
  in
  foo list [] seed

insertReadingPeriods =
  List.concatMap (\(phase, (task, mode)) ->
    [ (phase, (task, ReadOnly))
    , (phase, (task, mode))
    ]
  )

everything =
  let
    initialSeed =
      Random.initialSeed participantNum

    -- TODO add a choice about tutorial

    headToHeadTasks =
      headToHeadTaskTemplates
        |> List.concatMap (\template ->
             [ (HeadToHeadTask, (template, TextSelectOnly))
             , (HeadToHeadTask, (template, BoxSelectOnly))
             ]
           )
        |> shuffleList initialSeed
        |> Tuple.first
        |> insertReadingPeriods

    fullTasks =
      fullTaskTemplates
        |> List.map (\template -> (FullTask, (template, AllFeatures)))
        |> insertReadingPeriods

  in
    (headToHeadTasks, fullTasks)

headToHeadTasks = Tuple.first everything
fullTasks       = Tuple.second everything

tutorialTasks =
  let template i = "Step " ++ String.padLeft 2 '0' (toString i) in
  -- TODO use AllFeatures for structured editing tutorial
  List.map
    (\i -> (Tutorial, (template i, TextEditOnly)))
    (List.range 1 tutorialCount)

sequence : List State
sequence =
  [Start]
    ++ (List.map Step tutorialTasks)
    ++ [Transition1]
    ++ (List.map Step headToHeadTasks)
    ++ [Transition2]
    ++ (List.map Step fullTasks)
    ++ [End]

--------------------------------------------------------------------------------
-- Logging

_ =
  let
    _ = Debug.log "UserStudy.participantNum" participantNum
    _ = Debug.log "UserStudy.sequence" sequence
  in
    ()
