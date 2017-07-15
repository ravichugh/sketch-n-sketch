module UserStudy exposing
  ( State(End)
  , sequence
  , getTemplate
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

getTemplate state =
  case state of
    Start -> Debug.crash <| "getTemplate: bad userStudyState: " ++ toString state
    Step (_, (s, mode)) ->
      if mode == ReadOnly
        then s -- TODO add task files without instructions
        else s
    Transition1 -> "BLANK" -- TODO
    Transition2 -> "BLANK" -- TODO
    End -> "BLANK" -- TODO

allowPreviousStep state =
  case Utils.head "UserStudy.allowPreviousStep" state of
    Step (Tutorial, _) -> True
    Transition1        -> True
    _                  -> False

disablePreviousStep state =
  not (allowPreviousStep state)

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
