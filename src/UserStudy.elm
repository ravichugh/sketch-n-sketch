module UserStudy exposing (..)

import Utils

type State
  = NotStarted
  | InProgress Phase Int
  | Finished

type Phase
  = Tutorial
  | HeadToHeadTask
  | FullTask

--------------------------------------------------------------------------------

tutorialCount = 16

headToHeadTaskTemplates =
  [ "Three Rectangles"
  , "One Rectangle"
  , "Target Icon"
  ]

fullModeTaskTemplates =
  [ "Battery Icon"
  , "Lambda Icon"
  ]

--------------------------------------------------------------------------------

nextPhase state =
  case state of
    NotStarted -> InProgress Tutorial 1
    InProgress Tutorial _ -> InProgress HeadToHeadTask 1
    InProgress HeadToHeadTask _ -> InProgress FullTask 1
    InProgress FullTask _ -> Finished
    Finished -> Finished

nextStepInPhase phase i =
  let check lastIndex = if i == lastIndex then Nothing else Just (i+1) in
  case phase of
    Tutorial -> check tutorialCount
    HeadToHeadTask -> check (List.length (headToHeadTaskTemplates))
    FullTask -> check (List.length (fullModeTaskTemplates))

nextState state =
  case state of
    NotStarted -> nextPhase state
    InProgress phase i ->
      case nextStepInPhase phase i of
        Nothing -> nextPhase state
        Just j  -> InProgress phase j
    Finished -> nextPhase state

caption state =
  let progress s i j = s ++ " " ++ Utils.parens (toString i ++ "/" ++ toString j) in
  case state of
    NotStarted -> "Not started"
    InProgress Tutorial i -> progress "Tutorial" i tutorialCount
    InProgress HeadToHeadTask i -> progress "Head-to-head task" i (List.length headToHeadTaskTemplates)
    InProgress FullTask i -> progress "Full task" i (List.length fullModeTaskTemplates)
    Finished -> "Finished!"

getTemplate state =
  case state of
    InProgress Tutorial i -> "Step " ++ String.padLeft 2 '0' (toString i)
    InProgress HeadToHeadTask i -> Utils.geti i headToHeadTaskTemplates
    InProgress FullTask i -> Utils.geti i fullModeTaskTemplates
    _ -> Debug.crash <| "getTemplate: bad userStudyState: " ++ toString state
