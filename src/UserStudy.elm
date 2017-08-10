module UserStudy exposing
  ( enabled
  , hideIfEnabled
  , showIfEnabled
  , sequence
  , getState
  , stepTimeoutDuration
  , stepDescription
  , getTemplate
  , enableFeaturesForEditorMode
  , postProcessCode
  , postProcessProse
  , disableNextStep
  , disablePreviousStep
  , syntaxHelp
  , textSelectHelp
  , boxSelectHelp
  )

import Updatable exposing (Updatable)
import Utils
import Random
import Dict
import Time
import ImpureGoodies
import UserStudyLog
import Regex exposing (HowMany (AtMost, All), regex)

import InterfaceModel as Model exposing
  ( CodeToolsMenuMode(..)
  )

type alias State = (Phase, (String, EditorMode))

type Phase
  = Start
  | Tutorial
  | Transition1
  | Task TaskKind
  | Transition2
  | End

type TaskKind
  = HeadToHead
  | OpenEnded

type EditorMode
  = ReadOnly
  | TextEditOnly
  | TextSelectOnly
  | BoxSelectOnly
  | CodeToolsOnly
  | AllFeatures

--------------------------------------------------------------------------------

enabled : Bool
enabled =
  True

hideIfEnabled : List a -> List a
hideIfEnabled content =
  if enabled then
    []
  else
    content

showIfEnabled : List a -> List a
showIfEnabled content =
  if enabled then
    content
  else
    []

--------------------------------------------------------------------------------

getRandom g =
  ImpureGoodies.randomInt 0 (2^32)
    |> Random.initialSeed
    |> Random.step g
    |> Tuple.first

--------------------------------------------------------------------------------

getState i =
  Utils.geti i sequence

getPhase state =
  case state of
    (phase, (_, _)) -> phase

getTemplate state =
  case state of
    (_, (template, _)) -> template

getEditorMode state =
  case state of
    (_, (_, editorMode)) -> editorMode

disableNextStep i =
  case getPhase (getState i) of
    End -> True
    _   -> False

disablePreviousStep i =
  case getPhase (getState i) of
    Start       -> True
    Tutorial    -> False
    Transition1 -> False
    _           -> True

--------------------------------------------------------------------------------

enableFeaturesForEditorMode newState m =
  case getEditorMode newState of
    -- TODO remove showDeucePanel and showDeuceRightClickMenu
    -- TODO maybe use enableEditCodeInMenuBar instead of show
    ReadOnly ->
      { m | enableTextEdits = Updatable.create False
          , enableDeuceBoxSelection = False
          , enableDeuceTextSelection = False
          , codeToolsMenuMode = CTDisabled
          }
    TextEditOnly ->
      { m | enableTextEdits = Updatable.create True
          , enableDeuceBoxSelection = False
          , enableDeuceTextSelection = False
          , codeToolsMenuMode = CTDisabled
          }
    BoxSelectOnly ->
      { m | enableTextEdits = Updatable.create False
          , enableDeuceBoxSelection = True
          , enableDeuceTextSelection = False
          , codeToolsMenuMode = CTDisabled
          }
    TextSelectOnly ->
      { m | enableTextEdits = Updatable.create False
          , enableDeuceBoxSelection = False
          , enableDeuceTextSelection = True
          , codeToolsMenuMode = CTAll
          }
    CodeToolsOnly ->
      { m | enableTextEdits = Updatable.create False
          , enableDeuceBoxSelection = True
          , enableDeuceTextSelection = True
          , codeToolsMenuMode = CTAll
          }
    AllFeatures ->
      { m | enableTextEdits = Updatable.create True
          , enableDeuceBoxSelection = True
          , enableDeuceTextSelection = True
          , codeToolsMenuMode = CTAll
          }

--------------------------------------------------------------------------------

postProcessCode state templateCode =
  case state of
    (Task _, (_, ReadOnly)) -> chopInstructions templateCode
    _                       -> templateCode

chopInstructions templateCode =
  templateCode
    |> String.lines
    |> List.filter (not << String.startsWith ";")
    |> String.join "\n"

--------------------------------------------------------------------------------

postProcessProse newState m =
  let
    prose =
      Updatable.extract m.prose
    updateModel maybeString =
      { m | prose = Updatable.create maybeString }
  in
  case newState of

    (Tutorial, (step, _)) ->
      case step of
        "Step 11" -> updateModel (shuffle_123 step prose)
        "Step 12" -> updateModel (shuffle_12 step prose)
        "Step 13" -> updateModel (shuffle_12 step prose)
        "Step 14" -> updateModel (shuffle_12 step prose)
        "Step 15" -> updateModel (shuffle_12 step prose)
        "Step 16" -> updateModel (shuffle_12 step prose)
        "Step 17" -> updateModel (shuffle_12 step prose)
        "Step 18" -> updateModel (shuffle_12 step prose)
        "Step 19" -> updateModel (shuffle_12 step prose)
        "Step 20" -> updateModel (shuffle_12 step prose)
        "Step 21" -> updateModel (shuffle_12 step prose)
        "Step 22" -> updateModel (shuffle_12 step prose)
        "Step 23" -> updateModel (shuffle_12 step prose)
        _         -> m

    (Task _, (_, ReadOnly)) ->
      updateModel (Just readOnlyProse)
    (Task _, (_, BoxSelectOnly)) ->
      updateModel (replacePlaceholderInstructionsWith prose boxSelectOnlyProse)
    (Task _, (_, TextSelectOnly)) ->
      updateModel (replacePlaceholderInstructionsWith prose textSelectOnlyProse)
    (Task _, (_, CodeToolsOnly)) ->
      updateModel (replacePlaceholderInstructionsWith prose codeToolsOnlyProse)

    _ ->
      m

-- post-processing for tasks ---------------------------------------------------

replacePlaceholderInstructionsWith prose s =
  Maybe.map
    (Regex.replace All (regex "PLACEHOLDER INSTRUCTIONS") (always s))
    prose

readOnlyProse = """<p></p><p>
Read and understand the code.
All editing features are disabled for now.
When you are ready, press Next Step.
</p>"""

textSelectOnlyProse = """<p>
Use only TEXT-SELECT MODE to perform the edits below.
When you are done, press Next Step.
</p>"""

boxSelectOnlyProse = """<p>
Use only BOX-SELECT MODE to perform the edits below.
When you are done, press Next Step.
</p>"""

codeToolsOnlyProse = """<p>
Use TEXT-SELECT MODE and/or BOX-SELECT MODE to
perform the edits below. You can mix both modes,
like in the tutorial. Text edits are disabled.
When you are done, press Next Step.
</p>"""

allFeaturesProse = """<p>
Use NORMAL TEXT EDITS and/or TEXT-SELECT MODE
and/or BOX-SELECT MODE to perform the edits below.
When you are done, press Next Step.
</p>"""

-- post-processing for tutorial ------------------------------------------------

re_123 = regex <|
  """(.*<ul class="_123">)(.*)(<li class="_1">.*</li>)(.*)(<li class="_2">.*</li>)(.*)(<li class="_3">.*</li>)(.*)(</ul>.*)"""

shuffle_123 step maybeStr =
  case maybeStr of
    Nothing -> Nothing
    Just s ->
      case Regex.find (AtMost 1) re_123 s of
        [match] ->
          let
            li1 =
              Utils.geti 3 match.submatches
            li2 =
              Utils.geti 5 match.submatches
            li3 =
              Utils.geti 7 match.submatches
            (first, second, third) =
              let i = getRandom (Random.int 1 4) in
              let _ = UserStudyLog.log "Shuffle Tutorial _123 List" (toString (step, i)) in
              case i of
                1 -> (li1, li2, li3)
                2 -> (li2, li1, li3)
                3 -> (li3, li1, li2)
                _ -> (li3, li2, li1)
          in
          match.submatches
            |> Utils.replacei 3 first
            |> Utils.replacei 5 second
            |> Utils.replacei 7 third
            |> Utils.filterJusts
            |> String.concat
            |> Just
        _ ->
          let _ = UserStudyLog.log "Shuffle Tutorial _123 List" (toString (step, "Couldn't shuffle...")) in
          maybeStr

-- NOTE: the _12 case below is mostly copied from _123 above

re_12 = regex <|
  """(.*<ul class="_12">)(.*)(<li class="_1">.*</li>)(.*)(<li class="_2">.*</li>)(.*)(</ul>.*)"""

shuffle_12 step maybeStr =
  case maybeStr of
    Nothing -> Nothing
    Just s ->
      case Regex.find (AtMost 1) re_12 s of
        [match] ->
          let
            li1 =
              Utils.geti 3 match.submatches
            li2 =
              Utils.geti 5 match.submatches
            (first, second) =
              let i = getRandom (Random.int 1 2) in
              let _ = UserStudyLog.log "Shuffle Tutorial _12 List" (toString (step, i)) in
              case i of
                1 -> (li1, li2)
                _ -> (li2, li1)
          in
          match.submatches
            |> Utils.replacei 3 first
            |> Utils.replacei 5 second
            |> Utils.filterJusts
            |> String.concat
            |> Just
        _ ->
          let _ = UserStudyLog.log "Shuffle Tutorial _12 List" (toString (step, "Couldn't shuffle...")) in
          maybeStr

--------------------------------------------------------------------------------

syntaxHelp = """
TODO
"""

textSelectHelp =
  [ "Text select a SINGLE expression in the code."
  , "Select a tool from either the Code Tools menu or the right-click pop-up menu."
  , "Follow any instructions and finish."
  ]

boxSelectHelp =
  [ "Hold down Shift, and hover and click ALL relevant expressions and target positions."
  , "Select a tool from pop-up menu."
  , "Follow any instructions and finish."
  ]

--------------------------------------------------------------------------------

numTutorialSteps = 23
structuredEditingStartStep = 10

headToHeadTaskTemplates =
  [ "One Rectangle"
  , "Two Circles"
  , "Three Rectangles"
  , "Target Icon"
  ]

fullTaskTemplates =
  [ "Four Squares"
  , "Lambda Icon"
  ]


shuffleList list =
  let
    foo remaining reordered =
      case remaining of
        [] -> reordered
        _  ->
          let n = List.length remaining in
          let i = getRandom (Random.int 1 n) in
          let (x, nextRemaining) = (Utils.geti i remaining, Utils.removei i remaining) in
          foo nextRemaining (reordered ++ [x])
  in
  foo list []


insertReadingPeriods =
  List.concatMap (\(phase, (task, mode)) ->
    [ (phase, (task, ReadOnly))
    , (phase, (task, mode))
    ]
  )

headToHeadTasks =

  -- Do each template once in random order and with a random mode,
  -- and then do them all again in (a different) random order
  -- with the other modes. If the last template in the first pass
  -- and the first template in the second pass are the same,
  -- swap the first two tasks of the second pass.

  -- TODO: swap method produces bias. fix
  let
    (firstTimeThroughAll, secondTimeThroughAll) =
      let
        shuffledTemplates =
          shuffleList headToHeadTaskTemplates
        randomBools =
          getRandom (Random.list (List.length headToHeadTaskTemplates) Random.bool)
        makeTaskMode template bool =
          (Task HeadToHead, (template, if bool then TextSelectOnly else BoxSelectOnly))
      in
        ( Utils.zipWith makeTaskMode shuffledTemplates randomBools
        , Utils.zipWith makeTaskMode shuffledTemplates (List.map not randomBools)
            |> shuffleList
        )

    maybeReshuffle list =
      let
        getTask i =
          Utils.geti i list
        indexLastInFirstPass =
          List.length headToHeadTaskTemplates
        indexFirstInSecondPass =
          indexLastInFirstPass + 1
        indexSecondInSecondPass =
          indexLastInFirstPass + 2
      in
        if getTemplate (getTask indexLastInFirstPass)
            == getTemplate (getTask indexFirstInSecondPass)
        then
          list
            |> Utils.replacei indexFirstInSecondPass (getTask indexSecondInSecondPass)
            |> Utils.replacei indexSecondInSecondPass (getTask indexFirstInSecondPass)
        else
          list
  in
    firstTimeThroughAll ++ secondTimeThroughAll
      |> maybeReshuffle
      |> insertReadingPeriods


fullTasks =
  fullTaskTemplates
    |> List.map (\template -> (Task OpenEnded, (template, CodeToolsOnly)))
    |> insertReadingPeriods


tutorialTasks =
  let template i = "Step " ++ String.padLeft 2 '0' (toString i) in
  List.map
    (\i ->
      let editorMode =
        if i < structuredEditingStartStep
          then TextEditOnly
          else AllFeatures
      in
      (Tutorial, (template i, editorMode)))
    (List.range 1 numTutorialSteps)


sequence : List State
sequence =
  List.concat
    [ [(Start, ("Deuce Study Start", TextEditOnly))]
    , tutorialTasks
    , [(Transition1, ("Deuce Study Transition 1", TextEditOnly))]
    , headToHeadTasks
    , [(Transition2, ("Deuce Study Transition 2", AllFeatures))]
    , fullTasks
    , [(End, ("Deuce Study End", AllFeatures))]
    ]


stepTimeoutDuration : State -> Time.Time
stepTimeoutDuration (phase, _) =
  case phase of
    Task HeadToHead -> 6  * Time.minute
    Task OpenEnded  -> 12 * Time.minute
    _               -> Utils.infinity


isTutorialPhase : Phase -> Bool
isTutorialPhase phase =
  case phase of
    Start       -> False
    Tutorial    -> True
    Transition1 -> True
    Task _      -> False
    Transition2 -> False
    End         -> False

isTaskPhase : Phase -> Bool
isTaskPhase phase =
  case phase of
    Start       -> False
    Tutorial    -> False
    Transition1 -> False
    Task _      -> True
    Transition2 -> True
    End         -> False

stepPhases =
  List.map (\(phase, _) -> phase) sequence

tutorialStepsCount =
  Utils.count isTutorialPhase stepPhases

tutorialStepNumber currentI =
  Utils.zipi1 stepPhases
  |> Utils.count (\(i, phase) -> i <= currentI && isTutorialPhase phase)

taskStepsCount =
  Utils.count isTaskPhase stepPhases

taskStepNumber currentI =
  Utils.zipi1 stepPhases
  |> Utils.count (\(i, phase) -> i <= currentI && isTaskPhase phase)

stepDescription : Int -> String
stepDescription stepI =
  let unknownStepText = "User Study" in
  Utils.maybeGeti1 stepI stepPhases
  |> Maybe.map
      (\phase ->
        if phase == Start then
          "Welcome to the User Study!"
        else if isTutorialPhase phase then
          "Tutorial Step " ++ toString (tutorialStepNumber stepI) ++ "/" ++ toString tutorialStepsCount
        else if isTaskPhase phase then
          "Task Step " ++ toString (taskStepNumber stepI) ++ "/" ++ toString taskStepsCount
        else if phase == End then
          "Tasks Complete"
        else
          let _ = UserStudyLog.log "Unkown Phase" (toString phase) in
          unknownStepText
      )
  |> Maybe.withDefault unknownStepText

_ =
  UserStudyLog.log "UserStudy.sequence" (toString sequence)
