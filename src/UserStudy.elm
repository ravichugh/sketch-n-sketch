module UserStudy exposing
  ( State(End)
  , sequence
  , getTemplate
  , getFinalCode
  , disableNextStep
  , disablePreviousStep
  , syntaxHelp
  , textSelectHelp
  , boxSelectHelp
  )

import Utils
import Random
import Dict
import ImpureGoodies

import UserStudyLog

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

getTemplate state =
  case state of
    Start       -> "Deuce Study Start"
    Transition1 -> "Deuce Study Transition 1"
    Transition2 -> "Deuce Study Transition 2"
    End         -> "Deuce Study End"
    Step (_, (template, mode)) ->
      if mode == ReadOnly
        then template -- getFinalCode will make changes to the template code
        else template

getFinalCode state templateCode =
  case state of
    Step (Tutorial, ("Step 14", _)) -> reorderTutorialStep order_14_15_16 templateCode
    Step (Tutorial, ("Step 15", _)) -> reorderTutorialStep order_14_15_16 templateCode
    Step (Tutorial, ("Step 16", _)) -> reorderTutorialStep order_14_15_16 templateCode
    Step (Tutorial, ("Step 17", _)) -> reorderTutorialStep order_17 templateCode
    Step (Tutorial, (_, _))         -> templateCode
    Step (_, (_, TextEditOnly))     -> templateCode
    Step (_, (_, ReadOnly))         -> readOnly ++ chopInstructions templateCode
    Step (_, (_, TextSelectOnly))   -> textSelectOnly ++ templateCode
    Step (_, (_, BoxSelectOnly))    -> boxSelectOnly ++ templateCode
    Step (_, (_, AllFeatures))      -> allFeatures ++ templateCode
    _                               -> templateCode

-- post-processing for tasks ---------------------------------------------------

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
; Use NORMAL TEXT EDITS and/or TEXT SELECT MODE
; and/or BOX SELECT MODE to perform the edits.
; When you are done, press Next Step.
"""

chopInstructions templateCode =
  templateCode
    |> String.lines
    |> List.filter (not << String.startsWith ";")
    |> String.join "\n"

-- post-processing for tutorial ------------------------------------------------

order_14_15_16 =
  case tutorialVersion of
    1 -> ["_1","_2","_3"]
    2 -> ["_2","_1","_3"]
    3 -> ["_3","_1","_2"]
    4 -> ["_3","_2","_1"]
    _ -> Debug.crash "order: tutorialVersion > numTutorialVersions"

order_17 =
  case tutorialVersion of
    1 -> ["_1","_2"]
    2 -> ["_1","_2"]
    3 -> ["_2","_1"]
    4 -> ["_2","_1"]
    _ -> Debug.crash "order: tutorialVersion > numTutorialVersions"

reorderTutorialStep order templateCode =
  let
    prefixes =
      List.map (\s -> "; " ++ s) order

    updateDictionary prefix line =
      Dict.update prefix <| \maybeLines ->
        case maybeLines of
          Nothing    -> Just [line]
          Just lines -> Just (lines ++ [line])

    -- the following two passes assume that all the choices in
    -- templateCode appear in a single, contiguous section, e.g.
    --
    --   blah
    --   blah
    --   _begin
    --   _1 blah
    --   _1 blah
    --   _2 blah
    --   _3 blah
    --   _3 blah
    --   _3 blah
    --   _end
    --   blah

    createDictionary acc lines =
      case lines of
        [] ->
          (acc, [])
        "; _begin" :: rest ->
          let (acc_, list) = createDictionary acc rest in
          (acc_, "INSERT HERE" :: list)
        "; _end" :: rest ->
          (acc, rest)
        line :: rest ->
          if List.any (\prefix -> String.startsWith prefix line) prefixes then
            let
              -- assuming prefix is two characters (e.g. "_1", "_2", etc.)
              prefix =
                String.left 4 line
              s =
                "; " ++ String.dropLeft (String.length prefix) line
            in
            createDictionary (updateDictionary prefix s acc) rest
          else
            let (acc_, list) = createDictionary acc rest in
            (acc_, line :: list)

    insertReorderedLines (dict, lines) =
      case lines of
        [] ->
          []
        "INSERT HERE" :: rest ->
          let reordered = List.concatMap (flip Utils.justGet dict) prefixes in
          reordered ++ rest
        line :: rest ->
          line :: insertReorderedLines (dict, rest)
  in
  templateCode
    |> String.lines
    |> createDictionary Dict.empty
    |> insertReorderedLines
    |> String.join "\n"

--------------------------------------------------------------------------------

syntaxHelp = """
TODO
"""

textSelectHelp =
  [ "Text select something in the code."
  , "Select a tool from either the Edit Code menu or the right-click pop-up menu."
  , "Follow any instructions and finish."
  ]

boxSelectHelp =
  [ "Hold down Shift, and hover and click boxes."
  , "Select a tool from pop-up menu."
  , "Follow any instructions and finish."
  ]

--------------------------------------------------------------------------------
-- User Study Configuration Parameters

seed = ImpureGoodies.randomInt 0 (2^32)

--------------------------------------------------------------------------------

numTutorialSteps = 17
structuredEditingStartStep = 14
numTutorialVersions = 4

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
      Random.initialSeed seed

    (tutorialVersion, nextSeed) =
      Random.step (Random.int 1 numTutorialVersions) initialSeed

    headToHeadTasks =
      headToHeadTaskTemplates
        |> List.concatMap (\template ->
             [ (HeadToHeadTask, (template, TextSelectOnly))
             , (HeadToHeadTask, (template, BoxSelectOnly))
             ]
           )
        |> shuffleList nextSeed
        |> Tuple.first
        |> insertReadingPeriods

    fullTasks =
      fullTaskTemplates
        |> List.map (\template -> (FullTask, (template, AllFeatures)))
        |> insertReadingPeriods

  in
    (tutorialVersion, headToHeadTasks, fullTasks)

tutorialVersion = Utils.fst3 everything
headToHeadTasks = Utils.snd3 everything
fullTasks       = Utils.thd3 everything

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
    _ = UserStudyLog.log "UserStudy tutorialVersion" (toString tutorialVersion)
    _ = UserStudyLog.log "UserStudy.sequence" (toString sequence)
  in
    ()
