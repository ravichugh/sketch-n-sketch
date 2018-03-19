module Update exposing
  ( buildUpdatedValueFromEditorString
  , buildUpdatedValueFromDomListener
  , doUpdate
  , update -- For test
  )

import Info
import Lang exposing (..)
import Eval exposing (doEval)
import Utils
import UpdateUtils exposing (..)
import Syntax exposing (Syntax)
import LazyList exposing (LazyList)
import ParserUtils
import Results exposing
  ( Results(..)
  , ok1, oks, okLazy
  )
import MissingNumberMethods exposing (..)
import ValUnparser exposing (strVal)
import LangUtils exposing (..)
import LangSvg exposing
  ( NodeId, ShapeKind
  , AVal, AVal_(..), PathCounts, PathCmd(..), TransformCmd(..)
  , Attr, IndexedTree, RootedIndexedTree, IndexedTreeNode, IndexedTreeNode_(..)
  )

import Set
import LangParserUtils
import Dict exposing (Dict)
import Lazy
import Regex exposing (regex, HowMany(..), find, Match, escape)
import UpdateStack exposing (..)
import UpdateRegex exposing (..)
import UpdatedEnv exposing (UpdatedEnv)
import ValBuilder as Vb
import ValUnbuilder as Vu

unparse = Syntax.unparser Syntax.Elm
unparsePattern = Syntax.patternUnparser Syntax.Elm

doUpdate : Exp -> Val -> Result String Val -> Results String (UpdatedEnv, Exp)
doUpdate oldExp oldVal newValResult =
  newValResult
    --|> Result.map (\x -> let _ = Debug.log "#1" () in x)
    |> Results.fromResult
    |> Results.andThen (\out ->
      case UpdateUtils.defaultVDiffs oldVal out of
        Err msg -> Errs msg
        Ok Nothing -> ok1 (UpdatedEnv.original Eval.initEnv, oldExp)
        Ok (Just diffs) ->
          update (updateContext "initial update" Eval.initEnv oldExp oldVal out diffs) LazyList.Nil)

updateEnv: Env -> Ident -> Val -> VDiffs -> UpdatedEnv
updateEnv env k newValue modif =
  let aux: Int -> Env -> Env -> UpdatedEnv
      aux i acc env =
     case env of
       [] -> Debug.crash <| k ++ " not found in environment "
       ((kk, vv) as kv)::tail ->
         if kk == k then UpdatedEnv (List.reverse acc ++ ((kk, newValue)::tail)) [(i, modif)] else aux (i + 1) (kv::acc) tail
  in aux 0 [] env

-- Make sure that Env |- Exp evaluates to oldVal
-- NextAction is a list of HandlePreviousREsult followed by a list of Fork in the same list.
update : UpdateStack -> LazyList NextAction -> Results String (UpdatedEnv, Exp)
update updateStack nextToUpdate=
  --let _ = Debug.log ("\nUpdateStack "++updateStackName_ "  " updateStack) () in
  --let _ = Debug.log ("NextToUpdate" ++ (String.join "" <| List.map (nextActionsToString_ "  ") <| Results.toList nextToUpdate)) () in
  -- At the end of nextToUpdate, there are all the forks that can be explored later.
  case updateStack of -- callbacks to (maybe) push to the stack.
    UpdateContextS env e oldVal out diffs mb ->
       {--
      let _ = Debug.log (String.concat ["update: " , unparse e, " <-- ", valToString out, " -- env = " , envToString (pruneEnv e env), ", modifs = ", toString diffs]) () in
       --}
      update (getUpdateStackOp env e oldVal out diffs) (LazyList.maybeCons mb nextToUpdate)

    UpdateResultS fUpdatedEnv fOut mb -> -- Let's consume the stack !
       {--
      let _ = Debug.log (String.concat ["update final result: ", unparse fOut, " -- env = " , UpdatedEnv.show fUpdatedEnv]) () in
       --}
      case (LazyList.maybeCons mb nextToUpdate) of -- Let's consume the stack !
        LazyList.Nil ->
          --let _ = Debug.log "finished update with no fork" () in
          ok1 <| (fUpdatedEnv, fOut)
        LazyList.Cons head lazyTail ->
          case head of
            Fork msg newUpdateStack nextToUpdate2 ->
              --let _ = Debug.log "finished update with one fork" () in
              okLazy (fUpdatedEnv, fOut) <| (\lt m nus ntu2 -> \() ->
                --let _ = Debug.log ("Starting to look for other solutions, fork " ++ m ++ ",\n" ++ updateStackName nus) () in
                updateRec nus <| LazyList.appendLazy ntu2 lt) lazyTail msg newUpdateStack nextToUpdate2
            HandlePreviousResult msg f ->
              --let _ = Debug.log ("update needs to continue: " ++ msg) () in
              update (f fUpdatedEnv fOut) (Lazy.force lazyTail)

    UpdateResultAlternative msg updateStack maybeNext ->
      {--
      let _ = Debug.log (String.concat ["update result alternative ", msg, ": "]) () in
      --}
      update updateStack <| LazyList.appendLazy nextToUpdate <| (\msg -> Lazy.map ((\nb -> \mb ->
          case mb of
            Nothing -> LazyList.Nil
            Just alternativeUpdateStack ->
              LazyList.fromList [Fork msg alternativeUpdateStack nb]
        ) (LazyList.takeWhile (\nextAction ->
          case nextAction of
            HandlePreviousResult _ _-> True
            Fork _ _ _-> False
        ) nextToUpdate)) maybeNext) msg -- We need to close on the variable msg

    UpdateError msg ->
      Errs msg

getUpdateStackOp : Env -> Exp -> PrevOutput -> Output -> VDiffs -> UpdateStack
getUpdateStackOp env e oldVal newVal diffs =
   case e.val.e__ of
     EHole ws _ -> updateResultSameEnv env <| valToExp ws (IndentSpace "") newVal

     EConst ws num loc widget ->
       case getNum newVal of
         Err msg -> UpdateError msg
         Ok numv ->
          updateResultSameEnv env <| replaceE__ e <| EConst ws numv loc widget

     EBase ws m ->
       case m of
          EString quoteChar chars ->
            case newVal.v_ of
              VBase (VString newChars) ->   updateResultSameEnv env  <| replaceE__ e <| EBase ws (EString quoteChar newChars)
              _ -> updateResultSameEnv env <| valToExp ws (IndentSpace "") newVal
          _ -> updateResultSameEnv env <| valToExp ws (IndentSpace "") newVal

     EFun sp0 ps e sp1 ->
       case newVal.v_ of
         VClosure Nothing newPs newE newEnv ->
           case diffs of
             VClosureDiffs envModifs bodyModif -> -- Whatever the body, modified or not, we take it again.
               updateResult (UpdatedEnv newEnv envModifs) <| replaceE__ e <| EFun sp0 newPs newE sp1
             k -> Debug.crash <| "Unexpected modifications to a function: " ++ toString k
         _ -> Debug.crash "Trying to update a function with non-closure " <| valToString newVal

     EVar sp is ->
       let newUpdatedEnv = updateEnv env is newVal diffs in
       updateResult newUpdatedEnv e

     EList sp1 elems sp2 Nothing sp3 ->
       case (oldVal.v_, newVal.v_) of
         (VList origVals, VList newOutVals) ->
           case diffs of
             VListDiffs diffs ->
               let updateDiffs: Int -> UpdatedEnv ->       List (WS, Exp)  -> List (WS, Exp) -> List Val ->    List Val -> (List (Int, VListElemDiff)) -> UpdateStack
                   updateDiffs  i      collectedUpdatedEnv revElems           elemsToCollect    originalValues newValues   diffs =
                     case diffs of
                       [] ->
                         let finalElems = List.reverse <| reverseInsert elemsToCollect revElems in
                         updateResult collectedUpdatedEnv (replaceE__ e <| EList sp1 finalElems sp2 Nothing sp3)
                       (i1, modif)::modiftail ->
                         if i == i1 then
                           case modif of
                             VListElemDelete count ->
                               updateDiffs (i + count) collectedUpdatedEnv revElems (List.drop count elemsToCollect) (List.drop count originalValues) newValues modiftail

                             VListElemUpdate newModifs ->
                               case (elemsToCollect, originalValues, newValues) of
                                 ((sp, hdElem)::tlToCollect, origValue::origTail, newValue::newValuesTail) ->
                                   updateContinue "List" env hdElem origValue newValue newModifs  <|
                                     (\sp i revElems tlToCollect origTail newValuesTail  newUpdatedEnv newRawElem ->
                                     let finalEnv = UpdatedEnv.merge env collectedUpdatedEnv newUpdatedEnv in
                                     updateDiffs (i + 1) finalEnv ((sp, newRawElem)::revElems) tlToCollect origTail newValuesTail modiftail
                                    ) sp i revElems tlToCollect origTail newValuesTail
                                 _ -> Debug.crash "Unexpected missing elements"

                             VListElemInsert count ->
                               let (inserted, remainingNewVals) = Utils.split count newValues in
                               let insertionIndex = List.length revElems in
                               let ((wsBeforeCommaHead, valToWSExpHead), (wsBeforeCommaTail, valToWSExpTail), changeElementAfterInsert) =
                                    let me = Just e in
                                    if insertionIndex > 0 then
                                      if List.length elems > 1 then
                                        case List.drop (min insertionIndex (List.length elems - 1)) elems |> List.take 1 of
                                          [(wsComma,elemToCopy)] ->
                                             let psWs = ws <| Lang.precedingWhitespace elemToCopy in
                                             let indentation = if elemToCopy.start.line == elemToCopy.end.line
                                                   then InlineSpace
                                                   else IndentSpace (String.repeat (elemToCopy.start.col - 1) " ")
                                             in
                                             let policy = (wsComma, Lang.copyPrecedingWhitespace elemToCopy << valToExpFull (Just elemToCopy) psWs indentation) in
                                             (policy, policy, identity)
                                          _   -> Debug.crash "Internal error: There should be an element in this list's position"
                                      else -- Insertion index == 1 and List.length elems == 1
                                        case elems of
                                          [(wsHead, head)] ->
                                            let (wsComma, wsElem, indentation) = if e.start.line == e.end.line
                                              then (ws "", ws " ", InlineSpace)
                                              else if e.end.col - 1 > head.start.col then -- If the ] is after the value, then let's put the commas after the values.
                                                 (ws "",
                                                  ws <| "\n" ++ String.repeat (head.start.col - 1) " ",
                                                  IndentSpace (String.repeat (head.start.col - 1) " ")
                                                 )
                                              else
                                                 (ws <| "\n" ++ String.repeat (e.end.col - 2) " ",
                                                  ws (String.repeat (max (head.start.col - e.end.col - 1) 1) " "),
                                                  IndentSpace (String.repeat (e.end.col - 2) " "))
                                            in
                                            let policy = (wsComma, valToExpFull Nothing wsElem indentation) in
                                            (policy, policy, identity)
                                          _ ->  Debug.crash "Internal error: There should be an element in this list's position"
                                    else --if insertionIndex == 0 then -- Inserting the first element is always trickier
                                      case elems of
                                        [] ->
                                          if e.start.line == e.end.line then
                                            ( (ws "", valToExpFull Nothing (ws "") InlineSpace)
                                            , (ws " ", valToExpFull Nothing (ws " ") InlineSpace)
                                            , identity
                                            )
                                          else -- By default, multi-line lists will use the syntax [ elem1\n, elem2\n ...]
                                            let indentationSquareBracket = String.repeat (e.end.col - 2) " " in
                                            let indentation = indentationSquareBracket ++ "  " in
                                            ( (ws "", valToExpFull Nothing (ws " ") (IndentSpace indentation))
                                            , (ws <| "\n" ++ indentationSquareBracket, valToExpFull Nothing (ws " ") (IndentSpace indentation))
                                            , identity
                                            )
                                        (wsHead, head)::tail ->
                                          let (wsSecondBeforeComma, wsSecondBeforeValue, secondOrHead, indent) =
                                               case tail of
                                                 [] ->
                                                   if e.start.line == e.end.line then
                                                     (ws "", " ", head, InlineSpace)
                                                   else if e.end.col - 1 > head.start.col then -- The square bracket is after the element
                                                     let indentation = String.repeat (head.start.col - 1) " " in
                                                     (ws "", "\n" ++ indentation, head, IndentSpace indentation)
                                                   else
                                                     let indentation = String.repeat (e.end.col - 2) " " in
                                                     (ws <| "\n" ++ indentation, " ", head, IndentSpace indentation)
                                                 (wsNext, elemNext)::tail2 ->
                                                   let indentationSquareBracket = String.repeat (e.end.col - 2) " " in
                                                   let indentation = if elemNext.start.line == elemNext.end.line then
                                                        InlineSpace
                                                        else IndentSpace (indentationSquareBracket  ++ "  ") in
                                                   (wsNext, Lang.precedingWhitespace elemNext, elemNext, indentation)
                                          in
                                          ( (ws "", valToExpFull (Just head) (ws " ") indent)
                                          , (wsSecondBeforeComma, valToExpFull (Just secondOrHead) (ws wsSecondBeforeValue) indent)
                                          , \(nextWsBeforeComma, nextElem)-> (wsSecondBeforeComma, Lang.replacePrecedingWhitespace wsSecondBeforeValue nextElem)
                                          )
                                          -- We need to copy the whitespace of second to head.
                               in
                               let insertedExp = List.indexedMap (\index inserted ->
                                    ( (if index + insertionIndex == 0 then wsBeforeCommaHead else wsBeforeCommaTail)
                                    , (if index + insertionIndex == 0 then valToWSExpHead    else valToWSExpTail) inserted) ) inserted
                               in
                               let replaceFirst f l = case l of
                                 [] -> []
                                 head::tail -> f head::tail
                               in
                               let elemsToAdd = insertedExp in
                               -- TODO: Here in modifications, detect if we can duplicate a variable instead.
                               -- TODO: replaceFirst should be applied on the first not removed element from elemsToCollect
                               updateDiffs (i + 1) collectedUpdatedEnv (UpdateUtils.reverseInsert elemsToAdd revElems) (replaceFirst changeElementAfterInsert elemsToCollect) originalValues remainingNewVals modiftail
                         else
                           case (elemsToCollect, originalValues, newValues) of
                             (hd::tlToCollect, origValue::origTail, newValue::newValuesTail) ->
                               updateDiffs (i + 1) collectedUpdatedEnv (hd::revElems)  tlToCollect origTail newValuesTail diffs
                             _ -> Debug.crash "Unexpected missing elements"
               in updateDiffs 0 (UpdatedEnv.original env) [] elems origVals newOutVals diffs
             _ -> Debug.crash <| "Expected List modifications, got " ++ toString diffs
         _ -> UpdateError <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

     EList sp1 elems sp2 (Just tail) sp3 ->
       case (oldVal.v_, newVal.v_) of
         (VList origVals, VList newOutVals) ->
           case diffs of
             VListDiffs diffs -> --We do not allow insertions or deletions of elemnts before the tail.
               let updateDiffs: Int -> Int ->     UpdatedEnv -> List (WS, Exp) -> List (WS, Exp) -> List Val -> List Val -> List (Int, VListElemDiff) -> UpdateStack
                   updateDiffs  i      elemSize   collectedEnv  revElems          elemsToCollect    origVals    newOutVals  diffs =
                     case diffs of
                       [] ->  let finalElems = List.reverse <| UpdateUtils.reverseInsert elemsToCollect revElems in
                         updateResult collectedEnv (replaceE__ e <| EList sp1 finalElems sp2 (Just tail) sp3)
                       (i, m)::tailmodif ->
                         if i >= elemSize then
                           let finalElems = List.reverse <| UpdateUtils.reverseInsert elemsToCollect revElems in
                           let valsToRemove = List.length elemsToCollect in
                           let tailOldVal = List.drop valsToRemove origVals in
                           let tailNewOutVal = List.drop valsToRemove newOutVals in
                           updateContinue "EList tail" env tail (replaceV_ oldVal <| VList tailOldVal) (replaceV_ newVal <| VList tailNewOutVal) (VListDiffs <| UpdateUtils.offset (0 - elemSize) diffs) <| \newTailUpdatedEnv newTailExp ->
                             let finalUpdatedEnv = UpdatedEnv.merge env collectedEnv newTailUpdatedEnv in
                             updateResult finalUpdatedEnv <| replaceE__ e <| EList sp1 finalElems sp2 (Just newTailExp) sp3
                         else -- i < elemSize then
                           case m of
                             VListElemDelete count -> -- Let's check if we can propagate this delete for the tail.
                               case (origVals, elemsToCollect, newOutVals) of
                                 (headOrigVal::tailOrigVal, hdCollect::tlCollect, hdOut::tlOut) ->
                                   if (List.take count tailOrigVal |> List.all (valEqual headOrigVal)) && valEqual hdOut headOrigVal then
                                     updateDiffs (i + 1) elemSize collectedEnv (hdCollect::revElems) tlCollect tailOrigVal tlOut ((i + 1, VListElemDelete count)::tailmodif)
                                   else
                                     UpdateError <| "Cannot delete elements appended to the left of a :: . Trying to remove element " ++ valToString headOrigVal
                                 _ -> UpdateError <| "Expected non-empty lists, got at least one empty"
                             VListElemInsert count ->
                               case (origVals, elemsToCollect, newOutVals) of
                                 (headOrigVal::tailOrigVal, hdCollect::tlCollect, hdOut::tlOut) ->
                                   if (List.take count tlOut |> List.all (valEqual hdOut)) && valEqual hdOut headOrigVal then
                                     updateDiffs (i + 1) elemSize collectedEnv (hdCollect::revElems) tlCollect tailOrigVal tlOut ((i + 1, VListElemInsert count)::tailmodif)
                                   else
                                     UpdateError <| "Cannot inserted before elements appended to the left of a :: . Trying to insert element " ++ valToString headOrigVal
                                 _ -> UpdateError <| "Expected non-empty lists, got at least one empty"
                             VListElemUpdate newModifs ->
                               case (origVals, elemsToCollect, newOutVals) of
                                 (headOrigVal::tailOrigVal, (sp1, hdCollect)::tlCollect, hdOut::tlOut) ->
                                   updateContinue "EList ::" env hdCollect headOrigVal hdOut newModifs <| (
                                       \elemSize env collectedEnv revElems sp1 tlCollect tailOrigVal tlOut tailmodif -> \newUpdatedEnv newhdCollect ->
                                       let updatedEnv = UpdatedEnv.merge env collectedEnv newUpdatedEnv in
                                       updateDiffs (i + 1) elemSize updatedEnv ((sp1, newhdCollect)::revElems) tlCollect tailOrigVal tlOut tailmodif
                                     ) elemSize env collectedEnv revElems sp1 tlCollect tailOrigVal tlOut tailmodif

                                 _ -> UpdateError <| "Expected non-empty lists, got at least one empty"
               in
               updateDiffs 0 (List.length elems) (UpdatedEnv.original env) [] elems origVals newOutVals diffs
             _ -> UpdateError ("Expected  a List diff, got " ++ toString diffs)
         _ -> UpdateError ("Expected a list to update, got " ++ valToString newVal)
     ERecord sp1 mi es sp2 -> --Because records are typed, we should not allow the addition and removal of keys.
       case newVal.v_ of
         VRecord dOut ->
           case oldVal.v_ of
             VRecord dOld ->
               let errors = Dict.merge
                    (\keyOld valOld errs -> errs ++ ["Deleting key '" ++ keyOld++ "' from a record not allowed"])
                    (\key valOld valOut errs -> errs)
                    (\keyOut valOut errs -> errs ++ ["Inserting key '" ++ keyOut++ "' to a record not allowed"])
                    dOld dOut []
               in
               if not <| List.isEmpty errors then
                 UpdateError <| String.join ", " errors ++ "is not allowed. Maybe you wanted to use dictionaries?"
               else
                 case diffs of
                   VRecordDiffs dModifs ->
                     let updateDiff collectedEnv revCollectedEs esToCollect diffs = case esToCollect of
                       [] ->
                         if Dict.isEmpty diffs then
                           let finalEs = revCollectedEs |> List.reverse in
                           updateResult collectedEnv (replaceE__ e <| ERecord sp1 mi finalEs sp2)
                         else
                           case mi of
                             Nothing -> UpdateError <| "Trying to touch keys " ++ (Dict.keys diffs |> String.join ",") ++ " but they do not exist in " ++ Syntax.unparser Syntax.Elm e
                             Just (init, spm) ->
                               let shadowingKeys = Set.fromList <| Utils.recordKeys es in
                               case doEval Syntax.Elm env init of
                                 Err msg -> UpdateError msg
                                 Ok ((initv, _), _) ->
                                   case initv.v_ of
                                     VRecord dinit ->
                                       let newInitV = replaceV_ initv <| (dinit |>
                                            Dict.map (\k v ->  --Modify
                                             if Set.member k shadowingKeys then
                                               v
                                             else
                                               (Dict.get k dOut |> Utils.fromJust_ "ERecord update3") -- Push back all new values except those shadowed by es.
                                             ) |> VRecord)
                                       in
                                       updateContinue "ERecord init" env init initv newInitV (VRecordDiffs diffs) <|
                                         (\e env collectedEnv sp1 spm revCollectedEs sp2->  \newInitEnv newInit ->
                                         let finalEnv = UpdatedEnv.merge env collectedEnv newInitEnv in
                                         updateResult finalEnv (replaceE__ e <| ERecord sp1 (Just (newInit, spm)) (List.reverse revCollectedEs) sp2))
                                         e env collectedEnv sp1 spm revCollectedEs sp2
                                     _ -> UpdateError <| "Expected a record, got " ++ valToString initv
                       ((sp3, sp4, k, sp5, ei) as e1)::esToCollectTail ->
                         case Dict.get k diffs of
                           Nothing -> updateDiff collectedEnv (e1::revCollectedEs) esToCollectTail diffs
                           Just eiModif ->
                             let originalEi = Dict.get k dOld |> Utils.fromJust_ "ERecord update1" in
                             let modifiedEi = Dict.get k dOut |> Utils.fromJust_ "ERecord update2" in
                             let newDiffs = Dict.remove k diffs in
                             updateContinue ("ERecord " ++ k) env ei originalEi modifiedEi eiModif <|
                             ( \env collectedEnv sp3 sp4 k sp5 newDiffs->
                               \newUpdatedEnv newEi ->
                               let finalEnv = UpdatedEnv.merge env collectedEnv newUpdatedEnv in
                               updateDiff finalEnv ((sp3, sp4, k, sp5, newEi)::revCollectedEs) esToCollectTail newDiffs
                             ) env collectedEnv sp3 sp4 k sp5 newDiffs
                     in updateDiff (UpdatedEnv.original env) [] es dModifs
                   _ -> UpdateError ("Expected  a Record diff, got " ++ toString diffs)
             _ -> UpdateError ("Expected Record as original value, got " ++ valToString oldVal)
         _ -> UpdateError ("Expected Record as value to update from, got " ++ valToString newVal)

     ESelect sp0 e1 sp1 sp2 ident ->
       case doEval Syntax.Elm env e1 of
         Err msg -> UpdateError msg
         Ok ((initv, _), _) ->
           case initv.v_ of
             VRecord dinit ->
               let newE1Value = replaceV_ initv <| VRecord (Dict.insert ident newVal dinit) in
               let propagatedDiff = VRecordDiffs (Dict.fromList [(ident, diffs)]) in
               updateContinue "ESelect" env e1 initv newE1Value propagatedDiff <|
                 \newE1UpdatedEnv newE1 ->
                   updateResult newE1UpdatedEnv <| replaceE__ e <| ESelect sp0 newE1 sp1 sp2 ident
             _ -> UpdateError ("Expected Record, got " ++ valToString initv)

     EApp sp0 e1 e2s appType sp1 ->
       let maybeUpdateStack = case e1.val.e__ of
         ESelect es0 eRecord es1 es2 "apply" -> -- Special case here. apply takes a record and a value and applies the field apply to the value.
             -- The user may provide the reverse function if "unapply" is given, or "update"
             case doEval Syntax.Elm env eRecord of
               Err s -> Just <| UpdateError s
               Ok ((v1, _), _) ->
                 case v1.v_ of
                   VRecord d ->
                     if Dict.member "apply" d && (Dict.member "unapply" d || Dict.member "update" d) then
                       case e2s of
                         [] -> Nothing
                         [argument] ->
                           let mbUpdateField = case Dict.get "update" d of
                             Nothing -> Nothing
                             Just fieldUpdateClosure ->
                                case doEval Syntax.Elm env argument of
                                  Err s -> Just <| UpdateError s
                                  Ok ((vArg, _), _) ->
                                    let x = eVar "x" in
                                    let y = eVar "y" in
                                    let diffsVal = vDiffsToVal v1 diffs in
                                    let customArgument = Vb.record vArg identity <| Dict.fromList [
                                         ("input", vArg),
                                         ("output", newVal),
                                           ("outputNew", newVal), -- Redundant
                                         ("outputOld", oldVal),
                                           ("outputOriginal", oldVal), -- Redundant
                                           ("oldOutput", oldVal), -- Redundant
                                         ("diff", diffsVal),
                                           ("outDiff", diffsVal), -- Redundant
                                           ("diffOut", diffsVal) -- Redundant
                                         ] in
                                    let customExpr = replaceE__ e <| EApp space0 x [y] SpaceApp space0 in
                                    case doEval Syntax.Elm (("x", addUpdateCapability fieldUpdateClosure)::("y", customArgument)::env) customExpr of
                                      Err s -> Just <| UpdateError <| "while evaluating a lens, " ++ s
                                      Ok ((vResult, _), _) -> -- Convert vResult to a list of results.
                                        case Vu.record Ok vResult of
                                          Err msg -> Just <| UpdateError "The update closure should return either {values = [list of values]}, {error = \"Error string\"}, or more advanced { values = [...], diffs = [..Nothing/Just diff per value.]}"
                                          Ok d ->
                                            let error = case Dict.get "error" d of
                                                Just errorv -> case Vu.string errorv of
                                                  Ok e -> e
                                                  Err x -> "the .error of the result of updateApp should be a string, got " ++ valToString errorv
                                                Nothing -> ""
                                            in
                                            if error /= "" then Just <| UpdateError error
                                            else
                                              case Dict.get "values" d of
                                                Nothing -> Just <| UpdateError <| "updateApp should return a record containing a .values field or an .error field"
                                                Just values ->  case Vu.list Ok values of
                                                  Err x -> Just <| UpdateError <| "updateApp should return a record whose .values field is a list. Got " ++ valToString values
                                                  Ok valuesList ->
                                                    let valuesListLazy = LazyList.fromList valuesList in
                                                    let diffsListRes = case Dict.get "diffs" d of
                                                      Nothing -> Utils.projOk <|
                                                        List.map (\r ->
                                                          if valToString vArg == valToString r then
                                                            Ok Nothing
                                                          else UpdateUtils.defaultVDiffs vArg r) <| valuesList
                                                      Just resultDiffsV ->
                                                        Vu.list (UpdateUtils.valToMaybe valToVDiffs) resultDiffsV |>
                                                          Result.mapError (\msg -> "the .diffs of the result of updateApp should be a list of (Maybe differences). " ++ msg)
                                                    in
                                                    case diffsListRes of
                                                     Err msg -> Just <| UpdateError msg
                                                     Ok diffsList ->
                                                       let resultDiffs = LazyList.zip valuesListLazy (LazyList.fromList <| List.map Ok diffsList) in
                                                       case resultDiffs of -- TODO: Allow an API for the user to return modifications as well
                                                         LazyList.Nil -> Nothing
                                                         LazyList.Cons (head, headDiff) lazyTail ->
                                                           Just <| updateContinueRepeat ".update" env argument vArg head headDiff lazyTail <|
                                                             \newUpdatedEnvArg newArg ->
                                                             let newExp = replaceE__ e <| EApp sp0 (replaceE__ e1 <| ESelect es0 eRecord es1 es2 "apply") [newArg] appType sp1 in
                                                             updateResult newUpdatedEnvArg newExp
                           in
                           updateMaybeFirst2 mbUpdateField <| \_ ->
                             case Dict.get "unapply" d of
                               Just fieldUnapplyClosure ->
                                 case doEval Syntax.Elm env argument of
                                   Err s -> Just <| UpdateError s
                                   Ok ((vArg, _), _) ->
                                     let x = eVar "x" in
                                     let y = eVar "y" in
                                     let customArgument = newVal in
                                     let customExpr = replaceE__ e <| EApp space0 x [y] SpaceApp space0 in
                                     case doEval Syntax.Elm (("x", fieldUnapplyClosure)::("y", customArgument)::env) customExpr of
                                       Err s -> Just <| UpdateError <| "while evaluating a lens, " ++ s
                                       Ok ((vResult, _), _) -> -- Convert vResult to a list of results.
                                         case interpreterMaybeToMaybe "the result of executing 'unapply'" vResult of
                                           Err msg -> Just <| UpdateError msg
                                           Ok Nothing -> Nothing
                                           Ok (Just newOut) ->
                                             case UpdateUtils.defaultVDiffs vArg newOut of
                                               Err msg -> Just <| UpdateError msg
                                               Ok Nothing -> Just <| updateResultSameEnv env e
                                               Ok (Just newDiff) ->
                                                 Just <| updateContinue ".unapply" env argument vArg newOut newDiff <|
                                                   \newUpdatedEnvArg newArg ->
                                                   let newExp = replaceE__ e <| EApp sp0 (replaceE__ e1 <| ESelect es0 eRecord es1 es2 "apply") [newArg] appType sp1 in
                                                   updateResult newUpdatedEnvArg newExp
                               Nothing -> Nothing
                         _ -> Nothing
                     else Nothing
                   _ -> Nothing
         EVar _ "freeze" -> --Special meaning of freeze. Just check that it takes only one argument and that it's the identity.
            case e2s of  -- TODO: If freeze is not the first solution, it might prevent the updateError to show up.
              [argument] ->
                case doEval Syntax.Elm env argument of
                  Err s -> Just <| UpdateError s
                  Ok ((vArg, _), _) ->
                    if valEqual vArg oldVal then -- OK, that's the correct freeze semantics
                      if valEqual vArg newVal then
                        Just <| updateResultSameEnv env e
                      else
                        Just <| UpdateError ("You are trying to update " ++ unparse e ++ " with a value '" ++ valToString newVal ++ "' that is different from the value that it produced: '" ++ valToString oldVal ++ "'")
                    else Nothing
              _ -> Nothing
         _ ->
            Nothing
       in
       updateMaybeFirst maybeUpdateStack <| \_ ->
         case doEval Syntax.Elm env e1 of
           Err s       -> UpdateError s
           Ok ((v1, _),_) ->
             case v1.v_ of
               VClosure recName e1ps eBody env_ as vClosure ->
                 let ne1ps = List.length e1ps in
                 let es2ToEval = List.take ne1ps e2s in
                 let es2ForLater = List.drop ne1ps e2s in

                 if List.length es2ForLater > 0 then -- Rewriting of the expression so that it is two separate applications
                   updateContinue "Rewriting app" env (replaceE__ e <|
                     EApp sp0 (replaceE__ e <| EApp sp0 e1 es2ToEval SpaceApp sp1) es2ForLater SpaceApp sp1) oldVal newVal diffs <| (\newUpdatedEnv newBody ->
                     case newBody.val.e__ of
                       EApp _ innerApp newEsForLater _ _ ->
                         case innerApp.val.e__ of
                           EApp _ newE1 newEsToEval _ _ ->
                             updateResult newUpdatedEnv (replaceE__ e <| EApp sp0 newE1 (newEsToEval ++ newEsForLater) appType sp1)
                           e -> Debug.crash <| "Internal error: expected EApp, got " ++ toString e
                       e -> Debug.crash <| "Internal error: expected EApp, got " ++ toString e
                   )
                 else
                 case List.map (doEval Syntax.Elm env) es2ToEval |> Utils.projOk of
                   Err s       -> UpdateError s
                   Ok v2ls ->
                     let v2s = List.map (\((v2, _), _) -> v2) v2ls in
                     let ne2 = List.length e2s in
                     if ne1ps > ne2 then -- Less arguments than expected, hence partial application.
                       --let _ = Debug.log ("Less arguments than expected, instead of " ++ toString ne1ps ++ " got " ++ toString ne2) () in
                       let e1psNotUsed = List.drop ne2 e1ps in
                       let e1psUsed = List.take ne2 e1ps in
                       case (newVal.v_, diffs) of
                         (VClosure _ psOut outBody envOut_, VClosureDiffs modifEnv modifBody) ->
                           let updatedEnvOut = UpdatedEnv envOut_ modifEnv in
                           if UpdatedEnv.isUnmodified updatedEnvOut && Utils.maybeIsEmpty modifBody then
                             updateResultSameEnv env e -- at this point, no modifications
                           else
                           --let _ = Debug.log ("Updating with : " ++ valToString newVal) () in
                           let continuation newV1 newV1Diffs newV2s newV2sDiffs msg =
                             let e1_updater = case newV1Diffs of
                               Nothing -> \continuation -> continuation (UpdatedEnv.original env) e1
                               Just v1Diffs -> updateContinue ("VClosure1" ++ msg) env e1 v1 newV1 v1Diffs
                             in
                             let e2s_updater = case newV2sDiffs of
                               [] -> \continuation -> continuation (UpdatedEnv.original env) e2s
                               v2sDiffs -> updateContinueMultiple ("args of " ++ msg) env (List.map3 (\e2 v2 newV2 -> (e2, v2, newV2)) e2s v2s newV2s) v2sDiffs
                             in
                             e1_updater  <| \newE1UpdatedEnv newE1 ->
                                e2s_updater <| \newE2sUpdatedEnv newE2s ->
                                  let finalUpdatedEnv = UpdatedEnv.merge env newE1UpdatedEnv newE2sUpdatedEnv in
                                  updateResult finalUpdatedEnv <| replaceE__ e <| EApp sp0 newE1 newE2s appType sp1
                           in
                           case recName of
                             Nothing ->
                                case conssWithInversion (e1psUsed, v2s)
                                        (Just (env_,
                                               \newUpdatedEnv_ newE1ps ->
                                                 (replaceV_ v1 <| VClosure recName newE1ps outBody newUpdatedEnv_.val, VClosureDiffs newUpdatedEnv_.changes modifBody))) of
                                  Just (env__, consBuilder) -> --The environment now should align with updatedEnvOut_
                                    let ((newE1psUsed, newE1psUsedDiffs, newV2s, newV2sDiffs), newE1psToClosureAndDiff) = consBuilder updatedEnvOut in
                                    let (newV1, newV1Diffs) =
                                      case (newE1psUsedDiffs, modifBody) of
                                        ([], Nothing) -> (v1, Nothing)
                                        _ -> newE1psToClosureAndDiff (newE1psUsed ++ psOut) |> (\(a, b) -> (a, Just b))
                                     in
                                    continuation newV1 newV1Diffs newV2s newV2sDiffs "app"

                                  _          -> Debug.crash <| strPos e1.start ++ "bad environment, internal error in update"
                             Just f ->
                                --let _ = Debug.log ("Recursive updating with environment: " ++ envToString (List.take 4 updatedEnvOut_)) () in
                                case conssWithInversion (e1psUsed, v2s)
                                      (consWithInversion (pVar f, v1) -- This order to be consistent with eval, where f is put first in the environment.
                                        (Just (env_,
                                               \newUpdatedEnv_ newE1ps ->
                                                 (replaceV_ v1 <| VClosure recName newE1ps outBody newUpdatedEnv_.val, VClosureDiffs newUpdatedEnv_.changes modifBody )))) of
                                  Just (env__, consBuilder) -> --The environment now should align with updatedEnvOut_
                                    --let _ = Debug.log ("Original environment : " ++ envToString (List.take 4 env__)) () in
                                    let ((newE1psUsed, newE1psUsedDiffs, newV2s, newV2sDiffs),
                                         ((newPatFun, newPatFunDiffs, newArgFun, newArgFunDiffs),
                                           newE1psToClosureAndDiff)) = consBuilder updatedEnvOut in
                                    --let _ = Debug.log ("newArgFun : " ++ valToString newArgFun) () in
                                    --let _ = Debug.log ("v1 : " ++ valToString v1) () in
                                    let (newV1, newV1Diffs)  = case (newE1psUsedDiffs, modifBody, newArgFunDiffs) of
                                      ([], Nothing, Nothing) -> (v1, Nothing)
                                      ([], Nothing, _) -> (newArgFun, newArgFunDiffs)
                                      (_, _, Nothing) -> newE1psToClosureAndDiff (newE1psUsed ++ psOut) |> (\(a, b) -> (a, Just b))
                                      (_, _, Just realArgFunDiffs) ->
                                        let (closure, closuremodifs) = newE1psToClosureAndDiff (newE1psUsed ++ psOut) in
                                        let (newv, newDiffs) = mergeVal v1 newArgFun realArgFunDiffs closure closuremodifs in
                                        (newv, Just newDiffs)
                                    in
                                    continuation newV1 newV1Diffs newV2s newV2sDiffs "rec app"

                                  _          -> Debug.crash <| strPos e1.start ++ "bad environment, internal error in update"
                         v          -> UpdateError <| strPos e1.start ++ "Expected a closure in output, got " ++ valToString newVal

                     else -- The right number of arguments
                     let continuation newClosure newClosureDiffs newArgs newArgsDiffs msg = --Update the function and the arguments.
                       let e1_updater = case newClosureDiffs of
                         Nothing -> \continuation -> continuation (UpdatedEnv.original env) e1
                         Just (VClosureDiffs [] Nothing) -> \continuation -> continuation (UpdatedEnv.original env) e1
                         Just closureDiffs -> updateContinue msg env e1 v1 newClosure closureDiffs
                       in
                       let e2s_updater = case newArgsDiffs of
                         [] -> \continuation -> continuation (UpdatedEnv.original env) e2s
                         argsDiffs -> updateContinueMultiple ("args of " ++ msg) env (Utils.zip3 e2s v2s newArgs) argsDiffs
                       in
                        -- Cannot use the indices of because they are for the closure. But we could use other modifications at this stage, e.g. inserting a variable.
                       e1_updater <| \newE1UpdatedEnv newE1 ->
                          e2s_updater <| \newE2UpdatedEnv newE2s ->
                            let finalEnv = UpdatedEnv.merge env newE1UpdatedEnv newE2UpdatedEnv in
                            updateResult finalEnv <| replaceE__ e <| EApp sp0 newE1 newE2s appType sp1
                     in
                     case recName of
                       Nothing ->
                          case conssWithInversion (e1ps, v2s) (Just (env_,
                              \newUpdatedEnv_ -> \newPs newPatsDiffs newBody newBodyDiffs -> (replaceV_ v1 <| VClosure Nothing newPs newBody newUpdatedEnv_.val, VClosureDiffs newUpdatedEnv_.changes newBodyDiffs))) of
                            Just (env__, consBuilder) ->
                               -- consBuilder: Env -> ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure)
                                updateContinue "VClosure3" env__ eBody oldVal newVal diffs <| \newUpdatedEnv newBody ->
                                  let newBodyDiffs = if Syntax.unparser Syntax.Elm eBody == Syntax.unparser Syntax.Elm newBody then Nothing else Just EChanged in
                                  let ((newPats, newPatsDiffs, newArgs, newArgsDiffs), patsBodyToClosure) = consBuilder newUpdatedEnv in
                                  let (newClosure, newClosureDiff) = patsBodyToClosure newPats newPatsDiffs newBody newBodyDiffs in -- TODO: Once we return the diff of the expression, check for it before invoking patsBody...
                                  continuation newClosure (Just newClosureDiff) newArgs newArgsDiffs "full app"
                            _          -> UpdateError <| strPos e1.start ++ "bad environment"
                       Just f ->
                          case conssWithInversion (e1ps, v2s) (
                               consWithInversion (pVar f, v1) (
                               Just (env_, \newUpdatedEnv_ -> \newPs newPsDiffs newBody newBodyDiffs -> (replaceV_ v1 <| VClosure (Just f) newPs newBody newUpdatedEnv_.val, VClosureDiffs newUpdatedEnv_.changes newBodyDiffs)))) of
                            Just (env__, consBuilder) ->
                               -- consBuilder: Env -> ((Pat, Val), ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure))
                                updateContinue "VClosure6"  env__ eBody oldVal newVal diffs <| \newUpdatedEnv newBody ->
                                  let newBodyDiffs = if Syntax.unparser Syntax.Elm eBody == Syntax.unparser Syntax.Elm newBody then Nothing else Just EChanged in
                                  let ((newPats, newPatsDiffs, newArgs, newArgsDiffs),
                                      ((newPatFun, newPatFunDiffs, newArgFun, newArgFunDiffs), patsBodytoClosureAndDiff)) = consBuilder newUpdatedEnv in
                                  let (newClosure, newClosureDiff) =
                                     case (newPatsDiffs, newBodyDiffs, UpdatedEnv.isUnmodified newUpdatedEnv, newArgFunDiffs) of
                                       ([], Nothing, True, Nothing) ->
                                         (v1, Nothing)
                                       ([], Nothing, True, _) ->
                                         (newArgFun, newArgFunDiffs)
                                       (_, _, _, Nothing) ->
                                         patsBodytoClosureAndDiff newPats newPatsDiffs newBody newBodyDiffs |> \(a, b) -> (a, Just b)
                                       (_, _, _, Just realArgFunDiffs) ->
                                         let (vclosure, vclosureDiff) = (patsBodytoClosureAndDiff newPats newPatsDiffs newBody newBodyDiffs) in
                                         mergeVal v1 newArgFun realArgFunDiffs vclosure vclosureDiff |> \(a, b) -> (a, Just b)
                                  in
                                  continuation newClosure newClosureDiff newArgs newArgsDiffs "full rec app"
                            x -> Debug.crash <| "Internal error, should have get a list, got " ++ toString x

               VFun name argList evalDef maybeUpdateDef ->
                 case maybeUpdateDef of
                   Nothing -> UpdateError ("No built-in definition to update " ++ name)
                   Just updateDef ->
                     let arity = List.length argList in
                     let nAvailableArgs = List.length e2s in

                     if arity < nAvailableArgs then -- Rewriting of the expression so that it is two separate applications
                       let es2ToEval = List.take arity e2s in
                       let es2ForLater = List.drop arity e2s in
                       updateContinue "EApp VFun" env (replaceE__ e <|
                         EApp sp0 (replaceE__ e <| EApp sp0 e1 es2ToEval SpaceApp sp1) es2ForLater SpaceApp sp1) oldVal newVal diffs <| (\newUpdatedEnv newBody ->
                         case newBody.val.e__ of
                           EApp _ innerApp newEsForLater _ _ ->
                             case innerApp.val.e__ of
                               EApp _ newE1 newEsToEval _ _ ->
                                 updateResult newUpdatedEnv (replaceE__ e <| EApp sp0 newE1 (newEsToEval ++ newEsForLater) appType sp1)
                               e -> Debug.crash <| "Internal error: expected EApp, got " ++ Syntax.unparser Syntax.Elm innerApp
                           e -> Debug.crash <| "Internal error: expected EApp, got " ++ Syntax.unparser Syntax.Elm newBody
                       )
                     else if arity > nAvailableArgs then  -- Rewrite using eta-expansion.
                       let convertedBody = replaceE__ e1 <|
                         EApp sp0 (replaceE__ e <| EVar space1 name) (List.map (withDummyExpInfo << EVar space1) <| argList) SpaceApp sp0 in
                       let funconverted = replaceE__ e1 <| EFun
                            space0
                            (List.map (\n -> withDummyPatInfo <| PVar space1 n noWidgetDecl) <| argList)
                            convertedBody space0 in
                       updateContinue "EApp VFun eta" env
                         (replaceE__ e <| EApp space0 funconverted e2s SpaceApp space0)
                         oldVal
                         newVal
                         diffs
                         <| \newUpdatedEnv newBody ->
                           case newBody.val.e__ of
                             EApp _ funreconverted newEs _ _ ->
                               if expEqual funreconverted funconverted then
                                 updateResult newUpdatedEnv (replaceE__ e <| EApp sp0 e1 newEs SpaceApp sp1)
                               else UpdateError "Cannot modify the definition of a built-in function"
                             _ -> Debug.crash <| "Internal error: expected EApp, got" ++ Syntax.unparser Syntax.Elm e
                     else -- Right arity
                       case List.map (doEval Syntax.Elm env) e2s |> Utils.projOk of
                         Err s       -> UpdateError s
                         Ok v2ls     ->
                           let v2s = List.map (\((v2, _), _) -> v2) v2ls in
                           case updateDef v2s oldVal newVal of
                             Errs msg -> UpdateError msg
                             Oks ll ->
                               let llWithDiffResult = ll |> LazyList.map (\outputs ->
                                 (outputs, UpdateUtils.defaultTupleDiffs valToString defaultVDiffs v2s outputs)) in
                               updateOpMultiple "vfun" env e2s (\newE2s -> replaceE__ e <| EApp sp0 e1 newE2s appType sp1) v2s llWithDiffResult

               _ -> UpdateError <| strPos e1.start ++ " not a function"
     EIf sp0 cond sp1 thn sp2 els sp3 ->
       case doEval Syntax.Elm env cond of
         Ok ((v, _), _) ->
           case v.v_ of
             VBase (VBool b) ->
               if b then
                 updateContinue "IfThen" env thn oldVal newVal diffs <| \newUpdatedEnv newThn ->
                   updateResult newUpdatedEnv <| replaceE__ e <| EIf sp0 cond sp1 newThn sp2 els sp3
               else
                 updateContinue "IfElse" env els oldVal newVal diffs<| \newUpdatedEnv newEls ->
                   updateResult newUpdatedEnv <| replaceE__ e <| EIf sp0 cond sp1 thn sp2 newEls sp3
             _ -> UpdateError <| "Expected boolean condition, got " ++ valToString v
         Err s -> UpdateError s

     EOp sp1 op opArgs sp2 ->
       case (op.val, opArgs) of
         (NoWidgets, [arg]) ->
           updateContinue  "NoWidgets" env arg oldVal newVal diffs <| \newUpdatedEnv newArg ->
             updateResult newUpdatedEnv <| replaceE__ e <| EOp sp1 op [newArg] sp2
         (DebugLog, [arg]) ->
           updateContinue  "DebugLog" env arg oldVal newVal diffs <| \newUpdatedEnv newArg ->
             updateResult newUpdatedEnv <| replaceE__ e <| EOp sp1 op [newArg] sp2
         _ ->
           case Utils.projOk <| List.map (doEval Syntax.Elm env) opArgs of
             Err msg -> UpdateError msg
             Ok argsEvaled ->
               let ((vs, wss), envs) = Tuple.mapFirst List.unzip <| List.unzip argsEvaled in
               let args = List.map .v_ vs in
               case op.val of
                 Explode    -> Debug.crash "Not implemented: update Explode "
                 ToStrExceptStr ->
                   let default () =
                        case newVal.v_ of
                          VBase (VString s) ->
                            case Syntax.parser Syntax.Elm s of
                              Err msg -> UpdateError <| "Could not parse new output value '"++s++"' for ToStr expression " ++ ParserUtils.showError msg
                              Ok parsed ->
                                case doEval Syntax.Elm [] parsed of
                                  Err msg -> UpdateError msg
                                  Ok ((v, _), _) ->
                                    case (opArgs, vs) of
                                      ([opArg], [arg]) ->
                                        let continue = case UpdateUtils.defaultVDiffs arg v of
                                          Err msg -> \continuation -> UpdateError msg
                                          Ok Nothing -> \continuation -> updateResultSameEnv env e
                                          Ok (Just vDiff) -> updateContinue "EOp ToStrExceptStr default" env opArg arg v vDiff
                                        in
                                        continue <| \newUpdatedEnv newOpArg ->
                                            updateResult newUpdatedEnv <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                      e -> UpdateError <| "[internal error] Wrong number of arguments in update ToStrExceptStr: " ++ toString e
                          e -> UpdateError <| "Expected string, got " ++ valToString newVal
                   in
                   case vs of
                     [original] ->
                       case original.v_ of
                         VBase (VString origS) ->
                           case opArgs of
                             [opArg] ->
                               updateContinue "EOp ToStrExceptStr" env opArg original newVal diffs <| \newUpdatedEnv newOpArg ->
                                 updateResult newUpdatedEnv <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                             e -> UpdateError <| "[internal error] Wrong number of argument values in update ToStrExceptStr: " ++ toString e
                         _ -> -- Everything else is unparsed to a string, we just parse it.
                           default ()
                     _ -> UpdateError <| "[internale error] Wrong number or arguments in updateToStrExceptStr: " ++ toString e
                 ToStr      ->
                   case newVal.v_ of
                     VBase (VString s) ->
                       case Syntax.parser Syntax.Elm s of
                         Err msg -> UpdateError <| "Could not parse new output value '"++s++"' for ToStr expression. " ++ (ParserUtils.showError msg)
                         Ok parsed ->
                           case doEval Syntax.Elm [] parsed of
                             Err msg -> UpdateError msg
                             Ok ((v, _), _) ->
                               case (opArgs, vs) of
                                 ([opArg], [arg]) ->
                                   let continue = case UpdateUtils.defaultVDiffs arg v of
                                     Err msg -> \continuation -> UpdateError msg
                                     Ok Nothing -> \continuation -> updateResultSameEnv env e
                                     Ok (Just vDiff) -> updateContinue "EOp ToStr" env opArg arg v vDiff
                                   in
                                   continue <| \newUpdatedEnv newOpArg ->
                                         updateResult newUpdatedEnv <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                 e -> UpdateError <| "[internal error] Wrong number of arguments in update: " ++ toString e
                     e -> UpdateError <| "Expected string, got " ++ valToString newVal
                 RegexReplaceAllIn -> -- TODO: Move this in maybeUpdateMathOp
                   case vs of
                     [regexpV, replacementV, stringV] ->
                       let eRec env exp = doEval Syntax.Elm env exp |> Result.map (\((v, _), _) -> v) in
                       let uRec: Env -> Exp -> Val -> Val -> Results String (UpdatedEnv, Exp)
                           uRec env exp oldval newval =
                             case UpdateUtils.defaultVDiffs oldval newval of
                               Err msg -> Errs msg
                               Ok Nothing -> ok1 (UpdatedEnv.original env, exp)
                               Ok (Just newvalDiff) -> update (updateContext "recursive update" env exp oldval newval newvalDiff) LazyList.Nil
                       in
                       case UpdateRegex.updateRegexReplaceAllByIn
                           env eRec uRec regexpV replacementV stringV oldVal newVal of
                         Errs msg -> UpdateError msg
                         Oks ll -> updateOpMultiple "replaceAllIn" env opArgs (\newOpArgs -> replaceE__ e <| EOp sp1 op newOpArgs sp2) vs
                           (LazyList.map (\(a, b, c) ->
                             let outputVs = [a, b, c] in
                             (outputVs, UpdateUtils.defaultTupleDiffs valToString UpdateUtils.defaultVDiffs vs outputVs)
                             ) ll)
                     _ -> UpdateError "replaceAllIn requires regexp, replacement (fun or string) and the string"
                 RegexReplaceFirstIn -> -- TODO: Move this in maybeUpdateMathOp
                     case vs of
                       [regexpV, replacementV, stringV] ->
                         let eRec env exp = doEval Syntax.Elm env exp |> Result.map (\((v, _), _) -> v) in
                         let uRec: Env -> Exp -> Val -> Val -> Results String (UpdatedEnv, Exp)
                             uRec env exp oldval newval =
                               case UpdateUtils.defaultVDiffs oldval newval of
                                 Err msg -> Errs msg
                                 Ok Nothing -> ok1 (UpdatedEnv.original env, exp)
                                 Ok (Just newvalDiff) -> update (updateContext "recursive update" env exp oldval newval newvalDiff) LazyList.Nil
                         in
                         case UpdateRegex.updateRegexReplaceFirstByIn
                             env eRec uRec regexpV replacementV stringV oldVal newVal of
                           Errs msg -> UpdateError msg
                           Oks ll -> updateOpMultiple "replaceAllIn" env opArgs (\newOpArgs -> replaceE__ e <| EOp sp1 op newOpArgs sp2) vs
                             (LazyList.map (\(a, b, c) ->
                               let outputVs = [a, b, c] in
                               (outputVs, UpdateUtils.defaultTupleDiffs valToString UpdateUtils.defaultVDiffs vs outputVs)
                               ) ll)
                       _ -> UpdateError "replaceAllIn requires regexp, replacement (fun or string) and the string"
                 RegexExtractFirstIn ->
                   case (vs, opArgs) of
                     ([regexpV, stringV], [regexpE, stringE]) ->
                       case UpdateRegex.updateRegexExtractFirstIn regexpV stringV oldVal newVal of
                         Errs msg -> UpdateError msg
                         Oks ll ->
                           let llWithDiffs = LazyList.map (\newStringV -> (newStringV, UpdateUtils.defaultVDiffs stringV newStringV)) ll in
                           updateAlternatives "extractFirstIn" env stringE stringV llWithDiffs <| \newUpdatedEnv newStringE ->
                               updateResult newUpdatedEnv <| replaceE__ e <| EOp sp1 op [regexpE, newStringE] sp2
                     _ -> UpdateError "extractFirstIn requires regexp, replacement (fun or string) and the string"
                 _ ->
                   case maybeUpdateMathOp op vs oldVal newVal of
                     Errs msg -> UpdateError msg
                     Oks ll ->
                       updateOpMultiple "op" env opArgs (\newOpArgs -> replaceE__ e <| EOp sp1 op newOpArgs sp2) vs (LazyList.map (\outputVs ->
                         (outputVs, UpdateUtils.defaultTupleDiffs valToString UpdateUtils.defaultVDiffs vs outputVs)
                         ) ll)

     ECase sp1 input branches sp2 ->
       case doEval Syntax.Elm env input of
         Err msg -> UpdateError msg
         Ok ((inputVal, _), _) ->
           case branchWithInversion env inputVal branches of
             Nothing -> UpdateError <| "Match error: " ++ valToString inputVal ++ " on branches " ++ Syntax.unparser Syntax.Elm e
             Just ((branchEnv, branchExp), envValBranchBuilder) ->
               updateContinue "ECase" branchEnv branchExp oldVal newVal diffs <| \upUpdatedEnv upExp ->
                 let (newBranchUpdatedEnv, newInputVal, newInputValDiffs, nBranches, nBranchesDiffs) = envValBranchBuilder (upUpdatedEnv, upExp) in
                 let input_update = case newInputValDiffs of
                   Nothing -> \continuation -> continuation (UpdatedEnv.original env) input
                   Just m -> updateContinue "ECase 2" env input inputVal newInputVal m
                 in
                 input_update <| \newInputUpdatedEnv newInputExp ->
                   let finalUpdatedEnv = UpdatedEnv.merge env newBranchUpdatedEnv newInputUpdatedEnv in
                   let finalExp = replaceE__ e <| ECase sp1 newInputExp nBranches sp2 in
                   updateResult finalUpdatedEnv finalExp
     --  ETypeCase WS Exp (List TBranch) WS
     ELet sp1 letKind False p sp2 e1 sp3 body sp4 ->
         case doEval Syntax.Elm env e1 of
           Err s       -> UpdateError s
           Ok ((oldE1Val,_), _) ->
             case consWithInversion (p, oldE1Val) (Just (env, (\newUpdatedEnv -> newUpdatedEnv))) of
                Just (envWithE1, consBuilder) ->
                  updateContinue  "ELet"  envWithE1 body oldVal newVal diffs <| \newUpdatedEnvBody newBody ->
                    case consBuilder newUpdatedEnvBody of
                     ((newPat, newPatDiffs, newE1Val, newE1ValDiffs), newUpdatedEnvFromBody) ->
                       let e1_update = case newE1ValDiffs of
                         Nothing -> \continuation -> continuation (UpdatedEnv.original env) e1
                         Just m -> updateContinue "ELet2" env e1 oldE1Val newE1Val m
                       in
                       e1_update <| \newUpdatedEnvFromE1 newE1 ->
                         let finalUpdatedEnv = UpdatedEnv.merge env newUpdatedEnvFromBody newUpdatedEnvFromE1 in
                         let finalExp = replaceE__ e <| ELet sp1 letKind False newPat sp2 newE1 sp3 newBody sp4 in
                         updateResult finalUpdatedEnv finalExp
                Nothing ->
                  UpdateError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
     ELet sp1 letKind True p sp2 e1 sp3 body sp4 ->
         case doEval Syntax.Elm env e1 of
           Err s       -> UpdateError s
           Ok ((oldE1Val,_), _) ->
             case (p.val.p__, oldE1Val.v_) of
               (PVar _ fname _, VClosure Nothing x closureBody env_) ->
                 --let _   = Utils.assert "eval letrec" (env == env_) in
                 let oldE1ValNamed = { oldE1Val | v_ = VClosure (Just fname) x closureBody env } in
                 case consWithInversion (p, oldE1ValNamed) (Just (env, (\newUpdatedEnv -> newUpdatedEnv))) of
                    Just (envWithE1, consBuilder) ->
                      updateContinue "ELetrec"  envWithE1 body oldVal newVal diffs <| \newUpdatedEnvBody newBody ->
                        case consBuilder newUpdatedEnvBody of
                          ((newPat, newPatDiffs, newE1ValNamed, newE1ValNamedDiff), newUpdatedEnvFromBody) ->
                            let e1_update = case newE1ValNamedDiff of
                              Nothing -> \continuation -> continuation (UpdatedEnv.original env) e1
                              Just m ->
                                let newE1Val = case newE1ValNamed.v_ of
                                  VClosure (Just _) x vBody newEnv -> { newE1ValNamed | v_ = VClosure Nothing x vBody newEnv }
                                  _ -> Debug.crash "[internal error] This should have been a recursive method"
                                in
                                updateContinue "ELetrec2" env e1 oldE1Val newE1Val m
                            in e1_update <| \newUpdatedEnvE1 newE1 ->
                              let finalUpdatedEnv = UpdatedEnv.merge env newUpdatedEnvFromBody newUpdatedEnvE1 in
                              let finalExp = replaceE__ e <| ELet sp1 letKind True newPat sp2 newE1 sp3 newBody sp4 in
                              updateResult finalUpdatedEnv finalExp
                    Nothing ->
                      UpdateError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
               (PList _ _ _ _ _, _) ->
                   UpdateError <| strPos e1.start ++
                     """mutually recursive functions (i.e. letrec [...] = [...] e) \
                        not yet implemented""" --"
                      -- Implementation also requires modifications to LangSimplify.simply
                      -- so that clean up doesn't prune the funtions.
               _ ->
                 UpdateError <| strPos e.start ++ " bad letrec"

     EComment sp msg exp ->
       updateContinue "EComment" env exp oldVal newVal diffs <| \nv ne -> updateResult nv <| replaceE__ e <| EComment sp msg ne
     EOption a b c d exp ->
       updateContinue "EOption" env exp oldVal newVal diffs <| \nv ne -> updateResult nv <| replaceE__ e <| EOption a b c d ne
     ETyp a b c exp d    ->
       updateContinue "ETyp" env exp oldVal newVal diffs <| \nv ne -> updateResult nv <| replaceE__ e <| ETyp a b c ne d
     EColonType a exp b c d ->
       updateContinue "EColonType" env exp oldVal newVal diffs <| \nv ne -> updateResult nv <| replaceE__ e <| EColonType a ne b c d
     ETypeAlias a b c exp d ->
       updateContinue "ETypeAlias" env exp oldVal newVal diffs <| \nv ne -> updateResult nv <| replaceE__ e <| ETypeAlias a b c ne d
     EParens sp1 exp pStyle sp2->
       updateContinue "EParens" env exp oldVal newVal diffs <| \nv ne -> updateResult nv <| replaceE__ e <| EParens sp1 ne pStyle sp2
     {--ETypeCase sp1 e1 tbranches sp2 ->
       case eval_ syntax env (e::bt) e1 of
         Err s -> UpdateError s
         Ok (v1,sp1) ->
           case evalTBranches syntax env (e::bt) v1 tbranches of
             -- retVBoth and not addProvenanceToRet b/c only lets should return inner env
             Ok (Just (v2,sp2)) -> UpdateError "Typecase not updatable at this point"--Oks <| retVBoth [v2] (v2, sp1 ++ sp2) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
             UpdateError s              -> UpdateError s
             _                  -> UpdateError "Typecase not updatable at this point" --errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive typecase statement"
     --}
     _ ->
       let outStr = valToString newVal in
       UpdateError <| "Non-supported update " ++ envToString (pruneEnv e env) ++ "|-" ++ unparse e ++ " <-- " ++ outStr ++ " (was " ++ valToString oldVal ++ ")"

-- Errors are converted to empty solutions because updateRec is called once a solution has been found already.
updateRec: UpdateStack -> LazyList NextAction -> LazyList (UpdatedEnv, Exp)
updateRec updateStack nextToUpdate =
  case update updateStack nextToUpdate of
    Oks l -> l
    Errs msg -> LazyList.Nil

addToVClosureEnv: Env -> Val -> Val
addToVClosureEnv env v = case v.v_ of
  VClosure recName pats body oldEnv -> replaceV_ v <| VClosure recName pats body (env ++ oldEnv)
  _ -> v

-- Add more functions in the closure of a function, used for executing lenses
addUpdateCapability: Val -> Val
addUpdateCapability v =
  addToVClosureEnv [
    ("updateApp", replaceV_ v <|
      VFun "updateApp" ["{fun,input[,oldOutput],output[,outputDiff]}"] (\env args ->
        case args of
          [arg] ->
            case arg.v_ of
              VRecord d ->
                case (Dict.get "fun" d, Dict.get "input" d, Dict.get "output" d) of
                  (Just fun, Just input, Just newVal) ->
                    let reverseEnv = ("x", fun)::("y", input)::env in
                    let exp = (withDummyExpInfo <| EApp space0 (withDummyExpInfo <| EVar space0 "x") [withDummyExpInfo <| EVar space1 "y"] SpaceApp space0) in
                    let oldOut = case Dict.get "oldOutput" d of
                      Nothing -> case Dict.get "oldout" d of
                         Nothing -> case Dict.get "outputOld" d of
                           Nothing ->
                             Eval.doEval Syntax.Elm reverseEnv exp |> Result.map (\((v, _), _) -> v)
                           Just v -> Ok v
                         Just v -> Ok v
                      Just v -> Ok v
                    in
                    case oldOut of
                      Err msg -> Err msg
                      Ok oldOut ->
                        let outputDiff = case Dict.get "outputDiff" d of
                          Nothing -> case Dict.get "diffOutput" d of
                             Nothing -> case Dict.get "diffOut" d of
                               Nothing -> case Dict.get "outDiff" d of
                                 Nothing -> UpdateUtils.defaultVDiffs oldOut newVal
                                 Just v -> valToVDiffs v |> Result.map Just
                               Just v -> valToVDiffs v |> Result.map Just
                             Just v -> valToVDiffs v |> Result.map Just
                          Just v -> valToVDiffs v |> Result.map Just
                        in
                        --let _ = Debug.log "calling back update" () in
                        case outputDiff of
                          Err msg -> Err msg
                          Ok Nothing -> -- No need to call update
                            let resultingValue = (Vb.record v) identity (
                                 Dict.fromList [("values", (Vb.list v) identity [input]),
                                                ("diffs", (Vb.list v) identity [] )
                                 ])
                            in
                            Ok ((resultingValue, []), env)
                          Ok (Just newOutDiffs) ->
                            let basicResult = case update (updateContext "updateApp" reverseEnv exp oldOut newVal newOutDiffs) LazyList.Nil of
                              Errs msg -> (Vb.record v) (Vb.string v) (Dict.fromList [("error", msg)])
                              Oks ll ->
                                 let l = LazyList.toList ll in
                                 let lFiltered = List.filter (\(newReverseEnv, newExp) ->
                                   case newReverseEnv.changes of
                                      [] -> True
                                      [(1, _)] -> True
                                      _ -> False) l
                                 in
                                 if List.isEmpty lFiltered then
                                   if List.isEmpty l then
                                     (Vb.record v) identity (Dict.fromList [("values", (Vb.list v) identity []), ("diffs", (Vb.list v) identity [])])
                                   else
                                     (Vb.record v) (Vb.string v) (Dict.fromList [("error", "Only solutions modifying the constant function of updateApp")])
                                 else
                                   let (results, diffs) = lFiltered |> List.map (\(newReverseEnv, newExp) ->
                                     case newReverseEnv.val of
                                        ("x", newFun)::("y",newArg)::newEnv ->
                                          case newReverseEnv.changes of
                                            [] -> (newArg, Nothing)
                                            [(1, diff)] -> (newArg, Just diff)
                                            _ -> Debug.crash "Internal error: expected not much than (1, diff) in environment changes"
                                        _ -> Debug.crash "Internal error: expected x and y in environment"
                                     ) |> List.unzip in
                                   let maybeDiffsVal = diffs |> (Vb.list v) (maybeToVal v (vDiffsToVal v)) in
                                   (Vb.record v) identity (
                                        Dict.fromList [("values", (Vb.list v) identity results),
                                                     ("diffs", maybeDiffsVal )
                                        ])
                            in Ok ((basicResult, []), env)
                  (mbFun, mbInput, mbOutput) ->
                    Err <|
                    "updateApp requires a record with at least {fun,input,output}. Missing" ++
                     (Maybe.map (\_ -> "") mbFun |> Maybe.withDefault " fun") ++
                     (Maybe.map (\_ -> "") mbInput |> Maybe.withDefault " input") ++
                     (Maybe.map (\_ -> "") mbOutput |> Maybe.withDefault " output")
              _ -> Err <| "updateApp's argument should be a record {fun,input[,oldOutput],output[,outputDiff]}, but got " ++ valToString arg
          _ -> Err <| "updateApp expects 1 arguments ({fun,input[,oldOutput],output[,outputDiff]}), but got " ++ toString (List.length args)
      ) Nothing
    ),
    ("merge", replaceV_ v <|
      VFun "merge" ["original", "list_of_modified"] (\env args ->
        case args of
          [original, modifications] ->
            case modifications.v_ of
              VList modifications ->
                let modificationsWithDiffs = List.map (\m -> UpdateUtils.defaultVDiffs original m |> Result.map (\mbmodifs -> mbmodifs |> Maybe.map (\modifs -> (m, modifs)))) modifications in
                case modificationsWithDiffs |> Utils.projOk of
                   Err msg -> Err msg
                   --Ok Nothing -> Ok ((original, []), env)
                   Ok withModifs ->
                    let (newVal, _) = recursiveMergeVal original (List.filterMap identity withModifs) in  -- TODO: To bad, we are forgetting about diffs !
                    Ok ((newVal, []), env)
              _ -> Err  <| "updateApp merge 2 lists, but got " ++ toString (List.length args)
          _ -> Err  <| "updateApp merge 2 lists, but got " ++ toString (List.length args)
      ) Nothing
    ),
    ("diff", replaceV_ v <|
      VFun "diff" ["list_before", "list_after"] (\env args ->
        case args of
          [before, after] ->
            case [before.v_, after.v_] of
              [VList beforeList, VList afterList] ->
                --let _ = Debug.log ("going to do a diff:" ++ valToString before ++ " -> " ++ valToString after) () in
                let thediff = diffVals beforeList afterList in
                {-let _ = Debug.log ("the diff:" ++ ( String.join "," <|
                                                 List.map
                                                 (\d -> case d of
                                                   DiffAdded x -> "DiffAdded " ++ valToString (replaceV_ v<| VList x)
                                                   DiffRemoved x -> "DiffRemoved " ++ valToString (replaceV_ v<| VList x)
                                                   DiffEqual x -> "DiffSame " ++ valToString (replaceV_ v<| VList x)
                                                 ) thediff)) () in
                -}
                let res =  thediff
                     |> List.map (\x ->
                       replaceV_ v <| VRecord <| Dict.fromList <| case x of
                          DiffEqual els   -> [("kept"      , replaceV_ v <| VList els)]
                          DiffAdded els   -> [("inserted"  , replaceV_ v <| VList els)]
                          DiffRemoved els -> [("deleted"   , replaceV_ v <| VList els)]
                       )
                     |> (\x ->
                       Ok ((replaceV_ v <| VList x, []), env)
                     )
                --in let _ = Debug.log "Diff done" ()
                in
                res
              _ -> Err  <| "diff performs the diff on 2 lists, but got " ++ toString (List.length args)
          _ -> Err <|   "diff performs the diff on 2 lists, but got " ++ toString (List.length args)
      ) Nothing
    )
  ] v

interpreterListToList: String -> Val -> Result String (List Val)
interpreterListToList msg v = case v.v_ of
  VList elems -> Ok elems
  _ -> Err <| "Expected a list for " ++ msg ++ ", got " ++ valToString v

interpreterMaybeToMaybe: String -> Val -> Result String (Maybe Val)
interpreterMaybeToMaybe msg v = case v.v_ of
  VList [tag, e] ->
    case tag.v_ of
      VBase (VString "Just") -> Ok (Just e)
      _ -> Err <| "Expected 'Just' or 'Nothing' for " ++ msg ++ ", got " ++ valToString tag
  VList [tag] ->
    case tag.v_ of
      VBase (VString "Nothing") -> Ok Nothing
      _ -> Err <| "Expected 'Just' or 'Nothing' for " ++ msg ++ ", got " ++ valToString tag
  _ -> Err <| "Expected ['Just', x] or ['Nothing'] for " ++ msg ++ ", got " ++ valToString v

-- Compares the new reference with and original (which are at distance at most 2PI) and reports the relative smallest change to n
angleUpdate: Float -> Float -> Float -> Results String (List Num)
angleUpdate new old n =
   let increment = new - old in
  --Two directions are possible, we take the closest first modulo 2 PI
   if increment <= pi && increment >= -pi then
     ok1 [n + increment]
   else if increment > pi then
     ok1 [n + increment - 2*pi]
   else
     ok1 [n + increment + 2*pi]

maybeUpdateMathOp : Op -> List Val -> Val -> Val -> Results String (List Val)
maybeUpdateMathOp op operandVals oldOutVal newOutVal =
  case (oldOutVal.v_, newOutVal.v_) of
    (VBase (VString oldOut), VBase (VString newOut)) ->
      let operandsStr = operandVals
        |> List.map (\operand ->
            case operand.v_ of
              VBase v as vb-> Just vb
              _ -> Nothing
            )
        |> Utils.projJusts in
      case operandsStr of
        Just [VBase (VString sa) as va, VBase (VString sb) as vb] ->
          case op.val  of
            Plus ->
              let result = case newOutVal.v_ of
                VBase (VString s) ->
                   let saIsPrefix = String.startsWith sa s in
                   let sbIsSuffix = String.endsWith sb s in
                   if saIsPrefix && sbIsSuffix then -- Normally, we should check whitespaee to order. For now, append to left first.
                     let newsa = String.slice 0 (String.length s - String.length sb) s in
                     let newsb = String.dropLeft (String.length sa) s in
                     let newva = VBase <| VString <| newsa in
                     let newvb = VBase <| VString <| newsb in
                     if String.length newsa > String.length sa then -- Addition of elements, perhaps we need to stick them to a particular side.
                       let insertion = String.slice (String.length sa) (String.length newsa) newsa in
                       if String.length sa == 0 then --Fill empty string only with spaces by default
                         if LangParserUtils.isOnlySpaces insertion then
                           oks [[newva, vb], [va, newvb]]
                         else
                           oks [[va, newvb], [newva, vb]]
                       else if String.length sb == 0 then
                         if LangParserUtils.isOnlySpaces insertion then
                           oks [[va, newvb], [newva, vb]]
                         else
                           oks [[newva, vb], [va, newvb]]
                       else -- Both original strings had chars
                         let aEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length sa) sa in
                         let bStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 sb in
                         let insertedStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 insertion in
                         let insertedEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length insertion) insertion in
                         -- Stick inserted expression if some end match and not the other, else keep left.
                         if (bStartIsSspace == insertedEndIsSspace) && xor aEndIsSspace insertedStartIsSspace then
                           oks [[va, newvb], [newva, vb]]
                         else
                           oks [[newva, vb], [va, newvb]]
                          --LangParserUtils.isOnlySpaces (String.slice 0 1 insertion) &&
                     else if newva == va then --The strings did not change !!
                       oks [[va, vb]]
                     else
                       oks [[newva, vb], [va, newvb]]
                   else if saIsPrefix then
                     let newvb = VBase <| VString <| String.dropLeft (String.length sa) s in
                     ok1 [va, newvb]
                   else if sbIsSuffix then
                     let newva = VBase <| VString <| String.slice 0 (String.length s - String.length sb) s in
                     ok1 [newva, vb]
                   else -- Some parts on both strings were deleted, everything in between was added.
                     -- First, we look for suffixes of a that appear in the output. They will be prefered as split point
                     -- Second we look for prefixes of b that appear in the output. They will be prefered as split point
                     let positionsWeighted =
                       List.range 0 (String.length s)
                       |> List.foldl (\i acc ->
                          let prefixS = String.slice 0 i s in
                          let suffixS = String.dropLeft i s in
                          let strengthsa = commonSuffix sa prefixS in
                          let strengthsb = commonPrefix sb suffixS in
                          let weight = String.length strengthsa + String.length strengthsb in
                          if weight == 0 then acc else -- remove this condition to consider all possible splits.
                            ((prefixS, suffixS), 0 - weight)::acc
                            ) []
                       |> List.sortBy Tuple.second
                       |> List.map (\((newsa, newsb), _) ->
                           [VBase <| VString <| newsa, VBase <| VString <| newsb]
                       )
                     in
                     if not <| List.isEmpty positionsWeighted then oks positionsWeighted
                     else
                     let newsa = commonPrefix sa s in
                     let newsb = commonSuffix sb s in
                     let newva s = VBase <| VString <| newsa ++ s in
                     let newvb s = VBase <| VString <| s ++ newsb in
                     let insertion = String.slice (String.length newsa) (String.length s - String.length newsb) s in
                     if String.length newsa == 0 then -- Everything inserted belongs to newsa by default
                       oks [[newva insertion, newvb ""], [newva "", newvb insertion]]
                     else if String.length newsb == 0 then --Everything inserted belongs to newvb by default
                       oks [[newva "", newvb insertion], [newva insertion, newvb ""]]
                     else
                         let aEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length newsa) newsa in
                         let bStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 newsb in
                         let insertedStartIsSspace = LangParserUtils.isOnlySpaces <| String.slice 0 1 insertion in
                         let insertedEndIsSspace = LangParserUtils.isOnlySpaces <| String.slice (-1) (String.length insertion) insertion in
                         -- Stick inserted expression if some end match and not the other, else keep left.
                         if (bStartIsSspace == insertedEndIsSspace) && xor aEndIsSspace insertedStartIsSspace then
                           oks [[newva "", newvb insertion], [newva insertion, newvb ""]]
                         else
                           oks [[newva insertion, newvb ""], [newva "", newvb insertion]]
                _ -> Errs <| "Cannot yet update with non-string" ++ valToString newOutVal
              in
              Results.map (
                  List.map2 (\original newString ->
                    replaceV_ original <| newString
                  ) operandVals
                ) result
            o -> Errs <| "This operation is not supported for strings : " ++ toString o
        o -> Errs <| "Expected strings, got " ++ toString o

    (VConst _ (oldOut, _), VConst _ (newOut, _)) ->
      if oldOut == newOut then ok1 operandVals else
      let operands = operandVals
        |> List.map (\operand ->
            case operand.v_ of
              VConst _ (v, _) -> Just v
              _ -> Nothing
            )
        |> Utils.projJusts in
      case operands of
        Nothing -> Errs <| "Operands do not form a list of numbers: " ++ toString operandVals
        Just operands ->
          let result = case (op.val, operands) of
                (Plus,    [l,r]) -> oks [[newOut - r, r], [l, newOut - l]]
                (Minus,   [l,r]) -> oks [[r + newOut, r], [l, l - newOut]]
                (Mult,    [l,r]) -> oks [[l, newOut / l], [newOut / r, r]]
                (Div,     [l,r]) -> if newOut /= 0 then oks [[r * newOut, r], [l, l / newOut]]
                                    else ok1 [r * newOut, r]
                (Pow,     [l,r]) -> if l < 0 && r >= 0 && floor r == ceiling r then --Powers of negative must be with integer numbers
                                      if (floor r) % 2 == 0 then -- The result should be positive
                                        if newOut >= 0 then ok1 [0 - newOut ** (1 / r), r]
                                        else if newOut == -1 && l == -1 then
                                          if r > 0 then oks [[l, r - 1], [l, r + 1]]
                                          else ok1 [l, r + 1]
                                        else  Errs "No way to invert l^r <-- out where l < 0, r is even, out < 0 and out /= -1"
                                      else -- r is odd, so we preserve the negative sign.
                                        if newOut >= 0 then ok1 [newOut ** (1/r), r] else ok1 [0 - ( 0 - newOut) ** (1/r), r]
                                    else if l >= 0 then
                                      if newOut > 0 then oks [[newOut ** (1 / r), r], [l, logBase l newOut]]
                                      else if floor (1 / r) == ceiling (1/r) && 1/r < 0 then oks [[0 - (-newOut) ** (1 / r), r]]
                                      else Errs "No way to invert l^r <-- out where l >= 0, out < 0 and 1/r not an integer or not < 0"
                                    else Errs "No way to invert l^r <-- out where l < 0 and r < 0 or r is not an integer"
                (Mod,     [l,r]) -> ok1 [l + newOut - oldOut, r]
                (ArcTan2, [l,r]) -> -- We keep the same radius but change the angle
                                    let (radius, theta) = toPolar (r, l) in
                                    let (newR, newL) = fromPolar (radius, theta + newOut - oldOut) in
                                    ok1 [newL, newR]
                (Cos,     [n])   -> let newOutClamped = clamp -1 1 newOut in
                                    let moved = acos newOutClamped in -- value between 0 and PI
                                    -- We stay on the same quadrant
                                    let movedAbsolute  = -- value between 0 and 2*PI
                                      if ((n %% (2 * pi) + 3 * pi) %% (2 * pi) - pi >= 0) then moved
                                      else 2*pi - moved in
                                    let original = (n %% (2 * pi) + 2 * pi) %% (2 * pi) in
                                    angleUpdate movedAbsolute original n

                (Sin,     [n])   -> let newOutClamped = clamp -1 1 newOut in
                                    let moved = asin newOutClamped in -- value between -pi / 2 and pi / 2
                                    -- But n might be beyond [-pi/2, pi/2] !
                                    let movedAbsolute = -- value between -PI and PI
                                      if ((n %% (2 * pi) + 2 * pi + pi / 2) %% (2 * pi) - pi <= 0) then moved
                                      else if moved > 0 then pi - moved
                                      else -pi - moved in
                                    let original = (n %% (2 * pi) + 3 * pi) %% (2 * pi) - pi in
                                    angleUpdate movedAbsolute original n
                (ArcCos,  [n])   -> ok1 [cos newOut]
                (ArcSin,  [n])   -> ok1 [sin newOut]
                (Floor,   [n])   -> ok1 [n + newOut - oldOut]
                (Ceil,    [n])   -> ok1 [n + newOut - oldOut]
                (Round,   [n])   -> ok1 [n + newOut - oldOut]
                (Sqrt,    [n])   -> ok1 [newOut * newOut]
                (Pi,      [])    -> if newOut == pi then ok1 [] else Errs <| "Pi's value is 3.14159... and cannot be changed"
                _                -> Errs <| "Not the correct number of arguments for " ++ toString op ++ "(" ++ toString operandVals ++ ")"
          in
          Results.map (
            List.map2 (\original newNumber ->
              case original.v_ of
                VConst m0 (oldNumber, m1) -> replaceV_ original <| VConst m0 (newNumber, m1)
                x -> Debug.crash <| "[internal error] Did not get a VConst: " ++ toString x
            ) operandVals
          ) result
    _ -> Errs <|
           "Do not know how to revert computation "
             ++ toString op ++ "("
             ++ String.join ", " (List.map valToString operandVals)
             ++ ") <-- " ++ valToString newOutVal

commonPrefix: String -> String -> String
commonPrefix =
  let aux: String -> String -> String -> String
      aux  prefix    s1        s2 =
       case (String.uncons s1, String.uncons s2) of
         (Nothing, _) -> prefix
         (_, Nothing) -> prefix
         (Just (s1head, s1tail), Just (s2head, s2tail)) ->
           if s1head == s2head then aux (prefix ++ String.fromChar s1head) s1tail s2tail else prefix
  in aux ""

commonSuffix: String -> String -> String
commonSuffix s1 s2 = commonPrefix (String.reverse s1) (String.reverse s2) |> String.reverse

branchWithInversion: Env -> Val -> List Branch -> Maybe ((Env, Exp), (UpdatedEnv, Exp) -> (UpdatedEnv, Val, Maybe VDiffs, List Branch, TupleDiffs BranchDiffs))
branchWithInversion env input branches =
  case branches of
    [] -> Nothing
    head::tail ->
      case head.val of
        Branch_ sp1 pat exp sp2 ->
          case consWithInversion (pat, input) (Just (env, \newUpdatedEnv -> newUpdatedEnv)) of
            Nothing ->
              branchWithInversion env input tail |>
              Maybe.map (\((augEnv, exp), patValEnvRebuilder) ->
                ((augEnv, exp),
                (\(newUpdatedEnv, newExp) ->
                  let (updatedUpdatedEnv, updatedVal, vdiff, newTailBranches, branchdiffs) = patValEnvRebuilder (newUpdatedEnv, newExp) in
                  (updatedUpdatedEnv, updatedVal, vdiff, head::newTailBranches, UpdateUtils.offset 1 branchdiffs)
                  ))
              )
            Just (augEnv, patValEnvRebuilder) ->
              Just ((augEnv, exp), \(newAugUpdatedEnv, newExp) ->
                let ((newPat, newPatDiff, updatedVal, updatedValDiff), newUpdatedEnv) = patValEnvRebuilder newAugUpdatedEnv in
                let newBranch = replaceB__ head <| Branch_ sp1 newPat newExp sp2 in

                (newUpdatedEnv, updatedVal, updatedValDiff, newBranch :: tail, [(0, BChanged)])
              )

consWithInversion : (Pat, Val) -> Maybe (Env, UpdatedEnv -> a) -> Maybe (Env, UpdatedEnv -> ((Pat, Maybe PDiffs, Val, Maybe VDiffs), a))
consWithInversion pv menv =
  case (matchWithInversion pv, menv) of
    (Just (env_, envToPatVal), Just (env, envToA)) -> Just (env_ ++ env,
      \newUpdatedEnv ->
        let (newUpdatedEnv_, newUpdatedEnvTail) = UpdatedEnv.split (List.length env_) newUpdatedEnv in
        let newpv = if UpdatedEnv.isUnmodified newUpdatedEnv_ then (Tuple.first pv, Nothing, Tuple.second pv, Nothing)
             else envToPatVal newUpdatedEnv_ in
        (newpv, envToA newUpdatedEnvTail)
      )
    _                     -> Nothing


conssWithInversion : (List Pat, List Val) -> Maybe (Env, UpdatedEnv -> a) -> Maybe (Env, UpdatedEnv -> ((List Pat, TupleDiffs PDiffs, List Val, TupleDiffs VDiffs), a))
conssWithInversion pvs menv =
  case (menv, matchListWithInversion pvs) of
    (Just (env, envToA), Just (env_, envToPatsVals)) -> Just (env_ ++ env,
      \newUpdatedEnv ->
        let (newUpdatedEnv_, newUpdatedEnvTail) = UpdatedEnv.split (List.length env_) newUpdatedEnv in
        let newpatsvals = if UpdatedEnv.isUnmodified newUpdatedEnv then (Tuple.first pvs, [], Tuple.second pvs, []) else envToPatsVals newUpdatedEnv_ in
        (newpatsvals, envToA newUpdatedEnvTail)
      )
    _                     -> Nothing

-- Given a pattern and a value, maybe returns an environment where the variables of the pattern match sub-values
-- The second element takes a new environment and modifications to it, and returns the new pattern and values
matchWithInversion : (Pat, Val) -> Maybe (Env, UpdatedEnv -> (Pat, Maybe PDiffs, Val, Maybe VDiffs))
matchWithInversion (p,v) = case (p.val.p__, v.v_) of
  (PWildcard _, _) -> Just ([], \newUpdatedEnv ->
     case newUpdatedEnv.val of
       [] -> (p, Nothing, v, Nothing)
       _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ envToString newUpdatedEnv.val ++ " should have length 0"
     )
  (PVar ws x wd, _) -> Just ([(x,v)], \newUpdatedEnv ->
     case (newUpdatedEnv.val, newUpdatedEnv.changes) of
       ([(x, newV)], [(0, diffs)]) -> (p, Nothing, newV, Just diffs)
       (_, []) -> (p, Nothing, v, Nothing)
       _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ envToString newUpdatedEnv.val ++ " should have length 1"
     )
  (PAs sp0 x sp1 innerPat, _) ->
    matchWithInversion (innerPat, v) |> Maybe.map
      (\(env, updatedEnvReverse) -> ((x,v)::env, \newUpdatedEnv ->
        if UpdatedEnv.isUnmodified newUpdatedEnv then (p, Nothing, v, Nothing) else
        let (newUpdatedEnvX, newUpdatedEnvInner) = UpdatedEnv.split 1 newUpdatedEnv in
        if UpdatedEnv.isUnmodified newUpdatedEnvX then -- Then the other environment was modified
         case updatedEnvReverse newUpdatedEnvInner of
            (newInnerPat, mbModifPat, newVal, mbModifVal) ->
              case mbModifPat of
                Nothing -> (p, Nothing, newVal, mbModifVal)
                pmodif   -> (replaceP__ p <| PAs sp0 x sp1 newInnerPat, pmodif, newVal, mbModifVal)
        else -- newV is modified
          case (newUpdatedEnvX.val, newUpdatedEnvX.changes) of
          ([(_, newV)], [(0, mbModifVal)]) ->
              if UpdatedEnv.isUnmodified newUpdatedEnvInner then
                (p, Nothing, newV, Just mbModifVal)
              else
                case updatedEnvReverse newUpdatedEnvInner of
                  (newInnerPat, mbModifPat, newV2, mbModifVal2) ->
                    let (newVal, newMbModifVal) = mergeValMaybe v newV (Just mbModifVal) newV2 mbModifVal2 in
                    case mbModifPat of
                      Nothing -> (p, Nothing, newVal, newMbModifVal)
                      pmodif -> (replaceP__ p <| PAs sp0 x sp1 newInnerPat, pmodif, newVal, newMbModifVal)
          _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ envToString newUpdatedEnv.val ++ " should have length >= 1"
      ))

  (PList sp0 ps sp1 Nothing sp2, VList vs) ->
    if List.length ps /= List.length vs then Nothing else
    (ps,vs)
    |> matchListWithInversion
    |> Maybe.map (\(env, updatedEnvRenewer) ->
      (env, \newUpdatedEnv ->
        if UpdatedEnv.isUnmodified newUpdatedEnv then
          (p, Nothing, v, Nothing)
        else
          let (newPats, newPatsDiffs, newVals, newValsDiffs) = updatedEnvRenewer newUpdatedEnv in
          let (newPat, newPatDiff) = case newPatsDiffs of
            [] -> (p, Nothing)
            _ -> (replaceP__ p <| PList sp0 newPats sp1 Nothing sp2, Just PChanged)
          in
          let (newVal, newValDiff) = case newValsDiffs of
               []-> (v, Nothing)
               _ -> (replaceV_ v <| VList newVals, Just <| VListDiffs <| List.map (\(i, m) -> (i, VListElemUpdate m)) <| newValsDiffs)
          in
          (newPat, newPatDiff, newVal, newValDiff)))
  (PList sp0 ps sp1 (Just rest) sp2, VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if n > m then Nothing
    else
      let (vs1,vs2) = Utils.split n vs in
      (ps, vs1)
      |> matchListWithInversion
      |> consWithInversion (rest, replaceV_ v <| VList vs2) -- Maybe (Env, UpdatedEnv -> ((Pat, Maybe PDiffs, Val, Maybe VDiffs), a))
      |> Maybe.map (\(env, envRenewer) ->
        (env, (\newUpdatedEnv ->
          if UpdatedEnv.isUnmodified newUpdatedEnv then (p, Nothing, v, Nothing) else
          let ((newTailPat, mbTailPalDiffs, newTailVal, mbTailValDiffs),
               (newPats,    mbPatsDiffs,    newVals,    mbValsDiffs)) = envRenewer newUpdatedEnv in
          let (finalPat, finalPatDiffs) = case (mbTailPalDiffs, mbPatsDiffs) of
            (Nothing, []) -> (p, Nothing)
            _ -> (replaceP__ p <| PList sp0 newPats  sp1 (Just newTailPat) sp2, Just PChanged)
          in
          let (finalVal, finalValDiffs) = case (mbValsDiffs, newTailVal.v_, mbTailValDiffs) of
            ([], _, Nothing) -> (v, Nothing)
            (_, VList tailVals, _) -> (replaceV_ v <| (VList <| newVals ++ tailVals),
                                      Just <| VListDiffs <|
                                       (List.map (\(i, d) -> (i, VListElemUpdate d)) mbValsDiffs) ++ (case mbTailValDiffs of
                                         Nothing -> []
                                         Just (VListDiffs diffs) ->
                                           UpdateUtils.offset (List.length ps) diffs
                                         Just x -> Debug.crash <| "Expected VListDiffs, got " ++ toString x
                                           ))
            _ -> Debug.crash <| "RHS of list pattern is not a list: " ++ valToString newTailVal
          in
          (finalPat, finalPatDiffs, finalVal, finalValDiffs)
        ))
      )
        -- dummy VTrace, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst _ (n_,_)) -> if n == n_ then Just ([], \newEnv -> (p, Nothing, v, Nothing)) else Nothing
  (PConst _ n, _) -> Nothing
  (PBase _ bv, VBase bv_) -> if eBaseToVBase bv == bv_ then Just ([], \newEnv -> (p, Nothing, v, Nothing)) else Nothing
  (PBase _ n, _) -> Nothing
  (PParens sp0 innerPat sp1, _) ->
    matchWithInversion (innerPat, v)
    |> Maybe.map
      (\(env, envReverse) -> (env, \newUpdatedEnv ->
        if UpdatedEnv.isUnmodified newUpdatedEnv then (p, Nothing, v, Nothing) else
        case envReverse newUpdatedEnv of
          (newInnerPat, newInnerPatDiffs, newVal, newValDiffs) -> (replaceP__ p <| PParens sp0 newInnerPat sp1, newInnerPatDiffs, newVal, newValDiffs)
      ))
  (PRecord sp0 pd sp1, VRecord d) ->
      pd |> List.map (\(_, _, k, _, p) ->
        Dict.get k d |> Maybe.map (\v -> (p, v)))
      |> Utils.projJusts
      |> Maybe.andThen (matchListWithInversion << List.unzip)
      |> Maybe.map (\(env, envRenewer) ->
         (env, \newUpdatedEnv ->
           if UpdatedEnv.isUnmodified newUpdatedEnv then (p, Nothing, v, Nothing) else
           let (newPats, newPatsDiffs, newVals, newValsDiffs) = envRenewer newUpdatedEnv in
           let (newPat, newPatDiff) = case newPatsDiffs of
             [] -> (p, Nothing)
             _ -> (replaceP__ p <| PRecord sp0 (Utils.recordValuesMake pd newPats) sp1, Just PChanged)
           in
           let (newVal, newValDiff) = case newValsDiffs of
             [] -> (v, Nothing)
             _ -> (replaceV_ v <| VRecord (Utils.zip pd newVals |>
                                  List.foldl (\(pe, newv) dTemp->
                                    let k = Utils.recordKey pe in
                                    Dict.insert k newv dTemp
                                  ) d),
                                  Just <| VRecordDiffs (newValsDiffs |>
                                  List.foldl (\(i, newvdiff) (dTemp, remainingPds, currentIndex) ->
                                    let (discardedPts, nextPts) = Utils.split (i - currentIndex) remainingPds in
                                    case nextPts of
                                      (_, _, k, _, _)::_ ->
                                        (Dict.insert k newvdiff dTemp, nextPts, i)
                                      _ -> Debug.crash <| "Expected modification at index " ++ toString i ++ " but the list of patterns is " ++ Syntax.patternUnparser Syntax.Elm p
                                  ) (Dict.empty, pd, 0) |> \(d, _, _) -> d))
           in
           (newPat, newPatDiff, newVal, newValDiff)
         )
      )
  (PRecord _ _ _, _) -> Nothing

matchListWithInversion : (List Pat, List Val) -> Maybe (Env, UpdatedEnv -> (List Pat, TupleDiffs PDiffs, List Val, TupleDiffs VDiffs))
matchListWithInversion (ps, vs) =
  let l = List.length ps in
  let inverse_index i = l - 1 - i in
  List.foldl (\pv acc -> --: Maybe (Env, List (Env -> (Pat, Val, Env)))
    case (acc, matchWithInversion pv) of
      (Just (old, oldEnvBuilders), Just (new, newEnvBuilder)) -> Just (new ++ old,
           (\newUpdatedEnv ->
            let (headNewUpdatedEnv, tailModifiedNewEnv) = UpdatedEnv.split (List.length new) newUpdatedEnv in
            if UpdatedEnv.isUnmodified headNewUpdatedEnv then
              (Tuple.first pv, Nothing, Tuple.second pv, Nothing, tailModifiedNewEnv)
            else
              let (newPat, newPatDiff, newVal, newValDiff) = newEnvBuilder headNewUpdatedEnv in
              (newPat, newPatDiff, newVal, newValDiff, tailModifiedNewEnv)
          )::oldEnvBuilders
        )
      _                    -> Nothing
  ) (Just ([], [])) (Utils.zip ps vs)
  |> Maybe.map (\(finalEnv, envBuilders) -> -- envBuilders: List (Env -> (Pat, Val, Env)), but we want Env -> (Pat, Val), combining pattern/values into lists
    (finalEnv, \newUpdatedEnv ->
      if UpdatedEnv.isUnmodified newUpdatedEnv then (ps, [], vs, []) else
      let (newPats, newPatsDiffs, newVals, newValsDiffs, _) =
        List.foldl (\(eToPVE, inversed_i) (pats, patsDiffs, vals, valsDiffs, env)->
           let i = inverse_index inversed_i in
           let (p, pDiff, v, vDiff, e) = eToPVE env in
           let newPatsDiffs = case pDiff of
             Nothing -> patsDiffs
             Just m -> (i, m)::patsDiffs
           in
           let newValsDiffs = case vDiff of
             Nothing -> valsDiffs
             Just m -> (i, m)::valsDiffs
           in
           (p::pats, newPatsDiffs, v::vals, newValsDiffs, e)
           )  ([], [], [], [], newUpdatedEnv) (Utils.zipWithIndex envBuilders) in
      (newPats, newPatsDiffs, newVals, newValsDiffs)
    ))

getNum: Val -> Result String Num
getNum v =
  case v.v_ of
    VConst _ (num, _) -> Ok num
    _ -> Err <| "Cannot replace a number with " ++ valToString v


eBaseToVBase eBaseVal =
  case eBaseVal of
    EBool b     -> VBool b
    EString _ b -> VString b
    ENull       -> VNull

removeCommonPrefix l1 l2 =
  case (l1, l2) of
    ([], _) -> (l1, l2)
    (_, []) -> (l1, l2)
    (head::tail, head2::tail2) ->
      if head == head2 then
        removeCommonPrefix tail tail2
      else (l1, l2)

val : Val_ -> Val
val v_ = Val v_ (Provenance [] (eVar "DUMMYEXP") []) (Parents [])

-- vTrue    = vBool True
-- vFalse   = vBool False
-- vBool    = val << VBase << VBool
vStr     = val << VBase << VString
vConst   = val << VConst Nothing
-- vBase    = val << VBase
vList    = val << VList
-- vDict    = val << VDict


--------------------------------------------------------------------------------
-- Updated value from changes through text-based value editor

buildUpdatedValueFromEditorString : Syntax -> String -> Result String Val
buildUpdatedValueFromEditorString syntax valueEditorString =
  valueEditorString
    |> Syntax.parser syntax
    |> Result.mapError (\e -> ParserUtils.showError e)
    |> Result.andThen (Eval.doEval syntax [])
    |> Result.map (\((v, _), _) -> v)


--------------------------------------------------------------------------------
-- Updated value from changes through DOM editor

buildUpdatedValueFromDomListener
   : RootedIndexedTree
  -> (Dict (Int, String) String, Dict Int String)
  -> Val
buildUpdatedValueFromDomListener (rootId, oldTree) (attributeValueUpdates, textValueUpdates) =
  let
    oldValue : Val
    oldValue =
      oldTree
        |> Utils.justGet rootId
        |> .val

    newValue : Val
    newValue =
      updateNode rootId

    updateNode : NodeId -> Val
    updateNode thisId =
      let thisNode = Utils.justGet thisId oldTree in
      case (thisNode.interpreted, Dict.get thisId textValueUpdates) of
        (TextNode s, _) ->
          vList [vStr "TEXT", vStr s]

        (SvgNode kind attrs [_], Just newText) ->
          let newChild = vList [vStr "TEXT", vStr newText] in
          updateSvgNode thisId kind attrs [newChild]

        (SvgNode kind attrs childIndices, _) ->
          let newChildren = List.map updateNode childIndices in
          updateSvgNode thisId kind attrs newChildren

    updateSvgNode : NodeId -> ShapeKind -> List Attr -> List Val -> Val
    updateSvgNode thisId kind attrs children =
      let
        updatedAttributeNames =
          attributeValueUpdates
            |> Dict.foldl (\(i,s) _ acc -> if i == thisId then s::acc else acc) []

        existingAttributeNames =
          List.map Tuple.first attrs

        newAttributeNames =
          Utils.diffAsSet updatedAttributeNames existingAttributeNames
            |> Utils.removeAsSet "contenteditable"

        updatedOrRemovedAttributes =
          Utils.filterJusts (List.map (updateAttr thisId) attrs)

        newAttributes =
          List.map (\k ->
            vList [vStr k, vStr (Utils.justGet (thisId, k) attributeValueUpdates)]
          ) newAttributeNames

        finalAttributes =
          updatedOrRemovedAttributes ++ newAttributes
      in
      let _ =
        if newAttributes == [] then ()
        else
           let _ = Debug.log "new attributes" (thisId, List.map strVal newAttributes) in
           ()
      in
      vList [ vStr kind, vList finalAttributes, vList children ]

    updateAttr : NodeId -> LangSvg.Attr -> Maybe Val
    updateAttr thisId (attrName, attrAVal) =
      case Dict.get (thisId, attrName) attributeValueUpdates of
        Nothing ->
          Just (vList [vStr attrName, attrAVal.val])

        Just "NULL" ->
          -- let _ = Debug.log "attribute removed" attrName in
          Nothing

        Just newAttrValString ->
          -- let _ = Debug.log "need to update" (thisId, attrName, newAttrValString) in
          case (attrName, attrAVal.interpreted) of
            ("style", AStyle list) ->
              let
                updatedStyleAttributes =
                  newAttrValString
                    |> Regex.replace All (regex "/\\*.*\\*/") (always "") -- e.g. /* color: blue */
                    |> String.split ";"
                    |> List.map (String.split ":")
                    |> List.concatMap (\list ->
                         case list of
                           []      -> []
                           [""]    -> []
                           [s1,s2] -> [(s1,s2)]
                           _       -> let _ = Debug.log "WARN: updateAttr style" list in
                                      []
                       )
                    |> List.map (Utils.mapBoth String.trim)

                updatedStyleAttributeNames =
                  List.map Tuple.first updatedStyleAttributes

                existingStyleAttributeNames =
                  List.map Tuple.first list

                newStyleAttributeNames =
                  Utils.diffAsSet updatedStyleAttributeNames existingStyleAttributeNames

                updatedOrRemovedStyleAttributes =
                  updateStyleAttrs list updatedStyleAttributes

                newStyleAttributes =
                  List.map (\k ->
                    vList [vStr k, vStr (Utils.find_ updatedStyleAttributes k)]
                  ) newStyleAttributeNames

                finalStyleAttributes =
                  updatedOrRemovedStyleAttributes ++ newStyleAttributes
              in
              Just (vList [vStr attrName, vList finalStyleAttributes])

            ("style", _) ->
              let _ = Debug.log "WARN: updateAttr: bad style" (valToString attrAVal.val) in
              Just (vList [vStr attrName, attrAVal.val])

            _ ->
              Just (vList [vStr attrName, updateAttrVal attrAVal newAttrValString])

    updateStyleAttrs : List (String, AVal) -> List (String, String) -> List Val
    updateStyleAttrs oldList newList =
      let updateStyleAttr (k, attrAVal) =
        case Utils.maybeFind k newList of
          Just newAttrValString  ->
            Just (vList [vStr k, updateAttrVal attrAVal newAttrValString])
          Nothing ->
            -- let _ = Debug.log "style attribute removed" k in
            Nothing
      in
      Utils.filterJusts (List.map updateStyleAttr oldList)

    updateAttrVal : AVal -> String -> Val
    updateAttrVal oldAttrAVal newAttrValString =
      let translationFailed () =
        let _ =
          Debug.log "WARN: updateAttrVal failed" (valToString oldAttrAVal.val, newAttrValString)
        in
        oldAttrAVal.val
      in
      case oldAttrAVal.interpreted of
        AString _ ->
          vStr newAttrValString
        ANum _ ->
          String.toFloat newAttrValString
            |> Result.map (\n -> vConst (n, dummyTrace))
            |> Result.withDefault (translationFailed ())
        -- TODO: add more cases
        -- TODO: can update support change in representation type?
        _ ->
          translationFailed ()
  in
  let _ = Debug.log "old value" (valToString oldValue) in
  let _ = Debug.log "new value" (valToString newValue) in
  let _ = Debug.log "values equal" (valEqual oldValue newValue) in
  newValue
