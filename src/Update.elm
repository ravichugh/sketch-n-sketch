module Update exposing
  ( buildUpdatedValueFromEditorString
  , buildUpdatedValueFromHtmlString
  , update
  , vStr
  , vList
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
  ( Results
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
import ExpUnbuilder as Eu
import ImpureGoodies
import HTMLValParser
import HTMLParser
import Array
import Char
import LangParserUtils exposing (isSpace)

unparse = Syntax.unparser Syntax.Elm
unparsePattern = Syntax.patternUnparser Syntax.Elm

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
update : LazyList HandlePreviousResult -> LazyList Fork -> UpdateStack -> Results String (UpdatedEnv, UpdatedExp)
update callbacks forks updateStack =
  --let _ = Debug.log ("\nUpdateStack "++updateStackName_ "  " updateStack) () in
  --let _ = Debug.log ("NextToUpdate" ++ (String.join "" <| List.map (nextActionsToString_ "  ") <| Results.toList callbacks)) () in
  -- At the end of callbacks, there are all the forks that can be explored later.
  case updateStack of -- callbacks to (maybe) push to the stack.
    UpdateContextS env e prevLets oldVal newOut diffs mb ->
      {--
      let _ = Debug.log (String.concat ["update: " , unparse e, " <-- ", vDiffsToString oldVal newOut diffs]) () in
      let _ = case oldVal.v_ of
        VClosure _ _ _ _ -> ()
        _ -> Debug.log (String.concat ["(old value)" , valToString oldVal, " <-- ", valToString newOut]) () in
      let _ = Debug.log ("Previous lets are " ++ envToString prevLets) () in
      --}
      getUpdateStackOp env e prevLets oldVal newOut diffs |>
      update (LazyList.maybeCons mb callbacks) forks

    UpdateResultS fUpdatedEnv fOut mb -> -- Let's consume the stack !
       {--
      let _ = Debug.log (String.concat [
        "update final result: ", unparse fOut.val,
        {-" -- env = " , UpdatedEnv.show fUpdatedEnv-} ", modifs=", if fUpdatedEnv.changes == [] then "\nenvironment unchanged" else envDiffsToString fUpdatedEnv.val fUpdatedEnv.val fUpdatedEnv.changes,
        ",\nExpModifs=" ++ toString fOut.changes]) () in
       --}
      case (LazyList.maybeCons mb callbacks) of -- Let's consume the stack !
        LazyList.Nil ->
          case forks of
            LazyList.Nil ->
              ok1 <| (fUpdatedEnv, fOut)
            LazyList.Cons (Fork msg newUpdateStack callbacks2 forks2) lazyForkTail ->
              okLazy (fUpdatedEnv, fOut) <| (\lft m nus cb2 fk2 -> \() ->
                --let _ = Debug.log ("Exploring other updates: '" ++ m ++ "'") () in
                updateRec cb2 (Lazy.force lft) nus) lazyForkTail msg newUpdateStack callbacks2 forks2
        LazyList.Cons (HandlePreviousResult msg f) lazyTail ->
          update (Lazy.force lazyTail) forks <| f fUpdatedEnv fOut

    UpdateResultAlternative msg updateStack maybeNext ->
      {--
      let _ = Debug.log (String.concat ["update result alternative ", msg, ": "]) () in
      --}
      update callbacks (
        case Lazy.force maybeNext of
          Nothing -> forks
          Just alternativeUpdateStack ->
             LazyList.append (LazyList.fromList [Fork msg alternativeUpdateStack callbacks LazyList.Nil]) forks
        ) updateStack

    UpdateFails msg -> -- Let's explore other solutions. If there are none, return this error.
      case forks of -- Let's consume the stack !
        LazyList.Nil ->
          Err msg
        LazyList.Cons (Fork msgFork newUpdateStack callbacks2 forks2) lazyTail ->
          --let _ = Debug.log ("Got a non-critical error (" ++ msg ++ ") but there are some forks available... testing the next one: " ++ msgFork) () in
          update callbacks2 (LazyList.appendLazy forks2 lazyTail) newUpdateStack

    UpdateCriticalError msg -> -- Immediately stops without looking for new solutions
      Err msg

getUpdateStackOp : Env -> Exp -> PrevLets -> PrevOutput -> Output -> VDiffs -> UpdateStack
getUpdateStackOp env e prevLets oldVal newVal diffs =
   case e.val.e__ of
     EHole ws _ -> updateResultSameEnv env <| valToExp ws (IndentSpace "") newVal

     EConst ws num loc widget ->
       case getNum newVal of
         Err msg ->
           UpdateCriticalError msg
         Ok numv ->
          updateResultSameEnv env <| replaceE__ e <| EConst ws numv loc widget

     EBase ws m ->
       case m of
          EString quoteChar chars ->
            case newVal.v_ of
              VBase (VString newChars) ->
                case diffs of
                  VStringDiffs l ->
                    updateResultSameEnvDiffs env  (replaceE__ e <| EBase ws (EString quoteChar newChars)) (EStringDiffs l)
                  VUnoptimizedDiffs ->
                    updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                       (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                          case vdiffs of
                            VStringDiffs l ->
                               updateResultSameEnvDiffs env  (replaceE__ e <| EBase ws (EString quoteChar newChars)) (EStringDiffs l)
                            _ -> UpdateCriticalError <| "Expected VStringDiffs 1, got " ++ toString vdiffs
                  _ -> UpdateCriticalError <| "Expected VStringDiffs 2, got " ++ toString diffs
              _ -> updateResultSameEnv env <| valToExp ws (IndentSpace "") newVal
          _ -> updateResultSameEnv env <| valToExp ws (IndentSpace "") newVal

     EFun sp0 ps eBody sp1 ->
       case newVal.v_ of
         VClosure Nothing newPs newE newEnv ->
           case diffs of
             VClosureDiffs envModifs mbBodyModif -> -- Whatever the body, modified or not, we take it again.
               -- newPs == ps normally, there is no way we can change patterns.
               let updatedE = case mbBodyModif of
                 Nothing -> UpdatedExp e Nothing
                 Just bodyModif -> UpdatedExp (replaceE__ e <| EFun sp0 newPs newE sp1) (UpdateUtils.wrap 0 mbBodyModif)
               in
               updateResult (UpdatedEnv newEnv envModifs) updatedE
             VUnoptimizedDiffs ->
               updateResult (UpdatedEnv newEnv [(0, VUnoptimizedDiffs)]) (UpdatedExp e (Just (EConstDiffs EAnyDiffs)))
             k -> UpdateCriticalError <| "[internal error] Unexpected modifications to a function: " ++ toString k
         _ -> UpdateCriticalError <| "[internal error] Trying to update a function with non-closure " ++ valToString newVal

     EVar sp is ->
       let newUpdatedEnv = updateEnv env is newVal diffs in
       updateResult newUpdatedEnv (UpdatedExp e Nothing)

     EList sp1 elems sp2 Nothing sp3 ->
       case (oldVal.v_, newVal.v_) of
         (VList origVals, VList newOutVals) ->

           updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
             (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
           case vdiffs of
             VListDiffs vldiffs ->
               let updateDiffs: Int -> UpdatedEnv ->       List (WS, Exp) -> ListDiffs EDiffs -> List (WS, Exp) -> Maybe (WS -> Exp -> (WS, Exp)) ->  List Val ->    List Val -> (List (Int, ListElemDiff VDiffs)) -> UpdateStack
                   updateDiffs  i      collectedUpdatedEnv revElems          revEDiffs           elemsToCollect    changeWhitespaceNext               originalValues newValues   ldiffs =
                    case ldiffs of
                     [] ->
                       let finalElemsToCollect = case changeWhitespaceNext of
                         Nothing -> elemsToCollect
                         Just f -> case elemsToCollect of
                            (ws1, e1)::t -> f ws1 e1 :: t
                            [] -> []
                       in
                       let finalElems = List.reverse <| Utils.reverseInsert finalElemsToCollect revElems in
                       let updatedE= case List.reverse revEDiffs of
                         [] -> UpdatedExp e Nothing
                         l -> UpdatedExp (replaceE__ e <| EList sp1 finalElems sp2 Nothing sp3) (Just <| EListDiffs l)
                       in
                       updateResult collectedUpdatedEnv updatedE
                     (i1, modif)::modiftail ->
                       if i == i1 then
                         case modif of
                           ListElemDelete count ->
                             updateDiffs (i + count) collectedUpdatedEnv revElems ((i, ListElemDelete count)::revEDiffs) (List.drop count elemsToCollect) changeWhitespaceNext (List.drop count originalValues) newValues modiftail

                           ListElemUpdate newModifs ->
                             case (elemsToCollect, originalValues, newValues) of
                               ((sp, hdElem)::tlToCollect, origValue::origTail, newValue::newValuesTail) ->
                                 updateContinue "List" env hdElem [] origValue newValue newModifs  <|
                                   (\sp i revElems tlToCollect origTail newValuesTail  newUpdatedEnv newRawElem ->
                                   let finalEnv = UpdatedEnv.merge e diffs env collectedUpdatedEnv newUpdatedEnv in
                                   let newRevEDiffs = case newRawElem.changes of
                                     Nothing -> case changeWhitespaceNext of
                                         Nothing -> revEDiffs
                                         Just _ -> (i1, ListElemUpdate <| EConstDiffs EOnlyWhitespaceDiffs)::revEDiffs
                                     Just d -> (i1, ListElemUpdate d)::revEDiffs
                                   in
                                   let newSpRawElem = Maybe.map (\f -> f sp newRawElem.val) changeWhitespaceNext |> Maybe.withDefault (sp, newRawElem.val) in
                                   updateDiffs (i + 1) finalEnv (newSpRawElem::revElems) newRevEDiffs tlToCollect Nothing origTail newValuesTail modiftail
                                  ) sp i revElems tlToCollect origTail newValuesTail
                               _ -> UpdateCriticalError <| "[internal error] Unexpected missing elements to update from:\n" ++
                                 "diffs = " ++ toString diffs ++
                                 "\nelems = " ++ (List.map (\(ws, ex) -> ws.val ++ Syntax.unparser Syntax.Elm ex) elems |> String.join ",") ++
                                 "\noriginalValues = " ++ (List.map valToString origVals |> String.join ",") ++
                                 "\nnewValues = " ++  (List.map valToString newOutVals |> String.join ",")

                           ListElemInsert count ->
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
                                           (policy, policy, Nothing)
                                        _   -> Debug.crash <| "[internal error] There should be an element in this list's position"
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
                                          (policy, policy, Nothing)
                                        _ ->  Debug.crash <| "[internal error] There should be an element in this list's position"
                                  else --if insertionIndex == 0 then -- Inserting the first element is always trickier
                                    case elems of
                                      [] ->
                                        if e.start.line == e.end.line then
                                          ( (ws "", valToExpFull Nothing (ws "") InlineSpace)
                                          , (ws " ", valToExpFull Nothing (ws " ") InlineSpace)
                                          , Nothing
                                          )
                                        else -- By default, multi-line lists will use the syntax [ elem1\n, elem2\n ...]
                                          let indentationSquareBracket = String.repeat (e.end.col - 2) " " in
                                          let indentation = indentationSquareBracket ++ "  " in
                                          ( (ws "", valToExpFull Nothing (ws " ") (IndentSpace indentation))
                                          , (ws <| "\n" ++ indentationSquareBracket, valToExpFull Nothing (ws " ") (IndentSpace indentation))
                                          , Nothing
                                          )
                                      (_, head)::tail ->
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
                                        , Just <| \nextWsBeforeComma nextElem -> (wsSecondBeforeComma, Lang.replacePrecedingWhitespace wsSecondBeforeValue nextElem)
                                        )
                                        -- We need to copy the whitespace of second to head.
                             in
                             let insertedExp = List.indexedMap (\index inserted ->
                                  ( (if index + insertionIndex == 0 then wsBeforeCommaHead else wsBeforeCommaTail)
                                  , (if index + insertionIndex == 0 then valToWSExpHead    else valToWSExpTail) inserted) ) inserted
                             in
                             let elemsToAdd = insertedExp in
                             updateDiffs i collectedUpdatedEnv (Utils.reverseInsert elemsToAdd revElems) ((i, ListElemInsert count)::revEDiffs) elemsToCollect changeElementAfterInsert originalValues remainingNewVals modiftail
                       else --((i, ListElemDelete count)::revEDiffs)
                         case changeWhitespaceNext of
                           Nothing ->
                             let count = i1 - i in
                             let (skipped, remaining) = Utils.split count elemsToCollect in
                             updateDiffs i1 collectedUpdatedEnv (Utils.reverseInsert skipped revElems) revEDiffs remaining Nothing (List.drop count originalValues) (List.drop count newValues) ldiffs
                           Just f ->
                             case (elemsToCollect, originalValues, newValues) of
                               ((sp, hdElem)::tlToCollect, origValue::origTail, newValue::newValuesTail) ->
                                 updateDiffs (i+1) collectedUpdatedEnv (f sp hdElem :: revElems) ((i, ListElemUpdate (EConstDiffs EOnlyWhitespaceDiffs))::revEDiffs) tlToCollect Nothing origTail newValuesTail ldiffs
                               _ -> UpdateCriticalError <| "[internal error] Unexpected missing elements to update from (whitespace only):\n" ++
                                                                  "ldiffs = " ++ toString vldiffs ++
                                                                  "\nelems = [" ++ (List.map (\(ws, ex) -> ws.val ++ Syntax.unparser Syntax.Elm ex) elems |> String.join ",") ++
                                                                  "]\noriginalValues = [" ++ (List.map valToString origVals |> String.join ",") ++
                                                                  "]\nnewValues = [" ++  (List.map valToString newOutVals |> String.join ",") ++ "]"
                         {- _ -> UpdateCriticalError <| "[internal error] Unexpected missing elements to propagate ldiffs:\n" ++
                                   "ldiffs = " ++ toString ldiffs ++
                                   ",\ni=" ++ toString i ++
                                   ",\nelems = " ++ (List.map (\(ws, ex) -> ws.val ++ Syntax.unparser Syntax.Elm ex) elemsToCollect |> String.join ",") ++
                                   ",\noriginalValues = " ++ (List.map valToString originalValues |> String.join ",") ++
                                   ",\nnewValues = " ++  (List.map valToString newValues |> String.join ",") -}
               in updateDiffs 0 (UpdatedEnv.original env) [] [] elems Nothing origVals newOutVals vldiffs
             _ -> UpdateCriticalError <| "Expected VListDiffs, got " ++ toString vdiffs
         _ -> UpdateCriticalError <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

     EList sp1 elems sp2 (Just tail) sp3 ->
       case (oldVal.v_, newVal.v_) of
         (VList origVals, VList newOutVals) ->
           updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
             (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
           case vdiffs of
             VListDiffs ldiffs -> --We do not allow insertions or deletions of elemnts before the tail.
              let updateDiffs: Int -> Int ->     UpdatedEnv -> List (WS, Exp) -> TupleDiffs EDiffs -> List (WS, Exp) -> List Val -> List Val -> List (Int, ListElemDiff VDiffs) -> UpdateStack
                  updateDiffs  i      elemSize   collectedEnv  revElems          revEDiffs            elemsToCollect    origVals    newOutVals  ldiffs =
                   case ldiffs of
                     [] ->

                       let updatedList = case List.reverse revEDiffs of
                         [] -> UpdatedExp e Nothing
                         l -> let finalElems = List.reverse <| Utils.reverseInsert elemsToCollect revElems in
                              UpdatedExp (replaceE__ e <| EList sp1 finalElems sp2 (Just tail) sp3) (Just <| EChildDiffs l)
                       in
                       updateResult collectedEnv updatedList
                     (i, m)::tailmodif ->
                       if i >= elemSize then
                         let (finalElems, changesInOrder) =  case List.reverse revEDiffs of
                           [] -> (elems, [])
                           l -> (List.reverse <| Utils.reverseInsert elemsToCollect revElems, l)
                         in
                         let valsToRemove = List.length elemsToCollect in
                         let tailOldVal = List.drop valsToRemove origVals in
                         let tailNewOutVal = List.drop valsToRemove newOutVals in
                         updateContinue "EList tail" env tail [] (replaceV_ oldVal <| VList tailOldVal) (replaceV_ newVal <| VList tailNewOutVal) (VListDiffs <| UpdateUtils.offset (0 - elemSize) ldiffs) <| \newTailUpdatedEnv newUpdatedTailExp ->
                           let finalUpdatedEnv = UpdatedEnv.merge e diffs env collectedEnv newTailUpdatedEnv in
                           let finalChanges = case newUpdatedTailExp.changes of
                             Nothing -> case changesInOrder of
                                [] -> Nothing
                                l -> Just <| EChildDiffs l
                             Just tailDiff ->
                                Just <| EChildDiffs (changesInOrder ++ [(List.length elems, tailDiff)])
                           in
                           updateResult finalUpdatedEnv <| UpdatedExp (replaceE__ e <| EList sp1 finalElems sp2 (Just newUpdatedTailExp.val) sp3) finalChanges
                       else -- i < elemSize then
                         case m of
                           ListElemDelete count -> -- Let's check if we can propagate this delete for the tail.
                             case (origVals, elemsToCollect, newOutVals) of
                               (headOrigVal::tailOrigVal, hdCollect::tlCollect, hdOut::tlOut) ->
                                 if (List.take count tailOrigVal |> List.all (valEqual headOrigVal)) && valEqual hdOut headOrigVal then
                                   updateDiffs (i + 1) elemSize collectedEnv (hdCollect::revElems) revEDiffs tlCollect tailOrigVal tlOut ((i + 1, ListElemDelete count)::tailmodif)
                                 else
                                   UpdateFails <| "Cannot delete elements appended to the left of a :: . Trying to remove element " ++ valToString headOrigVal
                               _ -> UpdateCriticalError <| "Expected non-empty lists, got at least one empty"
                           ListElemInsert count ->
                             case (origVals, elemsToCollect, newOutVals) of
                               (headOrigVal::tailOrigVal, hdCollect::tlCollect, hdOut::tlOut) ->
                                 if (List.take count tlOut |> List.all (valEqual hdOut)) && valEqual hdOut headOrigVal then
                                   updateDiffs (i + 1) elemSize collectedEnv (hdCollect::revElems) revEDiffs tlCollect tailOrigVal tlOut ((i + 1, ListElemInsert count)::tailmodif)
                                 else
                                   UpdateFails <| "Cannot inserted before elements appended to the left of a :: . Trying to insert element " ++ valToString headOrigVal
                               _ -> UpdateCriticalError <| "Expected non-empty lists, got at least one empty"
                           ListElemUpdate newModifs ->
                             case (origVals, elemsToCollect, newOutVals) of
                               (headOrigVal::tailOrigVal, (sp1, hdCollect)::tlCollect, hdOut::tlOut) ->
                                 updateContinue "EList ::" env hdCollect [] headOrigVal hdOut newModifs <| (
                                     \elemSize env collectedEnv revElems sp1 tlCollect tailOrigVal tlOut tailmodif -> \newUpdatedEnv newhdCollect ->
                                     let updatedEnv = UpdatedEnv.merge e diffs env collectedEnv newUpdatedEnv in
                                     let newRevEDiffs = case newhdCollect.changes of
                                       Nothing -> revEDiffs
                                       Just d -> (i, d)::revEDiffs
                                     in
                                     updateDiffs (i + 1) elemSize updatedEnv ((sp1, newhdCollect.val)::revElems) newRevEDiffs tlCollect tailOrigVal tlOut tailmodif
                                   ) elemSize env collectedEnv revElems sp1 tlCollect tailOrigVal tlOut tailmodif

                               _ -> UpdateCriticalError <| "Expected non-empty lists, got at least one empty"
              in
              updateDiffs 0 (List.length elems) (UpdatedEnv.original env) [] [] elems origVals newOutVals ldiffs
             m -> UpdateCriticalError ("Expected  a List diff, got " ++ toString m)
         _ -> UpdateCriticalError ("Expected a list to update, got " ++ valToString newVal)
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
                 UpdateCriticalError <| String.join ", " errors ++ "is not allowed. Maybe you wanted to use dictionaries?"
               else
                 updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                   (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                 case vdiffs of
                   VRecordDiffs dModifs ->
                    let updateDiff: Int -> UpdatedEnv -> List (WS, WS, String, WS, Exp) -> List (Int, EDiffs) -> List (WS, WS, String, WS, Exp) -> Dict String VDiffs -> UpdateStack
                        updateDiff i collectedEnv revCollectedEs revEDiffs esToCollect dModifs = case esToCollect of
                     [] ->
                       let tailChanges = case List.reverse revEDiffs of
                         [] -> []
                         eDiffs -> if Utils.maybeIsEmpty mi then eDiffs else UpdateUtils.offset 1 eDiffs
                       in
                       if Dict.isEmpty dModifs then
                         let finalUpdatedE = case List.reverse revEDiffs of
                           [] -> UpdatedExp e Nothing
                           _ ->
                             let finalEs = revCollectedEs |> List.reverse in
                             let finalChange = Just <| EChildDiffs tailChanges in
                             UpdatedExp (replaceE__ e <| ERecord sp1 mi finalEs sp2) finalChange
                         in
                         updateResult collectedEnv finalUpdatedE
                       else
                         case mi of
                           Nothing -> UpdateCriticalError <| "Trying to touch keys " ++ (Dict.keys dModifs |> String.join ",") ++ " but they do not exist in " ++ Syntax.unparser Syntax.Elm e
                           Just (init, spm) ->
                             let shadowingKeys = Set.fromList <| Utils.recordKeys es in
                             case doEval Syntax.Elm env init of
                               Err msg -> UpdateCriticalError msg
                               Ok ((initv, _), initVEnv) ->
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
                                     updateContinue "ERecord init" env init (keepLets env initVEnv) initv newInitV (VRecordDiffs dModifs) <|
                                       (\e env collectedEnv sp1 spm revCollectedEs sp2->  \newInitEnv newInit ->
                                       let finalEnv = UpdatedEnv.merge e diffs env collectedEnv newInitEnv in
                                       let finalChanges = case newInit.changes of
                                         Nothing -> case tailChanges of
                                           [] -> Nothing
                                           l -> Just <| EChildDiffs l
                                         Just d -> Just <| EChildDiffs <| (0, d)::tailChanges
                                       in
                                       let finalExp = replaceE__ e <| ERecord sp1 (Just (newInit.val, spm)) (List.reverse revCollectedEs) sp2 in
                                       updateResult finalEnv (UpdatedExp finalExp finalChanges)
                                       )  e env collectedEnv sp1 spm revCollectedEs sp2
                                   _ -> UpdateCriticalError <| "Expected a record, got " ++ valToString initv
                     ((sp3, sp4, k, sp5, ei) as e1)::esToCollectTail ->
                       case Dict.get k dModifs of
                         Nothing -> updateDiff (i + 1) collectedEnv (e1::revCollectedEs) revEDiffs esToCollectTail dModifs
                         Just eiModif ->
                           let originalEi = Dict.get k dOld |> Utils.fromJust_ "ERecord update1" in
                           let modifiedEi = Dict.get k dOut |> Utils.fromJust_ "ERecord update2" in
                           let newDiffs = Dict.remove k dModifs in
                           updateContinue ("ERecord " ++ k) env ei [] originalEi modifiedEi eiModif <|
                           ( \env collectedEnv sp3 sp4 k sp5 newDiffs->
                             \newUpdatedEnv newUpdatedEi ->
                             let finalEnv = UpdatedEnv.merge e diffs env collectedEnv newUpdatedEnv in
                             let finalRevEDiffs = case newUpdatedEi.changes of
                               Nothing -> revEDiffs
                               Just d -> (i, d)::revEDiffs
                             in
                             updateDiff (i + 1) finalEnv ((sp3, sp4, k, sp5, newUpdatedEi.val)::revCollectedEs) finalRevEDiffs esToCollectTail newDiffs
                           ) env collectedEnv sp3 sp4 k sp5 newDiffs
                    in updateDiff 0 (UpdatedEnv.original env) [] [] es dModifs
                   VConstDiffs ->
                     updateResultSameEnv env <| valToExpFull (Just e) sp1 InlineSpace newVal
                   m-> UpdateCriticalError ("Expected a Record diff, got " ++ toString m)
             _ -> UpdateCriticalError ("Expected Record as original value, got " ++ valToString oldVal)
         _ -> UpdateCriticalError ("Expected Record as value to update from, got " ++ valToString newVal)

     ESelect sp0 e1 sp1 sp2 ident ->
       case doEval Syntax.Elm env e1 of
         Err msg -> UpdateCriticalError msg
         Ok ((initv, _), initVEnv) ->
           case initv.v_ of
             VRecord dinit ->
               let newE1Value = replaceV_ initv <| VRecord (Dict.insert ident newVal dinit) in
               let propagatedDiff = VRecordDiffs (Dict.fromList [(ident, diffs)]) in
               updateContinue "ESelect" env e1 (keepLets env initVEnv) initv newE1Value propagatedDiff <|
                 \newE1UpdatedEnv newE1 ->
                   let finalExp = replaceE__ e <| ESelect sp0 newE1.val sp1 sp2 ident in
                   let finalChanges = Maybe.map (\d -> EChildDiffs [(0, d)]) newE1.changes in
                   updateResult newE1UpdatedEnv <| UpdatedExp finalExp finalChanges
             _ -> UpdateCriticalError ("Expected Record, got " ++ valToString initv)

     EApp sp0 e1 e2s appType sp1 ->
       let isFreezing e1 =
         --Debug.log ("Testing if " ++ unparse e1 ++ " is freezing:") <|
         case e1.val.e__ of
         EVar _ "freeze" -> True --Special meaning of freeze. Just check that it takes only one argument and that it's the identity.
         ESelect _ e _ _ "freeze" -> case e.val.e__ of
           EVar _ "Update" -> True
           _ -> False
         _ -> False
       in
       let continueIfNotFrozen = if isFreezing e1 then
         --case e2s of
           --[argument] -> -- Since we call this function only if there is a difference, freeze will fail !
             --if valEqual oldVal newVal then -- OK, that's the correct freeze semantics
             --  \continuation -> updateResultSameEnvExp env e
             --else
         \continuation ->
           case diffs of
             VUnoptimizedDiffs -> -- Maybe it's the same value
               if valEqual oldVal newVal then
                 updateResultSameEnvExp env e
               else
                 UpdateFails <| "freeze tried to be updated with a different value : " ++ valToString newVal ++ " from " ++ valToString oldVal
             _ ->
               UpdateFails <| "Hit a freeze (line " ++ toString e.start.line ++ ")" --: You are trying to update " ++ unparse e ++ " (line " ++ toString e.start.line ++ ") with a value '" ++ valToString newVal ++ "' that is different from the value that it produced: '" ++ valToString oldVal ++ "'"
           --_ -> \continuation -> continuation()
         else \continuation -> continuation()
       in
       continueIfNotFrozen <| \_ ->
       let maybeUpdateStack = case e1.val.e__ of
         ESelect es0 eRecord es1 es2 "apply" -> -- Special case here. apply takes a record and a value and applies the field apply to the value.
             -- The user may provide the reverse function if "unapply" is given, or "update"
             case doEval Syntax.Elm env eRecord of
               Err s -> Just <| (UpdateCriticalError s, False)
               Ok ((v1, _), _) ->
                 case v1.v_ of
                   VRecord d ->
                     if Dict.member "apply" d && (Dict.member "unapply" d || Dict.member "update" d) then
                       let isApplyFrozen = case (Dict.get "apply" d |> Utils.fromJust_ "Update.maybeUpdateStack").v_ of
                         VClosure _ _ body _ -> case body.val.e__ of
                           EApp _ bodyFun _ _ _ -> isFreezing bodyFun
                           _ -> False
                         _ -> False
                       in
                       case e2s of
                         [] -> Nothing
                         [argument] ->
                           let mbUpdateField = case Dict.get "update" d of
                             Nothing -> Nothing
                             Just fieldUpdateClosure ->
                                Just <|
                                case doEval Syntax.Elm env argument of
                                  Err s -> UpdateCriticalError s
                                  Ok ((vArg, _), vArgEnv) ->
                                    let x = eVar "x" in
                                    let y = eVar "y" in
                                    updateMany (ifUnoptimizedDeepDiff oldVal newVal diffs)
                                      (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                                    let diffsVal =
                                      -- ImpureGoodies.logTimedRun ".update vDiffsToVal" <| \_ ->
                                      vDiffsToVal (Vb.fromVal v1) vdiffs in
                                    let customArgument = Vb.record Vb.identity (Vb.fromVal vArg) <| Dict.fromList [
                                         ("input", vArg),
                                         ("output", newVal),
                                           ("outputNew", newVal), -- Redundant
                                           ("newOutput", newVal), -- Redundant
                                         ("outputOld", oldVal),
                                           ("outputOriginal", oldVal), -- Redundant
                                           ("oldOutput", oldVal), -- Redundant
                                         ("diffs", diffsVal),
                                           ("diff", diffsVal),    -- Redundant
                                           ("outDiff", diffsVal), -- Redundant
                                           ("diffOut", diffsVal) -- Redundant
                                         ] in
                                    let xyApplication = replaceE__ e <| EApp space0 x [y] SpaceApp space0 in
                                    let xyEnv = [("x", fieldUpdateClosure), ("y", customArgument)] in
                                    case (
                                        --ImpureGoodies.logTimedRun (".update eval line " ++ toString e1.start.line) <| \_ ->
                                        doEval Syntax.Elm xyEnv xyApplication) of
                                      Err s -> UpdateCriticalError <| "while evaluating a lens, " ++ s
                                      Ok ((vResult, _), _) -> -- Convert vResult to a list of results.
                                        case Vu.record Ok vResult of
                                          Err msg -> UpdateCriticalError <|
                                           "The update closure should return either {values = [list of values]}, {error = \"Error string\"}, or more advanced { values = [...], diffs = [..Nothing/Just diff per value.]}. Got "
                                           ++ valToString vResult
                                          Ok d ->
                                            let error = case Dict.get "error" d of
                                                Just errorv -> case Vu.string errorv of
                                                  Ok e -> e
                                                  Err x -> "Line " ++ toString e.start.line ++ ": the .error of the result of .update should be a string, got " ++ valToString errorv
                                                Nothing -> ""
                                            in
                                            if error /= "" then UpdateCriticalError <| "Line " ++ toString e.start.line ++ ": " ++ error
                                            else
                                              case Dict.get "values" d of
                                                Nothing -> UpdateCriticalError <| "Line " ++ toString e.start.line ++ ": .update  should return a record containing a .values field or an .error field"
                                                Just values ->  case Vu.list Ok values of
                                                  Err x -> UpdateCriticalError <| "Line " ++ toString e.start.line ++ ": .update  should return a record whose .values field is a list. Got " ++ valToString values
                                                  Ok valuesList ->
                                                    let valuesListLazy = LazyList.fromList valuesList in
                                                    let diffsListRes = case Dict.get "diffs" d of
                                                      Nothing ->
                                                        --ImpureGoodies.logTimedRun ".update recomputing diffs" <| \_ ->
                                                        let vArgStr = valToString vArg in
                                                        Results.projOk <|
                                                        List.map (\r ->
                                                          (if diffs == VUnoptimizedDiffs
                                                             then defaultVDiffsShallow
                                                             else defaultVDiffs) vArg r) <| valuesList
                                                      Just resultDiffsV ->
                                                        Vu.list (Vu.maybe valToVDiffs) resultDiffsV |>
                                                        Results.fromResult |>
                                                        Result.mapError (\msg -> "Expected a list of (Maybe differences). " ++ msg ++ ", for " ++ valToString resultDiffsV) |>
                                                        Result.mapError (\msg -> "Line " ++ toString e.start.line ++ " for the .diffs of the result of .update: " ++ msg)
                                                    in
                                                    updateManys diffsListRes <| \diffsList ->
                                                    let resultDiffs = LazyList.zip valuesListLazy (LazyList.fromList <| List.map ok1 diffsList) in
                                                    case resultDiffs of
                                                      LazyList.Nil -> UpdateCriticalError "[internal error] no diffs found"
                                                      LazyList.Cons (head, headDiff) lazyTail ->
                                                        updateContinueRepeat ".update" env argument [] vArg head headDiff lazyTail <|
                                                          \newUpdatedEnvArg newUpdatedArg ->
                                                          let newExp = replaceE__ e <| EApp sp0 (replaceE__ e1 <| ESelect es0 eRecord es1 es2 "apply") [newUpdatedArg.val] appType sp1 in
                                                          let newChanges = newUpdatedArg.changes |> Maybe.map (\changes -> EChildDiffs [(1, changes)]) in
                                                          updateResult newUpdatedEnvArg (UpdatedExp newExp newChanges)
                           in
                           updateMaybeFirst2 "after testing update, testing unapply" (not isApplyFrozen) mbUpdateField <| \_ ->
                             case Dict.get "unapply" d of
                               Just fieldUnapplyClosure ->
                                 case doEval Syntax.Elm env argument of
                                   Err s -> Just <| UpdateCriticalError s
                                   Ok ((vArg, _), vArgEnv) ->
                                     let x = eVar "x" in
                                     let y = eVar "y" in
                                     let customArgument = newVal in
                                     let customExpr = replaceE__ e <| EApp space0 x [y] SpaceApp space0 in
                                     case doEval Syntax.Elm (("x", fieldUnapplyClosure)::("y", customArgument)::env) customExpr of
                                       Err s -> Just <| UpdateCriticalError <| "while evaluating the .unapply of a lens, " ++ s
                                       Ok ((vResult, _), _) -> -- Convert vResult to a list of results.
                                         case interpreterMaybeToMaybe "the result of executing 'unapply'" vResult of
                                           Err msg -> Just <| UpdateCriticalError msg
                                           Ok Nothing -> Nothing
                                           Ok (Just newOut) ->
                                             Just <|
                                               updateMany (UpdateUtils.defaultVDiffs vArg newOut) (\() -> updateResultSameEnvExp env e) <| \newDiff ->
                                               updateContinue ".unapply" env argument (keepLets env vArgEnv) vArg newOut newDiff <|
                                                \newUpdatedEnvArg newUpdatedArg ->
                                                let newExp = replaceE__ e <| EApp sp0 (replaceE__ e1 <| ESelect es0 eRecord es1 es2 "apply") [newUpdatedArg.val] appType sp1 in
                                                let newChanges = newUpdatedArg.changes |> Maybe.map (\changes -> EChildDiffs [(1, changes)]) in
                                                updateResult newUpdatedEnvArg (UpdatedExp newExp newChanges)
                               Nothing -> Nothing
                         _ -> Nothing
                     else Nothing
                   _ -> Nothing
         _ ->
            Nothing
       in
       updateMaybeFirst ("after testing update/unapply, testing apply (line " ++ toString e.start.line ++ ")") maybeUpdateStack <| \_ ->
         case doEval Syntax.Elm env e1 of
           Err s       ->
             if appType /= InfixApp then UpdateCriticalError s else
             case (e1.val.e__, e2s) of
               (EVar spp "++", [eLeft, eRight]) -> -- We rewrite ++ to a call to "append" or "plus" depending on the arguments
                 case (doEval Syntax.Elm env eLeft, doEval Syntax.Elm env eRight) of
                   (Err s, _) -> UpdateCriticalError s
                   (_, Err s) -> UpdateCriticalError s
                   (Ok ((v1, _), _), Ok ((v2, _), ws2)) ->
                      let rewrite2 exp2Builder =
                            updateContinue "Rewriting ++ to +" (("x", v1)::("y", v2)::env)
                              (exp2Builder (replaceE__ eLeft <| EVar space0 "x") (replaceE__ eRight <| EVar space0 "y")) [] oldVal newVal diffs <|
                            \newAugEnv newUpdatedE1 -> -- we discard changes to E1
                              case newAugEnv.val of
                                ("x", newV1)::("y", newV2)::newEnv ->
                                  let eLeft_continue =
                                    case diffsAt 0 newAugEnv.changes of
                                      Nothing -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp eLeft Nothing)
                                      Just d -> updateContinue "left of ++" env eLeft [] v1 newV1 d
                                  in
                                  let eRight_continue =
                                    case diffsAt 1 newAugEnv.changes of
                                      Nothing -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp eRight Nothing)
                                      Just d -> updateContinue "right of ++" env eRight [] v2 newV2 d
                                  in
                                  let finalEnv = dropDiffs 2 newAugEnv.changes in
                                  eLeft_continue <| \newELeftEnv newELeft ->
                                    eRight_continue <| \newERightEnv newERight ->
                                      let (newE, newEChanges) = case (newELeft.changes, newERight.changes) of
                                        (Nothing, Nothing) -> (e, Nothing)
                                        (e1Change, e2Change) ->
                                           (replaceE__ e <| EApp sp0 e1 [newELeft.val, newERight.val] appType sp1,
                                            combineEChildDiffs [(1, e1Change), (2, e2Change)])
                                      in
                                      let finalEnv = UpdatedEnv.merge e diffs env newELeftEnv newERightEnv in
                                      updateResult finalEnv <| UpdatedExp newE newEChanges
                                _ -> UpdateCriticalError <| "[Internal error] Expected at least 2 values in the environment, got " ++ envToString newAugEnv.val
                      in
                      case (v1.v_, v2.v_) of
                        (VBase (VString _), VBase (VString _)) ->
                          let _ = Debug.log <| "It's a string update !" ++ valToString v1 ++ " , " ++ valToString v2 ++ " <-- " ++ valToString newVal in
                          rewrite2 (\ex ey -> replaceE__ e <| EOp space1 space1 (withDummyRange Plus) [ex, ey] space0)
                        _ ->
                          rewrite2  (\ex ey -> replaceE__ e <| EApp space1 (replaceE__ e1 <| EVar space0 "append") [ex, ey] SpaceApp space0)
               _ -> UpdateCriticalError ("++ should be called with two arguments, was called on "++toString (List.length e2s)++". " ++ s)
           Ok ((v1, _),v1Env) ->
             case v1.v_ of
               VClosure recName e1ps eBody env_ as vClosure ->
                 let ne1ps = List.length e1ps in
                 let (es2ToEval, es2ForLater) = Utils.split ne1ps e2s in

                 if List.length es2ForLater > 0 then -- Rewriting of the expression so that it is two separate applications
                   let rewriting = replaceE__ e <| EApp sp0 (replaceE__ e <| EApp sp0 e1 es2ToEval SpaceApp sp1) es2ForLater SpaceApp sp1 in
                   updateContinue "evaluating app with correct number of arguments" env rewriting [] oldVal newVal diffs <| (\newUpdatedEnv newRewriting ->
                     case newRewriting.val.val.e__ of
                       EApp _ innerApp newEsForLater _ _ ->
                         case innerApp.val.e__ of
                           EApp _ newE1 newEsToEval _ _ ->
                             let newChanges = newRewriting.changes |> Maybe.map (UpdateUtils.flattenFirstEChildDiffs ne1ps) in
                             let newExp = replaceE__ e <| EApp sp0 newE1 (newEsToEval ++ newEsForLater) appType sp1 in
                             updateResult newUpdatedEnv <| UpdatedExp newExp newChanges
                           e -> UpdateCriticalError <| "[internal error] expected EApp, got " ++ toString e
                       e -> UpdateCriticalError <| "[internal error] expected EApp, got " ++ toString e
                   )
                 else
                 case List.map2 (\p e2 -> doEval Syntax.Elm env e2) e1ps es2ToEval |> Utils.projOk of
                   Err s       -> UpdateCriticalError s
                   Ok v2ls ->
                     let v2s = List.map (\((v2, _), _) -> v2) v2ls in
                     let ne2 = List.length e2s in
                     if ne1ps > ne2 then -- Less arguments than expected, hence partial application.
                       --let _ = Debug.log ("Less arguments than expected, instead of " ++ toString ne1ps ++ " got " ++ toString ne2) () in
                       let (e1psUsed, e1psNotUsed) = Utils.split ne2 e1ps in
                       updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                         (\() ->  updateResultSameEnvExp env e) <| \diffs ->
                       case (newVal.v_, diffs) of
                         (VClosure _ psOut outBody envOut_, VClosureDiffs modifEnv modifBody) ->
                           let updatedEnvOut = UpdatedEnv envOut_ modifEnv in
                           if UpdatedEnv.isUnmodified updatedEnvOut && Utils.maybeIsEmpty modifBody then
                             updateResultSameEnvExp env e -- at this point, no modifications
                           else
                           --let _ = Debug.log ("Updating with : " ++ valToString newVal) () in
                           let continuation newV1 newV1Diffs newV2s newV2sDiffs msg =
                             let e1_updater = case newV1Diffs of
                               Nothing -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp e1 Nothing)
                               Just v1Diffs -> updateContinue ("VClosure1" ++ msg) env e1 (keepLets env v1Env) v1 newV1 v1Diffs
                             in
                             let e2s_updater = case newV2sDiffs of
                               [] -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExpTuple e2s Nothing)
                               v2sDiffs -> updateContinueMultiple ("args of " ++ msg) env [] (List.map3 (\e2 v2 newV2 -> (e2, v2, newV2)) e2s v2s newV2s) v2sDiffs
                             in
                             e1_updater  <| \newE1UpdatedEnv newUpdatedE1 ->
                                e2s_updater <| \newE2sUpdatedEnv newUpdatedE2s ->
                                  let finalUpdatedEnv = UpdatedEnv.merge e diffs env newE1UpdatedEnv newE2sUpdatedEnv in
                                  let finalExp = replaceE__ e <| EApp sp0 newUpdatedE1.val newUpdatedE2s.val appType sp1 in
                                  let finalChanges = UpdateUtils.combineAppChanges newUpdatedE1.changes newUpdatedE2s.changes in
                                  updateResult finalUpdatedEnv <| UpdatedExp finalExp finalChanges
                           in
                           case recName of
                             Nothing ->
                                case conssWithInversion (e1psUsed, v2s) (Just (env_, identity)) of
                                  Just (env__, consBuilder) -> --The environment now should align with updatedEnvOut_
                                    let ((newV2s, newV2sDiffs), newUpdatedEnv_) = consBuilder updatedEnvOut in
                                    --let _ = Debug.log ("v1 : " ++ valToString v1) () in
                                    --let _ = Debug.log ("newV2s : " ++ (List.map valToString newV2s |> String.join "")) () in
                                    --let _ = Debug.log ("newV2sDiffs : " ++ (List.map toString newV2sDiffs |> String.join "")) () in
                                    let (newV1, newV1Diffs) =
                                      case (newUpdatedEnv_.changes, modifBody) of
                                        ([], Nothing) -> (v1, Nothing)
                                        _ ->
                                          (replaceV_ v1 <| VClosure recName (e1psUsed ++ psOut) outBody newUpdatedEnv_.val, Just <| VClosureDiffs newUpdatedEnv_.changes modifBody)
                                    in
                                    continuation newV1 newV1Diffs newV2s newV2sDiffs "app"

                                  _          -> UpdateCriticalError <| "[internal error] " ++ strPos e1.start ++ " bad environment, internal error in update"
                             Just f ->
                                --let _ = Debug.log ("Recursive updating with environment: " ++ envToString (List.take 4 updatedEnvOut_)) () in
                                case conssWithInversion (e1psUsed, v2s)
                                      (consWithInversion (pVar f, v1) -- This order to be consistent with eval, where f is put first in the environment.
                                        (Just (env_, identity))) of
                                  Just (env__, consBuilder) -> --The environment now should align with updatedEnvOut_
                                    --let _ = Debug.log ("Original environment : " ++ envToString (List.take 4 env__)) () in
                                    let ((newV2s, newV2sDiffs),
                                         ((newArgFun, newArgFunDiffs),
                                           newUpdatedEnv_)) = consBuilder updatedEnvOut in
                                    let newClosureAndDiffs envChanges =
                                      (replaceV_ v1 <| VClosure recName (e1psUsed ++ psOut) outBody newUpdatedEnv_.val, VClosureDiffs envChanges modifBody ) in
                                    --let _ = Debug.log ("v1 : " ++ valToString v1) () in
                                    --let _ = Debug.log ("newArgFun : " ++ valToString newArgFun) () in
                                    --let _ = Debug.log ("newArgFunDiffs : " ++ toString newArgFunDiffs) () in
                                    --let _ = Debug.log ("newV2s : " ++ (List.map valToString newV2s |> String.join "")) () in
                                    --let _ = Debug.log ("newV2sDiffs : " ++ (List.map toString newV2sDiffs |> String.join "")) () in
                                    let (newV1, newV1Diffs)  = case (newUpdatedEnv_.changes, modifBody, newArgFunDiffs) of
                                      ([], Nothing, Nothing) -> (v1, Nothing)
                                      ([], Nothing, _) -> (newArgFun, newArgFunDiffs)
                                      (envChanges, _, Nothing) -> newClosureAndDiffs envChanges |> (\(a, b) -> (a, Just b))
                                      (envChanges, _, Just realArgFunDiffs) ->
                                        let (closure, closuremodifs) = newClosureAndDiffs envChanges in
                                        let (newv, newDiffs) = mergeVal v1 newArgFun realArgFunDiffs closure closuremodifs in
                                        (newv, Just newDiffs)
                                    in
                                    continuation newV1 newV1Diffs newV2s newV2sDiffs "rec app"

                                  _          -> UpdateCriticalError <| "[internal error] " ++ strPos e1.start ++ " bad environment, internal error in update"
                         (v, vdiffs)      -> UpdateCriticalError <| strPos e1.start ++ "Expected a closure in output, got " ++ valToString newVal ++ " and diffs " ++ toString vdiffs

                     else -- The right number of arguments
                     let continuation newClosure newClosureDiffs newArgs newArgsDiffs msg = --Update the function and the arguments.
                       let e1_updater = case newClosureDiffs of
                         Nothing -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp e1 Nothing)
                         Just (VClosureDiffs [] Nothing) -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp e1 Nothing)
                         Just closureDiffs -> updateContinue msg env e1 (keepLets env v1Env) v1 newClosure closureDiffs
                       in
                       let e2s_updater = case newArgsDiffs of
                         [] -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExpTuple e2s Nothing)
                         argsDiffs -> updateContinueMultiple ("args of " ++ msg) env [] (Utils.zip3 e2s v2s newArgs) argsDiffs
                       in
                        -- Cannot use the indices of because they are for the closure. But we could use other modifications at this stage, e.g. inserting a variable.
                       e1_updater <| \newE1UpdatedEnv newUpdatedE1 ->
                         e2s_updater <| \newE2UpdatedEnv newUpdatedE2s ->
                            let finalEnv = UpdatedEnv.merge e diffs env newE1UpdatedEnv newE2UpdatedEnv in
                            let finalChanges = UpdateUtils.combineAppChanges newUpdatedE1.changes newUpdatedE2s.changes in
                            updateResult finalEnv <| UpdatedExp (replaceE__ e <| EApp sp0 newUpdatedE1.val newUpdatedE2s.val appType sp1) finalChanges
                     in
                     case recName of
                       Nothing ->
                          case conssWithInversion (e1ps, v2s) (Just (env_,
                              \newUpdatedEnv_ -> \newUpdatedBody -> (replaceV_ v1 <| VClosure Nothing e1ps newUpdatedBody.val newUpdatedEnv_.val, VClosureDiffs newUpdatedEnv_.changes newUpdatedBody.changes))) of
                            Just (env__, consBuilder) ->
                               -- consBuilder: Env -> ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure)
                                updateContinue "VClosure3" env__ eBody [] oldVal newVal diffs <| \newUpdatedEnv newUpdatedBody ->
                                  let ((newArgs, newArgsDiffs), bodytoClosureAndDiff) = consBuilder newUpdatedEnv in
                                  let (newClosure, newClosureDiff) = bodytoClosureAndDiff newUpdatedBody in -- TODO: Once we return the diff of the expression, check for it before invoking patsBody...
                                  continuation newClosure (Just newClosureDiff) newArgs newArgsDiffs "full app"
                            _          -> UpdateCriticalError <| strPos e1.start ++ "bad environment"
                       Just f ->
                          case conssWithInversion (e1ps, v2s) (
                               consWithInversion (pVar f, v1) (
                               Just (env_, \newUpdatedEnv_ -> \newUpdatedBody -> (replaceV_ v1 <| VClosure (Just f) e1ps newUpdatedBody.val newUpdatedEnv_.val, VClosureDiffs newUpdatedEnv_.changes newUpdatedBody.changes)))) of
                            Just (env__, consBuilder) ->
                               -- consBuilder: Env -> ((Pat, Val), ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure))
                                updateContinue "VClosure6"  env__ eBody [] oldVal newVal diffs <| \newUpdatedEnv newUpdatedBody ->
                                  let ((newArgs, newArgsDiffs),
                                      ((newArgFun, newArgFunDiffs), bodytoClosureAndDiff)) = consBuilder newUpdatedEnv in
                                  let (newClosure, newClosureDiff) =
                                     case (newUpdatedBody.changes, UpdatedEnv.isUnmodified newUpdatedEnv, newArgFunDiffs) of
                                       (Nothing, True, Nothing) ->
                                         (v1, Nothing)
                                       (Nothing, True, _) ->
                                         (newArgFun, newArgFunDiffs)
                                       (_, _, Nothing) ->
                                         bodytoClosureAndDiff newUpdatedBody |> \(a, b) -> (a, Just b)
                                       (_, _, Just realArgFunDiffs) ->
                                         let (vclosure, vclosureDiff) = bodytoClosureAndDiff newUpdatedBody in
                                         mergeVal v1 newArgFun realArgFunDiffs vclosure vclosureDiff |> \(a, b) -> (a, Just b)
                                  in
                                  continuation newClosure newClosureDiff newArgs newArgsDiffs "full rec app"
                            x -> UpdateCriticalError <| "[internal error] should have got a list, got " ++ toString x

               VFun name argList evalDef maybeUpdateDef ->
                 case maybeUpdateDef of
                   Nothing -> UpdateFails ("No built-in definition to update " ++ name)
                   Just updateDef ->
                     let arity = List.length argList in
                     let nAvailableArgs = List.length e2s in

                     if arity < nAvailableArgs then -- Rewriting of the expression so that it is two separate applications
                       let (es2ToEval, es2ForLater)  = Utils.split arity e2s in
                       let rewriting = replaceE__ e <| EApp sp0 (replaceE__ e <| EApp sp0 e1 es2ToEval SpaceApp sp1) es2ForLater SpaceApp sp1 in
                       updateContinue "EApp VFun" env rewriting [] oldVal newVal diffs <| (\newUpdatedEnv newRewriting ->
                         case newRewriting.val.val.e__ of
                           EApp _ innerApp newEsForLater _ _ ->
                             case innerApp.val.e__ of
                               EApp _ newE1 newEsToEval _ _ ->
                                 let finalChanges = newRewriting.changes |> Maybe.map (UpdateUtils.flattenFirstEChildDiffs arity) in
                                 let finalExp = replaceE__ e <| EApp sp0 newE1 (newEsToEval ++ newEsForLater) appType sp1 in
                                 updateResult newUpdatedEnv <| UpdatedExp finalExp finalChanges
                               e -> UpdateCriticalError <| "[internal error] expected EApp, got " ++ Syntax.unparser Syntax.Elm innerApp
                           e -> UpdateCriticalError <| "[internal error] expected EApp, got " ++ Syntax.unparser Syntax.Elm newRewriting.val
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
                         []
                         oldVal
                         newVal
                         diffs
                         <| \newUpdatedEnv newUpdatedBody ->
                           case newUpdatedBody.val.val.e__ of
                             EApp _ funreconverted newEs _ _ ->
                               if expEqual funreconverted funconverted then
                                 let finalExp = replaceE__ e <| EApp sp0 e1 newEs SpaceApp sp1 in
                                 let finalChanges = newUpdatedBody.changes in
                                 updateResult newUpdatedEnv <| UpdatedExp finalExp finalChanges
                               else UpdateFails "Cannot modify the definition of a built-in function"
                             _ -> UpdateCriticalError <| "[internal error] expected EApp, got" ++ Syntax.unparser Syntax.Elm e
                     else -- Right arity
                       case List.map (doEval Syntax.Elm env) e2s |> Utils.projOk of
                         Err s       -> UpdateCriticalError s
                         Ok v2ls     ->
                           let v2s = List.map (\((v2, _), _) -> v2) v2ls in
                           updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                              (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                           case updateDef v2s oldVal newVal vdiffs of
                             Err msg -> UpdateCriticalError msg
                             Ok LazyList.Nil -> UpdateFails <| "not solution for updating " ++ name
                             Ok ll ->
                               let llWithDiffResult = ll |> LazyList.map (\(outputs, diffs) ->
                                 let resMaybeDiffsOffsetted = UpdateUtils.offset 1 diffs in
                                 (v1::outputs, ok1 <| Just <| resMaybeDiffsOffsetted)) in
                               updateOpMultiple "vfun" env (e1::e2s) (\funAndNewE2s ->
                                     replaceE__ e <| EApp sp0 e1 (Utils.tail "vfun" funAndNewE2s) appType sp1
                                   ) [] (v1::v2s) llWithDiffResult

               _ -> UpdateCriticalError <| strPos e1.start ++ " not a function"
     EIf sp0 cond sp1 thn sp2 els sp3 ->
       case doEval Syntax.Elm env cond of
         Ok ((v, _), _) ->
           case v.v_ of
             VBase (VBool b) ->
               if b then
                 updateContinue "IfThen" env thn prevLets oldVal newVal diffs <| \newUpdatedEnv newUpdatedThn ->
                   let finalExp = replaceE__ e <| EIf sp0 cond sp1 newUpdatedThn.val sp2 els sp3 in
                   let finalChanges = UpdateUtils.wrap 1 newUpdatedThn.changes in
                   updateResult newUpdatedEnv <| UpdatedExp finalExp finalChanges
               else
                 updateContinue "IfElse" env els prevLets oldVal newVal diffs<| \newUpdatedEnv newUpdatedEls ->
                   let finalExp = replaceE__ e <| EIf sp0 cond sp1 thn sp2 newUpdatedEls.val sp3 in
                   let finalChanges = UpdateUtils.wrap 2 newUpdatedEls.changes in
                   updateResult newUpdatedEnv <| UpdatedExp finalExp finalChanges
             _ -> UpdateCriticalError <| "Expected boolean condition, got " ++ valToString v
         Err s -> UpdateCriticalError s

     EOp sp1 spo op opArgs sp2 ->
       case (op.val, opArgs) of
         (NoWidgets, [arg]) -> -- We don't need to compute the argument's value since it's the same that we are propagating
           updateContinue  "NoWidgets" env arg [] oldVal newVal diffs <| \newUpdatedEnv newUpdatedArg ->
             let finalExp = replaceE__ e <| EOp sp1 spo op [newUpdatedArg.val] sp2 in
             updateResult newUpdatedEnv <| UpdatedExp finalExp (UpdateUtils.wrap 0 newUpdatedArg.changes)
         (DebugLog, [arg]) ->
           updateContinue  "DebugLog" env arg [] oldVal newVal diffs <| \newUpdatedEnv newUpdatedArg ->
             let finalExp = replaceE__ e <| EOp sp1 spo op [newUpdatedArg.val] sp2 in
             updateResult newUpdatedEnv <| UpdatedExp finalExp (UpdateUtils.wrap 0 newUpdatedArg.changes)
         _ -> -- Here we need to compute the argument's values.
           case Utils.projOk <| List.map (doEval Syntax.Elm env) opArgs of
             Err msg -> UpdateCriticalError msg
             Ok argsEvaled ->
               let ((vs, wss), envs) = Tuple.mapFirst List.unzip <| List.unzip argsEvaled in
               let args = List.map .v_ vs in
               case op.val of
                 CurrentEnv ->
                   let resNewEnv = Vu.list (Vu.tuple2 Vu.string Vu.identity) newVal in
                   let resNewEnvDiffs = (case diffs of
                     VListDiffs listDiffs -> toTupleDiffs listDiffs
                     _ -> Nothing) |>
                     Result.fromMaybe "Insertions and deletions not authorized in environments" |>
                     Result.andThen (List.map (\(i, d) ->
                     case d of
                       VRecordDiffs dict ->
                         case (Dict.get Lang.ctorTuple dict, Dict.get "_1" dict, Dict.get "_2" dict) of
                           (Just _, _, _) -> Err <| "Unexpected change to the tuple constructor's name"
                           (_, Just _, _) -> Err <| "Cannot change the name of a variable"
                           (_, _, Just d) -> Ok [(i, d)]
                           _ -> Ok []
                       _ ->
                         Err <| "Expected VRecordDiffs, got " ++ toString d
                   ) >> Utils.projOk) |> Result.map (List.concatMap identity)
                   in
                   case (resNewEnv, resNewEnvDiffs) of
                     (Ok newEnv, Ok envDiffs) -> updateResult (UpdatedEnv newEnv envDiffs) (UpdatedExp e Nothing)
                     (_, Err msg) -> UpdateFails msg
                     (Err msg, _) -> UpdateCriticalError msg

                 Explode    ->
                   case (vs, opArgs) of
                     ([v],[opArg]) ->
                       case (Vu.list Vu.string oldVal, Vu.list Vu.string newVal, diffs) of
                         (Ok oldStrings, Ok newStrings, VListDiffs d) ->
                           let newResult = Vb.string (Vb.fromVal v) (String.join "" newStrings) in
                           let resNewResultDiffs =
                             let
                               aux: ListDiffs VDiffs -> List String -> List String ->
                                   (Int,           Int,             List StringDiffs) -> Result String VDiffs
                               aux  d                   oldStrings     newStrings
                                   (currentOffset, strLengthBefore, revAcc) = case d of
                                 [] -> Ok <| VStringDiffs <| List.reverse revAcc
                                 (i, localdiff)::taildiff ->
                                   let notModified = i - currentOffset in
                                   if notModified > 0 then
                                     let (sameOld, remainingOld) = Utils.split notModified oldStrings in
                                     let (_, remainingNew) = Utils.split notModified newStrings in
                                     let sameStringLength = List.map String.length sameOld |> List.sum in
                                     (i, strLengthBefore + sameStringLength, revAcc) |>
                                     aux d remainingOld remainingNew
                                   else case localdiff of
                                     ListElemInsert count ->
                                       let (inserted, remainingNew) = Utils.split count newStrings in
                                       let insertedStringLength = List.map String.length inserted |> List.sum in
                                       (i, strLengthBefore,
                                        (StringUpdate strLengthBefore strLengthBefore insertedStringLength)::revAcc
                                       ) |>
                                       aux taildiff oldStrings remainingNew

                                     ListElemDelete count ->
                                       let (deleted, remainingOld) = Utils.split count oldStrings in
                                       let deletedStringLength = List.map String.length deleted |> List.sum in
                                       (i + count, strLengthBefore + deletedStringLength,
                                         StringUpdate strLengthBefore (strLengthBefore + deletedStringLength) 0 :: revAcc) |>
                                       aux taildiff remainingOld newStrings

                                     ListElemUpdate dd -> case (oldStrings, newStrings, dd) of
                                       (headOld::tailOld, _::tailNew, VStringDiffs l) ->
                                         (i + 1, strLengthBefore + String.length headOld,
                                         Utils.reverseInsert (offsetStr strLengthBefore l) revAcc) |>
                                         aux taildiff tailOld tailNew
                                       ([], _, _) -> Err <| "Diff and values are not coherent for explode: " ++ toString diffs ++ "\n" ++ valToString oldVal ++ "\n" ++ valToString newVal
                                       (_, [], _) -> Err <| "Diff and values are not coherent for explode: " ++ toString diffs ++ "\n" ++ valToString oldVal ++ "\n" ++ valToString newVal
                                       _ -> Err <| "expected a VStringDiffs in the diffs of explode's output's elements, got " ++ toString dd
                             in aux d oldStrings newStrings (0, 0, [])
                           in
                           case resNewResultDiffs of
                             Err msg -> UpdateCriticalError <| msg
                             Ok newResultDiffs ->
                               updateContinue "argument of explode" env opArg [] v newResult newResultDiffs <| \newUpdatedEnv newUpdatedOpArg ->
                                 let finalExp = replaceE__ e <| EOp sp1 spo op [newUpdatedOpArg.val] sp2 in
                                 updateResult newUpdatedEnv <| UpdatedExp finalExp (UpdateUtils.wrap 0 newUpdatedOpArg.changes)
                         (Err msg, _,_ ) -> UpdateCriticalError <| "Expected a list of string as previous output for explode, got " ++ msg ++ " for " ++ valToString oldVal
                         (_, Err msg, _) -> UpdateCriticalError <| "Expected a list of string as new output for explode, got " ++ msg ++ " for " ++ valToString newVal
                         (_, _, _) -> UpdateCriticalError <| "Expected a VListDiffs for explode, got " ++ toString diffs
                     (_, _) -> UpdateCriticalError <| "Wrong number of arguments for explode, expected 1, got " ++ toString (List.length vs)
                 DictFromList ->
                   case (vs, opArgs) of
                     ([keyValuesList], [keyValuesListE]) ->
                       case Vu.list (Vu.tuple2 (Vu.dup Vu.identity (LangUtils.valToDictKey Syntax.Elm)) Vu.identity) keyValuesList of
                         Err msg -> UpdateCriticalError <| "Expected a list of key/values, got " ++ msg ++ " for " ++ valToString keyValuesList
                         Ok listKeyDictKeyValues ->
                            updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                              (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                            case (newVal.v_, vdiffs) of
                              (VDict newDict, VDictDiffs dictDiffs) ->
                                let (newListRev, newDiffsRev) = List.foldl (\(((k, dKey), v), i) (newListRev, newDiffsRev) ->
                                    case (Dict.get dKey dictDiffs, Dict.get dKey newDict)  of
                                      (Just VDictElemDelete, _) -> (newListRev, (i, ListElemDelete 1)::newDiffsRev)
                                      (Just (VDictElemUpdate x), Just newElem)  -> ((k, newElem)::newListRev, (i, ListElemUpdate (VRecordDiffs (Dict.fromList [("_2", x)])))::newDiffsRev)
                                      (Nothing, _) -> ((k, v)::newListRev, newDiffsRev)
                                      (d, v) -> Debug.crash <| "Unexpected diff: " ++ toString d ++ " on val " ++ (Maybe.map valToString v |> Maybe.withDefault "Nothing") ++ " when updating DictFromList -- if it was a VDictElemInsert, the key already existed"
                                    ) ([], []) <| Utils.zipWithIndex listKeyDictKeyValues
                                in
                                let (finalListRev, numberAdded) = Dict.foldl (\k v (newListRev, numberAdded) ->
                                      case v of
                                        VDictElemInsert -> case Dict.get k newDict of
                                          Just newV -> case dictKeyToVal Syntax.Elm k of
                                            Ok thekey ->  ((thekey, newV)::newListRev, numberAdded + 1)
                                            Err msg -> Debug.crash <| "Could not get a key out of " ++ toString k ++ " because " ++ msg
                                          _ -> Debug.crash <| "Unexpected VictElemInsert of " ++ toString k ++ " but this key was not found in the updating dictionary " ++ valToString newVal
                                        _ -> (newListRev, numberAdded)
                                      ) (newListRev, 0) dictDiffs
                                in
                                let finalDiffsRev = if numberAdded > 0 then (List.length listKeyDictKeyValues, ListElemInsert numberAdded)::newDiffsRev else newDiffsRev in
                                let finalValuesList = Vb.list (Vb.tuple2 Vb.identity Vb.identity) (Vb.fromVal keyValuesList) (List.reverse finalListRev) in
                                let finalDiffsList = VListDiffs <| List.reverse finalDiffsRev in
                                updateContinue "DictFromList" env keyValuesListE [] keyValuesList finalValuesList finalDiffsList <| \newEnv newKeyValuesListE ->
                                  let finalExp = replaceE__ e <| EOp sp1 spo op [newKeyValuesListE.val] sp2 in
                                  updateResult newEnv <| UpdatedExp finalExp (UpdateUtils.wrap 0 newKeyValuesListE.changes)
                              (_, d) -> UpdateCriticalError <| "Expected a VDictDiffs, got " ++ toString d
                     _ -> UpdateCriticalError <| " DictFromList expects one argument, got " ++ toString (List.length vs)
                 DictGet ->
                   case vs of
                     [key, dict] ->
                       case valToDictKey Syntax.Elm key of
                         Err msg -> UpdateCriticalError <| "Could not convert this to a dictionary key: " ++ valToString key
                         Ok dictKey ->
                           case dict.v_ of
                             VDict d ->
                               let continuation newDict newDiff =
                                 case opArgs of
                                   [keyE, dictE] ->
                                     updateContinue "DictGet" env dictE [] dict newDict newDiff <| \newEnv newDictE ->
                                       updateResult newEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [keyE, newDictE.val] sp2) (UpdateUtils.wrap 1 newDictE.changes)
                                   _ -> UpdateCriticalError <| "DictGet requires two arguments, got " ++ toString (List.length vs)
                               in
                               case Dict.get dictKey d of
                                 Nothing -> case Vu.constructor Ok newVal of
                                   Ok ("Nothing", []) -> updateResultSameEnvExp env e
                                   Ok ("Just", [v]) -> -- We inserted a new value !
                                     let newDiff = VDictDiffs <| Dict.fromList [(dictKey, VDictElemInsert)] in
                                     let newDict = replaceV_ dict <| VDict <| Dict.insert dictKey v d in
                                     continuation newDict newDiff
                                   _ -> UpdateCriticalError <| "Expected Nothing or Just x, got " ++ valToString newVal
                                 Just oldKeyVal -> -- We should have the old value there.
                                   --DictGet returns a Just
                                       case Vu.constructor Ok newVal of
                                         Ok ("Nothing", []) -> -- Key deleted
                                           let newDiff = VDictDiffs <| Dict.fromList [(dictKey, VDictElemDelete)] in
                                           let newDict = replaceV_ dict <| VDict <| Dict.remove dictKey d in
                                           continuation newDict newDiff
                                         Ok ("Just", [newKeyVal]) -> -- Updates
                                           updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                                             (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                                           case vdiffs of
                                             VRecordDiffs dDiffs ->
                                               case Dict.get Lang.ctorArgs dDiffs of
                                                 Nothing -> updateResultSameEnvExp env e
                                                 Just subdiff ->
                                                   updateMany (ifUnoptimizedShallowDiff oldKeyVal newKeyVal subdiff)
                                                     (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                                                   case vdiffs of
                                                     VRecordDiffs dDiffs2 ->
                                                       case Dict.get "_1" dDiffs2 of
                                                         Just thediff ->
                                                           let newDiff = VDictDiffs <| Dict.fromList [(dictKey, VDictElemUpdate thediff)] in
                                                           let newDict = replaceV_ dict <| VDict <| Dict.insert dictKey newKeyVal d in
                                                           continuation newDict newDiff
                                                         Nothing -> updateResultSameEnvExp env e
                                                     otherDiff -> UpdateCriticalError <| "Expected VRecordDiffs, got " ++ toString otherDiff
                                             e -> UpdateCriticalError <| "Expected VRecordDiffs, got " ++ toString e
                                         _ -> UpdateCriticalError <| "Expected Nothing or Just x, got " ++ valToString newVal
                             _ -> UpdateCriticalError <| "DictGet requires the second argument to be a dictionary, got " ++ valToString dict
                     _ -> UpdateCriticalError <| "DictGet requires two arguments, got " ++ toString (List.length vs)
                 DictRemove ->
                   case vs of
                     [key, dict] ->
                       case valToDictKey Syntax.Elm key of
                         Err msg -> UpdateCriticalError <| "Could not convert this to a dictionary key: " ++ valToString key
                         Ok dictKey ->
                           case dict.v_ of
                             VDict oldDict ->
                               case Dict.get dictKey oldDict of
                                 Nothing -> -- No key was deleted
                                   case opArgs of
                                     [keyE, dictE] ->
                                        updateContinue "DictRemove" env dictE [] oldVal newVal diffs <| \newEnv newDictE ->
                                          updateResult newEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [keyE, newDictE.val] sp2) (UpdateUtils.wrap 1 newDictE.changes)

                                     _ -> UpdateCriticalError <| "[internal error] DictRemove requires two arguments"
                                 Just oldValue -> -- There was a value. In case we try to insert this key again, we either fail the insertion or convert it to an update.
                                   -- Currently, we fail
                                   updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                                     (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                                   case vdiffs of
                                     VDictDiffs dDiffs ->
                                       let potentialErrors =  Dict.foldl (\k v acc ->
                                            acc |> Result.andThen (\_ ->
                                              if k == dictKey then Err <| "Cannot insert/update the key " ++ valToString key ++ " to dictionary because it is removed (line " ++ toString e.start.line ++ ")"
                                              else Ok ())) (Ok ()) dDiffs
                                       in
                                       case potentialErrors of
                                         Err msg -> UpdateFails msg
                                         Ok _ ->
                                           case opArgs of
                                             [keyE, dictE] ->
                                                case newVal.v_ of
                                                  VDict newValDict ->
                                                    let argNewvalDict = replaceV_ newVal <| VDict (Dict.insert dictKey oldValue newValDict) in
                                                    updateContinue "DictRemove" env dictE [] dict argNewvalDict diffs <| \newEnv newDictE ->
                                                      updateResult newEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [keyE, newDictE.val] sp2) (UpdateUtils.wrap 1 newDictE.changes)
                                                  _ -> UpdateCriticalError <| "[internal error] DictRemove was updated with a non-dict: " ++ valToString newVal
                                             _ -> UpdateCriticalError <| "[internal error] DictRemove requires two arguments"
                                     d -> UpdateFails <| "DictRemove: Expected VDictDiffs, got " ++ toString d
                             _ -> UpdateCriticalError <| "DictRemove requires the second argument to be a dictionary, got " ++ valToString dict
                     _ -> UpdateCriticalError <| "DictRemove requires two arguments, got " ++ toString (List.length vs)
                 DictInsert ->
                   case (vs, opArgs) of
                     ([key, insertedVal, dict], [keyE, insertedE, dictE]) ->
                       case valToDictKey Syntax.Elm key of
                         Err msg -> UpdateCriticalError <| "Could not convert this to a dictionary key: " ++ valToString key
                         Ok dictKey ->
                           case dict.v_ of
                             VDict d ->
                               case newVal.v_ of
                                 VDict dNew ->
                                   let valWIthoutKey = replaceV_ newVal <| VDict <| Dict.remove dictKey dNew in
                                   updateMany (ifUnoptimizedShallowDiff oldVal newVal diffs)
                                     (\() -> updateResultSameEnvExp env e) <| \vdiffs ->
                                   case vdiffs of
                                     VDictDiffs dDiffs ->
                                       let newDictDiff = Dict.remove dictKey dDiffs in
                                       let diffsWithoutKey = VDictDiffs <| newDictDiff in
                                       let continuation = if Dict.isEmpty newDictDiff then
                                         \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp dictE Nothing)
                                         else
                                          \continuation -> updateContinue "DictInsert - dict" env dictE [] oldVal valWIthoutKey diffsWithoutKey continuation
                                       in
                                       let continuationInserted = case (Dict.get dictKey dDiffs, Dict.get dictKey dNew) of
                                           (Nothing, _) -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp insertedE Nothing)
                                           (Just (VDictElemUpdate insertedDiff), Just newInsertedVal) -> \continuation ->
                                             updateContinue "DictInsert - value" env insertedE [] insertedVal newInsertedVal insertedDiff continuation
                                           (Just (VDictElemInsert), _) -> \continuation ->
                                             UpdateCriticalError <| "Discrepancy, you are trying to insert a key which should already exist " ++ valToString key
                                           (Just (VDictElemDelete), _) -> \continuation ->
                                             UpdateFails <| "Cannot delete a key which was added by insertion" ++ valToString key
                                           (Just (VDictElemUpdate _), Nothing) -> \continuation ->
                                             UpdateFails <| "Incoherency: cannot update a key if it does not appear in the output value: " ++ valToString key
                                       in
                                       continuation <| \newEnv newDictE ->
                                         continuationInserted  <| \newEnv2 newInsertedE ->
                                           let finalEnv = UpdatedEnv.merge e diffs env newEnv newEnv2 in
                                           let finalChanges = UpdateUtils.combineEChildDiffs [(1, newInsertedE.changes), (2, newDictE.changes)] in
                                           updateResult finalEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [keyE, newInsertedE.val, newDictE.val] sp2) finalChanges

                                     e -> UpdateCriticalError <| "[Internal error] Expected a VDictDiffs, got " ++ toString e
                                 _ -> UpdateCriticalError <| "DictInsert cannot be updated with something other than a dict, got " ++ valToString newVal
                             _ -> UpdateCriticalError <| "DictInsert requires the second argument to be a dictionary, got " ++ valToString dict
                     _ -> UpdateCriticalError <| "DictInsert requires tnree arguments, got " ++ toString (List.length vs)
                 ToStrExceptStr ->
                   let default original =
                        case newVal.v_ of
                          VBase (VString s) ->
                            case Syntax.parser Syntax.Elm s of
                              Err msg -> UpdateFails <| "Could not parse new output value '"++s++"' for ToStrExceptStr expression " ++ ParserUtils.showError msg ++ "\nOriginal value was " ++ valToString original
                              Ok parsed ->
                                case doEval Syntax.Elm [] parsed of
                                  Err msg -> UpdateFails msg
                                  Ok ((v, _), _) ->
                                    case (opArgs, vs) of
                                      ([opArg], [arg]) ->
                                        updateMany ((if diffs == VUnoptimizedDiffs then defaultVDiffsShallow else defaultVDiffs) arg v)
                                          (\() -> updateResultSameEnvExp env e) <| \vDiff ->
                                          updateContinue "EOp ToStrExceptStr default" env opArg [] arg v vDiff <| \newUpdatedEnv newOpArg ->
                                            updateResult newUpdatedEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [newOpArg.val] sp2) (UpdateUtils.wrap 0 newOpArg.changes)
                                      e -> UpdateCriticalError <| "[internal error] Wrong number of arguments in update ToStrExceptStr: " ++ toString e
                          e -> UpdateCriticalError <| "Expected string, got " ++ valToString newVal
                   in
                   case vs of
                     [original] ->
                       case original.v_ of
                         VBase (VString origS) ->
                           case opArgs of
                             [opArg] ->
                               updateContinue "EOp ToStrExceptStr" env opArg [] original newVal diffs <| \newUpdatedEnv newOpArg ->
                                 updateResult newUpdatedEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [newOpArg.val] sp2) (UpdateUtils.wrap 0 newOpArg.changes)
                             e -> UpdateCriticalError <| "[internal error] Wrong number of argument values in update ToStrExceptStr: " ++ toString e
                         _ -> -- Everything else is unparsed to a string, we just parse it.
                           default original
                     _ -> UpdateCriticalError <| "[internale error] Wrong number or arguments in updateToStrExceptStr: " ++ toString e
                 ToStr      ->
                   case newVal.v_ of
                     VBase (VString s) ->
                       case Syntax.parser Syntax.Elm s of
                         Err msg -> UpdateFails <| "Could not parse new output value '"++s++"' for ToStr expression. " ++ (ParserUtils.showError msg)
                         Ok parsed ->
                           case doEval Syntax.Elm [] parsed of
                             Err msg -> UpdateFails msg
                             Ok ((v, _), _) ->
                               case (opArgs, vs) of
                                 ([opArg], [arg]) ->
                                   updateMany ((if diffs == VUnoptimizedDiffs then defaultVDiffsShallow else defaultVDiffs) arg v)
                                     (\() -> updateResultSameEnvExp env e) <| \vDiff ->
                                       updateContinue "EOp ToStr" env opArg [] arg v vDiff <| \newUpdatedEnv newOpArg ->
                                         updateResult newUpdatedEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [newOpArg.val] sp2) (UpdateUtils.wrap 0 newOpArg.changes)
                                 e -> UpdateCriticalError <| "[internal error] Wrong number of arguments in update: " ++ toString e
                     e -> UpdateCriticalError <| "Expected string, got " ++ valToString newVal
                 Eq ->
                   if valEqual newVal oldVal then
                     updateResultSameEnvExp env e
                   else
                     UpdateFails "Don't know how to reverse Eq operation"
                 RegexExtractFirstIn ->
                   case (vs, opArgs) of
                     ([regexpV, stringV], [regexpE, stringE]) ->
                       case UpdateRegex.updateRegexExtractFirstIn regexpV stringV oldVal newVal of
                         Err msg -> UpdateCriticalError msg
                         Ok ll ->
                           let llWithDiffs = LazyList.map (\newStringV -> (newStringV, UpdateUtils.defaultVDiffs stringV newStringV)) ll in
                           updateAlternatives "extractFirstIn" env stringE [] stringV llWithDiffs <| \newUpdatedEnv newStringE ->
                               updateResult newUpdatedEnv <| UpdatedExp (replaceE__ e <| EOp sp1 spo op [regexpE, newStringE.val] sp2) (UpdateUtils.wrap 1 newStringE.changes)
                     _ -> UpdateCriticalError "extractFirstIn requires regexp, replacement (fun or string) and the string"
                 _ ->
                   case maybeUpdateMathOp op vs oldVal newVal diffs of
                     Err msg ->
                       UpdateCriticalError msg
                     Ok LazyList.Nil ->
                       UpdateFails <| "No way to update computation" ++ Syntax.unparser Syntax.Elm e ++ " with " ++ valToString newVal ++ " (was" ++ valToString oldVal ++ ")"
                     Ok ll ->
                       updateOpMultiple "op" env opArgs (\newOpArgs -> replaceE__ e <| EOp sp1 spo op newOpArgs sp2) [] vs (LazyList.map (Tuple.mapSecond tupleDiffsToDiffs) ll)

     ECase sp1 input branches sp2 ->
       case doEval Syntax.Elm env input of
         Err msg -> UpdateCriticalError msg
         Ok ((inputVal, _), inputValEnv) ->
           case branchWithInversion env inputVal branches of
             Nothing -> UpdateCriticalError <| "Match error: " ++ valToString inputVal ++ " on branches " ++ Syntax.unparser Syntax.Elm e
             Just ((branchEnv, branchExp), envValBranchBuilder) ->
               updateContinue "ECase" branchEnv branchExp [] oldVal newVal diffs <| \upUpdatedEnv upExp ->
                 let (newBranchUpdatedEnv, newInputVal, newInputValDiffs, nBranches, nBranchesDiffs) = envValBranchBuilder (upUpdatedEnv, upExp) in
                 let input_update = case newInputValDiffs of
                   Nothing -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp input Nothing)
                   Just m -> updateContinue "ECase 2" env input (keepLets env inputValEnv) inputVal newInputVal m
                 in
                 input_update <| \newInputUpdatedEnv newInputUpdatedExp ->
                   let finalUpdatedEnv = UpdatedEnv.merge e diffs env newBranchUpdatedEnv newInputUpdatedEnv in
                   let finalExp = replaceE__ e <| ECase sp1 newInputUpdatedExp.val nBranches sp2 in
                   let finalChanges = UpdateUtils.combineEChildDiffs <| (0, newInputUpdatedExp.changes)::(UpdateUtils.offset 1 nBranchesDiffs) in
                   updateResult finalUpdatedEnv <| UpdatedExp finalExp finalChanges
     --  ETypeCase WS Exp (List TBranch) WS
     ELet sp1 letKind False p sp2 e1 sp3 body sp4 ->
         case Utils.maybeWithLazyDefault (prevLetsFind (Vb.fromVal oldVal) prevLets p |> Maybe.map Ok) <|
              \() -> doEval Syntax.Elm env e1 |> Result.map (\((v, w), _) -> (v, [])) of
           Err s       -> UpdateCriticalError s
           Ok (oldE1Val, prevLetsWithoutE1) ->
             case consWithInversion (p, oldE1Val) (Just (env, (\newUpdatedEnv -> newUpdatedEnv))) of
                Just (envWithE1, consBuilder) ->
                  updateContinue  "ELet"  envWithE1 body prevLetsWithoutE1 oldVal newVal diffs <| \newUpdatedEnvBody newUpdatedBody ->
                    case consBuilder newUpdatedEnvBody of
                     ((newE1Val, newE1ValDiffs), newUpdatedEnvFromBody) ->
                       let e1_update = case newE1ValDiffs of
                         Nothing -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp e1 Nothing)
                         Just m -> updateContinue "ELet2" env e1 [] oldE1Val newE1Val m
                       in
                       e1_update <| \newUpdatedEnvFromE1 newUpdatedE1 ->
                         let finalUpdatedEnv = UpdatedEnv.merge e diffs env newUpdatedEnvFromBody newUpdatedEnvFromE1 in
                         let finalExp = replaceE__ e <| ELet sp1 letKind False p sp2 newUpdatedE1.val sp3 newUpdatedBody.val sp4 in
                         let finalChanges = UpdateUtils.combineEChildDiffs <| [(0, newUpdatedE1.changes), (1, newUpdatedBody.changes)] in
                         updateResult finalUpdatedEnv <| UpdatedExp finalExp finalChanges
                Nothing ->
                  UpdateCriticalError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
     ELet sp1 letKind True p sp2 e1 sp3 body sp4 ->
         case Utils.maybeWithLazyDefault (prevLetsFind (Vb.fromVal oldVal) prevLets p |> Maybe.map Ok) <|
             \() -> doEval Syntax.Elm env e1 |> Result.map (\((v, w), _) -> (v, [])) of
           Err s       -> UpdateCriticalError s
           Ok (oldE1Val,prevLetsWithoutE1) ->
             case (p.val.p__, oldE1Val.v_) of
               (PVar _ fname _, VClosure isRec x closureBody env_) ->
                 let oldE1ValNamed = if Utils.maybeIsEmpty isRec then replaceV_ oldE1Val <| VClosure (Just fname) x closureBody env_ else oldE1Val in
                 case consWithInversion (p, oldE1ValNamed) (Just (env, (\newUpdatedEnv -> newUpdatedEnv))) of
                    Just (envWithE1, consBuilder) ->
                      updateContinue "ELetrec"  envWithE1 body prevLetsWithoutE1 oldVal newVal diffs <| \newUpdatedEnvBody newUpdatedBody ->
                        case consBuilder newUpdatedEnvBody of
                          ((newE1ValNamed, newE1ValNamedDiff), newEnv_) ->
                            let e1_update = case newE1ValNamedDiff of
                              Nothing -> \continuation -> continuation (UpdatedEnv.original env) (UpdatedExp e1 Nothing)
                              Just m ->
                                let newE1Val = case newE1ValNamed.v_ of
                                  VClosure (Just _) x vBody newEnv -> replaceV_ newE1ValNamed <| VClosure Nothing x vBody newEnv
                                  _ -> Debug.crash <| "[Internal error] This should have been a recursive method"
                                in
                                updateContinue "ELetrec2" env e1 [] oldE1Val newE1Val m
                            in e1_update <| \newUpdatedEnvE1 newUpdatedE1 ->
                              let finalEnv = UpdatedEnv.merge e diffs env newEnv_ newUpdatedEnvE1 in
                              let finalExp = replaceE__ e <| ELet sp1 letKind True p sp2 newUpdatedE1.val sp3 newUpdatedBody.val sp4 in
                              let finalChanges = UpdateUtils.combineEChildDiffs <| [(0, newUpdatedE1.changes), (1, newUpdatedBody.changes)] in
                              updateResult finalEnv (UpdatedExp finalExp finalChanges)
                    Nothing ->
                      UpdateCriticalError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
               (PList _ _ _ _ _, _) ->
                   UpdateCriticalError <| strPos e1.start ++
                     """mutually recursive functions (i.e. letrec [...] = [...] e) \
                        not yet implemented""" --"
                      -- Implementation also requires modifications to LangSimplify.simply
                      -- so that clean up doesn't prune the funtions.
               _ ->
                 UpdateCriticalError <| strPos e.start ++ " bad letrec trying to match " ++ Syntax.patternUnparser Syntax.Elm p ++ " with " ++ valToString oldE1Val

     EColonType a exp b c d ->
       updateContinue "EColonType" env exp prevLets oldVal newVal diffs <| \nv ne -> updateResult nv <| UpdatedExp (replaceE__ e <| EColonType a ne.val b c d) (UpdateUtils.wrap 0 ne.changes)
     EParens sp1 exp pStyle sp2->
       updateContinue "EParens" env exp prevLets oldVal newVal diffs <| \nv ne ->
         let continue ne =
           updateResult nv <| UpdatedExp (replaceE__ e <| EParens sp1 ne.val pStyle sp2) (UpdateUtils.wrap 0 ne.changes)
         in
         case pStyle of
           HtmlSyntax -> -- Here we convert all newly inserted elements that are style to a string instead of a list of lists.
             continue <| replaceInsertions ne <| \inserted ->
               case inserted.val.e__ of
                 EList sp0 [(spe1, e1), (spe2, e2)] sp1 Nothing sp2 ->
                   case (eStrUnapply e1, Eu.list (Eu.viewtuple2 Eu.string Eu.string) e2) of
                     (Just "style", Ok styles) ->
                       Just <| replaceE__ inserted <| EList sp0 [(spe1, e1), (space0,
                         replaceE__ e2 <| EApp space0 (eVar "__mbstylesplit__") [
                           replaceE__ e2 <| EBase space0 <| EString "\"" <| LangParserUtils.implodeStyleValue <|
                             List.filterMap (\(name, value) ->
                               let trimmedName = String.trim name in
                               if trimmedName /= "" then Just (trimmedName, String.trim value) else Nothing) <| styles] SpaceApp space0)] sp1 Nothing sp2
                     _ -> Nothing
                 _ -> Nothing
           _ -> continue ne
     {--ETypeCase sp1 e1 tbranches sp2 ->
       case eval_ syntax env (e::bt) e1 of
         Err s -> UpdateCriticalError s
         Ok (v1,sp1) ->
           case evalTBranches syntax env (e::bt) v1 tbranches of
             -- retVBoth and not addProvenanceToRet b/c only lets should return inner env
             Ok (Just (v2,sp2)) -> UpdateCriticalError "Typecase not updatable at this point"--Ok <| retVBoth [v2] (v2, sp1 ++ sp2) -- Provenence basedOn vals control-flow agnostic: do not include scrutinee
             UpdateCriticalError s              -> UpdateCriticalError s
             _                  -> UpdateCriticalError "Typecase not updatable at this point" --errorWithBacktrace syntax (e::bt) <| strPos e1.start ++ " non-exhaustive typecase statement"
     --}
     _ ->
       let outStr = valToString newVal in
       UpdateCriticalError <| "Non-supported update " ++ envToString (pruneEnv e env) ++ "|-" ++ unparse e ++ " <-- " ++ outStr ++ " (was " ++ valToString oldVal ++ ")"

-- Errors are converted to empty solutions because updateRec is called once a solution has been found already.
updateRec: LazyList HandlePreviousResult -> LazyList Fork -> UpdateStack -> LazyList (UpdatedEnv, UpdatedExp)
updateRec callbacks forks updateStack =
  --ImpureGoodies.logTimedRun "Update.updateRec" <| \_ ->
  case update callbacks forks updateStack of
    Ok l -> l
    Err msg -> LazyList.Nil

interpreterListToList: String -> Val -> Result String (List Val)
interpreterListToList msg v = case v.v_ of
  VList elems -> Ok elems
  _ -> Err <| "Expected a list for " ++ msg ++ ", got " ++ valToString v

interpreterMaybeToMaybe: String -> Val -> Result String (Maybe Val)
interpreterMaybeToMaybe msg v =
  case Vu.constructor Ok v of
    Ok ("Just", [v]) -> Ok (Just v)
    Ok ("Nothing", []) -> Ok Nothing
    Err s -> Err s
    _ -> Err <| "Not a maybe : " ++ valToString v

-- Compares the new reference with and original (which are at distance at most 2PI) and reports the relative smallest change to n
angleUpdate: Float -> Float -> Float -> Results String Num
angleUpdate new old n =
   let increment = new - old in
  --Two directions are possible, we take the closest first modulo 2 PI
   if increment <= pi && increment >= -pi then
     ok1 <| n + increment
   else if increment > pi then
     ok1 <| n + increment - 2*pi
   else
     ok1 <| n + increment + 2*pi

maybeUpdateMathOp : Op -> List Val -> Val -> Val -> VDiffs -> Results String (List Val, TupleDiffs VDiffs)
maybeUpdateMathOp op operandVals oldOutVal newOutVal diffs =
  case (oldOutVal.v_, newOutVal.v_, op.val) of
    (VBase (VString oldOut), VBase (VString newOut), Plus) ->
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
              let saLength = String.length sa in
              --let _ = ImpureGoodies.log <| "Handling concatenation '" ++ sa ++ "' + '" ++ sb ++ "' <-- " ++ newOut in
              let aux: Int -> List StringDiffs -> List StringDiffs -> Results String (List Val, TupleDiffs VDiffs)
                  aux  offset revForSa            diffs               =
                    --let _ = ImpureGoodies.log <| "offset=" ++ toString offset ++ ", revForSa=" ++ toString revForSa ++", diffs=" ++ toString diffs in
                    case diffs of
                    [] ->
                      let indexCut = offset + saLength in
                      let newSa = replaceV_ oldOutVal <| VBase <| VString <| String.left     indexCut newOut in
                      let newSb = replaceV_ oldOutVal <| VBase <| VString <| String.dropLeft indexCut newOut in
                      let finalDiffs = if revForSa == [] then [] else [(0, VStringDiffs (List.reverse revForSa))] in
                      ok1 ([newSa, newSb], finalDiffs)
                    ((StringUpdate start end replacement) as su) :: diffsTail ->
                      if start < saLength && end > saLength then -- We need to split the diffs, it cannot encompass two positions
                        [aux offset revForSa (StringUpdate start saLength 0 ::StringUpdate saLength end replacement ::diffsTail),
                         aux offset revForSa (StringUpdate start saLength replacement ::StringUpdate saLength end 0 ::diffsTail)] |>
                           Results.projOk |> Results.andThen (LazyList.fromList >> Ok)
                      else if start > saLength || start == saLength && end > start then -- The diff happens to the right of saLength
                        let indexCutNew = saLength + offset in
                        let newSa = replaceV_ oldOutVal <| VBase <| VString <| String.left     indexCutNew newOut in
                        let newSb = replaceV_ oldOutVal <| VBase <| VString <| String.dropLeft indexCutNew newOut in
                        let finalDiffs =
                             (if revForSa == [] then [] else [(0, VStringDiffs (List.reverse revForSa))]) ++
                             [(1, VStringDiffs (offsetStr (0 - saLength) diffs))]
                        in
                        ok1 ([newSa, newSb], finalDiffs)
                       else if start == saLength && end == saLength then -- Ambiguity here. We return both solutions with some preferences
                        let aLeft = String.slice (max (start - 1) 0) start sa in
                        let bRight = String.slice (end - saLength) (end - saLength + 1) sb in
                        let inserted = String.slice (start + offset) (start + offset + replacement) newOut in
                        let replacements = [
                           (offset + start + replacement, List.reverse (su :: revForSa), offsetStr (0 - saLength) diffsTail),
                           (offset + start, List.reverse revForSa, offsetStr (0 - saLength) (su :: diffsTail))
                           ]
                        in
                        let orderedReplacementst =
                            if affinity aLeft inserted >= affinity inserted bRight || end - start >= 1 && end <= saLength then
                              oks replacements
                            else
                              oks  <| List.reverse replacements
                        in
                        orderedReplacementst |> Results.andThen (\(indexCut, forSa, forSb) ->
                          let newSa = replaceV_ oldOutVal <| VBase <| VString <| String.left     indexCut newOut in
                          let newSb = replaceV_ oldOutVal <| VBase <| VString <| String.dropLeft indexCut newOut in
                          let finalDiffs =
                            (if forSa == [] then [] else [(0, VStringDiffs forSa)]) ++
                            (if forSb == [] then [] else [(1, VStringDiffs forSb)])
                          in
                          ok1 ([newSa, newSb], finalDiffs))
                       else -- end < saLength
                         aux (offset - (end - start) + replacement) (su::revForSa) diffsTail
              in
              ifUnoptimizedShallowDiff oldOutVal newOutVal diffs |> Results.andThen (\mbvdiff ->
                case mbvdiff of
                  Just (VStringDiffs d) -> aux 0 [] d
                  Just dd ->
                    Err <| "Expected VStringDiffs 3, got " ++ toString dd
                  Nothing ->
                    ok1 ([oldOutVal, newOutVal], [])
              )
            o -> Err <| "This operation is not supported for strings : " ++ toString o
        o -> Err <| "Expected two strings, got " ++ toString o ++ " -- actually (to update the operation " ++ (operandVals |> List.map valToString |> String.join " + ") ++ valToString oldOutVal ++ " <- " ++ valToString newOutVal ++ ")"

    (VConst _ (oldOut, _), VConst _ (newOut, _), _) ->
      if oldOut == newOut then ok1 (operandVals, []) else
      let operands = operandVals
        |> List.map (\operand ->
            case operand.v_ of
              VConst _ (v, _) -> Just v
              _ -> Nothing
            )
        |> Utils.projJusts in
      case operands of
        Nothing -> Err <| "Operands do not form a list of numbers: " ++ toString operandVals
        Just operands ->
          let autoDiff: Num -> Num -> Results String (Num, Num) -> Results String (List Num, TupleDiffs VDiffs)
              autoDiff l r res = res |> Results.map (\(newL, newR) ->
                ([newL, newR],
                  (if newL /= l then [(0, VConstDiffs)] else []) ++
                  (if newR /= r then [(1, VConstDiffs)] else [])
                )
              )
          in
          let autoDiff1: Num -> Results String Num -> Results String (List Num, TupleDiffs VDiffs)
              autoDiff1 n res = res |> Results.map (\newN ->
                ([newN], if newN /= n then [(0, VConstDiffs)] else [])
              )
          in
          let result = case (op.val, operands) of
                (Plus,    [l,r]) -> oks [([newOut - r, r], [(0, VConstDiffs)]),
                                         ([l, newOut - l], [(1, VConstDiffs)])]
                (Minus,   [l,r]) -> oks [([r + newOut, r], [(0, VConstDiffs)]),
                                         ([l, l - newOut], [(1, VConstDiffs)])]
                (Mult,    [l,r]) -> let newR = newOut / l
                                        newL = newOut / r
                                    in
                                    oks [([l, newR], if newR /= r then [(1, VConstDiffs)] else []),
                                         ([newL, r], if newL /= l then [(0, VConstDiffs)] else [])]
                (Div,     [l,r]) -> if newOut /= 0 then
                                         let newL = r * newOut
                                             newR = l / newOut
                                         in
                                         oks [([newL, r], if newL /= l then [(0, VConstDiffs)] else []),
                                              ([l, newR], if newR /= r then [(1, VConstDiffs)] else [])]
                                    else ok1 ([0, r], if l == 0 then [] else [(0, VConstDiffs)])
                (Pow,     [l,r]) -> autoDiff l r <| if l < 0 && r >= 0 && floor r == ceiling r then --Powers of negative must be with integer numbers
                                      if (floor r) % 2 == 0 then -- The result should be positive
                                        if newOut >= 0 then ok1 (0 - newOut ** (1 / r), r)
                                        else if newOut == -1 && l == -1 then
                                          if r > 0 then oks [(l, r - 1), (l, r + 1)]
                                          else ok1 (l, r + 1)
                                        else  Err "No way to invert l^r <-- out where l < 0, r is even, out < 0 and out /= -1"
                                      else -- r is odd, so we preserve the negative sign.
                                        if newOut >= 0 then ok1 (newOut ** (1/r), r) else ok1 (0 - ( 0 - newOut) ** (1/r), r)
                                    else if l >= 0 then
                                      if newOut > 0 then oks [(newOut ** (1 / r), r), (l, logBase l newOut)]
                                      else if floor (1 / r) == ceiling (1/r) && 1/r < 0 then ok1 (0 - (-newOut) ** (1 / r), r)
                                      else Err "No way to invert l^r <-- out where l >= 0, out < 0 and 1/r not an integer or not < 0"
                                    else Err "No way to invert l^r <-- out where l < 0 and r < 0 or r is not an integer"
                (Mod,     [l,r]) -> let newL = l + newOut - oldOut in
                                    ok1 ([newL, r], if newL /= l then [(0, VConstDiffs)] else [])
                (ArcTan2, [l,r]) -> -- We keep the same radius but change the angle
                                    autoDiff l r <|
                                    let (radius, theta) = toPolar (r, l) in
                                    ok1 <| fromPolar (radius, theta + newOut - oldOut)
                (Cos,     [n])   -> autoDiff1 n <|
                                    let newOutClamped = clamp -1 1 newOut in
                                    let moved = acos newOutClamped in -- value between 0 and PI
                                    -- We stay on the same quadrant
                                    let movedAbsolute  = -- value between 0 and 2*PI
                                      if ((n %% (2 * pi) + 3 * pi) %% (2 * pi) - pi >= 0) then moved
                                      else 2*pi - moved in
                                    let original = (n %% (2 * pi) + 2 * pi) %% (2 * pi) in
                                    angleUpdate movedAbsolute original n

                (Sin,     [n])   -> autoDiff1 n <|
                                    let newOutClamped = clamp -1 1 newOut in
                                    let moved = asin newOutClamped in -- value between -pi / 2 and pi / 2
                                    -- But n might be beyond [-pi/2, pi/2] !
                                    let movedAbsolute = -- value between -PI and PI
                                      if ((n %% (2 * pi) + 2 * pi + pi / 2) %% (2 * pi) - pi <= 0) then moved
                                      else if moved > 0 then pi - moved
                                      else -pi - moved in
                                    let original = (n %% (2 * pi) + 3 * pi) %% (2 * pi) - pi in
                                    angleUpdate movedAbsolute original n
                (ArcCos,  [n])   -> autoDiff1 n <| ok1 (cos newOut)
                (ArcSin,  [n])   -> autoDiff1 n <| ok1 (sin newOut)
                (Floor,   [n])   -> autoDiff1 n <| ok1 (n + newOut - oldOut)
                (Ceil,    [n])   -> autoDiff1 n <| ok1 (n + newOut - oldOut)
                (Round,   [n])   -> autoDiff1 n <| ok1 (n + newOut - oldOut)
                (Sqrt,    [n])   -> autoDiff1 n <| ok1 (newOut * newOut)
                (Pi,      [])    -> if newOut == pi then ok1 ([], []) else Err <| "Pi's value is 3.14159... and cannot be changed"
                _                -> Err <| "Not the correct number of arguments for " ++ toString op ++ "(" ++ toString operandVals ++ ")"
          in
          Results.map (\(newOperands, newDiffs) ->
            (List.map2 (\original newNumber ->
              case original.v_ of
                VConst m0 (oldNumber, m1) -> replaceV_ original <| VConst m0 (newNumber, m1)
                x -> Debug.crash <| "[internal error] Did not get a VConst: " ++ toString x
            ) operandVals newOperands,
            newDiffs)
          ) result
    (VConst c (oldOut, d), VBase (VString newOut), _) ->
      case String.toFloat newOut of
        Err msg -> Ok LazyList.Nil -- We don't know how to reverse it, but it's not a critical error.
        Ok f ->
          maybeUpdateMathOp op operandVals oldOutVal (replaceV_ oldOutVal <| VConst c (f, d)) VConstDiffs
    _ -> Err <|
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

branchWithInversion: Env -> Val -> List Branch -> Maybe ((Env, Exp), (UpdatedEnv, UpdatedExp) -> (UpdatedEnv, Val, Maybe VDiffs, List Branch, TupleDiffs (Maybe EDiffs)))
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
                (\(newUpdatedEnv, newUpdatedExp) ->
                  let (updatedUpdatedEnv, updatedVal, vdiff, newTailBranches, branchdiffs) = patValEnvRebuilder (newUpdatedEnv, newUpdatedExp) in
                  (updatedUpdatedEnv, updatedVal, vdiff, head::newTailBranches, UpdateUtils.offset 1 branchdiffs)
                  ))
              )
            Just (augEnv, patValEnvRebuilder) ->
              Just ((augEnv, exp), \(newAugUpdatedEnv, newUpdatedExp) ->
                let ((updatedVal, updatedValDiff), newUpdatedEnv) = patValEnvRebuilder newAugUpdatedEnv in
                let newBranch = replaceB__ head <| Branch_ sp1 pat newUpdatedExp.val sp2 in
                let newDiff = [(0, newUpdatedExp.changes)] in
                (newUpdatedEnv, updatedVal, updatedValDiff, newBranch :: tail, newDiff)
              )

consWithInversion : (Pat, Val) -> Maybe (Env, UpdatedEnv -> a) -> Maybe (Env, UpdatedEnv -> ((Val, Maybe VDiffs), a))
consWithInversion pv menv =
  case (matchWithInversion pv, menv) of
    (Just (env_, envToVal), Just (env, envToA)) -> Just (env_ ++ env,
      \newUpdatedEnv ->
        let (newUpdatedEnv_, newUpdatedEnvTail) = UpdatedEnv.split (List.length env_) newUpdatedEnv in
        let newpv = if UpdatedEnv.isUnmodified newUpdatedEnv_ then (Tuple.second pv, Nothing)
             else envToVal newUpdatedEnv_ in
        (newpv, envToA newUpdatedEnvTail)
      )
    _                     -> Nothing


conssWithInversion : (List Pat, List Val) -> Maybe (Env, UpdatedEnv -> a) -> Maybe (Env, UpdatedEnv -> ((List Val, TupleDiffs VDiffs), a))
conssWithInversion pvs menv =
  case (menv, matchListWithInversion pvs) of
    (Just (env, envToA), Just (env_, envToVals)) -> Just (env_ ++ env,
      \newUpdatedEnv ->
        let (newUpdatedEnv_, newUpdatedEnvTail) = UpdatedEnv.split (List.length env_) newUpdatedEnv in
        let newvals = if UpdatedEnv.isUnmodified newUpdatedEnv then (Tuple.second pvs, []) else envToVals newUpdatedEnv_ in
        (newvals, envToA newUpdatedEnvTail)
      )
    _                     -> Nothing

-- Given a pattern and a value, maybe returns an environment where the variables of the pattern match sub-values
-- The second element takes a new environment and modifications to it, and returns the new pattern and values
matchWithInversion : (Pat, Val) -> Maybe (Env, UpdatedEnv -> (Val, Maybe VDiffs))
matchWithInversion (p,v) = case (p.val.p__, v.v_) of
  (PWildcard _, _) -> Just ([], \newUpdatedEnv ->
     case newUpdatedEnv.val of
       [] -> (v, Nothing)
       _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ envToString newUpdatedEnv.val ++ " should have length 0"
     )
  (PVar ws x wd, _) -> Just ([(x,v)], \newUpdatedEnv ->
     case (newUpdatedEnv.val, newUpdatedEnv.changes) of
       ([(x, newV)], [(0, diffs)]) -> (newV, Just diffs)
       (_, []) -> (v, Nothing)
       _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ envToString newUpdatedEnv.val ++ " should have length 1"
     )
  (PAs sp0 spi x sp1 innerPat, _) ->
    matchWithInversion (innerPat, v) |> Maybe.map
      (\(env, updatedEnvReverse) -> ((x,v)::env, \newUpdatedEnv ->
        if UpdatedEnv.isUnmodified newUpdatedEnv then (v, Nothing) else
        let (newUpdatedEnvX, newUpdatedEnvInner) = UpdatedEnv.split 1 newUpdatedEnv in
        if UpdatedEnv.isUnmodified newUpdatedEnvX then -- Then the other environment was modified
         updatedEnvReverse newUpdatedEnvInner
        else -- newV is modified
          case (newUpdatedEnvX.val, newUpdatedEnvX.changes) of
          ([(_, newV)], [(0, mbModifVal)]) ->
              if UpdatedEnv.isUnmodified newUpdatedEnvInner then
                (newV, Just mbModifVal)
              else
                case updatedEnvReverse newUpdatedEnvInner of
                  (newV2, mbModifVal2) ->
                    let (newVal, newMbModifVal) = mergeValMaybe v newV (Just mbModifVal) newV2 mbModifVal2 in
                    (newVal, newMbModifVal)
          _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ envToString newUpdatedEnv.val ++ " should have length >= 1"
      ))

  (PList sp0 ps sp1 Nothing sp2, VList vs) ->
    if List.length ps /= List.length vs then Nothing else
    (ps,vs)
    |> matchListWithInversion
    |> Maybe.map (\(env, updatedEnvRenewer) ->
      (env, \newUpdatedEnv ->
        if UpdatedEnv.isUnmodified newUpdatedEnv then
          (v, Nothing)
        else
          let (newVals, newValsDiffs) = updatedEnvRenewer newUpdatedEnv in
          let (newVal, newValDiff) = case newValsDiffs of
               []-> (v, Nothing)
               _ -> (replaceV_ v <| VList newVals, Just <| VListDiffs <| List.map (\(i, m) -> (i, ListElemUpdate m)) <| newValsDiffs)
          in
          (newVal, newValDiff)))
  (PList sp0 ps sp1 (Just rest) sp2, VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if n > m then Nothing
    else
      let (vs1,oldTailVals) = Utils.split n vs in
      let oldTailVal = replaceV_ v <| VList oldTailVals in
      (ps, vs1)
      |> matchListWithInversion
      |> consWithInversion (rest, oldTailVal) -- Maybe (Env, UpdatedEnv -> ((Val, Maybe VDiffs), a))
      |> Maybe.map (\(env, envRenewer) ->
        (env, (\newUpdatedEnv ->
          if UpdatedEnv.isUnmodified newUpdatedEnv then (v, Nothing) else
          let ((newTailVal, mbTailValDiffs),
               (newVals,    mbValsDiffs)) = envRenewer newUpdatedEnv in
          let (finalVal, finalValDiffs) = case (mbValsDiffs, newTailVal.v_, mbTailValDiffs) of
            ([], _, Nothing) -> (v, Nothing)
            (_, VList tailVals, _) -> (replaceV_ v <| (VList <| newVals ++ tailVals),
                                      Just <| VListDiffs <|
                                       (List.map (\(i, d) -> (i, ListElemUpdate d)) mbValsDiffs) ++ (case ifUnoptimizedShallowDiffMb oldTailVal newTailVal mbTailValDiffs of
                                         Nothing -> []
                                         Just (VListDiffs diffs) ->
                                           UpdateUtils.offset (List.length ps) diffs
                                         Just x -> Debug.crash <| "Expected VListDiffs, got " ++ toString x
                                           ))
            _ -> Debug.crash <| "RHS of list pattern is not a list: " ++ valToString newTailVal
          in
          (finalVal, finalValDiffs)
        ))
      )
        -- dummy VTrace, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst _ (n_,_)) -> if n == n_ then Just ([], \newEnv -> (v, Nothing)) else Nothing
  (PConst _ n, _) -> Nothing
  (PBase _ bv, VBase bv_) -> if eBaseToVBase bv == bv_ then Just ([], \newEnv -> (v, Nothing)) else Nothing
  (PBase _ n, _) -> Nothing
  (PParens sp0 innerPat sp1, _) ->
    matchWithInversion (innerPat, v)
    |> Maybe.map
      (\(env, envReverse) -> (env, \newUpdatedEnv ->
        if UpdatedEnv.isUnmodified newUpdatedEnv then (v, Nothing) else
        case envReverse newUpdatedEnv of
          (newVal, newValDiffs) -> (newVal, newValDiffs)
      ))
  (PRecord sp0 pd sp1, VRecord d) ->
      pd |> List.map (\(_, _, k, _, p) ->
        Dict.get k d |> Maybe.map (\v -> (p, v)))
      |> Utils.projJusts
      |> Maybe.andThen (matchListWithInversion << List.unzip)
      |> Maybe.map (\(env, envRenewer) ->
         (env, \newUpdatedEnv ->
           if UpdatedEnv.isUnmodified newUpdatedEnv then (v, Nothing) else
           let (newVals, newValsDiffs) = envRenewer newUpdatedEnv in
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
           (newVal, newValDiff)
         )
      )
  (PRecord _ _ _, _) -> Nothing

matchListWithInversion : (List Pat, List Val) -> Maybe (Env, UpdatedEnv -> (List Val, TupleDiffs VDiffs))
matchListWithInversion (ps, vs) =
  let l = List.length ps in
  let inverse_index i = l - 1 - i in
  List.foldl (\pv acc -> --: Maybe (Env, List (Env -> (Pat, Val, Env)))
    case (acc, matchWithInversion pv) of
      (Just (old, oldEnvBuilders), Just (new, newEnvBuilder)) -> Just (new ++ old,
           (\newUpdatedEnv ->
            let (headNewUpdatedEnv, tailModifiedNewEnv) = UpdatedEnv.split (List.length new) newUpdatedEnv in
            if UpdatedEnv.isUnmodified headNewUpdatedEnv then
              (Tuple.second pv, Nothing, tailModifiedNewEnv)
            else
              let (newVal, newValDiff) = newEnvBuilder headNewUpdatedEnv in
              (newVal, newValDiff, tailModifiedNewEnv)
          )::oldEnvBuilders
        )
      _                    -> Nothing
  ) (Just ([], [])) (Utils.zip ps vs)
  |> Maybe.map (\(finalEnv, envBuilders) -> -- envBuilders: List (Env -> (Pat, Val, Env)), but we want Env -> (Pat, Val), combining pattern/values into lists
    (finalEnv, \newUpdatedEnv ->
      if UpdatedEnv.isUnmodified newUpdatedEnv then (vs, []) else
      let (newVals, newValsDiffs, _) =
        List.foldl (\(eToVE, inversed_i) (vals, valsDiffs, env)->
           let i = inverse_index inversed_i in
           let (v, vDiff, e) = eToVE env in
           let newValsDiffs = case vDiff of
             Nothing -> valsDiffs
             Just m -> (i, m)::valsDiffs
           in
           (v::vals, newValsDiffs, e)
           )  ([], [], newUpdatedEnv) (Utils.zipWithIndex envBuilders) in
      (newVals, newValsDiffs)
    ))

getNum: Val -> Result String Num
getNum v =
  case v.v_ of
    VConst _ (num, _) -> Ok num
    VBase (VString s) -> String.toFloat s
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

buildUpdatedValueFromHtmlString: String -> Result String Val
buildUpdatedValueFromHtmlString htmlEditorString =
  HTMLParser.parseHTMLString htmlEditorString
  |> Result.mapError ParserUtils.showError
  |> Result.andThen (\nodes ->
    case nodes of
      [] -> Err "No nodes"
      head::tail ->
        Ok <| HTMLValParser.htmlNodeToElmViewInLeo (Vb.fromVal <| builtinVal "buildUpdatedValueFromHtmlString" <| VBase <| VString "") head
  )