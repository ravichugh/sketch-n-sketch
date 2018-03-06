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

unparse = Syntax.unparser Syntax.Elm
unparsePattern = Syntax.patternUnparser Syntax.Elm

doUpdate : Exp -> Val -> Result String Val -> Results String (Env, Exp)
doUpdate oldExp oldVal newValResult =
  newValResult
    --|> Result.map (\x -> let _ = Debug.log "#1" () in x)
    |> Results.fromResult
    |> Results.andThen (\out -> update (updateContext Eval.initEnv oldExp oldVal out) LazyList.Nil)

updateEnv: Env -> Ident -> Val -> Env
updateEnv env k value =
  case env of
    [] -> Debug.crash <| k ++ " not found in environment "
    ((kk, vv) as kv)::tail ->
      if kk == k then (kk, value)::tail else kv::updateEnv tail k value

-- Make sure that Env |- Exp evaluates to oldVal
-- NextAction is a list of HandlePreviousREsult followed by a list of Fork in the same list.
update : UpdateStack -> LazyList NextAction -> Results String (Env, Exp)
update updateStack nextToUpdate=
  --let _ = Debug.log ("\nUpdateStack "++updateStackName_ "  " updateStack) () in
  --let _ = Debug.log ("NextToUpdate" ++ (String.join "" <| List.map (nextActionsToString_ "  ") <| Results.toList nextToUpdate)) () in
  -- At the end of nextToUpdate, there are all the forks that can be explored later.
  case updateStack of -- callbacks to (maybe) push to the stack.
    UpdateContextS env e oldVal out mb ->
       {--
      let _ = Debug.log (String.concat ["update: " {-, unparse e-}, " <-- "]) () in --, valToString out, " -- env = " , envToString (pruneEnv e env), "|-"]) () in
       --}
      update (getUpdateStackOp env e oldVal out) (LazyList.maybeCons mb nextToUpdate)

    UpdateResultS fEnv fOut mb -> -- Let's consume the stack !
       {--
      let _ = Debug.log (String.concat ["update final result: " {-, unparse fOut, " -- env = " , envToString (pruneEnv fOut fEnv)-}]) () in
       --}
      case (LazyList.maybeCons mb nextToUpdate) of -- Let's consume the stack !
        LazyList.Nil ->
          --let _ = Debug.log "finished update with no fork" () in
          ok1 <| (fEnv, fOut)
        LazyList.Cons head lazyTail ->
          case head of
            Fork _ newUpdateStack nextToUpdate2 ->
              --let _ = Debug.log "finished update with one fork" () in
              okLazy (fEnv, fOut) <| (\lt -> \() ->
                updateRec newUpdateStack <| LazyList.appendLazy nextToUpdate2 lt) lazyTail
            HandlePreviousResult msg f ->
              --let _ = Debug.log ("update needs to continue: " ++ msg) () in
              update (f fEnv fOut) (Lazy.force lazyTail)

    UpdateResultAlternative msg updateStack maybeNext ->
      {--
      let _ = Debug.log (String.concat ["update result alternative ", msg, ": "]) () in
      --}
      update updateStack <| LazyList.appendLazy nextToUpdate <| Lazy.map ((\nb -> \mb ->
          case mb of
            Nothing -> LazyList.Nil
            Just alternativeUpdateStack ->
              LazyList.fromList [Fork "" alternativeUpdateStack nb]
        ) (LazyList.takeWhile (\nextAction ->
          case nextAction of
            HandlePreviousResult _ _-> True
            Fork _ _ _-> False
        ) nextToUpdate)) maybeNext

    UpdateError msg ->
      Errs msg

getUpdateStackOp : Env -> Exp -> PrevOutput -> Output -> UpdateStack
getUpdateStackOp env e oldVal newVal =
  if oldVal == newVal then updateResult env e else
  case e.val.e__ of
    EHole ws _ -> updateResult env <| valToExp ws (IndentSpace "") newVal

    EConst ws num loc widget ->
      case getNum newVal of
        Err msg -> UpdateError msg
        Ok numv ->
         updateResult env <| replaceE__ e <| EConst ws numv loc widget

    EBase ws m ->
      case m of
         EString quoteChar chars ->
           case newVal.v_ of
             VBase (VString newChars) ->   updateResult env <| replaceE__ e <| EBase ws (EString quoteChar newChars)
             _ -> updateResult env <| valToExp ws (IndentSpace "") newVal
         _ -> updateResult env <| valToExp ws (IndentSpace "") newVal
    EFun sp0 ps e sp1 ->
      case newVal.v_ of
        VClosure Nothing newPs newE newEnv -> updateResult newEnv <| replaceE__ e <| EFun sp0 newPs newE sp1
        _ -> Debug.crash "Trying to update a function with non-closure " <| valToString newVal
    EVar sp is ->
      let newEnv = updateEnv env is newVal in
      updateResult newEnv e

    EList sp1 elems sp2 Nothing sp3 ->
      case (oldVal.v_, newVal.v_) of
        (VList origVals, VList newOutVals) ->
          if List.length origVals == List.length newOutVals then
            updateContinueMultiple "list" env (Utils.zip3 (Utils.listValues elems) origVals newOutVals)  (\newEnv newElems ->
              updateResult newEnv <| replaceE__ e <| EList sp1 (Utils.listValuesMake elems newElems) sp2 Nothing sp3
            )
          else
            let thediff = diffVals origVals newOutVals in
            let updateDiff collectedEnv newElems elemsToCollect thediff = case thediff of
                  [] ->
                    updateResult collectedEnv (replaceE__ e <| EList sp1 newElems sp2 Nothing sp3)
                  DiffEqual same :: tailDiff-> let n = List.length same in
                    updateDiff collectedEnv (newElems ++ List.take n elemsToCollect) (List.drop n elemsToCollect) tailDiff
                  DiffRemoved deleted :: DiffAdded inserted :: tailDiff ->
                    let deletedCount = List.length deleted in
                    let insertedCount = List.length inserted in
                    let updatedCount = min deletedCount insertedCount in
                    let updatedBefore = List.take updatedCount deleted in
                    let updatedAfter = List.take updatedCount inserted in
                    let remaining =
                      if deletedCount > insertedCount then [DiffRemoved (List.drop insertedCount deleted)]
                      else if deletedCount == insertedCount then []
                      else [DiffAdded (List.drop deletedCount inserted)]
                    in
                    let (updatedElems, elemsToCollectTail) = Utils.split deletedCount elemsToCollect in
                    updateContinueMultiple "list" env (Utils.zip3 (Utils.listValues updatedElems) updatedBefore updatedAfter) (\newEnv newRawElems ->
                         let newElems2 = Utils.listValuesMake updatedElems newRawElems in
                         let collectedEnv2 = triCombine e env collectedEnv newEnv in
                         updateDiff collectedEnv2 (newElems ++ newElems2) elemsToCollectTail (remaining ++ tailDiff)
                      )

                  DiffRemoved deleted::tailDiff ->
                    let deletedCount = List.length deleted in
                    let elemsToCollectTail = List.drop deletedCount elemsToCollect in
                    updateDiff collectedEnv newElems elemsToCollectTail tailDiff

                  DiffAdded inserted::tailDiff ->
                    let insertionIndex = List.length newElems in
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
                    updateDiff collectedEnv (newElems ++ elemsToAdd) (replaceFirst changeElementAfterInsert elemsToCollect) tailDiff
            in
            updateDiff env [] elems thediff
        _ -> UpdateError <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

    EList sp1 elems sp2 (Just tail) sp3 ->
      case (oldVal.v_, newVal.v_) of
        (VList origVals, VList newOutVals) ->
          let elemsLength = List.length elems in
          if elemsLength <= List.length newOutVals && elemsLength >= 1 then
            let elemOrigVals = List.take elemsLength origVals in
            let elemNewVals = List.take elemsLength newOutVals in
            let tailOrigVals = List.drop elemsLength origVals in
            let tailNewVals = List.drop elemsLength newOutVals in
            let toEList eles = replaceE__ e <| EList sp1 eles sp2 Nothing sp3 in
            let toVList elvs = replaceV_ newVal <| VList elvs in
            updateContinue env (toEList elems) (toVList elemOrigVals) (toVList elemNewVals) <|
              HandlePreviousResult "EList1" <| \newElemListEnv newElemListExp ->
                let newElems = case newElemListExp.val.e__ of
                  EList _ ne _ _ _ -> ne
                  o -> Debug.crash <| "Expected a list, got " ++ toString o
                in
                updateContinue env tail (toVList tailOrigVals) (toVList tailNewVals) <|
                  HandlePreviousResult "EList2" <| \newTailEnv newTailExp ->
                    let finalEnv = triCombine e env newElemListEnv newTailEnv in
                    updateResult finalEnv <| replaceE__ e <| EList sp1 newElems sp2 (Just newTailExp) sp3
          else UpdateError <| "Cannot (yet) update a list concatenation " ++ unparse e ++ " with " ++ toString elemsLength ++ " heads by list of smaller length: " ++ valToString newVal
        _ -> UpdateError <| "Cannot update a list " ++ unparse e ++ " with non-list " ++ valToString newVal

    ERecord sp1 mi es sp2 -> --Because records are typed, we should not allow the addition and removal of keys.
      case newVal.v_ of
        VRecord dOut ->
          case oldVal.v_ of
            VRecord dOld ->
              let errors = Dict.merge
                   (\keyOld valOld errs -> errs ++ ["Deleting key '" ++ keyOld++ "' from a record"])
                   (\key valOld valOut errs -> errs)
                   (\keyOut valOut errs -> errs ++ ["Inserting key '" ++ keyOut++ "' to a record"])
                   dOld dOut []
              in
              if not <| List.isEmpty errors then
                UpdateError <| String.join ", " errors ++ "is not allowed. Maybe you wanted to use dictionaries?"
              else
                  es
                  |> List.map (\((sp0, sp1, k, sp2, e1) as ke1)->
                    let v = Utils.fromJust_  "ERecord  get" (Dict.get k dOut) in
                    (ke1, (e1, Dict.get k dOld |> Utils.fromJust_ "ERecord update0", v))
                  )
                  |> List.unzip
                  |> (\(kvs, problems) ->
                    updateContinueMultiple "ERecord" env problems (mi |> (\mi newEnv newExps ->
                      case mi of
                        Nothing ->
                          let newE = replaceE__ e <| ERecord sp1 Nothing (List.map2 (\newE kv -> Utils.recordValueMap kv newE) newExps kvs) sp2 in
                          updateResult newEnv newE
                        Just (init, spm) ->
                           let shadowingKeys = Utils.recordKeys kvs in
                             case doEval Syntax.Elm env init of
                                Err msg -> UpdateError msg
                                Ok ((initv, _), _) ->
                                  case initv.v_ of
                                    VRecord dinit ->
                                      let newInitV = replaceV_ initv <| (dinit |>
                                             Dict.map (\k v ->  --Modify
                                              if List.any ((==) k) shadowingKeys then
                                                v
                                              else
                                                (Dict.get k dOut |> Utils.fromJust_ "ERecord update3") -- Push back all new values except those shadowed by es.
                                              ) |> VRecord)
                                      in
                                      updateContinue env init initv newInitV <| HandlePreviousResult "'ERecord" <| \newInitEnv newInit ->
                                        let finalExp = replaceE__ e <| ERecord sp1 (Just (newInit, spm)) (List.map2 (\newE kv -> Utils.recordValueMap kv newE) newExps kvs) sp2 in
                                        let finalEnv = triCombine finalExp env newInitEnv newEnv in
                                        updateResult finalEnv finalExp
                                    _ -> UpdateError ("Expected Record, got " ++ valToString initv)
                    ))
                  )
            _ -> UpdateError ("Expected Record as original value, got " ++ valToString oldVal)
        _ -> UpdateError ("Expected Record as value to update from, got " ++ valToString newVal)

    ESelect sp0 e1 sp1 sp2 ident ->
      case doEval Syntax.Elm env e1 of
        Err msg -> UpdateError msg
        Ok ((initv, _), _) ->
          case initv.v_ of
            VRecord dinit ->
              updateContinue env e1 initv (replaceV_ initv <| VRecord (Dict.insert ident newVal dinit)) <| HandlePreviousResult "ESelect" <|(\newE1Env newE1 ->
                updateResult newE1Env (replaceE__ e <| ESelect sp0 newE1 sp1 sp2 ident))
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
                            Just fieldUpdateClosure ->
                               case doEval Syntax.Elm env argument of
                                 Err s -> Just <| UpdateError s
                                 Ok ((vArg, _), _) ->
                                   let x = eVar "x" in
                                   let y = eVar "y" in
                                   let customArgument = replaceV_ vArg <| VRecord <| Dict.fromList [
                                        ("input", vArg),
                                        ("outputNew", newVal),
                                        ("output", newVal), -- Sometimes it's much simpler to call output
                                        ("outputOriginal", oldVal),
                                        ("oldOutput", oldVal)
                                        ] in
                                   let customExpr = replaceE__ e <| EApp space0 x [y] SpaceApp space0 in
                                   case doEval Syntax.Elm (("x", addUpdateCapability fieldUpdateClosure)::("y", customArgument)::env) customExpr of
                                     Err s -> Just <| UpdateError <| "while evaluating a lens, " ++ s
                                     Ok ((vResult, _), _) -> -- Convert vResult to a list of results.
                                       case interpreterListToList vResult of
                                         Err msg -> Just <| UpdateError msg
                                         Ok l ->
                                           case LazyList.fromList l of
                                             LazyList.Nil -> Nothing
                                             LazyList.Cons head lazyTail ->
                                               Just <| updateContinueRepeat env argument vArg head lazyTail <| HandlePreviousResult "Lens update" <| \newEnvArg newArg ->
                                                 let newExp = replaceE__ e <| EApp sp0 (replaceE__ e1 <| ESelect es0 eRecord es1 es2 "apply") [newArg] appType sp1 in
                                                 updateResult newEnvArg newExp
                            Nothing -> Nothing
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
                                        case interpreterMaybeToMaybe vResult of
                                          Err msg -> Just <| UpdateError msg
                                          Ok Nothing -> Nothing
                                          Ok (Just newOut) ->
                                            Just <| updateContinue env argument vArg newOut <| HandlePreviousResult "Lens unapply" <| \newEnvArg newArg ->
                                              let newExp = replaceE__ e <| EApp sp0 (replaceE__ e1 <| ESelect es0 eRecord es1 es2 "apply") [newArg] appType sp1 in
                                              updateResult newEnvArg newExp
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
                       Just <| updateResult env e
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
                  updateContinue env (replaceE__ e <|
                    EApp sp0 (replaceE__ e <| EApp sp0 e1 es2ToEval SpaceApp sp1) es2ForLater SpaceApp sp1) oldVal newVal <| HandlePreviousResult "EApp" <| (\newEnv newBody ->
                    case newBody.val.e__ of
                      EApp _ innerApp newEsForLater _ _ ->
                        case innerApp.val.e__ of
                          EApp _ newE1 newEsToEval _ _ ->
                            updateResult newEnv (replaceE__ e <| EApp sp0 newE1 (newEsToEval ++ newEsForLater) appType sp1)
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
                      case newVal.v_ of
                        VClosure _ psOut outBody envOut_ ->
                          --let _ = Debug.log ("Updating with : " ++ valToString newVal) () in
                          case recName of
                            Nothing ->
                              case conssWithInversion (e1psUsed, v2s)
                                      (Just (env_, \newEnv_ newE1ps -> replaceV_ v1 <| VClosure recName newE1ps outBody newEnv_ )) of
                                Just (env__, consBuilder) -> --The environment now should align with envOut_
                                  let ((newE1psUsed, newV2s), newE1psToClosure) = consBuilder envOut_ in
                                  let newV1 = newE1psToClosure <| newE1psUsed ++ psOut in
    
                                  updateContinue env e1 v1 newV1 <| HandlePreviousResult "VClosure1" <| \newE1Env newE1 ->
                                    updateContinueMultiple "app" env (List.map3 (\e2 v2 newV2 -> (e2, v2, newV2)) e2s v2s newV2s) <| \newE2sEnv newE2s ->
                                      let finalEnv = triCombine e env newE1Env newE2sEnv in
                                      updateResult finalEnv (replaceE__ e <| EApp sp0 newE1 newE2s appType sp1)
    
                                _          -> Debug.crash <| strPos e1.start ++ "bad environment, internal error in update"
                            Just f ->
                              --let _ = Debug.log ("Recursive updating with environment: " ++ envToString (List.take 4 envOut_)) () in
                              case conssWithInversion (e1psUsed, v2s) (consWithInversion (pVar f, v1) -- This order to be consistent with eval, where f is put first in the environment.
                                      (Just (env_, \newEnv_ newE1ps -> replaceV_ v1 <| VClosure recName newE1ps outBody newEnv_ ))) of
                                Just (env__, consBuilder) -> --The environment now should align with envOut_
                                  --let _ = Debug.log ("Original environment : " ++ envToString (List.take 4 env__)) () in
                                  let ((newE1psUsed, newV2s), ((newPatFun, newArgFun), newE1psToClosure)) = consBuilder envOut_ in
                                  --let _ = Debug.log ("newArgFun : " ++ valToString newArgFun) () in
                                  --let _ = Debug.log ("v1 : " ++ valToString v1) () in
                                  let newV1  =
                                    if not (valEqual newArgFun v1) then newArgFun -- Just propagate the change to the closure itself TODO: Merge instead of selecting manually
                                    else newE1psToClosure <| newE1psUsed ++ psOut -- Regular replacement
                                  in
                                  --let _ = Debug.log ("newV1 : " ++ valToString newV1) () in
                                  updateContinue env e1 v1 newV1 <| HandlePreviousResult "VClosure2" <| \newE1Env newE1 ->
                                    updateContinueMultiple "rec app" env (List.map3 (\e2 v2 newV2 -> (e2, v2, newV2)) e2s v2s newV2s) <| \newE2sEnv newE2s ->
                                      let finalEnv = triCombine e env newE1Env newE2sEnv in
                                      updateResult finalEnv (replaceE__ e <| EApp sp0 newE1 newE2s appType sp1)
    
                                _          -> Debug.crash <| strPos e1.start ++ "bad environment, internal error in update"
                        v          -> UpdateError <| strPos e1.start ++ "Expected a closure in output, got " ++ valToString newVal
    
                    else
                    case recName of
                      Nothing ->
                        case conssWithInversion (e1ps, v2s) (Just (env_, \newEnv_ newPs newBody -> replaceV_ v1 <| VClosure Nothing newPs newBody newEnv_)) of
                          Just (env__, consBuilder) ->
                             -- consBuilder: Env -> ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure)
                              updateContinue env__ eBody oldVal newVal <| HandlePreviousResult "VClosure3" <| \newEnv newBody ->
                                let ((newPats, newArgs), patsBodyToClosure) = consBuilder newEnv in
                                let newClosure = patsBodyToClosure newPats newBody in
                                updateContinue env e1 v1 newClosure <| HandlePreviousResult "VClosure4" <| \newE1Env newE1 ->
                                  updateContinue env (replaceE__ e <| EList sp0 (List.map ((,) space0) e2s) sp1 Nothing sp1) (replaceV_ oldVal <| VList v2s) (replaceV_ oldVal <| VList newArgs) <|
                                    HandlePreviousResult "VClosure5" <| \newE2Env newE2List ->
                                      case newE2List.val.e__ of
                                        EList _ newE2s _ _ _ ->
                                          let finalEnv = triCombine e env newE1Env newE2Env in
                                          updateResult finalEnv <| replaceE__ e <| EApp sp0 newE1 (Utils.listValues newE2s) appType sp1
    
                                        x -> Debug.crash <| "Internal error, should have get a list, got " ++ toString x
                          _          -> UpdateError <| strPos e1.start ++ "bad environment"
                      Just f ->
                        case conssWithInversion (e1ps, v2s) (consWithInversion (pVar f, v1) (Just (env_, \newEnv_ newPs newBody -> replaceV_ v1 <| VClosure (Just f) newPs newBody newEnv_))) of
                          Just (env__, consBuilder) ->
                             -- consBuilder: Env -> ((Pat, Val), ((Pat, Val), (newPat: Pat) -> (newBody: Exp) -> VClosure))
                              updateContinue env__ eBody oldVal newVal <| HandlePreviousResult "VClosure6" <| \newEnv newBody ->
                                let ((newPats, newArgs), ((newPatFun, newArgFun), patsBodytoClosure)) = consBuilder newEnv in
                                let newClosure =
                                  if not (valEqual newArgFun v1) then newArgFun -- Just propagate the change to the closure itself TODO: Merge instead of selecting manually
                                  else patsBodytoClosure newPats newBody -- Regular replacement
                                in
                                updateContinue env e1 v1 newClosure <| HandlePreviousResult "VClosure7"  <| \newE1Env newE1 ->
                                  updateContinue env (replaceE__ e <| EList sp0 (List.map ((,) space0) e2s) sp1 Nothing sp1) (replaceV_ oldVal <| VList v2s) (replaceV_ oldVal <| VList newArgs) <|
                                    HandlePreviousResult "VClosure8" <| \newE2Env newE2List ->
                                      case newE2List.val.e__ of
                                        EList _ newE2s _ _ _ ->
                                          let finalEnv = triCombine e env newE1Env newE2Env in
                                          updateResult finalEnv <| replaceE__ e <| EApp sp0 newE1 (Utils.listValues newE2s) appType sp1
                                        x -> Debug.crash <| "Unexpected result of updating a list " ++ toString x
                          _          -> UpdateError <| strPos e1.start ++ "bad environment"
              VFun name argList evalDef maybeUpdateDef ->
                case maybeUpdateDef of
                  Nothing -> UpdateError ("No built-in definition to update " ++ name)
                  Just updateDef ->
                    let arity = List.length argList in
                    let nAvailableArgs = List.length e2s in

                    if arity < nAvailableArgs then -- Rewriting of the expression so that it is two separate applications
                      let es2ToEval = List.take arity e2s in
                      let es2ForLater = List.drop arity e2s in
                      updateContinue env (replaceE__ e <|
                        EApp sp0 (replaceE__ e <| EApp sp0 e1 es2ToEval SpaceApp sp1) es2ForLater SpaceApp sp1) oldVal newVal <| HandlePreviousResult "EApp VFun" <| (\newEnv newBody ->
                        case newBody.val.e__ of
                          EApp _ innerApp newEsForLater _ _ ->
                            case innerApp.val.e__ of
                              EApp _ newE1 newEsToEval _ _ ->
                                updateResult newEnv (replaceE__ e <| EApp sp0 newE1 (newEsToEval ++ newEsForLater) appType sp1)
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
                      updateContinue env
                        (replaceE__ e <| EApp space0 funconverted e2s SpaceApp space0)
                        oldVal
                        newVal
                        <| HandlePreviousResult "EApp VFun eta" <| \newEnv newBody ->
                          case newBody.val.e__ of
                            EApp _ funreconverted newEs _ _ ->
                              if expEqual funreconverted funconverted then
                                updateResult newEnv (replaceE__ e <| EApp sp0 e1 newEs SpaceApp sp1)
                              else UpdateError "Cannot modify the definition of a built-in function"
                            _ -> Debug.crash <| "Internal error: expected EApp, got" ++ Syntax.unparser Syntax.Elm e
                    else -- Right arity
                      case List.map (doEval Syntax.Elm env) e2s |> Utils.projOk of
                        Err s       -> UpdateError s
                        Ok v2ls     ->
                          let v2s = List.map (\((v2, _), _) -> v2) v2ls in
                          let results = updateDef v2s oldVal newVal in
                          updateOpMultiple "vfun" env e2s (\newE2s -> replaceE__ e <| EApp sp0 e1 newE2s appType sp1) v2s (LazyList.fromList results)

              _ -> UpdateError <| strPos e1.start ++ " not a function"
    EIf sp0 cond sp1 thn sp2 els sp3 ->
      case doEval Syntax.Elm env cond of
        Ok ((v, _), _) ->
          case v.v_ of
            VBase (VBool b) ->
              if b then
                updateContinue env thn oldVal newVal <| HandlePreviousResult "VClosureIfThen" <| \newEnv newThn ->
                  updateResult newEnv <| replaceE__ e <| EIf sp0 cond sp1 newThn sp2 els sp3
              else
                updateContinue env els oldVal newVal <| HandlePreviousResult "VClosureIfElse"  <| \newEnv newEls ->
                  updateResult newEnv <| replaceE__ e <| EIf sp0 cond sp1 thn sp2 newEls sp3
            _ -> UpdateError <| "Expected boolean condition, got " ++ valToString v
        Err s -> UpdateError s

    EOp sp1 op opArgs sp2 ->
      case (op.val, opArgs) of
        (NoWidgets, [arg]) ->
          updateContinue env arg oldVal newVal <| HandlePreviousResult "EOp" <| \newEnv newArg ->
            updateResult newEnv <| replaceE__ e <| EOp sp1 op [newArg] sp2
        _ ->
          case Utils.projOk <| List.map (doEval Syntax.Elm env) opArgs of
            Err msg -> UpdateError msg
            Ok argsEvaled ->
              let ((vs, wss), envs) = Tuple.mapFirst List.unzip <| List.unzip argsEvaled in
              let args = List.map .v_ vs in
              case op.val of
                Explode    -> Debug.crash "Not implemented: update Explode "
                DebugLog   -> Debug.crash "Not implemented: update DebugLog "
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
                                     ([opArg], [arg]) -> updateContinue env opArg arg v <|
                                         HandlePreviousResult "EOp ToStrExceptStr default"<| \env newOpArg ->
                                           updateResult env <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                     e -> UpdateError <| "[internal error] Wrong number of arguments in update ToStrExceptStr: " ++ toString e
                         e -> UpdateError <| "Expected string, got " ++ valToString newVal
                  in
                  case vs of
                    [original] ->
                      case original.v_ of
                        VBase (VString origS) ->
                          case opArgs of
                            [opArg] -> updateContinue env opArg original newVal <|
                              HandlePreviousResult "EOp ToStrExceptStr"<| \env newOpArg ->
                                updateResult env <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
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
                                ([opArg], [arg]) -> updateContinue env opArg arg v <|
                                    HandlePreviousResult "EOp ToStr"<| \env newOpArg ->
                                      updateResult env <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                e -> UpdateError <| "[internal error] Wrong number of arguments in update: " ++ toString e
                    e -> UpdateError <| "Expected string, got " ++ valToString newVal
                RegexReplaceAllIn -> -- TODO: Move this in maybeUpdateMathOp
                  case vs of
                    [regexpV, replacementV, stringV] ->
                      let eRec env exp = doEval Syntax.Elm env exp |> Result.map (\((v, _), _) -> v) in
                      let uRec: Env -> Exp -> Val -> Val -> Results String (Env, Exp)
                          uRec env exp oldval newval = update (updateContext env exp oldval newval) LazyList.Nil
                      in
                      case UpdateRegex.updateRegexReplaceAllByIn
                          env eRec uRec regexpV replacementV stringV oldVal newVal of
                        Errs msg -> UpdateError msg
                        Oks ll -> updateOpMultiple "replaceAllIn" env opArgs (\newOpArgs -> replaceE__ e <| EOp sp1 op newOpArgs sp2) vs (LazyList.map (\(a, b, c) -> [a, b, c]) ll)
                    _ -> UpdateError "replaceAllIn requires regexp, replacement (fun or string) and the string"
                _ ->
                  case maybeUpdateMathOp op vs oldVal newVal of
                    Errs msg -> UpdateError msg
                    Oks ll ->
                      updateOpMultiple "op" env opArgs (\newOpArgs -> replaceE__ e <| EOp sp1 op newOpArgs sp2) vs ll

    ECase sp1 input branches sp2 ->
      case doEval Syntax.Elm env input of
        Err msg -> UpdateError msg
        Ok ((inputVal, _), _) ->
          case branchWithInversion env inputVal branches of
            Nothing -> UpdateError <| "Match error: " ++ valToString inputVal ++ " on branches " ++ Syntax.unparser Syntax.Elm e
            Just ((branchEnv, branchExp), envValBranchBuilder) ->
              updateContinue branchEnv branchExp oldVal newVal <| HandlePreviousResult "ECase" <| \upEnv upExp ->
                let (newBranchEnv, newInputVal, nBranches) = envValBranchBuilder (upEnv, upExp) in
                updateContinue env input inputVal newInputVal <| HandlePreviousResult "ECase 2"<| \newInputEnv newInputExp ->
                  let finalEnv = triCombine e env newInputEnv newBranchEnv in
                  let finalExp = replaceE__ e <| ECase sp1 newInputExp nBranches sp2 in
                  updateResult finalEnv finalExp
    --  ETypeCase WS Exp (List TBranch) WS
    ELet sp1 letKind False p sp2 e1 sp3 body sp4 ->
        case doEval Syntax.Elm env e1 of
          Err s       -> UpdateError s
          Ok ((oldE1Val,_), _) ->
            case consWithInversion (p, oldE1Val) (Just (env, (\newEnv -> newEnv))) of
               Just (envWithE1, consBuilder) ->
                 updateContinue envWithE1 body oldVal newVal <| HandlePreviousResult "ELet" <| \newEnvWithE1 newBody ->
                   case consBuilder newEnvWithE1 of
                    ((newPat, newE1Val), newEnvFromBody) ->
                      updateContinue env e1 oldE1Val newE1Val <| HandlePreviousResult "ELet2" <| \newEnvFromE1 newE1 ->
                        let finalEnv = triCombine e env newEnvFromE1 newEnvFromBody in
                        let finalExp = replaceE__ e <| ELet sp1 letKind False newPat sp2 newE1 sp3 newBody sp4 in
                        updateResult finalEnv finalExp
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
                case consWithInversion (p, oldE1ValNamed) (Just (env, (\newEnv -> newEnv))) of
                   Just (envWithE1, consBuilder) ->
                     updateContinue envWithE1 body oldVal newVal <| HandlePreviousResult "ELetrec" <| \newEnvWithE1 newBody ->
                       case consBuilder newEnvWithE1 of
                         ((newPat, newE1ValNamed), newEnvFromBody) ->
                           let newE1Val = case newE1ValNamed.v_ of
                             VClosure (Just _) x vBody newEnv -> { newE1ValNamed | v_ = VClosure Nothing x vBody newEnv }
                             _ -> Debug.crash "[internal error] This should have been a recursive method"
                           in
                           updateContinue env e1 oldE1Val newE1Val <| HandlePreviousResult "ELetrec2"<| \newEnvFromE1 newE1 ->
                             let finalEnv = triCombine e env newEnvFromE1 newEnvFromBody in
                             let finalExp = replaceE__ e <| ELet sp1 letKind True newPat sp2 newE1 sp3 newBody sp4 in
                             updateResult finalEnv finalExp
                   Nothing ->
                     UpdateError <| strPos e.start ++ " could not match pattern " ++ (Syntax.patternUnparser Syntax.Elm >> Utils.squish) p ++ " with " ++ strVal oldE1Val
              (PList _ _ _ _ _, _) ->
                  UpdateError <| strPos e1.start ++
                    """mutually recursive functions (i.e. letrec [...] [...] e) \
                       not yet implemented""" --"
                     -- Implementation also requires modifications to LangSimplify.simply
                     -- so that clean up doesn't prune the funtions.
              _ ->
                UpdateError <| strPos e.start ++ " bad letrec"

    EComment sp msg exp ->
      updateContinue env exp oldVal newVal <| HandlePreviousResult "EComment"<| \nv ne -> updateResult nv <| replaceE__ e <| EComment sp msg ne
    EOption a b c d exp ->
      updateContinue env exp oldVal newVal <| HandlePreviousResult "EOption"<| \nv ne -> updateResult nv <| replaceE__ e <| EOption a b c d ne
    ETyp a b c exp d    ->
      updateContinue env exp oldVal newVal <| HandlePreviousResult "ETyp"<| \nv ne -> updateResult nv <| replaceE__ e <| ETyp a b c ne d
    EColonType a exp b c d ->
      updateContinue env exp oldVal newVal <| HandlePreviousResult "EColonType"<| \nv ne -> updateResult nv <| replaceE__ e <| EColonType a ne b c d
    ETypeAlias a b c exp d ->
      updateContinue env exp oldVal newVal <| HandlePreviousResult "ETypeAlias"<| \nv ne -> updateResult nv <| replaceE__ e <| ETypeAlias a b c ne d
    EParens sp1 exp pStyle sp2->
      updateContinue  env exp oldVal newVal <| HandlePreviousResult "EParens"<| \nv ne -> updateResult nv <| replaceE__ e <| EParens sp1 ne pStyle sp2
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
updateRec: UpdateStack -> LazyList NextAction -> LazyList (Env, Exp)
updateRec updateStack nextToUpdate =
  case update updateStack nextToUpdate of
    Oks l -> l
    Errs msg -> LazyList.Nil

addToVClosureEnv: Env -> Val -> Val
addToVClosureEnv env v = case v.v_ of
  VClosure recName pats body oldEnv -> replaceV_ v <| VClosure recName pats body (env ++ oldEnv)
  _ -> v

addUpdateCapability: Val -> Val
addUpdateCapability v =
  addToVClosureEnv [
    ("updateApp", replaceV_ v <|
      VFun "updateApp" ["function", "input", "prevOutput", "newOuput"] (\env args ->
        case args of
          [fun, oldArg, oldOut, newOut] ->
            let reverseEnv = ("x", fun)::("y", oldArg)::env in
            let exp = (withDummyExpInfo <| EApp space0 (withDummyExpInfo <| EVar space0 "x") [withDummyExpInfo <| EVar space1 "y"] SpaceApp space0) in
            --let _ = Debug.log "calling back update" () in
            let res =
                 update (updateContext reverseEnv exp oldOut newOut) LazyList.Nil
                 |> Results.fold Err
                   (LazyList.toList
                     >> List.concatMap (\(newReverseEnv, newExp) ->
                        case newReverseEnv of
                          ("x", newFun)::("y",newArg)::newEnv ->
                            if envEqual newEnv env && valEqual fun newFun && expEqual newExp exp then
                              [newArg]
                            else []
                          _ -> Debug.crash "Internal error: expected x and y in environment"
                        )
                     >> (\newArgs ->
                        let _ = Debug.log ("Update finished: " ++ (valToString <| replaceV_ v <| VList newArgs)) () in
                        Ok ((replaceV_ v <| VList newArgs, []), env)
                     )
                   )
            in
            --let _ = Debug.log "call to update completed" in
            res
          _ -> Err <| "updateApp expects 4 arguments (function argument oldvalue newvalue), but got " ++ toString (List.length args)
      ) Nothing
    ),
    ("merge", replaceV_ v <|
      VFun "merge" ["original", "list_of_modified"] (\env args ->
        case args of
          [original, modifications] ->
            case modifications.v_ of
              VList modifications ->
                -- mergeVals original modifications
                Debug.crash "merge not yet implemented" -- TODO: Once we retrieve back the merge function, expose it.
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
                          DiffEqual els   -> [("same",replaceV_ v <| VBase <| VString "+"),   ("elements", replaceV_ v <| VList els)]
                          DiffAdded els   -> [("added",replaceV_ v <| VBase <| VString "-"),   ("elements", replaceV_ v <| VList els)]
                          DiffRemoved els -> [("removed",replaceV_ v <| VBase <| VString "="),   ("elements", replaceV_ v <| VList els)]
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

interpreterListToList: Val -> Result String (List Val)
interpreterListToList v = case v.v_ of
  VList elems -> Ok elems
  _ -> Err <| "Expected a list, got " ++ valToString v

interpreterMaybeToMaybe: Val -> Result String (Maybe Val)
interpreterMaybeToMaybe v = case v.v_ of
  VList [tag, e] ->
    case tag.v_ of
      VBase (VString "Just") -> Ok (Just e)
      _ -> Err <| "Expected 'Just' or 'Nothing', got " ++ valToString tag
  VList [tag] ->
    case tag.v_ of
      VBase (VString "Nothing") -> Ok Nothing
      _ -> Err <| "Expected 'Just' or 'Nothing', got " ++ valToString tag
  _ -> Err <| "Expected ['Just', x] or ['Nothing'], got " ++ valToString v

listPrefixEqual: Int -> (a -> a -> Bool) -> List a -> List a -> Bool
listPrefixEqual n pred l1 l2 =
  if n == 0 then True else
  case l1 of
    [] -> False
    hd::tl -> case l2 of
      [] -> False
      hd2::tl2 -> if pred hd hd2 then listPrefixEqual (n-1) pred tl tl2 else False

listEqual: (a -> a -> Bool) -> List a -> List a -> Bool
listEqual pred l1 l2 =
    case l1 of
      [] -> case l2 of
        [] -> True
        _ -> False
      hd::tl -> case l2 of
        [] -> False
        hd2::tl2 -> if pred hd hd2 then listEqual pred tl tl2 else False

indicesOfModification: (a -> a -> Bool) -> List a -> List a -> LazyList (Int, Int, List a)
indicesOfModification equalTest input output =
  let test sup = --: LazyList (Int, Int, List a)
    let potentialWithSuppression =  --: LazyList (Int, Int, List a)
       let insLength = List.length output - List.length input + sup in
       LazyList.fromList (List.range 0 (List.length input - sup))
       |> LazyList.andThen (\index ->
         if (sup > 0 || insLength > 0 || index == 0) &&
             listPrefixEqual index equalTest output input &&
             listEqual equalTest (List.drop (List.length output - (List.length input - (index + sup))) output) (List.drop (index + sup) input) then
           LazyList.fromList [(index, sup, List.drop index output |> List.take insLength)]
         else LazyList.Nil
         )
    in
    if LazyList.isEmpty potentialWithSuppression then test (sup + 1) else potentialWithSuppression
  in
  test 0

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
  let aux prefix s1 s2 =
       case (String.uncons s1, String.uncons s2) of
         (Nothing, _) -> prefix
         (_, Nothing) -> prefix
         (Just (s1head, s1tail), Just (s2head, s2tail)) ->
           if s1head == s2head then aux (prefix ++ String.fromChar s1head) s1tail s2tail else prefix
  in aux ""

commonSuffix: String -> String -> String
commonSuffix s1 s2 = commonPrefix (String.reverse s1) (String.reverse s2) |> String.reverse

branchWithInversion: Env -> Val -> List Branch -> Maybe ((Env, Exp), (Env, Exp) -> (Env, Val, List Branch))
branchWithInversion env input branches =
  case branches of
    [] -> Nothing
    head::tail ->
      case head.val of
        Branch_ sp1 pat exp sp2 ->
          case consWithInversion (pat, input) (Just (env, \newEnv -> newEnv)) of
            Nothing ->
              Maybe.map (\((augEnv, exp), patValEnvRebuilder) ->
                ((augEnv, exp),
                (\(newEnv, newExp) ->
                  let (updatedEnv, updatedVal, newTailBranches) = patValEnvRebuilder (newEnv, newExp) in
                  (updatedEnv, updatedVal, head::newTailBranches)
                  ))
              ) <| branchWithInversion env input tail
            Just (augEnv, patValEnvRebuilder) ->
              Just ((augEnv, exp), \(newAugEnv, newExp) ->
                let ((newPat, newVal), newEnv) = patValEnvRebuilder newAugEnv in
                (newEnv, newVal, (replaceB__ head <| Branch_ sp1 newPat newExp sp2) :: tail)
              )

consWithInversion : (Pat, Val) -> Maybe (Env, Env -> a) -> Maybe (Env, Env -> ((Pat, Val), a))
consWithInversion pv menv =
  case (matchWithInversion pv, menv) of
    (Just (env_, envToPatVal), Just (env, envToA)) -> Just (env_ ++ env,
      \newEnv ->
        let (newEnv_, newEnvTail) = Utils.split (List.length env_) newEnv in
        (envToPatVal newEnv_, envToA newEnvTail)
      )
    _                     -> Nothing


conssWithInversion : (List Pat, List Val) -> Maybe (Env, Env -> a) -> Maybe (Env, Env -> ((List Pat, List Val), a))
conssWithInversion pvs menv =
  case (menv, matchListWithInversion pvs) of
    (Just (env, envToA), Just (env_, envToPatsVals)) -> Just (env_ ++ env,
      \newEnv ->
        let (newEnv_, newEnvTail) = Utils.split (List.length env_) newEnv in
        (envToPatsVals newEnv_, envToA newEnvTail)
      )
    _                     -> Nothing

matchWithInversion : (Pat, Val) -> Maybe (Env, Env -> (Pat, Val))
matchWithInversion (p,v) = case (p.val.p__, v.v_) of
  (PWildcard _, _) -> Just ([], \newEnv ->
     case newEnv of
       [] -> (p, v) -- TODO: Mikael, is this PWildcard case okay?
       _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ toString newEnv ++ " should have length 0"
     )
  (PVar ws x wd, _) -> Just ([(x,v)], \newEnv ->
     case newEnv of
       [(x, newV)] -> (p, newV)
       _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ toString newEnv ++ " should have length 1"
     )
  (PAs sp0 x sp1 innerPat, _) ->
    matchWithInversion (innerPat, v) |> Maybe.map
      (\(env, envReverse) -> ((x,v)::env, \newEnv ->
        case newEnv of
          (_, newV)::newEnv2 ->
            if valEqual newV v then
              case envReverse newEnv2 of
              (newInnerPat, newVal) -> (replaceP__ p <| PAs sp0 x sp1 newInnerPat, newVal)
            else
              case envReverse newEnv2 of
              (newInnerPat, _)      -> (replaceP__ p <| PAs sp0 x sp1 newInnerPat, newV)

          _ -> Debug.crash <| "Not the same shape before/after pattern update: " ++ toString newEnv ++ " should have length >= 1"
      ))

  (PList sp0 ps sp1 Nothing sp2, VList vs) ->
    (if List.length ps == List.length vs then Just (ps,vs) else Nothing)
    |> Maybe.andThen matchListWithInversion
    |> Maybe.map (\(env, envRenewer) ->
      (env, envRenewer >> \(newPats, newVals) ->
        (replaceP__ p <| PList sp0 newPats sp1 Nothing sp2, replaceV_ v <| VList newVals)
      )
    )
  (PList sp0 ps sp1 (Just rest) sp2, VList vs) ->
    let (n,m) = (List.length ps, List.length vs) in
    if n > m then Nothing
    else
      let (vs1,vs2) = Utils.split n vs in
      (ps, vs1)
      |> matchListWithInversion
      |> consWithInversion (rest, replaceV_ v <| VList vs2) -- Maybe (Env, Env -> ((Pat, Val), (List Pat, List Val)))
      |> Maybe.map (\(env, envRenewer) ->
        (env, envRenewer >> (\((newPat, newVal), (newPats, newVals)) ->
          case newVal.v_ of
            VList otherVals ->
              (replaceP__ p <| PList sp0 newPats sp1 (Just newPat) sp2, replaceV_ v <| (VList <| newVals ++ otherVals))
            _ -> Debug.crash <| "RHS of list pattern is not a list: " ++ valToString newVal
        ))
      )
        -- dummy VTrace, since VList itself doesn't matter
  (PList _ _ _ _ _, _) -> Nothing
  (PConst _ n, VConst _ (n_,_)) -> if n == n_ then Just ([], \newEnv -> (p, v)) else Nothing
  (PBase _ bv, VBase bv_) -> if eBaseToVBase bv == bv_ then Just ([], \newEnv -> (p, v)) else Nothing
  (PParens sp0 innerPat sp1, _) ->
    matchWithInversion (innerPat, v)
    |> Maybe.map
      (\(env, envReverse) -> (env, \newEnv ->
        case envReverse newEnv of
          (newInnerPat, newVal) -> (replaceP__ p <| PParens sp0 newInnerPat sp1, newVal)
      ))
  (PRecord sp0 pd sp1, VRecord d) ->
      pd |> List.map (\(_, _, k, _, p) ->
        Dict.get k d |> Maybe.map (\v -> (p, v)))
      |> Utils.projJusts
      |> Maybe.andThen (matchListWithInversion << List.unzip)
      |> Maybe.map (\(env, envRenewer) ->
         (env, envRenewer >> \(newPats, newVals) ->
           ( replaceP__ p <| PRecord sp0 (Utils.recordValuesMake pd newPats) sp1,
             replaceV_ v <| VRecord (Utils.zip pd newVals |>
             List.foldl (\(pe, newv) dTemp->
               let k = Utils.recordKey pe in
               Dict.insert k newv dTemp
             ) d)
           )
         )
      )
  (PRecord _ _ _, _) -> Nothing
  _ -> Debug.crash <| "Little evaluator bug: Eval.match " ++ (toString p.val.p__) ++ " vs " ++ (valToString v)

matchListWithInversion : (List Pat, List Val) -> Maybe (Env, Env -> (List Pat, List Val))
matchListWithInversion (ps, vs) =
  List.foldl (\pv acc -> --: Maybe (Env, List (Env -> (Pat, Val, Env)))
    case (acc, matchWithInversion pv) of
      (Just (old, oldEnvBuilders), Just (new, newEnvBuilder)) -> Just (new ++ old,
           [\newEnv ->
            let (headNewEnv, tailNewEnv) = Utils.split (List.length new) newEnv in
            let (newPat, newVal) = newEnvBuilder headNewEnv in
            (newPat, newVal, tailNewEnv)
          ] ++ oldEnvBuilders
        )
      _                    -> Nothing
  ) (Just ([], [])) (Utils.zip ps vs)
  |> Maybe.map (\(finalEnv, envBuilders) -> -- envBuilders: List (Env -> (Pat, Val, Env)), but we want Env -> (Pat, Val), combining pattern/values into lists
    (finalEnv, \newEnv ->
      let (newPats, newVals, _) =
        List.foldl (\eToPVE (pats, vals, env)->
           let (p, v, e) = eToPVE env in
           ([p] ++ pats, [v] ++ vals, e)
           )  ([], [], newEnv) envBuilders in
      (newPats, newVals)
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
