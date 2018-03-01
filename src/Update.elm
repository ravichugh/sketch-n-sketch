module Update exposing
  ( envEqual
  , expEqual
  , buildUpdatedValueFromEditorString
  , buildUpdatedValueFromDomListener
  , doUpdate
  , envToString -- For tests
  , triCombine -- For tests
  , update -- For tests
  , diffs
  )

import Lang exposing (..)
import Eval exposing (doEval)
import Utils
import Syntax exposing (Syntax)
import LazyList exposing (LazyList)
import Results exposing
  ( Results(..)
  , ok1, oks, okLazy
  )
import MissingNumberMethods exposing (..)
import ValUnparser exposing (strVal)
import LangTools exposing (valToExp, IndentStyle(..), pruneEnv, pruneEnvPattern, valToString)
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
import UpdateStack exposing (NextAction(..), UpdateStack(..), updateResults
  , Output, PrevOutput, nextActionsToString_, updateStackName_, updateMaybeFirst, updateContinueRepeat
  , updateMaybeFirst2, updateResult, updateContinue, updateContext)
import UpdateRegex exposing (..)

unparse = Syntax.unparser Syntax.Elm
unparsePattern = Syntax.patternUnparser Syntax.Elm

doUpdate : Exp -> Val -> Result String Val -> Results String (Env, Exp)
doUpdate oldExp oldVal newValResult =
  newValResult
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
         let _ = Debug.log (String.concat ["update: ", unparse e, " <-- ", valToString v, " -- env = " , envToString (pruneEnv e env), "|-"]) () in
       --}
      update (getUpdateStackOp env e oldVal out) (LazyList.maybeCons mb nextToUpdate)

    UpdateResultS fEnv fOut mb -> -- Let's consume the stack !
       {--
         let _ = Debug.log (String.concat ["update final result: ", unparse fOut, " -- env = " , envToString (pruneEnv fOut fEnv)]) () in
       --}
      case (LazyList.maybeCons mb nextToUpdate) of -- Let's consume the stack !
        LazyList.Nil -> ok1 <| (fEnv, fOut)
        LazyList.Cons head lazyTail ->
          case head of
            Fork _ newUpdateStack nextToUpdate2 ->
              okLazy (fEnv, fOut) <| (\lt -> \() ->
                updateRec newUpdateStack <| LazyList.appendLazy nextToUpdate2 lt) lazyTail
            HandlePreviousResult _ f ->
              update (f fEnv fOut) (Lazy.force lazyTail)

    UpdateResultAlternative msg updateStack maybeNext ->
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
            let results = indicesOfModification valEqual origVals newOutVals -- LazyList Exp
                 |> LazyList.map (\(insertionIndex, deletedCount, inserted) ->
                   -- Copy the whitespace of the previous list elements, if possible, and do this in a nested way
                   let ((wsBeforeCommaHead, valToWSExpHead), (wsBeforeCommaTail, valToWSExpTail), changeElementAfterInsert) =
                        if insertionIndex > 0 then
                          if List.length elems > 1 then
                            case List.drop (min insertionIndex (List.length elems - 1)) elems |> List.take 1 of
                              [(wsComma,elemToCopy)] ->
                                 let psWs = ws <| Lang.precedingWhitespace elemToCopy in
                                 let indentation = if elemToCopy.start.line == elemToCopy.end.line
                                       then InlineSpace
                                       else IndentSpace (String.repeat (elemToCopy.start.col - 1) " ")
                                 in
                                 let policy = (wsComma, Lang.copyPrecedingWhitespace elemToCopy << valToExp psWs indentation) in
                                 (policy, policy, identity)
                              _   -> Debug.crash "Internal error: There should be an element in this list's position"
                          else -- Insertion index == 1 and List.length elems == 1
                            case elems of
                              [head] ->
                                let (wsComma, wsElem, indentation) = if e.start.line == e.end.line
                                  then (ws "", ws " ", InlineSpace)
                                  else (ws <| "\n" ++ String.repeat (e.end.col - 1) " ",
                                        ws " ",
                                        IndentSpace (String.repeat (e.end.col - 1) " "))
                                in
                                let policy = (wsComma, valToExp wsElem indentation) in
                                (policy, policy, identity)
                              _ ->  Debug.crash "Internal error: There should be an element in this list's position"
                        else --if insertionIndex == 0 then -- Inserting the first element is always trickier
                          case elems of
                            [] ->
                              if e.start.line == e.end.line then
                                ( (ws "", valToExp (ws "") InlineSpace)
                                , (ws " ", valToExp (ws " ") InlineSpace)
                                , identity
                                )
                              else -- By default, multi-line lists will use the syntax [ elem1\n, elem2\n ...]
                                let indentationSquareBracket = String.repeat (e.end.col - 1) " " in
                                let indentation = indentationSquareBracket ++ "  " in
                                ( (ws "", valToExp (ws " ") (IndentSpace indentation))
                                , (ws <| "\n" ++ indentationSquareBracket, valToExp (ws " ") (IndentSpace indentation))
                                , identity
                                )
                            (wsHead, head)::tail ->
                              let (wsSecondBeforeComma, wsSecondBeforeValue, indent) =
                                   case tail of
                                     [] ->
                                       if e.start.line == e.end.line then
                                         (ws "", " ", InlineSpace)
                                       else
                                         let indentationSquareBracket = String.repeat (e.end.col - 1) " " in
                                         let indentation = indentationSquareBracket ++ "  " in
                                         (ws <| "\n" ++ indentationSquareBracket, " ", IndentSpace indentation)
                                     (wsNext, elemNext)::tail2 ->
                                       let indentationSquareBracket = String.repeat (e.end.col - 1) " " in
                                       let indentation = if elemNext.start.line == elemNext.end.line then
                                            InlineSpace
                                            else IndentSpace (indentationSquareBracket  ++ "  ") in
                                       (wsNext, Lang.precedingWhitespace elemNext, indentation)
                              in
                              ( (ws "", valToExp (ws " ") indent)
                              , (wsSecondBeforeComma, valToExp (ws wsSecondBeforeValue) indent)
                              , \(nextWsBeforeComma, nextElem)-> (wsSecondBeforeComma, Lang.replacePrecedingWhitespace wsSecondBeforeValue nextElem)
                              )
                              -- We need to copy the whitespace of second to head.
                   in
                   let insertedExp = List.indexedMap (\index inserted ->
                        ( (if index == 0 then wsBeforeCommaHead else wsBeforeCommaTail)
                        , (if index == 0 then valToWSExpHead    else valToWSExpTail) inserted) ) inserted
                   in
                   let replaceFirst f l = case l of
                     [] -> []
                     head::tail -> f head::tail
                   in
                   (env, replaceE__ e <|
                     EList sp1 (List.take insertionIndex elems ++ insertedExp ++ replaceFirst changeElementAfterInsert (List.drop (insertionIndex + deletedCount) elems)) sp2 Nothing sp3)
                 )
            in -- We need to convert this lazyList to a set of results
            case results of
              LazyList.Nil -> UpdateError <| "Internal error: there should have been at least one solution"
              LazyList.Cons (newEnv, newExp) lazyTail ->
                updateResults (updateResult newEnv newExp) (Lazy.map (LazyList.map (\(x, y) -> updateResult x y)) lazyTail)

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
        _ -> UpdateError ("Expected Record as value to update from, got " ++ valToString oldVal)

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
                                        ("outputOriginal", oldVal)
                                        ] in
                                   let customExpr = replaceE__ e <| EApp space0 x [y] SpaceApp space0 in
                                   case doEval Syntax.Elm (("x", fieldUpdateClosure)::("y", customArgument)::env) customExpr of
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
                             Err msg -> UpdateError <| "Could not parse new output value '"++s++"' for ToStr expression " ++ toString msg
                             Ok parsed ->
                               case doEval Syntax.Elm [] parsed of
                                 Err msg -> UpdateError msg
                                 Ok ((v, _), _) ->
                                   case (opArgs, vs) of
                                     ([opArg], [arg]) -> updateContinue env opArg arg v <|
                                         HandlePreviousResult "EOp ToStrExceptStr default"<| \env newOpArg ->
                                           updateResult env <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                     e -> UpdateError <| "[internal error] Wrong number of arguments in update ToStrExceptStr: " ++ toString e
                         e -> UpdateError <| "Expected string, got " ++ toString e
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
                        Err msg -> UpdateError <| "Could not parse new output value '"++s++"' for ToStr expression " ++ toString msg
                        Ok parsed ->
                          case doEval Syntax.Elm [] parsed of
                            Err msg -> UpdateError msg
                            Ok ((v, _), _) ->
                              case (opArgs, vs) of
                                ([opArg], [arg]) -> updateContinue env opArg arg v <|
                                    HandlePreviousResult "EOp ToStr"<| \env newOpArg ->
                                      updateResult env <| replaceE__ e <| EOp sp1 op [newOpArg] sp2
                                e -> UpdateError <| "[internal error] Wrong number of arguments in update: " ++ toString e
                    e -> UpdateError <| "Expected string, got " ++ toString e
                _ ->
                  case maybeUpdateMathOp op vs oldVal newVal of
                    Errs msg -> UpdateError msg
                    Oks LazyList.Nil -> UpdateError "[Internal error] No result for updating."
                    Oks (LazyList.Cons head lazyTail) ->
                      let headToUpdateStack: Int -> List Val-> Lazy.Lazy (LazyList (List Val)) -> UpdateStack
                          headToUpdateStack nth head lazyTail =
                            updateContinueMultiple ("op " ++ toString nth ++ "th") env (Utils.zip3 opArgs vs head) (\newEnv newOpArgs ->
                              --let _ = Debug.log ("before an alternative " ++ (String.join "," <| List.map valToString head)) () in
                              UpdateResultAlternative "UpdateResultAlternative maybeOp" (updateResult newEnv (replaceE__ e <| EOp sp1 op newOpArgs sp2))
                                (lazyTail |> Lazy.map (\ll ->
                                  --let _ = Debug.log ("Starting to evaluate another alternative if it exists ") () in
                                  case ll of
                                    LazyList.Nil -> Nothing
                                    LazyList.Cons newHead newLazyTail -> Just <| headToUpdateStack (nth + 1) newHead newLazyTail
                                )))
                      in headToUpdateStack 1 head lazyTail

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

-- Tri combine only checks dependencies that may have been changed.
triCombine: Exp -> Env -> Env -> Env -> Env
triCombine origExp originalEnv newEnv1 newEnv2 =
  let fv = LangTools.freeIdentifiers origExp in
  let aux acc originalEnv newEnv1 newEnv2 =
    case (originalEnv, newEnv1, newEnv2) of
        ([], [], []) -> acc
        ((x, v1)::oe, (y, v2)::ne1, (z, v3)::ne2) ->
          if x /= y || y /= z || x /= z then
            Debug.crash <| "Expected environments to have the same variables, got\n" ++
             toString x ++ ", " ++ toString y ++ ", " ++ toString z ++ " = " ++ valToString v1 ++ ",\n" ++
             valToString v2 ++ ",\n" ++
             valToString v3
          else
            if not (Set.member x fv) || not (valEqual v2 v1) then aux (acc ++ [(x, v2)]) oe ne1 ne2
            else aux (acc ++ [(x, v3)]) oe ne1 ne2
        _ -> Debug.crash <| "Expected environments to have the same size, got\n" ++
             toString originalEnv ++ ", " ++ toString newEnv1 ++ ", " ++ toString newEnv2
    in
  aux [] originalEnv newEnv1 newEnv2

-- Constructor for updating multiple expressions evaluated in the same environment.
-- TODO: Once triCombine is moved into aother file, move this method to UpdateStack
updateContinueMultiple: String -> Env -> List (Exp, PrevOutput, Output) -> (Env -> List Exp -> UpdateStack) -> UpdateStack
updateContinueMultiple msg env totalExpValOut continuation  =
  let aux i revAccExps envAcc expValOut =
        case expValOut of
          [] -> continuation envAcc (List.reverse revAccExps)
          (e, v, out)::tail ->
            updateContinue env e v out <|
              HandlePreviousResult (toString i ++ "/" ++ toString (List.length totalExpValOut) ++ " " ++ msg)  <| \newEnv newExp ->
                let newEnvAcc = triCombine newExp env envAcc newEnv in
                aux (i + 1) (newExp::revAccExps) newEnvAcc tail
  in aux 1 [] env totalExpValOut

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

envToString: Env -> String
envToString env =
  case env of
    [] -> ""
    (v, value)::tail -> "\n"  ++ v ++ "->\n" ++ (valToString value) ++ (envToString tail)

-- Equality checking
valEqual: Val -> Val -> Bool
valEqual v1 v2 = --let _ = Debug.log "valEqual of " (valToString v1, valToString v2) in
  valToString v1 == valToString v2
  {--case (v1.v_ , v2.v_) of
  (VConst _ (n1, _), VConst _ (n2, _)) -> n1 == n2
  (VBase vb1, VBase vb2) -> vb1 == vb2
  (VClosure nm1 p1 body1 env1, VClosure nm2 p2 body2 env2 ) ->
    nm1 == nm2 && listForAll2 patEqual p1 p2 && expEqual body1 body2 && envEqual (pruneEnv body1 env1) (pruneEnv body2 env2)
  (VList v1s, VList v2s) -> listForAll2 valEqual v1s v2s
  _ -> False--}

envEqual: Env -> Env -> Bool
envEqual env1 env2 = --let _ = Debug.log "envEqual " () in
  listForAll2 (\(x1, v1) (x2, v2) -> x1 == x2 && valEqual v1 v2) env1 env2

wsEqual: WS -> WS -> Bool
wsEqual ws1 ws2 = ws1.val == ws2.val

patEqual: Pat -> Pat -> Bool
patEqual p1_ p2_ = --let _ = Debug.log "patEqual " (unparsePattern p1_, unparsePattern p2_) in
  unparsePattern p1_ == unparsePattern p2_
{--  case (p1_.val.p__, p2_.val.p__) of
  (PVar sp1 ident1 _,PVar sp2 ident2 _) -> wsEqual sp1 sp2 && ident1 == ident2
  (PConst sp1 num1, PConst sp2 num2)  -> wsEqual sp1 sp2 && num1 == num2
  (PBase sp1 bv1, PBase sp2 bv2) -> wsEqual sp1 sp2 && bv1 == bv2
  (PList sp1 pats sp2 mpat sp3,PList sp4 pats2 sp5 mpat2 sp6) ->
    wsEqual sp1 sp4 && wsEqual sp2 sp5 && wsEqual sp3 sp6 && listForAll2 patEqual pats pats2
    && (case (mpat, mpat2) of
      (Nothing, Nothing) -> True
      (Just p1, Just p2) -> patEqual p1 p2
      _ -> False
    )
  (PAs sp1 name sp2 p1,PAs sp3 name2 sp4 p2) -> wsEqual sp1 sp3 && name == name2 && wsEqual sp2 sp4 && patEqual p1 p2
  (PParens sp1 p1 sp2,PParens sp3 p2 sp4) -> wsEqual sp1 sp3 && patEqual p1 p2 && wsEqual sp2 sp4
  _ -> False
  --}

branchEqual: Branch -> Branch -> Bool
branchEqual b1 b2 = case (b1.val, b2.val) of
  (Branch_ sp1 p1 e1 sp2, Branch_ sp3 p2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual e1 e2 && patEqual p1 p2

tbranchEqual: TBranch -> TBranch -> Bool
tbranchEqual t1 t2 = case (t1.val, t2.val) of
  (TBranch_ sp1 ty1 e1 sp2, TBranch_ sp3 ty2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual e1 e2 && typeEqual ty1 ty2

listForAll2: (a -> a -> Bool) -> List a -> List a -> Bool
listForAll2 f l1 l2 = case l1 of
  [] -> case l2 of
    [] -> True
    _ -> False
  h1::t1 -> case l2 of
    [] -> False
    h2::t2 -> if f h1 h2 then listForAll2 f t1 t2 else False

typeEqual: Type -> Type -> Bool
typeEqual ty1 ty2 = --let _ = Debug.log "typeEqual " (ty1, ty2) in
  case (ty1.val, ty2.val) of
  (TNum sp1, TNum sp2) -> wsEqual sp1 sp2
  (TBool sp1, TBool sp2) -> wsEqual sp1 sp2
  (TString sp1, TString sp2) -> wsEqual sp1 sp2
  (TNull sp1, TNull sp2) -> wsEqual sp1 sp2
  (TList sp1 t1 sp2, TList sp3 t2 sp4) ->  wsEqual sp1 sp3 && wsEqual sp2 sp4 && typeEqual t1 t2
  (TDict sp1 tk tv sp2, TDict sp3 tk2 tv2 sp4) -> wsEqual sp1 sp3 && wsEqual sp2 sp4 && typeEqual tk tk2 && typeEqual tv tv2
  (TTuple sp1 args sp2 mTail sp2e, TTuple sp3 args2 sp4 mTail2 sp4e) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && wsEqual sp2e sp4e &&
        listForAll2 typeEqual args args2 &&
        ( case (mTail, mTail2) of
          (Nothing, Nothing) -> True
          (Just t1, Just t2) -> typeEqual t1 t2
          _ -> False
        )
  (TArrow sp1 types1 sp2, TArrow sp3 types2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4  &&
      listForAll2 typeEqual types1 types2
  (TUnion sp1 types1 sp2, TUnion sp3 types2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4  &&
          listForAll2 typeEqual types1 types2
  (TNamed sp1 ident1, TNamed sp2 ident2) ->
    wsEqual sp1 sp2 && ident1 == ident2
  (TVar sp1 ident1, TVar sp2 ident2) ->
    wsEqual sp1 sp2 && ident1 == ident2
  (TForall sp1 ts1 t1 sp2, TForall sp3 ts2 t2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    ( case (ts1, ts2) of
        (One (sp1, a1), One (sp2, a2)) -> wsEqual sp1 sp2 && a1 == a2
        (Many sp1 elems sp2, Many sp3 elems2 sp4) -> wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
          listForAll2 (\(sp1, a1) (sp2, a2) -> wsEqual sp1 sp2 && a1 == a2) elems elems2
        _ -> False
    ) && typeEqual t1 t2
  (TWildcard sp1, TWildcard sp2) -> wsEqual sp1 sp2
  _ -> False

expEqual: Exp -> Exp -> Bool
expEqual e1_ e2_ =
  --let _ = Debug.log "expEqual " (unparse e1_, unparse e2_) in
  unparse e1_ == unparse e2_
{--
  case (e1_.val.e__, e2_.val.e__) of
  (EConst sp1 num1 _ _, EConst sp2 num2 _ _) -> wsEqual sp1 sp2 && num1 == num2
  (EBase sp1 bv1, EBase sp2 bv2) -> wsEqual sp1 sp2 && bv1 == bv2
  (EVar sp1 id1, EVar sp2 id2) -> wsEqual sp1 sp2 && id1 == id2
  (EFun sp1 pats body sp2, EFun sp3 pats2 body2 sp4) -> wsEqual sp1 sp3 &&
    listForAll2 patEqual pats pats2 &&
    expEqual body body2 &&
    wsEqual sp2 sp4
  (EApp sp1 fun args sp2, EApp sp3 fun2 args2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual fun fun2 &&
    listForAll2 expEqual args args2
  (EOp sp1 op1 args sp2, EOp sp3 op2 args2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && op1.val == op2.val &&
    listForAll2 expEqual args args2
  (EList sp1 args sp2 mTail sp2e, EList sp3 args2 sp4 mTail2 sp4e) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && wsEqual sp2e sp4e &&
    listForAll2 expEqual args args2 &&
    ( case (mTail, mTail2) of
      (Nothing, Nothing) -> True
      (Just t1, Just t2) -> expEqual t1 t2
      _ -> False
    )
  (EIf sp11 cond1 sp12 then1 sp13 else1 sp14, EIf sp21 cond2 sp22 then2 sp23 else2 sp4) ->
    wsEqual sp11 sp21 &&
    wsEqual sp12 sp22 &&
    wsEqual sp13 sp23 &&
    wsEqual sp14 sp24 &&
    expEqual cond1 cond2 && expEqual then1 then2 && expEqual else1 else2
  (ECase sp1 input1 branches1 sp2, ECase sp3 input2 branches2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    expEqual input1 input2 &&
    listForAll2 branchEqual branches1 branches2
  (ETypeCase sp1 input1 tbranches1 sp2, ETypeCase sp3 input2 tbranches2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    expEqual input1 input2 &&
    listForAll2 tbranchEqual tbranches1 tbranches2

  (ELet sp11 lk1 rec1 pat1 sp12 exp1 sp13 body1 sp14, ELet sp21 lk2 rec2 pat2 sp22 exp2 sp23 body2 sp24) ->
    wsEqual sp11 sp21 &&
    wsEqual sp12 sp22 &&
    wsEqual sp13 sp23 &&
    wsEqual sp14 sp24 &&
    lk1 == lk2 && rec1 == rec2 &&
    patEqual pat1 pat2 && expEqual body1 body2 && expEqual exp1 exp2
  (EComment sp1 s1 e1, EComment sp2 s2 e2) ->
    wsEqual sp1 sp2 && s1 == s2 && expEqual e1 e2
  (EOption sp1 wStr1 sp2 wStr2 exp1, EOption sp3 wStr3 sp4 wStr4 exp2) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    wStr1.val == wStr3.val && wStr2.val == wStr4.val &&
    expEqual exp1 exp2
  (ETyp sp1 pat1 t1 e1 sp2, ETyp sp3 pat2 t2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    patEqual pat1 pat2 &&
    typeEqual t1 t2 &&
    expEqual e1 e2
  (EColonType sp1 e1 sp2 t1 sp2e, EColonType sp3 e2 sp4 t2 sp4e) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && wsEqual sp2e sp4e &&
    expEqual e1 e2 && typeEqual t1 t2
  (ETypeAlias sp1 pat1 t1 e1 sp2, ETypeAlias sp3 pat2 t2 e2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    patEqual pat1 pat2 && expEqual e1 e2 && typeEqual t1 t2
  (EParens sp1 e1 pStyle1 sp2, EParens sp3 e2 pStyle2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 && expEqual e1 e2 && pStyle
  (EHole sp1 (Just v1), EHole sp2 (Just v2)) ->
    wsEqual sp1 sp2 && valEqual v1 v2
  (EHole sp1 Nothing, EHole sp2 Nothing) ->
    wsEqual sp1 sp2
  _ -> False
--}
{-
compareExp:     (WS -> WS -> comparison)
            -> (Num -> Num -> comparison)
            -> (EBaseVal -> EBaseVal -> comparison)
            -> (String -> String -> comparison) --Identifiers
            -> (Pat -> Pat -> comparison)
            -> (Exp -> Exp -> comparison) -- General case when the exps don't match.
            -> (List comparison -> comparison)
            -> Exp -> Exp -> comparison
compareExp wsCmp numCmp bvCmp idCmp patCmp defaultCmp fold e1_ e2_ =
  let recurse = compareExp wsCmp numCmp bvCmp idCmd patCmp defaultCmp fold in
  --let _ = Debug.log "expEqual " (unparse e1_, unparse e2_) in
  case (e1_.val.e__, e2_.val.e__) of
  (EConst sp1 num1 _ _, EConst sp2 num2 _ _) -> fold [wsCmp sp1 sp2, numCmp num1 num2]
  (EBase sp1 bv1, EBase sp2 bv2) -> fold [wsCmp sp1 sp2, bvCmp bv1 bv2]
  (EVar sp1 id1, EVar sp2 id2) -> fold [wsCmp sp1 sp2, idCmp id1 id2]
  (EFun sp1 pats body sp2, EFun sp3 pats2 body2 sp4) -> fold ([wsCmp sp1 sp3, wsCmp sp2 sp4] ++ (List.map2 patCmp pats pats2) ++ [recurse body body2])
  (EApp sp1 fun args sp2, EApp sp3 fun2 args2 sp4) -> fold ([wsCmp sp1 sp3, wsCmp sp2 sp4] ++ [recurse fun fun2] ++ List.map2 recurse args args2)
  (EOp sp1 op1 args sp2, EOp sp3 op2 args2 sp4) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 && op1.val == op2.val &&
    listForAll2 expEqual args args2
  (EList sp1 args sp2 mTail sp2e, EList sp3 args2 sp4 mTail2 sp4e) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 && wsCmp sp2e sp4e &&
    listForAll2 expEqual args args2 &&
    ( case (mTail, mTail2) of
      (Nothing, Nothing) -> True
      (Just t1, Just t2) -> expEqual t1 t2
      _ -> False
    )
  (EIf sp11 cond1 sp12 then1 sp13 else1 sp14, EIf sp21 cond2 sp22 then2 sp23 else2 sp4) ->
    wsCmp sp11 sp21 &&
    wsCmp sp12 sp22 &&
    wsCmp sp13 sp23 &&
    wsCmp sp14 sp24 &&
    expEqual cond1 cond2 && expEqual then1 then2 && expEqual else1 else2
  (ECase sp1 input1 branches1 sp2, ECase sp3 input2 branches2 sp4) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 &&
    expEqual input1 input2 &&
    listForAll2 branchEqual branches1 branches2
  (ETypeCase sp1 input1 tbranches1 sp2, ETypeCase sp3 input2 tbranches2 sp4) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 &&
    expEqual input1 input2 &&
    listForAll2 tbranchEqual tbranches1 tbranches2

  (ELet sp11 lk1 rec1 pat1 sp12 exp1 sp13 body1 sp14, ELet sp21 lk2 rec2 pat2 sp22 exp2 sp23 body2 sp24) ->
    wsCmp sp11 sp21 &&
    wsCmp sp12 sp22 &&
    wsCmp sp13 sp23 &&
    wsCmp sp14 sp24 &&
    lk1 == lk2 && rec1 == rec2 &&
    patEqual pat1 pat2 && expEqual body1 body2 && expEqual exp1 exp2
  (EComment sp1 s1 e1, EComment sp2 s2 e2) ->
    wsCmp sp1 sp2 && s1 == s2 && expEqual e1 e2
  (EOption sp1 wStr1 sp2 wStr2 exp1, EOption sp3 wStr3 sp4 wStr4 exp2) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 &&
    wStr1.val == wStr3.val && wStr2.val == wStr4.val &&
    expEqual exp1 exp2
  (ETyp sp1 pat1 t1 e1 sp2, ETyp sp3 pat2 t2 e2 sp4) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 &&
    patEqual pat1 pat2 &&
    typeEqual t1 t2 &&
    expEqual e1 e2
  (EColonType sp1 e1 sp2 t1 sp2e, EColonType sp3 e2 sp4 t2 sp4e) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 && wsCmp sp2e sp4e &&
    expEqual e1 e2 && typeEqual t1 t2
  (ETypeAlias sp1 pat1 t1 e1 sp2, ETypeAlias sp3 pat2 t2 e2 sp4) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 &&
    patEqual pat1 pat2 && expEqual e1 e2 && typeEqual t1 t2
  (EParens sp1 e1 pStyle1 sp2, EParens sp3 e2 pStyle2 sp4) ->
    wsCmp sp1 sp3 && wsCmp sp2 sp4 && expEqual e1 e2 && pStyle
  (EHole sp1 (Just v1), EHole sp2 (Just v2)) ->
    wsCmp sp1 sp2 && valEqual v1 v2
  (EHole sp1 Nothing, EHole sp2 Nothing) ->
    wsCmp sp1 sp2
  _ -> False

bvToString: EBaseVal -> String
bvToString b = unparse <| withDummyExpInfo <| EBase space0 <| b

-- Reports the difference between two expressions
diffs: Exp -> Exp -> List String
diffs e1_ e2_ =
  compareExp (\ws1 ws2 -> if ws1.val == ws2.val then [] else ["Whitespace:'"++ws1.val++"' -> '"++ws2.val++"'"])
     (\n1 n2 -> if n1 == n2 then [] else [toString n1 ++ " -> " ++ toString n2])
     (\bv1 bv2 -> if bv1 == bv2 then [] else
       [bvToString bv1 ++ " -> " ++ bvToString bv2])
     (\id1 id2 -> if id1 == id2 then [] else [id1 ++ " -> " ++ id2])
     (\p1 p2 -> if patEqual p1 p2 then [] else [unparsePattern p1 ++ " -> " ++ unparsePattern p2])
     (\e1 e2 -> if expEqqual e1 e2 then [] else [unparse e1 ++ " -> " ++ unparse e2])
     (\lString -> List.concatMap identity lString)
 -}
diffs: Exp -> Exp -> String
diffs e1 e2 =
   let s1 = unparse e1 in
   let s2 = unparse e2 in
   let pref = commonPrefix s1 s2 in
   let s1minusPref = String.dropLeft (String.length pref - 2) s1 in
   let s2minusPref = String.dropLeft (String.length pref - 2) s2 in
   let suff = commonSuffix s1minusPref s2minusPref in
   let s1minusSuff = String.dropRight (String.length suff - 2) s1minusPref in
   let s2minusSuff = String.dropRight (String.length suff - 2) s2minusPref in
   s1minusSuff ++ " -> " ++ s2minusSuff

--------------------------------------------------------------------------------
-- Value builders with dummy ids, brought back from the dead in Lang

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
    |> Result.mapError (\e -> toString e)
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
