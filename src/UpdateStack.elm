module UpdateStack exposing  (..)
import Lang exposing (..)
import Results exposing
  ( Results
  , ok1, oks, okLazy
  )
import LazyList exposing (..)
import Lazy
import Syntax
import ValUnparser exposing (strVal)
import UpdateUtils exposing (..)
import Utils exposing (reverseInsert)
import LangUtils exposing (envToString, valToString)
import Set exposing (Set)
import UpdatedEnv exposing (UpdatedEnv, original)
import Pos exposing (Pos)


type alias UpdatedExp = { val: Exp, changes: Maybe EDiffs }
type alias UpdatedExpTuple = { val: List Exp, changes: Maybe (TupleDiffs EDiffs) }

type alias PrevLets = Env -- In traversing order

type HandlePreviousResult = HandlePreviousResult String (UpdatedEnv -> UpdatedExp -> UpdateStack)
type Fork = Fork String UpdateStack (LazyList HandlePreviousResult) (LazyList Fork)

type UpdateStack = UpdateResultS     UpdatedEnv UpdatedExp (Maybe HandlePreviousResult)
                 | UpdateContextS    Env Exp PrevLets PrevOutput Output VDiffs (Maybe HandlePreviousResult)
                 | UpdateResultAlternative String UpdateStack (Lazy.Lazy (Maybe UpdateStack))
                 | UpdateFails String -- Soft fails, might try other branches. If no branch is available, will report this error.
                 | UpdateCriticalError String
                 -- The new expression, the new output, what to add to the stack with the result of the update above.


updatedExpToString: Exp -> UpdatedExp -> String
updatedExpToString e ue =
  ue.changes |> Maybe.map (UpdateUtils.eDiffsToString "" e ue.val) |> Maybe.withDefault "<no change>"

updatedExpToStringWithPositions: Exp -> UpdatedExp -> (String, List Exp)
updatedExpToStringWithPositions e ue =
  ue.changes |> Maybe.map (UpdateUtils.eDiffsToStringPositions ElmSyntax "" (Pos 0 0, (0, 0)) e ue.val >> (\(msg, (_, l)) -> (msg, l))) |> Maybe.withDefault ("<no change>", [])

handleResultToString_: String -> HandlePreviousResult -> String
handleResultToString_ indent handleResult = case handleResult of
  HandlePreviousResult msg _ -> "\n" ++ indent ++ "Prev " ++ msg

--  Fork msg u actions -> "\n" ++ indent ++ "Fork["++ updateStackName_ (indent ++ " ") u ++" => "  ++
--      String.join ", " (List.map (handleResultToString_ (indent ++ " ")) (LazyList.toList actions)) ++ "] " ++ msg

updateStackName: UpdateStack ->  String
updateStackName = updateStackName_ ""

updateStackName_: String -> UpdateStack ->  String
updateStackName_ indent u = case u of
  UpdateResultS _ exp mb -> "\n" ++ indent ++ "Res(" ++Syntax.unparser Syntax.Elm exp.val ++ (Maybe.map (handleResultToString_ indent) mb |> Maybe.withDefault "") ++ ")"
  UpdateContextS _ exp _ _ o _ (Just n) -> "\n" ++ indent ++ "Contn(" ++Syntax.unparser Syntax.Elm exp ++ " <-- " ++ outputToString o ++ ")[" ++ handleResultToString_ (indent ++ " ") n ++ "]"
  UpdateContextS _ e _ _ o _ Nothing->   "\n" ++ indent ++ "Ctx " ++ Syntax.unparser Syntax.Elm e ++ "<--" ++ outputToString o
  UpdateResultAlternative msg u ll -> "\n" ++ indent ++ "Alt("++updateStackName u ++") then [" ++ (case Lazy.force ll of
      Just u -> updateStackName_ (indent ++ " ") u ++ "]"
      Nothing -> "]"
    )
  UpdateFails msg  -> "\n" ++ indent ++ "UpdateFails " ++ msg
  UpdateCriticalError msg -> "\n" ++ indent ++ "UpdateCriticalError " ++ msg


updateResultSameEnvExp: Env -> Exp -> UpdateStack
updateResultSameEnvExp env exp = updateResult (UpdatedEnv.original env) (UpdatedExp exp Nothing)

updateResultSameEnv: Env -> Exp -> UpdateStack
updateResultSameEnv env exp = updateResult (UpdatedEnv.original env) (UpdatedExp exp (Just <| EConstDiffs EAnyDiffs))

updateResultSameEnvDiffs: Env -> Exp -> EDiffs-> UpdateStack
updateResultSameEnvDiffs env exp diffs = updateResult (UpdatedEnv.original env) (UpdatedExp exp (Just <| diffs))


updateResult: UpdatedEnv-> UpdatedExp -> UpdateStack
updateResult updatedEnv exp = UpdateResultS updatedEnv exp Nothing

updateContinue: String -> Env -> Exp -> PrevLets -> PrevOutput -> Output -> VDiffs -> (UpdatedEnv -> UpdatedExp -> UpdateStack) -> UpdateStack
updateContinue msg env exp prevLets oldVal newVal newValDiffs continuation = UpdateContextS env exp prevLets oldVal newVal newValDiffs (Just (HandlePreviousResult msg <| continuation))

updateContext: String -> Env -> Exp -> PrevLets -> PrevOutput -> Output -> VDiffs -> UpdateStack
updateContext msg env exp prevLets oldVal newVal newValDiffs = UpdateContextS env exp prevLets oldVal newVal newValDiffs Nothing

type alias Output = Val
type alias PrevOutput = Val

outputToString: Output -> String
outputToString = strVal

updateResults :  UpdateStack -> (Lazy.Lazy (LazyList UpdateStack)) -> UpdateStack
updateResults updateStack lazyAlternatives =
  UpdateResultAlternative "fromUpdateREsult" updateStack (lazyAlternatives |> Lazy.map (\alternatives ->
    case alternatives of
      LazyList.Nil -> Nothing
      LazyList.Cons uStack lazyTail ->
        Just (updateResults uStack lazyTail)
  ))

updateResultList: String -> LazyList UpdateStack -> UpdateStack
updateResultList msg alternatives =
  case alternatives of
    LazyList.Nil -> UpdateFails msg
    LazyList.Cons uStack lazyTail -> updateResults uStack lazyTail

updateContinueRepeat: String -> Env -> Exp -> PrevLets -> PrevOutput -> Output -> Results String (Maybe VDiffs)->
                      (Lazy.Lazy (LazyList (Output, Results String (Maybe VDiffs)))) -> (UpdatedEnv -> UpdatedExp -> UpdateStack) -> UpdateStack
updateContinueRepeat msg env e prevLets oldVal newVal diffsResults otherNewValModifs continuation =
  updateMany diffsResults
    (\() -> continuation (UpdatedEnv.original env) (UpdatedExp e Nothing))
    <| \diffs ->
  updateContinue msg env e prevLets oldVal newVal diffs <| \newUpdatedEnv newUpdatedE ->
    UpdateResultAlternative
      ("Alternative to " ++ msg)
      (UpdateResultS newUpdatedEnv newUpdatedE <| Just (HandlePreviousResult ("alternative continuation to " ++ msg) continuation))
      (otherNewValModifs |> Lazy.map
        (\ll ->
          case ll of
            LazyList.Nil -> Nothing
            LazyList.Cons (head, headModifs) lazyTail ->
              Just <| updateContinueRepeat msg env e prevLets oldVal head headModifs lazyTail continuation
        )
    )

updateAlternatives: String -> Env -> Exp -> PrevLets -> PrevOutput -> LazyList (Output, Results String (Maybe VDiffs)) -> (UpdatedEnv -> UpdatedExp -> UpdateStack) -> UpdateStack
updateAlternatives msg env e prevLets oldVal newValsDiffs continuation =
  case newValsDiffs of
    LazyList.Nil -> UpdateFails <| "No solution for " ++ msg
    LazyList.Cons (head, headModifs) lazyTail -> updateContinueRepeat msg env e prevLets oldVal head headModifs lazyTail continuation

updateMaybeFirst: String -> (Maybe (UpdateStack, Bool)) -> (() -> UpdateStack) -> UpdateStack
updateMaybeFirst msg mb ll =
   case mb of
     Nothing -> ll ()
     Just (u, b) ->
       if b then
         UpdateResultAlternative msg u (Lazy.lazy <| (\_ -> Just <| ll ()))
       else
         u

updateMaybeFirst2: String -> Bool -> Maybe UpdateStack -> (() -> Maybe UpdateStack) -> Maybe (UpdateStack, Bool)
updateMaybeFirst2 msg canContinueAfter mb ll =
   case mb of
     Nothing -> ll () |> Maybe.map (\u -> (u, canContinueAfter))
     Just u ->
       Just <| (UpdateResultAlternative msg u (Lazy.lazy <| (\_ -> ll ())), canContinueAfter)



-- Constructor for updating multiple expressions evaluated in the same environment.
updateContinueMultiple: String -> Env -> PrevLets -> List (Exp, PrevOutput, Output) -> TupleDiffs VDiffs -> (UpdatedEnv -> UpdatedExpTuple -> UpdateStack) -> UpdateStack
updateContinueMultiple  msg       env    prevLets     totalExpValOut                    diffs                continuation  =
  let totalExp = withDummyExpInfo <| EList space0 (totalExpValOut |> List.map (\(e, _, _) -> (space1, e))) space0 Nothing space0 in
  let aux: Int -> List Exp   -> TupleDiffs EDiffs -> UpdatedEnv -> List (Exp, PrevOutput, Output) ->TupleDiffs VDiffs -> UpdateStack
      aux  i      revAccExps    revAccEDiffs         updatedEnvAcc expValOut                        diffs =
        case diffs of
         [] ->
           let finalExpTupleDiffs = case List.reverse revAccEDiffs of
             [] -> Nothing
             l -> Just l
           in
           continuation updatedEnvAcc (UpdatedExpTuple (List.reverse (Utils.reverseInsert (List.map (\(e, _, _) -> e) expValOut) revAccExps)) finalExpTupleDiffs)
         (j, m) :: td ->
            if j > i then
              let (unchanged, remaining) = Utils.split (j - i) expValOut in
              let unchangedExps = unchanged |> List.map (\(e, _, _) -> e) in
               aux j (Utils.reverseInsert unchangedExps revAccExps) revAccEDiffs updatedEnvAcc remaining diffs
            else if j < i then Debug.crash <| "Unexpected modification index : " ++ toString j ++ ", expected " ++ toString i ++ " or above."
            else
              case expValOut of
                (e, oldOut, newOut)::tail ->
                  updateContinue (toString (i + 1) ++ "/" ++ toString (List.length totalExpValOut) ++ " " ++ msg) env e prevLets oldOut newOut m <|  \newUpdatedEnv newUpdatedExp ->
                    --let _ = Debug.log "started tricombine" () in
                    let newUpdatedEnvAcc = UpdatedEnv.merge e m env updatedEnvAcc newUpdatedEnv in
                    let newRevAccEDiffs = case newUpdatedExp.changes of
                      Nothing -> revAccEDiffs
                      Just d -> (i, d)::revAccEDiffs
                    in
                    --let _ = Debug.log "Finished tricombine" () in
                    aux (i + 1) (newUpdatedExp.val::revAccExps) newRevAccEDiffs newUpdatedEnvAcc tail td
                [] -> Debug.crash <| msg ++
                   "Expected at least one element because it was modified at index " ++ toString j ++
                   ", got nothing. We are at index " ++ toString i ++ " / length = " ++ toString (List.length totalExpValOut)
  in aux 0 [] [] (UpdatedEnv.original env) totalExpValOut diffs

updateManyMbHeadTail: a -> Lazy.Lazy (LazyList (Maybe a)) -> (a -> UpdateStack) -> UpdateStack
updateManyMbHeadTail  firstdiff otherdiffs builder =
  updateResults (builder firstdiff) (otherdiffs |> Lazy.map (\otherdiffs -> otherdiffs |> LazyList.filterMap identity |> LazyList.map builder))

-- Constructor for combining multiple expressions evaluated in the same environment, when there are multiple values available.
updateOpMultiple: String-> Env -> List Exp -> (List Exp -> Exp) -> PrevLets -> List PrevOutput -> LazyList (List Output, Results String (Maybe (TupleDiffs VDiffs))) -> UpdateStack
updateOpMultiple  hint     env    es          eBuilder             prevLets     prevOutputs        outputs =
  {-let _ = Debug.log ("updateOpMultiple called with " ++ String.join "," (List.map (Syntax.unparser Syntax.Elm) es) ++
          "\nprevOutputs = " ++ (List.map valToString prevOutputs |> String.join ",") ++
          "\nupdates = \n<--" ++ (outputs |> LazyList.toList |> List.map (\(o, d) -> (List.map valToString o |> String.join ",") ++ " (diffs " ++ toString d++ ")" ) |> String.join "\n<-- ")
      ) () in-}
  let aux: Int -> List Output -> Results String (Maybe (TupleDiffs VDiffs))-> Lazy.Lazy (LazyList (List Output, Results String (Maybe (TupleDiffs VDiffs)))) -> UpdateStack
      aux  nth    outputsHead    diffResult                                   lazyTail =
    let continue = case diffResult of
       Err msg ->
         \continuation -> UpdateCriticalError msg
       Ok LazyList.Nil ->
         \continuation -> UpdateCriticalError "[internal error] Empty diffs in updateOpMultiple"
       Ok (LazyList.Cons Nothing _) ->
         \continuation -> continuation (UpdatedEnv.original env) (UpdatedExpTuple es Nothing)
       Ok (LazyList.Cons (Just d) tail) ->
         \continuation -> updateManyMbHeadTail d tail <| \diff ->
           updateContinueMultiple (hint ++ " #" ++ toString nth) env prevLets (Utils.zip3 es prevOutputs outputsHead) diff continuation
    in
       UpdateResultAlternative "UpdateResultAlternative maybeOp"
         (continue <| \newUpdatedEnv newUpdatedOpArgs ->
           let newUpdatedExp = UpdatedExp (eBuilder newUpdatedOpArgs.val) (Maybe.map EChildDiffs newUpdatedOpArgs.changes) in
           updateResult newUpdatedEnv newUpdatedExp)
         (lazyTail |> Lazy.map (\ll ->
           --let _ = Debug.log ("Starting to evaluate another alternative if it exists ") () in
           case ll of
             LazyList.Nil -> Nothing
             LazyList.Cons (newHead, newHeadDiffs) newLazyTail -> Just <| aux (nth + 1) newHead newHeadDiffs newLazyTail
         ))
  in
  case outputs of
    LazyList.Nil -> UpdateFails <| "[Internal error] No result for updating " ++ hint
    LazyList.Cons (outputsHead, headDiffs) lazyTail ->
      aux 1 outputsHead headDiffs lazyTail

updateMany: Results String (Maybe vdiffs) -> (() -> UpdateStack) -> (vdiffs -> UpdateStack) -> UpdateStack
updateMany diffResult onSame builder =
  case diffResult of
    Err msg -> UpdateCriticalError msg
    Ok ll ->
      case ll of
        LazyList.Nil -> UpdateCriticalError "[internal error] Expected at least one diff, got nothing"
        LazyList.Cons Nothing _ -> onSame ()
        LazyList.Cons (Just d) tail ->
          updateManyMbHeadTail d tail builder

updateManyHeadTail: a -> Lazy.Lazy (LazyList a) -> (a -> UpdateStack) -> UpdateStack
updateManyHeadTail  firstdiff otherdiffs builder =
  updateResults (builder firstdiff) (otherdiffs |> Lazy.map (\otherdiffs -> otherdiffs |> LazyList.map builder))

updateManys: Results String a -> (a -> UpdateStack) -> UpdateStack
updateManys diffResult builder =
  case diffResult of
    Err msg -> UpdateCriticalError msg
    Ok ll ->
      case ll of
        LazyList.Nil -> UpdateCriticalError "[internal error] Expected at least one diff, got nothing"
        LazyList.Cons l tail ->
          updateManyHeadTail l tail builder


-- Given an updated exp, allows to replace some inserted expressions (could be used for alignments)
replaceInsertions: UpdatedExp -> (Exp -> Maybe Exp) -> UpdatedExp
replaceInsertions e f =
  case e.changes of
    Nothing -> e
    Just d -> case d of
      EChildDiffs cd ->
        let (children, rebuilder) = childExpsExtractors e.val in
        let aux: Int -> List Exp -> TupleDiffs EDiffs -> List Exp -> UpdatedExp
            aux i children diffs revAccChildren = case diffs of
          [] -> UpdatedExp (rebuilder <| List.reverse <| reverseInsert children revAccChildren) e.changes
          (j, subd)::tail ->
             if j > i then
              let (childrenOk, childrenTail) = Utils.split (j - i) children in
              aux j childrenTail diffs (reverseInsert childrenOk revAccChildren)
             else -- j == i
              case children of
                hdchild :: tlchild ->
                  (replaceInsertions (UpdatedExp hdchild (Just subd)) f).val :: revAccChildren |>
                  aux (i + 1) tlchild tail
                _ ->
                  aux i children tail revAccChildren
        in aux 0 children cd []
      EListDiffs ld ->
        case e.val.val.e__ of
          EList sp0 children sp1 Nothing sp2 ->
            let aux: Int -> List (WS, Exp) -> ListDiffs EDiffs -> List (WS, Exp) -> UpdatedExp
                aux i children diffs revAccChildren = case diffs of
              [] -> UpdatedExp (replaceE__ e.val <| EList sp0 (List.reverse <| reverseInsert children revAccChildren) sp1 Nothing sp2) e.changes
              (j, subd)::tail ->
                 if j > i then
                   let count = j - i in
                   let (childrenOk, childrenTail) = Utils.split count children in
                   aux j childrenTail diffs (reverseInsert childrenOk revAccChildren)
                 else -- j == i
                   case subd of
                     ListElemUpdate d ->
                       case children of
                         (sp, hdchild) :: tlchild ->
                           (sp, (replaceInsertions (UpdatedExp hdchild (Just d)) f).val) :: revAccChildren |>
                           aux (i + 1) tlchild tail
                         _ ->
                           aux i children tail revAccChildren
                     ListElemDelete count -> -- They were deleted and hence they are no more.
                       aux (i + count) children tail revAccChildren
                     ListElemInsert count ->
                       let (inserted, rem) = Utils.split count children in
                       let insertedMapped = inserted |> List.map (\(sp, e) -> (sp, postMapExp f e)) in
                       reverseInsert insertedMapped revAccChildren |>
                       aux i rem tail
            in aux 0 children ld []
          _ -> e
      EConstDiffs _ -> e
      EStringDiffs _ -> e

postMapExp: (Exp -> Maybe Exp) -> Exp -> Exp
postMapExp f e =
  let (children, rebuilder) = childExpsExtractors e in
  let newChildren = List.map (postMapExp f) children in
  let newElem = rebuilder newChildren in
  (f newElem) |> Maybe.withDefault newElem


keepLets origEnv newEnv = List.reverse <| List.take (List.length newEnv - List.length origEnv) newEnv

prevLetsFind: (Val_ -> Val) -> PrevLets -> Pat-> Maybe (Val, Env)
prevLetsFind val_ env p =
  let deconstructEnv name env = case env of
    [] -> Nothing
    ((headname, headval)::tail) -> if headname == name then Just (headval, tail) else Nothing
  in
  let recurse: PrevLets -> Pat -> Maybe (Val, Env)
      recurse env p =
    case p.val.p__ of
       PVar _ name _ -> deconstructEnv name env
       PAs _ _ name _ inner -> deconstructEnv name env |> Maybe.andThen (\(v, env1) ->
           recurse env1 inner |> Maybe.map (\(_, newEnv) -> (v, newEnv))
         )
       PConst _ n -> Just <| (val_ <| VConst Nothing (n, dummyTrace), env)
       PBase _ (EBool b) -> Just <| (val_ <| VBase (VBool b), env)
       PBase _ (EString _ s) -> Just <| (val_ <| VBase (VString s), env)
       PBase _ _ -> Nothing
       PWildcard _ -> Nothing
       PParens _ innerP _ -> recurse env innerP
       PList _  [] _ mbtail _ -> case mbtail of
         Nothing -> Just <| (val_ <| VList [], env)
         Just inner -> recurse env inner
       PList sp0 (head::tail) sp1 mbtail sp2 ->
         recurse env head |> Maybe.andThen (\(v, env1) ->
           recurse env1 (replaceP__ p <| PList sp0 tail sp1 mbtail sp2) |> Maybe.andThen (\(v2, env2) ->
             vListUnapply v2 |> Maybe.map (\res -> (val_ <| VList (v::res), env2))
           )
         )
       PRecord _ mbinit elems -> Nothing -- we might be missing fields here, except it's a datatype constructor ?
  in
  recurse env p

