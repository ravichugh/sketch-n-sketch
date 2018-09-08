module UpdatedEnv exposing (
  original, merge, recursiveMerge, split, isUnmodified, show, set, expandRecEnvReverse)
import Lang exposing (..)
import UpdateUtils
import Utils
import LangUtils exposing (envToString, valEqual)
import ValBuilder as Vb

-- Useful to merge environments faster.
-- Maybe will containn things like "insert a variable with these dependences"

-- This definition is now in Lang
-- type alias UpdatedEnv = { val: Env, changes: EnvDiffs }

-- Declares an environment as unmodified
original: Env -> UpdatedEnv
original env = UpdatedEnv env []

-- Merges two modified environments
merge: Env -> UpdatedEnv -> UpdatedEnv -> UpdatedEnv
merge env env1 env2 =
  if isUnmodified env1 then env2 else if isUnmodified env2 then env1 else
  let (finalEnv, finalChanges) = UpdateUtils.mergeEnv env env1.val env1.changes env2.val env2.changes in
  UpdatedEnv finalEnv finalChanges

recursiveMerge: Env -> List UpdatedEnv -> UpdatedEnv
recursiveMerge env modifiedEnvs = case modifiedEnvs of
  [] -> original env
  [head] -> head
  a::b::tail -> recursiveMerge env ((merge env a b)::tail)

set: String -> Val -> VDiffs -> UpdatedEnv -> UpdatedEnv
set key newVal newDiff updatedEnv =
  let aux: Env -> EnvDiffs -> (Env, EnvDiffs)
      aux env envDiffs = case env of
        [] -> ([], envDiffs)
        (ke, ve)::envTail ->
          let newEnvDiffs = case envDiffs of
               [] -> [Just newDiff]
               headDiff :: tailDiffs -> Just newDiff :: tailDiffs
          in
          if ke == key then ((ke, newVal)::envTail, newEnvDiffs)
          else
            let (tailEnvDiffs, headDiff) = case envDiffs of
              [] -> ([], Nothing)
              mbd::tailDiffs -> (tailDiffs, mbd)
            in
            let (newTailEnv, newTailEnvDiffs) = aux  envTail tailEnvDiffs
            in
            ((ke, ve)::newTailEnv, headDiff :: newTailEnvDiffs)
  in
  let (newEnv, newEnvDiffs) = aux updatedEnv.val updatedEnv.changes in
  UpdatedEnv newEnv newEnvDiffs

--offset: Int -> EnvDiffs -> EnvDiffs
--offset = UpdateUtils.offset

-- Concatenates two modified environments, keeping track of where the modifications happened.
append: UpdatedEnv -> UpdatedEnv -> UpdatedEnv
append env1 env2 =
  let n = List.length env1.val in
  let env = env1.val ++ env2.val in
  let totalchanges = UpdateUtils.completeDiffs n env1.changes ++ env2.changes in
  UpdatedEnv env <| totalchanges

-- Returns the first n elements, and the remaining elements
split: Int -> UpdatedEnv -> (UpdatedEnv, UpdatedEnv)
split n env =
  let (changes1, changes2) = Utils.split n env.changes in
  let (env1, env2) = Utils.split n env.val in
  (UpdatedEnv env1 changes1, UpdatedEnv env2 changes2)

isUnmodified: UpdatedEnv -> Bool
isUnmodified menv = menv.changes |> List.isEmpty

show: UpdatedEnv -> String
show updatedEnv =
  let prunedEnv acc m e = case (m, e) of
    ([], e) -> List.reverse acc
    (mbD::is, head::tail) -> case mbD of
       Nothing -> prunedEnv acc m tail
       Just d -> prunedEnv (head::acc) is tail
    (_, []) -> List.reverse acc
  in
  "modified:" ++ envToString (prunedEnv [] updatedEnv.changes updatedEnv.val ) ++ "\n(" ++ toString updatedEnv.changes ++ ")"

-- When comparing VClosures, how to get the modifications
{-create: Set Ident -> Env -> Env -> UpdatedEnv
create ks oldEnv newEnv = --Very slow process, we need to optimize that
  let aux: Int -> Set Ident ->  List (Ident, Val) -> List Int -> Env -> Env -> UpdatedEnv
      aux  i      freeVariables accEnv               accModifs   oldEnv newEnv =
    if Set.isEmpty freeVariables then UpdatedEnv (List.reverse accEnv ++ newEnv) accModifs
    else case (oldEnv, newEnv) of
       ([], []) -> UpdatedEnv (List.reverse accEnv) (List.reverse accModifs)
       ((oldk, oldv)::oldtail, (newk, newv)::newtail) ->
         if oldk /= newk then Debug.crash <| "Comparing tow environments which do not have the same order of keys:" ++ oldk ++ " /= " ++ newk
         else if Set.member oldk freeVariables then
           let newModifs = if valEqual oldv newv then accModifs else i::accModifs in
           aux (i + 1) (Set.remove oldk freeVariables) ((newk, newv)::accEnv) newModifs oldtail newtail
         else aux (i + 1) freeVariables ((oldk, newv)::accEnv) accModifs oldtail newtail
       (_, _) -> Debug.crash <| "Comparing tow environments which do not have the same size" ++ envToString oldEnv ++ " /= " ++ envToString newEnv
  in aux 0 ks [] [] oldEnv newEnv
  -}

{-
expandRecEnv recNames closureEnv =
  let (nonRecEnv, remainingEnv) = Utils.split (List.length recNames) closureEnv in
  let recEnv2 = nonRecEnv |> List.map (\((name, v) as pair) -> case v.v_ of
       VClosure [] p b closedEnv -> (name, replaceV_ v <| VClosure recNames p b (nonRecEnv ++ closedEnv))
       _ -> pair
     )
  in
  recEnv2 ++ remainingEnv
-}

foldLeftWithDiff: a -> UpdatedEnv -> (a -> (String, Val) -> Maybe VDiffs -> a) -> a
foldLeftWithDiff acc updatedEnv lambda =
  let aux env changes acc = case env of
    [] -> acc
    entry :: envTail ->
       case changes of
        [] ->
          lambda acc entry Nothing |>
          aux envTail changes
        mbD::changesTail ->
          lambda acc entry mbD |>
          aux envTail changes
  in aux updatedEnv.val updatedEnv.changes acc

-- Update version of Lang.expandRecEnv. It merges any recursive modifications to the environment itself.
expandRecEnvReverse: List String -> Env -> UpdatedEnv -> UpdatedEnv
expandRecEnvReverse recNames closureEnv updatedExpandedEnv =
  let n = List.length recNames in
  let (nonRecEnv, remainingEnv) = Utils.split n closureEnv in
  let (updatedRecEnv2, updatedRemainingEnv) = split n updatedExpandedEnv in
  let (accNonRecEnv, revAccNameClosuresMbDiffs) =
     foldLeftWithDiff
        (original nonRecEnv, [])                        updatedRecEnv2 <|
       \(accNonRecEnv,                  revAccNameClosuresMbDiffs) (name, v) mbVDiffs -> case v.v_ of
       VClosure recNames p newBody newClosedEnv ->
         let newClosedEnvDiffs = case mbVDiffs of
           Just (VClosureDiffs envDiff _) -> envDiff
           _ -> []
         in
         let newBodyDiffs = case mbVDiffs of
           Just (VClosureDiffs _ bodyDiffs) -> bodyDiffs
           _ -> Nothing
         in
         let (updatedNonRecEnv, updatedRemainingEnv) = split n (UpdatedEnv newClosedEnv newClosedEnvDiffs) in
         let updatedBody = UpdatedExp newBody newBodyDiffs in
         let updatedClosure = updated.vClosure (Vb.fromVal v) [] p updatedBody updatedRemainingEnv in
         (merge nonRecEnv accNonRecEnv updatedNonRecEnv,
          (name, updatedClosure)::revAccNameClosuresMbDiffs)
       _ -> (accNonRecEnv, (name, UpdatedVal v mbVDiffs):: revAccNameClosuresMbDiffs)
  in
  let globalUpdatedNonRecEnv = updated.env <| List.reverse revAccNameClosuresMbDiffs in
  let finalUpdatedNonRecEnv = merge nonRecEnv accNonRecEnv globalUpdatedNonRecEnv in
  append finalUpdatedNonRecEnv updatedRemainingEnv

