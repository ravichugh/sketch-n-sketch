module UpdatedEnv exposing (UpdatedEnv, original, offset, merge, split, isUnmodified, show)
import Lang exposing (..)
import UpdateUtils exposing (EnvDiffs, VDiffs(VUnoptimizedDiffs))
import Utils
import LangUtils exposing (envToString, valEqual)
import Set exposing (Set)
import Dict exposing (Dict)
import UpdateUnoptimized

-- Useful to merge environments faster.
-- Maybe will containn things like "insert a variable with these dependences"

type alias UpdatedEnv = { val: Env, changes: EnvDiffs }

-- Declares an environment as unmodified
original: Env -> UpdatedEnv
original env = UpdatedEnv env []

-- Merges two modified environments
merge: Exp -> VDiffs -> Env -> UpdatedEnv -> UpdatedEnv -> UpdatedEnv
merge e vdiffs env env1 env2 =
  case vdiffs of
    VUnoptimizedDiffs ->
      let is = LangUtils.freeIdentifiers e in
      { val = UpdateUnoptimized.mergeEnvGeneral e is env env1.val env2.val, changes = [(0, VUnoptimizedDiffs)] }
    _ ->
      if isUnmodified env1 then env2 else if isUnmodified env2 then env1 else
      let (finalEnv, finalChanges) = UpdateUtils.mergeEnv env env1.val env1.changes env2.val env2.changes in
      UpdatedEnv finalEnv finalChanges

offset: Int -> EnvDiffs -> EnvDiffs
offset = UpdateUtils.offset

-- Concatenates two modified environments, keeping track of where the modifications happened.
append: UpdatedEnv -> UpdatedEnv -> UpdatedEnv
append env1 env2 =
  let n = List.length env1.val in
  let changes2 = offset n <| env2.changes in
  let env = env1.val ++ env2.val in
  UpdatedEnv env <| env1.changes ++ changes2

-- Returns the first n elements, and the remaining elements
split: Int -> UpdatedEnv -> (UpdatedEnv, UpdatedEnv)
split n env =
  let (changes1, changes2Offset) = Utils.spanWhile (\(i, _) -> i < n) env.changes in
  let (env1, env2) = Utils.split n env.val in
  let changes2 = offset (0 - n) changes2Offset in
  (UpdatedEnv env1 changes1, UpdatedEnv env2 changes2)

isUnmodified: UpdatedEnv -> Bool
isUnmodified menv = menv.changes |> List.isEmpty

show: UpdatedEnv -> String
show updatedEnv =
  let prunedEnv acc i m e = case (m, e) of
    ([], e) -> List.reverse acc
    ((j, d)::is, head::tail) -> if j == i then prunedEnv (head::acc) (i + 1) is tail else prunedEnv acc (i + 1) m tail
    (_, []) -> List.reverse acc
  in
  "modified:" ++ envToString (prunedEnv [] 0 updatedEnv.changes updatedEnv.val ) ++ "\n(" ++ toString updatedEnv.changes ++ ")"

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