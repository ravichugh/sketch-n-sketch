module UpdatedEnv exposing (..)
import Lang exposing (..)
import UpdateUtils
import Utils
import LangUtils exposing (envToString, valEqual)
import Set exposing (Set)

-- Useful to merge environments faster.
-- Maybe will containn things like "insert a variable with these dependences"

type alias UpdatedEnv = { val: Env, indices: List Int }

-- Declares an environment as unmodified
original: Env -> UpdatedEnv
original env = UpdatedEnv env []

-- Merges two modified environments
merge: Env -> UpdatedEnv -> UpdatedEnv -> UpdatedEnv
merge env env1 env2 =
  let (finalEnv, finalIndices) = UpdateUtils.mergeEnv env env1.indices env1.val env2.indices env2.val in
  UpdatedEnv finalEnv finalIndices

-- Concatenates two modified environments, keeping track of where the modifications happened.
append: UpdatedEnv -> UpdatedEnv -> UpdatedEnv
append env1 env2 =
  let n = List.length env1.val in
  let indices2 = env2.indices |> List.map (\i -> i + n) in
  let env = env1.val ++ env2.val in
  UpdatedEnv env <| env1.indices ++ indices2

-- Returns the first n elements, and the remaining elements
split: Int -> UpdatedEnv -> (UpdatedEnv, UpdatedEnv)
split n env =
  let (indices1, indices2Offset) = Utils.spanWhile (\i -> i < n) env.indices in
  let (env1, env2) = Utils.split n env.val in
  let indices2 = List.map (\i -> i - n) indices2Offset in
  (UpdatedEnv env1 indices1, UpdatedEnv env2 indices2)

isUnmodified: UpdatedEnv -> Bool
isUnmodified menv = menv.indices |> List.isEmpty

show: UpdatedEnv -> String
show updatedEnv =
  let prunedEnv acc i m e = case (m, e) of
    ([], e) -> List.reverse acc
    (j::is, head::tail) -> if j == i then prunedEnv (head::acc) (i + 1) is tail else prunedEnv acc (i + 1) m tail
    (_, []) -> List.reverse acc
  in
  "modified:" ++ envToString (prunedEnv [] 0 updatedEnv.indices updatedEnv.val )

-- When comparing VClosures, how to get the modifications
create: Set Ident -> Env -> Env -> UpdatedEnv
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