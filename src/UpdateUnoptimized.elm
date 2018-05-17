module UpdateUnoptimized exposing (..)

import Lang exposing (..)
import Set exposing (Set)
import LangUtils exposing (valToString, valEqual, typeEqual, expEqual)
import Dict exposing (Dict)
import Syntax
import Info exposing (..)

-- For benchmarking only
mergeEnvGeneral: Exp -> Set Ident -> Env -> Env -> Env -> Env
mergeEnvGeneral origExp fv originalEnv newEnv1 newEnv2 =
  --let _ = Debug.log ("TriCombine starts on " ++ Syntax.unparser Syntax.Elm origExp) (envToString originalEnv, envToString newEnv1, envToString newEnv2) in
  let aux: Env -> Set Ident -> Env ->      Env ->  Env ->  Env
      aux  revAcc fv           originalEnv newEnv1 newEnv2 =
       --let _ = Debug.log "aux " (envToString acc, envToString originalEnv, envToString newEnv1, envToString newEnv2) in
       case (originalEnv, newEnv1, newEnv2) of
         ([], [], []) -> List.reverse revAcc
         ((x, v1)::oe, (y, v2)::ne1, (z, v3)::ne2) ->
           if x /= y || y /= z || x /= z then
             Debug.crash <| "Expected environments to have the same variables, got\n" ++
              x ++ " = " ++ valToString v1 ++ "\n" ++
              y ++ " = " ++ valToString v2 ++ "\n" ++
              z ++ " = " ++ valToString v3 ++ "\n" ++
              (List.take 5 originalEnv |> List.map Tuple.first |> String.join ",") ++ "\n" ++
               (List.take 5 newEnv1 |> List.map Tuple.first |> String.join ",") ++ "\n" ++
               (List.take 5 newEnv2 |> List.map Tuple.first |> String.join ",") ++ "\n" ++
               Syntax.unparser Syntax.Elm origExp
           else if Set.member x fv then
             aux ((x, mergeVal v1 v2 v3)::revAcc) (Set.remove x fv) oe ne1 ne2
           else
    --         let _ = Debug.log (x ++ " not member of free variables of " ++ Syntax.unparser Syntax.Elm origExp) "" in
             aux ((x, v1)::revAcc) fv oe ne1 ne2
         _ -> Debug.crash <| "Expected environments to have the same size, got\n" ++
              toString originalEnv ++ ", " ++ toString newEnv1 ++ ", " ++ toString newEnv2
       in
  aux [] fv originalEnv newEnv1 newEnv2 -- |> \x -> let _ = Debug.log "tricombine result" (envToString x) in x

-- Merges values using a diffing algorithm.
mergeVal: Val -> Val -> Val -> Val
mergeVal original modified1 modified2 =
  case (original.v_, modified1.v_, modified2.v_) of    -- TODO: Find multiple elem insertions and deletions
    (VBase (VString originalString), VBase (VString modified1String), VBase (VString modified2String)) ->
      replaceV_ original <| VBase (VString <| mergeString originalString modified1String modified2String)
    (VList originalElems, VList modified1Elems, VList modified2Elems) ->
      replaceV_ original <| VList <| mergeList mergeVal originalElems modified1Elems modified2Elems
    (VRecord originalDict, VRecord modified1Dict, VRecord modified2Dict) ->
      replaceV_ original <| VRecord <| mergeDict mergeVal originalDict modified1Dict modified2Dict
    (VDict originalDict, VDict modified1Dict, VDict modified2Dict) ->
      replaceV_ original <| VDict <| mergeDict mergeVal originalDict modified1Dict modified2Dict
    (VClosure mbRec0 pats0 body0 env0, VClosure mbRec1 pats1 body1 env1, VClosure mbRec2 pats2 body2 env2) ->
      if mbRec0 == mbRec1 && mbRec1 == mbRec2 then
        if patsEqual pats0 pats1 pats2 then
          let newEnv = mergeEnvGeneral body0 (Set.diff (LangUtils.freeIdentifiers body0) (LangUtils.identifiersSetInPats pats0)) env0 env1 env2 in
          let newBody = mergeExp body0 body1 body2 in
          replaceV_ original <| VClosure mbRec0 pats0 newBody newEnv
        else if valEqual original modified1 then modified2 else modified1
      --(VRecord originalElems, VRecord modified1Elems, VRecord modified2Elems)->
      --  Dict.keys originalElems
      else if valEqual original modified1 then modified2 else modified1
    _ ->
      --let _ = Debug.log ("mergeVal" ++ valToString original ++ " "  ++ valToString modified1 ++ " " ++ valToString modified2) " " in
      let result = if valEqual original modified1 then modified2 else modified1 in
      --let _ = Debug.log ("mergeVal=" ++ valToString result) "" in
      result


patsEqual: List Pat -> List Pat -> List Pat -> Bool
patsEqual pats1 pats2 pats3 = List.all identity <| List.map3 (\p0 p1 p2 -> patEqual p0 p1 && patEqual p1 p2) pats1 pats2 pats3

patEqual: Pat -> Pat -> Bool
patEqual p1 p2 = Syntax.patternUnparser Syntax.Elm p1 == Syntax.patternUnparser Syntax.Elm p2


mergeString: String -> String -> String -> String
mergeString o e1 e2 = if o == e1 then e2 else e1 -- Could do a better line-based diff.

-- Guarantees that
-- * If updated lists have equal size, elements will be merged aligned.
-- * A list which was not modified makes that the other lists replaces the original list.
-- Would be better to have a real diffing algorithm.
mergeList: (a -> a -> a -> a) -> List a -> List a -> List a -> List a
mergeList submerger =
  let aux: List a -> List a -> List a -> List a
      aux originals modified1 modified2 =
       case (originals, modified1, modified2) of
         ([], [], v) -> v -- Added elements
         ([], v, []) -> v -- Added elements
         ([], v1, v2) -> aux v1 v1 v2 -- Added elements
         (o, [], v) ->  -- Deleted orgs. Maybe v had insertions from o that we need to carry over ?
           {-if v == o then []
           else
             v
             |> List.filter (\vElem ->
                 not <|
                 List.any (valEqual vElem) o
               ) -- All the elements of v which do not belong to o-}
           []
         (o, v, []) ->
           []
           --aux originals modified2 modified1
         (ohd::otl, v1hd::v1tl, v2hd::v2tl) ->
           submerger ohd v1hd v2hd :: aux otl v1tl v2tl
  in aux


mergeDict: (v -> v -> v -> v) -> Dict k v -> Dict k v -> Dict k v -> Dict k v
mergeDict submerger originalDict modified1Dict modified2Dict =
  let originalElems = Dict.toList originalDict |> List.map (\(k, v) -> (k, Just v)) in
  let modified1Elems = originalElems |> List.map (\(k, v) -> (k, Dict.get k modified1Dict)) in
  let modified2Elems = originalElems |> List.map (\(k, v) -> (k, Dict.get k modified2Dict)) in
  let mergedKeyValues = mergeList (\o v1 v2 -> --Deletion only for now.
         case (o, v1, v2) of
           ((k, Nothing), _, _) -> Debug.crash "Impossible case in mergeVal"
           (_, (k, Nothing), _) -> (k, Nothing)
           (_, _, (k, Nothing)) -> (k, Nothing)
           ((k, Just jo), (_, Just jv1), (_, Just jv2)) -> (k, Just <| submerger jo jv1 jv2)
         ) originalElems modified1Elems modified2Elems
       |> List.filterMap (\(k, mb) -> Maybe.map (\v -> (k, v)) mb)
       |> Dict.fromList
  in
  let insertedKeyValues = Dict.union (Dict.diff modified1Dict originalDict) (Dict.diff modified2Dict originalDict) in
  Dict.union insertedKeyValues mergedKeyValues


mergeExp: Exp -> Exp -> Exp -> Exp
mergeExp o e1 e2 =
  let default () = (if expEqual o e1 then e2 else e1).val.e__ in
  let result = case (o.val.e__, e1.val.e__, e2.val.e__) of
       (EFun sp0 pats0 body0 esp0,
        EFun sp1 pats1 body1 esp1,
        EFun sp2 pats2 body2 esp2) ->
          if patsEqual pats0 pats1 pats2 then
            EFun (mergeWS sp0 sp1 sp2) pats0 (mergeExp body0 body1 body2) (mergeWS esp0 esp1 esp2)
          else default ()
       (EApp sp0 fun0 args0 appStyle1 esp0,
        EApp sp1 fun1 args1 appStyle2 esp1,
        EApp sp2 fun2 args2 appStyle3 esp2) ->
         EApp (mergeWS sp0 sp1 sp2) (mergeExp fun0 fun1 fun2) (mergeList mergeExp args0 args1 args2) appStyle1 (mergeWS esp0 esp1 esp2)

       (EOp sp0 spo0 op0 args0 esp0,
        EOp sp1 spo1 op1 args1 esp1,
        EOp sp2 spo2 op2 args2 esp2) ->
         if op0.val == op1.val && op1.val == op2.val then
           EOp (mergeWS sp0 sp1 sp2) (mergeWS spo0 spo1 spo2) op0 (mergeList mergeExp args0 args1 args2) (mergeWS esp0 esp1 esp2)
         else default ()

       (EList sp0 args0 isp0 mTail0 esp0,
        EList sp1 args1 isp1 mTail1 esp1,
        EList sp2 args2 isp2 mTail2 esp2) ->
         EList (mergeWS sp0 sp1 sp2) (mergeList (\(s0, v0) (s1, v1) (s2, v2) -> (mergeWS s0 s1 s2, mergeExp v0 v1 v2)) args0 args1 args2) (mergeWS isp0 isp1 isp2)
             (mergeMaybe mergeExp mTail0 mTail1 mTail2) (mergeWS esp0 esp1 esp2)
       (EIf spc0 cond0 spt0 then0 spe0 else0 esp0,
        EIf spc1 cond1 spt1 then1 spe1 else1 esp1,
        EIf spc2 cond2 spt2 then2 spe2 else2 esp2) ->
         EIf (mergeWS spc0 spc1 spc2)  (mergeExp cond0 cond1 cond2)
             (mergeWS spt0 spt1 spt2)  (mergeExp then0 then1 then2)
             (mergeWS spe0 spe1 spe2)  (mergeExp else0 else1 else2)
             (mergeWS esp0 esp1 esp2)
       (ECase sp0 input0 branches0 esp0,
        ECase sp1 input1 branches1 esp1,
        ECase sp2 input2 branches2 esp2) ->
         ECase (mergeWS sp0 sp1 sp2) (mergeExp input0 input1 input2)
               (mergeList mergeBranch branches0 branches1 branches2)
              (mergeWS esp0 esp1 esp2)
       (ETypeCase sp0 input0 tbranches0 esp0,
        ETypeCase sp1 input1 tbranches1 esp1,
        ETypeCase sp2 input2 tbranches2 esp2) ->
         ETypeCase (mergeWS sp0 sp1 sp2) (mergeExp input0 input1 input2)
           (mergeList mergeTBranch tbranches0 tbranches1 tbranches2)
           (mergeWS esp0 esp1 esp2)
       (ELet sp0 lk0 rec0 pat0 spi0 exp0 spj0 body0 esp0,
        ELet sp1 lk1 rec1 pat1 spi1 exp1 spj1 body1 esp1,
        ELet sp2 lk2 rec2 pat2 spi2 exp2 spj2 body2 esp2) ->
         if lk0 == lk1 && lk1 == lk2 then
           if rec0 == rec1 && rec1 == rec2 then
             if patEqual pat0 pat1 && patEqual pat1 pat2 then
               ELet (mergeWS sp0 sp1 sp2)
                    lk0 rec0 pat0
                    (mergeWS spi0 spi1 spi2)
                    (mergeExp exp0 exp1 exp2)
                    (mergeWS spj0 spj1 spj2)
                    (mergeExp body0 body1 body2)
                    (mergeWS esp0 esp1 esp2)
             else default ()
           else default ()
         else default ()
       (EOption sp0 kStr0 spi0 wStr0 exp0,
        EOption sp1 kStr1 spi1 wStr1 exp1,
        EOption sp2 kStr2 spi2 wStr2 exp2) ->
         EOption (mergeWS sp0 sp1 sp2)
                 (mergeInfo mergeString kStr0 kStr1 kStr2)
                 (mergeWS spi0 spi1 spi2)
                 (mergeInfo mergeString wStr0 wStr1 wStr2)
                 (mergeExp exp0 exp1 exp2)
       (ETyp sp0 pat0 t0 e0 esp0,
        ETyp sp1 pat1 t1 e1 esp1,
        ETyp sp2 pat2 t2 e2 esp2) ->
          if (patEqual pat0 pat1 && patEqual pat1 pat2) then
            if typeEqual t0 t1 && typeEqual t1 t2 then
             ETyp (mergeWS sp0 sp1 sp2)
                  pat0
                  t0
                  (mergeExp e0 e1 e2)
                  (mergeWS esp0 esp1 esp2)
            else default()
          else default ()
       (EColonType sp0 e0 spi0 t0 esp0,
        EColonType sp1 e1 spi1 t1 esp1,
        EColonType sp2 e2 spi2 t2 esp2) ->
          if typeEqual t0 t1 && typeEqual t1 t2 then
            EColonType (mergeWS sp0 sp1 sp2) (mergeExp e0 e1 e2) (mergeWS spi0 spi1 spi2) t0 (mergeWS esp0 esp1 esp2)
          else default ()
       (ETypeAlias sp0 pat0 t0 e0 esp0,
        ETypeAlias sp1 pat1 t1 e1 esp1,
        ETypeAlias sp2 pat2 t2 e2 esp2) ->
          if (patEqual pat0 pat1 && patEqual pat1 pat2) then
            if typeEqual t0 t1 && typeEqual t1 t2 then
             ETypeAlias (mergeWS sp0 sp1 sp2)
                  pat0
                  t0
                  (mergeExp e0 e1 e2)
                  (mergeWS esp0 esp1 esp2)
            else default()
          else default ()
       (EParens sp0 e0 pStyle0 esp0,
        EParens sp1 e1 pStyle1 esp1,
        EParens sp2 e2 pStyle2 esp2) ->
         if pStyle0 == pStyle1 && pStyle1 == pStyle2 then
             EParens (mergeWS sp0 sp1 sp2) (mergeExp e0 e1 e2) pStyle0 (mergeWS esp0 esp1 esp2)
         else default ()
       (EHole sp0 (Just v0),
        EHole sp1 (Just v1),
        EHole sp2 (Just v2)) ->
         EHole (mergeWS sp0 sp1 sp2) (Just (mergeVal v0 v1 v2))
       _ -> default ()
  in
  replaceE__ o result

mergeBranch: Branch -> Branch -> Branch -> Branch
mergeBranch o e1 e2 =
  case (o.val, e1.val, e2.val) of
    (Branch_ sp0 pat0 exp0 spe0,
     Branch_ sp1 pat1 exp1 spe1,
     Branch_ sp2 pat2 exp2 spe2) ->
       -- Check that the patterns are the same. If not takes the first pattern change.
       if patEqual pat0 pat1 && patEqual pat1 pat2 then
         {o | val = Branch_ (mergeWS sp0 sp1 sp2) pat0 (mergeExp exp0 exp1 exp2) (mergeWS spe0 spe1 spe2) }
       else if Syntax.patternUnparser Syntax.Elm pat0 == Syntax.patternUnparser Syntax.Elm pat1 then e2 else e1

mergeTBranch: TBranch -> TBranch -> TBranch -> TBranch
mergeTBranch o e1 e2 =
  case (o.val, e1.val, e2.val) of
    (TBranch_ sp0 typ0 exp0 spe0,
     TBranch_ sp1 typ1 exp1 spe1,
     TBranch_ sp2 typ2 exp2 spe2) ->
       -- Check that the patterns are the same. If not takes the first pattern change.
       if typeEqual typ0 typ1 && typeEqual typ1 typ2 then
         {o | val = TBranch_ (mergeWS sp0 sp1 sp2) typ0 (mergeExp exp0 exp1 exp2) (mergeWS sp0 sp1 sp2) }
       else if Syntax.typeUnparser Syntax.Elm typ0 == Syntax.typeUnparser Syntax.Elm typ1 then e2 else e1

mergeMaybe: (a -> a -> a -> a) -> Maybe a -> Maybe a -> Maybe a -> Maybe a
mergeMaybe submerger o e1 e2 =
  case (o, e1, e2) of
    (Nothing, Nothing, _) -> e2
    (Nothing, _, Nothing) -> e1
    (Nothing, Just m1, Just m2) -> Just (submerger m2 m1 m2)
    (Just o1, _, Nothing) -> Nothing
    (Just o1, Nothing, _) -> Nothing
    (Just o1, Just m1, Just m2) ->  Just (submerger o1 m1 m2)

mergeWS: WS -> WS -> WS -> WS -- No advanced strategy. No synthesis pushes concurrent changes in whitespace.
mergeWS o e1 e2 = if o.val == e1.val then e2 else e1

mergeInfo: (a -> a -> a -> a) -> WithInfo a ->WithInfo a -> WithInfo a -> WithInfo a
mergeInfo merger w1 w2 w3 = Info.replaceInfo w1 (merger w1.val w2.val w3.val)
