module UpdateUtils exposing (..)

import Lang exposing (..)
import LangTools exposing (valToExp, pruneEnv, pruneEnvPattern, valToString)
import Syntax
import Results exposing (Results(..), ok1, LazyList(..))

unparse = Syntax.unparser Syntax.Elm
unparsePattern = Syntax.patternUnparser Syntax.Elm
unparseType= Syntax.typeUnparser Syntax.Elm

-- Combine mupltiple runs on updating the same expression to a single result
combineResults: (Env, Exp) -> List (Result String (Env, Exp)) -> Result String (Env, Exp)
combineResults ((envo, expo) as original) results =
  case results of
        [] -> ok1 original
        [head] -> head
        Errs msg::tail ->
          case combineResults original tail of
            Errs msg2 -> Errs <| msg ++ msg2
            anything  -> anything
        (Oks r1 as head)::Errs msg::tail ->
          combineResults original (head::tail)
        Oks LazyNil :: Oks r2 :: tail -> Oks LazyNil
        Oks r1 :: Oks LazyNil :: tail -> Oks LazyNil
        Oks (LazyCons (env1, exp1) lazyTail1):: Oks (LazyCons (env2, exp2) lazyTail2):: tail ->
          let finalEnv = triCombine expo envo env1 env2 in
          let finalExp = mergeExp expo exp1 exp2 in
          Oks (LazyCons (finalEnv, finalExp) (Lazy.lazy)

          -- Time to merge all possible results. May result in exponential blowup if each result is ambiguous.

recursiveMerge: (a -> a -> a -> a) -> a -> List a -> a
recursiveMerge merge original modifications =
  case modifications of
    [] -> original
    [head] -> merge original head original
    head1::head2::tail ->
      recursiveMerge merge original (merge original head1 head2 :: tail)

recursiveMergeEnv exp = recursiveMerge (triCombine exp)
recursiveMergeVal = recursiveMerge mergeVal
recursiveMergeExp = recursiveMerge mergeExp

-- Tri combine only checks dependencies that may have been changed. It performs diffing though.
triCombine: Exp -> Env -> Env -> Env -> Env
triCombine origExp originalEnv newEnv1 newEnv2 =
  let fv = LangTools.freeIdentifiers origExp in
  --let _ = Debug.log "TriCombine starts !" () in
  let aux acc originalEnv newEnv1 newEnv2 =
       case (originalEnv, newEnv1, newEnv2) of
         ([], [], []) -> acc
         ((x, v1)::oe, (y, v2)::ne1, (z, v3)::ne2) ->
           if x /= y || y /= z || x /= z then
             Debug.crash <| "Expected environments to have the same variables, got\n" ++
              toString x ++ " = " ++ toString v1 ++ "\n" ++
              toString y ++ " = " ++ toString v2 ++ "\n" ++
              toString z ++ " = " ++ toString v3
           else
             aux (acc ++ mergeVal v1 v2 v3) oe ne1 ne2
         _ -> Debug.crash <| "Expected environments to have the same size, got\n" ++
              toString originalEnv ++ ", " ++ toString newEnv1 ++ ", " ++ toString newEnv2
       in
  aux [] originalEnv newEnv1 newEnv2

-- Merges values using a diffing algorithm.
mergeVal: Val -> Val -> Val -> Val
mergeVal original modified1 modified2 =
  case (original.v_, modified1.v_, modified2.v_) of    -- TODO: Find multiple elem insertions and deletions
    (VList originalElems, VList modified1Elems, VList modified2Elems) ->
      VList <| mergeList mergeVal originalElems modified1Elems modified2Elems
    (VRecord originalDict, VRecord modified1Dict, VRecord modified2Dict) ->
      VRecord <| mergeDict originalDict modified1Dict modified2Dict
    (VDict originalDict, VDict modified1Dict, VDict modified2Dict) ->
      VDict <| mergeDict originalDict modified1Dict modified2Dict
    (VClosure mbRec0 pats0 body0 env0, VClosure mbRec1 pats1 body1 env1, VClosure mbRec2 pats2 body2 env2) ->
      if mbRec0 == mbRec1 && mbRec1 == mbRec2 then
        if patsEqual pats0 pats1 pats2 then
          let newEnv = triCombine body0 env0 env1 env2 in
          let newBody = mergeExp body0 body1 body2 in
          VClosure mbRec0 pats0 newBody newEnv
        else if valEqual original modified1 then modified2 else modified1
      --(VRecord originalElems, VRecord modified1Elems, VRecord modified2Elems)->
      --  Dict.keys originalElems
      else if valEqual original modified1 then modified2 else modified1
    _ ->
      if valEqual original modified1 then modified2 else modified1

patsEqual: List Pat -> List Pat -> List Pat -> Bool
patsEqual = List.all identity <| List.map3 (\p0 p1 p2 -> patEqual p0 p1 && patEqual p1 p2)

mergeWS: WS -> WS -> WS -> WS -- No advanced strategy. No synthesis pushes concurrent changes in whitespace.
mergeWS o e1 e2 = if o.val == e1.val then e2 else e1

mergeString: String -> String -> String -> String
mergeString o e1 e2 = if o == e1 then e2 else e2 -- Could do a better line-based diff.

mergeExp: Exp -> Exp -> Exp -> Exp
mergeExp o e1 e2 =
  let default () = (if expEqual o e1 then e2 else e1).val.e__ in
  let result = case (o.val.e__, e1.val.e__, e2.val.e__) of
       (EFun sp0 pats0 body0 esp0,
        EFun sp1 pats1 body1 esp1,
        EFun sp2 pats2 body2 esp2) ->
          if patsEqual pats0 pats1 then
            EFun (mergeWS sp0 sp1 sp2) pats0 (mergeExp body0 body1 body2) (mergeWS esp0 esp1 esp2)
          else default ()
       (EApp sp0 fun0 args0 esp0,
        EApp sp1 fun1 args1 esp1,
        EApp sp2 fun2 args2 esp2) ->
         EApp (mergeWS sp0 sp1 sp2) (mergeExp fun0 fun1 fun2) (mergeList mergeExp args0 args1 args2) (mergeWS esp0 esp1 esp2)

       (EOp sp0 op0 args0 esp0,
        EOp sp1 op1 args1 esp1,
        EOp sp3 op2 args2 esp2) ->
         if op0.val == op1.val && op1.val == op2.val then
           EOp (mergeWS sp0 sp1 sp2) op0 (mergeList mergeExp args0 args1 args2) (mergeWS esp0 esp1 esp2)
         else default ()

       (EList sp0 args0 isp0 mTail0 esp0,
        EList sp1 args1 isp1 mTail1 esp1,
        EList sp2 args2 isp2 mTail2 esp2) ->
         EList (mergeWS sp0 sp1 sp2) (mergeList mergeExp args0 args1 args2) (mergeWS isp0 isp1 isp2)
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
       (EComment sp0 s0 e0,
        EComment sp1 s1 e1,
        EComment sp2 s2 e2) ->
         EComment (mergeWS sp0 sp1 sp2)
           (mergeString s0 s1 s2)
           (mergeExp e0 e1 e2)
       (EOption sp0 kStr0 spi0 wStr0 exp0,
        EOption sp1 kStr1 spi1 wStr1 exp1,
        EOption sp2 kStr2 spi2 wStr2 exp2) ->
         EOption (mergeWS sp0 sp1 sp2)
                 (mergeString kStr0 kStr1 kStr2)
                 (mergeWS spi0 spi1 spi2)
                 (mergeString wStr0 wStr1 wStr2)
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
             EParens (mergeWS sp0 sp1 sp2) (mergeWS e0 e1 e2) pStyle0 (mergeWS esp0 esp1 esp2)
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
  case (o.v_, e1.v_, e2.v_) of
    (Branch_ sp0 pat0 exp0 spe0,
     Branch_ sp1 pat1 exp1 spe1,
     Branch_ sp2 pat2 exp2 spe2) ->
       -- Check that the patterns are the same. If not takes the first pattern change.
       if patEqual pat0 pat1 && patEqual pat1 pat2 then
         {o | v_ = Branch_ (mergeWS sp0 sp1 sp2) pat0 (mergeExp exp0 exp1 exp2) (mergeWS sp0 sp1 sp2) }
       else if unparsePattern pat0 == unparsePattern pat1 then e2 else e1

mergeTBranch: TBranch -> TBranch -> TBranch -> TBranch
mergeTBranch o e1 e2 =
  case (o.v_, e1.v_, e2.v_) of
    (TBranch_ sp0 typ0 exp0 spe0,
     TBranch_ sp1 typ1 exp1 spe1,
     TBranch_ sp2 typ2 exp2 spe2) ->
       -- Check that the patterns are the same. If not takes the first pattern change.
       if typeEqual typ0 typ1 && typeEqual yp1 typ2 then
         {o | v_ = TBranch_ (mergeWS sp0 sp1 sp2) typ0 (mergeExp exp0 exp1 exp2) (mergeWS sp0 sp1 sp2) }
       else if unparseType typ0 == unparseType typ1 then e2 else e1

mergeMaybe: (a -> a -> a -> a) -> Maybe a -> Maybe a -> Maybe a -> Maybe a
mergeMaybe submerger o e1 e2 =
  case (o, e1, e2) of
    (Nothing, Nothing, _) -> e2
    (Nothing, _, Nothing) -> e1
    (Nothing, Just m1, Just m2) -> Just (submerger m2 m1 m2)
    (Just o1, Just _, Nothing) -> Nothing
    (Just o1, Nothing, Just _) -> Nothing
    (Just o1, Just m1, Just m2) ->  Just (submerger o1 m1 m2)

-- Guarantees that
-- * If updated lists have equal size, elements will be merged aligned.
-- * A list which was not modified makes that the other lists replaces the original list.
-- Would be better to have a real diffing algorithm.
mergeList: (a -> a -> a -> a) -> List a -> List a -> List a -> List a
mergeList submerger =
  let aux originals modified1 modified2 =
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
  let originalElems = Dict.toList originalDict |> Dict.map (\k v -> Just v) in
  let modified1Elems = originalElems |> List.map (\(k, v) -> (k, Dict.get k modified1Dict)) in
  let modified2Elems = originalElems |> List.map (\(k, v) -> (k, Dict.get k modified2Dict)) in
  let mergedKeyValues = mergeList (\(o, v1, v2) -> --Deletion only for now.
       case (o, v1, v2) of
         ((k, Nothing), _, _) -> Debug.crash "Impossible case in mergeVal"
         (_, (k, Nothing), _) -> (k, Nothing)
         (_, _, (k, Nothing)) -> (k, Nothing)
         ((k, Just jo), (_, Just jv1), (k, Just jv2)) -> (k, Just <| submerger jo jv1 jv2)
       ) originalElems modified1Elems modified2Elems
       |> List.maybeMap (\(k, mb) -> case mb of
         Nothing -> Nothing
         Just v -> Just (k, v)
       )
       |> Dict.fromList
  let insertedKeyValues = Dict.union (Dict.diff modified1Dict originalDict) (Dict.diff modified2Dict originalDict) in
  Dict.union insertedKeyValues mergedKeyValues



envToString: Env -> String
envToString env =
  case env of
    [] -> ""
    (v, value)::tail -> v ++ "->" ++ (valToString value) ++ " " ++ (envToString tail)

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


