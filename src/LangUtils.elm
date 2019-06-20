module LangUtils exposing (..)

import Syntax exposing (Syntax)
import Lang exposing (..)
import Utils
import Dict
import Set exposing (Set)
import Info
import ParserUtils
import ImpureGoodies
import HTMLParser

-- Removes from the environment all variables that are already bound by a pattern
pruneEnvPattern: List Pat -> Env -> Env
pruneEnvPattern pats env =
  case pats of
    [] -> env
    pat::tail ->
        let varspattern = identifiersSetInPat pat in
        List.filter (\(x, _) -> not <| Set.member x varspattern) env
        |> pruneEnvPattern tail

removeVarsInEnv: Set Ident -> Env -> Env
removeVarsInEnv boundVars env =
  List.filter (\(x, _) -> not (Set.member x boundVars)) env
        
pruneEnv: Exp -> Env -> Env
pruneEnv exp env = -- Remove all the initial environment that is on the right.
  let freeVars = freeIdentifiers exp in
  List.filter (\(x, _) -> Set.member x freeVars) env

-- NOTE: This function is mostly copied in Eval, as a temporary, hacky
-- workaround to dependency issues.



valToDictKey : Syntax -> Val -> Result String (String, String)
valToDictKey syntax val =
  Ok (valToString val, "val")

dictKeyToVal: Syntax -> (String, String) -> Result String Val
dictKeyToVal syntax (key, _) =
  Syntax.parser syntax key
  |> Result.mapError ParserUtils.showError
  |> Result.andThen (\e ->
      simpleExpToVal syntax e
    )

valFromExpVal_: Exp -> Val_ -> Val
valFromExpVal_ e v_ = { v_ = v_, provenance = Provenance [] e [], parents = Parents []}

-- Transforms an expression that does not need evaluation to a value
simpleExpToVal: Syntax -> Exp -> Result String Val
simpleExpToVal syntax e =
  case (unwrapExp e) of
    EConst _ num _ _ -> Ok <| valFromExpVal_ e <| VConst Nothing (num, dummyTrace)
    EBase  _ (EBool b) -> Ok <| valFromExpVal_ e <| VBase (VBool b)
    EBase  _ (EString _ s) -> Ok <| valFromExpVal_ e <| VBase (VString s)
    EBase  _ ENull -> Ok <| valFromExpVal_ e <| VBase (VNull)
    EList _ elems _ Nothing _ -> Utils.listValues elems |> List.map (simpleExpToVal syntax)
        |> Utils.projOk |> Result.map (VList >> valFromExpVal_ e)
    ERecord _ Nothing elems _ ->
      recordEntriesFromDeclarations elems
      |> Result.fromMaybe "Record is not simple enough not to be computed"
      |> Result.andThen (\l -> l
        |> List.map (\(_, _, k, _, v) -> simpleExpToVal syntax v |> Result.map (\v -> (k, v)))
        |> Utils.projOk |> Result.map (Dict.fromList >> VRecord >> valFromExpVal_ e))
    EOp _ _ op [arg] _ ->
      case op.val of
        DictFromList ->
          simpleExpToVal syntax arg
          |> Result.andThen (\d ->
            case d.v_ of
              VList elems -> elems |> List.map (\pair -> case pair.v_ of
                    VList [key, value] -> valToDictKey syntax key |> Result.map (\k -> (k, value))
                    v -> Err <| "Expected a tuple, got " ++ valToString d
                  ) |> Utils.projOk |> Result.map (Dict.fromList >> VDict >> valFromExpVal_ e)
              _ -> Err "Cannot create a dictionary from something else than a list of tuples"
            )
        _ -> Err <| "Could not consider as a simple expression " ++ toString e
    _ -> Err <| "Could not consider as a simple expression " ++ toString e

type IndentStyle = InlineSpace | IndentSpace String

foldIndentStyle: String -> (String -> String) -> IndentStyle -> String
foldIndentStyle inlineString indentStringFun style =
  case style of
    InlineSpace -> inlineString
    IndentSpace m -> indentStringFun m

foldIndent: String -> IndentStyle -> String
foldIndent inlineString = foldIndentStyle inlineString (\x -> "\n" ++ x)

increaseIndent: IndentStyle -> IndentStyle
increaseIndent style =
  case style of
      InlineSpace -> InlineSpace
      IndentSpace m -> IndentSpace (m ++ "  ")

valToExp: WS -> IndentStyle -> Val -> Exp
valToExp= valToExpFull Nothing

valToExpFull: Maybe Exp -> WS -> IndentStyle -> Val -> Exp
valToExpFull copyFrom sp_ indent v =
  let sp = Maybe.andThen (\e ->
    case unwrapExp e of
       EApp _ _ _ _ _ -> Just <| ws <| "" -- If it's an application, we don't keep the whitespace. Could be an encoding of something.
       _ -> Just <| ws <| precedingWhitespace e) copyFrom |> Maybe.withDefault sp_ in
  withDummyExpInfo <| case v.v_ of
    VConst mb num     -> EConst sp (Tuple.first num) dummyLoc noWidgetDecl
    VBase (VBool b)   -> EBase  sp <| EBool b
    VBase (VString s) ->
      let default = EBase  sp <| EString defaultQuoteChar s in
      if String.length s > 50 && String.contains "\n" s then
        EParens space0 (withDummyExpInfo default) LongStringSyntax space0
      else
        default
    VBase (VNull)     -> EBase  sp <| ENull
    VList vals ->
      case vHtmlNodeUnapply v of
        Just (tagName, attrs, children) -> -- In this case, if copyFrom
          let defaultAttrs () =
                case List.map vTupleViewUnapply attrs |> Utils.projJusts of
                   Nothing -> eList0 (List.map (valToExpFull Nothing space0 indent) attrs) Nothing
                   Just keyValues ->
                     let elems = List.map (\(key, value) ->
                          (space1, eListWs [(space0, valToExp space0 indent key), (space0, valToExp space0 indent value)] Nothing)) keyValues in
                     withDummyExpInfo <| EList space0 elems space0 Nothing space0
          in
          let defaultChildren () =
               eList0 (List.map (valToExpFull Nothing space0 indent) children) Nothing
          in
          let newClosingType = ws <|
               if HTMLParser.isVoidElement tagName then
               encoding_voidclosing else ""
          in
          case copyFrom |> Maybe.map unwrapExp of
            Just (EList unusedWS1 [(unusedWS2, tagExp), (unusedWS3, attrExp), (spaceBeforeEndOpeningTag, childExp)] closingType Nothing spaceAfterTagClosing) ->
              let mbNewAttrExp = case unwrapExp attrExp of
                EList _ attributes _ Nothing _ ->
                    if List.length attributes >= 2 then
                      Just <| valToExpFull (Just attrExp) space0 indent (replaceV_ v <| VList attrs)
                    else
                     Nothing
                _ -> Nothing
              in
              let newAttrExp = mbNewAttrExp |> Utils.maybeWithDefaultLazy defaultAttrs in
              let newChildExp =
                   case unwrapExp childExp of
                        EList _ oldChildren _ Nothing _ ->
                          if List.length oldChildren >= List.length children then
                             eListWs (List.map2 (\(s, old) n -> (s, valToExpFull (Just old) space0 indent n)) oldChildren children) Nothing
                          else
                            defaultChildren ()
                        _ ->
                          defaultChildren ()
              in
              EList unusedWS1 [(unusedWS2, eStr0 tagName),
                 (unusedWS3, newAttrExp), (spaceBeforeEndOpeningTag, newChildExp)]
                 newClosingType Nothing spaceAfterTagClosing
            _ ->
              EList space0 [(space0, eStr tagName), (space0, defaultAttrs ()), (space0, defaultChildren ())] newClosingType Nothing space0
        Nothing ->
      let defaultSpCommaHd = ws "" in
      let defaultSpCommaTail = space0 in
      let (precedingWS, ((spaceCommaHead, v2expHead), (spaceCommaTail, v2expTail)), spBeforeEnd) =
           copyFrom |> Maybe.andThen (\e ->
            case unwrapExp e of
                EList csp0 celems _ _ cspend ->

                    let valToExps =  case celems of
                       (sphd1, hd1)::(sphd2, hd2)::tail -> ((sphd1, valToExpFull <| Just hd1), (sphd2, valToExpFull <| Just hd2))
                       [(sphd1, hd1)] -> ((sphd1, valToExpFull <| Just hd1), (defaultSpCommaTail, valToExpFull <| Just hd1))
                       [] -> ((defaultSpCommaHd, valToExp), (defaultSpCommaTail, valToExp))
                    in
                    Just (csp0, valToExps, cspend)
                _ -> Nothing
            ) |>
            Maybe.withDefault (sp, ((defaultSpCommaHd, valToExp), (defaultSpCommaTail, valToExp)), if List.isEmpty vals then space0 else ws <| foldIndent "" indent)
      in
      case vals of
         [] -> EList precedingWS [] space0 Nothing spBeforeEnd
         head::tail ->
            let headExp = (spaceCommaHead, v2expHead (ws <| foldIndentStyle "" (\_ -> " ") indent) (increaseIndent indent) head) in
            let tailExps = List.map (\y -> (spaceCommaTail, v2expTail (ws <| foldIndent " " <| increaseIndent indent) (increaseIndent indent) y)) tail in
            EList precedingWS (headExp :: tailExps) space0 Nothing spBeforeEnd
    VClosure recNames patterns body env ->
      let (recEnv, remEnv) = Utils.split (List.length recNames) env in
      let prunedEnv = removeVarsInEnv (Set.fromList <| List.map Tuple.first recEnv) <| pruneEnvPattern patterns <| pruneEnv body <| env in
      case prunedEnv of
        [] -> EFun sp patterns body space0
        (name, v)::tail ->
          let baseCase =  withDummyExpInfo <| EFun (ws <| foldIndent "" indent) patterns body space0 in
          let startCase =
                case recNames of
                  [] -> baseCase
                  [f] -> withDummyExpInfo <|
                    ELet sp Let (Declarations [0] [] []
                      [(True,
                        [LetExp Nothing space1 (withDummyPatInfo <| PVar space0 f noWidgetDecl) FunArgAsPats space1 baseCase])]) space1 (withDummyExpInfo <| EVar (ws <| foldIndent " " indent) f)
                  _ -> Debug.crash "cannot convert back a VCLosure with multiple defs to an ELet. Need syntax support for that."
          in
          let bigbody = List.foldl (\(n, v) body ->
            withDummyExpInfo <| eLet__ (ws <| foldIndent "" indent) Let False (withDummyPatInfo <| PVar (ws " ") n noWidgetDecl) space1 (valToExp space1 (increaseIndent indent) v) space1 body space0) startCase tail
          in
          eLet__ sp Let False (withDummyPatInfo <| PVar (ws " ") name noWidgetDecl) space1 (valToExp space1 (increaseIndent indent) v) space1 bigbody space0
    VRecord values ->
      let copiedResult =
       case copyFrom of
        Just e ->
           case unwrapExp e of
             ERecord csp0 Nothing (Declarations po tps anns lxs as decls) cspEnd ->
               case lxs |> elemsOf |> List.map (\(LetExp _ _ p _ _ _) ->
                 pVarUnapply p) |> Utils.projJusts of
                 Just keyList ->
                    if Set.fromList keyList == Set.fromList (Dict.keys values) then
                      lxs |> elemsOf |> List.map (\(LetExp spc spe p fs se e1) ->
                        pVarUnapply p |> Maybe.andThen (\key -> Dict.get key values) |> Maybe.map (\v ->
                           LetExp spc spe p fs se <| valToExpFull (Just e1) space1 (increaseIndent indent) v
                        )) |> Utils.projJusts |> Maybe.map (\newLxs ->
                         ERecord csp0 Nothing (Declarations po tps anns (regroup lxs newLxs)) cspEnd
                        )
                    else
                      Nothing
                 Nothing -> Nothing
             _ -> Nothing
        Nothing -> Nothing
       in
       case copiedResult of
         Just x -> x
         Nothing ->
       let (isTuple, keyValues) = case vRecordTupleUnapply v of
          Just (kv, elements) -> (True, kv::elements)
          Nothing -> (False, Dict.toList values)
       in
      let (precedingWS, keys, ((spaceComma, spaceKey, spaceEqual, v2expHead),
                               (spaceCommaTail, spaceKeyTail, spaceEqualTail, v2expTail)), spBeforeEnd) =
           (sp, Dict.keys values,
           ((Nothing, space1, space1, valToExp),
            (Just space0, ws (foldIndent " " <| increaseIndent indent), space1, valToExp)),
           if isTuple then ws "" else if Dict.isEmpty values then space1 else ws <| foldIndent "" indent) in
      eRecord__ precedingWS Nothing (List.indexedMap (\i (key, v) ->
          if i == 0 then (spaceComma,     spaceKey,     key, spaceEqual,     v2expHead (if isTuple then ws "" else ws " ") (increaseIndent <| increaseIndent indent) v)
          else           (spaceCommaTail, spaceKeyTail, key, spaceEqualTail, v2expTail (if isTuple && i <= 1 then ws "" else ws " ") (increaseIndent <| increaseIndent indent) v)
        ) keyValues) spBeforeEnd
    VDict vs ->
      EOp sp space0 (Info.withDummyInfo DictFromList) [withDummyExpInfo <|
        EList space1 (
          Dict.toList vs |> List.indexedMap (\i (key, value) ->
            let spaceComma = if i == 0 then ws "" else ws <| foldIndent "" indent in
            let spaceElem = ws " " in
            (spaceComma, valToExp  spaceElem (increaseIndent indent) (replaceV_ v <| vTuple [dictKeyToVal Syntax.Leo key |> Utils.fromOk "valToExp", value])))
          ) space0 Nothing space0] space0
    VFun name _ _ _ -> EVar sp name

valToString: Val -> String
valToString = Syntax.unparser Syntax.Leo << valToExp (ws "") (IndentSpace "")

logVal msg v = let _ = Debug.log (msg ++ ":" ++ valToString v) () in v
logExp msg v = let _ = Debug.log (msg ++ ":" ++ Syntax.unparser Syntax.Leo v) () in v
logEnv msg exp env = let _ = Debug.log (msg ++ ":" ++ envToString (pruneEnv exp env)) () in env

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
patEqual p1_ p2_ = --let _ = Debug.log "patEqual " (Syntax.patternUnparser Syntax.Leo p1_, Syntax.patternUnparser Syntax.Leo p2_) in
  Syntax.patternUnparser Syntax.Leo p1_ == Syntax.patternUnparser Syntax.Leo p2_
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
  case (ty1.val.t__, ty2.val.t__) of
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
  (TApp sp1 ident1 types1 _, TApp sp2 ident2 types2 _) ->
    wsEqual sp1 sp2 && typeEqual ident1 ident2 && listForAll2 typeEqual types1 types2
  (TVar sp1 ident1, TVar sp2 ident2) ->
    wsEqual sp1 sp2 && ident1 == ident2
  (TForall sp1 pats1 t1 sp2, TForall sp3 pats2 t2 sp4) ->
    wsEqual sp1 sp3 && wsEqual sp2 sp4 &&
    listForAll2 (\a1 a2 -> a1 == a2) pats1 pats2
    && typeEqual t1 t2
  (TWildcard sp1, TWildcard sp2) -> wsEqual sp1 sp2
  _ -> False


expEqual: Exp -> Exp -> Bool
expEqual e1_ e2_ =
  --let _ = Debug.log "expEqual " (Syntax.unparser Syntax.Leo e1_, Syntax.unparser Syntax.Leo e2_) in
  Syntax.unparser Syntax.Leo e1_ == Syntax.unparser Syntax.Leo e2_
{--
  case ((unwrapExp e1_), (unwrapExp e2_)) of
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
  (EHole sp1 (ESnapHole v1), EHole sp2 (ESnapHole v2)) ->
    wsEqual sp1 sp2 && valEqual v1 v2
  (EHole sp1 EEmptyHole, EHole sp2 EEmptyHole) ->
    wsEqual sp1 sp2
  _ -> False
--}

-- Given a predicate and a pattern, returns true if the patterns extracts a top-level field satisfying the predicate
patternExtractsField fieldPredicate pat = case pat.val.p__ of
  PParens _ p _ -> patternExtractsField fieldPredicate p
  PAs _ p1 _ p2 -> patternExtractsField fieldPredicate p2 || patternExtractsField fieldPredicate p1
  PVar _ _ _ -> True
  PRecord _ ps _ -> List.any (\(_, _, k, _, _) -> fieldPredicate k) ps
  PColonType _ p _ _ -> patternExtractsField fieldPredicate p
  PConst _ _ -> False
  PBase _ _ -> False
  PWildcard _ -> False
  PList _ _ _ _ _ -> False
