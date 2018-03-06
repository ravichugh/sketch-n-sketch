module LangUtils exposing (..)

import Syntax exposing (Syntax)
import Lang exposing (..)
import Utils
import Dict
import Set
import Info
import ParserUtils

branchPatExps : List Branch -> List (Pat, Exp)
branchPatExps branches =
  List.map
    (.val >> \(Branch_ _ pat exp _) -> (pat, exp))
    branches


identifiersListInPat : Pat -> List Ident
identifiersListInPat pat =
  case pat.val.p__ of
    PVar _ ident _              -> [ident]
    PList _ pats _ (Just pat) _ -> List.concatMap identifiersListInPat (pat::pats)
    PList _ pats _ Nothing    _ -> List.concatMap identifiersListInPat pats
    PAs _ ident _ pat           -> ident::(identifiersListInPat pat)
    _                           -> []


identifiersListInPats : List Pat -> List Ident
identifiersListInPats pats =
  List.concatMap
    identifiersListInPat
    pats


-- All identifiers used or bound throughout the given exp
identifiersList : Exp -> List Ident
identifiersList exp =
  let folder e__ acc =
    case e__ of
       EVar _ ident ->
         ident::acc

       EFun _ pats _ _ ->
         (List.concatMap identifiersListInPat pats) ++ acc

       ECase _ _ branches _ ->
         let pats = branchPats branches in
         (List.concatMap identifiersListInPat pats) ++ acc

       ELet _ _ _ pat _ _ _ _ _ ->
         (identifiersListInPat pat) ++ acc

       _ ->
         acc
  in
  foldExpViaE__
    folder
    []
    exp



identifiersSet : Exp -> Set.Set Ident
identifiersSet exp =
  identifiersList exp
  |> Set.fromList


identifiersSetInPat : Pat -> Set.Set Ident
identifiersSetInPat pat =
  identifiersListInPat pat
  |> Set.fromList


identifiersSetInPats : List Pat -> Set.Set Ident
identifiersSetInPats pats =
  List.map identifiersSetInPat pats
  |> Utils.unionAll


expToMaybeIdent : Exp -> Maybe Ident
expToMaybeIdent exp =
  case exp.val.e__ of
    EVar _ ident -> Just ident
    _            -> Nothing

freeVars : Exp -> List Exp
freeVars exp =
  let removeIntroducedBy pats vars =
    let introduced = identifiersListInPats pats in
    vars |> List.filter (\var -> not <| List.member (Utils.fromJust_ "freeVars" <| expToMaybeIdent var) introduced)
  in
  case exp.val.e__ of
    EVar _ x                           -> [exp]
    EFun _ pats body _                 -> freeVars body |> removeIntroducedBy pats
    ECase _ scrutinee branches _       ->
      let freeInEachBranch =
        branchPatExps branches
        |> List.concatMap (\(bPat, bExp) -> freeVars bExp |> removeIntroducedBy [bPat])
      in
      freeVars scrutinee ++ freeInEachBranch
    ELet _ _ False pat _ boundExp _ body _ -> freeVars boundExp ++ (freeVars body |> removeIntroducedBy [pat])
    ELet _ _ True  pat _ boundExp _ body _ -> freeVars boundExp ++ freeVars body |> removeIntroducedBy [pat]
    _                                  -> childExps exp |> List.concatMap freeVars


-- Which var idents in this exp refer to something outside this exp?
-- This is wrong for TypeCases; TypeCase scrutinee patterns not included. TypeCase scrutinee needs to turn into an expression (done on Brainstorm branch, I believe).
freeIdentifiers : Exp -> Set.Set Ident
freeIdentifiers exp =
  freeVars exp
  |> List.map expToMaybeIdent
  |> Utils.projJusts
  |> Utils.fromJust_ "LangTools.freeIdentifiers"
  |> Set.fromList


-- Removes from the environment all variables that are already bound by a pattern
pruneEnvPattern: List Pat -> Env -> Env
pruneEnvPattern pats env =
  case pats of
    [] -> env
    pat::tail ->
        let varspattern = identifiersSetInPat pat in
        List.filter (\(x, _) -> not <| Set.member x varspattern) env
        |> pruneEnvPattern tail

pruneEnv: Exp -> Env -> Env
pruneEnv exp env = -- Remove all the initial environment that is on the right.
  --let (p1, p2) = removeCommonPrefix (List.reverse Eval.initEnv) (List.reverse env) in
  --List.reverse p1
  --List.take 5 env
  --List.take (List.length env - List.length Eval.initEnv) env
  --env
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
  case e.val.e__ of
    EConst _ num _ _ -> Ok <| valFromExpVal_ e <| VConst Nothing (num, dummyTrace)
    EBase  _ (EBool b) -> Ok <| valFromExpVal_ e <| VBase (VBool b)
    EBase  _ (EString _ s) -> Ok <| valFromExpVal_ e <| VBase (VString s)
    EBase  _ ENull -> Ok <| valFromExpVal_ e <| VBase (VNull)
    EList _ elems _ Nothing _ -> Utils.listValues elems |> List.map (simpleExpToVal syntax)
        |> Utils.projOk |> Result.map (VList >> valFromExpVal_ e)
    ERecord _ Nothing elems _ -> elems |> List.map (\(_, _, k, _, v) -> simpleExpToVal syntax v |> Result.map (\v -> (k, v)))
        |> Utils.projOk |> Result.map (Dict.fromList >> VRecord >> valFromExpVal_ e)
    EOp _ op [arg] _ ->
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
  let sp = Maybe.map (ws << precedingWhitespace) copyFrom |> Maybe.withDefault sp_ in
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
      let defaultSpCommaHd = ws "" in
      let defaultSpCommaTail = space0 in
      let (precedingWS, ((spaceCommaHead, v2expHead), (spaceCommaTail, v2expTail)), spBeforeEnd) = copyFrom |> Maybe.andThen (\e -> case e.val.e__ of
        EList csp0 celems _ _ cspend ->

            let valToExps =  case celems of
               (sphd1, hd1)::(sphd2, hd2)::tail -> ((sphd1, valToExpFull <| Just hd1), (sphd2, valToExpFull <| Just hd2))
               [(sphd1, hd1)] -> ((sphd1, valToExpFull <| Just hd1), (defaultSpCommaTail, valToExpFull <| Just hd1))
               [] -> ((defaultSpCommaHd, valToExp), (defaultSpCommaTail, valToExp))
            in
            Just (csp0, valToExps, cspend)
        _ -> Nothing
      ) |> Maybe.withDefault (sp, ((defaultSpCommaHd, valToExp), (defaultSpCommaTail, valToExp)), if List.isEmpty vals then space0 else ws <| foldIndent "" indent) in
      case vals of
        [] -> EList precedingWS [] space0 Nothing spBeforeEnd
        head::tail ->
            let headExp = (spaceCommaHead, v2expHead (ws <| foldIndentStyle "" (\_ -> " ") indent) (increaseIndent indent) head) in
            let tailExps = List.map (\y -> (spaceCommaTail, v2expTail (ws <| foldIndent " " <| increaseIndent indent) (increaseIndent indent) y)) tail in
            EList precedingWS (headExp :: tailExps) space0 Nothing spBeforeEnd
    VClosure mRec patterns body env ->
      let prunedEnv = pruneEnvPattern patterns (pruneEnv body env) in
      case prunedEnv of
        [] -> EFun sp patterns body space0
        (name, v)::tail ->
          let baseCase =  withDummyExpInfo <| EFun (ws <| foldIndent "" indent) patterns body space0 in
          let startCase =
                case mRec of
                  Nothing -> baseCase
                  Just f -> withDummyExpInfo <| ELet sp Let True (withDummyPatInfo <| PVar (ws " ") f noWidgetDecl) space1 baseCase space1 (withDummyExpInfo <| EVar (ws <| foldIndent " " indent) f) space0
          in
          let bigbody = List.foldl (\(n, v) body -> withDummyExpInfo <| ELet (ws <| foldIndent "" indent) Let False (withDummyPatInfo <| PVar (ws " ") n noWidgetDecl) space1 (valToExp space1 (increaseIndent indent) v) space1 body space0) startCase tail
          in
          ELet sp Let False (withDummyPatInfo <| PVar (ws " ") name noWidgetDecl) space1 (valToExp space1 (increaseIndent indent) v) space1 bigbody space0
    VRecord values ->
      let defaultspcHd = space0 in
      let defaultspkHd =  ws " " in
      let defaultspeHd = ws " " in
      let defaultspcTl = space0 in
      let defaultspkTl = ws (foldIndent " " <| increaseIndent indent) in
      let defaultspeTl = ws " " in
      let (precedingWS, keys, ((spaceComma, spaceKey, spaceEqual, v2expHead),
                               (spaceCommaTail, spaceKeyTail, spaceEqualTail, v2expTail)), spBeforeEnd) = copyFrom |> Maybe.andThen (\e -> case e.val.e__ of
           ERecord csp0 _ celems cspend ->
             let existingkeys =  Utils.recordKeys celems in
             let finalKeys = existingkeys ++ (Dict.keys values |> List.filter (\k -> not (List.any (\e -> e == k) existingkeys))) in
             let valToExps = case celems of
                    (spc1, spk1, _, spe1, hd1)::(spc2, spk2, _, spe2, hd2)::tail ->
                      ((spc1, spk1, spe1, valToExpFull <| Just hd1), (spc2, spk2, spe2, valToExpFull <| Just hd2))
                    [(spc1, spk1, _, spe1, hd1)] ->
                      ((spc1, spk1, spe1, valToExpFull <| Just hd1), (defaultspcTl, defaultspkTl, defaultspeTl, valToExpFull <| Just hd1))
                    [] -> ((defaultspcHd, defaultspkHd, defaultspeHd, valToExp), (defaultspcTl, defaultspkTl, defaultspeTl, valToExp))
             in
             Just (csp0, finalKeys, valToExps, cspend)
           _ -> Nothing
         ) |> Maybe.withDefault (sp, Dict.keys values, ((defaultspcHd, defaultspkHd, defaultspeHd, valToExp), (defaultspcTl, defaultspkTl, defaultspeTl, valToExp)),
           if Dict.isEmpty values then space1 else ws <| foldIndent "" indent) in
      ERecord precedingWS Nothing (List.indexedMap (\i key ->
        case Dict.get key values of
          Nothing -> Debug.crash <| "Internal error: Could not retrieve key " ++ toString key
          Just v -> if i == 0 then (spaceComma, spaceKey, key, spaceEqual, v2expHead (ws " ") (increaseIndent <| increaseIndent indent) v)
            else (spaceCommaTail, spaceKeyTail, key, spaceEqualTail, v2expTail (ws " ") (increaseIndent <| increaseIndent indent) v)
        ) keys) spBeforeEnd
    VDict vs ->
      EOp sp (Info.withDummyInfo DictFromList) [withDummyExpInfo <|
        EList space1 (
          Dict.toList vs |> List.indexedMap (\i (key, value) ->
            let spaceComma = if i == 0 then ws "" else ws <| foldIndent "" indent in
            let spaceElem = ws " " in
            (spaceComma, valToExp  spaceElem (increaseIndent indent) (replaceV_ v <| VList [dictKeyToVal Syntax.Elm key |> Utils.fromOk "valToExp", value])))
          ) space0 Nothing space0] space0
    VFun name _ _ _ -> EVar sp name

valToString: Val -> String
valToString = Syntax.unparser Syntax.Elm << valToExp (ws "") (IndentSpace "")
