module StaticAnalysis exposing (..)

import Lang exposing (..)
import LangTools exposing (..)
import Utils

import Dict exposing (Dict)
import Set exposing (Set)

type alias DependencyEnv = List (Ident, EId)


isDependentOn : Dict EId (Set EId) -> EId -> EId -> Bool
isDependentOn dependencies focusEId dependentEId =
  Set.member dependentEId (eidDependencies dependencies focusEId)


eidDependencies : Dict EId (Set EId) -> EId -> Set EId
eidDependencies dependencies eid =
  let dependenciesBFS visited toVisit =
    case toVisit of
       []             -> visited
       eid::remaining ->
        if Set.member eid visited then
          dependenciesBFS visited remaining
        else
          let newVisited = Set.insert eid visited in
          let newToVisit = (Utils.getWithDefault eid Set.empty dependencies |> Set.toList) ++ remaining in
          dependenciesBFS newVisited newToVisit
  in
  dependenciesBFS Set.empty (Utils.getWithDefault eid Set.empty dependencies |> Set.toList)


-- preludeGrossDependencies : Dict EId (Set EId)
-- preludeGrossDependencies = grossDependencies_ [] FastParser.prelude


grossDependencies : Exp -> Dict EId (Set EId)
grossDependencies exp =
  grossDependencies_ [] exp


-- Dict from EId to EIds immediately depended upon.
-- Specifically, if the evaluated value of any of the dependent EIds change, the evaluated value at this EId is liable to change.
-- Conservative: If a pattern cannot be statically matched, dependent on the entire expression matched against.
grossDependencies_ : DependencyEnv -> Exp -> Dict EId (Set EId)
grossDependencies_ identToDepEId program =
  let addDependency eid dependsOnEId programDependencies =
    let priorDeps = Utils.getWithDefault eid Set.empty programDependencies in
    programDependencies
    |> Dict.insert eid (Set.insert dependsOnEId priorDeps)
  in
  let childrenValueExpEIds exp =
    childExps exp
    |> List.map (expEffectiveExp >> .val >> .eid)
  in
  let handleLetexp exp isRec letExps bindingNumber globalAcc identToDepEId =
    (,) globalAcc <|
    Utils.foldLeft identToDepEId letExps <|
                 \identToDepEId (LetExp _ _ pat _ _ boundExp) ->
       let newBindings =
          case tryMatchExp pat boundExp of
            Match identToExp -> identToExp |> List.map (\(ident, exp) -> (ident, exp.val.eid))
            _                -> identifiersListInPat pat |> List.map (\ident -> (ident, boundExp.val.eid)) -- Conservative dependency on entire bound expression.
       in
       newBindings ++ identToDepEId
  in
  let handleEFun funcExp identToDepEId =
    let newBindings =
       expToFuncPats funcExp
       |> List.concatMap identifiersListInPat
       |> List.map (\ident -> (ident, -1)) -- If shadowing, not dependent on whatever previously bound that identifier.
    in
    newBindings ++ identToDepEId
  in
  let handleCaseBranch caseExp branch branchI identToDepEId =
    let scrutinee = expToCaseScrutinee caseExp in
    let pat = branchPat branch in
    let newBindings =
       case tryMatchExp pat scrutinee of
         Match identToExp -> identToExp |> List.map (\(ident, exp) -> (ident, exp.val.eid))
         _                -> identifiersListInPat pat |> List.map (\ident -> (ident, scrutinee.val.eid)) -- Conservative dependency on entire scutinee.
    in
    newBindings ++ identToDepEId
  in
  let addThisExpDeps exp programDependencies identToDepEId =
    -- Expressions are not dependent on themselves because that is trivial.
    let newDepEIds =
       case exp.val.e__ of
         EConst _ n loc wd -> []
         EBase _ bVal      -> []
         EVar _ ident      ->
           case Utils.maybeFind ident identToDepEId of
             Just -1  -> []
             Just eid -> [eid]
             Nothing  -> []

         EFun _ pats body _                      -> [(expEffectiveExp body).val.eid] -- The value of a function is how it transforms inputs into outputs, as determined by its body expression.
         EOp _ _ op argExps _                    -> childrenValueExpEIds exp
         EList _ heads _ maybeRest _             -> childrenValueExpEIds exp
         ERecord _ mb es _                       -> childrenValueExpEIds exp
         ESelect _ _ _ _ _                       -> childrenValueExpEIds exp
         EIf _ pred _ trueBranch _ falseBranch _ -> childrenValueExpEIds exp
         ECase _ scrutinee branches _            -> childrenValueExpEIds exp
         EApp _ funcExp argExps _ _              -> childrenValueExpEIds exp
         ELet _ _ _ _ body                       -> [expEffectiveExp body |> .val |> .eid]
         EColonType _ body _ _ _                 -> [expEffectiveExp body |> .val |> .eid]
         EParens _ body _ _                      -> [expEffectiveExp body |> .val |> .eid]
         EHole _ _                               -> []
    in
    newDepEIds
    |> List.foldl
        (addDependency exp.val.eid)
        programDependencies
  in
  program
  |> foldExpTopDownWithScope
      addThisExpDeps
      handleLetexp
      handleEFun
      handleCaseBranch
      Dict.empty
      identToDepEId
