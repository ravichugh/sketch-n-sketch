module TinyStructuredEditorsForLowLowPricesEval exposing (evalToStringTaggedWithProjectionPaths, tagVal)

import Set exposing (Set)

import Lang
-- import Types2
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)


setTag : Set ProjectionPath -> TaggedValue -> TaggedValue
setTag paths w = { w | paths = paths }


-- Following Fig. 12 "Generic extraction" in Acar et al. (A Core Calculus for Provenance, arXiv version https://arxiv.org/pdf/1310.6299v2.pdf).
-- Configured for dependency provenance (p. 19).
dependencyAnnotators =
  { constant      = Set.empty -- D_𝑐
  , function      = Set.empty -- D_𝜅
  , application   = \closureTags   resultTags       -> Set.union closureTags resultTags -- D_𝚊𝚙𝚙
  , caseAndBranch = \scrutineeTags branchResultTags -> Set.union scrutineeTags branchResultTags -- D_𝐿 and D_𝑅
  , operation     = \operandsTags                   -> Utils.unionAll operandsTags
  }


-- Dependency tagging evaluation on the core langauage.
eval : Env -> Exp -> Result String TaggedValue
eval env exp =
  let noTag v = TaggedValue v Set.empty in -- ⊥ in Acar et al. Fig. 12
  case exp of
    EFun funcName argName funcBody ->
      Ok <| TaggedValue (VClosure env funcName argName funcBody) dependencyAnnotators.function

    EVar varName ->
      case Utils.maybeFind varName env of
        Just boundValue -> Ok boundValue -- No extra annotation in Acar et al. Fig. 12
        Nothing         -> Err <| "Variable " ++ varName ++ " not found!"

    EApp funcExp argExp ->
      eval env funcExp |> Result.andThen (\funcTaggedVal ->
      eval env argExp  |> Result.andThen (\argTaggedVal ->
        case funcTaggedVal.v of
          VClosure env funcName argName funcBody ->
            eval ((funcName, funcTaggedVal)::(argName, argTaggedVal)::env) funcBody
            |> Result.map (\resultTaggedVal ->
              resultTaggedVal |> setTag (dependencyAnnotators.application funcTaggedVal.paths resultTaggedVal.paths)
            )

          _ ->
            Err <| "Left side of application should be a function but got " ++ unparseToUntaggedString funcTaggedVal ++ "!"
      ))

    ECtor ctorName argExps ->
      argExps
      |> List.map (eval env)
      |> Utils.projOk
      |> Result.map (\argTaggedVals -> VCtor ctorName argTaggedVals)
      |> Result.map noTag -- No annotation in Acar et al. Fig. 12

    ECase scrutinee branches ->
      eval env scrutinee |> Result.andThen (\scrutineTaggedVal ->
        case scrutineTaggedVal.v of
          VCtor ctorName argTaggedVals ->
            let maybeMatchingBranch =
              branches
              |> Utils.findFirst (\(branchCtorName, argNames, branchExp) -> ctorName == branchCtorName)
            in
            case maybeMatchingBranch of
              Just (_, argNames, branchExp) ->
                case Utils.maybeZip argNames argTaggedVals of
                  Just bindings ->
                    eval (bindings ++ env) branchExp
                    |> Result.map (\branchResultTaggedVal ->
                      branchResultTaggedVal |> setTag (dependencyAnnotators.caseAndBranch scrutineTaggedVal.paths branchResultTaggedVal.paths)
                    )

                  Nothing ->
                    Err <| ctorName ++ " value is carrying " ++ toString (List.length argTaggedVals) ++ " values but the case statement branch binds " ++ toString (List.length argNames) ++ " values (" ++ Utils.toSentence argNames ++ ")!"

              _ ->
                Err <| "Non-exhaustive case statement! This value fell through: " ++ unparseToUntaggedString scrutineTaggedVal

          _ ->
            Err <| "Case scrutinee should evaluate to a data type contructor, but instead got " ++ unparseToUntaggedString scrutineTaggedVal ++ "!"
      )

    EString string ->
      Ok <| TaggedValue (VString string) dependencyAnnotators.constant

    EAppend e1 e2 ->
      -- Analogous to tupling or applying a constructor, both of which are unannotated in Acar et al. Fig. 12
      Result.map2 VAppend (eval env e1) (eval env e2)
      |> Result.map noTag

    ENum num ->
      Ok <| TaggedValue (VNum num) dependencyAnnotators.constant

    ENumToString argExp ->
      eval env argExp
      |> Result.andThen (\argTaggedVal ->
        case argTaggedVal.v of
          VNum num -> Ok <| TaggedValue (VString (toString num)) (dependencyAnnotators.operation [argTaggedVal.paths])
          _        -> Err <| "Built-in toString only supports numbers but was given " ++ unparseToUntaggedString argTaggedVal ++ "!"
      )


-- Set up the value of interest with path tags, which are propogated during evaluation.
--
-- Fig. 11 "Path annotation operation" in Acar et al.
-- AddTag/AddTagDeep in our appendix. We handle closures differently than Acar et al., but we also don't expect closures in the initial value of interest so it doesn't matter.
tagVal : ProjectionPath -> TaggedValue -> TaggedValue
tagVal path w =
  let deeperTagged =
    case w.v of
      VClosure funcEnv funcName varName body ->
        w

      VCtor ctorName ws ->
        let taggedWs = ws |> Utils.mapi1 (\(i, w) -> tagVal (path ++ [i]) w) in
        { w | v = VCtor ctorName taggedWs }

      VString string ->
        w

      VAppend w1 w2 ->
        let
          w1Tagged = tagVal (path ++ [1]) w1
          w2Tagged = tagVal (path ++ [2]) w2
        in
        { w | v = VAppend w1Tagged w2Tagged }

      VNum num ->
        w
  in
  setTag (Set.insert path w.paths) deeperTagged


-- Simple flattening and verification that we evaluated only to string literals and append operations.
taggedValueToMaybeStringTaggedWithProjectionPaths : TaggedValue -> Maybe StringTaggedWithProjectionPaths
taggedValueToMaybeStringTaggedWithProjectionPaths w =
  case w.v of
    VString string ->
      Just <| TaggedString string w.paths

    VAppend w1 w2 ->
      taggedValueToMaybeStringTaggedWithProjectionPaths w1 |> Maybe.andThen (\string1Tagged ->
      taggedValueToMaybeStringTaggedWithProjectionPaths w2 |> Maybe.map     (\string2Tagged ->
        TaggedStringAppend string1Tagged string2Tagged w.paths
      ))

    _ ->
      Nothing


-- If both sides of an append have the same path, (recursively) move that path to the append instead.
--
-- e.g. ("x"{1.•} ++ "y"{1.•}){} => ("x"{} ++ "y"{}){1.•}
--
-- Then, keep only the outermost occurence of each path.
--
-- e.g. ("x"{1.•} ++ "y"{}){1.•} => ("x"{} ++ "y"{}){1.•}
tidyUpProjectionPaths : StringTaggedWithProjectionPaths -> StringTaggedWithProjectionPaths
tidyUpProjectionPaths stringTaggedWithProjectionPaths =
  let
    coalese stringTaggedWithProjectionPaths =
      case stringTaggedWithProjectionPaths of
        TaggedString _ _                      -> stringTaggedWithProjectionPaths
        TaggedStringAppend left right pathSet ->
          let
            leftCoalesed  = coalese left
            rightCoalesed = coalese right
            sharedPaths   = Set.intersect (stringTag leftCoalesed) (stringTag rightCoalesed)
          in
          TaggedStringAppend
              (leftCoalesed  |> mapStringTag (\pathSet -> Set.diff pathSet sharedPaths))
              (rightCoalesed |> mapStringTag (\pathSet -> Set.diff pathSet sharedPaths))
              (Set.union sharedPaths pathSet)

    keepOutermostOnly pathsInAncestors stringTaggedWithProjectionPaths =
      let withAncestorPathsRemoved =
        stringTaggedWithProjectionPaths |> mapStringTag (\pathSet -> Set.diff pathSet pathsInAncestors)
      in
      case withAncestorPathsRemoved of
        TaggedString _ _                             -> withAncestorPathsRemoved
        TaggedStringAppend left right cleanedPathSet ->
          let
            pathSetWithAncestors = Set.union pathsInAncestors cleanedPathSet
            leftCleaned          = keepOutermostOnly pathSetWithAncestors left
            rightCleaned         = keepOutermostOnly pathSetWithAncestors right
          in
          TaggedStringAppend leftCleaned rightCleaned cleanedPathSet
  in
  stringTaggedWithProjectionPaths
  |> coalese
  |> keepOutermostOnly Set.empty


-- No Prelude for now.
evalToStringTaggedWithProjectionPaths : Exp -> TaggedValue -> Result String StringTaggedWithProjectionPaths
evalToStringTaggedWithProjectionPaths program valueOfInterestTagged =
  let initialEnv = [("valueOfInterestTagged", valueOfInterestTagged)] in
  eval initialEnv program |> Result.andThen (\w ->
    case taggedValueToMaybeStringTaggedWithProjectionPaths w of
      Just stringTagged -> Ok stringTagged
      Nothing           -> Err <| "Result was not just strings and appends! " ++ unparseToUntaggedString w
  )
  |> Result.map tidyUpProjectionPaths
