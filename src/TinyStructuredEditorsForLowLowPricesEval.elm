module TinyStructuredEditorsForLowLowPricesEval exposing (evalToStringTaggedWithProjectionPaths, tagVal)

import Char
import Set exposing (Set)
import String

import Lang
import LeoUnparser exposing (unparseType)
import Types2
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
  , operation     = \operandsTags                   -> Utils.unionAll operandsTags -- We do not apply this rule to string appends because it would be messy. Arguably, string append in our system is analogous to tupling or applying a constructor, both of which are unannotated in Acar et al. Fig. 12
  }


-- Dependency tagging evaluation on the core langauage.
eval : List Types2.DataTypeDef -> MultipleDispatchFunctions -> Env -> Exp -> Result String TaggedValue
eval dataTypeDefs multipleDispatchFunctions env exp =
  let recurse = eval dataTypeDefs multipleDispatchFunctions in
  let noTag v = TaggedValue v Set.empty in -- ⊥ in Acar et al. Fig. 12
  case exp of
    EFun funcName argName funcBody ->
      Ok <| TaggedValue (VClosure env funcName argName funcBody) dependencyAnnotators.function

    EVar varName ->
      case Utils.maybeFind varName env of
        Just boundValue ->
          Ok boundValue -- No extra annotation in Acar et al. Fig. 12
        Nothing ->
          case Utils.maybeFind3 varName multipleDispatchFunctions of
            Just _  -> Ok <| TaggedValue (VClosureDynamic varName) dependencyAnnotators.function
            Nothing -> Err <| "Variable " ++ varName ++ " not found!"

    EApp funcExp argExp ->
      recurse env funcExp |> Result.andThen (\funcTaggedVal ->
      recurse env argExp  |> Result.andThen (\argTaggedVal ->
        case funcTaggedVal.v of
          VClosure funcEnv funcName argName funcBody ->
            let
              multipleDispatchImplementationNames = multipleDispatchFunctions |> List.map (\(_, _, uniqueName) -> uniqueName)
              implementationNamesNotInFuncEnv =
                multipleDispatchImplementationNames
                |> List.filter (\implementationName -> Utils.maybeFind implementationName funcEnv == Nothing)
              envToPropagateToCall =
                env
                |> List.filter (\(name, _) -> List.member name implementationNamesNotInFuncEnv)
            in
            recurse ([(funcName, funcTaggedVal), (argName, argTaggedVal)] ++ envToPropagateToCall ++ funcEnv) funcBody
            |> Result.map (\resultTaggedVal ->
              resultTaggedVal |> setTag (dependencyAnnotators.application funcTaggedVal.paths resultTaggedVal.paths)
            )

          VClosureDynamic funcName ->
            case findMultipleDispatchImplementationNameBasedOnArgType dataTypeDefs multipleDispatchFunctions funcName argTaggedVal of
              Just implementationUniqueName ->
                EApp (EVar implementationUniqueName) (EVar "evaled arg")
                |> recurse (("evaled arg", argTaggedVal)::env)
              Nothing ->
                Err <| "Could not find matching " ++ funcName ++ " implementation for argument " ++ unparseToUntaggedString argTaggedVal ++ "!"

          _ ->
            Err <| "Left side of application should be a function but got " ++ unparseToUntaggedString funcTaggedVal ++ " in " ++ toString exp ++ "!"
      ))

    ECtor ctorName argExps ->
      argExps
      |> List.map (recurse env)
      |> Utils.projOk
      |> Result.map (\argTaggedVals -> VCtor ctorName argTaggedVals)
      |> Result.map noTag -- No annotation in Acar et al. Fig. 12

    ECase scrutinee branches ->
      recurse env scrutinee |> Result.andThen (\scrutineTaggedVal ->
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
                    recurse (bindings ++ env) branchExp
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
      Result.map2 VAppend (recurse env e1) (recurse env e2)
      |> Result.map noTag

    ENum num ->
      Ok <| TaggedValue (VNum num) dependencyAnnotators.constant

    ENumToString argExp ->
      recurse env argExp
      |> Result.andThen (\argTaggedVal ->
        case argTaggedVal.v of
          VNum num -> Ok <| TaggedValue (VString (toString num)) (dependencyAnnotators.operation [argTaggedVal.paths])
          _        -> Err <| "Built-in toString only supports numbers but was given " ++ unparseToUntaggedString argTaggedVal ++ "!"
      )

    ENumLTE e1 e2 ->
      recurse env e1 |> Result.andThen (\taggedVal1 ->
      recurse env e2 |> Result.andThen (\taggedVal2 ->
        case (taggedVal1.v, taggedVal2.v) of
          (VNum num1, VNum num2) -> Ok <| TaggedValue (VCtor (if num1 <= num2 then "True" else "False") []) (dependencyAnnotators.operation [taggedVal1.paths, taggedVal2.paths])
          _                      -> Err <| "Built-in <= only supports numbers but was given " ++ unparseToUntaggedString taggedVal1 ++ " and " ++ unparseToUntaggedString taggedVal2 ++ "!"
      ))


-- Set up the value of interest with path tags, which are propagated during evaluation.
--
-- Fig. 11 "Path annotation operation" in Acar et al.
-- AddTag/AddTagDeep in our appendix. We handle closures differently than Acar et al., but we also don't expect closures in the initial value of interest so it doesn't matter.
tagVal : ProjectionPath -> TaggedValue -> TaggedValue
tagVal path w =
  let deeperTagged =
    case w.v of
      VClosure funcEnv funcName varName body ->
        w

      VClosureDynamic ident ->
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


findMultipleDispatchImplementationNameBasedOnArgType : List Types2.DataTypeDef -> MultipleDispatchFunctions -> Ident -> TaggedValue -> Maybe Ident
findMultipleDispatchImplementationNameBasedOnArgType dataTypeDefs multipleDispatchFunctions funcName argTaggedVal =
  let
    candidateImplementations =
      multipleDispatchFunctions
      |> List.filter (\(name, _, _) -> name == funcName)

    maybeFirstArgType tipe =
      case Types2.matchArrowRecurse tipe of
        Just (typeVarNames, firstArgType::_, retType) -> Just firstArgType
        _                                             -> Nothing

    maybeTargetTypeName =
      case argTaggedVal.v of
        VClosure _ _ _ _  -> Nothing -- No higher-order dispatch.
        VClosureDynamic _ -> Nothing -- No higher-order dispatch.
        VCtor ctorName _  -> Types2.ctorNameToMaybeDataTypeDef ctorName dataTypeDefs |> Maybe.map (\(typeName, _) -> typeName)
        VString _         -> Just "String"
        VAppend _ _       -> Just "String"
        VNum _            -> Just "Num"

    typeToName tipe =
      let
        recurse = typeToName

        unsupported () =
          let _ = Utils.log <| "TinyStructuredEditorsForLowLowPricesEval.findMultipleDispatchImplementationNameBasedOnArgType does not yet support " ++ unparseType tipe in
          " Not Found "

        handleVarOrApp () =
          case Types2.varOrAppToMaybeIdentAndArgTypes tipe of
            Just (typeName, argTypes) -> typeName
            Nothing                   -> unsupported ()
      in
      case Lang.unwrapType tipe of
        Lang.TNum _                                   -> "Num"
        Lang.TBool _                                  -> Debug.crash <| "TinyStructuredEditorsForLowLowPricesEval.findMultipleDispatchImplementationNameBasedOnArgType: TBools should not occur here: should already be converted to TVar instead!"
        Lang.TString _                                -> "String"
        Lang.TNull _                                  -> unsupported ()
        Lang.TList _ elemType _                       -> unsupported ()
        Lang.TDict _ keyType valueType _              -> unsupported ()
        Lang.TRecord _ maybeExtendVarNameWs entries _ -> unsupported () -- Should just see TApp's...
        Lang.TTuple _ headTypes _ maybeRestType _     -> unsupported ()
        Lang.TArrow _ typeList _                      -> unsupported ()
        Lang.TUnion _ typeList _                      -> unsupported ()
        Lang.TApp _ fType argTypes _                  -> handleVarOrApp ()
        Lang.TVar _ name                              -> handleVarOrApp ()
        Lang.TForall _ tPats innerType _              -> unsupported ()
        Lang.TParens _ innerType _                    -> recurse innerType
        Lang.TWildcard _                              -> unsupported ()

    maybeSpecificCandidate =
      case maybeTargetTypeName of
        Just targetTypeName ->
          candidateImplementations
          |> Utils.findFirst (\(_, funcType, _) ->
            case maybeFirstArgType funcType of
              Just firstArg -> typeToName firstArg == targetTypeName
              Nothing       -> False
          )
        Nothing ->
          Nothing

    isTypeVar tipe =
      case Lang.unwrapType tipe of
        Lang.TVar _ name -> String.toList name |> List.map (Char.isLower) |> List.head |> (==) (Just True)
        _                -> False

    maybeNonSpecificCandidate =
      candidateImplementations
      |> Utils.findFirst (\(_, funcType, _) ->
        maybeFirstArgType funcType
        |> Maybe.map isTypeVar
        |> Maybe.withDefault False
      )
  in
  [ maybeSpecificCandidate
  , maybeNonSpecificCandidate
  ]
  |> Utils.firstMaybe
  |> Maybe.map (\(_, _, uniqueImplementationName) -> uniqueImplementationName)


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


-- No full Prelude for now.
evalToStringTaggedWithProjectionPaths : List Types2.DataTypeDef -> MultipleDispatchFunctions -> Exp -> TaggedValue -> Result String StringTaggedWithProjectionPaths
evalToStringTaggedWithProjectionPaths dataTypeDefs multipleDispatchFunctions program valueOfInterestTagged =
  let initialEnv =
    let
      compose =
        -- (<<) f g = \x -> f (g x)
        VClosure [] "<<" "f" (EFun "" "g" (EFun "" "x"
            (EApp (EVar "f")
                (EApp (EVar "g") (EVar "x"))
            )
        ))

      lte =
        -- (<=) l r = builtInLTE l r
        VClosure [] "<=" "l" (EFun "" "r"
          (ENumLTE (EVar "l") (EVar "r"))
        )

      gte =
        -- (>=) l r = builtInLTE r l
        VClosure [] "<=" "l" (EFun "" "r"
          (ENumLTE (EVar "r") (EVar "l"))
        )
    in
    [ ("valueOfInterestTagged", valueOfInterestTagged)
    , ("numToStringBuiltin", noTag <| VClosure [] "numToStringBuiltin" "x" (ENumToString (EVar "x")))
    , ("<<", noTag <| compose)
    , ("<=", noTag <| lte)
    , (">=", noTag <| gte)
    ]
  in
  eval dataTypeDefs multipleDispatchFunctions initialEnv program |> Result.andThen (\w ->
    case taggedValueToMaybeStringTaggedWithProjectionPaths w of
      Just stringTagged -> Ok  <| mapStringTag (Set.insert []) stringTagged -- Tag whole string with root path. TODO: do this at every invocation of toString
      Nothing           -> Err <| "Result was not just strings and appends! " ++ unparseToUntaggedString w
  )
  |> Result.map tidyUpProjectionPaths
