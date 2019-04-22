module TinyStructuredEditorsForLowLowPricesActions exposing (generateActionsForValueAndAssociateWithStringLocations, applyReplacement)

import Dict exposing (Dict)
import Set exposing (Set)

import Lang
import LeoUnparser exposing (unparseType)
import Types2
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)
import TinyStructuredEditorsForLowLowPricesEval exposing (tagVal)


applyReplacement : ProjectionPath -> TaggedValue -> TaggedValue -> TaggedValue
applyReplacement pathToReplace replacement valueOfInterestTagged =
  valueOfInterestTagged
  |> mapTaggedValue
      (\subvalueOfInterestTagged ->
        if subvalueOfInterestTagged.paths == Set.singleton pathToReplace
        then tagVal pathToReplace replacement
        else subvalueOfInterestTagged
      )


-- By default we attempt to copy ctor arguments from the current value.
--
-- Returns a list of indices into the old argument list pointing to where
-- new arguments should be copied from. A value of "Nothing" indicates not
-- to copy (use a generic default instead).
--
-- The list is the same length as the number of arguments to the new ctor.
--
-- Algorithm: For each new arg, use the first yet unused arg of the same
--            type from the old args.
defaultArgumentMappingForCtorChange : List Lang.Type -> List Lang.Type -> List (Maybe Int)
defaultArgumentMappingForCtorChange oldCtorArgTypes newCtorArgTypes =
  let
    typeEnv = [] -- No type aliases allowed for now.

    -- Tag each old arg as unused at the beginning of the fold.
    oldArgsUsedAndTypes : List (Bool, Lang.Type)
    oldArgsUsedAndTypes = List.map ((,) False) oldCtorArgTypes

    markUsed : Int -> List (Bool, Lang.Type) -> List (Bool, Lang.Type)
    markUsed i oldArgsUsedAndTypes =
      oldArgsUsedAndTypes
      |> Utils.getReplacei1 i (\(_, oldArgType) -> (True, oldArgType))

    (mapping, _) =
      newCtorArgTypes
      |> Utils.foldl
          ([], oldArgsUsedAndTypes)
          (\newArgType (mapping, oldArgsUsedAndTypes) ->
            let
              maybeUnusedMatchingArgI =
                oldArgsUsedAndTypes
                |> Utils.findi (\(isUsed, oldArgType) -> not isUsed && Types2.typeEquiv typeEnv oldArgType newArgType)
            in
            case maybeUnusedMatchingArgI of
              Just unusedMatchingArgI ->
                ( mapping ++ [Just unusedMatchingArgI]
                , oldArgsUsedAndTypes |> markUsed unusedMatchingArgI
                )

              _ ->
                ( mapping ++ [Nothing]
                , oldArgsUsedAndTypes
                )
          )
  in
  mapping


-- No type alias support yet.
-- Run dataTypeDefsWithoutTBools and dataConDef through expandType if you need to handle type aliases.
--
-- Not particularly precise in the presence of type variables.
-- This function is unsatisfying but may be sufficient for our examples.
isTerminalDataConDef : List Types2.DataTypeDef -> List Ident -> Types2.DataConDef -> Bool
isTerminalDataConDef dataTypeDefsWithoutTBools dataTypeNamesSeen (ctorName, argTypes) =
  argTypes
  |> List.all
      (\argType ->
        case Types2.varOrAppToMaybeIdentAndArgTypes argType of
          Just (typeName, _) ->
            if List.member typeName dataTypeNamesSeen then
              False
            else
              case Utils.maybeFind typeName dataTypeDefsWithoutTBools of
                Just dataConDefs -> dataConDefs |> List.any (isTerminalDataConDef dataTypeDefsWithoutTBools (typeName::dataTypeNamesSeen))
                Nothing          -> True
          Nothing ->
            True
      )


-- When we can't copy from the current ctor.
--
-- Doesn't handle type vars correctly yet.
maybeDefaultValueForType : List Types2.DataTypeDef -> Lang.Type -> Maybe TaggedValue
maybeDefaultValueForType dataTypeDefsWithoutTBools tipe =
  -- No type aliases for now.
  let
    recurse = maybeDefaultValueForType dataTypeDefsWithoutTBools

    _ =
      if Lang.isDeprecatedType tipe
      then Utils.log <| unparseType tipe ++ " is deprecated! (seen in TinyStructuredEditorsForLowLowPricesActions.defaultValueForType)"
      else ()

    return untaggedPrevalue = Just (noTag untaggedPrevalue)

    unsupported () =
      let _ = Utils.log <| "TinyStructuredEditorsForLowLowPricesActions.defaultValueForType does not yet support " ++ unparseType tipe in
      Nothing

    handleVarOrApp () =
      case Types2.varOrAppToMaybeIdentAndArgTypes tipe of
        Just (typeName, argTypes) ->
          case Utils.maybeFind typeName dataTypeDefsWithoutTBools of
            Just dataConDefs ->
              let
                maybeDataConDefToUse =
                  dataConDefs
                  |> Utils.findFirst (isTerminalDataConDef dataTypeDefsWithoutTBools [typeName])
              in
              case maybeDataConDefToUse of
                Just (ctorName, argTypes) ->
                  List.map recurse argTypes
                  |> Utils.projJusts
                  |> Maybe.map (noTag << VCtor ctorName)

                Nothing ->
                  let _ = Utils.log <| "TinyStructuredEditorsForLowLowPricesActions.defaultValueForType cannot find non-recursive constructor for " ++ typeName in
                  Nothing

            Nothing ->
              let _ = Utils.log <| "TinyStructuredEditorsForLowLowPricesActions.defaultValueForType cannot find data type definition for " ++ typeName in
              Nothing

        Nothing ->
          unsupported ()
  in
  case Lang.unwrapType tipe of
    Lang.TNum _                                   -> return <| VNum 0.0
    Lang.TBool _                                  -> Debug.crash <| "TinyStructuredEditorsForLowLowPricesActions.defaultValueForType: TBools should not occur here: should already be converted to TVar instead!"
    Lang.TString _                                -> return <| VString ""
    Lang.TNull _                                  -> unsupported ()
    Lang.TList _ elemType _                       -> unsupported ()
    Lang.TDict _ keyType valueType _              -> unsupported ()
    Lang.TRecord _ maybeExtendVarNameWs entries _ -> unsupported () -- Should just see TApp's...
    Lang.TTuple _ headTypes _ maybeRestType _     -> unsupported ()
    Lang.TArrow _ typeList _                      -> unsupported ()
    Lang.TUnion _ typeList _                      -> unsupported ()
    Lang.TApp _ fType argTypes _                  -> handleVarOrApp ()
    Lang.TVar _ name                              -> handleVarOrApp ()
    Lang.TForall _ tPats innerType _              -> unsupported () -- boundIdents = List.map tPatToIdent tPats
    Lang.TParens _ innerType _                    -> recurse innerType
    Lang.TWildcard _                              -> unsupported ()


ctorNameToMaybeDataTypeDef : Ident -> List Types2.DataTypeDef -> Maybe Types2.DataTypeDef
ctorNameToMaybeDataTypeDef targetCtorName dataTypeDefsWithoutTBools =
  dataTypeDefsWithoutTBools
  |> Utils.findFirst
      (\(typeName, dataConDefs) ->
        dataConDefs
        |> List.any (\(ctorName, ctorArgTypes) -> ctorName == targetCtorName)
      )


-- Given a value, generates SpecificActions for that value and then
-- associates those actions with projection paths that appears in the string.
-- Since not all projection paths for which we generate actions are guarenteed to
-- appear in the toString representation, such actions are assigned to a node closer to the root.
--
-- The returned dict is a 1-to-1 mapping (actions are not duplicated).
generateActionsForValueAndAssociateWithStringLocations
  :  Lang.Exp
  -> TaggedValue
  -> StringTaggedWithProjectionPaths
  -> Dict ProjectionPath (List SpecificAction)
generateActionsForValueAndAssociateWithStringLocations program valueOfInterestTagged stringTaggedWithProjectionPaths =
  let
    specificActions : Set SpecificAction
    specificActions =
      let
        -- Use a data type for booleans instead of primitive Lang.TBool.
        dataTypeDefsWithoutTBools =
          let replaceTBoolWithTVarInDataConDef (ctorName, argTypes) =
            let replaceTBoolWithTVar tipe =
              case Lang.unwrapType tipe of
                Lang.TBool _ -> Lang.tVar0 "Bool"
                _            -> tipe
            in
            ( ctorName
            , argTypes |> List.map (Lang.mapType replaceTBoolWithTVar)
            )
          in
          Types2.getDataTypeDefs program
          |> List.map (\(dataTypeName, dataConDefs) -> (dataTypeName, List.map replaceTBoolWithTVarInDataConDef dataConDefs))
          |> (::) ("Bool", [("True", []), ("False", [])])
      in
      valToSpecificActions
          dataTypeDefsWithoutTBools
          valueOfInterestTagged

    projectionPathsInString : Set ProjectionPath
    projectionPathsInString =
      gatherStringTags stringTaggedWithProjectionPaths
      |> Utils.unionAll

    projectionPathsInActions : List ProjectionPath
    projectionPathsInActions =
      specificActions
      |> Set.map specificActionProjectionPath
      |> Set.toList

    -- Note: can't change the paths on the actions themselves because the path recorded on
    -- each action refers to where in the value to change. Hence we build a map.
    actionProjectionPathToStringProjectionPath : Dict ProjectionPath ProjectionPath
    actionProjectionPathToStringProjectionPath =
      let makeActionProjectionPathToStringProjectionPathEntry actionProjectionPath =
        let stringProjectionPath =
          Utils.prefixes actionProjectionPath -- Longest prefix (the original path) appears first.
          |> Utils.findFirst (flip Set.member projectionPathsInString)
          |> Utils.maybeWithDefaultLazy (\_ -> Debug.crash <| "TinyStructuredEditorsForLowLowPricesActions.generateActionsForValueAndAssociateWithStringLocations expected projectionPathsInString to have a root element []! " ++ toString projectionPathsInString)
        in
        (actionProjectionPath, stringProjectionPath)
      in
      projectionPathsInActions
      |> List.map makeActionProjectionPathToStringProjectionPathEntry
      |> Dict.fromList


    stringProjectionPathToSpecificActions : Dict ProjectionPath (List SpecificAction)
    stringProjectionPathToSpecificActions =
      let makeStringProjectionPathToActionEntry specificAction =
        let stringProjectionPath =
          let errStr = "TinyStructuredEditorsForLowLowPricesActions.generateActionsForValueAndAssociateWithStringLocations expected to find action projection path in actionProjectionPathToStringProjectionPath!" in
          actionProjectionPathToStringProjectionPath
          |> Utils.justGet_ errStr (specificActionProjectionPath specificAction)
        in
        (stringProjectionPath, specificAction)
      in
      specificActions
      |> Set.toList
      |> List.map makeStringProjectionPathToActionEntry
      |> Utils.pairsToDictOfLists
  in
  stringProjectionPathToSpecificActions


valToSpecificActions : List Types2.DataTypeDef -> TaggedValue -> Set SpecificAction
valToSpecificActions dataTypeDefsWithoutTBools valueOfInterestTagged =
  let recurse = valToSpecificActions dataTypeDefsWithoutTBools in
  case valueOfInterestTagged.v of
    VClosure _ _ _ ->
      Set.empty

    VCtor ctorName argVals ->
      let
        deeperActions = argVals |> List.map recurse |> Utils.unionAll
      in
      case ctorNameToMaybeDataTypeDef ctorName dataTypeDefsWithoutTBools of
        Just (thisTypeName, thisTypeDataConDefs) ->
          let
            removeActions =
              Set.empty

            insertActions =
              Set.empty

            changeCtorActions =
              -- For each alternative ctor, fill in args with first matching type; otherwise default
              let
                thisCtorArgTypes =
                  ctorName |> Utils.find "TinyStructuredEditorsForLowLowPricesActions.valToSpecificActions changeCtorActions" thisTypeDataConDefs

                otherConDefs =
                  thisTypeDataConDefs
                  |> List.filter (Tuple.first >> (/=) ctorName)
              in
              otherConDefs
              |> List.map
                  (\(otherCtorName, otherCtorArgTypes) ->
                    let
                      _ = Debug.log "(ctorName, otherCtorName)" (ctorName, otherCtorName)

                      copyMapping : List (Maybe Int) -- Indices into thisCtor's args
                      copyMapping =
                        defaultArgumentMappingForCtorChange thisCtorArgTypes otherCtorArgTypes

                      maybeNewArgVals : Maybe (List TaggedValue)
                      maybeNewArgVals =
                        Utils.zip otherCtorArgTypes copyMapping
                        |> List.map
                            (\(otherCtorArgType, maybeCopyI) ->
                              case maybeCopyI of
                                Just copyI -> Just <| Utils.geti copyI argVals
                                Nothing    -> maybeDefaultValueForType dataTypeDefsWithoutTBools otherCtorArgType
                            )
                        |> Utils.projJusts
                    in
                    case maybeNewArgVals of
                      Just newArgVals ->
                        let clearTags = mapTaggedValue (.v >> noTag) in
                        valueOfInterestTagged.paths -- valueOfInterest should be freshly tagged so there should only be at most 1 tag
                        |> Set.map (\path -> Replace path (clearTags <| noTag <| VCtor otherCtorName newArgVals))
                      Nothing -> Set.empty
                  )
              |> Utils.unionAll
          in
          Utils.unionAll [removeActions, insertActions, changeCtorActions, deeperActions]

        Nothing ->
          let _ = Utils.log <| "TinyStructuredEditorsForLowLowPricesActions.valToSpecificActions warning: not find ctor " ++ ctorName ++ " in dataTypeDefs: " ++ toString dataTypeDefsWithoutTBools in
          deeperActions

    VString _ ->
      valueOfInterestTagged.paths |> Set.map Scrub

    VAppend w1 w2 ->
      let _ = Utils.log "Did not expect a VAppend in TinyStructuredEditorsForLowLowPricesActions.valToSpecificActions" in
      Set.union (recurse w1) (recurse w2)

    VNum _ ->
      valueOfInterestTagged.paths |> Set.map Scrub
