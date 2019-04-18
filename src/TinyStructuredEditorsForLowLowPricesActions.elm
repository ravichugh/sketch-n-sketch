module TinyStructuredEditorsForLowLowPricesActions exposing (generateActionsForValueAndAssociateWithStringLocations)

import Set exposing (Set)

import Lang
import LeoUnparser exposing (unparseType)
import Types2
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)
import TinyStructuredEditorsForLowLowPricesDesugaring exposing (desugarVal)
import TinyStructuredEditorsForLowLowPricesEval exposing (tagVal)



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


generateActionsForValueAndAssociateWithStringLocations
  :  Lang.Exp
  -> Lang.Val
  -> StringTaggedWithProjectionPaths
  -> StringTaggedWithSpecificActions
generateActionsForValueAndAssociateWithStringLocations program valueOfInterest stringTaggedWithProjectionPaths =
  let
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
          (valueOfInterest |> desugarVal |> tagVal [])
  in
  stringTaggedWithProjectionPaths
  |> mapTaggedStringTags (\pathSet -> specificActions)


mapTaggedStringTags : (Set t1 -> Set t2) -> AppendedTaggedStrings t1 -> AppendedTaggedStrings t2
mapTaggedStringTags f taggedString =
  let recurse = mapTaggedStringTags f in
  case taggedString of
    TaggedString string tagSet           -> TaggedString string (f tagSet)
    TaggedStringAppend left right tagSet -> TaggedStringAppend (recurse left) (recurse right) (f tagSet)


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
                        valueOfInterestTagged.paths -- valueOfInterest should be freshly tagged so there should only be at most 1 tag
                        |> Set.map (\path -> Replace path (noTag <| VCtor otherCtorName newArgVals))
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
