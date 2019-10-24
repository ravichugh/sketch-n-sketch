module TinyStructuredEditorsForLowLowPricesActions exposing (generateActionsForValueAndAssociateWithStringLocations, replaceAtPath)

import Dict exposing (Dict)
import Set exposing (Set)

import Lang
import LeoUnparser exposing (unparseType)
import Types2
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)
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
-- Run dataTypeDefs and dataConDef through expandType if you need to handle type aliases.
--
-- Not particularly precise in the presence of type variables.
-- This function is unsatisfying but may be sufficient for our examples.
isTerminalDataConDef : List Types2.DataTypeDef -> List Ident -> Types2.DataConDef -> Bool
isTerminalDataConDef dataTypeDefs dataTypeNamesSeen (ctorName, argTypes) =
  argTypes
  |> List.all
      (\argType ->
        case Types2.varOrAppToMaybeIdentAndArgTypes argType of
          Just (typeName, _) ->
            if List.member typeName dataTypeNamesSeen then
              False
            else
              case Utils.maybeFind typeName dataTypeDefs of
                Just (typeArgNames, dataConDefs) -> dataConDefs |> List.any (isTerminalDataConDef dataTypeDefs (typeName::dataTypeNamesSeen))
                Nothing                          -> True
          Nothing ->
            True
      )


-- When we can't copy from the current ctor.
--
-- Doesn't handle type vars correctly yet.
maybeDefaultValueForType : List Types2.DataTypeDef -> Lang.Type -> Maybe TaggedValue
maybeDefaultValueForType dataTypeDefs tipe =
  -- No type aliases for now.
  let
    recurse = maybeDefaultValueForType dataTypeDefs

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
          case Utils.maybeFind typeName dataTypeDefs of
            Just (typeArgNames, dataConDefs) ->
              let
                maybeDataConDefToUse =
                  dataConDefs
                  |> Utils.findFirst (isTerminalDataConDef dataTypeDefs [typeName])
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


-- Given a value, generates SpecificActions for that value and then
-- associates those actions with projection paths that appears in the string.
-- Since not all projection paths for which we generate actions are guarenteed to
-- appear in the toString representation, such actions are assigned to a node closer to the root.
--
-- The returned dict can be thought of as a many-to-1 mapping, actions are not duplicated.
generateActionsForValueAndAssociateWithStringLocations
  :  List Types2.DataTypeDef
  -> Maybe Lang.Type
  -> TaggedValue
  -> StringTaggedWithProjectionPaths
  -> Dict ProjectionPath (List SpecificAction)
generateActionsForValueAndAssociateWithStringLocations dataTypeDefs maybeValueOfInterestType valueOfInterestTagged stringTaggedWithProjectionPaths =
  let
    specificActions : Set SpecificAction
    specificActions =
      let
        _ =
          if maybeValueOfInterestType == Nothing
          then Utils.log "No type provided/inferred for TinyStructuredEditorsForLowLowPrices value of interest. Polymorphic type variable will not be instantiated causing some actions to be unavailable."
          else ()
      in
      valToSpecificActions
          dataTypeDefs
          valueOfInterestTagged
          maybeValueOfInterestType
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


replaceAtPath : ProjectionPath -> TaggedValue -> TaggedValue -> TaggedValue
replaceAtPath pathToReplace replacement rootValueOfInterestTagged =
  let clearTags = mapTaggedValue (.v >> noTag) in
  rootValueOfInterestTagged
  |> mapTaggedValue
      (\subvalueOfInterestTagged ->
        if subvalueOfInterestTagged.paths == Set.singleton pathToReplace
        then replacement
        else subvalueOfInterestTagged
      )
  |> clearTags


-- Type, if given, should be concrete: no free variables.
-- (That's the point of providing a type: so we can know when `List a` is actually `List Num` and provide more actions.)
valToSpecificActions : List Types2.DataTypeDef -> TaggedValue -> Maybe Lang.Type -> TaggedValue -> Set SpecificAction
valToSpecificActions dataTypeDefs rootValueOfInterestTagged maybeType valueOfInterestTagged =
  let
    recurse = valToSpecificActions dataTypeDefs rootValueOfInterestTagged

    -- In practice, should always result in one action.
    replacementActionSet : ChangeType -> TaggedValue -> Set SpecificAction
    replacementActionSet changeType taggedValue =
      valueOfInterestTagged.paths -- valueOfInterest should be freshly tagged so there should always be exactly 1 tag
      |> Set.map (\path -> NewValue changeType path (replaceAtPath path taggedValue rootValueOfInterestTagged))
  in
  case valueOfInterestTagged.v of
    VClosure _ _ _ _ ->
      Set.empty

    VClosureDynamic _ ->
      Set.empty

    VCtor ctorName argVals ->
      case Types2.ctorNameToMaybeDataTypeDef ctorName dataTypeDefs of
        Just (thisTypeName, (thisTypeArgNames, thisTypeDataConDefs)) ->
          let
            -- If no type variables, we don't necessarily need the explicit type annotation.
            thisType =
              if thisTypeArgNames == []
              then maybeType |> Maybe.withDefault (Lang.tVar0 thisTypeName)
              else maybeType |> Maybe.withDefault (Lang.tVar0 "*** no type ***")

            typeVarNameToType : List (Ident, Lang.Type)
            typeVarNameToType =
              maybeType
              -- |> Debug.log "maybeType"
              |> Maybe.andThen Types2.varOrAppToMaybeIdentAndArgTypes
              |> Maybe.map (\(_, argTypes) -> argTypes)
              |> Maybe.withDefault []
              |> Utils.zip thisTypeArgNames
              -- |> Debug.log "typeVarNameToType"

            thisTypeDataConDefsReified =
              thisTypeDataConDefs
              |> List.map (\(ctorName, ctorArgTypes) -> (ctorName, ctorArgTypes |> List.map (Lang.applyTypeSubst typeVarNameToType)))

            thisCtorArgTypes =
              ctorName
              |> Utils.find "TinyStructuredEditorsForLowLowPricesActions.valToSpecificActions changeCtorActions" thisTypeDataConDefsReified

            otherConDefs =
              thisTypeDataConDefsReified
              |> List.filter (Tuple.first >> (/=) ctorName)

            deeperActions =
              List.map2 recurse (List.map Just thisCtorArgTypes) argVals
              |> Utils.unionAll

            removeActions =
              -- Only single recursion supported for removal.
              -- Replace with any child of the same type.
              --
              -- Place each remove action on:
              -- 1. The node being removed, and
              -- 2. The immediate children of the node being removed, but not the child retained instead. These are
              --    the contained values that will be removed by the removal of their container.
              let
                typeEnv           = []
                recursiveArgIs    = thisCtorArgTypes |> Utils.findAllIndices (Types2.typeEquiv typeEnv thisType)

                -- The root value with this node replaced with each of its recursive children.
                --
                -- Returns List (childIndexReplaced, wholeReplacementVal)
                newValuesWithArgI : List (Int, TaggedValue)
                newValuesWithArgI =
                  Set.toList valueOfInterestTagged.paths -- should always be a singleton here.
                  |> Utils.cartProd recursiveArgIs
                  |> List.map (\(recursiveArgI, pathToReplace) -> (recursiveArgI, replaceAtPath pathToReplace (Utils.geti recursiveArgI argVals) rootValueOfInterestTagged))
              in
              argVals
              |> Utils.mapi1 (\(argI, argVal) -> (argI, argVal.paths)) -- Note: argVal.paths should always be a singleton set.
              |> (::) ((-1, valueOfInterestTagged.paths))              -- Also add actions to this node itself; it has no argI.
              |> Utils.cartProd newValuesWithArgI
              |> List.filter (\((replacedArgI, _       ), (pathsArgI, _    )) -> replacedArgI /= pathsArgI)
              |> List.map    (\((_,            newValue), (_,         paths)) -> paths |> Set.map (\path -> NewValue Remove path newValue))
              |> Utils.unionAll

            insertActions =
              -- Only single recursion supported for insert.
              -- For any ctor that has an arg of the same type, puts the current node there.
              -- Use defaults for the rest of the arguments.
              let typeEnv = [] in
              thisTypeDataConDefsReified
              |> List.concatMap
                  (\(ctorName, ctorArgTypes) ->
                    let
                      recursiveArgIs            = ctorArgTypes |> Utils.findAllIndices (Types2.typeEquiv typeEnv thisType)
                      ctorDefaultArgumentMaybes = ctorArgTypes |> List.map (maybeDefaultValueForType dataTypeDefs)
                    in
                    case Utils.projJusts ctorDefaultArgumentMaybes of
                      Just ctorDefaultArguments ->
                        recursiveArgIs
                        |> List.map
                            (\argI ->
                              let ctorArgVals = ctorDefaultArguments |> Utils.replacei argI valueOfInterestTagged in
                              replacementActionSet Insert (noTag <| VCtor ctorName ctorArgVals)
                            )

                      Nothing ->
                        []
                  )
              |> Utils.unionAll

            changeCtorActions =
              -- For each alternative ctor, fill in args with first matching type; otherwise default
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
                                Nothing    -> maybeDefaultValueForType dataTypeDefs otherCtorArgType |> Debug.log "maybeDefaultValueForType"
                            )
                        |> Utils.projJusts
                    in
                    case Debug.log "maybeNewArgVals" maybeNewArgVals of
                      Just newArgVals -> replacementActionSet ChangeCtor (noTag <| VCtor otherCtorName newArgVals)
                      Nothing         -> Set.empty
                  )
              |> Utils.unionAll
          in
          Utils.unionAll [removeActions, insertActions, changeCtorActions, deeperActions]

        Nothing ->
          let _ = Utils.log <| "TinyStructuredEditorsForLowLowPricesActions.valToSpecificActions warning: not find ctor " ++ ctorName ++ " in dataTypeDefs: " ++ toString dataTypeDefs in
          Set.empty

    VString _ ->
      -- In practice, should always result in one action.
      Set.map EditText valueOfInterestTagged.paths

    VAppend w1 w2 ->
      let _ = Utils.log "Did not expect a VAppend in TinyStructuredEditorsForLowLowPricesActions.valToSpecificActions" in
      Set.union (recurse maybeType w1) (recurse maybeType w2)

    VNum _ ->
      -- In practice, should always result in one action.
      Set.map Scrub valueOfInterestTagged.paths
