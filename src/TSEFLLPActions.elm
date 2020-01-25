module TSEFLLPActions exposing (makeProjectionPathToSpecificActions, makeProjectionPathToType, replaceAtPath, arrangeInsertActions)

import Dict exposing (Dict)
import Set exposing (Set)

import Lang
import LeoUnparser exposing (unparseType)
import Types2
import Utils

import TSEFLLPTypes exposing (..)
import TSEFLLPEval exposing (tagVal)
import TSEFLLPPolys


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
      then Utils.log <| unparseType tipe ++ " is deprecated! (seen in TSEFLLPActions.defaultValueForType)"
      else ()

    return untaggedPrevalue = Just (noTag untaggedPrevalue)

    unsupported () =
      let _ = Utils.log <| "TSEFLLPActions.defaultValueForType does not yet support " ++ unparseType tipe in
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
                  let _ = Utils.log <| "TSEFLLPActions.defaultValueForType cannot find non-recursive constructor for " ++ typeName in
                  Nothing

            Nothing ->
              let _ = Utils.log <| "TSEFLLPActions.defaultValueForType cannot find data type definition for " ++ typeName in
              Nothing

        Nothing ->
          unsupported ()
  in
  case Lang.unwrapType tipe of
    Lang.TNum _                                   -> return <| VNum 0.0
    Lang.TBool _                                  -> Debug.crash <| "TSEFLLPActions.defaultValueForType: TBools should not occur here: should already be converted to TVar instead!"
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


-- -- Given a value, generates SpecificActions for that value and then
-- -- associates those actions with projection paths that appears in the string.
-- -- Since not all projection paths for which we generate actions are guarenteed to
-- -- appear in the toString representation, such actions are assigned to a node closer to the root.
-- --
-- -- The returned dict can be thought of as a many-to-1 mapping, actions are not duplicated.
-- generateActionsForValueAndAssociateWithStringLocations
--   :  List Types2.DataTypeDef
--   -> Maybe Lang.Type
--   -> TaggedValue
--   -> StringTaggedWithProjectionPaths
--   -> Dict ProjectionPath (List SpecificAction)
-- generateActionsForValueAndAssociateWithStringLocations dataTypeDefs maybeValueOfInterestType valueOfInterestTagged stringTaggedWithProjectionPaths =
--   let
--     specificActions : Set SpecificAction
--     specificActions =
--       let
--         _ =
--           if maybeValueOfInterestType == Nothing
--           then Utils.log "No type provided/inferred for TSEFLLP value of interest. Polymorphic type variable will not be instantiated causing some actions to be unavailable."
--           else ()
--       in
--       valToSpecificActions
--           dataTypeDefs
--           valueOfInterestTagged
--           maybeValueOfInterestType
--           valueOfInterestTagged
--
--     projectionPathsInString : Set ProjectionPath
--     projectionPathsInString =
--       gatherStringTags stringTaggedWithProjectionPaths
--       |> Utils.unionAll
--
--     projectionPathsInActions : List ProjectionPath
--     projectionPathsInActions =
--       specificActions
--       |> Set.map specificActionProjectionPath
--       |> Set.toList
--
--     -- Note: can't change the paths on the actions themselves because the path recorded on
--     -- each action refers to where in the value to change. Hence we build a map.
--     actionProjectionPathToStringProjectionPath : Dict ProjectionPath ProjectionPath
--     actionProjectionPathToStringProjectionPath =
--       let makeActionProjectionPathToStringProjectionPathEntry actionProjectionPath =
--         let stringProjectionPath =
--           Utils.prefixes actionProjectionPath -- Longest prefix (the original path) appears first.
--           |> Utils.findFirst (flip Set.member projectionPathsInString)
--           |> Utils.maybeWithDefaultLazy (\_ -> Debug.crash <| "TSEFLLPActions.generateActionsForValueAndAssociateWithStringLocations expected projectionPathsInString to have a root element []! " ++ toString projectionPathsInString)
--         in
--         (actionProjectionPath, stringProjectionPath)
--       in
--       projectionPathsInActions
--       |> List.map makeActionProjectionPathToStringProjectionPathEntry
--       |> Dict.fromList
--
--
--     stringProjectionPathToSpecificActions : Dict ProjectionPath (List SpecificAction)
--     stringProjectionPathToSpecificActions =
--       let makeStringProjectionPathToActionEntry specificAction =
--         let stringProjectionPath =
--           let errStr = "TSEFLLPActions.generateActionsForValueAndAssociateWithStringLocations expected to find action projection path in actionProjectionPathToStringProjectionPath!" in
--           actionProjectionPathToStringProjectionPath
--           |> Utils.justGet_ errStr (specificActionProjectionPath specificAction)
--         in
--         (stringProjectionPath, specificAction)
--       in
--       specificActions
--       |> Set.toList
--       |> List.map makeStringProjectionPathToActionEntry
--       |> Utils.pairsToDictOfLists
--   in
--   stringProjectionPathToSpecificActions


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


makeProjectionPathToSpecificActions : List Types2.DataTypeDef -> TaggedValue -> Maybe Lang.Type -> Dict ProjectionPath (Set SpecificAction)
makeProjectionPathToSpecificActions dataTypeDefs rootValueOfInterestTagged maybeType =
  valToSpecificActions dataTypeDefs rootValueOfInterestTagged maybeType rootValueOfInterestTagged
  |> Set.toList
  |> List.map (\action -> (specificActionProjectionPath action, action))
  |> Utils.pairsToDictOfLists
  |> Dict.map (\projectionPath actions -> Set.fromList actions)


-- Apply type substitutions based on reified maybeType.
--
-- Does nothing interesting if maybeType is Nothing.
reifyDataConDefs : Maybe Lang.Type -> (List String, List (String, List Lang.Type)) -> List (String, List Lang.Type)
reifyDataConDefs maybeType (typeArgNames, typeDataConDefs) =
  let
    typeVarNameToType : List (Ident, Lang.Type)
    typeVarNameToType =
      maybeType
      -- |> Debug.log "maybeType"
      |> Maybe.andThen Types2.varOrAppToMaybeIdentAndArgTypes
      |> Maybe.map (\(_, argTypes) -> argTypes)
      |> Maybe.withDefault []
      |> Utils.zip typeArgNames
      -- |> Debug.log "typeVarNameToType"
  in
  typeDataConDefs
  |> List.map (\(ctorName, ctorArgTypes) -> (ctorName, ctorArgTypes |> List.map (Lang.applyTypeSubst typeVarNameToType)))


-- For displaying subvalue labels in the view.
--
-- Type, if given, should be concrete: no free variables. No handling of closures.
makeProjectionPathToType : List Types2.DataTypeDef -> Maybe Lang.Type -> TaggedValue -> Dict ProjectionPath Lang.Type
makeProjectionPathToType dataTypeDefs maybeType valueOfInterestTagged =
  let
    recurse = makeProjectionPathToType dataTypeDefs

    -- In practice, should always result in one action.
    logType : Dict ProjectionPath Lang.Type -> Lang.Type -> Dict ProjectionPath Lang.Type
    logType dict tipe =
      valueOfInterestTagged.paths -- valueOfInterest should be freshly tagged so there should always be exactly 1 tag
      |> Set.foldl (\path dict -> Dict.insert path tipe dict) dict
  in
  case valueOfInterestTagged.v of
    VClosure _ _ _ _ ->
      Dict.empty

    VClosureDynamic _ ->
      Dict.empty

    VCtor ctorName argVals ->
      case Types2.ctorNameToMaybeDataTypeDef ctorName dataTypeDefs of
        Just (thisTypeName, (thisTypeArgNames, thisTypeDataConDefs)) ->
          let
            -- If no type variables, we don't necessarily need the explicit type annotation.
            maybeThisType =
              case (thisTypeArgNames, maybeType) of
                ([], Nothing) -> Just (Lang.tVar0 thisTypeName)
                _             -> maybeType

            thisTypeDataConDefsReified = reifyDataConDefs maybeType (thisTypeArgNames, thisTypeDataConDefs)

            thisCtorArgTypes =
              ctorName
              |> Utils.find "TSEFLLPActions.makeProjectionPathToType thisCtorArgTypes" thisTypeDataConDefsReified

            dictDeeper =
              List.map2 recurse (List.map Just thisCtorArgTypes) argVals
              |> Utils.unionAllDicts
          in
          case maybeThisType of
            Nothing       -> dictDeeper
            Just thisType -> logType dictDeeper thisType

        Nothing ->
          let _ = Utils.log <| "TSEFLLPActions.makeProjectionPathToType warning: not find ctor " ++ ctorName ++ " in dataTypeDefs: " ++ toString dataTypeDefs in
          case maybeType of
            Nothing       -> Dict.empty
            Just thisType -> logType Dict.empty thisType

    VString _ ->
      logType Dict.empty <| Lang.withDummyTypeInfo (Lang.TString Lang.space0)

    VAppend w1 w2 ->
      let _ = Utils.log "Did not expect a VAppend in TSEFLLPActions.makeProjectionPathToType" in
      let dictDeeper = Dict.union (recurse maybeType w1) (recurse maybeType w2) in
      logType dictDeeper <| Lang.withDummyTypeInfo (Lang.TString Lang.space0)

    VNum _ ->
      logType Dict.empty <| Lang.withDummyTypeInfo (Lang.TNum Lang.space0)


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

            thisTypeDataConDefsReified = reifyDataConDefs maybeType (thisTypeArgNames, thisTypeDataConDefs)

            thisCtorArgTypes =
              ctorName
              |> Utils.find "TSEFLLPActions.valToSpecificActions changeCtorActions" thisTypeDataConDefsReified

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
              -- 1. NOT on the node being removed
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
              -- |> (::) ((-1, valueOfInterestTagged.paths))              -- Also add actions to this node itself; it has no argI.
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
          let _ = Utils.log <| "TSEFLLPActions.valToSpecificActions warning: not find ctor " ++ ctorName ++ " in dataTypeDefs: " ++ toString dataTypeDefs in
          Set.empty

    VString _ ->
      -- In practice, should always result in one action.
      Set.map EditText valueOfInterestTagged.paths

    VAppend w1 w2 ->
      let _ = Utils.log "Did not expect a VAppend in TSEFLLPActions.valToSpecificActions" in
      Set.union (recurse maybeType w1) (recurse maybeType w2)

    VNum _ ->
      -- In practice, should always result in one action.
      Set.map Scrub valueOfInterestTagged.paths


-- 1. Find shapes presumably corresponding to prior element, insert location, and following element:
--      a. max path < targetPath and presumably in same container
--      b. path == targtPath
--      c. min path > targetPath and presumably in same container
-- 2. Produce a candidate point for each shape (bot right for before, top left for at and after).
-- 3. Use lowest, rightmost point.
arrangeInsertActions : Dict ProjectionPath Lang.Type -> Dict ProjectionPath (Set TSEFLLPPolys.PixelShape) -> Dict ProjectionPath (Set SpecificAction) -> Dict (Int, Int) (Set SpecificAction)
arrangeInsertActions pathToType projectionPathToShapeSet projectionPathToSpecificActions =
  let
    -- Which paths are not the same type as their parent?
    -- An estimate of where container roots are.
    newTypeRoots =
      pathToType
      |> Dict.keys
      |> List.filter (\path ->
        case List.reverse path of
          [] ->
            True
          _::parentPathReversed ->
            case (Dict.get path pathToType, Dict.get (List.reverse parentPathReversed) pathToType) of
              (Just thisType, Just parentType) ->
                let typeEnv = [] in
                not (Types2.typeEquiv typeEnv thisType parentType)
              _ ->
                False
      )


    insertActions =
      projectionPathToSpecificActions
      |> Dict.values
      |> Utils.unionAll
      |> Set.toList
      |> List.filter (specificActionMaybeChangeType >> (==) (Just Insert))

    -- Sorted by path: shallow to deep, left to right.
    projectionPathShapeSetPairs = Dict.toList projectionPathToShapeSet

    -- South-east most corner on lowest edge.
    shapeSetToMaybeBotRightCorner shapeSet =
      shapeSet
      |> Set.toList
      |> List.map TSEFLLPPolys.shapeToBotRightCorner
      |> Utils.maximumBy (\(x, y) -> (y, x))

    shapeSetToMaybeTopLeftCorner shapeSet =
      shapeSet
      |> Set.toList
      |> List.map TSEFLLPPolys.shapeToTopLeftCorner
      |> Utils.minimumBy (\(x, y) -> (y, x))

    locationActionPairs =
      insertActions
      |> List.map (\action ->
        let
          actionPath = specificActionProjectionPath action

          containerRoot =
            newTypeRoots
            |> Utils.findLast (Utils.isPrefixOf actionPath)
            |> Maybe.withDefault []

          shapeSetJustBeforeInsert =
            projectionPathShapeSetPairs
            |> Utils.findLast (\(path, _) -> path < actionPath && Utils.isPrefix containerRoot path)
            |> Maybe.map Tuple.second
            |> Maybe.withDefault Set.empty

          shapeSetAtInsert =
            projectionPathShapeSetPairs
            |> Utils.findFirst (\(path, _) -> path == actionPath)
            |> Maybe.map Tuple.second
            |> Maybe.withDefault Set.empty

          shapeSetAfterInsert =
            projectionPathShapeSetPairs
            |> Utils.findFirst (\(path, _) -> path > actionPath && Utils.isPrefix containerRoot path)
            |> Maybe.map Tuple.second
            |> Maybe.withDefault Set.empty

          (xVotes, yVotes) =
            [ shapeSetToMaybeBotRightCorner shapeSetJustBeforeInsert
            , shapeSetToMaybeTopLeftCorner  shapeSetAtInsert
            , shapeSetToMaybeTopLeftCorner  shapeSetAfterInsert
            ]
            |> Utils.filterJusts
            |> List.unzip

          -- -- Prefer the insert location directly.
          -- bestPoint =
          --   shapeSetToMaybeTopLeftCorner shapeSetAtInsert
          --   |> Utils.maybeOrElseLazy (\() -> shapeSetToMaybeBotRightCorner shapeSetJustBeforeInsert)
          --   |> Utils.maybeOrElseLazy (\() -> shapeSetToMaybeTopLeftCorner  shapeSetAfterInsert)
          --   |> Maybe.withDefault (0,0)

          -- meanX = List.sum xVotes // (max 1 (List.length xVotes))
          -- meanY = List.sum yVotes // (max 1 (List.length yVotes))

          -- medianX = List.map toFloat xVotes |> Utils.median |> Maybe.map round |> Maybe.withDefault 0
          -- medianY = List.map toFloat yVotes |> Utils.median |> Maybe.map round |> Maybe.withDefault 0

          lowestRight =
            Utils.zip xVotes yVotes
            |> Utils.maximumBy (\(x, y) -> (y, x))
            |> Maybe.withDefault (0,0)
        in
        -- ((meanX, meanY), action)
        -- ((medianX, medianY), action)
        (lowestRight, action)
        -- (bestPoint, action)
      )
  in
  locationActionPairs
  |> Utils.pairsToDictOfLists
  |> Dict.map (\point actions -> Set.fromList actions)
