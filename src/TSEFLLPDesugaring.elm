-- Convert from ordinary Sketch-n-Sketch language to our core language for tiny structured editors.
module TSEFLLPDesugaring exposing (makeDesugaredToStringProgram, desugarVal, replaceTBoolTListWithTVarTApp, dataTypeDefsWithoutTBoolsTLists)

import Set exposing (Set)

import ImpureGoodies
import Lang
import LangTools
import Types2
import Utils
import ValUnparser

import TSEFLLPTypes exposing (..)


nilExp            = ECtor "Nil" []
consExp head tail = ECtor "Cons" [head, tail]
trueExp           = ECtor "True" []
falseExp          = ECtor "False" []


nilTaggedVal            = noTag <| VCtor "Nil" []
consTaggedVal head tail = noTag <| VCtor "Cons" [head, tail]
trueTaggedVal           = noTag <| VCtor "True" []
falseTaggedVal          = noTag <| VCtor "False" []

type alias Ref a = { ref : a }
type alias CounterRef = Ref Int

newRef : a -> Ref a
newRef value =
  { ref = value }

getRef : Ref a -> a
getRef ref =
  ref.ref

setRef : Ref a -> a -> ()
setRef ref value =
  let _ = ImpureGoodies.mutateRecordField ref "ref" value in
  ()


-- Create a desugared program with a main expression that calls
-- "toString" on a variable named "valueOfInterestTagged".
-- The evaluator will insert the valueOfInterestTagged binding into
-- the execution environment.
--
-- def1 = ...
-- def2 = ...
-- def3 = ...
-- toString valueOfInterestTagged
makeDesugaredToStringProgram : Lang.Exp -> (MultipleDispatchFunctions, Exp)
makeDesugaredToStringProgram program =
  let
    freshVariableCounterRef      = newRef 1
    multipleDispatchFunctionsRef = newRef []
  in
  let desugaredExp =
    program
    |> LangTools.mapLastTopLevelExp (\_ -> Lang.eCall "toString" [Lang.eVar "valueOfInterestTagged"])
    |> desugarExp freshVariableCounterRef multipleDispatchFunctionsRef
    |> moveCommonPrefixesAndSuffixesOutsideCaseBranches
  in
  -- Debug.log "(MultipleDispatchFunctions, Exp)" <|
  (getRef multipleDispatchFunctionsRef, desugaredExp)


-- Conversions:
-- - True/False to constructors
-- - List literals to Cons/Nil constructors
-- - Multi-arg functions to nesting single-arg functions
-- - Multi-arg applications to binary applications
-- - Let-exps to function application
-- - If-then-else to case split on boolean
--
-- Notably unsupported:
-- - numeric binops
-- - records
-- - non-var argument patterns
--
-- MultipleDispatchFunctions is List (name, type annotation, desugared unique name)
-- See note in TSEFLLPTypes
desugarExp : CounterRef -> Ref MultipleDispatchFunctions -> Lang.Exp -> Exp
desugarExp freshVariableCounterRef multipleDispatchFunctionsRef langExp =
  -- Pass around a partially applied desugar func to helpers. Results in
  -- less noisy code than explicitly passing freshVariableCounterRef
  -- and multipleDispatchFunctionsRef down the stack.
  let partiallyAppliedDesugarExp = desugarExp freshVariableCounterRef multipleDispatchFunctionsRef in
  let recurse = partiallyAppliedDesugarExp in
  case Lang.unwrapExp langExp of
    Lang.EConst _ num loc wd             -> ENum num
    Lang.EBase _ (Lang.EBool True)       -> trueExp
    Lang.EBase _ (Lang.EBool False)      -> falseExp
    Lang.EBase _ (Lang.EString _ string) -> EString string
    Lang.EBase _ Lang.ENull              -> EString "TSEFLLP core language does not support null"
    Lang.EVar _ name                     -> EVar name
    Lang.EFun _ pats bodyExp _           ->
      -- Binarize all functions.
      case pats |> List.map LangTools.patToMaybePVarIdent |> Utils.projJusts of
        Just argNames -> makeMultiArgFunction argNames (recurse bodyExp)
        Nothing       -> EString "TSEFLLP core language does not support functions with non-var argument patterns"

    Lang.EOp _ _ op argExps _ ->
      case (op.val, argExps) of
        (Lang.Plus, [e1, e2])  -> EAppend (recurse e1) (recurse e2) -- Plus is always string append. Use the numPlus function for numeric addition.
        (Lang.Plus, _)         -> EString <| "TSEFLLP core language does not support any non-binary Plus operation"
        (Lang.Minus, [e1, e2]) -> ENumOp Minus (recurse e1) (recurse e2)
        (Lang.Minus, _)        -> EString <| "TSEFLLP core language does not support any non-binary Minus operation"
        (Lang.Eq, [e1, e2])    -> ENumOp Eq (recurse e1) (recurse e2)
        (Lang.Eq, _)           -> EString <| "TSEFLLP core language does not support any non-binary Eq operation"
        (Lang.StrLength, [e1]) -> EStrLen (recurse e1)
        (Lang.ToStr, [e1])     -> ENumToString (recurse e1)
        _                      -> EString <| "TSEFLLP core language does not support the " ++ toString op.val ++ " operation"

    Lang.EList _ wsHeads _ maybeTail _  ->
      let desugaredTailExp = maybeTail |> Maybe.map recurse |> Maybe.withDefault nilExp in
      wsHeads |> Utils.foldr desugaredTailExp (\(_, head) desugaredTailExp -> consExp (recurse head) desugaredTailExp)

    Lang.ERecord _ _ decls _ ->
      case Lang.recordEntriesFromDeclarations decls of
        Just entries ->
          case Lang.entriesToMaybeCtorNameAndArgExps entries of
            Just (ctorName, argExps) -> ECtor ctorName (List.map recurse argExps)
            Nothing                  -> EString "TSEFLLP could not decipher record"

        Nothing ->
          EString "TSEFLLP core language does not yet support records"

    Lang.ESelect _ e1 _ _ name            -> EString "TSEFLLP core language does not yet support record field selection"
    Lang.EApp _ funcExp argExps appType _ -> makeMultiApp (recurse funcExp) (List.map recurse argExps) -- Binarize all applications.

    Lang.ELet _ _ declarations _ bodyExp ->
      let
        (Lang.Declarations _ _ _ letExpGroups) =
          gatherAndRenameDynamicFunctions freshVariableCounterRef multipleDispatchFunctionsRef declarations
      in
      desugarLetExpGroups partiallyAppliedDesugarExp letExpGroups bodyExp

    Lang.EIf _ conditionExp _ thenExp _ elseExp _ ->
      ECase (recurse conditionExp)
          [ ("True",  [], recurse thenExp) -- Ctor name, ctor argument binding names, branch exp
          , ("False", [], recurse elseExp) -- Ctor name, ctor argument binding names, branch exp
          ]

    Lang.ECase _ scrutineeExp branches _ ->
      let
        maybeDesugaredBranches =
          Lang.branchPatExps branches
          |> List.map
              (\(pat, branchExp) ->
                case Lang.patToMaybeCtorNameAndArgPats pat of
                  Just (ctorName, argPats) ->
                    argPats
                    |> List.map LangTools.patToMaybePVarIdent
                    |> Utils.projJusts
                    |> Maybe.map (\argNames -> (ctorName, argNames, recurse branchExp))

                  Nothing ->
                    case Lang.unwrapPat pat of
                      Lang.PBase _ (Lang.EBool True)  -> Just ("True",  [], recurse branchExp)
                      Lang.PBase _ (Lang.EBool False) -> Just ("False", [], recurse branchExp)
                      _                               -> Nothing
              )
          |> Utils.projJusts
      in
      case maybeDesugaredBranches of
        Just desugaredBranches -> ECase (recurse scrutineeExp) desugaredBranches
        Nothing                -> EString "TSEFLLP core language does not support case patterns other than Ctor x1 x2 x3 or True or False"

    Lang.EColonType _ innerExp _ _ _ -> recurse innerExp
    Lang.EParens _ innerExp _ _      -> recurse innerExp
    Lang.EHole _ _                   -> EString "??"


getFreshVariable : CounterRef -> String -> String
getFreshVariable freshVariableCounterRef baseName =
  let i = getRef freshVariableCounterRef in
  let _ = setRef freshVariableCounterRef (i + 1) in
  baseName ++ toString i

addMultipleDispatchFunction : Ref MultipleDispatchFunctions -> Ident -> Lang.Type -> Ident -> ()
addMultipleDispatchFunction multipleDispatchFunctionsRef originalName tipe uniqueName =
  getRef multipleDispatchFunctionsRef
  |> (::) (originalName, replaceTBoolTListWithTVarTApp tipe, uniqueName)
  |> setRef multipleDispatchFunctionsRef


-- See TSEFLLPTypes for explanation of how we handle dynamic functions
--
-- Goal here is to:
-- 1. Assign unique names to each dynamic function implementation
-- 2. Catalog the type annotations of each dynamic function so we can dispatch
--    an implementation based on the argument type.
--
-- Mutates CounterRef to generate fresh variable names.
-- Mutates multipleDispatchFunctionsRef.
gatherAndRenameDynamicFunctions : CounterRef -> Ref MultipleDispatchFunctions -> Lang.Declarations -> Lang.Declarations
gatherAndRenameDynamicFunctions freshVariableCounterRef multipleDispatchFunctionsRef declarations =
  let
    (declarationList, rebuildDeclarations) =
      Lang.getDeclarationsExtractors declarations

    (Lang.Declarations printOrder _ _ _) = declarations

    (newDeclarationListInPrintOrder, _) =
      declarationList
      |> Utils.reorder printOrder
      |> Utils.foldl ([], Nothing) processDeclaration

    newDeclarationListInInternalOrder =
      newDeclarationListInPrintOrder
      |> Utils.zip printOrder
      |> List.sortBy Tuple.first
      |> List.map Tuple.second

    newDeclarations =
      rebuildDeclarations newDeclarationListInInternalOrder

    processDeclaration declaration (newDeclarationListInPrintOrder, maybePriorTypeAnnotation) =
      let noChange = (newDeclarationListInPrintOrder ++ [declaration], Nothing) in
      case declaration of
        Lang.DeclExp (Lang.LetExp maybeCommaSpace ws1 pat funArgStyle ws2 boundExp) ->
          case Lang.identifierFromPat pat of
            Just name ->
              if List.member name Lang.dynamicDispatchIdentifiers then
                let
                  uniqueName     = getFreshVariable freshVariableCounterRef (name ++ " dynamic")
                  newPat         = Lang.pVar uniqueName
                  newDeclaration = Lang.DeclExp (Lang.LetExp maybeCommaSpace ws1 newPat funArgStyle ws2 boundExp)
                  _ =
                    case maybePriorTypeAnnotation of
                      Just (Lang.LetAnnotation _ _ typePat _ _ tipe) ->
                        if Lang.identifierFromPat typePat == Just name
                        then addMultipleDispatchFunction multipleDispatchFunctionsRef name tipe uniqueName
                        else ()
                      Nothing -> ()
                in
                (newDeclarationListInPrintOrder ++ [newDeclaration], Nothing)
              else
                noChange

            Nothing ->
              noChange

        Lang.DeclType _ ->
          noChange

        Lang.DeclAnnotation letAnnotation ->
          (newDeclarationListInPrintOrder ++ [declaration], Just letAnnotation)

  in
  newDeclarations


-- Pass desugarExp down the stack; cleaner than explicitly passing the references around.
desugarLetExpGroups : (Lang.Exp -> Exp) -> List (Bool, List Lang.LetExp) -> Lang.Exp -> Exp
desugarLetExpGroups desugarExp letExpGroups letBody =
  letExpGroups
  |> Utils.foldr (desugarExp letBody) (desugarLetExpGroup desugarExp)


desugarLetExpGroup : (Lang.Exp -> Exp) -> (Bool, List Lang.LetExp) -> Exp -> Exp
desugarLetExpGroup desugarExp ((isRec, letExps) as letExpGroup) desugaredLetBody =
  if isRec then
    desugarRecursiveLetExps desugarExp letExps desugaredLetBody
  else
    letExps |> Utils.foldr desugaredLetBody (desugarLetExp desugarExp)


makeMultiArgFunction : List Ident -> Exp -> Exp
makeMultiArgFunction argNames desugaredBodyExp =
  argNames |> Utils.foldr desugaredBodyExp (EFun "")


makeLetViaApp : Ident -> Exp -> Exp -> Exp
makeLetViaApp varName desugaredBoundExp desugaredLetBody =
  EApp (EFun "" varName desugaredLetBody) desugaredBoundExp


-- Binarize all applications.
makeMultiApp : Exp -> List Exp -> Exp
makeMultiApp desugaredFuncExp desugaredArgExps =
  desugaredArgExps
  |> Utils.foldl desugaredFuncExp (\desugaredArgExp desugaredFuncExp -> EApp desugaredFuncExp desugaredArgExp)
  -- let firstApp = EApp (desugarExp funcExp) (desugarExp firstArgExp) in
  -- otherArgExps |> Utils.foldl firstApp (\argExp desugaredFuncExp -> EApp desugaredFuncExp (desugarExp argExp))


-- Let desugared to function application.
desugarLetExp : (Lang.Exp -> Exp) -> Lang.LetExp -> Exp -> Exp
desugarLetExp desugarExp (Lang.LetExp _ _ pat _ _ boundExp) desugaredLetBody =
  case LangTools.patToMaybePVarIdent (Lang.patEffectivePat pat) of
    Just ident  -> makeLetViaApp ident (desugarExp boundExp) desugaredLetBody
    _           -> EString <| "TSEFLLP core language does not support multi var let patterns" ++ toString (Lang.patEffectivePat pat)


-- Following Exercise 9 of https://caml.inria.fr/pub/docs/u3-ocaml/ocaml-ml.html#Exo-9
--
-- Given:
--
-- let rec f1 = λx. a1
--     and f2 = λx. a2
--     and f3 = λx. a3
-- in
-- a
--
-- Desugars to:
-- let rec f1' = λf2. λf3. λx. let f1 = f1' f2 f3 in
--                             a1
-- in
-- let rec f2' =      λf3. λx. let f2 = f2' f3    in
--                             let f1 = f1' f2 f3 in
--                             a2
-- in
-- let rec f3' =           λx. let f3 = f3'       in
--                             let f2 = f2' f3    in
--                             let f1 = f1' f2 f3 in
--                             a3
-- in
-- -- The given answer to Exercise 9 forgot the following lets:
-- let f3 = f3'       in
-- let f2 = f2' f3    in
-- let f1 = f1' f2 f3 in
-- a
desugarRecursiveLetExps : (Lang.Exp -> Exp) -> List Lang.LetExp -> Exp -> Exp
desugarRecursiveLetExps desugarExp letExps desugaredLetBody =
  let
    -- Helpers; could be at the top level but used only in this function.

    makeLetRecViaApp recName desugaredBoundExp desugaredLetBody =
      case desugaredBoundExp of
        EFun "" fVarName fBody -> makeLetViaApp recName (EFun recName fVarName fBody) desugaredLetBody
        EFun _  fVarName fBody -> EString "TSEFLLP whyyyyyy is an EFun getting recursivized twicee....."
        _                      -> EString "TSEFLLP recursive bindings only supported for functions"

    -- Given:   λx. λy. λz. e
    -- Returns: λx. λy. λz. e'
    mapDeepestEFunBody f exp =
      let recurse = mapDeepestEFunBody f in
      case exp of
        EFun fRecName fVarName fBody -> EFun fRecName fVarName (recurse fBody)
        _                            -> f exp

  in
  let
    maybeRecNames =
      letExps
      |> List.map (Lang.patOfLetExp >> Lang.patEffectivePat >> LangTools.patToMaybePVarIdent)
      |> Utils.projJusts
  in
  case maybeRecNames of
    Just recNames ->
      let
        -- Recusive indicies needed.
        recIsNeeded = List.range 1 (List.length recNames)

        -- Names for f1' f2' f3' above.
        intermediateName name = name ++ "SinglyRecursive"

        -- let f3 = f3'       in
        -- let f2 = f2' f3    in
        -- let f1 = f1' f2 f3 in
        -- a
        applyToRemakeRecFuncs recIsNeeded scopeBodyExp =
          case List.maximum recIsNeeded of
            Just recI ->
              -- Wrap once and recurse inward.
              let
                fName             = Utils.geti recI recNames
                fIntermediateName = intermediateName fName
                argNames          = List.drop recI recNames
                remade            = makeMultiApp (EVar fIntermediateName) (List.map EVar argNames)
              in
              makeLetViaApp fName remade <|
                applyToRemakeRecFuncs (Utils.removeAsSet recI recIsNeeded) scopeBodyExp

            Nothing ->
              scopeBodyExp

        desugaredRawLetBindings = letExps |> List.map (Lang.bindingOfLetExp >> desugarExp)

        -- let rec f1' = λf2. λf3. λx. let f1 = f1' f2 f3 in
        --                             a1
        -- in
        -- let rec f2' =      λf3. λx. let f2 = f2' f3    in
        --                             let f1 = f1' f2 f3 in
        --                             a2
        -- in
        -- let rec f3' =           λx. let f3 = f3'       in
        --                             let f2 = f2' f3    in
        --                             let f1 = f1' f2 f3 in
        --                             a3
        -- in
        wrapWithLetRecs recIsNeeded scopeBodyExp =
          case List.minimum recIsNeeded of
            Just recI ->
              -- Wrap once and recurse inward.
              let
                fName                  = Utils.geti recI recNames
                fIntermediateName      = intermediateName fName
                argNames               = List.drop recI recNames
                desugaredBinding       = Utils.geti recI desugaredRawLetBindings
                bodyWithRemadeRecFuncs = desugaredBinding |> mapDeepestEFunBody (applyToRemakeRecFuncs (List.range 1 recI)) -- let f1 = f1' f2 f3 in funcBody
                wrappedWithRecArgs     = makeMultiArgFunction argNames bodyWithRemadeRecFuncs -- λf2. λf3. bodyWithRemadeRecFuncs
              in
              makeLetRecViaApp fIntermediateName wrappedWithRecArgs <|
                wrapWithLetRecs (Utils.removeAsSet recI recIsNeeded) scopeBodyExp

            Nothing ->
              scopeBodyExp

        -- This part:
        -- let f3 = f3'       in
        -- let f2 = f2' f3    in
        -- let f1 = f1' f2 f3 in
        -- a
        mainScopeExp = applyToRemakeRecFuncs recIsNeeded desugaredLetBody
      in
      mainScopeExp
      |> wrapWithLetRecs recIsNeeded

    Nothing ->
      EString "TSEFLLP desugaring: Each recursive functions must be bound to a single variable pattern!"


desugarVal : Lang.Val -> TaggedValue
desugarVal langVal =
  let
    recurse        = desugarVal
    vString string = noTag <| VString string
  in
  case langVal.v_ of
    Lang.VList []       -> nilTaggedVal
    Lang.VList langVals -> Utils.foldr nilTaggedVal consTaggedVal (List.map recurse langVals)
    Lang.VDict d        -> vString "TSEFLLP core language does not support dictionaries"
    Lang.VRecord d      ->
      case Lang.valToMaybeCtorNameAndArgVals langVal of
        Just (ctorName, argLangVals) -> noTag <| VCtor ctorName (List.map recurse argLangVals)
        Nothing                      -> vString <| "TSEFLLP core language does not yet support records " ++ ValUnparser.strVal langVal

    Lang.VConst offsetProvenance (num, tr)      -> noTag <| VNum num
    Lang.VBase (Lang.VBool True)                -> trueTaggedVal
    Lang.VBase (Lang.VBool False)               -> falseTaggedVal
    Lang.VBase (Lang.VString string)            -> vString string
    Lang.VBase Lang.VNull                       -> vString "TSEFLLP core language does not support null"
    Lang.VClosure recNames pats bodyExp funcEnv -> vString "TSEFLLP cannot desugar closure values"
    Lang.VFun _ _ _ _                           -> vString "TSEFLLP core language does not support VFun"



--------- Normalization ---------

-- Change:
--
-- case tail of
--   Nil      -> toString head
--   Cons _ _ -> toString head + "," + elemsToString tail
--
-- To:
--
-- toString head + case tail of
--   Nil      -> ""
--   Cons _ _ -> "," + elemsToString tail
--
-- Inuitively, parts that are shared among all branches
-- should not be dependent on the scrutinee. I don't
-- believe it is in general possible to precisely
-- identify all such sharing, but syntactically extracting
-- common prefixes/suffixes will catch the above example
-- and a few others.


-- Var names only; not Ctor names.
freeNameSet : Exp -> Set Ident
freeNameSet exp =
  let recurse = freeNameSet in
  case exp of
    EFun funcName argName funcBody -> recurse funcBody |> Utils.removeAllFromSet [funcName, argName]
    EVar varName                   -> Set.singleton varName
    EApp funcExp argExp            -> Set.union (recurse funcExp) (recurse argExp)
    ECtor ctorName argExps         -> argExps |> List.map recurse |> Utils.unionAll
    EString string                 -> Set.empty
    EAppend e1 e2                  -> Set.union (recurse e1) (recurse e2)
    EStrLen argExp                 -> recurse argExp
    ENum num                       -> Set.empty
    ENumToString argExp            -> recurse argExp
    ENumOp op e1 e2                -> Set.union (recurse e1) (recurse e2)
    EAddDependency e1 e2           -> Set.union (recurse e1) (recurse e2)
    ECase scrutinee branches       ->
      let branchNameSets =
        branches
        |> List.map (\(branchCtorName, argNames, branchExp) ->
          recurse branchExp |> Utils.removeAllFromSet argNames
        )
      in
      Utils.unionAll <| recurse scrutinee :: branchNameSets


-- Turns:
--
-- "{\n" + nextIndent + join "," strs + "\n}"
--
-- Into:
--
-- ["{", "\n", nextIndent, join "," strs, "\n", "}"]
flattenAppends : Exp -> List Exp
flattenAppends exp =
  let recurse = flattenAppends in
  case exp of
    EFun funcName argName funcBody -> [exp]
    EVar varName                   -> [exp]
    EApp funcExp argExp            -> [exp]
    ECtor ctorName argExps         -> [exp]
    ECase scrutinee branches       -> [exp]
    EString string                 -> string |> String.split "" |> List.map EString
    EAppend e1 e2                  -> recurse e1 ++ recurse e2
    EStrLen argExp                 -> [exp]
    ENum num                       -> [exp]
    ENumToString argExp            -> [exp]
    ENumOp op e1 e2                -> [exp]
    EAddDependency e1 e2           -> [exp]


-- Undo flattenAppends
rebuildAppends : List Exp -> Exp
rebuildAppends exps =
  let recurse = rebuildAppends in
  case exps of
    EString ""                   :: rest -> recurse rest -- Remove "" if possible.
    [exp]                                -> exp -- Don't add empty string at end if exps is non-empty.
    EString str1 :: EString str2 :: rest -> recurse (EString (str1 ++ str2) :: rest)
    exp                          :: rest -> EAppend exp (recurse rest)
    []                                   -> EString ""

-- Rudimentary for now. Let-exps in a case branch will confound the algoritm.
--
-- Possible improvements:
--   - Move let-exps that don't need to be inside a case branch outside the case.
--   - Move free prefixes / suffixes in a let-body outside the let-exp.
moveCommonPrefixesAndSuffixesOutsideCaseBranches : Exp -> Exp
moveCommonPrefixesAndSuffixesOutsideCaseBranches exp =
  let recurse = moveCommonPrefixesAndSuffixesOutsideCaseBranches in
  case exp of
    EFun funcName argName funcBody -> EFun funcName argName (recurse funcBody)
    EVar varName                   -> exp
    EApp funcExp argExp            -> EApp (recurse funcExp) (recurse argExp)
    ECtor ctorName argExps         -> ECtor ctorName (List.map recurse argExps)
    EString string                 -> exp
    EAppend e1 e2                  -> EAppend (recurse e1) (recurse e2)
    EStrLen argExp                 -> EStrLen (recurse argExp)
    ENum num                       -> exp
    ENumToString argExp            -> ENumToString (recurse argExp)
    ENumOp op e1 e2                -> ENumOp op (recurse e1) (recurse e2)
    EAddDependency e1 e2           -> EAddDependency (recurse e1) (recurse e2)
    ECase scrutinee branches       ->
      let
        (branchCtorNames, argNameLists, branchExps) = Utils.unzip3 branches

        branchExpsNormalizedSeparately = List.map recurse branchExps

        branchExpsFlattened = List.map flattenAppends branchExpsNormalizedSeparately

        caseHasStringAppends =
          branchExpsFlattened
          |> List.any (\appendedExps -> List.length appendedExps >= 2)

        -- Slightly conservative by smashing these together instead of inspecting by branch.
        branchBoundNameSet =
          argNameLists
          |> List.map Set.fromList
          |> Utils.unionAll

        commonPrefixParts =
          branchExpsFlattened
          |> Utils.commonPrefix
          -- Make sure prefix doesn't include any part dependent on branch-bound name.
          |> Utils.takeWhile (freeNameSet >> Set.intersect branchBoundNameSet >> Set.size >> (==) 0)

        branchExpsFlattenedNoPrefix =
          branchExpsFlattened
          |> List.map (List.drop (List.length commonPrefixParts))

        commonSuffixParts =
          branchExpsFlattenedNoPrefix
          |> List.map List.reverse
          |> Utils.commonPrefix
          -- Make sure suffix doesn't include any part dependent on branch-bound name.
          |> Utils.takeWhile (freeNameSet >> Set.intersect branchBoundNameSet >> Set.size >> (==) 0)
          |> List.reverse

        branchExpsFlattenedNoPrefixNoSuffix =
          branchExpsFlattenedNoPrefix
          |> List.map (Utils.dropLast (List.length commonSuffixParts))

        branchExpsRebuild =
          branchExpsFlattenedNoPrefixNoSuffix
          |> List.map rebuildAppends

        prefixSuffixPulledOutsideCase =
          rebuildAppends <|
            commonPrefixParts ++
            [ECase scrutinee (List.map3 (,,) branchCtorNames argNameLists branchExpsRebuild)] ++
            commonSuffixParts
      in
      if caseHasStringAppends
      then prefixSuffixPulledOutsideCase
      else ECase scrutinee (List.map3 (,,) branchCtorNames argNameLists branchExpsNormalizedSeparately)




------------- Types -------------

-- We use (a small subset of) Lang.Type instead of defining our own Type grammar.
-- Make sure types coming from Leo are run through replaceTBoolTListWithTVarTApp.

tAppList : Lang.Type -> Lang.Type
tAppList elemType = Lang.tApp0 (Lang.tVar0 "List") [elemType] Lang.SpaceApp

builtinDataTypes : List Types2.DataTypeDef
builtinDataTypes =
  let a = Lang.tVar0 "a" in
  [ ("Bool",  ([],    [("True", []),    ("False", [])]))
  , ("Maybe", (["a"], [("Nothing", []), ("Just", [a])]))             -- The Leo parser does not allow `type Maybe a = ...`; otherwise we would just put this in our examples.
  , ("List",  (["a"], [("Nil", []),     ("Cons", [a, tAppList a])])) -- The Leo parser does not allow `type List a = ...`; otherwise we would just put this in our examples.
  ]

replaceTBoolTListWithTVarTApp : Lang.Type -> Lang.Type
replaceTBoolTListWithTVarTApp tipe =
  tipe |> Lang.mapType (\t ->
    case Lang.unwrapType t of
      Lang.TBool _            -> Lang.tVar0 "Bool"
      Lang.TList _ elemType _ -> tAppList elemType
      _                       -> t
  )

-- Copied from Types2, for reference:
--
-- type alias DataConDef = (Ident, List Type)
-- type alias DataTypeDef = (Ident, (List Ident, List DataConDef)) -- (Type name, (type arg names, constructors))
-- type alias DataTypeEnv = List DataTypeDef

-- Use a data type for booleans instead of primitive Lang.TBool.
-- Also, convert TList's to TApp's of the List type constructor.
dataTypeDefsWithoutTBoolsTLists : Lang.Exp -> List Types2.DataTypeDef
dataTypeDefsWithoutTBoolsTLists langExp =
  let
    replaceTBoolTListWithTVarTAppInDataConDef (ctorName, argTypes) =
      ( ctorName
      , argTypes |> List.map replaceTBoolTListWithTVarTApp
      )
  in
  Types2.getDataTypeDefs langExp
  |> List.map (\(dataTypeName, (typeArgNames, dataConDefs)) -> (dataTypeName, (typeArgNames, List.map replaceTBoolTListWithTVarTAppInDataConDef dataConDefs)))
  |> (++) builtinDataTypes
