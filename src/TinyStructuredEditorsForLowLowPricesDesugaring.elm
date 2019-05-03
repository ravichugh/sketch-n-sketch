-- Convert from ordinary Sketch-n-Sketch language to our core language for tiny structured editors.
module TinyStructuredEditorsForLowLowPricesDesugaring exposing (makeDesugaredToStringProgram, desugarVal)

import Set exposing (Set)

import Javascript
import Lang
import LangTools
-- import Types2
import Utils
import ValUnparser

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)


nilExp            = ECtor "Nil" []
consExp head tail = ECtor "Cons" [head, tail]
trueExp           = ECtor "True" []
falseExp          = ECtor "False" []


nilTaggedVal            = noTag <| VCtor "Nil" []
consTaggedVal head tail = noTag <| VCtor "Cons" [head, tail]
trueTaggedVal           = noTag <| VCtor "True" []
falseTaggedVal          = noTag <| VCtor "False" []


-- Create a desugared program with a main expression that calls
-- the toString function on a variable named "valueOfInterestTagged".
-- The evaluator will insert the valueOfInterestTagged binding into
-- the execution environment.
--
-- def1 = ...
-- def2 = ...
-- def3 = ...
-- renderingFunctionName valueOfInterestTagged
makeDesugaredToStringProgram : Lang.Exp -> Ident -> Exp
makeDesugaredToStringProgram program renderingFunctionName =
  program
  |> LangTools.mapLastTopLevelExp (\_ -> Lang.eCall renderingFunctionName [Lang.eVar "valueOfInterestTagged"])
  |> desugarExp


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
desugarExp : Lang.Exp -> Exp
desugarExp langExp =
  case Lang.unwrapExp langExp of
    Lang.EConst _ num loc wd             -> ENum num
    Lang.EBase _ (Lang.EBool True)       -> trueExp
    Lang.EBase _ (Lang.EBool False)      -> falseExp
    Lang.EBase _ (Lang.EString _ string) -> EString string
    Lang.EBase _ Lang.ENull              -> EString "TinyStructuredEditorsForLowLowPrices core language does not support null"
    Lang.EVar _ name                     -> EVar name
    Lang.EFun _ pats bodyExp _           ->
      -- Binarize all functions.
      case pats |> List.map LangTools.patToMaybePVarIdent |> Utils.projJusts of
        Just argNames -> makeMultiArgFunction argNames (desugarExp bodyExp)
        Nothing       -> EString "TinyStructuredEditorsForLowLowPrices core language does not support functions with non-var argument patterns"

    Lang.EOp _ _ op argExps _ ->
      case (op.val, argExps) of
        (Lang.Plus, [e1, e2]) -> EAppend (desugarExp e1) (desugarExp e2)
        (Lang.Plus, _)        -> EString <| "TinyStructuredEditorsForLowLowPrices core language does not support any non-binary Plus operation"
        (Lang.ToStr, [e1])    -> ENumToString (desugarExp e1)
        _                     -> EString <| "TinyStructuredEditorsForLowLowPrices core language does not support the " ++ toString op.val ++ " operation"

    Lang.EList _ wsHeads _ maybeTail _  ->
      let desugaredTailExp = maybeTail |> Maybe.map desugarExp |> Maybe.withDefault nilExp in
      wsHeads |> Utils.foldr desugaredTailExp (\(_, head) desugaredTailExp -> consExp (desugarExp head) desugaredTailExp)

    Lang.ERecord _ _ decls _ ->
      case Lang.recordEntriesFromDeclarations decls of
        Just entries ->
          case Lang.entriesToMaybeCtorNameAndArgExps entries of
            Just (ctorName, argExps) -> ECtor ctorName (List.map desugarExp argExps)
            Nothing                  -> EString "TinyStructuredEditorsForLowLowPrices could not decipher record"

        Nothing ->
          EString "TinyStructuredEditorsForLowLowPrices core language does not yet support records"

    Lang.ESelect _ e1 _ _ name            -> EString "TinyStructuredEditorsForLowLowPrices core language does not yet support record field selection"
    Lang.EApp _ funcExp argExps appType _ -> makeMultiApp (desugarExp funcExp) (List.map desugarExp argExps) -- Binarize all applications.

    Lang.ELet _ _ (Lang.Declarations _ _ _ letExpGroups) _ bodyExp ->
      desugarLetExpGroups letExpGroups bodyExp

    Lang.EIf _ conditionExp _ thenExp _ elseExp _ ->
      ECase (desugarExp conditionExp)
          [ ("True",  [], desugarExp thenExp) -- Ctor name, ctor argument binding names, branch exp
          , ("False", [], desugarExp elseExp) -- Ctor name, ctor argument binding names, branch exp
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
                    |> Maybe.map (\argNames -> (ctorName, argNames, desugarExp branchExp))

                  Nothing ->
                    Nothing
              )
          |> Utils.projJusts
      in
      case maybeDesugaredBranches of
        Just desugaredBranches -> ECase (desugarExp scrutineeExp) desugaredBranches
        Nothing                -> EString "TinyStructuredEditorsForLowLowPrices core language does not support case patterns other than Ctor x1 x2 x3"

    Lang.EColonType _ innerExp _ _ _ -> desugarExp innerExp
    Lang.EParens _ innerExp _ _      -> desugarExp innerExp
    Lang.EHole _ _                   -> EString "??"


desugarLetExpGroups : List (Bool, List Lang.LetExp) -> Lang.Exp -> Exp
desugarLetExpGroups letExpGroups letBody =
  letExpGroups
  |> Utils.foldr (desugarExp letBody) desugarLetExpGroup


desugarLetExpGroup : (Bool, List Lang.LetExp) -> Exp -> Exp
desugarLetExpGroup ((isRec, letExps) as letExpGroup) desugaredLetBody =
  if isRec then
    desugarRecursiveLetExps letExps desugaredLetBody
  else
    letExps |> Utils.foldr desugaredLetBody desugarLetExp


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
desugarLetExp : Lang.LetExp -> Exp -> Exp
desugarLetExp (Lang.LetExp _ _ pat _ _ boundExp) desugaredLetBody =
  case LangTools.patToMaybePVarIdent (Lang.patEffectivePat pat) of
    Just ident  -> makeLetViaApp ident (desugarExp boundExp) desugaredLetBody
    _           -> EString "TinyStructuredEditorsForLowLowPrices core language does not support multi var let patterns"


-- Following Exercise 9 of https://caml.inria.fr/pub/docs/u3-ocaml/ocaml-ml.html#toc5
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
desugarRecursiveLetExps : List Lang.LetExp -> Exp -> Exp
desugarRecursiveLetExps letExps desugaredLetBody =
  let
    -- Helpers; could be at the top level but used only in this function.

    makeLetRecViaApp recName desugaredBoundExp desugaredLetBody =
      case desugaredBoundExp of
        EFun "" fVarName fBody -> makeLetViaApp recName (EFun recName fVarName fBody) desugaredLetBody
        EFun _  fVarName fBody -> EString "TinyStructuredEditorsForLowLowPrices whyyyyyy is an EFun getting recursivized twicee....."
        _                      -> EString "TinyStructuredEditorsForLowLowPrices recursive bindings only supported for functions"

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
        recIsNeeded = List.range 1 (List.length recNames) -- Indicies.

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
      EString "TinyStructuredEditorsForLowLowPrices desugaring: Each recursive functions must be bound to a single variable pattern!"


desugarVal : Lang.Val -> TaggedValue
desugarVal langVal =
  let (taggedVal, newCache) = desugarVal_ [] langVal in
  taggedVal


-- The cache was for desugaring execution environments efficiently.
-- We don't desugar execution environments anymore, it can probably be removed.
desugarVal_ : List (Lang.Val, TaggedValue) -> Lang.Val -> (TaggedValue, List (Lang.Val, TaggedValue))
desugarVal_ cache langVal =
  case cache |> Utils.findFirst (\(cachedLangVal, cachedTaggedVal) -> Javascript.tripleEqualsOperator langVal cachedLangVal) of
    Just (cachedLangVal, cachedTaggedVal) ->
      (cachedTaggedVal, cache)

    Nothing ->
      let
        vString string         = noTag <| VString string
        ret newCache taggedVal = (taggedVal, (langVal, taggedVal)::newCache)
      in
      case langVal.v_ of
        Lang.VList []       -> ret cache <| nilTaggedVal
        Lang.VList langVals ->
          let (taggedVal, newCache) =
            langVals
            |> Utils.foldr
                (nilTaggedVal, cache)
                (\headLangVal (tailTaggedVal, cache) ->
                  let (headTaggedVal, newCache) = desugarVal_ cache headLangVal in
                  ( consTaggedVal headTaggedVal tailTaggedVal
                  , newCache
                  )
                )
          in
          ret newCache <| taggedVal

        Lang.VDict d   -> ret cache <| vString "TinyStructuredEditorsForLowLowPrices core language does not support dictionaries"
        Lang.VRecord d ->
          case Lang.valToMaybeCtorNameAndArgVals langVal of
            Just (ctorName, argLangVals) ->
              let (argTaggedVals, newCache) =
                argLangVals
                |> Utils.foldr
                    ([], cache)
                    (\argLangVal (argTaggedVals, cache) ->
                      let (argTaggedVal, newCache) = desugarVal_ cache argLangVal in
                      ( argTaggedVal::argTaggedVals
                      , newCache
                      )
                    )
              in
              ret newCache <| noTag <| VCtor ctorName argTaggedVals

            Nothing ->
              ret cache <| vString <| "TinyStructuredEditorsForLowLowPrices core language does not yet support records" ++ ValUnparser.strVal langVal

        Lang.VConst offsetProvenance (num, tr)      -> ret cache <| noTag <| VNum num
        Lang.VBase (Lang.VBool True)                -> ret cache <| trueTaggedVal
        Lang.VBase (Lang.VBool False)               -> ret cache <| falseTaggedVal
        Lang.VBase (Lang.VString string)            -> ret cache <| vString string
        Lang.VBase Lang.VNull                       -> ret cache <| vString "TinyStructuredEditorsForLowLowPrices core language does not support null"
        Lang.VClosure recNames pats bodyExp funcEnv -> ret cache <| vString "TinyStructuredEditorsForLowLowPrices cannot desugar closure values"
        Lang.VFun _ _ _ _                           -> ret cache <| vString "TinyStructuredEditorsForLowLowPrices core language does not support VFun"
