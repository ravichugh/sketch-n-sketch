-- Convert from ordinary Sketch-n-Sketch language to our core language for tiny structured editors.
module TinyStructuredEditorsForLowLowPricesDesugaring exposing (..)

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


noTag v = TaggedValue v Set.empty

nilTaggedVal            = noTag <| VCtor "Nil" []
consTaggedVal head tail = noTag <| VCtor "Cons" [head, tail]
trueTaggedVal           = noTag <| VCtor "True" []
falseTaggedVal          = noTag <| VCtor "False" []


desugarEnv : Lang.Env -> Env
desugarEnv langEnv =
  let (env, newCache) = desugarEnv_ [] langEnv in
  env


-- Cache desugarings to prevent (super?) exponential blowup when desugaring closure environments.
desugarEnv_ : List (Lang.Val, TaggedValue) -> Lang.Env -> (Env, List (Lang.Val, TaggedValue))
desugarEnv_ cache langEnv =
  let (env, newCache) =
    langEnv
    |> Utils.foldr ([], cache)
        (\(ident, langVal) (env, cache) ->
          let (taggedVal, newCache) = desugarVal_ cache langVal in
          ( (ident, taggedVal)::env
          , newCache
          )
        )
  in
  (env, newCache)


-- Conversions:
-- - True/False to constructors
-- - List literals to Cons/Nil constructors
-- - Multi-arg functions to nesting single-arg functions
-- - Multi-arg applications to binary applications
-- - Let-exps to function application
-- - If-then-else to case split on boolean
--
-- Notably unsupported:
-- - numeric constants
-- - numeric binops
-- - records
-- - non-var argument patterns
-- - recursion
-- - mutual recursion
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
        Just (firstArgName::otherArgNames) ->
          -- Handle otherArgNames by wrapping bodyExp with a bunch of one-arg functions.
          let desugaredBodyExp = otherArgNames |> Utils.foldr (desugarExp bodyExp) EFun in
          EFun firstArgName desugaredBodyExp

        Just [] ->
          EString "TinyStructuredEditorsForLowLowPrices core language does not support zero argument functions"

        Nothing ->
          EString "TinyStructuredEditorsForLowLowPrices core language does not support functions with non-var argument patterns"

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

    Lang.ESelect _ e1 _ _ name                                -> EString "TinyStructuredEditorsForLowLowPrices core language does not yet support record file selection"
    Lang.EApp _ funcExp [] appType _                          -> EString "TinyStructuredEditorsForLowLowPrices core language does not support zero argument applications"
    Lang.EApp _ funcExp (firstArgExp::otherArgExps) appType _ ->
      -- Binarize all applications.
      let firstApp = EApp (desugarExp funcExp) (desugarExp firstArgExp) in
      otherArgExps |> Utils.foldl firstApp (\argExp desugaredFuncExp -> EApp desugaredFuncExp (desugarExp argExp))

    Lang.ELet _ _ (Lang.Declarations _ _ _ letExpGroups) _ bodyExp ->
      -- TODO revist desugaring order
      let
        maybeIdentBoundExpPairs =
          Lang.elemsOf letExpGroups
          |> List.map
              (\(Lang.LetExp _ _ pat _ _ boundExp) ->
                case pat.val.p__ of
                    Lang.PVar _ ident _  -> Just (ident, boundExp)
                    _                    -> Nothing
              )
          |> Utils.projJusts
      in
      case maybeIdentBoundExpPairs of
        Just (identBoundExpPairs) ->
          identBoundExpPairs
          |> Utils.foldr (desugarExp bodyExp) (\(ident, boundExp) desugaredBodyExp -> EApp (EFun ident desugaredBodyExp) (desugarExp boundExp))

        Nothing ->
          EString "TinyStructuredEditorsForLowLowPrices core language does not support multi var let patterns"

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



desugarVal : Lang.Val -> TaggedValue
desugarVal langVal =
  let (taggedVal, newCache) = desugarVal_ [] langVal in
  taggedVal


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

        Lang.VConst offsetProvenance (num, tr) -> ret cache <| noTag <| VNum num
        Lang.VBase (Lang.VBool True)           -> ret cache <| trueTaggedVal
        Lang.VBase (Lang.VBool False)          -> ret cache <| falseTaggedVal
        Lang.VBase (Lang.VString string)       -> ret cache <| vString string
        Lang.VBase Lang.VNull                  -> ret cache <| vString "TinyStructuredEditorsForLowLowPrices core language does not support null"
        Lang.VClosure recNames pats bodyExp funcEnv  ->
          -- Binarize all functions.
          case pats |> List.map LangTools.patToMaybePVarIdent |> Utils.projJusts of
            Just (firstArgName::otherArgNames) ->
              let freeIdentifiersSet = Lang.freeIdentifiers (Lang.eFun pats bodyExp) in
              -- Is the function recursive?
              case recNames |> List.filter (flip Set.member freeIdentifiersSet) of
                [] ->
                  -- Handle otherArgNames by wrapping bodyExp with a bunch of one-arg functions.
                  let
                    desugaredBodyExp         = otherArgNames |> Utils.foldr (desugarExp bodyExp) EFun
                    (desugaredEnv, newCache) = desugarEnv_ cache funcEnv
                  in
                  ret newCache <| noTag <| VClosure desugaredEnv firstArgName desugaredBodyExp

                _ ->
                  ret cache <| vString <| "TinyStructuredEditorsForLowLowPrices core language does not yet support (mutually) recursive functions (recnames: " ++ Utils.toSentence recNames ++ ") (env names: " ++ Utils.toSentence (List.map Tuple.first funcEnv) ++ ")"

            Just [] ->
              ret cache <| vString "TinyStructuredEditorsForLowLowPrices core language does not support zero argument functions"

            Nothing ->
              ret cache <| vString "TinyStructuredEditorsForLowLowPrices core language does not support functions with non-var argument patterns"

        Lang.VFun _ _ _ _ -> ret cache <| vString "TinyStructuredEditorsForLowLowPrices core language does not support VFun"
