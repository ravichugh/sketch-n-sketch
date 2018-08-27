module Types2 exposing
  ( typecheck
  , makeDeuceTool
  , AceTypeInfo
  , aceTypeInfo
  , dummyAceTypeInfo
  )

import Info exposing (withDummyInfo)
import Lang exposing (..)
import LangTools
import LangUtils
import ElmUnparser exposing (unparse, unparseType)
import Ace
-- can't depend on Model, since ExamplesGenerated depends on Types2
import Utils

import EditDistance

unparseMaybeType mt =
  case mt of
    Nothing -> "NO TYPE"
    Just t  -> unparseType t


--------------------------------------------------------------------------------

type alias AceTypeInfo =
  { annotations : List Ace.Annotation
  , highlights : List Ace.Highlight
  , tooltips : List Ace.Tooltip
  }

dummyAceTypeInfo =
  AceTypeInfo [] [] []

aceTypeInfo : Exp -> AceTypeInfo
aceTypeInfo exp =
  { highlights =
      []

  , annotations =
      let
        addErrorAnnotation e =
          -- TODO for now, ignoring top-level prog and implicit main ---------------------------
          case ((unExpr e).start.line, (unExpr e).val.e__) of
            (1, _) -> []
            (_, EVar _ "main") -> []
            (_, ELet _ Let (Declarations [0] [] [] [(False, [LetExp _ _ p _ _ _])]) _ _) -> []
            _ ->
          --------------------------------------------------------------------------------------
          case ((unExpr e).val.typ, (unExpr e).val.typeError) of
            (Just _, Nothing) ->
              []

            _ ->
              [ { row =
                    (unExpr e).start.line - 1
                , type_ =
                    "error"
                , text =
                    String.concat
                      [ "Type error ["
                      , "eid: ", toString (unExpr e).val.eid, "; "
                      , "col: ", toString (unExpr e).start.col
                      , "]: ", "\n"
                      , String.trim (unparse e), "\n"
                      ]
                }
              ]

        errorAnnotations =
          foldExp (\e acc -> addErrorAnnotation e ++ acc) [] exp

        summaryAnnotation =
          case errorAnnotations of
            [] ->
              { row = 0, type_ = "info", text= "No type errors!" }

            _ ->
              { row = 0, type_ = "warning", text="Type errors below..." }
      in
        summaryAnnotation :: errorAnnotations

  , tooltips =
      let addTooltip e =
        { row = (unExpr e).start.line - 1
        , col = (unExpr e).start.col - 1
        , text = "EId: " ++ toString (unExpr e).val.eid
        } 
      in
      -- Ace tooltips are token-based, so can't have them for expression
      -- forms that don't have an explicit start token
      --
      -- foldExp (\e acc -> addTooltip e :: acc) [] exp
      []
  }


--------------------------------------------------------------------------------

type alias TypeEnv = List TypeEnvElement

type TypeEnvElement
  = HasType Pat (Maybe Type)
  | TypeVar Ident
  -- | TypeAlias Pat Type


lookupVar : TypeEnv -> Ident -> Maybe (Maybe Type)
lookupVar gamma x =
  case gamma of
    HasType p mt :: gammaRest ->
      Utils.firstOrLazySecond
        (lookupVarInPat gamma x p mt)
        (\_ -> lookupVar gammaRest x)

    _ :: gammaRest ->
      lookupVar gammaRest x

    [] ->
      Nothing


lookupVarInPat : TypeEnv -> Ident -> Pat -> Maybe Type -> Maybe (Maybe Type)
lookupVarInPat gamma x p mt =
  case p.val.p__ of
    PConst _ _ -> Nothing
    PBase _ _ -> Nothing
    PWildcard _ -> Nothing

    PVar _ y _ ->
      if x == y then
        Just mt
      else
        Nothing

{-
  | PList WS (List Pat) WS (Maybe Pat) WS -- TODO store WS before commas, like EList
  | PAs WS Pat WS Pat
  | PParens WS Pat WS
  | PRecord WS {- { -}  (List (Maybe WS {- , -}, WS, Ident, WS{-=-}, Pat)) WS{- } -}
  | PColonType WS Pat WS Type
-}

    _ ->
      Nothing


varsOfGamma gamma =
  case gamma of
    HasType p mt :: gammaRest ->
      varsOfPat p ++ varsOfGamma gammaRest

    _ :: gammaRest ->
      varsOfGamma gammaRest

    [] ->
      []


varsOfPat pat =
  Tuple.second <|
    mapFoldPatTopDown
        (\p acc ->
          case p.val.p__ of
            PVar _ y _ -> (p, y :: acc)
            _          -> (p, acc)
        )
        []
        pat


varNotFoundSuggestions x gamma =
  let
    result =
      List.concatMap maybeSuggestion (varsOfGamma gamma)

    maybeSuggestion y =
      let
        xLength =
          String.length x
        xSorted =
          List.sort (String.toList x)
        ySorted =
          List.sort (String.toList y)
        distance =
          EditDistance.levenshtein xSorted ySorted
            -- lowerBound: abs (xLength - yLength)
            -- upperBound: max xLength yLength
        closeEnough =
          if xLength <= 3 && distance <= xLength - 1 then
            True
          else if distance <= 3 then
            True
          else
            False
      in
        if closeEnough then
          [y]
        else
          []
  in
    result


--------------------------------------------------------------------------------

typeEquiv t1 t2 =
  -- LangUtils.typeEqual is ws-sensitive.
  -- will need to do alpha-renaming too.
  -- TODO: this is temporary
  String.trim (unparseType t1) == String.trim (unparseType t2)


--------------------------------------------------------------------------------

typecheck : Exp -> Exp
typecheck e =
  let result = inferType [] {} e in
  result.newExp


inferType
    : TypeEnv
   -> {}
   -> Exp
   -> { newExp: Exp }
        -- the inferred Maybe Type is in newExp.val.typ

inferType gamma stuff thisExp =
  let (childExps, rebuildExp) = childExpsExtractors thisExp in
  case (unExpr thisExp).val.e__ of
    EConst _ _ _ _ ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TNum space1))) }

    EBase _ (EBool _) ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TBool space1))) }

    EBase _ (EString _ _) ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TString space1))) }

    EVar ws x ->
      let
        suggestions =
          List.map
            (\y -> (y, EVar ws y |> replaceE__ thisExp))
            (varNotFoundSuggestions x gamma)
      in
      { newExp = thisExp |> setTypeError (VarNotFound x suggestions) }

    EParens ws1 innerExp parensStyle ws2 ->
      let
        result =
          inferType gamma stuff innerExp

        newExp =
          EParens ws1 result.newExp parensStyle ws2
            |> replaceE__ thisExp
            |> setType (unExpr result.newExp).val.typ
      in
        { newExp = newExp }

    EColonType ws1 innerExp ws2 annotatedType ws3 ->
      let
        result =
          checkType gamma stuff thisExp innerExp annotatedType

        (newInnerExp, finishNewExp) =
          if result.okay then
            (result.newExp, Basics.identity)

          else
            -- the call to checkType calls:
            -- setTypeError (ExpectedButGot annotatedType result.newExp.val.typ)
            --
            -- here, adding extra breadcrumb about the solicitorExp.
            --
            (result.newExp, setExtraTypeInfo (ExpectedExpToHaveSomeType (unExpr innerExp).val.eid))

        newExp =
          EColonType ws1 newInnerExp ws2 annotatedType ws3
            |> replaceE__ thisExp
            |> setType (Just annotatedType)
            |> finishNewExp
      in
        { newExp = newExp }

    EFun ws1 pats body ws2 ->
      let
        newGamma =
          -- TODO: just putting vars in env for now
          List.map (\pat -> HasType pat Nothing) pats ++ gamma
        result =
          inferType newGamma stuff body
        newExp =
          EFun ws1 pats result.newExp ws2
            |> replaceE__ thisExp
      in
      { newExp = newExp }

    ELet ws1 letKind (Declarations po letTypes letAnnots letExps) ws2 body ->
      -- TODO: to get started, just processing individual equations,
      --       and not adding them to environment for body
      let
        resultEquations =
          inferTypes gamma stuff (Utils.dropLast 1 childExps)

        resultBody =
          inferType gamma stuff body

        newChildExps =
          Utils.snoc resultEquations.newExps resultBody.newExp

        newExp =
          rebuildExp newChildExps
      in
        { newExp = newExp |> setType Nothing }

    ERecord ws1 maybeExpWs (Declarations po letTypes letAnnots letExps) ws2 ->
      let
        eRecordError s =
          { newExp =
              thisExp
                |> setTypeError (OtherTypeError ["not supported in records: " ++ s])
          }
      in
      case (maybeExpWs, letTypes, letAnnots, letExps) of
        (Just _, _, _, _) ->
          eRecordError "base expression"

        (Nothing, _::_, _, _) ->
          eRecordError "type definitions"

        (Nothing, _, _::_, _) ->
          eRecordError "type annotations"

        (Nothing, [], [], letExps) ->
          let
            maybeListLetExp =
              List.map (\(isRec, listLetExps) ->
                         case (isRec, listLetExps) of
                           (False, [letExp]) -> Just letExp
                           _                 -> Nothing
                       ) letExps
            rebuildLetExps =
              List.map (\newLetExp -> (False, [newLetExp]))
          in
          case Utils.projJusts maybeListLetExp of
            Nothing ->
              eRecordError "wasn't expecting these letExps..."

            Just listLetExp ->
              let
                (listLetExpMinusCtor, finishLetExpsAndFieldTypes) =
                  let
                    default =
                      ( listLetExp
                      , \(newListLetExp, maybeFieldTypes) -> (newListLetExp, maybeFieldTypes)
                      )
                  in
                  case listLetExp of
                    [] ->
                      default

                    firstLetExp :: restListLetExp ->
                      let (LetExp mbWs1 ws2 p funArgStyle ws3 e) = firstLetExp in
                      case (p.val.p__, (unExpr e).val.e__) of
                        (PVar _ pname _, EBase _ (EString _ ename)) ->
                          if String.startsWith "Tuple" ename then
                            ( restListLetExp
                            , \(newRestListLetExp, fieldMaybeTypes) ->
                                ( firstLetExp
                                    :: newRestListLetExp
                                , Just (Lang.ctor (withDummyRange << TVar space0) TupleCtor ename)
                                    :: fieldMaybeTypes
                                )
                            )

                          else
                            default

                        _ ->
                          default

                (newListLetExp, maybeFieldTypes) =

                  List.foldl
                    (\(LetExp mbWs1 ws2 p funArgStyle ws3 e) (acc1,acc2) ->
                      let
                        result =
                          inferType gamma stuff e
                        newLetExp =
                          LetExp mbWs1 ws2 p funArgStyle ws3 result.newExp
                        maybeFieldType =
                          case p.val.p__ of
                            PVar _ fieldName _ ->
                              (unExpr result.newExp).val.typ
                                |> Maybe.map (\t -> (Nothing, space1, fieldName, space1, t))
                            _ ->
                              Nothing -- TODO: report error around non-var field
                      in
                        ( newLetExp :: acc1 , maybeFieldType :: acc2 )
                    )
                    ([], [])
                    listLetExpMinusCtor

                  |> (\(list1, list2) -> (List.reverse list1, List.reverse list2))

                  |> finishLetExpsAndFieldTypes

                newLetExps =
                  rebuildLetExps newListLetExp

                newExp =
                  case Utils.projJusts maybeFieldTypes of
                    Just fieldTypes ->
                      ERecord ws1 maybeExpWs (Declarations po letTypes letAnnots newLetExps) ws2
                        |> replaceE__ thisExp
                        |> setType (Just (withDummyInfo (TRecord space0 Nothing fieldTypes space1)))

                    Nothing ->
                      let
                        fieldError =
                          (Nothing, space1, "XXX", space1, withDummyInfo (TVar space1 "XXX"))
                        fieldTypesWithXXXs =
                          List.map (Maybe.withDefault fieldError) maybeFieldTypes
                        recordTypeWithXXXs =
                          withDummyInfo (TRecord space0 Nothing fieldTypesWithXXXs space1)
                        error =
                          OtherTypeError
                            [ "Some fields are okay, but others are not: "
                            , unparseType recordTypeWithXXXs
                            ]
                      in
                        ERecord ws1 maybeExpWs (Declarations po letTypes letAnnots newLetExps) ws2
                          |> replaceE__ thisExp
                          |> setTypeError error
              in
                { newExp = newExp }

    _ ->
      { newExp = thisExp |> setType Nothing }


inferTypes
    : TypeEnv
   -> {}
   -> List Exp
   -> { newExps: List Exp }
inferTypes gamma stuff exps =
  let (newExps, _) =
    List.foldl (\exp (newExpsAcc,stuffAcc) ->
                 let result = inferType gamma stuffAcc exp in
                 (result.newExp :: newExpsAcc, stuffAcc)
               )
               ([], stuff)
               exps
  in
  { newExps = List.reverse newExps }


checkType
    : TypeEnv
   -> {}
   -> Exp -- the expression whose analysis calls ("solicits") checkType.
          -- the solicitorExp can be used to generate type error messages, but
          -- this funciton rewrites only thisExp. the caller needs to add any
          -- desired metadata to solictorExp.
   -> Exp
   -> Type
   -> { okay: Bool, newExp: Exp }
checkType gamma stuff solicitorExp thisExp expectedType =
  let result = inferType gamma stuff thisExp in
  case (unExpr result.newExp).val.typ of
    Nothing ->
      { okay = False
      , newExp =
          result.newExp
            |> setTypeError (ExpectedButGot expectedType
                                            (Just (unExpr solicitorExp).val.eid)
                                            (unExpr result.newExp).val.typ)
      }

    Just inferredType ->
      if typeEquiv inferredType expectedType then
        { okay = True
        , newExp = result.newExp
        }

      else
        { okay = False
        , newExp =
            result.newExp
              |> setTypeError (ExpectedButGot expectedType
                                              (Just (unExpr solicitorExp).val.eid)
                                              (unExpr result.newExp).val.typ)
        }


--------------------------------------------------------------------------------

-- Currently shoving entire type error message and suggested fixes into Deuce.
-- So every line is a Synthesis Result.

deuceShow : Exp -> String -> SynthesisResult
deuceShow inputExp s =
  -- TODO: everything is a SynthesisResult, so pass in inputExp as dummy...
  synthesisResult s inputExp


deuceTool : String -> Exp -> SynthesisResult
deuceTool =
  synthesisResult


showTypeError : Exp -> Exp -> TypeError -> List SynthesisResult
showTypeError inputExp thisExp typeError =
  case typeError of
    OtherTypeError strings ->
      List.map (deuceShow inputExp) strings

    ExpectedButGot expectedType _ maybeActualType ->
      [ deuceShow inputExp <|
          "Expected: " ++ unparseType expectedType
      , deuceShow inputExp <|
          "Got: " ++ Maybe.withDefault "Nothing" (Maybe.map unparseType maybeActualType)
      , deuceShow inputExp <|
          "Will eventually insert hole [" ++ unparse thisExp ++ "] with expected type..."
      ]

    VarNotFound x suggestions ->
      let
        message =
          deuceShow inputExp <| "Cannot find variable `" ++ x ++ "`"
      in
        if List.length suggestions == 0 then
          [ message ]
        else
          message
            :: deuceShow inputExp "Maybe you want one of the following?"
            :: List.map
                 (\(y, ey) -> deuceTool y (replaceExpNode (unExpr thisExp).val.eid ey inputExp))
                 suggestions


makeDeuceTool : Exp -> EId -> DeuceTransformation
makeDeuceTool inputExp eId = \() ->
  let
    exp =
      LangTools.justFindExpByEId inputExp eId

    -- posInfo =
    --   [ show <| "Start: " ++ toString exp.start ++ " End: " ++ toString exp.end
    --   ]

    deuceTypeInfo =
      case ((unExpr exp).val.typ, (unExpr exp).val.typeError) of
        (Nothing, Nothing) ->
          [ deuceShow inputExp <|
              "This expression wasn't processed by the typechecker..."
          ]

        (Just t, Nothing) ->
          [ deuceShow inputExp <| "Type: " ++ unparseType t ]

        (_, Just typeError) ->
          showTypeError inputExp exp typeError

    insertAnnotationTool =
      case (unExpr exp).val.typ of
        Just typ ->
          let e__ =
            EParens space1
                    (withDummyExpInfo (EColonType space0 exp space1 typ space0))
                    Parens
                    space0
          in
          [ deuceTool "Insert Annotation" (replaceExpNodeE__ByEId eId e__ inputExp) ]

        Nothing ->
          []
  in
    List.concat <|
      [ deuceTypeInfo
      , insertAnnotationTool
      ]
