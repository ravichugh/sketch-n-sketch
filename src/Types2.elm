module Types2 exposing
  ( typecheck
  , makeDeuceExpTool
  , makeDeucePatTool
  , AceTypeInfo
  , aceTypeInfo
  , dummyAceTypeInfo
  )

import Info exposing (WithInfo, withDummyInfo)
import Lang exposing (..)
import LangTools
import LangUtils
import ElmUnparser exposing (unparse, unparsePattern, unparseType)
import Ace
-- can't depend on Model, since ExamplesGenerated depends on Types2
import Utils

import Regex
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
        -- ept is "e" or "p" or "t"
        processThing ept thing toId thingToString =
          case (thing.val.typ, thing.val.typeError) of
            (Just _, Nothing) ->
              []
            _ ->
              addErrorAnnotation ept thing toId thingToString

        addErrorAnnotation ept thing toId thingToString =
          [ { row =
                thing.start.line - 1
            , type_ =
                "error"
            , text =
                String.concat
                  [ "Type error ["
                  , ept, "id: ", toString (toId thing.val), "; "
                  , "col: ", toString thing.start.col
                  , "]: ", "\n"
                  , String.trim (thingToString thing), "\n"
                  ]
            }
          ]

        processExp e =
          -- TODO for now, ignoring top-level prog and implicit main ---------------------------
          case ((unExpr e).start.line, (unExpr e).val.e__) of
            (1, _) -> []
            (_, EVar _ "main") -> []
            -- (_, ELet _ Let (Declarations [0] [] [] [(False, [LetExp _ _ p _ _ _])]) _ _) -> []
            _ ->
          --------------------------------------------------------------------------------------
          let
            result =
              annots1 ++ annots2

            annots1 =
              processThing "e" (unExpr e) .eid (Expr >> unparse)

            annots2 =
              case (unExpr e).val.e__ of
                EFun _ pats _ _->
                  pats
                    |> List.concatMap (\pat -> processThing "p" pat .pid unparsePattern)

                ELet _ _ (Declarations _ _ letAnnots _) _ _ ->
                  -- TODO: why aren't top-level pattern errors appearing in Ace annotations?
                  letAnnots
                    |> List.concatMap (\(LetAnnotation _ _ pat _ _ _) ->
                         processThing "p" pat .pid unparsePattern
                       )

                _ ->
                  []
          in
            result

        errorAnnotations =
          foldExp (\e acc -> processExp e ++ acc) [] exp

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


addHasMaybeType : (Pat, Maybe Type) -> TypeEnv -> TypeEnv
addHasMaybeType (p, mt) gamma =
  HasType p mt :: gamma


addHasType : (Pat, Type) -> TypeEnv -> TypeEnv
addHasType (p, t) gamma =
  addHasMaybeType (p, Just t) gamma


addTypeVar : Ident -> TypeEnv -> TypeEnv
addTypeVar typeVar gamma =
  TypeVar typeVar :: gamma


lookupVar : TypeEnv -> Ident -> Maybe (Maybe Type)
lookupVar gamma x =
  case gamma of
    HasType p mt :: gammaRest ->
      Utils.firstOrLazySecond
        (lookupVarInPat x p mt)
        (\_ -> lookupVar gammaRest x)

    _ :: gammaRest ->
      lookupVar gammaRest x

    [] ->
      Nothing


lookupVarInPat : Ident -> Pat -> Maybe Type -> Maybe (Maybe Type)
lookupVarInPat x p mt =
  case p.val.p__ of
    PConst _ _ -> Nothing
    PBase _ _ -> Nothing
    PWildcard _ -> Nothing

    PVar _ y _ ->
      if x == y then
        Just mt
      else
        Nothing

    -- TODO
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


findUnboundTypeVars : TypeEnv -> Type -> Maybe (List Ident)
findUnboundTypeVars gamma typ =
  let
    typeVarsInGamma =
      List.foldl (\binding acc ->
        case binding of
          TypeVar a -> a :: acc
          _         -> acc
      ) [] gamma

    freeTypeVarsInType =
      freeVarsType typeVarsInGamma typ
  in
    case Utils.listDiff freeTypeVarsInType typeVarsInGamma of
      [] ->
        Nothing

      unboundTypeVars ->
        Just unboundTypeVars


freeVarsType : List Ident -> Type -> List Ident
freeVarsType typeVarsInGamma typ =
  let
    result =
      helper typeVarsInGamma typ

    helper boundTypeVars typ =
      case typ.val of
        TVar _ a ->
          if a == "->" then
            []
          else if List.member a boundTypeVars then
            []
          else
            [a]

        TApp _ t0 typs _ ->
          let
            newBoundTypeVars =
              case matchArrow typ of
                Just (typeVars, _, _) ->
                  typeVars ++ boundTypeVars

                Nothing ->
                  boundTypeVars
          in
            List.concat (List.map (helper newBoundTypeVars) (t0::typs))

        TParens _ innerType _ ->
          helper boundTypeVars innerType

        _ ->
          let _ = Debug.log "TODO: implement freeVars for" (unparseType typ) in
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

type alias ArrowType = (List Ident, List Type, Type)


stripAllOuterTParens : Type -> Type
stripAllOuterTParens typ =
  case typ.val of
    TParens _ innerType _ ->
      stripAllOuterTParens innerType

    _ ->
      typ


-- This currently does not recurse into retType, so the argTypes list
-- always has length one.
--
-- This strips TParens off the outer type and off the arg and ret types.
--
matchArrow : Type -> Maybe ArrowType
matchArrow typ =
  let
    result =
      case (stripAllOuterTParens typ).val of
        TApp ws1 t0 typs InfixApp ->
          let
            typeVars =
              matchTypeVars ws1
          in
          case (t0.val, typs) of
            (TVar _ "->", [argType, retType]) ->
              Just ( typeVars
                   , [stripAllOuterTParens argType]
                   , stripAllOuterTParens retType
                   )
            _ ->
              Nothing
        _ ->
          Nothing

    _ =
      result
        |> Maybe.map (\(typeVars, argTypes, retType) ->
             (typeVars, List.map unparseType argTypes, unparseType retType)
           )
        |> if False
           then Debug.log "matchArrowType"
           else Basics.identity
  in
    result


matchTypeVars : WS -> List Ident
matchTypeVars ws =
  let
    regex =
      -- Grouping all type var characters and spaces into a single
      -- string, then splitting below. Would be better to split/group
      -- words directly in the regex...
      --
      "^[ ]*{-[ ]*forall [ ]*([a-z ]+)[ ]*-}[ ]*$"
    matches =
      Regex.find Regex.All (Regex.regex regex) ws.val
    result =
      case matches of
        [{submatches}] ->
          case Utils.projJusts submatches of
            Just [string] ->
              string
                |> Utils.squish
                |> String.split " "
            _ ->
              []
        _ ->
          []

    _ =
      result
        |> if False
           then Debug.log "matchTypeVars"
           else Basics.identity
  in
    result


-- This is currently not taking prior whitespace into account.
--
rebuildArrow : ArrowType -> Type
rebuildArrow (typeVars, argTypes, retType) =
  withDummyInfo <|
    TApp (rebuildTypeVars typeVars)
         (withDummyInfo (TVar space1 "->"))
         (argTypes ++ [retType])
         InfixApp


-- This is currently not taking prior whitespace into account.
--
rebuildTypeVars : List Ident -> WS
rebuildTypeVars typeVars =
  case typeVars of
    [] ->
      space0
    _  ->
      withDummyInfo <|
        "{- forall " ++ String.join " " typeVars ++ " -} "


--------------------------------------------------------------------------------

copyTypeInfoFrom : Exp -> Exp -> Exp
copyTypeInfoFrom fromExp toExp =
  let
    copyTypeFrom : Exp -> Exp -> Exp
    copyTypeFrom fromExp toExp =
      toExp |> setType (unExpr fromExp).val.typ

    copyTypeErrorFrom : Exp -> Exp -> Exp
    copyTypeErrorFrom fromExp toExp =
      case (unExpr fromExp).val.typeError of
        Just typeError ->
          toExp |> setTypeError typeError
        Nothing ->
          toExp
  in
  toExp
    |> copyTypeFrom fromExp
    |> copyTypeErrorFrom fromExp


--------------------------------------------------------------------------------

typecheck : Exp -> Exp
typecheck e =
  let result = inferType [] { inputExp = e } e in
  result.newExp

-- extra stuff for typechecker
type alias Stuff =
  { inputExp : Exp  -- root expression (model.inputExp)
  }

inferType
    : TypeEnv
   -> Stuff
   -> Exp
   -> { newExp: Exp }
        -- the inferred Maybe Type is in newExp.val.typ

inferType gamma stuff thisExp =
  case (unExpr thisExp).val.e__ of
    EConst _ _ _ _ ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TNum space1))) }

    EBase _ (EBool _) ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TBool space1))) }

    EBase _ (EString _ _) ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TString space1))) }

    EVar ws x ->
      case lookupVar gamma x of
        Just mt ->
          { newExp = thisExp |> setType mt }

        Nothing ->
          let
            message =
              deuceShow stuff.inputExp <| "Cannot find variable `" ++ x ++ "`"
            suggestions =
              List.map
                (\y -> (y, EVar ws y |> replaceE__ thisExp))
                (varNotFoundSuggestions x gamma)
            items =
              if List.length suggestions == 0 then
                [ message ]
              else
                message
                  :: deuceShow stuff.inputExp "Maybe you want one of the following?"
                  :: List.map
                       (\(y, ey) -> deuceTool y (replaceExpNode (unExpr thisExp).val.eid ey stuff.inputExp))
                       suggestions
          in
          { newExp = thisExp |> setTypeError (VarNotFound x items) }

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
      case findUnboundTypeVars gamma annotatedType of
        Nothing ->
          let
            result =
              checkType gamma stuff innerExp annotatedType

            (newInnerExp, finishNewExp) =
              if result.okay then
                (result.newExp, Basics.identity)

              else
                -- the call to checkType calls:
                -- setTypeError (ExpectedButGot annotatedType typ)
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

        Just unboundTypeVars ->
          -- TODO: Highlight occurrences of unbound variables with
          -- type polygons.
          --
          let
            newExp =
              thisExp
                |> setTypeError
                     (OtherTypeError
                        [ "ill-formed type annotation"
                        , "unbound: " ++ String.join " " unboundTypeVars
                        ])
          in
            { newExp = newExp }

{-
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
-}
    EFun ws1 pats body ws2 ->
      { newExp =
          thisExp
            |> setTypeError
                 (OtherTypeError ["trying to synthesize unannotated..."])
      }

    ELet ws1 letKind (Declarations po [] letAnnots letExps) ws2 body ->
      let
        -- Process LetAnnotations ----------------------------------------------

        -- type alias AnnotationTable = List (Ident, Type)

        -- (newLetAnnots, annotTable) : (List LetAnnotation, AnnotationTable)
        (newLetAnnots, annotTable) =
          letAnnots
            |> List.foldl processLetAnnotation ([], [])
            |> Tuple.mapFirst List.reverse
            |> Tuple.mapFirst markUndefinedAnnotations

        _ =
          annotTable
            |> List.map (Tuple.mapSecond unparseType)
            |> if True
               then Debug.log "annotTable"
               else Basics.identity

        processLetAnnotation
            (LetAnnotation ws0 ws1 pat fas ws2 typ) (accLetAnnots, accTable) =
          let
            (newPat, newTable) =
              processPat accTable pat typ
            newLetAnnots =
              LetAnnotation ws0 ws1 newPat fas ws2 typ :: accLetAnnots
          in
            (newLetAnnots, newTable)

        -- Somewhat similar to lookupVarInPat.
        --
        processPat : List (Ident, Type) -> Pat -> Type -> (Pat, List (Ident, Type))
        processPat accTable pat typ =
          case (pat.val.p__, typ.val) of
            (PVar _ x _, _) ->
              let
                errors =
                  errors1 ++ errors2

                errors1 =
                  Utils.maybeFind x accTable
                    |> Maybe.map (\_ ->
                         [ "Can't have multiple annotations for same name."
                         -- Report locations of other annotations, if desired...
                         ]
                       )
                    |> Maybe.withDefault []

                errors2 =
                  findUnboundTypeVars gamma typ
                    |> Maybe.map (\unboundTypeVars ->
                         [ "ill-formed type annotation"
                         , "unbound: " ++ String.join " " unboundTypeVars
                         ]
                       )
                    |> Maybe.withDefault []

              in
                if List.length errors == 0 then
                  ( pat |> setPatType (Just typ)
                  , (x, typ) :: accTable
                  )

                else
                  ( pat |> setPatTypeError (OtherTypeError errors)
                  , accTable
                  )

            _ ->
              ( pat |> setPatTypeError
                  (OtherTypeError ["this kind of type annotation is currently unsupported"])
              , accTable
              )

        markUndefinedAnnotations : List LetAnnotation -> List LetAnnotation
        markUndefinedAnnotations =
          let
            varsDefinedInThisELet =
              letExps
                |> List.concatMap Tuple.second
                |> List.concatMap (\(LetExp _ _ pat _ _ _) -> varsOfPat pat)

            maybeMarkPat =
              mapPat
                (\p ->
                   case p.val.p__ of
                     PVar _ x _ ->
                       if List.member x varsDefinedInThisELet then
                         p
                       else
                         p |> setPatTypeError (OtherTypeError ["this name is not defined"])
                     _ ->
                       p
                )
          in
            List.map (\(LetAnnotation ws0 ws1 pat fas ws2 typ) ->
              LetAnnotation ws0 ws1 (maybeMarkPat pat) fas ws2 typ
            )

        -- Process LetExps -----------------------------------------------------

        (newLetExps, newGamma) =
          letExps
            |> List.foldl processLetExp ([], gamma)
            |> Tuple.mapFirst List.reverse

        processLetExp (isRec, listLetExp) (accLetExpsRev, accGamma) =
          let
            listLetExpAndMaybeType : List (LetExp, Maybe Type)
            listLetExpAndMaybeType =
              listLetExp
                |> List.map (\letExp ->
                     let (LetExp ws0 ws1 pat fas ws2 expEquation) = letExp in
                     case pat.val.p__ of
                       PVar _ x _ ->
                         (letExp, Utils.maybeFind x annotTable)

                       -- To support other kinds of pats, will have to
                       -- walk expEquation as much as possible to push down
                       -- annotations. And any remaining annotations will
                       -- have to be treated as EColonTypes.
                       _ ->
                         let
                           newPat =
                             pat |> setPatTypeError
                               (OtherTypeError ["pattern not yet supported by type checker"])
                         in
                           (LetExp ws0 ws1 newPat fas ws2 expEquation, Nothing)
                   )

            gammaForEquations =
              if isRec == False then
                accGamma

              else
                let
                  assumedRecPatTypes : List (Pat, Maybe Type)
                  assumedRecPatTypes =
                    listLetExpAndMaybeType
                      |> List.map (\((LetExp _ _ pat _ _ _), maybeAnnotatedType) ->
                           (pat, maybeAnnotatedType)
                         )
                in
                  List.foldl addHasMaybeType accGamma assumedRecPatTypes

            newListLetExp =
              listLetExpAndMaybeType
                |> List.map (\( (LetExp ws0 ws1 pat fas ws2 expEquation)
                              , maybeAnnotatedType
                              ) ->
                     case maybeAnnotatedType of
                       Nothing ->
                         let
                           result =
                             inferType gammaForEquations stuff expEquation

                           newPat =
                             pat |> setPatType (unExpr result.newExp).val.typ
                         in
                         LetExp ws0 ws1 newPat fas ws2 result.newExp

                       Just annotatedType ->
                         let
                           result =
                             checkType gammaForEquations stuff expEquation annotatedType

                           newPat =
                             if result.okay then
                               pat |> setPatType (Just annotatedType)
                             else
                               pat |> setPatTypeError (OtherTypeError ["type error"])

                           -- TODO: add tool option to change annotation if result.okay == False
                           newExpEquation =
                             result.newExp
                         in
                         LetExp ws0 ws1 newPat fas ws2 newExpEquation
                   )

            newGamma =
              let
                maybePatTypes : Maybe (List (Pat, Type))
                maybePatTypes =
                  newListLetExp
                    |> List.map (\(LetExp _ _ newPat _ _ _) ->
                         newPat.val.typ |> Maybe.map ((,) newPat)
                       )
                    |> Utils.projJusts
              in
                -- Add bindings only if every LetExp type checked.
                case maybePatTypes of
                  Nothing ->
                    accGamma

                  Just patTypes ->
                    List.foldl addHasType accGamma patTypes
          in
            ((isRec, newListLetExp) :: accLetExpsRev, accGamma)

        -- Process Let-Body ----------------------------------------------------

        resultBody =
          inferType newGamma stuff body

        newBody =
          resultBody.newExp

        -- Rebuild -------------------------------------------------------------

        newExp =
          ELet ws1 letKind (Declarations po [] newLetAnnots newLetExps) ws2 newBody
            |> replaceE__ thisExp
            |> copyTypeInfoFrom newBody
      in
        { newExp = newExp }

    ELet ws1 letKind (Declarations po letTypes letAnnots letExps) ws2 body ->
      { newExp =
          thisExp
            |> setTypeError
                 (OtherTypeError ["not yet supporting type definitions..."])
      }

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

                  |> Utils.mapFirstSecond List.reverse List.reverse

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
   -> Stuff
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
   -> Stuff
   -> Exp
   -> Type
   -> { okay: Bool, newExp: Exp }
checkType gamma stuff thisExp expectedType =
  case ( (unExpr thisExp).val.e__
       , expectedType.val
       , matchArrow expectedType
       ) of

    (_, TParens _ innerExpectedType _, _) ->
      let
        result =
          checkType gamma stuff thisExp innerExpectedType
        newExp =
          thisExp
            |> copyTypeInfoFrom result.newExp
      in
      { okay = result.okay, newExp = newExp }

    (EParens ws1 innerExp parensStyle ws2, _, _) ->
      let
        result =
          checkType gamma stuff innerExp expectedType
        newExp =
          EParens ws1 result.newExp parensStyle ws2
            |> replaceE__ thisExp
            |> copyTypeInfoFrom result.newExp
      in
      { okay = result.okay, newExp = newExp }

    -- Not recursing into function body or retType because of the
    -- EParens and TParens cases, above.
    --
    (EFun ws1 pats body ws2, _, Just (typeVars, argTypes, retType)) ->
      if List.length pats < List.length argTypes then
        { okay = False
        , newExp =
            thisExp
              |> setTypeError
                   (OtherTypeError <|
                      "TODO List.length pats < List.length argTypes"
                        :: List.map unparsePattern pats
                        ++ List.map unparseType argTypes)
        }

      else if List.length pats > List.length argTypes then
        let
          -- Break up thisExp EFun into two nested EFuns, and check that.
          --
          result =
            checkType gamma stuff rewrittenThisExp expectedType

          (prefixPats, suffixPats) =
            Utils.split (List.length argTypes) pats

          rewrittenBody =
            -- TODO: Probably need to do something better with ids/breadcrumbs...
            Expr (withDummyInfo (exp_ (EFun space0 suffixPats body space0)))

          rewrittenThisExp =
            -- TODO: Probably need to do something better with ids/breadcrumbs...
            Expr (withDummyInfo (exp_ (EFun space0 prefixPats rewrittenBody space0)))

          (newPrefixPats, newSuffixPats, newBody) =
            case (unExpr result.newExp).val.e__ of
              EFun _ newPrefixPats innerFunc _ ->
                case (unExpr innerFunc).val.e__ of
                  EFun _ newSuffixPats newCheckedBody _ ->
                    (newPrefixPats, newSuffixPats, newCheckedBody)
                  _ ->
                    Debug.crash "the structure of the rewritten EFun has changed..."
              _ ->
                Debug.crash "the structure of the rewritten EFun has changed..."

          newExp =
            -- Keeping the structure of the original EFun in tact, not
            -- the rewrittenThisExp version. May need to track some
            -- breadcrumbs for stuffing type info into selection polygons...
            --
            EFun ws1 (newPrefixPats ++ newSuffixPats) newBody ws2
              |> replaceE__ thisExp
              |> copyTypeInfoFrom result.newExp
        in
        { okay = result.okay, newExp = newExp }

      else {- List.length pats == List.length argTypes -}
        let
          patTypes =
            Utils.zip pats argTypes
          newGamma_ =
            List.foldl addTypeVar gamma typeVars
          newGamma =
            List.foldl addHasType newGamma_ patTypes
          newPats =
            List.map (\(p,t) -> p |> setPatType (Just t)) patTypes
          result =
            checkType newGamma stuff body retType
        in
          if result.okay then
            { okay = True
            , newExp =
                EFun ws1 newPats result.newExp ws2
                  |> replaceE__ thisExp
                  |> setType (Just expectedType)
            }

          else
            let
              maybeActualType =
                (unExpr result.newExp).val.typ
                  |> Maybe.map (\actualRetType ->
                       rebuildArrow (typeVars, argTypes, actualRetType)
                     )
            in
            { okay = False
            , newExp =
                EFun ws1 newPats result.newExp ws2
                  |> replaceE__ thisExp
                  |> setTypeError (ExpectedButGot expectedType Nothing maybeActualType)
            }

    _ ->
      let
        result =
          inferType gamma stuff thisExp
        _ =
          (unparse thisExp, unparseType expectedType, expectedType)
            |> if False
               then Debug.log "catch-all synthesis rule"
               else Basics.identity
      in
        case (unExpr result.newExp).val.typ of
          Nothing ->
            { okay = False
            , newExp =
                result.newExp
                -- Don't want to overwrite existing error...
                --
                -- |> setTypeError (ExpectedButGot expectedType
                --                                 (unExpr result.newExp).val.typ)
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
                    |> setTypeError (ExpectedButGot expectedType (Just inferredType))
              }


--------------------------------------------------------------------------------

-- Currently shoving entire type error message and suggested fixes into Deuce.
-- So every line is a DeuceTypeInfoItem === SynthesisResult.

deuceShow : Exp -> String -> DeuceTypeInfoItem
deuceShow inputExp s =
  -- TODO: everything is a SynthesisResult, so pass in inputExp as dummy...
  synthesisResult s inputExp


deuceTool : String -> Exp -> DeuceTypeInfoItem
deuceTool =
  synthesisResult


showTypeError : Exp -> TypeError -> List DeuceTypeInfoItem
showTypeError inputExp typeError =
  case typeError of
    OtherTypeError strings ->
      List.map (deuceShow inputExp) strings

    ExpectedButGot expectedType maybeActualType ->
      [ deuceShow inputExp <|
          "Expected: " ++ unparseType expectedType
      , deuceShow inputExp <|
          "Got: " ++ Maybe.withDefault "Nothing" (Maybe.map unparseType maybeActualType)
      , deuceShow inputExp <|
          "Will eventually insert hole around this expression with expected type..."
      , deuceShow inputExp <|
          "Maybe an option to change expected type..."
      ]

    VarNotFound x items ->
      items


makeDeuceExpTool : Exp -> Exp -> DeuceTransformation
makeDeuceExpTool = makeDeuceToolForThing Expr unExpr


makeDeucePatTool : Exp -> Pat -> DeuceTransformation
makeDeucePatTool = makeDeuceToolForThing Basics.identity Basics.identity


makeDeuceToolForThing
   : (WithInfo (WithTypeInfo b) -> a)
  -> (a -> WithInfo (WithTypeInfo b))
  -> Exp
  -> a -- thing is a Thing (Exp or Pat or Type)
  -> DeuceTransformation
makeDeuceToolForThing wrap unwrap inputExp thing = \() ->
  let
    -- exp =
    --   LangTools.justFindExpByEId inputExp eId

    -- posInfo =
    --   [ show <| "Start: " ++ toString exp.start ++ " End: " ++ toString exp.end
    --   ]

    deuceTypeInfo =
      case ((unwrap thing).val.typ, (unwrap thing).val.typeError) of
        (Nothing, Nothing) ->
          [ deuceShow inputExp <|
              "This expression wasn't processed by the typechecker..."
          ]

        (Just t, Nothing) ->
          [ deuceShow inputExp <| "Type: " ++ unparseType t ]

        (_, Just typeError) ->
          showTypeError inputExp typeError

{-
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
-}
  in
    List.concat <|
      [ deuceTypeInfo
      -- , insertAnnotationTool
      ]
