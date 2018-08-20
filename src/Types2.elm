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
          case (e.start.line, e.val.e__) of
            (1, _) -> []
            (_, EVar _ "main") -> []
            (_, ELet _ Let (Declarations [0] ([], []) [] ([LetExp _ _ p _ _ _], [0])) _ _) -> []
            _ ->
          --------------------------------------------------------------------------------------
          case (e.val.typ, e.val.typeError) of
            (Just _, Nothing) ->
              []

            _ ->
              [ { row = e.start.line - 1
                , type_ = "error"
                , text = "Type error: " ++ unparse e ++ "\n"
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
        { row = e.start.line - 1
        , col = e.start.col - 1
        , text = "EId: " ++ toString e.val.eid
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
        -- the inferred Maybe Type is in newExp.typ

inferType gamma stuff thisExp =
  let (childExps, rebuildExp) = childExpsExtractors thisExp in
  case thisExp.val.e__ of
    EConst _ _ _ _ ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TNum space1))) }

    EBase _ (EBool _) ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TBool space1))) }

    EBase _ (EString _ _) ->
      { newExp = thisExp |> setType (Just (withDummyInfo (TString space1))) }

    EParens ws1 innerExp parensStyle ws2 ->
      let
        result =
          inferType gamma stuff innerExp

        newExp =
          EParens ws1 result.newExp parensStyle ws2
            |> replaceE__ thisExp
            |> setType result.newExp.val.typ
      in
        { newExp = newExp }

    EColonType ws1 innerExp ws2 annotatedType ws3 ->
      let
        result =
          checkType gamma stuff thisExp innerExp annotatedType

        newInnerExp =
          if result.okay then
            result.newExp

          else
            result.newExp
              -- call to checkType calls
              -- setTypeError (ExpectedButGot annotatedType result.newExp.val.typ)

        newExp =
          EColonType ws1 newInnerExp ws2 annotatedType ws3
            |> replaceE__ thisExp
            |> setType (Just annotatedType)
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
   -> Exp -- the expression whose analysis calls ("solicits") checkType
   -> Exp
   -> Type
   -> { okay: Bool, newExp: Exp }
checkType gamma stuff solicitorExp thisExp expectedType =
  let result = inferType gamma stuff thisExp in
  case result.newExp.val.typ of
    Nothing ->
      { okay = False
      , newExp =
          result.newExp
            |> setTypeError (ExpectedButGot expectedType result.newExp.val.typ)
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
              |> setTypeError (ExpectedButGot expectedType result.newExp.val.typ)
        }


--------------------------------------------------------------------------------

unparseTypeError : TypeError -> List String
unparseTypeError typeError =
  case typeError of
    ExpectedButGot expectedType maybeActualType ->
      [ "Expected: " ++ unparseType expectedType
      , "Got: " ++ Maybe.withDefault "Nothing" (Maybe.map unparseType maybeActualType)
      ]


makeDeuceTool : Exp -> EId -> DeuceTransformation
makeDeuceTool inputExp eId = \() ->
  let
    show s =
      synthesisResult s inputExp

    tool =
      synthesisResult

    exp =
      LangTools.justFindExpByEId inputExp eId

    -- posInfo =
    --   [ show <| "Start: " ++ toString exp.start ++ " End: " ++ toString exp.end
    --   ]

    typeInfo =
      case (exp.val.typ, exp.val.typeError) of
        (Nothing, Nothing) -> -- Debug.crash "noooope"
          [ show <| "This expression wasn't processed by the typechecker (or there was a bug)..." ]

        (Just t, Nothing) ->
          [ show <| "Type: " ++ unparseType t ]

        (_, Just typeError) ->
          List.concat <|
            [ List.map show <| unparseTypeError typeError
            , [show <| "Will eventually insert hole [" ++ unparse exp ++ "] with expected type..."]
            ]

    insertAnnotationTool =
      case exp.val.typ of
        Just typ ->
          let e__ =
            EParens space1
                    (withDummyExpInfo (EColonType space0 exp space1 typ space0))
                    Parens
                    space0
          in
          [ tool "Insert Annotation" (replaceExpNodeE__ByEId eId e__ inputExp) ]

        Nothing ->
          []
  in
    List.concat <|
      [ typeInfo
      , insertAnnotationTool
      ]
