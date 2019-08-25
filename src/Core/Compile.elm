module Core.Compile exposing
  ( exp
  , holeContext
  , datatypeContext
  )

import Lang as L
import Core.Lang as C
import Types2 as T

import Utils

type CompileError
  -- Expression errors
  = NegativeNumberNotSupported Int
  | NonIntegralNumberNotSupported Float
  | StringNotSupported String
  | NullNotSupported
  | ZeroParameterFunction L.Exp
  | NonIdentifierPattern L.Pat
  | ZeroArgumentApplication L.Exp
  | OperatorNotSupported L.Exp
  | IfNotSupported L.Exp
  | NonConstructorBranchPattern L.Pat
  | MultiArityConstructorPattern L.Pat
  | ImproperLet L.Exp
  | UnsupportedHoleKind L.Hole
  | SelectNotSupported L.Exp
  | NonUnaryConstructorApplication L.Exp
  | RecordNotSupported L.Exp
  | ImproperRecord L.Exp
  -- Type errors
  | ImproperArrowType L.Type
  | InternalTypeNotSupported L.Type
  | NonVarBindSpec (Maybe T.BindingSpecification)
  | MultiPatBindSpec (Maybe T.BindingSpecification)
  | BindingWithoutType T.TypeEnvElement
  | NonVarTypeBinding T.TypeEnvElement
  | TypeVarNotSupported T.TypeEnvElement
  | TypeAliasNotSupported T.TypeEnvElement
  | NonUnaryConstructorDefinition String

zeroName : String
zeroName =
  "Z"

succName : String
succName =
  "S"

trueName : String
trueName =
  "T"

falseName : String
falseName =
  "F"

consName : String
consName =
  "Cons"

nilName : String
nilName =
  "Nil"

exp : L.Exp -> Result CompileError C.Exp
exp lexp =
  case L.unwrapExp lexp of
    L.EConst _ num _ _ ->
      case Utils.intFromFloat num of
        Just n ->
          if n < 0 then
            Err (NegativeNumberNotSupported n)
          else if n == 0 then
            Ok (C.nullaryCtor zeroName)
          else -- if n > 0
            exp (L.eConstDummyLoc (toFloat <| n - 1))
              |> Result.map (C.ECtor succName)

        Nothing ->
          Err (NonIntegralNumberNotSupported num)

    L.EBase _ baseVal ->
      case baseVal of
        L.EBool b ->
          Ok <|
            C.nullaryCtor <|
              if b then trueName else falseName

        L.EString _ s ->
          Err (StringNotSupported s)

        L.ENull ->
          Err NullNotSupported

    L.EVar _ x ->
      Ok (C.EVar x)

    L.EFun _ pats body _ ->
      case pats of
        [] ->
          Err (ZeroParameterFunction lexp)

        [pat] ->
          case L.identifierFromPat pat of
            Just param ->
              exp body
                |> Result.map (C.EFix Nothing param)

            Nothing ->
              Err (NonIdentifierPattern pat)

        head :: tail ->
          exp <|
            L.eFun [head] (L.eFun tail body)

    L.EApp _ eHead eArgs _ _ ->
      case L.toTupleGet lexp of
        Just (n, i, arg) ->
          Result.map (C.EProj n i) (exp arg)

        Nothing ->
          case eArgs of
            [] ->
              Err (ZeroArgumentApplication lexp)

            [eArg] ->
              case L.unwrapExp eHead of
                L.EVar _ "assert" ->
                  Result.map
                    ( \arg ->
                        C.EAssert
                          (C.EProj 2 1 arg)
                          (C.EProj 2 2 arg)
                    )
                    (exp eArg)

                _ ->
                  Result.map2 C.EApp (exp eHead) (exp eArg)

            argHead :: argTail ->
              exp <|
                L.eApp (L.eApp eHead [argHead]) argTail

    L.EOp _ _ op args _ ->
      case (op.val, args) of
        -- Used for assert: a == b ~> (a, b)
        (L.Eq, [eLeft, eRight]) ->
          Result.map2
            (\left right -> C.ETuple [left, right])
            (exp eLeft)
            (exp eRight)

        _ ->
          Err (OperatorNotSupported lexp)

    L.EList _ entries _ _ _ ->
      case List.map Tuple.second entries of
        [] ->
          Ok (C.nullaryCtor nilName)

        head :: tail ->
          Result.map2
            (\hd tl -> C.ECtor consName (C.ETuple [hd, tl]))
            (exp head)
            (exp (L.eList tail Nothing))

    L.EIf _ _ _ _ _ _ _ ->
      Err (IfNotSupported lexp)

    L.ECase _ eScrutinee branches _ ->
      let
        branch : L.Branch -> Result CompileError (String, (String, C.Exp))
        branch b =
          case b.val of
            L.Branch_ _ pat body _ ->
              case L.unwrapPat pat of
                L.PRecord _ entries  _ ->
                  case L.entriesToMaybeCtorNameAndArgPats entries of
                    Just (ctorName, ctorArgPats) ->
                      case ctorArgPats of
                        [ctorArgPat] ->
                          case L.identifierFromPat ctorArgPat of
                            Just argName ->
                              Result.map
                                (\b -> (ctorName, (argName, b)))
                                (exp body)

                            Nothing ->
                              Err (NonIdentifierPattern ctorArgPat)

                        _ ->
                          Err (MultiArityConstructorPattern pat)

                    Nothing ->
                      Err (NonConstructorBranchPattern pat)

                _ ->
                  Err (NonConstructorBranchPattern pat)
      in
        Result.map2 C.ECase
          (exp eScrutinee)
          (Utils.projOk <| List.map branch branches)

    L.ELet _ _ declarations _ body ->
      let
        annotateRecursiveName : String -> C.Exp -> C.Exp
        annotateRecursiveName recursiveName e =
          case e of
            C.EFix _ param body ->
              C.EFix (Just recursiveName) param body

            _ ->
              e
      in
        case L.recordEntriesFromDeclarations declarations of
          Just entries ->
            let
              nameBindingPairs =
                List.map
                  (\(_, _, ident, _, exp) -> (ident, exp))
                  entries
            in
              case nameBindingPairs of
                [] ->
                  exp body

                [(name, binding)] ->
                  Result.map2 C.EApp
                    (Result.map (C.EFix Nothing name) (exp body))
                    (Result.map (annotateRecursiveName name) (exp binding))

                head :: tail ->
                  exp <|
                    L.eLet [head] (L.eLet tail body)

          Nothing ->
            Err (ImproperLet lexp)

    -- Types get compiled away
    L.EColonType _ body _ _ _ ->
      exp body

    L.EParens _ body _ _ ->
      exp body

    L.EHole _ holeKind ->
      case holeKind of
        L.EEmptyHole holeId ->
          Ok (C.EHole holeId)

        _ ->
          Err (UnsupportedHoleKind holeKind)

    L.ERecord _ _ declarations _ ->
      case L.recordEntriesFromDeclarations declarations of
        Just entries ->
          case L.tupleEncodingUnapply entries of
            Just tupleEntries ->
              tupleEntries
                |> List.map (Tuple.second >> exp)
                |> Utils.projOk
                |> Result.map C.ETuple

            Nothing ->
              case L.entriesToMaybeCtorNameAndArgExps entries of
                Just (ctorName, args) ->
                  case args of
                    [arg] ->
                      Result.map (C.ECtor ctorName) (exp arg)

                    _ ->
                      Err (NonUnaryConstructorApplication lexp)

                _ ->
                  Err (RecordNotSupported lexp)

        Nothing ->
          Err (ImproperRecord lexp)

    L.ESelect _ _ _ _ _ ->
      Err (SelectNotSupported lexp)

typ : L.Type -> Result CompileError C.Typ
typ tau =
  case T.matchArrow tau of
    Just ([], [argType], retType) ->
      Result.map2 C.TArr (typ argType) (typ retType)

    Just _ ->
      Err (ImproperArrowType tau)

    Nothing ->
      case L.tupleTypeArguments tau of
        Just components ->
          components
            |> List.map typ
            |> Utils.projOk
            |> Result.map C.TTuple

        Nothing ->
          case L.unwrapType tau of
            L.TVar _ datatypeName ->
              Ok (C.TData datatypeName)

            L.TParens _ tauInner _ ->
              typ tauInner

            _ ->
              Err (InternalTypeNotSupported tau)

bindSpec : Maybe T.BindingSpecification -> Result CompileError C.BindSpec
bindSpec b =
  let
    unwrapVar : L.Pat -> Result CompileError String
    unwrapVar p =
      case L.unwrapPat p of
        L.PVar _ x _ ->
          Ok x

        _ ->
          Err (NonVarBindSpec b)
  in
    case b of
      Just (T.Rec ps) ->
        case ps of
          [p] ->
            Result.map C.Rec (unwrapVar p)

          _ ->
            Err (MultiPatBindSpec b)

      Just (T.Arg p) ->
        Result.map C.Arg (unwrapVar p)

      Just (T.Dec p) ->
        Result.map C.Dec (unwrapVar p)

      Nothing ->
        Ok C.NoSpec

typeContext : T.TypeEnv -> Result CompileError C.TypeContext
typeContext =
  let
    typeEnvElement
      : T.TypeEnvElement -> Result CompileError (Maybe C.TypeBinding)
    typeEnvElement t =
      case t of
        T.HasType pat maybeType maybeBindSpec ->
          case L.unwrapPat pat of
            L.PVar _ x _ ->
              Result.map2 (\tau bs -> Just (x, (tau, bs)))
                ( maybeType
                    |> Result.fromMaybe (BindingWithoutType t)
                    |> Result.andThen typ
                )
                ( bindSpec maybeBindSpec
                )

            _ ->
              Err (NonVarTypeBinding t)

        T.TypeVar _ ->
          Err (TypeVarNotSupported t)

        T.TypeAlias _ _ ->
          Err (TypeAliasNotSupported t)

        T.TypeDef _ ->
          Ok Nothing
  in
    List.map typeEnvElement
      >> Utils.projOk
      >> Result.map Utils.filterJusts

holeContext : T.HoleEnv -> Result CompileError C.HoleContext
holeContext =
  let
    holeEnvElement :
      T.HoleEnvElement
        -> Result CompileError (C.HoleName, (C.TypeContext, C.Typ, C.BindSpec))
    holeEnvElement (holeId, (gamma, tau)) =
      Result.map2 (\gamma_ tau_ -> (holeId, (gamma_, tau_, C.NoSpec)))
        (typeContext gamma)
        (typ tau)
  in
    List.map holeEnvElement >> Utils.projOk

datatypeContext : T.DatatypeEnv -> Result CompileError C.DatatypeContext
datatypeContext =
  let
    datatypeConstructor :
      (L.Ident, List L.Type) -> Result CompileError (String, C.Typ)
    datatypeConstructor (ctorName, argTypes) =
      case argTypes of
        [argType] ->
          Result.map ((,) ctorName) (typ argType)

        _ ->
          Err (NonUnaryConstructorDefinition ctorName)

    datatypeDef :
      (L.Ident, (List L.Ident, List T.DataConDef))
        -> Result CompileError (String, List (String, C.Typ))
    datatypeDef (datatypeName, (_, constructors)) =
      constructors
        |> List.map datatypeConstructor
        |> Utils.projOk
        |> Result.map ((,) datatypeName)
  in
    List.map datatypeDef >> Utils.projOk
