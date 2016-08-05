module Types where

import Lang exposing (..)
import LangParser2
import OurParser2 as P
import LangUnparser exposing (unparsePat, unparseType)
import Utils
import Config

import Dict
import Set
import String

equal : Type -> Type -> Bool
equal t1 t2 =
  (astsMatch t1 t2) && (identifiersEquivalent t1 t2)

-- Do the types match, ignoring type variable names?
astsMatch t1 t2 =
  case (t1.val, t2.val) of
    (TNum _, TNum _)       -> True
    (TBool _, TBool _)     -> True
    (TString _, TString _) -> True
    (TNull _, TNull _)     -> True
    (TList _ listType1 _, TList _ listType2 _) -> astsMatch listType1 listType2
    (TDict _ keyType1 valueType1 _,
     TDict _ keyType2 valueType2 _) -> (astsMatch keyType1 keyType2) && (astsMatch valueType1 valueType2)
    (TTuple _ typeList1 _ maybeRestType1 _,
     TTuple _ typeList2 _ maybeRestType2 _) ->
      let maybeRestTypesMatch =
        case (maybeRestType1, maybeRestType2) of
          (Nothing, Nothing)               -> True
          (Just restType1, Just restType2) -> astsMatch restType1 restType2
          _                                -> False
      in
      maybeRestTypesMatch && (Utils.listsEqualBy astsMatch typeList1 typeList2)
    (TArrow _ typeList1 _,
     TArrow _ typeList2 _) -> Utils.listsEqualBy astsMatch typeList1 typeList2
    (TUnion _ typeList1 _,
     TUnion _ typeList2 _) -> Utils.listsEqualBy astsMatch typeList1 typeList2
    (TNamed _ ident1,
     TNamed _ ident2)      -> ident1 == ident2
    (TVar _ _, TVar _ _)   -> True
    (TWildcard _,
     TWildcard _)          -> True
    (TForall _ typeVars1 t1 _, TForall _ typeVars2 t2 _) ->
      List.length typeVars1 == List.length typeVars2 && astsMatch t1 t2
    _                      -> False

-- Presuming the types have the same AST structure, do the identifiers used
-- produce the same semantic meaning?
--
-- TODO depending on use, need to take scope into account...
--
-- e.g. (-> (List a) (List b)) is equivalent to (-> (List b) (List c))
--      but not to (-> (List y) (List y))
--
identifiersEquivalent t1 t2 =
  let flatIdents t =
    case t.val of
      TNum _    -> []
      TBool _   -> []
      TString _ -> []
      TNull _   -> []
      TList _ listType _          -> flatIdents listType
      TDict _ keyType valueType _ -> (flatIdents keyType) ++ (flatIdents valueType)
      TTuple _ typeList _ maybeRestType _ ->
        let restTypeIdents =
          case maybeRestType of
            Just restType -> flatIdents restType
            Nothing       -> []
        in
        (List.concatMap flatIdents typeList) ++ restTypeIdents

      TArrow _ typeList _ -> List.concatMap flatIdents typeList
      TUnion _ typeList _ -> List.concatMap flatIdents typeList
      TNamed _ _          -> []
      TVar _ ident        -> [ident]
      TWildcard _         -> []
      TForall _ _ _ _     -> Debug.crash "identifiersEquiv TForall TODO"
  in
  Utils.oneToOneMappingExists (flatIdents t1) (flatIdents t2)


valIsType val tipe =
  let unsupported msg =
    Debug.crash <| "typing values against " ++ msg ++ " is not supported"
  in
  case (val.v_, tipe.val) of
    (VConst _, TNum _)               -> True
    (VBase (VBool _), TBool _)       -> True
    (VBase (VString _), TString _)   -> True
    (VBase VNull, TNull _)           -> True
    (VList list, TList _ listType _) -> List.all (\v -> valIsType v listType) list
    (_, TDict _ _ _ _)               -> unsupported "dictionary types"
    (VList vlist, TTuple _ typeList _ maybeRestType _) ->
      let typeListsMatch =
        List.foldl
            (\(v, t) res -> res && valIsType v t)
            True
            (Utils.zip vlist typeList)
      in
      case maybeRestType of
        Nothing ->
          typeListsMatch && (List.length vlist == List.length typeList)
        Just restType ->
          typeListsMatch &&
          List.length vlist >= List.length typeList &&
          List.all
              (\v -> valIsType v restType)
              (List.drop (List.length typeList) vlist)
    (_, TArrow _ _ _)        -> unsupported "arrow types"
    (_, TUnion _ typeList _) -> List.any (valIsType val) typeList
    (_, TNamed _ _)          -> unsupported "type aliases"
    (_, TVar _ _)            -> unsupported "type variables"
    (_, TWildcard _)         -> True
    _                        -> False


------------------------------------------------------------------------------
-- Type Checking

type TypeEnvBinding
  = HasType Ident Type   -- TODO add patterns
  | CheckType Ident Type -- TODO add patterns
  | TypeVar Ident
  | TypeAlias Pat Type

type alias TypeEnv     = List TypeEnvBinding
type alias TypeError   = String
type alias RawConstraint  = (Type, Type)
type alias Constraint  = (Int, (Type, Type))
type alias Constraints = List Constraint
type alias EInfo       = P.WithInfo EId

type alias TypeInfo =
  { constraints : Constraints
  , solvedConstraints : Constraints
  , typeErrors : List TypeError
  , rawTypes : Dict.Dict EId (Maybe Type)
  , finalTypes : Dict.Dict EId (Maybe Type)
  , namedExps : List (Pat, EId)
  , constraintCount : Int
  , constraintVarCount : Int
  }

type alias AndTypeInfo a =
  { result : a
  , typeInfo : TypeInfo
  }

debugLog = Config.debugLog Config.debugTypeChecker

-- AST Helpers for Types -----------------------------------------------------

tBool   = withDummyRange (TBool " ")
tNum    = withDummyRange (TNum " ")
tString = withDummyRange (TString " ")
tNull   = withDummyRange (TNull " ")
tVar x  = withDummyRange (TVar " " x)

tTupleRest ts tRest = withDummyRange (TTuple " " ts "" tRest "")
tTuple ts = tTupleRest ts Nothing

tArrow (argTypes, retType) = withDummyRange (TArrow " " (argTypes ++ [retType]) "")
tPolyArrow vars arrowType  = tForall vars (tArrow arrowType)

tForall vars t =
  let vars' = case vars of
    []       -> Debug.crash "tForall: no vars"
    [a]      -> [(" ", a)]
    a::vars' -> ("",a) :: List.map (\a' -> (" ", a')) vars'
  in
  withDummyRange (TForall " " vars' t "" )

eInfoOf : Exp -> EInfo
eInfoOf e = { val = e.val.eid, start = e.start, end = e.end }

strEInfo : EInfo -> String
strEInfo eInfo = Utils.spaces [toString eInfo.val, strPos eInfo.start ]

strRawConstraint : RawConstraint -> String
strRawConstraint (t1,t2) =
  Utils.spaces [String.trim (unparseType t1), "=", String.trim (unparseType t2)]

strConstraint : Constraint -> String
strConstraint (i,rawConstraint) =
  Utils.spaces [Utils.bracks (toString i), strRawConstraint rawConstraint]

-- Primitive Types -----------------------------------------------------------

-- could move these to (extern typ x T) definitions in Prelude...
opTypeTable : List (Op_, Type)
opTypeTable =
  List.map (Utils.mapSnd parseT)
    [ (Pi         , " Num")

    , (ToStr      , " (forall a (-> a String))")
    , (DebugLog   , " (forall a (-> a String))")

    , (Eq         , " (forall a (-> a a Bool))")

    , (Cos        , " (-> Num Num)")
    , (Sin        , " (-> Num Num)")
    , (ArcCos     , " (-> Num Num)")
    , (ArcSin     , " (-> Num Num)")
    , (ArcTan2    , " (-> Num Num Num)")
    , (Floor      , " (-> Num Num)")
    , (Ceil       , " (-> Num Num)")
    , (Round      , " (-> Num Num)")
    , (Sqrt       , " (-> Num Num)")
    , (Plus       , " (-> Num Num Num)")
    , (Minus      , " (-> Num Num Num)")
    , (Mult       , " (-> Num Num Num)")
    , (Div        , " (-> Num Num Num)")
    , (Lt         , " (-> Num Num Bool)")
    , (Mod        , " (-> Num Num Num)")
    , (Pow        , " (-> Num Num Num)")

    , (DictEmpty  , " TODO") -- " (forall (k v) (Dict k v))")
    , (DictGet    , " TODO") -- " (forall (k v) (-> k (Dict k v) (union v Null)))"
    , (DictRemove , " TODO") -- " (forall (k v) (-> k (Dict k v) (Dict k v)))"
    , (DictInsert , " TODO") -- " (forall (k v) (-> k v (Dict k v) (Dict k v)))"
    -- , (DictMem , " TODO") -- " (forall (k v) (-> k (Dict k v) Bool))"
    ]

parseT : String -> Type
parseT s =
  case LangParser2.parseT s of
    Err _ -> Debug.crash <| "bad primitive op type: " ++ s
    Ok t  -> t

opType : Op -> Type
opType op =
  case Utils.maybeFind op.val opTypeTable of
    Just t  -> t
    Nothing -> Debug.crash <| "opType not defined: " ++ strOp op.val

-- Operations on Type Environments -------------------------------------------

-- Bindings in the returned TypeEnv are in reverse order, so that
-- lookup* functions can look for most recent bindings from front
-- to back.
--
addBindings : List Pat -> List Type -> Result () TypeEnv
addBindings pats types =
  case Utils.maybeZip pats types of
    Nothing ->
      let _ = debugLog "addBindings: can't zip" () in
      Err ()
    Just patsAndTypes ->
      Ok (List.foldl addBindingsOne [] patsAndTypes)
        -- don't reverse, because fold-left

addBindingsOne : (Pat, Type) -> TypeEnv -> TypeEnv
addBindingsOne (p, t) acc =
  case p.val of
    PConst _ _     -> acc
    PBase _ _      -> acc
    PVar _ x _     -> HasType x t :: acc
    PAs _ x _ xPat -> addBindingsOne (xPat, t) (HasType x t :: acc)
    PList _ ps _ mpRest _ ->
      case t.val of
        TTuple _ ts _ mtRest _ ->
          let restBinding =
            case (mpRest, mtRest) of
              (Nothing, Nothing) -> []
              (Just pRest, Just tRest) ->
                case (pRest.val, tRest.val) of
                  (PVar _ xRest _, TList _ tInvariant _) -> [HasType xRest tInvariant]
                  _ -> debugLog "addBindings PList: ERROR 1 TODO" []
              _ -> debugLog "addBindings PList: ERROR 2 TODO" []
          in
          case addBindings ps ts of
            Ok newBindings -> restBinding ++ newBindings ++ acc
            Err () ->
              let s1 = String.trim (unparsePat p) in
              let s2 = String.trim (unparseType t) in
              let _ = debugLog "addBindings PList: ERROR 3 TODO" (s1, s2) in
              restBinding ++ acc
        _ ->
          let _ = debugLog "addBindings: PList ERROR 4 TODO" () in
          acc

addTypBindings : Pat -> Type -> Result () TypeEnv
addTypBindings p t =
  case p.val of
    PVar _ x _ -> Ok [CheckType x t]
    _          -> Debug.crash "addTypBindings: Currently, only (typ x T) is supported."

lookupVar : TypeEnv -> Ident -> Maybe Type
lookupVar typeEnv x =
  case typeEnv of
    []                         -> Nothing
    TypeVar _ :: typeEnv'      -> lookupVar typeEnv' x
    TypeAlias _ _ :: typeEnv'  -> lookupVar typeEnv' x
    HasType x' t :: typeEnv'   -> if x == x' then Just t else lookupVar typeEnv' x
    CheckType x' t :: typeEnv' ->
      if x == x' then
        Nothing -- variable x not yet defined; explain the error
      else
        lookupVar typeEnv' x

lookupTypAnnotation : TypeEnv -> Pat -> Maybe Type
lookupTypAnnotation typeEnv p =
  case p.val of
    PVar _ x _ -> lookupTypAnnotation_ typeEnv x
    _          -> Nothing -- only supporting (typ x T) for now

lookupTypAnnotation_ : TypeEnv -> Ident -> Maybe Type
lookupTypAnnotation_ typeEnv x =
  case typeEnv of
    []                         -> Nothing
    TypeVar _      :: typeEnv' -> lookupTypAnnotation_ typeEnv' x
    TypeAlias _ _  :: typeEnv' -> lookupTypAnnotation_ typeEnv' x
    CheckType x' t :: typeEnv' -> if x == x' then Just t else lookupTypAnnotation_ typeEnv' x
    HasType x' t   :: typeEnv' -> if x == x' then Nothing else lookupTypAnnotation_ typeEnv' x

-- Operations on TypeInfos ---------------------------------------------------

addRawConstraints : (List RawConstraint) -> TypeInfo -> TypeInfo
addRawConstraints constraints typeInfo =
  let k = typeInfo.constraintCount in
  let n = List.length constraints in
  let constraints' = Utils.zip [k+1..k+n] constraints in
  { typeInfo | constraints = constraints' ++ typeInfo.constraints
             , constraintCount = k + n }

addNamedExp : Pat -> EId -> TypeInfo -> TypeInfo
addNamedExp p eid typeInfo =
  { typeInfo | namedExps = (p, eid) :: typeInfo.namedExps }

addRawType : EId -> Maybe Type -> TypeInfo -> TypeInfo
addRawType eid mt typeInfo =
  { typeInfo | rawTypes = Dict.insert eid mt typeInfo.rawTypes }

addFinalType : EId -> Maybe Type -> TypeInfo -> TypeInfo
addFinalType eid mt typeInfo =
  { typeInfo | finalTypes = Dict.insert eid mt typeInfo.finalTypes }

addTypeError : TypeError -> TypeInfo -> TypeInfo
addTypeError typeError typeInfo =
  { typeInfo | typeErrors = typeError :: typeInfo.typeErrors }

generateConstraintVars : Int -> TypeInfo -> (List Ident, TypeInfo)
generateConstraintVars n typeInfo =
  let k = typeInfo.constraintVarCount in
  let vars = List.map (\i -> "_x" ++ toString i) [k+1..k+n] in
  (vars, { typeInfo | constraintVarCount = k + n })

-- Operations on Arrows ------------------------------------------------------

stripArrow : Type -> Maybe ArrowType
stripArrow t =
  case t.val of
    TArrow _ ts _ -> Just (splitTypesInArrow ts)
    _             -> Nothing

stripPolymorphicArrow : Type -> Maybe (List Ident, ArrowType)
stripPolymorphicArrow t =
  case t.val of
    TForall _ typeVars t0 _ -> -- requiring all type variables in one TForall
      stripArrow t0 |> Utils.bindMaybe (\arrow -> Just (List.map snd typeVars, arrow))
    _ ->
      stripArrow t |> Utils.bindMaybe (\arrow -> Just ([], arrow))

-- TODO remove this function (or move to Parser) when TArrow is updated
splitTypesInArrow : List Type -> ArrowType
splitTypesInArrow ts =
  let n = List.length ts in
  let argTypes = List.take (n-1) ts in
  case List.drop (n-1) ts of
    [returnType] -> (argTypes, returnType)
    _            -> Debug.crash "splitTypesInArrow"

isArrowTemplate : List Type -> Bool
isArrowTemplate argTypes =
  not (Set.isEmpty (constraintVarsOf argTypes))

constraintVarsOf : List Type -> Set.Set Ident
constraintVarsOf argTypes =
  -- detect constraint vars added by TS-Fun
  let isConstraintVar a =
    case Utils.munchString "_x" a of
      Nothing -> False
      Just _  -> True
  in
  List.foldl (\argType acc ->
    case argType.val of
      TVar _ a -> if isConstraintVar a then Set.insert a acc else acc
      _        -> acc
  ) Set.empty argTypes

constraintVarsOfArrow : ArrowType -> Set.Set Ident
constraintVarsOfArrow (argTypes, _) = constraintVarsOf argTypes

-- Type Well-Formedness -------------------------------------------- G |- T --

isWellFormed : TypeEnv -> Type -> Bool
isWellFormed typeEnv tipe =
  let (prenexVars, tipe') =
    case tipe.val of
      TForall _ typeVars t _ -> (List.map snd typeVars, t)
      _                      -> ([], tipe)
  in
  let typeEnv' = List.map TypeVar (List.reverse prenexVars) ++ typeEnv in
  let noNestedForalls =
    foldType (\t acc ->
       case t.val of
         TForall _ _ _ _ -> False
         _               -> acc
     ) tipe' True
  in
  let allVarsBound =
    foldType (\t acc ->
       case t.val of
         TNamed _ x -> Debug.log "well-formed TNamed?" <|
                       acc && List.member (TypeVar x) typeEnv'
         TVar _ x   -> acc && List.member (TypeVar x) typeEnv'
         _          -> acc
     ) tipe' True
  in
  noNestedForalls && allVarsBound


-- Type Conversion ------------------------------------------ G |- e < T; C --

checkType : TypeInfo -> TypeEnv -> Exp -> Type -> AndTypeInfo Bool
checkType typeInfo typeEnv e goalType =
  case e.val.e__ of

    EFun _ pats eBody _ -> -- [TC-Fun]
      case stripPolymorphicArrow goalType of
        Nothing -> { typeInfo = typeInfo, result = False }
        Just (typeVars, (argTypes, returnType)) ->
          case addBindings pats argTypes of
            Err () -> { typeInfo = typeInfo, result = False }
            Ok newBindings ->
              let newTypeBindings = List.map TypeVar (List.reverse typeVars) in
              let typeEnv' = newBindings ++ newTypeBindings ++ typeEnv in
              checkType typeInfo typeEnv' eBody returnType

    EIf _ e1 e2 e3 _ -> -- [TC-If]
      let result1 = checkType typeInfo typeEnv e1 tBool in
      let result2 = checkType result1.typeInfo typeEnv e2 goalType in
      let result3 = checkType result2.typeInfo typeEnv e3 goalType in
      { result = result1.result && result2.result && result3.result
      , typeInfo = result3.typeInfo
      }

    _ -> -- [TC-Sub]
      let result1 = synthesizeType typeInfo typeEnv e in
      let typeInfo1 = result1.typeInfo in
      case result1.result of
        Nothing ->
          let err =
            Utils.spaces <|
              [ "checkType"
              , (toString e.val.eid)
              , String.trim (unparseType goalType)
              , "failed to synthesize a type"
              ]
          in
          { result = False, typeInfo = addTypeError err typeInfo1 }
        Just t1 ->
          let result2 = checkSubtype typeInfo1 t1 goalType in
          case result2.result of
            Err err ->
              let err' =
                Utils.spaces <|
                  [ "checkType"
                  , (toString e.val.eid)
                  , err
                  ]
              in
              { result = False, typeInfo = addTypeError err' result2.typeInfo }
            Ok () ->
              { result = True, typeInfo = result2.typeInfo }

-- Type Synthesis ------------------------------------------- G |- e > T; C --

finishSynthesizeType eid maybeType typeInfo =
  { result = maybeType
  , typeInfo = addRawType eid maybeType typeInfo
  }

-- TODO distinguish between Nothing for type error vs. N/A
synthesizeType : TypeInfo -> TypeEnv -> Exp -> AndTypeInfo (Maybe Type)
synthesizeType typeInfo typeEnv e =
  let finish = finishSynthesizeType e.val.eid in
  -- TODO add error messages throughout, where finish Nothing ...

  -- TODO move outside
  let tsAppMono typeInfo eArgs (argTypes, retType) =
    case Utils.maybeZip eArgs argTypes of
      Nothing -> finish Nothing typeInfo
      Just argsAndTypes ->
        let (argsOkay, typeInfo') =
           List.foldl (\(ei,ti) (acc1,acc2) ->
             let res = checkType acc2 typeEnv ei ti in
             (acc1 && res.result, res.typeInfo)
           ) (True, typeInfo) argsAndTypes
        in
        finish (if argsOkay then Just retType else Nothing) typeInfo'
  in

  case e.val.e__ of

    EColonType _ e1 _ t1 _ -> -- [TS-AnnotatedExp]
      if not (isWellFormed typeEnv t1) then
        let err =
          Utils.spaces <|
            [ (toString e.val.eid)
            , "Type annotation not well-formed:"
            , String.trim (unparseType t1)
            ]
        in
        finish Nothing (addTypeError err typeInfo)
      else
        let result1 = checkType typeInfo typeEnv e1 t1 in
        if result1.result
          then finish (Just t1) result1.typeInfo
          else finish Nothing result1.typeInfo

    EConst _ _ _ _ -> -- [TS-Const]
      finish (Just tNum) typeInfo

    EBase _ baseVal -> -- [TS-Const]
      case baseVal of
        EBool _     -> finish (Just tBool) typeInfo
        EString _ _ -> finish (Just tString) typeInfo
        ENull       -> finish (Just tNull) typeInfo

    EVar _ x -> -- [TS-Var]
      case lookupVar typeEnv x of
        Just t  -> finish (Just t) typeInfo
        Nothing ->
          let err =
            Utils.spaces <|
              [ (toString e.val.eid)
              , "var not found: "
              , x
              ]
          in
          finish Nothing (addTypeError err typeInfo)

    EFun _ ps eBody _ -> -- [TS-Fun]
      let (constraintVars, typeInfo') = generateConstraintVars (List.length ps) typeInfo in
      let argTypes = List.map tVar constraintVars in
      case addBindings ps argTypes of
        Err () -> finish Nothing typeInfo'
        Ok newBindings ->
          let typeEnv' = newBindings ++ typeEnv in
          let result1 = synthesizeType typeInfo' typeEnv' eBody in
          case result1.result of
            Nothing ->
              let _ = debugLog "can't synthesize type for function body" () in
              finish Nothing result1.typeInfo
            Just retType ->
              finish (Just (tArrow (argTypes, retType))) result1.typeInfo

    EOp _ op [] _ -> -- [TS-Op]
      finish (Just (opType op)) typeInfo

    EOp _ op eArgs _ -> -- [TS-Op]
      case stripPolymorphicArrow (opType op) of
        Just ([], arrowType) ->
          tsAppMono typeInfo eArgs arrowType
        Just _ ->
          let _ = debugLog "TS-Op: handle polymorphism TODO" () in
          finish Nothing typeInfo
        Nothing ->
          finish Nothing typeInfo

    EApp _ eFunc eArgs _ -> -- [TS-App]
      let result1 = synthesizeType typeInfo typeEnv eFunc in
      case result1.result of
        Nothing ->
          finish Nothing result1.typeInfo
        Just t1 ->
          case stripPolymorphicArrow t1 of
            Just ([], (argTypes, retType)) ->
              if isArrowTemplate argTypes then
                let _ = debugLog "TS-App: arrow template, solve on demand TODO" () in
                finish Nothing result1.typeInfo
              else
                tsAppMono result1.typeInfo eArgs (argTypes, retType)
            Just _ ->
              let _ = debugLog "TS-App: handle polymorphism TODO" () in
              finish Nothing result1.typeInfo
            Nothing ->
              let err = "TS-App: t1 not arrow..." in
              finish Nothing (addTypeError err result1.typeInfo)

    EList _ es _ (maybeRest) _ -> -- [TS-List]
      let (maybeTypes, typeInfo') =
        List.foldl
           (\ei (accMaybeTypes, accTypeInfo) ->
             let result = synthesizeType accTypeInfo typeEnv ei in
             (result.result :: accMaybeTypes, result.typeInfo))
           ([], typeInfo)
           (List.reverse es)
      in
      case Utils.projJusts maybeTypes of
        Nothing -> finish Nothing typeInfo'
        Just [] -> finish (Just (tTuple [])) typeInfo'
        Just ts ->
          case joinManyTypes ts of
            Err err -> finish Nothing typeInfo'
            Ok t    -> finish (Just (tTuple ts)) typeInfo'

    EIf _ e1 e2 e3 _ -> -- [TS-If]
      let result1 = checkType typeInfo typeEnv e1 tBool in
      if not result1.result then
        finish Nothing typeInfo
      else
        let result2 = synthesizeType result1.typeInfo typeEnv e2 in
        let result3 = synthesizeType result2.typeInfo typeEnv e3 in
        case (result2.result, result3.result) of
          (Just t2, Just t3) ->
            case joinTypes t2 t3 of
              Ok t23 -> finish (Just t23) result3.typeInfo
              Err err ->
                finish Nothing result3.typeInfo
          _ ->
            finish Nothing result3.typeInfo

    ECase _ _ _ _ -> -- [TS-Case]
      let _ = debugLog "synthesizeType ECase TODO" () in
      finish Nothing typeInfo

    ETypeCase _ _ _ _ -> -- [TS-Typecase]
      let _ = debugLog "synthesizeType ETypeCase TODO" () in
      finish Nothing typeInfo

    ELet ws1 letKind rec p e1 e2 ws2 ->

      let tsLet e1HasGoalType =
        let result1 = synthesizeType typeInfo typeEnv e1 in
        case result1.result of
          Nothing ->
            finish Nothing result1.typeInfo
          Just t1 ->
            let result1' =
              case (e1HasGoalType, e1.val.e__, stripArrow t1) of
                (False, EFun _ _ _ _, Just arrowType) ->
                  finishTsLetUnannotatedFunc result1.typeInfo e1.val.eid arrowType
                _ ->
                  { result = Just t1, typeInfo = result1.typeInfo }
            in
            case result1'.result of
              Nothing -> result1'
              Just t1' ->
                case addBindings [p] [t1'] of
                  Err () ->
                    finish Nothing result1'.typeInfo
                  Ok newBindings ->
                    let typeEnv' = newBindings ++ typeEnv in
                    let typeInfo1'' = addNamedExp p e1.val.eid result1'.typeInfo in
                    let result2 = synthesizeType typeInfo1'' typeEnv' e2 in
                    finish result2.result result2.typeInfo
      in

      case (lookupTypAnnotation typeEnv p, e1.val.e__) of

        (Nothing, _) -> -- [TS-Let]
          tsLet False

        (Just t1, EColonType _ _ _ t1' _) ->
          if t1 == t1' then tsLet True
          else -- throwing an error rather than doing a subtype check
            let err =
              Utils.spaces <|
                [ "checkType"
                , (toString e.val.eid)
                , "Double annotation. Remove one."
                ]
            in
            finish Nothing (addTypeError err typeInfo)

        (Just t1, _) -> -- [TS-AnnotatedLet]
          if not (isWellFormed typeEnv t1) then
            let err =
              Utils.spaces <|
                [ (toString e.val.eid)
                , "Type annotation not well-formed, at def:"
                , String.trim (unparseType t1)
                ]
            in
            finish Nothing (addTypeError err typeInfo)
          else
            let e1' = replaceE__ e1 (EColonType "" e1 "" t1 "") in
            let e' = replaceE__ e (ELet ws1 letKind rec p e1' e2 ws2) in
            synthesizeType typeInfo typeEnv e'

    EComment _ _ e1 ->
      let result1 = synthesizeType typeInfo typeEnv e1 in
      finish Nothing result1.typeInfo

    EOption _ _ _ _ e1 ->
      let result1 = synthesizeType typeInfo typeEnv e1 in
      finish Nothing result1.typeInfo

    ETyp _ p t e1 _ ->
      case addTypBindings p t of
        Err () -> finish Nothing typeInfo
        Ok newBindings ->
          let typeEnv' = newBindings ++ typeEnv in
          let result1 = synthesizeType typeInfo typeEnv' e1 in
          finish Nothing result1.typeInfo

    ETypeAlias _ p t e1 _ ->
      let typeEnv' = TypeAlias p t :: typeEnv in
      let result1 = synthesizeType typeInfo typeEnv' e1 in
      finish Nothing result1.typeInfo

    EIndList _ _ _ ->
      let _ = debugLog "synthesizeType EIndList" () in
      finish Nothing typeInfo

finishTsLetUnannotatedFunc : TypeInfo -> EId -> ArrowType -> AndTypeInfo (Maybe Type)
finishTsLetUnannotatedFunc typeInfo eFuncId arrow =
  let vars = Set.toList (constraintVarsOfArrow arrow) in
  case solveConstraints vars typeInfo.constraints of
    Err err ->
      { result = Nothing, typeInfo = addTypeError err typeInfo }
    Ok (unifier, remainingConstraints, solvedConstraints) ->
      let arrow' = rewriteArrow unifier arrow in
      let unconstrainedVars = Set.toList (constraintVarsOfArrow arrow') in
      let arrow =
        if unconstrainedVars == [] then
          tArrow arrow'
        else
          -- TODO pick nicer variable names: a, b, etc.
          let newTypeVars = List.map (String.dropLeft 1) unconstrainedVars in
          tPolyArrow newTypeVars <|
            rewriteArrow
              (List.map (\a -> (a, tVar (String.dropLeft 1 a))) unconstrainedVars)
              arrow'
      in
      -- let _ = debugLog "arrow after solve" (unparseType arrow) in
      { result = Just arrow
      , typeInfo = addFinalType eFuncId (Just arrow)
                     { typeInfo | constraints = remainingConstraints
                                , solvedConstraints = solvedConstraints
                                    ++ typeInfo.solvedConstraints }
      }

-- Subtype Checking ------------------------------------------- T1 <: T2; C --

checkSubtype : TypeInfo -> Type -> Type -> AndTypeInfo (Result TypeError ())
checkSubtype typeInfo t1 t2 =
  let err () =
     Err <| Utils.spaces
       [ "checkSubtype failed:"
       , String.trim (unparseType t1), " <: "
       , String.trim (unparseType t2)
       ]
  in
  if t1.val == t2.val then { result = Ok (), typeInfo = typeInfo }
  else case (t1.val, t2.val) of
    -- TODO add more cases
    (TTuple _ ts _ Nothing _, TList _ tInvariant _) ->
      let n = List.length ts in
      checkSubtypeList typeInfo (Utils.zip ts (List.repeat n tInvariant))
    (TTuple _ ts _ (Just tRest) _, TList _ tInvariant _) ->
      case tRest.val of
        TList _ t' _ ->
          let ts' = ts ++ [t'] in
          let n = List.length ts' in
          checkSubtypeList typeInfo (Utils.zip ts' (List.repeat n tInvariant))
        _ ->
          { result = Err "checkSubtype TTuple bad rest", typeInfo = typeInfo }
    (TVar _ a, _) ->
      case Utils.munchString "_x" a of
        Just _  -> { result = Ok (), typeInfo = addRawConstraints [(t1, t2)] typeInfo }
        Nothing -> { result = err (), typeInfo = typeInfo }
    _ ->
      { result = err (), typeInfo = typeInfo }

checkSubtypeList : TypeInfo -> List (Type, Type) -> AndTypeInfo (Result TypeError ())
checkSubtypeList typeInfo list =
  let (result, typeInfo') =
     List.foldl
       (\(t1,t2) (accResult, accTypeInfo) ->
         case accResult of
           Err err -> (accResult, accTypeInfo)
           Ok ()   -> let nextResult = checkSubtype accTypeInfo t1 t2 in
                      (nextResult.result, nextResult.typeInfo)
       ) (Ok (), typeInfo) list
  in
  { result = result, typeInfo = typeInfo' }

-- Joining Types -------------------------------------------------------------

joinTypes : Type -> Type -> Result TypeError Type
joinTypes t1 t2 =
  -- TODO add more cases
  -- TODO could allow output constraints
  let dummyTypeInfo = initTypeInfo in
  case (checkSubtype dummyTypeInfo t1 t2).result of
    Ok () -> Ok t2
    _ ->
      case (checkSubtype dummyTypeInfo t2 t1).result of
        Ok () -> Ok t1
        _ ->
          Err "joinTypes TODO"

joinManyTypes : List Type -> Result TypeError Type
joinManyTypes ts =
  case ts of
    [] -> Debug.crash "joinManyTypes: empty list"
    t::ts' ->
      List.foldl (\tNext acc ->
        case acc of
          Err err    -> Err err
          Ok tJoined -> joinTypes tNext tJoined
      ) (Ok t) ts'

-- Constraint Solving --------------------------------------------------------

type alias Unifier = List (Ident, Type)

solveConstraints : List Ident -> Constraints -> Result TypeError (Unifier, Constraints, Constraints)
solveConstraints vars constraints =
  -- let _ = debugLog "solveConstraints for" (vars) in
  let result =
     List.foldl (\constraint acc ->
       case acc of
         Err err -> Err err
         Ok (accUnifier, accRemaining, accRemoved) ->
           case solveConstraint vars constraint accUnifier of
             Err err -> Err err
             Ok (accUnifier', removedThisOne) ->
               if removedThisOne
               then Ok (accUnifier', accRemaining, constraint::accRemoved)
               else Ok (accUnifier', constraint::accRemaining, accRemoved)
     ) (Ok ([],[],[])) constraints
  in
  result

solveConstraint : List Ident -> Constraint -> Unifier -> Result TypeError (Unifier, Bool)
solveConstraint vars constraint unifier =
  let unify a t =
    case Utils.maybeFind a unifier of
      Nothing -> Ok ((a,t) :: unifier, True)
      Just tPrevious ->
        if t == tPrevious
          then Ok (unifier, True)
          else Err <| Utils.spaces
                 [ "Unification failure:"
                 , String.trim (unparseType tPrevious)
                 , String.trim (unparseType t)
                 ]
  in
  let (id,(t1,t2)) = constraint in
  case (t1.val, t2.val) of
    (TVar _ a, _) -> if List.member a vars then unify a t2 else Ok (unifier, False)
    (_, TVar _ a) -> solveConstraint vars (id, (t2, t1)) unifier
    _ ->
      let _ = debugLog "solveConstraint TODO" (unparseType t1, unparseType t2) in
      Ok (unifier, False)

applySolution : Unifier -> Type -> Type
applySolution unifier =
  mapType <| \t -> case t.val of
    TVar _ a ->
      case Utils.maybeFind a unifier of
        Just t' -> t'
        Nothing -> t
    _ ->
      t

rewriteArrow : Unifier -> ArrowType -> ArrowType
rewriteArrow unifier (argTypes, retType) =
  let argTypes' = List.map (applySolution unifier) argTypes in
  let retType' = applySolution unifier retType in
  (argTypes', retType')

-- Entry Point for Typechecking ----------------------------------------------

typecheck : Exp -> TypeInfo
typecheck e =
  let _ = debugLog "TYPE CHECKING" "..." in
  let result = synthesizeType initTypeInfo initTypeEnv e in
  let _ = displayTypeInfo result.typeInfo in
  result.typeInfo

initTypeInfo : TypeInfo
initTypeInfo =
  { constraints = []
  , solvedConstraints = []
  , typeErrors = []
  , rawTypes = Dict.empty
  , finalTypes = Dict.empty
  , namedExps = []
  , constraintCount = 0
  , constraintVarCount = 0
  }

initTypeEnv : TypeEnv
initTypeEnv =
  -- TODO typecheck prelude.little
  [ HasType "blobs" (parseT " (-> (List (List Svg)) Svg)")
  , HasType "line" (parseT " (-> Num Num Num Num Num Num Svg)")
  , HasType "rectangle" (parseT " (-> Num Num Num Num [Num Num Num Num] Svg)")
  ]

displayTypeInfo : TypeInfo -> ()
displayTypeInfo typeInfo =
  -- let _ = displayRawTypes typeInfo in
  -- let _ = displayConstraints typeInfo in
  let _ = displayNamedExps typeInfo in
  let _ = displayTypeErrors typeInfo in
  ()

displayRawTypes : TypeInfo -> ()
displayRawTypes typeInfo =
  Dict.foldl (\eid maybeType () ->
    case maybeType of
      Nothing -> ()
      Just t  ->
        let s = LangUnparser.unparseType t in
        let _ = debugLog "synthesized type: " (eid, s) in
        ()
  ) () typeInfo.rawTypes

displayConstraints : TypeInfo -> ()
displayConstraints typeInfo =
  let display cap constraints =
    if constraints == [] then ()
    else
      let _ = debugLog cap () in
      let _ = List.foldl (debugLog << strConstraint) () constraints in
      ()
  in
  let _ = display "REMAINING CONSTRAINTS" typeInfo.constraints in
  let _ = display "SOLVED CONSTRAINTS" typeInfo.solvedConstraints in
  ()

displayNamedExps : TypeInfo -> ()
displayNamedExps typeInfo =
  let _ = debugLog "NAMED EXPS" () in
  List.foldr (\(p, eid) () ->
    let s1 = String.trim (LangUnparser.unparsePat p) in
    case (Dict.get eid typeInfo.finalTypes, Dict.get eid typeInfo.rawTypes) of
      (Just (Just t), _) ->
        let s2 = String.trim (LangUnparser.unparseType t) in
        let _ = debugLog (s1 ++ " : " ++ s2 ++ " ") (strPos p.start) in
        ()
      (_, Just (Just t)) ->
        let s2 = String.trim (LangUnparser.unparseType t) in
        let _ = debugLog (s1 ++ " : " ++ s2 ++ " (raw) ") (strPos p.start) in
        ()
      _ -> ()
  ) () typeInfo.namedExps

displayTypeErrors : TypeInfo -> ()
displayTypeErrors typeInfo =
  let n = List.length typeInfo.typeErrors in
  if n == 0 then ()
  else
    let _ = debugLog "# TYPE ERRORS" n in
    List.foldr debugLog () typeInfo.typeErrors



{--

  T ::= B | (-> T1 T2) | (union T1 T2)
  S ::= (forall a S) | T

  *** Type Checking  (G |- e < S; C) ***
  *** Type Synthesis (G |- e > T; C) ***

  G |- e < T; C
  ------------------- [TS-AnnotatedExp]
  G |- (e : T) > T; C

  (typ x T1) \in G   G |- (let x (e1 : T1) e2) > T2; C
  ---------------------------------------------------- [TS-AnnotatedLet]
  G |- (let x e1 e2) > T2; C

  G |- e > T1; C1   T1 <: T2; C2
  ------------------------------ [TC-Sub]
  G |- e < T2; C1+C2

  ----------------- [TS-Const]   ---------------- [TS-Var]
  G |- c > ty(c); -              G |- x > G(x); -

  G |- e1 < Int; C1   G |- e2 < Int: C2
  ------------------------------------- [TS-Plus]
  G |- (+ e1 e2) > Int; C1+C2

  ty(op) = (-> T1 ... Tn T)   G |- ei < Ti; Ci
  -------------------------------------------- [TS-Op]
  G |- (op e1 ... en) > T; Ci

  G, ai, x:T1 |- e < T2; C
  ---------------------------------------- [TC-Fun]
  G |- (\x e) < (forall ai (-> T1 T2)); C

  X fresh   G, x:X |- e > T; C
  ---------------------------- [TS-Fun]
  G |- (\x e) > (-> X T); C

  G |- e1 > T1; C1   X11,X12 fresh   G |- e2 < X11; C2
  ---------------------------------------------------- [TS-App-1] TODO
  G |- e1 e2 > X12; C1+C2+{T1=(X11->X12)}

  G |- e1 > (forall ai (-> T11 T12)); C1
  G |- e2 > T2; C2
  Ti = Solve(G..., ai, T11=T2)
  ------------------------------------- [TS-App-2] TODO
  G |- e1 e2 > T12[Ti/ai]; C1+C2

  G |- e1 > T1; C1   Solve(G,T1,C1) = (S1,C1') TODO
  G, x:S1 |- e2 > T2; C2
  -------------------------------------------- [TS-Let]
  G |- (let x e1 e2) > T2; C1'+C2

  G |- e1 < Bool; C1       [TC-If]   G |- e1 < Bool; C1                 [TS-If]
  G |- e2 < T; C2                    G |- e2 > T2; C2
  G |- e3 < T; C3                    G |- e3 > T3; C3
  --------------------------------   ------------------------------------------
  G |- (if e1 e2 e3) < T; C1+C2+C3   G |- (if e1 e2 e3) > Join(T2,T3); C1+C2+C3

  G |- x > Tx; Cx          [TC-Typecase]   G |- x > Tx; Cx               [TS-Typecase]
  forall i.                                forall i.
    Si = T_x - T_1 - ... - T_{i_1} + T_i     Si = ...
    G, x:Si |- ei < T; Ci                    G, x:Si |- ei > Ti; Ci
  --------------------------------------   -------------------------------------------
  G |- (typecase x (ti ei)) < T; Cx+Ci     G |- (typecase x (ti ei)) > Join(Ti); Cx+Ci

  *** Subtype Checking ***

  ... usual subtyping rules and:

  T1 <: T2
  -----------   -------------   -------------
  T1 <: T2; -   X <: T; {X=T}   T <: X; {X=T}

--}
