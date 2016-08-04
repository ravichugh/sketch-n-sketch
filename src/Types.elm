module Types where

import Lang exposing (..)
import LangParser2
import LangUnparser exposing (unparsePat, unparseType)
import Utils
import Config

import Dict
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
    (VBase (Bool _), TBool _)        -> True
    (VBase (String _), TString _)    -> True
    (VBase Null, TNull _)            -> True
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
type alias Constraint  = (Type, Type)
type alias Constraints = List Constraint

type alias TypeInfo =
  { constraints : Constraints
  , typeErrors : List TypeError
  , rawTypes : Dict.Dict EId (Maybe Type)
  , namedExps : List (Pat, EId)
  , genSymCount : Int
  }

type alias AndTypeInfo a = { a | typeInfo : TypeInfo }

debugLog = Config.debugLog Config.debugTypeChecker

-- AST Helpers for Types -----------------------------------------------------

tBool   = withDummyRange (TBool " ")
tNum    = withDummyRange (TNum " ")
tString = withDummyRange (TString " ")
tNull   = withDummyRange (TNull " ")
tVar x  = withDummyRange (TVar " " x)

tTupleRest ts tRest = withDummyRange (TTuple " " ts "" tRest "")
tTuple ts = tTupleRest ts Nothing

tArrow argTypes retType = withDummyRange (TArrow " " (argTypes ++ [retType]) "")

-- Primitive Types -----------------------------------------------------------

-- could move these to (extern typ x T) definitions in Prelude...
opTypeTable : List (Op_, Type)
opTypeTable =
  List.map (Utils.mapSnd parseT)
    [ (Pi         , " Num")

    , (ToStr      , " TODO") -- " (forall a (-> a String))"
    , (DebugLog   , " TODO") -- " (forall a (-> a String))"

    , (Eq         , " TODO") -- " (forall a (-> a a Bool))"

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

addConstraints : Constraints -> TypeInfo -> TypeInfo
addConstraints constraints typeInfo =
  { typeInfo | constraints = typeInfo.constraints ++ constraints }

addNamedExp : Pat -> EId -> TypeInfo -> TypeInfo
addNamedExp p eid typeInfo =
  { typeInfo | namedExps = (p, eid) :: typeInfo.namedExps }

addRawType : EId -> Maybe Type -> TypeInfo -> TypeInfo
addRawType eid mt typeInfo =
  { typeInfo | rawTypes = Dict.insert eid mt typeInfo.rawTypes }

addTypeError : TypeError -> TypeInfo -> TypeInfo
addTypeError typeError typeInfo =
  { typeInfo | typeErrors = typeError :: typeInfo.typeErrors }

generateConstraintVars : Int -> TypeInfo -> (List Ident, TypeInfo)
generateConstraintVars n typeInfo =
  let k = typeInfo.genSymCount in
  let vars = List.map (\i -> "_x" ++ toString i) [k+1..k+n] in
  (vars, { typeInfo | genSymCount = k + n })

-- Operations on Arrows ------------------------------------------------------

isPolymorphicArrow : Type -> Maybe (List Ident, ArrowType)
isPolymorphicArrow t =
  let stripArrow t =
    case t.val of
      TArrow _ ts _ -> Just (splitTypesInArrow ts)
      _             -> Nothing
  in
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
  -- TODO detect TS-Fun, to solve on demand...
  False

-- Type Conversion ------------------------------------------ G |- e < T; C --

checkType : TypeInfo -> TypeEnv -> Exp -> Type -> AndTypeInfo {success: Bool}
checkType typeInfo typeEnv e goalType =
  case e.val.e__ of

    EFun _ pats eBody _ -> -- [TC-Fun]
      case isPolymorphicArrow goalType of
        Nothing -> { typeInfo = typeInfo, success = False }
        Just (typeVars, (argTypes, returnType)) ->
          case addBindings pats argTypes of
            Err () -> { typeInfo = typeInfo, success = False }
            Ok newBindings ->
              let newTypeBindings = List.map TypeVar (List.reverse typeVars) in
              let typeEnv' = newBindings ++ newTypeBindings ++ typeEnv in
              checkType typeInfo typeEnv' eBody returnType

    EIf _ e1 e2 e3 _ -> -- [TC-If]
      let result1 = checkType typeInfo typeEnv e1 tBool in
      let result2 = checkType result1.typeInfo typeEnv e2 goalType in
      let result3 = checkType result2.typeInfo typeEnv e3 goalType in
      { success = result1.success && result2.success && result3.success
      , typeInfo = result3.typeInfo
      }

    _ -> -- [TC-Sub]
      let result1 = synthesizeType typeInfo typeEnv e in
      let typeInfo1 = result1.typeInfo in
      case result1.tipe of
        Nothing ->
          let err =
            Utils.spaces <|
              [ "checkType"
              , (toString e.val.eid)
              , String.trim (unparseType goalType)
              , "failed to synthesize a type"
              ]
          in
          { success = False, typeInfo = addTypeError err typeInfo1 }
        Just t1 ->
          case checkOrConstrainSubtype t1 goalType of
            Err err ->
              let err' =
                Utils.spaces <|
                  [ "checkType"
                  , (toString e.val.eid)
                  , err
                  ]
              in
              { success = False, typeInfo = addTypeError err' typeInfo1 }
            Ok constraints ->
              { success = True, typeInfo = addConstraints constraints typeInfo1 }

-- Type Synthesis ------------------------------------------- G |- e > T; C --

-- TODO distinguish between Nothing for type error vs. N/A
synthesizeType : TypeInfo -> TypeEnv -> Exp -> AndTypeInfo { tipe: Maybe Type }
synthesizeType typeInfo typeEnv e =
  let finish maybeType typeInfo' =
    { tipe = maybeType
    , typeInfo = addRawType e.val.eid maybeType typeInfo'
    }
  in

  let tsAppMono typeInfo eArgs (argTypes, retType) =
    case Utils.maybeZip eArgs argTypes of
      Nothing -> finish Nothing typeInfo
      Just argsAndTypes ->
        let (argsOkay, typeInfo') =
           List.foldl (\(ei,ti) (acc1,acc2) ->
             let res = checkType acc2 typeEnv ei ti in
             (acc1 && res.success, res.typeInfo)
           ) (True, typeInfo) argsAndTypes
        in
        finish (if argsOkay then Just retType else Nothing) typeInfo'
  in

  case e.val.e__ of

    EColonType _ e1 _ t1 _ -> -- [TS-AnnotatedExp]
      let result1 = checkType typeInfo typeEnv e1 t1 in
      let tipe = if result1.success then Just t1 else Nothing in
      let typeInfo' = result1.typeInfo in
      finish tipe typeInfo'

    EConst _ _ _ _ -> -- [TS-Const]
      finish (Just tNum) typeInfo

    EBase _ baseVal -> -- [TS-Const]
      case baseVal of
        Bool _   -> finish (Just tBool) typeInfo
        String _ -> finish (Just tString) typeInfo
        Null     -> finish (Just tNull) typeInfo
        Star     -> finish (Nothing) typeInfo

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
          case result1.tipe of
            Nothing -> finish Nothing typeInfo
            Just retType ->
              finish (Just (tArrow argTypes retType)) typeInfo'

    EOp _ op [] _ -> -- [TS-Op]
      finish (Just (opType op)) typeInfo

    EOp _ op eArgs _ -> -- [TS-Op]
      case isPolymorphicArrow (opType op) of
        Just ([], arrowType) ->
          tsAppMono typeInfo eArgs arrowType
        Just _ ->
          let _ = debugLog "TS-Op: handle polymorphism TODO" () in
          finish Nothing typeInfo
        Nothing ->
          finish Nothing typeInfo

    EApp _ eFunc eArgs _ -> -- [TS-App]
      let result1 = synthesizeType typeInfo typeEnv eFunc in
      case result1.tipe of
        Nothing ->
          finish Nothing result1.typeInfo
        Just t1 ->
          case isPolymorphicArrow t1 of
            Just ([], (argTypes, retType)) ->
              if isArrowTemplate argTypes then
                let _ = debugLog "TS-App: arrow template TODO" () in
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
             (result.tipe :: accMaybeTypes, result.typeInfo))
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

    EIf _ _ _ _ _ -> -- [TS-If]
      let _ = debugLog "synthesizeType EIf TODO" () in
      finish Nothing typeInfo

    ECase _ _ _ _ -> -- [TS-Case]
      let _ = debugLog "synthesizeType ECase TODO" () in
      finish Nothing typeInfo

    ETypeCase _ _ _ _ -> -- [TS-Typecase]
      let _ = debugLog "synthesizeType ETypeCase TODO" () in
      finish Nothing typeInfo

    ELet ws1 letKind rec p e1 e2 ws2 ->

      let tsLet () =
        let result1 = synthesizeType typeInfo typeEnv e1 in
        case result1.tipe of
          Nothing ->
            -- TODO for now, continuing to process body even without
            -- a binding for the (failed) equation
            let result2 = synthesizeType result1.typeInfo typeEnv e2 in
            finish result2.tipe result2.typeInfo
          Just t1 ->
            case addBindings [p] [t1] of
              Err () ->
                finish Nothing result1.typeInfo
              Ok newBindings ->
                -- TODO eagerly solve and remove constraints
                let typeEnv' = newBindings ++ typeEnv in
                let typeInfo1' = addNamedExp p e1.val.eid result1.typeInfo in
                let result2 = synthesizeType typeInfo1' typeEnv' e2 in
                finish result2.tipe result2.typeInfo
      in

      case (lookupTypAnnotation typeEnv p, e1.val.e__) of

        (Nothing, _) -> -- [TS-Let]
          tsLet ()

        (Just t1, EColonType _ _ _ t1' _) ->
          if t1 == t1' then tsLet ()
          else -- throwing an error rather than doing a subtype check
            Debug.crash "[TS-Let]: Double annotation. Remove one."

        (Just t1, _) -> -- [TS-AnnotatedLet]
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

-- Subtype Checking ------------------------------- T1 <: T2 -- T1 <: T2; C --

checkSubtype : Type -> Type -> Result TypeError (Maybe ())
checkSubtype t1 t2 =
  if equal t1 t2 then Ok (Just ())
  else case (t1.val, t2.val) of
    -- TODO add more cases
    (TTuple _ ts _ Nothing _, TList _ tInvariant _) ->
      let n = List.length ts in
      checkSubtypeList (Utils.zip ts (List.repeat n tInvariant))
    (TTuple _ ts _ (Just tRest) _, TList _ tInvariant _) ->
      case tRest.val of
        TList _ t' _ ->
          let ts' = ts ++ [t'] in
          let n = List.length ts' in
          checkSubtypeList (Utils.zip ts' (List.repeat n tInvariant))
        _ ->
          Err "checkSubtype TTuple bad rest"
    _ ->
      Err <| Utils.spaces
        [ "checkSubtype failed:"
        , String.trim (unparseType t1)
        , String.trim (unparseType t2)
        ]

checkSubtypeList : List (Type, Type) -> Result TypeError (Maybe ())
checkSubtypeList list =
  List.foldl (\(t1,t2) acc ->
    case (acc, checkSubtype t1 t2) of
      (Ok (Just ()), Ok (Just ())) -> Ok (Just ())
      (Err err, _)                 -> Err err
      (_, Err err)                 -> Err err
      _                            -> Err "checkSubtypeList TODO error"
  ) (Ok (Just ())) list

checkOrConstrainSubtype : Type -> Type -> Result TypeError Constraints
checkOrConstrainSubtype t1 t2 =
  case checkSubtype t1 t2 of
    Err err      -> Err err
    Ok (Just ()) -> Ok []
    Ok Nothing   ->
      debugLog "checkOrConstrainSubtype TODO add constraint" <| Ok []

-- Joining Types -------------------------------------------------------------

joinTypes : Type -> Type -> Result TypeError Type
joinTypes t1 t2 =
  -- TODO
  case checkSubtype t1 t2 of
    Ok (Just ()) -> Ok t2
    _ ->
      case checkSubtype t2 t1 of
        Ok (Just ()) -> Ok t1
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

solveConstraints : TypeEnv -> Type -> Constraints -> Result TypeError (Type, Constraints)
solveConstraints typeEnv t constraints =
  debugLog "solveConstraints TODO" <| Ok (t, [])

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
  , typeErrors = []
  , rawTypes = Dict.empty
  , namedExps = []
  , genSymCount = 0
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

displayNamedExps : TypeInfo -> ()
displayNamedExps typeInfo =
  let _ = debugLog "NAMED EXPS" () in
  List.foldr (\(p, eid) () ->
    case Dict.get eid typeInfo.rawTypes of
      Just (Just t) ->
        let s1 = String.trim (LangUnparser.unparsePat p) in
        let s2 = String.trim (LangUnparser.unparseType t) in
        let _ = debugLog (s1 ++ " : " ++ s2 ++ " , eid") eid in
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
