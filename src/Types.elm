module Types where

import Lang exposing (..)
import LangParser2 as Parser
import OurParser2 as P
import LangUnparser exposing (unparsePat, unparseType)
import Utils
import Ace
import Config

import Dict
import Set
import String
import Graphics.Element exposing (show)

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
    (TForall _ (One _) t1 _, TForall _ (One _) t2 _) -> astsMatch t1 t2
    (TForall _ (Many _ typeVars1 _) t1 _, TForall _ (Many _ typeVars2 _) t2 _) ->
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
  , activeConstraints : Constraints
  , typeErrors : List (P.WithPos TypeError)
  -- TODO remove Maybe from rawTypes and finalTypes
  , rawTypes : Dict.Dict EId (P.Pos, Maybe Type)
  , finalTypes : Dict.Dict EId (Maybe Type)
  , namedExps : List (Pat, EId)
  , constraintCount : Int
  , constraintVarCount : Int
  , preludeTypeEnv : Maybe TypeEnv
  }

type alias AceTypeInfo =
  { annotations : List Ace.Annotation
  , highlights : List Ace.Highlight
  , tooltips : List Ace.Tooltip
  }

type alias AndTypeInfo a =
  { result : a
  , typeInfo : TypeInfo
  }

debugLog = Config.debugLog Config.debugTypeChecker

stopAtError = False
  -- if False, continue typechecking after failed let equation (any expression)
sanityChecks = False
  -- if True, check well-formedness of synthesized types

-- AST Helpers for Types -----------------------------------------------------

tBool   = withDummyRange (TBool " ")
tNum    = withDummyRange (TNum " ")
tString = withDummyRange (TString " ")
tNull   = withDummyRange (TNull " ")
tVar x  = withDummyRange (TVar " " x)

tTupleRest ts tRest = withDummyRange (TTuple " " ts "" tRest "")
tTuple ts = tTupleRest ts Nothing

tList t = withDummyRange (TList " " t "")

tArrow (argTypes, retType) = withDummyRange (TArrow " " (argTypes ++ [retType]) "")
tPolyArrow vars arrowType  = tForall vars (tArrow arrowType)

tForall vars t =
  case vars of
    []    -> Debug.crash "tForall: no vars"
    [a]   -> withDummyRange (TForall " " (One (" ", a)) t "")
    a::bs -> let typeVars = ("",a) :: List.map (\a -> (" ", a)) bs in
             withDummyRange (TForall " " (Many " " typeVars "") t "")

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
  case Parser.parseT s of
    Err _ -> Debug.crash <| "bad primitive op type: " ++ s
    Ok t  -> t

opType : Op -> Type
opType op =
  case Utils.maybeFind op.val opTypeTable of
    Just t  -> t
    Nothing -> Debug.crash <| "opType not defined: " ++ strOp op.val

-- Operations on Type Environments -------------------------------------------

-- TODO add output constraints, for arguments that are lists, funcs, etc.

-- Bindings in the returned TypeEnv are in reverse order, so that
-- lookup* functions can look for most recent bindings from front
-- to back.
--
addBindings : List Pat -> List Type -> TypeEnv -> Result TypeError TypeEnv
addBindings pats types typeEnv =
  case Utils.maybeZip pats types of
    Nothing  -> Err "addBindings: can't zip"
    Just pts -> addBindingsMany pts typeEnv

addBindingsMany : List (Pat, Type) -> TypeEnv -> Result TypeError TypeEnv
addBindingsMany patsAndTypes typeEnv =
  List.foldl (\pt macc ->
    case macc of
      Err err -> Err err
      Ok acc  -> addBindingsOne pt acc
  ) (Ok typeEnv) patsAndTypes -- don't reverse, because fold-left

addBindingsOne : (Pat, Type) -> TypeEnv -> Result TypeError TypeEnv
addBindingsOne (p, t) acc =
  case p.val of

    PConst _ _     -> Ok acc
    PBase _ _      -> Ok acc
    PVar _ "_" _   -> Ok acc
    PVar _ x _     -> Ok (HasType x t :: acc)
    PAs _ x _ xPat -> addBindingsOne (xPat, t) (HasType x t :: acc)

    PList _ ps _ mpRest _ ->
      case t.val of

        TTuple _ ts _ mtRest _ ->
          let maybeRestBinding =
            case (mpRest, mtRest) of
              (Nothing, Nothing) -> Ok []

              (Just pRest, Just tRest) ->
                case (pRest.val, tRest.val) of
                  (PVar _ xRest _, TList _ tInvariant _) -> Ok [HasType xRest tInvariant]
                  _                                      -> Err "addBindings PList: ERROR 1 TODO"
              _                                          -> Err "addBindings PList: ERROR 2 TODO"
          in
          case (addBindings ps ts acc, maybeRestBinding) of
            (Ok acc', Ok restBinding) -> Ok (restBinding ++ acc')
            _ ->
              Err (Utils.spaces [unparsePat p, unparseType t])

        TList _ tInvariant _ ->
          case addBindings ps (List.repeat (List.length ps) tInvariant) acc of
            Err err ->
              Err <| Utils.spaces <|
                [ "addBindings", unparsePat p, unparseType t, err ]
            Ok acc' ->
              case mpRest of
                Nothing    -> Ok acc'
                Just pRest -> addBindingsOne (pRest, t) acc' -- t ~= tList tInvariant

        _ ->
          Err <| Utils.spaces [ "addBindings failed:", unparsePat p, unparseType t ]

addRecBinding rec p t typeEnv =
  if not rec then typeEnv
  else
    case p.val of
      PVar _ x _ -> HasType x t :: typeEnv
      _ ->
        let _ = debugLog "addRecBinding: multi TODO" (unparsePat p, unparseType t) in
        typeEnv

addTypeVarBindings : List Ident -> TypeEnv -> TypeEnv
addTypeVarBindings typeVars typeEnv =
  let newTypeBindings = List.map TypeVar (List.reverse typeVars) in
  newTypeBindings ++ typeEnv

addTypBindings : Pat -> Type -> TypeEnv -> Result () TypeEnv
addTypBindings p t typeEnv =
  case p.val of
    PVar _ x _ -> Ok (CheckType x t :: typeEnv)
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
             , activeConstraints = constraints' ++ typeInfo.activeConstraints
             , constraintCount = k + n }

addNamedExp : Pat -> EId -> TypeInfo -> TypeInfo
addNamedExp p eid typeInfo =
  { typeInfo | namedExps = (p, eid) :: typeInfo.namedExps }

addRawType : EId -> P.Pos -> Maybe Type -> TypeInfo -> TypeInfo
addRawType eid pos mt typeInfo =
  { typeInfo | rawTypes = Dict.insert eid (pos, mt) typeInfo.rawTypes }

addFinalType : EId -> Maybe Type -> TypeInfo -> TypeInfo
addFinalType eid mt typeInfo =
  { typeInfo | finalTypes = Dict.insert eid mt typeInfo.finalTypes }

addTypeErrorAt : P.Pos -> TypeError -> TypeInfo -> TypeInfo
addTypeErrorAt pos typeError typeInfo =
  { typeInfo | typeErrors = (P.WithPos typeError pos) :: typeInfo.typeErrors }

generateConstraintVars : Int -> TypeInfo -> (List Ident, TypeInfo)
generateConstraintVars n typeInfo =
  let k = typeInfo.constraintVarCount in
  let vars = List.map (\i -> "_x" ++ toString i) [k+1..k+n] in
  (vars, { typeInfo | constraintVarCount = k + n })

-- Operations on Arrows ------------------------------------------------------

type alias ArrowType = (List Type, Type)

stripArrow : Type -> Maybe ArrowType
stripArrow t =
  case t.val of
    TArrow _ ts _ -> Just (splitTypesInArrow ts)
    _             -> Nothing

stripPolymorphicArrow : Type -> Maybe (List Ident, ArrowType)
stripPolymorphicArrow t =
  case t.val of
    -- requiring all type variables in one TForall
    TForall _ (One typeVar) t0 _ ->
      stripArrow t0 |> Utils.bindMaybe (\arrow -> Just ([snd typeVar], arrow))
    TForall _ (Many _ typeVars _) t0 _ ->
      stripArrow t0 |> Utils.bindMaybe (\arrow -> Just (List.map snd typeVars, arrow))
    _ ->
      stripArrow t |> Utils.bindMaybe (\arrow -> Just ([], arrow))

splitTypesInArrow : List Type -> ArrowType
splitTypesInArrow ts =
  let n = List.length ts in
  let argTypes = List.take (n-1) ts in
  case List.drop (n-1) ts of
    [returnType] -> (argTypes, returnType)
    _            -> Debug.crash "splitTypesInArrow"

isArrowTemplate : Type -> Maybe ArrowType
isArrowTemplate tipe =
  case tipe.val of
    TArrow _ ts _ -> if Set.isEmpty (constraintVarsOf ts)
                       then Nothing
                       else Just (splitTypesInArrow ts)
    _             -> Nothing

-- detect constraint vars added by TS-Fun (could track in types instead)
isConstraintVar : Ident -> Bool
isConstraintVar a =
  case Utils.munchString "_x" a of
    Nothing -> False
    Just _  -> True

constraintVarsOf : List Type -> Set.Set Ident
constraintVarsOf ts =
  List.foldl (\argType acc ->
    case argType.val of
      TVar _ a -> if isConstraintVar a then Set.insert a acc else acc
      _        -> acc
  ) Set.empty ts

constraintVarsOfArrow : ArrowType -> Set.Set Ident
constraintVarsOfArrow (argTypes, retType) =
  constraintVarsOf (argTypes ++ [retType])

newArrowTemplate typeInfo n =
  let (constraintVars, typeInfo') = generateConstraintVars (1 + n) typeInfo in
  let arrow = splitTypesInArrow (List.map tVar constraintVars) in
  (arrow, typeInfo')

-- Type Well-Formedness -------------------------------------------- G |- T --

isWellFormed : TypeEnv -> Type -> Bool
isWellFormed typeEnv tipe =
  let (prenexVars, tipe') =
    case tipe.val of
      TForall _ (Many _ vars _) t _ -> (List.map snd vars, t)
      TForall _ (One var) t _       -> ([snd var], t)
      _                             -> ([], tipe)
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
         TVar _ x   -> if isConstraintVar x
                         then True
                         else acc && List.member (TypeVar x) typeEnv'
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
          case addBindings pats argTypes (addTypeVarBindings typeVars typeEnv) of
            Err err -> { result = False, typeInfo = addTypeErrorAt e.start err typeInfo }
            Ok typeEnv' -> checkType typeInfo typeEnv' eBody returnType

    EIf _ e1 e2 e3 _ -> -- [TC-If]
      let result1 = checkType typeInfo typeEnv e1 tBool in
      let result2 = checkType result1.typeInfo typeEnv e2 goalType in
      let result3 = checkType result2.typeInfo typeEnv e3 goalType in
      { result = result1.result && result2.result && result3.result
      , typeInfo = result3.typeInfo
      }

    -- TODO [TC-Case]

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
          { result = False, typeInfo = addTypeErrorAt e.start err typeInfo1 }
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
              { result = False, typeInfo = addTypeErrorAt e.start err' result2.typeInfo }
            Ok () ->
              { result = True, typeInfo = result2.typeInfo }

-- Type Synthesis ------------------------------------------- G |- e > T; C --

-- TODO allow mixing syntactic sugar forms for EFun/EApp

finishSynthesizeWithType eid pos tipe typeInfo =
  { result   = Just tipe
  , typeInfo = addRawType eid pos (Just tipe) typeInfo
  }

finishSynthesizeWithError pos error typeInfo =
  { result   = Nothing
  , typeInfo = addTypeErrorAt pos error typeInfo
  }

propagateResult result =
  { result = Nothing
  , typeInfo = result.typeInfo
  }

-- Nothing means type error or N/A (EComment, EOption, ETyp, etc.)
--
synthesizeType : TypeInfo -> TypeEnv -> Exp -> AndTypeInfo (Maybe Type)
synthesizeType typeInfo typeEnv e =
  let finish =
    { withType  = finishSynthesizeWithType e.val.eid e.start
    , withError = finishSynthesizeWithError e.start
    } in

  case e.val.e__ of

    EColonType _ e1 _ t1 _ -> -- [TS-AnnotatedExp]
      if not (isWellFormed typeEnv t1) then
        let err =
          Utils.spaces <|
            [ (toString e.val.eid)
            , strPos t1.start
            , "Type annotation not well-formed:"
            , String.trim (unparseType t1)
            ]
        in
        finish.withError err typeInfo
      else
        let result1 = checkType typeInfo typeEnv e1 t1 in
        if result1.result
          then finish.withType t1 result1.typeInfo
          else { result = Nothing, typeInfo = result1.typeInfo }

    EConst _ _ _ _ -> -- [TS-Const]
      finish.withType tNum typeInfo

    EBase _ baseVal -> -- [TS-Const]
      case baseVal of
        EBool _     -> finish.withType tBool typeInfo
        EString _ _ -> finish.withType tString typeInfo
        ENull       -> finish.withType tNull typeInfo

    EVar _ x -> -- [TS-Var]
      case lookupVar typeEnv x of
        Just t  -> finish.withType t typeInfo
        Nothing ->
          let err =
            Utils.spaces <|
              [ (toString e.val.eid)
              , "var not found: "
              , x
              ]
          in
          finish.withError err typeInfo

    EFun _ ps eBody _ -> -- [TS-Fun]
      let (arrow, typeInfo') = newArrowTemplate typeInfo (List.length ps) in
      tsFun finish typeInfo' typeEnv ps eBody arrow

    EOp _ op [] _ -> -- [TS-Op]
      finish.withType (opType op) typeInfo

    EOp _ op eArgs _ -> -- [TS-Op]
      case stripPolymorphicArrow (opType op) of
        Just ([], arrowType) ->
          tsAppMono finish typeInfo typeEnv eArgs arrowType
        Just polyArrowType ->
          tsAppPoly finish typeInfo typeEnv eArgs polyArrowType
        Nothing ->
          finish.withError "synthesizeType: EOp ..." typeInfo

    EApp _ eFunc eArgs _ -> -- [TS-App]
      let result1 = synthesizeType typeInfo typeEnv eFunc in
      case result1.result of
        Nothing ->
          finish.withError "synthesizeTyp: EApp ..." result1.typeInfo
        Just t1 ->
          case stripPolymorphicArrow t1 of
            Just ([], arrowType) ->
              tsAppMono finish result1.typeInfo typeEnv eArgs arrowType
            Just polyArrowType ->
              tsAppPoly finish result1.typeInfo typeEnv eArgs polyArrowType
            Nothing ->
              let err = "TS-App: t1 not arrow..." in
              finish.withError err result1.typeInfo

    EList _ es _ maybeRest _ -> -- [TS-List]
      let (maybeTypes, typeInfo') =
        List.foldl
           (\ei (accMaybeTypes, accTypeInfo) ->
             let result = synthesizeType accTypeInfo typeEnv ei in
             (result.result :: accMaybeTypes, result.typeInfo))
           ([], typeInfo)
           (List.reverse es)
      in
      case Utils.projJusts maybeTypes of
        Nothing -> finish.withError "synthesizeType: EList 1 ..." typeInfo'
        Just ts ->
          case maybeRest of
            Nothing -> finish.withType (tTuple ts) typeInfo'
            Just eRest ->
              let result = synthesizeType typeInfo' typeEnv eRest in
              case result.result of
                Nothing -> finish.withError "synthesizeType: EList 2 ..." result.typeInfo
                Just tRest -> finish.withType (tTupleRest ts (Just tRest)) result.typeInfo

    EIf _ e1 e2 e3 _ -> -- [TS-If]
      let result1 = checkType typeInfo typeEnv e1 tBool in
      if not result1.result then
        finish.withError "synthesizeType: EIf 1 ..." result1.typeInfo
      else
        let result2 = synthesizeType result1.typeInfo typeEnv e2 in
        let result3 = synthesizeType result2.typeInfo typeEnv e3 in
        case (result2.result, result3.result) of
          (Just t2, Just t3) ->
            case joinTypes t2 t3 of
              Ok t23 -> finish.withType t23 result3.typeInfo
              Err err ->
                finish.withError "synthesizeType: EIf 2 ..." result3.typeInfo
          _ ->
            finish.withError "synthesizeType: EIf 3 ..." result3.typeInfo

    ECase _ e0 branches _ -> -- [TS-Case]
      let result1 = synthesizeType typeInfo typeEnv e0 in
      case result1.result of
        Nothing -> finish.withError "synthesizeType: ECase ..." result1.typeInfo
        Just t1 ->
          let _ =
            if Set.isEmpty (constraintVarsOf [t1]) then ()
            else debugLog "ECase: TODO constraints based on patterns" ()
          in
          let maybeThings =
             List.foldl (\pe acc ->
               let (Branch_ _ pi ei _) = pe.val in
               case (acc, addBindingsOne (pi, t1) typeEnv) of
                 (Ok things, Ok typeEnvi) -> Ok ((typeEnvi,ei) :: things)
                 (Err err, _)             -> Err err
                 (_, Err err)             -> Err err
             ) (Ok []) branches
          in
          case maybeThings of
            Err err ->
              let err' = Utils.spaces [ "ECase: could not typecheck all patterns", err ] in
              finish.withError err' result1.typeInfo
            Ok things ->
              let result2 = synthesizeBranchTypes result1.typeInfo things in
              case Utils.projJusts result2.result of
                Nothing ->
                  let err = "ECase: could not typecheck all branches" in
                  finish.withError err result2.typeInfo
                Just ts ->
                  case joinManyTypes ts of
                    Err err -> finish.withError err result2.typeInfo
                    Ok t    -> finish.withType t result2.typeInfo

    ETypeCase _ _ _ _ -> -- [TS-Typecase]
      finish.withError "synthesizeType: ETypeCase ..." typeInfo

    ELet ws1 letKind rec p e1 e2 ws2 ->
      case (p.val, lookupTypAnnotation typeEnv p, rec, e1.val.e__) of

        (PVar _ "dummyPreludeMain" _, _, _, _) ->
          { result = Nothing
          , typeInfo = { typeInfo | preludeTypeEnv = Just typeEnv }
          }

        (_, Nothing, True, EFun _ ps _ _) -> -- [TS-LetRec-Fun] [TS-Fun-LetRec]
          let (arrow, typeInfo') = newArrowTemplate typeInfo (List.length ps) in
          let t1 = tArrow arrow in
          let e1' = replaceE__ e1 (EColonType "" e1 "" t1 "") in
          let typeEnv' = addRecBinding True p t1 typeEnv in
          tsLet finish.withType typeInfo' typeEnv' p e1' e2

        (_, Nothing, _, _) -> -- [TS-Let]
          tsLet finish.withType typeInfo typeEnv p e1 e2

        (_, Just t1, _, EColonType _ _ _ t1' _) ->
          if t1 == t1' then
            let typeEnv' = addRecBinding rec p t1 typeEnv in
            tsLet finish.withType typeInfo typeEnv' p e1 e2
          else -- throwing an error rather than doing a subtype check
            let err =
              Utils.spaces <|
                [ "checkType"
                , (toString e.val.eid)
                , "Double annotation. Remove one."
                ]
            in
            finish.withError err typeInfo

        (_, Just t1, _, _) -> -- [TS-AnnotatedLet]
          if not (isWellFormed typeEnv t1) then
            let err =
              Utils.spaces <|
                [ (toString e.val.eid)
                , strPos t1.start
                , "Type annotation not well-formed, at def:"
                , String.trim (unparseType t1)
                ]
            in
            if stopAtError then
              finish.withError err typeInfo
            else
              let t1 = tVar "__NO_TYPE__" in
              let typeInfo' = addTypeErrorAt t1.start err typeInfo in
              tsLetFinishE2 finish.withType typeInfo' typeEnv p t1 (eInfoOf e1) e2
          else
            let typeEnv' = addRecBinding rec p t1 typeEnv in
            let e1' = replaceE__ e1 (EColonType "" e1 "" t1 "") in
            let e' = replaceE__ e (ELet ws1 letKind rec p e1' e2 ws2) in
            synthesizeType typeInfo typeEnv' e'

    EIndList _ _ _ ->
      finish.withError "synthesizeType: EIndList ..." typeInfo

    -- don't need Ace annotations for the remaining expression kinds,
    -- so not calling not calling addRawType (i.e. finish)

    EComment _ _ e1 ->
      propagateResult <| synthesizeType typeInfo typeEnv e1

    EOption _ _ _ _ e1 ->
      propagateResult <| synthesizeType typeInfo typeEnv e1

    ETyp _ p t e1 _ ->
      case addTypBindings p t typeEnv of
        Err () -> { result = Nothing, typeInfo = typeInfo }
        Ok typeEnv' -> propagateResult <| synthesizeType typeInfo typeEnv' e1

    ETypeAlias _ p t e1 _ ->
      let typeEnv' = TypeAlias p t :: typeEnv in
      propagateResult <| synthesizeType typeInfo typeEnv' e1

tsAppMono finish typeInfo typeEnv eArgs (argTypes, retType) =
  case Utils.maybeZip eArgs argTypes of
    Nothing -> finish.withError "tsAppMono 1 ..." typeInfo
    Just argsAndTypes ->
      let (argsOkay, typeInfo') =
         List.foldl (\(ei,ti) (acc1,acc2) ->
           let res = checkType acc2 typeEnv ei ti in
           (acc1 && res.result, res.typeInfo)
         ) (True, typeInfo) argsAndTypes
      in
      if argsOkay
        then finish.withType retType typeInfo'
        else finish.withError "tsAppMono 2 ..." typeInfo'

tsAppPoly finish typeInfo typeEnv eArgs (typeVars, (argTypes, retType)) =
  let result = synthesizeTypeMany typeInfo typeEnv eArgs in
  case Utils.projJusts result.result of

    Nothing ->
      let err = "TS-App-Poly: could not typecheck all branches" in
      finish.withError err result.typeInfo

    Just tActuals ->
      let (constraintVars, typeInfo') =
        generateConstraintVars (List.length typeVars) result.typeInfo in
      let subst = List.map2 (\x y -> (x, tVar y)) typeVars constraintVars in
      let typeInfo'' =
         addRawConstraints
           (Utils.zip (List.map (applyUnifier subst) argTypes) tActuals)
           typeInfo'
      in
      let result = solveConstraintsFor typeInfo'' constraintVars in
      case result.result of
        Err err ->
          finish.withError err typeInfo''
        Ok unifier ->
          let retType' = retType |> applyUnifier subst |> applyUnifier unifier in
          finish.withType retType' result.typeInfo

tsFun finish typeInfo typeEnv ps eBody (argTypes, retType) =
  case addBindings ps argTypes typeEnv of
    Err err -> finish.withError err typeInfo
    Ok typeEnv' ->
      let result1 = synthesizeType typeInfo typeEnv' eBody in
      case result1.result of
        Nothing ->
          finish.withError "can't synthesize type for function body" result1.typeInfo
        Just retType' ->
          finish.withType
            (tArrow (argTypes, retType))
            (addRawConstraints [(retType, retType')] result1.typeInfo)

tsLet finishWithType typeInfo typeEnv p e1 e2 =
  let result1 = synthesizeType typeInfo typeEnv e1 in
  case result1.result of
    Nothing ->
      if stopAtError then
        { result = Nothing, typeInfo = result1.typeInfo }
      else
        let t1 = tVar "__NO_TYPE__" in
        tsLetFinishE2 finishWithType result1.typeInfo typeEnv p t1 (eInfoOf e1) e2

    Just t1 ->
      let result1' =
        case isArrowTemplate t1 of
          Just arrowType -> solveTemplateArrow result1.typeInfo (eInfoOf e1) arrowType
          Nothing        -> { result = Just t1, typeInfo = result1.typeInfo }
      in
      case result1'.result of
        Nothing  -> result1'
        Just t1' -> tsLetFinishE2 finishWithType result1'.typeInfo typeEnv p t1' (eInfoOf e1) e2

tsLetFinishE2 finishWithType typeInfo typeEnv p t1 eInfo1 e2 =
  if not sanityChecks then
    tsLetFinishE2_ finishWithType typeInfo typeEnv p t1 eInfo1.val e2
  else if isWellFormed typeEnv t1 then
    tsLetFinishE2_ finishWithType typeInfo typeEnv p t1 eInfo1.val e2
  else
    let err =
      Utils.spaces <|
        [ "[TYPE SYSTEM BUG]"
        , (toString eInfo1.val)
        , strPos t1.start
        , "Synthesized type not well-formed:"
        , String.trim (unparseType t1)
        ]
    in
    let typeInfo' = addTypeErrorAt eInfo1.start err typeInfo in
    if stopAtError then
      { result = Nothing, typeInfo = typeInfo' }
    else
      tsLetFinishE2_ finishWithType typeInfo' typeEnv p t1 eInfo1.val e2

tsLetFinishE2_ finishWithType typeInfo typeEnv p t1 e1eid e2 =
  case addBindings [p] [t1] typeEnv of
    Err err ->
      { result = Nothing, typeInfo = addTypeErrorAt p.start err typeInfo }
    Ok typeEnv' ->
      let typeInfo' = typeInfo |> addFinalType e1eid (Just t1) |> addNamedExp p e1eid in
      let result2 = synthesizeType typeInfo' typeEnv' e2 in
      case result2.result of
        Nothing -> { result = Nothing, typeInfo = result2.typeInfo }
        Just t2 -> finishWithType t2 result2.typeInfo

solveTemplateArrow : TypeInfo -> EInfo -> ArrowType -> AndTypeInfo (Maybe Type)
solveTemplateArrow typeInfo eFuncInfo arrow =
  let vars = Set.toList (constraintVarsOfArrow arrow) in
  let result = solveConstraintsFor typeInfo vars in
  case result.result of
    Err err ->
      { result = Nothing, typeInfo = addTypeErrorAt eFuncInfo.start err typeInfo }
    Ok unifier ->
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
      , typeInfo = addFinalType eFuncInfo.val (Just arrow) result.typeInfo }

synthesizeBranchTypes : TypeInfo -> List (TypeEnv, Exp) -> AndTypeInfo (List (Maybe Type))
synthesizeBranchTypes typeInfo list =
  let (maybeTypes, typeInfo') =
     List.foldl (\(typeEnvi,ei) (acc1,acc2) ->
       let resulti = synthesizeType acc2 typeEnvi ei in
       (resulti.result::acc1, resulti.typeInfo)
     ) ([], typeInfo) list
  in
  { result = List.reverse maybeTypes, typeInfo = typeInfo' }

synthesizeTypeMany : TypeInfo -> TypeEnv -> List Exp -> AndTypeInfo (List (Maybe Type))
synthesizeTypeMany typeInfo typeEnv es =
  synthesizeBranchTypes typeInfo (List.map (\ei -> (typeEnv, ei)) es)

-- Subtype Checking ------------------------------------------- T1 <: T2; C --

type alias SubtypeResult = AndTypeInfo (Result TypeError ())

checkSubtype : TypeInfo -> Type -> Type -> SubtypeResult
checkSubtype typeInfo tipe1 tipe2 =

  let ok  = { typeInfo = typeInfo, result = Ok () } in
  let err = { typeInfo = typeInfo, result = Err <| Utils.spaces
                [ "checkSubtype failed:"
                , String.trim (unparseType tipe1), " <: "
                , String.trim (unparseType tipe2)
                ] } in
  let okConstrain =
    { result = Ok (), typeInfo = addRawConstraints [(tipe1, tipe2)] typeInfo } in

  case (tipe1.val, tipe2.val) of

    (TNum _, TNum _)       -> ok
    (TBool _, TBool _)     -> ok
    (TString _, TString _) -> ok
    (TNull _, TNull _)     -> ok

    (TNamed _ a, TNamed _ b) ->
      if a == b then ok
      else
        let _ = debugLog "checkSubtype: expand aliases TODO" () in
        err

    -- constrain type inference vars; equate type vars
    (TVar _ a, TVar _ b) ->
      if isConstraintVar a && isConstraintVar b then (if a == b then ok else okConstrain)
      else if isConstraintVar a then okConstrain
      else if isConstraintVar b then okConstrain
      else if a == b then ok
      else err

    (TVar _ a, _) -> if isConstraintVar a then okConstrain else err
    (_, TVar _ b) -> if isConstraintVar b then okConstrain else err

    (TList _ t1 _, TList _ t2 _) -> checkSubtype typeInfo t1 t2

    (TDict _ k1 v1 _, TDict _ k2 v2 _) ->
      checkEquivType typeInfo k1 k2 `bindSubtypeResult` \typeInfo' ->
      checkSubtype typeInfo' v1 v2

    (TTuple _ ts1 _ mt1 _, TTuple _ ts2 _ mt2 _) ->
      case Utils.maybeZip ts1 ts2 of
        Nothing ->
          { result = Err "checkSubtype TTuple bad lengths", typeInfo = typeInfo }
        Just list ->
          checkSubtypeList typeInfo list `bindSubtypeResult` \typeInfo' ->
          checkSubMaybeType typeInfo' mt1 mt2

    -- converting from tuples to lists
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

    (TUnion _ _ _, TUnion _ _ _) -> let _ = debugLog "checkSubtype: TUnion TODO" () in err

    (TArrow _ arrow1 _, TArrow _ arrow2 _) ->
       let (args1, ret1) = splitTypesInArrow arrow1 in
       let (args2, ret2) = splitTypesInArrow arrow2 in
       case Utils.maybeZip args2 args1 of
         Nothing -> err
         Just contraChecks ->
           let result = checkSubtypeList typeInfo contraChecks in
           case result.result of
             Err _ -> err
             Ok () ->
               checkSubtype result.typeInfo ret1 ret2

    _ -> err

checkSubtypeList : TypeInfo -> List (Type, Type) -> SubtypeResult
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

checkSubMaybeType : TypeInfo -> Maybe Type -> Maybe Type -> SubtypeResult
checkSubMaybeType typeInfo mt1 mt2 =
  case (mt1, mt2) of
    (Just t1, Just t2) -> checkSubtype typeInfo t1 t2
    (Nothing, Nothing) -> { result = Ok (), typeInfo = typeInfo }
    _                  -> { result = Err "checkSubMaybeType failed...", typeInfo = typeInfo}

-- Check type equality modulo constraints.
--
checkEquivType : TypeInfo -> Type -> Type -> SubtypeResult
checkEquivType typeInfo tipe1 tipe2 =
  checkSubtype typeInfo  tipe1 tipe2 `bindSubtypeResult` \typeInfo' ->
  checkSubtype typeInfo' tipe2 tipe1

-- Check type equality without depending on any constraints.
--
checkEqualType : Type -> Type -> Bool
checkEqualType tipe1 tipe2 =
  let result = checkEquivType initTypeInfo tipe1 tipe2 in
  case result.result of
    Err _ -> False
    Ok () -> result.typeInfo.constraints == []

bindSubtypeResult : SubtypeResult -> (TypeInfo -> SubtypeResult) -> SubtypeResult
bindSubtypeResult res1 f =
  case res1.result of
    Err err -> { result = Err err, typeInfo = res1.typeInfo }
    Ok ()   -> f res1.typeInfo

-- Joining Types -------------------------------------------------------------

-- TODO could allow output constraints
joinTypes : Type -> Type -> Result TypeError Type
joinTypes t1 t2 =
  let dummyTypeInfo = initTypeInfo in
  case (checkSubtype dummyTypeInfo t1 t2).result of
    Ok () -> Ok t2
    _ ->
      case (checkSubtype dummyTypeInfo t2 t1).result of
        Ok () -> Ok t1
        _     ->
          case joinTypes_ t1 t2 of
            Ok t  -> Ok t
            Err _ -> joinTypes_ t2 t1

joinTypes_ : Type -> Type -> Result TypeError Type
joinTypes_ t1 t2 =
  let err =
     Err <| Utils.spaces
       [ "joinTypes failed:", unparseType t1, unparseType t2 ] in

  case (t1.val, t2.val) of

    (TTuple _ [] _ Nothing _, TList _ tInvariant _) -> Ok t2
    (TTuple _ [] _ Nothing _, TTuple _ ts _ mtRest _) ->
      case joinManyTypes ts of
        Err _ -> err
        Ok tJoin ->
          case mtRest of
            Nothing -> Ok (tList tJoin)
            Just tRest ->
              case joinTypes (tList tJoin) tRest of
                Err _ -> err
                Ok tListType -> Ok tListType

    -- TODO add more cases
    _ -> err

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

strUnifier : Unifier -> String
strUnifier =
  Utils.bracks <<
    Utils.spaces <<
      List.map (\(x,t) -> x ++ "=" ++ String.trim (unparseType t))

solveConstraintsFor : TypeInfo -> List Ident -> AndTypeInfo (Result TypeError Unifier)
solveConstraintsFor typeInfo vars =
  case unify vars [] [] typeInfo.activeConstraints of
    Ok (unifier, activeConstraints') ->
      -- let _ = Debug.log "unifer" (strUnifier unifier) in
      { result = Ok (List.reverse unifier)
      , typeInfo = { typeInfo | activeConstraints = List.reverse activeConstraints' } }
    Err err ->
      let _ = debugLog "TODO display the constraints that failed..." () in
      { result = Err err, typeInfo = typeInfo } -- restoring original constraints

unify : List Ident -> Constraints -> Unifier -> Constraints -> Result TypeError (Unifier, Constraints)
unify vars accActive accUnifier cs = case cs of

  [] -> Ok (accUnifier, accActive)

  one :: rest ->
    let (id,(t1,t2)) = one in
    if checkEqualType t1 t2 then unify vars accActive accUnifier rest
    else case (t1.val, t2.val) of

      (TVar _ a, TVar _ b) ->
        if List.member a vars then
          unify vars accActive ((a,t2)::accUnifier) (applyUnifierToConstraints [(a,t2)] rest)
        else if List.member b vars then
          unify vars accActive ((b,t1)::accUnifier) (applyUnifierToConstraints [(b,t1)] rest)
        else
          unify vars (one::accActive) accUnifier rest
      (TVar _ a, _) ->
        if List.member a vars then
          unify vars accActive ((a,t2)::accUnifier) (applyUnifierToConstraints [(a,t2)] rest)
        else
          unify vars (one::accActive) accUnifier rest
      (_, TVar _ b) ->
        if List.member b vars then
          unify vars accActive ((b,t1)::accUnifier) (applyUnifierToConstraints [(b,t1)] rest)
        else
          unify vars (one::accActive) accUnifier rest

      -- TODO ids for derived constraints

      (TList _ t1' _, TList _ t2' _) ->
        let induced = [(-1, (t1', t2'))] in
        unify vars accActive accUnifier (induced ++ rest)

      (TArrow _ ts1 _, TArrow _ ts2 _) ->
        case Utils.maybeZip ts1 ts2 of
          Nothing -> Err "unify TArrow: different arity"
          Just list ->
            let induced = List.map (\raw -> (-1, raw)) list in
            unify vars accActive accUnifier (induced ++ rest)

      _ ->
        let err = Utils.spaces <|
          [ "Unification failure:"
          , String.trim (unparseType t1)
          , String.trim (unparseType t2) ]
        in
        let _ = debugLog "TODO fail rather than continue... " err in
        unify vars (one::accActive) accUnifier rest
        -- Err err

-- TODO may need to apply entire unifier left-to-right
applyUnifier : Unifier -> Type -> Type
applyUnifier unifier =
  mapType <| \t -> case t.val of
    TVar _ a ->
      case Utils.maybeFind a unifier of
        Just t' -> t'
        Nothing -> t
    _ ->
      t

applyUnifierToConstraints : Unifier -> Constraints -> Constraints
applyUnifierToConstraints unifier =
  List.map <| Utils.mapSnd <| \(t1,t2) ->
    (applyUnifier unifier t1, applyUnifier unifier t2)

rewriteArrow : Unifier -> ArrowType -> ArrowType
rewriteArrow unifier (argTypes, retType) =
  let argTypes' = List.map (applyUnifier unifier) argTypes in
  let retType' = applyUnifier unifier retType in
  (argTypes', retType')

-- Entry Point for Typechecking ----------------------------------------------

typecheck : Exp -> AceTypeInfo
typecheck e =
  let _ = debugLog "TYPE CHECKING" "..." in
  let result = synthesizeType initTypeInfo preludeTypeEnv e in
  let _ = displayTypeInfo result.typeInfo in
  aceTypeInfo result.typeInfo

initTypeInfo : TypeInfo
initTypeInfo =
  { constraints = []
  , activeConstraints = []
  , typeErrors = []
  , rawTypes = Dict.empty
  , finalTypes = Dict.empty
  , namedExps = []
  , constraintCount = 0
  , constraintVarCount = 0
  , preludeTypeEnv = Nothing
  }

preludeTypeEnv : TypeEnv
preludeTypeEnv =
  let {typeInfo} = synthesizeType initTypeInfo [] Parser.prelude in
  case typeInfo.preludeTypeEnv of
    Just env -> env
    Nothing ->
      Debug.log "ERROR with preludeTypeEnv: \
                 dummyPreludeMain not found in prelude.little" []

displayTypeInfo : TypeInfo -> ()
displayTypeInfo typeInfo =
  -- let _ = displayRawTypes typeInfo in
  -- let _ = displayConstraints typeInfo in
  let _ = displayNamedExps typeInfo in
  let _ = displayTypeErrors typeInfo in
  ()

displayRawTypes : TypeInfo -> ()
displayRawTypes typeInfo =
  Dict.foldl (\eid (_, maybeType) () ->
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
  let _ = display "ALL CONSTRAINTS" typeInfo.constraints in
  let _ = display "ACTIVE CONSTRAINTS" typeInfo.activeConstraints in
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
      (_, Just (_, Just t)) ->
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
    List.foldr (\typeError () ->
      let _ = debugLog typeError.val (strPos typeError.pos) in
      ()
    ) () typeInfo.typeErrors

aceTypeInfo : TypeInfo -> AceTypeInfo
aceTypeInfo typeInfo =
{-
  let annots =
     -- for now, displaying (one of the) bindings for each line
     List.foldr (\(p, eid) acc ->
       let s1 = String.trim (LangUnparser.unparsePat p) in
       case (Dict.get eid typeInfo.finalTypes, Dict.get eid typeInfo.rawTypes) of
         (Just (Just t), _) ->
           let text = s1 ++ " : " ++ String.trim (LangUnparser.unparseType t) ++ " " in
           { row = p.start.line - 1, text = text, type_ = "info" } :: acc
         (_, Just (_, Just t)) ->
           let text = s1 ++ " : " ++ String.trim (LangUnparser.unparseType t) ++ " " in
           { row = p.start.line - 1, text = text, type_ = "info" } :: acc
         _ ->
           acc
     ) [] typeInfo.namedExps
  in
-}
  let errorAnnots =
    -- on each line that has at least one type error
    typeInfo.typeErrors
      |> List.foldr (\typeError acc -> Set.insert (typeError.pos.line - 1) acc) Set.empty
      |> Set.toList
      |> List.map (\i -> { row = i, text = "Type Error...", type_ = "error" } )
  in
  let (errorTips, errorHighlights) =
     List.foldr (\typeError (acc1,acc2) ->
       let (i,j) = (typeError.pos.line - 1, typeError.pos.col - 1) in
       let tip = { row = i, col = j, text = typeError.val } in
       let marker =
         { color = "orange"
         , range = { start = { row = i+1, column = j+1 }
                   , end   = { row = i+1, column = j+2 } } }
       in
       (tip::acc1, marker::acc2)
     ) ([],[]) typeInfo.typeErrors
  in
  let varTypeTips =
     List.foldr (\(p, eid) acc ->
       let s1 = String.trim (LangUnparser.unparsePat p) in
       case (Dict.get eid typeInfo.finalTypes, Dict.get eid typeInfo.rawTypes) of
         (Just (Just t), _) ->
           let text = s1 ++ " : " ++ String.trim (LangUnparser.unparseType t) ++ " " in
           { row = p.start.line - 1, col = p.start.col - 1, text = text } :: acc
         (_, Just (_, Just t)) ->
           let text = s1 ++ " : " ++ String.trim (LangUnparser.unparseType t) ++ " " in
           { row = p.start.line - 1, col = p.start.col - 1, text = text } :: acc
         _ ->
           acc
     ) [] typeInfo.namedExps
  in
  let expTypeTips =
     Dict.foldr (\eid (pos, maybeRawType) acc ->
       let record t =
         let text = String.trim (unparseType t) in
           { row = pos.line - 1, col = pos.col - 1, text = text }
       in
       case (Dict.get eid typeInfo.finalTypes, maybeRawType) of
         (Just (Just finalType), _) -> record finalType :: acc
         (_, Just rawType)          -> record rawType :: acc
         _                          -> acc
     ) [] typeInfo.rawTypes
  in
  { annotations = errorAnnots
  , highlights = errorHighlights
  , tooltips = -- errorTips last to shadow any expTypeTips
      varTypeTips ++ expTypeTips ++ errorTips
  }

-- dummy for stand-alone compilation
main = show 1


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
