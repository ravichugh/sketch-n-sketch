module Types exposing (..)

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
    (VConst _ _, TNum _)             -> True
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
sanityChecks = True
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

tUnion ts = withDummyRange (TUnion " " ts "")

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
    [ (Pi         , " Num ") -- TODO

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
    -- hard-coding intersection type for Plus in EOp rule
    -- , (Plus       , " (-> Num Num Num)")
    , (Plus       , " DUMMY")
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
  -- TODO figure out why this is parsing as a TNamed
  if s == " Num " then tNum else
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
  let fail s =
    Err <| Utils.spaces [ "addBindings", unparsePat p, unparseType t, s ] in

  case (p.val, t.val) of

    (PList _ _ _ _ _, TNamed _ a) ->
      case expandTypeAlias acc a of
        Nothing -> fail "Type alias not defined"
        Just ta -> addBindingsOne (p, ta) acc

    (PConst _ _, _)     -> Ok acc
    (PBase _ _, _)      -> Ok acc
    (PVar _ "_" _, _)   -> Ok acc
    (PVar _ x _, _)     -> Ok (HasType x t :: acc)
    (PAs _ x _ xPat, _) -> addBindingsOne (xPat, t) (HasType x t :: acc)

    (PList _ ps _ mpRest _, TTuple _ ts _ mtRest _) ->
      let maybeRestBinding =
        case (mpRest, mtRest) of
          (Nothing, Nothing) -> Ok []

          (Just pRest, Just tRest) ->
            case (pRest.val, tRest.val) of
              (PVar _ xRest _, TList _ tInvariant _) -> Ok [HasType xRest tInvariant]
              _                                      -> fail "PList ERROR 1 TODO"

          _ -> fail "PList ERROR 2 TODO"
      in
      case (addBindings ps ts acc, maybeRestBinding) of
        (Ok acc_, Ok restBinding) -> Ok (restBinding ++ acc_)
        _                         -> fail ""

    (PList _ ps _ mpRest _, TList _ tInvariant _) ->
      case addBindings ps (List.repeat (List.length ps) tInvariant) acc of
        Err err -> fail err
        Ok acc_ ->
          case mpRest of
            Nothing    -> Ok acc_
            Just pRest -> addBindingsOne (pRest, t) acc_ -- t ~= tList tInvariant

    _ -> fail ""

addRecBinding rec p t typeEnv =
  if not rec then typeEnv
  else
    case p.val of
      PVar _ x _ ->
        let tMono = -- monomorphic recursion
          case stripPolymorphicArrow t of
            Nothing         -> t
            Just (_, arrow) -> tArrow arrow
        in
        HasType x tMono :: typeEnv
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
    TypeVar _ :: typeEnv_      -> lookupVar typeEnv_ x
    TypeAlias _ _ :: typeEnv_  -> lookupVar typeEnv_ x
    HasType x_ t :: typeEnv_   -> if x == x_ then Just t else lookupVar typeEnv_ x
    CheckType x_ t :: typeEnv_ ->
      if x == x_ then
        Nothing -- variable x not yet defined; explain the error
      else
        lookupVar typeEnv_ x

lookupPat : TypeEnv -> Pat -> Maybe Type
lookupPat typeEnv p =
  case p.val of

    PVar _ x _  -> lookupVar typeEnv x
    PAs _ x _ _ -> lookupVar typeEnv x

    PConst _ _            -> Just tNum
    PBase _ (EBool _)     -> Just tBool
    PBase _ (EString _ _) -> Just tString
    PBase _ ENull         -> Just tNull

    PList _ ps _ mp _ ->
      Utils.projJusts (List.map (lookupPat typeEnv) ps) |> Utils.bindMaybe (\ts ->
        case mp of
          Nothing -> Just (tTuple ts)
          Just pRest ->
            lookupPat typeEnv pRest |> Utils.bindMaybe (\tRest ->
              Just (tTupleRest ts (Just tRest))
            )
      )

lookupTypAnnotation : TypeEnv -> Pat -> Maybe Type
lookupTypAnnotation typeEnv p =
  case p.val of
    PVar _ x _ -> lookupTypAnnotation_ typeEnv x
    _          -> Nothing -- only supporting (typ x T) for now

lookupTypAnnotation_ : TypeEnv -> Ident -> Maybe Type
lookupTypAnnotation_ typeEnv x =
  case typeEnv of
    []                         -> Nothing
    TypeVar _      :: typeEnv_ -> lookupTypAnnotation_ typeEnv_ x
    TypeAlias _ _  :: typeEnv_ -> lookupTypAnnotation_ typeEnv_ x
    CheckType x_ t :: typeEnv_ -> if x == x_ then Just t else lookupTypAnnotation_ typeEnv_ x
    HasType x_ t   :: typeEnv_ -> if x == x_ then Nothing else lookupTypAnnotation_ typeEnv_ x

-- expanding once, not recursively
--
expandTypeAlias : TypeEnv -> Ident -> Maybe Type
expandTypeAlias typeEnv x =
  let check pts =
    case pts of
      [] -> Nothing
      (p,t) :: pts_ ->
        case (p.val, t.val) of
          (PVar _ x_ _, _) ->
            if x == x_ then Just t else check pts_
          (PList _ ps _ Nothing _, TTuple _ ts _ Nothing _) ->
            check (Utils.zip ps ts ++ pts_)
               -- arities of ps ts should have been checked at definition
          _ ->
            Nothing
  in
  case typeEnv of
    TypeAlias p t :: typeEnv_ -> case check [(p,t)] of
                                   Just tx -> Just tx
                                   Nothing -> expandTypeAlias typeEnv_ x
    _ :: typeEnv_             -> expandTypeAlias typeEnv_ x
    []                        -> Nothing

lookupTypeAlias : TypeEnv -> Ident -> Bool
lookupTypeAlias typeEnv x =
  case expandTypeAlias typeEnv x of
    Just _  -> True
    Nothing -> False

narrowUnionType : Pat -> List Type -> Type -> TypeEnv -> Result TypeError (List Type, TypeEnv)
narrowUnionType p tAfterPreviousCases tThisCase typeEnv =
  let tThisCase_ =
    case (tThisCase.val, tAfterPreviousCases) of
      (TWildcard _, [t1]) -> t1
      (TWildcard _, _)    -> tUnion tAfterPreviousCases
      _                   -> tThisCase
  in
  let tAfterThisCase = subtractType tAfterPreviousCases tThisCase_ in
  case addBindingsOne (p, tThisCase_) typeEnv of
    Err err     -> Err err
    Ok typeEnv_ -> Ok (tAfterThisCase, typeEnv_)

subtractType : List Type -> Type -> List Type
subtractType union1 tipe2 =
  case tipe2.val of
    TUnion _ union2 _ -> List.foldl (flip subtractType) union1 union2
    _                 -> List.foldl
                           (\t1 acc -> if checkEqualType t1 tipe2 then acc else t1::acc)
                           [] union1

-- Operations on TypeInfos ---------------------------------------------------

addRawConstraints : (List RawConstraint) -> TypeInfo -> TypeInfo
addRawConstraints constraints typeInfo =
  let k = typeInfo.constraintCount in
  let n = List.length constraints in
  let constraints_ = Utils.zip (List.range (k+1) (k+n)) constraints in
  { typeInfo | constraints = constraints_ ++ typeInfo.constraints
             , activeConstraints = constraints_ ++ typeInfo.activeConstraints
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
  let vars = List.map (\i -> "_x" ++ toString i) (List.range (k+1) (k+n)) in
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
      stripArrow t0 |> Utils.bindMaybe (\arrow -> Just ([Tuple.second typeVar], arrow))
    TForall _ (Many _ typeVars _) t0 _ ->
      stripArrow t0 |> Utils.bindMaybe (\arrow -> Just (List.map Tuple.second typeVars, arrow))
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
  let (constraintVars, typeInfo_) = generateConstraintVars (1 + n) typeInfo in
  let arrow = splitTypesInArrow (List.map tVar constraintVars) in
  (arrow, typeInfo_)

-- Type Well-Formedness -------------------------------------------- G |- T --

isWellFormed : TypeEnv -> Type -> Bool
isWellFormed typeEnv tipe =
  let (prenexVars, tipe_) =
    case tipe.val of
      TForall _ (Many _ vars _) t _ -> (List.map Tuple.second vars, t)
      TForall _ (One var) t _       -> ([Tuple.second var], t)
      _                             -> ([], tipe)
  in
  let typeEnv_ = List.map TypeVar (List.reverse prenexVars) ++ typeEnv in
  let noNestedForalls =
    foldType (\t acc ->
       case t.val of
         TForall _ _ _ _ -> False
         _               -> acc
     ) tipe_ True
  in
  let allVarsBound =
    foldType (\t acc ->
       case t.val of
         TNamed _ x -> acc && lookupTypeAlias typeEnv_ x
         TVar _ x   -> if isConstraintVar x
                         then False
                         else acc && List.member (TypeVar x) typeEnv_
         _          -> acc
     ) tipe_ True
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
            Ok typeEnv_ -> checkType typeInfo typeEnv_ eBody returnType

    EIf _ e1 e2 e3 _ -> -- [TC-If]
      let result1 = checkType typeInfo typeEnv e1 tBool in
      let result2 = checkType result1.typeInfo typeEnv e2 goalType in
      let result3 = checkType result2.typeInfo typeEnv e3 goalType in
      { result = result1.result && result2.result && result3.result
      , typeInfo = result3.typeInfo
      }

    ECase _ e0 branches _ -> -- [TC-Case]
      let result1 = synthesizeType typeInfo typeEnv e0 in
      case result1.result of
        Nothing -> { result = False, typeInfo = result1.typeInfo }
        Just t1 ->
          let result_branches =
             List.foldl (\pe acc ->
               let (Branch_ _ pi ei _) = pe.val in
               case addBindingsOne (pi, t1) typeEnv of
                 Err err ->
                   { result = False
                   , typeInfo = addTypeErrorAt pi.start err acc.typeInfo }
                 Ok typeEnvi ->
                   let resulti = checkType acc.typeInfo typeEnvi ei goalType in
                   { result = resulti.result && acc.result
                   , typeInfo = resulti.typeInfo }
             ) { result = True, typeInfo = result1.typeInfo } branches
          in
          case result_branches.result of
            True -> result_branches
            False ->
              let err = "couldn't check all branches" in
              { result = False
              , typeInfo = addTypeErrorAt e.start err result_branches.typeInfo }

    ETypeCase _ p tbranches _ -> -- [TC-Typecase]
      case lookupPat typeEnv p of
        Nothing ->
          let err = "no type for the pattern: " ++ unparsePat p in
          { result = False, typeInfo = addTypeErrorAt p.start err typeInfo }

        Just tp ->
          case tp.val of
            TUnion _ union _ ->
              let (unionResidual, result_branches) =
                 List.foldl (\te (acc1, acc2) ->
                   let (TBranch_ _ ti ei _) = te.val in
                   case narrowUnionType p acc1 ti typeEnv of
                     Err err ->
                       let acc2_ = { result = False
                                   , typeInfo = addTypeErrorAt p.start err acc2.typeInfo } in
                       (acc1, acc2_)
                     Ok (acc1_, typeEnvi) ->
                       let resulti = checkType acc2.typeInfo typeEnvi ei goalType in
                       let acc2_ = { result = resulti.result && acc2.result
                                   , typeInfo = resulti.typeInfo } in
                       (acc1_, acc2_)
                 ) (union, { result = True, typeInfo = typeInfo }) tbranches
              in
              -- TODO could check unionResidual for exhaustiveness
              case result_branches.result of
                True -> result_branches
                False ->
                  let err = "couldn't check all branches" in
                  { result = False
                  , typeInfo = addTypeErrorAt e.start err result_branches.typeInfo }

            _ ->
              let err = "pattern is not a union type: " ++ unparseType tp in
              { result = False, typeInfo = addTypeErrorAt p.start err typeInfo }

    -- TODO [TC-Let]

    EComment _ _ e1 -> checkType typeInfo typeEnv e1 goalType

    -- TODO push goal down other sequencing forms

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
          let result2 = checkSubtype typeInfo1 typeEnv t1 goalType in
          case result2.result of
            Err err ->
              let err_ =
                Utils.spaces <|
                  [ "checkType"
                  , (toString e.val.eid)
                  , err
                  ]
              in
              { result = False, typeInfo = addTypeErrorAt e.start err_ result2.typeInfo }
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
  result
{-
  { result = Nothing
  , typeInfo = result.typeInfo
  }
-}

-- Nothing means type error or N/A (EComment, EOption, ETyp, etc.)
--
synthesizeType : TypeInfo -> TypeEnv -> Exp -> AndTypeInfo (Maybe Type)
synthesizeType typeInfo typeEnv e =
  let finish =
    { withType  = finishSynthesizeWithType e.val.eid e.start
    , withError = finishSynthesizeWithError e.start
    }
    in

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
      let (arrow, typeInfo_) = newArrowTemplate typeInfo (List.length ps) in
      tsFun finish typeInfo_ typeEnv ps eBody arrow

    EOp _ op [] _ -> -- [TS-Op]
      finish.withType (opType op) typeInfo

    EOp _ op eArgs _ -> -- [TS-Op]
      case (op.val, stripPolymorphicArrow (opType op)) of

        (Plus, _) -> -- hard-coded intersection type
          let result = tsAppMono finish typeInfo typeEnv eArgs ([tNum, tNum], tNum) in
          case result.result of
            Just _ -> result
            Nothing ->
              tsAppMono finish typeInfo typeEnv eArgs ([tString, tString], tString)

        (_, Just ([], arrowType)) ->
          tsAppMono finish typeInfo typeEnv eArgs arrowType
        (_, Just polyArrowType) ->
          tsAppPoly finish typeInfo typeEnv eArgs polyArrowType
        (_, Nothing) ->
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
      let (maybeTypes, typeInfo_) =
        List.foldl
           (\ei (accMaybeTypes, accTypeInfo) ->
             let result = synthesizeType accTypeInfo typeEnv ei in
             (result.result :: accMaybeTypes, result.typeInfo))
           ([], typeInfo)
           (List.reverse es)
      in
      case Utils.projJusts maybeTypes of
        Nothing -> finish.withError "synthesizeType: EList 1 ..." typeInfo_
        Just ts ->
          case maybeRest of
            Nothing -> finish.withType (tTuple ts) typeInfo_
            Just eRest ->
              let result = synthesizeType typeInfo_ typeEnv eRest in
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
              let err_ = Utils.spaces [ "ECase: could not typecheck all patterns", err ] in
              finish.withError err_ result1.typeInfo
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

    ETypeCase _ p tbranches _ -> -- [TS-Typecase]
      case lookupPat typeEnv p of
        Nothing ->
          let err = "no type for the pattern: " ++ unparsePat p in
          { result = Nothing, typeInfo = addTypeErrorAt p.start err typeInfo }

        Just tp ->
          case tp.val of
            TUnion _ union _ ->
              let (_, maybeThings) =
                 List.foldl (\te (acc1, acc2) ->
                   let (TBranch_ _ ti ei _) = te.val in
                   case (acc2, narrowUnionType p acc1 ti typeEnv) of
                     (Ok things, Ok (acc1_, typeEnvi)) -> (acc1_, Ok ((typeEnvi,ei) :: things))
                     (Err err, _)                      -> (acc1, Err err)
                     (_, Err err)                      -> (acc1, Err err)
                 ) (union, (Ok [])) tbranches
              in
              case maybeThings of
                Err err ->
                  let err_ = Utils.spaces [ "ETypeCase: could not typecheck all patterns", err ] in
                  finish.withError err_ typeInfo
                Ok things ->
                  let result2 = synthesizeBranchTypes typeInfo things in
                  case Utils.projJusts result2.result of
                    Nothing ->
                      let err = "ETypeCase: could not typecheck all branches" in
                      finish.withError err result2.typeInfo
                    Just ts ->
                      case joinManyTypes ts of
                        Err err -> finish.withError err result2.typeInfo
                        Ok t    -> finish.withType t result2.typeInfo

            _ ->
              let err = "pattern is not a union type: " ++ unparseType tp in
              { result = Nothing, typeInfo = addTypeErrorAt p.start err typeInfo }

    ELet ws1 letKind rec p e1 e2 ws2 ->
      case (p.val, lookupTypAnnotation typeEnv p, rec, e1.val.e__) of

        (PVar _ "dummyPreludeMain" _, _, _, _) ->
          { result = Nothing
          , typeInfo = { typeInfo | preludeTypeEnv = Just typeEnv }
          }

        (_, Nothing, True, EFun _ ps _ _) -> -- [TS-LetRec-Fun] [TS-Fun-LetRec]
          let (arrow, typeInfo_) = newArrowTemplate typeInfo (List.length ps) in
          let t1 = tArrow arrow in
          let e1_ = replaceE__ e1 (EColonType "" e1 "" t1 "") in
          let typeEnv_ = addRecBinding True p t1 typeEnv in
          tsLet finish.withType typeInfo_ typeEnv_ p e1_ e2

        (_, Nothing, _, _) -> -- [TS-Let]
          tsLet finish.withType typeInfo typeEnv p e1 e2

        (_, Just t1, _, EColonType _ _ _ t1_ _) ->
          if t1 == t1_ then
            let typeEnv_ = addRecBinding rec p t1 typeEnv in
            tsLet finish.withType typeInfo typeEnv_ p e1 e2
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
              let typeInfo_ = addTypeErrorAt t1.start err typeInfo in
              tsLetFinishE2 finish.withType typeInfo_ typeEnv p t1 (eInfoOf e1) e2
          else
            let typeEnv_ = addRecBinding rec p t1 typeEnv in
            let e1_ = replaceE__ e1 (EColonType "" e1 "" t1 "") in
            let e_ = replaceE__ e (ELet ws1 letKind rec p e1_ e2 ws2) in
            synthesizeType typeInfo typeEnv_ e_

    -- don't need Ace annotations for the remaining expression kinds,
    -- so not calling not calling addRawType (i.e. finish)

    EComment _ _ e1 ->
      propagateResult <| synthesizeType typeInfo typeEnv e1

    EOption _ _ _ _ e1 ->
      propagateResult <| synthesizeType typeInfo typeEnv e1

    ETyp _ p t e1 _ ->
      -- TODO check well-formedness
      case addTypBindings p t typeEnv of
        Err () -> { result = Nothing, typeInfo = typeInfo }
        Ok typeEnv_ -> propagateResult <| synthesizeType typeInfo typeEnv_ e1

    ETypeAlias _ p t e1 _ ->
      -- TODO check well-formedness
      let typeEnv_ = TypeAlias p t :: typeEnv in
      propagateResult <| synthesizeType typeInfo typeEnv_ e1

-- TODO need to instantiateTypes of arguments, as in tsAppPoly
tsAppMono finish typeInfo typeEnv eArgs (argTypes, retType) =
  let checkArgs argsAndTypes retType =
    let (argsOkay, typeInfo_) =
       List.foldl (\(ei,ti) (acc1,acc2) ->
         let res = checkType acc2 typeEnv ei ti in
         (acc1 && res.result, res.typeInfo)
       ) (True, typeInfo) argsAndTypes
    in
    if argsOkay then
      finish.withType retType typeInfo_
    else
      finish.withError "Function arguments don't match required types." typeInfo_
  in
  let (nArgs, nTypes) = (List.length eArgs, List.length argTypes) in
  if nArgs == nTypes then
    checkArgs (Utils.zip eArgs argTypes) retType
  else if nArgs < nTypes then
    let argTypes_ = List.take nArgs argTypes in
    let retType_  = tArrow (List.drop nArgs argTypes, retType) in
    checkArgs (Utils.zip eArgs argTypes_) retType_
  else
    -- TODO check syntactic structure of retType for more arrows
    finish.withError "Too many arguments to this function." typeInfo

tsAppPoly finish typeInfo typeEnv eArgs polyArrow =
  -- let (typeVars, (argTypes, retType)) = polyArrow in
  let result = synthesizeTypeMany typeInfo typeEnv eArgs in
  case Utils.projJusts result.result of

    Nothing ->
      let err = "Could not typecheck all of the arguments." in
      finish.withError err result.typeInfo

    Just tActuals ->
      let result1 = instantiatePolyArrowWithConstraintVars result.typeInfo polyArrow in
      let result2 = instantiateTypesWithConstraintVars result1.typeInfo tActuals in
      let (constraintVars1, (argTypes_, retType_)) = result1.result in
      let (constraintVars2, tActuals_) = result2.result in
      let typeInfo_ = addRawConstraints (Utils.zip argTypes_ tActuals_) result2.typeInfo in
      let constraintVars = constraintVars1 ++ constraintVars2 in
      let result3 = solveConstraintsFor typeInfo_ typeEnv constraintVars in
      case result3.result of
        Err err ->
          finish.withError err typeInfo_
        Ok unifier ->
          let retType__ = retType_ |> applyUnifier unifier in
          let (nArgs, nTypes) = (List.length eArgs, List.length argTypes_) in
          if nArgs == nTypes then
            finish.withType retType__ result3.typeInfo
          else if nArgs < nTypes then
            let remainingArgTypes_ =
              List.drop (List.length eArgs) argTypes_
                |> List.map (applyUnifier unifier)
            in
            finish.withType (tArrow (remainingArgTypes_, retType__)) result3.typeInfo
          else
            -- TODO check syntactic structure of retType for more arrows
            finish.withError "Too many arguments to this function." result3.typeInfo

instantiatePolyArrowWithConstraintVars
  : TypeInfo -> (List Ident, ArrowType) -> AndTypeInfo (List Ident, ArrowType)
instantiatePolyArrowWithConstraintVars typeInfo (typeVars, (argTypes, retType)) =
  let (constraintVars, typeInfo_) =
    generateConstraintVars (List.length typeVars) typeInfo in
  let subst = List.map2 (\x y -> (x, tVar y)) typeVars constraintVars in
  let argTypes_ = List.map (applyUnifier subst) argTypes in
  let retType_ = applyUnifier subst retType in
  { result = (constraintVars, (argTypes_, retType_)), typeInfo = typeInfo_ }

instantiateTypeWithConstraintVars : TypeInfo -> Type -> AndTypeInfo (List Ident, Type)
instantiateTypeWithConstraintVars typeInfo t =
  case stripPolymorphicArrow t of
    Nothing -> { result = ([], t), typeInfo = typeInfo }
    Just polyArrow ->
      let result = instantiatePolyArrowWithConstraintVars typeInfo polyArrow in
      let (constraintVars, arrow) = result.result in
      { result = (constraintVars, tArrow arrow), typeInfo = result.typeInfo }

instantiateTypesWithConstraintVars : TypeInfo -> List Type -> AndTypeInfo (List Ident, List Type)
instantiateTypesWithConstraintVars typeInfo ts =
  let (newVars, newTypes, newTypeInfo) =
     List.foldl (\t (acc1,acc2,acc3) ->
       let result = instantiateTypeWithConstraintVars acc3 t in
       let (newVars, t_) = result.result in
       (acc1 ++ newVars, acc2 ++ [t_], result.typeInfo)
     ) ([],[],typeInfo) ts
  in
  { result = (newVars, newTypes), typeInfo = newTypeInfo }

tsFun finish typeInfo typeEnv ps eBody (argTypes, retType) =
  case addBindings ps argTypes typeEnv of
    Err err -> finish.withError err typeInfo
    Ok typeEnv_ ->
      let result1 = synthesizeType typeInfo typeEnv_ eBody in
      case result1.result of
        Nothing ->
          finish.withError "can't synthesize type for function body" result1.typeInfo
        Just retType_ ->
          finish.withType
            (tArrow (argTypes, retType))
            (addRawConstraints [(retType, retType_)] result1.typeInfo)

tsLet finishWithType typeInfo typeEnv p e1 e2 =
  let result1 = synthesizeType typeInfo typeEnv e1 in
  case result1.result of
    Nothing ->
      if stopAtError then result1
      else
        let t1 = tVar "__NO_TYPE__" in
        tsLetFinishE2 finishWithType result1.typeInfo typeEnv p t1 (eInfoOf e1) e2

    Just t1 ->
      let result1_ =
        case isArrowTemplate t1 of
          Just arrowType -> solveTemplateArrow result1.typeInfo typeEnv (eInfoOf e1) arrowType
          Nothing        -> { result = Just t1, typeInfo = result1.typeInfo }
      in
      case result1_.result of
        Just t1_ -> tsLetFinishE2 finishWithType result1_.typeInfo typeEnv p t1_ (eInfoOf e1) e2
        Nothing  ->
          if stopAtError then result1_
          else
            let t1_ = tVar "__NO_TYPE__" in
            tsLetFinishE2 finishWithType result1_.typeInfo typeEnv p t1_ (eInfoOf e1) e2

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
    let typeInfo_ = addTypeErrorAt eInfo1.start err typeInfo in
    if stopAtError then
      { result = Nothing, typeInfo = typeInfo_ }
    else
      tsLetFinishE2_ finishWithType typeInfo_ typeEnv p t1 eInfo1.val e2

tsLetFinishE2_ finishWithType typeInfo typeEnv p t1 e1eid e2 =
  case addBindings [p] [t1] typeEnv of
    Err err ->
      { result = Nothing, typeInfo = addTypeErrorAt p.start err typeInfo }
    Ok typeEnv_ ->
      let typeInfo_ = typeInfo |> addFinalType e1eid (Just t1) |> addNamedExp p e1eid in
      let result2 = synthesizeType typeInfo_ typeEnv_ e2 in
      case result2.result of
        Nothing -> { result = Nothing, typeInfo = result2.typeInfo }
        Just t2 -> finishWithType t2 result2.typeInfo

solveTemplateArrow : TypeInfo -> TypeEnv -> EInfo -> ArrowType -> AndTypeInfo (Maybe Type)
solveTemplateArrow typeInfo typeEnv eFuncInfo arrow =
  let vars = Set.toList (constraintVarsOfArrow arrow) in
  let result = solveConstraintsFor typeInfo typeEnv vars in
  case result.result of
    Err err ->
      { result = Nothing, typeInfo = addTypeErrorAt eFuncInfo.start err typeInfo }
    Ok unifier ->
      let arrow_ = rewriteArrow unifier arrow in
      let unconstrainedVars = Set.toList (constraintVarsOfArrow arrow_) in
      let arrow =
        if unconstrainedVars == [] then
          tArrow arrow_
        else
          -- TODO pick nicer variable names: a, b, etc.
          let newTypeVars = List.map (String.dropLeft 1) unconstrainedVars in
          tPolyArrow newTypeVars <|
            rewriteArrow
              (List.map (\a -> (a, tVar (String.dropLeft 1 a))) unconstrainedVars)
              arrow_
      in
      -- let _ = debugLog "arrow after solve" (unparseType arrow) in
      { result = Just arrow
      , typeInfo = addFinalType eFuncInfo.val (Just arrow) result.typeInfo }

synthesizeBranchTypes : TypeInfo -> List (TypeEnv, Exp) -> AndTypeInfo (List (Maybe Type))
synthesizeBranchTypes typeInfo list =
  let (maybeTypes, typeInfo_) =
     List.foldl (\(typeEnvi,ei) (acc1,acc2) ->
       let resulti = synthesizeType acc2 typeEnvi ei in
       (resulti.result::acc1, resulti.typeInfo)
     ) ([], typeInfo) list
  in
  { result = List.reverse maybeTypes, typeInfo = typeInfo_ }

synthesizeTypeMany : TypeInfo -> TypeEnv -> List Exp -> AndTypeInfo (List (Maybe Type))
synthesizeTypeMany typeInfo typeEnv es =
  synthesizeBranchTypes typeInfo (List.map (\ei -> (typeEnv, ei)) es)

-- Subtype Checking ------------------------------------------- T1 <: T2; C --

type alias SubtypeResult = AndTypeInfo (Result TypeError ())

-- needs TypeEnv to expand type aliases

checkSubtype : TypeInfo -> TypeEnv -> Type -> Type -> SubtypeResult
checkSubtype typeInfo typeEnv tipe1 tipe2 =

  let errAdd msg =
    { typeInfo = typeInfo
    , result = Err <| Utils.spaces
        [ "checkSubtype failed:"
        , String.trim (unparseType tipe1), " <: "
        , String.trim (unparseType tipe2)
        , msg
        ] } in
  let err = errAdd "" in
  let ok = { typeInfo = typeInfo, result = Ok () } in
  let okConstrain =
    { result = Ok (), typeInfo = addRawConstraints [(tipe1, tipe2)] typeInfo } in

  case (tipe1.val, tipe2.val) of

    (TNum _, TNum _)       -> ok
    (TBool _, TBool _)     -> ok
    (TString _, TString _) -> ok
    (TNull _, TNull _)     -> ok

    -- take care to control unrolling of aliases...

    (TNamed _ a, TNamed _ b) ->
      if a == b then ok
      else
        let maybeNewGoal =
          case (expandTypeAlias typeEnv a, expandTypeAlias typeEnv b) of
            (Just tipe1_, Just tipe2_) -> Just (tipe1_, tipe2_)
            (Just tipe1_, Nothing)     -> Just (tipe1_, tipe2 )
            (Nothing, Just tipe2_)     -> Just (tipe1 , tipe2_)
            (Nothing, Nothing)         -> Nothing
        in
        case maybeNewGoal of
          Just (tipe1_,tipe2_) -> checkSubtype typeInfo typeEnv tipe1_ tipe2_
          Nothing              -> err

    (TNamed _ a, _) ->
      case expandTypeAlias typeEnv a of
        Just tipe1_ -> checkSubtype typeInfo typeEnv tipe1_ tipe2
        Nothing     -> err

    (_, TNamed _ b) ->
      case expandTypeAlias typeEnv b of
        Just tipe2_ -> checkSubtype typeInfo typeEnv tipe1 tipe2_
        Nothing     -> err

    (TUnion _ ts1 _, TUnion _ ts2 _) ->
      let allOk =
         List.foldl
           (\t1 -> Utils.bindMaybe (\acc -> checkSubtypeSomeRight acc typeEnv t1 ts2))
           (Just typeInfo) ts1
      in
      case allOk of
        Nothing        -> err
        Just typeInfo_ -> { result = Ok (), typeInfo = typeInfo_ }

    (TList _ t1 _, TList _ t2 _) -> checkSubtype typeInfo typeEnv t1 t2

    (TDict _ k1 v1 _, TDict _ k2 v2 _) ->
      checkEquivType typeInfo typeEnv k1 k2 |> bindSubtypeResult (\typeInfo_ ->
      checkSubtype typeInfo_ typeEnv v1 v2)

    (TTuple _ ts1 _ mt1 _, TTuple _ ts2 _ mt2 _) ->
      case Utils.maybeZip ts1 ts2 of
        Nothing -> errAdd "lengths of tuple types are not equal"
        Just list ->
          checkSubtypeList typeInfo typeEnv list |> bindSubtypeResult (\typeInfo_ ->
          checkSubMaybeType typeInfo_ typeEnv mt1 mt2)

    -- converting from tuples to lists
    (TTuple _ ts _ Nothing _, TList _ tInvariant _) ->
      let n = List.length ts in
      checkSubtypeList typeInfo typeEnv (Utils.zip ts (List.repeat n tInvariant))
    (TTuple _ ts _ (Just tRest) _, TList _ tInvariant _) ->
      case tRest.val of
        TList _ t_ _ ->
          let ts_ = ts ++ [t_] in
          let n = List.length ts_ in
          checkSubtypeList typeInfo typeEnv (Utils.zip ts_ (List.repeat n tInvariant))
        _ ->
          errAdd "the rest type of the tuple is not a list type"

    (TArrow _ arrow1 _, TArrow _ arrow2 _) ->
       let (args1, ret1) = splitTypesInArrow arrow1 in
       let (args2, ret2) = splitTypesInArrow arrow2 in
       case Utils.maybeZip args2 args1 of
         Nothing -> err
         Just contraChecks ->
           let result = checkSubtypeList typeInfo typeEnv contraChecks in
           case result.result of
             Err _ -> err
             Ok () ->
               checkSubtype result.typeInfo typeEnv ret1 ret2

    -- constrain type inference vars; equate type vars
    (TVar _ a, TVar _ b) ->
      if isConstraintVar a && isConstraintVar b then (if a == b then ok else okConstrain)
      else if isConstraintVar a then okConstrain
      else if isConstraintVar b then okConstrain
      else if a == b then ok
      else err

    -- handle all cases with one catch-all below:
    -- (but leaving TNamed cases above...)

    _ ->
      tryCatchAlls err
         [ \() -> checkSubtypeTVar tipe1 okConstrain err
         , \() -> checkSubtypeTVar tipe2 okConstrain err
         , \() -> checkSubtypeUnionRight typeInfo typeEnv tipe1 tipe2
         , \() -> checkSubtypeUnionLeft typeInfo typeEnv tipe1 tipe2
         , \() -> checkSubtypeFoldLeft typeInfo typeEnv tipe1 tipe2
         , \() -> checkSubtypeSingletonUnion typeInfo typeEnv tipe1 tipe2
         ]

tryCatchAlls err list =
  case list of
    []         -> err
    f :: list_ -> case f () of
                    Nothing -> tryCatchAlls err list_
                    Just typeInfo_ -> { result = Ok (), typeInfo = typeInfo_ }

checkSubtypeTVar t okConstrain err =
  case t.val of
    TVar _ a ->
      if isConstraintVar a then
        let result = okConstrain in
        Just result.typeInfo
      else
        Nothing
    _ -> Nothing

checkSubtypeUnionRight : TypeInfo -> TypeEnv -> Type -> Type -> Maybe TypeInfo
checkSubtypeUnionRight typeInfo typeEnv tipe1 tipe2 =
  case (tipe1.val, tipe2.val) of
    (_, TUnion _ ts _) ->
      Utils.bindMaybe Just (checkSubtypeSomeRight typeInfo typeEnv tipe1 ts)
    _ -> Nothing

checkSubtypeUnionLeft : TypeInfo -> TypeEnv -> Type -> Type -> Maybe TypeInfo
checkSubtypeUnionLeft typeInfo typeEnv tipe1 tipe2 =
  case (tipe1.val, tipe2.val) of
    (TUnion _ ts1 _, _) ->
      let obligations = Utils.zip ts1 (List.repeat (List.length ts1) tipe2) in
      let result = checkSubtypeList typeInfo typeEnv obligations in
      case result.result of
        Ok () -> Just result.typeInfo
        Err _ -> Nothing
    _ -> Nothing

checkSubtypeFoldLeft : TypeInfo -> TypeEnv -> Type -> Type -> Maybe TypeInfo
checkSubtypeFoldLeft typeInfo typeEnv tipe1 tipe2 =
  case coerceTupleToList tipe1 of
    Just (Ok tipe1_) ->
      let result = checkSubtype typeInfo typeEnv tipe1_ tipe2 in
      case result.result of
        Ok () -> Just result.typeInfo
        Err _ -> Nothing
    _ -> Nothing

checkSubtypeSingletonUnion : TypeInfo -> TypeEnv -> Type -> Type -> Maybe TypeInfo
checkSubtypeSingletonUnion typeInfo typeEnv tipe1 tipe2 =
  case tipe1.val of
    TUnion _ [t1] _ ->
      let result = checkSubtype typeInfo typeEnv t1 tipe2 in
      case result.result of
        Ok () -> Just result.typeInfo
        Err _ -> Nothing
    _ -> Nothing

checkSubtypeList : TypeInfo -> TypeEnv -> List (Type, Type) -> SubtypeResult
checkSubtypeList typeInfo typeEnv list =
  let (result, typeInfo_) =
     List.foldl
       (\(t1,t2) (accResult, accTypeInfo) ->
         case accResult of
           Err err -> (accResult, accTypeInfo)
           Ok ()   -> let nextResult = checkSubtype accTypeInfo typeEnv t1 t2 in
                      (nextResult.result, nextResult.typeInfo)
       ) (Ok (), typeInfo) list
  in
  { result = result, typeInfo = typeInfo_ }

checkSubMaybeType : TypeInfo -> TypeEnv -> Maybe Type -> Maybe Type -> SubtypeResult
checkSubMaybeType typeInfo typeEnv mt1 mt2 =
  case (mt1, mt2) of
    (Just t1, Just t2) -> checkSubtype typeInfo typeEnv t1 t2
    (Nothing, Nothing) -> { result = Ok (), typeInfo = typeInfo }
    _                  -> { result = Err "checkSubMaybeType failed...", typeInfo = typeInfo}

checkSubtypeSomeRight : TypeInfo -> TypeEnv -> Type -> List Type -> Maybe TypeInfo
checkSubtypeSomeRight typeInfo typeEnv t1 ts2 =
  List.foldl (\t2 acc ->
    case acc of
      Just _  -> acc
      Nothing ->
        let result = checkSubtype typeInfo typeEnv t1 t2 in
        case result.result of
          Ok () -> Just result.typeInfo
          Err _ -> Nothing
  ) Nothing ts2

{-
checkSubtypeSomeLeft : TypeInfo -> List Type -> Type -> Maybe TypeInfo
checkSubtypeSomeLeft typeInfo ts1 t2 =
  List.foldl (\t1 acc ->
    case acc of
      Just _  -> acc
      Nothing ->
        let result = checkSubtype typeInfo t1 t2 in
        case result.result of
          Ok () -> Just result.typeInfo
          Err _ -> Nothing
  ) Nothing ts1
-}

-- Check type equality modulo constraints.
--
checkEquivType : TypeInfo -> TypeEnv -> Type -> Type -> SubtypeResult
checkEquivType typeInfo typeEnv tipe1 tipe2 =
  (checkSubtype typeInfo  typeEnv tipe1 tipe2) |> bindSubtypeResult (\typeInfo_ ->
  checkSubtype typeInfo_ typeEnv tipe2 tipe1)

-- Check type equality without depending on any constraints.
--
checkEqualType : Type -> Type -> Bool
checkEqualType tipe1 tipe2 =
  let dummyTypeEnv = [] in
  let result = checkEquivType initTypeInfo dummyTypeEnv tipe1 tipe2 in
  case result.result of
    Err _ -> False
    Ok () -> result.typeInfo.constraints == []

-- Check subtyping without depending on any constraints.
--
checkSubtypeSimple : TypeEnv -> Type -> Type -> Bool
checkSubtypeSimple typeEnv tipe1 tipe2 =
  let result = checkSubtype initTypeInfo typeEnv tipe1 tipe2 in
  case result.result of
    Err _ -> False
    Ok () -> True

bindSubtypeResult : (TypeInfo -> SubtypeResult) -> SubtypeResult -> SubtypeResult
bindSubtypeResult f res1 =
  case res1.result of
    Err err -> { result = Err err, typeInfo = res1.typeInfo }
    Ok ()   -> f res1.typeInfo

coerceTupleToList : Type -> Maybe (Result TypeError Type)
coerceTupleToList t =
  case t.val of
    TTuple _ [] _ mtRest _ -> Nothing
    TTuple _ ts _ mtRest _ ->
      case joinManyTypes ts of
        Err err -> Just (Err err)
        Ok tJoin ->
          case mtRest of
            Nothing -> Just (Ok (tList tJoin))
            Just tRest ->
              case joinTypes (tList tJoin) tRest of
                Err err      -> Just (Err err)
                Ok tListType -> Just (Ok tListType)
    _ ->
      Nothing

{-
expandType : TypeEnv -> Type -> Maybe Type
expandType typeEnv t =
  case t.val of
    TNamed _ a -> expandTypeAlias typeEnv a
    _          -> Nothing
-}

-- Joining Types -------------------------------------------------------------

-- TODO could allow output constraints
joinTypes : Type -> Type -> Result TypeError Type
joinTypes t1 t2 =
  let dummyTypeInfo = initTypeInfo in
  let dummyTypeEnv = [] in
  case (checkSubtype dummyTypeInfo dummyTypeEnv t1 t2).result of
    Ok () -> Ok t2
    _ ->
      case (checkSubtype dummyTypeInfo dummyTypeEnv t2 t1).result of
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

    (_, TUnion _ ts _) ->
      let someEqualType =
         List.foldl (\tipe2 acc ->
           case acc of Just () -> Just ()
                       Nothing -> if checkEqualType t1 tipe2
                                    then Just ()
                                    else Nothing
         ) Nothing ts
      in
      case someEqualType of
        Just () -> Ok t2
        Nothing -> Ok (tUnion (t1::ts))

    _ ->
      -- err
      -- TODO check that both types are flat
      Ok (tUnion [t1, t2])

joinManyTypes : List Type -> Result TypeError Type
joinManyTypes ts =
  case ts of
    [] -> Debug.crash "joinManyTypes: empty list"
    t::ts_ ->
      List.foldl (\tNext acc ->
        case acc of
          Err err    -> Err err
          Ok tJoined -> joinTypes tNext tJoined
      ) (Ok t) ts_

-- Constraint Solving --------------------------------------------------------

type alias Unifier = List (Ident, Type)

strUnifier : Unifier -> String
strUnifier =
  Utils.bracks <<
    Utils.spaces <<
      List.map (\(x,t) -> x ++ "=" ++ String.trim (unparseType t))

-- needs TypeEnv to expand type aliases

solveConstraintsFor : TypeInfo -> TypeEnv -> List Ident -> AndTypeInfo (Result TypeError Unifier)
solveConstraintsFor typeInfo typeEnv vars =
  case unify typeEnv vars [] [] typeInfo.activeConstraints of
    Ok (unifier, activeConstraints_) ->
      -- let _ = Debug.log "unifer" (strUnifier unifier) in
      { result = Ok (List.reverse unifier)
      , typeInfo = { typeInfo | activeConstraints = List.reverse activeConstraints_ } }
    Err err ->
      let _ = debugLog "TODO display the constraints that failed..." () in
      { result = Err err, typeInfo = typeInfo } -- restoring original constraints

unify : TypeEnv -> List Ident -> Constraints -> Unifier -> Constraints -> Result TypeError (Unifier, Constraints)
unify typeEnv vars accActive accUnifier cs = case cs of

  [] -> Ok (accUnifier, accActive)

  one :: rest ->
    let recurse = unify typeEnv vars in
    let (id,(t1,t2)) = one in
    let err =
      Utils.spaces <|
        [ "Unification failure:"
        , String.trim (unparseType t1)
        , String.trim (unparseType t2) ] in

    if checkEqualType t1 t2 then recurse accActive accUnifier rest
    else case (t1.val, t2.val) of

      (TVar _ a, TVar _ b) ->
        if List.member a vars then
          recurse accActive ((a,t2)::accUnifier) (applyUnifierToConstraints [(a,t2)] rest)
        else if List.member b vars then
          recurse accActive ((b,t1)::accUnifier) (applyUnifierToConstraints [(b,t1)] rest)
        else
          recurse (one::accActive) accUnifier rest
      (TVar _ a, _) ->
        if List.member a vars then
          recurse accActive ((a,t2)::accUnifier) (applyUnifierToConstraints [(a,t2)] rest)
        else
          recurse (one::accActive) accUnifier rest
      (_, TVar _ b) ->
        if List.member b vars then
          recurse accActive ((b,t1)::accUnifier) (applyUnifierToConstraints [(b,t1)] rest)
        else
          recurse (one::accActive) accUnifier rest

      -- TODO ids for derived constraints

      (TList _ t1_ _, TList _ t2_ _) ->
        let induced = [(-1, (t1_, t2_))] in
        recurse accActive accUnifier (induced ++ rest)

      (TArrow _ ts1 _, TArrow _ ts2 _) ->
        case Utils.maybeZip ts1 ts2 of
          Nothing -> Err "unify TArrow: different arity"
          Just list ->
            let induced = List.map (\raw -> (-1, raw)) list in
            recurse accActive accUnifier (induced ++ rest)

      (TTuple _ ts1 _ mtRest1 _, TTuple _ ts2 _ mtRest2 _) ->
        case Utils.maybeZip ts1 ts2 of
          Nothing -> Err "unify TTuple: different arity"
{-
          Nothing ->
            let (n1, n2) = (List.length ts1, List.length ts2) in
            if n1 < n2 then
              case mtRest1 of
                Nothing -> errAdd "unify TTuple: different arity"
                Just tRest1 ->
                  let (ts2_prefix, ts2_suffix) = (List.take n1 ts2, List.drop n1 ts2) in
                  let induced = [ (-1, (t1, tTuple ts2_prefix))
                                , (-1, (tRest1, tTuple ts2_suffix)) ] in
                  recurse accActive accUnifier (induced ++ rest)
            else
              errAdd "unify TTuple: different arity"
-}
          Just list ->
            let induced = List.map (\raw -> (-1, raw)) list in
            case (mtRest1, mtRest2) of
              (Nothing, Nothing) ->
                recurse accActive accUnifier (induced ++ rest)
              (Just tRest1, Just tRest2) ->
                let induced_ = (-1, (tRest1, tRest2)) :: induced in
                recurse accActive accUnifier (induced_ ++ rest)
              _ ->
                Err "unify TTuple: rest types don't match"

      (TList _ tInvariant _, TTuple _ ts _ mtRest _) ->
        let induced = List.map (\ti -> (-1, (tInvariant, ti))) ts in
        let induced_ =
          case mtRest of
            Nothing    -> induced
            Just tRest -> (-1, (tList tInvariant, tRest)) :: induced
        in
        recurse accActive accUnifier (induced_ ++ rest)

      -- the setup of the TNamed cases is very similar to checkSubtype...

      (TNamed _ a, TNamed _ b) ->
        let maybeNewGoal =
          case (expandTypeAlias typeEnv a, expandTypeAlias typeEnv b) of
            (Just t1_, Just t2_) -> Just (t1_, t2_)
            (Just t1_, Nothing)  -> Just (t1_, t2 )
            (Nothing, Just t2_)  -> Just (t1 , t2_)
            (Nothing, Nothing)   -> Nothing
        in
        case maybeNewGoal of
          Just (t1_,t2_) -> let induced = [(-1, (t1_, t2_))] in
                            recurse accActive accUnifier (induced ++ rest)
          Nothing        -> Err err

      (TNamed _ a, _) ->
        case expandTypeAlias typeEnv a of
          Just t1_ -> let induced = [(-1, (t1_, t2))] in
                      recurse accActive accUnifier (induced ++ rest)
          Nothing  -> Err err

      (_, TNamed _ b) ->
        case expandTypeAlias typeEnv b of
          Just t2_ -> let induced = [(-1, (t1, t2_))] in
                      recurse accActive accUnifier (induced ++ rest)
          Nothing  -> Err err

      _ ->
        -- simple subtyping in unification to help with union types
        if checkSubtypeSimple typeEnv t1 t2 then recurse accActive accUnifier rest
        else if checkSubtypeSimple typeEnv t2 t1 then recurse accActive accUnifier rest
        else

        Err err

-- TODO may need to apply entire unifier left-to-right
applyUnifier : Unifier -> Type -> Type
applyUnifier unifier =
  mapType <| \t -> case t.val of
    TVar _ a ->
      case Utils.maybeFind a unifier of
        Just t_ -> t_
        Nothing -> t
    _ ->
      t

applyUnifierToConstraints : Unifier -> Constraints -> Constraints
applyUnifierToConstraints unifier =
  List.map <| Utils.mapSnd <| \(t1,t2) ->
    (applyUnifier unifier t1, applyUnifier unifier t2)

rewriteArrow : Unifier -> ArrowType -> ArrowType
rewriteArrow unifier (argTypes, retType) =
  let argTypes_ = List.map (applyUnifier unifier) argTypes in
  let retType_ = applyUnifier unifier retType in
  (argTypes_, retType_)

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
  []
{-
  let {typeInfo} = synthesizeType initTypeInfo [] Parser.prelude in
  case typeInfo.preludeTypeEnv of
    Just env -> env
    Nothing ->
      Debug.log "ERROR with preludeTypeEnv: \
                 dummyPreludeMain not found in prelude.little" []
-}

displayTypeInfo : TypeInfo -> ()
displayTypeInfo typeInfo =
  -- let _ = displayRawTypes typeInfo in
  -- let _ = displayConstraints typeInfo in
  -- let _ = displayNamedExps typeInfo in
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


dummyAceTypeInfo = { annotations = [], highlights = [], tooltips = [] }




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
