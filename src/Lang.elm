module Lang exposing (..)

import Char
import String
import Debug
import Dict exposing (Dict)
import Set
import Debug
import Regex

import Info exposing (..)
import Pos exposing (..)
import Utils
import Results exposing (Results)
import String

--------------------------------------------------------------------------------
-- Whitespace
--------------------------------------------------------------------------------

type alias WS_ = String
type alias WS = WithInfo WS_

ws : WS_ -> WS
ws =
  withDummyInfo

space0 : WS
space0 =
  ws ""

space1 : WS
space1 =
  ws " "

newline1 : WS
newline1 =
  ws "\n"

newline2 : WS
newline2 =
  ws "\n\n"

--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

type alias Loc = (LocId, Frozen, Ident)  -- "" rather than Nothing b/c comparable
type alias LocId = Int
type alias Ident = String
type alias Num = Float
  -- may want to preserve decimal point for whole floats,
  -- so that parse/unparse are inverses and for WidgetDecls

type alias Frozen = String -- b/c comparable
(frozen, unann, thawed, assignOnlyOnce) = ("!", "", "?", "~")

type alias LocSet = Set.Set Loc

type alias Pat     = WithInfo Pat_
type Exp           = Expr (WithInfo Exp_)
type alias Type    = WithInfo Type_
type alias Op      = WithInfo Op_
type alias Branch  = WithInfo Branch_
type alias TBranch = WithInfo TBranch_

--------------------------------------------------------------------------------
-- Constructing records

ctorArgs : String
ctorArgs =
  "args"

type CtorKind
  = TupleCtor
  | DataTypeCtor

ctorTuple = "$t_ctor"
ctorDataType = "$d_ctor"

stringifyCtorKind : CtorKind -> String
stringifyCtorKind ctorKind =
  case ctorKind of
    TupleCtor -> ctorTuple
    DataTypeCtor -> ctorDataType

ctorKind : String -> Maybe CtorKind
ctorKind s =
  case s of
    "$t_ctor" ->
      Just TupleCtor
    "$d_ctor" ->
      Just DataTypeCtor
    _ ->
      Nothing

recordEntriesToCtorKind : List (Maybe WS, WS, String, WS, t) -> Maybe CtorKind
recordEntriesToCtorKind entries =
  case entries of
    [] ->
      Nothing
    (_, _, key, _, _) :: rest ->
      let entryKind = ctorKind key in
      case entryKind of
        Nothing -> recordEntriesToCtorKind rest
        _ -> entryKind

-- Returns Just TupleCtor if e is a tuple, Just DataTypeCtor if it's a datatype, and Nothing otherwise
expToCtorKind : Exp -> Maybe CtorKind
expToCtorKind e =
  case unwrapExp e of
    ERecord _ _ decls _ ->
      recordEntriesFromDeclarations decls |> Maybe.andThen recordEntriesToCtorKind
    _ ->
      Nothing

ctorTupleName: Int -> String
ctorTupleName i = "Tuple" ++ toString i

ctor : (String -> t) -> CtorKind -> String -> (Maybe WS, WS, Ident, WS, t)
ctor tagger ctorKind name =
  ( Nothing, space0, stringifyCtorKind ctorKind, space0
  , tagger name
  )

ctorVal : (String -> t) -> CtorKind -> String -> (Ident, t)
ctorVal tagger ctorKind name =
  ( stringifyCtorKind ctorKind, tagger name)

argName: Int -> String
argName index = "_" ++ toString index

nameToArg: String -> Result String Int
nameToArg s = case String.uncons s of
  Just ('_', number) -> String.toInt number
  _ -> Err <| "This cannot be an argument's name for data constructors and tuples: '" ++ s ++ "'. It should be _[number]."

numericalEntry : Int -> (Maybe WS, t) -> (Maybe WS, WS, Ident, WS, t)
numericalEntry index (wsBeforeComma, binding) =
  (wsBeforeComma, space0, argName index, space0, binding)

numericalValEntry : Int -> t -> (Ident, t)
numericalValEntry index binding =
  (argName index, binding)

vTuple: List Val -> Val_
vTuple vals =
  VRecord <| Dict.fromList <| (ctorVal (builtinVal "Lang.vTuple" << VBase << VString) TupleCtor (ctorTupleName (List.length vals)))::Utils.indexedMapFrom 1 numericalValEntry vals

getDatatypeName: Val -> Maybe String
getDatatypeName v = vRecordUnapplyField ctorDataType v |> Maybe.andThen vStringUnapply

getViewDatatypeName: Val -> Maybe String
getViewDatatypeName v = case vListUnapply v of
  Just [head,attrs,children] -> 
    let isAttrCorrect = (case attrs.v_ of
         VList _ -> True
         _ -> False
       )
    in
    let isChildrenCorrect = (case children.v_ of
         VList _ -> True
         _ -> False
      )
    in
    if isAttrCorrect && isChildrenCorrect then
      vStringUnapply head
    else Nothing
  _ -> Nothing

--------------------------------------------------------------------------------


-- TODO add constant literals to patterns, and match 'svg'
type Pat__
  = PVar WS Ident WidgetDecl
  | PConst WS Num
  | PBase WS EBaseVal
  | PWildcard WS
  | PList WS (List Pat) WS (Maybe Pat) WS -- TODO store WS before commas, like EList
  | PAs WS Pat WS Pat
  | PParens WS Pat WS
  | PRecord WS {- { -}  (List (Maybe WS {- , -}, WS, Ident, WS{-=-}, Pat)) WS{- } -}
  | PColonType WS Pat WS Type

type Op_
  -- nullary ops
  = Pi
  | DictEmpty
  | CurrentEnv
  -- unary ops
  | DictFromList
  | Cos | Sin | ArcCos | ArcSin
  | Floor | Ceil | Round
  | ToStr
  | ToStrExceptStr -- Keeps strings, but use ToStr for everything else
  | Sqrt
  | Explode
  | DebugLog
  | NoWidgets
  --  | DictMem   -- TODO: add this
  -- binary ops
  | Plus | Minus | Mult | Div
  | Lt | Eq
  | Mod | Pow
  | ArcTan2
  | DictGet
  | DictRemove
  -- trinary ops
  | DictInsert
  | RegexExtractFirstIn


maybeEvalMathOp : Op_ -> List Num -> Maybe Num
maybeEvalMathOp op_ operands =
  case (op_, operands) of
    (Plus,    [l,r]) -> Just <| (+) l r
    (Minus,   [l,r]) -> Just <| (-) l r
    (Mult,    [l,r]) -> Just <| (*) l r
    (Div,     [l,r]) -> Just <| (/) l r
    (Pow,     [l,r]) -> Just <| (^) l r
    (Mod,     [l,r]) -> Just <| toFloat <| (%) (floor l) (floor r) -- might want an error/warning for non-int
    (ArcTan2, [l,r]) -> Just <| atan2 l r
    (Cos,     [n])   -> Just <| cos n
    (Sin,     [n])   -> Just <| sin n
    (ArcCos,  [n])   -> Just <| acos n
    (ArcSin,  [n])   -> Just <| asin n
    (Floor,   [n])   -> Just <| toFloat <| floor n
    (Ceil,    [n])   -> Just <| toFloat <| ceiling n
    (Round,   [n])   -> Just <| toFloat <| round n
    (Sqrt,    [n])   -> Just <| sqrt n
    (Pi,      [])    -> Just <| pi
    _                -> Nothing

type alias Operator =
  (WS, Ident)

-- type alias Thing == Exp or Pat (or Type, soon)
--
-- Ideally, these three types would have the same field name for IDs.

type alias EId  = Int
type alias PId  = Int
type alias TId = Int
type alias Exp_ = WithTypeInfo { e__ : Exp__, eid : EId }
type alias Pat_ = WithTypeInfo { p__ : Pat__, pid : PId }
type alias Type_ = { t__ : Type__, tid: TId }

type alias WithTypeInfo a =
  { a | typ : Maybe Type
      , typeError : Maybe TypeError
      , extraTypeInfo : Maybe ExtraTypeInfo
  }

-- because Exp_ is defined via WithTypeInfo, no constructor called Exp_
makeExp_ e__ eid =
  { e__ = e__
  , eid = eid
  , typ = Nothing
  , typeError = Nothing
  , extraTypeInfo = Nothing
  }

makePat_ p__ pid =
  { p__ = p__
  , pid = pid
  , typ = Nothing
  , typeError = Nothing
  , extraTypeInfo = Nothing
  }

makeType_ t__ tid =
  { t__ = t__
  , tid = tid
  }

setTypeForThing : Maybe Type -> WithInfo (WithTypeInfo a) -> WithInfo (WithTypeInfo a)
setTypeForThing typ thing =
  let thing_ = thing.val in
  { thing | val = { thing_ | typ = typ } }

setTypeErrorForThing : TypeError -> WithInfo (WithTypeInfo a) -> WithInfo (WithTypeInfo a)
setTypeErrorForThing error thing =
  let thing_ = thing.val in
  { thing | val = { thing_ | typeError = Just error } }

setExtraTypeInfoForThing : ExtraTypeInfo -> WithInfo (WithTypeInfo a) -> WithInfo (WithTypeInfo a)
setExtraTypeInfoForThing info thing =
  let thing_ = thing.val in
  { thing | val = { thing_ | extraTypeInfo = Just info } }

setType : Maybe Type -> Exp -> Exp
setType typ =
  unExpr >> setTypeForThing typ >> Expr

setTypeError : TypeError -> Exp -> Exp
setTypeError error =
  unExpr >> setTypeErrorForThing error >> Expr

setExtraTypeInfo : ExtraTypeInfo -> Exp -> Exp
setExtraTypeInfo info =
  unExpr >> setExtraTypeInfoForThing info >> Expr

setPatType : Maybe Type -> Pat -> Pat
setPatType = setTypeForThing

setPatTypeError : TypeError -> Pat -> Pat
setPatTypeError = setTypeErrorForThing

setPatExtraTypeInfo : ExtraTypeInfo -> Pat -> Pat
setPatExtraTypeInfo = setExtraTypeInfoForThing


--------------------------------------------------------------------------------

type alias DataConstructor = (WS, Ident, List Type, WS)

--------------------------------------------------------------------------------
-- The following expressions count as "top-level":
--   * definition (def, not let)
--   * comment
--   * option
--   * type declaration
--   * type alias
--   * type definition
--------------------------------------------------------------------------------

-- SpaceApp: fun arg
-- LeftApp: fun <| arg
-- RightApp: arg |> fun
-- InfixApp: arg1 fun arg2
type ApplicationType = SpaceApp | LeftApp WS | RightApp WS | InfixApp

-- TODO once Little syntax is deprecated, remove some unused WS args

-- 'aux = \a -> b' and 'aux a = b'
-- are parsed the same way but the first one is EFunArgsAfterEqual, the second EFunArgAsPats
type FunArgStyle = FunArgAsPats | FunArgsAfterEqual

type alias OptCommaSpace = Maybe WS
type alias AliasSpace = Maybe WS

type LetExp_ t      = LetExp        OptCommaSpace WS Pat FunArgStyle WS{-before = -} t {- usual exp. -}
type alias LetExp   = LetExp_ Exp
type LetType        = LetType       OptCommaSpace WS AliasSpace Pat FunArgStyle WS Type {- prefixed with ‘  type’. If the maybe is Just Ws, then it's an alias) -}
type LetAnnotation  = LetAnnotation OptCommaSpace WS Pat FunArgStyle WS{-before : -} Type

-- The unparsing order give successive indexes on which elements to unparse.
-- The groupingInfo gives the number of mutually definitions to evaluate at once.
-- (Mutually) recursive defintiions are constrained to be syntactic lambdas.
type alias PrintOrder         = List Int
type alias IsRec = Bool
type alias GroupsOf a = List (IsRec, List a)
type alias LetTypes       = GroupsOf LetType  -- I'd love to write it "List LetType |> GroupsOf"
type alias LetAnnotations = List LetAnnotation
type alias LetExps t      = GroupsOf (LetExp_ t)

type Declarations_ t = Declarations PrintOrder LetTypes LetAnnotations (LetExps t)
type alias Declarations = Declarations_ Exp
type Declaration_ t = DeclExp (LetExp_ t) | DeclType LetType | DeclAnnotation LetAnnotation
type alias Declaration = Declaration_ Exp

type ExpBuilder__ t1 t2
  = EConst WS Num Loc WidgetDecl
  | EBase WS EBaseVal
  | EVar WS Ident
  | EFun WS (List Pat) t2 WS -- WS: before (, before )
  -- TODO remember paren whitespace for multiple pats, like TForall
  | EApp WS t1 (List t1) ApplicationType WS
  | EOp WS WS Op (List t1) WS
  | EList WS (List (WS{-,-}, t1)) WS (Maybe t1) WS -- the first WS{-,-} is a dummy
  | EIf WS t1 WS{-then-} t2 WS{-else-} t2 WS{-REMOVE-}
  | ECase WS t1 (List Branch) WS
  | ELet WS LetKind (Declarations_ t2) WS{-in or ;-} t2
  | EColonType WS t2 WS Type WS -- type annotation
  | EParens WS t2 ParensStyle WS
  | EHole WS Hole
  | ERecord WS {- { -} (Maybe (t1, WS) {- | -}) (Declarations_ t1) WS {- }-}
  | ESelect WS t1 WS {-.-} WS Ident
    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))

    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type alias Exp__ = ExpBuilder__ Exp Exp

expEId : Exp -> EId
expEId (Expr e) = e.val.eid

unExpr : Exp -> WithInfo Exp_
unExpr (Expr e) = e

unwrapExp : Exp -> Exp__
unwrapExp (Expr e) = e.val.e__

type ParensStyle = Parens | LongStringSyntax | LeoSyntax | HtmlSyntax

type Hole
  = EEmptyHole
  | ESnapHole Val

type Type__
  = TNum WS
  | TBool WS
  | TString WS
  | TNull WS -- TODO: remove
  | TList WS Type WS
  | TDict WS Type Type WS
  | TRecord WS (Maybe (Ident, WS {- | -})) (List (Maybe WS, {- , -} WS, Ident, WS{-:-}, Type)) WS {- }-}
  | TTuple WS (List Type) WS (Maybe Type) WS -- TODO: keep using only the encoding? if so, remove?
  | TArrow WS (List Type) WS -- not used in the new LeoParser. Use infix TApp instead; TODO: keep using only the encoding? if so, remove?
  | TUnion WS (List Type) WS -- not used in the new LeoParser. Use infix TApp instead; TODO: keep using only the encoding? if so, remove?
  | TApp WS Type (List Type) ApplicationType
  | TVar WS Ident
  | TForall WS (List TPat) Type WS -- TODO this is not being used; remove?
  | TParens WS Type WS
  | TWildcard WS

dummyType0 =
  withDummyTypeInfo (TWildcard space0)
dummyType wsb =
  withDummyTypeInfo (TWildcard wsb)

-- Currently shoving entire type error message and suggested fixes into Deuce.
-- So every line is a DeuceTypeInfoItem === SynthesisResult.
--
-- TODO: Any benefit of explicit TypeError datatype? Could just do
--   type alias TypeError = SynthesisResult (for now)
--
-- TODO: Update Deuce UI with API for "Functional Types" info and errors.
--
type alias DeuceTypeInfoItem = SynthesisResult

type TypeError
  = ExpectedButGot Type (Maybe Type)
  | VarNotFound Ident (List DeuceTypeInfoItem)
  | OtherTypeError (List String)

-- Information for an expression that is relevant to
-- other expressions that have above TypeErrors
type ExtraTypeInfo
  = HighlightWhenSelected EId

type alias TPat = WithInfo TPat_
type TPat_ = TPatVar WS Ident

-- TODO : Get rid of this once we merge TPat and Pat
patToTPat pat = case pat.val.p__ of
  PVar ws name _ -> Just <| replaceInfo pat <| TPatVar ws name
  _ -> Nothing

type Branch_  = Branch_ WS Pat Exp WS
type TBranch_ = TBranch_ WS Type Exp WS

type LetKind = Let | Def
type alias Rec = Bool

type alias WidgetDecl = WithInfo WidgetDecl_

-- Bool is True for hidden widget
type WidgetDecl_
  = IntSlider (WithInfo Int) Token (WithInfo Int) Caption Bool
  | NumSlider (WithInfo Num) Token (WithInfo Num) Caption Bool
  | NoWidgetDecl -- rather than Nothing, to work around parser types

type Axis = X | Y
type Sign = Positive | Negative

-- Vals are for provenance
-- Need actual vals because they are mutably tagged with parents.
type Widget
  = WIntSlider Int Int String Int Val Loc Bool
  | WNumSlider Num Num String Num Val Loc Bool
  | WPoint NumTr Val NumTr Val
  | WOffset1D NumTr NumTr Axis Sign NumTr Val Val Val -- baseXNumTr baseYNumTr axis sign amountNumTr amountProvenance endXProvenance endYProvenance
  | WCall Val (List Val) Val Widgets -- funcVal argVals retVal retWs

type alias Widgets = List Widget

offsetWidget1DEffectiveAmountAndEndPoint ((baseX, baseXTr), (baseY, baseYTr)) axis sign (amount, amountTr) =
  let (effectiveAmount, op) =
    case sign of
       Positive -> (amount, Plus)
       Negative -> (-amount, Minus)
  in
  let ((endX, endXTr), (endY, endYTr)) =
    case axis of
       X -> ((baseX + effectiveAmount, TrOp op [baseXTr, amountTr]), (baseY, baseYTr))
       Y -> ((baseX, baseXTr), (baseY + effectiveAmount, TrOp op [baseYTr, amountTr]))
  in
  (effectiveAmount, ((endX, endXTr), (endY, endYTr)))

type alias Token = WithInfo String

type alias Caption = Maybe (WithInfo String)

type alias Val = { v_ : Val_, provenance : Provenance, parents : Parents }

valParents val = let (Parents parents) = val.parents in parents

type Parents = Parents (List Val) -- Avoiding recursive type alias :(

type Val_
  = VConst (Maybe (Axis, NumTr, Val)) NumTr -- Maybe (Axis, value in other dimension, Val with provenance in other dimension)
  | VBase VBaseVal
  | VClosure (List Ident) (List Pat) Exp Env
  | VList (List Val)
  | VRecord (Dict String Val) -- It's a record indexed by the keys
  | VDict VDict_ -- Can be constructed only by "dict [[key, value], [key2, value2]] and 'empty'
  | VFun String -- Name
         (List String) -- Name of arguments
         (List Val -> Result String (Val, Widgets)) -- Evaluation rule
         (Maybe (List Val -> Val -> Val -> VDiffs -> Results String (List Val, TupleDiffs VDiffs))) -- Maybe Update rule

type alias VDict_ = Dict (String, String) Val -- First key string is unparsed key, the second type is the value. See Eval.valToDictKey

type alias NumTr = (Num, Trace)

defaultQuoteChar = "\""
type alias QuoteChar = String

-- TODO combine all base exps/vals into PBase/EBase/VBase
type VBaseVal -- unlike Ints, these cannot be changed by Sync
  = VBool Bool
  | VString String
  | VNull

type EBaseVal
  = EBool Bool
  | EString QuoteChar String
  | ENull

eBaseValsEqual ebv1 ebv2 =
  case (ebv1, ebv2) of
    (EBool b1,       EBool b2)       -> b1 == b2
    (EString _ str1, EString _ str2) -> str1 == str2
    (ENull,          ENull)          -> True
    _                                -> False

type Trace = TrLoc Loc | TrOp Op_ (List Trace)

endPosLetExp: LetExp -> Pos
endPosLetExp def = case def of
  LetExp _ _ _ _ _ (Expr e1) -> e1.end

patOfLetExp: LetExp -> Pat
patOfLetExp (LetExp _ _ p _ _ _) = p

bindingOfLetExp: LetExp -> Exp
bindingOfLetExp (LetExp _ _ _ _ _ e1) = e1

allTraceLocs : Trace -> List Loc
allTraceLocs trace =
  case trace of
    TrLoc loc     -> [loc]
    TrOp _ traces -> List.concatMap allTraceLocs traces

type Provenance = Provenance Env Exp (List Val) -- Env, Exp, Vals immediately used to calculate this; "basedOn" provenance, like traces, ignores control flow.

provenanceEnv     (Provenance env exp basedOn) = env
provenanceExp     (Provenance env exp basedOn) = exp
provenanceBasedOn (Provenance env exp basedOn) = basedOn

-- If using these, you may also want expEffectiveExp and friends.
valExp : Val -> Exp
valExp val = val.provenance |> provenanceExp

valEId : Val -> EId
valEId val = valExp val |> expEId

type alias Env = List (Ident, Val)
type alias Backtrace = List Exp


--------------------------------------------------------------------------------
-- Synthesis Results (for DeuceTools and OutputTools)
--------------------------------------------------------------------------------

type SynthesisResult =
  SynthesisResult { description : String
                  , exp         : Exp
                  , diffs       : List Exp -- These exps can be virtual, only the start and end position matter
                  , isSafe      : Bool -- Is this transformation considered "safe"?
                  , sortKey     : List Float -- For custom sorting criteria. Sorts ascending.
                  , children    : Maybe (List SynthesisResult) -- Nothing means not calculated yet.
                  }

synthesisResult description exp =
  SynthesisResult <|
    { description = description
    , exp         = exp
    , diffs       = []
    , isSafe      = True
    , sortKey     = []
    , children    = Nothing
    }


--------------------------------------------------------------------------------
-- Predicates (for DeuceTools and OutputTools)
--------------------------------------------------------------------------------

type PredicateValue
    -- Good to go, and can accept no more arguments
  = FullySatisfied
    -- Good to go, but can accept more arguments if necessary
  | Satisfied
    -- Not yet good to go, but with more arguments may be okay
  | Possible
    -- Not good to go, and no additional arguments will make a difference
  | Impossible

-- NOTE: Descriptions should be an *action* in sentence case with no period at
--       the end, e.g.:
--         * Select a boolean value
--         * Select 4 integers
type alias Predicate =
  { description : String
  , value : PredicateValue
  }

predicateFullySatisfied : Predicate -> Bool
predicateFullySatisfied pred =
  case pred.value of
    FullySatisfied ->
      True
    Satisfied ->
      False
    Possible ->
      False
    Impossible ->
      False

predicateSatisfied : Predicate -> Bool
predicateSatisfied pred =
  case pred.value of
    FullySatisfied ->
      True
    Satisfied ->
      True
    Possible ->
      False
    Impossible ->
      False

predicatePossible : Predicate -> Bool
predicatePossible pred =
  case pred.value of
    FullySatisfied ->
      True
    Satisfied ->
      True
    Possible ->
      True
    Impossible ->
      False

predicateImpossible : Predicate -> Bool
predicateImpossible pred =
  case pred.value of
    FullySatisfied ->
      False
    Satisfied ->
      False
    Possible ->
      False
    Impossible ->
      True


--------------------------------------------------------------------------------
-- Deuce Selections and Tools
--------------------------------------------------------------------------------

type alias DeuceSelections =
  ( List (LocId, (WS, Num, Loc, WidgetDecl))  -- number literals
  , List (EId, (WS, EBaseVal))                -- other base value literals
  , List EId                                  -- expressions (including literals)
  , List PathedPatternId                      -- patterns
  , List TId                                  -- types
  , List (EId, BindingNumber)                 -- binding or declarations
  , List DeclarationTargetPosition            -- declaration target positions
  , List ExpTargetPosition                    -- expression target positions
  , List PatTargetPosition                    -- pattern target positions
  )

type DeuceTransformation
  = InactiveDeuceTransform
  | NoInputDeuceTransform (() -> List SynthesisResult)
  | RenameDeuceTransform (String -> List SynthesisResult)
  | SmartCompleteDeuceTransform (String -> List SynthesisResult)

type alias DeuceTool =
  { name : String
  , func : DeuceTransformation -- synthesizes several results of ways to change the code
  , reqs : List Predicate -- requirements to run the tool
  , id : String -- unique, unchanging identifier
  }


------------------------------------------------------------------------------

-- We currently have two forms of pattern ids.
--
-- PathedPatternIds are older and reference a pattern by its scopeId and a
-- path to the pattern (a list of children indices to follow, 1-based). These
-- are somewhat useful for inserting (specify the path at which to insert) and
-- for reordering function arguments (can find the corresponding argument
-- location at a callsite by following the pattern path through the callsite
-- argument expressions).
--
-- PIds are newer and  reference a pattern by a singular number rather than
-- PathedPatternId's ((scopeEId, branchI), path). PIds are useful not only
-- because they are simpler but because a pattern that is moved around the
-- program will retain its same PId. Checking if we accidently captured or
-- freed any variables is as simple as checking that all variables references
-- point to the same PIds as before.
--
-- Because PathedPatternIds are older, there are many places in the code that
-- might benefit from using PIds instead. However, PathedPatternIds are probably
-- still a better fit in a few places (mentioned above).

-- ELet/EFun/ECase.
-- Int is an index indicating
-- * the branch number for ECase (1-based index),
-- * the declaration number for ELet (0-based index),
-- * the pattern number for EFun (1-based index)
type alias ScopeId = (EId, Int)

-- The List Int is how to walk the pattern to reach the pattern target's position
type alias PathedPatternId = (ScopeId, List Int)

type BeforeAfter = Before | After

type InsertionMethod =
    InsertDeclarationLevel BeforeAfter
  | InsertPatternLevel BeforeAfter (List Int {-path to pattern-})

type alias PatTargetPosition = (BeforeAfter, PathedPatternId, PId)

type alias ExpTargetPosition = (BeforeAfter, EId)

type alias DeclarationTargetPosition = (BeforeAfter, (EId, BindingNumber))

type TargetPosition
  = ExpTargetPosition ExpTargetPosition
  | PatTargetPosition PatTargetPosition
  | DeclarationTargetPosition DeclarationTargetPosition


scopeIdToScopeEId : ScopeId -> EId
scopeIdToScopeEId (scopeEId, _) = scopeEId

pathedPatIdToScopeId : PathedPatternId -> ScopeId
pathedPatIdToScopeId (scopeId, _) = scopeId

pathedPatIdToPath : PathedPatternId -> List Int
pathedPatIdToPath (_, path) = path

pathedPatIdToScopeEId : PathedPatternId -> EId
pathedPatIdToScopeEId pathedPatId =
  pathedPatId |> pathedPatIdToScopeId |> scopeIdToScopeEId

-- Increment last path index by 1
pathedPatIdRightSibling : PathedPatternId -> Maybe PathedPatternId
pathedPatIdRightSibling (scopeId, path) =
  pathRightSibling path
  |> Maybe.map (\newPath -> (scopeId, newPath))

pathRightSibling : List Int -> Maybe (List Int)
pathRightSibling path =
  Utils.maybeMapLast ((+) 1) path

pathAfterElementRemoved : List Int -> List Int -> Maybe (List Int)
pathAfterElementRemoved removedPath path =
  case (removedPath, path) of
    ([removedI], [pathI]) ->
      if removedI < pathI
      then Just [pathI - 1]
      else Just [pathI]

    ([removedI], pathI::ps) ->
      if removedI == pathI then
        let _ = Debug.log ("removed pat path is supertree of target path " ++ toString (removedPath, path)) in
        Nothing
      else if removedI < pathI then
        Just ((pathI - 1)::ps)
      else
        Just path

    (removedI::rs, pathI::ps) ->
      if removedI == pathI then
        pathAfterElementRemoved rs ps |> Maybe.map ((::) pathI)
      else
        Just path -- Removed element is in a different subtree

    (_::_, []) ->
        Just path -- Removed element is in a different subtree

    ([], _::_) ->
      let _ = Debug.log ("removed pat path is supertree of target path " ++ toString (removedPath, path)) in
      Nothing

    ([], []) ->
      -- This should be caught by the very first case.
      -- Unless the initial call was with paths ([], []). But why would that happen?
      Debug.crash <| "Lang.pathAfterElementRemoved why did this get called?!" ++ toString (removedPath, path)

patTargetPositionToTargetPathedPatId : PatTargetPosition -> PathedPatternId
patTargetPositionToTargetPathedPatId (beforeAfter, referencePathedPatId, _) =
  let
    (referenceScopeId, referencePath) =
      referencePathedPatId
    targetPath =
      let
        referencePathAsPList =
          case referencePath of
            [] -> [1]
            _  -> referencePath
      in
        case beforeAfter of
          Before -> referencePathAsPList
          After  ->
            pathRightSibling referencePathAsPList
            |> Utils.fromJust_ ("invalid target pattern id path of [] in target path position: " ++ toString (beforeAfter, referencePathedPatId))
  in
    (referenceScopeId, targetPath)

type alias AccAux a = a -> a

bindLetExpsToLetAnnotation: Declarations -> (List (IsRec, List (LetExp, List LetAnnotation)), List (LetAnnotation, Int))
bindLetExpsToLetAnnotation (Declarations printOrder types anns exps as decls) =
  -- Although exps might have been reordered, if they have the same identifier,
  -- they will be in the same order.
  -- Annotations have the same order.
  -- All annotations having the same name refer to all identifiers having this name.
  -- We also return the list of annotations which do not annotate anything.
  let annotationsWithPats = anns |>
        List.concatMap (\(LetAnnotation _ _ p _ _ _ as letann) ->
           identifiersListInPat p |> List.map (\id ->
             (id, letann)
           )
        )
      annotationsForId: Ident -> List LetAnnotation
      annotationsForId id = List.filterMap (\(i, letann) -> if i == id then Just letann else Nothing) annotationsWithPats
      uselessAnnotations = Utils.zipWithIndex anns |>
        List.filter (\(LetAnnotation _ _ p _ _ _ as letann, i) ->
            (identifiersListInPat p |> List.all (\id ->
              exps |> List.all (\(_, group) -> List.all (\(LetExp _ _ p2 _ _ _) ->
                not <| List.member id (identifiersListInPat p2)
              ) group)
            ))
        )
      result =
        exps |> (List.map <| Tuple.mapSecond <| List.map <| \(LetExp _ _ p _ _ _ as letexp) ->
          identifiersListInPat p |>
          List.concatMap annotationsForId |>
          (,) letexp
        )
  in
  (result, uselessAnnotations)

getDeclarationsInOrder: Declarations -> List Declaration
getDeclarationsInOrder decls =
  getDeclarationsInOrderWithIndex decls |> List.unzip |> Tuple.first

-- The index is the original on in the list Types ++ Annotations ++ LetExps.
getDeclarationsInOrderWithIndex: Declarations -> List (Declaration, Int)
getDeclarationsInOrderWithIndex (Declarations unparsingOrder _ _ _  as decls) =
  getDeclarations decls |> Utils.zipWithIndex |> Utils.reorder unparsingOrder

getDeclarations: Declarations -> List Declaration
getDeclarations (Declarations po types annotations exps) =
  if List.isEmpty po then List.map DeclExp (elemsOf exps) else
  List.map DeclType (elemsOf types) ++ List.map DeclAnnotation annotations ++ List.map DeclExp (elemsOf exps)

getDeclarationsExtractors: Declarations -> (List Declaration, List Declaration -> Declarations)
getDeclarationsExtractors (Declarations unparsingOrder types annotations letexps as decls) =
  (getDeclarations decls, \newDeclarations ->
    let newTypes = List.filterMap (\def -> case def of
      DeclType x -> Just x
      _ -> Nothing) newDeclarations
    in
    let newAnnotations = List.filterMap (\def -> case def of
          DeclAnnotation x -> Just x
          _ -> Nothing) newDeclarations
    in
    let newExps = List.filterMap (\def -> case def of
          DeclExp x -> Just x
          _ -> Nothing) newDeclarations
    in
    Declarations unparsingOrder (regroup types newTypes) newAnnotations (regroup letexps newExps)
  )

letExpOf: Declaration -> Maybe LetExp
letExpOf decl = case decl of
  DeclExp x -> Just x
  _ -> Nothing

-- CAREFUL: This is non-breaking space (used in LangSVG.printHTML and also removed from parsing in THMLValParser)
tab k = String.repeat k "  "

-- TODO take into account indent and other prefix of current line
fitsOnLine s =
  if String.length s > 70 then False
  else if List.member '\n' (String.toList s) then False
  else True

isLet e = case (unwrapExp e) of
  ELet _ _ _ _ _ -> True
  _                -> False

isList e = case (unwrapExp e) of
  EList _ _ _ _ _ -> True
  _               -> False

isSingletonList e = case (unwrapExp e) of
  EList _ [_] _ Nothing _ -> True
  _                       -> False

isPair e = case (unwrapExp e) of
  EList _ [_, _] _ Nothing _ -> True
  _                          -> False

isVar e = case (unwrapExp e) of
  EVar _ _ -> True
  _        -> False

isFunc e = case (unwrapExp e) of
  EFun _ _ _ _ -> True
  _            -> False

isPVar p = case p.val.p__ of
  PVar _ _ _ -> True
  _          -> False

-- What exp actually determines the evaluated value of this exp?
-- See LangTools.outerSameValueExp for expanding outward.
expEffectiveExp : Exp -> Exp
expEffectiveExp exp =
  expEffectiveExps exp
  |> Utils.last "expEffectiveExp shouldn't happen"

-- What expressions will surely resolve to the same value?
expEffectiveExps : Exp -> List Exp
expEffectiveExps exp =
  case (unwrapExp exp) of
    EColonType _ body _ _ _   -> exp :: expEffectiveExps body
    ELet _ _ _ _ body         -> exp :: expEffectiveExps body
    EParens _ e _ _           -> exp :: expEffectiveExps e
    EOp _ _ {val} [operand] _ -> if val == DebugLog || val == NoWidgets then exp :: expEffectiveExps operand else [exp]
    _                         -> [exp]

-- Skip through PParens
patEffectivePat : Pat -> Pat
patEffectivePat pat =
  patEffectivePats pat
  |> Utils.last "patEffectivePat shouldn't happen"

patEffectivePats : Pat -> List Pat
patEffectivePats pat =
  case pat.val.p__ of
    PParens _ innerPat _ -> pat :: patEffectivePats innerPat
    _                    -> [pat]

------------------------------------------------------------------------------
-- Mapping WithInfo/WithPos

mapValField f r = { r | val = f r.val }


------------------------------------------------------------------------------
-- Mapping

-- Children visited/replaced first. (Post-order traversal.)
--
-- Note that visit order matters for some code (clone detection: leaf visit order matters to determine argument order).
--
-- Children are visited in right-to-left order (opposite of order returned by the childExps function).
-- This lets you use mapFoldExp as a foldr: a simple cons in the accumulator will result in nodes in left-to-right order.
mapFoldExp : (Exp -> a -> (Exp, a)) -> a -> Exp -> (Exp, a)
mapFoldExp f initAcc e =
  let
    (children, rebuilder) = childExpsExtractors e
    (newChildren, accAfterChildren) =
     Utils.foldLeft ([], initAcc) (List.reverse children) <|
     \(accChildren, acc) child ->
       let (newChild, newAcc) = mapFoldExp f acc child in
       (newChild::accChildren, newAcc)
    newE = rebuilder newChildren
  in f newE accAfterChildren

-- Children visited/replaced first. (Post-order traversal.)
--
-- Children are visited in right-to-left order (opposite of order returned by the childPats function).
mapFoldPat : (Pat -> a -> (Pat, a)) -> a -> Pat -> (Pat, a)
mapFoldPat f initAcc p =
  let recurse = mapFoldPat f in
  let wrap p__ = replaceP__ p p__ in
  let wrapAndMap = f << wrap in
  -- Make sure pats are left-to-right so they are visited right-to-left.
  let recurseAll initAcc pats =
    pats
    |> List.foldr
        (\pat (newPats, acc) ->
          let (newPat, newAcc) = recurse acc pat in
          (newPat::newPats, newAcc)
        )
        ([], initAcc)
  in
  case p.val.p__ of
    PVar ws ident wd -> f p initAcc
    PConst ws num    -> f p initAcc
    PBase ws ebv     -> f p initAcc
    PWildcard ws     -> f p initAcc

    PList ws1 ps ws2 Nothing ws3 ->
      let (newPs, newAcc) = recurseAll initAcc ps in
      wrapAndMap (PList ws1 newPs ws2 Nothing ws3) newAcc

    PList ws1 ps ws2 (Just pTail) ws3 ->
      let (newPTail, newAcc) = recurse initAcc pTail in
      let (newPs, newAcc2)   = recurseAll newAcc ps in
      wrapAndMap (PList ws1 newPs ws2 (Just newPTail) ws3) newAcc2

    PRecord ws1 ps ws2 ->
      let (newPs, newAcc) = recurseAll initAcc (Utils.recordValues ps) in
      wrapAndMap (PRecord ws1 (Utils.recordValuesMake ps newPs) ws2) newAcc

    PAs ws1 pChild1 ws2 pChild2 ->
      let (newPChild2, newAcc) = recurse initAcc pChild2 in
      let (newPChild1, newAcc2) = recurse newAcc pChild1 in
      wrapAndMap (PAs ws1 newPChild1 ws2 newPChild2) newAcc2

    PParens ws1 pChild ws2 ->
      let (newPChild, newAcc) = recurse initAcc pChild in
      wrapAndMap (PParens ws1 newPChild ws2) newAcc

    PColonType ws1 pChild ws2 tp ->
      let (newPChild, newAcc) = recurse initAcc pChild in
      wrapAndMap (PColonType ws1 newPChild ws2 tp) newAcc

-- Nodes visited/replaced in top-down, left-to-right order.
-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapFoldExpTopDown : (Exp -> a -> (Exp, a)) -> a -> Exp -> (Exp, a)
mapFoldExpTopDown f initAcc e =
  mapFoldExpTopDownWithScope (\e a b ->
    let (f1, f2) = f e a in (f1, f2, b)) (\e r g bn a b -> (a, b)) (\e b -> b) (\e br i b -> b) initAcc () e

-- Nodes visited/replaced in top-down, left-to-right order.
-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapFoldPatTopDown : (Pat -> a -> (Pat, a)) -> a -> Pat -> (Pat, a)
mapFoldPatTopDown f initAcc p =
  let (newP, newAcc) = f p initAcc in
  let ret p__ acc =
    (replaceP__ newP p__, acc)
  in
  let recurse acc child =
    mapFoldPatTopDown f acc child
  in
  let recurseAll acc pats =
    pats
    |> List.foldl
        (\pat (newPats, acc) ->
          let (newPat, newAcc) = recurse acc pat in
          (newPats ++ [newPat], newAcc)
        )
        ([], acc)
  in
  case newP.val.p__ of
    PVar ws ident wd -> (newP, newAcc)
    PConst ws num    -> (newP, newAcc)
    PBase ws ebv     -> (newP, newAcc)
    PWildcard ws     -> (newP, newAcc)

    PList ws1 ps ws2 Nothing ws3 ->
      let (newPs, newAcc2) = recurseAll newAcc ps in
      ret (PList ws1 newPs ws2 Nothing ws3) newAcc2

    PList ws1 ps ws2 (Just pTail) ws3 ->
      let (newPs, newAcc2)    = recurseAll newAcc ps in
      let (newPTail, newAcc3) = recurse newAcc2 pTail in
      ret (PList ws1 newPs ws2 (Just newPTail) ws3) newAcc3

    PRecord ws1 ps ws2 ->
      let (newPsvalues, newAcc2)    = recurseAll newAcc (Utils.recordValues ps) in
      ret (PRecord ws1 (Utils.recordValuesMake ps newPsvalues) ws2) newAcc2

    PAs ws1 pChild1 ws2 pChild2 ->
      let (newPChild1, newAcc2) = recurse newAcc pChild1 in
      let (newPChild2, newAcc3) = recurse newAcc2 pChild2 in
      ret (PAs ws1 newPChild1 ws2 newPChild2) newAcc3

    PParens ws1 pChild ws2 ->
      let (newPChild, newAcc2) = recurse newAcc pChild in
      ret (PParens ws1 newPChild ws2) newAcc2

    PColonType ws1 pChild ws2 tp ->
      let (newPChild, newAcc2) = recurse newAcc pChild in
      ret (PColonType ws1 newPChild ws2 tp) newAcc2

-- Nodes visited/replaced in top-down, left-to-right order.
-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapFoldTypeTopDown : (Type -> a -> (Type, a)) -> a -> Type -> (Type, a)
mapFoldTypeTopDown f initAcc t =
  let (newT, newAcc) = f t initAcc in
  let ret t__ acc =
    (replaceT__ newT t__, acc)
  in
  let recurse acc child =
    mapFoldTypeTopDown f acc child
  in
  let recurseAll acc typs =
    typs
    |> List.foldl
        (\typ (newTyps, acc) ->
          let (newTyp, newAcc) = recurse acc typ in
          (newTyps ++ [newTyp], newAcc)
        )
        ([], acc)
  in
  case newT.val.t__ of
    TNum _ ->
      (newT, newAcc)
    TBool _ ->
      (newT, newAcc)
    TString _ ->
      (newT, newAcc)
    TNull _ ->
      (newT, newAcc)
    TList ws1 tChild1 ws2 ->
      let (newTChild1, newAcc2) = recurse newAcc tChild1 in
      ret (TList ws1 newTChild1 ws2) newAcc2
    TDict ws1 tChild1 tChild2 ws2 ->
      let (newTChild1, newAcc2) = recurse newAcc tChild1 in
      let (newTChild2, newAcc3) = recurse newAcc2 tChild2 in
      ret (TDict ws1 newTChild1 newTChild2 ws2) newAcc3
    TRecord ws1 head entries ws2  ->
      let (newTsvalues, newAcc2) = recurseAll newAcc (Utils.recordValues entries) in
      ret (TRecord ws1 head (Utils.recordValuesMake entries newTsvalues) ws2) newAcc2
    TTuple ws1 ts ws2 Nothing ws3 ->
      let (newTs, newAcc2) = recurseAll newAcc ts in
      ret (TTuple ws1 newTs ws2 Nothing ws3) newAcc2
    TTuple ws1 ts ws2 (Just tTail) ws3 ->
      let (newTs, newAcc2)  = recurseAll newAcc ts in
      let (newTTail, newAcc3) = recurse newAcc2 tTail in
      ret (TTuple ws1 newTs ws2 (Just newTTail) ws3) newAcc3
    TArrow ws1 ts ws2 ->
      let (newTs, newAcc2) = recurseAll newAcc ts in
      ret (TArrow ws1 newTs ws2) newAcc2
    TUnion ws1 ts ws2 ->
      let (newTs, newAcc2) = recurseAll newAcc ts in
      ret (TUnion ws1 newTs ws2) newAcc2
    TApp ws1 tChild1 ts ws2 ->
      let (newTChild1, newAcc2) = recurse newAcc tChild1 in
      let (newTs, newAcc3) = recurseAll newAcc2 ts in
      ret (TApp ws1 newTChild1 newTs ws2) newAcc3
    TVar _ _ ->
      (newT, newAcc)
    TForall ws1 tpats tChild1 ws2 ->
      let (newTChild1, newAcc2) = recurse newAcc tChild1 in
      ret (TForall ws1 tpats newTChild1 ws2) newAcc2
    TParens ws1 tChild1 ws2 ->
      let (newTChild1, newAcc2) = recurse newAcc tChild1 in
      ret (TParens ws1 newTChild1 ws2) newAcc2
    TWildcard _ ->
      (newT, newAcc)

startBindingNumLetType _ = 0

startBindingNumLetAnnotation (Declarations printOrder tps annots letexpsGroups as decls) =
  List.sum (List.map (Tuple.second >> List.length) tps)

startBindingNumLetExp (Declarations printOrder tps annots letexpsGroups  as decls) =
  startBindingNumLetAnnotation decls + List.length annots

-- Nodes visited/replaced in top-down, left-to-right order.
-- Includes user-defined scope information.
-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapFoldExpTopDownWithScope
  :  (Exp -> a -> b -> (Exp, a, b))
     -- handleLetExp. The binding number is the one of the first LetExp.
  -> (Exp -> IsRec -> List LetExp -> BindingNumber -> a -> b -> (a, b))
     -- hanldeEFun
  -> (Exp -> b -> b)
      -- hanldeCaseBranch
  -> (Exp -> Branch -> BindingNumber -> b -> b)
  -> a -- Global accumulator
  -> b -- Scope, e.g. a new Exp
  -> Exp
  -> (Exp, a)
mapFoldExpTopDownWithScope f handleLetExp handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e =
  let (newE, newGlobalAcc, newScopeTempAcc) = f e initGlobalAcc initScopeTempAcc in
  let ret e__ globalAcc =
    (replaceE__ newE e__, globalAcc)
  in
  let recurse globalAcc scopeTempAcc child =
    mapFoldExpTopDownWithScope f handleLetExp handleEFun handleCaseBranch globalAcc scopeTempAcc child
  in
  let recurseAll globalAcc scopeTempAcc exps =
    exps
    |> List.foldl
        (\exp (newExps, globalAcc) ->
          let (newExp, newGlobalAcc) = recurse globalAcc scopeTempAcc exp in
          (newExps ++ [newExp], newGlobalAcc)
        )
        ([], globalAcc)
  in
  let mapDeclarations: a -> b ->         Declarations -> (Declarations, a, b)
      mapDeclarations  globalAcc  scopeTempAcc (Declarations printOrder tps annots letexpsGroups as decls) =
    let (_, newRevGroups, newGlobalAcc, newAccScope) = Utils.foldLeft
          (startBindingNumLetExp decls,
             [],        globalAcc, scopeTempAcc) letexpsGroups <|
         \(bindingNumberOfGroupHead,
             revGroups, accGlobal, accScope) (isRec, letexps) -> let
              (updatedAccGlobal, nextScope) =
                handleLetExp newE isRec letexps bindingNumberOfGroupHead accGlobal accScope
              bindingScope =
                if isRec then nextScope else accScope
              (finalAccGlobal, newLetExps) = Tuple.mapSecond List.reverse <|
                Utils.foldLeft (updatedAccGlobal, []) letexps <|
                              \(accGlobal, revNewLetExps) (LetExp spc spp p fs spe e1) ->
                  let (newE1, newAccGlobal) = recurse accGlobal bindingScope e1 in
                  (newAccGlobal, LetExp spc spp p fs spe newE1 :: revNewLetExps)
             in
             (bindingNumberOfGroupHead + List.length letexps,
               (isRec, newLetExps) :: revGroups, finalAccGlobal, nextScope)
    in
    (Declarations printOrder tps annots  (List.reverse newRevGroups), newGlobalAcc, newAccScope)
  in
  case unwrapExp newE of
    EConst _ _ _ _ -> (newE, newGlobalAcc)
    EBase _ _      -> (newE, newGlobalAcc)
    EVar _ _       -> (newE, newGlobalAcc)
    EFun ws1 ps e1 ws2 ->
      let newScopeTempAcc2 = handleEFun newE newScopeTempAcc in
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc2 e1 in
      ret (EFun ws1 ps newE1 ws2) newGlobalAcc2

    EApp ws1 e1 es apptype ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc e1 in
      let (newEs, newGlobalAcc3) = recurseAll newGlobalAcc2 newScopeTempAcc es in
      ret (EApp ws1 newE1 newEs apptype ws2) newGlobalAcc3

    EOp ws1 wso op es ws2 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc newScopeTempAcc es in
      ret (EOp ws1 wso op newEs ws2) newGlobalAcc2

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc newScopeTempAcc (Utils.listValues es) in
      ret (EList ws1 (Utils.listValuesMake es newEs) ws2 Nothing ws3) newGlobalAcc2

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc e1 in
      let (newEs, newGlobalAcc3) = recurseAll newGlobalAcc2 newScopeTempAcc (Utils.listValues es) in
      ret (EList ws1 (Utils.listValuesMake es newEs) ws2 (Just newE1) ws3) newGlobalAcc3

    ERecord ws1 Nothing decls ws2 ->
      let (newDecls, newGlobalAcc2, _) = mapDeclarations newGlobalAcc newScopeTempAcc decls in
      ret (ERecord ws1 Nothing newDecls ws2) newGlobalAcc2

    ERecord ws1 (Just (mi, wsi)) decls ws2 ->
      let (newMi, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc mi in
      let (newDecls, newGlobalAcc3, _) = mapDeclarations newGlobalAcc2 newScopeTempAcc decls in
      ret (ERecord ws1 (Just (newMi, wsi)) newDecls ws2) newGlobalAcc3

    ESelect ws0 e1 ws1 ws2 ident ->
      let (newE, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc e1 in
      ret (ESelect ws0 newE ws1 ws2 ident) newGlobalAcc2

    EIf ws1 e1 ws2 e2 ws3 e3 ws4 ->
      case recurseAll newGlobalAcc newScopeTempAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newGlobalAcc2) -> ret (EIf ws1 newE1 ws2 newE2 ws3 newE3 ws4) newGlobalAcc2
        _                                      -> Debug.crash "I'll buy you a beer if this line of code executes. - Brian"

    ECase ws1 e1 branches ws2 ->
      -- Note: ECase given to handleBranch has original scrutinee for now.
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc e1 in
      let (newBranches, newGlobalAcc3) =
        branches
        |> Utils.foldli1
            (\(i, branch) (newBranches, globalAcc) ->
              let newScopeTempAcc2 = handleCaseBranch newE branch i newScopeTempAcc in
              let (Branch_ bws1 p ei bws2) = branch.val in
              let (newEi, newGlobalAcc3) = recurse globalAcc newScopeTempAcc2 ei in
              (newBranches ++ [{ branch | val = Branch_ bws1 p newEi bws2 }], newGlobalAcc3)
            )
            ([], newGlobalAcc2)
      in
      ret (ECase ws1 newE1 newBranches ws2) newGlobalAcc3

    ELet ws1 lettype decls spIn body ->
      let (newDecls, newGlobalAcc2, newScopeAcc) = mapDeclarations newGlobalAcc newScopeTempAcc decls in
      let (newBody, newGlobalAcc3) = recurse newGlobalAcc2 newScopeAcc body in
      ret (ELet ws1 lettype newDecls spIn newBody) newGlobalAcc3

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc e1 in
      ret (EColonType ws1 newE1 ws2 tipe ws3) newGlobalAcc2

    EParens ws1 e1 pStyle ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc e1 in
      ret (EParens ws1 newE1 pStyle ws2) newGlobalAcc2

    EHole _ _ -> (newE, newGlobalAcc)


mapExp : (Exp -> Exp) -> Exp -> Exp
mapExp f e =
  -- Accumulator thrown away; just need something that type checks.
  let (newExp, _) = mapFoldExp (\exp _ -> (f exp, ())) () e in
  newExp

mapPat : (Pat -> Pat) -> Pat -> Pat
mapPat f p =
  -- Accumulator thrown away; just need something that type checks.
  let (newPat, _) = mapFoldPat (\pat _ -> (f pat, ())) () p in
  newPat

-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapExpTopDown : (Exp -> Exp) -> Exp -> Exp
mapExpTopDown f e =
  -- Accumulator thrown away; just need something that type checks.
  let (newExp, _) = mapFoldExpTopDown (\exp _ -> (f exp, ())) () e in
  newExp

-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapPatTopDown : (Pat -> Pat) -> Pat -> Pat
mapPatTopDown f p =
  -- Accumulator thrown away; just need something that type checks.
  let (newPat, _) = mapFoldPatTopDown (\pat _ -> (f pat, ())) () p in
  newPat

-- Preserves EIds
mapExpViaExp__ : (Exp__ -> Exp__) -> Exp -> Exp
mapExpViaExp__ f e =
  let f_ exp = replaceE__ exp (f (unwrapExp exp)) in
  mapExp f_ e

-- Folding function returns just newGlobalAcc instead of (newExp, newGlobalAcc)
foldExpTopDownWithScope
  :  (Exp -> accT -> scopeAccT -> accT)
  -> (Exp -> IsRec -> List LetExp -> BindingNumber -> accT -> scopeAccT -> (accT, scopeAccT))
  -> (Exp -> scopeAccT -> scopeAccT)
  -> (Exp -> Branch -> BindingNumber -> scopeAccT -> scopeAccT)
  -> accT
  -> scopeAccT
  -> Exp
  -> accT
foldExpTopDownWithScope f handleLetExps handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e =
  let (_, finalGlobalAcc) =
    mapFoldExpTopDownWithScope
        (\e globalAcc scopeTempAcc -> (e, f e globalAcc scopeTempAcc, scopeTempAcc))
        handleLetExps handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e
  in
  finalGlobalAcc

replaceV_ : Val -> Val_ -> Val
replaceV_ v v_ = { v | v_ = v_ }

mapVal : (Val -> Val) -> Val -> Val
mapVal f v = case v.v_ of
  VList vs         -> f { v | v_ = VList (List.map (mapVal f) vs) }
  VDict d          -> f { v | v_ = VDict     (Dict.map (\_ v -> mapVal f v) d) } -- keys ignored
  VRecord d        -> f { v | v_ = VRecord <| Dict.map (\_ v -> mapVal f v) d } -- fields ignored
  VConst _ _       -> f v
  VBase _          -> f v
  VClosure _ _ _ _ -> f v
  VFun _ _ _ _     -> f v

foldVal : (Val -> a -> a) -> Val -> a -> a
foldVal f v a = case v.v_ of
  VList vs         -> f v (List.foldl (foldVal f) a vs)
  VDict d          -> f v (List.foldl (foldVal f) a (Dict.values d)) -- keys ignored
  VRecord d        -> f v (List.foldl (foldVal f) a (Dict.values d)) -- keys ignored
  VConst _ _       -> f v a
  VBase _          -> f v a
  VClosure _ _ _ _ -> f v a
  VFun _ _ _ _     -> f v a

-- Not using foldVal so we can get the children in a nicer order.
flattenValTree : Val -> List Val
flattenValTree val =
  val :: List.concatMap flattenValTree (childVals val)

childVals : Val -> List Val
childVals val = case val.v_ of
  VList vs         -> vs
  VDict d          -> Dict.values d -- keys ignored
  VRecord d        -> Dict.values d -- fields ignored
  VConst _ _       -> []
  VBase _          -> []
  VClosure _ _ _ _ -> []
  VFun _ _ _ _     -> []

-- Fold through preorder traversal
foldExp : (Exp -> a -> a) -> a -> Exp -> a
foldExp f acc exp =
  List.foldl f acc (flattenExpTree exp)

foldExpViaE__ : (Exp__ -> a -> a) -> a -> Exp -> a
foldExpViaE__ f acc exp =
  let f_ exp = f (unwrapExp exp) in
  foldExp f_ acc exp

-- Search for eid in root, replace matching node based on given function
mapExpNode : EId -> (Exp -> Exp) -> Exp -> Exp
mapExpNode eid f root =
  mapExp
      (\exp ->
        if expEId exp == eid
        then f exp
        else exp
      )
      root

replaceExpNode : EId -> Exp -> Exp -> Exp
replaceExpNode eid newNode root =
  mapExpNode eid (always newNode) root

replaceExpNodePreservingPrecedingWhitespace : EId -> Exp -> Exp -> Exp
replaceExpNodePreservingPrecedingWhitespace eid newNode root =
  mapExpNode
      eid
      (\exp -> replacePrecedingWhitespace (precedingWhitespace exp) newNode)
      root

replaceExpNodeE__ : Exp -> Exp__ -> Exp -> Exp
replaceExpNodeE__ oldNode newE__ root =
  replaceExpNodeE__ByEId (expEId oldNode) newE__ root

replaceExpNodeE__ByEId : EId -> Exp__ -> Exp -> Exp
replaceExpNodeE__ByEId eid newE__ root =
  let esubst = Dict.singleton eid newE__ in
  applyESubst esubst root

-- Like applyESubst, but you give Exp not E__
replaceExpNodes : (Dict EId Exp) -> Exp -> Exp
replaceExpNodes eidToNewNode root =
  mapExpTopDown -- top down to handle replacements that are subtrees of each other; a naive eidToNewNode could however make this loop forever
    (\exp ->
      case Dict.get (expEId exp) eidToNewNode of
        Just newExp -> newExp
        Nothing     -> exp
    )
    root

replaceExpNodesPreservingPrecedingWhitespace : (Dict EId Exp) -> Exp -> Exp
replaceExpNodesPreservingPrecedingWhitespace eidToNewNode root =
  mapExpTopDown -- top down to handle replacements that are subtrees of each other; a naive eidToNewNode could however make this loop forever
    (\exp ->
      case Dict.get (expEId exp) eidToNewNode of
        Just newExp -> replacePrecedingWhitespace (precedingWhitespace exp) newExp
        Nothing     -> exp
    )
    root

mapPatNodePat : PId -> (Pat -> Pat) -> Pat -> Pat
mapPatNodePat pid f rootPat =
  mapPat
      (\pat ->
        if pat.val.pid == pid
        then f pat
        else pat
      )
      rootPat

-- Search for pid in root, replace matching pat based on given function
mapPatNode : PId -> (Pat -> Pat) -> Exp -> Exp
mapPatNode pid f root =
  mapExpViaExp__
      (\e__ ->
        case e__ of
          ELet  ws1 lettype (Declarations printOrder tps annots exps) spaceBeforeIn body ->
            let newExps = elemsOf exps |> List.map (
             \(LetExp mbc sp0 name funPolicy spEq e1) ->
               LetExp mbc sp0 (mapPatNodePat pid f name) funPolicy spEq e1
              )
            in
            ELet ws1 lettype (Declarations printOrder tps annots (regroup exps newExps)) spaceBeforeIn body
          EFun ws1 pats body ws2                            -> EFun ws1 (List.map (mapPatNodePat pid f) pats) body ws2
          ECase ws1 scrutinee branches ws2                  -> ECase ws1 scrutinee (mapBranchPats (mapPatNodePat pid f) branches) ws2
          _                                                 -> e__
      )
      root

replacePatNodePreservingPrecedingWhitespace : PId -> Pat -> Exp -> Exp
replacePatNodePreservingPrecedingWhitespace pid newPat root =
  mapPatNode
      pid
      (\pat -> replacePrecedingWhitespacePat (precedingWhitespacePat pat) newPat)
      root

mapType : (Type -> Type) -> Type -> Type
mapType f tipe =
  let recurse = mapType f in
  let wrap: Type__ -> Type
      wrap = replaceT__ tipe in
  case tipe.val.t__ of
    TNum _       -> f tipe
    TBool _      -> f tipe
    TString _    -> f tipe
    TNull _      -> f tipe
    TVar _ _     -> f tipe
    TWildcard _  -> f tipe

    TList ws1 t1 ws2        -> f (wrap (TList ws1 (recurse t1) ws2))
    TDict ws1 t1 t2 ws2     -> f (wrap (TDict ws1 (recurse t1) (recurse t2) ws2))
    TArrow ws1 ts ws2       -> f (wrap (TArrow ws1 (List.map recurse ts) ws2))
    TUnion ws1 ts ws2       -> f (wrap (TUnion ws1 (List.map recurse ts) ws2))
    TApp ws1 t ts appType   -> f (wrap (TApp ws1 (recurse t) (List.map recurse ts) appType))
    TForall ws1 vars t1 ws2 -> f (wrap (TForall ws1 vars (recurse t1) ws2))

    TTuple ws1 ts ws2 mt ws3 ->
      f (wrap (TTuple ws1 (List.map recurse ts) ws2 (Maybe.map recurse mt) ws3))
    TRecord ws1 mi ts ws2       ->
      f (wrap (TRecord ws1 mi (Utils.recordValuesMap recurse ts) ws2))
    TParens ws1 t ws2 -> f (wrap (TParens ws1 (recurse t) ws2))

foldType : (Type -> a -> a) -> Type -> a -> a
foldType f tipe acc =
  let foldTypes f tipes acc = List.foldl (\t acc -> foldType f t acc) acc tipes in
  case tipe.val.t__ of
    TNum _          -> acc |> f tipe
    TBool _         -> acc |> f tipe
    TString _       -> acc |> f tipe
    TNull _         -> acc |> f tipe
    TVar _ _        -> acc |> f tipe
    TWildcard _     -> acc |> f tipe
    TList _ t _     -> acc |> foldType f t |> f tipe
    TDict _ t1 t2 _ -> acc |> foldType f t1 |> foldType f t2 |> f tipe
    TForall _ _ t _ -> acc |> foldType f t |> f tipe
    TArrow _ ts _   -> acc |> foldTypes f ts |> f tipe
    TUnion _ ts _   -> acc |> foldTypes f ts |> f tipe
    TApp _ t ts _   -> acc |> f tipe |> foldTypes f ts |> f tipe

    TTuple _ ts _ Nothing _  -> acc |> foldTypes f ts |> f tipe
    TTuple _ ts _ (Just t) _ -> acc |> foldTypes f (ts++[t]) |> f tipe
    TRecord _ _ ts _  -> acc |> foldTypes f (Utils.recordValues ts) |> f tipe
    TParens _ t _ -> foldType f t acc

------------------------------------------------------------------------------
-- Traversing

eidIs : EId -> Exp -> Bool
eidIs targetEId exp = expEId exp == targetEId

-- Returns pre-order list of expressions
-- O(n^2) memory
flattenExpTree : Exp -> List Exp
flattenExpTree exp =
  exp :: List.concatMap flattenExpTree (childExps exp)

-- DFS
findFirstNode : (Exp -> Bool) -> Exp -> Maybe Exp
findFirstNode predicate exp =
  if predicate exp then
    Just exp
  else
    childExps exp
    |> Utils.mapFirstSuccess (findFirstNode predicate)

-- find+map a node (DFS)
mapFirstSuccessNode : (Exp -> Maybe a) -> Exp -> Maybe a
mapFirstSuccessNode f exp =
  case f exp of
    Just result -> Just result
    Nothing     -> childExps exp |> Utils.mapFirstSuccess (mapFirstSuccessNode f)

findExpByEId : Exp -> EId -> Maybe Exp
findExpByEId program targetEId =
  findFirstNode (eidIs targetEId) program

findLetexpByBindingNumber: Exp -> BindingNumber -> Maybe LetExp
findLetexpByBindingNumber program targetBinding =
  case unwrapExp program of
    ELet _ _ decls _ _ ->
      case getDeclarationsInOrder decls |> flip Utils.nth targetBinding of
         Ok (DeclExp x) -> Just x
         _ -> Nothing
    _ -> Nothing

-- justFindExpByEId is in LangTools (it needs the unparser for error messages).
-- LangTools.justFindExpByEId : EId -> Exp -> Exp
-- LangTools.justFindExpByEId eid exp =
--   findExpByEId exp eid
--   |> Utils.fromJust__ (\() -> "Couldn't find eid " ++ toString eid ++ " in " ++ unparseWithIds exp)


findPatByPId : Exp -> PId -> Maybe Pat
findPatByPId program targetPId =
  findScopeExpAndPatByPId program targetPId
  |> Maybe.map (\((scopeExp, _), pat) -> pat)


findScopeExpAndPatByPId : Exp -> PId -> Maybe ((Exp, BindingNumber), Pat)
findScopeExpAndPatByPId program targetPId =
  program
  |> mapFirstSuccessNode
      (\e ->
        let maybeTargetPat: Maybe (BindingNumber, Pat)
            maybeTargetPat =
          case unwrapExp e of
             ELet _ _ decls _ _      ->
               getDeclarationsInOrderWithIndex decls |> Utils.mapFirstSuccess (
                 \(decl, i) -> case decl of
                   DeclExp (LetExp mbc spp pat funStyle sp0 e1) ->
                     findPatInPat targetPId pat |> Maybe.map ((,) i)
                   DeclType (LetType mbc sp spa pat funStyle sp0 t1) ->
                     findPatInPat targetPId pat |> Maybe.map ((,) i)
                   DeclAnnotation (LetAnnotation mbc sp pat funStyle sp0 t1) ->
                     findPatInPat targetPId pat |> Maybe.map ((,) i)
               )
             EFun _ pats _ _          ->
               Utils.mapFirstSuccess
                 (\(p, i) -> findPatInPat targetPId p |> Maybe.map ((,) i))
                   (Utils.zipWithIndex pats)
             ECase _ _ branches _     ->
               Utils.mapFirstSuccess
                 (\(pat, j) -> findPatInPat targetPId pat |> Maybe.map ((,) j))
                   (Utils.zipWithIndex (branchPats branches))
             _                        -> Nothing
        in
        maybeTargetPat |> Maybe.map (\(binding, pat) -> ((e, binding), pat))
      )


findPatInPat : PId -> Pat -> Maybe Pat
findPatInPat targetPId pat =
  flattenPatTree pat
  |> Utils.findFirst (.val >> .pid >> (==) targetPId)


findExpByLocId : Exp -> LocId -> Maybe Exp
findExpByLocId program targetLocId =
  let isTarget exp =
    case (unwrapExp exp) of
       EConst _ _ (locId, _, _) _ -> locId == targetLocId
       _                          -> False
  in
  findFirstNode isTarget program

locIdToEId : Exp -> LocId -> Maybe EId
locIdToEId program locId =
  findExpByLocId program locId |> Maybe.map expEId

locToLocId : Loc -> LocId
locToLocId (locId, _, _) = locId

-- For each node for which `predicate` returns True, return it and its ancestors
-- For each matching node, ancestors appear in order: root first, match last.
findAllWithAncestors : (Exp -> Bool) -> Exp -> List (List Exp)
findAllWithAncestors predicate exp =
  findAllWithAncestors_ predicate [] exp

findAllWithAncestors_ : (Exp -> Bool) -> List Exp -> Exp -> List (List Exp)
findAllWithAncestors_ predicate ancestors exp =
  let ancestorsAndThis = ancestors ++ [exp] in
  let thisResult       = if predicate exp then [ancestorsAndThis] else [] in
  let recurse exp      = findAllWithAncestors_ predicate ancestorsAndThis exp in
  thisResult ++ List.concatMap recurse (childExps exp)

commonAncestors : (Exp -> Bool) -> Exp -> List Exp
commonAncestors pred exp =
  findAllWithAncestors pred exp
  |> List.map (Utils.dropLast 1) -- Never return an expression that the predicate matched: it will be moved/removed/replaced
  |> Utils.commonPrefix

-- Target expression is last in list.
findWithAncestorsByEId : Exp -> EId -> Maybe (List Exp)
findWithAncestorsByEId exp targetEId =
  if expEId exp == targetEId then
    Just [exp]
  else
    childExps exp
    |> Utils.mapFirstSuccess (\child -> findWithAncestorsByEId child targetEId)
    |> Maybe.map (\descendents -> exp::descendents)


-- Nothing means not found
-- Just Nothing means EId is program root
-- Just (Just exp) mean EId found and parent found
parentByEId : Exp -> EId -> Maybe (Maybe Exp)
parentByEId program targetEId =
  findWithAncestorsByEId program targetEId
  |> Maybe.map (Utils.takeLast 2 >> Utils.dropLast 1 >> List.head)


-- Children left-to-right.
childExps : Exp -> List Exp
childExps e = childExpsExtractors e |> Tuple.first

singleArgExtractor: String -> Exp -> (Exp -> Exp__) -> (List Exp -> Exp)
singleArgExtractor msg exp f l = case l of
  head::_ -> replaceE__ exp  <| f head
  _ -> Debug.crash <| "[internal error] Unespected empty list for " ++ msg

multiArgExtractor: String -> Exp -> (List Exp -> Exp__) -> (List Exp -> Exp)
multiArgExtractor msg exp f l = replaceE__ exp <| f l

groupsOfExtractors: GroupsOf a -> (List a, List a -> GroupsOf a)
groupsOfExtractors groups = case groups of
  [] -> ([], \x -> [])
  (isRec, group1) :: tail -> let
      (tailA, tailABuilder) = groupsOfExtractors tail
      rebuilder newList =
         let (newGroup1, newTailA) = Utils.split (List.length group1) newList in
         (isRec, newGroup1) :: tailABuilder newTailA
    in
    (group1 ++ tailA, rebuilder)

declExtractors: Declarations -> (List Exp, List Exp -> Declarations)
declExtractors (Declarations printOrder tps annots letexpsGroups) =
  let (letexps, letexpsBuilder) = groupsOfExtractors letexpsGroups in
  (letexps |> List.map (\(LetExp mbc sp0 name funStyle spEq e1) -> e1),
   \newExps ->
     let newLetExpsGroups = letexpsBuilder <|
        List.map2 (\(LetExp mbc sp0 name funStyle spEq e1) newE1 ->
        LetExp mbc sp0 name funStyle spEq newE1) letexps newExps
     in
      (Declarations printOrder tps annots newLetExpsGroups)
  )

-- Children left-to-right, with a way to rebuild the expression if given the same exps)
childExpsExtractors : Exp -> (List Exp, List Exp -> Exp)
childExpsExtractors e =
  case (unwrapExp e) of
    EConst _ _ _ _          -> ([], \_ -> e)
    EBase _ _               -> ([], \_ -> e)
    EVar _ _                -> ([], \_ -> e)
    EFun ws1 ps e_ ws2      -> ([e_], singleArgExtractor "EFun-unexp" e <| \newE -> EFun ws1 ps newE ws2)
    EOp ws1 wso op es ws2       -> (es, multiArgExtractor "EOp-unexp" e <| \newEs -> EOp ws1 wso op newEs ws2)
    EList ws1 es ws2 m ws3  ->
      case m of
        Just e  -> (Utils.listValues es ++ [e], multiArgExtractor "EList-unexp" e  <| \newEs ->  EList ws1 (Utils.listValuesMake es <| Utils.dropLast 1 newEs) ws2 (Just (Utils.last "childExps-EList" newEs)) ws3)
        Nothing ->( Utils.listValues es, multiArgExtractor "EList-unexp" e  <| \newEs ->  EList ws1 (Utils.listValuesMake es <| newEs) ws2 Nothing ws3)
    ERecord ws1 mw decls ws2 ->
      let (declExps, declRebuilder) = declExtractors decls in
      case mw of
         Just (e, w) -> (e :: declExps,
           multiArgExtractor "ERecord-unexp" e  <| \newExps ->
             case newExps of
               newE :: newDefExps ->
                 let newDecls = declRebuilder newDefExps in
                 ERecord ws1 (Just (newE , w)) newDecls ws2
               [] -> Debug.crash "[Internal Error] Unexpected rebuild of ERecord, missing fields")
         Nothing -> (declExps,
           multiArgExtractor "ERecord-unexp" e  <| \newDefExps ->
             let newDecls = declRebuilder newDefExps in
             ERecord ws1 Nothing newDecls ws2)
    ESelect sp0 e sp1 sp2 name       -> ([e], multiArgExtractor "ESelect-unexp" e <| \newEs -> ESelect sp0 (Utils.head "childExps-ESelect" newEs) sp1 sp2 name)
    EApp ws1 f es apptype ws2        -> (f :: es, multiArgExtractor "EApp-unexp" e <| \newEs -> EApp ws1 (Utils.head "childExps-EApp" newEs) (Utils.tail "childExps-Eapp" newEs) apptype ws2)
    ELet  ws1 lettype decls spaceBeforeIn body ->
      let (declExps, declRebuilder) = declExtractors decls in
      (declExps ++ [body], multiArgExtractor "ELet-unexp" e <| \newExps ->
        case Utils.snocUnapply newExps of
          Just (newDeclExps, newBody) ->
             let newDecls = declRebuilder newDeclExps in
             ELet  ws1 lettype newDecls spaceBeforeIn newBody
          _ -> Debug.crash <| "Expected at least one expression for the body of ELet, got Nothing")

    EIf ws1 e1 ws2 e2 ws3 e3 ws4     -> ([e1, e2, e3], multiArgExtractor "EIf-unexp" e <| \newEs -> case newEs of
        [newE1, newE2, newE3] -> EIf ws1 newE1 ws2 newE2 ws3 newE3 ws4
        _ -> Debug.crash "childExps-EIf")
    ECase ws1 e branches ws2         -> let (es, esExtractor) = branchExpsExtractor branches in
      (e :: es, multiArgExtractor "ECase-unexp" e <| \newEs ->  ECase ws1 (Utils.head "childExps-ECase" newEs) (Utils.tail "childExps-ECAse" newEs |> esExtractor) ws2)
    EColonType ws1 e ws2 tipe ws3    -> ([e], singleArgExtractor  "EColonType-unexp" e <| \newE -> EColonType ws1 newE ws2 tipe ws3)
    EParens a1 e a2 a3               -> ([e], singleArgExtractor  "EParens-unexp" e <| \newE -> EParens a1 newE a2 a3)
    EHole _ _                        -> ([], \_ -> e)

allEIds : Exp -> List EId
allEIds exp =
  flattenExpTree exp |> List.map expEId


------------------------------------------------------------------------------
-- Conversion

valToNum : Val -> Num
valToNum v = case v.v_ of
  VConst _  (n, _) -> n
  _                -> Debug.crash "Lang.valToNum"

valToTrace : Val -> Trace
valToTrace v = case v.v_ of
  VConst _  (n, tr) -> tr
  _                 -> Debug.crash "Lang.valToTrace"

valIsNum : Val -> Bool
valIsNum v = case v.v_ of
  VConst _  _ -> True
  _           -> False

------------------------------------------------------------------------------
-- Location Substitutions
-- Expression Substitutions

type alias Subst = Dict LocId Num
type alias SubstPlus = Dict LocId (WithInfo Num)
type alias SubstMaybeNum = Dict LocId (Maybe Num)

type alias ESubst = Dict EId Exp__

type alias TwoSubsts = { lsubst : Subst, esubst : ESubst }

-- For unparsing traces, possibily inserting variables: d
type alias SubstStr = Dict LocId String

applyLocSubst : Subst -> Exp -> Exp
applyLocSubst s = applySubst { lsubst = s, esubst = Dict.empty }

applyESubst : ESubst -> Exp -> Exp
applyESubst s = applySubst { lsubst = Dict.empty, esubst = s }

applySubst : TwoSubsts -> Exp -> Exp
applySubst subst exp =
  let replacer =
    (\e ->
       let e__ConstReplaced =
         let e__ = (unwrapExp e) in
         case e__ of
            EConst ws n loc wd ->
              let locId = Utils.fst3 loc in
              case Dict.get locId subst.lsubst of
                Just n_ -> EConst ws n_ loc wd
                Nothing -> e__
                -- 10/28: substs from createMousePosCallbackSlider only bind
                -- updated values (unlike substs from Sync)
            _ -> e__
       in
       let e__New =
         case Dict.get (expEId e) subst.esubst of
            Just e__New -> e__New
            Nothing     -> e__ConstReplaced
       in
       replaceE__ e e__New
    )
  in
  mapExp replacer exp


applyESubstPreservingPrecedingWhitespace : ESubst -> Exp -> Exp
applyESubstPreservingPrecedingWhitespace esubst exp =
  let replacer =
    (\e ->
       case Dict.get (expEId e) esubst of
         Just e__New -> replaceE__PreservingPrecedingWhitespace e e__New
         Nothing     -> e
    )
  in
  mapExp replacer exp

{-
-- for now, LocId instead of EId
type alias ESubst = Dict LocId Exp__

applyESubst : ESubst -> Exp -> Exp
applyESubst esubst =
  mapExpViaExp__ <| \e__ -> case e__ of
    EConst _ i -> case Dict.get (Utils.fst3 i) esubst of
                    Nothing   -> e__
                    Just e__' -> e__'
    _          -> e__
-}

-- Idents take priority if in locId is in both dicts.
locIdToExpFromFrozenSubstAndNewNames : Dict LocId Num -> Dict LocId Ident -> Dict LocId Exp
locIdToExpFromFrozenSubstAndNewNames locIdToFrozenNum locIdToIdent =
  Dict.merge
      (\locId n       locIdToExp -> locIdToExp |> Dict.insert locId (eConst n (dummyLoc_ frozen)))
      (\locId n ident locIdToExp -> locIdToExp |> Dict.insert locId (eVar ident))
      (\locId   ident locIdToExp -> locIdToExp |> Dict.insert locId (eVar ident))
      locIdToFrozenNum
      locIdToIdent
      Dict.empty

traceToExp : Dict LocId Exp -> Trace -> Exp
traceToExp locIdToExp trace =
  case trace of
    TrLoc (locId, _, _) ->
      case Dict.get locId locIdToExp of
        Just exp -> exp
        Nothing  -> eVar ("couldNotFindLocId" ++ toString locId ++ "InLocIdToExpDict")

    TrOp op childTraces ->
      let childExps = List.map (traceToExp locIdToExp) childTraces in
      eOp op childExps

-----------------------------------------------------------------------------
-- Utility

branchExp : Branch -> Exp
branchExp branch =
  let (Branch_ _ _ exp _) = branch.val in
  exp

branchExpExtractor : Branch -> (Exp, Exp -> Branch)
branchExpExtractor branch =
  case branch.val of
   Branch_ a b exp c -> (exp, \newExp -> replaceB__ branch <| Branch_ a b newExp c)

branchExps : List Branch -> List Exp
branchExps branches =
  List.map branchExp branches

branchExpsExtractor : List Branch -> (List Exp, List Exp -> List Branch)
branchExpsExtractor branches =
  List.map branchExpExtractor branches |> List.unzip |> Tuple.mapSecond (\expsToBranch ->
    \newExps -> List.map2 (\toBranch newExp -> toBranch newExp) expsToBranch newExps
  )


tbranchExp : TBranch -> Exp
tbranchExp tbranch =
  let (TBranch_ _ _ exp _) = tbranch.val in
  exp

tbranchExpExtractor : TBranch -> (Exp, Exp -> TBranch)
tbranchExpExtractor tbranch =
  case tbranch.val of
    TBranch_ a b exp c  -> (exp, \newExp -> replaceTB__ tbranch <| TBranch_ a b newExp c)

tbranchExps : List TBranch -> List Exp
tbranchExps tbranches =
  List.map tbranchExp tbranches

tbranchExpsExtractor : List TBranch -> (List Exp, List Exp -> List TBranch)
tbranchExpsExtractor tbranches =
  List.map tbranchExpExtractor tbranches |> List.unzip |> Tuple.mapSecond (\expsToBranch ->
      \newExps -> List.map2 (\toBranch newExp -> toBranch newExp) expsToBranch newExps)

branchPat : Branch -> Pat
branchPat branch =
  let (Branch_ _ pat _ _) = branch.val in
  pat

branchPats : List Branch -> List Pat
branchPats branches =
  List.map branchPat branches

mapBranchPats : (Pat -> Pat) -> List Branch -> List Branch
mapBranchPats f branches =
  branches
  |> List.map
      (\branch ->
        let (Branch_ ws1 pat exp ws2) = branch.val in
        { branch | val = Branch_ ws1 (f pat) exp ws2 }
      )

tbranchType : TBranch -> Type
tbranchType tbranch =
  let (TBranch_ _ tipe _ _) = tbranch.val in
  tipe

tbranchTypes : List TBranch -> List Type
tbranchTypes tbranches =
  List.map tbranchType tbranches

-- Need parent expression since case expression branches into several scopes
isScope : Maybe Exp -> Exp -> Bool
isScope maybeParent exp =
  let isObviouslyScope =
    case (unwrapExp exp) of
       ELet _ _ _ _ _  -> True
       EFun _ _ _ _      -> True
       _                 -> False
  in
  case maybeParent of
    Just parent ->
      case unwrapExp parent of
        ECase _ predicate branches _ -> predicate /= exp
        _                            -> isObviouslyScope
    Nothing -> isObviouslyScope

isNumber : Exp -> Bool
isNumber exp =
  case (unwrapExp exp) of
    EConst _ _ _ _ -> True
    _              -> False

-- Disregards syncOptions
isFrozenNumber : Exp -> Bool
isFrozenNumber exp =
  case (unwrapExp exp) of
    EConst _ _ (_, ann, _) _ -> ann == frozen
    _                        -> False

varsOfPat : Pat -> List Ident
varsOfPat pat =
  case pat.val.p__ of
    PVar _ x _              -> [x]
    _ -> List.concatMap varsOfPat (childPats pat)

flattenPatTree : Pat -> List Pat
flattenPatTree pat =
  pat :: List.concatMap flattenPatTree (childPats pat)


-- Children left-to-right.
childPats : Pat -> List Pat
childPats pat =
  case pat.val.p__ of
    PConst _ _              -> []
    PBase _ _               -> []
    PVar _ _ _              -> []
    PWildcard _             -> []
    PList _ ps _ Nothing _  -> ps
    PRecord _ ps _          -> Utils.recordValues ps
    PColonType _ p _ _      -> [p]
    PList _ ps _ (Just p) _ -> ps ++ [p]
    PAs _ p1 _ p2           -> [p1, p2]
    PParens _ p _           -> [p]

flattenTypeTree : Type -> List Type
flattenTypeTree typ =
  typ :: List.concatMap flattenTypeTree (childTypes typ)

-- Children left-to-right.
childTypes : Type -> List Type
childTypes typ =
  case typ.val.t__ of
    TNum _ ->
      []
    TBool _ ->
      []
    TString _ ->
      []
    TNull _ ->
      []
    TList _ t _ ->
      [t]
    TDict _ key val _ ->
      [key, val]
    TRecord _ _ entries _  ->
      List.map (\(_, _, _, _, t) -> t) entries
    TTuple _ heads _ maybeTail _ ->
      heads ++ (Maybe.withDefault [] << Maybe.map List.singleton) maybeTail
    TArrow _ ts _ ->
      ts
    TUnion _ ts _ ->
      ts
    TApp _ func args _ ->
      func :: args
    TVar _ _ ->
      []
    TForall _ _ result _ ->
      []
    TParens _ inner _ ->
      [inner]
    TWildcard _ ->
      []

-----------------------------------------------------------------------------
-- Lang Options

-- all options should appear before the first non-comment expression

optionRegex = Regex.regex "#\\s*(\\w+)\\s*:\\s*(\\w+)"

getOptionsFromString: String -> List (String, String)
getOptionsFromString whitespace =
  Regex.find Regex.All optionRegex whitespace |> List.concatMap (\m ->
    case m.submatches of
      [Just key, Just value] -> [(key, value)]
      _ -> []
  )

getOptions : Exp -> List (String, String)
getOptions e = getOptionsFromString <| precedingWhitespace e


------------------------------------------------------------------------------
-- Error Messages

strPos p =
  let (i,j) = (toString p.line, toString p.col) in
  "(Line:" ++ i ++ " Col:" ++ j ++ ")"


------------------------------------------------------------------------------
-- Abstract Syntax Helpers

-- NOTE: the Exp builders use dummyPos

-- val : Val_ -> Val
-- val = flip Val (Provenance [] dummyExp)

builtinVal: String -> Val_ -> Val
builtinVal msg x = Val x (Provenance [] (withDummyExpInfo (EVar space0 "msg" )) []) (Parents [])

exp_ : Exp__ -> Exp_
exp_ e__ = makeExp_ e__ (-1)

pat_ : Pat__ -> Pat_
pat_ p__ = makePat_ p__ (-1)

type_ : Type__ -> Type_
type_ t__ = makeType_ t__ (-1)

withDummyRange x            = WithInfo x dummyPos dummyPos
withDummyPatInfo p__        = WithInfo (pat_ p__) dummyPos dummyPos
withDummyExp_Info e__       = WithInfo (exp_ e__) dummyPos dummyPos
withDummyExpInfo e__        = Expr <| withDummyExp_Info e__
withDummyPatInfoPId pid p__ = WithInfo (makePat_ p__ pid) dummyPos dummyPos
withDummyExpInfoEId eid e__ = withDummyRange (makeExp_ e__ eid)
withDummyBranchInfo b_      = WithInfo b_ dummyPos dummyPos
withDummyTypeInfo t__       = WithInfo (type_ t__) dummyPos dummyPos

replaceE__ : Exp -> Exp__ -> Exp
replaceE__ (Expr e) e__ = let e_ = e.val in Expr { e | val = { e_ | e__ = e__ } }

mapNodeE__ : (Exp__ -> Exp__ ) -> Exp -> Exp
mapNodeE__ f e = replaceE__ e (f (unwrapExp e))

replaceP__ : Pat -> Pat__ -> Pat
replaceP__ p p__ = let p_ = p.val in { p | val = { p_ | p__ = p__ } }

mapNodeP__ : (Pat__ -> Pat__ ) -> Pat -> Pat
mapNodeP__ f p = replaceP__ p (f p.val.p__)

replaceB__ : Branch -> Branch_ -> Branch
replaceB__ b b_ = { b | val = b_ }

replaceTB__ : TBranch -> TBranch_ -> TBranch
replaceTB__ b b_ = { b | val = b_ }

replaceT__: Type -> Type__ -> Type
replaceT__ t t__ = let t_ = t.val in { t | val = { t_ | t__ = t__ } }

replaceP__PreservingPrecedingWhitespace  : Pat -> Pat__ -> Pat
replaceP__PreservingPrecedingWhitespace  p p__ =
  replaceP__ p p__ |> replacePrecedingWhitespacePat (precedingWhitespacePat p)

replaceE__PreservingPrecedingWhitespace : Exp -> Exp__ -> Exp
replaceE__PreservingPrecedingWhitespace e e__ =
  replaceE__ e e__ |> replacePrecedingWhitespace (precedingWhitespace e)

replaceBranchExp : Branch -> Exp -> Branch
replaceBranchExp branch exp =
  let (Branch_ bws1 p _ bws2) = branch.val in
  { branch | val = Branch_ bws1 p exp bws2 }

replaceTBranchExp : TBranch -> Exp -> TBranch
replaceTBranchExp tbranch exp =
  let (TBranch_ tbws1 tipe _ tbws2) = tbranch.val in
  { tbranch | val = TBranch_ tbws1 tipe exp tbws2 }

setEId : EId -> Exp -> Exp
setEId eid (Expr e) = let e_ = e.val in Expr { e | val = { e_ | eid = eid } }

setPId : PId -> Pat -> Pat
setPId pid p = let p_ = p.val in { p | val = { p_ | pid = pid } }

setTId : TId -> Type -> Type
setTId tid t = let t_ = t.val in { t | val = { t_ | tid = tid } }

clearEId e = setEId -1 e
clearPId p = setPId -1 p

clearPIds p = mapPatTopDown clearPId p

mapLetPats: (Pat -> Pat) -> LetExp -> LetExp
mapLetPats f (LetExp mbc sp0 name     funStyle spEq e1) =
              LetExp mbc sp0 (f name) funStyle spEq e1

mapAnnotPat: (Pat -> Pat) -> LetAnnotation -> LetAnnotation
mapAnnotPat f (LetAnnotation mbc sp0 name funStyle spEq t1) =
               LetAnnotation mbc sp0 (f name) funStyle spEq t1

clearNodeIds e =
  let eidCleared = clearEId e in
  case unwrapExp eidCleared of
    EConst ws n (locId, annot, ident) wd  -> replaceE__ eidCleared (EConst ws n (0, annot, "") wd)
    ELet ws1 letkind (Declarations printOrder tps annots exps) s body ->
      replaceE__ eidCleared (ELet ws1 letkind (
        Declarations printOrder tps (List.map (mapAnnotPat clearPIds) annots) (regroup exps <| List.map (mapLetPats clearPIds) <| elemsOf exps)) s body)
    EFun ws1 pats body ws2                -> replaceE__ eidCleared (EFun ws1 (List.map clearPIds pats) body ws2)
    ECase ws1 scrutinee branches ws2      -> replaceE__ eidCleared (ECase ws1 scrutinee (mapBranchPats clearPIds branches) ws2)
    _                                     -> eidCleared

dummyLoc_ b = (0, b, "")
dummyTrace_ b = TrLoc (dummyLoc_ b)

dummyLoc     = dummyLoc_ unann
dummyTrace   = dummyTrace_ unann
dummyProvenance = Provenance [] (eTuple0 []) []

-- TODO interacts badly with auto-abstracted variable names...
dummyLocWithDebugInfo : Frozen -> Num -> Loc
dummyLocWithDebugInfo b n = (0, b, "")

eOp op_ es = withDummyExpInfo <| EOp space1 space1 (withDummyRange op_) es space0

ePlus e1 e2 = eOp Plus [e1, e2]
eMinus e1 e2 = eOp Minus [e1, e2]

eBool  = withDummyExpInfo << EBase space1 << EBool
eStr   = withDummyExpInfo << EBase space1 << EString "\"" -- defaultQuoteChar
eStr0  = withDummyExpInfo << EBase space0 << EString "\"" -- defaultQuoteChar
eTrue  = eBool True
eFalse = eBool False
eNull  = withDummyExpInfo <| EBase space1 <| ENull
eIf c t e   = withDummyExpInfo <| EIf space0 c space1 t space1 e space0

eApp (Expr e) es = Expr <| withInfo (exp_ <| EApp space1 (Expr e) es SpaceApp space0) e.start (Utils.maybeLast es |> Maybe.map (\(Expr e_) -> e_.end) |> Maybe.withDefault e.end)
eCall fName es   = eApp (eVar0 fName) es
eFun ps e        = withDummyExpInfo <| EFun space1 ps e space0
eRecord kvs      = withDummyExpInfo <| eRecord__ space1 Nothing (List.map
  (\(k, v) -> (Just space0, space1, k, space1, v))
  kvs) space1
eSelect e name = withDummyExpInfo  <| ESelect space0 e space0 space0 name

recordEntriesFromDeclarations: Declarations -> Maybe (List (Maybe WS, WS, Ident, WS, Exp))
recordEntriesFromDeclarations (Declarations _ _ _ letexps) =
  letexps |> elemsOf |> List.map (\(LetExp wsComma wsBefore p _ wsEq e) ->
    case p.val.p__ of
      PVar ws ident _ ->
       Just (wsComma, wsBefore, ident, wsEq, e)
      _ -> Nothing
  ) |> Utils.projJusts

-- Given a declarations, what are the public record keys it builds.
recordKeys: Declarations -> List Ident
recordKeys (Declarations _ _ _ exps) =
  exps |> List.concatMap (\(isRec, letexps) ->
    List.concatMap (\(LetExp _ _ p _ _ _) -> identifiersListInPat p) letexps
    )

tApp a b c d = withDummyTypeInfo <| TApp a b c d

desugarEApp e es = case es of
  []      -> Debug.crash "desugarEApp"
  [e1]    -> eApp e [e1]
  e1::es_ -> desugarEApp (eApp e [e1]) es_

desugarEFun ps e = case ps of
  []      -> Debug.crash "desugarEFun"
  [p]     -> eFun [p] e
  p::ps_  -> eFun [p] (desugarEFun ps_ e)

ePair e1 e2 = withDummyExpInfo <| EList space1 [e1,e2] space0 Nothing space0

noWidgetDecl = withDummyRange NoWidgetDecl

rangeSlider kind a b =
  withDummyRange <|
    kind (withDummyRange a) (withDummyRange "-") (withDummyRange b) Nothing False

intSlider = rangeSlider IntSlider
numSlider = rangeSlider NumSlider

colorNumberSlider = intSlider 0 499

-- See also LangTools.wrapWithLets
eLets xes eBody = case xes of
  (x,e)::xes_ -> eLet [(x,e)] (eLets xes_ eBody)
  []          -> eBody

-- Given [("a", aExp), ("b", bExp)] bodyExp
-- Produces (let [a b] [aExp bExp] bodyExp)
--
-- If given singleton list, produces a simple non-list let.
--
-- But you probably want to use LangTools.newLetFancyWhitespace.
eLetOrDef : LetKind -> List (Ident, Exp) -> Exp -> Exp
eLetOrDef letKind namesAndAssigns bodyExp =
  let (pat, assign) = patBoundExpOf namesAndAssigns in
  withDummyExpInfo <|
    ELet newline1 letKind (Declarations [0] [] []
      [(isBodyPossiblyRecursive assign, [LetExp Nothing space1 pat FunArgAsPats space1 assign])]) space1 bodyExp

patBoundExpOf : List (Ident, Exp) -> (Pat, Exp)
patBoundExpOf namesAndAssigns =
  case List.unzip namesAndAssigns of
    ([name], [assign]) -> (pVar name, replacePrecedingWhitespace " " assign)
    (names, assigns)   -> (pListOfPVars names, eList (setExpListWhitespace "" " " assigns) Nothing)

eLet : List (Ident, Exp) -> Exp -> Exp
eLet = eLetOrDef Let

eDef : List (Ident, Exp) -> Exp -> Exp
eDef = eLetOrDef Def

-- Previous definition
eLet__ wsStart letOrDef isRec name spEq binding spIn rest wsEnd =
  ELet wsStart letOrDef (Declarations [0] [] [] [
  (isBodyPossiblyRecursive binding, [LetExp Nothing space0 name FunArgAsPats spEq binding])]) spIn rest

-- Previous definition
eRecord__ wsBefore mbInit keyValues wsBeforeEnd =
  ERecord wsBefore mbInit
    (Declarations (List.range 0 (List.length keyValues - 1))
    []
    []
    (List.map (\(mbComma, spBefore,key,spEq,value) ->
      (isBodyPossiblyRecursive value, [LetExp mbComma spBefore (pVar key) FunArgAsPats spEq value])) keyValues)) wsBeforeEnd

eRecord__Unapply e__ = case e__ of
  ERecord wsBefore mbInit (Declarations po [] [] letexps) wsBeforeEnd ->
    letexps |> List.map (\(isRec, group) -> case group of
          [LetExp mbComma spBefore p FunArgAsPats spEq value] ->
            case pVarUnapply p of
              Just key -> Just (mbComma, spBefore, key, spEq, value)
              Nothing -> Nothing
          _ -> Nothing
      ) |> Utils.projJusts |> Maybe.map (\x -> (wsBefore, x, wsBeforeEnd))
  _ -> Nothing

eTypeAlias__ ws1 pat t rest wsEnd =
    ELet newline1 Def (Declarations [0]
      ([(False, [LetType Nothing ws1 (Just space1) pat FunArgAsPats space1 t])]) [] []) space1 rest

eTyp_ wsStart pat t rest wsEnd =
    ELet newline1 Def (Declarations [0] [] [LetAnnotation Nothing wsStart pat FunArgAsPats space1 t] []) space1 rest

eVar0 a           = withDummyExpInfo <| EVar space0 a
eVar a            = withDummyExpInfo <| EVar space1 a
eVarAt pos a      = withInfo (exp_ <| EVar space1 a) pos { pos | col = pos.col + String.length a}
eConst0 a b       = withDummyExpInfo <| EConst space0 a b noWidgetDecl
eConst a b        = withDummyExpInfo <| EConst space1 a b noWidgetDecl
eConstDummyLoc0 a = withDummyExpInfo <| EConst space0 a dummyLoc noWidgetDecl
eConstDummyLoc a  = withDummyExpInfo <| EConst space1 a dummyLoc noWidgetDecl
eList0 a b        = withDummyExpInfo <| EList space0 (List.map ((,) space0) a) space0 b space0
eList a b         = withDummyExpInfo <| EList space1 (List.map ((,) space0) a) space0 b space0
eListWs a b       = withDummyExpInfo <| EList space1 a space0 b space0
eSnapHoleVal0 v   = withDummyExpInfo <| EHole space0 (ESnapHole v)
eSnapHoleVal v    = withDummyExpInfo <| EHole space1 (ESnapHole v)
eEmptyHoleVal0    = withDummyExpInfo <| EHole space0 EEmptyHole
eEmptyHoleVal     = withDummyExpInfo <| EHole space1 EEmptyHole

eColonType (Expr e) t = withInfo (exp_ <| EColonType space1 (Expr e) space1 t space0) e.start t.end

pVar0 a        = withDummyPatInfo <| PVar space0 a noWidgetDecl
pVar a         = withDummyPatInfo <| PVar space1 a noWidgetDecl
pWildcard0     = withDummyPatInfo <| PWildcard space0
pWildcard ps   = withDummyPatInfo <| PWildcard space0
pList0 ps      = withDummyPatInfo <| PList space0 ps space0 Nothing space0
pList ps       = withDummyPatInfo <| PList space1 ps space0 Nothing space0
pAs x p        = withDummyPatInfo <| PAs space0 p space1 (withDummyPatInfo <| PVar space1 x noWidgetDecl)

pTuple: WS -> List (Maybe WS, Pat) -> WS -> Pat
pTuple spBeforeOpenParen keyValues spBeforeCloseParen =
  withDummyPatInfo <| pTuple__ spBeforeOpenParen keyValues spBeforeCloseParen

tupleEncodingApply: (String -> t) -> List (Maybe WS, t) -> List (Maybe WS, WS, Ident, WS, t)
tupleEncodingApply ctorEncoding values =
  ((ctorVal
    ((,) Nothing << ctorEncoding)
    TupleCtor (ctorTupleName (List.length values))
  )::(Utils.indexedMapFrom 1 numericalValEntry values)) |>
      List.map (\(key, (space, pat)) ->
        (space, space0, key, space0, pat)
      )

pTuple__: WS -> List (Maybe WS, Pat) -> WS -> Pat__
pTuple__ spBeforeOpenParen keyValues spBeforeCloseParen =
  case keyValues of
    [(spHead, head)] -> PParens spBeforeOpenParen head spBeforeCloseParen
    _ ->
      let patternList = tupleEncodingApply (withDummyPatInfo << PBase space0 << EString "\"") keyValues in
      PRecord spBeforeOpenParen patternList  spBeforeCloseParen

tupleEncodingUnapply: List (Maybe WS, WS, Ident, WS, t) -> Maybe (List (Maybe WS, t))
tupleEncodingUnapply keyValues =
  if recordEntriesToCtorKind keyValues == Just TupleCtor then
    keyValues
    |> List.filter
         ( \(_, _, elName, _, _) ->
             String.startsWith "_" elName
         )
    |> List.sortBy
         ( \(_, _, elName, _, _) ->
             elName
               |> String.dropLeft 1
               |> String.toInt
               |> Result.withDefault -1
         )
    |> List.map (\(spc, spn, n, spe, e) -> (spc, e))
    |> Utils.mapHead (\(spc, e) -> (Nothing, e))
    |> Just
  else Nothing

pTupleUnapply: Pat -> Maybe (WS, List (Maybe WS, Pat), WS)
pTupleUnapply pat =
  case pat.val.p__ of
    PParens spBeforeOpenParen head spBeforeCloseParen -> Just (spBeforeOpenParen, [(Nothing, head)], spBeforeCloseParen)
    PRecord spBeforeOpenParen keyValues spBeforeCloseParen ->
      case tupleEncodingUnapply keyValues of
        Nothing -> Nothing
        Just values -> Just (spBeforeOpenParen, values, spBeforeCloseParen)
    _ -> Nothing

eTuple0 a         = withDummyExpInfo <| eTuple__ space0 (
  List.indexedMap (\i x -> (if i == 0 then Nothing else Just space0, x)) a) space0
eTuple a          = withDummyExpInfo <| eTuple__ space1 (
  List.indexedMap (\i x -> (if i == 0 then Nothing else Just space0, x)) a) space0

eTuple__: WS -> List (Maybe WS, Exp) -> WS -> Exp__
eTuple__ spBeforeOpenParen keyValues spBeforeCloseParen =
  case keyValues of
    [(spHead, head)] -> EParens spBeforeOpenParen head Parens spBeforeCloseParen
    _ ->
      let elemList = tupleEncodingApply (withDummyExpInfo << EBase space0 << EString "\"") keyValues in
      eRecord__ spBeforeOpenParen Nothing elemList  spBeforeCloseParen

eTupleUnapply: Exp -> Maybe (WS, List (Maybe WS, Exp), WS)
eTupleUnapply e =
 case unwrapExp e of
    EParens spBeforeOpenParen head Parens spBeforeCloseParen -> Just (spBeforeOpenParen, [(Nothing, head)], spBeforeCloseParen)
    (ERecord _ Nothing _ _)  as e__->
      case eRecord__Unapply e__ of
        Just (spBeforeOpenParen, keyValues, spBeforeCloseParens) ->
          case tupleEncodingUnapply keyValues of
            Just values -> Just (spBeforeOpenParen, values, spBeforeCloseParens)
            Nothing -> Nothing
        Nothing ->  Nothing
    _ -> Nothing

pListOfPVars names = pList (listOfPVars names)

eLetUnapply: Exp -> Maybe ((Ident, Exp), Exp)
eLetUnapply e = case unwrapExp e of
  ELet _ _ (Declarations _ [] [] [(_, [LetExp _ _ p _ _ assign])]) _ bodyExp ->
    case p.val.p__ of
      PVar _ ident _ -> Just ((ident, assign), bodyExp)
      _ -> Nothing
  _ -> Nothing

eFunUnapply: Exp -> Maybe (List Pat, Exp)
eFunUnapply e = case (unwrapExp e) of
  EFun _ ps body _ -> Just (ps, body)
  _ -> Nothing

eVarUnapply e = case (unwrapExp e) of
  EVar _ s -> Just s
  _ -> Nothing

eStrUnapply e = case (unwrapExp e) of
  EBase _ (EString _ s) -> Just s
  _ -> Nothing

eConstUnapply e = case (unwrapExp e) of
  EConst _ n _ _ -> Just n
  _ -> Nothing

eListUnapply e = case (unwrapExp e) of
  EList _ elems _ Nothing  _-> Just <| List.map Tuple.second elems
  _ -> Nothing

eListUnapplyWS e = case (unwrapExp e) of
  EList _ elems _ Nothing  _-> Just <| elems
  _ -> Nothing

eOpUnapply1 expectedOp e = case (unwrapExp e) of
  EOp _ _ op [arg] _ -> if op.val == expectedOp then Just arg else Nothing
  _ -> Nothing

eOpUnapply2 expectedOp e = case (unwrapExp e) of
  EOp _ _ op [e1, e2] _ -> if op.val == expectedOp then Just (e1, e2) else Nothing
  _ -> Nothing


eAppUnapply1 e = case (unwrapExp e) of
  EApp _ e1 [e2] _ _ -> Just (e1, e2)
  _ -> Nothing

eAppUnapply2 e = case (unwrapExp e) of
  EApp _ e1 [e2, e3] _ _ -> Just (e1, e2, e3)
  _ -> Nothing

eAppUnapply e = case (unwrapExp e) of
  EApp _ e1 es _ _ -> Just (e1, es)
  _ -> Nothing

eParensUnapplyIf syntax e = case (unwrapExp e) of
  EParens _ inner s _ -> if s == syntax then Just inner else Nothing
  _ -> Nothing

vStrUnapply v = case v.v_ of
  VBase (VString s) -> Just s
  _ -> Nothing

vListUnapply v = case v.v_ of
  VList elems -> Just elems
  _ -> Nothing

vHtmlNodeUnapply v = case vListUnapply v of
  Just [tagVal, attrsVal, childrenVal] -> case vStringUnapply tagVal of
    Just tag -> Just (tag, attrsVal, childrenVal)
    _ -> Nothing
  _ -> Nothing

vStringUnapply v = case v.v_ of
  VBase (VString s) -> Just s
  _ -> Nothing

vRecordUnapplyField field v = case v.v_ of
  VRecord d -> Dict.get field d
  _ -> Nothing

vRecordTupleUnapply: Val -> Maybe ((String, Val), List (String, Val))
vRecordTupleUnapply v = case v.v_ of
  VRecord d ->
    let keyValues = Dict.toList d in
    keyValues |> Utils.maybeFind ctorTuple |> (\x -> case x of
      Nothing -> Nothing
      Just v->
        let orderedKeyValues = keyValues
             |> List.filter
                  ( \(key, value) ->
                      String.startsWith "_" key
                  )
             |> List.sortBy
                  ( \(key, value)  ->
                      key
                        |> String.dropLeft 1
                        |> String.toInt
                        |> Result.withDefault -1
                  )
        in Just ((ctorTuple, v), orderedKeyValues)
    )
  _ -> Nothing

pVarUnapply p = case p.val.p__ of
  PVar _ s _ -> Just s
  _ -> Nothing

tpVarUnapply p = case p.val of
  TPatVar _ s -> Just s

-- note: dummy ids...
-- vTrue    = vBool True
-- vFalse   = vBool False
-- vBool    = val << VBase << VBool
-- vStr     = val << VBase << VString
-- vConst   = val << VConst Nothing
-- vBase    = val << VBase
-- vList    = val << VList
-- vDict    = val << VDict

unwrapVList : Val -> Maybe (List Val_)
unwrapVList v =
  case v.v_ of
    VList vs -> Just <| List.map .v_ vs
    _        -> Nothing

-- TODO names/types

unwrapVList_ : String -> Val -> List Val_
unwrapVList_ s v = vListToVals s v |> List.map .v_

vListToVals : String -> Val -> List Val
vListToVals s v = case v.v_ of
  VList vs -> vs
  _        -> Debug.crash <| "vListToVals: " ++ s

unwrapVBaseString_ : String -> Val_ -> String
unwrapVBaseString_ s v_ = case v_ of
  VBase (VString k) -> k
  _                 -> Debug.crash <| "unwrapVBaseString_: " ++ s


eRaw__ = EVar
eRaw0  = eVar0
eRaw   = eVar

listOfRaw = listOfVars

listOfVars xs =
  case xs of
    []     -> []
    x::xs_ -> eVar0 x :: List.map eVar xs_

listOfPVars xs =
  case xs of
    []     -> []
    x::xs_ -> pVar0 x :: List.map pVar xs_

listOfNums ns =
  case ns of
    []     -> []
    n::ns_ -> eConst0 n dummyLoc :: List.map (flip eConst dummyLoc) ns_

-- listOfNums1 = List.map (flip eConst dummyLoc)

type alias AnnotatedNum = (Num, Frozen, WidgetDecl)
  -- may want to move this up into EConst

listOfAnnotatedNums : List AnnotatedNum -> List Exp
listOfAnnotatedNums list =
  case list of
    [] -> []
    (n,ann,wd) :: list_ ->
      withDummyExpInfo (EConst space0 n (dummyLoc_ ann) wd) :: listOfAnnotatedNums1 list_

listOfAnnotatedNums1 =
 List.map (\(n,ann,wd) -> withDummyExpInfo (EConst space1 n (dummyLoc_ ann) wd))

minMax x y             = (min x y, max x y)
minNumTr (a,t1) (b,t2) = if a <= b then (a,t1) else (b,t2)
maxNumTr (a,t1) (b,t2) = if a >= b then (a,t1) else (b,t2)
minMaxNumTr nt1 nt2    = (minNumTr nt1 nt2, maxNumTr nt1 nt2)

plusNumTr (n1,t1) (n2,t2)  = (n1 + n2, TrOp Plus [t1, t2])
minusNumTr (n1,t1) (n2,t2) = (n1 + n2, TrOp Minus [t1, t2])


------------------------------------------------------------------------------
-- Whitespace helpers
------------------------------------------------------------------------------

zeroWidthWSAfter : WithInfo a -> WS
zeroWidthWSAfter wi = withInfo "" wi.end wi.end

precedingWhitespace : Exp -> String
precedingWhitespace exp =
  precedingWhitespaceExp__ (unwrapExp exp)

newlineCount : String -> Int
newlineCount s = List.length (String.split "\n" s) - 1

extractIndentation : String -> String
extractIndentation string =
  String.split "\n" string
  |> List.drop 1 -- If no newline, consider that no indentation.
  |> Utils.maybeLast
  |> Maybe.withDefault ""
  |> tabsToSpaces


indentationOf : Exp -> String
indentationOf exp =
  extractIndentation (precedingWhitespace exp)

indentationOfBranch : Branch -> String
indentationOfBranch branch =
  extractIndentation (precedingWhitespaceBranch branch)

-- Given an EId in the program, what is the indentation on the line on which the expression starts?
indentationAt : EId -> Exp -> String
indentationAt eid program =
  case findWithAncestorsByEId program eid of
    Just ancestorsAndExp ->
      ancestorsAndExp
      |> List.reverse
      |> List.map precedingWhitespace
      |> Utils.mapFirstSuccess
          (\ws ->
            if String.contains "\n" ws
            then Just (extractIndentation ws)
            else Nothing
          )
      |> Maybe.withDefault ""

    Nothing ->
      ""

precedingWhitespaceBranch : Branch -> String
precedingWhitespaceBranch branch =
  let (Branch_ wsb _ _ _) = branch.val in
  wsb.val

precedingWhitespaceDeclarationWithInfo: Declaration -> WS
precedingWhitespaceDeclarationWithInfo decl = case decl of
  DeclExp (LetExp _ ws _ _ _ _) -> ws
  DeclType (LetType _ ws _ _ _ _ _) -> ws
  DeclAnnotation (LetAnnotation _ ws _ _ _ _) -> ws

mapWs: (String -> String) -> WS -> WS
mapWs f ws = replaceInfo ws (f ws.val)

mapPrecedingWhitespaceDeclaration: (String -> String) -> Declaration -> Declaration
mapPrecedingWhitespaceDeclaration f decl = case decl of
  DeclExp (LetExp spc ws p fs spe e1) -> DeclExp <| LetExp spc (mapWs f ws) p fs spe e1
  DeclType (LetType spc ws spa p fs spe e1) -> DeclType <| LetType spc (mapWs f ws) spa p fs spe e1
  DeclAnnotation (LetAnnotation spc ws p fs spe e1) -> DeclAnnotation <| LetAnnotation spc (mapWs f ws) p fs spe e1

precedingWhitespacePat : Pat -> String
precedingWhitespacePat pat = .val <| precedingWhitespaceWithInfoPat pat

precedingWhitespaceWithInfoPat : Pat -> WS
precedingWhitespaceWithInfoPat pat =
  case pat.val.p__ of
    PVar   ws1 ident wd        -> ws1
    PConst ws1 n               -> ws1
    PBase  ws1 v               -> ws1
    PWildcard ws1              -> ws1
    PList  ws1 ps ws2 rest ws3 -> ws1
    PRecord ws1 es ws2         -> ws1
    PAs    ws1 p1 ws2 p2       -> ws1
    PParens ws1 p ws2          -> ws1
    PColonType ws1 _ _ _       -> ws1

precedingWhitespaceWithInfoExp : Exp -> WS
precedingWhitespaceWithInfoExp e =
  precedingWhitespaceWithInfoExp__ (unwrapExp e)


precedingWhitespaceExp__ : Exp__ -> String
precedingWhitespaceExp__ e__ =
  (precedingWhitespaceWithInfoExp__ e__).val

precedingWhitespaceWithInfoExp__ : Exp__ -> WS
precedingWhitespaceWithInfoExp__ e__ =
  case e__ of
    EBase      ws v                             -> ws
    EConst     ws n l wd                        -> ws
    EVar       ws x                             -> ws
    EFun       ws1 ps e1 ws2                    -> ws1
    EApp       ws1 e1 es apptype  ws2           -> ws1
    EList      ws1 es ws2 rest ws3              -> ws1
    ERecord    ws1 mi es ws2                    -> ws1
    ESelect    ws1 e ws2 ws3 s                  -> ws1
    EOp        ws1 wso op es ws2                -> ws1
    EIf        ws1 e1 ws2 e2 ws3 e3 ws4         -> ws1
    ELet       ws1 kind decls ws body           -> ws1
    ECase      ws1 e1 bs ws2                    -> ws1
    EColonType ws1 e ws2 tipe ws3               -> ws1
    EParens    ws1 e pStyle ws2                 -> ws1
    EHole      ws h                             -> ws

precedingWhitespaceWithInfoType: Type -> WS
precedingWhitespaceWithInfoType t =
  case t.val.t__ of
    TNum ws -> ws
    TBool ws -> ws
    TString ws -> ws
    TNull ws -> ws
    TList ws _ _ -> ws
    TDict ws _ _ _ -> ws
    TTuple ws _ _ _ _ ->ws
    TRecord ws _ _ _ -> ws
    TArrow ws _ _ -> ws
    TUnion ws _ _ -> ws
    TApp ws _ _ _ -> ws
    TVar ws _ -> ws
    TForall ws _ _ _ -> ws
    TWildcard ws -> ws
    TParens ws _ _ -> ws

allWhitespaces : Exp -> List String
allWhitespaces exp =
  allWhitespaces_ exp |> List.map .val

maybeToList: Maybe a -> List a
maybeToList p = case p of
  Just p -> [p]
  Nothing -> []

allWhitespacesDecls: Declarations -> List WS
allWhitespacesDecls decls =
  List.concatMap (\def -> case def of
     DeclAnnotation (LetAnnotation sp1 sp0 name funStyle spEq tp) ->
       (maybeToList sp1) ++ [sp0] ++ allWhitespacesPat_ name ++ [spEq] ++ allWhitespacesType_ tp
     DeclExp (LetExp sp1 sp0 name funStyle spEq e1) ->
       (maybeToList sp1) ++ [sp0] ++ allWhitespacesPat_ name ++ [spEq] ++ allWhitespaces_ e1
     DeclType (LetType sp1 sp0 mbSpAlias name funStyle spEq tp) ->
       (maybeToList sp1) ++ [sp0] ++  (maybeToList mbSpAlias) ++
        allWhitespacesPat_ name ++ [spEq] ++ allWhitespacesType_ tp
    ) (getDeclarationsInOrder decls)

allWhitespaces_ : Exp -> List WS
allWhitespaces_ exp =
  let allWhitespacesBranch branch =
    let (Branch_ ws1 pat e ws2) = branch.val in
    [ws1] ++ allWhitespacesPat_ pat ++ allWhitespaces_ e ++ [ws2]
  in
  let allWhitespacesTBranch tbranch =
    let (TBranch_ ws1 tipe e ws2) = tbranch.val in
    [ws1] ++ allWhitespacesType_ tipe ++ allWhitespaces_ e ++ [ws2]
  in
  case (unwrapExp exp) of
    EBase      ws v                         -> [ws]
    EConst     ws n l wd                    -> [ws]
    EVar       ws x                         -> [ws]
    EFun       ws1 ps e1 ws2                -> [ws1] ++ List.concatMap allWhitespacesPat_ ps ++ allWhitespaces_ e1 ++ [ws2]
    EApp       ws1 e1 es SpaceApp ws3       -> [ws1] ++ List.concatMap allWhitespaces_ (e1::es) ++ [ws3]
    EApp       ws1 e1 es (LeftApp ws2) ws3  -> [ws1] ++ allWhitespaces_ e1 ++ [ws2] ++ List.concatMap allWhitespaces_ es ++ [ws3]
    EApp       ws1 e1 es (RightApp ws2) ws3 -> [ws1] ++ List.concatMap allWhitespaces_ es ++ [ws2] ++ allWhitespaces_ e1 ++ [ws3]
    EApp       ws1 e1 [g1, g2] InfixApp ws3  -> [ws1] ++ allWhitespaces_ g1 ++ allWhitespaces_ e1 ++ allWhitespaces_ g2 ++ [ws3]
    EApp       ws1 e1 es InfixApp ws3        -> [ws1] ++ List.concatMap allWhitespaces_ (e1::es) ++ [ws3]
    EList      ws1 es ws2 rest ws3          -> [ws1]
                                                 ++ List.concatMap (\(ws_i,e_i) -> ws_i :: allWhitespaces_ e_i) es
                                                 ++ [ws2] ++ (rest |> Maybe.map allWhitespaces_ |> Maybe.withDefault []) ++ [ws3]
    ERecord    ws1 mi decls ws2                -> [ws1]
                                                 ++ (mi |> Maybe.map (Tuple.first >> allWhitespaces_) |> Maybe.withDefault [])
                                                 ++ allWhitespacesDecls decls
                                                 ++ [ws2]
    ESelect    ws0 e ws1 ws2 s              -> allWhitespaces_ e ++ [ws1, ws2]
    EOp        ws1 wso op es ws2            -> [ws1] ++ [wso] ++ List.concatMap allWhitespaces_ es ++ [ws2]
    EIf        ws1 e1 ws2 e2 ws3 e3 ws4     -> [ws1] ++ allWhitespaces_ e1
                                                 ++ [ws2] ++ allWhitespaces_ e2
                                                 ++ [ws3] ++ allWhitespaces_ e3
                                                 ++ [ws4]
    ELet        ws1 kind decls ws2 body     -> [ws1] ++ allWhitespacesDecls decls ++ [ws2] ++ allWhitespaces_ body
    ECase      ws1 e1 bs ws2                -> [ws1] ++ allWhitespaces_ e1 ++ List.concatMap allWhitespacesBranch bs ++ [ws2]
    EColonType ws1 e ws2 tipe ws3           -> [ws1] ++ allWhitespaces_ e ++ [ws2] ++ allWhitespacesType_ tipe ++ [ws2]
    EParens    ws1 e pStyle ws2             -> [ws1] ++ allWhitespaces_ e ++ [ws2]
    EHole      ws h                         -> [ws]


allWhitespacesPat : Pat -> List String
allWhitespacesPat pat =
  allWhitespacesPat_ pat |> List.map .val


allWhitespacesPat_ : Pat -> List WS
allWhitespacesPat_ pat =
  case pat.val.p__ of
    PVar   ws ident wd         -> [ws]
    PConst ws n                -> [ws]
    PBase  ws v                -> [ws]
    PWildcard ws               -> [ws]
    PList  ws1 ps ws2 rest ws3 -> [ws1] ++ List.concatMap allWhitespacesPat_ ps ++ [ws2] ++ (rest |> Maybe.map allWhitespacesPat_ |> Maybe.withDefault []) ++ [ws3]
    PRecord ws1 ps ws2         -> [ws1] ++ List.concatMap allWhitespacesPat_ (Utils.recordValues ps) ++ [ws2]
    PAs    ws1 p1 ws2 p2       -> [ws1] ++ allWhitespacesPat_ p1 ++ [ws2] ++ allWhitespacesPat_ p2
    PParens ws1 p ws2          -> [ws1, ws2] ++ allWhitespacesPat_ p
    PColonType ws1 p ws2 tp    -> [ws1, ws2] ++ allWhitespacesPat_ p ++ allWhitespacesType_ tp

allWhitespacesType_ : Type -> List WS
allWhitespacesType_ tipe =
  let allWhitespacesForTPat tpat =
    case tpat.val of
       TPatVar ws _ -> [ws]
  in
    case tipe.val.t__ of
      TNum ws                             -> [ws]
      TBool ws                            -> [ws]
      TString ws                          -> [ws]
      TNull ws                            -> [ws]
      TList ws1 elemType ws2              -> [ws1] ++ allWhitespacesType_ elemType ++ [ws2]
      TRecord ws1 mb2 listWsIdWsExpWs ws2 -> [ws1] ++ (case mb2 of
        Nothing -> []
        Just (_, wsm) -> [wsm]) ++ List.concatMap allWhitespacesType_  (Utils.recordValues listWsIdWsExpWs) ++ [ws2]
      TDict ws1 keyType valueType ws2     -> [ws1] ++ allWhitespacesType_ keyType ++ allWhitespacesType_ valueType ++ [ws2]
      TTuple ws1 heads ws2 maybeTail ws3  -> [ws1] ++ List.concatMap allWhitespacesType_ heads ++ [ws2] ++ (maybeTail |> Maybe.map allWhitespacesType_ |> Maybe.withDefault []) ++ [ws3]
      TArrow ws1 ts ws2                   -> [ws1] ++ List.concatMap allWhitespacesType_ ts ++ [ws2]
      TUnion ws1 ts ws2                   -> [ws1] ++ List.concatMap allWhitespacesType_ ts ++ [ws2]
      TApp ws ident ts at                 -> [ws] ++ List.concatMap allWhitespacesType_ ts
      TVar ws ident                       -> [ws]
      TForall ws1 params tipe ws2         -> [ws1] ++ List.concatMap allWhitespacesForTPat params ++ allWhitespacesType_ tipe ++ [ws2]
      TWildcard ws                        -> [ws]
      TParens ws t ws2                    -> [ws] ++ allWhitespacesType_ t ++ [ws2]


addPrecedingWhitespace : String -> Exp -> Exp
addPrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\oldWs -> oldWs ++ newWs) exp


replacePrecedingWhitespace : String -> Exp -> Exp
replacePrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\_ -> newWs) exp


replacePrecedingWhitespacePat : String -> Pat -> Pat
replacePrecedingWhitespacePat newWs pat =
  mapPrecedingWhitespacePat (\_ -> newWs) pat


copyPrecedingWhitespace : Exp -> Exp -> Exp
copyPrecedingWhitespace source target =
  replacePrecedingWhitespace (precedingWhitespace source) target


copyPrecedingWhitespacePat : Pat -> Pat -> Pat
copyPrecedingWhitespacePat source target =
  replacePrecedingWhitespacePat (precedingWhitespacePat source) target

mapPrecedingWhitespace : (String -> String) -> Exp -> Exp
mapPrecedingWhitespace stringMap exp =
  let mapWs s = ws (stringMap s.val) in
  mapPrecedingWhitespaceWS mapWs exp

-- Does not recurse.
mapPrecedingWhitespaceWS : (WS -> WS) -> Exp -> Exp
mapPrecedingWhitespaceWS mapWs exp =
  let
    e__New =
      case (unwrapExp exp) of
        EBase      ws v                             -> EBase      (mapWs ws) v
        EConst     ws n l wd                        -> EConst     (mapWs ws) n l wd
        EVar       ws x                             -> EVar       (mapWs ws) x
        EFun       ws1 ps e1 ws2                    -> EFun       (mapWs ws1) ps e1 ws2
        EApp       ws1 e1 es apptype ws2            -> EApp       (mapWs ws1) e1 es apptype ws2
        EList      ws1 es ws2 rest ws3              -> EList      (mapWs ws1) es ws2 rest ws3
        ERecord    ws1 mi es ws2                    -> ERecord    (mapWs ws1) mi es ws2
        ESelect    ws1 e ws2 ws3 s                  -> ESelect    (mapWs ws1) e ws2 ws3 s
        EOp        ws1 wso op es ws2                -> EOp        (mapWs ws1) wso op es ws2
        EIf        ws1 e1 ws2 e2 ws3 e3 ws4         -> EIf        (mapWs ws1) e1 ws2 e2 ws3 e3 ws4
        ELet       ws1 kind decls ws2 body          -> ELet       (mapWs ws1) kind decls ws2 body
        ECase      ws1 e1 bs ws2                    -> ECase      (mapWs ws1) e1 bs ws2
        EColonType ws1 e ws2 tipe ws3               -> EColonType (mapWs ws1) e ws2 tipe ws3
        EParens    ws e pStyle ws2                  -> EParens    (mapWs ws) e pStyle ws2
        EHole      ws h                             -> EHole      (mapWs ws) h
  in
    replaceE__ exp e__New

mapPrecedingWhitespacePat : (String -> String) -> Pat -> Pat
mapPrecedingWhitespacePat stringMap pat =
  let mapWs s = ws (stringMap s.val) in
  mapPrecedingWhitespacePatWS mapWs pat

mapPrecedingWhitespacePatWS : (WS -> WS) -> Pat -> Pat
mapPrecedingWhitespacePatWS mapWs pat =
  let
    p__ =
      case pat.val.p__ of
        PVar   ws ident wd         -> PVar   (mapWs ws) ident wd
        PConst ws n                -> PConst (mapWs ws) n
        PBase  ws v                -> PBase  (mapWs ws) v
        PWildcard ws               -> PWildcard (mapWs ws)
        PList  ws1 ps ws2 rest ws3 -> PList  (mapWs ws1) ps ws2 rest ws3
        PRecord ws1 ps ws2         -> PRecord (mapWs ws1) ps ws2
        PAs    ws1 p1 ws2 p2       -> PAs    (mapWs ws1) p1 ws2 p2
        PParens ws1 p ws2          -> PParens (mapWs ws1) p ws2
        PColonType ws1 p ws2 tp    -> PColonType (mapWs ws1) p ws2 tp
  in
    replaceP__ pat p__

mapPrecedingWhitespaceType: (String -> String) -> Type -> Type
mapPrecedingWhitespaceType stringMap tpe =
  let mapWs s = ws (stringMap s.val) in
  mapPrecedingWhitespaceTypeWS mapWs tpe

mapPrecedingWhitespaceTypeWS: (WS -> WS) -> Type -> Type
mapPrecedingWhitespaceTypeWS f tp =
  let t__ =
    case tp.val.t__ of
       TNum ws -> TNum (f ws)
       TBool ws -> TBool (f ws)
       TString ws -> TString (f ws)
       TNull ws -> TNull (f ws)
       TList ws a b -> TList (f ws) a b
       TDict ws a b c -> TDict (f ws) a b c
       TTuple ws a b c d -> TTuple (f ws) a b c d
       TRecord ws c a b -> TRecord (f ws) c a b
       TArrow ws a b -> TArrow (f ws) a b
       TUnion ws a b -> TUnion (f ws) a b
       TApp ws a b c -> TApp (f ws) a b c
       TVar ws a -> TVar (f ws) a
       TForall ws a b c -> TForall (f ws) a b c
       TWildcard ws -> TWildcard (f ws)
       TParens ws a b -> TParens (f ws) a b
  in
    replaceT__ tp t__

ensureWhitespace : String -> String
ensureWhitespace s =
  if s == "" then " " else s


ensureWhitespaceExp : Exp -> Exp
ensureWhitespaceExp exp =
  mapPrecedingWhitespace ensureWhitespace exp


ensureWhitespacePat : Pat -> Pat
ensureWhitespacePat pat =
  mapPrecedingWhitespacePat ensureWhitespace pat

ensureNoWhitespacePat : Pat -> Pat
ensureNoWhitespacePat pat =
  mapPrecedingWhitespacePat (\_ -> "") pat

ensureWhitespaceNNewlines : Int -> String -> String
ensureWhitespaceNNewlines n s =
  let nlCount = newlineCount s in
  String.repeat (n - nlCount) "\n" ++ s
  |> ensureWhitespace

-- whitespaceTwoNewlines : String -> String
-- whitespaceTwoNewlines string =
--   "\n\n" ++ extractIndentation string

ensureWhitespaceNewlineExp : Exp -> Exp
ensureWhitespaceNewlineExp exp =
  ensureWhitespaceNNewlinesExp 1 exp

ensureWhitespaceNNewlinesExp : Int -> Exp -> Exp
ensureWhitespaceNNewlinesExp n exp =
  mapPrecedingWhitespace (ensureWhitespaceNNewlines n) exp

-- whitespaceTwoNewlinesExp : Exp -> Exp
-- whitespaceTwoNewlinesExp exp =
--   mapPrecedingWhitespace whitespaceTwoNewlines

ensureNNewlines : Int -> String -> String -> String
ensureNNewlines n indentationIfNoPreviousNewlines ws =
  let previousNewlineCount = List.length (String.split "\n" ws) - 1 in
  if previousNewlineCount == 0 then
    String.repeat n "\n" ++ indentationIfNoPreviousNewlines ++ ws
  else if previousNewlineCount < n then
    String.repeat n "\n" ++
      indent indentationIfNoPreviousNewlines (removeIndentation
        (minIndentation maxIndentation ws) ws)
  else
    ws

ensureNNewlinesExp : Int -> String -> Exp -> Exp
ensureNNewlinesExp n indentationIfNoPreviousNewlines exp =
  mapPrecedingWhitespace (ensureNNewlines n indentationIfNoPreviousNewlines) exp


-- If exp is multline or a let/def:
--   Make sure preceeding whitespace contains a newline.
--   Indent to given indentation level.
-- Otherwise:
--   Ensure preceeding whitespace is at least one space character.
ensureWhitespaceSmartExp : Int -> String -> Exp -> Exp
ensureWhitespaceSmartExp newlineCountIfMultiline indentationIfMultiline exp =
  if isLet exp || expHasNewlines exp then
    exp
    |> ensureWhitespaceNNewlinesExp newlineCountIfMultiline
    |> replaceIndentation indentationIfMultiline
  else
    ensureWhitespaceExp exp


expHasNewlines : Exp -> Bool
expHasNewlines exp =
  List.any (String.contains "\n") (allWhitespaces exp)


patHasNewlines : Pat -> Bool
patHasNewlines pat =
  List.any (String.contains "\n") (allWhitespacesPat pat)


setExpListWhitespace : String -> String -> List Exp -> List Exp
setExpListWhitespace firstWs sepWs exps =
  case exps of
    []                  -> []
    firstExp::laterExps ->
      replacePrecedingWhitespace firstWs firstExp :: List.map (replacePrecedingWhitespace sepWs) laterExps


setPatListWhitespace : String -> String -> List Pat -> List Pat
setPatListWhitespace firstWs sepWs pats =
  case pats of
    []                  -> []
    firstPat::laterPats ->
      replacePrecedingWhitespacePat firstWs firstPat :: List.map (replacePrecedingWhitespacePat sepWs) laterPats


-- copyListWhitespace is in LangTools to access the unparser
-- copyListWhitespace : Exp -> Exp -> Exp
-- copyListWhitespace templateList list =
--   case (unwrapExp templateList, unwrapExp list) of
--     (EList ws1 _ ws2 _ ws3, EList _ heads _ maybeTail _) ->
--       replaceE__ list (EList ws1 heads ws2 maybeTail ws3)
--
--     _ ->
--       Debug.crash <| "Lang.copyListWs expected lists, but given " ++ unparseWithIds templateList ++ " and " ++ unparseWithIds list


imitateExpListWhitespace : List Exp -> List Exp -> List Exp
imitateExpListWhitespace oldExps newExps =
  imitateExpListWhitespace_ oldExps "" newExps


-- nextWs is to handle empty multiline list
-- e.g. "[\n]" nextWs is "\n"
imitateExpListWhitespace_ : List Exp -> String -> List Exp -> List Exp
imitateExpListWhitespace_ oldExps nextWs newExps =
  let (firstWs, sepWs) =
    case oldExps of
       first::second::_ -> (precedingWhitespace first, precedingWhitespace second)
       first::[]        -> (precedingWhitespace first, if precedingWhitespace first == "" then " " else precedingWhitespace first)
       []               -> if String.contains "\n" nextWs then (indent "  " nextWs, indent "  " nextWs) else ("", " ")
  in
  case newExps of
    [] ->
      []

    first::rest ->
      let firstWithNewWs = replacePrecedingWhitespace firstWs first in
      let restWithNewWs =
        rest
        |> List.map
            (\e ->
              if precedingWhitespace e == "" then
                replacePrecedingWhitespace sepWs e
              else if List.member e oldExps then
                e
              else
                replacePrecedingWhitespace sepWs e
            )
      in
      firstWithNewWs :: restWithNewWs


imitatePatListWhitespace : List Pat -> List Pat -> List Pat
imitatePatListWhitespace oldPats newPats =
  let (firstWs, sepWs) =
    case oldPats of
       first::second::_ -> (precedingWhitespacePat first, precedingWhitespacePat second)
       first::[]        -> (precedingWhitespacePat first, if precedingWhitespacePat first == "" then " " else precedingWhitespacePat first)
       []               -> ("", " ")
  in
  case newPats of
    [] ->
      []

    first::rest ->
      let firstWithNewWs = replacePrecedingWhitespacePat firstWs first in
      let restWithNewWs =
        rest
        |> List.map
            (\p ->
              if precedingWhitespacePat p == "" then
                replacePrecedingWhitespacePat sepWs p
              else if List.member p oldPats then
                p
              else
                replacePrecedingWhitespacePat sepWs p
            )
      in
      firstWithNewWs :: restWithNewWs


-- 4 spaces per tab.
tabsToSpaces : String -> String
tabsToSpaces ws = Regex.replace Regex.All (Regex.regex "\t") (\_ -> "    ") ws


-- The indentation difference from exp1 to exp2 is applied to exp.
copyIndentationChange : Exp -> Exp -> Exp -> Exp
copyIndentationChange exp1 exp2 exp =
  let delta = String.length (indentationOf exp2 |> tabsToSpaces) - String.length (indentationOf exp1 |> tabsToSpaces) in
  applyIndentationDelta delta exp


-- Add/remove n spaces of indentation.
applyIndentationDelta : Int -> Exp -> Exp
applyIndentationDelta delta exp =
  let processWS ws =
    ws
    |> Regex.replace
        (Regex.AtMost 1)
        (Regex.regex "\n[ \t]*$") -- $ in Javascript is end-of-string by default.
        (\match ->
          let priorSpacesCount = match.match |> String.dropLeft 1 |> tabsToSpaces |> String.length in
          "\n" ++ String.repeat (priorSpacesCount + delta) " "
        )
  in
  mapExp (mapPrecedingWhitespace processWS) exp


-- Unindents until an expression is flush to the edge, then adds spaces to the indentation.
replaceIndentation : String -> Exp -> Exp
replaceIndentation spaces exp =
  indentExp spaces (unindent exp)

replaceIndentationDeclaration: String -> Declaration -> Declaration
replaceIndentationDeclaration spaces decl=
  indentDeclaration spaces (unindentDeclaration decl)

removeTabs: Exp -> Exp
removeTabs = mapExp (mapPrecedingWhitespace tabsToSpaces)

minString s1 s2 = if String.length s1 < String.length s2 then s1 else s2

minIndentation: String -> String -> String
minIndentation prevMinIndent ws =
  Regex.find Regex.All (Regex.regex "\n( *)") ws
  |> List.map .submatches
  |> List.concat
  |> List.foldl (\mba minIndent -> case mba of
        Nothing -> minIndent
        Just a -> minString minIndent a
    ) prevMinIndent

maxIndentation = String.repeat 100 " "

minIndentationExp: String -> Exp -> String
minIndentationExp prevMinIndent exp =
  foldExpViaE__
   (\e__ smallest ->
     minIndentation smallest (precedingWhitespaceExp__ e__))
   prevMinIndent exp

minIndentationType: String -> Type -> String
minIndentationType prevMinIndent tpe =
  allWhitespacesType_ tpe
  |> List.foldl (\{val} minIndent -> minIndentation minIndent val) prevMinIndent

minIndentationListMaybeWS: String -> List (Maybe WS) -> String
minIndentationListMaybeWS prevMinIndent l = case l of
  [] -> prevMinIndent
  Just head :: tail -> minIndentationListMaybeWS  (minIndentation prevMinIndent head.val) tail
  Nothing :: tail -> minIndentationListMaybeWS prevMinIndent tail

minIndentationDeclaration: String -> Declaration -> String
minIndentationDeclaration prevMinIndent decl =
  let newPrevMinIndent = minIndentation prevMinIndent (precedingWhitespaceDeclarationWithInfo decl |> .val) in
  case decl of
    DeclExp (LetExp sc s _ _ se e1) ->
      minIndentationExp (minIndentationListMaybeWS newPrevMinIndent [sc, Just s, Just se]) e1
    DeclAnnotation (LetAnnotation sc s _ _ se t1) ->
      minIndentationType (minIndentationListMaybeWS newPrevMinIndent [sc, Just s, Just se]) t1
    DeclType (LetType sc s sa _ _ se t1) ->
      minIndentationType (minIndentationListMaybeWS newPrevMinIndent [sc, Just s, sa, Just se]) t1

removeIndentation: String -> String -> String
removeIndentation smallestIndentation ws =
  ws |> Regex.replace Regex.All (Regex.regex ("\n" ++ smallestIndentation)) (\_ -> "\n")

removeIndentationWS: String -> WS -> WS
removeIndentationWS smallestIndentation ws = mapWs (removeIndentation smallestIndentation) ws

removeIndentationMaybeWS: String -> Maybe WS -> Maybe WS
removeIndentationMaybeWS smallestIndentation = Maybe.map (removeIndentationWS smallestIndentation)

removeIndentationExp: String -> Exp -> Exp
removeIndentationExp smallestIndentation exp =
  mapExp (mapPrecedingWhitespace (removeIndentation smallestIndentation)) exp

removeIndentationType: String -> Type -> Type
removeIndentationType smallestIndentation tpe =
  mapType (mapPrecedingWhitespaceType (removeIndentation smallestIndentation)) tpe

removeIndentationDeclaration: String -> Declaration -> Declaration
removeIndentationDeclaration si {-smallestIndentation-} decl =
  case decl of
    DeclExp (LetExp sc s p fs se e1) ->
      DeclExp (LetExp (removeIndentationMaybeWS si sc)
        (removeIndentationWS si s) p fs (removeIndentationWS si se)
        (removeIndentationExp si e1))
    DeclAnnotation (LetAnnotation sc s p fs se t1) ->
      DeclAnnotation (LetAnnotation (removeIndentationMaybeWS si sc)
        (removeIndentationWS si s)
        p fs
        (removeIndentationWS si se)
        (removeIndentationType si t1))
    DeclType (LetType sc s sa p fs se t1) ->
      DeclType (LetType (removeIndentationMaybeWS si sc) (removeIndentationWS si s)
        (removeIndentationMaybeWS si sa) p fs (removeIndentationWS si se)
        (removeIndentationType si t1))

-- Finds lowest amount of indentation and then removes it from all expressions.
unindent : Exp -> Exp
unindent exp =
  let expWsAsSpaces = removeTabs exp in
  let smallestIndentation = minIndentationExp maxIndentation expWsAsSpaces in
  removeIndentationExp smallestIndentation expWsAsSpaces

unindentDeclaration: Declaration -> Declaration
unindentDeclaration decl =
  let smallestIndentation = minIndentationDeclaration maxIndentation decl in
  removeIndentationDeclaration smallestIndentation decl

indent : String -> String -> String
indent spaces ws =
  ws |> String.reverse
     |> Regex.replace (Regex.AtMost 1) (Regex.regex "\n") (\_ -> spaces ++ "\n")
     |> String.reverse

indentWs : String -> WS -> WS
indentWs spaces ws =
  replaceInfo ws (indent spaces ws.val)

indentMaybeWS: String -> Maybe WS -> Maybe WS
indentMaybeWS spaces = Maybe.map (indentWs spaces)

-- Increases indentation by spaces string.
indentExp : String -> Exp -> Exp
indentExp spaces e =
  mapExp (mapPrecedingWhitespaceWS (indentWs spaces)) e

indentType: String -> Type -> Type
indentType spaces t =
  mapType (mapPrecedingWhitespaceTypeWS (indentWs spaces)) t

indentDeclaration: String -> Declaration -> Declaration
indentDeclaration spaces decl =
  case decl of
    DeclExp (LetExp sc s p fs se e1) ->
      DeclExp (LetExp (indentMaybeWS spaces sc)
        (indentWs spaces s) p fs
        (indentWs spaces se)
        (indentExp spaces e1))
    DeclAnnotation (LetAnnotation sc s p fs se t1) ->
      DeclAnnotation (LetAnnotation (indentMaybeWS spaces sc)
        (indentWs spaces s)
        p fs
        (indentWs spaces se)
        (indentType spaces t1))
    DeclType (LetType sc s sa p fs se t1) ->
      DeclType (LetType (indentMaybeWS spaces sc) (indentWs spaces s)
        (indentMaybeWS spaces sa) p fs (indentWs spaces se)
        (indentType spaces t1))

-- Same as indent, but always push top level exp right even if no newline.
pushRight : String -> Exp -> Exp
pushRight spaces e =
  indentExp spaces e
  |> replacePrecedingWhitespace (precedingWhitespace e ++ spaces)

--------------------------------------------------------------------------------
-- Code Object Traversal
--------------------------------------------------------------------------------
-- NOTE: Much of this code is very similar to the `foldExp` code, but we also
--       need to fold on patterns (and possibly types) as well as target
--       positions for Deuce, so this code heavily draws from the earlier code
--       but makes it more general. The old code is left in for backward
--       compatibility.
--------------------------------------------------------------------------------
-- Zero-indexed declarations
-- BindingNumber is a pattern's position in an expression
-- For EFun it's the n+1-th pattern
-- For ELet it's the n+1-th declaration in the list LetType ++ LetAnnotations ++ LetExps
-- For ECase it's the n+1-th branch.
type alias BindingNumber = Int

{-
letexpByIndex: BindingNumber -> GroupsOf LetExp -> Maybe ((List LetExp, Bool, BindingNumber), GroupsOf LetExp)
letexpByIndex index letexps =
  case letexps of
    [] -> Nothing
    (isRec, l1) :: tail ->
      let i = List.length l1 in
      if index < i then
        Just ((l1, isRec, index), tail)
      else
        letexpByIndex (index - i) tail
-}

-- Functions that consider the group as a list of letexps
elemsOf: GroupsOf a -> List a
elemsOf groups = List.concatMap (\(_, l) -> l) groups

regroup: GroupsOf a -> List a -> GroupsOf a
regroup groups newElems =
  List.reverse <| Tuple.first <|
  Utils.foldLeft ([], newElems) groups <|
                \(revAcc, newElemsList) (isRec, oldElems) ->
     let (newGroup, newTail) = Utils.split (List.length oldElems) newElemsList in
     ((isRec, newGroup)::revAcc, newTail)

type CodeObject
  = E Exp -- Expression
  | P (Exp, BindingNumber) Pat -- Pattern; knows own parent and a path segment (BindingNumber for ELet, branch number for ECase, 0 for EFun)
  | T Type -- Type
  | D (WithInfo EId) BindingNumber -- Let binding equation (declaration)
  | DT BeforeAfter WS Exp BindingNumber -- Declaration Target (binding)
  | ET BeforeAfter WS Exp -- Exp target
  | PT BeforeAfter WS (Exp, BindingNumber) Pat -- Pat target (knows the parent of its target)
  | TT BeforeAfter WS Type -- Type target

extractInfoFromCodeObject : CodeObject -> WithInfo CodeObject
extractInfoFromCodeObject codeObject =
  case codeObject of
    E (Expr e)   -> replaceInfo e codeObject
    P _ p -> replaceInfo p codeObject
    T t   -> replaceInfo t codeObject
    D eid bindingNumber ->
             replaceInfo eid codeObject
    DT _ ws _ _ -> replaceInfo ws codeObject
    ET _ ws _   -> replaceInfo ws codeObject
    PT _ ws _ _ -> replaceInfo ws codeObject
    TT _ ws _   -> replaceInfo ws codeObject

isTarget : CodeObject -> Bool
isTarget codeObject =
  case codeObject of
    DT _ _ _ _ ->
      True
    ET _ _ _ ->
      True
    PT _ _ _ _ ->
      True
    TT _ _ _ ->
      True
    _ ->
      False

isTextSelectable : CodeObject -> Bool
isTextSelectable codeObject =
  not <| isTarget codeObject

isWord : CodeObject -> Bool
isWord codeObject =
  case codeObject of
    E e ->
      case (unwrapExp e) of
        (EConst _ _ _ _) ->
          True
        (EBase _ _) ->
          True
        (EVar _ _) ->
          True
        (EHole _ EEmptyHole) ->
          True
        _ ->
          False
    P _ p ->
      case p.val.p__ of
        (PVar _ _ _) ->
          True
        (PConst _ _) ->
          True
        (PBase _ _) ->
          True
        (PWildcard _) ->
          True
        _ ->
          False
    _ ->
      False

wsBefore : CodeObject -> WS
wsBefore codeObject =
  case codeObject of
    E e   -> precedingWhitespaceWithInfoExp e
    P e p -> precedingWhitespaceWithInfoPat p
    T t ->
      precedingWhitespaceWithInfoType t
    D eid bindingNum ->
      withInfo "" eid.start eid.end
    DT _ ws _ _ ->
      ws
    ET _ ws _ ->
      ws
    PT _ ws _ _ ->
      ws
    TT _ ws _ ->
      ws

modifyWsBefore : (WS -> WS) -> CodeObject -> CodeObject
modifyWsBefore f codeObject =
  case codeObject of
    E (Expr e) ->
      let
        eVal =
          e.val
        newE__ =
           mapPrecedingWhitespaceWS f (Expr e) |> unwrapExp
      in
        E <| Expr { e | val = { eVal | e__ = newE__ } }
    P e p ->
      let
        pVal =
          p.val
        newP__ = (mapPrecedingWhitespacePatWS f p) |> .val |> .p__
      in
        P e { p | val = { pVal | p__ = newP__ } }
    T t ->
      let
        newVal = mapPrecedingWhitespaceTypeWS f t |> .val
      in
        T { t | val = newVal }
    D eid bindingNum ->
      D eid bindingNum
    DT a ws b c ->
      DT a (f ws) b c
    ET a ws b ->
      ET a (f ws) b
    PT a ws b c ->
      PT a (f ws) b c
    TT a ws b ->
      TT a (f ws) b

isImplicitMain : Exp -> Bool
isImplicitMain e =
  case eLetUnapply e of
    Just (("_IMPLICIT_MAIN", _), _) -> True
    _ -> False

isHiddenCodeObject : CodeObject -> Bool
isHiddenCodeObject codeObject =
  case codeObject of
    E e ->
      isImplicitMain e
    ET _ _ e ->
      isImplicitMain e
    DT _ _ e _ ->
      isImplicitMain e
    _ ->
      False

splitBeforeWhitespace: WS -> (Maybe WS, WS)
splitBeforeWhitespace e1Ws =
  -- Altered Whitespace
  --
  -- In the following example programs, * represents the first space
  -- and ~ represents the second space
  --
  -- Case 1:
  --   x = 2 + 3****
  --   ~~~~~~
  --   ~y = x
  -- Case 2:
  --   x = 2 + 3,~~y = x
  if e1Ws.start.line == e1Ws.end.line then
    ( Nothing, e1Ws )
  -- Case 2
  else
    let
      breakpoint =
        { line =
            e1Ws.start.line + 1
          , col =
              1
        }
    in
      -- Note that col = 0 makes the Deuce polygon renderer extend
      -- the polygon to maxCol (desired).
      ( Just { e1Ws | end = { breakpoint | col = 0 } }
      , { e1Ws | start = breakpoint }
      )

declarationsCodeObjects : Exp -> Declarations -> List CodeObject
declarationsCodeObjects e declarations =
  let isTupleOrDatatype = expToCtorKind e |> Utils.maybeToBool in
  getDeclarationsInOrderWithIndex declarations |> List.concatMap (\(def, index) ->
    case def of
      DeclType (LetType mbSpColon sp1 mbSpAlias p1 funStyle spEq tp) ->
        let (spAfterPreviousDecl, spBeforeThisDecl) =
          if index == 0 then (Nothing, sp1)
          else case mbSpColon of
             Nothing -> splitBeforeWhitespace sp1
             Just _ -> (Nothing, sp1)
        in
        (Maybe.map (\spc -> [DT After spc e (index - 1)]) mbSpColon |> Maybe.withDefault []) ++
        (spAfterPreviousDecl |> Maybe.map (\sp -> [DT After sp e (index - 1)]) |> Maybe.withDefault []) ++
        [ DT Before spBeforeThisDecl e index,
          D (withInfo (expEId e) p1.start tp.end) index,
          P (e, index) p1,
          T tp,
          TT After (withInfo "" tp.end tp.end) tp
        ]
      DeclAnnotation (LetAnnotation mbSpColon sp1 p1 funStyle ws2 tp) ->
        let (spAfterPreviousDecl, spBeforeThisDecl) =
          if index == 0 then (Nothing, sp1)
          else splitBeforeWhitespace sp1
        in
        (Maybe.map (\spc -> [DT After spc e (index - 1)]) mbSpColon |> Maybe.withDefault []) ++
        (spAfterPreviousDecl |> Maybe.map (\sp -> [DT After sp e (index - 1)]) |> Maybe.withDefault []) ++
        [ DT Before spBeforeThisDecl e index
        , D (withInfo (expEId e) p1.start tp.end) index
        , P (e, index) p1
        , PT After ws2 (e, index) p1
        , T tp
        ]
      DeclExp (LetExp mbSpColon sp1 p1 funStyle ws2 (Expr e1)) ->
        ( if isTupleOrDatatype then
            []
          else
            let (spAfterPreviousDecl, spBeforeThisDecl) =
              if index == 0 then (Nothing, sp1)
              else splitBeforeWhitespace sp1
            in
            -- The space before the comma is always a target for the previous declaration.
            (Maybe.map (\spc -> [DT After spc e (index - 1)]) mbSpColon |> Maybe.withDefault []) ++
            (spAfterPreviousDecl |> Maybe.map (\sp -> [DT After sp e (index - 1)]) |> Maybe.withDefault []) ++
            [ DT Before spBeforeThisDecl e index
            , D (withInfo (expEId e) p1.start e1.end) index
            , P (e, index) p1
            , PT After ws2 (e, index) p1
            ]
        ) ++
        [ E <| Expr e1
        , ET After (zeroWidthWSAfter e1) <| Expr e1
        ]
  )

type WSBeforeComma_ a
  = ActualWS_ WS
  | GetterWS_ (a -> WS)

childCodeObjects : CodeObject -> List CodeObject
childCodeObjects co =
  let
    insertPrecedingWhitespaceTargetBeforeEachChild =
      List.concatMap (\child ->
        let wsb = wsBefore child in
        (case child of
          E e   -> [ET Before wsb e]
          P e p -> [PT Before wsb e p]
          T t ->   [TT Before wsb t]
          _ -> []
        ) ++
        [child]
      )
    genericListLike : (a -> CodeObject) -> (WS -> a -> CodeObject) -> List (WSBeforeComma_ a, a) -> (List CodeObject, Maybe a)
    genericListLike toCodeObject toAfterTarget itemsWithMBWSBeforeCommas =
      itemsWithMBWSBeforeCommas |>
        List.foldl
          (\(wsBeforeComma, cur) (accReversed, mbPrev) ->
            ( [ toCodeObject cur ] ++
              (case (mbPrev, wsBeforeComma) of
                (Nothing, _) -> []
                (Just prev, ActualWS_ ws) -> [ toAfterTarget ws prev ]
                (Just prev, GetterWS_ prevToWS) -> [ toAfterTarget (prevToWS prev) prev ]
              ) ++
              accReversed
              , Just cur
            )
          )
          ([], Nothing) |>
            Tuple.mapFirst List.reverse
    genericListLikeWithLastTarget : (a -> CodeObject) -> (WS -> a -> CodeObject) -> List (WSBeforeComma_ a, a) -> WS -> List CodeObject
    genericListLikeWithLastTarget toCodeObject toAfterTarget itemsWithMBWSBeforeCommas wsa =
      let (valueCOs, mbLastValue) = genericListLike toCodeObject toAfterTarget itemsWithMBWSBeforeCommas in
      valueCOs ++
      case mbLastValue of
        Nothing -> []
        Just lastValue -> [ toAfterTarget wsa lastValue ]
    genericList : (a -> CodeObject) -> (WS -> a -> CodeObject) -> List (WSBeforeComma_ a, a) -> WS -> Maybe a -> WS -> List CodeObject
    genericList toCodeObject toAfterTarget headsWithMBWSBeforeCommas ws2 mbTail ws3 =
      let (headsWithSucceedingWhitespace, lastHead) = genericListLike toCodeObject toAfterTarget headsWithMBWSBeforeCommas in
      headsWithSucceedingWhitespace ++
      case (mbTail, lastHead) of
        (Nothing, Nothing) ->
          []
        (Just tail, Nothing) ->
          [ toCodeObject tail ]
        (Nothing, Just lastHead) ->
          [ toAfterTarget ws3 lastHead ]
        (Just tail, Just lastHead) ->
          -- TODO it seems that in the current framework, it may be impossible
          -- to have a target immediately after the closing brace in a list
          -- that has a tail
          [ toAfterTarget ws2 lastHead
          , toCodeObject tail
          ]
    mbWSToWSBeforeComma mbWS =
      case mbWS of
        Nothing -> GetterWS_ zeroWidthWSAfter
        Just ws -> ActualWS_ ws
    genericRecord toCodeObject toAfterTarget recs =
      genericListLikeWithLastTarget toCodeObject toAfterTarget <| List.map (\(mbWS, _, _, _, r) -> (mbWSToWSBeforeComma mbWS, r)) recs
  in
  List.filter (not << isHiddenCodeObject) <|
    case co of
      E e ->
        insertPrecedingWhitespaceTargetBeforeEachChild <|
          case unwrapExp e of
            EFun _ ps e1 _ ->
              ( List.map (\(p, i) -> P (e, i) p) (Utils.zipWithIndex ps) ) ++
              ( case Utils.maybeLast ps of
                  Just pLast ->
                    [ PT After (zeroWidthWSAfter pLast) (e, List.length ps - 1) pLast
                    ]
                  Nothing ->
                    []
              ) ++
              [ E e1 ]
            EApp _ e1 es _ _ ->
              [ E e1 ] ++
              ( List.map E es )
            -- TODO only add targets after each operand if it's an infix operator
            EOp _ _ _ es _ ->
              es |>
                List.concatMap (\(Expr operand) ->
                  [ E <| Expr operand
                  , ET After (zeroWidthWSAfter operand) <| Expr operand
                  ]
                )
            EList _ es ws2 m ws3  ->
              genericList E (ET After) (List.map (Tuple.mapFirst ActualWS_) es) ws2 m ws3
            ERecord _ mbExpWs decls ws2 ->
              ( case mbExpWs of
                Just (eInit, wsAfterInit) ->
                  [ E eInit
                  , ET After wsAfterInit eInit
                  ]
                Nothing ->
                  []
              ) ++
              declarationsCodeObjects e decls
            ESelect _ e _ _ _ ->
              [ E e ]
            EIf _ (Expr e1) _ (Expr e2) _ e3 _ ->
              [ E <| Expr e1
              , ET After (zeroWidthWSAfter e1) <| Expr e1
              , E <| Expr e2
              , ET After (zeroWidthWSAfter e2) <| Expr e2
              , E e3
              ]
            ECase _ (Expr e1) branches _ ->
              [ E <| Expr e1
              , ET After (zeroWidthWSAfter e1) <| Expr e1
              ] ++
              ( List.concatMap
                ( \(b, i) -> b.val |> \(Branch_ _ branchP branchE branchBeforeArrow) ->
                  let (Expr branchE_) = branchE in
                  [ P (e, i) branchP
                  , PT After branchBeforeArrow (e, i) branchP
                  , E branchE
                  , ET After (zeroWidthWSAfter branchE_) branchE
                  ]
                )
                (Utils.zipWithIndex branches)
              )
            ELet ws1 letType decls ws3 e2 ->
              declarationsCodeObjects e decls ++
              [ E e2 ]
            EColonType _ e1 wsm t1 _ ->
              [ E e1
              , ET After wsm e1
              , T t1
              ]
            EParens _ e1 _ ws2 ->
              [ E e1
              , ET After ws2 e1
              ]
            _ -> []
      P e p ->
        insertPrecedingWhitespaceTargetBeforeEachChild <|
          case p.val.p__ of
            PList _ ps ws2 m ws3 ->
              genericList (P e) (\ws -> PT After ws e) (List.map ((,) <| GetterWS_ zeroWidthWSAfter) ps) ws2 m ws3
            PRecord _ ps ws2 ->
              genericRecord (P e) (\ws -> PT After ws e) ps ws2
            PAs _ p1 wsAs p2 ->
              [ P e p1
              -- TODO , PT After wsAs e p1
              -- TODO ^ does nothing - fix by deleting withZeroSpace
              , PT After (zeroWidthWSAfter p1) e p1
              , P e p2
              ]
            PParens _ p1 ws2 ->
              [ P e p1
              , PT After ws2 e p1
              ]
            PColonType _ p1 ws2 tp ->
              [ P e p1
              , PT After ws2 e p1
              , T tp
              ]
            _ -> []
      T t ->
        insertPrecedingWhitespaceTargetBeforeEachChild <|
          case t.val.t__ of
            TList _ t1 _ ->
              [ T t1 ]
            TDict _ t1 t2 _ ->
              [ T t1
              , T t2
              ]
            TTuple _ ts ws2 mbTail ws3 ->
              genericList T (TT After) (List.map ((,) <| GetterWS_ zeroWidthWSAfter) ts) ws2 mbTail ws3
            TRecord _ _ ts ws2 ->
              genericRecord T (TT After) ts ws2
            TArrow _ ts ws2 ->
              genericListLikeWithLastTarget T (TT After) (List.map ((,) <| GetterWS_ zeroWidthWSAfter) ts) ws2
            TUnion _ ts ws2 ->
              genericListLikeWithLastTarget T (TT After) (List.map ((,) <| GetterWS_ zeroWidthWSAfter) ts) ws2
            TApp _ _ ts _ ->
              List.map T ts
            TForall _ _ t1 ws2 ->
              [ T t1
              , TT After ws2 t1
              ]
            TParens _ t1 ws2 ->
              [ T t1
              , TT After ws2 t
              ]
            _ ->
              []
      D _ _ ->
        []
      DT _ _ _ _->
        []
      ET _ _ _ ->
        []
      PT _ _ _ _ ->
        []
      TT _ _ _ ->
        []

flattenToCodeObjects : CodeObject -> List CodeObject
flattenToCodeObjects codeObject =
  codeObject ::
    List.concatMap flattenToCodeObjects (childCodeObjects codeObject)

foldCode : (CodeObject -> a -> a) -> a -> CodeObject -> a
foldCode f acc code =
  List.foldl f acc (flattenToCodeObjects code)

-- Some helper functions that use CodeObjects

hasPid : PId -> CodeObject -> Bool
hasPid pid codeObject =
  case codeObject of
    P _ p ->
      p.val.pid == pid
    _ ->
      False

--------------------------------------------------------------------------------
-- Getting PathedPatternIds from PIds
--------------------------------------------------------------------------------
-- NOTE: Similar to DependenceGraph.foldPatternsWithIds
--------------------------------------------------------------------------------

rootPathedPatternId : ScopeId -> PathedPatternId
rootPathedPatternId scopeId =
  (scopeId, [])

tagPatList : PathedPatternId -> List Pat -> List (PId, PathedPatternId)
tagPatList (scopeId, path) =
  List.concat <<
    List.indexedMap
      ( \index pat ->
          tagSinglePat
            (scopeId, path ++ [index + 1])
            pat
      )

tagSinglePat : PathedPatternId -> Pat -> List (PId, PathedPatternId)
tagSinglePat ppid pat =
    let childPPID index = Tuple.mapSecond (\path -> path ++ [index]) ppid in
    (pat.val.pid, ppid) ::
      case pat.val.p__ of
        PConst _ _  ->
          []
        PBase _ _ ->
          []
        PVar _ _ _  ->
          []
        PWildcard _ ->
          []
        PAs _ p1 _ p2 ->
          tagSinglePat (childPPID 1) p1 ++ tagSinglePat (childPPID 2) p2
        PList _ ps _ Nothing _  ->
          tagPatList ppid ps
        PList _ ps _ (Just pTail) _ ->
          tagPatList ppid (ps ++ [pTail])
        PParens _ p1 _ ->
          tagSinglePat (childPPID 1) p1
        PRecord ws1 listWsIdWsExpWs ws2 ->
          tagPatList ppid (Utils.recordValues listWsIdWsExpWs)
        PColonType _ p1 _ _ ->
          tagSinglePat (childPPID 1) p1

tagBranchList
  :  EId
  -> List Branch
  -> List (PId, PathedPatternId)
tagBranchList eid =
  List.concat <<
    List.indexedMap
      ( \index branch ->
          case branch.val of
            Branch_ _ p _ _ ->
              tagSinglePat (rootPathedPatternId (eid, index + 1)) p
      )

-- Return the tags of the patterns of this current expression (not the children)
taggedExpPats : Exp -> List (PId, PathedPatternId)
taggedExpPats exp =
  case (unwrapExp exp) of
    EFun _ ps _ _ ->
      ps |>
      List.indexedMap (\i p ->
        tagSinglePat (rootPathedPatternId (expEId exp, i + 1)) p
      ) |>
      List.concatMap identity
    ECase _ _ branches _ ->
      tagBranchList (expEId exp) branches
    ELet _ _ defs _ _ ->
      List.concatMap (\(def, index) -> case def of
        DeclAnnotation (LetAnnotation _ _ p0 _ _ _) ->
          tagSinglePat (rootPathedPatternId (expEId exp, index)) p0
        DeclExp (LetExp _  _ p0 _ _ _) ->
          tagSinglePat (rootPathedPatternId (expEId exp, index)) p0
        DeclType (LetType _ _ _ p0 _ _ _) ->
          tagSinglePat (rootPathedPatternId (expEId exp, index)) p0
      ) (getDeclarations defs |> Utils.zipWithIndex)
    _ ->
      []

computePatMap : Exp -> Dict PId PathedPatternId
computePatMap =
  Dict.fromList << List.concatMap taggedExpPats << flattenExpTree

pidToPathedPatternId : Exp -> PId -> Maybe PathedPatternId
pidToPathedPatternId program pid =
  program
  |> mapFirstSuccessNode (taggedExpPats >> Utils.maybeFind pid)

--------------------------------------------------------------------------------
-- Dealing with top-level expressions
--------------------------------------------------------------------------------

-- A "nested expression" is a non-top-level expression
firstNestedExp : Exp -> Exp
firstNestedExp e =
  case (unwrapExp e) of
    ELet _ Def _ _ eRest ->
      firstNestedExp eRest
    _ ->
      e

--------------------------------------------------------------------------------
-- Fixing operators in Leo syntax
--------------------------------------------------------------------------------

--fixOps : Exp -> Exp
--fixOps =
--  let
--    fixOp e =
--      let
--        newE__ =
--          case (unwrapExp e) of
--            EApp ws1 function arguments ws2 ->
--              case function of
--                EVar wsBeforeIdentifier identifier ->
--                  case getOpFromIdentifier identifier of
--                    Just op ->
--                      EOp wsBeforeIdentifier op arguments ws2
--
--                _ ->
--                  (unwrapExp e)
--
--  in
--    map fixOp

branchPatExps : List Branch -> List (Pat, Exp)
branchPatExps branches =
  List.map
    (.val >> \(Branch_ _ pat exp _) -> (pat, exp))
    branches


identifiersListInPat : Pat -> List Ident
identifiersListInPat pat =
  case pat.val.p__ of
    PVar _ ident _              -> [ident]
    PList _ pats _ (Just pat) _ -> List.concatMap identifiersListInPat (pat::pats)
    PList _ pats _ Nothing    _ -> List.concatMap identifiersListInPat pats
    PAs _ p1 _ p2               -> (identifiersListInPat p1) ++ (identifiersListInPat p2)
    PRecord _ pvalues _         -> List.concatMap identifiersListInPat (Utils.recordValues pvalues)
    PConst _ _                  -> []
    PBase _ _                   -> []
    PWildcard _                 -> []
    PParens _ p _               -> identifiersListInPat p
    PColonType _ p _ _          -> identifiersListInPat p


identifiersListInPats : List Pat -> List Ident
identifiersListInPats pats =
  List.concatMap
    identifiersListInPat
    pats


-- All identifiers used or bound throughout the given exp
identifiersList : Exp -> List Ident
identifiersList exp =
  let folder e__ acc =
    case e__ of
       EVar _ ident ->
         ident::acc

       EFun _ pats _ _ ->
         (List.concatMap identifiersListInPat pats) ++ acc

       ECase _ _ branches _ ->
         let pats = branchPats branches in
         (List.concatMap identifiersListInPat pats) ++ acc

       ELet _ _ decls _ _ ->
           (getDeclarations decls |> List.concatMap (\def -> case def of
             DeclExp (LetExp _ _ pat _ _ _) ->
               (identifiersListInPat pat) ++ acc
             DeclType (LetType _ _ _ pat _ _ _) ->
               (identifiersListInPat pat) ++ acc
             DeclAnnotation (LetAnnotation _ _ pat _ _ _) ->
               (identifiersListInPat pat) ++ acc
              )) ++ acc
       _ ->
         acc
  in
  foldExpViaE__
    folder
    []
    exp



identifiersSet : Exp -> Set.Set Ident
identifiersSet exp =
  identifiersList exp
  |> Set.fromList


identifiersSetInPat : Pat -> Set.Set Ident
identifiersSetInPat pat =
  identifiersListInPat pat
  |> Set.fromList


identifiersSetInPats : List Pat -> Set.Set Ident
identifiersSetInPats pats =
  List.map identifiersSetInPat pats
  |> Utils.unionAll


expToMaybeIdent : Exp -> Maybe Ident
expToMaybeIdent exp =
  case (unwrapExp exp) of
    EVar _ ident -> Just ident
    _            -> Nothing

-- returns the idents declared by the ancestors of e
-- currently returned in no particular order and with duplicates
visibleIdents : Exp -> Exp -> Maybe (List Ident)
visibleIdents root e =
  expEId e |>
  findWithAncestorsByEId root |>
  Maybe.map (\eWithAncestors ->
    let eAndAncestorEIds = List.map expEId eWithAncestors |> Set.fromList in
    Utils.removeLastElement eWithAncestors |>
    List.concatMap (\ancestor ->
      case unwrapExp ancestor of
        EFun _ pats _ _ ->
          identifiersListInPats pats
        ECase _ _ branches _ ->
          let mbAncestorBranch =
            branches |>
            Utils.findFirst (\branch -> Set.member (branchExp branch |> expEId) eAndAncestorEIds)
          in
          case mbAncestorBranch of
            Nothing             -> []
            Just ancestorBranch -> branchPat ancestorBranch |> identifiersListInPat
        ELet _ _ (Declarations _ _ _ groupedExps) _ _ ->
          let accumulate remainingGroupedExps =
            case remainingGroupedExps of
              [] ->
                []
              -- TODO this doesn't seem to be working properly with recursive groups:
              -- are mutually recursive defs being grouped properly?
              (isRec, letExps) :: rest ->
                let eIsPartOfThisGroup =
                  groupBoundExps letExps |>
                  List.any (\boundExp -> Set.member (expEId boundExp) eAndAncestorEIds)
                in
                groupIdentifiers letExps ++
                ( if eIsPartOfThisGroup then
                    []
                  else
                    accumulate rest
                )
          in
          accumulate groupedExps
        _ -> []
    )
  )

freeVars : Exp -> List Exp
freeVars exp =
  let removeIntroducedBy pats vars =
    let introduced = identifiersListInPats pats in
    vars |> List.filter (\var -> not <| List.member (Utils.fromJust_ "freeVars" <| expToMaybeIdent var) introduced)
  in
  case (unwrapExp exp) of
    EVar _ x                           -> [exp]
    EFun _ pats body _                 -> freeVars body |> removeIntroducedBy pats
    ECase _ scrutinee branches _       ->
      let freeInEachBranch =
        branchPatExps branches
        |> List.concatMap (\(bPat, bExp) -> freeVars bExp |> removeIntroducedBy [bPat])
      in
      freeVars scrutinee ++ freeInEachBranch
    ELet _ _ (Declarations _ _ _ grouppedExps) _ body ->
      foldRightGroup grouppedExps (freeVars body) <|
       \expGroup isRec indents ->
         let (pats, freeVarsBoundExps) = expGroup |> List.map (\(LetExp _ _ pat _ _ boundExp) -> (pat, freeVars boundExp)) |> List.unzip in
         let freeVarsBoundExpsFlat = List.concatMap identity freeVarsBoundExps in
         (indents |> removeIntroducedBy pats) ++
              (if isRec then freeVarsBoundExpsFlat |> removeIntroducedBy pats else freeVarsBoundExpsFlat)
    _ -> childExps exp |> List.concatMap freeVars

groupIdentifiers: List LetExp -> List Ident
groupIdentifiers = List.concatMap (\(LetExp _ _ p _ _ _) -> identifiersListInPat p)

groupBoundExps: List LetExp -> List Exp
groupBoundExps = List.map (\(LetExp _ _ _ _ _ e) -> e)

isTypeMutuallyRecursive: List LetType -> Bool
isTypeMutuallyRecursive group = List.length group >= 2 ||
    List.all (\(LetType _ _ spAlias _ _ _ tp) -> spAlias /= Nothing && (case tp.val.t__ of
       TForall _ _ _ _  -> True
       _ -> False)
      ) group

-- A group is mutually recursive iff it has at least 2 members or all expressions are syntactic lambdas.
-- In practice, only the second condition is valid, but it happens that the first one implies the second.
isMutuallyRecursive: List LetExp -> Bool
isMutuallyRecursive group = List.length group >= 2 || (
  List.all (\(LetExp _ _ _ _ _ e) -> isBodyPossiblyRecursive e) group)

isBodyPossiblyRecursive: Exp -> Bool
isBodyPossiblyRecursive e = eFunUnapply e /= Nothing

unconsGroup: GroupsOf a -> Maybe (Bool, List a, GroupsOf a)
unconsGroup groups = case groups of
  [] -> Nothing
  (isRec, g)::tail -> Just (isRec, g, tail)

consGroup: Bool -> List a -> GroupsOf a -> GroupsOf a
consGroup isRec group tail = (isRec, group) :: tail

foldLeftGroup: b -> GroupsOf a -> (b -> List a -> Bool -> b) -> b
foldLeftGroup acc elems_ callback =
  let aux: b  -> GroupsOf a -> b
      aux acc    elems = case elems of
       [] -> acc
       (isRec, elemGroup) :: tail ->
         aux (callback acc elemGroup isRec) tail
  in aux acc elems_

foldRightGroup: GroupsOf a -> b -> (List a -> Bool -> b -> b) -> b
foldRightGroup elems acc callback =
  foldLeftGroup acc (List.reverse elems) <|
    \acc group isRec -> callback (List.reverse group) isRec acc

extractGroupInfo: (a -> b) -> (List b -> Bool) -> List (List a) -> GroupsOf b
extractGroupInfo f isRec groups =
  groups |> List.map (\group ->
    let bGroup = List.map f group in
    (isRec bGroup, bGroup))

-- Which var idents in this exp refer to something outside this exp?
-- This is wrong for TypeCases; TypeCase scrutinee patterns not included. TypeCase scrutinee needs to turn into an expression (done on Brainstorm branch, I believe).
freeIdentifiers : Exp -> Set.Set Ident
freeIdentifiers exp = freeIdentifiersList exp |> Set.fromList

freeIdentifiersList : Exp -> List Ident
freeIdentifiersList exp =
  --ImpureGoodies.getOrUpdateCache exp "freeIdentifiers" <| \() -> -- This is not working for now.
  freeVars exp
  |> List.map expToMaybeIdent
  |> Utils.projJusts
  |> Utils.fromJust_ "LangTools.freeIdentifiersList"

getTopLevelOptions: Exp -> List (String, String)
getTopLevelOptions e = getOptions e

-- Diffs

type alias TupleDiffs a = List (Int, a)
type alias ListDiffs a = List (Int, ListElemDiff a)

type ListElemDiff a = ListElemUpdate a | ListElemInsert Int | ListElemDelete Int

type VDictElemDiff = VDictElemDelete | VDictElemInsert | VDictElemUpdate VDiffs

type StringDiffs = StringUpdate {-original start: -}Int {-original end: -}Int {- chars replacing -}Int

type alias EnvDiffs = TupleDiffs VDiffs
-- The environment of a closure if it was modified, the modifications of an environment else.
type VDiffs = VClosureDiffs EnvDiffs (Maybe EDiffs)
            | VListDiffs (ListDiffs VDiffs)
            | VStringDiffs (List StringDiffs)
            | VDictDiffs (Dict (String, String) VDictElemDiff)
            | VRecordDiffs (Dict String VDiffs)
            | VConstDiffs

type EDiffs = EConstDiffs EWhitespaceDiffs
            | EListDiffs (ListDiffs EDiffs)
            | EStringDiffs (List StringDiffs)
            | EChildDiffs (TupleDiffs EDiffs) -- Also for records

type EWhitespaceDiffs = EOnlyWhitespaceDiffs | EAnyDiffs

type UpdateReturn = Inputs (List Val) | InputsWithDiffs (List (Val, Maybe VDiffs))

vListDiffsUnapply: VDiffs -> Maybe (ListDiffs VDiffs)
vListDiffsUnapply vdiffs = case vdiffs of
  VListDiffs d -> Just d
  _ -> Nothing

vRecordDiffsUnapply: VDiffs -> Maybe (Dict String VDiffs)
vRecordDiffsUnapply x = case x of
  VRecordDiffs dict -> Just dict
  _ -> Nothing

-- Given a string "_1", "_2" ... returns if there is a diff associated to it for a diff on datatypes
vDatatypeDiffsGet: String -> VDiffs -> Maybe VDiffs
vDatatypeDiffsGet n d = d |> vRecordDiffsUnapply |> Maybe.andThen (Dict.get ctorArgs) |> Maybe.andThen vRecordDiffsUnapply |> Maybe.andThen (Dict.get n)

offsetStr: Int -> List StringDiffs -> List StringDiffs
offsetStr n diffs =
  --Debug.log ("computing offset of " ++ toString n ++ " on " ++ toString diffs)  <|
  List.map (\sd -> case sd of
    StringUpdate start end replaced -> StringUpdate (start + n) (end + n) replaced) diffs

diffOps = {
  mbVClosureDiffs = \envDiffs mbBodyDiffs ->
    if envDiffs /= [] || mbBodyDiffs  /= Nothing then
      Just <| VClosureDiffs envDiffs mbBodyDiffs
    else Nothing
  }

type alias UpdatedExp = { val: Exp, changes: Maybe EDiffs }
type alias UpdatedExpTuple = { val: List Exp, changes: Maybe (TupleDiffs EDiffs) }
type alias UpdatedDeclarations = {val: Declarations, changes: Maybe (TupleDiffs EDiffs)}
type alias UpdatedVal = { val: Val, changes: Maybe VDiffs }
type alias UpdatedEnv = { val: Env, changes: EnvDiffs }

updatedVal = { unapply = \updatedVal -> (updatedVal.val, updatedVal.changes) }

eChildDiffs = Maybe.map EChildDiffs

-- Builders for updated vals.
updated = {
   -- vClosure: (Val_ -> Val) -> List String -> List Pat -> UpdatedExp -> UpdatedEnv -> UpdatedVal
   vClosure = \valBuilder recNames p updatedExp updatedEnv ->
     diffOps.mbVClosureDiffs updatedEnv.changes updatedExp.changes |>
     UpdatedVal (valBuilder <| VClosure recNames p updatedExp.val updatedEnv.val),

   --: List (String, (Val, Maybe VDiffs)) -> UpdatedEnv
   env = \namesUpdatedVals ->
     let (revEnv, revDiffs) = Utils.foldLeftWithIndex ([], []) namesUpdatedVals <|
       \(revEnv, revDiffs) index (name, {val, changes}) ->
          case changes of
           Nothing -> ((name, val)::revEnv, revDiffs)
           Just d -> ((name, val)::revEnv, (index, d)::revDiffs)
     in
     UpdatedEnv (List.reverse revEnv) (List.reverse revDiffs)
  }



-- To expand the environment of a closure among mutually recursive definition
-- The environment should contain the non-recursive versions of the values first
-- This function returns a closure environment in which we can evaluate the closure's body.
expandRecEnv: List String -> Env -> Env
expandRecEnv recNames closureEnv =
  let (nonRecEnv, remainingEnv) = Utils.split (List.length recNames) closureEnv in
  let recEnv2 = nonRecEnv |> List.map (\((name, v) as pair) -> case v.v_ of
       VClosure [] p b closedEnv -> (name, replaceV_ v <| VClosure recNames p b (nonRecEnv ++ closedEnv))
       _ -> pair
     )
  in
  recEnv2 ++ remainingEnv

