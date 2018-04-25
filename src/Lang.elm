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
type alias Exp     = WithInfo Exp_
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

ctorTupleName: Int -> String
ctorTupleName i = "Tuple" ++ toString i

ctor : (String -> t) -> CtorKind -> String -> (WS, WS, Ident, WS, t)
ctor tagger ctorKind name =
  ( space0, space0, stringifyCtorKind ctorKind, space0
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

numericalEntry : Int -> (WS, t) -> (WS, WS, Ident, WS, t)
numericalEntry index (wsBeforeComma, binding) =
  (wsBeforeComma, space0, argName index, space0, binding)

numericalValEntry : Int -> t -> (Ident, t)
numericalValEntry index binding =
  (argName index, binding)

vTuple: List Val -> Val_
vTuple vals =
  VRecord <| Dict.fromList <| (ctorVal (builtinVal "Lang.vTuple" << VBase << VString) TupleCtor (ctorTupleName (List.length vals)))::Utils.indexedMapFrom 1 numericalValEntry vals

--------------------------------------------------------------------------------


-- TODO add constant literals to patterns, and match 'svg'
type Pat__
  = PVar WS Ident WidgetDecl
  | PConst WS Num
  | PBase WS EBaseVal
  | PWildcard WS
  | PList WS (List Pat) WS (Maybe Pat) WS -- TODO store WS before commas, like EList
  | PAs WS Ident WS Pat
  | PParens WS Pat WS
  | PRecord WS {- { -}  (List (WS {- , -}, WS, Ident, WS{-=-}, Pat)) WS{- } -}

type Op_
  -- nullary ops
  = Pi
  | DictEmpty
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

type alias EId  = Int
type alias PId  = Int
type alias Exp_ = { e__ : Exp__, eid : EId }
type alias Pat_ = { p__ : Pat__, pid : PId }

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

type Exp__
  = EConst WS Num Loc WidgetDecl
  | EBase WS EBaseVal
  | EVar WS Ident
  | EFun WS (List Pat) Exp WS -- WS: before (, before )
  -- TODO remember paren whitespace for multiple pats, like TForall
  -- | EFun WS (OneOrMany Pat) Exp WS
  | EApp WS Exp (List Exp) ApplicationType WS
  | EOp WS Op (List Exp) WS
  | EList WS (List (WS{-,-}, Exp)) WS (Maybe Exp) WS -- the first WS{-,-} is a dummy
  | EIf WS Exp WS{-then-} Exp WS{-else-} Exp WS{-REMOVE-}
  | ECase WS Exp (List Branch) WS
  | ETypeCase WS Exp (List TBranch) WS
  | ELet WS LetKind Rec Pat WS{-=-} Exp WS{-in or ;-} Exp WS{-REMOVE-}
  | EComment WS String Exp
  | EOption WS (WithInfo String) WS (WithInfo String) Exp
  | ETyp WS Pat Type Exp WS -- type declaration
  | EColonType WS Exp WS Type WS -- type annotation
  | ETypeAlias WS Pat Type Exp WS -- type alias
  | ETypeDef WS (WS, Ident) (List (WS, Ident)) WS (List DataConstructor) Exp WS
  | EParens WS Exp ParensStyle WS
  | EHole WS (Maybe Val) -- Internal intermediate, should not appear in code. (Yet.)
  | ERecord WS {- { -} (Maybe (Exp, WS) {- | -}) (List (WS, {-,-} WS, Ident, WS{-=-}, Exp)) {- }-} WS
  | ESelect WS Exp WS {-.-} WS Ident
    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))

    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type ParensStyle = Parens | LongStringSyntax | ElmSyntax | HtmlSyntax

type Type_
  = TNum WS
  | TBool WS
  | TString WS
  | TNull WS
  | TList WS Type WS
  | TDict WS Type Type WS
  | TRecord WS (Maybe (Ident, WS {- | -})) (List (WS, {- , -} WS, Ident, WS{-:-}, Type)) {- }-} WS
  | TTuple WS (List Type) WS (Maybe Type) WS
  | TArrow WS (List Type) WS
  | TUnion WS (List Type) WS
  | TApp WS Ident (List Type)
  | TVar WS Ident
  | TForall WS (OneOrMany (WS, Ident)) Type WS
  | TWildcard WS

type OneOrMany a          -- track concrete syntax for:
  = One a                 --   x
  | Many WS (List a) WS   --   ws1~(x1~...~xn-ws2)

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
  | VClosure (Maybe Ident) (List Pat) Exp Env
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
valEId val = (valExp val).val.eid

type alias Env = List (Ident, Val)
type alias Backtrace = List Exp

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

type alias ScopeId = (EId, Int) -- ELet/EFun/ECase. Int is the branch number for ECase (always 1 for others)

type alias PathedPatternId = (ScopeId, List Int)

type BeforeAfter = Before | After

type alias PatTargetPosition = (BeforeAfter, PathedPatternId)

type alias ExpTargetPosition = (BeforeAfter, EId)

type TargetPosition
  = ExpTargetPosition ExpTargetPosition
  | PatTargetPosition PatTargetPosition


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
patTargetPositionToTargetPathedPatId (beforeAfter, referencePathedPatId) =
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

-- CAREFUL: This is non-breaking space (used in LangSVG.printHTML and also removed from parsing in THMLValParser)
tab k = String.repeat k "  "

-- TODO take into account indent and other prefix of current line
fitsOnLine s =
  if String.length s > 70 then False
  else if List.member '\n' (String.toList s) then False
  else True

isLet e = case e.val.e__ of
  ELet _ _ _ _ _ _ _ _ _ -> True
  _                      -> False

isList e = case e.val.e__ of
  EList _ _ _ _ _ -> True
  _               -> False

isSingletonList e = case e.val.e__ of
  EList _ [_] _ Nothing _ -> True
  _                       -> False

isPair e = case e.val.e__ of
  EList _ [_, _] _ Nothing _ -> True
  _                          -> False

isVar e = case e.val.e__ of
  EVar _ _ -> True
  _        -> False

isFunc e = case e.val.e__ of
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
  case exp.val.e__ of
    ETyp _ _ _ body _         -> exp :: expEffectiveExps body
    EColonType _ body _ _ _   -> exp :: expEffectiveExps body
    ETypeAlias _ _ _ body _   -> exp :: expEffectiveExps body
    ELet _ _ _ _ _ _ _ body _ -> exp :: expEffectiveExps body
    EParens _ e _ _           -> exp :: expEffectiveExps e
    EComment _ _ e            -> exp :: expEffectiveExps e
    EOption _ _ _ _ e         -> exp :: expEffectiveExps e
    EOp _ {val} [operand] _   -> if val == DebugLog || val == NoWidgets then exp :: expEffectiveExps operand else [exp]
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
  let recurse = mapFoldExp f in
  let wrap e__ = replaceE__ e e__ in
  let wrapAndMap = f << wrap in
  -- Make sure exps are left-to-right so they are visited right-to-left.
  let recurseAll initAcc exps =
    exps
    |> List.foldr
        (\exp (newExps, acc) ->
          let (newExp, newAcc) = recurse acc exp in
          (newExp::newExps, newAcc)
        )
        ([], initAcc)
  in
  case e.val.e__ of
    EConst _ _ _ _ -> f e initAcc
    EBase _ _      -> f e initAcc
    EVar _ _       -> f e initAcc
    EFun ws1 ps e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EFun ws1 ps newE1 ws2) newAcc

    EApp ws1 e1 es apptype ws2 ->
      let (newEs, newAcc)  = recurseAll initAcc es in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (EApp ws1 newE1 newEs apptype ws2) newAcc2

    EOp ws1 op es ws2 ->
      let (newEs, newAcc) = recurseAll initAcc es in
      wrapAndMap (EOp ws1 op newEs ws2) newAcc

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newAcc) = recurseAll initAcc (Utils.listValues es) in
      wrapAndMap (EList ws1 (Utils.listValuesMake es newEs) ws2 Nothing ws3) newAcc

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newE1, newAcc)  = recurse initAcc e1 in
      let (newEs, newAcc2) = recurseAll newAcc (Utils.listValues es) in
      wrapAndMap (EList ws1 (Utils.listValuesMake es newEs) ws2 (Just newE1) ws3) newAcc2

    ERecord ws1 Nothing es ws2 ->
      let (newEs, newAcc) = recurseAll initAcc (List.map Utils.recordValue es) in
      wrapAndMap (ERecord ws1 Nothing (Utils.recordValuesMake es newEs) ws2) newAcc

    ERecord ws1 (Just (mi, wsi)) es ws2 ->
      let (newEs, newAcc) = recurseAll initAcc (List.map Utils.recordValue es) in
      let (newMi, newAcc2) = recurse newAcc mi in
      wrapAndMap (ERecord ws1 (Just (newMi, wsi)) (Utils.recordValuesMake es newEs) ws2) newAcc2

    ESelect ws0 e ws1 ws2 s ->
      let (newE, newAcc) = recurse initAcc e in
      wrapAndMap (ESelect ws0 newE ws1 ws2 s) newAcc

    EIf ws1 e1 ws2 e2 ws3 e3 ws4 ->
      case recurseAll initAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newAcc) -> wrapAndMap (EIf ws1 newE1 ws2 newE2 ws3 newE3 ws4) newAcc
        _                               -> Debug.crash "I'll buy you a beer if this line of code executes. - Brian"

    ECase ws1 e1 branches ws2 ->
      let (newBranches, newAcc) =
        branches
        |> List.foldr
            (\branch (newBranches, acc) ->
              let (Branch_ bws1 p ei bws2) = branch.val in
              let (newEi, newAcc) = recurse acc ei in
              ({ branch | val = Branch_ bws1 p newEi bws2 }::newBranches, newAcc)
            )
            ([], initAcc)
      in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (ECase ws1 newE1 newBranches ws2) newAcc2

    ETypeCase ws1 e1 tbranches ws2 ->
      let (newBranches, newAcc) =
        tbranches
        |> List.foldr
            (\tbranch (newBranches, acc) ->
              let (TBranch_ bws1 t ei bws2) = tbranch.val in
              let (newEi, newAcc) = recurse acc ei in
              ({ tbranch | val = TBranch_ bws1 t newEi bws2 }::newBranches, newAcc)
            )
            ([], initAcc)
      in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (ETypeCase ws1 newE1 newBranches ws2) newAcc2

    EComment ws s e1 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EComment ws s newE1) newAcc

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EOption ws1 s1 ws2 s2 newE1) newAcc

    ELet ws1 k b p ws2 e1 ws3 e2 ws4 ->
      let (newE2, newAcc) = recurse initAcc e2 in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (ELet ws1 k b p ws2 newE1 ws3 newE2 ws4) newAcc2

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (ETyp ws1 pat tipe newE1 ws2) newAcc

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EColonType ws1 newE1 ws2 tipe ws3) newAcc

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (ETypeAlias ws1 pat tipe newE1 ws2) newAcc

    ETypeDef ws1 ident vars ws2 dcs e1 ws3 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (ETypeDef ws1 ident vars ws2 dcs newE1 ws3) newAcc

    EParens ws1 e pStyle ws2 ->
      let (newE, newAcc) = recurse initAcc e in
      wrapAndMap (EParens ws1 newE pStyle ws2) newAcc

    EHole _ _ -> f e initAcc


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

    PAs ws1 ident ws2 pChild ->
      let (newPChild, newAcc) = recurse initAcc pChild in
      wrapAndMap (PAs ws1 ident ws2 newPChild) newAcc

    PParens ws1 pChild ws2 ->
      let (newPChild, newAcc) = recurse initAcc pChild in
      wrapAndMap (PParens ws1 newPChild ws2) newAcc

-- Nodes visited/replaced in top-down, left-to-right order.
-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapFoldExpTopDown : (Exp -> a -> (Exp, a)) -> a -> Exp -> (Exp, a)
mapFoldExpTopDown f initAcc e =
  let (newE, newAcc) = f e initAcc in
  let ret e__ acc =
    (replaceE__ newE e__, acc)
  in
  let recurse acc child =
    mapFoldExpTopDown f acc child
  in
  let recurseAll acc exps =
    exps
    |> List.foldl
        (\exp (newExps, acc) ->
          let (newExp, newAcc) = recurse acc exp in
          (newExps ++ [newExp], newAcc)
        )
        ([], acc)
  in
  case newE.val.e__ of
    EConst _ _ _ _ -> (newE, newAcc)
    EBase _ _      -> (newE, newAcc)
    EVar _ _       -> (newE, newAcc)
    EFun ws1 ps e1 ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (EFun ws1 ps newE1 ws2) newAcc2

    EApp ws1 e1 es appType ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      let (newEs, newAcc3) = recurseAll newAcc2 es in
      ret (EApp ws1 newE1 newEs appType ws2) newAcc3

    EOp ws1 op es ws2 ->
      let (newEs, newAcc2) = recurseAll newAcc es in
      ret (EOp ws1 op newEs ws2) newAcc2

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newAcc2) = recurseAll newAcc (Utils.listValues es) in
      ret (EList ws1 (Utils.listValuesMake es newEs) ws2 Nothing ws3) newAcc2

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newEs, newAcc2) = recurseAll newAcc (Utils.listValues es) in
      let (newE1, newAcc3) = recurse newAcc2 e1 in
      ret (EList ws1 (Utils.listValuesMake es newEs) ws2 (Just newE1) ws3) newAcc3

    ERecord ws1 Nothing es ws2 ->
      let (newEs, newAcc2) = recurseAll newAcc (Utils.recordValues es) in
      ret (ERecord ws1 Nothing (Utils.recordValuesMake es newEs) ws2) newAcc2

    ERecord ws1 (Just (mi, wsi)) es ws2 ->
      let (newMi, newAcc2) = recurse newAcc mi in
      let (newEs, newAcc3) = recurseAll newAcc2 (Utils.recordValues es) in
      ret (ERecord ws1 (Just (newMi, wsi)) (Utils.recordValuesMake es newEs) ws2) newAcc3

    ESelect ws0 e ws1 ws2 s ->
      let (newE, newAcc2) = recurse newAcc e in
      ret (ESelect ws0 newE ws1 ws2 s) newAcc2

    EIf ws1 e1 ws2 e2 ws3 e3 ws4 ->
      case recurseAll newAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newAcc2) -> ret (EIf ws1 newE1 ws2 newE2 ws3 newE3 ws4) newAcc2
        _                                -> Debug.crash "I'll buy you a beer if this line of code executes. - Brian"

    ECase ws1 e1 branches ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      let (newBranches, newAcc3) =
        branches
        |> List.foldl
            (\branch (newBranches, acc) ->
              let (Branch_ bws1 p ei bws2) = branch.val in
              let (newEi, newAcc3) = recurse acc ei in
              (newBranches ++ [{ branch | val = Branch_ bws1 p newEi bws2 }], newAcc3)
            )
            ([], newAcc2)
      in
      ret (ECase ws1 newE1 newBranches ws2) newAcc3

    ETypeCase ws1 e1 tbranches ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      let (newBranches, newAcc3) =
        tbranches
        |> List.foldl
            (\tbranch (newBranches, acc) ->
              let (TBranch_ bws1 t ei bws2) = tbranch.val in
              let (newEi, newAcc3) = recurse acc ei in
              (newBranches ++ [{ tbranch | val = TBranch_ bws1 t newEi bws2 }], newAcc3)
            )
            ([], newAcc2)
      in
      ret (ETypeCase ws1 newE1 newBranches ws2) newAcc3

    EComment ws s e1 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (EComment ws s newE1) newAcc2

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (EOption ws1 s1 ws2 s2 newE1) newAcc2

    ELet ws1 k b p ws2 e1 ws3 e2 ws4 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      let (newE2, newAcc3) = recurse newAcc2 e2 in
      ret (ELet ws1 k b p ws2 newE1 ws3 newE2 ws4) newAcc3

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (ETyp ws1 pat tipe newE1 ws2) newAcc2

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (EColonType ws1 newE1 ws2 tipe ws3) newAcc2

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (ETypeAlias ws1 pat tipe newE1 ws2) newAcc2

    ETypeDef ws1 ident vars ws2 dcs e1 ws3 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (ETypeDef ws1 ident vars ws2 dcs newE1 ws3) newAcc2

    EParens ws1 e pStyle ws2 ->
      let (newE, newAcc2) = recurse newAcc e in
      ret (EParens ws1 newE pStyle ws2) newAcc2

    EHole _ _ -> (newE, newAcc)


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

    PAs ws1 ident ws2 pChild ->
      let (newPChild, newAcc2) = recurse newAcc pChild in
      ret (PAs ws1 ident ws2 newPChild) newAcc2

    PParens ws1 pChild ws2 ->
      let (newPChild, newAcc2) = recurse newAcc pChild in
      ret (PParens ws1 newPChild ws2) newAcc2

-- Nodes visited/replaced in top-down, left-to-right order.
-- Includes user-defined scope information.
-- Careful, a poorly constructed mapping function can cause this to fail to terminate.
mapFoldExpTopDownWithScope
  :  (Exp -> a -> b -> (Exp, a))
  -> (Exp -> b -> b)
  -> (Exp -> b -> b)
  -> (Exp -> Branch -> Int -> b -> b)
  -> a
  -> b
  -> Exp
  -> (Exp, a)
mapFoldExpTopDownWithScope f handleELet handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e =
  let (newE, newGlobalAcc) = f e initGlobalAcc initScopeTempAcc in
  let ret e__ globalAcc =
    (replaceE__ newE e__, globalAcc)
  in
  let recurse globalAcc scopeTempAcc child =
    mapFoldExpTopDownWithScope f handleELet handleEFun handleCaseBranch globalAcc scopeTempAcc child
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
  case newE.val.e__ of
    EConst _ _ _ _ -> (newE, newGlobalAcc)
    EBase _ _      -> (newE, newGlobalAcc)
    EVar _ _       -> (newE, newGlobalAcc)
    EFun ws1 ps e1 ws2 ->
      let newScopeTempAcc = handleEFun newE initScopeTempAcc in
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc newScopeTempAcc e1 in
      ret (EFun ws1 ps newE1 ws2) newGlobalAcc2

    EApp ws1 e1 es apptype ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      let (newEs, newGlobalAcc3) = recurseAll newGlobalAcc2 initScopeTempAcc es in
      ret (EApp ws1 newE1 newEs apptype ws2) newGlobalAcc3

    EOp ws1 op es ws2 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc initScopeTempAcc es in
      ret (EOp ws1 op newEs ws2) newGlobalAcc2

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc initScopeTempAcc (Utils.listValues es) in
      ret (EList ws1 (Utils.listValuesMake es newEs) ws2 Nothing ws3) newGlobalAcc2

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      let (newEs, newGlobalAcc3) = recurseAll newGlobalAcc2 initScopeTempAcc (Utils.listValues es) in
      ret (EList ws1 (Utils.listValuesMake es newEs) ws2 (Just newE1) ws3) newGlobalAcc3

    ERecord ws1 Nothing es ws2 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc initScopeTempAcc (Utils.recordValues es) in
      ret (ERecord ws1 Nothing (Utils.recordValuesMake es newEs) ws2) newGlobalAcc2

    ERecord ws1 (Just (mi, wsi)) es ws2 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc initScopeTempAcc (Utils.recordValues es) in
      let (newMi, newGlobalAcc3) = recurse newGlobalAcc2 initScopeTempAcc mi in
      ret (ERecord ws1 (Just (newMi, wsi)) (Utils.recordValuesMake es newEs) ws2) newGlobalAcc3

    ESelect ws0 e ws1 ws2 ident ->
      let (newE, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e in
      ret (ESelect ws0 newE ws1 ws2 ident) newGlobalAcc2

    EIf ws1 e1 ws2 e2 ws3 e3 ws4 ->
      case recurseAll newGlobalAcc initScopeTempAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newGlobalAcc2) -> ret (EIf ws1 newE1 ws2 newE2 ws3 newE3 ws4) newGlobalAcc2
        _                                      -> Debug.crash "I'll buy you a beer if this line of code executes. - Brian"

    ECase ws1 e1 branches ws2 ->
      -- Note: ECase given to handleBranch has original scrutinee for now.
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      let (newBranches, newGlobalAcc3) =
        branches
        |> Utils.foldli1
            (\(i, branch) (newBranches, globalAcc) ->
              let newScopeTempAcc = handleCaseBranch newE branch i initScopeTempAcc in
              let (Branch_ bws1 p ei bws2) = branch.val in
              let (newEi, newGlobalAcc3) = recurse globalAcc newScopeTempAcc ei in
              (newBranches ++ [{ branch | val = Branch_ bws1 p newEi bws2 }], newGlobalAcc3)
            )
            ([], newGlobalAcc2)
      in
      ret (ECase ws1 newE1 newBranches ws2) newGlobalAcc3

    ETypeCase ws1 e1 tbranches ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      let (newBranches, newGlobalAcc3) =
        tbranches
        |> List.foldl
            (\tbranch (newBranches, globalAcc) ->
              let (TBranch_ bws1 t ei bws2) = tbranch.val in
              let (newEi, newGlobalAcc3) = recurse globalAcc initScopeTempAcc ei in
              (newBranches ++ [{ tbranch | val = TBranch_ bws1 t newEi bws2 }], newGlobalAcc3)
            )
            ([], newGlobalAcc2)
      in
      ret (ETypeCase ws1 newE1 newBranches ws2) newGlobalAcc3

    EComment ws s e1 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (EComment ws s newE1) newGlobalAcc2

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (EOption ws1 s1 ws2 s2 newE1) newGlobalAcc2

    ELet ws1 k isRec p ws2 e1 ws3 e2 ws4 ->
      let newScopeTempAcc = handleELet newE initScopeTempAcc in
      let (newE1, newGlobalAcc2) =
        if isRec
        then recurse newGlobalAcc newScopeTempAcc e1
        else recurse newGlobalAcc initScopeTempAcc e1
      in
      let (newE2, newGlobalAcc3) = recurse newGlobalAcc2 newScopeTempAcc e2 in
      ret (ELet ws1 k isRec p ws2 newE1 ws3 newE2 ws4) newGlobalAcc3

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (ETyp ws1 pat tipe newE1 ws2) newGlobalAcc2

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (EColonType ws1 newE1 ws2 tipe ws3) newGlobalAcc2

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (ETypeAlias ws1 pat tipe newE1 ws2) newGlobalAcc2

    ETypeDef ws1 ident vars ws2 dcs e1 ws3 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (ETypeDef ws1 ident vars ws2 dcs newE1 ws3) newGlobalAcc2

    EParens ws1 e pStyle ws2 ->
      let (newE, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e in
      ret (EParens ws1 newE pStyle ws2) newGlobalAcc2

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
  let f_ exp = replaceE__ exp (f exp.val.e__) in
  mapExp f_ e

-- Folding function returns just newGlobalAcc instead of (newExp, newGlobalAcc)
foldExpTopDownWithScope
  :  (Exp -> accT -> scopeAccT -> accT)
  -> (Exp -> scopeAccT -> scopeAccT)
  -> (Exp -> scopeAccT -> scopeAccT)
  -> (Exp -> Branch -> Int -> scopeAccT -> scopeAccT)
  -> accT
  -> scopeAccT
  -> Exp
  -> accT
foldExpTopDownWithScope f handleELet handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e =
  let (_, finalGlobalAcc) =
    mapFoldExpTopDownWithScope
        (\e globalAcc scopeTempAcc -> (e, f e globalAcc scopeTempAcc))
        handleELet handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e
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
  let f_ exp = f exp.val.e__ in
  foldExp f_ acc exp

-- Search for eid in root, replace matching node based on given function
mapExpNode : EId -> (Exp -> Exp) -> Exp -> Exp
mapExpNode eid f root =
  mapExp
      (\exp ->
        if exp.val.eid == eid
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
  replaceExpNodeE__ByEId oldNode.val.eid newE__ root

replaceExpNodeE__ByEId : EId -> Exp__ -> Exp -> Exp
replaceExpNodeE__ByEId eid newE__ root =
  let esubst = Dict.singleton eid newE__ in
  applyESubst esubst root

-- Like applyESubst, but you give Exp not E__
replaceExpNodes : (Dict EId Exp) -> Exp -> Exp
replaceExpNodes eidToNewNode root =
  mapExpTopDown -- top down to handle replacements that are subtrees of each other; a naive eidToNewNode could however make this loop forever
    (\exp ->
      case Dict.get exp.val.eid eidToNewNode of
        Just newExp -> newExp
        Nothing     -> exp
    )
    root

replaceExpNodesPreservingPrecedingWhitespace : (Dict EId Exp) -> Exp -> Exp
replaceExpNodesPreservingPrecedingWhitespace eidToNewNode root =
  mapExpTopDown -- top down to handle replacements that are subtrees of each other; a naive eidToNewNode could however make this loop forever
    (\exp ->
      case Dict.get exp.val.eid eidToNewNode of
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
          ELet ws1 kind isRec pat ws2 boundExp ws3 body ws4 -> ELet ws1 kind isRec (mapPatNodePat pid f pat) ws2 boundExp ws3 body ws4
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
  let wrap t_ = WithInfo t_ tipe.start tipe.end in
  case tipe.val of
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
    TApp ws1 ident ts       -> f (wrap (TApp ws1 ident (List.map recurse ts)))
    TForall ws1 vars t1 ws2 -> f (wrap (TForall ws1 vars (recurse t1) ws2))

    TTuple ws1 ts ws2 mt ws3 ->
      f (wrap (TTuple ws1 (List.map recurse ts) ws2 (Utils.mapMaybe recurse mt) ws3))
    TRecord ws1 mi ts ws2       ->
      f (wrap (TRecord ws1 mi (Utils.recordValuesMap recurse ts) ws2))

foldType : (Type -> a -> a) -> Type -> a -> a
foldType f tipe acc =
  let foldTypes f tipes acc = List.foldl (\t acc -> foldType f t acc) acc tipes in
  case tipe.val of
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
    TApp _ _ ts     -> acc |> foldTypes f ts |> f tipe

    TTuple _ ts _ Nothing _  -> acc |> foldTypes f ts |> f tipe
    TTuple _ ts _ (Just t) _ -> acc |> foldTypes f (ts++[t]) |> f tipe
    TRecord _ _ ts _  -> acc |> foldTypes f (Utils.recordValues ts) |> f tipe

------------------------------------------------------------------------------
-- Traversing

eidIs : EId -> Exp -> Bool
eidIs targetEId exp = exp.val.eid == targetEId

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

-- justFindExpByEId is in LangTools (it needs the unparser for error messages).
-- LangTools.justFindExpByEId : EId -> Exp -> Exp
-- LangTools.justFindExpByEId eid exp =
--   findExpByEId exp eid
--   |> Utils.fromJust__ (\() -> "Couldn't find eid " ++ toString eid ++ " in " ++ unparseWithIds exp)


findPatByPId : Exp -> PId -> Maybe Pat
findPatByPId program targetPId =
  findScopeExpAndPatByPId program targetPId
  |> Maybe.map (\(scopeExp, pat) -> pat)


findScopeExpAndPatByPId : Exp -> PId -> Maybe (Exp, Pat)
findScopeExpAndPatByPId program targetPId =
  program
  |> mapFirstSuccessNode
      (\e ->
        let maybeTargetPat =
          case e.val.e__ of
            ELet _ _ _ pat _ _ _ _ _ -> findPatInPat targetPId pat
            EFun _ pats _ _          -> Utils.mapFirstSuccess (findPatInPat targetPId) pats
            ECase _ _ branches _     -> Utils.mapFirstSuccess (findPatInPat targetPId) (branchPats branches)
            _                        -> Nothing
        in
        maybeTargetPat |> Maybe.map (\pat -> (e, pat))
      )


findPatInPat : PId -> Pat -> Maybe Pat
findPatInPat targetPId pat =
  flattenPatTree pat
  |> Utils.findFirst (.val >> .pid >> (==) targetPId)


findExpByLocId : Exp -> LocId -> Maybe Exp
findExpByLocId program targetLocId =
  let isTarget exp =
    case exp.val.e__ of
      EConst _ _ (locId, _, _) _ -> locId == targetLocId
      _                          -> False
  in
  findFirstNode isTarget program

locIdToEId : Exp -> LocId -> Maybe EId
locIdToEId program locId =
  findExpByLocId program locId |> Maybe.map (.val >> .eid)

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

-- Returns a boolean tag indicating whether the ancestor is a scope.
-- Most notably, a let is NOT a scope for its bound exp, only its body.
-- (An ECase is also NOT a scope for its scrutinee, but is for its branches.)
findAllWithAncestorsScopesTagged : (Exp -> Bool) -> Exp -> List (List (Exp, Bool))
findAllWithAncestorsScopesTagged predicate exp =
  findAllWithAncestorsScopesTagged_ predicate [] exp

findAllWithAncestorsScopesTagged_ : (Exp -> Bool) -> List (Exp, Bool) -> Exp -> List (List (Exp, Bool))
findAllWithAncestorsScopesTagged_ predicate ancestors exp =
  let ancestorsAndThisNoScope = ancestors ++ [(exp, False)] in
  let ancestorsAndThisScope   = ancestors ++ [(exp, True)] in
  let thisResult = if predicate exp then [ancestorsAndThisNoScope] else [] in
  let recurseNoScope exp = findAllWithAncestorsScopesTagged_ predicate ancestorsAndThisNoScope exp in
  let recurseScope exp   = findAllWithAncestorsScopesTagged_ predicate ancestorsAndThisScope exp in
  case exp.val.e__ of
    ELet _ _ _ _ _ boundExp _ body _ -> thisResult ++ recurseNoScope boundExp  ++ recurseScope body
    ECase _ scrutinee branches _     -> thisResult ++ recurseNoScope scrutinee ++ List.concatMap recurseScope (branchExps branches)
    EFun _ _ body _                  -> thisResult ++ recurseScope body
    _                                -> thisResult ++ List.concatMap recurseNoScope (childExps exp)

commonAncestors : (Exp -> Bool) -> Exp -> List Exp
commonAncestors pred exp =
  findAllWithAncestors pred exp
  |> List.map (Utils.dropLast 1) -- Never return an expression that the predicate matched: it will be moved/removed/replaced
  |> Utils.commonPrefix

-- Target expression is last in list.
findWithAncestorsByEId : Exp -> EId -> Maybe (List Exp)
findWithAncestorsByEId exp targetEId =
  if exp.val.eid == targetEId then
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

singleArgExtractor: String -> (Exp -> Exp__) -> (List Exp -> Exp)
singleArgExtractor msg f l = case l of
  head::_ -> replaceE__ head <| f head
  _ -> Debug.crash <| "[internal error] Unespected empty list for " ++ msg

multiArgExtractor: String -> (List Exp -> Exp__) -> (List Exp -> Exp)
multiArgExtractor msg f l = case l of
  head::_ -> replaceE__ head <| f l
  _ -> Debug.crash <| "[internal error] Unespected empty list for " ++ msg

-- Children left-to-right, with a way to rebuild the expression if given the same exps)
childExpsExtractors : Exp -> (List Exp, List Exp -> Exp)
childExpsExtractors e =
  case e.val.e__ of
    EConst _ _ _ _          -> ([], \_ -> e)
    EBase _ _               -> ([], \_ -> e)
    EVar _ _                -> ([], \_ -> e)
    EFun ws1 ps e_ ws2      -> ([e_], singleArgExtractor "EFun-unexp" <| \newE -> EFun ws1 ps newE ws2)
    EOp ws1 op es ws2       -> (es, multiArgExtractor "EOp-unexp" <| \newEs -> EOp ws1 op newEs ws2)
    EList ws1 es ws2 m ws3  ->
      case m of
        Just e  -> (Utils.listValues es ++ [e], multiArgExtractor "EList-unexp" <| \newEs ->  EList ws1 (Utils.listValuesMake es <| Utils.dropLast 1 newEs) ws2 (Just (Utils.last "childExps-EList" newEs)) ws3)
        Nothing ->( Utils.listValues es, multiArgExtractor "EList-unexp" <| \newEs ->  EList ws1 (Utils.listValuesMake es <| newEs) ws2 Nothing ws3)
    ERecord ws1 mw es ws2 ->
      case mw of
         Just (e, w) -> ([e] ++ Utils.recordValues es, multiArgExtractor "ERecord-unexp" <| \newEs -> ERecord ws1 (Just (Utils.head "childExps-ERecord" newEs, w)) (Utils.recordValuesMake es (Utils.tail  "childExps-ERecord" newEs)) ws2)
         Nothing -> (Utils.recordValues es, multiArgExtractor "ERecord-unexp" <|  \newEs -> ERecord ws1 Nothing (Utils.recordValuesMake es newEs) ws2)
    ESelect sp0 e sp1 sp2 name       -> ([e], multiArgExtractor "ESelect-unexp" <| \newEs -> ESelect sp0 (Utils.head "childExps-ESelect" newEs) sp1 sp2 name)
    EApp ws1 f es apptype ws2        -> (f :: es, multiArgExtractor "EApp-unexp" <| \newEs -> EApp ws1 (Utils.head "childExps-EApp" newEs) (Utils.tail "childExps-Eapp" newEs) apptype ws2)
    ELet ws1 k b p ws2 e1 ws3 e2 ws4 -> ([e1, e2], multiArgExtractor "ELet-unexp" <| \newEs -> case newEs of
      [newE1, newE2] -> ELet ws1 k b p ws2 newE1 ws3 newE2 ws4
      _ -> Debug.crash "childExps-ELet")
    EIf ws1 e1 ws2 e2 ws3 e3 ws4     -> ([e1, e2, e3], multiArgExtractor "EIf-unexp" <| \newEs -> case newEs of
        [newE1, newE2, newE3] -> EIf ws1 newE1 ws2 newE2 ws3 newE3 ws4
        _ -> Debug.crash "childExps-EIf")
    ECase ws1 e branches ws2         -> let (es, esExtractor) = branchExpsExtractor branches in
      (e :: es, multiArgExtractor "ECase-unexp" <| \newEs ->  ECase ws1 (Utils.head "childExps-ECase" newEs) (Utils.tail "childExps-ECAse" newEs |> esExtractor) ws2)
    ETypeCase ws1 e tbranches ws2    -> let (es, esExtractor) = tbranchExpsExtractor tbranches in
      (e :: es, multiArgExtractor "ETypeCase-unexp" <| \newEs ->  ETypeCase ws1 (Utils.head "childExps-ECase" newEs) (Utils.tail "childExps-ECAse" newEs |> esExtractor) ws2)
    EComment ws s e1                 -> ([e1], singleArgExtractor "EComment-unexp" <| \newE ->EComment ws s newE              )
    EOption ws1 s1 ws2 s2 e1         -> ([e1], singleArgExtractor "EOption-unexp" <| \newE ->EOption ws1 s1 ws2 s2 newE      )
    ETyp ws1 pat tipe e ws2          -> ([e], singleArgExtractor  "ETyp-unexp" <| \newE -> ETyp ws1 pat tipe newE ws2      )
    EColonType ws1 e ws2 tipe ws3    -> ([e], singleArgExtractor  "EColonType-unexp" <| \newE -> EColonType ws1 newE ws2 tipe ws3)
    ETypeAlias ws1 pat tipe e ws2    -> ([e], singleArgExtractor  "ETypeAlias-unexp" <| \newE -> ETypeAlias ws1 pat tipe newE ws2)
    ETypeDef a1 a2 a3 a4 a5 e a6     -> ([e], singleArgExtractor  "ETypeDef-unexp" <| \newE -> ETypeDef a1 a2 a3 a4 a5 newE a6 )
    EParens a1 e a2 a3               -> ([e], singleArgExtractor  "EParens-unexp" <| \newE -> EParens a1 newE a2 a3)
    EHole _ _                        -> ([], \_ -> e)

allEIds : Exp -> List EId
allEIds exp =
  flattenExpTree exp |> List.map (.val >> .eid)


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
        let e__ = e.val.e__ in
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
        case Dict.get e.val.eid subst.esubst of
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
      case Dict.get e.val.eid esubst of
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
    case exp.val.e__ of
      ELet _ _ _ _ _ _ _ _ _ -> True
      EFun _ _ _ _           -> True
      _                      -> False
  in
  case maybeParent of
    Just parent ->
      case parent.val.e__ of
        ECase _ predicate branches _ -> predicate /= exp
        _                            -> isObviouslyScope
    Nothing -> isObviouslyScope

isNumber : Exp -> Bool
isNumber exp =
  case exp.val.e__ of
    EConst _ _ _ _ -> True
    _              -> False

-- Disregards syncOptions
isFrozenNumber : Exp -> Bool
isFrozenNumber exp =
  case exp.val.e__ of
    EConst _ _ (_, ann, _) _ -> ann == frozen
    _                        -> False

isComment : Exp -> Bool
isComment exp =
  case exp.val.e__ of
    EComment _ _ _ -> True
    _              -> False

isOption : Exp -> Bool
isOption exp =
  case exp.val.e__ of
    EOption _ _ _ _ _ -> True
    _                 -> False

varsOfPat : Pat -> List Ident
varsOfPat pat =
  case pat.val.p__ of
    PConst _ _              -> []
    PBase _ _               -> []
    PWildcard _             -> []
    PVar _ x _              -> [x]
    PList _ ps _ Nothing _  -> List.concatMap varsOfPat ps
    PList _ ps _ (Just p) _ -> List.concatMap varsOfPat (p::ps)
    PRecord _ ps _          -> List.concatMap varsOfPat <| Utils.recordValues ps
    PAs _ x _ p             -> x::(varsOfPat p)
    PParens _ p _           -> varsOfPat p


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
    PList _ ps _ (Just p) _ -> ps ++ [p]
    PAs _ _ _ p             -> [p]
    PParens _ p _           -> [p]

-----------------------------------------------------------------------------
-- Lang Options

-- all options should appear before the first non-comment expression

getOptions : Exp -> List (String, String)
getOptions e = case e.val.e__ of
  EOption _ s1 _ s2 e1 -> (s1.val, s2.val) :: getOptions e1
  EComment _ _ e1      -> getOptions e1
  _                    -> []


------------------------------------------------------------------------------
-- Error Messages

errorPrefix = "[Evaluation Error]" -- NOTE: same as errorPrefix in Native/codeBox.js
crashWithMsg s  = Debug.crash <| errorPrefix ++ "\n\n" ++ s
errorMsg s      = Err <| errorPrefix ++ "\n\n" ++ s

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
exp_ = flip Exp_ (-1)

pat_ : Pat__ -> Pat_
pat_ = flip Pat_ (-1)

withDummyRange x            = WithInfo x dummyPos dummyPos
withDummyPatInfo p__        = WithInfo (pat_ p__) dummyPos dummyPos
withDummyExpInfo e__        = WithInfo (exp_ e__) dummyPos dummyPos
withDummyPatInfoPId pid p__ = WithInfo (Pat_ p__ pid) dummyPos dummyPos
withDummyExpInfoEId eid e__ = WithInfo (Exp_ e__ eid) dummyPos dummyPos

replaceE__ : Exp -> Exp__ -> Exp
replaceE__ e e__ = let e_ = e.val in { e | val = { e_ | e__ = e__ } }

mapNodeE__ : (Exp__ -> Exp__ ) -> Exp -> Exp
mapNodeE__ f e = replaceE__ e (f e.val.e__)

replaceP__ : Pat -> Pat__ -> Pat
replaceP__ p p__ = let p_ = p.val in { p | val = { p_ | p__ = p__ } }

mapNodeP__ : (Pat__ -> Pat__ ) -> Pat -> Pat
mapNodeP__ f p = replaceP__ p (f p.val.p__)

replaceB__ : Branch -> Branch_ -> Branch
replaceB__ b b_ = { b | val = b_ }

replaceTB__ : TBranch -> TBranch_ -> TBranch
replaceTB__ b b_ = { b | val = b_ }

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
setEId eid e = let e_ = e.val in { e | val = { e_ | eid = eid } }

setPId : PId -> Pat -> Pat
setPId pid p = let p_ = p.val in { p | val = { p_ | pid = pid } }

clearEId e = setEId -1 e
clearPId p = setPId -1 p

clearPIds p = mapPatTopDown clearPId p

clearNodeIds e =
  let eidCleared = clearEId e in
  case eidCleared.val.e__ of
    EConst ws n (locId, annot, ident) wd  -> replaceE__ eidCleared (EConst ws n (0, annot, "") wd)
    ELet ws1 kind b p ws2 e1 ws3 e2 ws4   -> replaceE__ eidCleared (ELet ws1 kind b (clearPIds p) ws2 e1 ws3 e2 ws4)
    EFun ws1 pats body ws2                -> replaceE__ eidCleared (EFun ws1 (List.map clearPIds pats) body ws2)
    ECase ws1 scrutinee branches ws2      -> replaceE__ eidCleared (ECase ws1 scrutinee (mapBranchPats clearPIds branches) ws2)
    ETypeCase ws1 scrutinee tbranches ws2 -> replaceE__ eidCleared (ETypeCase ws1 scrutinee tbranches ws2)
    ETyp ws1 pat tipe e ws2               -> replaceE__ eidCleared (ETyp ws1 (clearPIds pat) tipe e ws2)
    ETypeAlias ws1 pat tipe e ws2         -> replaceE__ eidCleared (ETypeAlias ws1 (clearPIds pat) tipe e ws2)
    _                                     -> eidCleared

dummyLoc_ b = (0, b, "")
dummyTrace_ b = TrLoc (dummyLoc_ b)

dummyLoc     = dummyLoc_ unann
dummyTrace   = dummyTrace_ unann
dummyProvenance = Provenance [] (eTuple0 []) []

-- TODO interacts badly with auto-abstracted variable names...
dummyLocWithDebugInfo : Frozen -> Num -> Loc
dummyLocWithDebugInfo b n = (0, b, "")

eOp op_ es = withDummyExpInfo <| EOp space1 (withDummyRange op_) es space0

ePlus e1 e2 = eOp Plus [e1, e2]
eMinus e1 e2 = eOp Minus [e1, e2]

eBool  = withDummyExpInfo << EBase space1 << EBool
eStr   = withDummyExpInfo << EBase space1 << EString "\"" -- defaultQuoteChar
eStr0  = withDummyExpInfo << EBase space0 << EString "\"" -- defaultQuoteChar
eTrue  = eBool True
eFalse = eBool False
eNull  = withDummyExpInfo <| EBase space1 <| ENull
eIf c t e   = withDummyExpInfo <| EIf space0 c space1 t space1 e space0

eApp e es      = withDummyExpInfo <| EApp space1 e es SpaceApp space0
eCall fName es = eApp (eVar0 fName) es
eFun ps e      = withDummyExpInfo <| EFun space1 ps e space0
eRecord kvs    = withDummyExpInfo <| ERecord space1 Nothing (List.map (\(k, v) -> (space0, space1, k, space1, v)) kvs) space1
eSelect e name = withDummyExpInfo  <| ESelect space0 e space0 space0 name

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
  withDummyExpInfo <| ELet newline1 letKind False pat space1 assign space1 bodyExp space0

patBoundExpOf : List (Ident, Exp) -> (Pat, Exp)
patBoundExpOf namesAndAssigns =
  case List.unzip namesAndAssigns of
    ([name], [assign]) -> (pVar name, replacePrecedingWhitespace " " assign)
    (names, assigns)   -> (pListOfPVars names, eList (setExpListWhitespace "" " " assigns) Nothing)

eLet : List (Ident, Exp) -> Exp -> Exp
eLet = eLetOrDef Let

eDef : List (Ident, Exp) -> Exp -> Exp
eDef = eLetOrDef Def


eVar0 a           = withDummyExpInfo <| EVar space0 a
eVar a            = withDummyExpInfo <| EVar space1 a
eConst0 a b       = withDummyExpInfo <| EConst space0 a b noWidgetDecl
eConst a b        = withDummyExpInfo <| EConst space1 a b noWidgetDecl
eConstDummyLoc0 a = withDummyExpInfo <| EConst space0 a dummyLoc noWidgetDecl
eConstDummyLoc a  = withDummyExpInfo <| EConst space1 a dummyLoc noWidgetDecl
eList0 a b        = withDummyExpInfo <| EList space0 (List.map ((,) space0) a) space0 b space0
eList a b         = withDummyExpInfo <| EList space1 (List.map ((,) space0) a) space0 b space0
eListWs a b       = withDummyExpInfo <| EList space1 a space0 b space0
eTuple0 a         = eList0 a Nothing
eTuple a          = eList a Nothing
eHoleVal0 v       = withDummyExpInfo <| EHole space0 (Just v)
eHoleVal v        = withDummyExpInfo <| EHole space1 (Just v)

eColonType e t    = withDummyExpInfo <| EColonType space1 e space1 (withDummyRange t) space0

eComment a b   = withDummyExpInfo <| EComment space1 a b

pVar0 a        = withDummyPatInfo <| PVar space0 a noWidgetDecl
pVar a         = withDummyPatInfo <| PVar space1 a noWidgetDecl
pList0 ps      = withDummyPatInfo <| PList space0 ps space0 Nothing space0
pList ps       = withDummyPatInfo <| PList space1 ps space0 Nothing space0
pAs x p        = withDummyPatInfo <| PAs space1 x space1 p

pListOfPVars names = pList (listOfPVars names)

eLetUnapply: Exp -> Maybe ((Ident, Exp), Exp)
eLetUnapply e = case e.val.e__ of
  ELet _ _ False p _ assign _ bodyExp _ ->
    case p.val.p__ of
      PVar _ ident _ -> Just ((ident, assign), bodyExp)
      _ -> Nothing
  _ -> Nothing

eFunUnapply: Exp -> Maybe (List Pat, Exp)
eFunUnapply e = case e.val.e__ of
  EFun _ ps body _ -> Just (ps, body)
  _ -> Nothing

eRecordUnapply: Exp -> Maybe (List (String, Exp))
eRecordUnapply e = case e.val.e__ of
  ERecord _ Nothing es _ -> Just <| List.map (\(_, _, k, _, v) -> (k, v)) es
  _ -> Nothing

eVarUnapply e = case e.val.e__ of
  EVar _ s -> Just s
  _ -> Nothing

eStrUnapply e = case e.val.e__ of
  EBase _ (EString _ s) -> Just s
  _ -> Nothing

eConstUnapply e = case e.val.e__ of
  EConst _ n _ _ -> Just n
  _ -> Nothing

eListUnapply e = case e.val.e__ of
  EList _ elems _ Nothing  _-> Just <| List.map Tuple.second elems
  _ -> Nothing

eListUnapplyWS e = case e.val.e__ of
  EList _ elems _ Nothing  _-> Just <| elems
  _ -> Nothing

eOpUnapply1 expectedOp e = case e.val.e__ of
  EOp _ op [arg] _ -> if op.val == expectedOp then Just arg else Nothing
  _ -> Nothing

eOpUnapply2 expectedOp e = case e.val.e__ of
  EOp _ op [e1, e2] _ -> if op.val == expectedOp then Just (e1, e2) else Nothing
  _ -> Nothing


eAppUnapply1 e = case e.val.e__ of
  EApp _ e1 [e2] _ _ -> Just (e1, e2)
  _ -> Nothing

eAppUnapply2 e = case e.val.e__ of
  EApp _ e1 [e2, e3] _ _ -> Just (e1, e2, e3)
  _ -> Nothing

eAppUnapply e = case e.val.e__ of
  EApp _ e1 es _ _ -> Just (e1, es)
  _ -> Nothing

vStrUnapply v = case v.v_ of
  VBase (VString s) -> Just s
  _ -> Nothing

vListUnapply v = case v.v_ of
  VList elems -> Just elems
  _ -> Nothing


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


precedingWhitespace : Exp -> String
precedingWhitespace exp =
  precedingWhitespaceExp__ exp.val.e__


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


precedingWhitespacePat : Pat -> String
precedingWhitespacePat pat =
  .val <|
    case pat.val.p__ of
      PVar   ws ident wd         -> ws
      PConst ws n                -> ws
      PBase  ws v                -> ws
      PWildcard ws               -> ws
      PList  ws1 ps ws2 rest ws3 -> ws1
      PRecord ws1 es ws2      -> ws1
      PAs    ws1 ident ws2 p     -> ws1
      PParens ws1 p ws2          -> ws1

precedingWhitespaceWithInfoExp : Exp -> WS
precedingWhitespaceWithInfoExp e =
  precedingWhitespaceWithInfoExp__ e.val.e__


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
    EOp        ws1 op es ws2                    -> ws1
    EIf        ws1 e1 ws2 e2 ws3 e3 ws4         -> ws1
    ELet       ws1 kind rec p ws2 e1 ws3 e2 ws4 -> ws1
    ECase      ws1 e1 bs ws2                    -> ws1
    ETypeCase  ws1 e1 bs ws2                    -> ws1
    EComment   ws s e1                          -> ws
    EOption    ws1 s1 ws2 s2 e1                 -> ws1
    ETyp       ws1 pat tipe e ws2               -> ws1
    EColonType ws1 e ws2 tipe ws3               -> ws1
    ETypeAlias ws1 pat tipe e ws2               -> ws1
    ETypeDef   ws1 ident vars ws2 dcs rest ws3  -> ws1
    EParens    ws1 e pStyle ws2                 -> ws1
    EHole      ws mv                            -> ws


allWhitespaces : Exp -> List String
allWhitespaces exp =
  allWhitespaces_ exp |> List.map .val


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
  case exp.val.e__ of
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
    ERecord    ws1 mi es ws2                -> [ws1]
                                                 ++ (mi |> Maybe.map (Tuple.first >> allWhitespaces_) |> Maybe.withDefault [])
                                                 ++ List.concatMap (\(wsc, wsk, _, wse, v) -> [wsc, wsk, wse] ++ allWhitespaces_ v) es
                                                 ++ [ws2]
    ESelect    ws0 e ws1 ws2 s              -> allWhitespaces_ e ++ [ws1, ws2]
    EOp        ws1 op es ws2                -> [ws1] ++ List.concatMap allWhitespaces_ es ++ [ws2]
    EIf        ws1 e1 ws2 e2 ws3 e3 ws4     -> [ws1] ++ allWhitespaces_ e1
                                                 ++ [ws2] ++ allWhitespaces_ e2
                                                 ++ [ws3] ++ allWhitespaces_ e3
                                                 ++ [ws4]
    ELet       ws1 kind rec p ws2 e1 ws3 e2 ws4 -> [ws1]
                                                     ++ allWhitespacesPat_ p
                                                     ++ [ws2]
                                                     ++ allWhitespaces_ e1
                                                     ++ [ws3]
                                                     ++ allWhitespaces_ e2
                                                     ++ [ws4]
    ECase      ws1 e1 bs ws2                -> [ws1] ++ allWhitespaces_ e1 ++ List.concatMap allWhitespacesBranch bs ++ [ws2]
    ETypeCase  ws1 e1 bs ws2                -> [ws1] ++ allWhitespaces_ e1 ++ List.concatMap allWhitespacesTBranch bs ++ [ws2]
    EComment   ws s e1                      -> [ws] ++ allWhitespaces_ e1
    EOption    ws1 s1 ws2 s2 e1             -> [ws1, ws2] ++ allWhitespaces_ e1
    ETyp       ws1 pat tipe e ws2           -> [ws1] ++ allWhitespacesPat_ pat ++ allWhitespacesType_ tipe ++ allWhitespaces_ e ++ [ws2]
    EColonType ws1 e ws2 tipe ws3           -> [ws1] ++ allWhitespaces_ e ++ [ws2] ++ allWhitespacesType_ tipe ++ [ws2]
    ETypeAlias ws1 pat tipe e ws2           -> [ws1] ++ allWhitespacesPat_ pat ++ allWhitespacesType_ tipe ++ allWhitespaces_ e ++ [ws2]
    ETypeDef ws1 (wsBeforeIdent, ident)
             vars ws2 dcs rest ws3          -> [ws1, wsBeforeIdent] ++
                                                 List.map Tuple.first vars ++
                                                 [ws2] ++
                                                 List.concatMap
                                                   ( \(dcws1, _, ts, dcws2) ->
                                                       [dcws1] ++
                                                         List.concatMap allWhitespacesType_ ts ++
                                                         [dcws2]
                                                   )
                                                   dcs ++
                                                 allWhitespaces_ rest ++
                                                 [ws3]
    EParens    ws1 e pStyle ws2             -> [ws1] ++ allWhitespaces_ e ++ [ws2]
    EHole      ws mv                        -> [ws]


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
    PAs    ws1 ident ws2 p     -> [ws1, ws2] ++ allWhitespacesPat_ p
    PParens ws1 p ws2          -> [ws1, ws2] ++ allWhitespacesPat_ p

allWhitespacesType_ : Type -> List WS
allWhitespacesType_ tipe =
  let allWhitespacesForAllInner oneOrMany =
    case oneOrMany of
      One (ws, ident) -> [ws]
      Many ws1 inner ws2 -> [ws1] ++ List.map (\(ws, ident) -> ws) inner ++ [ws2]
  in
    case tipe.val of
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
      TApp ws ident ts                    -> [ws] ++ List.concatMap allWhitespacesType_ ts
      TVar ws ident                       -> [ws]
      TForall ws1 innerOneOrMany tipe ws2 -> [ws1] ++ allWhitespacesForAllInner innerOneOrMany ++ allWhitespacesType_ tipe ++ [ws2]
      TWildcard ws                        -> [ws]


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


-- Does not recurse.
mapPrecedingWhitespace : (String -> String) -> Exp -> Exp
mapPrecedingWhitespace stringMap exp =
  let
    mapWs s =
      ws (stringMap s.val)
    e__New =
      case exp.val.e__ of
        EBase      ws v                             -> EBase      (mapWs ws) v
        EConst     ws n l wd                        -> EConst     (mapWs ws) n l wd
        EVar       ws x                             -> EVar       (mapWs ws) x
        EFun       ws1 ps e1 ws2                    -> EFun       (mapWs ws1) ps e1 ws2
        EApp       ws1 e1 es apptype ws2            -> EApp       (mapWs ws1) e1 es apptype ws2
        EList      ws1 es ws2 rest ws3              -> EList      (mapWs ws1) es ws2 rest ws3
        ERecord    ws1 mi es ws2                    -> ERecord    (mapWs ws1) mi es ws2
        ESelect    ws1 e ws2 ws3 s                  -> ESelect    (mapWs ws1) e ws2 ws3 s
        EOp        ws1 op es ws2                    -> EOp        (mapWs ws1) op es ws2
        EIf        ws1 e1 ws2 e2 ws3 e3 ws4         -> EIf        (mapWs ws1) e1 ws2 e2 ws3 e3 ws4
        ELet       ws1 kind rec p ws2 e1 ws3 e2 ws4 -> ELet       (mapWs ws1) kind rec p ws2 e1 ws3 e2 ws4
        ECase      ws1 e1 bs ws2                    -> ECase      (mapWs ws1) e1 bs ws2
        ETypeCase  ws1 e1 bs ws2                    -> ETypeCase  (mapWs ws1) e1 bs ws2
        EComment   ws s e1                          -> EComment   (mapWs ws) s e1
        EOption    ws1 s1 ws2 s2 e1                 -> EOption    (mapWs ws1) s1 ws2 s2 e1
        ETyp       ws1 pat tipe e ws2               -> ETyp       (mapWs ws1) pat tipe e ws2
        EColonType ws1 e ws2 tipe ws3               -> EColonType (mapWs ws1) e ws2 tipe ws3
        ETypeAlias ws1 pat tipe e ws2               -> ETypeAlias (mapWs ws1) pat tipe e ws2
        ETypeDef   ws1 ident vars ws2 dcs rest ws3  -> ETypeDef   (mapWs ws1) ident vars ws2 dcs rest ws3
        EParens    ws e pStyle ws2                  -> EParens    (mapWs ws) e pStyle ws2
        EHole      ws mv                            -> EHole      (mapWs ws) mv
  in
    replaceE__ exp e__New


mapPrecedingWhitespacePat : (String -> String) -> Pat -> Pat
mapPrecedingWhitespacePat stringMap pat =
  let
    mapWs s =
      ws (stringMap s.val)
    p__ =
      case pat.val.p__ of
        PVar   ws ident wd         -> PVar   (mapWs ws) ident wd
        PConst ws n                -> PConst (mapWs ws) n
        PBase  ws v                -> PBase  (mapWs ws) v
        PWildcard ws               -> PWildcard (mapWs ws)
        PList  ws1 ps ws2 rest ws3 -> PList  (mapWs ws1) ps ws2 rest ws3
        PRecord ws1 ps ws2         -> PRecord (mapWs ws1) ps ws2
        PAs    ws1 ident ws2 p     -> PAs    ws1 ident ws2 (mapPrecedingWhitespacePat stringMap p)
        PParens ws1 p ws2          -> PParens (mapWs ws1) p ws2
  in
    replaceP__ pat p__


ensureWhitespace : String -> String
ensureWhitespace s =
  if s == "" then " " else s


ensureWhitespaceExp : Exp -> Exp
ensureWhitespaceExp exp =
  mapPrecedingWhitespace ensureWhitespace exp


ensureWhitespacePat : Pat -> Pat
ensureWhitespacePat pat =
  mapPrecedingWhitespacePat ensureWhitespace pat

ensureWhitespaceNNewlines : Int -> String -> String
ensureWhitespaceNNewlines n s =
  let newlineCount = List.length (String.split "\n" s) - 1 in
  String.repeat (n - newlineCount) "\n" ++ s
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
    String.repeat n "\n" ++ indentationIfNoPreviousNewlines
  else if previousNewlineCount < n then
    String.repeat n "\n" ++ extractIndentation ws
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
--   case (templateList.val.e__, list.val.e__) of
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
      []               -> if String.contains "\n" nextWs then (indentWs "  " nextWs, indentWs "  " nextWs) else ("", " ")
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
  indent spaces (unindent exp)


-- Finds lowest amount of indentation and then removes it from all expressions.
unindent : Exp -> Exp
unindent exp =
  let expWsAsSpaces = mapExp (mapPrecedingWhitespace tabsToSpaces) exp in
  let smallestIndentation =
    expWsAsSpaces
    |> foldExpViaE__
        (\e__ smallest ->
          case Regex.find Regex.All (Regex.regex "\n( *)$") (precedingWhitespaceExp__ e__) |> List.map .submatches |> List.concat of
            [Just indentation] -> if String.length indentation < String.length smallest then indentation else smallest
            _                  -> smallest
        )
        (String.repeat 100 " ")
  in
  let removeIndentation ws =
    ws |> Regex.replace Regex.All (Regex.regex ("\n" ++ smallestIndentation)) (\_ -> "\n")
  in
  mapExp (mapPrecedingWhitespace removeIndentation) expWsAsSpaces

indentWs : String -> String -> String
indentWs spaces ws =
  ws |> String.reverse
     |> Regex.replace (Regex.AtMost 1) (Regex.regex "\n") (\_ -> spaces ++ "\n")
     |> String.reverse

-- Increases indentation by spaces string.
indent : String -> Exp -> Exp
indent spaces e =
  mapExp (mapPrecedingWhitespace (indentWs spaces)) e

-- Same as indent, but always push top level exp right even if no newline.
pushRight : String -> Exp -> Exp
pushRight spaces e =
  indent spaces e
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

type CodeObject
  = E Exp -- Expression
  | P Exp Pat -- Pattern; knows own parent
  | T Type -- Type
  | LBE (WithInfo EId) -- Let binding equation
  | ET BeforeAfter WS Exp -- Exp target
  | PT BeforeAfter WS Exp Pat -- Pat target (knows the parent of its target)
  | TT BeforeAfter WS Type -- Type target

extractInfoFromCodeObject : CodeObject -> WithInfo CodeObject
extractInfoFromCodeObject codeObject =
  case codeObject of
    E e ->
      { e | val = codeObject }
    P _ p ->
      { p | val = codeObject }
    T t ->
      { t | val = codeObject }
    LBE eid ->
      { eid | val = codeObject }
    ET _ ws _ ->
      { ws | val = codeObject }
    PT _ ws _ _ ->
      { ws | val = codeObject }
    TT _ ws _ ->
      { ws | val = codeObject }

isTarget : CodeObject -> Bool
isTarget codeObject =
  case codeObject of
    E _ ->
      False
    P _ _ ->
      False
    T _ ->
      False
    LBE _ ->
      False
    ET _ _ _ ->
      True
    PT _ _ _ _ ->
      True
    TT _ _ _ ->
      True

isSelectable : CodeObject -> Bool
isSelectable codeObject =
  case codeObject of
    E e ->
      case e.val.e__ of
        EComment _ _ _ ->
          False
        EOption _ _ _ _ _ ->
          False
        _ ->
          True
    _ ->
      True

isTextSelectable : CodeObject -> Bool
isTextSelectable codeObject =
  (isSelectable codeObject) &&
  (not <| isTarget codeObject)

isWord : CodeObject -> Bool
isWord codeObject =
  case codeObject of
    E e ->
      case e.val.e__ of
        (EConst _ _ _ _) ->
          True
        (EBase _ _) ->
          True
        (EVar _ _) ->
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
    E e ->
      precedingWhitespaceWithInfoExp__ e.val.e__

    P e p ->
      case p.val.p__ of
        PVar ws _ _ ->
          ws
        PConst ws _ ->
          ws
        PBase ws _ ->
          ws
        PWildcard ws ->
          ws
        PList ws _ _ _ _ ->
          ws
        PRecord ws _ _ ->
          ws
        PAs _ _ _ p ->
          wsBefore <| P e p
        PParens ws _ _ ->
          ws
    T t ->
      case t.val of
        TNum ws ->
          ws
        TBool ws ->
          ws
        TString ws ->
          ws
        TNull ws ->
          ws
        TList ws _ _ ->
          ws
        TDict ws _ _ _ ->
          ws
        TTuple ws _ _ _ _ ->
          ws
        TRecord ws _ _ _ ->
          ws
        TArrow ws _ _ ->
          ws
        TUnion ws _ _ ->
          ws
        TApp ws _ _ ->
          ws
        TVar ws _ ->
          ws
        TForall ws _ _ _ ->
          ws
        TWildcard ws ->
          ws
    LBE eid ->
      { start =
          eid.start
      , end =
          eid.end
      , val =
          ""
      }
    ET _ ws _ ->
      ws
    PT _ ws _ _ ->
      ws
    TT _ ws _ ->
      ws

modifyWsBefore : (WS -> WS) -> CodeObject -> CodeObject
modifyWsBefore f codeObject =
  case codeObject of
    E e ->
      let
        eVal =
          e.val
        newE__ =
          case eVal.e__ of
            EConst ws a b c ->
              EConst (f ws) a b c
            EBase ws a ->
              EBase (f ws) a
            EVar ws a ->
              EVar (f ws) a
            EFun ws a b c ->
              EFun (f ws) a b c
            EApp ws a b c d ->
              EApp (f ws) a b c d
            EOp ws a b c ->
              EOp (f ws) a b c
            EList ws a b c d ->
              EList (f ws) a b c d
            ERecord ws a b c ->
              ERecord (f ws) a b c
            ESelect ws a b c d ->
              ESelect (f ws) a b c d
            EIf ws a b c d e_ f_ ->
              EIf (f ws) a b c d e_ f_
            ECase ws a b c  ->
              ECase (f ws) a b c
            ETypeCase ws a b c ->
              ETypeCase (f ws) a b c
            ELet ws a b c d e_ f_ g h ->
              ELet (f ws) a b c d e_ f_ g h
            EComment ws a b ->
              EComment (f ws) a b
            EOption ws a b c d ->
              EOption (f ws) a b c d
            ETyp ws a b c d  ->
              ETyp (f ws) a b c d
            EColonType ws a b c d ->
              EColonType (f ws) a b c d
            ETypeAlias ws a b c d ->
              ETypeAlias (f ws) a b c d
            ETypeDef ws a b c d e_ f_ ->
              ETypeDef (f ws) a b c d e_ f_
            EParens ws a pStyle b ->
              EParens (f ws) a pStyle b
            EHole ws mv ->
              EHole (f ws) mv
      in
        E { e | val = { eVal | e__ = newE__ } }
    P e p ->
      let
        pVal =
          p.val
        newP__ =
          case pVal.p__ of
            PVar ws a b ->
              PVar (f ws) a b
            PConst ws a ->
              PConst (f ws) a
            PBase ws a ->
              PBase (f ws) a
            PWildcard ws ->
              PWildcard (f ws)
            PList ws a b c d ->
              PList (f ws) a b c d
            PRecord ws a b ->
              PRecord (f ws) a b
            PAs ws ident ws2 p2 ->
              case modifyWsBefore f (P e p2) of
                P e3 p3 -> PAs ws ident ws2 p3
                _ -> Debug.crash "Internal error: modifyWsBefore cannot return something else than P if provided with P"
            PParens ws p2 ws2 ->
              PParens (f ws) p2 ws2
      in
        P e { p | val = { pVal | p__ = newP__ } }
    T t ->
      let
        newVal =
          case t.val of
            TNum ws ->
              TNum (f ws)
            TBool ws ->
              TBool (f ws)
            TString ws ->
              TString (f ws)
            TNull ws ->
              TNull (f ws)
            TList ws a b ->
              TList (f ws) a b
            TDict ws a b c ->
              TDict (f ws) a b c
            TTuple ws a b c d ->
              TTuple (f ws) a b c d
            TRecord ws c a b ->
              TRecord (f ws) c a b
            TArrow ws a b ->
              TArrow (f ws) a b
            TUnion ws a b ->
              TUnion (f ws) a b
            TApp ws a b ->
              TApp (f ws) a b
            TVar ws a ->
              TVar (f ws) a
            TForall ws a b c ->
              TForall (f ws) a b c
            TWildcard ws ->
              TWildcard (f ws)
      in
        T { t | val = newVal }
    LBE eid ->
      LBE eid
    ET a ws b ->
      ET a (f ws) b
    PT a ws b c ->
      PT a (f ws) b c
    TT a ws b ->
      TT a (f ws) b

isImplicitMain : Exp -> Bool
isImplicitMain e =
  case e.val.e__ of
    ELet _ _ _ name _ _ _ _ _ ->
      case name.val.p__ of
        PVar _ "_IMPLICIT_MAIN" _ ->
          True
        _ ->
          False
    _ ->
      False

isHiddenCodeObject : CodeObject -> Bool
isHiddenCodeObject codeObject =
  case codeObject of
    E e ->
      isImplicitMain e
    ET _ _ e ->
      isImplicitMain e
    _ ->
      False

childCodeObjects : CodeObject -> List CodeObject
childCodeObjects co =
  List.filter (not << isHiddenCodeObject) <|
    case co of
      E e ->
        case e.val.e__ of
          EConst ws1 _ _ _ ->
            [ ET Before ws1 e ]
          EBase ws1 _ ->
            [ ET Before ws1 e ]
          EVar ws1 _ ->
            [ ET Before ws1 e ]
          EFun ws1 ps e1 ws2 ->
            [ ET Before ws1 e
            ] ++
            ( List.map (P e) ps
            ) ++
            ( case Utils.maybeLast ps of
                Just pLast ->
                  [ PT After (WithInfo "" pLast.end pLast.end) e pLast
                  ]
                Nothing ->
                  []
            ) ++
            [ E e1
            , ET After ws2 e1
            ]
          EApp ws1 e1 es aoptype ws2 ->
            [ ET Before ws1 e
            , E e1
            ] ++
            ( List.map E es
            ) ++
            ( case Utils.maybeLast es of
                Just lastE ->
                  [ ET After ws2 lastE ]
                Nothing ->
                  []
            )
          EOp ws1 _ es ws2 ->
            [ ET Before ws1 e
            ] ++
            ( List.map E es
            ) ++
            ( case Utils.maybeLast es of
                Just lastE ->
                  [ ET After ws2 lastE ]
                Nothing ->
                  []
            )
          EList ws1 es ws2 m ws3  ->
            let
              lastHead =
                case Utils.maybeLast es of
                  Just (_,lastHead) ->
                    [ lastHead ]
                  Nothing ->
                    []
            in
              [ ET Before ws1 e
              ] ++
              ( List.map (E << Tuple.second) es
              ) ++
              ( case m of
                  Just eTail ->
                    ( List.map (ET After ws2) lastHead
                    ) ++
                    [ E eTail
                    , ET After ws3 eTail
                    ]
                  Nothing ->
                    ( List.map (ET After ws3) lastHead
                    )
              )
          ERecord ws1 mbExpWs listWsIdWsExpWs ws2 ->
            let
              firstValue =
                case listWsIdWsExpWs of
                  (_, _, _, _, firstValue) :: tail -> [firstValue]
                  [] -> []
            in
            [ ET Before ws1 e
            ] ++
            ( case mbExpWs of
                Just (eInit, ws2) ->
                  ( List.map (ET Before ws2) firstValue
                  ) ++
                  [ E eInit
                  , ET After ws2 eInit]
                Nothing ->
                  []
            ) ++
            ( List.map (E << (\(_, _, _, _, v) -> v)) listWsIdWsExpWs )
          ESelect ws0 e _ _ _ ->
              [ E e ]
          EIf ws1 e1 _ e2 _ e3 ws2 ->
              [ ET Before ws1 e
              , E e1
              , E e2
              , E e3
              , ET After ws2 e3
              ]
          ECase ws1 e1 branches _ ->
            [ ET Before ws1 e
            , E e1
            ] ++
            ( case List.head branches of
                Just b ->
                  case b.val of
                    Branch_ branchWS1 _ _ _ ->
                      [ ET After branchWS1 e1 ]
                Nothing ->
                  []
            ) ++
            ( List.concatMap
                ( .val >> \(Branch_ _ branchP branchE branchWS2) ->
                    [ P e branchP
                    , E branchE
                    , ET After branchWS2 branchE
                    ]
                )
                branches
            )
          ETypeCase ws1 e1 tbranches _ ->
            [ ET Before ws1 e
            , E e1
            ] ++
            ( case List.head tbranches of
                Just tb ->
                  case tb.val of
                    TBranch_ tbranchWS1 _ _ _ ->
                      [ ET After tbranchWS1 e1 ]
                Nothing ->
                  []
            ) ++
            ( List.concatMap
                ( .val >> \(TBranch_ _ branchT branchE branchWS2) ->
                    [ T branchT
                    , E branchE
                    , ET After branchWS2 branchE
                    ]
                )
                tbranches
            )
          ELet ws1 lk _ p1 ws2 e1 ws3 e2 ws4 ->
            let
              -- Altered Whitespace
              --
              -- In the following example little programs, * represents special1
              -- and ~ represents special2. Note that (+ 2 3) is e1.
              --
              -- Case 1:
              --   (def x*(+ 2 3))
              --
              -- Case 2:
              --   (def x******
              --   ~~~~(+ 2 3))
              (special1, special2) =
                let
                  e1Ws =
                    wsBefore << E <| e1
                in
                  -- Case 1
                  if e1Ws.start.line == e1Ws.end.line then
                    ( e1Ws
                    , withDummyInfo ""
                    )
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
                      ( { e1Ws | end = { breakpoint | col = 0 } }
                      , { e1Ws | start = breakpoint }
                      )
            in
              [ ET Before ws1 e
              , LBE
                  -- TODO LBE Bounds
                  { start =
                      { line =
                          e.start.line
                      , col =
                          -- TODO Make this work well with both Elm and Little.
                          -- We'd like to have something like:
                          --
                          --   if syntax == Syntax.Little && lk == Let then
                          --     e.start.col + 1
                          --   else
                          --     e.start.col
                          --
                          -- But this function doesn't know about syntax (yet)
                            e.start.col
                      }
                  , end =
                      if lk == Def then
                        e.end
                      else
                        e1.end
                  , val =
                      e.val.eid
                  }
              , P e p1
              , PT After special1 e p1
              , E e1
                  |> modifyWsBefore (always special2)
              ] ++
              ( case lk of
                  Let ->
                    [ E e2
                    , ET After ws4 e2
                    ]
                  Def ->
                    [ ET After ws4 e1
                    , E e2
                    ]
              )
          EComment ws1 _ e1 ->
            [ ET Before ws1 e
            , E e1
            ]
          EOption ws1 _ _ _ e1 ->
            [ ET Before ws1 e
            , E e1
            ]
          ETyp ws1 p1 t1 e1 ws2 ->
            [ ET Before ws1 e
            , P e p1
            , T t1
            , TT After ws2 t1
            , E e1
            ]
          EColonType _ e1 _ t1 _ ->
            [E e1, T t1]
          ETypeAlias ws1 p1 t1 e1 ws2 ->
            [ ET Before ws1 e
            , P e p1
            , T t1
            , TT After ws2 t1
            , E e1
            ]
          ETypeDef ws1 ident vars ws2 dcs e1 ws3 ->
            [ ET Before ws1 e
            ] ++
            ( List.concatMap
                ( \(_, _, ts, _) ->
                    List.map T ts
                )
                dcs
            ) ++
            [ E e1
            ]
          EParens ws1 e1 pStyle ws2 ->
            [ ET Before ws1 e
            , E e1
            , ET After ws2 e1
            ]
          EHole ws _ ->
            [ ET Before ws e ]
      P e p ->
        case p.val.p__ of
          PVar ws1 _ _ ->
            [ PT Before ws1 e p ]
          PConst ws1 _ ->
            [ PT Before ws1 e p ]
          PBase ws1 _ ->
            [ PT Before ws1 e p ]
          PWildcard ws1 ->
            [ ]
          PList ws1 ps ws2 m ws3 ->
            let
              lastHead =
                case Utils.maybeLast ps of
                  Just lastHead ->
                    [ lastHead ]
                  Nothing ->
                    []
            in
              [ PT Before ws1 e p
              ] ++
              ( List.map (P e) ps
              ) ++
              ( case m of
                  Just pTail ->
                    ( List.map (PT After ws2 e) lastHead
                    ) ++
                    [ P e pTail
                    , PT After ws3 e pTail
                    ]
                  Nothing ->
                    ( List.map (PT After ws3 e) lastHead
                    )
              )
          PRecord ws1 ps ws2 ->
            let
              lastHead =
                case Utils.maybeLast ps of
                  Just lastHead ->
                    [ Utils.recordValue lastHead ]
                  Nothing ->
                    []
            in
              [ PT Before ws1 e p
              ] ++
              List.map (P e) (Utils.recordValues ps)
              ++
              ( List.map (PT After ws2 e) lastHead
              )
          PAs ws1 _ _ p1 ->
            [ PT Before ws1 e p
            , P e p1
            ]
          PParens ws1 p1 ws2 ->
            [ PT Before ws1 e p1
            , P e p1
            , PT After ws2 e p1 ]
      T t ->
        case t.val of
          TNum ws1 ->
            [ TT Before ws1 t ]
          TBool ws1 ->
            [ TT Before ws1 t ]
          TString ws1 ->
            [ TT Before ws1 t ]
          TNull ws1 ->
            [ TT Before ws1 t ]
          TList ws1 t1 ws2 ->
            [ TT Before ws1 t
            , T t1
            , TT After ws2 t1
            ]
          TDict ws1 t1 t2 ws2 ->
            [ TT Before ws1 t
            , T t1
            , T t2
            , TT After ws2 t2
            ]
          TTuple ws1 ts ws2 m ws3 ->
            let
              lastHead =
                case Utils.maybeLast ts of
                  Just lastHead ->
                    [ lastHead ]
                  Nothing ->
                    []
            in
              [ TT Before ws1 t
              ] ++
              ( List.map T ts
              ) ++
              ( case m of
                  Just tTail ->
                    ( List.map (TT After ws2) lastHead
                    ) ++
                    [ T tTail
                    , TT After ws3 tTail
                    ]
                  Nothing ->
                    ( List.map (TT After ws3) lastHead
                    )
              )
          TRecord ws1 mb ts ws2 ->
            let
              lastHead =
                case Utils.maybeLast ts of
                  Just lastHead ->
                    [ Utils.recordValue lastHead ]
                  Nothing ->
                    []
            in
            [ TT Before ws1 t
            ]
            ++ List.map T (Utils.recordValues ts)
            ++
            ( List.map (TT After ws2) lastHead )
          TArrow ws1 ts ws2 ->
            let
              lastHead =
                case Utils.maybeLast ts of
                  Just lastHead ->
                    [ lastHead ]
                  Nothing ->
                    []
            in
              [ TT Before ws1 t
              ] ++
              ( List.map T ts
              ) ++
              ( List.map (TT After ws2) lastHead
              )
          TUnion ws1 ts ws2 ->
            let
              lastHead =
                case Utils.maybeLast ts of
                  Just lastHead ->
                    [ lastHead ]
                  Nothing ->
                    []
            in
              [ TT Before ws1 t
              ] ++
              ( List.map T ts
              ) ++
              ( List.map (TT After ws2) lastHead
              )
          TApp ws1 _ ts ->
            [ TT Before ws1 t
            ] ++
            ( List.map T ts
            )
          TVar ws1 _ ->
            [ TT Before ws1 t ]
          TForall ws1 _ t1 ws2 ->
            [ TT Before ws1 t
            , T t1
            , TT After ws2 t1
            ]
          TWildcard ws1 ->
            [ TT Before ws1 t ]
      LBE _ ->
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

hasPatWithPid : PId -> Exp -> Bool
hasPatWithPid pid =
  Utils.hasMatchingElement (hasPid pid) << childCodeObjects << E

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
        PAs _ _ _ p1 ->
          -- TODO Unsure if this is the right ppid (it is the same as the
          --      parent).
          tagSinglePat ppid p1
        PList _ ps _ Nothing _  ->
          tagPatList ppid ps
        PList _ ps _ (Just pTail) _ ->
          tagPatList ppid (ps ++ [pTail])
        PParens _ p1 _ ->
          tagSinglePat ppid p1
        PRecord ws1 listWsIdWsExpWs ws2 ->
          tagPatList ppid (Utils.recordValues listWsIdWsExpWs)

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

taggedExpPats : Exp -> List (PId, PathedPatternId)
taggedExpPats exp =
  case exp.val.e__ of
    EFun _ ps _ _ ->
      tagPatList (rootPathedPatternId (exp.val.eid, 1)) ps
    ECase _ _ branches _ ->
      tagBranchList exp.val.eid branches
    ELet _ _ _ p1 _ _ _ _ _ ->
      tagSinglePat (rootPathedPatternId (exp.val.eid, 1)) p1
    ETyp _ p1 _ _ _ ->
      tagSinglePat (rootPathedPatternId (exp.val.eid, 1)) p1
    ETypeAlias _ p1 _ _ _ ->
      tagSinglePat (rootPathedPatternId (exp.val.eid, 1)) p1
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
  case e.val.e__ of
    ELet _ Def _ _ _ _ _ eRest _ ->
      firstNestedExp eRest
    EComment _ _ eRest ->
      firstNestedExp eRest
    EOption _ _ _ _ eRest ->
      firstNestedExp eRest
    ETyp _ _ _ eRest _ ->
      firstNestedExp eRest
    ETypeAlias _ _ _ eRest _ ->
      firstNestedExp eRest
    _ ->
      e

--------------------------------------------------------------------------------
-- Fixing operators in Elm syntax
--------------------------------------------------------------------------------

--fixOps : Exp -> Exp
--fixOps =
--  let
--    fixOp e =
--      let
--        newE__ =
--          case e.val.e__ of
--            EApp ws1 function arguments ws2 ->
--              case function of
--                EVar wsBeforeIdentifier identifier ->
--                  case getOpFromIdentifier identifier of
--                    Just op ->
--                      EOp wsBeforeIdentifier op arguments ws2
--
--                _ ->
--                  e.val.e__
--
--  in
--    map fixOp

getTopLevelOptions: Exp -> List (String, String)
getTopLevelOptions e =
  case e.val.e__ of
    EOption _ wkey _ wValue following ->
      (wkey.val, wValue.val)::getTopLevelOptions following
    EComment _ _ eRest ->
      getTopLevelOptions eRest
    _ -> []

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
            | VUnoptimizedDiffs -- For benchmarking against no diff

type EDiffs = EConstDiffs EWhitespaceDiffs
            | EListDiffs (ListDiffs EDiffs)
            | EStringDiffs (List StringDiffs)
            | EChildDiffs (TupleDiffs EDiffs) -- Also for records

type EWhitespaceDiffs = EOnlyWhitespaceDiffs | EAnyDiffs

vListDiffsUnapply: VDiffs -> Maybe (ListDiffs VDiffs)
vListDiffsUnapply vdiffs = case vdiffs of
  VListDiffs d -> Just d
  _ -> Nothing

offsetStr: Int -> List StringDiffs -> List StringDiffs
offsetStr n diffs =
  --Debug.log ("computing offset of " ++ toString n ++ " on " ++ toString diffs)  <|
  List.map (\sd -> case sd of
    StringUpdate start end replaced -> StringUpdate (start + n) (end + n) replaced) diffs
