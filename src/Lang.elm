module Lang exposing (..)

import String
import Debug
import Dict exposing (Dict)
import Set
import Debug
import Regex

import Utils

--------------------------------------------------------------------------------
-- Pos
--------------------------------------------------------------------------------

type alias Pos =
  { line : Int
  , col : Int
  }

startPos : Pos
startPos =
  { line = 1
  , col = 1
  }

dummyPos : Pos
dummyPos =
  { line = -1
  , col = -1
  }

posFromRowCol : (Int, Int) -> Pos
posFromRowCol (row, col) =
  { line = row
  , col = col
  }

--------------------------------------------------------------------------------
-- WithPos
--------------------------------------------------------------------------------

type alias WithPos a =
  { val : a
  , pos : Pos
  }

--------------------------------------------------------------------------------
-- WithInfo
--------------------------------------------------------------------------------

type alias WithInfo a =
  { val : a
  , start : Pos
  , end : Pos
  }

withInfo : a -> Pos -> Pos -> WithInfo a
withInfo x start end =
  { val = x
  , start = start
  , end = end
  }

withDummyInfo : a -> WithInfo a
withDummyInfo x =
  withInfo x dummyPos dummyPos

mapInfo : (a -> b) -> WithInfo a -> WithInfo b
mapInfo f wa =
  { wa | val = f wa.val }

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

-- TODO add constant literals to patterns, and match 'svg'
type Pat__
  = PVar WS Ident WidgetDecl
  | PConst WS Num
  | PBase WS EBaseVal
  | PList WS (List Pat) WS (Maybe Pat) WS
  | PAs WS Ident WS Pat

type Op_
  -- nullary ops
  = Pi
  | DictEmpty
  -- unary ops
  | Cos | Sin | ArcCos | ArcSin
  | Floor | Ceil | Round
  | ToStr
  | Sqrt
  | Explode
  | DebugLog
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

type alias EId  = Int
type alias PId  = Int
type alias Exp_ = { e__ : Exp__, eid : EId }
type alias Pat_ = { p__ : Pat__, pid : PId }

--------------------------------------------------------------------------------
-- The following expressions count as "top-level":
--   * definition (def, not let)
--   * comment
--   * option
--   * type declaration
--   * type alias
--------------------------------------------------------------------------------

type Exp__
  = EConst WS Num Loc WidgetDecl
  | EBase WS EBaseVal
  | EVar WS Ident
  | EFun WS (List Pat) Exp WS -- WS: before (, before )
  -- TODO remember paren whitespace for multiple pats, like TForall
  -- | EFun WS (OneOrMany Pat) Exp WS
  | EApp WS Exp (List Exp) WS
  | EOp WS Op (List Exp) WS
  | EList WS (List Exp) WS (Maybe Exp) WS
  | EIf WS Exp Exp Exp WS
  | ECase WS Exp (List Branch) WS
  | ETypeCase WS Pat (List TBranch) WS
  | ELet WS LetKind Rec Pat Exp Exp WS
  | EComment WS String Exp
  | EOption WS (WithInfo String) WS (WithInfo String) Exp
  | ETyp WS Pat Type Exp WS
  | EColonType WS Exp WS Type WS
  | ETypeAlias WS Pat Type Exp WS

    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))

    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type Type_
  = TNum WS
  | TBool WS
  | TString WS
  | TNull WS
  | TList WS Type WS
  | TDict WS Type Type WS
  | TTuple WS (List Type) WS (Maybe Type) WS
  | TArrow WS (List Type) WS
  | TUnion WS (List Type) WS
  | TNamed WS Ident
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

type Widget
  = WIntSlider Int Int String Int Loc Bool
  | WNumSlider Num Num String Num Loc Bool
  | WPoint NumTr NumTr
  | WOffset1D NumTr NumTr Axis Sign NumTr

type alias Widgets = List Widget

type alias Token = WithInfo String

type alias Caption = Maybe (WithInfo String)

type alias VTrace = List EId
type alias Val    = { v_ : Val_, vtrace : VTrace }

type Val_
  = VConst (Maybe (Axis, NumTr)) NumTr -- Maybe (Axis, value in other dimention)
  | VBase VBaseVal
  | VClosure (Maybe Ident) Pat Exp Env
  | VList (List Val)
  | VDict VDict_

type alias VDict_ = Dict.Dict (String, String) Val

type alias NumTr = (Num, Trace)

defaultQuoteChar = "'"
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
  patPathRightSibling path
  |> Maybe.map (\newPath -> (scopeId, newPath))

patPathRightSibling : List Int -> Maybe (List Int)
patPathRightSibling path =
  Utils.maybeMapLastElement ((+) 1) path

pathAfterElementRemoved : List Int -> List Int -> Maybe (List Int)
pathAfterElementRemoved removedPath path =
  case (removedPath, path) of
    ([removedI], [pathI]) ->
      if removedI <= pathI
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
            patPathRightSibling referencePathAsPList
            |> Utils.fromJust_ ("invalid target pattern id path of [] in target path position: " ++ toString (beforeAfter, referencePathedPatId))
  in
    (referenceScopeId, targetPath)

------------------------------------------------------------------------------
-- Unparsing

strBaseVal v = case v of
  VBool True  -> "true"
  VBool False -> "false"
  VString s   -> "'" ++ s ++ "'"
  VNull       -> "null"

strVal     = strVal_ False
strValLocs = strVal_ True

strNum     = toString
-- strNumDot  = strNum >> (\s -> if String.contains "[.]" s then s else s ++ ".0")

strNumTrunc k =
  strNum >> (\s -> if String.length s > k then String.left k s ++ ".." else s)

strVal_ : Bool -> Val -> String
strVal_ showTraces v =
  let foo = strVal_ showTraces in
  let sTrace = if showTraces then Utils.braces (toString v.vtrace) else "" in
  sTrace ++
  case v.v_ of
    VConst maybeAxis (i,tr) -> strNum i ++ if showTraces then Utils.angleBracks (toString maybeAxis) ++ Utils.braces (strTrace tr) else ""
    VBase b                 -> strBaseVal b
    VClosure _ _ _ _        -> "<fun>"
    VList vs                -> Utils.bracks (String.join " " (List.map foo vs))
    VDict d                 -> "<dict " ++ (Dict.toList d |> List.map (\(k, v) -> (toString k) ++ ":" ++ (foo v)) |> String.join " ") ++ ">"

strOp op = case op of
  Plus          -> "+"
  Minus         -> "-"
  Mult          -> "*"
  Div           -> "/"
  Lt            -> "<"
  Eq            -> "="
  Pi            -> "pi"
  Cos           -> "cos"
  Sin           -> "sin"
  ArcCos        -> "arccos"
  ArcSin        -> "arcsin"
  ArcTan2       -> "arctan2"
  Floor         -> "floor"
  Ceil          -> "ceiling"
  Round         -> "round"
  ToStr         -> "toString"
  Explode       -> "explode"
  Sqrt          -> "sqrt"
  Mod           -> "mod"
  Pow           -> "pow"
  DictEmpty     -> "empty"
  DictInsert    -> "insert"
  DictGet       -> "get"
  DictRemove    -> "remove"
  DebugLog      -> "debug"

strLoc (k, b, mx) =
  "k" ++ toString k ++ (if mx == "" then "" else "_" ++ mx) ++ b

strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])

traceToMaybeIdent tr =
  case tr of
    TrLoc (_, _, ident) -> if ident /= "" then Just ident else Nothing
    _                   -> Nothing

tab k = String.repeat k "  "

-- TODO take into account indent and other prefix of current line
fitsOnLine s =
  if String.length s > 70 then False
  else if List.member '\n' (String.toList s) then False
  else True

isLet e = case e.val.e__ of
  ELet _ _ _ _ _ _ _ -> True
  -- EComment _ _ e1    -> isLet e1
  _                  -> False


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
  let wrap e__ = WithInfo (Exp_ e__ e.val.eid) e.start e.end in
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

    EApp ws1 e1 es ws2 ->
      let (newEs, newAcc)  = recurseAll initAcc es in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (EApp ws1 newE1 newEs ws2) newAcc2

    EOp ws1 op es ws2 ->
      let (newEs, newAcc) = recurseAll initAcc es in
      wrapAndMap (EOp ws1 op newEs ws2) newAcc

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newAcc) = recurseAll initAcc es in
      wrapAndMap (EList ws1 newEs ws2 Nothing ws3) newAcc

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newE1, newAcc)  = recurse initAcc e1 in
      let (newEs, newAcc2) = recurseAll newAcc es in
      wrapAndMap (EList ws1 newEs ws2 (Just newE1) ws3) newAcc2

    EIf ws1 e1 e2 e3 ws2 ->
      case recurseAll initAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newAcc) -> wrapAndMap (EIf ws1 newE1 newE2 newE3 ws2) newAcc
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

    ETypeCase ws1 pat tbranches ws2 ->
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
      wrapAndMap (ETypeCase ws1 pat newBranches ws2) newAcc

    EComment ws s e1 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EComment ws s newE1) newAcc

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EOption ws1 s1 ws2 s2 newE1) newAcc

    ELet ws1 k b p e1 e2 ws2 ->
      let (newE2, newAcc) = recurse initAcc e2 in
      let (newE1, newAcc2) = recurse newAcc e1 in
      wrapAndMap (ELet ws1 k b p newE1 newE2 ws2) newAcc2

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (ETyp ws1 pat tipe newE1 ws2) newAcc

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (EColonType ws1 newE1 ws2 tipe ws3) newAcc

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc) = recurse initAcc e1 in
      wrapAndMap (ETypeAlias ws1 pat tipe newE1 ws2) newAcc


-- Nodes visited/replaced in top-down, left-to-right order.
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

    EApp ws1 e1 es ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      let (newEs, newAcc3) = recurseAll newAcc2 es in
      ret (EApp ws1 newE1 newEs ws2) newAcc3

    EOp ws1 op es ws2 ->
      let (newEs, newAcc2) = recurseAll newAcc es in
      ret (EOp ws1 op newEs ws2) newAcc2

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newAcc2) = recurseAll newAcc es in
      ret (EList ws1 newEs ws2 Nothing ws3) newAcc2

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newEs, newAcc2) = recurseAll newAcc es in
      let (newE1, newAcc3) = recurse newAcc2 e1 in
      ret (EList ws1 newEs ws2 (Just newE1) ws3) newAcc3

    EIf ws1 e1 e2 e3 ws2 ->
      case recurseAll newAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newAcc2) -> ret (EIf ws1 newE1 newE2 newE3 ws2) newAcc2
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

    ETypeCase ws1 pat tbranches ws2 ->
      let (newBranches, newAcc2) =
        tbranches
        |> List.foldl
            (\tbranch (newBranches, acc) ->
              let (TBranch_ bws1 t ei bws2) = tbranch.val in
              let (newEi, newAcc2) = recurse acc ei in
              (newBranches ++ [{ tbranch | val = TBranch_ bws1 t newEi bws2 }], newAcc2)
            )
            ([], newAcc)
      in
      ret (ETypeCase ws1 pat newBranches ws2) newAcc2

    EComment ws s e1 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (EComment ws s newE1) newAcc2

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (EOption ws1 s1 ws2 s2 newE1) newAcc2

    ELet ws1 k b p e1 e2 ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      let (newE2, newAcc3) = recurse newAcc2 e2 in
      ret (ELet ws1 k b p newE1 newE2 ws2) newAcc3

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (ETyp ws1 pat tipe newE1 ws2) newAcc2

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (EColonType ws1 newE1 ws2 tipe ws3) newAcc2

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, newAcc2) = recurse newAcc e1 in
      ret (ETypeAlias ws1 pat tipe newE1 ws2) newAcc2


-- Nodes visited/replaced in top-down, left-to-right order.
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

    PList ws1 ps ws2 Nothing ws3 ->
      let (newPs, newAcc2) = recurseAll newAcc ps in
      ret (PList ws1 newPs ws2 Nothing ws3) newAcc2

    PList ws1 ps ws2 (Just pTail) ws3 ->
      let (newPs, newAcc2)    = recurseAll newAcc ps in
      let (newPTail, newAcc3) = recurse newAcc2 pTail in
      ret (PList ws1 newPs ws2 (Just newPTail) ws3) newAcc3

    PAs ws1 ident ws2 pChild ->
      let (newPChild, newAcc2) = recurse newAcc pChild in
      ret (PAs ws1 ident ws2 newPChild) newAcc2


-- Nodes visited/replaced in top-down, left-to-right order.
-- Includes user-defined scope information.
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

    EApp ws1 e1 es ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      let (newEs, newGlobalAcc3) = recurseAll newGlobalAcc2 initScopeTempAcc es in
      ret (EApp ws1 newE1 newEs ws2) newGlobalAcc3

    EOp ws1 op es ws2 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc initScopeTempAcc es in
      ret (EOp ws1 op newEs ws2) newGlobalAcc2

    EList ws1 es ws2 Nothing ws3 ->
      let (newEs, newGlobalAcc2) = recurseAll newGlobalAcc initScopeTempAcc es in
      ret (EList ws1 newEs ws2 Nothing ws3) newGlobalAcc2

    EList ws1 es ws2 (Just e1) ws3 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      let (newEs, newGlobalAcc3) = recurseAll newGlobalAcc2 initScopeTempAcc es in
      ret (EList ws1 newEs ws2 (Just newE1) ws3) newGlobalAcc3

    EIf ws1 e1 e2 e3 ws2 ->
      case recurseAll newGlobalAcc initScopeTempAcc [e1, e2, e3] of
        ([newE1, newE2, newE3], newGlobalAcc2) -> ret (EIf ws1 newE1 newE2 newE3 ws2) newGlobalAcc2
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

    ETypeCase ws1 pat tbranches ws2 ->
      let (newBranches, newGlobalAcc2) =
        tbranches
        |> List.foldl
            (\tbranch (newBranches, globalAcc) ->
              let (TBranch_ bws1 t ei bws2) = tbranch.val in
              let (newEi, newGlobalAcc2) = recurse globalAcc initScopeTempAcc ei in
              (newBranches ++ [{ tbranch | val = TBranch_ bws1 t newEi bws2 }], newGlobalAcc2)
            )
            ([], newGlobalAcc)
      in
      ret (ETypeCase ws1 pat newBranches ws2) newGlobalAcc2

    EComment ws s e1 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (EComment ws s newE1) newGlobalAcc2

    EOption ws1 s1 ws2 s2 e1 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (EOption ws1 s1 ws2 s2 newE1) newGlobalAcc2

    ELet ws1 k isRec p e1 e2 ws2 ->
      let newScopeTempAcc = handleELet newE initScopeTempAcc in
      let (newE1, newGlobalAcc2) =
        if isRec
        then recurse newGlobalAcc newScopeTempAcc e1
        else recurse newGlobalAcc initScopeTempAcc e1
      in
      let (newE2, newGlobalAcc3) = recurse newGlobalAcc2 newScopeTempAcc e2 in
      ret (ELet ws1 k isRec p newE1 newE2 ws2) newGlobalAcc3

    ETyp ws1 pat tipe e1 ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (ETyp ws1 pat tipe newE1 ws2) newGlobalAcc2

    EColonType ws1 e1 ws2 tipe ws3 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (EColonType ws1 newE1 ws2 tipe ws3) newGlobalAcc2

    ETypeAlias ws1 pat tipe e1 ws2 ->
      let (newE1, newGlobalAcc2) = recurse newGlobalAcc initScopeTempAcc e1 in
      ret (ETypeAlias ws1 pat tipe newE1 ws2) newGlobalAcc2


mapExp : (Exp -> Exp) -> Exp -> Exp
mapExp f e =
  -- Accumulator thrown away; just need something that type checks.
  let (newExp, _) = mapFoldExp (\exp _ -> (f exp, ())) () e in
  newExp

mapExpTopDown : (Exp -> Exp) -> Exp -> Exp
mapExpTopDown f e =
  -- Accumulator thrown away; just need something that type checks.
  let (newExp, _) = mapFoldExpTopDown (\exp _ -> (f exp, ())) () e in
  newExp

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
  :  (Exp -> a -> b -> a)
  -> (Exp -> b -> b)
  -> (Exp -> b -> b)
  -> (Exp -> Branch -> Int -> b -> b)
  -> a
  -> b
  -> Exp
  -> a
foldExpTopDownWithScope f handleELet handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e =
  let (_, finalGlobalAcc) =
    mapFoldExpTopDownWithScope
        (\e globalAcc scopeTempAcc -> (e, f e globalAcc scopeTempAcc))
        handleELet handleEFun handleCaseBranch initGlobalAcc initScopeTempAcc e
  in
  finalGlobalAcc

mapVal : (Val -> Val) -> Val -> Val
mapVal f v = case v.v_ of
  VList vs         -> f { v | v_ = VList (List.map (mapVal f) vs) }
  VDict d          -> f { v | v_ = VDict (Dict.map (\_ v -> mapVal f v) d) } -- keys ignored
  VConst _ _       -> f v
  VBase _          -> f v
  VClosure _ _ _ _ -> f v

foldVal : (Val -> a -> a) -> Val -> a -> a
foldVal f v a = case v.v_ of
  VList vs         -> f v (List.foldl (foldVal f) a vs)
  VDict d          -> f v (List.foldl (foldVal f) a (Dict.values d)) -- keys ignored
  VConst _ _       -> f v a
  VBase _          -> f v a
  VClosure _ _ _ _ -> f v a

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
replaceExpNodes : (Dict.Dict EId Exp) -> Exp -> Exp
replaceExpNodes eidToNewNode root =
  mapExpTopDown -- top down to handle replacements that are subtrees of each other; a naive eidToNewNode could however make this loop forever
    (\exp ->
      case Dict.get exp.val.eid eidToNewNode of
        Just newExp -> newExp
        Nothing     -> exp
    )
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
    TNamed _ _   -> f tipe
    TVar _ _     -> f tipe
    TWildcard _  -> f tipe

    TList ws1 t1 ws2        -> f (wrap (TList ws1 (recurse t1) ws2))
    TDict ws1 t1 t2 ws2     -> f (wrap (TDict ws1 (recurse t1) (recurse t2) ws2))
    TArrow ws1 ts ws2       -> f (wrap (TArrow ws1 (List.map recurse ts) ws2))
    TUnion ws1 ts ws2       -> f (wrap (TUnion ws1 (List.map recurse ts) ws2))
    TForall ws1 vars t1 ws2 -> f (wrap (TForall ws1 vars (recurse t1) ws2))

    TTuple ws1 ts ws2 mt ws3 ->
      f (wrap (TTuple ws1 (List.map recurse ts) ws2 (Utils.mapMaybe recurse mt) ws3))

foldType : (Type -> a -> a) -> Type -> a -> a
foldType f tipe acc =
  let foldTypes f tipes acc = List.foldl (\t acc -> foldType f t acc) acc tipes in
  case tipe.val of
    TNum _          -> acc |> f tipe
    TBool _         -> acc |> f tipe
    TString _       -> acc |> f tipe
    TNull _         -> acc |> f tipe
    TNamed _ _      -> acc |> f tipe
    TVar _ _        -> acc |> f tipe
    TWildcard _     -> acc |> f tipe
    TList _ t _     -> acc |> foldType f t |> f tipe
    TDict _ t1 t2 _ -> acc |> foldType f t1 |> foldType f t2 |> f tipe
    TForall _ _ t _ -> acc |> foldType f t |> f tipe
    TArrow _ ts _   -> acc |> foldTypes f ts |> f tipe
    TUnion _ ts _   -> acc |> foldTypes f ts |> f tipe

    TTuple _ ts _ Nothing _  -> acc |> foldTypes f ts |> f tipe
    TTuple _ ts _ (Just t) _ -> acc |> foldTypes f (ts++[t]) |> f tipe


------------------------------------------------------------------------------
-- Traversing

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
  findFirstNode (\exp -> exp.val.eid == targetEId) program

-- justFindExpByEId is in LangTools (it needs the unparser for error messages).
-- LangTools.justFindExpByEId : EId -> Exp -> Exp
-- LangTools.justFindExpByEId eid exp =
--   findExpByEId exp eid
--   |> Utils.fromJust__ (\() -> "Couldn't find eid " ++ toString eid ++ " in " ++ unparseWithIds exp)

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
childExps e =
  case e.val.e__ of
    EConst _ _ _ _          -> []
    EBase _ _               -> []
    EVar _ _                -> []
    EFun ws1 ps e_ ws2      -> [e_]
    EOp ws1 op es ws2       -> es
    EList ws1 es ws2 m ws3  ->
      case m of
        Just e  -> es ++ [e]
        Nothing -> es
    EApp ws1 f es ws2               -> f :: es
    ELet ws1 k b p e1 e2 ws2        -> [e1, e2]
    EIf ws1 e1 e2 e3 ws2            -> [e1, e2, e3]
    ECase ws1 e branches ws2        -> e :: (branchExps branches)
    ETypeCase ws1 pat tbranches ws2 -> tbranchExps tbranches
    EComment ws s e1                -> [e1]
    EOption ws1 s1 ws2 s2 e1        -> [e1]
    ETyp ws1 pat tipe e ws2         -> [e]
    EColonType ws1 e ws2 tipe ws3   -> [e]
    ETypeAlias ws1 pat tipe e ws2   -> [e]


------------------------------------------------------------------------------
-- Conversion

valToTrace : Val -> Trace
valToTrace v = case v.v_ of
  VConst _  (_, trace) -> trace
  _                    -> Debug.crash "valToTrace"


------------------------------------------------------------------------------
-- Location Substitutions
-- Expression Substitutions

type alias Subst = Dict.Dict LocId Num
type alias SubstPlus = Dict.Dict LocId (WithInfo Num)
type alias SubstMaybeNum = Dict.Dict LocId (Maybe Num)

type alias ESubst = Dict.Dict EId Exp__

type alias TwoSubsts = { lsubst : Subst, esubst : ESubst }

-- For unparsing traces, possibily inserting variables: d
type alias SubstStr = Dict.Dict LocId String

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
type alias ESubst = Dict.Dict LocId Exp__

applyESubst : ESubst -> Exp -> Exp
applyESubst esubst =
  mapExpViaExp__ <| \e__ -> case e__ of
    EConst _ i -> case Dict.get (Utils.fst3 i) esubst of
                    Nothing   -> e__
                    Just e__' -> e__'
    _          -> e__
-}


-----------------------------------------------------------------------------
-- Utility

branchExps : List Branch -> List Exp
branchExps branches =
  List.map
    (.val >> \(Branch_ _ _ exp _) -> exp)
    branches

tbranchExps : List TBranch -> List Exp
tbranchExps tbranches =
  List.map
    (.val >> \(TBranch_ _ _ exp _) -> exp)
    tbranches

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

tbranchTypes : List TBranch -> List Type
tbranchTypes tbranches =
  List.map
    (.val >> \(TBranch_ _ tipe _ _) -> tipe)
    tbranches

branchPatExps : List Branch -> List (Pat, Exp)
branchPatExps branches =
  List.map
    (.val >> \(Branch_ _ pat exp _) -> (pat, exp))
    branches


-- Need parent expression since case expression branches into several scopes
isScope : Maybe Exp -> Exp -> Bool
isScope maybeParent exp =
  let isObviouslyScope =
    case exp.val.e__ of
      ELet _ _ _ _ _ _ _ -> True
      EFun _ _ _ _       -> True
      _                  -> False
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
    PVar _ x _              -> [x]
    PList _ ps _ Nothing _  -> List.concatMap varsOfPat ps
    PList _ ps _ (Just p) _ -> List.concatMap varsOfPat (p::ps)
    PAs _ x _ p             -> x::(varsOfPat p)


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
    PList _ ps _ Nothing _  -> ps
    PList _ ps _ (Just p) _ -> ps ++ [p]
    PAs _ _ _ p             -> [p]


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

errorPrefix = "[Little Error]" -- NOTE: same as errorPrefix in Native/codeBox.js
crashWithMsg s  = Debug.crash <| errorPrefix ++ "\n\n" ++ s
errorMsg s      = Err <| errorPrefix ++ "\n\n" ++ s

strPos p =
  let (i,j) = (toString p.line, toString p.col) in
  "(Line:" ++ i ++ " Col:" ++ j ++ ")"


------------------------------------------------------------------------------
-- Abstract Syntax Helpers

-- NOTE: the Exp builders use dummyPos

val : Val_ -> Val
val = flip Val [-1]

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
    EConst ws n (locId, annot, ident) wd -> replaceE__ eidCleared (EConst ws n (0, annot, "") wd)
    ELet ws1 kind b p e1 e2 ws2          -> replaceE__ eidCleared (ELet ws1 kind b (clearPIds p) e1 e2 ws2)
    EFun ws1 pats body ws2               -> replaceE__ eidCleared (EFun ws1 (List.map clearPIds pats) body ws2)
    ECase ws1 scrutinee branches ws2     -> replaceE__ eidCleared (ECase ws1 scrutinee (mapBranchPats clearPIds branches) ws2)
    ETypeCase ws1 pat tbranches ws2      -> replaceE__ eidCleared (ETypeCase ws1 (clearPIds pat) tbranches ws2)
    ETyp ws1 pat tipe e ws2              -> replaceE__ eidCleared (ETyp ws1 (clearPIds pat) tipe e ws2)
    ETypeAlias ws1 pat tipe e ws2        -> replaceE__ eidCleared (ETypeAlias ws1 (clearPIds pat) tipe e ws2)
    _                                    -> eidCleared

dummyLoc_ b = (0, b, "")
dummyTrace_ b = TrLoc (dummyLoc_ b)

dummyLoc = dummyLoc_ unann
dummyTrace = dummyTrace_ unann

eOp op_ es = withDummyExpInfo <| EOp space1 (withDummyRange op_) es space0

ePlus e1 e2 = eOp Plus [e1, e2]
eMinus e1 e2 = eOp Minus [e1, e2]

eBool  = withDummyExpInfo << EBase space1 << EBool
eStr   = withDummyExpInfo << EBase space1 << EString defaultQuoteChar
eStr0  = withDummyExpInfo << EBase space0 << EString defaultQuoteChar
eTrue  = eBool True
eFalse = eBool False

eApp e es = withDummyExpInfo <| EApp space1 e es space0
eFun ps e = withDummyExpInfo <| EFun space1 ps e space0

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

eLets xes eBody = case xes of
  (x,e)::xes_ -> eLet [(x,e)] (eLets xes_ eBody)
  []          -> eBody


-- Given [("a", aExp), ("b", bExp)] bodyExp
-- Produces (let [a b] [aExp bExp] bodyExp)
--
-- If given singleton list, produces a simple non-list let.
eLetOrDef : LetKind -> List (Ident, Exp) -> Exp -> Exp
eLetOrDef letKind namesAndAssigns bodyExp =
  let (pat, assign) = patBoundExpOf namesAndAssigns in
  withDummyExpInfo <| ELet newline1 letKind False pat assign bodyExp space0

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
eList0 a b        = withDummyExpInfo <| EList space0 a space0 b space0
eList a b         = withDummyExpInfo <| EList space1 a space0 b space0
eTuple0 a         = eList0 a Nothing
eTuple a          = eList a Nothing

eColonType e t    = withDummyExpInfo <| EColonType space1 e space1 (withDummyRange t) space0

eComment a b   = withDummyExpInfo <| EComment space1 a b

pVar0 a        = withDummyPatInfo <| PVar space0 a noWidgetDecl
pVar a         = withDummyPatInfo <| PVar space1 a noWidgetDecl
pList0 ps      = withDummyPatInfo <| PList space0 ps space0 Nothing space0
pList ps       = withDummyPatInfo <| PList space1 ps space0 Nothing space0
pAs x p        = withDummyPatInfo <| PAs space1 x space1 p

pListOfPVars names = pList (listOfPVars names)

-- note: dummy ids...
vTrue    = vBool True
vFalse   = vBool False
vBool    = val << VBase << VBool
vStr     = val << VBase << VString
vConst   = val << VConst Nothing
vBase    = val << VBase
vList    = val << VList
vDict    = val << VDict

unwrapVList : Val -> Maybe (List Val_)
unwrapVList v =
  case v.v_ of
    VList vs -> Just <| List.map .v_ vs
    _        -> Nothing

-- TODO names/types

unwrapVList_ : String -> Val -> List Val_
unwrapVList_ s v = case v.v_ of
  VList vs -> List.map .v_ vs
  _        -> Debug.crash <| "unwrapVList_: " ++ s

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
      PList  ws1 es ws2 rest ws3 -> ws1
      PAs    ws1 ident ws2 p     -> ws1


precedingWhitespaceExp__ : Exp__ -> String
precedingWhitespaceExp__ e__ =
  .val <|
    case e__ of
      EBase      ws v                     -> ws
      EConst     ws n l wd                -> ws
      EVar       ws x                     -> ws
      EFun       ws1 ps e1 ws2            -> ws1
      EApp       ws1 e1 es ws2            -> ws1
      EList      ws1 es ws2 rest ws3      -> ws1
      EOp        ws1 op es ws2            -> ws1
      EIf        ws1 e1 e2 e3 ws2         -> ws1
      ELet       ws1 kind rec p e1 e2 ws2 -> ws1
      ECase      ws1 e1 bs ws2            -> ws1
      ETypeCase  ws1 pat bs ws2           -> ws1
      EComment   ws s e1                  -> ws
      EOption    ws1 s1 ws2 s2 e1         -> ws1
      ETyp       ws1 pat tipe e ws2       -> ws1
      EColonType ws1 e ws2 tipe ws3       -> ws1
      ETypeAlias ws1 pat tipe e ws2       -> ws1

addPrecedingWhitespace : String -> Exp -> Exp
addPrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\oldWs -> oldWs ++ newWs) exp


replacePrecedingWhitespace : String -> Exp -> Exp
replacePrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\oldWs -> newWs) exp


replacePrecedingWhitespacePat : String -> Pat -> Pat
replacePrecedingWhitespacePat newWs pat =
  mapPrecedingWhitespacePat (\oldWs -> newWs) pat


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
        EBase      ws v                     -> EBase      (mapWs ws) v
        EConst     ws n l wd                -> EConst     (mapWs ws) n l wd
        EVar       ws x                     -> EVar       (mapWs ws) x
        EFun       ws1 ps e1 ws2            -> EFun       (mapWs ws1) ps e1 ws2
        EApp       ws1 e1 es ws2            -> EApp       (mapWs ws1) e1 es ws2
        EList      ws1 es ws2 rest ws3      -> EList      (mapWs ws1) es ws2 rest ws3
        EOp        ws1 op es ws2            -> EOp        (mapWs ws1) op es ws2
        EIf        ws1 e1 e2 e3 ws2         -> EIf        (mapWs ws1) e1 e2 e3 ws2
        ELet       ws1 kind rec p e1 e2 ws2 -> ELet       (mapWs ws1) kind rec p e1 e2 ws2
        ECase      ws1 e1 bs ws2            -> ECase      (mapWs ws1) e1 bs ws2
        ETypeCase  ws1 pat bs ws2           -> ETypeCase  (mapWs ws1) pat bs ws2
        EComment   ws s e1                  -> EComment   (mapWs ws) s e1
        EOption    ws1 s1 ws2 s2 e1         -> EOption    (mapWs ws1) s1 ws2 s2 e1
        ETyp       ws1 pat tipe e ws2       -> ETyp       (mapWs ws1) pat tipe e ws2
        EColonType ws1 e ws2 tipe ws3       -> EColonType (mapWs ws1) e ws2 tipe ws3
        ETypeAlias ws1 pat tipe e ws2       -> ETypeAlias (mapWs ws1) pat tipe e ws2
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
        PList  ws1 ps ws2 rest ws3 -> PList  (mapWs ws1) ps ws2 rest ws3
        PAs    ws1 ident ws2 p     -> PAs    (mapWs ws1) ident ws2 p
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


ensureWhitespaceNewline : String -> String
ensureWhitespaceNewline s =
  if String.contains "\n" s then s else "\n" ++ s

-- whitespaceTwoNewlines : String -> String
-- whitespaceTwoNewlines string =
--   "\n\n" ++ extractIndentation string

ensureWhitespaceNewlineExp : Exp -> Exp
ensureWhitespaceNewlineExp exp =
  mapPrecedingWhitespace ensureWhitespaceNewline exp

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
ensureWhitespaceSmartExp : String -> Exp -> Exp
ensureWhitespaceSmartExp indentationIfMultiline exp =
  if isLet exp || List.any (precedingWhitespace >> String.contains "\n") (flattenExpTree exp) then
    exp
    |> ensureWhitespaceNewlineExp
    |> replaceIndentation indentationIfMultiline
  else
    ensureWhitespaceExp exp


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
  let (firstWs, sepWs) =
    case oldExps of
      first::second::_ -> (precedingWhitespace first, precedingWhitespace second)
      first::[]        -> (precedingWhitespace first, if precedingWhitespace first == "" then " " else precedingWhitespace first)
      []               -> ("", " ")
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
tabsToSpaces ws = Regex.replace Regex.All (Regex.regex "\t") (\_ -> "    ") ws


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


-- Increases indentation by spaces string.
indent : String -> Exp -> Exp
indent spaces e =
  let processWS ws =
    ws |> String.reverse
       |> Regex.replace (Regex.AtMost 1) (Regex.regex "\n") (\_ -> spaces ++ "\n")
       |> String.reverse
  in
  mapExp (mapPrecedingWhitespace processWS) e

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
  = E Exp
  | P Exp Pat -- Patterns know their parent
  | T Type
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
    ET _ _ _ ->
      True
    PT _ _ _ _ ->
      True
    TT _ _ _ ->
      True

childCodeObjects : CodeObject -> List CodeObject
childCodeObjects co =
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
          [ E e1
          , ET After ws2 e1
          ]
        EApp ws1 e1 es ws2 ->
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
                Just lastHead ->
                  [ lastHead ]
                Nothing ->
                  []
          in
            [ ET Before ws1 e
            ] ++
            ( List.map E es
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
        EIf ws1 e1 e2 e3 ws2 ->
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
        ETypeCase ws1 p1 tbranches _ ->
          [ ET Before ws1 e
          , P e p1
          ] ++
          ( case List.head tbranches of
              Just tb ->
                case tb.val of
                  TBranch_ tbranchWS1 _ _ _ ->
                    [ PT After tbranchWS1 e p1 ]
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
        ELet ws1 lk _ p1 e1 e2 ws2 ->
          [ ET Before ws1 e
          , P e p1
          , E e1
          ] ++
          ( case lk of
              Let ->
                [ E e2
                , ET After ws2 e2
                ]
              Def ->
                [ ET After ws2 e1
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
    P e p ->
      case p.val.p__ of
        PVar ws1 _ _ ->
          [ PT Before ws1 e p ]
        PConst ws1 _ ->
          [ PT Before ws1 e p ]
        PBase ws1 _ ->
          [ PT Before ws1 e p ]
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
        PAs ws1 _ _ p1 ->
          [ PT Before ws1 e p
          , P e p1
          ]
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
        TNamed ws1 _ ->
          [ TT Before ws1 t ]
        TVar ws1 _ ->
          [ TT Before ws1 t ]
        TForall ws1 _ t1 ws2 ->
          [ TT Before ws1 t
          , T t1
          , TT After ws2 t1
          ]
        TWildcard ws1 ->
          [ TT Before ws1 t ]
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

-- Helper function for computePatMap
tagPatList
  :  ScopeId
  -> List Pat
  -> List (PId, PathedPatternId)
tagPatList scopeId pats =
  let
    manyMapper : PathedPatternId -> List Pat -> List (PId, PathedPatternId)
    manyMapper ppid =
      let
        path =
          Tuple.second ppid
      in
        List.concat <<
          List.indexedMap
            ( \index pat ->
                singleMapper
                  (scopeId, path ++ [index + 1])
                  pat
            )
    singleMapper : PathedPatternId -> Pat -> List (PId, PathedPatternId)
    singleMapper ppid p =
        (p.val.pid, ppid) ::
          case p.val.p__ of
            PConst _ _  ->
              []
            PBase _ _ ->
              []
            PVar _ _ _  ->
              []
            PAs _ _ _ p1 ->
              -- TODO Unsure if this is the right ppid (it is the same as the
              --      parent).
              singleMapper ppid p1
            PList _ ps _ Nothing _  ->
              manyMapper ppid ps
            PList _ ps _ (Just pTail) _ ->
              manyMapper ppid (ps ++ [pTail])
  in
    case pats of
      [ pat ] ->
        singleMapper (scopeId, []) pat
      _ ->
        manyMapper (scopeId, []) pats

-- Helper function for computePatMap
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
              tagPatList (eid, index + 1) [ p ]
      )

computePatMap : Exp -> Dict PId PathedPatternId
computePatMap =
  let
    taggedChildPats : Exp -> List (PId, PathedPatternId)
    taggedChildPats e =
      case e.val.e__ of
        EFun _ ps _ _ ->
          tagPatList (e.val.eid, 1) ps
        ECase _ _ branches _ ->
          tagBranchList e.val.eid branches
        ETypeCase _ p1 _ _ ->
          tagPatList (e.val.eid, 1) [ p1 ]
        ELet _ _ _ p1 _ _ _ ->
          tagPatList (e.val.eid, 1) [ p1 ]
        ETyp _ p1 _ _ _ ->
          tagPatList (e.val.eid, 1) [ p1 ]
        ETypeAlias _ p1 _ _ _ ->
          tagPatList (e.val.eid, 1) [ p1 ]
        _ ->
          []
  in
    Dict.fromList << List.concatMap taggedChildPats << flattenExpTree

--------------------------------------------------------------------------------
-- Dealing with top-level expressions
--------------------------------------------------------------------------------

-- A "nested expression" is a non-top-level expression
firstNestedExp : Exp -> Exp
firstNestedExp e =
  case e.val.e__ of
    ELet _ Def _ _ _ eRest _ ->
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
