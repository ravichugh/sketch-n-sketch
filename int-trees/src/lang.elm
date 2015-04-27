module Lang where

import String
import Debug
import Dict

import Utils

------------------------------------------------------------------------------

type alias Loc = Int
type alias Ident = String

type Pat
  = PVar Ident
  | PList (List Pat) (Maybe Pat)

type Op
  = Plus | Minus | Mult
  | Lt

type Exp
  = EConst Int Loc
  | EBase BaseVal
  | EVar Ident
  | EFun (List Pat) Exp
  | EApp Exp (List Exp)
  | EOp Op (List Exp)
  | EList (List Exp) (Maybe Exp)
  | EIf Exp Exp Exp
  | ECase Exp (List (Pat, Exp))
  | ELet Bool Pat Exp Exp

    -- EFun [] e     impossible
    -- EFun [p] e    (\p. e)
    -- EFun ps e     (\(p1 ... pn) e) === (\p1 (\p2 (... (\pn e) ...)))
    
    -- EApp f []     impossible
    -- EApp f [x]    (f x)
    -- EApp f xs     (f x1 ... xn) === ((... ((f x1) x2) ...) xn)

type Val
  = VConst Int Trace
  | VBase BaseVal
  | VClosure (Maybe Ident) Pat Exp Env
  | VList (List Val)
  | VHole

type BaseVal -- unlike Ints, these cannot be changed by Sync
  = Bool Bool
  | String String
  | Star -- placeholder used by sync

type Trace = TrLoc Loc | TrOp Op (List Trace)

type alias Env = List (Ident, Val)


------------------------------------------------------------------------------
-- Unparsing

strBaseVal v = case v of
  Bool True  -> "true"
  Bool False -> "false"
  String s   -> "\'" ++ s ++ "\'"
  Star       -> "X"

strVal     = strVal_ False
strValLocs = strVal_ True

strVal_ showTraces v =
  let foo = strVal_ showTraces in
  case v of
    VConst i tr      -> toString i
                          ++ if | showTraces -> Utils.braces (strTrace tr)
                                | otherwise  -> ""
    VBase b          -> strBaseVal b
    VClosure _ _ _ _ -> "<fun>"
    VList vs         -> Utils.bracks (String.join " " (List.map foo vs))
    VHole            -> "??"

strOp op = case op of {Plus -> "+"; Minus -> "-"; Mult -> "*"; Lt -> "<"}

strLoc l = "k" ++ toString l

strTrace tr = case tr of
  TrLoc l   -> strLoc l
  TrOp op l ->
    Utils.parens (String.concat
      [strOp op, " ", String.join " " (List.map strTrace l)])

strPat p = case p of
  PVar x     -> x
  PList ps m -> let s = Utils.spaces (List.map strPat ps) in
                case m of
                  Nothing   -> Utils.bracks s
                  Just rest -> Utils.bracks (s ++ " | " ++ strPat rest)

tab k = String.repeat k "  "

sExpK k     = (++) (tab k) << sExp_ False k
sExpLocsK k = (++) (tab k) << sExp_ True k
sExp        = sExpK 0
sExpLocs    = sExpLocsK 0

sExp_ showLocs k e =
  let foo = sExp_ showLocs in
  case e of
    EBase v        -> strBaseVal v
    EConst i l     -> toString i
                        ++ if | showLocs  -> Utils.braces (strLoc l)
                              | otherwise -> ""
    EVar x         -> x
    EFun [p] e     -> Utils.parens <|
                        "\\" ++ strPat p ++ "\n" ++ tab (k+1) ++ foo (k+1) e
    EFun ps e      -> Utils.parens <|
                        "\\" ++ Utils.parens (Utils.spaces (List.map strPat ps)) ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e
    EApp e1 [e2]   -> Utils.parens <| foo k e1 ++ " " ++ foo k e2
    EApp e1 es     -> Utils.parens <|
                        foo k e1 ++ " " ++ Utils.spaces (List.map (foo k) es)
    EOp op [e1,e2] -> Utils.parens <|
                        String.join " " [strOp op, foo k e1, foo k e2]
    EList es mrest -> Utils.bracks <|
                        let s = String.join " " (List.map (foo k) es) in
                        case mrest of
                          Nothing -> s
                          Just e  -> s ++ " | " ++ foo k e
    EIf e1 e2 e3   -> Utils.parens <|
                        "if " ++ foo k e1 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e2 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e3
    ELet b p e1 e2 -> Utils.parens <|
                        (if b then "letrec " else "let ") ++ strPat p ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e1 ++ "\n" ++
                          tab (k+1) ++ foo (k+1) e2
    ECase e1 l ->
      let bar (pi,ei) =
        tab (k+1) ++ Utils.parens (strPat pi ++ " " ++ foo (k+1) ei) in
      Utils.parens <|
        "case " ++ foo k e1 ++ "\n" ++ Utils.lines (List.map bar l)


------------------------------------------------------------------------------
-- Mapping

mapExp : (Exp -> Exp) -> Exp -> Exp
mapExp f e =
  let foo = mapExp f in
  case e of
    EConst _ _     -> f e
    EBase _        -> f e
    EVar _         -> f e
    EFun ps e'     -> f (EFun ps (foo e'))
    EApp e1 es     -> f (EApp (foo e1) (List.map foo es))
    EOp op es      -> f (EOp op (List.map foo es))
    EList es m     -> f (EList (List.map foo es) (Utils.mapMaybe foo m))
    EIf e1 e2 e3   -> f (EIf (foo e1) (foo e2) (foo e3))
    ECase e1 l     -> f (ECase (foo e1) (List.map (\(p,ei) -> (p, foo ei)) l))
    ELet b p e1 e2 -> f (ELet b p (foo e1) (foo e2))

mapVal : (Val -> Val) -> Val -> Val
mapVal f v = case v of
  VList vs         -> f (VList (List.map (mapVal f) vs))
  VConst _ _       -> f v
  VBase _          -> f v
  VClosure _ _ _ _ -> f v
  VHole            -> f v


------------------------------------------------------------------------------
-- Substitutions

type alias Subst = Dict.Dict Loc Int

applySubst : Subst -> Exp -> Exp
applySubst subst e = case e of
  EConst _ l -> case Dict.get l subst of Just i -> EConst i l
  EBase _    -> e
  EVar _     -> e
  EFun _ _   -> e   -- not recursing into lambdas
  EOp op es  -> EOp op (List.map (applySubst subst) es)
  EList es m -> EList (List.map (applySubst subst) es)
                      (Utils.mapMaybe (applySubst subst) m)
  EApp f es  -> EApp (applySubst subst f) (List.map (applySubst subst) es)
  ELet b p e1 e2 ->
    ELet b p (applySubst subst e1) (applySubst subst e2) -- TODO
  EIf e1 e2 e3 ->
    EIf (applySubst subst e1) (applySubst subst e2) (applySubst subst e3)


------------------------------------------------------------------------------
-- Value Contexts

type alias VContext = Val
  -- invariant: a VContext is a Val with exactly one VHole

fillHoleWith : VContext -> Val -> Val
fillHoleWith vc w = case vc of
  VHole            -> w
  VConst _ _       -> vc
  VBase _          -> vc
  VClosure _ _ _ _ -> vc   -- not recursing into closures
  VList vs         -> VList (List.map (flip fillHoleWith w) vs)

type VDiff = Same Val | Diff VContext Val Val

diff : Val -> Val -> Maybe VDiff
diff v1 v2 = 
  let res = diff_ v1 v2 in
  case res of
    Just (Diff vc w1 w2) ->
      let (v1',v2') = (fillHoleWith vc w1, fillHoleWith vc w2) in
      if | eqV (v1,v1') && eqV (v2,v2') -> res
         | otherwise ->
             Debug.crash (String.join "\n"
               ["bad diff", strVal vc, strVal w1, strVal w2])
    _ -> res

eqV (v1,v2) = case (v1, v2) of            -- equality modulo traces
  (VConst i tr, VConst j _) -> i == j
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> False
      Just l  -> List.all eqV l
  _ -> v1 == v2
  
-- assuming that v1 is the value resulting from eval (so it has proper locs)
-- and that v2 has dummy locs

diff_ v1 v2 = case (v1, v2) of
  (VBase Star, VConst _ _) -> Just (Same v2)
  (VConst i tr, VConst j _) ->
    if | i == j    -> Just (Same (VConst i tr))  -- cf. comment above
       | otherwise -> Just (Diff VHole v1 (VConst j tr))
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> Nothing
      Just l ->
        List.foldr (\(vi1,vi2) acc ->
          case acc of
            Nothing -> Nothing
            Just (Same (VList vs)) ->
              case diff_ vi1 vi2 of
                Nothing              -> Nothing
                Just (Same v)        -> justSameVList (v::vs)
                Just (Diff vc w1 w2) -> Just (Diff (VList (vc::vs)) w1 w2)
            Just (Diff (VList vs) w1 w2) ->
              case diff_ vi1 vi2 of
                Nothing              -> Nothing
                Just (Same v)        -> Just (Diff (VList (v::vs)) w1 w2)
                Just (Diff _ _ _)    -> Nothing
            Just (Diff _ _ _) ->
              Debug.crash "diff_: error?"
        ) (justSameVList []) l
  _ ->
    if | v1 == v2  -> Just (Same v1)
       | otherwise -> Nothing
           
justSameVList = Just << Same << VList
      

------------------------------------------------------------------------------
-- Abstract Syntax Helpers

dummyLoc = 0
dummyTrace = TrLoc dummyLoc
eConst = flip EConst dummyLoc
vConst = flip VConst dummyTrace
ePlus e1 e2 = EOp Plus [e1,e2]

eBool  = EBase << Bool
eTrue  = eBool True
eFalse = eBool False

vBool  = VBase << Bool
vTrue  = vBool True
vFalse = vBool False

eApp e es = case es of
  [e1]    -> EApp e [e1]
  e1::es' -> eApp (EApp e [e1]) es'

eFun ps e = case ps of
  [p]     -> EFun [p] e
  p::ps'  -> EFun [p] (eFun ps' e)

