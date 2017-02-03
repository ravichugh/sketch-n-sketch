module CodeMotion exposing (moveDefinitionPat, moveDefinitionBeforeLet)

import Lang exposing (..)
import LangUnparser exposing
  ( mapPrecedingWhitespace
  , precedingWhitespace, replacePrecedingWhitespace
  , precedingWhitespacePat, replacePrecedingWhitespacePat
  )
import DependenceGraph exposing
  ( ScopeGraph, ScopeId, PatternId
  , BeforeAfter, PatTargetPosition, ExpTargetPosition -- maybe move these here
  , ScopeOrder(..)
  , parentScopeOf, childScopesOf
  )
import InterfaceModel exposing (SynthesisResult)
import Utils

import Dict


------------------------------------------------------------------------------

{-
              -- ELet WS LetKind Rec Pat Exp Exp WS
type alias LetExp  = (WS,LetKind,Rec,Pat,Exp,Exp,WS)
type alias LetDecl = (WS,LetKind,Rec,Pat,Exp,EId,WS)

mapLetExp : (LetExp -> a) -> EId -> Exp -> a
mapLetExp f i exp =
  let maybeResult =
    foldExp (\e acc ->
      if e.val.eid /= i then acc
      else
        case e.val.e__ of
          ELet ws1 letKind rec p e1 e2 ws2 ->
            Just (f (ws1, letKind, rec, p, e1, e2, ws2))
          _ ->
            let _ = Debug.log "WARN: not an ELet:" i in
            Nothing
      ) Nothing exp
  in
  case maybeResult of
    Just result -> result
    Nothing     -> Debug.crash ("mapLetExp: " ++ toString i)

-- TODO: collect all LetDecls at once, perhaps put in Model

getLetDecl : EId -> Exp -> LetDecl
getLetDecl = mapLetExp <|
  \(ws1, letKind, rec, p, e1, e2,         ws2) ->
   (ws1, letKind, rec, p, e1, e2.val.eid, ws2)

getLetBody : EId -> Exp -> Exp
getLetBody = mapLetExp <|
  \(ws1, letKind, rec, p, e1, e2, ws2) -> e2
-}

letBodyOf : Exp -> Exp
letBodyOf e = case e.val.e__ of
  ELet ws1 letKind rec p e1 e2 ws2 -> e2
  _                                -> Debug.crash "letBody"


------------------------------------------------------------------------------

type alias VarEquation = (Ident, Exp)
type alias RebuildLetExp = Exp -> Exp

-- returns the binding (x, e1) to be removed from let-expression at scopeId,
-- and a function foo that rewrites this let-expression by wrapping the
--   residual pattern p_ and equation e1_ (that remain after plucking)
--   around a rewritten body e2_ (provided as an argument)
--
pluck : PatternId -> Exp -> Maybe (VarEquation, RebuildLetExp)
pluck patId =
  foldExp (\e acc -> Utils.plusMaybe (pluck_ patId e) acc) Nothing

pluck_ : PatternId -> Exp -> Maybe (VarEquation, RebuildLetExp)
pluck_ (scopeId, path) e =
  if e.val.eid /= scopeId then Nothing
  else case e.val.e__ of

    ELet ws1 letKind rec p e1 e2 ws2 ->
      case (p.val, e1.val.e__, path) of

        (PVar _ x _, _, []) ->
          Just ((x, e1), \e2_ -> e2_)

        ( PList pws1 ps pws2 Nothing pws3
        , EList ews1 es ews2 Nothing ews3
        , [i]
        ) ->
          case (List.length ps == List.length es, (Utils.geti i ps).val) of
            (False, _) ->
              Debug.log "pluck different lengths" Nothing

            (True, PVar _ xi _) ->
              let ps_ = Utils.removei i ps in
              let es_ = Utils.removei i es in
              let ei = Utils.geti i es in
              if List.length ps_ == 0 then
                Just ((xi, ei), \e2_ -> e2_)
              else
                let p_ = replaceP_  p (PList pws1 ps_ pws2 Nothing pws3) in
                let e1_ = replaceE__ e1 (EList ews1 es_ ews2 Nothing ews3) in
                let let_ e2_ = replaceE__ e (ELet ws1 letKind rec p_ e1_ e2_ ws2) in
                Just ((xi, ei), let_)

            (True, pi) ->
              let _ = Debug.log "trying to pluck" pi in
              Nothing

        _ ->
          let _ = Debug.log "pluck: bad pattern" (scopeId, path) in
          Nothing

    _ ->
      let _ = Debug.log "pluck: bad Exp__" e.val.e__ in
      Nothing


------------------------------------------------------------------------------

ensureWhitespace : String -> String
ensureWhitespace s = if s == "" then " " else s

ensureWhitespaceExp : Exp -> Exp
ensureWhitespaceExp exp = mapPrecedingWhitespace ensureWhitespace exp

ensureWhitespaceExps : List Exp -> List Exp
ensureWhitespaceExps exps =
  case exps of
    []         -> []
    hd :: rest -> ensureWhitespaceExp hd :: rest

ensureWhitespacePats : List Pat -> List Pat
ensureWhitespacePats pats =
  case pats of
    []         -> []
    hd :: rest -> replacePrecedingWhitespacePat
                    (ensureWhitespace (precedingWhitespacePat hd)) hd :: rest

strUnsafeBool b = if b then "[UNSAFE] " else ""


------------------------------------------------------------------------------

moveDefinitionBeforeLet : ScopeGraph -> PatternId -> EId -> Exp -> List SynthesisResult
moveDefinitionBeforeLet scopeGraph sourcePat targetScope exp =

  let sourceScope = Tuple.first sourcePat in
  let ((xPlucked, ePlucked), residualLetExp) =
    Utils.fromJust_ "moveDefUpBeforeLet" (pluck sourcePat exp) in

  let move upDown prefix =
    let caption =
      let
        x = DependenceGraph.lookupIdent sourcePat scopeGraph
        y = DependenceGraph.lookupIdent (targetScope, []) scopeGraph
      in
      prefix ++ Utils.spaces ["move", x, upDown, "before", y]
    in
    let newExp =
      flip mapExp exp <| \e ->   -- this used to be called insertLet
        if e.val.eid == sourceScope then
          let e2_ = letBodyOf e in
          residualLetExp e2_
        else if e.val.eid == targetScope then
          withDummyPos <|        -- this used to be called insertLet_
            ELet (precedingWhitespace e) (getLetKind e) False
              (pVar xPlucked) (ensureWhitespaceExp ePlucked)
              e ""
        else
          e
    in
    [ { description = caption, exp = newExp } ]
  in

  let shadowing =
    DependenceGraph.checkVisible scopeGraph xPlucked sourcePat targetScope in

  let moveUp () =
    let unsafe =
      strUnsafeBool <|
        DependenceGraph.patternTransitivelyDependsOnScope scopeGraph
          sourcePat targetScope
    in
    move "up" (shadowing ++ unsafe)
  in

  case DependenceGraph.scopeOrder scopeGraph sourceScope targetScope of

    SameScope ->
      []

    ChildScope ->
      moveUp ()

    NearestCommonAncestor commonScope ->
      let parentOfTargetScope = parentScopeOf targetScope scopeGraph in
      case DependenceGraph.scopeOrder scopeGraph commonScope parentOfTargetScope of
        SameScope  -> moveUp ()
        _          -> []
          -- ChildScope case is handled by the outer ChildScope case

    ParentScope ->
      let unsafe =
        strUnsafeBool <|
          DependenceGraph.usedOnTheWayDownTo scopeGraph
            sourcePat targetScope False
      in
      move "down" (shadowing ++ unsafe)

getLetKind : Exp -> LetKind
getLetKind e =
  case e.val.e__ of
    ELet _ lk _ _ _ _ _ -> lk
    _                   -> Debug.log "getLetKind..." Let


------------------------------------------------------------------------------

moveDefinitionPat : ScopeGraph -> PatternId -> PatTargetPosition -> Exp -> List SynthesisResult
moveDefinitionPat scopeGraph sourcePat targetPos exp =
  let sourceScope = Tuple.first sourcePat in
  let targetScope = Tuple.first (Tuple.second targetPos) in
  let pluckedStuff = Utils.fromJust_ "moveDefPat_" <|
    pluck sourcePat exp
  in

  let shadowing =
    -- TODO checkVisible for PLists
    let targetId = Tuple.first (Tuple.second targetPos) in
    DependenceGraph.checkVisible scopeGraph
       (Tuple.first (Tuple.first pluckedStuff)) sourcePat targetId
  in

  case DependenceGraph.scopeOrder scopeGraph sourceScope targetScope of

    ChildScope ->
      let unsafe =
        strUnsafeBool <|
          DependenceGraph.patternTransitivelyDependsOnScope scopeGraph
            sourcePat targetScope
      in
      moveDefinitionPat_ scopeGraph sourcePat targetPos exp
         pluckedStuff "up" (shadowing ++ unsafe)

    ParentScope ->
      let unsafe =
        strUnsafeBool <|
          DependenceGraph.usedOnTheWayDownTo scopeGraph
            sourcePat targetScope True
      in
      moveDefinitionPat_ scopeGraph sourcePat targetPos exp
         pluckedStuff "down" (shadowing ++ unsafe)

    SameScope ->
      let unsafe = strUnsafeBool False in
      moveDefinitionPat_ scopeGraph sourcePat targetPos exp
         pluckedStuff "over" (shadowing ++ unsafe)

    NearestCommonAncestor _ ->
      []

moveDefinitionPat_ scopeGraph sourcePat targetPos exp pluckedStuff upDownOver prefix =
  let caption =
    captionMoveDefinitionBeforeAfterPat scopeGraph sourcePat targetPos upDownOver prefix
  in
  let newExp =
    insertPat scopeGraph (Tuple.first sourcePat) pluckedStuff targetPos exp
  in
  [ { description = caption, exp = newExp } ]

insertPat : ScopeGraph -> ScopeId -> (VarEquation, RebuildLetExp) -> PatTargetPosition -> Exp -> Exp
insertPat scopeGraph sourceId (pluckedEquation, residualLetExp) patTargetPosition =
  let targetId = Tuple.first (Tuple.second patTargetPosition) in
  mapExp <| \e ->
    if e.val.eid == sourceId then
      let e2_ = letBodyOf e in
      if sourceId /= targetId
        then residualLetExp e2_
        else insertPat_ pluckedEquation patTargetPosition (residualLetExp e2_)
    else if e.val.eid == targetId then
      insertPat_ pluckedEquation patTargetPosition e
    else
      e

insertPat_ (xMove, eMove) (beforeAfter, (targetId, targetPath)) e =
  case e.val.e__ of
    ELet ws1 letKind rec p e1 e2 ws2 ->
      case (p.val, e1.val.e__, targetPath) of
        (PVar _ x _, _, []) ->
          let j = if beforeAfter == 0 then 0 else 1 in
          let ps = List.take j [p]
                     ++ [pVar xMove]
                     ++ List.drop j [p] in
          let es = List.take j [e1]
                     ++ [ensureWhitespaceExp eMove]
                     ++ List.drop j [e1] in
          let p_ = replaceP_ p (PList " " ps "" Nothing "") in
          let e1_ = replaceE__ e1 (EList " " es "" Nothing "") in
          replaceE__ e (ELet ws1 letKind rec p_ e1_ e2 ws2)

        ( PList pws1 ps pws2 Nothing pws3
        , EList ews1 es ews2 Nothing ews3
        , [i]
        ) ->
          let j = if beforeAfter == 0 then i - 1 else i in
          let ps_ =
            List.take j ps
              ++ [pVar xMove]
              ++ ensureWhitespacePats (List.drop j ps) in
          let es_ =
            List.take j es
              ++ [ensureWhitespaceExp eMove]
              ++ ensureWhitespaceExps (List.drop j es) in
          let p_  = replaceP_ p (PList pws1 ps_ pws2 Nothing pws3) in
          let e1_ = replaceE__ e1 (EList ews1 es_ ews2 Nothing ews3) in
          replaceE__ e (ELet ws1 letKind rec p_ e1_ e2 ws2)

        _ ->
          let _ = Debug.log "insertPat_: pattern" p.val in
          e

    _ ->
      let _ = Debug.log "insertPat_: not ELet" e.val.e__ in
      e

captionMoveDefinitionBeforeAfterPat scopeGraph sourcePat targetPos upDownOver prefix =
  let caption =
    let
      x = DependenceGraph.lookupIdent sourcePat scopeGraph
      y = DependenceGraph.lookupIdent (Tuple.second targetPos) scopeGraph
      beforeAfter = if Tuple.first targetPos == 0 then "before" else "after"
    in
    Utils.spaces ["move", x, upDownOver, beforeAfter, y]
  in
  prefix ++ caption
