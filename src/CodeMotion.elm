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


------------------------------------------------------------------------------

-- return value (x, e1, e2') includes
--   the binding (x, e1) to be removed from expression scopeId and
--   the expression e2' that scopeId should be replaced with
--
pluck : PatternId -> Exp -> Maybe (Ident, Exp, Exp)
pluck patId =
  foldExp (\e acc -> Utils.plusMaybe (pluck_ patId e) acc) Nothing

pluck_ : PatternId -> Exp -> Maybe (Ident, Exp, Exp)
pluck_ (scopeId, path) e =
  if e.val.eid /= scopeId then Nothing
  else case e.val.e__ of

    ELet ws1 letKind rec p e1 e2 ws2 ->
      case (p.val, e1.val.e__, path) of

        (PVar _ x _, _, []) ->
          Just (x, e1, e2)

        ( PList pws1 ps pws2 Nothing pws3
        , EList ews1 es ews2 Nothing ews3
        , [i]
        ) ->
          case (Utils.geti i ps).val of
            PVar _ xi _ ->
              let ps_ = Utils.removei i ps in
              let es_ = Utils.removei i es in
              let ei = Utils.geti i es in
              if List.length ps_ == 0 then
                Just (xi, ei, e2)
              else
                let p_ = replaceP_  p (PList pws1 ps_ pws2 Nothing pws3) in
                let e1_ = replaceE__ e1 (EList ews1 es_ ews2 Nothing ews3) in
                let let_ = replaceE__ e  (ELet ws1 letKind rec p_ e1_ e2 ws2) in
                Just (xi, ei, let_)

            pi ->
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


------------------------------------------------------------------------------

moveDefinitionBeforeLet : ScopeGraph -> PatternId -> EId -> Exp -> List SynthesisResult
moveDefinitionBeforeLet scopeGraph sourcePat targetScope exp =

  let moveUp () =
    let unsafe =
       DependenceGraph.patternTransitivelyDependsOnScope scopeGraph
         sourcePat targetScope
    in
    moveDefinitionBeforeLet_ scopeGraph sourcePat targetScope exp
       "up" insertUpBeforeLet unsafe
  in

  let sourceScope = Tuple.first sourcePat in
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
        DependenceGraph.usedOnTheWayDownTo scopeGraph
           sourcePat targetScope False
      in
      moveDefinitionBeforeLet_ scopeGraph sourcePat targetScope exp
        "down" insertDownBeforeLet unsafe

moveDefinitionBeforeLet_ scopeGraph sourcePat targetScope exp upDown insertBeforeLet unsafe =
  let sourceScope = Tuple.first sourcePat in
  let pluckedStuff = Utils.fromJust_ "moveDefUpBeforeLet" <|
    pluck sourcePat exp
  in
  let caption =
    captionMoveDefinitionBeforeLet scopeGraph sourcePat targetScope upDown unsafe
  in
  let newExp =
    insertBeforeLet scopeGraph (Tuple.first sourcePat) pluckedStuff targetScope exp
  in
  [ { description = caption, exp = newExp } ]

insertUpBeforeLet : ScopeGraph -> ScopeId -> (Ident, Exp, Exp) -> ScopeId -> Exp -> Exp
insertUpBeforeLet scopeGraph sourceId (xMove, eMove, newSourceIdExp) targetId =
  mapExp <| \e ->
    if e.val.eid == sourceId then newSourceIdExp
    else if e.val.eid == targetId then rewriteTargetId_ xMove eMove e
    else e

insertDownBeforeLet : ScopeGraph -> ScopeId -> (Ident, Exp, Exp) -> ScopeId -> Exp -> Exp
insertDownBeforeLet scopeGraph sourceId (xMove, eMove, newSourceIdExp) targetId =
  mapExp <| \e ->
    if e.val.eid == sourceId then
      flip mapExp newSourceIdExp <| \ee ->
        if ee.val.eid == targetId then rewriteTargetId_ xMove eMove ee
        else ee
    else
      e

rewriteTargetId_ xMove eMove eee =
  let ws1 = precedingWhitespace eee in
  let letKind = getLetKind eee in
  withDummyPos <|
    ELet ws1 letKind False (pVar xMove) (ensureWhitespaceExp eMove) eee ""

getLetKind : Exp -> LetKind
getLetKind e =
  case e.val.e__ of
    ELet _ lk _ _ _ _ _ -> lk
    _                   -> Debug.log "getLetKind..." Let

captionMoveDefinitionBeforeLet scopeGraph sourcePat targetScope upDown unsafe =
  let prefix = if unsafe then "UNSAFE: " else "" in
  let caption =
    let
      x = DependenceGraph.lookupIdent sourcePat scopeGraph
      y = DependenceGraph.lookupIdent (targetScope, []) scopeGraph
    in
    Utils.spaces ["move", x, upDown, "before", y]
  in
  prefix ++ caption


------------------------------------------------------------------------------

moveDefinitionPat : ScopeGraph -> PatternId -> PatTargetPosition -> Exp -> List SynthesisResult
moveDefinitionPat scopeGraph sourcePat targetPosition exp =
  let sourceScope = Tuple.first sourcePat in
  let targetScope = Tuple.first (Tuple.second targetPosition) in

  case DependenceGraph.scopeOrder scopeGraph sourceScope targetScope of

    ChildScope ->
      let unsafe =
        DependenceGraph.patternTransitivelyDependsOnScope scopeGraph
           sourcePat targetScope
      in
      moveDefinitionPat_ scopeGraph sourcePat targetPosition exp
        "up" insertUpPat unsafe

    ParentScope ->
      let unsafe =
        DependenceGraph.usedOnTheWayDownTo scopeGraph
           sourcePat targetScope True
      in
      moveDefinitionPat_ scopeGraph sourcePat targetPosition exp
        "down" insertDownPat unsafe

    SameScope ->
      let unsafe = False in
      moveDefinitionPat_ scopeGraph sourcePat targetPosition exp
        "over" insertDownPat unsafe

    NearestCommonAncestor _ ->
      []

moveDefinitionPat_ scopeGraph sourcePat targetPos exp upDownOver insertPat unsafe =
  let pluckedStuff = Utils.fromJust_ "moveDefPat_" <|
    pluck sourcePat exp
  in
  let caption =
    captionMoveDefinitionBeforeAfterPat scopeGraph sourcePat targetPos upDownOver unsafe
  in
  let newExp =
    insertPat scopeGraph (Tuple.first sourcePat) pluckedStuff targetPos exp
  in
  [ { description = caption, exp = newExp } ]

insertUpPat : ScopeGraph -> ScopeId -> (Ident, Exp, Exp) -> PatTargetPosition -> Exp -> Exp
insertUpPat scopeGraph sourceId (xMove, eMove, newSourceIdExp) patTargetPosition =
  let targetId = Tuple.first (Tuple.second patTargetPosition) in
  mapExp <| \e ->
    if e.val.eid == sourceId then
      newSourceIdExp
    else if e.val.eid == targetId then
      insertPat_ xMove eMove patTargetPosition e
    else
      e

insertDownPat : ScopeGraph -> ScopeId -> (Ident, Exp, Exp) -> PatTargetPosition -> Exp -> Exp
insertDownPat scopeGraph sourceId (xMove, eMove, newSourceIdExp) patTargetPosition =
  let targetId = Tuple.first (Tuple.second patTargetPosition) in
  mapExp <| \e ->
    if e.val.eid == sourceId then
      flip mapExp newSourceIdExp <| \ee ->
        if ee.val.eid == targetId then
          insertPat_ xMove eMove patTargetPosition ee
        else
          ee
    else
      e

insertPat_ xMove eMove (beforeAfter, (targetId, targetPath)) e =
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

captionMoveDefinitionBeforeAfterPat scopeGraph sourcePat targetPos upDownOver unsafe =
  let prefix = if unsafe then "UNSAFE: " else "" in
  let caption =
    let
      x = DependenceGraph.lookupIdent sourcePat scopeGraph
      y = DependenceGraph.lookupIdent (Tuple.second targetPos) scopeGraph
      beforeAfter = if Tuple.first targetPos == 0 then "before" else "after"
    in
    Utils.spaces ["move", x, upDownOver, beforeAfter, y]
  in
  prefix ++ caption
