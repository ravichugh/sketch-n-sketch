module CodeMotion exposing (moveDefinitionPat, moveDefinitionAboveLet)

import Lang exposing (..)
import LangUnparser exposing
  ( mapPrecedingWhitespace
  , precedingWhitespace, replacePrecedingWhitespace
  , precedingWhitespacePat, replacePrecedingWhitespacePat
  )
import DependenceGraph exposing
  ( ScopeGraph, ScopeId, PatternId
  , BeforeAfter, PatTargetPosition, ExpTargetPosition -- maybe move these here
  )
import Utils


------------------------------------------------------------------------------

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

getLetKind : Exp -> LetKind
getLetKind e =
  case e.val.e__ of
    ELet _ lk _ _ _ _ _ -> lk
    _                   -> Debug.log "getLetKind..." Let

insertLet : ScopeId -> (Ident, Exp, Exp) -> ExpTargetPosition -> Exp -> Exp
insertLet sourceId (xMove, eMove, newSourceIdExp) (beforeAfter, targetId) =
  mapExp <| \e ->
    if e.val.eid == sourceId then
      newSourceIdExp
    else if e.val.eid == targetId && beforeAfter == 0 then
      let ws1 = precedingWhitespace e in
      let letKind = getLetKind e in
      withDummyPos (ELet ws1 letKind False (pVar xMove) eMove e "")
    else
      e

insertPat : ScopeId -> (Ident, Exp, Exp) -> PatTargetPosition -> Exp -> Exp
insertPat sourceId (xMove, eMove, newSourceIdExp) patTargetPosition =
  mapExp <| \e ->
    if e.val.eid == sourceId then
      newSourceIdExp
    else if e.val.eid == Tuple.first (Tuple.second patTargetPosition) then
      insertPat_ xMove eMove patTargetPosition e
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

moveDefinitionAboveLet : PatternId -> EId -> Exp -> Maybe Exp
moveDefinitionAboveLet sourcePat targetId exp =
  case sourcePat of
    (sourceId, []) ->
      Just <| flip mapExp exp <| \e ->
        if e.val.eid == sourceId then
          getLetBody sourceId exp
        else if e.val.eid == targetId then
          let (_,_,rec,px,ex,_,ws2) = getLetDecl sourceId exp in
          let ws1_ = precedingWhitespace e in
          let letKind_ = getLetKind e in
          replaceE__ e (ELet ws1_ letKind_ rec px ex e ws2)
        else
          e

    (sourceId, sourcePath) ->
      case pluck sourcePat exp of
        Nothing -> Debug.crash ("couldn't pluck " ++ toString sourcePat)
        Just stuff ->
          Just <| insertLet sourceId stuff (0, targetId) exp


moveDefinitionPat : PatternId -> PatTargetPosition -> Exp -> Maybe Exp
moveDefinitionPat sourcePat targetPosition exp =

  if Tuple.first sourcePat == Tuple.first (Tuple.second targetPosition) then
    let _ = Debug.log "TODO scope ids are the same" (sourcePat, targetPosition) in
    Nothing

  else
    case pluck sourcePat exp of
      Nothing -> Debug.crash ("couldn't pluck " ++ toString sourcePat)
      Just stuff ->
        Just (insertPat (Tuple.first sourcePat) stuff targetPosition exp)
