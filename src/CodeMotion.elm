module CodeMotion exposing (moveDefinition)

import Lang exposing (..)
import LangUnparser exposing (replacePrecedingWhitespace)
import DependenceGraph exposing
  ( ScopeGraph, ScopeId, PatternId
  , BeforeAfter(..), TargetPosition -- maybe move these here
  )


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

moveDefinition : PatternId -> TargetPosition -> Exp -> Exp
moveDefinition sourceId targetPosition exp =
  case (sourceId, targetPosition) of

    ((xScopeId, []), (Before, (yScopeId, []))) ->
      flip mapExp exp <| \e ->
        if e.val.eid == xScopeId then
          getLetBody xScopeId exp
        else if e.val.eid == yScopeId then
          let (ws1,letKind,rec,px,ex,_,ws2) = getLetDecl xScopeId exp in
          replaceE__ e (ELet ws1 letKind rec px ex e ws2)
        else
          e

    _ ->
      exp
