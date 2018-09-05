module Blobs exposing (..)

import Lang exposing (..)
import Utils


--------------------------------------------------------------------------------
-- Simple Program and Blob Types

type alias SplitProgram = (TopDefs, MainExp)

type alias SimpleSplitProgram = (TopDefs, List BlobExp, List BlobExp -> Exp)

-- TODO store Idents and "types" in TopDefs. also use for lambda tool.

type alias TopDef  = (WS, Pat, Exp, WS)
type alias TopDefs = (List TopDef, Maybe Declarations)

type MainExp
  = SvgConcat (List Exp) (List Exp -> Exp)
  | Blobs (List BlobExp) (List BlobExp -> Exp)
  | OtherExp Exp

type BlobExp
  = OtherBlob Exp
  | NiceBlob Exp NiceBlob

type NiceBlob
  = VarBlob Ident                          -- x
  | CallBlob (Ident, List Exp)             -- (f args)
  | WithBoundsBlob (Exp, Ident, List Exp)  -- (withBounds bounds (f args))
  | WithAnchorBlob (Exp, Ident, List Exp)  -- (withAnchor anchor (f args))

varBlob e x            = NiceBlob e (VarBlob x)
callBlob e tuple       = NiceBlob e (CallBlob tuple)
withBoundsBlob e tuple = NiceBlob e (WithBoundsBlob tuple)
withAnchorBlob e tuple = NiceBlob e (WithAnchorBlob tuple)

maybeSimpleProgram : Exp -> Maybe SimpleSplitProgram
maybeSimpleProgram e =
  let (defs, mainExp) = splitExp e in
  case mainExp of
    SvgConcat _ _ -> Nothing
    OtherExp _    -> Nothing
    Blobs blobs f -> Just (defs, blobs, f)

splitExp : Exp -> SplitProgram
splitExp e =
  case unwrapExp e of
    ELet ws1 Def (Declarations printOrder _ _ exps as decls) ws main ->
      ((List.map (\(LetExp _ ws1 p1 _ ws2 e1) -> (ws1, p1, e1, ws2)) <| elemsOf exps, Just decls), toMainExp main)
    _ ->
      (([], Nothing), toMainExp e)

fuseExp : SplitProgram -> Exp
fuseExp ((defs, mbDecls), mainExp) =
  case mbDecls of
    Nothing -> fromMainExp mainExp
    Just (Declarations po tp ann oldExps) ->
      let newExps = List.map2 (\(newWs1, newP1, newE1, newS2) (LetExp sp1 ws1 p1 funStyle ws2 e1) ->
        LetExp sp1 newWs1 newP1 funStyle newS2 newE1) defs (elemsOf oldExps) in
      withDummyExpInfo <| ELet space0 Def (Declarations po tp ann (newExps |> regroup oldExps)) space1 (fromMainExp mainExp)

toMainExp : Exp -> MainExp
toMainExp e =
  Utils.elseMaybe
    (Utils.plusMaybe (maybeSvgConcat e) (maybeBlobs e))
    (OtherExp e)

fromMainExp : MainExp -> Exp
fromMainExp me =
  case me of
    SvgConcat shapes f -> f shapes
    Blobs shapes f     -> f shapes
    OtherExp e         -> e

maybeSvgConcat : Exp -> Maybe MainExp
maybeSvgConcat main =
  case unwrapExp main of
    EApp ws1 e1 [eAppConcat] appType ws2 ->
      case ((unwrapExp e1), (unwrapExp eAppConcat)) of
        (EVar _ "svg", EApp ws3 eConcat [e2] appType2 ws4) ->
          case ((unwrapExp eConcat), (unwrapExp e2)) of
            (EVar _ "concat", EList ws5 oldList ws6 Nothing ws7) ->
              let updateExpressionList newList =
                let
                  e2New         = replaceE__ e2 <| EList ws5 (Utils.listValuesMake oldList newList) ws6 Nothing ws7
                  eAppConcatNew = replaceE__ eAppConcat <| EApp ws3 eConcat [e2New] appType2 ws4
                  mainNew       = replaceE__ main <| EApp ws1 e1 [eAppConcatNew] appType ws2
                in
                if ws1.val == "" then addPrecedingWhitespace "\n\n" mainNew
                else if ws1.val == "\n" then addPrecedingWhitespace "\n" mainNew
                else mainNew
              in
              Just (SvgConcat (Utils.listValues oldList) updateExpressionList)

            _ -> Nothing
        _     -> Nothing
    _         -> Nothing

-- very similar to above
maybeBlobs : Exp -> Maybe MainExp
maybeBlobs main =
  case (unwrapExp main) of
    EApp ws1 eBlobs [eArgs] appType ws2 ->
      case ((unwrapExp eBlobs), (unwrapExp eArgs)) of
        (EVar _ "blobs", EList ws5 oldList ws6 Nothing ws7) ->
          let rebuildExp newBlobExpList =
            let newExpList = List.map fromBlobExp newBlobExpList in
            let
              eArgsNew = replaceE__ eArgs <| EList ws5 (Utils.listValuesMake oldList newExpList) ws6 Nothing ws7
              mainNew  = replaceE__ main <| EApp ws1 eBlobs [eArgsNew] appType ws2
            in
            if ws1.val == "" then addPrecedingWhitespace "\n\n" mainNew
            else if ws1.val == "\n" then addPrecedingWhitespace "\n" mainNew
            else mainNew
          in
          let blobs = List.map toBlobExp (Utils.listValues oldList) in
          Just (Blobs blobs rebuildExp)

        _     -> Nothing
    _         -> Nothing

toBlobExp : Exp -> BlobExp
toBlobExp e =
  case (unwrapExp e) of
    EVar _ x -> varBlob e x
    EApp _ eWith [eWithArg, eFunc] appType _ ->
      case ((unwrapExp eWith)) of
        EVar _ with ->
          case (unwrapExp eFunc) of
            EVar _ x ->
              case with of
                "withBounds" -> NiceBlob e (WithBoundsBlob (eWithArg, x, []))
                "withAnchor" -> NiceBlob e (WithAnchorBlob (eWithArg, x, []))
                _            -> OtherBlob e
            EApp _ eF eArgs appType2 _ ->
              case (unwrapExp eF) of
                EVar _ f ->
                  case with of
                    "withBounds" -> NiceBlob e (WithBoundsBlob (eWithArg, f, eArgs))
                    "withAnchor" -> NiceBlob e (WithAnchorBlob (eWithArg, f, eArgs))
                    _            -> OtherBlob e
                _        -> OtherBlob e
            _ -> OtherBlob e
        _ -> OtherBlob e
    EApp _ eFunc eArgs appType _ ->
      case (unwrapExp eFunc) of
        EVar _ f -> NiceBlob e (CallBlob (f, eArgs))
        _        -> OtherBlob e
    _ -> OtherBlob e

fromBlobExp : BlobExp -> Exp
fromBlobExp be =
  case be of
    OtherBlob e  -> e
    NiceBlob e _ -> e
