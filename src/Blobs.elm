module Blobs where

import Lang exposing (..)
import LangUnparser exposing (addPrecedingWhitespace)
import Utils


--------------------------------------------------------------------------------
-- Simple Program and Blob Types

type alias Program = (TopDefs, MainExp)

-- TODO store Idents and "types" in TopDefs. also use for lambda tool.

type alias TopDef  = (WS, Pat, Exp, WS)
type alias TopDefs = List TopDef

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

splitExp : Exp -> Program
splitExp e =
  case e.val.e__ of
    ELet ws1 Def False p1 e1 e2 ws2 ->
      let (defs, main) = splitExp e2 in
      ((ws1,p1,e1,ws2)::defs, main)
    _ ->
      ([], toMainExp e)

fuseExp : Program -> Exp
fuseExp (defs, mainExp) =
  let recurse defs =
    case defs of
      [] -> fromMainExp mainExp
      (ws1,p1,e1,ws2)::defs' ->
        withDummyPos <| ELet ws1 Def False p1 e1 (recurse defs') ws2
  in
  recurse defs

toMainExp : Exp -> MainExp
toMainExp e =
  maybeSvgConcat e `Utils.plusMaybe` maybeBlobs e `Utils.elseMaybe` OtherExp e

fromMainExp : MainExp -> Exp
fromMainExp me =
  case me of
    SvgConcat shapes f -> f shapes
    Blobs shapes f     -> f shapes
    OtherExp e         -> e

maybeSvgConcat : Exp -> Maybe MainExp
maybeSvgConcat main =
  case main.val.e__ of
    EApp ws1 e1 [eAppConcat] ws2 ->
      case (e1.val.e__, eAppConcat.val.e__) of
        (EVar _ "svg", EApp ws3 eConcat [e2] ws4) ->
          case (eConcat.val.e__, e2.val.e__) of
            (EVar _ "concat", EList ws5 oldList ws6 Nothing ws7) ->
              let updateExpressionList newList =
                let
                  e2'         = replaceE__ e2 <| EList ws5 newList ws6 Nothing ws7
                  eAppConcat' = replaceE__ eAppConcat <| EApp ws3 eConcat [e2'] ws4
                  main'       = replaceE__ main <| EApp ws1 e1 [eAppConcat'] ws2
                in
                if ws1 == "" then addPrecedingWhitespace "\n\n" main'
                else if ws1 == "\n" then addPrecedingWhitespace "\n" main'
                else main'
              in
              Just (SvgConcat oldList updateExpressionList)

            _ -> Nothing
        _     -> Nothing
    _         -> Nothing

-- very similar to above
maybeBlobs : Exp -> Maybe MainExp
maybeBlobs main =
  case main.val.e__ of
    EApp ws1 eBlobs [eArgs] ws2 ->
      case (eBlobs.val.e__, eArgs.val.e__) of
        (EVar _ "blobs", EList ws5 oldList ws6 Nothing ws7) ->
          let rebuildExp newBlobExpList =
            let newExpList = List.map fromBlobExp newBlobExpList in
            let
              eArgs' = replaceE__ eArgs <| EList ws5 newExpList ws6 Nothing ws7
              main'  = replaceE__ main <| EApp ws1 eBlobs [eArgs'] ws2
            in
            if ws1 == "" then addPrecedingWhitespace "\n\n" main'
            else if ws1 == "\n" then addPrecedingWhitespace "\n" main'
            else main'
          in
          let blobs = List.map toBlobExp oldList in
          Just (Blobs blobs rebuildExp)

        _     -> Nothing
    _         -> Nothing

toBlobExp : Exp -> BlobExp
toBlobExp e =
  case e.val.e__ of
    EVar _ x -> varBlob e x
    EApp _ eWith [eWithArg, eFunc] _ ->
      case (eWith.val.e__) of
        EVar _ with ->
          case eFunc.val.e__ of
            EVar _ x ->
              case with of
                "withBounds" -> NiceBlob e (WithBoundsBlob (eWithArg, x, []))
                "withAnchor" -> NiceBlob e (WithAnchorBlob (eWithArg, x, []))
                _            -> OtherBlob e
            EApp _ eF eArgs _ ->
              case eF.val.e__ of
                EVar _ f ->
                  case with of
                    "withBounds" -> NiceBlob e (WithBoundsBlob (eWithArg, f, eArgs))
                    "withAnchor" -> NiceBlob e (WithAnchorBlob (eWithArg, f, eArgs))
                    _            -> OtherBlob e
                _        -> OtherBlob e
            _ -> OtherBlob e
        _ -> OtherBlob e
    EApp _ eFunc eArgs _ ->
      case eFunc.val.e__ of
        EVar _ f -> NiceBlob e (CallBlob (f, eArgs))
        _        -> OtherBlob e
    _ -> OtherBlob e

fromBlobExp : BlobExp -> Exp
fromBlobExp be =
  case be of
    OtherBlob e  -> e
    NiceBlob e _ -> e
