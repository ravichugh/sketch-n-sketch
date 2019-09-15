module Core.Uncompile exposing
  ( exp
  , res
  , holeFilling
  )

import Dict

import Core.Lang as C
import Lang as L
import UnLang as U
import Info exposing (withDummyInfo)
import TriEval

exp : C.Exp -> L.Exp
exp cexp =
  case cexp of
    C.EFix f x body ->
      let
        lambdaTerm =
          L.eFun [L.pVar x] (exp body)
      in
        case f of
          Just fName ->
            L.eLet
              [(fName, lambdaTerm)]
              (L.eVar fName)

          Nothing ->
            lambdaTerm

    C.EApp special e1 e2 ->
      let
        application =
          if special then
            L.eAppSpecial
          else
            L.eApp
      in
        application (exp e1) [exp e2]

    C.EVar x ->
      L.eVar x

    C.ETuple es ->
      L.eTuple (List.map exp es)

    C.EProj n i arg ->
      L.fromTupleGet (n, i, exp arg)

    C.ECtor ctorName arg ->
      L.eDatatype ctorName [exp arg]

    C.ECase scrutinee branches ->
      let
        branch : (String, (String, C.Exp)) -> L.Branch
        branch (ctorName, (argName, body)) =
           withDummyInfo <|
             L.Branch_
               L.space0
               (L.pDatatype ctorName [L.pVar argName])
               (exp body)
               L.space1
      in
        L.eCase (exp scrutinee) (List.map branch branches)

    C.EHole _ ->
      L.eEmptyHole0

    C.EAssert e1 e2 ->
      let
        equality =
          L.eOp L.Eq [exp e1, exp e2]
      in
        L.eApp (L.eVar "assert") [L.eParens equality]

res_ : C.Res -> U.UnExp ()
res_ r =
  case r of
    C.RFix en f x body ->
      U.UFunClosure () (env en) f x (exp body)

    C.RTuple rs ->
      U.UTuple () (List.map res_ rs)

    C.RCtor ctorName arg ->
      U.UConstructor () ctorName (res_ arg)

    C.RHole en holeName ->
      U.UHoleClosure () (env en) (holeName, -1)

    C.RApp r1 r2 ->
      U.UApp () (res_ r1) (res_ r2)

    C.RProj n i arg ->
      U.UGet () n i (res_ arg)

    C.RCase en scrutinee branches ->
      let
        branch : (String, (String, C.Exp)) -> (String, String, L.Exp)
        branch (ctorName, (argName, body)) =
          (ctorName, argName, exp body)
      in
        U.UCase () (env en) (res_ scrutinee) (List.map branch branches)

res : C.Res -> U.UnExp ()
res =
  res_ >> TriEval.setHoleIndexes

env : C.Env -> U.Env
env =
  let
    binding : (String, C.Res) -> U.EnvBinding
    binding (x, r) =
      U.VarBinding x (res_ r, Nothing)
  in
    List.map binding

holeFilling : C.HoleFilling -> U.HoleFilling
holeFilling =
  Dict.fromList >> Dict.map (\_ -> exp)
