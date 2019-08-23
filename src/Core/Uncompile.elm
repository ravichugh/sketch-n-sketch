module Core.Uncompile exposing
  ( exp
  , res
  )

import Core.Lang as C
import Lang as L
import UnLang as U
import Info exposing (withDummyInfo)

exp : C.Exp -> L.Exp
exp cexp =
  case cexp of
    EFix f x body ->
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

    EApp e1 e2 ->
      L.eApp (exp e1) (exp e2)

    EVar x ->
      L.eVar x

    ETuple es ->
      L.eTuple (List.map exp es)

    EProj n i arg ->
      L.fromTupleGet (n, i, arg)

    ECtor ctorName arg ->
      eDatatype ctorName [exp arg]

    ECase scrutinee branches ->
      let
        branch : (String, (String, Exp)) -> L.Branch
        branch (ctorName, (argName, body)) =
           withDummyInfo <|
             L.Branch_
               space0
               (L.pDatatype ctorName [L.pVar argName])
               (exp body)
               space1
      in
        L.eCase (exp scrutinee) (List.map branch branches)

    EHole _ ->
      L.eEmptyHole0

    EAssert e1 e2 ->
      let
        equality =
          L.eOp Eq [exp e1, exp e2]
      in
        L.eApp (L.eVar "assert") (L.parens equality)

res_ : C.Res -> U.UnExp ()
res_ r =
  case r of
    C.RFix en f x body ->
      U.UFunClosure (env en) f x body

    C.RTuple rs ->
      U.UTuple () (List.map res_ rs)

    C.RCtor ctorName arg ->
      U.UConstructor () ctorName (res_ arg)

    C.RHole en _ ->
      U.UHoleClosure () (env en) (-1, -1)

    C.RApp r1 r2 ->
      U.UApp () (res_ r1) (res_ r2)

    C.RProj n i arg ->
      U.UGet () n i (res_ arg)

    C.RCase en scrutinee branches ->
      let
        branch : (String, (String, L.Exp)) -> (String, String, L.Exp)
        branch (ctorName, (argName, body)) =
          (ctorName, argName, body)
      in
        U.UCase () (env en) (res_ scrutinee) (List.map branch branches)

res : C.Res -> U.UnExp ()
res =
  res_ >> TriEval.setHoleIndexes

env : C.Env -> U.Env
env =
  let
    binding : (String, R.Res) -> U.EnvBinding
    binding (x, r) =
      VarBinding x (r, Nothing)
  in
    List.map binding
