module Core.Lang exposing (..)

type alias HoleName =
  Int

type Exp
  = EFix (Maybe String) String Exp
  | EApp Exp Exp
  | EVar String
  | ETuple (List Exp)
  | EProj Int Int Exp -- (n, i, arg)
  | ECtor String Exp
  | ECase Exp (List (String, (String, Exp)))
  | EHole HoleName
  | EAssert Exp Exp

type Typ
  = TArr Typ Typ
  | TTuple (List Typ)
  | TData String

type Res
  = RFix Env (Maybe String) String Exp
  | RTuple (List Res)
  | RCtor String Res
  | RHole Env HoleName
  | RApp Res Res
  | RProj Int Int Res
  | RCase Env Res (List (String, (String, Exp)))

type alias Env =
  List (String, Res)

type alias DatatypeContext =
  List (String, List (String, Typ))

nullaryCtor : String -> Exp
nullaryCtor name =
  ECtor name (ETuple [])
