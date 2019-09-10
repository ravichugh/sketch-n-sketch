module Core.Lang exposing (..)

type alias HoleName =
  Int

type Exp
  = EFix (Maybe String) String Exp
  | EApp Bool Exp Exp
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

type BindSpec
  = NoSpec
  | Rec String
  | Arg String
  | Dec String

type alias TypeBinding =
  (String, (Typ, BindSpec))

type alias TypeContext =
  List TypeBinding

type alias DatatypeContext =
  List (String, List (String, Typ))

-- (hole name, (type context, type, function decrease requirement, match depth))
type alias HoleContext =
  List (HoleName, (TypeContext, Typ, Maybe String, Int))

type Value
  = VTuple (List Value)
  | VCtor String Value

type alias HoleFilling =
  List (HoleName, Exp)

type alias ResumptionAssertion =
  (Res, Value)

type alias ResumptionAssertions =
  List ResumptionAssertion

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

nullaryCtor : String -> Exp
nullaryCtor name =
  ECtor name (ETuple [])

valueToRes : Value -> Res
valueToRes v =
  case v of
    VTuple components ->
      RTuple (List.map valueToRes components)

    VCtor ctorName arg ->
      RCtor ctorName (valueToRes arg)
