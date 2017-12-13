module Update exposing  (..)

import Lang exposing (..)

val_to_exp: WS -> Val -> Exp
val_to_exp ws v =
  case v.v_ of
    VConst mb num -> EConst ws (0, unann, "") num (WithInfo NoWidgetDecl)

    VBase (VBool b) -> EBase ws <! EBool b

    VBase (VString s) -> EBase ws <! EString s

    VBase (VNull) -> EBase ws <! ENull

    VClosure mbid pattern body env -> EFun ws (List pattern) body -- Not sure about this one.

    VList vals -> EList ws (List.map vals val_to_exp) (WithInfo "") Nothing (WithInfo "")

    --VDict vs ->

update : Env -> Exp -> Val -> Val -> (Exp, Env)
update env e oldVal newVal =
  case e.val.e of
    EConst ws num loc widget -> val_to_exp ws newVal

    EBase ws m -> val_to_exp ws newVal

    