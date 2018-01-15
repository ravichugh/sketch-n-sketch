module Update exposing  (..)
import Tuple exposing (first)
import Lang exposing (..)

val_to_exp: WS -> Val -> Exp__
val_to_exp ws v =
  case v.v_ of
    VConst mb num     -> EConst ws (first num) (0, unann, "") (withDummyInfo NoWidgetDecl)
    VBase (VBool b)   -> EBase  ws <| EBool b
    VBase (VString s) -> EBase  ws <| EString defaultQuoteChar s
    VBase (VNull)     -> EBase  ws <| ENull
    --VClosure Nothing pattern body env -> EFun ws (List pattern) body -- Not sure about this one.
    VList vals -> EList ws (List.map (val_to_exp (withDummyInfo "") >> withDummyExpInfo) vals) (withDummyInfo "") Nothing <| withDummyInfo ""
    _ -> Debug.crash <| "Trying to get an exp of the value " ++ toString v
    --VDict vs ->

-- Make sure that Env |- Exp evaluates to oldVal
update : Env -> Exp__ -> Val -> Val -> Result String (Env, Exp__)
update env e oldVal newVal =
  case e of
    EConst ws num loc widget -> Ok <| (env, val_to_exp ws newVal)
    EBase ws m -> Ok <| (env, val_to_exp ws newVal)
    EVar ws is ->
      (case env of
        []            -> Err <| "No " ++ is ++ " found. \nVariables in scope: " ++ (String.join " " <| List.map Tuple.first env)
        (k0,v0) :: l_ -> if is == k0
                           then Ok ((is, newVal) :: l_, e)
                           else
                             Result.map (Tuple.mapFirst (\newEnv -> (k0, v0) :: newEnv)) <| update l_ e oldVal newVal)
    EFun ws0 [p] e ws1 ->
      -- oldVal ==  VClosure Nothing p e env
      (case newVal.v_ of
        VClosure Nothing newP newE newEnv -> Ok (newEnv, EFun ws0 [newP] newE ws1)
        _ -> Err <| "Expected non-recursive closure, got " ++ toString newVal
      )
    EFun ws0 ps e ws1 ->
      update env (desugarEFun ps e).val.e__ oldVal newVal
      |> Result.map (\(newEnv, newExp) ->
        case newExp of
          EFun _ [p] newBody _ -> (newEnv, EFun ws0 ps newBody ws1)
          _ -> Debug.crash <| "Failed to recover a Fun, got " ++ toString newExp
        )
    _ -> Debug.crash <| "Non-supported update " ++ toString (env, e, oldVal, newVal)