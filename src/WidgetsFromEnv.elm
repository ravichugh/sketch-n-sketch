module WidgetsFromEnv exposing (..)

import Lang exposing (..)
import ValWidgets


widgetsFromEnv : Env -> List Widget
widgetsFromEnv env =
  env
  |> List.filterMap (\(ident, val) -> ValWidgets.valToMaybeWidget val)