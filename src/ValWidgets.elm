module ValWidgets exposing (..)

import Lang exposing (..)

-- Just produces point widgets for now.
valToMaybeWidget : Val -> Maybe Widget
valToMaybeWidget val =
  case val.v_ of
    VList [v1, v2] ->
      case (v1.v_, v2.v_) of
        (VConst _ nt1, VConst _ nt2) -> Just (WPoint nt1 v1 nt2 v2 val)
        _                            -> Nothing

    _ -> Nothing
