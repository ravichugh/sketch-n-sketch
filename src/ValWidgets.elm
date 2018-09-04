module ValWidgets exposing (..)

import Lang exposing (..)
import Types

-- Right now, just used for overlapping widget subsumption in Eval.eval
widgetToMaybeVal : Widget -> Maybe Val
widgetToMaybeVal widget =
  case widget of
    WIntSlider low high caption current val loc hidden                              -> Just val
    WNumSlider low high caption current val loc hidden                              -> Just val
    WPoint xNumTr xVal yNumTr yVal pairVal                                          -> Just pairVal
    WOffset1D baseXNumTr baseYNumTr axis sign amountNumTr amountVal endXVal endYVal -> Just amountVal
    WCall callEId funcVal argVals returnVal returnWidgets                           -> Nothing
    WList val                                                                       -> Just val


isSameWidgetType : Widget -> Widget -> Bool
isSameWidgetType w1 w2 =
  case (w1, w2) of
    (WIntSlider _ _ _ _ _ _ _ , WIntSlider _ _ _ _ _ _ _)  -> True
    (WNumSlider _ _ _ _ _ _ _ , WNumSlider _ _ _ _ _ _ _)  -> True
    (WPoint _ _ _ _ _         , WPoint _ _ _ _ _)          -> True
    (WOffset1D _ _ _ _ _ _ _ _, WOffset1D _ _ _ _ _ _ _ _) -> True
    (WCall _ _ _ _ _          , WCall _ _ _ _ _)           -> True
    (WList _                  , WList _)                   -> True
    _                                                      -> False


valToMaybeWidget : Val -> Maybe Widget
valToMaybeWidget val =
  case val.v_ of
    VList vs ->
      case (vs, List.map .v_ vs) of
        ([v1, v2], [VConst _ nt1, VConst _ nt2]) -> Just (WPoint nt1 v1 nt2 v2 val)
        (_::_, _)                                ->
          case Types.valToMaybeType val |> Maybe.map (.val >> .t__) of
            Just (TList _ _ _) -> Just (WList val) -- Elements of list are of a homogeneous type.
            _                  -> Nothing          -- Elements of list are heterogenous. This excludes our pseudo-ADTs for SVG from generating widgets.
        _                                        -> Nothing

    _ -> Nothing
