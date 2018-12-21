module ValWidgets exposing (..)

import Lang exposing (..)
import Provenance
import Types
import Utils
-- import LangUnparser -- debug only

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


-- So we don't produce a multitude of sliders on the same loc if the loc was inside
-- a function called multiple times.
--
-- Need our own comparison function because there are vals in here (meaning potentially glacial comparisons).
widgetsSameForPostprocessingRemoval : Widget -> Widget -> Bool
widgetsSameForPostprocessingRemoval widget1 widget2 =
  -- Int/Num sliders are compared only on the loc (we don't want to produce multiple sliders for the same loc)
  -- Everything else compared on everything.
  case (widget1, widget2) of
    ( WIntSlider low1 high1 caption1 current1 val1 loc1 hidden1                              , WIntSlider low2 high2 caption2 current2 val2 loc2 hidden2                               ) -> loc1 == loc2
    ( WNumSlider low1 high1 caption1 current1 val1 loc1 hidden1                              , WNumSlider low2 high2 caption2 current2 val2 loc2 hidden2                               ) -> loc1 == loc2
    ( WPoint xNumTr1 xVal1 yNumTr1 yVal1 pairVal1                                            , WPoint xNumTr2 xVal2 yNumTr2 yVal2 pairVal2                                             ) -> xNumTr1 == xNumTr2 && Provenance.valEqFast xVal1 xVal2 && yNumTr1 == yNumTr2 && Provenance.valEqFast yVal1 yVal2 && Provenance.valEqFast pairVal1 pairVal2 -- Shouldn't happen
    ( WOffset1D baseXNumTr1 baseYNumTr1 axis1 sign1 amountNumTr1 amountVal1 endXVal1 endYVal1, WOffset1D baseXNumTr2 baseYNumTr2 axis2 sign2 amountNumTr2 amountVal2 endXVal2 endYVal2 ) -> baseXNumTr1 == baseXNumTr2 && baseYNumTr1 == baseYNumTr2 && axis1 == axis2 && sign1 == sign2 && amountNumTr1 == amountNumTr2 && Provenance.valEqFast amountVal1 amountVal2 && Provenance.valEqFast endXVal1 endXVal2 && Provenance.valEqFast endYVal1 endYVal2 -- Shouldn't happen
    ( WCall callEId1 funcVal1 argVals1 returnVal1 returnWidgets1                             , WCall callEId2 funcVal2 argVals2 returnVal2 returnWidgets2                              ) -> callEId1 == callEId2 && Provenance.valEqFast funcVal1 funcVal2 && (List.map2 Provenance.valEqFast argVals1 argVals2 |> Utils.allTrue) && Provenance.valEqFast returnVal1 returnVal2 && (List.map2 widgetsSameForPostprocessingRemoval returnWidgets1 returnWidgets2 |> Utils.allTrue) -- Shouldn't happen.
    ( WList val1                                                                             , WList val2                                                                              ) -> Provenance.valEqFast val1 val2 -- Shouldn't happen.
    _                                                                                                                                                                                    -> False



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
            maybeType          ->
              -- let _ = maybeType |> Maybe.map (Types.withDummyRangeAndNoRoles >> LangUnparser.unparseType False >> Debug.log "valToMaybeWidget fail") in
              Nothing -- Elements of list are heterogenous. This excludes our pseudo-ADTs for SVG from generating widgets.
        _                                        -> Nothing

    _ -> Nothing
