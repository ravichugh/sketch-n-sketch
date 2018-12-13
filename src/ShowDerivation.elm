module ShowDerivation exposing (build)

import Html exposing (Html, table, tr, td, text)
import Html.Attributes as Attr
import Html.Events as E
import Set
import String

import ElmUnparser
import InterfaceModel exposing (Model, Msg)
import Lang exposing (..)
import LangTools
import SleekLayout
import Utils
import ValUnparser


build : SleekLayout.BoundingBox -> Model -> List (Html Msg)
build dimensions model =
  [ drawDerivation model.inputVal ]

drawDerivation : Val -> Html Msg
drawDerivation conclusionVal =
  let
    premiseValues = provenancePremiseVals conclusionVal.provenance
    env           = provenanceEnv conclusionVal.provenance
    exp           = provenanceExp conclusionVal.provenance
    freeIdents    = LangTools.freeIdentifiers exp
    filteredEnv   = env |> List.filter (\(ident,_) -> Set.member ident freeIdents) |> Utils.removeShadowedKeys
    expStr        = ElmUnparser.unparse exp |> Utils.squish
    valStr        = ValUnparser.strVal conclusionVal
    envStr        = "[" ++ String.join ", " (List.map (\(ident, val) -> ident ++ "↦" ++ ValUnparser.strVal val) filteredEnv) ++ "]"
  in
  table
      [Attr.class "derivation"]
      [ tr [] [td [] (List.map drawDerivation premiseValues)]
      , tr [] [td [] [text <| envStr ++ " ⊢ " ++ expStr ++ " ⇓ " ++ valStr]]
      ]
