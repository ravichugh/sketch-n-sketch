module Palettes exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E

import Lang exposing (..)
import LeoUnparser exposing (unparse)
import Model exposing (Msg)
import Controller

import Utils


--------------------------------------------------------------------------------

type alias PaletteDefinition model msg =
  { atType : Type
  , init : model
  , decode : Exp -> Maybe model
  , encode : model -> Exp__
  , toExp : model -> Exp__
  , update : msg -> model -> model
  , view : PaletteExpInfo -> model -> Html msg
  }


--------------------------------------------------------------------------------
-- "Framework" for Embedding Palette GUIs

view : String -> PaletteExpInfo -> Html Msg
view paletteName =
  case paletteName of
    "Checkbox" -> embedPalette paletteName checkbox
    _          -> always (Html.text ("palette not defined: " ++ paletteName))


embedPalette : String -> PaletteDefinition model msg -> PaletteExpInfo -> Html Msg
embedPalette paletteName palette paletteExpInfo =
  let
    { program, paletteExp, paletteExpEApp } =
      paletteExpInfo

    (ws1, eFunc, (ePaletteName, ePaletteModel, eExpansion), appType, ws2) =
      paletteExpEApp

    rewriteProgram : Exp__ -> Exp__ -> Exp
    rewriteProgram newModel__ newExpansion__ =
      let
        newPaletteExp__ =
          EApp ws1
               eFunc
               [ ePaletteName
               , replaceE__PreservingPrecedingWhitespace ePaletteModel newModel__ 
               , replaceE__PreservingPrecedingWhitespace eExpansion newExpansion__
               ]
               appType
               ws2
      in
        replaceExpNode
          (unExpr paletteExp).val.eid
          (replaceE__ paletteExp newPaletteExp__)
          program
  in
  case palette.decode ePaletteModel of
    Nothing ->
      Html.text <| paletteName ++ ".decode failed: " ++ unparse ePaletteModel

    Just model ->
      let
        paletteHtml : Html msg
        paletteHtml =
          palette.view paletteExpInfo model

        msgToMsg : (msg -> Msg)
        msgToMsg msg =
          let
            newModel =
              palette.update msg model

            newProgram =
              rewriteProgram
                (palette.encode newModel)
                (palette.toExp newModel)
          in
            Controller.msgSelectSynthesisResult newProgram
      in
        -- Html.h1 [] [ Html.text <| paletteName ++ " Palette" ]
        Html.map msgToMsg paletteHtml


--------------------------------------------------------------------------------
-- Checkbox

type alias CheckboxModel = Bool

type CheckboxMsg = CheckboxClick Bool

checkbox : PaletteDefinition CheckboxModel CheckboxMsg
checkbox =
  let
    atType =
      withDummyTypeInfo (TBool space1)

    init =
      False

    decode exp =
      if String.trim (unparse exp) == "True" then
        Just True
      else if String.trim (unparse exp) == "False" then
        Just False
      else
        Nothing

    encode newBool =
      EBase space1 (EBool newBool)

    toExp newBool =
      encode newBool

    update (CheckboxClick newBool) _ =
      newBool

    view stuff currentBool =
      Html.div
        [ Attr.style [ ("padding", "20px") ]
        ]
        [ Html.input
            [ Attr.type_ "checkbox"
            , Attr.checked currentBool
            , Attr.style [ ("transform", "scale(3)") ]
            , E.onCheck (\newBool -> CheckboxClick newBool)
            ]
            []
        ]
  in
    { atType = atType
    , init = init
    , decode = decode
    , encode = encode
    , toExp = toExp
    , update = update
    , view = view
    }
