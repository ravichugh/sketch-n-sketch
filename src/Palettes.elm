module Palettes exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E

import Lang exposing (..)
import LeoParser exposing (parse)
import LeoUnparser exposing (unparse)
import Model exposing (Msg)
import Controller

import Utils


--------------------------------------------------------------------------------

type alias PaletteDefinition model msg =
  { atType : Type
  , init : model
  , decode : Exp -> Maybe model
      -- if it were really important to have custom formatting for
      -- serialized models to be preserved, could have decode also return
      -- a function for re-encoding with a new model. and scrap encode.
  , encode : model -> Exp__
  , toExp : model -> Exp__
  , update : msg -> model -> model
  , view : model -> Html msg
  }


--------------------------------------------------------------------------------
-- "Framework" for Embedding Palette GUIs

view : String -> PaletteExpInfo -> Exp -> Html Msg
view paletteName =
  case paletteName of
    "Checkbox" ->
      embedPalette paletteName checkbox
    "NumButtons" ->
      embedPalette paletteName numButtons
    "Matrix" ->
      embedPalette paletteName matrix
    _ ->
      always (always (Html.text ("palette not defined: " ++ paletteName)))


embedPalette : String -> PaletteDefinition model msg -> PaletteExpInfo -> Exp -> Html Msg
embedPalette paletteName palette paletteExpInfo program =
  let
    { paletteExp, paletteExpEApp } =
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
          palette.view model

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
-- Do these exist somewhere?

decodeNum exp =
  case (unExpr exp).val.e__ of
    EConst _ n _ _ -> Just n
    _              -> Nothing

decodeInt exp =
  decodeNum exp
    |> Maybe.map round
       -- TODO could check that num is actually whole

decodeRecord exp =
  case (unExpr exp).val.e__ of
    ERecord _ Nothing decls _ ->
      decls
        |> recordEntriesFromDeclarations
        |> Maybe.map (List.map (\(_,_,f,_,e) -> (f,e)))
    _ ->
      Nothing

decodeList exp =
  case (unExpr exp).val.e__ of
    EList _ wsExps _ Nothing _ ->
      Just (List.map Tuple.second wsExps)
    _ ->
      Nothing


--------------------------------------------------------------------------------
-- Checkbox

type alias CheckboxModel = Bool

type CheckboxMsg = CheckboxClick

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

    update CheckboxClick oldBool =
      not oldBool

    view currentBool =
      Html.div
        [ Attr.style [ ("padding", "20px") ]
        ]
        [ Html.input
            [ Attr.type_ "checkbox"
            , Attr.checked currentBool
            , Attr.style [ ("transform", "scale(3)") ]
            , E.onCheck (always CheckboxClick)
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


--------------------------------------------------------------------------------
-- NumButtons

type alias NumButtonsModel = (Float, Float, Float)

type NumButtonsMsg = NumButtonsOffset Int -- (+1) or (-1)

numButtons : PaletteDefinition NumButtonsModel NumButtonsMsg
numButtons =
  let
    atType =
      withDummyTypeInfo (TNum space1)

    init =
      (0, 5, 10)

    encode (min, num, max) =
      eTuple (listOfNums [min, num, max]) |> unExpr |> .val |> .e__

    toExp (_, num, _) =
      EConst space1 num dummyLoc noWidgetDecl

    update (NumButtonsOffset offset) (min, num, max) =
      (min, num + toFloat offset, max)

    decode exp =
      let
        strings =
          unparse exp
            |> String.trim
            |> String.map
                 (\c -> if c == ',' || c == '(' || c == ')'
                          then ' '
                          else c)
            |> String.words
      in
        case strings of
          [s1, s2, s3] ->
            String.toFloat s1 |> Result.andThen (\n1 ->
            String.toFloat s2 |> Result.andThen (\n2 ->
            String.toFloat s3 |> Result.andThen (\n3 ->
              Ok (n1, n2, n3)
            )))

            |> Result.toMaybe

          _ ->
            Nothing

    view (min, num, max) =
      Html.div
        [ Attr.style [ ("padding", "20px") ]
        ]
        [ Html.button
            [ Attr.disabled (num - 1 < min)
            , E.onClick (NumButtonsOffset (-1))
            ]
            [ Html.text <| "-1 to " ++ toString (num - 1) ]
        , Html.text (toString num)
        , Html.button
            [ Attr.disabled (num + 1 > max)
            , E.onClick (NumButtonsOffset 1)
            ]
            [ Html.text <| "+1 to " ++ toString (num + 1) ]
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


--------------------------------------------------------------------------------
-- Matrix

type alias MatrixModel =
  { numRows : Int
  , numCols : Int
  , data : List (List Float)
  }

type MatrixMsg
  = MatrixNoop
  | MatrixSet { row : Int, col : Int, newNum : Float }

matrix : PaletteDefinition MatrixModel MatrixMsg
matrix =
  let
    atType =
      withDummyTypeInfo (TVar space1 "blaaaaaaaah")

    init =
      { numRows = 2
      , numCols = 3
      , data = [[0,0,0],[0,0,0]]
      }

    toExp {data} =
      data
        |> List.map (\nums -> eList (listOfNums nums) Nothing)
        |> flip eList Nothing
        |> unExpr |> .val |> .e__

    update msg oldModel =
      case msg of
        MatrixNoop ->
          oldModel

        MatrixSet {row, col, newNum} ->
          let
            newRow =
              oldModel.data
                |> Utils.geti row
                |> Utils.replacei col newNum

            newData =
              oldModel.data
                |> Utils.replacei row newRow
          in
            { oldModel | data = newData }

    view {numRows, numCols, data} =
      let
        grid =
          List.range 1 numRows |> List.map (\i ->
            List.range 1 numCols |> List.map (\j ->
              let
                num = data |> Utils.geti i |> Utils.geti j
              in
              Html.textarea
                [ Attr.style
                    [ ("width", "20pt")
                    ]
                , E.onInput <| \text ->
                    case String.toFloat (String.trim text) of
                      Ok n  -> MatrixSet {row=i, col=j, newNum=n}
                      Err _ -> MatrixNoop
                ]
                [ Html.text <| toString num
                ]
            )
          )
          |> List.map (Html.div [])

      in
      Html.div
        [ Attr.style [ ("padding", "20px") ]
        ]
        grid

    decode exp =
      decodeRecord exp |> Maybe.andThen (\fieldExps ->
        case fieldExps of
          [("numRows", e1), ("numCols", e2), ("data", e3)] ->
            decodeInt  e1 |> Maybe.andThen (\numRows ->
            decodeInt  e2 |> Maybe.andThen (\numCols ->
            decodeList e3 |> Maybe.andThen (\exps ->
              exps
                |> List.map decodeList
                |> Utils.projJusts
                |> Maybe.andThen (\lists ->
                     lists
                       |> List.map (List.map decodeNum)
                       |> List.map Utils.projJusts
                       |> Utils.projJusts
                       |> Maybe.andThen (\data ->
                            Just { numRows = numRows
                                 , numCols = numCols
                                 , data = data
                                 }
            )))))

          _ -> Nothing
      )

    encode {numRows, numCols, data} =
      let
        strRow row =
          List.map toString row
            |> String.join ","
            |> Utils.bracks

        strData =
          data
            |> List.map strRow
            |> String.join ","
            |> Utils.bracks

        str =
          "{ numRows = " ++ toString numRows ++
          ", numCols = " ++ toString numCols ++
          ", data = " ++ strData ++
          " }"
      in
        str
          |> parse
          |> Result.withDefault (eStr "Bad encode. Bad matrix palette. Bad")
          |> unExpr |> .val |> .e__
  in
    { atType = atType
    , init = init
    , toExp = toExp
    , update = update
    , view = view
    , decode = decode
    , encode = encode
    }
