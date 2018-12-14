module Palettes exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E

import Lang exposing (..)
import LeoParser
import LeoUnparser
import Model exposing (Msg)
import Controller

import Utils


--------------------------------------------------------------------------------

type alias DecodeEncode a =
  Exp -> Maybe (a, a -> Exp)

type alias PaletteDefinition model msg =
  { atType : Type
  , init : model
  , toExp : model -> Exp__
  , update : msg -> model -> model
  , view : model -> Html msg
  , decodeEncode : DecodeEncode model
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
        |> LeoParser.freshen -- b/c added new nodes
  in
  case palette.decodeEncode ePaletteModel of
    Nothing ->
      Html.text <|
        paletteName ++ ".decode failed: " ++ LeoUnparser.unparse ePaletteModel

    Just (model, encodeModel) ->
      let
        paletteHtml : Html msg
        paletteHtml =
          palette.view model

        msgToMsg : (msg -> Msg)
        msgToMsg msg =
          let
            newModel =
              Debug.log "newModel" <|
              palette.update msg model

            newProgram =
              rewriteProgram
                (encodeModel newModel |> unExpr |> .val |> .e__)
                (palette.toExp newModel)
          in
            Controller.msgSelectSynthesisResult newProgram
      in
        -- Html.h1 [] [ Html.text <| paletteName ++ " Palette" ]
        Html.map msgToMsg paletteHtml


displayPaletteElements : List (Html a) -> Html a
displayPaletteElements =
  Html.div
    [ Attr.style
        [ ("padding", "20px")
        , ("width", "max-content")
        ]
    ]


--------------------------------------------------------------------------------
-- Decoder/Re-encoders

decodeEncodeBool : DecodeEncode Bool
decodeEncodeBool exp =
  case (unExpr exp).val.e__ of
    EBase ws (EBool bool) ->
      Just (bool, \newBool -> EBase ws (EBool newBool) |> replaceE__ exp)
    _ ->
      Nothing

decodeEncodeNum : DecodeEncode Num
decodeEncodeNum exp =
  case (unExpr exp).val.e__ of
    EConst ws n loc wd ->
      Just (n, \newNum -> EConst ws newNum loc wd |> replaceE__ exp)
    _ ->
      Nothing

decodeEncodeInt : DecodeEncode Int
decodeEncodeInt exp =
  decodeEncodeNum exp |>
    -- TODO not checking that n is whole
    Maybe.map (\(n, encodeNum) -> (round n, toFloat >> encodeNum))

-- ignoring actual field names
decodeEncodeRecord : DecodeEncode (List Exp)
decodeEncodeRecord exp =
  case (unExpr exp).val.e__ of
    ERecord ws1 Nothing decls ws2 ->
      let
        (exps, encodeDecls) = declExtractors decls
      in
        Just
          ( exps
          , \newExps ->
              ERecord ws1 Nothing (encodeDecls newExps) ws2
                |> replaceE__ exp
          )
    _ ->
      Nothing

-- be careful, if new list is longer...
--
decodeEncodeList : DecodeEncode (List Exp)
decodeEncodeList exp =
  case (unExpr exp).val.e__ of
    EList ws1 wsExps ws2 Nothing ws3 ->
      let
        (wsList, expList) = List.unzip wsExps
      in
        Just
          ( expList
          , \newExpList ->
              EList ws1 (Utils.zip wsList newExpList) ws2 Nothing ws3
                |> replaceE__ exp
          )
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

    decodeEncode =
      decodeEncodeBool

    toExp newBool =
      EBase space1 (EBool newBool)

    update CheckboxClick oldBool =
      not oldBool

    view currentBool =
      displayPaletteElements
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
    , decodeEncode = decodeEncode
    , toExp = toExp
    , update = update
    , view = view
    }


--------------------------------------------------------------------------------
-- NumButtons

type alias NumButtonsModel =
  { min : Float, num : Float, max : Float }

type NumButtonsMsg = NumButtonsOffset Int -- (+1) or (-1)

numButtons : PaletteDefinition NumButtonsModel NumButtonsMsg
numButtons =
  let
    atType =
      withDummyTypeInfo (TNum space1)

    init =
      { min = 0, num = 5, max = 10 }

    toExp {num} =
      EConst space1 num dummyLoc noWidgetDecl

    update (NumButtonsOffset offset) model =
      { model | num = model.num + toFloat offset }

    decodeEncode exp =
      decodeEncodeRecord exp |> Maybe.andThen (\(exps, encodeRecord) ->
        case exps of
          [e1,e2,e3] ->
            decodeEncodeNum e1 |> Maybe.andThen (\(n1, encodeNum1) ->
            decodeEncodeNum e2 |> Maybe.andThen (\(n2, encodeNum2) ->
            decodeEncodeNum e3 |> Maybe.map (\(n3, encodeNum3) ->
              ( { min = n1, num = n2, max = n3 }
              , \{min, num, max} ->
                  encodeRecord [encodeNum1 min, encodeNum2 num, encodeNum3 max]
              )
            )))
          _ ->
            Nothing
      )

    view {min, num, max} =
      let
        incrementDecrementButton offset caption =
          let newNum = num + toFloat offset in
          Html.button
            [ Attr.disabled <| newNum < min || newNum > max
            , E.onClick <| NumButtonsOffset offset
            ]
            [ Html.text caption ]
      in
      displayPaletteElements
        [ incrementDecrementButton (-1) "-1"
        , Html.text <| toString num
        , incrementDecrementButton 1 "+1"
        ]
  in
    { atType = atType
    , init = init
    , toExp = toExp
    , update = update
    , view = view
    , decodeEncode = decodeEncode
    }


--------------------------------------------------------------------------------
-- Matrix

type alias MatrixModel =
  { numRows : Int
  , numCols : Int
  , data : List (List Float)
  }

type AddRemove = Add | Remove

type MatrixMsg
  = MatrixNoop
  | MatrixSet { row : Int, col : Int, newNum : Float }
  | MatrixCol AddRemove Int

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

        MatrixCol addRemove col ->
          let
            newNumCols =
              case addRemove of
                Add    -> oldModel.numCols + 1
                Remove -> oldModel.numCols - 1

            newData =
              if newNumCols == 1 then
                List.repeat oldModel.numRows [0]
              else
                oldModel.data
                  |> List.map (Utils.concatMapi1 (\(j, val) ->
                       case ( addRemove
                            , j == col
                            , col == 1 + oldModel.numCols && j == oldModel.numCols
                            ) of
                         (Add, True, _) ->
                           [val, val]
                         (Add, False, True) ->
                           [val, val]
                         (Remove, True, _) ->
                           []
                         _ ->
                           [val]
                     ))

            _ = Debug.log "newData" newData
          in
            { oldModel | numCols = newNumCols, data = newData }

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
                    [ ("width", "30pt")
                    , ("height", "18pt")
                    , ("font-size", "12pt")
                    , ("resize", "none")
                    , ("overflow-x", "scroll")
                    , ("overflow-y", "hidden")
                    , ("overflow-wrap", "unset")
                    ]
                , E.onInput <| \text ->
                    case String.toFloat (String.trim text) of
                      Ok n  -> MatrixSet { row = i, col = j, newNum = n }
                      Err _ -> MatrixNoop
                ]
                [ Html.text <| toString num
                ]
            )
            -- |> flip Utils.snoc (Html.button [] [Html.text "blah"])
          )
          -- |> List.map ((::) (Html.div [ Attr.style [ ("width", "7.5pt"), ("display", "inline-block") ] ] []))
          |> List.map (Html.div [])

        colButtons =
          let
            makeButton addRemove j =
              Html.button
                [ Attr.style
                    [ ("width", "15pt")
                    ]
                , Attr.class "hidden-until-hover"
                , E.onClick <| MatrixCol addRemove j
                ]
                [ Html.text <| if addRemove == Add then "+" else "-" ]
          in
            List.concat <|
              [ List.concatMap
                  (\j -> [ makeButton Add j, makeButton Remove j ])
                  (List.range 1 numCols)
              , [ makeButton Add (1 + numCols) ]
              ]
      in
      displayPaletteElements
        (colButtons ++ grid)

    decodeEncode exp =
      decodeEncodeRecord exp |> Maybe.andThen (\(exps, encodeRecord) ->
        case exps of
          [e1,e2,e3] ->
            -- ignoring encodeList functions, b/c lengths may differ...

            decodeEncodeInt  e1 |> Maybe.andThen (\(numRows, encodeNumRows) ->
            decodeEncodeInt  e2 |> Maybe.andThen (\(numCols, encodeNumCols) ->
            -- decodeEncodeList e3 |> Maybe.andThen (\(exps, encodeData) ->
            decodeEncodeList e3 |> Maybe.andThen (\(exps, _) ->
              exps
                |> List.map decodeEncodeList
                |> Utils.projJusts
                |> Maybe.andThen (\list ->
                     let
                       -- (listRows, listEncodeRows) =
                       (listRows, _) =
                         List.unzip list

                       encodeNewData : List (List Exp) -> Exp
                       encodeNewData blah =
                         eList (List.map (flip eList Nothing) blah) Nothing
                           -- Utils.zip blah listEncodeRows
                           --   |> List.map (\(x,f) -> f x)
                           --   |> encodeData
                     in
                       listRows
                         |> List.map (List.map decodeEncodeNum)
                         |> List.map Utils.projJusts
                         |> Utils.projJusts
                         |> Maybe.map (\dataAndEncoders ->
                              let
                                -- (data, datumEncoders) =
                                (data, _) =
                                  dataAndEncoders
                                    |> List.map List.unzip
                                    |> List.unzip

                                encodeNewRows : List (List Float) -> List (List Exp)
                                encodeNewRows =
                                  List.map (List.map eConstDummyLoc)
                                    -- Utils.zip data datumEncoders
                                    --   |> List.map (\(nums, encodeNums) ->
                                    --        Utils.zip nums encodeNums
                                    --          |> List.map (\(x,f) -> f x)
                                    --      )
                              in
                                ( { numRows = numRows
                                  , numCols = numCols
                                  , data = data
                                  }
                                , \{numRows, numCols, data} ->
                                    encodeRecord
                                      [ encodeNumRows numRows
                                      , encodeNumCols numCols
                                      , encodeNewRows data |> encodeNewData
                                      ]
                                )
                            )
                   )
            )))
          _ ->
            Nothing
      )
  in
    { atType = atType
    , init = init
    , toExp = toExp
    , update = update
    , view = view
    , decodeEncode = decodeEncode
    }
