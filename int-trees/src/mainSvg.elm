module MainSvg where

import Svg
import Svg.Attributes as A
import Html
import Graphics.Element as E exposing (Element)
import Text
import Color
import Debug

import Lang exposing (..)
import LangParser
import LangSvg exposing (valToSvg)
import MicroTests
import Sync
import Utils

------------------------------------------------------------------------------

valToElt w h v =
  E.color Color.lightGray <|
    let html = Svg.svg [] (valToSvg v) in
    -- let html = Svg.svg [ A.x "0", A.y "0", A.viewBox "0 0 323.141 322.95" ] (valToSvg v) in
    Html.toElement w h html

showMonoString s = E.leftAligned (Text.monospace (Text.fromString s))
showString s     = E.leftAligned (Text.fromString s)

expToElt : Exp -> Element
expToElt = showMonoString << Lang.sExp

showOne (w,h,test) =
  let {e,v,vnew} = test in
  let l1 = [ showString "Original Program", expToElt e
           , showString "Original Canvas",  valToElt w h v
           , showString "Updated Canvas",   valToElt w h vnew
           ] in
  let l2 =
    case Sync.sync e v vnew of
      Err e -> [[ E.show e ]]
      Ok results ->
        flip Utils.mapi results <| \(i,((ei,vi),vdiff)) ->
          [ showString <| "Option " ++ toString i ++ " "
                          ++ Utils.parens ("vdiff = " ++ toString vdiff)
          , expToElt ei
          , valToElt w h vi
          ]
  in
  let br = Html.toElement   1 20 (Html.br [] []) in
  let hr = Html.toElement 600 20 (Html.hr [] []) in
  E.flow E.right [
    E.spacer 10 10
  , E.flow E.down
      (List.intersperse (E.spacer 10 10) (l1 ++ List.concat l2) ++ [br,hr,br])
  ]

main : Element
main =
  let tests = [
      (600, 100, MicroTests.test15 ())
    , (600, 100, MicroTests.test16 ())
    , (600, 100, MicroTests.test17 ())
    , (600, 100, MicroTests.test18 ())
    , (600, 100, MicroTests.test19 ())
    , (600, 100, MicroTests.test20 ())
    , (600, 100, MicroTests.test21 ())
    , (600, 400, MicroTests.test22 ())
    , (600, 200, MicroTests.test23 ())
    , (600, 200, MicroTests.test24 ())
    , (600, 200, MicroTests.test25 ())
    , (600, 200, MicroTests.test26 ())
    , (600, 200, MicroTests.test27 ())
    , (600, 200, MicroTests.test28 ())
  ] in
  E.flow E.down (List.map showOne tests)

