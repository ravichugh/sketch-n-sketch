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
import LangSvg
import MicroTests
import Sync
import Utils

------------------------------------------------------------------------------

valToElt w h v =
  E.color Color.lightGray <|
    Html.toElement w h (LangSvg.valToHtml w h v)

showMonoString s = E.leftAligned (Text.monospace (Text.fromString s))
showString s     = E.leftAligned (Text.fromString s)

expToElt : Exp -> Element
expToElt = showMonoString << Lang.sExp
expLocsToElt = showMonoString << Lang.sExpLocs

showOne (w,h,test) =
  let {e,v,vnew} = test in
  let l1 = [ showString "Original Program", expToElt e --, expLocsToElt e
           , showString "Original Canvas",  valToElt w h v ] in
  let l =
    case vnew of
      VList [] -> l1
      _ ->
        let l2 = [ showString "Updated Canvas", valToElt w h vnew ] in
        let l3 =
          case Sync.sync e v vnew of
            Err e -> [[ E.show e ]]
            Ok results ->
              flip Utils.mapi results <| \(i,((ei,vi),vdiff)) ->
                [ showString <| "Option " ++ toString i ++ " "
                                ++ Utils.parens ("vdiff = " ++ toString vdiff)
                , expToElt ei
                , valToElt w h vi
                ]
        in l1 ++ l2 ++ List.concat l3
  in
  let l = l ++ [showMonoString <| Sync.printZoneTable v] in
  let l = l ++ [showMonoString <| LangSvg.printIndexedTree v] in
  let br = Html.toElement   1 20 (Html.br [] []) in
  let hr = Html.toElement 600 20 (Html.hr [] []) in
  E.flow E.right [
    E.spacer 10 10
  , E.flow E.down
      (List.intersperse (E.spacer 10 10) (l ++ [br,hr,br]))
  ]

tests =
  [
      (MicroTests.test15 ())
    , (MicroTests.test16 ())
    , (MicroTests.test17 ())
    , (MicroTests.test18 ())
    , (MicroTests.test19 ())
    , (MicroTests.test20 ())
    , (MicroTests.test21 ())
    , (MicroTests.test22 ())
    , (MicroTests.test23 ())
    , (MicroTests.test24 ())
    , (MicroTests.test25 ())
    , (MicroTests.test26 ())
    , (MicroTests.test27 ())
    , (MicroTests.test28 ())
    , (MicroTests.test29 ())
    , (MicroTests.test30 ())
    , (MicroTests.test31 ())
    , (MicroTests.test32 ())
    , (MicroTests.test33 ())
    , (MicroTests.test34 ())
    , (MicroTests.test35 ())
    , (MicroTests.test36 ())
    , (MicroTests.test37 ())
    , (MicroTests.test38 ())
    , (MicroTests.test39 ())
    , (MicroTests.test40 ())
    , (MicroTests.test41 ())
    , (MicroTests.test42 ())
  ]

main : Element
main =
  let 
    parameterizedTests = 
      List.map2 (\(w,h) m -> (w,h,m)) 
      [
        (600, 100)
      , (600, 100)
      , (600, 100)
      , (600, 100)
      , (600, 100)
      , (600, 100)
      , (600, 100)
      , (600, 600)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 600)
      , (600, 300)
      , (600, 300)
      , (600, 200)
      , (600, 200)
      , (600, 330)
      , (600, 330)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      , (600, 200)
      ] 
      tests
  in
    E.flow E.down (List.map showOne parameterizedTests)

