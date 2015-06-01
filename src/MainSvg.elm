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
  [ (600, 100, MicroTests.test15)
  , (600, 100, MicroTests.test16)
  , (600, 100, MicroTests.test17)
  , (600, 100, MicroTests.test18)
  , (600, 100, MicroTests.test19)
  , (600, 100, MicroTests.test20)
  , (600, 100, MicroTests.test21)
  , (600, 600, MicroTests.test22)
  , (600, 200, MicroTests.test23)
  , (600, 200, MicroTests.test24)
  , (600, 200, MicroTests.test25)
  , (600, 200, MicroTests.test26)
  , (600, 200, MicroTests.test27)
  , (600, 200, MicroTests.test28)
  , (600, 200, MicroTests.test29)
  , (600, 200, MicroTests.test30)
  , (600, 600, MicroTests.test31)
  , (600, 300, MicroTests.test32)
  , (600, 300, MicroTests.test33)
  , (600, 200, MicroTests.test34)
  , (600, 200, MicroTests.test35)
  , (600, 330, MicroTests.test36)
  , (600, 330, MicroTests.test37)
  , (600, 200, MicroTests.test38)
  , (600, 200, MicroTests.test39)
  , (600, 200, MicroTests.test40)
  , (600, 200, MicroTests.test41)
  , (600, 200, MicroTests.test42)
  , (600, 200, MicroTests.test43)
  , (600, 300, MicroTests.test44)
  ]

sampleTests =
  tests
    |> List.map Utils.thd3
    |> Utils.mapi (\(i,f) ->
         let name = "test" ++ toString (i+14) in
         let thunk () = let {e,v} = f () in {e=e, v=v} in
         (name, thunk))
    |> List.reverse

main : Element
main =
  E.flow E.down (List.map (showOne << Utils.mapThd3 (\f -> f ())) tests)

