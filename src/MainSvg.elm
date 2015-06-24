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
          case Sync.inferLocalUpdates Sync.defaultOptions e v vnew of
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

main : Element
main =
  E.flow E.down (List.map (showOne << Utils.mapThd3 (\f -> f ())) MicroTests.tests)

