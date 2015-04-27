module MainSvg where

import Svg exposing (..)
import Svg.Attributes as A exposing (cx, cy, r)
import Html
import Graphics.Element as E exposing (Element)
import Text
import Color

import Lang exposing (..)
import LangParser
import MicroTests
import Sync
import Utils

valToSvg : Val -> List Svg
valToSvg v = case v of
  VList vs -> List.map valToSvg_ vs

valToSvg_ v =
  let str = toString in
  case v of
    VList [VConst x _, VConst y _] ->
      circle [ cx (str x), cy (str y), r "5", A.fill "#999999" ] []

valToElt : Val -> Element
valToElt v =
  E.color Color.lightGray <|
    let html = Svg.svg [] (valToSvg v) in
    let (w,h) = (400, 100) in
    Html.toElement w h html

showMonoString s = E.leftAligned (Text.monospace (Text.fromString s))
showString s     = E.leftAligned (Text.fromString s)

expToElt : Exp -> Element
expToElt = showMonoString << Lang.sExp

main : Element
main =
  let {e,v,vnew} = MicroTests.test16 () in
  let l1 = [ showString "Original Program", expToElt e
           , showString "Original Canvas", valToElt v
           , showString "Updated Canvas", valToElt vnew ] in
  let l2 =
    case Sync.sync e v vnew of
      Err e -> [[ E.show e ]]
      Ok results ->
        flip Utils.mapi results <| \(i,((ei,vi),vdiff)) ->
          [ showString <| "Option " ++ toString i ++ " "
                          ++ Utils.parens ("vdiff = " ++ toString vdiff)
          , expToElt ei
          , valToElt vi
          ]
  in
  E.flow E.right [
    E.spacer 10 10
  , E.flow E.down (List.intersperse (E.spacer 10 10) (l1 ++ List.concat l2))
  ]

