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
import MicroTests
import Sync
import Utils

valToSvg : Val -> List Svg.Svg
valToSvg v = case v of
  VList vs -> flip List.map vs <| \v1 -> case v1 of
    VList (VBase (String shape) :: vs') ->
      let attrs = flip List.map vs' <| \v2 -> case v2 of
        VList [VBase (String a), VConst i _]       -> (attr a) (toString i)
        VList [VBase (String a), VBase (String s)] -> (attr a) s
        VList [VBase (String "points"), VList pts] ->
          let s =
            Utils.spaces <|
              flip List.map pts <| \v3 -> case v3 of
                VList [VConst x _, VConst y _] ->
                  toString x ++ "," ++ toString y
          in
          (attr "points") s
      in
      (svg shape) attrs []

------------------------------------------------------------------------------

funcsSvg = [
    ("circle", Svg.circle)
  , ("line", Svg.line)
  , ("polygon", Svg.polygon)
  , ("rect", Svg.rect)
  ]

funcsAttr = [
    ("cx", A.cx)
  , ("cy", A.cy)
  , ("fill", A.fill)
  , ("height", A.height)
  , ("points", A.points)
  , ("r", A.r)
  , ("stroke", A.stroke)
  , ("strokeWidth", A.strokeWidth)
  , ("width", A.width)
  , ("x", A.x)
  , ("x1", A.x1)
  , ("x2", A.x2)
  , ("y", A.y)
  , ("y1", A.y1)
  , ("y2", A.y2)
  ]

find d s =
  case Utils.maybeFind s d of
    Just f  -> f
    Nothing -> Debug.crash <| "MainSvg.find: " ++ s

attr = find funcsAttr
svg  = find funcsSvg

------------------------------------------------------------------------------

valToElt : Val -> Element
valToElt v =
  E.color Color.lightGray <|
    let html = Svg.svg [] (valToSvg v) in
    -- let html = Svg.svg [ A.x "0", A.y "0", A.viewBox "0 0 323.141 322.95" ] (valToSvg v) in
    let (w,h) = (600, 100) in
    Html.toElement w h html

showMonoString s = E.leftAligned (Text.monospace (Text.fromString s))
showString s     = E.leftAligned (Text.fromString s)

expToElt : Exp -> Element
expToElt = showMonoString << Lang.sExp

showOne test =
  let {e,v,vnew} = test in
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
      MicroTests.test15 ()
    , MicroTests.test16 ()
    , MicroTests.test17 ()
    , MicroTests.test18 ()
    , MicroTests.test19 ()
    , MicroTests.test20 ()
    , MicroTests.test21 ()
    , MicroTests.test22 ()
  ] in
  E.flow E.down (List.map showOne tests)

