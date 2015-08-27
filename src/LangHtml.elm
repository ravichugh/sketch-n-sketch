-- LangHtml.elm

module LangHtml where
-- module LangSvg (valToHtml, valToIndexedTree, printIndexedTree) where

import Html
import Html.Attributes as HA
import Svg
import Svg.Attributes as A
import VirtualDom

-- in Svg.elm:
--   type alias Svg = VirtualDom.Node
--   type alias Attribute = VirtualDom.Property

-- in Html.elm:
--   type alias Html = VirtualDom.Node

import Debug
import Set
import String
import Char
import Dict exposing (Dict)
import ColorNum

import Lang exposing (..)
import Utils

-- Noodling about what it would take to replace LangSvg with this, and have the
-- output be retargeted to elm-html instead of elm-svg.
