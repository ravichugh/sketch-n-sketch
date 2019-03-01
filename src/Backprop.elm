module Backprop exposing
  ( backprop
  )

import UnExp exposing (..)
import Example exposing (..)
import UnDeclarations exposing (..)

import TriEval

import Utils

dontCareHole : Example
dontCareHole =
  ExConstructor "---dontCareHole---" (ExNum -3468801)

backprop : UnExp () -> Example -> Maybe Constraints
backprop u ex =
  if ex == dontCareHole then
    Just []
  else
    case (u, ex) of
      (UConstructor _ uIdent uInner, ExConstructor exIdent exInner) ->
        if uIdent == exIdent then
          backprop uInner exInner
        else
          Nothing

      (UNum _ uN, ExNum exN) ->
        if uN == exN then
          Just []
        else
          Nothing

      (UBool _ uB, ExBool exB) ->
        if uB == exB then
          Just []
        else
          Nothing

      (UString _ uS, ExString exS) ->
        if uS == exS then
          Just []
        else
          Nothing

      (UTuple _ uInners, ExTuple exInners) ->
        if List.length uInners == List.length exInners then
          exInners
            |> List.map2 backprop uInners
            |> Utils.projJusts
            |> Maybe.map List.concat
        else
          Nothing

      (UFunClosure _ env params body, ExPartialFunction bindings) ->
        let
          backpropBinding (arguments, outputExample) =
            if List.length params == List.length arguments then
              body
                |> TriEval.evalWithEnv (Utils.zip params arguments ++ env)
                |> Result.toMaybe
                |> Maybe.andThen (flip backprop outputExample)
            else
              Nothing
        in
          bindings
            |> List.map backpropBinding
            |> Utils.projJusts
            |> Maybe.map List.concat

      (UHoleClosure _ env (i, j), _) ->
        Just [(i, (env, ex))]

      (UApp _ (UHoleClosure _ env (i, _)) uArgs, _) ->
        Just [(i, (env, ExPartialFunction [(uArgs, ex)]))]

      (UGet _ n i uArg, _) ->
        let
          exTuple =
            ExTuple <|
              List.repeat (i - 1) dontCareHole
                ++ [ex]
                ++ List.repeat (n - i) dontCareHole
        in
          backprop uArg exTuple

      (UCase _ env uScrutinee branches, _) ->
        Debug.crash "Case not yet supported for backprop"
--         backpropBranch (constructorName, varName, binding) =
--           backprop
--             uScrutinee
--             (ExConstructor constructorName dontCareHole)

      _ ->
        Nothing
