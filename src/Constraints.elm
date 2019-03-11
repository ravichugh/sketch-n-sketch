module Constraints exposing
  ( assertEqual
  )

import UnLang exposing (..)
import Utils

assertEqual : UnExp d -> UnExp d -> Maybe Constraints
assertEqual u1 u2 =
  case (u1, u2) of
    (UConstructor _ ctorName1 uInner1, UConstructor _ ctorName2 uInner2) ->
      if ctorName1 == ctorName2 then
        assertEqual uInner1 uInner2
      else
        Nothing

    (UNum _ n1, UNum _ n2) ->
      if n1 == n2 then
        Just []
      else
        Nothing

    (UBool _ b1, UBool _ b2) ->
      if b1 == b2 then
        Just []
      else
        Nothing

    (UString _ s1, UString _ s2) ->
      if s1 == s2 then
        Just []
      else
        Nothing

    (UTuple _ us1, UTuple _ us2) ->
      if List.length us1 == List.length us2 then
        List.map2 assertEqual us1 us2
          |> Utils.projJusts
          |> Maybe.map List.concat
      else
        Nothing

    -- TODO partial function?
    -- TODO function closure?

    (UHoleClosure _ env (holeId, _), _) ->
      Maybe.map (\ex -> [(holeId, (env, ex))]) <|
        expToExample u2

    (_, UHoleClosure _ env (holeId, _)) ->
      Maybe.map (\ex -> [(holeId, (env, ex))]) <|
        expToExample u1

    (UApp _ u1 u1Args, UApp _ u2 u2Args) ->
      if List.length u1Args == List.length u2Args then
        let
          funcConstraints =
            assertEqual u1 u2

          argConstraints =
            List.map2 assertEqual u1Args u2Args
              |> Utils.projJusts
              |> Maybe.map List.concat
        in
          Maybe.map2 (++) funcConstraints argConstraints
      else
        Nothing

    (UGet _ n1 i1 u1, UGet _ n2 i2 u2) ->
      if n1 == n2 && i1 == i2 then
        assertEqual u1 u2
      else
        Nothing

    -- TODO throw away environments?
    -- TODO match branches?
    (UCase _ _ u1 _, UCase _ _ u2 _) ->
      assertEqual u1 u2

    _ ->
      Nothing
