isGroup g =
  -- Neutral element in the group
  case List.find (== g._2) g._1 of
    Nothing -> """@g: The neutral element @g._2 is not even in the set!"""
    Just x ->
  -- Neutrality
  case List.find (\x -> g._3 g._2 x /= x) g._1 of
    Just x -> """@g: @g._2 · @x ≠ @x so @g._2 is not a neutral element."""
    Nothing ->
  -- Operation stays in the group
  case cartProd g._1 g._1 |>
       List.find (\[x, y] ->
           List.find (== (g._3 x y)) g._1 == Nothing) of
    Just [x, y] -> """@g: @x · @y == @(g._3 x y) which is not the set."""
    Nothing ->
  -- Operation is associative
  case cartProd g._1 (cartProd g._1 g._1) |>
       List.find (\[x, [y, z]] -> g._3 x (g._3 y z) /= g._3 (g._3 x y) z) of
    Just [x, [y, z]] -> """@g: @x · (@y · @z) == @(g._3 x (g._3 y z)) ≠ @(g._3 (g._3 x y) z) = (@x · @y) · @z so the operation is not associative."""
    Nothing ->
  -- Every element has an inverse
  case List.find (\x -> List.find (\y -> g._3 x y == g._2) g._1 == Nothing ) g._1 of
    Just x -> """@g: @x does not have an inverse!"""
    Nothing ->
  """@g: It looks like it is a group!"""

groupCandidates = [
    ([-1, 1],0,+)
  , ([-2, -1, 0, 1, 2],0,+)
  , ([0, 1, 2],0,+)
  , ([-1, 0, 1],0,*)
  , ([-1, 0, 1],0,+)
  , ([-1, 0, 1],1,*)
  , ([-1, 0, 1],1,+)
  , ([0, 1],0,+)
  , ([0, 1],0,*)
  , ([0, 1],1,+)
  , ([0, 1],1,*)
  , ([0, 1],0,-)
  , ([-1, 1],1,*)]

<span>@(map (\g -> <span>@(isGroup g)<br></span>) groupCandidates)</span>