let (<<) f g x =
  f (g x)

let (>>) f g x =
  g (f x)
