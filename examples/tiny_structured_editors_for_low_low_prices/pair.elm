type Pair a b = Pair a b

pairToString aToStr bToStr pair =
  case pair of
    Pair a b -> "(" + aToStr a + "," + bToStr b + ")"

toString : Pair String Num -> String
toString strNumPair  =
  pairToString (\s -> '"' + s + '"') (\n -> toString n) strNumPair

(Pair "key" 10 : Pair String Num)
