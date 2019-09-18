type Pair a b = Pair a b

pairToString aToStr bToStr pair =
  case pair of
    Pair a b -> "(" + aToStr a + "," + bToStr b + ")"

intervalToString : Pair String Num -> String
intervalToString strNumPair  =
  pairToString (\s -> '"' + s + '"') (\n -> toString n) strNumPair

(Pair "key" 10 : Pair String Num)
