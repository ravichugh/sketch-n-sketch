type Pair a b = Pair a b | Left a | Right b | Nothing

pairToString aToStr bToStr pair =
  case pair of
    Pair a b -> "(" + aToStr a + "," + bToStr b + ")"
    Left a   -> "(" + aToStr a + ", - )"
    Right b  -> "( - ," + bToStr b + ")"
    Nothing  -> "( - , - )"

toString : Num -> String
toString n = numToStringBuiltin n

toString : String -> String
toString str = '"' + str + '"' -- no escaping

toString : Pair a b -> String
toString strNumPair  =
  pairToString toString toString strNumPair

(Pair "key" 10 : Pair String Num)
