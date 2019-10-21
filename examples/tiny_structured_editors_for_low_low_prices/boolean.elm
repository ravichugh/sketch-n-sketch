type MyBool = T | F

toString : MyBool -> String
toString bool =
  case bool of
    T -> "true"
    F -> "false"

(T : MyBool)
