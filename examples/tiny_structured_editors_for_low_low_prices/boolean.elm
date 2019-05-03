type MyBool = T | F

intervalToString : MyBool -> String
intervalToString bool =
  case bool of
    T -> "true"
    F -> "false"

(T : MyBool)
