type MyBool = T | F
type Nat    = S Nat | Z

-- Pierce. TAPL Chapter 11. p. 144

isEven x =
  case x of
    Z    -> T
    S sx -> isOdd sx

isOdd x =
  case x of
    Z    -> F
    S sx -> isEven sx

intervalToString : Nat -> String
intervalToString nat =
  case isEven nat of
    T -> "true"
    F -> "false"

((S (S (S Z))) : Nat)
