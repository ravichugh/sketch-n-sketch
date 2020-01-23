type Nat = Zero | Succ Nat

toString : Num -> String
toString n = numToStringBuiltin n

natToNum : Nat -> Num
natToNum n = case n of
  Zero   -> 0
  Succ m -> numPlus 1 (natToNum m) -- Bare + is string append :/

toString : Nat -> String
toString nat = toString (natToNum nat)

(Succ (Succ Zero) : Nat)
