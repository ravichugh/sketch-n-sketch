type Begin    = NegInf          | After Num Bool
type End      = Before Num Bool | Inf
type Interval = Interval Begin End

-- toString : Begin -> String
-- toString begin =
--   case begin of
--     NegInf          -> "(-∞"
--     After num True  -> "[" ++ toString num
--     After num False -> "(" ++ toString num
--
-- toString : End -> String
-- toString end =
--   case end of
--     Before num True  -> toString num ++ "]"
--     Before num False -> toString num ++ ")"
--     Inf              -> "∞)"


-- toString : Begin -> String
-- toString begin =
--   case begin of
--     NegInf             -> "(-∞"
--     After num isClosed -> if isClosed then "[" + toString num else "(" + toString num
--
-- toString : End -> String
-- toString end =
--   case end of
--     Before num isClosed -> if isClosed then toString num + "]" else toString num + ")"
--     Inf                 -> "∞)"


toString : Num -> String
toString n = numToStringBuiltin n

toString : Begin -> String
toString begin =
  case begin of
    NegInf             -> "(-∞"
    After num isClosed -> (if isClosed then "[" else "(") + toString num

toString : End -> String
toString end =
  case end of
    Before num isClosed -> toString num + (if isClosed then "]" else ")")
    Inf                 -> "∞)"

toString : Interval -> String
toString interval =
  case interval of
    Interval begin end -> toString begin + "," + toString end

valueOfInterest =
  Interval NegInf (Before 10 True) : Interval

valueOfInterest
