type Begin    = NegInf | After Num Bool
type End      = Before Num Bool | Inf
type Interval = Interval Begin End

-- beginToString : Begin -> String
-- beginToString begin =
--   case begin of
--     NegInf          -> "(-∞"
--     After num True  -> "[" ++ toString num
--     After num False -> "(" ++ toString num
--
-- endToString : End -> String
-- endToString end =
--   case end of
--     Before num True  -> toString num ++ "]"
--     Before num False -> toString num ++ ")"
--     Inf              -> "∞)"


-- beginToString : Begin -> String
-- beginToString begin =
--   case begin of
--     NegInf             -> "(-∞"
--     After num isClosed -> if isClosed then "[" + toString num else "(" + toString num
--
-- endToString : End -> String
-- endToString end =
--   case end of
--     Before num isClosed -> if isClosed then toString num + "]" else toString num + ")"
--     Inf                 -> "∞)"


beginToString : Begin -> String
beginToString begin =
  case begin of
    NegInf             -> "(-∞"
    After num isClosed -> (if isClosed then "[" else "(") + toString num

endToString : End -> String
endToString end =
  case end of
    Before num isClosed -> toString num + (if isClosed then "]" else ")")
    Inf                 -> "∞)"

intervalToString : Interval -> String
intervalToString interval =
  case interval of
    Interval begin end -> beginToString begin + "," + endToString end

(Interval NegInf (Before 10 True) : Interval)
