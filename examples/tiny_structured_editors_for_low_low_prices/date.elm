type Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December

type Date = Date Num Month Num

toString : Num -> String
toString n = numToStringBuiltin n

toString : Month -> String
toString month =
  case month of
    January   -> "January"
    February  -> "February"
    March     -> "March"
    April     -> "April"
    May       -> "May"
    June      -> "June"
    July      -> "July"
    August    -> "August"
    September -> "September"
    October   -> "October"
    November  -> "November"
    December  -> "December"

-- "January 23, 1999"
toString : Date -> String
toString date =
  case date of
    Date year month day ->
      toString month + " " + toString day + ", " + toString year

(Date 2020 May 9 : Date)
