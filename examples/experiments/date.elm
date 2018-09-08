  date = {
    region = "us"

    days = freeze [
    {number=0, name= "Monday", abbr="Mon"},
    {number=1, name= "Tuesday", abbr="Tue"},
    {number=2, name= "Wednesday", abbr="Wed"},
    {number=3, name= "Thursday", abbr="Thu"},
    {number=4, name= "Friday", abbr="Fri"},
    {number=5, name= "Saturday", abbr="Sat"},
    {number=6, name= "Sunday", abbr="Sun"}]
    
    months = freeze [
    {number=0, name= "January", abbr="Jan"},
    {number=1, name= "February", abbr="Feb"},
    {number=2, name= "March", abbr="Mar"},
    {number=3, name= "April", abbr="Apr"},
    {number=4, name= "May", abbr="May"},
    {number=5, name= "June", abbr="Jun"},
    {number=6, name= "July", abbr="Jul"},
    {number=7, name= "August", abbr="Aug"},
    {number=8, name= "September", abbr="Sep"},
    {number=9, name= "October", abbr="Oct"},
    {number=10, name= "November", abbr="Nov"},
    {number=11, name= "December", abbr="Dec"}]
    
    regexpDaysMonths =(days |> List.map (\{name,abbr} ->
      name + "|" + abbr
    ) |> String.join "|") + "|" + (months |> List.map (\{name,abbr} ->
      name + "|" + abbr) |> String.join "|")
    
    today () = {year=2018,month=8,day=7,weekday=5,hour=0,minute=0,second=0}
    
    addSeconds seconds ({second} as d) =
      let final = mod (second + seconds) 60 in
      let minutesToAdd = floor ((second + seconds) / 60) in
      let d2 = {d | second = final} in
      if minutesToAdd > 0 then addMinutes minutesToAdd d2 else d2

    addMinutes minutes ({minute} as d) =
      let final = mod (minute + minutes) 60 in
      let hoursToAdd = floor ((minute + minutes) / 60) in
      let d2 = {d | minute = final} in
      if hoursToAdd > 0 then addHours hoursToAdd d2 else d2

    addHours hours ({day, hour} as d) =
      let final = mod (hour + hours) 24 in
      let daysToAdd = floor ((hour + hours) / 24) in
      let d2 = {d | hour = final} in
      if daysToAdd > 0 then { d2 | day = day + dayToAdd } else d2 -- TODO Continue or better system
    
    fromString string =
      findInterleavings 0 (regexpDaysMonths + """|\d\d\d\d([-/\.])\d?\d\1\d?\d|\d?\d([-/\.])\d?\d\1\d\d\d\d|\d?\d(:|h)\d?\d((?::|m)\d\d)?|\d\d\d\d|\d?\d|pm|am""") string
      |> foldl (\a (({year,month,day,weekday,hour,minute,second} as d, lastParsed) as acc) ->
        case a of
          Left string -> acc
          Right found ->
            let _ = Debug.log "found" found in
            List.findByAReturnB .name .number found.match days
            |> Maybe.map (\newWeekday ->
              ({d | weekday = newWeekday}, Just "weekday")
              )
            |> Maybe.withDefaultLazy (\_ ->
            List.findByAReturnB .abbr .number found.match days
            |> Maybe.map (\newWeekday ->
              ({d | weekday = newWeekday}, Just "weekday")
              )
            |> Maybe.withDefaultLazy (\_ ->
            List.findByAReturnB .name .number found.match months
            |> Maybe.map (\newMonth ->
              ({d | month = newMonth}, Just "month")
              )
            |> Maybe.withDefaultLazy (\_ ->
            List.findByAReturnB .abbr .number found.match months
            |> Maybe.map (\newMonth ->
              ({d | month = newMonth}, Just "month")
              )
            |> Maybe.withDefaultLazy (\_ ->
            extractFirstIn """^(\d\d\d\d)([-/\.])(\d?\d)\1(\d?\d)$""" found.match
            |> Maybe.map (\[newYearStr, _, newMonthStr, newDayStr] ->
              ({d | year = String.toInt newYearStr,
                    month = String.toInt newMonthStr - 1,
                    day = String.toInt newDayStr - 1}, Just "dddd/dd/dd")
              )
            |> Maybe.withDefaultLazy (\_ ->
            extractFirstIn """^(\d?\d)([-/\.])(\d?\d)\1(\d\d\d\d)$""" found.match
            |> Maybe.map (\[newDM1, _, newDM2, newYearStr] ->
              let
                dm1 = String.toInt newDM1 - 1 
                dm2 = String.toInt newDM2 - 1 in
              if region == "us" || newDM2 >= 12 then
                ({d | year = String.toInt newYearStr,
                    month = dm1,
                    day = dm2}, Just "dd/dd/dddd")
              else
                ({d | year = String.toInt newYearStr,
                    month = dm2,
                    day = dm1}, Just "dd/dd/dddd")
                
              )
            |> Maybe.withDefaultLazy (\_ ->
            extractFirstIn """^\d\d\d\d$""" found.match
            |> Maybe.map (\[newYearStr] ->
              ({d | year = String.toInt newYearStr}, Just "year"))
            |> Maybe.withDefaultLazy (\_ ->
            extractFirstIn """^(\d?\d)[:h](\d?\d)(?:[:m](\d\d))?)$""" found.match
            |> Maybe.map (\[newHour, newMinute, newSecond] ->
              ({d | hour = String.toInt newHour,
                    minute = String.toInt newMinute,
                    second = String.toInt newSecond}, Just "time"))
            |> Maybe.withDefaultLazy (\_ ->
            extractFirstIn """^(pm|am)$""" found.match
            |> Maybe.map (\[pmOrAm] -> 
              if pmOrAm == "pm" && d.hour < 12 && lastParsed == Just "time" then ({ d | hour = d.hour + 12}, Just "time")
              else if pmOrAm == "am" && d.hour == 12 && lastParsed == Just "time" then ({d | hour = 0}, Just "time")
              else acc)
            |> Maybe.withDefaultLazy (\_ ->
              let n = String.toInt found.match in
              case lastParsed of
                Just "month" -> ({d | day = n - 1}, Just "day")
                Just "weekday" -> ({d | day = n - 1}, Just "day")
                _ -> acc
            )))))))))
      ) (today (), Nothing) |> Tuple.first

    numberToDay n = List.findByAReturnB .number .name n days
    numberToDayAbbr n = List.findByAReturnB .number .abbr n days
    dayToNumber d = List.findByAReturnB .name .number d days
    abbrToNumber d = List.findByAReturnB .abbr .number d days
    
    explodeDate = identity -- for now
    
    format template date =
      let {year,month,day,weekday,hour,minute,second} = explodeDate date in
      template
      |> Regex.replace "yyyy|yy|MMMM|MMM|MM|M|dddd|ddd|dd|d|HH|H|hh|h|mm|m|ss|s|tt|t|\"[^\"]+\"" (
        \m -> case m.match of
        "yyyy" -> toString year
        "yy" ->   String.takeRight 2 <| toString year
        "MMMM" -> List.findByAReturnB .number .name month months |> Maybe.withDefault (freeze <| """<Name of Month=@month>""")
        "MMM"  -> List.findByAReturnB .number .abbr day months |> Maybe.withDefault (freeze <| """<Abbr of Month=@month>""")
        "MM" ->   String.padLeft 2 "0" <| toString (month + freeze 1)
        "M" ->    toString (month + freeze 1)
        "dddd" -> List.findByAReturnB .number .name weekday days |> Maybe.withDefault (freeze <| """<Name of Weekday=@weekday>""")
        "ddd" ->  List.findByAReturnB .number .abbr weekday days |> Maybe.withDefault (freeze <| """<Abbr of Weekday=@weekday>""")
        "dd"  ->  String.padLeft 2 "0" <| toString (day + freeze 1)
        "d"  ->   toString (day + freeze 1)
        "hh" ->   String.padLeft 2 "0" <| toString (mod hour 12)
        "h" ->    toString (mod hour 12)
        "HH" ->   String.padLeft 2 "0" <| toString hour
        "H" ->    toString hour
        "mm" ->   String.padLeft 2 "0" <| toString minute
        "m" ->    toString minute
        "ss" ->   String.padLeft 2 "0" <| toString second
        "s" ->    toString second
        "t"  ->   Update.lens {
                    apply hour = if hour < 12 then "a" else "p",
                    update {input=hour, outputNew} = if outputNew == "a" then Ok (Inputs [hour - 12]) else if outputNew == "p" then Ok (Inputs [hour + 12]) else Err ("Not a/p for am/pm: " + outputNew) } hour
        "tt" ->   Update.lens {
                    apply hour = if hour < 12 then "am" else "pm",
                    update {input=hour, outputNew} = if outputNew == "am" then Ok (Inputs [hour - 12]) else if outputNew == "pm" then Ok (Inputs [hour + 12]) else Err ("Not a/p for am/pm: " + outputNew) } hour
        _ -> case extractFirstIn "^\"([^\"]+)\"$" m.match of
          Just [txt] -> txt
          Nothing -> m.match)
  }

  -- {year=2018,month=8,day=7,weekday=5,hour=13,minute=34,second=7}
  <span>@(
    let d =
      date.fromString "Mon 8 Sep 4:03 pm" |>
      date.addMinutes 5
    in   
    date.format "dddd, MMMM d, yyyy \"at\" H\"h\"mm \"or\" h:mm tt" d)</span>