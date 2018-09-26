(lengthStr, alphabet) = (freeze "0" + "000", "LIVE")

(nbDigits, alphabetArray) = (toString (String.length lengthStr), explode alphabet)

universal_number lengthStr =
  let alphabetSizeStr =
    let aux acc a = case a of [] -> acc; head::tail -> aux (acc + "#") tail in
    aux "" alphabetArray
  in
  let stringRepeat sequence size =
    let aux acc sz = case String.uncons sz of
      Just ("#", tail) -> aux (acc + sequence) tail
      _ -> acc
    in aux "" size in
  let times sequence sizeBinary =
    let string_aux acc sizeB = case String.uncons sizeB of
      Just ("0", tail) ->
        let newAcc = stringRepeat acc alphabetSizeStr in
        string_aux newAcc tail
      Just ("1", tail) ->
        let newAcc = stringRepeat acc alphabetSizeStr + sequence in
        string_aux newAcc tail
      _ -> acc
    in
    string_aux "" sizeBinary in
  let numabBin = ("1" + replaceFirstIn "." "" lengthStr) in
  let numab = String.length (times "#" numabBin) in
  let init =
    let aux acc letterList =
      case letterList of
        [] -> acc
        head :: tail -> aux (acc + (times head numabBin)) tail
    in aux "" alphabetArray
  in
  let repeatedPrefix = "^.{" + (toString numab) + "}" in
  let positionNext posStr =
    let aux acc pos = if Regex.matchIn repeatedPrefix pos then (
        let newPos = replaceFirstIn repeatedPrefix "" pos in
        let newAcc = acc + "#" in
        aux newAcc newPos
      ) else (
        acc + (let k a l = case String.uncons l of Just ("#", tail) -> k (a + pos) tail; _ -> a in k "" alphabetSizeStr)
      )
    in aux "" posStr in
  let init0 = replaceAllIn "." "0" init in
  let cycle startStr currentStr state acc =
    let newCurrentStr = positionNext currentStr in
    let newCurrentStrLength = toString (String.length newCurrentStr) in
    let newAcc = acc + (
      replaceFirstIn ("^.{" + toString (String.length currentStr) + "}(.).*$") "$1" init
    ) in
    let newState = replaceFirstIn ("^(.{"+newCurrentStrLength+ "," +newCurrentStrLength +"}).")  (\m -> (nth m.group 1) + "1") state in
    if (Regex.matchIn ("^" + startStr + "$") newCurrentStr) then (
      [newState, newAcc]
    ) else (
      cycle startStr newCurrentStr newState newAcc
    ) in
  let loop state acc =
    if (Regex.matchIn "^1*$" state) then
      acc
    else (
      let startPos = replaceAllIn "." "#" (replaceFirstIn "^(1*)0.*$" "$1" state) in
      let [newState, newAcc] = cycle startPos startPos state acc in
      loop newState newAcc
    ) in
  let debrujin = loop init0 "" in
  let lm1 = toString (String.length (replaceFirstIn "." "" lengthStr)) in
  debrujin + (replaceFirstIn ("^(.{"+lm1+"}).*$") "$1" debrujin)

u_number = universal_number lengthStr

<div style="margin:10px">
<span>
  <h1>Finite Univeral Numbers</h1>
  This program serves as an example of creating a page
  that can be way longer than the code used to produce it.
  If you download it, you will see that its size is almost constant,
  regardless on the size of the sequence displayed below.
  <br><br>
  One of the smallest sequences on <code>@alphabet</code> containing
  all @nbDigits-digit sequences has length @(toString (String.length u_number))
  and is given below. The computation implements the idea given in the wikipedia
  article <a href="https://en.wikipedia.org/wiki/De_Bruijn_sequence#Construction">De Bruijn Sequence</a>. To test the validity of this claim, press <kbd>CTRL+F</kbd> and look for any sequence of @nbDigits characters among <span>`</span>@alphabet<span>`</span>.<br>
  <code style="whitespace:pre-wrap;max-width:100%">@u_number</code></span></div>