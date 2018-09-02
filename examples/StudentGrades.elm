type Student = Student String Num

studentsGrades = [
  Student "Alice" 64.7,
  Student "Bob" 78.5,
  Student "Eve" 57.9,
  Student "Seya" 100,
  --http://listofrandomnames.com,
  Student "Madelene" 88.3,
  Student "Mack" 32.8,
  Student "Clarita" 90.5,
  Student "Ulrike" 98.5,
  Student "Ferdinand" 74.4,
  Student "Tamara" 93.5,
  Student "Kellee" 82.9,
  Student "Leanne" 83.7,
  Student "Natalya" 43.8,
  Student "Leonore" 84.5,
  Student "Linette" 98.5,
  Student "Salley" 88.4,
  Student "Aleisha" 88.6,
  Student "Sabine" 92.2,
  Student "Ozella" 60.9,
  Student "Misti" 86.7,
  Student "Jenny" 83.3,
  Student "Nubia" 92.3,
  Student "Tennillee" 95.1,
  Student "Margaret" 81.9,
  Student "Loida" 82.8,
  Student "Cassandra" 65.3,
  Student "Derrick" 66.4,
  Student "Rudolph" 83.2,
  Student "Rafael" 86.6
]

gradeOf Student _ avg = avg

type CutOff = CutOff String Num

cutoffs = [
  CutOff "A+" 96.8,
  CutOff "A" 90,
  CutOff "A-" 86,
  CutOff "B+" 80.4,
  CutOff "B" 76,
  CutOff "B-" 68.5,
  CutOff "C+" 61,
  CutOff "C" 55.8,
  CutOff "C-" 40.4,
  CutOff "D" 29.5,
  CutOff "E" 0]

updateDecimal x = {
  apply x = x
  update {outputNew=xp} =
    Ok (Inputs [round (xp * 10) / 10])
}.apply x

-- Step to freeze the grades but still allow changes in the names
weightedAverages = List.map (\Student name grade -> Student name (freeze grade)) studentsGrades
weightedAverages = sortBy (\(Student _ n1) (Student _ n2) -> n1 > n2) weightedAverages -- sort and shadow

assignLetter avg = let
  foo cutoffs = case cutoffs of
    [] -> error 'CRASH'
    (CutOff letter cutoff) :: rest ->
      if avg >= cutoff then letter else foo rest
  in foo cutoffs

buckets = let 
  initBucket = List.map (\c -> (c, [])) cutoffs
  addToBucket buckets letter std = case buckets of
    (((CutOff bletter _) as c, v) as head)::tail ->
      if bletter == letter then (c, v ++ [std])::tail
      else head :: addToBucket tail letter std
    [] -> error <| "Letter " + letter + " not found in buckets"
  
  add sortedStudents buckets =
    case sortedStudents of
      [] -> buckets
      ((Student studName avg) as std) :: remaining_students ->
        addToBucket buckets (assignLetter avg) std |>
        add remaining_students 
  in
  add weightedAverages initBucket

numStudents = List.length weightedAverages

barChart x y w h = let
  barWidth = w / numStudents
  bars = List.indexedMap (\i (Student name avg) -> let
      xi = x + (i * barWidth)
      hi = h * (freeze 0.01! * avg)
      yi = y + h - hi
      in line 'blue' 2 xi yi (xi + barWidth) yi)
    <| List.reverse weightedAverages
  separators = cutoffs |> List.concatMap
      (\(CutOff abc cutoff) -> let
        y = (freeze y + freeze h * freeze 0.01! * (freeze 100! - updateDecimal cutoff))
        in
        [line 'black' (Update.softFreeze 2) (Update.softFreeze x) y (Update.softFreeze <| x + w) y,
        <text x=20 y=y style="fill:black;font-family:monospace;font-size:12pt">@abc</text>,
        <text x=w+60 y=y style="fill:black;font-family:monospace;font-size:12pt">@abc</text>
        ])
  background = rectWithBorder 'gray' 5 'lightgray' x y w h
  in List.concatMap identity [
    [background],
    bars,
    separators
  ]

checkAdjacentBuckets =
  let epsilon = 0.72{0-1.9} in
  <span>Ok</span> :: (
  zip buckets (tl buckets) |>
  List.map
    (\((s1, l1), (s2, l2)) ->
      if List.isEmpty l1 || List.isEmpty l2 then <span>Ok</span> else
      let
        Student n2 a2 = hd l2
        Student n1 a1 = last l1
      in
      if a2 + epsilon < a1 then <span>Ok</span>
      else <span>@n2 (grade @a2) and @n1 (grade @a1) are too close</span>
    ))

width = 550
height = 389
chart = barChart 50 0 width height

-- Lense to change the number of students in a category by modifying the category's threshold.
changeStudents index = Update.lens2 {
    apply (cutoff, count) = count
    update {input=(cutoff, prevCount), outputNew=newCount} =
      if newCount > prevCount then let -- Lower the bar, we want more students
        (CutOff _ nextCutoff, stds2) = nth buckets (index  + 1)
        (upgrading, staying) = List.split (newCount - prevCount) stds2
        worstUpgradingAvg = gradeOf <| last upgrading
        bestStayingAvg = case List.head staying of
          Just std -> gradeOf std
          Nothing -> nextCutoff + 0.1
        newCutoff = (worstUpgradingAvg + bestStayingAvg) / 2
        in Ok (Inputs [(newCutoff, prevCount)])

      else let  -- Raise the bar, we want less students
        currentStudents = nth buckets index |> Tuple.second
        (staying, declassed) = List.split (List.length currentStudents - (prevCount - newCount)) currentStudents
        Student _ bestDeclassedAvg = hd declassed
        in if List.isEmpty staying then
          Ok (Inputs [(bestDeclassedAvg + 0.1, prevCount)])
        else let
          Student _ worstStayingAvg = last staying
          newCutoff = (worstStayingAvg + bestDeclassedAvg) / 2
          in Ok (Inputs [(newCutoff, prevCount)])
  }
       
gradingData = Update.applyLens {
  apply grades = List.map (\(Student name avg) -> name + "\t" + toString avg) grades |>
    String.join "\n"
  update {outputNew} = -- We need to parse outputNew.
    let newStudents = Regex.split "\n" outputNew |> List.map (\line ->
      case Regex.extract """([^\t,:]+)[\t,:]+([\d\.]+)""" line of
        Just [name, avg] -> Student name (String.toFloat avg)
        Nothing -> error <| "I do not recognize this as a name / grade: " + line
    ) in Ok (Inputs [newStudents])
} studentsGrades

<div style="margin:10px" contenteditable="true">
<h1>Fair Final Grades</h1>
<h2>Input averages</h2>
@Html.forceRefresh<|<textarea
  onkeyup="if(this.getAttribute('v') != this.value) this.setAttribute('v', this.value)"
v=gradingData style="margin:0px;height:84px;width:217px">@gradingData</textarea>
<h2>Grade diagram</h2>
<svg width=(toString (width + 100)) height=toString(height)>@chart</svg>
<h2>Grade by category</h2>
<table>
  <tr><th>Grade</th><th>Students</th><th>Groupped</th><th>Threshold</th><th>Status</th></tr>
  @(List.indexedMap (\i ((CutOff name t, stds), feedback) ->
    <tr style="background:"+(if i % 2 == 0 then "#dedede" else "white")>
      <td>@name</td>
      <td title=(stds|>List.map(\Student name grade ->"""@name:@grade""")|>String.join",")>
        @changeStudents(i)(t)<|List.length(stds)</td>
      @(if i % 3 == 0 then
        <td rowspan="3">@(-- for above: 
          buckets |> List.drop i |> List.take 3 |>
          List.map (\(n, stds) -> List.length stds) |> List.sum)
        </td>
      else [])
      <td>@t</td>
      <td>@feedback</td>
      </tr>
  ) (zip buckets checkAdjacentBuckets))
</table>
</div>