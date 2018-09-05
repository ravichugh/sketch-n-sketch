options = {production = False}

main = htmlpass <| markdown <|
<div class="outer"><div class="inner" contenteditable="true">
# Sketch-n-sketch Tutorial - Fair grades
_This tutorial supposes that you have Sketch-n-sketch 0.7.1 configured correctly_  
  
Finally, your teaching assistants corrected all the assignments,
the mid-terms and the final exam.
You are left with a list of student names and a grade over 100.  
How to fairly map these grades to letters, such as A+, A, A-, B+... until E?

To start, paste the following code in the Sketch-n-sketch editor:
@newcode<|"""@path1

-- Displays the interface
main = <span><h1>Fair grades</h1>
  We want to give a grade to our students.<br><br>
  @path2
  </span>"""
@displaycode
Above is the expected result. Note that the <code>main = </code> is optional, if the program ends with an expression, it will be the main.
@displayproductionevalcode

## Student data
Now is the time to import the grades.  
Let's suppose you managed to export a comma-separated value file containing on each line a name and a grade.  
@replacepath(path1)<|"""input_data = @(q3)Alice,64.7
Bob,78.5
Eve,57.9
Seya,100
Madelene,88.3
Mack,32.8
Clarita,90.5
Ulrike,98.5
Ferdinand,74.4
Tamara,93.5
Kellee,82.9
Leanne,83.7
Natalya,43.8
Leonore,84.5
Linette,98.5
Salley,88.4
Aleisha,88.6
Sabine,92.2
Ozella,60.9
Misti,86.7
Jenny,83.3
Nubia,92.3
Tennillee,95.1
Margaret,81.9
Loida,82.8
Cassandra,65.3
Derrick,66.4
Rudolph,83.2
Rafael,86.6@(q3)

@path3
"""## Convert student data
First, we convert this data to a list of structured records or datatypes.
@replacepath(path3)<|"""studentsGrades = 
  List.map (\line -> case Regex.extract "^(.*),(.*)" line of
    Just [name, gradeString] ->
      Student name (Update.freeze <| String.toFloat gradeString)
  ) <| Regex.split "\r?\n" input_data

@path4"""
Our language can define arbitrary data constructors, provided they start with a capital name, hence the <code>Student</code> constructor.
Note that we use <code>Update.freeze</code> to prevent any change to be propagated to the grade at this stage.

## Define the cut-offs
First, we need to define associations between letters and minimum grades.
We have a first guess (e.g. from previous year, or a linear guess).

@replacepath(path4)<|"""cutoffs = [
  CutOff "A+" 96.8,
  CutOff "A" 90,
  CutOff "A-" 86,
  CutOff "B+" 82.9,
  CutOff "B" 76,
  CutOff "B-" 68.5,
  CutOff "C+" 61,
  CutOff "C" 55.8,
  CutOff "C-" 40.4,
  CutOff "D" 29.5,
  CutOff "E" 0]

@path5"""## Display cut-offs and grades as SVG
We display cut-offs as horizontal lines, and the grades as bars,
to "see" where the cuts are. To make it meaningful, we sort students by grade.

@replacepath(path5)<|"""sortedGrades =
  sortBy (\(Student _ n1) (Student _ n2) -> n1 > n2) studentsGrades

numStudents = List.length sortedGrades

{softFreeze=keep} = Update
textstyle = "fill:black;font-family:monospace;font-size:12pt"

barChart x y w h = let
  barWidth = w / numStudents
  bars = List.indexedMap (\i (Student name avg) -> let
      xi = x + (i * barWidth)
      hi = h * (freeze 0.01! * avg)
      yi = y + h - hi
      in line 'blue' 2 xi yi (xi + barWidth) yi)
    <| List.reverse sortedGrades
  separators = cutoffs |> List.concatMap
      (\(CutOff abc cutoff) -> let
        y = (freeze y + freeze h * freeze 0.01! *
              (freeze 100! - cutoff))
        in
        [line 'black' (keep 2) (keep x) y (keep <| x + w) y,
        <text x=20 y=y style=textstyle>@@abc</text>,
        <text x=w+60 y=y style=textstyle>@@abc</text>
        ])
  background = rectWithBorder 'gray' 5 'lightgray' x y w h
  in List.concatMap identity [
    [background],
    bars,
    separators
  ]
width = 450
height = 389
chart = barChart 50 0 width height

@path6"""
@replacepath(path2)<|"""
<svg width=(toString (width + 100)) height=toString(height)>@@chart</svg>

@path2_1
"""

You should obtain the following result, after running the program.
@displayproductionevalcode

Thanks to SVG editing capabilities, we can now move the horizontal line to change cut-offs.

## First lens: 1-digit cut-offs
If you try to change a cut-off from the SVG, the cut-off number might start to have a lot of decimals. However, only one is necessary.
To avoid this, we add a lens to modify how the cut-off is updated.

@replacepath(path6)<|"""
updateDecimal = Update.lens {
  apply cutOff = cutOff
  update {input=oldCutOff,outputNew=newCutOff} =
    Ok (Inputs [round (newCutOff * 10) / 10])
}

@path7"""
@replacepath("freeze 100! - cutoff")<|"freeze 100! - updateDecimal cutoff"
A lens is a pair of two functions (here <code>apply</code> and <code>update</code>),
such that the first contains the logic to compute forward, and the second the logic to back-propagate an updated value.
In our case, this function takes the new output and round it to the nearest multiple of 0.1.
Since a lens is a record, we can invoke it using a special constructor called <code>Update.lens</code>.
Try to modify one cutoff to see: all cut-offs are rounded in the code.
@displayproductionevalcode

## Table of results
At this point, we hope that our cut-offs are well placed.
We now want to know how many students there are in each category.

@replacepath(path7)<|"""assignLetter avg = 
  List.find (\CutOff letter cutoff -> avg >= cutoff) cutoffs |>
  case of
    Nothing -> error <|
      @(q3)Could not find a letter for this average: @@avg@(q3)
    Just (CutOff letter cutoff) -> letter

buckets = let 
  initBucket = List.map (\c -> (c, [])) cutoffs
  addToBucket buckets letter student = case buckets of
    (((CutOff bletter _) as c, v) as head)::tail ->
      if bletter == letter then (c, v ++ [student])::tail
      else head :: addToBucket tail letter student
    [] -> error <| "Letter " + letter + " not found in buckets"
  in
  List.foldl (\((Student studName avg) as student) buckets ->
    addToBucket buckets (assignLetter avg) student
  ) initBucket sortedGrades

displayStudents =
  List.map (\Student name grade ->@q3@@name:@@grade@q3) >> String.join ","

displayBuckets = 
  <table>
  <tr><th>Grade</th><th>Students</th><th>Groupped</th><th>Cut-offs</th></tr>
  @@(List.indexedMap (\i (CutOff name cutoff, students) ->
    <tr style="background:"+(if i % 2 == 0 then "#dedede" else "white")>
      <td>@@name</td>
      <td title=displayStudents(students)>@@List.length(students)</td>
      @@(if i % 3 == 0 then
        <td rowspan="3">@@(
          buckets |> List.drop i |> List.take 3 |>
          List.map (\(n, students) -> List.length students) |> List.sum)
        </td>
      else [])
      <td>@@cutoff</td>
      </tr>
  ) buckets)
</table>

@path8
"""
@replacepath(path2_1)<|"""<h2>Grade by category</h2>
@@displayBuckets
"""
You should see the following table appear below the graph in the output view:
@displayproductionevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """
Note that you can again change the cutoffs from this table right where they are displayed!
 
## Unfair letter attribution.
We want to know if there are unfair letter attribution.
A possibly unfair letter attribution arises if two student grades are very close to each other (e.g. 0.2),
but they have been assigned different letters.
To avoid being contested, it is better to make sure there is no possibly unfair letter attribution.
We compute and display this information in the table.

@replacepath(path8)<|"""ifunfair i =
  if i == 0 then "" else let
  epsilon = 0.72{0-1.9}
  (CutOff nameA cutoffA, studentsAbove) = nth buckets (i - 1)
  (CutOff nameB cutoffB, studentsBelow) = nth buckets i
  in case (List.last studentsAbove, List.head studentsBelow) of
    (Just (Student enviee gradeA), Just (Student envier gradeB)) ->
      if gradeA - gradeB < epsilon then
      <span>@@envier (@@gradeB, @@nameB) might protest that @@enviee had almost
      the same grade (@@gradeA) but was assigned @@nameA</span>
      else <span></span>
    _ -> <span></span>

@path9
"""
@replacepath("<th>Cut-offs</th>")<|"""<th>Cut-offs</th><th>Status</th>"""
@replacepath("<td>@cutoff</td>")<|"""<td>@@cutoff</td><td>@@ifunfair(i)</td>"""
@displayproductionevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """

It looks like we were right, if you did not change the original cut-offs, we have one unfair letter attribution.
Of course, we can use the graph to change the cut-offs to avoid that, or change the cut-offs directly in the table.
What if we had a way to just resolve the potential complaint using buttons, such as "Upgrade XXX" or "Downgrade XXX"?

## Button to modify cut-offs

Adding buttons to upgrade or downgrade a student's means that these buttons will modify the cut-off.
This behavior can be locally defined with the following trick:
When we click a button, it modifies a property using JavaScript that is supposed to contain the cut-off, with a new cut-off.

Let us focus on the function "ifunfair" for this task.
@replacepath("was assigned @nameA</span>")<|"""
was assigned @@nameA.
<button onclick=@(q3)this.setAttribute('v', '@@(gradeB - 0.1)')@(q3)
  v=toString(updateDecimal cutoffA)>Give @@nameA to @@envier</button>
<button onclick=@(q3)this.setAttribute('v', '@@(gradeA + 0.1)')@(q3)
  v=toString(updateDecimal cutoffA)>Give @@nameB to @@enviee</button>
</span>
"""
@displayproductionevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """
You can now precisely resolve the dilemmas before they arrive, much faster than if you had to talk to students!
What is the fastest way to resolve the problem here? To downgrade or to upgrade?

## Second lens: Group size

Sometimes we wish we could simply change the number of students in a group to change the cut-off.
This is possible, we can add a lens applied to the displayed number of students.
This lens takes care of modifying the cut-offs in the backward direction.
Note that we never change the count itself!

@replacepath(path9)<|"""updateGroupCount bucketNum cutoff numStudents =
  Update.lens2 {
    apply (cutoff, count) = count
    update {input=(cutoff, prevCount), outputNew=newCount} =
      if newCount > prevCount then let -- Lower the bar, we want more students
        (CutOff _ cutoffBelow, stds2) = nth buckets (bucketNum  + 1)
        (upgrading, staying) = List.split (newCount - prevCount) stds2
        Student _ newCutoff = last upgrading
        in Ok (Inputs [(newCutoff, prevCount)])

      else let  -- Raise the bar, we want less students
        (_, students) = nth buckets bucketNum
        (staying, declassed) = List.split (
          List.length students - (prevCount - newCount)) students
        Student _ belowCutoff = hd declassed
        in Ok (Inputs [(belowCutoff + 0.1, prevCount)])
  } cutoff numStudents 
"""

We now focus on the main function.
@replacepath("@List.length(students)")<|"""@@updateGroupCount(i)(cutoff)<|
        List.length(students)"""

Try now to change the number of students in one group to see the cut-offs change appropriately.
For example, the you can decrease to 4 the number of students in B+ (downgrade Kellee) or increase to 6 the number of students in B+ (upgrade Loida).
@displayproductionevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """

## Final code
You can compare your code with the code used by this tutorial:
@displaycode

## Enhancements
What else do you want to try?
</div>
<style>
#outputCanvas {
  overflow: hidden
}
div.outer {
  width:100%;
  height:calc(100% - 0px);
  background:#BBB;
  overflow-y: scroll;
}
div.inner {
  padding-top:10px;
  padding-left:10px;
  padding-right:10px;
  margin:0px;
  max-width:600px;
  margin-left:auto;
  margin-right:auto;
  background:white
}
div.inner > h2 {
  padding-top: 20px;
}
div.intermediateresult {
  font-style: italic;
  color: #AAA;
}
code.snippet {
  white-space:pre;
  display:block;
  margin:10px;
  padding: 5px;
  border:1px solid black;
}
code.error {
  color:red;
  white-space:pre;
}
div.outputwrapper {
  -webkit-box-shadow: 5px 10px 5px 0px rgba(0,0,0,0.5);
  -moz-box-shadow: 5px 10px 5px 0px rgba(0,0,0,0.5);
  box-shadow: 5px 10px 5px 0px rgba(0,0,0,0.5);
  margin:10px;
  padding:10px;
  border:2px solid black
}
</style>
</div>

path1 = "-- We will write code here #1"
path2 = "Graphics coming soon..."
path3 = "-- We will write code here #2"
path4 = "-- We will write code here #3"
path5 = "-- We will write code here #4"
path6 = "-- We will write code here #5"
path7 = "-- We will write code here #6"
path8 = "-- We will write code here #7"
path9 = "-- We will write code here #8"
path2_1 = " Table coming soon..."
path2_2 = ""

newcode code = <newcode code=code></newcode>
replacepath path code = <replacepath path=path code=code class="snippet"></replacepath>
displaycode = <displaycode class="snippet"></displaycode>
displayevalcode = <displayevalcode></displayevalcode>
displayproductionevalcode = <displayproductionevalcode></displayproductionevalcode>

displayevalcodeLocalReplace regex replacement = <displayevalcode replace=regex by=replacement></displayevalcode>
displayproductionevalcodeLocalReplace regex replacement = <displayproductionevalcode replace=regex by=replacement></displayproductionevalcode>

displayintermediateresult display src =
  if display then
    case __evaluate__ (__CurrentEnv__) src of
      Err msg -> <code class="error">@msg</code>
      Ok evalNode ->
        <div class="outputwrapper">@evalNode</div>
  else
    <div class="intermediateresult">options.production is off. <button onclick="this.setAttribute('v', 'True')" v=(toString options.production)>Turn it on</button> to display the intermediate result there.</div>
    
markdown =
  Html.replace ("(?:^|\n)(#+)\\s(.+)") (\match ->
    [<@("h" + toString (String.length (nth match.group 1)))>@(nth match.group 2)</@>]
  ) >>
  Html.replace ("_(?=\\S)(.*?)_") (\match ->
    [<i>@(nth match.group 1)</i>]) >>
  Html.replace "(\r?\n|  )\r?\n" (\_ -> [<br>])

localReplace src attrs = case attrs of
  ["replace", regex]::["by", replacement]::attrs ->
    localReplace (Regex.replace (escape regex) (\_ -> replacement) src) attrs
  attrs -> (src, attrs)

escape = Regex.replace """\(|\)""" (\m -> if m.match == "(" then """\(""" else """\)""")
  
htmlpass htmlnode = 
  let aux src htmlnode = case htmlnode of
    ["newcode", ["code", code]::attrs, []] ->
      (code, htmlnode)
    ["replacepath", ["path", path]::["code", code]::attrs, []] ->
      let newSrc = Regex.replace (escape path) (\_ -> code) src in
      (newSrc, <span>Replace the code <code>@path</code> by<code @attrs>@code</code></span>)
    ["displaycode", attrs, []] ->
      (src, ["code", attrs, [["TEXT", src]]])
    ["displayproductionevalcode", attrs, []] ->
      let (localSrc, localAttrs) = localReplace src attrs in
      (src, displayintermediateresult options.production <| localSrc + "\n\nmain")
    ["displayevalcode", attrs, []] ->
      let (localSrc, localAttrs) = localReplace src attrs in
      (src, displayintermediateresult True <| localSrc + "\n\nmain")
    [tag, attrs, children] ->
      let (newSrc, newRevChildren) =
        List.foldl (\child (tmpSrc, revChildren) ->
          let (newTmpSrc, newChild) = aux tmpSrc child in
          (newTmpSrc, newChild::revChildren)
        ) (src, []) children
      in
      (newSrc, [tag, attrs, List.reverse newRevChildren])
    _ -> (src, htmlnode)
  in Tuple.second <| aux "" htmlnode

q3 = "\"\"\""