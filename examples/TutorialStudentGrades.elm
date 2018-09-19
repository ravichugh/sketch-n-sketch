t = tutorialUtils

options = {production=True}

main = t.htmlpass options <| t.markdown <|
<div class="outer"><div class="inner" contenteditable=(toString (not options.production))>
# Sketch-n-sketch Tutorial - Fair grades
<i>This tutorial assumes that you have
<a href="https://mikaelmayer.github.io/sketch-n-sketch/">Sketch-n-sketch</a> running on your browser.
The 'left pane' references the left half of that interface, whereas the 'right pane' references the right half of that interface. If you encounter editing issues, try resizing your window.</i>  
  
At last, your teaching assistants have graded all the assignments,
the mid-terms and the final exam.
They've provided you with a list of student names and their average grades out of 100.
How do you fairly map these scores to letter grades, such as A+, A, A-, B+ ... down to E?

To start, replace the program in the Sketch-n-sketch editor by selecting everything and pasting the following code on the left pane of the Sketch-n-sketch editor:
@t.newcode<|"""@(placeholder 1)

-- Displays the interface
main = <span><h1>Fair grades</h1>
  We want to give grades to our students.<br><br>
  @graphicsPlaceholder
  </span>"""
@t.displaycode
After clicking on "Run", you should see the below result in the right pane.
Note that the <code>main = </code> is optional, if the program ends with an expression, it will automatically insert <code>main = </code> in front of it.
@t.displayevalcode

## Update basics
Sketch-n-sketch not only displays the webpage, it allows you to modify it.  
In the right panel,
<ul><li>type "student" inside "Fair grades" so that it becomes "Fair student grades".@t.hiddenreplace("Fair grades")("Fair student grades")@t.displayevalcode</li>
<li>A pop-up menu "Output Editor" appears on the left panel.</li>
<li>Hover over "Update program".</li>
<li>Hover over the first solution.</li>
<li>You'll see that the code is updated accordingly:@t.displaycode</li>
<li>Click on the first solution to accept the change.</li></ul>
FYI: Right next to the right panel, a button labelled "Auto Sync" enables the propagation of unambiguous changes automatically. We won't be using it in this tutorial.

## Import the grades
From the spreadsheet that your assistants provided you, you
generated a Comma-Separated Value (CSV) file with each line containing a name and a grade.
Fortunately, sketch-n-sketch can handle raw strings by enclosing them in triple quotes <code>"""</code>.
Let us assign this string data to the variable <code>@var_input_data</code>  
@t.replace(placeholder 1)<|"""@var_input_data = @(q3)Alice,64.7
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

@(placeholder 2)"""## Convert student data
If you want to display this data, you could simply insert <code>@@@var_input_data</code> into the <code>main</code> code, but that would only display the string, which would not be very helpful.  
First, we convert this data to a list of datatypes like <code>Student "Alice" 64.7</code> and store it to a variable <code>@var_studentsGrades</code>.  
@t.replace(placeholder 2)<|"""type Student = Student String Num

@var_studentsGrades = 
  @var_input_data
  |> Regex.split "\r?\n"
  |> List.map (\line -> case Regex.extract "^(.*),(.*)" line of
    Just [name, gradeString] ->
      Student name (Update.freeze <| String.toFloat gradeString)
  )

@(placeholder 3)"""First, notice how we define a datatype constructor <code>Student</code> that accepts a string (the name) and a number (the grade out of 100). This datatype is like a pair that stores the two pieces of information.  
The construction <code>b |> f a</code> is equivalent to the more natural <code>f a b</code> (a function <code>f</code> applied to two arguments <code>a</code> and <code>b</code>) but it is a good way to visualize the transformation step by step.  
Here, we use the library function <code>Regex.split</code> to split the <code>@var_input_data</code> on newlines, which gives us a list of strings.
Then, a regular expression is applied to each of these "lines" to extract the data before and after the comma, so that we can build a <code>Student</code> datum with the extracted <code>name</code> and the grade that we convert to a float (number).  
Note that we also add <code>Update.freeze</code> to make sure we will never modify this grade indirectly via the output.

## Define the cut-offs
We need to define associations between letter grades and the minimum scores (i.e., cutoffs) required to obtain them.
Let's say we have a first guess (e.g., using the cutoffs from the previous year, or a simple linear function).

@t.replace(placeholder 3)<|"""type CutOff = CutOff String Num

cutoffs = [
  CutOff "A+" 96.8,
  CutOff "A"  90,
  CutOff "A-" 86,
  CutOff "B+" 82.9,
  CutOff "B"  76,
  CutOff "B-" 68.5,
  CutOff "C+" 61,
  CutOff "C"  55.8,
  CutOff "C-" 40.4,
  CutOff "D"  29.5,
  CutOff "E"  0]

@(placeholder 4)"""## Example: get students in each group
Just as an exercise, if we wanted to display how many students obtained a grade between "B-" and "A+" (both inclusive), we could insert the following code next to <code>@graphicsPlaceholder</code> in <code>main</code>:
@t.displaylocalcode(example1)
You would end up with the following output in the right pane.
@t.displayevalcodeLocalReplace(graphicsPlaceholder)(example1)
This is just an example to illustrate general-purpose computing techniques (such as recursion, pattern matching, list construction and deconstruction), but for now you can discard this code snippet.
## Display cut-offs and grades as SVG
We display cut-offs as horizontal black lines, and the grades as short blue lines,
to "see" where the grades are in relation to the cutoffs. To make it meaningful, we sort students by grade.

@t.replace(placeholder 4)<|"""sortedGrades =
  @var_studentsGrades
  |> sortBy (\(Student _ n1) (Student _ n2) -> n1 > n2) 

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

@(placeholder 5)"""
@t.replace(graphicsPlaceholder)<|"""<span contenteditable="false">
<svg width=toString(width + 100)
     height=toString(height)>@@chart</svg></span><br>

@tablePlaceholder
"""

You should obtain the following result, after running the program.
@t.displayevalcode

Thanks to SVG editing capabilities, we can now move the horizontal lines up and down to change the cut-offs.

Make sure the line @t.lineof("""CutOff "B"""") is visible in the code editor (in the left panel).

Your task is now to move the cut-off of the letter B (in the right panel) to accept one more student (by dragging it slowly downwards).
A pop-up "Output Editor" appears. Hover over "Update program", it then takes 1 second to find where to change the cut-off. Click on the solution to accept it.
You should obtain something like the following on the right-hand side.  
@t.displayevalcodeLocalReplace("CutOff \"B\"  76")<|"CutOff \"B\"  72.7"

## First lens: 1-digit cut-offs
After this update, the cut-off number for the letter B might start to have a lot of decimals.
However, only one is necessary.
To avoid this, we add a lens to modify how the cut-off is updated.

@t.replace(placeholder 5)<|"""updateDecimal = Update.lens {
  apply cutOff = cutOff
  update {input=oldCutOff,outputNew=newCutOff} =
    Ok (Inputs [round (newCutOff * 10) / 10])
}

@(placeholder 6)"""
@t.replace("freeze 100! - cutoff")<|"freeze 100! - updateDecimal cutoff"
A lens is a pair of two functions (here <code>apply</code> and <code>update</code>),
such that the first contains the logic to do normal forward computation, and the second the logic to back-propagate an updated value.
In our case, this function takes the new output and rounds it to the nearest multiple of 0.1.
Since a lens is a record, we invoke it on an argument using a special constructor called <code>Update.lens</code>.  
  
Modify the cut-off for letter B back to where it was, approximately (just above the blue line right above it).
You will observe that, on update, the cut-off contains only one decimal in the code.

## Table of results
At this point, we hope that our cut-offs are well placed.
We now want to know how many students there are in each category.

@t.replace(placeholder 6)<|"""assignLetter avg = 
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
  List.map (\Student name grade ->@q3@@name:@@grade@q3)
  >> String.join ","

displayBuckets = 
  <table>
  <tr><th>Grade</th><th>Students</th><th>Grouped</th>
    <th>Cut-offs</th></tr>
  @@(List.indexedMap (\i (CutOff name cutoff, students) ->
    <tr style="background:"+(
         if i % 2 == 0 then "#dedede" else "white")>
      <td>@@name</td>
      <td title=displayStudents(students)
        >@@List.length(students)</td>
      @@(if i % 3 == 0 then
        <td rowspan="3">@@(
          buckets |> List.drop i |> List.take 3
          |> List.map (\(n, students) ->
            List.length students) |> List.sum)
        </td>
      else [])
      <td>@@cutoff</td>
      </tr>
  ) buckets)
</table>

@(placeholder 7)"""
@t.replace(tablePlaceholder)<|"""<h2>Grade by category</h2>
@@displayBuckets"""
You should see the following table appear below the graph in the output view:
@t.displayevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """
Note that you can again change the cutoffs from this table right where they are displayed, using basic update techniques.
 
## Unfair letter grade assignments
We want to know if any letter grades have been assigned unfairly.
A potentially unfair letter assignment arises if two student grades are very close to each other (e.g., the difference is less than 0.5),
but they have been assigned different letters.
To avoid being contested, it is best to make sure there is no potentially unfair letter grade assignment.
We compute and display this information in the table.

@t.replace(placeholder 7)<|"""ifunfair i =
  if i == 0 then "" else let
  epsilon = 0.5{0-1.9}
  (CutOff nameA cutoffA, studentsAbove) = nth buckets (i - 1)
  (CutOff nameB cutoffB, studentsBelow) = nth buckets i
  in case (List.last studentsAbove, List.head studentsBelow) of
    (Just (Student enviee gradeA),
     Just (Student envier gradeB)) ->
      if gradeA - gradeB < epsilon then
      <span>@@envier (@@gradeB, @@nameB) might protest that
      @@enviee had almost the same grade (@@gradeA) but was
      assigned @@nameA</span>
      else <span></span>
    _ -> <span></span>

@(placeholder 8)
"""
@t.replace("<th>Cut-offs</th>")<|"""<th>Cut-offs</th><th>Status</th>"""
@t.replace("<td>@cutoff</td>")<|"""<td>@@cutoff</td><td>@@ifunfair(i)</td>"""
@t.displayevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """

It looks like our concern was justified - we have one unfair grade assignment.
Of course, we can use the graph to change the cut-offs to avoid that, or change the cut-offs directly in the table.
But what if we had a way to resolve the potential complaint just using buttons, such as "Upgrade XXX" or "Downgrade XXX"?

## Buttons to modify cut-offs
Adding buttons to upgrade or downgrade a student's grade means that these buttons will modify the cut-off.
This behavior can be locally defined with the following trick:
When we click a button, it uses Javascript to modify the property that is supposed to contain the cut-off with a new cut-off.

Let us focus on the function "ifunfair" for this task.  
@t.replace("assigned @nameA</span>")<|"""assigned @@nameA.
<button onclick=@(q3)this.setAttribute('v', '@@(gradeB - 0.1)')@(q3)
 v=toString(updateDecimal cutoffA)>Give @@nameA to @@envier
</button>
<button onclick=@(q3)this.setAttribute('v', '@@(gradeA + 0.1)')@(q3)
 v=toString(updateDecimal cutoffA)>Give @@nameB to @@enviee
</button>
</span>"""
@t.displayevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """
You can now precisely resolve the dilemmas before they arrive, much faster than if you had to talk to students.
What is the fastest way to resolve all problems here? To downgrade or to upgrade? Can you guess from the graph?

## Second lens: Group size
Sometimes we wish we could simply change the number of students in a group to change the cut-off.
In typical interfaces this is impossible, but in Sketch-n-Sketch
we can apply a lens to the displayed number of students.
This lens takes care of modifying the cut-offs in the backward direction.
Note that we never change the logic used to compute the count in the code - the change to the count in the output is used to change
the cut-offs in the code.
@t.replace(placeholder 8)<|"""updateGroupCount bucketNum cutoff numStudents =
  Update.lens2 {
  apply (cutoff, count) = count
  update {input=(cutoff, prevCount), outputNew=newCount} =
    if newCount > prevCount then let
      -- Lower the bar, we want more students 
      (CutOff _ cutoffBelow, stds2) = nth buckets (bucketNum+1)
      (upgrading, staying) =
        List.split (newCount-prevCount) stds2
      Student _ newCutoff = last upgrading
      in Ok (Inputs [(newCutoff, prevCount)])

    else let  -- Raise the bar, we want less students
      (_, students) = nth buckets bucketNum
      (staying, declassed) = List.split (
        List.length students - (prevCount - newCount)) students
      Student _ belowCutoff = hd declassed
      in Ok (Inputs [(belowCutoff + 0.1, prevCount)])
  } cutoff numStudents"""
@t.replace("@List.length(students)")<|"""@@updateGroupCount(i)(cutoff)<|
        List.length(students)"""

Now try to change the number of students in one group to see the cut-offs change appropriately.
For example, you can change the number of students with a B+ to 4 (downgrading Kellee) or 6 (upgrading Loida).
@t.displayevalcodeLocalReplace("main = ")<|"""main = <span><h2>Grade by category</h2>
  @@displayBuckets
</span>
notmain = """

## Final code
You can compare your code with the code used by this tutorial:
@t.displaycode

## Enhancements
What else do you want to try?
</div>
<style>
#outputCanvas {
  overflow: hidden
  font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
}
div.outer {
  width:100%;
  height:calc(100% - 0px);
  background:#BBB;
  overflow-y: scroll;
  font-size: 16pt;
}
div.inner {
  padding-top:10pt;
  padding-left:10pt;
  padding-right:10pt;
  margin:0px;
  max-width:500pt;
  margin-left:auto;
  margin-right:auto;
  background:white
}
@@media only screen and (orientation: portrait) {
    div.outer {
        font-size: 16pt;
    }
    div.inner {
      margin:0px;
      width: calc(100%-10pt);
      max-width: 98%  !important;
      margin-left:auto;
      margin-right:auto;
      background:white
    }
    code.snippet {
      font-size: 2em;
    }
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
  overflow-x: scroll;
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
  border:2px solid black;
  margin-bottom: 15px;
}</style>
</div>

graphicsPlaceholder = "Graphics coming soon..."
placeholder i = """-- We will write code here #@i"""
tablePlaceholder = " Table coming soon..."

var_input_data = "input_data"
var_studentsGrades = "studentsGrades"

example1 = """@@(let
    thresholdOf name =
      let aux cutoffs = case cutoffs of
        [] -> 0
        CutOff n t :: tail -> if name == n then t else aux tail
      in aux cutoffs
    
    minGrade = thresholdOf "B-"
    maxGrade = 100
  in
  (@var_studentsGrades
  |> List.filterMap (\Student name grade ->
    if grade >= minGrade && grade < maxGrade
    then Just name else Nothing)
  |> String.join ", ") ++ " have at least a B-"
)"""

q3 = "\"\"\""
