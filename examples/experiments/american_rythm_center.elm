-- It could come or be stored in a database
instructors = 
  [ {id=1, name = "Susanna Wellman"}
  , {id=2, name = "Bo Munz"}
  , {id=3, name = "Alisia Mayville"}
  , {id=4, name = "Shenika Sciacca"}
  , {id=5, name = "Clinton Randazzo"}
  , {id=6, name = "Gayla Vannorman"}
  , {id=7, name = "Mildred Arzate"}
  , {id=8, name = "Royal Burchett"}
  ]
  
-- It could come from or be stored in a database.
courses = [
    { date = "Mon 5:00 pm - 6:00 pm"
    , title ="Modern - Open Level"
    , instructorId = 1}
  , { date = "Mon 6:00 pm - 7:15 pm"
    , title ="Tap - Level I (Beg)"
    , instructorId = 2}
  , { date = "Mon 6:00 pm - 7:00 pm"
    , title ="Musical Theatre Dance- Intermediate"
    , instructorId = 3}
  , { date = "Mon 7:15 pm - 8:30 pm"
    , title ="Tap -- (Advanced Tap)"
    , instructorId = 4}
  , { date = "Mon 8:00 pm - 9:00 pm"
    , title ="Heels - Intermediate"
    , instructorId = 5}
  , { date = "Tue 4:00 pm - 4:45 pm"
    , title ="Intro To African Dance (Ages 6-8)"
    , instructorId = 6}
  , { date = "Tue 4:45 pm - 5:30 pm"
    , title ="Intro To Tap (Ages 6-8)"
    , instructorId = 6}
  , { date = "Tue 6:00 pm - 7:50 pm"
    , title ="Tap - Level II (Fast Beginner)"
    , instructorId = 2}
  , { date = "Tue 7:00 pm - 8:00 pm"
    , title ="Hip Hop - Adult Beginner/Intermediate"
    , instructorId = 7}
  , { date = "Tue 7:15 pm - 8:30 pm"
    , title ="Tap - Level III/V"
    , instructorId = 2}
  , { date = "Tue 8:00 pm - 9:00 pm"
    , title ="Heels"
    , instructorId = 8}
  , { date = "Tue 9:00 pm - 10:00 pm"
    , title ="Clasic Walz"
    , instructorId = 0}
  ]

{-
Possible scenarios, each one being not trivial with usual technologies.
-- Rename a course
-- Rename an instructor
-- Change an instructor by replacing their name with another one
-- Add a missing instructor by replacing "To come..." with a name of an existing one
-- Rename "To come..." to "Not assigned"
-- Duplicate a row by clicking on the "+" button.
-- Delete a row by clicking on the "-" button.
-- TODO: Change the format of the date/time
-- TODO: Filter/Sort the table
-- TODO: Change
-}
main = <div class="mainwebpage">
 <img class="logo_lager" src="https://chicagotap.org/wp-content/uploads/2015/10/header-main-chrp2.png" alt="" style="height: 120px; padding: 5px 0px 0px;">
 <a href="https://chicagotap.org/american-rhythm-center/class-schedule/">original page</a>
 <table><tr class="schedule_header">
  <th>Date</th><th>Class</th><th>Instructor</th><th>admin</th>
 </tr>@List.map(\{date,title,instructorId} ->
     let instructorName = 
          List.findByAReturnB .id .name instructorId instructors
          |> Maybe.withDefault "To come..."
     in
     <tr>
       <td>@date</td>
       <td class="mbo_class">@title</td>
       <td class="trainer">@instructorName</td>
       <td>
         @Html.buttonToDuplicateEnclosing("tr")[["title", "duplicate this line"]]("+")
         @Html.buttonToDeleteEnclosing("tr")[["title", "remove this line"]]("-")</td>
     </tr>
   )(courses)
   @mblineadd
</table>


<style>
.mainwebpage {
  padding: 20px;
  background: #666;
  height: calc(100% - 40px);
}
.mainwebpage table {
  background-color: #f5f5f5;
  border: none;
  width: 100%;
  border-collapse: collapse;
  border-spacing: 0px;
  margin-top: .75em;
}
tr {
  background-color: #f5f5f5;
}
th {
  border: none;
  border-collapse: collapse;
  text-align: left;
  vertical-align: top;
  color: #ffffff !important;
  background-color: #5CB3FF !important;
  padding: .25em .5em;
  border-top: 1px solid #aaa;
  border-bottom: 1px solid #aaa;
  background: #e5e5e5;
  color: #222;
  font-size: 9px;
  line-height: 18px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  z-index: 2;
  
}
td {
  font-size: 12px;
  color: #808080;
  padding: .5em .5em;
  border: none;
  border-top: 1px solid #e5e5e5;
  border-collapse: collapse;
  text-align: left;
  vertical-align: top;
}
.mbo_class {
  font-weight: bold;
  color: #175de8 !important;
}
.trainer {
  color: #07b;
}
</style>
</div>

-- For educational purpose: What people did before
{- findByAReturnB is a better Bidirectional version of:
List.find (\i ->
  i.id == instructorId) instructors
|> Maybe.map .name
|> Maybe.withDefault "To come..."
-}
mblineadd = {-
  <tr>
    <td><input type="text" id="newdate" placeholder="new date"></td>
    <td><input type="text" id="newtitle" placeholder="new date"></td>
    <td><select id="newinstructor">@List.map(\{id,name} -> <option>@name</option>)(instructors)</select></td>
    <td><button title="Add a new course"
        v=(Update.lens {
          apply x = ""
          update {input = courses, outputNew = newCourseStr} =
            case evaluate newCourseStr of
              [newDate, newTitle, newInstructorId] ->
                Ok (InputsWithDiffs [(courses ++ [{date=newDate, title=newTitle, instructorId=newInstructorId}],
                  Just (VListDiffs [(List.length courses, ListElemInsert 1)]))])
              _ -> Err <| "Could not parse new course: " ++ newCourseStr
        } courses)
        onclick="""
          this.setAttribute("v",
            api.valToString(api.nativeToVal(
              [newdate.value,
               newtitle.value,
               newinstructor.selectedIndex + 1])))"""
       >+</button></td>
  </tr> -}
  <span></span>