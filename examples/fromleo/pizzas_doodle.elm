{join} = String
{matchIn} = Regex
{indices, map, find} = List

{ onChangeAttribute } = Html

css = """
.partTableCell{
  cursor:pointer;
}
.partTableCell:hover{
  outline: 1px solid black;
}
.hiddenAcc{
  display:block;
  position:absolute;
  top:-999em;
}
.form-control, textarea, select, input[type="text"], input[type="password"], input[type="datetime"], input[type="datetime-local"], input[type="date"], input[type="month"], input[type="time"], input[type="week"], input[type="number"], input[type="email"], input[type="url"], input[type="search"], input[type="tel"], input[type="color"], .uneditable-input{
  height:30px;
  padding-top:4px;
  padding-bottom:4px;
  border-radius:0;
  border-color:#b7b7b7;
  font-size:13px;
}
.textPoll th, .textPoll .foot td{
  max-width:94px;
}
label{
  font-weight:normal;
}
label{
  display:inline-block;
  margin-bottom:5px;
  font-weight:bold;
}
table.poll tr.participation td {
  text-align:center;
  background-color:#ffffff;
}
table.poll tr.participant td.n {
  background-color:#ffccca;
}
table.poll tr.participant td.y {
  background-color:#d1f3d1;
}
table.poll tr.participant td {
  text-align:center;
  vertical-align:middle;
  height:33px;
}
table.poll tr.participation.inEdit td:hover{
  background-color:#d6d6d6;
}
table.poll tr.participation td.pname,table.poll tr.participant td.pname{
  position:relative;
  min-width:182px;
  width:182px;
  border-top:1px solid #fff;
  border-bottom:2px solid #fff;
  background:url('http://doodle.com/builtstatic/1465286543/doodle/graphics/sprites/common/normal-s92f91c2182.png') -15px -972px no-repeat #fff;
  imageRendering:-moz-crisp-edges;
  imageRendering:-o-crisp-edges;
  imageRendering:-webkit-optimize-contrast;
  imageRendering:crisp-edges;
  -ms-interpolation-mode:nearest-neighbor;
  padding:0 2px 0 0;
}
table.poll tr.header.month th.xsep{
  background:url("http://doodle.com/graphics/polls/tick31r.png") right 0 repeat-y #3385e4;
  padding-right:13px;
}
table.poll td.pname div.pname{
  text-align:left;
  font-size:15px;
  line-height:15px;
  padding:8px 0 5px 0;
  margin-left:3px;
  white-space:nowrap;
  overflow:hidden;
  max-width:135px;
  width:135px;
  position:relative;
}
table.poll tr.date th, table.poll tr.date td{
  background:#3385e4;
  color:#fff;
}
table.poll{
  border-collapse:separate;
}
table.poll tr.date.month th{
  padding-top:7px;
  padding-bottom:3px;
}
table.poll tr.header th, table.poll tr.header td{
  padding:0 10px 0;
}
table.poll th, table.poll td{
  font-size:13px;
  line-height:17px;
  font-weight:normal;
}
table.poll tr.participation td.pname input{
  font-size:12px;
  height:24px;
  line-height:20px;
  margin:3px 0 3px 3px;
  width:131px;
  padding:2px 6px 2px 9px;
}
table.poll tr th.nonHeader.partCount, table.poll tr td.partCount{
  background:#fff;
  padding:0 0 9px 0;
  vertical-align:bottom;
  font-size:13px;
  color:#323232;
  min-width:184px;
  width:184px;
  font-weight:bold;
  max-width:none;
}
table.poll tr.sums td{
  text-align:center;
  line-height:16px;
  padding-left:5px;
  padding-right:8px;
  padding-top:5px;
  color:#6f6f6f;
}
table.poll tr.participant td.q {
  background-color:#eaeaea;
}
pre {
  cursor:text;
}
.columnadder {
  opacity: 0.1;
}
.columnadder:hover {
  opacity: 1;
}
"""

participants = [
  "John Doe",
  "Mam Butterfly",
  "Mysterious Man"
  ]

choix = [
  [0, 0, True ],
  [0, 1, False],
  [0, 2, False],
  [1, 0, False],
  [1, 1, True ],
  [1, 2, False],
  [2, 0, False ],
  [2, 2, True ]
]

menus = [
  "Margharita",
  "Pepperoni",
  "Chicken"
]


addPerson participants choix = {
  apply (participants, choix) = Update.freeze ""
  update {input=(oldParticipants, oldChoix), outputNew = newParticipant} =
    if newParticipant /= "" then
      let newParticipants = oldParticipants ++ [newParticipant] in
      let newChoix = choix ++ (map (\i -> [len oldParticipants, i, False]) (indices menus)) in
      {values = [(newParticipants, newChoix)] }
    else {values = [(oldParticipants, oldChoix)]}
}.apply (participants, choix)


total menuId =
  let n = choix |> List.map (\[userId, mId, check] -> if check && menuId == mId then 1 else 0) |> List.sum in
  <td>@n</td>

onChangeAttribute model controller =
    { apply model = Update.freeze ""
      update {input, outputNew} = { values = [controller input outputNew] }
    }.apply model
 
wantsToEat personId menuId =
  let predicate [pId, mId, b] = pId == personId && mId == menuId in
  case find predicate choix of
    Just ([pId, mId, checked] as choix) ->
      if checked then
        <td class=(Update.softFreeze "partTableCell xsep pok y")
            onclick="this.classList.remove('y');this.classList.add('n');this.setAttribute('x', 'NO')"
            title=((nth participants personId) + ", " + (nth menus menuId) + ": Yes")
            x=(onChangeAttribute checked (\old newValue -> newValue == "YES"))
            >
          <span class="glyphicon glyphicon-ok"></span>
        </td>
      else
        <td class=(Update.softFreeze "partTableCell xsep pn n")
            onclick="this.classList.remove('n');this.classList.add('y');this.setAttribute('y', 'YES')"
            title=((nth participants personId) + ", " + (nth menus menuId)+ ": No")
            y=(onChangeAttribute checked (\old newValue -> newValue == "YES"))
            > </td>
    _ ->
      <td class="partTableCell q sep pog" title=((nth participants personId) + ", " + (nth menus menuId)) onclick="this.setAttribute('z', 't')"
      z=(onChangeAttribute choix (\old _ -> [personId, menuId, True]::choix))
      >?</td>

ligne menu = <th>@menu</th>

pollInfo personId =
  <tr class="participant partMyself">
    <td class="pname" id="part1367130755">
      <span class="pname">@(nth participants personId)</span>
    </td>
    @(map (\menusId -> wantsToEat personId menusId) (indices menus))
  </tr>

numMenus = len menus
  
page =
  <table cellpadding="0" cellspacing="0" class="poll textPoll " summary="LARA Thai">
    <tbody>
      <tr class="header date month">
        <th class="nonHeader partCount boldText">
          <div>@(toString (len participants) + " participants")</div>
        </th>
        @(map (\menu -> <th>@menu</th>) menus)
        <th class="columnadder"><button onclick="""this.setAttribute('v@numMenus', 'T')""" @([["""v@numMenus""", onChangeAttribute menus (\oldMenus _ -> oldMenus++["""Meal#@numMenus"""])]])>+</button></th>
      </tr>
      <tr></tr>
      @(map pollInfo (indices participants))
      <tr class="participation yesNo">
        <td class="pname">
          <label class="hiddenAcc" forid="pname" title="l10n_yourName"></label>
          <input class="form-control" id="pname" maxlength="64" name ="name" placeholder="Your name"
             onkeypress="""if(event.keyCode == 13) this.setAttribute("v", this.value);"""
             type ="text" v=(addPerson participants choix)>
        </td>
        @(map total (indices menus) )
      </tr>
    </tbody>
  </table>

date = "Thursday, April 26th 2018"
urlencode txt = txt |>
  Regex.replace ":" "%2F5" |>
  Regex.replace "/" "%2F" |>
  Regex.replace " " "%20" |>
  Regex.replace "\n" "%0A"

command = String.join "," <| map (\(index, menu) -> 
  let [_, _, [[_, t]]] = total index in
  let who = choix |> List.filterMap (\[pId, mId, checked] -> if checked && mId == index then Just (nth participants pId) else Nothing) in
  if t == "0" then "" else " " + t + " pizza" + (if t=="1" then "" else "s") +" " + menu + " (" + String.join ", " who + ")"
  ) <| zipWithIndex menus

sms = """Hello Pizzaiolo,
Always a pleaure to do business with you.
For the lunch of @date, we would like to order@command.
I'll pick up the pizzas at 1:15pm.
Best regards,
The Dream Team
"""

<div style="margin:20px">
  <style>@css</style>
  <h1>Pizza meeting, @date</h1>
  <p>Choose your favorite pizzas. We will order some to satisfy everyone</p>
  @page
  <a contenteditable="False" href="""mailto:pizzaiolo@@gmail.com?subject=Pizzas for @date&body=@(urlencode sms)""">Send this email to the pizzaiolo</a>
  <br>
  <pre style="white-space:pre-wrap">@sms</pre>
</div>