input = """Nested and factorized diffs: 0-3
Distributed sources: 1-4
First real interactive course content: 2-6
Compiler and traces: 3-7
Bidirectional-enabled type checker: 0-10
Server running on our language: 8-12
Rights managements enforced: 10-14
Collaborative editing tool à la Google doc: 9-15
Imperative features (PHP support): 15-18"""

input_array = input |> Regex.split "\r?\n" |> List.map (
 Regex.extract """(.*):\s(\d+)-(\d+)*""" >> case of Just [name, start, end] ->
   [name, String.toInt start, String.toInt end])

mpx = 15

dd =  ["Jun-Aug 2018", "Sep-Nov 2018", "Dec 18-Feb 19", "Mar-May 2019", "Jun-Aug 2019", "Sep-Nov 2019"]

input_table = <table class="input_table">
<colgroup>
  <col>
    @(List.range 0 17 |> List.map (\i -> <col style="""width:@(mpx)px !important;""">))
</colgroup>
<tbody>
<tr><th></th>
@(List.range 0 5 |> List.map (\i -> <th colspan="3">@(nth dd i)</th>))</tr>
@(input_array |> List.concatMap (\[name, start, end] ->
  [<tr class="regularth"><td>@name</td>
    @(List.range 1 start |> List.map (\_ -> <td class="before"></td>))
    <td start=start end=end colspan="""@(end - freeze start)"""><div class="during"></div></td>
    @(List.range end 17 |> List.map (\_ -> <td class="after"></td>))
  </tr>]
)
)
</tbody>
</table>

main = <span><style>
.input_table {
  border-collapse: collapse;
}
.input_table th:first-child {
  width: 10em;
}
.input_table th:not(:first-child) {
  height: 10px;
}
.input_table th:not(:first-child):not(:last-child) {
  border-right: 2px solid #888;
}
.input_table td:not(:first-child) {
  height: 2em;
}
.separation {
  min-height: 2px;
}
.during {
  height:1.9em;background:blue;
  width: 100%;
}
.before {
  height:10px;background:#EEE;
  border-right: 1px solid #888;
}
.before:nth-child(3n+2) {
  border-left: 2px solid #888;
}
.after:nth-last-child(3n) {
  border-left: 2px solid #888;
}
.before:nth-child(2) {
  border-left: 1px solid #888;
}
.after {
  border-left: 1px solid #888;
}
.after:last-child {
  border-right: 1px solid #888;
}
.regularth {
  margin-bottom: 5px;
}
</style>@input_table</span>