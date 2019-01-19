todoItem prop =
  <li class=@(if prop.done then "done" else "")
     onclick=@(Html.do prop .toggle)>
     @prop.value
  </li>

-- Test
oneprop = { id=1, value="Hello", done=True, toggle this = { this | done = not done } }
<span>
<style>
li.done { color: #888; text-decoration: line-through }
</style>
<ul>
 @todoItem(oneprop)
</ul>