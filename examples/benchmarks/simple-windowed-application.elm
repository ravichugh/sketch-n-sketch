--#updatedelay: 0
clicks = 0

<div>
  @(if clicks == 0 then "There have been no clicks yet" else clicks)
  <button onclick=@(Html.do clicks (+ 1))>click me</button>
</div>