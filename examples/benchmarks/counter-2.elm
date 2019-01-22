--#updatedelay: 0
value = 0

<div>
  <h1>@value</h1>
  <button onclick=@(Html.do value (+ 1))>increment</button>
  <button onclick=@(Html.do value (\_ -> __jsEval__ """
      confirm("Random?") ? Math.floor(Math.random() * 10) : @value
      """))>random
    </button>
</div>