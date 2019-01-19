state = {count = 0}

<div>
  <h1>@(state.count)</h1>
  <button onclick=@(Html.do state.count (+ -1))>-
    </button>
  <button onclick=@(Html.do state.count (+ 1))>+
    </button>
</div>