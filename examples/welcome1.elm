appenddef = """letrec append a b = case a of [] -> b; (h::t) -> h :: append t b in
let text x = [["TEXT", x]] in """

evalupdate code isText =
  <div class="code" style="""background:@(if isText then "aliceblue" else "white");border:2px solid black;margin-bottom:2px;border-radius:8px;""">
    <textarea onkeyup="if(this.getAttribute('v')!=this.value) this.setAttribute('v', this.value)" v=(code) style="margin:0px;width:296px;height:52px;margin:5px;">@(Html.text code)</textarea>
    <pre style="vertical-align:top;margin-top:0px;">@(
      (if isText then (\x -> Html.text <| toString x) else (\x -> [x])) <| evaluate <| Debug.log "finalcode" (appenddef + code)
    )</pre>
  </div>

title = Html.text "Sketch-n-Sketch Docs"

main =
  <div style="padding:20px">
    <h2>Welcome to @title!</h2>
      <p><b>What's this?</b> @title provides a reversible general-purpose language.
         You can edit both the program (on the left) or the resulting value (on the right).
         For example:
      </p>
      @[evalupdate "let x = \"Hello \" in\nx + \"\"\"and @(x)world\"\"\"" True]
      @[evalupdate "let x = \"red\" in\n<h3 style=(\"color:\"+x)>\n  This is @(text x)</h3>" False]
      <p>See some more examples from File -&gt; New From Template in
        the menu bar, or by pressing the Previous and Next
        buttons in the top-right corner.</p>
    <script>
      function handleMutation(mutations) {
        mutations.forEach(function(mutation) {
          mutation.target.value = mutation.target.getAttribute("v");
        }) }
      var textAreasObserver = new MutationObserver(handleMutation);
      var textAreas = document.querySelectorAll(".code &gt; textarea");
      for (i = 0; i &lt; textAreas.length; i++)
        textAreasObserver.observe(textAreas[i], {attributes: true});
     </script>
  </div>