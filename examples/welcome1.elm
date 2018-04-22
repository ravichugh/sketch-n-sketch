evalupdate code =
  <div class="code">
    <textarea onkeyup="this.setAttribute('v', this.value)" v=(code)>@(Html.text code)</textarea>
    <pre style="display:inline-block;vertical-align:top;margin-top:0px;">@(Html.text <| toString (evaluate code))</pre>
  </div>

title = Html.text "Sketch-n-Sketch Docs"

main =
  <div style="padding:20px">
    <h2>Welcome to @title!</h2>
      <p><b>What's this?</b> @title provides a reversible general-purpose language.
         You can edit both the program (on the left) or the resulting value (on the right).
         Try it now, change anything in this view!
      </p>
      @[evalupdate "\"Hello \" + \"world\""]
      <p>See some examples from File -&gt; New From Template in
        the menu bar, or by pressing the Previous and Next
        buttons in the top-right corner.
       </p>
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