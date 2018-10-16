title = "Sketch-n-Sketch Docs"

evalupdate code leoLang =
  <div class="code" style="""background:@(if leoLang then "aliceblue" else "white");
       border:2px solid black;margin-bottom:2px;border-radius:8px;""">
    <textarea onkeyup="""if(this.getAttribute('v')!=this.value)
       this.setAttribute('v', this.value)""" v=(code)
       style="margin:0px;width:296px;height:52px;margin:5px;">@code</textarea>
    <pre style="vertical-align:top;margin-top:0px;">@(
      let res = evaluate <| LensLess.appendStrDef + code in
      if leoLang then toString res else res
    )</pre></div>

main =
  <div style="padding:20px">
    <h2>Welcome to @title!</h2>
      <p><b>What's this?</b>
      @title provides a reversible general-purpose language.
         You can edit both the program (on the left/top) or the resulting value
         (on the right/bottom). For example:</p>
      @(evalupdate "let x = \"Hello \" in\nx + \"\"\"and @(x)world\"\"\"" True)
      @(evalupdate """let x = "blue" in
<h3 style="color:"+x>This is @@x, change me!</h3>""" False)
      <p>Turn auto-sync on (on the right) to see the changes be applied
      immediately if they are not ambiguous !</p>
      <p>See some more examples from File -&gt; New From Template in
        the menu bar, or by pressing the Previous and Next
        buttons in the top-right corner.</p>
    @(Html.observeCopyValueToAttribute ".code > textarea" "v")</div>