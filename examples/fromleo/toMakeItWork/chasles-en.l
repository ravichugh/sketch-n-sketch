// download: chasles-relation-en.html
// output: html
[``@let subtitle = "Remember what you read/red about the relation of Chasles/shaal"
@let regex = "(\\s|^)(\\S*)/(\\S*)(?=\\s|$)"
@let t = {display = replaceAllIn regex subtitle "$1$2", speak = replaceAllIn regex subtitle "$1$3"}
@let [A, B, C, AX, AY, BX, BY, CX, CY] = ["A", "B", "C", "50", "80", "250", "50", "270", "180"]
<svg viewBox="0 0 350 200">
  <defs>
    <marker id="arrowhead" markerWidth="10" markerHeight="7" refX="9" refY="2" orient="auto">
      <polygon points="0 0, 10 2, 0 4"></polygon>
    </marker>
  </defs>
  <line x1="@AX" y1="@AY" x2="@BX" y2="@BY" stroke="#000" stroke-width="2" marker-end="url(#arrowhead)"></line>
  <line x1="@BX" y1="@BY" x2="@CX" y2="@CY" stroke="#000" stroke-width="2" marker-end="url(#arrowhead)"></line>
  <line x1="@AX" y1="@AY" x2="@CX" y2="@CY" stroke="#000" stroke-width="2" marker-end="url(#arrowhead)"></line>
  <text x="@AX" y="@AY" transform="translate(-7,12)" fill="black">@A</text>
  <text x="@BX" y="@BY" transform="translate(5,5)" fill="black">@B</text>
  <text x="@CX" y="@CY" transform="translate(2,10)" fill="black">@C</text>
  <line x1="10" y1="135" x2="30" y2="135" stroke="#000" stroke-width="1" marker-end="url(#arrowhead)"></line>
  <line x1="45" y1="135" x2="65" y2="135" stroke="#000" stroke-width="1" marker-end="url(#arrowhead)"></line>
  <line x1="80" y1="135" x2="100" y2="135" stroke="#000" stroke-width="1" marker-end="url(#arrowhead)"></line>
  <text x="10" y="150" fill="black">@A@B + @B@C = @A@C</text>
  <text x="10" y="175" fill="black">@(AX + !"," + AY + !" " + BX + !"," + BY + !" " + CX + !"," + CY)</text>
</svg>
<p id="view" class="lead">@(t.display + !"<span></span>")</p>
<style>#voice {cursor: pointer}</style>
<span class="glyphicon glyphicon-volume-up"></span><em id="voice">@(t.speak)</em>``,
{tag="script",attributes={src="http://code.responsivevoice.org/responsivevoice.js"}},
{tag="script",children=[{tag="#text",value=``
  setTimeout(function(){
    document.getElementById("voice").onclick = function() {
      var content = document.getElementById("voice").textContent;
      alert("Speaking aloud '"+content+"'. This is just a simulation.")
      //responsiveVoice.speak(content, "US English Female")
    }
  },10)
``}]}]