loadExp exp = 
  """app.ports.setExampleByName.send(@exp)"""

load name = loadExp """"@name""""

hash = "decodeURIComponent(location.hash.substring(1))"

<div style="margin:10px">
  @(let hash = __jsEval__ hash in
    if hash /= "" then
    [<h1>Loading '@hash'...</h1>,
     <span>This page will soon refresh to display @hash. if it does not work, you can always browse examples below:</span>]
    else [])
  <h1>Quick start</h1>
  <ul>
    <li><a onclick=(load "3: Conference Budget")>Conference budget</a></li>
    <li><a onclick=(load "5: Scalable Recipe Editor")>Scalable Recipe Editor</a></li>
    <li><a onclick=(load "8: Markdown Editor")>Markdown Editor</a></li>
    <li><a onclick=(load "12: LaTeX Editor")>LaTeX Editor</a></li>
    <li><a onclick=(load "14: Slides")>Slides</a></li>
    <li><a onclick=(load "15: Docs")>Docs</a></li>
  </ul>
  <script>
    setTimeout(function() {
      if(location.hash != "") {
        @(loadExp hash);
      }
    }, 500);
  </script>
</div>