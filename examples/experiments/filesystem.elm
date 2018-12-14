initfileOperations = [
  ("f", CreateFolder ["a.txt", "b.txt", "c.txt"]),
  ("f/a.txt", Write "" "Hello" []),
  ("f/b.txt", Write "" "world" []),
  ("f/c.txt", Write "" "!" [])]

computedFileOperations = []

fileOperations = Update.lens2 {
  apply (x, y) = x
  update {input=(x,y),outputNew} = Ok (Inputs [(x, outputNew)])
} initfileOperations computedFileOperations

fs = nodejs.delayed fileOperations

Html.forceRefresh <|
<span>
<span id="listdircontent">@("""@(fs.listdircontent "f")""")</span><br>
<button onclick="""
document.getElementById("listdircontent").innerText =
  `[("b.txt", "world")]`
""">Delete a and c</button><br>
<button onclick="""
document.getElementById("listdircontent").innerText =
  `[ ("a.txt", "Hello"),("d.txt", "big"),("b.txt", "world"), ("c.txt", "!"),("e.txt","Bye!") ]`
""">Insert d and e</button><br>
<button onclick="""
document.getElementById("listdircontent").innerText =
  `[ ("a-new.txt", "Hello"), ("b.txt", "world"), ("c-new.txt", "!") ]`
""">Rename a and c</button><br>
<button onclick="""
document.getElementById("listdircontent").innerText =
  `[ ("a.txt", "Hello"), ("b.txt", "warld"), ("c.txt", "?") ]`
""">Modify b and c</button><br>

</span>