sync v1v2 = {
  apply ((s1, n1), (s2, n2)) = (
    if s1 == s2 then ((s1, n1), "")
    else let t = Html.freshTag () in
      ((s1, n1), <@t><script id=t mkupdate="">
setTimeout(function() {
document.getElementById("@t").setAttribute("mkupdate",`unify`)
}, 0);
</script></@>))
  update {input = ((s1, n1), (s2, n2)), output = ((newS, _), maybeScript) } = 
    let newsn = (
      if n1.recentlyModified then s1 else
      if n2.recentlyModified then s2 else
      newS, {recentlyModified=False})
    in
    Ok (Inputs [(newsn, newsn)])
}.apply v1v2

--------------------------------------------------------------

v1 = ("Hi big world!", {recentlyModified=False})
v2 = ("Hi big world!", {recentlyModified=False})
v3 = ("Hi big world!", {recentlyModified=False})

(v12, script) = sync (v1, v2)
(v123, script2) = sync (v12, v3)

<h1 style="padding:10px">
@v123._1
@script
@script2
</h1>