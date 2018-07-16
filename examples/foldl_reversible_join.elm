big = "big"
world = " world"
list = ["Hello", "2", "", big, world, "!"]

[color1, color2] = ["red", "blue"]
highlight color x = <span style="""color:@color""">@x</span>

list2 = [[1, 2], [], 3, [4, 5]]

<div style="margin:10px">
<h1>Reversible join and concatMap</h1>
<code>String.join</code> and <code>List.concatMap_</code> are two functions making use of <code>List.foldl2</code>. It allows to deal with insertion and deletions of elements in the original list.
<h2><code>String.join</code></h2>
<span><code style="font-size:1.5em">join_ @(toString list) =<br>@highlight(color1)(toString<|String.join "" list)</code><br>
Try the following on @highlight(color1)("""the result above in @color1""") to see of join is cleverly thought::
<ul>
<li>Insert 1 to the left of 2 - it first snaps with numbers</li>
<li>Insert 1 to the right of 2 - if first snaps with empty string, then number</li>
<li>Insert 'a' without quotes to the right of 2</li>
<li>Delete 'big'</li>
<li>Delete 'big w'</li>
<li>Delete ' world'</li>
<li>Delete 'g world'</li>
<li>Replace 'Hello2' by 'Hi'</li>
</ul>
</span>
<h2><code>List.concatMap_</code></h2>
<span>
<code style="font-size:1.5em">concatMap wrapOrId @(toString list2) =<br> @highlight(color2)(toString (List.concatMap_ (\x -> [x]) (\[head] as headList -> case head of
  [] -> head
  _ :: _ -> head
  _ -> headList) list2))</code><br>

Try the following on @highlight(color2)("""the result above in @color2"""):
<ul>
<li>Remove the elements 2, 3</li>
<li>Insert an element, e.g. 8, right before 3</li>
<li>Insert an element, e.g. 8, right after 3</li>
<li>Remove 4, 5</li>
<li>Remove all elements, and then insert one element</li>
</ul>
</span>
</div>