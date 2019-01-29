-- From the paper "Edit lenses" (POPL 2012), figure 2.
-- Their edition scenario:
-- * Insert Salinger between Kerouac and Tolstoy in the second column
-- This inserts Inr Salinger in the original list.
-- However, had they inserted it between Schumann and Beethoven, there would have been an ambiguity that is very well captured by our approach.
-- Furthermore, they require to store the complement, which is built-in in our approach.

tagged = [
  Inl "Schumann",
  Inr "Kerouac",
  Inr "Tolstoy",
  Inl "Beethoven"]

view1 = List.filter (case of
  Inl _ -> True; _ -> False) tagged |> List.map (\Inl x -> <li>@x</li>)

view2 = List.filter (case of
  Inr _ -> True; _ -> False) tagged |> List.map (\Inr x -> <li>@x</li>)
  
Html.forceRefresh <|
Update.freezeExcept (always "template") [view1, view2] <| \[view1, view2] -> <div>
  <ul style="display:inline-block">@view1</ul>
  <ul style="display:inline-block">@view2</ul>
</div>