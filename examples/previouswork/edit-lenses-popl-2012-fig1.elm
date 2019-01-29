-- From the paper "Edit lenses" (POPL 2012)
-- Their supported scenario is the following
-- * Add a line Monteverdi, 1567-1643 at the end of the first list
-- * Correct the country to "Italy" for Monteverdi on the second list
--   Before hitting update, correct Shumann => Schumann on the second lsit
-- * Either
--   * Delete the Monteverdi line (first list) and reinsert it above
--     The country will miss in the second list
--   * (not supported in our system) reorder the elements.


default = {name="?name?", dates="?dates?", country="?country?"}

data = [
  {name="Schubert", dates="1797-1828", country="Austria"},
  {name="Shumann", dates="1810-1856", country="Germany"}]

view1 = mapWithDefault default (
  \{name, dates} ->   <li>@name, @dates</li>) data

view2 = mapWithDefault default (
  \{name, country} -> <li>@name, @country</li>) data

Html.forceRefresh <|
Update.freezeExcept (always "template") [view1, view2] <| \[view1, view2] -> <div>
  <ul>@view1</ul>
  <ul>@view2</ul>
</div>