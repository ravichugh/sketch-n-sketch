type alias Space = String
type Exp = App Exp Exp | Lambda Space Space Ident Exp | Var Space Ident | Parens Space Exp Space

unparse = case of
  Var space ident -> space + ident
  App e1 e2 -> unparse e1 + unparse e2
  Lambda sp1 sp2 ident e1 -> sp1 + "λ" + sp2 + ident + "." + unparse e1
  Parens sp1 e1 sp2 -> sp1 + "(" + unparse e1 + sp2 + ")"

parse = let
  parseVar str = case Regex.extract """^(\s*)(\w+)([\s\S]*)$""" str of
    Just [sp, ident, remaining] -> Just (Var sp ident, remaining)
    _ -> Nothing
  parseLambda str = case Regex.extract """^(\s*)[λ\\](\s*)(\w+)\.([\s\S]*)$""" str of
    Just [sp1, sp2, ident, remaining] -> let (e1, r) = parseApp Nothing remaining in
      Just (Lambda sp1 sp2 ident e1, r)
    _ -> Nothing
  parseParens str = case Regex.extract """^(\s*)\(([\s\S]*)$""" str of
    Just [sp1, remaining] -> 
      let (parsed, r2) = parseApp Nothing remaining in
      case Regex.extract """^(\s*)\)([\s\S]*)$""" r2 of
        Just [sp2, remaining2] -> Just (Parens sp1 parsed sp2, remaining2)
        _ -> Nothing
    _ ->
      Nothing
  mbAppPrev mbPrevious x = case mbPrevious of
    Nothing -> x
    Just prev -> App prev x
  parseApp mbPrevious str = 
    parseVar str
    |> Maybe.withDefaultLazy (\_ ->
      parseParens str
      |> Maybe.withDefaultLazy (\_ ->
        parseLambda str
        |> Maybe.withDefault (End, str)
      )
    ) |> case of
     (End, remaining) -> case mbPrevious of
       Nothing -> (Lambda "" "" "z" (Var "" "z"), remaining)
       Just x -> (x, remaining)
     (headTerm, remaining) -> parseApp (Just (mbAppPrev mbPrevious headTerm)) remaining
  in
  parseApp Nothing >> Tuple.first

-- Text rendering
display = Update.lens {
  apply = unparse
  update {outputNew} = Ok (Inputs [parse outputNew])
}

-- HTML rendering
globalColors = ["#0CF", "blue", "green", "orange", "black", "brown", "purple"]

displayHtml = Update.lens {
  textOf = case of
    ["TEXT", s]::tail -> s + textOf tail
    [tag, attrs, children]::tail ->
      textOf children + textOf tail
    _ -> ""
  extractNewColor ident colorVars colors =
    case if colors == [] then globalColors else colors of
     head :: tail -> (head, Dict.insert ident head colorVars, tail)
  unparseHtml colorVars colors = case of
    Var space ident ->
      [["TEXT", space], <span class="var" style=(
         "color:"+(Dict.get ident colorVars |> Maybe.withDefault "black"))>@ident</span>]
    App e1 e2 -> unparseHtml colorVars colors e1 ++ unparseHtml colorVars colors e2
    Lambda sp1 sp2 ident e1 -> 
      let (thisColor, newColorVars, newColors) = extractNewColor ident colorVars colors in
      [["TEXT", sp1 + "λ" + sp2], <span class="var" style=("color:"+thisColor)>@ident</span>, ["TEXT", "."]] ++ unparseHtml newColorVars newColors e1
    Parens sp1 e1 sp2 ->
      let (thisColor, newColorVars, newColors) = extractNewColor "(" colorVars colors in
      [["TEXT", sp1], <span class="parens" style=("color:"+thisColor)>(</span>] ++ unparseHtml newColorVars newColors e1 ++ [["TEXT", sp2], <span class="parens" style=("color:"+thisColor)>)</span>]
  apply x = [<span class="topexp">@unparseHtml(Dict.fromList [])(globalColors)(x)</span>]
  update {outputNew} = Ok (Inputs [parse (textOf outputNew)])
}

-- Lambda calculus computation
replace x f e = case e of
  Var sp ident -> if ident == x then f else e
  App e1 e2 -> App (replace x f e1) (replace x f e2)
  Lambda sp1 sp2 ident e1 -> if ident == x then e else Lambda sp1 sp2 ident (replace x f e1)
  Parens sp1 e1 sp2 -> Parens sp1 (replace x f e1) sp2

unwrapParens e1 = case e1 of
  Parens _ e _ -> e
  _ -> e1

-- Smallsteps semantics
smallstep e = case e of
  App e1 e2 -> 
    let e2next = smallstep e2 in
    if e2next == e2 then
      let e1next = smallstep e1 in
      if e1next == e1 then
        case unwrapParens e1next of
          Lambda sp1 sp2 ident e3 -> replace ident e2 e3
          _ -> App e1 e2
      else App e1next e2
    else App e1 e2next
  Lambda sp1 sp2 ident e1 -> Lambda sp1 sp2 ident (smallstep e1)
  Var sp x -> Var sp x
  Parens sp1 e1 sp2 -> Parens sp1 (smallstep e1) sp2

-- Displays the computation until a certain limit
compute limit termStr =
  let aux n term = if n == 0 then "" else " → " + unparse term + aux (n - 1) (smallstep term) in
  termStr + aux (limit - 1) (smallstep (parse termStr))

-- Displays the computation in HTML until a certain limit
computeHtml limit termStr =
  let aux first n term =
    if n == 0 then [] else (if first then [] else [["TEXT", " →"], ["br", [], []]]) ++
      displayHtml term ++ aux False (n - 1) (smallstep term)
  in aux True limit (parse termStr)

globallimit = 5

<span style="display:inline-block;margin:20px"><h1>Interactive Lambda calculus</h1>
You can test out various computations by changing the first line of this computation, and then selecting an update.
It currently performs @globallimit small computation steps.<br><br>
<span>@(computeHtml globallimit """((\x. \select. select x x) (\f. f)) (\fst. \snd. fst)""")</span>
<br>
<br>
Try to change the following as well:
<ul>
<li>The result of the computation</li>
<li>The syntax highlight colors</li>
<li>The arrow symbol</li>
<li>The font used for calculus</li>
<li>In the program, stop the computation if it does not progress</li>
</ul>
</span>