r0 = """\((?:(?!\(|\)).)*\)"""

next r = """\((?:(?!\(|\)).|@r)*\)"""

rr = List.foldl (always next) r0 <| List.range 1 10

rrExtract = """^([\s\S]*?)(@rr)([\s\S]*)$"""

noparens = Regex.matchIn """^[^\(\)]*$"""
hasclosing string = Regex.extract """^([^\)]*)\)([\s\S]*)$""" string
hasopening string = Regex.extract """^([^\(]*)\(([\s\S]*)$""" string

textParsed start string = (if string /= "" then Update.sizeFreeze [Text start string] else [])

unparse = case of
  Text _ s -> s
  Block _ inner -> "(" + (List.map unparse inner |> String.join "") + ")"

onParseInsert inserted = [List.map unparse inserted |> String.join "" |> Left 0]

parse offset string =
  if noparens string then
    textParsed offset string
  else
  findInterleavings 0 rr string
  |> List.concatMap_ (always []) (\[head] as headList -> case head of
    Left text start -> textParsed (offset + start) text
    Right {match, start = start::_} ->
      let inner = String.substring 1 (String.length match - 1) match in
      let start0 = start + 1 in
      Html.insertionDeletionUpdatesTo onParseInsert headList <|
        Block (offset + start) (parse (offset + start + 1) inner)
  )

<pre>@(toString <| parse 0 """ ) ( Hello (World) with ( an (immense) joy) ! ) unbalanced ( opening ( with good cheer) """)</pre> 