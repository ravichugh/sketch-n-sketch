markdown =
  let fix phase node =
    let new_node = phase node in
    if new_node == node then node else fix phase new_node
  in
  let title_phase =
    Html.replace """(?:\r?\n|^)(#+)\s*(.*)(?=\r?\n)""" (\{submatches=[levelStr, title]} ->
    let level = String.length levelStr in
    [<@("""h@level""")>@title</@>])
  in
  let inline_link_phase =
    Html.replace """\[((?:(?!\]).)*)\]\(((?:(?!\)).)*)\)""" (\{submatches=[text,url]} ->
      [<a href=url>@text</a>]
    )
  in
  let external_link_phase node =
    Html.replace """\[((?:(?!\]).)*)\]\[((?:(?!\)).)*)\]""" (\{match,submatches=[text,key]} ->
      case Html.find """\[@key\]:\s*(.*)""" node of
        [] -> [["TEXT", match]]
        {submatches=url::_}::_ -> [<a href=url>@text</a>]) node
  in
  let remove_external_links_residuals_phase =
    Html.replace """\[\w+\]:\s*.*""" (\_ -> [])
  in
  let paragraph_phase =
    Html.replace """(?:(?:\r?\n){2,}|  \r?\n""" (\_ ->
      [<br>])
  in
  --let emph_phase =
  --  Html.replace """(__|\*\*)
  \node -> node
  |> title_phase
  |> paragraph_phase
  |> inline_link_phase
  |> external_link_phase
  |> remove_external_links_residuals_phase

Html.forceRefresh <| (freeze markdown) <span>
## Hello world

This is a paragraph leading to [my website](http://mikaelmayer.com).  
You can also have [links][1] whose URL is defined afterwards

[1]: http://www.google.ch
</span>