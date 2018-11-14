initEnv = __CurrentEnv__

markdown node =
    let
        regexFootnotes = """\r?\n\[\^([^\]]+)\]:\s*((?:(?!\r?\n\r?\n)[\s\S])+)"""
        regexReferences = """\r?\n\[(?!\^)([^\]]+)\]:\s*(\S+)"""
        footnotes = Html.find regexFootnotes node
                     |> List.map (\m -> (nth m.group 1, nth m.group 2))
                     |> List.indexedMap (\i (name, value) -> (name, (i + 1, value)))
        references = Html.find regexReferences node
                     |> List.map (\m -> (nth m.group 1, nth m.group 2))
        notCode = case of ["code", _, _] -> False; _ -> True
        notTitle = case of [tag, _, _] -> not (Regex.matchIn """h\d""" tag); _ -> True
        notList = case of [tag, _, _] -> tag /= "ul" && tag /= "ol"; _ -> True
        notPara = case of ["p", _, _] -> False; _ -> True
        r: String -> (Match -> List HtmlNode) -> HtmlNode -> HtmlNode
        r = Html.replaceIf notCode
        r2 = Html.replaceIf (\x -> notCode x && notTitle x && notList x && notPara x)
        lregex = """(?:\r?\n|^)((?:(?![\r\n])\s)*)(\*|-|\d+\.)(\s+)((?:.*)(?:\r?\n\1  ?\3(?:.*))*(?:\r?\n\1(?:\*|-|\d+\.)\3(?:.*)(?:\r?\n\1 \3(?:.*))*)*)"""
        handleLists node  =
          r lregex (
            \m -> let indent = nth m.group 1
                      afterindent = nth m.group 3
                      ul_ol = case nth m.group 2 of "*" -> "ul"; "-" -> "ul"; _ -> "ol"
                      elements = 
                        Regex.split """\r?\n@indent(?:\*|-|\d+\.)@afterindent""" (nth m.group 4)
                  in
                  [<@ul_ol>@(List.map (\elem -> handleLists <li>@elem</li>) elements)</@>]) node
    in (
    node
    |> r """@regexReferences|@regexFootnotes""" (\m -> [])
    |> (\result -> -- Expand footnotes
      if List.length footnotes == 0 then result
      else case result of
        [tag, attrs, children] ->
          [tag, attrs, children ++ Update.sizeFreeze [
            <div class="footnotes"><hr><ol>@(footnotes |>
              List.map (\(name, (n, value)) -> 
                <li id="""fn@n"""><p>@value<a href="""#fnref@n""">↩</a></p></li>
              ))</ol></div>]
          ])
    |> r "(```)([\\s\\S]*?)\\1(?!`)" (\m -> [<pre><code>@(nth m.group 2 |> String.trim)</code></pre>])
    |> r """(^|\r?\n)(#+)\s*([^\r\n]*)""" (\m -> [<@("""h@(String.length (nth m.group 2))""")>@(nth m.group 3)</@>])
    |> handleLists
    |> r2 "(\r?\n\r?\n(?:\\\\noindent\r?\n)?|^)((?=\\s*\\w|\\S)[\\s\\S]*?)(?=\r?\n\r?\n|\r?\n$|$)" (\m -> [<p>@(nth m.group 2)</p>])
    |> r """\[([^\]]+)\](\^?)(\(|\[)([^\)\]]+)(\)|\])""" (\m -> [
      case nth m.group 3 of
        "(" -> <a href=(nth m.group 4) @(if nth m.group 2 == "^" then [["target", "_blank"]] else [])>@(nth m.group 1)</a>
        _ -> listDict.get (nth m.group 4) references |> case of
              Just link -> <a href=link>@(nth m.group 1)</a>
              Nothing -> ["TEXT", m.match]
        ])
    |> r """\[\^([^\]]+)\]""" (\m -> 
      listDict.get (nth m.group 1) footnotes |> case of
        Just (n, key) -> [ <a href="""#fn@n""" class="footnoteRef" id="""fnref@n"""><sup>@n</sup></a>]
        Nothing -> [["TEXT", m.match]])
    |> r "(`)(?=[^\\s`])(.*?)\\1" (\m -> [<code>@(nth m.group 2)</code>])
    |> r """(\*{1,3}|_{1,3})(?=[^\s\*_])((?:(?!\\\*|\_).)*?)\1""" (\m -> [
      case nth m.group 1 |> String.length of
        1 -> <em>@(nth m.group 2)</em>
        2 -> <strong>@(nth m.group 2)</strong>
        _ -> <em><strong>@(nth m.group 2)</strong></em>])
    |> r """&mdash;|\\\*|\\_""" (\m -> [["TEXT", case m.match of
      "&mdash;" -> "—"
      "\\*" -> String.drop 1 m.match
      "\\_" -> String.drop 1 m.match
      ]])
    )

load root path = 
  let loadraw path = 
        nodejs.fileread path
        |> Maybe.map (\x ->
           __evaluate__ ([("root", root), ("load", load root)] ++ initEnv) x
           |> case of Ok x -> x; Err msg -> <error>@msg</error>)
        |> Maybe.withDefaultLazy (\_ -> <error>file @path not found</error>)
  in
  loadraw path

handleposts root kind =
  let posttemplate = nodejs.fileread """src/@kind/post-template.src.html""" |> Maybe.withDefault """<error>src/@kind/post-template.src.html not found</error>"""
  in
  nodejs.listdircontent """src/@kind/posts"""
  |> List.map (\(filename, filecontent) ->
    let _ = Debug.log """@filename""" () in
    let finalname = Regex.extract """^(.*)\.md$""" filename
         |> Maybe.map (\[name] -> name + ".html")
         |> Maybe.withDefaultLazy (\() -> """Filename @filename not a *.md""")
    in
    let contentwithoutcomments = Regex.replace """<!--[\s\S]*?-->""" (\m -> freeze "") filecontent in
    let finalcontent = __evaluate__ (("root", root) :: initEnv) (Update.expressionFreeze """<span>@contentwithoutcomments</span>""") |>
      (case of Ok x -> markdown x; Err msg -> <error>@msg</error>)
    in
    __evaluate__ (("content", finalcontent)::("root", root)::("load", load root)::initEnv) posttemplate
    |> (case of Ok x -> x; Err msg -> <error>Error: @msg</error>)
    |> (,) """../@kind/@finalname"""
    )

expandSkeleton root file outtarget =
  (outtarget, load root file)

toWriteVal = {- (handleposts ".." "blog") ++ -} (handleposts ".." "tutorial") {- ++
[ expandSkeleton "."   "src/index.src.html"                             "../index.html"
, expandSkeleton ".."  "src/releases/index.src.html"                    "../releases/index.html"
, expandSkeleton ".."  "src/blog/index.src.html"                        "../blog/index.html"
, expandSkeleton ".."  "src/tutorial/index.src.html"                    "../tutorial/index.html"
, expandSkeleton "../.." "src/tutorial/icfp-2018/index.src.html" "../tutorial/icfp-2018/index.html"]
-}

_ = toWriteVal |> List.map (\(name, content) -> 
  let aux node = case node of
    ["error", [], [["TEXT", msg]]] -> """
@name: @msg"""
    [tag, attrs, children] -> List.map aux children |> String.join ""
    _ -> ""
  in aux content) |> String.join "" |> (\x -> if x /= "" then Debug.log "Warning:@x" () else ())

toWriteRaw = toWriteVal |> List.map (\(name, content) -> (name, valToHTMLSource content))