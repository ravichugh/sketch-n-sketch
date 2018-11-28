initEnv = __CurrentEnv__

load root path = 
  let loadraw path = 
        nodejs.fileread path
        |> Maybe.map (\x ->
           __evaluate__ ([("root", root), ("load", load root)] ++ initEnv) (Regex.replace """<!--[\s\S]*?-->""" (\m -> freeze "") x)
           |> case of Ok x -> x; Err msg -> <error>@msg</error>)
        |> Maybe.withDefaultLazy (\_ -> <error>file @path not found</error>)
  in
  loadraw path

q3 = "\"\"\""
  
handleposts root kind =
  let posttemplate = nodejs.fileread """src/@kind/post-template.src.html""" |>
       Maybe.withDefault """<error>src/@kind/post-template.src.html not found</error>"""
  in
  nodejs.listdircontent """src/@kind/posts"""
  |> List.map (\(filename, filecontent) -> -- let _ = Debug.log """@filename""" () in
    let finalname = Regex.extract """^(.*)\.md$""" filename
         |> Maybe.map (\[name] -> name + ".html")
         |> Maybe.withDefaultLazy (\() -> """Filename @filename not a *.md""")
    in
    let finalcontent = __evaluate__ (("root", root) :: initEnv) (Update.expressionFreeze """@q3@filecontent@q3""") |>
      (case of Ok x -> String.markdown x; Err msg -> """<error>@msg</error>""") |>
      (\x -> __evaluate__ initEnv """<span>@x</span>""") |> 
      (case of Ok x -> x; Err msg -> <error>@msg</error>)
      -- Html.parse -- Should I do 
    in
    __evaluate__ (("content", finalcontent)::("root", root)::("load", load root)::initEnv) posttemplate
    |> (case of Ok x -> x; Err msg -> <error>Error: @msg</error>) |> (\x -> let _ = Debug.log "writing" """../@kind/@finalname""" in x)
    |> ((,) """../@kind/@finalname""")
    )

expandSkeleton root file outtarget =
  (Debug.log "writing " outtarget, load root file)

toWriteVal =
  (handleposts ".." "blog") ++
  (handleposts ".." "tutorial")  ++ [
  expandSkeleton "."   "src/index.src.html"                             "../index.html"
, expandSkeleton ".."  "src/releases/index.src.html"                    "../releases/index.html"
, expandSkeleton ".."  "src/blog/index.src.html"                        "../blog/index.html"
, expandSkeleton ".."  "src/tutorial/index.src.html"                    "../tutorial/index.html"
, expandSkeleton "../.." "src/tutorial/icfp-2018/index.src.html" "../tutorial/icfp-2018/index.html"
]

_ = toWriteVal |> List.map (\(name, content) -> 
  let aux node = case node of
    ["error", [], [["TEXT", msg]]] -> """
@name: @msg"""
    [tag, attrs, children] -> List.map aux children |> String.join ""
    _ -> ""
  in aux content) |> String.join "" |> (\x -> if x /= "" then Debug.log """Warning:@x""" () else ())

toWriteRaw = toWriteVal |> List.map (\(name, content) -> (name, valToHTMLSource content))