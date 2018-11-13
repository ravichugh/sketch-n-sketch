load root path = 
  let loadraw path = 
    @(nodejs.fileread path
    |> Maybe.map (\x ->
       __evaluate__ ([("root", root), ("load", load root)] ++ __CurrentEnv__) x
       |> case of Ok x -> x; Err msg -> <error>@msg</error>)
    |> Maybe.withDefaultLazy (\_ -> <error>file @path not found</error>))
  in
  loadraw path

loadEnv = __CurrentEnv__

handleposts kind =
  let posttemplate = nodejs.fileread "src/@kind/post-template.src.html" |> Maybe.withDefault """<html><body>@kind/post-template.src.html not found</body></html>"""
  in
  nodejs.listdircontent """src/@kind/posts"""
  |> List.map (\(filename, filecontent) ->
    let finalname = Regex.extract """^(.*)\.md$""" filename
      |> Maybe.map (\[name] -> name + ".html")
      |> Maybe.withDefaultLazy (\() -> """Filename @filename not a *.md""")
    in
    __evaluate__ (("content", filecontent)::loadEnv) posttemplate
    |> (case of Ok x -> x; Err msg -> <html><body>Error: @msg</body></html>)
    |> (,) """../@kind/@finalname"""
    )
  )
  
blogposts = handleposts "blog"
tutorialposts = handleposts "tutorial"

expandSkeleton root file outtarget =
  (outtarget, load root file)

main = blogposts ++ tutorialposts ++
[ expandSkeleton "."   "src/index.src.html"                             "../index.html"
, expandSkeleton ".."  "src/releases/index.src.html"                    "../releases/index.html"
, expandSkeleton ".."  "src/blog/index.src.html"                        "../blog/index.html"
, expandSkeleton ".."  "src/tutorial/index.src.html"                    "../tutorial/index.html"
, expandSkeleton "../.." "src/tutorial/icfp-2018/index.src.html" "../tutorial/icfp-2018/index.html"]

main