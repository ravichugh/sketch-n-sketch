(fs) = nodejs.delayed fileOperations
(initEnv) = __CurrentEnv__

-- The function loadWithRoot puts "load" into the environment of loaded files with the provided root
loadWithRoot: String -> String -> Result String HtmlElement
(loadWithRoot) root path = 
  fs.read path
  |> Result.fromMaybeLazy (\_ -> """@path not found""")
  |> Result.andThen (
     __evaluate__ (("root", root)::("load", loadWithRoot root)::initEnv))

(debugmsg) = if willwrite then "recomputing for writing" else "recomputing for update"
(dumpHtmlObject) x = Update.freeze "<!DOCTYPE html>\n" + valToHTMLSource x

(handleposts) root kind =
  fs.read """src/@kind/post-template.src.html""" |>
  |> Result.fromMaybeLazy (\_ -> [Error """src/@kind/post-template.src.html not found"""])
  |> Result.andThen (\posttemplate ->
    fs.listdircontent """src/@kind/posts"""
    |> List.map (\(filename, filecontent) ->
      let resFinalName = Regex.extract """^(.*)\.md$""" filename |>
            Result.fromMaybeLazy (\_ -> """Post file @filename not a *.md""")
            Result.map (\[name] -> name + ".html")
          resFinalContent = __evaluate__ (("root", root) :: initEnv) (
              Update.expressionFreeze """@String.q3@filecontent@String.q3""") |>
            Result.map (\x -> __evaluate__ initEnv """<span>@(String.markdown x)</span>""")
      in
      (flips.abc_bca Result.andThen2) resFinalName resFinalContent <|
      \finalname finalContent ->
      __evaluate__ (("content", finalContent)::("root", root)::("load", loadWithRoot root)::initEnv) posttemplate |>
      Result.map (\x ->      let _ = Debug.log debugmsg """../@kind/@finalname""" in
        Write """../@kind/@finalname""" (dumpHtmlObject x)) |>
      Result.withDefaultMapError (\msg ->
        Error """Could not write ../@kind/@finalname: @msg"""))
  ) |> Result.withDefaultMapError identity

(expandSkeleton) root file outtarget =
  Write (Debug.log debugmsg outtarget) (dumpHtmlObject <| loadWithRoot root file)

-- Tasks

blogposts = handleposts ".." "blog"

tutorials = handleposts ".." "tutorial"

indexes = [
  expandSkeleton "."   "src/index.src.html"                             "../index.html"
, expandSkeleton ".."  "src/releases/index.src.html"                    "../releases/index.html"
, expandSkeleton ".."  "src/blog/index.src.html"                        "../blog/index.html"
, expandSkeleton ".."  "src/tutorial/index.src.html"                    "../tutorial/index.html"
, expandSkeleton "../.." "src/tutorial/icfp-2018/index.src.html" "../tutorial/icfp-2018/index.html"
{-
, expandSkeleton "../.." "src/tutorial/icfp-2018/index.src.html" "../tutorial/icfp-2018/index.html"
, expandSkeleton "../.." "src/misc/pldi2016-artifact.src.html" "../releases/v0.4.1/pldi2016-artifact.html"
-}
]

all = blogposts ++ tutorials  ++ indexes