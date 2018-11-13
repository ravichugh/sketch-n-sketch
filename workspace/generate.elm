nodejs = { nodejs |
  listdir: String -> List String
  listdir foldername = __jsEval__ """
        (function() {
          const fs = require("fs");
          const path = require('path');
          var filesfolders =
            fs.readdirSync(@(jsCode.stringOf foldername));
          return filesfolders.filter(filename => {
             var filename = @(jsCode.stringOf foldername) + "/" + filename; 
             var stat = fs.lstatSync(filename);
             return !stat.isDirectory() }
          )
        })()
      """
    
  listdircontent foldername = listdir foldername |>
    List.map (\name -> (name, nodejs.fileread name |> Maybe.withDefault (freeze """Unknown file @name""")))
}

load root path = 
  let loadraw path = 
        nodejs.fileread path
        |> Maybe.map (\x ->
           __evaluate__ ([("root", root), ("load", load root)] ++ __CurrentEnv__) x
           |> case of Ok x -> x; Err msg -> <error>@msg</error>)
        |> Maybe.withDefaultLazy (\_ -> <error>file @path not found</error>)
  in
  loadraw path

loadEnv = __CurrentEnv__

handleposts kind =
  let posttemplate = nodejs.fileread "src/@kind/post-template.src.html" |> Maybe.withDefault """<error>src/@kind/post-template.src.html not found</error>"""
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

main |> List.map (\(name, content) -> 
  case content of
    ["error", [], [["TEXT", msg]]] -> """@name: @msg"""
    _ -> """@name: ok""") |> String.join "\n"