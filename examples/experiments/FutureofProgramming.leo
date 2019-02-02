preludeEnv = __CurrentEnv__

{- @render selector elem = case __closure_unapply__ selector of
  (["implicitvar"], body) -> case Regex.extract """^\s*implicitvar._(\d)\s*$""" of
    Just [column] -> "containing " + elem + " in column " + column
    _ -> "that " + toString selector + " equals " + elem
-}
{- @paraphrase = \table elem selector ->
       """Lookup row of @table @(render selector elem)"""
 -}
lookup table element selector = case table of
  [] -> Nothing
  row :: otherRows -> if selector row == element then Just row else lookup otherRows element selector

commands = [
  ("Abstract", "ALT+A", "concrete elements being selected"),
  ("Concretize", "ALT+C", "abstract elements being selected")
  ]

command =
  let commands = List.map (\(a, b, c) -> (String.toLowerCase a, b, c)) commands in
  \arg ->
  Update.freezeExcept (\d ->  """command @d""") arg <| \arg ->
  <abbr class="command" title=@(lookup commands (String.toLowerCase arg) ._1 |> Maybe.map (\x -> x._2 + ", " + x._3) |> Maybe.withDefault "unknown command")>@arg</abbr>

assignment varname params content =
  <div class="assignment">
    <span class="variable">@varname</span>@(if params == [] then [] else
      [["TEXT", " of "], <div class="params">@(List.map (\p -> <span class="variable">@p</span>) params)</div>]) =
    <div class="content">@content</div>
  </div>

htmlPostMap f n = case n of
  [_, _] -> n
  [t, attrs, children] -> f [t, attrs, List.map (htmlPostMap f) children]

htmlPreMap: Env -> (Env -> HtmlNode -> (Env, HtmlNode)) -> HtmlNode -> HtmlNode
htmlPreMap env f n = case f env n of
  (_, [_, _]) -> n
  (envp, [t, attrs, children]) ->
    [t, attrs, List.map (htmlPreMap envp f) children]

Html = { Html |
  addClass name [t, attrs, c] =
    [t, 
    attrDict.update "class" (case of
      Just classes -> if Regex.matchIn ("\\b" + name + "\\b") classes then Just classes else Just (classes + " " + name)
      Nothing -> Just name) attrs
    , c]
  addAttr name value [t, attrs, c] =
    [t, attrDict.insert name value attrs, c]
}

classFromProvenance provenance =
  if provenance == Abstract then "abstraction" else "concretization"

markabstract provenance name =
  Html.addClass (classFromProvenance provenance) >> Html.addAttr "title" name

mkAssignments context params args =
  List.map2 (\pat a -> case (pat, a) of
      (["span", ["class", "variable"]::_, [["TEXT", p]]], _) ->
        [(p, (context, ([], a)))]
      (["span", ["class", "tuple"]::_, patChildren],
       ["span", ["class", "tuple"]::_, argChildren]) ->
        List.map2 (mkAssignments context) patChildren argChildren
        |> List.concatMap identity
      _ -> []
    ) params args
  |> List.concatMap identity

repeatDef mapping listElem =
  case listElem of
    ["span", ["class", "table"]::_, children] ->
      <span class="repeat">@(List.map mapping children)</span>
    _ ->
      <span class="error">Not a valid table @listElem</span>

-- TODO: Find out why it's not working.
abstractAssignment (name, (context, (params, content))) =
  (,) name <|
  let aux env params = case params of 
        [] -> 
          -- TODO: Now replace the content with the env (replacements)
          if env == [] then content else
          htmlPostMap (case of
            ["span", ["class", "variable"]::_, [["TEXT", name]]] ->
              listDict.get env name
              |> Maybe.withDefault <span class="variable notfound">@name</span>
          ) content
        p :: pTail ->
          \element ->
            let remaining =
                 mkAssignments context [p] [element]
                 |> List.map (\(n, (_, (_, c))) -> (n, c))
            in
            aux (remaining ++ env) pTail
  in aux [] params
          

mkDoc (assignments) body =
  <div class="docwrapper">
  <div class="docblock doc">
  <div class="preamble">@(
    List.map (\(name, (params, content)) -> assignment name params content) assignments
  )</div>
  @(htmlPreMap (Concrete, List.map (\(name, p) -> (name, (Abstract, p))) assignments)
    (\(context, assignments) n ->
      case n of
      ["span", [["class","variable"]], [["TEXT", name]]] as v ->
        listDict.get name assignments
        |> Maybe.map (\(provenance, (params, content)) ->
          (,) (provenance, assignments) <|
          if params == [] then
            markabstract provenance name content
          else
            markabstract provenance name content |> Html.addClass "partial"
          )
        |> Maybe.withDefault ((context, assignments), ["span",
             [["class","variable notfound"]], [["TEXT", name]]])
      ["span", [["class", "application"]],
          ["span", [["class","variable"]], [["TEXT", name]]] :: args] ->
        listDict.get name assignments
        |> Maybe.map (\(provenance, (params, content))  ->
          let newAssignments = mkAssignments context params args ++
                  assignments
          in
          ((provenance, newAssignments), markabstract provenance name content)
        )
        |> Maybe.withDefault ((context, assignments), ["span", [["class","variable notfound"]], [["TEXT", name]]])
      ["span", [["class", "formula"]], [["TEXT", formula]]] ->
        (,) (context, assignments) <|
        case __evaluate__ (("repeat", repeatDef) :: (List.map abstractAssignment assignments) ++ preludeEnv) formula of
          Err msg -> <span class="error">@msg</span>
          Ok v -> <span class=@(classFromProvenance context) title=@(formula)>@v</span>
      _ -> ((context, assignments), n)
   ) body)
  </div></div>

<html><head><meta content="text/html; charset=UTF-8" http-equiv="content-type"><style>
span.error {
  display: inline-block;
  max-height: 10em;
  overflow-y: scroll;
}
span.table {
  display: table;
  border-collapse: separate;
}
span.tuple {
  display: table-row;
}
span.tuple:not(:first-child) {
  border-top: 1px solid black;
}
span.tuple > * {
  display: table-cell;
  padding-left: 2px;
  padding-right: 2px;
  border-color: gray;
  border-style: none;
  border-bottom-left-radius: 10px;
  border-bottom-right-radius: 10px;
  border-bottom: 1px solid gray;
}
.doc {
  font-family: Calibri, serif;
  font-size: 16px;
}
.preamble:not(:empty) {
  border-bottom: 1px solid gray;
  padding-bottom: 2px;
}
.assignment {
}
.assignment > .content {
  display: inline-block;
  border: 1px solid #DDD;
  vertical-align: middle;
  text-align: center;
  min-height: 2em;
  min-width: 2em;
  padding: 2px;
}
div.params {
  display: inline-block;
  vertical-align: middle;
}
div.params > span.variable {
  display: block;
}
.assignment > .variable {
}
.variable {
  font-family: monospace;
  color: blue;
  font-weight: bold;
}
.abstraction {
  color: rgba(0, 0, 0, 0.5);
}
.concretization {
  color: black;
}
.docblock {
  padding-left: 20px;
  padding-top: 20px;
  padding-bottom: 5px;
  padding-right: 10px;
  display: block;
  border-style: solid;
  border-color: transparent;
  border-width: 4px;
  background-color: #dadce0;
  border-radius: 8px;
  box-shadow: none;
  background-color: #FFF;
  background-clip: padding-box;
  border: solid transparent;
  border-width: 1px 1px 1px 6px;
  min-height: 28px;
  box-shadow: inset 2px 2px 0 rgba(0,0,0,.2), inset 0 -2px 0 rgba(0,0,0,.07);
}
.doc h1 {
  color: rgb(46, 116, 181);
  font-size: 1.3em;
}
.docwrapper {
  display: block;
  padding-left: 1em;
  padding-top: 1em;
  background: #f8f9fa;
  margin-left: 2em;
  margin-bottom: 1em;
}
.command {
  outline: 1px solid #CCC;
  background: #EEE;
}
body > div {
  margin-left: auto;
  margin-right: auto;
  max-width: 600px;
  overflow-x: hidden;
}
body a {
  color: inherit;
  cursor: pointer;
}
:root {
  font-family: medium-content-serif-font,Georgia,Cambria,"Times New Roman",Times,serif;
  letter-spacing: .01rem;
  font-weight: 400;
  font-style: normal;
  font-size: 21px;
  line-height: 1.58;
  letter-spacing: -.003em;
}
</style></head><body><div>

<h1>Abstract and Concrete: Future of Programming</h1>

<p>I recently read a <a href="https://www.google.com/url?q=https://hbr.org/2019/01/what-will-software-look-like-once-anyone-can-create-it&amp;sa=D&amp;ust=1549033963248000">very interesting article</a> that highlighted that, until the invention of arabic numbers, multiplication using roman numerals was difficult. The arabic positional number system made it much easier. It was the same for software. Programming software is difficult. Until someone comes up with the equivalent of arabic numbers for programming, it will remain so.</p><p

>This made me think profoundly. What makes it that programming software is difficult? Is it that spreadsheets are cryptic? Too many <abbr title="Application Programming Interface">API</abbr>s? Complicated build tools? Not close enough to assembly language? Or to far away?</p>

<p>I am a big fan of the idea of <a href="https://www.google.com/url?q=https://coda.io/welcome&amp;sa=D&amp;ust=1549033963249000">Coda</a>&nbsp;that mixes spreadsheets with word-like documents and makes it possible to create text that appears based on conditions, for example.<br
>However, it does not feel like I have access to the full power of a programming language. The language for computing cells is a domain-specific language with tons of operators, but there is no way to create reusable abstractions yet. If it appears some day, it is likely to copy Google Spreadsheet by offering <a href="https://developers.google.com/apps-script/guides/sheets/functions">abstractions as macros</a>&nbsp;that are no longer part of the visible document, and cannot be edited with the same and nice visual interface.</p>

<p>Programming is all about abstraction. Indeed, <a href="https://www.google.com/url?q=https://www.youtube.com/watch?v%3DV10hzjgoklA&amp;sa=D&amp;ust=1549033963250000">Lambda Calculus</a>&nbsp;is being discovered as the ancestor of all modern programming languages and is the same as logic itself. Moreover, Lambda Calculus features only two building blocks: abstraction and concretization. Isn&rsquo;t all software based on this idea? Starting concrete, and then abstracting away parts that become data inputs, or starting abstract, and then test using concrete values?</p>

<p>I recently created <a href= "https://github.com/MikaelMayer/Editor">Editor</a>, a local HTTP server to edit dynamic and static webpages in the browser, and have modifications back-propagated to the files themselves. It gave me the idea for this article.</p>

<p>In the remaining of this article, I will sketch an imaginary tutorial for a word processor of the future that is for software the equivalent of arabic numbers for numbers. It emphasizes the power of the Abstract and Concrete building blocks.</p>

<h2>MikoWord Tutorial</h2>

<p>Welcome to MikoWord!</p>

<p>MikoWord is a word processor for webpages which will satisfy both programmers and non-programmers. MikoWord is built on the idea of two major actions: Abstract and Concrete. In this tutorial aimed at beginners, we will explain how to combine these powerful notions by walking through an example. We will create a rich and dynamic help page for an application named MySoft. In this tutorial, you will learn:</p>
<ul><li
>How to clone relevant pieces of information (e.g. the name of a product)</li><li
>How to create reusable abstractions (e.g. a formatting style)</li><li
>How to repeat abstractions (e.g. introducing a list)</li><li
>How to use abstractions and customize them.</li></ul>
</p>

<p>No coding skills are required. Let’s start!</p>

<h3>Step 1: Simple abstraction</h3>

<p>
MikoWord displays a blank page, in which you can immediately write something.
Go ahead, write and apply styles to obtain:
</p>
<img style="margin-left:2em" src="FutureofProgramming-ribbon.png" alt="FutureofProgramming-ribbon.png">
@(mkDoc [] 
<div>
<h1>MySoft 1.0 Tutorial</h1>
Welcome to MySoft 1.0!
</div>)
<p>
In a regular document, every time you update MySoft’s version, you would have to update it manually everywhere.
In MikoWord, it is easier. Select <span class="doc">MySoft 1.0</span>, and @(command "abstract") it.
</p>
Abstracting is giving a name to something concrete.
If you hover over the now greyed <span class="doc">MySoft 1.0</span> in the title, you will now see the name this content has been automatically given (mysoft). The content is a mini-document in itself, but we'll see how to benefit from this feature later.
@(mkDoc [("mysoft", ([], <span class="textvalue">MySoft 1.0</span>))]
<div>
<h1><span class="variable">mysoft</span> Tutorial</h1>
Welcome to MySoft 1.0!
</div>)
<p>
Don't worry, all declarations like <span class="variable">mysoft</span> will be hidden from the final document's version.
Now copy <span class="doc abstraction">MySoft 1.0</span> from the title, and paste it to replace <span class="doc">MySoft 1.0</span> in the document body. You obtain something like:</p>
@(mkDoc [("mysoft", ([], <span class="textvalue">MySoft 1.0</span>))] 
<div>
<h1><span class="variable">mysoft</span> Tutorial</h1>
Welcome to <span class="variable">mysoft</span>!
</div>)
<p>
Rename <span class="doc">1.0</span> to <span class="doc">1.1</span>. You can rename it in any of the three places, and you'll obtain:
</p>
@(mkDoc [("mysoft", ([], <span class="textvalue">MySoft 1.1</span>))]
<div>
<h1><span class="variable">mysoft</span> Tutorial</h1>
Welcome to <span class="variable">mysoft</span>!
</div>)
Thanks to automatic correction and from now on, each time you will type mysoft followed by a space, it will replace it with <span class="doc abstraction">MySoft 1.1</span>. Don't worry about the fact that these occurrences are in gray, they will revert to their initial color once you export the document. Alternatively, you can go to settings and change the display of reusable variables.
<h3>Step 2: Abstract to concrete</h3>
<p>Let us imagine that, in your document, you want to describe a set of keyboard shortcuts as memos, and then some sample scenarios on how to use them.
Let us start by creating a sample shortcut table. Create a 3x2 table, remove the borders inside it, and fill it as follow:</p>

<style>
table.kbd {
  border-collapse:collapse;
  border-left:1px solid black;
  border-right:1px solid black;
  margin-top: 2px;
  border-radius: 5px;
}
table.kbd td {
  padding: 5px;
}
table.kbd tr:first-child {
  border-top:1px solid black;
}
table.kbd tr:last-child {
  border-bottom:1px solid black;
}
</style>
@(mkDoc []
<div>
  <table class="kbd"><tbody>
  <tr><td>Command name:</td><td>Context menu</td></tr>
  <tr><td>Keyboard shortcut:</td><td>Right-click or ALT+M</td></tr>
  <tr><td>When to use:</td><td>When caret blinks in doc</td></tr>
  </tbody>
  </table>
</div>)

We want to reuse this box. Instead of just copy-pasting it, we will abstract it so that boxes are in sync. Select the table, @(command "abstract") it, and duplicate the abstraction as in the previous section.

@(mkDoc [("table", ([], <table class="kbd"><tbody>
  <tr><td>Command name:</td><td>Context menu</td></tr>
  <tr><td>Keyboard shortcut:</td><td>Right-click or ALT+M</td></tr>
  <tr><td>When to use:</td><td>When caret blinks in doc</td></tr>
  </tbody>
  </table>))]
<div>
  <span class="variable">table</span>
  <span class="variable">table</span>
</div>)

Now if you changed one of the values in any table, it would be changed anywhere. We need to specify what values we should be able to change independently. Select <span class="doc">Context menu</span> in one of the last two tables and @(command "concretize") it. Alternatively, you could have selected it from the first table and @(command "abstract")ed it. The result looks like follows:

@(mkDoc [("table", ([<span class="variable">contextmenu</span>], <table class="kbd"><tbody>
  <tr><td>Command name:</td><td><span class="variable">contextmenu</span></td></tr>
  <tr><td>Keyboard shortcut:</td><td>Right-click or ALT+M</td></tr>
  <tr><td>When to use:</td><td>When caret blinks in doc</td></tr>
  </tbody>
  </table>))]
<div>
  <span class="application"><span class="variable">table</span><span>Context menu</span></span>
  <span class="application"><span class="variable">table</span><span>Context menu</span></span>
</div>)

Now if you rename the "Context Menu" from the last table to "Save", the change will remain on this table only:

@(mkDoc [("table", ([<span class="variable">contextmenu</span>], <table class="kbd"><tbody>
  <tr><td>Command name:</td><td><span class="variable">contextmenu</span></td></tr>
  <tr><td>Keyboard shortcut:</td><td>Right-click or ALT+M</td></tr>
  <tr><td>When to use:</td><td>When caret blinks in doc</td></tr>
  </tbody>
  </table>))]
<div>
  <span class="application"><span class="variable">table</span><span>Context menu</span></span>
  <span class="application"><span class="variable">table</span><span>Save</span></span>
</div>)

Continue and @(command "concretize") the remaining two arguments, and rename them appropriately so that you should see something like:

@(mkDoc [("table", ([<span class="variable">contextmenu</span>, <span class="variable">right</span>, <span class="variable">when</span>], <table class="kbd"><tbody>
  <tr><td>Command name:</td><td><span class="variable">contextmenu</span></td></tr>
  <tr><td>Keyboard shortcut:</td><td><span class="variable">right</span></td></tr>
  <tr><td>When to use:</td><td><span class="variable">when</span></td></tr>
  </tbody>
  </table>))]
<div>
  <span class="application"><span class="variable">table</span><span
    >Context menu</span><span
    >Right-click or ALT+M</span><span
    >When caret blinks in doc</span></span>
  <span class="application"><span class="variable">table</span><span
    >Save</span><span
    >CTRL+S</span><span
    >When file is not saved yet</span></span>
</div>)
Using this interface, if you replace "Keyboard shortcut" by "Shortcut", it will be updated everywhere. That can be very useful.
Save your file. If you forget to save your file and your browser unexpectedly quits, a backup will always be available.
<h3>Step 3: Advanced abstractions</h3>
<p>In the previous section, we saw how to define memos for shortcuts. Now, we want to use these shortcut definitions to augment simple tutorials, by displaying a hint on every command that a tutorial uses. That way, users would not have to remember all these commands or look them up themselves.</p>

<p>Select the last two tables, and @(command "abstract") them.
Instead of inserting one more name, MikoWord detects that there is a repetition and instead suggest to merge these calls. Accept. You will then see the following result:
</p>
@(mkDoc [("items", ([], <span class="table"><span class="tuple"><span
    >Context menu</span><span
    >Right-click or ALT+M</span><span
    >When caret blinks in doc</span></span><span class="tuple"
><span
    >Save</span><span
    >CTRL+S</span><span
    >When file is not saved yet</span></span></span>
)),
("table", ([<span class="tuple"><span class="variable">contextmenu</span><span class="variable">right</span><span class="variable">when</span></span>], <table class="kbd"><tbody>
  <tr><td>Command name:</td><td><span class="variable">contextmenu</span></td></tr>
  <tr><td>Keyboard shortcut:</td><td><span class="variable">right</span></td></tr>
  <tr><td>When to use:</td><td><span class="variable">when</span></td></tr>
  </tbody>
  </table>))]
<div>
  <span class="formula">repeat table items</span>
</div>)



<h3>Step 4: Custom abstractions</h3>

</div>
</body></html>