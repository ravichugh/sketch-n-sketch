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
  ("Concretize", "ALT+C", "abstract elements being selected"),
  ("Remark", "CTRL+R", "any element being selected"),
  ("Context menu", "Right-click or ALT+M", "anywhere")
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
        mkAssignments context patChildren argChildren
      _ -> []
    ) params args
  |> List.concatMap identity

repeatDef(mapping, listElem) =
  case listElem of
    ["span", ["class", "table"]::_, children] ->
      <span class="repeat">@(List.map mapping children)</span>
    _ ->
      <span class="error">Not a valid table @listElem</span>

tableDict = {
    -- like listDict.get but for any row
  find predicate = Update.lens {
    apply list = case list of
      [] -> Nothing
      row :: tail -> if predicate row then Just row else apply tail
    update = case of
      {input, outputOld = Nothing, outputNew = Just newRow} ->
        Ok (Inputs [insert predicate newRow input])
      {input, outputOld = Just _, outputNew = Nothing} ->
        Ok (Inputs [remove predicate input])
      uInput -> Update.default apply uInput
    }
  remove predicate list = case list of
    [] -> []
    head :: tail -> if predicate head then tail else head :: remove predicate tail
  insert predicate row list = case list of
    [] -> [row]
    head :: tail -> if predicate head then row :: tail else head :: insert predicate row tail
}



vlookupDef (table, value, extractColumn, generatorColumn) =
  case table of
    ["span", ["class", "table"]::_, children] ->
      tableDict.find (\x -> case x of
        ["span", ["class", "tuple"]::_, elems] ->
          List.nthMaybe (extractColumn - 1) elems == Just value
        _ -> Debug.log ("Not a tuple:" ++ toString x) False
        ) children
      |> Maybe.andThen (case of
          ["span", ["class", "tuple"]::_, elems] ->
            List.nthMaybe (generatorColumn - 1) elems 
          _ -> Nothing
        )
      |> Maybe.withDefault <span class="error">#Err: @value not found</span>
    _ ->
      <span class="error">Not a valid table @listElem</span>

      
abstractAssignment (name, (context, (params, content))) =
  (,) name <|
  let aux env params = case params of 
        [] -> 
          -- TODO: Now replace the content with the env (replacements)
          if env == [] then content else
          htmlPostMap (case of
            ["span", ["class", "variable"]::_, [["TEXT", name]]] ->
              listDict.get name env
              |> Maybe.withDefault <span class="variable notfound">@name</span>
            v -> v
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
        |> Maybe.map (\(provenance, (params, content)) ->
          let newAssignments = mkAssignments context params args ++
                  assignments
          in
          ((provenance, newAssignments), markabstract provenance name content)
        )
        |> Maybe.withDefault ((context, assignments), ["span", [["class","variable notfound"]], [["TEXT", name]]])
      ["span", [["class", "formula"]], [["TEXT", formula]]] ->
        (,) (context, assignments) <|
        case __evaluate__ (("repeat", repeatDef) :: ("vlookup", vlookupDef) :: (List.map abstractAssignment assignments) ++ preludeEnv) formula of
          Err msg -> <span class="error">@msg</span>
          Ok v -> <span class=@(classFromProvenance Abstract) title=@(formula)>@v</span>
      _ -> ((context, assignments), n)
   ) body)
  </div></div>

<html><head><meta content="text/html; charset=UTF-8" http-equiv="content-type"><style>
span.remark {
  padding: 2px;
  outline: solid black 1px;
  -webkit-box-shadow: 5px 5px 0px 0px #989898; 
  box-shadow: 5px 5px 0px 0px #989898;
  position: relative;
  top: -0.5em;
}
span.error {
  display: inline-block;
  max-height: 10em;
  overflow-y: scroll;
  white-space: pre-wrap;
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
  background: repeating-linear-gradient(
    45deg,
    rgba(0, 0, 255, 0.05),
    rgba(0, 0, 255, 0.05) 10px,
    white 10px,
    white 20px);
}
.concretization {
  background: white;
  outline: 1px solid rgba(0,0,0,0.2);
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
  font-size: 1.6em;
}
.doc h2 {
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
  background: #F8F8F8;
  cursor: help;
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

<p>I am a big fan of the idea of <a href="https://www.google.com/url?q=https://coda.io/welcome&amp;sa=D&amp;ust=1549033963249000">Coda</a>&nbsp;that mixes spreadsheets with word-like documents and makes it possible to create text that appears based on conditions, for example. It is very nice to have a domain-specific language for computing cells as in any spreadsheets, and to be able to insert cells into a document.<br

>However, there is no way to create <a href="https://www.youtube.com/watch?v=ZfytHvgHybA">reusable abstractions</a> yet. If abstractions are some day user-definable, it is likely that Coda will end up copying Google Spreadsheet and Microsoft Excel by giving access to their APIs and offering <a href="https://developers.google.com/apps-script/guides/sheets/functions">abstractions as macros</a>&nbsp;that are no longer part of the visible document, and cannot be edited with the same and nice visual interface.</p>

<!--p>Programming is all about abstraction. Indeed, <a href="https://www.google.com/url?q=https://www.youtube.com/watch?v%3DV10hzjgoklA&amp;sa=D&amp;ust=1549033963250000">Lambda Calculus</a>&nbsp;is being discovered as the ancestor of all modern programming languages and is the same as logic itself. Moreover, Lambda Calculus features only two building blocks: abstraction and concretization. Isn&rsquo;t all software based on this idea? Starting concrete, and then abstracting away parts that become data inputs, or starting abstract, and then test using concrete values?</p-->

<!--p>I recently created <a href= "https://github.com/MikaelMayer/Editor">Editor</a>, a local HTTP server to edit dynamic and static webpages in the browser, and have modifications back-propagated to the files themselves. It gave me the idea for this article.</p-->

<p>Because programming is all about abstraction, let us imagine what if, in a fictional word processor interface, we had access to a direct command to <em>abstract</em>.
Since, abstractions are for programs the equivalent of arabic positional symbols for numbers, can they also be a good candidate for developing software?</p>

<h2>MikoWord Tutorial</h2>

<p>Welcome to MikoWord!</p>

<p>MikoWord is a word processor for webpages which will satisfy both programmers and non-programmers. MikoWord features two simple but powerful ideas: to abstract and to concretize.
In this tutorial aimed at beginners, we will explain how to combine these powerful notions by walking through several examples.
We will create a rich and dynamic help page for an application named MySoft. In this tutorial, you will learn:</p>
<ul><li
>How to reuse relevant pieces of information (e.g. the name of a product)</li><li
>How to reuse partial pieces of informations (e.g. formatting)</li><li
>How to repeat abstractions (e.g. introducing a list)</li><li
>How to have MikoWord infer asbtractions for you.</li><li
>How to create your own abstractions (advanced)</li></ul>
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
In MikoWord, it is easier. Select "<span class="doc">MySoft 1.0</span>", and @(command "abstract") it.
</p>
Abstracting is giving a name to something concrete.
If you hover over the now dash-outlined "<span class="doc abstraction">MySoft 1.0</span>" in the title, you will now see that it has been assigned the name <span class="variable">mysoft</span>. The portion to the right of <span class="variable">mysoft</span> is called its <em>content</em>. The content is actually a mini-document in itself that can be further modified:
@(mkDoc [("mysoft", ([], <span class="textvalue">MySoft 1.0</span>))]
<div>
<h1><span class="variable">mysoft</span> Tutorial</h1>
Welcome to MySoft 1.0!
</div>)
<p>
Don't worry, all declarations like <span class="variable">mysoft</span> will be hidden from the final document's version, and the background will disappear. If you prefer something else than stripes, go to Settings > styles > reused content and change the style.

By opening the @(command "context menu") over <span class="variable">mysoft</span>, you can also specify to display abstractions only when they are needed during edition, depending on where your caret it. For this tutorial, we will always display them.<br> 
Now copy "<span class="doc abstraction">MySoft 1.0</span>" from the title, and paste it to replace "<span class="doc">MySoft 1.0</span>" in the document body. You obtain something like:</p>
@(mkDoc [("mysoft", ([], <span class="textvalue">MySoft 1.0</span>))] 
<div>
<h1><span class="variable">mysoft</span> Tutorial</h1>
Welcome to <span class="variable">mysoft</span>!
</div>)
<p>
Rename "<span class="doc">1.0</span>" to "<span class="doc">1.1</span>". You can rename it in any of the three places, and you'll obtain:
</p>
@(mkDoc [("mysoft", ([], <span class="textvalue">MySoft 1.1</span>))]
<div>
<h1><span class="variable">mysoft</span> Tutorial</h1>
Welcome to <span class="variable">mysoft</span>!
</div>)
Thanks to automatic correction and from now on, each time you will type "<span class="doc">mysoft</span>" followed by a space, it will replace it with "<span class="doc abstraction">MySoft 1.1</span>".
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

We want to reuse this box for the other memos.
Instead of just copy-pasting it, we will abstract it so that boxes are always using the same style. Select the table, @(command "abstract") it, and copy-paste it to duplicate it as in the previous section.

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

If you changed one of the values in any table, the change would be propagated to the other tables. We need to specify what values we should be able to change independently. Select "<span class="doc">Context menu</span>" in one of the last two tables and @(command "concretize") it. Alternatively, you could have selected it from the first table and @(command "abstract")ed it. The result looks like follows:

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

Rename "<span class="doc">Context Menu</span>" from the last table to "<span class="doc">Save</span>". The change will stay on the modified table:

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

Continue and @(command "concretize") "<span class="doc">Right-click or ALT+M</span>" and "<span class="doc">When caret blinks in doc</span>", and rename them appropriately so that you obtain something like:

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
Using this interface, if you rename "Keyboard shortcut" by "Shortcut", the renaming would appear in the other tables.
Rename <span class="variable">table</span> to <span class="variable">memo</span>, <span class="variable">contextmenu</span> to <span class="variable">name</span>, <span class="variable">right</span> to <span class="variable">shortcut</span> and <span class="variable">when</span> to <span class="variable">context</span>. Good names are always useful for later. 
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
("table", ([<span class="tuple"><span class="variable">command</span><span class="variable">shortcut</span><span class="variable">context</span></span>], <table class="kbd"><tbody>
  <tr><td>Command name:</td><td><span class="variable">command</span></td></tr>
  <tr><td>Keyboard shortcut:</td><td><span class="variable">shortcut</span></td></tr>
  <tr><td>When to use:</td><td><span class="variable">context</span></td></tr>
  </tbody>
  </table>))]
<div>
  <span class="formula">repeat(table,items)</span>
</div>)
As you hover over the table, you'll see that it uses a single formula to display this table: "<span class="doc">repeat table items</span>". More on this language later. If you double click on this table, you can also change this formula, as in any spreadsheet; but you can also change the value. More on this later.
<h3>Step 4: Intelligent abstractions</h3>
Now let's write our tutorial content. In the sequel, we only display the elements that are relevant, that is, the <span class="variabl">items</span> and the tutorial's abstractions themselves.
@(mkDoc [("items", ([], <span class="table"><span class="tuple"><span
    >Context menu</span><span
    >Right-click or ALT+M</span><span
    >When caret blinks in doc</span></span><span class="tuple"
><span
    >Save</span><span
    >CTRL+S</span><span
    >When file is not saved yet</span></span></span>
)),("mysoft", ([], <span class="textvalue">MySoft 1.0</span>))] 
<div>
<h2><span class="variable">mysoft</span>: Save actions</h2>
After any edit, remember to Save your document.
</div>)
We want to display a tooltip when the user hovers "<span class="doc">Save</span>" that displays the keyboard shortcut.
Let's add the tooltip. Select "<span class="doc">Save</span>" in the document, under the title, and @(command "abstract") it.
Select the abstraction, and add a @(command "remark") to it. As the inline pop-up is revealed, enter "<span class="doc">CTRL+S</span>" in it, as follows.
@(mkDoc [("items", ([], <span class="table"><span class="tuple"><span
    >Context menu</span><span
    >Right-click or ALT+M</span><span
    >When caret blinks in doc</span></span><span class="tuple"
><span
    >Save</span><span
    >CTRL+S</span><span
    >When file is not saved yet</span></span></span>
)),
("mysoft", ([], <span class="textvalue">MySoft 1.0</span>)),
("save", ([], <span class="textvalue">Save<span class="remark">CTRL+S</span></span>))] 
<div>
<h2><span class="variable">mysoft</span>: Save actions</h2>
After any edit, remember to <span class="variable">save</span> your document.
</div>)
Note that the remark also appears next to <span class="variable">save</span>.
To the right of <span class="variable">save</span>, select "<span class="doc">Save</span>" and @(command "abstract") it.
@(mkDoc [("items", ([], <span class="table"><span class="tuple"><span
    >Context menu</span><span
    >Right-click or ALT+M</span><span
    >When caret blinks in doc</span></span><span class="tuple"
><span
    >Save</span><span
    >CTRL+S</span><span
    >When file is not saved yet</span></span></span>
)),
("mysoft", ([], <span class="textvalue">MySoft 1.0</span>)),
("save", ([<span class="variable">arg</span>], <span class="textvalue"><span class="variable">arg</span><span class="remark">CTRL+S</span></span>))] 
<div>
<h2><span class="variable">mysoft</span>: Save actions</h2>
After any edit, remember to <span class="application"><span class="variable">save</span><span>Save</span></span> your document.
</div>)
Rename <span class="variable">save</span> to <span class="variable">command</span>, and <span class="variable">arg</span> to <span class="variable">name</span>. We now want the tooltip to be created depending on <span class="variable">name</span>'s content. Select "<span class="doc">CTRL+S</span>" in the remark next to <span class="variable">arg</span> and @(command "abstract") it.
Click on the little <img src="FutureofProgramming-lightning.png"> and choose "infer". Instead of creating a new variable, it will try to infer its content from the context. In this case, it manages to find a formula:
@(mkDoc [("items", ([], <span class="table"><span class="tuple"><span
    >Context menu</span><span
    >Right-click or ALT+M</span><span
    >When caret blinks in doc</span></span><span class="tuple"
><span
    >Save</span><span
    >CTRL+S</span><span
    >When file is not saved yet</span></span></span>
)),
("mysoft", ([], <span class="textvalue">MySoft 1.0</span>)),
("command", ([<span class="variable">name</span>], <span class="textvalue"><span class="variable">name</span><span class="remark"><span class="formula">vlookup(items,name,1,2)</span></span></span>))] 
<div>
<h2><span class="variable">mysoft</span>: Save actions</h2>
After any edit, remember to <span class="application"><span class="variable">command</span><span>Save</span></span> your document.
</div>)
<p>Once you will be more familiar with the language, you will be able to enter these formulas yourself (@(command "abstract"), then choose "formula" from <img src="FutureofProgramming-lightning.png">).</p>
<p>
Write in the document "<span class="doc">Open the </span>".
Copy "<span class="doc">Save</span>" with its tooltip, paste it after what you just wrote, and replace the command name by <span class="doc">Context menu</span>. The tooltip is automatically updated:</p>
@(mkDoc [("items", ([], <span class="table"><span class="tuple"><span
    >Context menu</span><span
    >Right-click or ALT+M</span><span
    >When caret blinks in doc</span></span><span class="tuple"
><span
    >Save</span><span
    >CTRL+S</span><span
    >When file is not saved yet</span></span></span>
)),
("mysoft", ([], <span class="textvalue">MySoft 1.0</span>)),
("command", ([<span class="variable">name</span>], <span class="textvalue"><span class="variable">name</span><span class="remark"><span class="formula">vlookup(items,name,1,2)</span></span></span>))] 
<div>
<h2><span class="variable">mysoft</span>: Save actions</h2>
After any edit, remember to <span class="application"><span class="variable">command</span><span>Save</span></span> your document. Open the <span class="application"><span class="variable">command</span><span>Context menu</span></span>
</div>)
You are now able to write a document with enriched content. Furthermore, when you will want to create a version for Mac, or localize the tutorial in another language, you will just have to further abstract over items, you won't have to do replacements.

<h3>Step 5: Custom abstraction</span>



</div>
</body></html>