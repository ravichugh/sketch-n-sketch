t = tutorialUtils

options = {production=True}

main = t.htmlpass options <| t.markdown <|
<div class="outer"><div class="inner" contenteditable=(toString (not options.production))>
# Sketch-n-sketch Tutorial - Multilingual Memory game
<i>This tutorial assumes that you have
<a href="https://mikaelmayer.github.io/sketch-n-sketch/">Sketch-n-sketch</a> running on your browser.
The 'left pane' references the left half of that interface, whereas the 'right pane' references the right half of that interface. If you encounter editing issues, try resizing your window.</i>  
  
The memory game consists of pairs of identical cards placed face down.
The player turns over two cards. If they are the same, they are left face up.
If they are not, they are turned back face down.  
  
In this tutorial, we will create an educational variant of the memory game
 to associate images to their transcriptions in a given language.
This is our roadmap:
<ul>
<li>Import images and descriptions</li>
<li>Design a functional memory game</li>
<li>Add translations to the game</li>
</ul>
## Import images and descriptions
We will import images and descriptions from <a href="https://www.englisch-hilfen.de/en/words/kitchen.htm">this website</a>. Visit the link to see what the table looks like.  
We want to import this table as source code we can work with.
For that, the trick is to actually write source code with some dummy data,
transform it to be rendered as a table,
and add the remaining rows by copying them from the website and pasting them into the output.
Sketch-n-sketch will figure out for us how to reverse the execution and, finally,
update our source data to produce the given table.

In sketch-n-sketch, erase the program in the left pane and replace it with the following:
@t.newcode<|"""images = [
  Image "dog" "https://upload.wikimedia.org/wikipedia/commons/7/7d/Labrador_Chocolate.jpg"
]

@(placeholder 1)

main = <table>
@@Update.freezeExcept(always "template")(images)(\images-> <tbody>@@(
  images
  |> List.map (\Image name url ->
     <tr><td><img src=url></td><td>@@name</td></tr>)
  |> Update.onUpdate (
     Html.filter (not << Html.isEmptyText))
  )</tbody>)
</table>"""
@t.displaycode
After clicking on "Run", you should see the below result in the right pane.  
@t.displayevalcode
Let us explain this code a bit.  
First because we want to change the data, we prepend <code>@@Update.freezeExcept</code> to the rendered table to make sure the expression itself will not be modified when we change the output, only the data.
Second, we transform the list of images to a list of <code>&lt;tr&gt;</code> rows, each containing two <code>&lt;td&gt;</code> cells. The first cell contains the image, the second cell the name.  
Because the rows we are importing will contain whitespace, mostly newlines (which our program does not produce), we make sure to remove them with the instruction <code>Update.onUpdate (Html.filter (not &lt;< Html.isEmptyText))</code>.
This instruction is the last one because it will be the first to execute when the program executes in reverse.

Note that the <code>main = </code> is optional, if the program ends with an expression, it will automatically insert <code>main = </code> in front of it.

Great. You can try to rename "dog" in the output to see that the rename propagates to the corresponding <code>Image</code>.  
Now, time to import the images.

On the website, right-click on the table. In major browsers a menu item "Inspect" or "Inspector" will lead you to a developer console that will display the source code of the page at this point. To display the actual source code, you might have to right-click the element again and "edit as HTML".
Now select the first 8 rows in the Html source. A row starts with &lt;tr&gt; and ends with &lt;/tr&gt;.
Alternatively, you can copy the rows from the box below.

@t.displaylocalcode<|"""<tr>
<td><img src="/images/kitchen/034.jpg"></td>
<td>barbecue</td>
</tr>
<tr>
<td><img src="/images/kitchen/024.jpg"></td>
<td>bread basket</td>
</tr>
<tr>
<td><img src="/images/kitchen/037.jpg"></td>
<td>cake slice</td>
</tr>
<tr>
<td><img src="/images/kitchen/007.jpg"></td>
<td>champagne cooler</td>
</tr>
<tr>
<td><img src="/images/kitchen/020.jpg"></td>
<td>chest freezer</td>
</tr>
<tr>
<td><img src="/images/kitchen/035.jpg"></td>
<td>coffee machine</td>
</tr>
<tr>
<td><img src="/images/kitchen/032.jpg"></td>
<td>cooker, stove</td>
</tr>
<tr>
<td><img src="/images/kitchen/006.jpg"></td>
<td>corkscrew</td>
</tr>"""

Once you have this data is in the clipboard, click on the "Html" button to the right of the Sketch-n-sketch interface to see the source code as html. Paste the clipboard contents right after the last <code>&lt;/tr&gt;</code>.
The popup "Output Editor" appears; hover over "Update program" and click on the first solution.
The variable 'images' is now populated.  
You've successfully imported data that would have normally required the use of regular expressions or custom programs to parse.  
  
You now need to fix the URL of the images you imported, because they are relative.
In the code, <b>delete the first row</b> containing the information about the dog (and the comma in front of the second entry).
@t.hiddenreplace("""images = [
  Image "dog" "https://upload.wikimedia.org/wikipedia/commons/7/7d/Labrador_Chocolate.jpg"
]""")<|"""images = [
  Image "barbecue" "/images/kitchen/034.jpg"
, Image "bread basket" "/images/kitchen/024.jpg"
, Image "cake slice" "/images/kitchen/037.jpg"
, Image "champagne cooler" "/images/kitchen/007.jpg"
, Image "chest freezer" "/images/kitchen/020.jpg"
, Image "coffee machine" "/images/kitchen/035.jpg"
, Image "cooker, stove" "/images/kitchen/032.jpg"
, Image "corkscrew" "/images/kitchen/006.jpg"
]"""
@t.replace("""img src=url""")<|"""img src="https://www.englisch-hilfen.de"+url"""

You should now obtain the following table:
@t.displayevalcode

FYI: the source code at this point is:
@t.displaycode
## Design the logic
We want to create two cards for each image, one for the name,
the other for the picture. We will then shuffle these cards and,
using css tricks, display them 'face down' on a 4x4 grid.
With the power of Javascript, we will handle clicks and even
show a modal dialog to congratulate the player at the end.

Fortunately, we coded all of this in Javascript and CSS
for you so you just need to do some replacements.
First, the logic for building the deck.
@t.replace(placeholder 1)<|"""@(placeholder 2)

g = random.generator (random.seedOf 1)

pairs = images
  |> List.indexedConcatMap (\i (Image name url) -> [(i, Named name), (i, Image url)])
  |> flip g.shuffleList_ (\_ list ->
    list |> List.map (\(i, elem) -> case elem of 
    Named name -> card i <span>@@name</span>
    Image url -> card i <img src=("https://www.englisch-hilfen.de"+url)>)
  )

card i child =
<div class="flip-container" num=i
  ignore-selected="false">
	<div class="flipper">
		<div class="front"
		  onclick=@(q3)if(@@(isOkToSelect "this.parentNode.parentNode.parentNode")) this.parentNode.parentNode.setAttribute('ignore-selected', '' + (this.parentNode.parentNode.getAttribute('ignore-selected') == 'false'));@@(checkPair "this.parentNode.parentNode.parentNode")@q3></div>
		<div class="back">@@child</div>
	</div>
</div>
"""
Now, we add the css and javascript for displaying the deck.
@t.replace("""main = <table>...</table>""")<|"""main = Html.forceRefresh <div>
  @buttonPlaceholder The game is in English.
  <div class="pairsContainer">
    @@pairs
    <div id="successBox" class="modal" ignore-visible="false">
      <div class="modal-content">
        <span class="close">×</span>
        <p>Congratulations! You finished the memory game.</p>
      </div>
    </div>
  </div>
  <style>@@css</style>
  </div>

css = @q3
.pairsContainer {
  max-width: 535px;
  margin: 10px;
  outline: 2px solid #8A9;
  outline-radius: 2px;
  padding: 2px;
  padding-left: 5px;
  background: #AFC
}

/* Credits to https://davidwalsh.name/css-flip */
/* entire container, keeps perspective */
.flip-container {
	perspective: 250px;
	display:inline-block;
}
	/* flip the pane when clicked */
.flip-container[ignore-selected=true] .flipper {
		transform: rotateY(180deg);
}

.flip-container, .front, .back {
	width: 128px;
	height: 128px;
	margin: 2px;
}

/* flip speed goes here */
.flipper {
	transition: 0.6s;
	transform-style: preserve-3d;

	position: relative;
}

/* hide back of pane during swap */
.front, .back {
	backface-visibility: hidden;
	position: absolute;
	top: 0;
	left: 0;
}

/* front pane, placed above back */
.front {
	z-index: 2;
	/* for firefox 31 */
	transform: rotateY(0deg);
	background-image: linear-gradient(to top left, #F00, #FF0);
}

/* back, initially hidden pane */
.back {
	transform: rotateY(180deg);
	text-align: center;
  display: inline-table;
  font-size: 20px;
  background-image: linear-gradient(to bottom right, #EEE, #CCC);
  
  display: flex;
  align-items: center;
  justify-content: center;
}
.back > span {
	display:table-cell;
	vertical-align: middle;
	text-align: center;
}
/* The Modal (background) */
.modal {
    display: none; /* Hidden by default */
    position: absolute; /* Stay in place */
    z-index: 7; /* Sit on top */
    left: 0;
    top: 0;
    right: 0;
    bottom: 0;
    width: 100%; /* Full width */
    height: 100%; /* Full height */
    overflow: auto; /* Enable scroll if needed */
    background-color: rgb(0,0,0); /* Fallback color */
    background-color: rgba(0,0,0,0.4); /* Black w/ opacity */
}

/* Modal Content/Box */
.modal-content {
    background-color: #fefefe;
    margin: 15% auto; /* 15% from the top and centered */
    padding: 20px;
    border: 1px solid #888;
    width: 50%; /* Could be more or less, depending on screen size */
}

/* The Close Button */
.close {
    color: #aaa;
    float: right;
    font-size: 28px;
    font-weight: bold;
}

.close:hover,
.close:focus {
    color: black;
    text-decoration: none;
    cursor: pointer;
}

[ignore-visible=false] {
  display:none;
}
[ignore-visible=true] {
  display:block;
}@q3

notInPair parent = @q3(function(parent) {
    var selected = parent.querySelectorAll("[ignore-selected=true]");
    var notInPair = []
    for(var i = 0; i < selected.length; i++) {
      var hasPair = false;
      for(var j = 0; j < selected.length; j++) {
        if(i != j && selected[j].getAttribute("num") == selected[i].getAttribute("num")) {
          console.log(i, j);
          hasPair = true;
          break;
        }
      }
      if(!hasPair) {
        notInPair.push(selected[i])
      }
    }
    return notInPair;
  })(@@parent)@q3

isOkToSelect parent = @q3(@@(notInPair parent)).length == 0 || (@@(notInPair parent)).length % 2 == 1@q3

checkPair parent = @q3
// If the number of selected cards is odd, do nothing.
// Else, set a timer to hide the cards that are not in pairs.
var notInPair = @@(notInPair parent)
if(notInPair.length > 0 && notInPair.length % 2 == 0) {
  setTimeout((function(notInPair) {
    return function() {
      for(var i = 0; i < notInPair.length; i++) {
        notInPair[i].setAttribute("ignore-selected", "false")
      }
    }
  })(notInPair), 1000)
} else {
  console.log("launching timeout to check everything on on", @@parent)
  setTimeout((function(parentNode) {
    return function() {
      var hidden = parentNode.querySelectorAll("[ignore-selected=false]");
      if(hidden.length == 0) {
        parentNode.querySelector("#successBox").setAttribute("ignore-visible", "true");
        parentNode.querySelector("#successBox .close").onclick = function() {
          parentNode.querySelector("#successBox").setAttribute("ignore-visible", "false");
        }
      }
    }
  })(@@parent), 1000)
}@q3"""

After running the program, you should obtain the following game.
Click on the squares to reveal them.
@t.displayevalcode

A few notes for how we achieved this result:
<ul>
<li>When attributes are prefixed with <code>ignore-</code>, their modifications are ignored by the update engine. Hence, we only allow the javascript to modify these attributes. Because CSS can read attributes, we can change the appearance of cards when we click on them, without triggering the update engine.</li>
<li>We actually create complex javascript code inside the "onclick" events of cards, by inlining the code for <code>isOkToSelecct</code> and <code>checkPair</code>. That's fine, we are not looking at the generated source code anyway.</li>
<li>We use similar tricks to display the modal dialog box when the game is over.</li>
</ul>
Now try to find the corkscrew and its image (hints: 2North-2East, 1North-1West).

## Translation logic
We now want to translate the game to make it available in several languages.
For this, let us add a table of translations.
@t.replace(placeholder 2)<|"""translations = [
  ("English", [("translation1", "corkscrew")
              ]),
  ("French", [("translation1", "corkscrew")
             ])]

languages = List.map Tuple.first translations
languageIndex = 1
language = nth languages languageIndex

@(placeholder 3)"""
The table of translations is a list of pairs of language name and translations.
Translations are a list of keywords associated to a word in the language.
From this table, we extract the list of available languages (<code>languages</code>),
and given a <code>languageIndex</code> we can compute the current language's name.

We did not translate the word into French yet because we are going to do so from the game itself soon.  
  
We need to know when to use translated text and when not to. Let us use the convention that any word prefixed with a $ will be subject to translation using the table above.
@t.replace("Image \"corkscrew\"")<|"Image \"$translation1\""
Now, the 'corkscrew' card shows '$translation1' instead - we want to see it go back to being 'corkscrew' again.
@t.replace(placeholder 3)<|
"""translate translations languageIndex node =
  let currentTranslation = nth translations languageIndex |> Tuple.second |> Dict.fromList in
  Html.replace @q3\$(\w+|\$)@q3 (\m -> 
    if m.match == "$" then [["TEXT", m.match]] else
      let key = nth m.group 1 in
      case Dict.get key currentTranslation of
        Nothing -> [["TEXT", m.match]]
        Just definition -> [["TEXT", definition]]
    ) node
  @(placeholder 4)"""
@t.replace("main = Html.forceRefresh <div>")<|"""main = translate translations languageIndex <|
  Html.forceRefresh <div>"""
@t.replace("The game is in English.")<|"""The game is in @@(language)."""

Good job. Now it's back to "corkscrew" again.
But wait, doesn't the game say that the language is French?
Directly on the corkscrew card, replace "corkscrew" with "tire-bouchon" (the French translation).
Make sure @t.lineof("(\"French\",") is visible to see the changes.
@t.hiddenreplace("""("French", [("translation1", "corkscrew")""")<|
                 """("French", [("translation1", "tire-bouchon")"""

## Add translations from the output
For now, we cannot translate anything other than "corkscrew", because it is the only entry in the translation table.
We wish we could directly edit the output and add a flag that says "Add a translation entry for this word".
We can use lenses for this purpose. We decide that if we write '{:word:}' in the game, it should create an entry for 'word' in every language.
@t.replace(placeholder 4)<|
"""|> \htmlNode ->
    Update.lens2 {
      apply (htmlNode, _) = htmlNode
      update {input = (_, translations), outputNew=newHtmlNode} =
        Html.find @q3\{:([^\}]*(?!\})\S[^\}]*):\}@q3 newHtmlNode
        |> List.foldl (\matchToTranslate (updatedHtmlNode, currentTranslation, translations) ->
            let definition = nth matchToTranslate.group 1
                name = freshVarName "translation" 1 currentTranslation
                textToFind = @q3\{:@@(tutorialUtils.escape definition):\}@q3
            in
            (Html.replace textToFind (\_ -> [["TEXT", "$" + name]]) updatedHtmlNode,
             Dict.insert name definition currentTranslation,
             List.map (\(lang, d) -> (lang, d ++ [(name, definition)])) translations)
          ) (newHtmlNode, currentTranslation, translations)
        |> \(finalHtmlNode, _, newTranslations) ->
          Ok (Inputs [(finalHtmlNode, newTranslations)])
    } htmlNode translations

freshVarName name i dictionary =
    if Dict.member (name + toString i) (dictionary) then freshVarName name (i + 1) (dictionary) else name + toString i

@(placeholder 5)"""
Be careful about the whitespace - there should be two spaces before the first <code>|&gt;</code> you just inserted, so that this code stays inside the function.
Here is an explanation of how the above code works:
<ul>
<li>We name the result of translation <code>htmlNode</code>.</li>
<li>We apply a lens to this result and the list of all translations.</li>
<li>In the forward evaluation, this lens returns the html node.</li>
<li>In the backwards evaluation, it looks for occurrences of {:...:}, creates variable names for them, and adds the definitions to all languages in translations. Furthermore, it replaces them with the new variables prefixed with '$'.</li>
<li>The new variable names are guaranteed to not yet exist.</li></ul>
<li>The notation <code>|&gt;</code> means "pass whatever is before <code>|&gt;</code> as the argument to the function that comes after <code>|&gt;</code>".</li>
Good. In the output, find and rename "cake slice" to "{:cake slice:}".
@t.hiddenreplace("\"translation1\", \"corkscrew\")")<|""""translation1", "corkscrew")
              , ("translation2", "cake slice")"""
@t.hiddenreplace("\"translation1\", \"tire-bouchon\")")<|""""translation1", "corkscrew")
              , ("translation2", "pelle à tarte")"""
After update, the braces and colon should disappear in the output. You can now replace "cake slice" by "pelle à tarte" and see that this modification is propagated right into the code.
Had we done this translation automatically, we would have had the French equivalent of "slice of cake", which is not at all the same thing. Translating in context can be very useful.

## Translation toolbar
It can be tedious to manually type the characters {:...:} to translate every word. Fortunately, using Javascript we can make a button that does this on the selected text:

@t.replace(buttonPlaceholder)<|"""<menu>
      <button
      title="Make the selected text translatable"
      onclick=@q3
        var r = window.getSelection().getRangeAt(0);
        var t = r.cloneContents().textContent;
        document.execCommand( 'insertText', false, "{:" + t + ":}")@q3
      contenteditable="false">@makeselectiontranslatable</button>
      @othermenuitems
    </menu>"""

Good. Now, in the output, find and select the text "cooker, stove". Click on the button "@makeselectiontranslatable". After the update roundtrip, you can edit the translation to "cuisinière, four". 
@t.hiddenreplace("\"translation2\", \"cake slice\")")<|""""translation1", "corkscrew")
              , ("translation3", "cooker, stove")"""
@t.hiddenreplace("\"translation2\", \"pelle à tarte\")")<|""""translation1", "corkscrew")
              , ("translation3", "cuisinière, four")"""
This is much easier than manually writing {:...:}.

What about selecting the language in which we are displaying the game? Easy.
@t.replace(othermenuitems)<|"""@@(Html.select [] languages languageIndex)
@othermenuitems"""
<code>Html.select</code> is a lens that displays options, with the third argument designating the selected one. It is coded in such a way that, if you change the selection, it changes the third argument.
Select English, and then go back to French, to see how this works.

What about adding a new language? For that, we display an input text box that, if submitted,
uses its value as a new language name and copies the current translations for it.

@t.replace(placeholder 5)<|
"""langAdder = Update.lens2 {
  apply (languageIndex, translations) = ""
  update {input=(languageIndex, translations), outputNew=newLanguage} =
    let translationsDict = Dict.fromList translations in
    if not (newLanguage == "") && not (Dict.member newLanguage translationsDict) then 
      let toCopy = Dict.apply translationsDict language in
      let newLanguageIndex = List.length translations in
      Ok (InputsWithDiffs [((newLanguageIndex, translations ++ [(newLanguage, toCopy)])
                           ,Just (
                             Update.vTupleDiffs_1_2
                               VConstDiffs
                               (VListDiffs [(List.length translations, ListElemInsert 1)])))])
    else
      Ok (InputsWithDiffs [((languageIndex, translations), Nothing)])
  } languageIndex translations"""
@t.replace(othermenuitems)<|"""<input style="margin-left:10px;width:180px;" type="text"
  v=langAdder
  placeholder="New Language (e.g. German)"
  title="Enter the name of a new language here and press ENTER"
  onchange="this.setAttribute('v',this.value)">"""

A few notes on what we just did.
<ul>
<li>The variable <code>langAdder</code> is computed as the empty string.</li>
<li>When the input is submitted, the Javascript updates langAdder with the new language name.</li>
<li>The lens has the power to change the languageIndex and the translations. It duplicates the current translation and appends it to the end of the table of translations, and changes the language index to switch to the new language.</li>
<li>So far, we've returned values in lenses with <code>Inputs</code> and a list of possible values.
Another (more advanced) way is to use <code>InputsWithDiffs</code> and a list of pair of possible values
and their <i>difference</i> to the old values. That way, the engine does not need to compute differences itself.</li>
</ul>
  
The result should look like:
@t.displayevalcode

As an exercise, you can translate the remaining cards into the language of your choice.

## Experiments
If you have more time, you can try to accomplish the following tasks:
<ul>
<li>Can you translate the text of the button "@makeselectiontranslatable" itself?</li>
<li>And how can we translate the tooltip that appears when we hover over this button?</li>
<li>Right now, translation names are "translation1", "translation2", which is boring.
Could you infer a better (unique) name from the content itself? You might find the following code snippet useful: <code>Regex.replace """[^a-zA-Z0-9]""" (\_ -> "") "Word with space"</code></li>
<li>It can be tedious to see which text is translated and which is not.
You could replace <code>[["TEXT", definition]]</code> (@(t.lineof """[["TEXT", definition]]""")) with <code>
[&lt;span style="@highlightstyle"&gt;@@definition&lt;/span&gt;]]</code> to see the difference (e.g. <span style=highlightstyle>tire-bouchon</span>).
Could you add a toolbar button that allows you to do the switch easily? Hint: create a boolean <code>doHighlight = True</code> and render a bidirectional checkbox using <code>Html.checkbox "Highlights" "Highlight translatable text" doHighlight</code>.
</li>
<li>How can you translate the language names that are displayed?</li>
</ul>

## Final code
Here is the code used in this tutorial for reference.
@t.displaycode
</div>
<style>
#outputCanvas {
  overflow: hidden
  font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
}
div.outer {
  width:100%;
  height:calc(100% - 0px);
  background:#BBB;
  overflow-y: scroll;
  font-size: 16pt;
}
div.inner {
  padding-top:10pt;
  padding-left:10pt;
  padding-right:10pt;
  margin:0px;
  max-width:500pt;
  margin-left:auto;
  margin-right:auto;
  background:white;
  line-height:1.2em;
}
@@media only screen and (orientation: portrait) {
    div.outer {
        font-size: 16pt;
    }
    div.inner {
      margin:0px;
      width: calc(100%-10pt);
      max-width: 98%  !important;
      margin-left:auto;
      margin-right:auto;
      background:white
    }
    code.snippet {
      font-size: 2em;
    }
}
div.inner h1 {
  line-height: 1em;
}
div.inner h2 {
  padding-top: 20px;
}
div.intermediateresult {
  font-style: italic;
  color: #AAA;
}
code.snippet {
  white-space:pre;
  display:block;
  margin:10px;
  padding: 5px;
  border:1px solid black;
  overflow-x: scroll;
  line-height:1em;
}
code.error {
  color:red;
  white-space:pre;
}
div.outputwrapper {
  -webkit-box-shadow: 5px 10px 5px 0px rgba(0,0,0,0.5);
  -moz-box-shadow: 5px 10px 5px 0px rgba(0,0,0,0.5);
  box-shadow: 5px 10px 5px 0px rgba(0,0,0,0.5);
  margin:10px;
  padding:10px;
  border:2px solid black;
  margin-bottom: 15px;
}</style>
</div>

buttonPlaceholder = "Translation menu coming soon."
othermenuitems = "Other menu items coming soon."
highlightstyle = "outline: 2px solid lightgreen"
makeselectiontranslatable = "Make selection translatable"

placeholder i = """-- We will write code here #@i"""

q3 = "\"\"\""
