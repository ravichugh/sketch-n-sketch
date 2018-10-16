translations = [
  ("English", [("fork", "fork")
              ]),
  ("French", [("fork", "fourchette")
             ])]

languages = List.map Tuple.first translations
languageIndex = 1
language = nth languages languageIndex

translate translations languageIndex node =
  let currentTranslation = nth translations languageIndex |> Dict.fromList in
  Html.replace """\$(\w+|\$)""" (\m -> 
    if m.match == "$" then [["TEXT", m.match]] else
      let key = nth m.group 1 in
      case Dict.get key currentTranslation of
        Nothing -> [["TEXT", m.match]]
        Just definition -> [["TEXT", definition]]
    ) node
  |> \x ->
    let allTranslationDicts = List.map (Tuple.mapSecond Dict.fromList) translations in
    { apply (x, _) = x
      update {input = (x, allTranslationDicts), newOutput} =
        Html.find """\{:([^\}]*(?!\})\S[^\}]*):\}""" newOutput
        |> List.foldl (\matchToTranslate (newOutput, currentTranslation, allTranslationDicts) ->
            let definition = nth matchToTranslate.group 1
                name = freshVarName "translation" 1 currentTranslation
                textToFind = """\{:@(Regex.replace """\\|\{|\}|\[|\]|\$|\.|\?|\+|\(|\)""" (\m -> "\\" + m.match) definition):\}"""
            in
            (Html.replace textToFind (\_ -> [["TEXT", "$" + name]]) newOutput,
             Dict.insert name definition currentTranslation,
             List.map (\(lang, d) -> (lang, Dict.insert name definition d)) allTranslationDicts)
          ) (newOutput, currentTranslation, allTranslationDicts)
        |> \(newOutput, _, newTranslationsLangDict) ->
          Ok (Inputs [(Debug.log "newOutput" newOutput, newTranslationsLangDict)])
    }.apply (x, allTranslationDicts)


freshVarName name i dictionary =
    if Dict.member (name + toString i) (dictionary) then freshVarName name (i + 1) (dictionary) else name + toString i

g = random.generator (random.seedOf 1)

(g, selectedImages) = g.randomSublist 8 images

pairs = selectedImages
  |> List.indexedConcatMap (\i (Duo name url) -> [(i, Named name), (i, Image url)])
  |> flip g.shuffleList_ (\g list ->
    list |> List.map (\(i, elem) -> case elem of 
    Named name -> card i <span>@name</span>
    Image url -> card i <img src=("https://www.englisch-hilfen.de"+url)>)
  )

card i child =
<div class="flip-container" num=i
  ignore-selected="false">
	<div class="flipper">
		<div class="front"
		  onclick="""if(@isOkToSelect) this.parentNode.parentNode.setAttribute('ignore-selected', '' + (this.parentNode.parentNode.getAttribute('ignore-selected') == 'false'));@checkPair"""></div>
		<div class="back">@child</div>
	</div>
</div>

main = translate translations language <|
  <div>
    <menu>
      <button
      title="Make the selected text translatable"
      onclick="""
        var r = window.getSelection().getRangeAt(0);
        var t = r.cloneContents().textContent;
        document.execCommand( 'insertText', false, "{:" + t + ":}")"""
      contenteditable="false">Make selection translatable</button>
    </menu>
  <div class="pairsContainer">
  @pairs
  <div id="successBox" class="modal" ignore-visible="false">
  <div class="modal-content">
    <span class="close">×</span>
    <p>Congratulations! You finished the memory game.</p>
  </div>
</div></div>
  <style>@css</style>
  </div>


images = [
  Duo "barbecue" "/images/kitchen/034.jpg"
, Duo "bread basket" "/images/kitchen/024.jpg"
, Duo "cake slice" "/images/kitchen/037.jpg"
, Duo "champagne cooler" "/images/kitchen/007.jpg"
, Duo "chest freezer" "/images/kitchen/020.jpg"
, Duo "coffee machine" "/images/kitchen/035.jpg"
, Duo "cooker, stove" "/images/kitchen/032.jpg"
, Duo "corkscrew" "/images/kitchen/006.jpg"
, Duo "cup" "/images/kitchen/009.jpg"
, Duo "deep fryer" "/images/kitchen/043.jpg"
, Duo "dishwasher" "/images/kitchen/026.jpg"
, Duo "egg-cup" "/images/kitchen/015.jpg"
, Duo "extractor hood" "/images/kitchen/033.jpg"
, Duo "$fork" "/images/kitchen/001.jpg"
, Duo "(frying) pan" "/images/kitchen/016.jpg"
, Duo "funnel" "/images/kitchen/004.jpg"
, Duo "glass" "/images/kitchen/010.jpg"
, Duo "grater" "/images/kitchen/039.jpg"
, Duo "jug" "/images/kitchen/011.jpg"
, Duo "juicer" "/images/kitchen/025.jpg"
, Duo "kettle" "/images/kitchen/038.jpg"
, Duo "kitchen scales" "/images/kitchen/041.jpg"
, Duo "knife" "/images/kitchen/002.jpg"
, Duo "ladle" "/images/kitchen/012.jpg"
, Duo "meat fork" "/images/kitchen/013.jpg"
, Duo "microwave" "/images/kitchen/036.jpg"
, Duo "grinder" "/images/kitchen/021.jpg"
, Duo "mixer" "/images/kitchen/028.jpg"
, Duo "oven glove" "/images/kitchen/005.jpg"
, Duo "pepper mill" "/images/kitchen/031.jpg"
, Duo "pressure cooker" "/images/kitchen/042.jpg"
, Duo "refrigerator" "/images/kitchen/019.jpg"
, Duo "rolling pin" "/images/kitchen/008.jpg"
, Duo "scissors" "/images/kitchen/022.jpg"
, Duo "sieve" "/images/kitchen/029.jpg"
, Duo "spoon" "/images/kitchen/003.jpg"
, Duo "springform pan" "/images/kitchen/018.jpg"
, Duo "steak hammer" "/images/kitchen/027.jpg"
, Duo "toaster" "/images/kitchen/014.jpg"
, Duo "waffle-iron" "/images/kitchen/017.jpg"
, Duo "wine shelf" "/images/kitchen/023.jpg"
]

  
css = """
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
}"""


notInPair = """(function() {
    var selected = document.querySelectorAll("[ignore-selected=true]");
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
  })()"""

isOkToSelect = """@(notInPair).length == 0 || @(notInPair).length % 2 == 1"""

checkPair = """
// If the number of selected cards is odd, do nothing.
// Else, set a timer to hide the cards that are not in pairs.
var notInPair = @notInPair
if(notInPair.length > 0 && notInPair.length % 2 == 0) {
  setTimeout((function(notInPair) {
    return function() {
      for(var i = 0; i < notInPair.length; i++) {
        notInPair[i].setAttribute("ignore-selected", "false")
      }
    }
  })(notInPair), 1000)
} else {
  setTimeout(function() {
    var hidden = document.querySelectorAll("[ignore-selected=false]");
    if(hidden.length == 0) {
      document.getElementById("successBox").setAttribute("ignore-visible", "true");
      document.querySelector("#successBox .close").onclick = function() {
        document.getElementById("successBox").setAttribute("ignore-visible", "false");
      }
    }
  }, 1000)
}
"""