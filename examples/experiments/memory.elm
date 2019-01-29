-- TODO: Import images from https://www.englisch-hilfen.de/en/words/kitchen.htm

-- Because we base decisions on random numbers,
-- to update, it's essential that these random numbers are deterministically computed.
seed = 1

g = random.generator (random.seedOf seed)

(g, selectedImages) = g.randomSublist 8 images

pairs = selectedImages
  |> List.indexedConcatMap (\i (Duo name url) -> [(i, Named name), (i, Image url)]
  |> flip g.shuffleList_ (\g list ->
    list |> List.map (\(i, elem) -> case elem of 
    Named name -> card i <span>@name</span>
    Image url -> card i <img src=url>)
  )

importStep = <table>
@Update.freezeExcept(always "tbody template")(images)(\images-><tbody>@(
  images
  |> List.map (\Duo name url ->
     <tr><td><img src=url/></td><td>@name</td></tr>)
  |> Update.onUpdate (
     Html.filter (not << Html.isEmptyText))
  )</tbody>)
</table>

main = importStep

images = [
  Duo "dog" "https://upload.wikimedia.org/wikipedia/commons/7/7d/Labrador_Chocolate.jpg"
]

card i child =
<div class="flip-container" num=i
  ignore-selected="false">
	<div class="flipper">
		<div class="front"
		  onclick="""if(@isOkToSelect) this.parentNode.parentNode.setAttribute('ignore-selected', '' + (this.parentNode.parentNode.getAttribute('ignore-selected') == 'false'));@checkPair"""></div>
		<div class="back">@child</div>
	</div>
</div>

_ = """@card(1)(<img src="https://upload.wikimedia.org/wikipedia/commons/7/7d/Labrador_Chocolate.jpg">)
  @card(1)(<span>Dog</span>)
  @card(2)(<img src="https://vignette.wikia.nocookie.net/sims/images/f/f4/The_Sims_4_Cat_Icon.png/revision/latest/scale-to-width-down/40?cb=20171112051930">)
  @card(2)(<span>Cat</span>)"""

main = <div>
  @pairs
  <div id="successBox" class="modal" ignore-visible="false">
  <div class="modal-content">
    <span class="close">×</span>
    <p>Congratulations! You finished the memory game.</p>
  </div>
</div>
  <style>@css</style>
  </div>

css = """ /* Credits to https://davidwalsh.name/css-flip */
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
	background-image: linear-gradient(to bottom right, red, yellow);
}

/* back, initially hidden pane */
.back {
	transform: rotateY(180deg);
	text-align: center;
  display: inline-table;
  font-size: 20px;
  background-color: #EEE;
}
.back > span {
	display:table-cell;
	vertical-align: middle;
	text-align: center;
}
/* The Modal (background) */
.modal {
    display: none; /* Hidden by default */
    position: fixed; /* Stay in place */
    z-index: 7; /* Sit on top */
    left: 0;
    top: 0;
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