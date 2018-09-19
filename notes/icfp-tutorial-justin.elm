t = tutorialUtils

options = {production=True}

main = t.htmlpass options <| t.markdown <|
<div class="outer"><div class="inner" contenteditable=(toString (not options.production))>
# Sketch-n-sketch ICFP Tutorial - Justin's part
<i>This tutorial assumes that you have
<a href="https://mikaelmayer.github.io/sketch-n-sketch/">Sketch-n-sketch</a> running on your browser.
The 'left pane' references the left half of that interface, whereas the 'right pane' references the right half of that interface. If you encounter editing issues, try resizing your window.</i>  
  
Let's start with the most basic program.

@t.newcode<|"""main =
  [ "div"
  , []
  , []
  ]"""
@t.displaycode

We can output HTML nodes by creating this tree structure:
@t.newcode<|"""main =
  [ "div" -- tag name
  , []    -- attributes
  , [ [ "h1"
      , []
      , [ ["TEXT", "Hello, world!"] ]
      ]
    ]
  ]"""

@t.displaycode  
## Variables
We can introduce a variable:
@t.hiddenreplace("main =")<|"""name =
  "Justin"

main ="""
@t.hiddenreplace(" world")<|"\" + name + \""
@t.displaycode  
## Bidirectional Programming 1
Uh oh! Formatting issue ("Hello,Justin!")!
We can add the space in the output,
and add the space to the "Hello, " string (not the name string).
@t.hiddenreplace("\"Hello,\"")("\"Hello, \"")
@t.displaycode

## HTML Literals
This tree is really clunky/annoying to type out, though... luckily, we have HTML literals! Notice the @@ sign.

@t.newcode<|"""name =
  "Justin"

main =
  <div>
    <h1>Hello, @@name!</h1>
  </div>"""

Now let's make a list of our favorite things.
@t.hiddenreplace("</h1>")<|"""</h1>
    <h2>These are a few of my favorite things</h2>
    <ul>
      <li>Raindrops on roses</li>
      <li>Whiskers on kittens</li>
      <li>Bright copper kettles</li>
      <li>Warm woolen mittens</li>
      <li>Brown paper packages tied up with strings</li>
    </ul>"""
@t.displaycode

Let's add some additional information to these list items.

@t.hiddenreplace("<li>R")<|"""<li>I like r"""
@t.hiddenreplace("<li>W")<|"""<li>I like w"""
@t.hiddenreplace("<li>B")<|"""<li>I like b"""

@t.displaycode  
## Functions
That was a lot of typing!
Let's abstract it into the function so we don't have
to ever do that again in the future (change to "really like").

@t.hiddenreplace("</li>")<|"""")"""
@t.hiddenreplace("<li>I like ")<|"""@@(like """"
@t.hiddenreplace("name =")<|"""like x =
  <li>I really like @@x</li>

name ="""
Replace also the <code>&lt;li&gt;I like ...&lt;/li&gt;</code>
by <code>@@(like "...")</code>. You should get the following:
@t.displaycode
## List Mapping
But we can do better! Let's make a list of our favorite things, and map over it:
@t.hiddenreplace("name =")<|"""myFavoriteThings =
  [ "raindrops on roses"
  , "whiskers on kittens"
  , "bright copper kettles"
  , "warm woolen mittens"
  , "brown paper packages tied up with strings"
  ]

name ="""
@t.hiddenreplace("<ul>...</ul>")<|"""<ul>
      @@(List.map like myFavoriteThings)
    </ul>"""
@t.displaycode
## Saving
Now let's save the file.
## Styles
We can modify styles, too:
@t.hiddenreplace("""main =
  <div""")<|"""main =
  <div style="padding: 10px;""""
@t.displaycode
Let's add a lot of style, actually!
@t.hiddenreplace("name =")<|"""
shoppingList =
  [ ("red", "apples")
  , ("orange", "carrots")
  , ("yellow", "bananas")
  , ("green", "kale")
  , ("blue", "berries")
  , ("indigo", "dye")
  , ("violet", "flowers")
  ]
  
shoppingEntry (color, item) =
  <li style=("color:" + color)
     >@@color @@item</li>

name ="""
@t.hiddenreplace("</ul>")<|"""</ul>
    <h2>Shopping list</h2>
    <ul>
      @@(List.map shoppingEntry shoppingList)
    </ul>"""
@t.displaycode
## Abstraction
And we'll abstract once more:
@t.hiddenreplace("name =")<|"""info title listViewer list =
  <section>
    <h2>@@title</h2>
    <ul>
      @@(List.map listViewer list)
    </ul>
  </section>

name ="""
@t.hiddenreplace("</h1>...</div>")<|"""</h1>
    @@(info "These are a few of my favorite things" like myFavoriteThings)
    @@(info "Shopping list" shoppingEntry shoppingList)
  </div>"""
@t.displaycode
## Lambda
And to finish it off (note the lambda):
@t.hiddenreplace("\"Justin\"")<|""""Justin"

myData =
  [ ("These are a few of my favorite things", like, myFavoriteThings)
  , ("Shopping list", shoppingEntry, shoppingList)
  ]"""
@t.hiddenreplace("</h1>...</div>")<|"""</h1>
    @@(List.map (\(t, lv, l) -> info t lv l) myData)
  </div>"""
@t.displaycode
@t.displayevalcode
## Bidirectional Programming 2
Let's change the output of our shopping list!
@t.hiddenreplace("\"red\"")("\"yellow\"")
@t.hiddenreplace("\"blue\"")("\"black\"")
@t.hiddenreplace("\"indigo\"")("\"cyan\"")
@t.hiddenreplace("\"violet\"")("\"purple\"")
@t.displaycode
You should obtain the following result:
@t.displayevalcode
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
  background:white
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
div.inner > h2 {
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

placeholder i = """-- Checkpoint #@i"""

q3 = "\"\"\""