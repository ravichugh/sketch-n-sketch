options = {production=True}

main = t.htmlpass options <| t.markdown <|
<div class="outer"><div class="inner" contenteditable="true">
# Sketch-n-sketch Tutorial - Pizza website
<i>This tutorial assumes that you have
<a href="https://mikaelmayer.github.io/sketch-n-sketch/">Sketch-n-sketch</a> running on your browser.
The 'left pane' references the left half of that interface, whereas the 'right pane' references the right half of that interface. If you encounter editing issues, try resizing your window.</i>  
  
How long would it take to
<ul>
<li>create a website from scratch</li>
<li>that displays options of pizzas,</li>
<li>that users can choose from and have their preferences saved,</li>
<li>that enables users to change their preferences,</li>
<li>that enables users to delete their preferences,</li>
<li>everything translated in two languages,</li>
<li>the website itself being easy to maintain,</li>
<li>without spitting out just a "spreadsheet"?</li>
</ul> 

Using Sketch-n-sketch, this takes a few minutes and 25 lines of code.

## Create a website from scratch
To start, replace the program in the Sketch-n-sketch editor by selecting everything and pasting the following code on the left pane of the Sketch-n-sketch editor:
@t.newcode<|"""user = "Mikael"

@(placeholder 1)

main = <span>Hello @@user!<br>
@pizzasPlaceholder
</span> |> Html.forceRefresh
"""
@t.displaycode
Click on the green "Run" button to see the website. <code>Html.forceRefresh</code>  ensures in this interface that the website is always in sync with the code.  
  
As an exercise, in the right pane, try deleting the space between "Hello" and "Mikael".
Then update the program by hovering and then clicking on the dialog box.
Again, reinsert the space between "Hello" and "Mikael". Note how you can resolve ambiguities.

## Display options of pizzas
Let us create a select box that let users select from a list of pizzas.  
@t.replace(pizzasPlaceholder)<|
"""I want a @@(Html.select [] ["@chooseyourpizza", "Margharita", "Queen", "Calzone"] 0)"""
You should obtain the following result:
@t.displayevalcode  
  
If you now modify a pizza, you should see that the 0 turns into the index of the chosen pizza.
Let us store the choice in a variable.
@t.replace(placeholder 1)<|"""@(placeholder 2)

choice = 0"""
@t.replace(""""Calzone"] 0""")<|""""Calzone"] choice"""

Now if you choose a different pizza, the variable choice will be updated instead of an inlined constant.
However, this is single-user only. How to record the preferences of many users?

## Store a choice per user.
@t.replace(placeholder 2)<|"""userdata = [("Mikael", 1)]"""
@t.replace("choice = 0")<|"""choice = case listDict.get user userdata of
  Just c -> c
  Nothing -> 0

@(placeholder 3)"""
<code>listDict</code> is a library that considers a list of pairs as a linear dictionary. Its method <code>get</code> takes a key and a dictionary, and either returns <code>Just x</code> where <code>x</code> is the value corresponding to the key, or <code>Nothing</code> if the key was not found.

Change the username to "John", either in the program or in the output view.
Remark that now, the select box for pizzas takes the choice number 0. If you change this choice, it does not yet create an entry for "John", but instead modifies the default choice! In a sense this is normal. It changes the zero where it is defined.

## Create a new user entry
To create a new entry whenever a new user makes a choice c, it should not change this default value, but instead push back <code>Just c</code> to the call <code>listDict.get user userdata</code>.
If the value previously computed was <code>Nothing</code>, updating will have the effect of inserting the new entry in the dictionary.  
@t.replace("""choice = ... Nothing -> 0""")<|
"""choice = listDict.get user userdata
         |> Update.lens {
           apply = case of
             Just c -> c
             Nothing -> 0
           update {outputNew} = Ok (Inputs [Just outputNew])
         }
"""

Try now to change the preference for the user "John". It creates an entry in the dictionary that can even be updated. Nice!

## Simplify the code
This is a situation so common that we designed a primitive for such a lens.  
@t.replace("Update.lens ... }")<|"""Maybe.withDefaultReplace 0"""
If you now change the username and select a pizza, it will now offer two possible updates: either modify the default value, or create an entry for this user.
To make sure it does not modify this default value, we can freeze it.  
@t.replace("""Maybe.withDefaultReplace 0""")<|
"""Maybe.withDefaultReplace (freeze 0)"""

## Deleting preferences
The last step at this point is to let user delete their data.
For that, if a user selects the first choice (the one that says "@chooseyourpizza"), we want to push back <code>Nothing</code> to the call of <code>listDict.get</code>. In this case, <code>listDict.get</code> deletes the key/value pair, this is a defined behavior.
@t.replace("""Maybe.withDefaultReplace (freeze 0)""")<|
"""Maybe.orElseReplace (freeze (Just 0))
         |> Maybe.getUnless 0"""

The explanation is the following. <code>Maybe.orElse</code> takes another option (either <code>Nothing</code> or <code>Just x</code>) and returns it if the one before is <code>Nothing</code>. In the reverse direction, the variant <code>Maybe.orElseReplace</code> has the special effect that it can push a <code>Nothing</code> to the one before if we push back a <code>Nothing</code>.

<code>Maybe.getUnless</code> takes a <code>Just x</code> and returns <code>x</code>.
In the reverse direction, if the new x is the argument of <code>Maybe.getUnless</code> (in our case, 0), then this function pushes back <code>Nothing</code>.

Try it. Change the username, select a pizza, then entry is added to <code>userdata</code>.
Select the option @chooseyourpizza again, the entry is deleted.

# Translate in two languages.
If you want to precisely know how translation works, follow the <a href="https://mikaelmayer.github.io/TutorialMemory.html">last part of the Memory Tutorial</a>.
However, for now, you can just do the following.

@t.replace(placeholder 3)<|
"""dictionary = [
  ("English", [
              ]),
  ("French", [
               ])
]
indexLang = 1"""
@t.replace("<span>Hello @user!<br>")<|"""Html.translate dictionary indexLang <|
  <span>Hello @@user! @@Html.select[](List.map Tuple.first dictionary)(indexLang)<br>"""

Now just replace in the output "Hello" by "{:Hello:}" and update the program.
A few more explanations:
<ul>
<li>It creates two entry named <code>Hello1</code> in the French and English dictionaries.</li>
<li>It replaces the text "Hello" by "$Hello1" in the code.</li>
<li>In the output, it simply displays the current translation "Hello"</li>
<li>If you switch the language to "French", it still displays "Hello"</li>
<li>In the output, rename "Hello" to "Salut". It modifies the entry in French.</li>
</ul>
That concludes this tutorial.
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

chooseyourpizza = "Choose your pizza"
pizzasPlaceholder = "Pizzas here!"
placeholder i = """-- We will write code here #@i"""

q3 = "\"\"\""

t = tutorialUtils
