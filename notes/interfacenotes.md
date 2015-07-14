# TODO

* catch eval errors
* retain whitespace in parsing
* parse error messages
* local storage for edits and options
* rotation zones
* color picker zones
* text zones
* little options
* freeze annotations on variables (and uses)
* scrollbars / zoom
* ball of clay tool
* svg to little tool
* defs and other thin wrapper
* highlight zone assignments
* cursors based on zone assignment

* better structural diff

* refactor data structs / tryToBeSmart
* symbolic trigger solutions

* ~~option to revert changes~~
* ~~structure changing updates~~
* ~~comments in source code~~
* ~~allow \verb+!+ by default instead of \verb+?+~~
* ~~scratchpad~~
* ~~paths~~
* ~~fix SVG format~~
* ~~more kinds of plausible updates...~~
* ~~rename Interface and Main files~~
* ~~\verb+(def x e1) e2 === (let x e1 e2)+ ~~
* ~~valOfAPath2 bug~~
* ~~logo~~
* ~~fav icon~~
* ~~JavaScript catch-all~~
* ~~manually change font in JS again~~
* ~~print zone/locset~~
* ~~separate little files~~
* ~~string attribute bug~~
* ~~zone bug with ferris/pie~~
* ~~parse errors in zone info area~~
* ~~remove MicroTests from dropdown~~

# Things to do

* ~~Move index attribute into attrs for Objects~~
* ~~Move shape attribute into attrs for Objects~~
* ~~Have MouseDown events also update WorkingVal~~
  - ~~Write util function to update a val given an ID~~
  - ~~Make sure given IDs are unique~~
  - ~~Have SelectObject send the correct ID over its event handler~~
  - ~~Call util function to perform updates in MouseDown code~~

## Zones

* Create children SVGs along with the parent Objects
  - ~~Create center SVG~~
  - Create border SVGs
* Tie event handlers only to the children SVGs
  - ~~Center SVG~~
  - Border SVGs
* ~~SelectObject takes an ID and a zone keyword~~
* MovingObj keeps track of what type of zone is selected
* MouseDown code only changes attributes that are manipulable by that zone
  - ~~Util function to propagate attribute changes to children SVGs along with
  parent needed~~ (Obseleted by change of approach)
* MouseDown gives an outline to the proper zone

## Syncing

* ~~Have Model track inputExp~~
* ~~Create function to generate static/not manipulatable text box and image~~
  - Still need to make visuals non-manipulatable
* ~~Have Model track if we're in a manipulable state or not~~
* ~~Hook Sync button up to events~~
* ~~Have Sync event call sync on inputExp, inputVal, and workingVal~~
* ~~Modify View code to generate stack of static renderings upon being called in
static mode, which are in possibleChanges~~
  - Move Select button to proper location
  - Do further testing on other microtests
  - Beautify and fix overlaps/ugliness
* Have static rendering come along with a button and an ID
* ~~Make new Event called SelectOption or something to that effect that takes a
choice ID~~
* ~~Have SelectOption make the chosen element of possibleChanges the new
input/working quantities and clear out the old possibilities~~

## Cleanup
* Remove Debug.log from buildSvg and upstate
* Clean "|" code out
* Make ZoneType type instead of using Strings

## Other
* Eval.run should return Val with outer "svg"?
  - LangSvg.valToIndexedTree assumes this, must wrap in "svg" otherwise 
* Sync.sync takes vals - is it a problem if they are dumped from an IndexedTree?
* ~~Could use IndexedTreeNode -> Val function;~~
  - ~~The order of the Vals that go into sync matter, right? So just dumping the
    dictionary doesn't make sense. However, that means that we still need to
    keep track of an intermediate Val, which we said that the IndexedTree would
    prevent. Actually, we _should_ be able to reconstruct the original Val from
    the dict, so long as we can order the children of any given svg node
    properly.~~
* ~~Split files: view, zones ~~
* ~~Change high level data representations~~
* Create more nuanced data representation for attr { List (String,String)}

## Implement more nuanced Textbox functionality
* Syntax Highlighting
  - Need some agreement on how we want to do this

