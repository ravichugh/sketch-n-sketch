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
* Tie event handlers only to the children SVGs
* ~~SelectObject takes an ID and a zone keyword~~
* MovingObj keeps track of what type of zone is selected
* MouseDown code only changes attributes that are manipulable by that zone
  - Util function to propagate attribute changes to children SVGs along with
  parent needed
* MouseDown gives an outline to the proper zone

## Syncing

* Have Model track inputExp
* Create function to generate static/not manipulable text box and image
* Have Model track if we're in a manipulable state or not
* Insert Sync button
* Have Sync event call sync on inputExp, inputVal, and workingVal
* Modify View code to generate stack of static renderings upon being called in
static mode, which are in possibleChanges
* Have static rendering come along with a button and an ID
* Make new Event called SelectChoice or something to that effect that takes a
choice ID
* Have SelectChoice make the chosen element of possibleChanges the new
input/working quantities and clear out the old possibilities

## Cleanup
* Remove Debug.log from buildSvg and upstate
* Clean "|" code out
* Make ZoneType type instead of using Strings
