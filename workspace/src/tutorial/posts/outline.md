#Tentative User Study Materials Outline
The objective is to have a series of exercises that introduce the features of the tool in an acessible manner. By the end of the exercises, users should be well acquatined with the tool and feel like they are equipped to recreate all or almost all of the examples that come with the program.

Examples we want to be sure to work through:
- Three boxes (our hello_world!)
- Sketch-n-Sketch Logo
- Ferris Wheel

Examples that are also in the paper:
- Chicago Flag
- Chicago Botanic Gardens logo
- Active Trans logo
- Custom UI Widgets

If we reasonably expect them to work on it for an average of an hour a day for maybe five days over the course of seven days that we give it to them, then we could shoot for five hour-long exercises (lessons? should work out the verbage) that each has multiple exercises that keep the user interested. However, I think we should also leave a little bit of an extra buffer just in case they get really into it. I don't think we should expect any one to get that into it and go through them all, but it would be good to have them (and maybe for the future, if we open it to a broader audience).

Concepts:
- How to use the interface
- How to specify shapes in `little`
	- Simple shapes (`rect`, `circle`, `ellipse`)
	- More complicated shapes (`line`, `ngon`?)
	- Arbitrary paths (`path` and how to specify 'Q', etc)
- How to use maps, other sorts of FP structures in `little`
- Using case and if statements
- How to setup and manipulate 'indirect' parameters (like the delta in the logo)
- Freezing/Thawing constants
- Advanced structuring of the program to get certain zones to manipulate certain parameters
- Making UI widgets
- Nuances of 'inlining' parameters
- Nuances of z-ordering
- svgViewBox and readying files for 'export'
- Limitations when using control flow (case/if)
- Using the svg thin wrapper to add non-standard attributes

##Step 1
Primary Example: Three Boxes

Objective: Basic familiarity with the interface and simplest syntax features

Syntax to introduce:
- `svg`
- `let` or `def`
- `rect`
- `\()` (lambda)
- `[]` (lists)

##Step 2
Primary Example: Sketch-n-Sketch Logo, Chicago Flag

Objective: Understanding how to setup 'indirect' parameters to be manipulated,
thawing/freezing, and the 'Group Box' pattern.

Syntax to introduce:
- `let [pattern]` (further reinforce)
- `!/?`
- `nStar`
- Color Numbers `(0-500)`

##Step 3
Primary Example: Ferris Wheel

Objective: Reinforce previous lessons and understand how UI Widgets work and can
be used to extend the built-in widgets.

Syntax to introduce:
- `hSlider`
- `line`
- Trig functions
- `concat` 

Note: Suggested project could include a more full featured color picker (RGBA,
for example).

##Step 4
Primary Example: Active Trans? Sailboat? Botanic Gardens? Eye Icon?

Objective: Understand how `path` works, and the particular concerns that come with it

Syntax to introduce:
- `path`

##Step 5
Primary Example: Sample Rotations? A random SVG from the web?

Objective: Understand how to use the 'thin wrapper' syntax to define SVG nodes that aren't built in

Syntax to introduce:
- `svgViewBox`
- `['nodeType' [attrs] [children]]`

##Bonus Step A
Primary Example: Sailboat? Solar System?

Objective: Be inspired by more 'dynamic' and sophisticated examples

Notes:
- Be sure to mention that this is bonus
- The idea here is that the UI Widgets can be leveraged to make certain types of graphics that are dependent upon time (but aren't really animations) much easier to produce than otherwise

##Bonus Step B
Primary Example: Bar Graph? Line Graph?

Objective: Be inspired by the data visualization possiblities of the tool

Notes:
- Be sure to mention that this is more experimental right now (no dataset import syntax, data tranformation/sorting/etc is unwieldy)
- The idea here is that the potential exists for toolkits to be made (for example, the bar graph that we made) that, when then applied to view data, can provide a nice way to quickly look at the aspect of the data the user would like to zone in on
