# Implementation Notes for retarget-html Branch

The intent behind this document is to note all of the major changes we made to
the 'vanilla' Sketch-n-Sketch source to enable HTML output instead of SVG
output. Further, this document will hope 

## Target Goals

* Zones for Padding/Margin
* Traces on Strings for Textarea Zones
* Element Abstraction fully realized
* 'Pausing' the canvas to allow editing in different states
* Zoom/pan in iframe, can be done with CSS 
* Allow for flow / snapping zone interaction as seen in Javascript/SASS [Layout Grid](https://clippings.github.io/layout-grid/)

## Theory

In this section we explain each of the concepts we wanted to employ as well as the rationale behind each one.

#### Re-orient the Sketch-n-sketch SVG tool to Dorian, an HTML/CSS tool
1. Rationale:
	* The existing Sketch-n-sketch architecture opens up possibilities for manipulation of other file types
	* HTML is a similar output format to SVG, making it a good fit for the existing infrastructure
	* Web pages are another example of where programmatic specification is lacking or bulky (Elm, Django, JavaScript) and direct manipulation (Dreamweaver) isn't particularly expressive, compostable, or extensible

#### Rebuilt Basic Zone Implementation
1. Rationale:
	* Zones were originally implemented as Svg's, with the switch to Html, zones were switched as well.
	* In HTML, there is a lot more state that can be kept track of which was not as immediately relevant in SVG graphics. Namely, states like 'hover', 'visited', and more very commonly change the attributes that would be directly manipulated by Zones. Thus, Zones must be changed to capture this information

#### Element Abstraction

1. Why do we need an element abstraction?
	* Since width & heights are known, useful abstractions such as Flows are possible
	* Flow is an extremely common pattern when designing webpages of all levels of complexity (Elm, etc.)
	* Leads to a nice program layout with a series of definitions that all get passed to a Flow function

2. With Elements, we get a nice pipelined style for building styled html objects, as opposed to the 'monolithic' style that we went with with SVG. This is important for the style in which we go about defining the Prelude functions.

#### Element Versions of HTML Nodes

In general, we've been requiring the `width` and `height` arguments for the element versions of the node functions. The position for all members of the Element abstraction should be set to `absolute`, but it is most helpful if this is done when the `top` and `left` attributes are set. Since it's expected that all these functions will get passed to a styling function, it's possible that this can be done automatically there.

#### Flow

Flow is an extremely common pattern when it comes to the design of webpages of all levels of complexity. Just like it is employed in Elm, it is helpful to implement it in Dorian to allow for its use. Due to the untyped nature of Little, using Flows and Elements is more of a convention that does nothing to limit what the programmer can do while being a very useful tool for placing multiple nodes in a particular relation to each other. Further, combined with conditionals, Flows could be used to help layout multiple versions (e.g. mobile, desktop, tablet) of a website with exactly the same nodes and styles. This helps to alleviate some of the major pain points that come with specifying HTML by hand or multiple pages in a direct manipulation editor.

### CSS Pseudoselectors

With the way that the attributes on the SVG nodes were done, we could not capture information that would allow the changing of the style attributes of HTML nodes based on events like 'hover'. CSS pseudoselectors solve this problem, but at the cost of not being able to use all inline styles like we did with SVG. So, we decided to go with a syntax that allows attributes to be selected in different 'modes'. So,

`['top' 100]` corresponds to `top: 100px` on the 'default' version of that element, and

`['top:hover' 50]` corresponds to `top: 50px` on the 'hover' version of that element. 

So, if these styles were applied to the same div, we would get a combined CSS document including:

```
#autogen-style-name-X { top: 100px }
#autogen-style-name-X:hover { top: 100px }
```

## Practice

This section explains the implementation details of the above concepts.

* For each section:
  * Relevant Functions
  * Caveats for said functions
  * Why any strangeness exists
  * What hasn't been implemented yet, but is/was intended to be

#### Re-orient the Sketch-n-sketch SVG tool to Dorian, an HTML/CSS tool
1. Implementation:
	* Retooled LangSvg.elm to LangHtml.elm, consisting mostly of:
		* Changing all functions arguments that dealt with Svg nodes to Html nodes
		* Changed buildSvg function to buildHtml (See InterfaceView2.elm line 125)
		* Changed display canvas to be an embedded iframe (See InterfaceView2.elm line 128)
2. Things left to implement:
	* See Zones

#### Rebuilt Basic Zone Implementation
1. Implementation:
	* Removed / Commented out existing zone implementation in the second half of InterfaceView2 & InterfaceController
	* Added new basic zone creation functions into InterfaceView2.elm (see lines ~212-270)
	* Basic Zones implemented for divs, imgs, tables.
2. Things left to implement:
	* Return of Color Zones and other slider-based zones

### Element Abstraction in Prelude
1. With Elements, we get a nice pipelined style for building styled html objects
	* One can invoke the element version of a function by using e + function (eDiv, eImg, eTable, etc.)
	* A width and a heigh must be provided to this function
		* arg order: width height (children or source)
	* This can then be pipelined to an element styling function (eStyle) with takes a list of key-value styles as arguments
		* Example:

			```
			; Three Divs
			(def threeDivsInt
			  (let [left0 top0 w h sep] [40 28 60 130 110]
			  (let divi (\i
			    (let lefti (+ left0 (* i sep))
			    (eStyle [ [ 'top'  top0  ]
			              [ 'left' lefti ]
			              [ 'background-color' 'lightblue' ] ]
			            (eDiv w h []) ) ) )
			  (basicDoc [] (map divi [0! 1! 2!])) ) ) )
			```

#### Element Versions of HTML Nodes

```
;; eStyle : Attributes -> Node -> Node
;; argument order - attrs to add, node to add styles to
;; Adds a list of attributes to a node - is helpful when using the Element
;; abstraction, as the constructor functions do not have a Style field.
;; Attributes in this case should be CSS
;; TODO: deal with double instances of the same attr?
(def eStyle (\(newAttrs [node attrs children])
  [node
    (append newAttrs attrs )
    children ] ) )

;; eDiv : Width -> Height -> Children -> Node
;; argument order - width, height, initial children
;; Make a Div that has a specified width and height so as to be compatible with
;; the Element abstraction
(def eDiv (\(w h initialChildren)
  (eStyle [ ['width' w] ['height' h] ['position' 'absolute' ] ]
    [ 'div' [] initialChildren ] ) ) )

;; eImg : Width -> Height -> Src -> Node
;; argument order - width, height, source URL
;; Make an image element with the specified width and height
(def eImg (\(w h url)
  ['img' [ ['content' (+ 'url("' (+ url '")'))] 
           ['width' w] ['height' h]
           ['position' 'absolute' ] ] []] ) )

;; eTable : Width -> Height -> Data -> Attributes -> Node
;; argument order - width, height, data
;; make a simple table with a specified width and height
  (def eTable (\(w h headers data attrs)
   ['table' 
    (append attrs [ ['width' w] 
                    ['height' h]
                    ['position' 'absolute'] ])
     (append [(tableheader attrs attrs headers)]
             (map (\d (tr [] attrs d)) data) ) ] ) )

;; eComplexTable : Width -> Height -> Data -> Attributes -> Node
;; argument order - width, height, data
;; make a complex table with a specified width and height
;; first set of attrs goes to headers, second to tr, third to td
  (def eComplexTable (\(w h headers data hattrs rattrs dattrs)
   ['table' 
    (append rattrs [ ['width' w] 
                    ['height' h]
                    ['position' 'absolute'] ])
     (append [(tableheader hattrs dattrs headers)]
             (map (\d (tr rattrs dattrs d)) data) ) ] ) )
```

#### Implementations of Flow

Currently, we only have one implementation of Flow in one example (ordering). This section will be updated when its syntax is finalized.

The syntax guidelines that we have been going with have been that all information that is needed for the flow should be passed in the form of separate arguments, if possible. So, for a `flowDown` function an appropriate type signature would be:

```
flowDown : Spacing (Int) -> Initial Offset (Int) -> List Elements
```

Where the `flowDown` function is meant to flow elements down vertically starting from a `y` offset and space them out according the the spacing argument. There is just enough information passed to the function to get the job done with a minimum of attributes assumed about the inputs.

### Collection of CSS Styles

CSS styles are collected at the `printHtml` phase of the rendering process. Namely, in `printNode` here:

https://github.com/ravichugh/sketch-n-sketch/blob/retarget-html/src/LangHtml.elm#L279

the `getAllClasses` function is called that walks the DOM and creates new CSS entries for everything that needs it. This function is here:

https://github.com/ravichugh/sketch-n-sketch/blob/retarget-html/src/LangHtml.elm#L300

This does *not* link to Zones at all yet, which needs to be worked on. Currently Zones do not capture any information pertaining to the state of the webpage that is being edited.
