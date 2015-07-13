##Bar graph

```
(letrec indexedzip_
  (\(acc xs)
    (case xs
      ([x | xx] [[acc x] | (indexedzip_ (+ acc 1) xx)])
      (_ [])))
(let indexedzip (\xs (indexedzip_ 0 xs))
(let [x0 y0 sep] [28 28 20]
(let xaxis (rect 'black' (- x0 8) (+ y0 400) 800 5)
(let yaxis (rect 'black' (- x0 8) y0 5 400)
(letrec range
  (\(b t)
    (if (< b t)
      [b | (range (+ b 1) t)]
      [t]))
(let data (map (\a (sin a)) (map (\b (* b 0.25)) (range 0 40)))
(let bars
  (map
    (\[i j] (rect 'lightblue' (+ x0 (* i sep)) (+ y0 (- 400 (* 100 j))) 20 (* 100 j)))
    (indexedzip  data))
  (svg  [xaxis yaxis | bars])))))))))
```

##Graph (nodes and edges)

```
(let node (\[x y] (circle 'lightblue' x y 20))
(let edge (\[[x y] [i j]] (line 'lightgreen' 5 x y i j))
(letrec genpairs
  (\xs
    (case xs
      ([x y | xx] [[x y] | (append (genpairs  (cons x xx)) (genpairs  (cons y xx)))])
      ([x] [])
      ([] [])))
(let pts [[200 50] [400 50] [100 223] [200 389] [400 391] [500 223]]
(let nodes (map node pts)
(let pairs (genpairs  pts)
(let edges (map edge pairs)
  (svg  (append edges nodes)))))))))
```

##Fractal Tree

Generate the branches by starting with a child list, then recursively generating children until number of steps is depleted.

```
; A fractal tree
(defrec mod (\(x m) (if (< x m) x (mod (- x m) m))))
(def nsin (\n (if (< n (/ 3.14159 2)) (sin n) (cos (mod n (/ 3.14159 2))))))
(def ncos (\n (if (< n (/ 3.14159 2)) (cos n) (sin (mod n (/ 3.14159 2))))))
(def [initwd initlen] [10! 150!])
(def [steps stepslider] (hSlider true 20! 420! 550! 3! 8! 'Steps ' 4))
(def [bendn bendnslider] (hSlider true 20! 420! 580! 1! 8! 'Bend ' 1))
(def initangle (/ 3.14159! 2!))
(def bend (/ 3.14159! bendn))
(defrec exp (\(base pow)
  (if (< pow 1) 1 (* base (exp base (- pow 1))))))
(def mkleftx (\(stepnum theta px) 
  (- px (* (/ initlen stepnum) (ncos (+ theta (* (exp 0.5 stepnum) bend)))))))
(def mkrightx (\(stepnum theta px)
  (+ px (* (/ initlen stepnum) (ncos (- theta (* (exp 0.5 stepnum) bend)))))))
(def mklefty (\(stepnum theta py)
  (- py (* (/ initlen stepnum) (nsin (+ theta (* (exp 0.5 stepnum) bend)))))))
(def mkrighty (\(stepnum theta py)
  (- py (* (/ initlen stepnum) (nsin (- theta (* (exp 0.5 stepnum) bend)))))))
(defrec genchildren (\(stepnum maxstep theta px2 py2) 
  (if (< maxstep stepnum) 
    [] 
    (append 
      [ (line 'black' (/ initwd stepnum) px2 py2 
          (mkleftx stepnum theta px2)
          (mklefty stepnum theta py2))
        (line 'black' (/ initwd stepnum) px2 py2
          (mkrightx stepnum theta px2)
          (mkrighty stepnum theta py2))]
      (append
        (genchildren (+ stepnum 1) maxstep (+ theta (* (exp 0.5 stepnum) bend))
          (mkleftx stepnum theta px2)
          (mklefty stepnum theta py2))
        (genchildren (+ stepnum 1) maxstep (- theta (* (exp 0.5 stepnum) bend))
          (mkrightx stepnum theta px2)
          (mkrighty stepnum theta py2)))))))
(def trunk (line 'black' initwd 210 400 210 250))
(def branches (genchildren 2 steps initangle 210 250))
(svg (concat [ [ trunk | branches ] bendnslider stepslider]))
```

##Solar System
A representation of the solar system that can be advanced through time, where the relative positions of the planets in their orbits are correct as well as the relative radii of the orbits themselves.

```
; Visualization of the solar system 
(def aupx 12)
(def [ox oy] [200 400])
; Relative radii of the planet orbits, in au
(def [ merorb venorb earorb marorb juporb satorb uraorb neporb ] 
     [ 0.387! 0.723! 1! 1.524! 5.203! 9.539! 19.18! 30.06! ]
)
; Relative orbital period to the Earth
(def [ meryr venyr earyr maryr jupyr satyr urayr nepyr ]
     [ 0.2409! 0.616! 1! 1.9! 12! 29.5! 84! 165! ]
)
; Function to place a body
(def planet (\(color orb yr radius)
  (\t (circle color  (+ ox (* aupx (* orb (cos (* t (/ 6.28318 yr))))))
                       (+ oy (* aupx (* orb (sin (* t (/ -6.28318 yr))))))
                       radius))))
; Visual for each body
; Each takes a time to be displayed at
(def sun (circle 'yellow' ox oy 10))
(def mercury (planet 'lightred'   merorb meryr 4))
(def venus   (planet 'orange'     venorb venyr 5))
(def earth   (planet 'green'      earorb earyr 5))
(def mars    (planet 'red'        marorb maryr 4))
(def jupiter (planet 'brown'      juporb jupyr 6))
(def saturn  (planet 'sandybrown' satorb satyr 6))
(def uranus  (planet 'blue'       uraorb urayr 6))
(def neptune (planet 'darkblue'   neporb nepyr 6))
; Visual for the rings
(def rings (reverse (map (\orb (ring 'lightgrey' 2! ox oy (* aupx orb)))
                [ merorb venorb earorb marorb juporb satorb uraorb neporb ])))
(def [time timeslider] (hSlider true 20! 600! 20! 1! 1000! 'Day ' 1))
(def rev (\(x f) (f x)))
(def planets (map (rev (/ time 365)) [mercury venus earth mars jupiter saturn uranus neptune]))
(svg (concat [ rings [sun | planets] timeslider ]))
```

##2D Slider
A two dimensional slider in a similar style to the slider that already exists.

```
(def xySlider_
  (\(dropBall roundInt xStart xEnd yStart yEnd minx maxx miny maxy xcaption ycaption curx cury)
    (def [rCorner wEdge rBall] [4! 3! 10!])
    (def [xDiff yDiff xValDiff yValDiff] [(- xEnd xStart) (- yEnd yStart) (- maxx minx) (- maxy miny)])
    (def ballx (+ xStart (* xDiff (/ (- curx minx) xValDiff))))
    (def bally (+ yStart (* yDiff (/ (- cury miny) yValDiff))))
    (def ballx_ (clamp xStart xEnd ballx))
    (def bally_ (clamp yStart yEnd bally))
    (def rball_ (if dropBall (if (< maxx curx) 0 rBall) rBall))
    (def rball__ (if dropBall (if (< maxy cury) 0 rball_) rBall))
    (def xval
      (def xval_ (clamp minx maxx curx))
      (if roundInt (round xval_) xval_))
    (def yval
      (def yval_ (clamp miny maxy cury))
      (if roundInt (round yval_) yval_))
    (def shapes
      [ (line 'black' wEdge xStart yStart xEnd yStart)
        (line 'black' wEdge xStart yStart xStart yEnd)
        (line 'black' wEdge xStart yEnd xEnd yEnd)
        (line 'black' wEdge xEnd yStart xEnd yEnd)
        (circle 'black' xStart yStart rCorner)
        (circle 'black' xStart yEnd rCorner)
        (circle 'black' xEnd yStart rCorner)
        (circle 'black' xEnd yEnd rCorner)
        (circle 'black' ballx_ bally_ rball__)
        (text (- (+ xStart (/ xDiff 2)) 40) (+ yEnd 20) (+ xcaption (toString xval)))
        (text (+ xEnd 10) (+ yStart (/ yDiff 2)) (+ ycaption (toString yval))) ])
  [ [ xval yval ] shapes ]))
(def xySlider (xySlider_ false))
(def [ [ a b ] slider ] (xySlider false 20! 420! 20! 420! 0! 100! 0! 100! 'X Axis: ' 'Y Axis: ' 20 20))
(svg slider)
```

##Color Picker with 2d Slider

Because color theory is subtle and complex, however compelling, I'm going to do two separate 2D sliders - one for RG and the other for BA and see how that looks.

```
; The slider
(def xySlider_
  (\(dropBall roundInt xStart xEnd yStart yEnd minx maxx miny maxy xcaption ycaption curx cury)
    (def [rCorner wEdge rBall] [4! 3! 10!])
    (def [xDiff yDiff xValDiff yValDiff] [(- xEnd xStart) (- yEnd yStart) (- maxx minx) (- maxy miny)])
    (def ballx (+ xStart (* xDiff (/ (- curx minx) xValDiff))))
    (def bally (+ yStart (* yDiff (/ (- cury miny) yValDiff))))
    (def ballx_ (clamp xStart xEnd ballx))
    (def bally_ (clamp yStart yEnd bally))
    (def rball_ (if dropBall (if (< maxx curx) 0 rBall) rBall))
    (def rball__ (if dropBall (if (< maxy cury) 0 rball_) rBall))
    (def xval
      (def xval_ (clamp minx maxx curx))
      (if roundInt (round xval_) xval_))
    (def yval
      (def yval_ (clamp miny maxy cury))
      (if roundInt (round yval_) yval_))
    (def shapes
      [ (line 'black' wEdge xStart yStart xEnd yStart)
        (line 'black' wEdge xStart yStart xStart yEnd)
        (line 'black' wEdge xStart yEnd xEnd yEnd)
        (line 'black' wEdge xEnd yStart xEnd yEnd)
        (circle 'black' xStart yStart rCorner)
        (circle 'black' xStart yEnd rCorner)
        (circle 'black' xEnd yStart rCorner)
        (circle 'black' xEnd yEnd rCorner)
        (circle 'black' ballx_ bally_ rball__)
        (text (- (+ xStart (/ xDiff 2)) 40) (+ yEnd 20) (+ xcaption (toString xval)))
        (text (+ xEnd 10) (+ yStart (/ yDiff 2)) (+ ycaption (toString yval))) ])
  [ [ xval yval ] shapes ]))
(def xySlider (xySlider_ false))
;
; RG and BA sliders
(def [ [ red green ] rgSlider ] (xySlider true 20! 200! 20! 200! 0! 255! 0! 255! 'Red: ' 'Green: ' 78 215))
(def [ [ blue alpha ] baSlider ] (xySlider true 20! 200! 240! 430! 0! 255! 0! 255! 'Blue: ' 'Alpha: ' 220 100))
(def colorBall (circle [ red green blue alpha ] 400 220 100!))
(svg (cons colorBall (append rgSlider baSlider)))
```

##Phasic Things
Waves, rotation, and phase are all things that are difficult to capture in a normal graphical editor and even more difficult to manipulate. This could provide some compelling examples for both normal direct manipulation as well as sliders.

Possible examples:
* A boat on a sea, waves and boat's position on top is determined using trig
* Interference pattern, as visualized by changing alpha of a radial or linear gradient. Frequency of emission sources could be manipulated with sliders.
* Rotation of a cube, with shading easily determined by value of one rotational parameter (unidirectional light)
* Turning a page in a book (would require some math for the curve at the edge of the page)
* A bouncing ball with path shown
* A hanging spring or a swinging pendulum, where the size of the block determines the 'mass' and the resulting period/aplitude

##Relate and introduceParameter
Two example use cases:
* User places three otherwise identical boxes in a near-horizontal line, then asks the program to 'relate' them. Ideally, the program should deduce that the only varying parameters (the x and y positions of the boxes) can be modeled (like with a best fit line) by introducing a few parameters. Namely, by introducing a named constant for the y position that all three boxes share and by introducing two named constants for the x position that are a part of a linear function (an x0 and a separation, effectively).
* User creates a larger, more complicated shape/graphic by building it up from smaller polygons (such as using triangles and rectangles to make the city skyline in the Active Trans Logo example). These are ultimately inteded to be one shape, so the user then 'groups' the shapes and fuses them into one polyline. The user then selects the points that makes up the skyline (just the top ones) and asks the program to 'relate' them. Ideally, the program should output an option where a parameter was introduced into the y values of the points, where the other points are assigned as offsets to the 'base' point. Then, if the user picks this choice, they would be able to manipulate the skyline as in the example.

###introduceParameter
Perhaps in the case when the Vals passed to the 'relate' function have exactly the same structure, the algorithm could consist of successive best fits on each of the constants at a leaf of the Val. So, in the case of the boxes, there are two parameters on which fits are done, the x and the y (the other attributes, being identical, presumably are already related by a named constant). The fits done are in increasing complexity, from linear to quadratic, to more exotic ones like logarithmic and fourier. The r^2 values for these could then act as a ranking to determine which ones are to be higher ranked than the others.

These fits could also be compared to a few special ones that human users will tend to place along, like vertical and horizontal lines. grids and circles might be hard to detect and parameterize, but they are also common placement patterns that would be good to detect.

Work in progress:

```latex

If $V_i \sim V_n$ for $ i \in [0,m], n \in [0,m] $,
$relate(V_0, ..., V_m) = \{ V^0, ..., V^K \}$ where 

$V^N \in introduceParameter(n_0, ..., n_m) $ for $ n \in V_i $.

$n$ is meant to be a constant in the input $V_i$ Vals. The arguments to $introduceParameter()$ are meant to be the constants that correspond to the same 'structural location' in all of the input $V_i$. $introduceParameter()$ returns a set of $V^N$, each of which is a set of replacement Vals that correspond to one possible parameter introduction. So, that's where any fitting might happen. The parameter introductions potentially result in new expressions $e_i$ that replace the previous $n_i$ in the $V_i$. The options that are then shown are all the substitutions that correspond to the elements of the $V^N$.

Could this be expressed with our current substitution notation? To complete this specification, I think I need to look at exactly how we did things in the technical paper.

```

I think this can be done with the currently existing substitution syntax, as I think it allows substitution of any numeric constant `$n^l$` with any Val `$V$`. So, we could substitute constants with expressions. One thing we are missing, however, is something that would allow the insertion of new expressions altogether. This comes into play when a parameter is introduced into the program and there is no natural place to put the name for it. For instance, in the `little` program below:

```clojure
(def [x0 w h] [20 50 50])
(def box1 'blue' x0 100 w h)
(def box2 'blue' (* x0 3) 110 w h)
(def box3 'blue' (* x0 7) 90 w h)
(svg [box1 box2 box3])
```

>A parameter that should be introduced is into the `y` field of the boxes, and a natural place to put it is in the def at the top of the program. Thus, we can use the substitution syntax that already exists to replace the final constant in the substitution in the AST with an expression that includes that constant along with another substitution. However, in the program below:

Actually, I don't think that this is true. Substitutions in the AST still only happen at numeric literals, and I don't think this can be extended to include the naming of new parameters.

```clojure
(def box1 'blue' 20 100 50 50)
(def box2 'blue' 60 110 50 50)
(def box3 'blue' 140 90 50 50)
(svg [box1 box2 box3])
```

>There exists no place to make such a parameter introduction. There is no location in the AST that corresponds to a numeric constant that can be replaced to include the naming of another variable.

See above.

What we need is a syntax for substituting expressions in the AST. If we allow an 'empty' expression that can have a location that corresponds to the start of the program, why not:

```latex
Define \emph{Expression Substitution} as $\varepsilon(e^{l_0}, ..., e^{l_n}) = \varepsilon_0$ such that:

$\varepsilon_0 e = e'$ where $e^{l_i} \in e'$ is such that $e^{l_i} = e^{l_n} \forall i \in [0,n]$.
```

Alternately, we could just extend the definition of substitution to include expression substitution.

Then, we run into another limitation of our current notation, which is that traces don't include location information about expressions, only numeric constants. In order to encode structure changing updates, I think we need to somehow capture that information in such a way that the `relate` and `introduceParameter` functions can use them.

To introduce location information for expressions into traces, why not simply add another field to expressions in traces? So:

```latex
Before: trace $t = (* n^{l_a} (+ n^{l_b} n^{l_c}))$

Now: trace $t = (* l_n n^(l_a) (+ l_m n^{l_b} n^{l_c}))$ where $l_n, l_m$ are locations in the AST in the usual definition.
```

A naive method of using this would be to just pass the entire AST to `relate` along with the Vals that it's attempting to relate. The Vals, which have traces that include the locations of each expression, can be used to deduce expression substitutions that will satisfy some sort of constraints that capture the 'close' output that we're looking for. Then, `relate` would return multiple new ASTs that would be presented to the user to choose between.

###Fitting in introduceParameter

Starting with a very simple case, we could make a few guesses as to how best to model the numerical constants given:

* As a single constant `a`
* As a linear function `a * i + b`
* As a quadratic function `a * i^2 + b * i + c`
* etc.

For each guess, we could order the constants from least to greatest, assign an index to them based on this ordering, then perform a 'best fit' (likely via some form of regression). Then, we would order each option by some parameter of 'closeness' (like an r^2 value, for example). This approach assumes/generates the very typical pattern:

```clojure
(map placementFunction (range 0 n))
```

Where `placementFunction` is only for one parameter (like the `y` value in the previous example). This could work well for the common case of a sequence of placements along a line or other parameters that are linearly/quadratically related. What this misses is inter-variable dependence, might generate different orderings for different parameters of the shape (which might be able to be resolved by scanning before and pre-selecting one ordering or a few probable orderings to go into all relationship guesses), and at this juncture needs a template function for each possible shape.

To get an example going; in the limited case of performing a parameter introduction on one known attribute for a group of shapes that are all the same shape, `introduceParameter` might take the form:

```elm
type alias FitParameter = Num
type alias Index = Num 
type FitFunction = (FitParameter -> Index -> Val)
                 | (FitParameter -> FitParameter -> Index -> Val)
                 | ...
                 | (FitParameter_0 -> ... -> FitParameter_(N-1) -> Index -> Val)
type TemplateFunction = FitFunction -> (Index -> Val)
-- Ex. templateFunction1 linearFit = map (\y -> square 10 (linearFit a b y) 7)
-- a and b deduced from guessLinear

introduceParamater : List Val -> Expression -> List Expression
introduceParameter valsToRelate inputProgram =
  let thingsToFit = getQuantities valsToRelate
      -- Each element of list is (constantPlacementFunction, const)
      constantGuesses = map (\(tFunc, ns) -> guessConstant tFunc ns) thingsToFit
      -- Each element of list is (linearPlacementFunction, slope, intercept)
      linearGuesses = map (\(tFunc, ns) -> guessLinear tFunc ns) thingsToFit
      ...
      -- Substitution helpers
      makeAssignments : List (FitParameter, Name) -> Expression
      makeAssignments = --Whatever the internal syntax is for (def [name1, name2] [param1, param2])...
      insertPlacementFuncs : (Index -> Val) -> Expression
      insertPlacementFuncs = --Whatever the internal syntax is for
          -- (def placementFunction (\(name0 ... nameN) (shape ...)))
          -- (def shapes (map placementFunction (range 0 M)))
      -- Actually perform substitution
      ...
  -- Return new, substituted expressions
  in concat [ ... ]
  
getQuantities : List Val -> List (TemplateFunction, List Num)

guessConstant : TemplateFunction -> List Num -> (Index -> Val, (FitParameter, Name(?)))
guessLinear : TemplateFunction -> List Num -> (Index -> Val, (FitParameter, Name(?)), (FitParameter, Name(?)))
guess<NParamsNeeded> : TemplateFunction -> List Num -> (Index -> Val, FitParameter_0, ..., FitParameter_(N-1))
```

Before I get too much more into the weeds, I think some conference is necessary to figure out if this is really the way that we want to go with this.
