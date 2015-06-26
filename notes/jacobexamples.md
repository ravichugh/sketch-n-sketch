##Flag of Chicago Example

Version 1: Looks like flag, but interactivity is slow and wonky
(some zones also don't seem to be reacting)

```

(let nstar
  (\(n cx cy len1 len2 rot)
    (let pti
      (\[i len]
        (let anglei (+ rot (/ (* i (pi)) n))
        (let xi (+ cx (* len (cos anglei)))
        (let yi (+ cy (* len (sin anglei)))
          [xi yi]))))
    (let lengths
      (map
        (\b
          (if b
            len1
            len2))
        (concat  (repeat n [true false])))
    (let indices (list0N  (- (* 2! n) 1!))
      (polygon 'red' 'DUMMY' 0 (map pti (zip indices lengths)))))))
(let upright (/ (* 3! (pi)) 2!)
(let [x0 y0 space sep ni nj pts lstripe wstripe] [108 113 454! 145 0! 3! 6! 50 60]
(let [outerLen innerLen] [50 20]
(let stripes
  (map
    (\i
      (rect
        'lightblue'
        lstripe
        (* y0 i)
        (+ x0 space)
        wstripe))
    [1! 3!])
  (svg 
    (append
      stripes
      (map
        (\i
          (let off (* i sep)
            (nstar pts (+ x0 off) (+ y0 sep) outerLen innerLen upright)))
        (range ni nj)))))))))

```

Version 2: With outer box

```
```

(let nstar
  (\(n cx cy len1 len2 rot)
    (let pti
      (\[i len]
        (let anglei (+ rot (/ (* i (pi)) n))
        (let xi (+ cx (* len (cos anglei)))
        (let yi (+ cy (* len (sin anglei)))
          [xi yi]))))
    (let lengths
      (map
        (\b
          (if b
            len1
            len2))
        (concat  (repeat n [true false])))
    (let indices (list0N  (- (* 2! n) 1!))
      (polygon 'red' 'DUMMY' 0 (map pti (zip indices lengths)))))))
(let upright (/ (* 3! (pi)) 2!)
(let [x0 y0 sep ni nj pts lstripe wstripe] [108 113 145 0! 3! 6! 50 30]
(let [outerLen innerLen] [50 20]
(let stripes
  (map
    (\i
      (rect
        'lightblue'
        lstripe
        (* y0 i)
        (* x0 5.2)
        (* 2 wstripe)))
    [1! 3!])
  (svg 
    (concat
      [(rect 'white' )
      stripes
      (map
        (\i
          (let off (* i sep)
            (nstar pts (+ x0 off) (+ y0 sep) outerLen innerLen upright)))
        (range ni nj)))))))))

```

##Pie Chart Example

Version 1 : Sliced circle, interacting with central zones of lines does not seem to be propagating upwards

```
(let [x y rad] [350 250 175]
(let slice (\(xend yend) (line 'white' 6 x y xend yend))
  (svg
    [(circle 'orange' x y rad)
    (slice (+ x rad) y)
    (slice x (- rad y))
    (slice (- x rad) y)])))
```
Version 2 : Updated using polar coordinates and angles, scales but lines don't move

```

(let toRadian
  (\a
    (* (/ (pi) 180!) a))
(let [x y rad] [350 250 175]
(let cut 
  (\ang
    (let xend (* rad (cos ang))
    (let yend (* rad (sin ang))
    (line 'white' 6 x y (+ x xend) (+ y yend)))))
(let angles [0 45 90 180]
(let radangs (map toRadian angles)
(let cuts (map cut radangs)
  (svg
    (append [(circle 'orange' x y rad)] cuts))))))))

```

Version 3: Allowed pie cuts to be moveable

```

(let toRadian
  (\a
    (* (/ (pi) 180!) a))
(let [x y rad] [350 250 175]
(let cut 
  (\ang
    (let xend (* rad (cos ang))
    (let yend (* rad (sin ang))
    (line 'white' 6 (+ x rad) (+ y rad) (+ x xend) (+ y yend)))))
(let angles [0 45 90 180]
(let radangs (map toRadian angles)
(let cuts (map cut radangs)
  (svg
    (append [(circle 'orange' (+ x rad) (+ y rad) rad)] cuts))))))))

```

## Frank Lloyd Wright Example

http://www.artic.edu/aic/collections/citi/images/standard/WebLarge/WebImg_000207/123332_2318933.jpg

Version 1: First pass using polygons

```
(let [w x y z] [200 300 400 500]
(let bwpoly (polygon 'white' 'black' 3)
  (svg
    [(bwpoly [[110 120] [130 140] [w x] [y z]])
    (bwpoly [[110 120] [130 140] [w x] [y z]])
    (bwpoly [[110 120] [130 140] [w x] [y z]])
    (bwpoly [[110 120] [130 140] [w x] [y z]])])))
```

Version 2: Working Copy

Interesting idea, possibility of making topigraphical maps?

```
(let [x1 x2 x3 x4 x5 x6 x7 x8] [43 170 295 544 417 783 183 649]
(let [y1 y2 y3 y4 y5 y6 y7 y8] [45 154 270 376 446 860 213 328]
(let bwpoly (polygon 'white' 'black' 3)
  (svg 
    [(bwpoly  [[x1 y6] [x1 y1] [x6 y1] [x6 y6]])
     (bwpoly  [[x1 y1] [x5 y7] [x3 y3] [x1 y2]])
     (bwpoly  [[x6 y1] [x5 y7] [x4 y3] [x6 y2]])
     (bwpoly  [[x5 y7] [x3 y3] [x5 y8] [x4 y3]])
     (bwpoly  [[x1 y4] [x3 y3] [x5 y8] [x7 y5]])
     (bwpoly  [[x6 y4] [x4 y3] [x5 y8] [x8 y5]])]))))
```

Version 3: Code cleanup and adding additional shapes and colors

```
(let [x1 x2 x3 x4 x5 x6 x7 x8] [43 170 295 544 417 783 183 649]
(let [y1 y2 y3 y4 y5 y6 y7 y8] [45 154 270 376 446 860 213 328]
(let bwpoly (polygon 'white' 'black' 3)
(let blkline (\[[a b] [c d]] (line 'black' 3 a b c d))
  (svg 
    (append
      (map
        bwpoly
        [[[x1 y6] [x1 y1] [x6 y1] [x6 y6]]
         [[x1 y1] [x5 y7] [x3 y3] [x1 y2]]
         [[x6 y1] [x5 y7] [x4 y3] [x6 y2]]
         [[x5 y7] [x3 y3] [x5 y8] [x4 y3]]
         [[x1 y4] [x3 y3] [x5 y8] [x7 y5]]
         [[x6 y4] [x4 y3] [x5 y8] [x8 y5]]])
      (map blkline [[[x7 y5] [x7 y6]] [[x8 y5] [x8 y6]]])))))))
```

Version 4: Flatter copy + black shapes

```
(let [x1 x2 x3 x4 x5 x6 x7 x8] [24 170 258 565 414 812 154 677]
(let [y1 y2 y3 y4 y5 y6 y7 y8] [44 101 177 251 288 778 142 216]
(let bwpoly (polygon 'white' 'black' 3)
(let bbpoly (polygon 'black' 'black' 3)
(let blkline (\[[a b] [c d]] (line 'black' 3 a b c d))
  (svg 
    (concat
      [(map
        bwpoly
        [[[x1 y6] [x1 y1] [x6 y1] [x6 y6]]
         [[x1 y1] [x5 y7] [x3 y3] [x1 y2]]
         [[x6 y1] [x5 y7] [x4 y3] [x6 y2]]
         [[x5 y7] [x3 y3] [x5 y8] [x4 y3]]
         [[x1 y4] [x3 y3] [x5 y8] [x7 y5]]
         [[x6 y4] [x4 y3] [x5 y8] [x8 y5]]])
      (map blkline [[[x7 y5] [x7 y6]] [[x8 y5] [x8 y6]]])
      (map
        bbpoly
        [[[x3 y6] [x3 y5] [x5 y4] [x4 y5] [x4 y6]]
         [[x1 y3] [x7 y2] [x7 y3] [x1 y4]]
         [[x6 y3] [x8 y2] [x8 y3] [x6 y4]]])])))))))
```

And without black shapes:

```
(let [x1 x2 x3 x4 x5 x6 x7 x8] [64 170 280 555 419 794 186 649]
(let [y1 y2 y3 y4 y5 y6 y7 y8] [45 99 154 214 256 860 125 180]
(let bwpoly (polygon 'lightyellow' 'black' 3)
(let blkline (\[[a b] [c d]] (line 'black' 3 a b c d))
  (svg 
    (append
      (map
        bwpoly
        [[[x1 y6] [x1 y1] [x6 y1] [x6 y6]]
         [[x1 y1] [x5 y7] [x3 y3] [x1 y2]]
         [[x6 y1] [x5 y7] [x4 y3] [x6 y2]]
         [[x5 y7] [x3 y3] [x5 y8] [x4 y3]]
         [[x1 y4] [x3 y3] [x5 y8] [x7 y5]]
         [[x6 y4] [x4 y3] [x5 y8] [x8 y5]]
         [[x3 y6] [x3 y5] [x5 y4] [x4 y5] [x4 y6]]
         [[x1 y3] [x7 y2] [x7 y3] [x1 y4]]
         [[x6 y3] [x8 y2] [x8 y3] [x6 y4]]])
      (map blkline [[[x7 y5] [x7 y6]] [[x8 y5] [x8 y6]]])))))))
```

##Slider Bar Example
Building a slider bar that can be used to toggle other parameters

```
(let [x y w1 h1 w2 cx cy] [41 360 500! 50! 50! 213 50!]
(let bar (rect 'gray' x y w1 h1)
(let slider (if (< cx (+ x w1)) (rect 'lightgray' cx y w2 h1) (rect 'lightgray' (+ x w1) y w2 h1))
  (svg  [bar slider]))))
```

```
(let [x y w1 h1 w2 cx cy] [125 346 500! 50! 50! 421 50!]
(let bar (rect 'gray' x y w1 h1)
(let slider
  (if (< cx (+ x w1))
    (if (< cx x)
      (rect 'lightgray' x y w2 h1)
      (rect 'lightgray' cx y w2 h1))
    (rect 'lightgray' (+ x w1) y w2 h1))
  (svg  [bar slider]))))
```
```
(let [x0 y0 min max dim cx] [80! 400! 70! 300! 50! 80]
    (let [sx sy] [309 216]
    (let samplecirc (circle 'orange' sx sy cx)
    (let button (\\n (square 'lightgray' n y0 dim))
    (let bar (rect 'gray' x0 y0 max dim)
    (let slider
      (if (< cx max)
        (if (< min cx)
          (button (+ cx x0))
          (button x0))
        (button (+ x0 max)))
      (svg  [samplecirc bar slider])))))))
```

(let toRadian
      (\\a
        (* (/ (pi) 180!) a))
    (let [sx sy rad] [350 250 175]
    (let cut 
      (\\ang
        (let xend (* rad (cos ang))
        (let yend (* rad (sin ang))
        (line 'white' 6 x y (+ x xend) (+ y yend)))))
    (let [a1 a2 a3 a4] [0 45 90 180]
    (let radangs (map toRadian [a1 a2 a3 a4])
    (let cuts (map cut radangs)
    (let samplecirc (circle 'orange' sx sy rad)
    (let [x0 y0 min max dim] [80! 400 0! 360! 50!]
    (let button (\\n (square 'lightgray' n y0 dim))
    (let bar (rect 'gray' x0 y0 max dim)
    (let slider
      (if (< a1 max)
        (if (< min a1)
          (button (+ a1 x0))
          (button x0))
        (button (- (+ x0 max) dim)))
      (svg  [samplecirc cuts bar slider]))))))))))))
##American Colonial Flag Example

(let nstar
    (\\(n cx cy len1 len2 rot)
      (let pti
        (\\[i len]
          (let anglei (+ rot (/ (* i (pi)) n))
          (let xi (+ cx (* len (cos anglei)))
          (let yi (+ cy (* len (sin anglei)))
            [xi yi]))))
      (let lengths
        (map
          (\\b
            (if b
              len1
              len2))
          (concat  (repeat n [true false])))
      (let indices (list0N  (- (* 2! n) 1!))
        (polygon 'orange' 'DUMMY' 0 (map pti (zip indices lengths)))))))
    (let rotate (\\a (/ (* (+ 9! a) (pi)) 6!))
    (let [x0 y0 wstripe xoff yoff ni nj pts xstripe hstripe radius] [108 20 500! 120 100 0! 12! 5! 52 20 55]
    (let [outerLen innerLen] [10 4]
    (let block (rect '#09096d' xstripe y0 (/ wstripe 3) (* 7 hstripe))
    (let stripes
      (map
        (\\i (rect 'red' xstripe (* hstripe i) wstripe hstripe))
        [1! 3! 5! 7! 9! 11! 13!])
    (let base (append stripes [block])
      (svg 
        (append
          base
          (map
            (\\i
                (nstar
                  pts
                  (+ xoff (* radius (cos (rotate  i))))
                  (+ yoff (* radius (sin (rotate  i))))
                  outerLen
                  innerLen
                  (rotate  i)))
          (range ni nj)))))))))))

##Modern American Flag example

(let nstar
    (\\(n cx cy len1 len2 rot)
      (let pti
        (\\[i len]
          (let anglei (+ rot (/ (* i (pi)) n))
          (let xi (+ cx (* len (cos anglei)))
          (let yi (+ cy (* len (sin anglei)))
            [xi yi]))))
      (let lengths
        (map
          (\\b
            (if b
              len1
              len2))
          (concat  (repeat n [true false])))
      (let indices (list0N  (- (* 2! n) 1!))
        (polygon 'orange' 'DUMMY' 0 (map pti (zip indices lengths)))))))
    (let upright (/ (* 3! (pi)) 2!)
    (let [x0 y0 wstripe sep ni nj pts xstripe hstripe radius] [108 20 500! 45 0! 49! 5! 52 20 80]
    (let [outerLen innerLen] [10 4]
    (let block (rect '#09096d' xstripe y0 (* wstripe (/ 2 5)) (* 7 hstripe))
    (let stripes
      (map
        (\\i (rect 'red' xstripe (* hstripe i) wstripe hstripe))
        [1! 3! 5! 7! 9! 11! 13!])
    (let base (append stripes [block])
      (svg 
        (append
          base
          (map
            (\\i
              (let off (* i sep)
                (nstar
                  pts
                  (+ x0 off)
                  (+ y0 sep)
                  outerLen
                  innerLen
                  upright)))
          (range ni nj)))))))))))

(let [x0 y0 wstripe sep ni nj pts xstripe hstripe radius] [108 20 500! 45 0! 49! 5! 52 20 80]
    (let [outerLen innerLen] [10 4]
    (let block (rect '#09096d' xstripe y0 (* wstripe (/ 2 5)) (* 7 hstripe))
    (let stripes
      (map
        (\\i (rect 'red' xstripe (* hstripe i) wstripe hstripe))
        [1! 3! 5! 7! 9! 11! 13!])
    (let base (append stripes [block])
      (svg 
        (append
          base
          (map
            (\\i
              (let off (* i sep)
                (circle
                  'orange'
                  (+ x0 off)
                  (+ y0 sep)
                  (- outerLen innerLen))))
          (range ni nj)))))))))

(let [x0 y0 sep ni nj pts wstripe hstripe radius] [108 20 20! 0! 12! 5! 500 20 55]
    (let [outerLen innerLen] [10 4]
    (let block (rect '#09096d' x0 y0 (* wstripe (/ 2 5)) (* 7 hstripe))
    (let stripes
      (map
        (\\i (rect 'red' x0 (+ y0 (* i sep)) wstripe hstripe))
        [0! 2! 4! 6! 8! 10! 12!])
    (let base (append stripes [block])
      (svg 
        (append
          base
          (map
            (\\[i j]
                (circle
                  'orange'
                  (+ x0 (* i sep))
                  (+ y0 (* j sep))
                  (- outerLen innerLen)))
          (cartProd (range 0 9) (range 0 4)))))))))

##French Sudan Flag

(let [x0 y0 w h r] [50 29 150 300 40]
(let stripe (\[color x] (rect color x y0 w h))
(let figline (\[[a b] [c d]] (line 'black' (/ r 4) a b c d))
(let [x1 x2 x3] [348 450 550]
(let [y1 y2 y3 y4 y5] [100 120 150 190 247]
  (svg 
    (append
      (map stripe [['blue' x0] ['orange' (+ x0 w)] ['red' (+ x0 (* 2 w))]])
      (map
        figline
        [[[x2 y1] [x2 y4]]
         [[x1 y2] [x1 y3]]
         [[x1 y3] [x3 y3]]
         [[x3 y2] [x3 y3]]
         [[x1 y5] [x1 y4]]
         [[x1 y4] [x3 y4]]
         [[x3 y4] [x3 y5]]]))))))))

(let [x0 y0 w h r] [50 30 150 300 20]
    (let xoff (+ x0 w)
    (let yoff (+ y0 (/ h 4))
    (let stripe (\\[color x] (rect color x y0 w h))
    (let figline (\\[[a b] [c d]] (line 'black' (/ r 2) a b c d))
    (let [x1 x2 x3] [(+ xoff 25) (+ xoff 75) (+ xoff 125)]
    (let [y1 y2 y3 y4] [yoff (+ yoff 45) (+ yoff 115) (+ yoff 150)]
      (svg 
        (append
          (map stripe [['blue' x0] ['white' (+ x0 w)] ['red' (+ x0 (* 2 w))]])
          (snoc
            (circle 'black' x2 y1 r)
            (map
              figline
              [[[x1 y1] [x1 y2]]
               [[x1 y2] [x3 y2]]
               [[x3 y1] [x3 y2]]
               [[x1 y4] [x1 y3]]
               [[x1 y3] [x3 y3]]
               [[x3 y3] [x3 y4]]
               [[x2 y1] [x2 y3]]])))))))))))

(let [x0 y0 w h r] [50 30 150 300 20]
    (let xoff (+ x0 w)
    (let yoff (+ y0 (/ h 4))
    (let stripe (\\[color x] (rect color x y0 w h))
    (let figline (\\[[a b] [c d]] (line 'black' (/ r 2) a b c d))
    (let [x1 x2 x3] [(+ xoff 25) (+ xoff 75) (+ xoff 125)]
    (let [y1 y2 y3 y4] [yoff (+ yoff 45) (+ yoff 115) (+ yoff 150)]
      (svg 
        (append
          (map stripe [['blue' x0] ['white' (+ x0 w)] ['red' (+ x0 (* 2 w))]])
          (snoc
            (ellipse 'black' x2 y1 (/ w 10) (/ h 15))
            (map
              figline
              [[[x1 y1] [x1 y2]]
               [[x1 y2] [x3 y2]]
               [[x3 y1] [x3 y2]]
               [[x1 y4] [x1 y3]]
               [[x1 y3] [x3 y3]]
               [[x3 y3] [x3 y4]]
               [[x2 y1] [x2 y3]]]))))))))))))

(let [x0 y0 w h r] [50 30 150 300 20]
    (let stripe (\\[color x] (rect color x y0 w h))
    (let figline (\\[[a b] [c d]] (line 'black' (/ r 2) a b c d))
    (let [x1 x2 x3] (map (\\n (+ x0 (* w n))) [2 2.3 2.6])
    (let [y1 y2 y3 y4] (map (\\n (+ y0 (/ h n))) [4.3 3.3 1.9 1.4])
      (svg 
        (append
          (map stripe [['blue' x0] ['white' (+ x0 w)] ['red' (+ x0 (* 2 w))]])
          (snoc
            (ellipse 'black' x2 y1 (/ w 7.5) (/ h 15))
            (map
              figline
              [[[x1 y1] [x1 y2]]
               [[x1 y2] [x3 y2]]
               [[x3 y1] [x3 y2]]
               [[x1 y4] [x1 y3]]
               [[x1 y3] [x3 y3]]
               [[x3 y3] [x3 y4]]
               [[x2 y1] [x2 y3]]]))))))

##Frank Lloyd Wrigth 2

(let [x0 x1 x2 x3 x4 x5 x6 x7 x8] [50 100 150 200 250 300 350 400 450]
    (let [y0 y1 y2 y3 y4 y5 y6 y7 y8] [50 100 150 200 250 300 350 400 450]
    (let bluepoly (polygon 'blue' 'black' 3)
    (let redpoly (polygon 'red' 'black' 3)
    (let ypairup (\\(a bs) (map (\\b [a b])))
    (let xpairup (\\(a bs) (map (\\b [b a])))
    (let rowtop (xpairup y0 [x0 x1 x2 x3 x4 x5 x6 x7 x8])
    (let rowbot (xpairup y8 [x0 x1 x2 x3 x4 x5 x6 x7 x8])
    (let colleft (ypairup x0 [y0 y1 y2 y3 y4 y5 y6 y7 y8])
    (let colright (ypairup x8 [y0 y1 y2 y3 y4 y5 y6 y7 y8])
    (let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
      (svg 
        (map blkline
          (append
            (zip rowtop rowbot)
            (zip colleft colright)))))))))))))))

(let [x0 y0] [50 50]
    (let [w1 w2 w3 w4 w5 w6 w7 w8] [(+ x0 50) (+ w1 50) (+ w2 50) (+ w3 50) (+ w4 50) (+ w5 50) (+ w6 50) (+ w7 50)]
    (let [y0 y1 y2 y3 y4 y5 y6 y7 y8] [50 100 150 200 250 300 350 400 450]
    (let bluepoly (polygon 'blue' 'black' 3)
    (let redpoly (polygon 'red' 'black' 3)
    (let ypairup (\\(a bs) (map (\\b [a b]) bs))
    (let xpairup (\\(a bs) (map (\\b [b a]) bs))
    (let rowtop (xpairup y0 [w1 w2 w3 w4 w5 w6 w7 w8])
    (let rowbot (xpairup y8 [w1 w2 w3 w4 w5 w6 w7 w8])
    (let colleft (ypairup x0 [y0 y1 y2 y3 y4 y5 y6 y7 y8])
    (let colright (ypairup x8 [y0 y1 y2 y3 y4 y5 y6 y7 y8])
    (let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
      (svg
        (append
          (map blkline
            (append
              (zip rowtop rowbot)
              (zip colleft colright)))
          (append
            (map redpoly 
              [[[x0 y0] [x1 y0] [x1 y1] [x0 y1]]
              [[x1 y1] [x2 y1] [x2 y2] [x1 y2]]
              [[x2 y2] [x3 y2] [x3 y3] [x2 y3]]])
            (map (\\[x y r] (circle 'yellow' x y r)) 
              [[x5 y2 x1] [x5 y5 x0] [x5 y7 25]]))
        ))))))))))))))

(let [x0 y0 w h max] [50 50 50 50 10!]
(let blkline (\[[a b] [c d]] (line 'black' 3 a b c d))
(let redpoly (polygon 'red' 'black' 3)
(let dimension [0! 1! 2! 3! 4! 5! 6! 7! 8! 9! 10!]
(let verticals
  (zip
    (map (\n [(+ x0 (* w n)) y0]) dimension)
    (map (\n [(+ x0 (* w n)) (+ y0 (* h max))]) dimension))
(let horizontals
  (zip
    (map (\n [x0 (+ y0 (* h n))]) dimension)
    (map (\n [(+ x0 (* w max)) (+ y0 (* h n))]) dimension))
  (svg
    (map blkline
      (append verticals horizontals)))
))))))

(let [x0 y0 w h max] [50 50 50 50 10!]
(let blkline (\[[a b] [c d]] (line 'black' 3 a b c d))
(let redpoly (polygon 'red' 'black' 3)
(let dimension [0! 1! 2! 3! 4! 5! 6! 7! 8! 9! 10!]
(let xoff (\n (+ x0 (* w n)))
(let yoff (\n (+ y0 (* h n)))
(let verticals
  (zip
    (map (\n [(xoff n) y0]) dimension)
    (map (\n [(xoff n) (+ y0 (* h max))]) dimension))
(let horizontals
  (zip
    (map (\n [x0 (yoff n)]) dimension)
    (map (\n [(+ x0 (* w max)) (yoff n)]) dimension))
  (svg
    (append
      (map redpoly
        [[[(xoff 0!) (yoff 0!)] [(xoff 1!) (yoff 0!)] [(xoff 1!) (yoff 1!)] [(xoff 0!) (yoff 1!)]]])
      (map blkline
        (append verticals horizontals))))
))))))))

(let [x0 y0 w h max] [50 50 50 50 10!]
    (let xoff (\\n (+ x0 (* w n)))
    (let yoff (\\n (+ y0 (* h n)))
    (let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
    (let redpoly (\\[a b] (polygon 'red' 'black' 3 
      [[(xoff a) (yoff a)] [(xoff a) (yoff b)] [(xoff b) (yoff b)] [(xoff b) (yoff a)]]))
    (let dimension [0! 1! 2! 3! 4! 5! 6! 7! 8! 9! 10!]
    (let verticals
      (zip
        (map (\\n [(xoff n) y0]) dimension)
        (map (\\n [(xoff n) (+ y0 (* h max))]) dimension))
    (let horizontals
      (zip
        (map (\\n [x0 (yoff n)]) dimension)
        (map (\\n [(+ x0 (* w max)) (yoff n)]) dimension))
      (svg
        (append
          (map blkline
            (append verticals horizontals))
          (append
            (map redpoly
              [[0 1] [1 2] [2 3] [3 4]])
            (map (\\[x y r] (circle 'yellow' x y r)) 
              [[(xoff 6) (yoff 3) (* 3 w)] [(xoff 6) (yoff 7) (* 2 w)] [(xoff 6) (yoff 9) w]]))))))))))))

(let [x0 y0 w h max] [72 72 45 56 10!]
    (let xoff (\\n (+ x0 (* w n)))
    (let yoff (\\n (+ y0 (* h n)))
    (let redpoly
      (\\[a b]
        (polygon
          'red'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
    (let dimension [0! 1! 2! 3! 4! 5! 6! 7! 8! 9! 10!]
    (let verticals
      (zip
        (map (\\n [(xoff  n) y0]) dimension)
        (map (\\n [(xoff  n) (+ y0 (* h max))]) dimension))
    (let horizontals
      (zip
        (map (\\n [x0 (yoff  n)]) dimension)
        (map (\\n [(+ x0 (* w max)) (yoff  n)]) dimension))
      (svg 
        (append
          (map blkline (append verticals horizontals))
          (append
            (append
              (map redpoly [[0 1] [1 2] [2 3] [3 4]])
              (map (\\[x y] (ellipse 'blue' x y (* w 4) h))) [[(xoff 2) (yoff 7)]]))
            (map
              (\\[x y r] (circle 'yellow' x y r))
              [[(xoff  6) (yoff  2) (+ w h)]
               [(xoff  6) (yoff  7) max]
               [(xoff  6) (yoff  5) (/ (+ w h) 2)]])))))))))))

(let [x0 y0 w h max] [72 72 45 56 10!]
    (let xoff (\\n (+ x0 (* w n)))
    (let yoff (\\n (+ y0 (* h n)))
    (let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
    (let redpoly
      (\\[a b]
        (polygon
          'red'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
    (let bwpoly
      (\\[a b]
        (polygon
          'white'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
    (let [d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10] [0 1 2 3 4 5 6 7 8 9 10]
      (svg 
        (append
          (map bwpoly [[d0 d1] [d1 d2] [d2 d3] [d3 d4] [d4 d5] [d5 d6] [d6 d7] [d7 d8] [d8 d9] [d9 d10]])
          (append
            (append
              (let [p0 p1 p2 p3 p4] [0 1 2 3 4]
                (map redpoly [[p0 p1] [p1 p2] [p2 p3] [p3 p4]]))
              (map (\\[x y] (ellipse 'blue' x y (* w 4) h)) [[(xoff 5) (yoff 9)]]))
            (map
              (\\[x y r] (circle 'yellow' x y r))
              [[(xoff  6) (yoff  2) (+ w h)]
               [(xoff  6) (yoff  7) max]
               [(xoff  6) (yoff  5) (/ (+ w h) 2)]]))))))))))))

(let [x0 y0 w h max] [72 72 45 56 10!]
    (let xoff (\\n (+ x0 (* w n)))
    (let yoff (\\n (+ y0 (* h n)))
    (let redpoly
      (\\[a b]
        (polygon
          'red'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
    (let bwpoly
      (\\[a b]
        (polygon
          'white'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
      (svg 
        (append
          (let [d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10] [0 1 2 3 4 5 6 7 8 9 10]
            (map bwpoly [[d0 d1] [d1 d2] [d2 d3] [d3 d4] [d4 d5] [d5 d6] [d6 d7] [d7 d8] [d8 d9] [d9 d10]]))
          (append
            (append
              (let [p0 p1 p2 p3 p4] [0 1 2 3 4]
                (map redpoly [[p0 p1] [p1 p2] [p2 p3] [p3 p4]]))
              (map (\\[x y] (ellipse 'blue' x y (* w 4) h))) [[(xoff 5) (yoff 9)]]))
            (map
              (\\[x y r] (circle 'yellow' x y r))
              [[(xoff  6) (yoff  2) (+ w h)]
               [(xoff  6) (yoff  7) max]
               [(xoff  6) (yoff  5) (/ (+ w h) 2)]]))))))))))

(let [x0 y0 w h max] [72 72 45 56 10!]
    (let xoff (\\n (+ x0 (* w n)))
    (let yoff (\\n (+ y0 (* h n)))
    (let redpoly
      (\\[a b]
        (polygon
          'red'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
    (let bwpoly
      (\\[a b]
        (polygon
          'white'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
      (svg 
        (append
          (let [d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10] [0 1 2 3 4 5 6 7 8 9 10]
            (map bwpoly [[d0 d1] [d1 d2] [d2 d3] [d3 d4] [d4 d5] [d5 d6] [d6 d7] [d7 d8] [d8 d9] [d9 d10]]))
          (append
            (let [p0 p1 p2 p3 p4] [0 1 2 3 4]
              (map redpoly [[p0 p1] [p1 p2] [p2 p3] [p3 p4]]))
            (map
              (\\[x y r] (circle 'yellow' x y r))
              [[(xoff  6) (yoff  2) (+ w h)]
               [(xoff  6) (yoff  7) max]
               [(xoff  6) (yoff  5) (/ (+ w h) 2)]])))))))))

(let [x0 y0 ni nj pts w h rad] [108 20 0! 12! 5! 500 20 6]
    (let block (rect '#09096d' x0 y0 (* w (/ 2! 5!)) (* 7! h))
    (let stripes
      (map
        (\\i (rect 'red' x0 (+ y0 (* i h)) w h))
        [0! 2! 4! 6! 8! 10! 12!])
    (let base (append stripes [block])
      (svg 
        (append
          base
          (map
            (\\[i j]
              (let xsep (/ w 25!)
                (circle
                  'white'
                  (+ x0 (* i xsep))
                  (+ y0 (* j h))
                  rad)))
          (append (cartProd (range 0.5 6.5) (range 0.75 4.75)) (cartProd (range 1 5) (range 1 4))))))))))


(let [x0 y0 w h max] [69 55 53.2 74.4 10!]
(let xoff (\\n (+ x0 (* w n)))
(let yoff (\\n (+ y0 (* h n)))
(let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
(let redpoly
  (\\[a b]
    (polygon
      'red'
      'black'
      3
      [[(xoff  a) (yoff  a)]
       [(xoff  a) (yoff  b)]
       [(xoff  b) (yoff  b)]
       [(xoff  b) (yoff  a)]]))
(let dimension
  [0!
   1
   2
   2.9
   2.4
   1.5
   9.1
   7.9
   8.2
   8.7
   10!]
(let verticals
  (zip
    (map (\\n [(xoff  n) y0]) dimension)
    (map (\\n [(xoff  n) (+ y0 (* h max))]) dimension))
(let horizontals
  (zip
    (map (\\n [x0 (yoff  n)]) dimension)
    (map (\\n [(+ x0 (* w max)) (yoff  n)]) dimension))
  (svg 
    (append
      (map blkline (append verticals horizontals))
      (append
        (append
          (let [p0 p1 p2 p3 p4] [0 1 2 2.9 5]
            (map redpoly [[p0 p1] [p1 p2] [p2 p3] [p3 p4]]))
          (map (\\[x y] (ellipse 'blue' x y (* w 4) h)) [[(xoff  5) (yoff  9)]]))
        (map
          (\\[x y r] (circle 'yellow' x y r))
          [[(xoff  6) (yoff  1.75) (+ w h)]
           [(xoff  6) (yoff  7) (/ (+ w h) 4)]
           [(xoff  6) (yoff  5) (/ (+ w h) 2)]]))))))))))))

(let nstar
    (\\(n cx cy len1 len2 rot)
      (let pti
        (\\[i len]
          (let anglei (+ rot (/ (* i (pi)) n))
          (let xi (+ cx (* len (cos anglei)))
          (let yi (+ cy (* len (sin anglei)))
            [xi yi]))))
      (let lengths
        (map
          (\\b
            (if b
              len1
              len2))
          (concat  (repeat n [true false])))
      (let indices (list0N  (- (* 2! n) 1!))
        (polygon 'white' 'DUMMY' 0 (map pti (zip indices lengths)))))))
    (let rotate (\\a (/ (* (+ 9! a) (pi)) 6!))
    (let [x0 y0 ni nj pts w h] [108 20 0! 12! 5! 500 260]
    (let hstripe (/ h 13!)
    (let [blockw blockh] [(/ w 3!) (* 7! hstripe)]
    (let min
      (if (< blockw blockh)
        (* 0.4! blockw)
        (* 0.4! blockh))
    (let [outerLen innerLen] [10 4]
    (let block (rect '#09096d' x0 y0 blockw blockh)
    (let stripes
      (map
        (\\i (rect 'red' x0 (+ y0 (* i hstripe)) w hstripe))
        [0! 2! 4! 6! 8! 10! 12!])
    (let base (append stripes [block])
      (svg
        (cons (rect 'white' (- x0 10) (- y0 10) (+ w 15) (+ h 15))
        (append
          base
          (map
            (\\i
                (nstar
                  pts
                  (+ (+ x0 (/ w 6!)) (* min (cos (rotate  i))))
                  (+ (+ y0 (* hstripe 3.5!)) (* min (sin (rotate  i))))
                  outerLen
                  innerLen
                  (rotate  i)))
          (range ni nj)))))))))))))))

(let [x0 y0 ni nj pts w h rad] [108 20 0! 12! 5! 500 200 6]
    (let hstripe (/ h 13!)
    (let block (rect '#09096d' x0 y0 (* w (/ 2! 5!)) (* 7! hstripe))
    (let stripes
      (map
        (\\i (rect 'red' x0 (+ y0 (* i hstripe)) w hstripe))
        [0! 2! 4! 6! 8! 10! 12!])
    (let base (append stripes [block])
      (svg
        (cons (rect 'white' (- x0 10) (- y0 10) (+ w 20) (+ h 20)) 
        (append
          base
          (map
            (\\[i j]
              (let xsep (/ w 15!)
              (let ysep (* hstripe 1.3)
                (circle
                  'white'
                  (+ x0 (* i xsep))
                  (+ y0 (* j ysep))
                  rad))))
          (append (cartProd (range 0.5 5.5) (range 0.75 4.75)) (cartProd (range 1 5) (range 1.2 4.2)))))))))))

(let nstar
    (\\(n cx cy len1 len2 rot)
      (let pti
        (\\[i len]
          (let anglei (+ rot (/ (* i (pi)) n))
          (let xi (+ cx (* len (cos anglei)))
          (let yi (+ cy (* len (sin anglei)))
            [xi yi]))))
      (let lengths
        (map
          (\\b
            (if b
              len1
              len2))
          (concat  (repeat n [true false])))
      (let indices (list0N  (- (* 2! n) 1!))
        (polygon 'red' 'DUMMY' 0 (map pti (zip indices lengths)))))))
    (let upright (/ (* 3! (pi)) 2!)
    (let [x0 y0 space sep ni nj pts w h] [108 113 145 0! 3! 6! 454 250]
    (let [outerLen innerLen] [25 10]
    (let stripes
      (map
        (\\i
          (rect
            'lightblue'
            x0
            (* y0 i)
            w
            (/ h 5!)))
        [1! 3!])
      (svg 
        (append
          stripes
          (map
            (\\i
              (let off (* i sep)
                (nstar pts (+ x0 off) (+ y0 sep) outerLen innerLen upright)))
            (range ni nj)))))))))