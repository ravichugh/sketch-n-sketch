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