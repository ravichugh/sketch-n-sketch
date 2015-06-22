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
    (append
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

```
(let [x1 x2 x3 x4 x5 x6 x7 x8] [45 170 298 544 417 783 183 649]
(let [y1 y2 y3 y4 y5 y6 y7 y8] [45 170 267 385 446 860 205 328]
(let bwpoly (polygon 'white' 'black' 3)
  (svg 
    [(bwpoly  [[x1 y6] [x1 y1] [x6 y1] [x6 y6]])
     (bwpoly  [[x1 y1] [x5 y7] [x3 y3] [x1 y2]])
     (bwpoly  [[x6 y1] [x5 y7] [x4 y3] [x6 y2]])
     (bwpoly  [[x5 y7] [x3 y3] [x5 y8] [x4 y3]])
     (bwpoly  [[x1 y4] [x3 y3] [x5 y8] [x7 y5]])
     (bwpoly  [[x6 y4] [x4 y3] [x5 y8] [x8 y5]])]))))
```