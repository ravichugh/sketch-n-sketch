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
(let [x0 y0 sep ni nj pts xrect] [108 113 145 0! 3! 6! 46]
(let [outerLen innerLen] [50 20]
(let stripes
  (map
    (\i
      (rect
        'lightblue'
        xrect
        (mult y0 i)
        (mult x0 5.2)
        (mult 2 (- outerLen innerLen))))
    [1! 3!])
  (svg 
    (append
      stripes
      (map
        (\i
          (let off (mult i sep)
            (nstar pts (+ x0 off) (+ y0 sep) outerLen innerLen upright)))
        (range ni nj)))))))))

```

