##Bar graph
```
(let [x0 y0 sep] [28 28 60]
(svg 
  (map
    (\[i j] (rect 'red' (+ x0 (mult i sep)) (+ y0 (- 400 (mult 100 j))) 50 (mult 100 j)))
    [[0 2] [1 4] [2 1]])))
```

Sync.evalTrace doesn't support - as an operation?
Shouldn't parse until button is pushed

This code:
```
(let [x0 y0 sep] [28 28 60]
(svg 
  (map
    (\[i j] (rect 'red' (+ x0 (mult i sep)) (+ y0 (minus 400 (mult 100 j))) 50 (mult 100 j)))
    (zip
        [0 1 2]
        [2 4 1]))))
```
Leads to a 'too much recursion' error.

Helper functions:
```
(let indexedzip (\xs (indexedzip_ 0 xs)))
(letrec indexedzip_ (\acc xs (case xs 
    ([x|xx] [[acc x]| (indexedzip_ (+ acc 1) xx))
    (_      []                                  )
    )))
```

```
let [x0 y0 sep] [28 28 60]
(svg 
  (map
    (\[i j] (rect 'red' (+ x0 (mult i sep)) (+ y0 (minus 400 (mult 100 j))) 50 (mult 100 j)))
    (indexedzip [2 4 1])))
```

All together?
```
(let indexedzip (\xs (indexedzip_ 0 xs))
(letrec indexedzip_ (\acc xs (case xs 
    ([x|xx] [[acc x]| (indexedzip_ (+ acc 1) xx)])
    (_      []                                  )
    ))
0))

let [x0 y0 sep] [28 28 60]

(svg 
  (map
    (\[i j] (rect 'red' (+ x0 (mult i sep)) (+ y0 (minus 400 (mult 100 j))) 50 (mult 100 j)))
    (indexedzip [2 4 1])))
```

Need to look into how to define functions at the top of the input.

```
(letrec indexedzip_
  (\(acc xs)
    (case xs
      ([x | xx] [[acc x] | (indexedzip_ (+ acc 1) xx)])
      (_ [])))
(let indexedzip (\xs (indexedzip_ 0 xs))
(let [x0 y0 sep] [28 28 60]
  (svg 
    (map
      (\[i j]
        (rect
          'red'
          (+ x0 (mult i sep))
          (+ y0 (minus 400 (mult 100 j)))
          50
          (mult 100 j)))
      (indexedzip  [2 4 1]))))))
```
The above code runs, but gives an inexhaustive pattern match error in Lang.

Catching parse errors would be nice - highlighting problematic spots would be even better.

```
(letrec indexedzip_
 (\(acc xs)
    (case xs
      ([x | xx] [[acc x] | (indexedzip_ (+ acc 1) xx)])
      (_ [])))
(let indexedzip (\xs (indexedzip_ 0 xs))
(let xaxis (rect 'black' (- x0 50) (+ y0 400) 300 10)
(let yaxis (rect 'black' (- x0 50) y0 10 300)
(let bars (map (\[i j] (rect 'red' (+ x0 (* i sep)) (+ y0 (- 400 (* 100 j))) 50 (* 100 j))) (indexedzip [2 4 1]))
(let [x0 y0 sep] [28 28 60]
  (svg ([xaxis | yaxis | bars]))))))))`
