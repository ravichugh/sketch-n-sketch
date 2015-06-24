##Bar graph

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

```
(letrec indexedzip_
  (\(acc xs)
    (case xs
      ([x | xx] [[acc x] | (indexedzip_ (+ acc 1) xx)])
      (_ [])))
(let indexedzip (\xs (indexedzip_ 0 xs))
(let [x0 y0 sep] [28 28 60]
(let xaxis (rect 'black' (- x0 8) (+ y0 400) 300 10)
(let yaxis (rect 'black' (- x0 8) y0 10 300)
(let bars
  (map
    (\[i j] (rect 'red' (+ x0 (* i sep)) (+ y0 (- 400 (* 100 j))) 50 (* 100 j)))
    (indexedzip  [2 4 1]))
  (svg  [xaxis | yaxis | bars])))))))
```

##Graph (nodes and edges)

```
(let node (\[x y] (circle 'blue' x y 20))
(let edge (\[[x y] [i j]] (line_ x y i j))
(letrec genpairs (\xs 
  (case xs 
    ([x y | xx] [[x y] (append (genpairs [x | xx]) (genpairs ([y | xx])))]) 
    (_ [])
  )
)
(let pts [[100 200] [200 300]]
(let edges (map edge (genpairs pts))
(let nodes (map node pts)
(svg (append edges nodes))
))))))
```

The line function isn't working quite yet.

```
(let (edge (\[[x y] [i j]] (line 'green' x y i j))
(let lin (edge [[100 200] [200 300]])
(svg [lin]))))
```

```
(let node (\[x y] (circle 'blue' x y 20))
(let edge (\[[x y] [i j]] (line 'green' x y i j))
(letrec genpairs (\xs 
  (case xs 
    ([x | y | xx] [[x y] | (genpairs [x | xx]) | (genpairs ([y | xx]))]) 
    (_ [])
  )
)
(let pts [[100 200] [200 300]]
(let edges (map edge (genpairs pts))

(svg [])
)))))
```
