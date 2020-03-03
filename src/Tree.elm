module Tree exposing
  ( ..
  )

type Tree a
  = Leaf
  | Node (Tree a) a (Tree a)

-- Assumes "tree" is a binary search tree
binsert : comparable -> Tree comparable -> Tree comparable
binsert y tree =
  case tree of
    Leaf ->
      Node Leaf y Leaf

    Node left x right ->
      if y == x then
        tree
      else if y < x then
        Node (binsert y left) x right
      else -- y > x
        Node left x (binsert y right)

preOrder : Tree a -> List a
preOrder tree =
  case tree of
    Leaf ->
      []

    Node left x right ->
      [x] ++ preOrder left ++ preOrder right

inOrder : Tree a -> List a
inOrder tree =
  case tree of
    Leaf ->
      []

    Node left x right ->
      inOrder left ++ [x] ++ inOrder right

postOrder : Tree a -> List a
postOrder tree =
  case tree of
    Leaf ->
      []

    Node left x right ->
      postOrder left ++ postOrder right ++ [x]

countLeaves : Tree a -> Int
countLeaves tree =
  case tree of
    Leaf ->
      1

    Node left _ right ->
      countLeaves left + countLeaves right

countNodes : Tree a -> Int
countNodes tree =
  case tree of
    Leaf ->
      0

    Node left _ right ->
      1 + countNodes left + countNodes right

map : (a -> b) -> Tree a -> Tree b
map f tree =
  case tree of
    Leaf ->
      Leaf

    Node left x right ->
      Node (map f left) (f x) (map f right)

countNodesAtLevel : Int -> Tree a -> Int
countNodesAtLevel level tree =
  case tree of
    Leaf ->
      0

    Node left _ right ->
      if level == 0 then
        1
      else
        countNodesAtLevel (level - 1) left
          + countNodesAtLevel (level - 1) right
