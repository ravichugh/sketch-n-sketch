type Expr = Nil | Cons Expr Expr | Int Int | Var String

type Path = List (Up | Down String)
type Clone d = Clone Path d
type Diff = DUpdate (List (String, Diffs))
           | DNew Expr (List (String, Clone Path Diffs))
type alias Diffs = List Diff

originalList = (Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) (Nil))))

dSame = [DUpdate []]

-- Keep the same list structure, just replace the first
-- element by the second and vice-versa. It's the most lens-compatible.
diffs1 = [DUpdate [
    ("hd", [DNew (Var "h") [("h", Clone [Up, Down "tl", Down "hd"] dSame)]]),
    ("tl", [DUpdate [
       ("hd", [DNew (Var "h") [("h", Clone [Up, Up, Down "hd"] dSame)]])]])]]

-- Replace the first element by the second,
-- the tail by the sub-tail after inserting the first original element.
diffs1bis = [DUpdate [
    ("hd", [DNew (Var "h") [("h", Clone [Up, Down "tl", Down "hd"] dSame)]]),
    ("tl", [DNew (Cons (Var "h") (Var "t")) [
       ("h", Clone [Up, Down "hd"] dSame),
       ("t", Clone [Down "tl"] dSame)]])]]

-- Delete the first element, and then insert the previous first original element
-- in front of the tail of the remaining list
diffs2 = [DNew (Var "t") [
  ("t", Clone [Down "tl"] [DUpdate [
       ("tl", [DNew (Cons (Var "h") (Var "t")) [
         ("h", Clone [Up, Up, Down "hd"] dSame),
         ("t", Clone [] dSame)]])
     ]]
  )]]

-- Fully build a new list by combining the previously second element,
-- the previously first element and the previous tail of the tail
diffs3 = [DNew (Cons (Var "h1") (Cons (Var "h2") (Var "t"))) [
  ("h1", Clone [Down "tl", Down "hd"] dSame),
  ("h2", Clone [Down "hd"] dSame),
  ("t", Clone [Down "tl", Down "tl"] dSame)]]

-- Build a new list by combining the previously second element,
-- and insert the previously first element in front of the tail
diffs4 = [DNew (Cons (Var "h") (Var "t")) [
  ("h", Clone [Down "tl", Down "hd"] dSame),
  ("t", Clone [Down "tl", Down "tl"] [DNew (Cons (Var "h") (Var "t")) [
     ("h", Clone [Up, Up, Down "hd"] dSame),
     ("t", Clone [] dSame)] ])
  ]]

-- Build a new list by combining the previously second element,
-- and replace the tail by a new list consisting of the previously first element
-- and the previously tail element. Sounds dumb but why not.
diffs5 = [DNew (Cons (Var "h") (Var "t")) [
  ("h", Clone [Down "tl", Down "hd"] dSame),
  ("t", Clone [Down "tl"] [DNew (Cons (Var "h") (Var "t")) [
    ("h", Clone [Up, Down "hd"] dSame),
    ("t", Clone [Down "tl"] dSame)] ])]]

-- Expr utils
replace: (String -> Maybe Expr) -> Expr -> Expr
replace f e = case e of
  Nil -> Nil
  Cons a b -> Cons (replace f a) (replace f b)
  Int _ -> e
  Var x -> case f x of
    Nothing -> e
    Just newE -> newE

getChild: String Expr -> Maybe Expr
getChild name e = case e of
  Cons e1 e2 -> if name == "hd" then Just e1 else if name == "tl" then Just e2 else Nothing
  _ -> Nothing

-- List and result utils
projOksConcat = List.projOks >> Result.map List.concat

-- The context is simply a stack of previously seen Expr
applyDiffs expr diffs =
  let aux: List Expr -> Expr -> Diffs -> Result String (List Expr)
      aux context expr diffs = 
        -- Question: Shall we call aux with the context of the cloned element
        -- or the context of the place where it is being cloned?
       let getClone: List Expr -> Expr -> Path -> Diffs -> Result String (List Expr)
           getClone cloneContext expr path diffs = 
         case path of
         [] -> aux cloneContext expr diffs
         Up :: tailPath -> case cloneContext of
           headContext :: tailContext ->
             getClone tailContext headContext tailPath diffs
           [] -> Err "cloning Up is not available under empty context"
         Down name :: tailPath -> case getChild name expr of
           Just newExpr -> getClone (expr :: cloneContext) newExpr tailPath diffs
           Nothing -> Err """Cloning Down '@name' not available on @expr"""
       in
       diffs |>
       List.map (\diff -> case diff of
         DUpdate [] -> Ok [expr]
         DUpdate l -> case expr of
           Cons hd tl ->
             let rnewHds = case listDict.get "hd" l of
                   Nothing -> Ok [hd]
                   Just ds -> aux (expr::context) hd ds
                 rnewTls = case listDict.get "tl" l of
                   Nothing -> Ok [tl]
                   Just ds -> aux (expr::context) tl ds
             in
             Result.map2 (\newHds newTls ->
               List.cartesianProductWith Cons (Debug.log "newHds" newHds) newTls
             ) rnewHds rnewTls
           _ -> Err """Could not apply @(DUpdate l) to @expr"""
         DNew e cloneEnv ->
           List.foldl (\(name, Clone path diffs) resE ->
             resE |> Result.andThen (List.map (\e ->
               getClone context expr path diffs |>
               Result.map (
                 List.map (\replacement ->
                   replace (\n -> if n == name then Just replacement else Nothing) e)
               )) >> projOksConcat)
           ) (Ok [e]) cloneEnv
       ) |> projOksConcat
  in aux [] expr diffs

displayApplyDiffs original diffs = <span>applyDiffs<br>&nbsp;&nbsp;(@("""@original"""))<br>&nbsp;&nbsp;@("""@diffs""") =<br>&nbsp;&nbsp;@("""@(applyDiffs original diffs)""")<br><br></span>

<div>
@(displayApplyDiffs (Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) (Nil)))) diffs1)
@(displayApplyDiffs (Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) (Nil)))) diffs1bis)
@(displayApplyDiffs (Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) (Nil)))) diffs2)
@(displayApplyDiffs (Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) (Nil)))) diffs3)
@(displayApplyDiffs (Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) (Nil)))) diffs4)
@(displayApplyDiffs (Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) (Nil)))) diffs5)
</div>