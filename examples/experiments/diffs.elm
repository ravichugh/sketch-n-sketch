type Expr = Nil | Cons Expr Expr | Int Int | Var String | Concat Expr Expr | Parens Expr

type Path = List (Up | Down String)
type Clone d = Clone Path d
type Diff = DUpdate (List (String, Diffs))
           | DNew Expr (List (String, Clone Path Diffs))
type alias Diffs = List Diff

originalList = Cons (Var "a2") (Cons (Int 1) (Cons (Int 3) Nil))

originalExpr = Concat (Parens (Cons (Int 2) Nil)) (Parens (Cons (Var "a2") (Cons (Int 3) Nil)))

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
  Parens x -> Parens (replace f x)
  Concat e1 e2 -> Concat (replace f e1) (replace f e2)

getChild: String Expr -> Maybe Expr
getChild name e = case e of
  Cons e1 e2 -> if name == "hd" then Just e1 else if name == "tl" then Just e2 else Nothing
  Concat e1 e2 -> if name == "arg1" then Just e1 else if name == "arg2" then Just e2 else Nothing
  Parens e1 -> if name == "_1"  then Just e1 else Nothing
  _ -> Nothing

-- List and result utils
projOksConcat = List.projOks >> Result.map List.concat

-- The context is simply a stack of previously seen Expr
applyDiffs: Expr -> Diffs -> Result String (List Expr)
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
         DUpdate l ->
           let recurse name subExpr = case listDict.get name l of
              Nothing -> Ok [subExpr]
              Just ds -> aux (expr::context) subExpr ds
           in
           case expr of
           Cons hd tl -> Result.map2 (List.cartesianProductWith Cons) (recurse "hd" hd) (recurse "tl" tl)
           Parens sub -> Result.map (List.map Parens) (recurse "_1" sub)
           Concat e1 e2 -> Result.map2 (List.cartesianProductWith Cons) (recurse "arg1" e1) (recurse "arg2" e2)
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

type EvalStep = EvalContinue Expr (Expr -> EvalStep) | EvalResult Expr | EvalError String  

getEvalStep: Expr -> EvalStep
getEvalStep expr = case expr of
  Cons hd tl ->
     EvalContinue hd <| \hdv ->
     EvalContinue tl <| \tlv ->
     EvalResult <| Cons hdv tlv
  Parens sub -> EvalContinue sub EvalResult
  Concat e1 e2 ->
    EvalContinue e1 <| \e1v ->
    EvalContinue e2 <| \e2v ->
      let aux m1 m2 =
        case m1 of
          Nil -> Ok m2
          Cons x1 x2 -> aux x2 m2 |> Result.map (Cons x1)
          _ -> Err """Cannot concatenate @m1"""
      in case aux e1v e2v of
        Err msg -> EvalError msg
        Ok x -> EvalResult x
  e -> EvalResult e

eval_: EvalStep -> List (Expr -> EvalStep) -> Result String Expr
eval_ evalStep callbacks =
  case evalStep of
    EvalContinue what callback ->
      --let _ = Debug.log """EvalContinue @what (1 + @(List.length callbacks) callbacks)""" () in
      eval_ (getEvalStep what) (callback :: callbacks)
    EvalResult x ->
      --let _ = Debug.log """EvalResult @x (@(List.length callbacks) callbacks)""" () in
      case callbacks of
      head :: tail -> eval_ (head x) tail
      [] -> Ok x
    EvalError msg -> Err msg

eval: Expr -> Result String Expr
eval expr = eval_ (EvalContinue expr EvalResult) []

simplify diff = case diff of
  DUpdate l -> DUpdate (List.filter (\(name, subd) -> subd /= [DUpdate []]) l)
  DNew e c -> diff

-- Remove any Down - Up sequence in a path.
simplifyPath path = case path of
  Down x :: tail -> case simplifyPath tail of
    Up :: tail2 -> tail2
    y -> Down x :: y
  x :: tail -> x :: simplifyPath tail
  _ -> path

simplifyUpdate updateAction = case updateAction of
  UpdateAlternative [u] -> u
  _ -> updateAction
  
getListLength: Expr -> Maybe Int
getListLength expr = case expr of
  Nil -> Just 0
  Cons _ tail -> getListLength tail |> Maybe.map (+ 1)
  _ -> Nothing

-- Given a Down* path to a value and an Expr, computes a Down* path of expressions.
updateDownPath: Expr -> Path -> Path
updateDownPath expr path = let _ = Debug.log """updateDownPath @expr @path""" () in
  Debug.log """updateDownPath @expr @path = """ <|
  case expr of
  Parens sub -> Down "_1" :: updateDownPath sub path
  Int x -> path
  Var x -> path
  Nil -> path
  Cons hd tl -> case path of
    (head as (Down x)) :: pathTail ->
      case getChild x expr of
        Just child -> head :: updateDownPath child pathTail
        Nothing -> let _ = Debug.log """updateDownPath @expr path""" () in path
    _ -> let _ = Debug.log """updateDownPath @expr path""" () in path
  Concat e1 e2 -> -- Could be put in a lens, e.g. mapPath ?
    case eval e1 of
      Ok v1 ->
        let aux v1 p = case (v1, p) of
          (Cons hd tl, Down "tl" :: pTail) ->
            aux tl pTail
          (Cons hd tl, Down "hd" :: pTail) ->
            Down "arg1" :: updateDownPath e1 path
          (Nil, pTail) ->
            Down "arg2" :: updateDownPath e2 pTail
        in aux v1 path
      Err msg -> error msg

-- transforms a value path to an expression path statically
-- The context stores only expressions which match the Up of an value
updateExprPath context expr path = case path of
  Up :: pathTail ->
    case context of
      head :: tail -> updateExprPath tail head pathTail
      [] -> Debug.log """updateExprPath @context @expr""" path
  _ -> updateDownPath expr path

-- Given a Down* path to a value and an Expr, computes a Down* path of expressions.
followExprPath: List Expr -> Path -> Expr -> Maybe (List Expr, Expr)
followExprPath context path expr =
  case path of
    Down x :: pathTail ->
      case getChild x expr of
        Just child -> followExprPath (expr :: context) pathTail child
        Nothing -> Nothing -- Err """No child @x for @expr"""
    Up :: pathTail -> case context of
      head :: tail -> followExprPath tail pathTail head
      _ -> Nothing
    [] -> Just (context, expr)
      
-- Given a diffs, maps all the paths that escape the structure using pathMaker
-- pathMaker is provided only the part of the path that starts after the Up that
-- escapes the current scope of the Diffs
mapEscapingPaths: (Path -> Path) -> Diffs {- Value -}-> Diffs {- Expression -}
mapEscapingPaths pathMaker diffs = flip List.map diffs <| \diff ->
  case diff of
    DUpdate l ->
      let updatedPathMaker path = case path of -- We ignore one more level of Up
            Up {- To the diffs level -} :: pathTail ->
              Up :: pathMaker pathTail
            _ -> path
      in
      DUpdate (List.map (Tuple.mapSecond <| mapEscapingPaths updatedPathMaker) l)
    DNew insertedExp cloneEnv ->
      DNew insertedExp <| flip List.map cloneEnv <| \(name, Clone path cdiffs) ->
        let updatedPathMaker newPath = pathMaker (simplifyPath (path ++ newPath)) in
        let newClonePath = case path of
              Up {- out of scope -} :: pathTail -> Up :: pathMaker pathTail
              _ -> path
        in
        (name, Clone newClonePath (mapEscapingPaths updatedPathMaker cdiffs))

type UpdateStep =
  UpdateContinue (List Expr) Expr Diffs (Diffs -> UpdateStep) |
  UpdateAlternative (List (UpdateStep)) |
  UpdateResult Diffs |
  UpdateError String 

getUpdateStep: List Expr -> Expr -> Diffs -> UpdateStep
getUpdateStep context expr vdiffs =
  let mapChildrenPaths: Path {-Value starting at expression -} -> Path {- Expression-based -}
      mapChildrenPaths escapingPath =
        let _ = Debug.log """mapChildrenPaths @escapingPath""" () in
        case escapingPath of
         -- If the path immediately goes down another child of this expression
         Down x :: _ -> updateDownPath expr escapingPath
         -- We defer this decision to the outer
         _ -> escapingPath
  in
  let _ = Debug.log """getUpdateStep @expr @vdiffs""" () in
  case expr of
  Int x -> UpdateResult vdiffs
  Var x -> UpdateResult vdiffs
  Parens sub -> -- context is not updated because Parens disappears in evaluation
    UpdateContinue context sub vdiffs <| \newSubDiffsV ->
      let newSubDiffsE = mapEscapingPaths (\escapingPath -> Up :: escapingPath) newSubDiffsV in
      UpdateResult [DUpdate [("_1", newSubDiffsE)]]
  Cons hd tl ->
    vdiffs |> List.map (\diff -> case diff of
     DUpdate l ->
       let updateContinueSub name subExpr callback = case listDict.get name l of
            Nothing -> callback dSame
            Just ds -> UpdateContinue (expr::context) subExpr ds callback
       in
       updateContinueSub "hd" hd <| \diffsHdRaw ->
       updateContinueSub "tl" tl <| \diffsTlRaw ->
        let diffsHdE = mapEscapingPaths mapChildrenPaths diffsHdRaw
            diffsTlE = mapEscapingPaths mapChildrenPaths diffsTlRaw
            _ = Debug.log """diffsHdRaw: @diffsHdRaw --> @diffsHdE""" ()
        in
        UpdateResult [simplify (DUpdate [("hd", diffsHdE), ("tl", diffsTlE)])]
     DNew newE cloneEnv -> 
       let aux cloneEnv revCloneAcc  = case cloneEnv of
         [] ->
           UpdateResult [DNew newE (List.reverse revCloneAcc)]
         (name, Clone cpath cdiffs)::cloneEnvTail ->
           if cdiffs == dSame then
             (name, Clone cpath cdiffs) :: revCloneAcc |>
             aux cloneEnvTail
           else
           let updatedcpath = updateExprPath context expr cpath in
           case followExprPath context updatedcpath expr of 
             Nothing -> UpdateError """could not find path in context @(name, Clone updatedcpath cdiffs)"""
             Just (newContext, subexpr) ->
               UpdateContinue newContext subexpr cdiffs <| \newcdiffs ->
               (name, Clone updatedcpath newcdiffs) :: revCloneAcc |>
               aux cloneEnvTail
       in aux cloneEnv []
       -- TODO: what to do with cdiffs? we should walk the tree as well.
    ) |> UpdateAlternative
  Concat e1 e2 -> -- The mother of all Edit lenses. Not yet map, yet alone apply, but still.
    case eval e1 of    
      Err msg -> UpdateError msg
      Ok v1 -> -- We split the diffs given the original value v1
        let _ = Debug.log """v1 : @v1""" () in
        let continueWith arg1diffsV arg2diffsV =
              let updateContinueSub subExpr subDiffs callback =
                    if subDiffs == dSame then callback dSame
                    else UpdateContinue [] subExpr subDiffs callback
              in
              updateContinueSub e1 arg1diffsV <| \arg1diffsE ->
              updateContinueSub e2 arg2diffsV <| \arg2diffsE ->
              UpdateResult <| [simplify <| DUpdate [("arg1", arg1diffsE), ("arg2", arg2diffsE)]]
        in
        let listLength v = case v of 
             Nil -> 0
             Cons _ tl -> 1 + listLength tl
        in
        let sizeLeft = listLength v1 in
        let splitDiffs n accDiffs rdiffs = 
          --let _ = Debug.log """splitDiffs @n @(accDiffs dSame) @rdiffs""" () in
          if n == sizeLeft then -- Now we need to fix rdiffs paths
            let fixrdiffs escapingPath =  --let _ = Debug.log """fixrdiffs @escapingPath""" () in
              let aux numPreviousUp path =
                if numPreviousUp > sizeLeft then -- Full escape of the original list
                  Up {- concat is like a wrap there -} :: path
                else
                  case path of
                  Up :: pathTail -> aux (numPreviousUp + 1) pathTail
                  Down "hd" :: pathTail ->
                    {- numPreviousUp <= sizeLeft so it is rewritten as a path that goes up to the concatenation, and then-
                       down to the left argument -}
                      Down "arg1" :: updateDownPath e1 (
                        List.range 1 (sizeLeft - numPreviousUp) |>
                        List.foldl (\_ t -> Down "tl" :: t) path)
                  Down _ :: _ -> Debug.log "weird concat path, there is an Up followed by " path -- Weird. The path stays the same.
              in aux 1 escapingPath
            in continueWith (accDiffs dSame) (mapEscapingPaths fixrdiffs rdiffs)
          else
            simplifyUpdate <| UpdateAlternative <|
            flip List.map rdiffs <| \diff ->
              case diff of
                DUpdate l ->
                  let newHds = case listDict.get "hd" l of
                    Nothing -> dSame
                    Just hdds -> 
                      let fixldiffs escapingPath =
                        let (mbDown, pathTail) = List.split (sizeLeft - n) escapingPath in
                        if List.length mbDown == sizeLeft - n && List.all (== (Down "tl")) mbDown then
                          List.range 1 n |> List.foldl (\_ x -> Up :: x) (Up {- To concat -} :: Down "arg2" {- to second arg -} :: updateDownPath e2 pathTail)
                        else
                          let (mbUp, pathTail) = List.split n escapingPath in
                          if List.length mbUp == n && List.all (== Up) mbUp then
                          case pathTail of
                            Up {- truly escape the list expr -} :: pathTail2 ->
                              mbUp ++ (Up {- to concat expr -}) :: pathTail
                            _ -> path
                          else path
                      in mapEscapingPaths fixldiffs hdds
                  in
                  let newAccDiffs newTds =
                        [simplify <| DUpdate [("hd", newHds), ("tl", newTds)]]
                  in
                  case listDict.get "tl" l of
                    Nothing -> continueWith (newAccDiffs dSame) dSame
                    Just tlds -> splitDiffs (n + 1) newAccDiffs tlds
                _ -> UpdateError "DNew not yet supported in Concat update - coming soon !"
        in
        splitDiffs 0 identity vdiffs
    
    

type alias Callbacks = List (Diffs -> UpdateStep)
type alias Fork = (UpdateStep, Callbacks)

update_: UpdateStep -> Callbacks -> List Fork -> Result String Diffs
update_ updateStep callbacks alternatives =
  case updateStep of
    UpdateContinue context what diffs callback ->
      update_ (getUpdateStep context what diffs) (callback :: callbacks) alternatives
    UpdateResult x ->
      case callbacks of
      head :: tail -> update_ (head x) tail alternatives
      [] -> 
        case alternatives of
          (ha, ca) :: ta ->
            case update_ ha ca ta of -- In real, there would be a lazy call to update_
              Ok xs -> Ok (x ++ xs)
              Err msg -> Ok x
          [] -> Ok x
    UpdateAlternative (head :: tail) ->
      update_ head callbacks ((List.map (flip (,) callbacks) tail) ++ alternatives)
    UpdateError msg -> Err msg

update: Expr -> Diffs -> Result String Expr
update expr diffs = update_ (UpdateContinue [] expr diffs UpdateResult) [] []
  
{--
originalExpr = Parens (Cons (Parens (Int 1)) (Parens (Cons (Parens (Var "a2")) Nil)))
outputDiffs = [DUpdate [("hd", [DNew (Var "h") [("h", Clone [Up, Down "tl", Down "hd"] [DUpdate []])]])]]
--}
{--
originalExpr = Concat (Parens (Cons (Parens (Int 2)) Nil)) (Parens (Cons (Var "a1") (Cons (Int 3) Nil)))
outputDiffs = [DUpdate [
  ("hd", [DNew (Var "h") [("h", Clone [Up, Down "tl", Down "hd"] [DUpdate []])]]),
  ("tl", [DUpdate [(
    "hd", [DNew (Var "h") [("h", Clone [Up, Up, Down "hd"] [DUpdate []])]])
  ]])]]
--}
{--} -- TODO: illustrate how DNew for Cons should use updateDownPath
originalExpr = Cons (Int 1) (Parens (Concat (Cons (Parens (Var "a2")) Nil) (Cons (Int 3) Nil)))
outputDiffs = [DNew (Cons (Var "h") (Var "t")) [
  ("h", Clone [Down "tl", Down "hd"] [DNew (Var "h") [("h", Clone [Up, Down "tl", Down "hd"] dSame)]]),
  ("t", Clone [] dSame)]]
--}
exprDiffs = update originalExpr outputDiffs |> .args._1

<pre><h2>diffs</h2>
@(toString <| exprDiffs)
<h2>result</h2>
@(toString <| applyDiffs originalExpr exprDiffs)
</pre>