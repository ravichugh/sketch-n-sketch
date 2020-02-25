-- Dynamic dispatch names: toString, showsPrecFlip

-- Any functions with defined non-default instances
-- need to be dynamic.
--
-- In this case: "showsPrec"
-- But we can only be dynamic in the first argument,
-- so we'll flip the argument order "showsPrecFlip"


-- "Set" is a reserved name in Leo surface language.
-- Have to call it something else.
type MySet a
  = Bin Int a (MySet a) (MySet a)
  | Tip


-- Excerpted from https://hackage.haskell.org/package/containers-0.6.2.1/docs/src/Data.Set.Internal.html ((c) Daan Leijen 2002 BSD License)
--

-- data Set a    = Bin {-# UNPACK #-} !Size !a !(Set a) !(Set a)
--               | Tip
--
-- type Size     = Int
--
-- instance Show a => Show (Set a) where
--   showsPrec p xs = showParen (p > 10) $
--     showString "fromList " . shows (toList xs)
--
-- toList :: Set a -> [a]
-- toList = toAscList
--
-- toAscList :: Set a -> [a]
-- toAscList = foldr (:) []
--
-- foldr :: (a -> b -> b) -> b -> Set a -> b
-- foldr f z = go z
--   where
--     go z' Tip           = z'
--     go z' (Bin _ x l r) = go (f x (go z' r)) l


-- From GHC repo at 8.6.5 release. ghc/libraries/base/GHC/Show.hs
--
-- type ShowS = String -> String
--
-- class  Show a  where
--     {-# MINIMAL showsPrec | show #-}
--
--     -- | Convert a value to a readable 'String'.
--     --
--     -- 'showsPrec' should satisfy the law
--     --
--     -- > showsPrec d x r ++ s  ==  showsPrec d x (r ++ s)
--     --
--     -- Derived instances of 'Text.Read.Read' and 'Show' satisfy the following:
--     --
--     -- * @(x,\"\")@ is an element of
--     --   @('Text.Read.readsPrec' d ('showsPrec' d x \"\"))@.
--     --
--     -- That is, 'Text.Read.readsPrec' parses the string produced by
--     -- 'showsPrec', and delivers the value that 'showsPrec' started with.
--
--     showsPrec :: Int    -- ^ the operator precedence of the enclosing
--                         -- context (a number from @0@ to @11@).
--                         -- Function application has precedence @10@.
--               -> a      -- ^ the value to be converted to a 'String'
--               -> ShowS
--
--     -- | A specialised variant of 'showsPrec', using precedence context
--     -- zero, and returning an ordinary 'String'.
--     show      :: a   -> String
--
--     -- | The method 'showList' is provided to allow the programmer to
--     -- give a specialised way of showing lists of values.
--     -- For example, this is used by the predefined 'Show' instance of
--     -- the 'Char' type, where values of type 'String' should be shown
--     -- in double quotes, rather than between square brackets.
--     showList  :: [a] -> ShowS
--
--     showsPrec _ x s = show x ++ s
--     show x          = shows x ""
--     showList ls   s = showList__ shows ls s
--
--
-- -- | equivalent to 'showsPrec' with a precedence of 0.
-- shows           :: (Show a) => a -> ShowS
-- shows           =  showsPrec 0
--
--
-- showList__ :: (a -> ShowS) ->  [a] -> ShowS
-- showList__ _     []     s = "[]" ++ s
-- showList__ showx (x:xs) s = '[' : showx x (showl xs)
--   where
--     showl []     = ']' : s
--     showl (y:ys) = ',' : showx y (showl ys)
--
--
-- -- | utility function converting a 'String' to a show function that
-- -- simply prepends the string unchanged.
-- showString      :: String -> ShowS
-- showString      =  (++)
--
--
-- -- | utility function that surrounds the inner show function with
-- -- parentheses when the 'Bool' parameter is 'True'.
-- showParen       :: Bool -> ShowS -> ShowS
-- showParen b p   =  if b then showChar '(' . p . showChar ')' else p
--
--
-- showSpace :: ShowS
-- showSpace = {-showChar ' '-} \ xs -> ' ' : xs
--
--
-- showCommaSpace :: ShowS
-- showCommaSpace = showString ", "



showList ls s = showList__ shows ls s

showList__ showx list s = case list of
  Nil       -> "[]" + s
  Cons x xs ->
    let showl list = case list of
      Nil       -> "]" + s
      Cons y ys -> "," + showx y (showl ys)
    in
    "[" + showx x (showl xs)

show x = shows x ""

shows = showsPrec 0

showsPrec precN a = showsPrecFlip a precN

showChar l r = l + r

showString l r = l + r

showParen b p =  if b then showChar "(" << p << showChar ")" else p

-- showSpace = \xs -> " " + xs

-- showCommaSpace = showString ", "

-- -- The core language (and the string provenance) doesn't have a concept of "character"
-- -- so we can't translate the Haskell precisely without losing string provenance.
-- --
-- -- Also, no escaping in displayed string.
-- showsPrecFlip : String -> Num -> String -> String
-- showsPrecFlip str _ s = '"' + str + '"' + s

showsPrecFlip : Num -> Num -> String -> String
showsPrecFlip num _ s = numToStringBuiltin num + s

-- showsPrecFlip : Bool -> Num -> String -> String
-- showsPrecFlip bool precN = case bool of
--   False ->
--     showString "False"
--   True -> showString "True"

showsPrecFlip : List a -> Num -> String -> String
showsPrecFlip list _ = showList list



-- Translation to our language:
--
--
-- instance Show a => Show (Set a) where
--   showsPrec p xs = showParen (p > 10) $
--     showString "fromList " . shows (toList xs)
showsPrecFlip : MySet a -> Num -> String -> String
showsPrecFlip xs p = showParen (p >= 11) <|
  showString "fromList " << shows (toList xs)
--
-- toList :: Set a -> [a]
-- toList = toAscList
toList = toAscList
--
-- toAscList :: Set a -> [a]
-- toAscList = foldr (:) []
cons a list = Cons a list
toAscList = foldr cons []
--
-- foldr :: (a -> b -> b) -> b -> Set a -> b
-- foldr f z = go z
--   where
--     go z' Tip           = z'
--     go z' (Bin _ x l r) = go (f x (go z' r)) l
foldr : (a -> b -> b) -> b -> MySet a -> b
foldr f z =
  let go zPrime set =
    case set of
      Tip         -> zPrime
      Bin _ x l r -> go (f x (go zPrime r)) l
  in
  go z



empty : MySet a
empty = Tip

singleton : a -> MySet a
singleton a = Bin 0 a empty empty

-- Okay, these are NOT the Haskell versions of insert/remove which
-- do all sorts of fancy balancing. The point remains that the data
-- and the ADT are not 1-to-1. The ADT has extra structure.
--
-- We don't balance or maintain size (both unused in the toString
-- machinery) but we do maintain the node order.
insert : a -> MySet a -> MySet a
insert a set = case set of
  Tip         -> singleton a
  Bin _ b l r ->
    if a <= b then
      if a >= b then
        set
      else
        Bin 0 b (insert a l) r
    else
      Bin 0 b l (insert a r)

-- remove : a -> MySet a -> MySet a
-- remove a set = case set of
--   Tip         -> set
--   Bin _ b l r ->
--     if a <= b then
--       if a >= b then
--         -- Not efficient or balancing lol
--         foldr insert l r
--       else
--         Bin 0 b (remove a l) r
--     else
--       Bin 0 b l (remove a r)


toString : a -> String
toString = show

set =
  empty
  |> insert 5
  |> insert 3
  |> insert 5
  |> insert 2
  |> insert 7

(set : MySet Num)
