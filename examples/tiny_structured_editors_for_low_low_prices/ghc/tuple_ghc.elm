-- Dynamic dispatch names: toString, showsPrecFlip

-- Any functions with defined non-default instances
-- need to be dynamic.
--
-- In this case: "showsPrec"
-- But we can only be dynamic in the first argument,
-- so we'll flip the argument order "showsPrecFlip"

type Tuple2 a b       = Tuple2 a b
type Tuple3 a b c     = Tuple3 a b c


-- From GHC repo at 8.6.5 release. ghc/libraries/base/GHC/List.hs
--
-- -- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- -- and thus must be applied to non-empty lists.
--
-- foldr1                  :: (a -> a -> a) -> [a] -> a
-- foldr1 f = go
--   where go [x]            =  x
--         go (x:xs)         =  f x (go xs)
--         go []             =  errorEmptyList "foldr1"


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
-- instance Show a => Show [a]  where
--   {-# SPECIALISE instance Show [String] #-}
--   {-# SPECIALISE instance Show [Char] #-}
--   {-# SPECIALISE instance Show [Int] #-}
--   showsPrec _         = showList
--
--
-- instance  (Show a, Show b) => Show (a,b)  where
--   showsPrec _ (a,b) s = show_tuple [shows a, shows b] s
--
--
-- instance (Show a, Show b, Show c) => Show (a, b, c) where
--   showsPrec _ (a,b,c) s = show_tuple [shows a, shows b, shows c] s
--
--
-- show_tuple :: [ShowS] -> ShowS
-- show_tuple ss = showChar '('
--               . foldr1 (\s r -> s . showChar ',' . r) ss
--               . showChar ')'
--
--
-- showChar        :: Char -> ShowS
-- showChar        =  (:)
--
--
-- instance  Show Char  where
--     showsPrec _ '\'' = showString "'\\''"
--     showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''
--
--     showList cs = showChar '"' . showLitString cs . showChar '"'
--
--
-- instance Show Int where
--     showsPrec = showSignedInt



-- Translation to our language:



-- foldr1                  :: (a -> a -> a) -> [a] -> a
-- foldr1 f = go
--   where go [x]            =  x
--         go (x:xs)         =  f x (go xs)
--         go []             =  errorEmptyList "foldr1"
--
-- Nested patterns not supported by core language, so have to de-nest manually.
foldr1 : (a -> a -> a) -> List a -> a
foldr1 f =
  let go list = case list of
    Cons x xs -> case xs of
      Nil      -> x
      Cons _ _ -> f x (go xs)
    -- Missing Nil branch will throw error if list non-empty
  in
  go


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
show x = shows x ""
--     showList ls   s = showList__ shows ls s
--
--
-- -- | equivalent to 'showsPrec' with a precedence of 0.
-- shows           :: (Show a) => a -> ShowS
-- shows           =  showsPrec 0
shows =  showsPrec 0

showsPrec precN a = showsPrecFlip a precN
--
--
-- instance Show a => Show [a]  where
--   {-# SPECIALISE instance Show [String] #-}
--   {-# SPECIALISE instance Show [Char] #-}
--   {-# SPECIALISE instance Show [Int] #-}
--   showsPrec _         = showList
--
--
-- instance  (Show a, Show b) => Show (a,b)  where
--   showsPrec _ (a,b) s = show_tuple [shows a, shows b] s
showsPrecFlip : Tuple2 a b -> Num -> String -> String
showsPrecFlip tuple2 _ s = case tuple2 of
  Tuple2 a b -> show_tuple [shows a, shows b] s
--
--
-- instance (Show a, Show b, Show c) => Show (a, b, c) where
--   showsPrec _ (a,b,c) s = show_tuple [shows a, shows b, shows c] s
showsPrecFlip : Tuple3 a b -> Num -> String -> String
showsPrecFlip tuple3 _ s = case tuple3 of
  Tuple3 a b c -> show_tuple [shows a, shows b, shows c] s
--
--
-- show_tuple :: [ShowS] -> ShowS
-- show_tuple ss = showChar '('
--               . foldr1 (\s r -> s . showChar ',' . r) ss
--               . showChar ')'
-- We don't have primitive chars, only single element strings.
-- (<<) f g = \x -> f (g x) -- Part of core language Prelude
show_tuple ss = showChar "("
             << foldr1 (\s r -> s << showChar "," << r) ss
             << showChar ")"
--
--
-- showChar        :: Char -> ShowS
-- showChar        =  (:)
-- (+) is a primitive operator in a our language and cannot be partially applied, so don't curry
showChar l r = l + r
--
--
-- -- | @since 2.01
-- instance  Show Char  where
--     showsPrec _ '\'' = showString "'\\''"
--     showsPrec _ c    = showChar '\'' . showLitChar c . showChar '\''
--
--     showList cs = showChar '"' . showLitString cs . showChar '"'
--
-- The core language (and the string provenance) doesn't have a concept of "character"
-- so we can't translate the Haskell precisely without losing string provenance.
--
-- Also, no escaping in displayed string.
showsPrecFlip : String -> Num -> String -> String
showsPrecFlip str _ s = '"' + str + '"' + s
--
--
-- -- | @since 2.01
-- instance Show Int where
--     showsPrec = showSignedInt
showsPrecFlip : Num -> Num -> String -> String
showsPrecFlip num _ s = numToStringBuiltin num + s




toString : a -> String
toString = show

-- (Tuple3 "a" 1 (Tuple2 10 "ten") : Tuple3 String Num (Tuple2 Num String))
(Tuple2 10 "ten" : Tuple2 Num String)
