-- List definition, for reference. The Sketch-n-Sketch
-- surface language treats lists as a separate type
-- (not a datatype), so the following is actually ignored
-- and we have to bake the List datatype definition into
-- the core language.
type List a = Nil
            | Cons a (List a)

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
-- showList__ :: (a -> ShowS) ->  [a] -> ShowS
-- showList__ _     []     s = "[]" ++ s
-- showList__ showx (x:xs) s = '[' : showx x (showl xs)
--   where
--     showl []     = ']' : s
--     showl (y:ys) = ',' : showx y (showl ys)
--
-- -- | equivalent to 'showsPrec' with a precedence of 0.
-- shows           :: (Show a) => a -> ShowS
-- shows           =  showsPrec 0
--
-- -- | @since 2.01
-- instance Show a => Show [a]  where
--   {-# SPECIALISE instance Show [String] #-}
--   {-# SPECIALISE instance Show [Char] #-}
--   {-# SPECIALISE instance Show [Int] #-}
--   showsPrec _         = showList

--
-- Okay, and now the translation to our language.
--
-- We don't have typeclasses so, in this case,
-- we have to pass around an extra argument containing
-- the function to turn a list element to a string.
--

-- showList__ :: (a -> ShowS) ->  [a] -> ShowS
-- showList__ _     []     s = "[]" ++ s
-- showList__ showx (x:xs) s = '[' : showx x (showl xs)
--   where
--     showl []     = ']' : s
--     showl (y:ys) = ',' : showx y (showl ys)
showList__ showx list s =
  -- where
  --   showl []     = ']' : s
  --   showl (y:ys) = ',' : showx y (showl ys)
  let showl list =
    case list of
      Nil       -> "]" + s
      Cons y ys -> "," + showx y (showl ys)
  in
  -- showList__ _     []     s = "[]" ++ s
  -- showList__ showx (x:xs) s = '[' : showx x (showl xs)
  case list of
    Nil       -> "[]" + s
    Cons x xs -> "["  + showx x (showl xs)


-- showList ls   s = showList__ shows ls s
showList showsElem ls s = showList__ showsElem ls s

-- instance Show a => Show [a]  where
--   {-# SPECIALISE instance Show [String] #-}
--   {-# SPECIALISE instance Show [Char] #-}
--   {-# SPECIALISE instance Show [Int] #-}
--   showsPrec _         = showList
showsPrec showsElem _ = showList showsElem

-- shows :: (Show a) => a -> ShowS
-- shows =  showsPrec 0
shows showsElem = showsPrec showsElem 0

-- show x = shows x ""
intervalToString : List Num -> String
intervalToString list =
  -- showsPrec _ x s = show x ++ s
  -- shows :: (Show a) => a -> ShowS
  -- shows =  showsPrec 0
  let showsElem n s = toString n + s in -- Number version
  -- let showsElem elem s = '"' + elem + '"' + s in -- String version
  shows showsElem list ""

-- The desugaring step turns this into Cons's and Nil's
([1, 2, 3] : List Num)
