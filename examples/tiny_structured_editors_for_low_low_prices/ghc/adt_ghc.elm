-- Dynamic dispatch names: toString, showsPrecFlip

-- Any functions with defined non-default instances
-- need to be dynamic.
--
-- In this case: "showsPrec"
-- But we can only be dynamic in the first argument,
-- so we'll flip the argument order "showsPrecFlip"

type ADT a
  = Ctor1
  | Ctor2 Int
  | Ctor3 a String
  | Ctor4 (ADT a)

-- This is already in the "prelude" for the core language and
-- the surface language parser doesn't like us repeating it (Bools
-- are built in to the surface language) but here it is for reference.
-- type Bool
--   = True
--   | False


-- We can see the derived show instances using the GHC option -ddump-deriv
--
-- After a bit of cleanup, the ADTs above has these Show instances:
--
-- ==================== Derived instances ====================
-- Derived class instances:
--   instance Show a => Show (ADT a) where
--     showsPrec _ Ctor1
--       = showString "Ctor1"
--     showsPrec precN (Ctor2 int)
--       = showParen
--           (precN >= 11)
--           ((showString "Ctor2 ") . (showsPrec 11 int))
--     showsPrec precN (Ctor3 a str)
--       = showParen
--           (precN >= 11)
--           ((showString "Ctor3 ") .
--              ((showsPrec 11 a) . (showSpace . (showsPrec 11 str)))
--           )
--     showsPrec precN (Ctor4 adt)
--       = showParen
--           (precN >= 11)
--           ((showString "Ctor4 ") . (showsPrec 11 adt))
--
--
--   instance Show Bool where
--     showsPrec _ False
--       = showString "False"
--     showsPrec _ True = showString "True"






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



show x = shows x ""

shows =  showsPrec 0

showsPrec precN a = showsPrecFlip a precN

-- showChar        :: Char -> ShowS
-- showChar        =  (:)
-- (+) is a primitive operator in a our language and cannot be partially applied, so don't curry
showChar l r = l + r

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

-- instance Show Int where
--     showsPrec = showSignedInt
showsPrecFlip : Num -> Num -> String -> String
showsPrecFlip num _ s = numToStringBuiltin num + s



-- Translation to our language:

-- -- | utility function converting a 'String' to a show function that
-- -- simply prepends the string unchanged.
-- showString      :: String -> ShowS
-- showString      =  (++)
--
-- (+) is a primitive operator in a our language and cannot be partially applied, so don't curry
showString l r = l + r
--
-- -- | utility function that surrounds the inner show function with
-- -- parentheses when the 'Bool' parameter is 'True'.
-- showParen       :: Bool -> ShowS -> ShowS
-- showParen b p   =  if b then showChar '(' . p . showChar ')' else p
showParen b p =  if b then showChar "(" << p << showChar ")" else p
--
-- showSpace :: ShowS
-- showSpace = {-showChar ' '-} \ xs -> ' ' : xs
showSpace = \xs -> " " + xs


--   instance Show a => Show (ADT a) where
--     showsPrec _ Ctor1
--       = showString "Ctor1"
--     showsPrec precN (Ctor2 int)
--       = showParen
--           (precN >= 11)
--           ((showString "Ctor2 ") . (showsPrec 11 int))
--     showsPrec precN (Ctor3 a str)
--       = showParen
--           (precN >= 11)
--           ((showString "Ctor3 ") .
--              ((showsPrec 11 a) . (showSpace . (showsPrec 11 str)))
--           )
--     showsPrec precN (Ctor4 adt)
--       = showParen
--           (precN >= 11)
--           ((showString "Ctor4 ") . (showsPrec 11 adt))
showsPrecFlip : ADT a -> Num -> String -> String
showsPrecFlip adt precN = case adt of
  Ctor1 ->
    showString "Ctor1"
  Ctor2 int ->
    showParen
        (precN >= 11)
        ((showString "Ctor2 ") << (showsPrec 11 int))
  Ctor3 a str ->
    showParen
        (precN >= 11)
        ((showString "Ctor3 ") <<
           ((showsPrec 11 a) << (showSpace << (showsPrec 11 str)))
        )
  Ctor4 adt ->
    showParen
        (precN >= 11)
        ((showString "Ctor4 ") << (showsPrec 11 adt))


--   instance Show Bool where
--     showsPrec _ False
--       = showString "False"
--     showsPrec _ True = showString "True"
showsPrecFlip : Bool -> Num -> String -> String
showsPrecFlip bool precN = case bool of
  False ->
    showString "False"
  True -> showString "True"




toString : a -> String
toString = show

adt = Ctor3 True "asdf"

-- (Ctor1 : ADT Num)
-- (Ctor2 5 : ADT Num)
-- (Ctor3 True "asdf" : ADT Bool)
(Ctor4 adt : ADT Bool)
-- (False : Bool)
