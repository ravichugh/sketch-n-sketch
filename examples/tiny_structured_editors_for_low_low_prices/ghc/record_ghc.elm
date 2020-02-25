-- Dynamic dispatch names: toString, showsPrecFlip

-- Any functions with defined non-default instances
-- need to be dynamic.
--
-- In this case: "showsPrec"
-- But we can only be dynamic in the first argument,
-- so we'll flip the argument order "showsPrecFlip"


-- data Record a b c
--   = Record { field1 :: a, field2 :: b, field3 :: c, int :: Int }
--   deriving (Show)
--

-- Core language does not have support records, only ADTs.
type Record a b c
  = Record a b c Num


-- We can see the derived show instances using the GHC option -ddump-deriv
--
-- After a bit of cleanup:
--
-- ==================== Derived instances ====================
-- Derived class instances:
-- instance (Show a, Show b, Show c) =>
--          Show (Record a b c) where
--   showsPrec
--     precN
--     (Record a b c int)
--     = showParen
--         (precN >= 11)
--         ((.)
--            (showString "Record {")
--            ((.)
--               (showString "field1 = ")
--               ((.)
--                  (showsPrec 0 a)
--                  ((.)
--                     showCommaSpace
--                     ((.)
--                        (showString "field2 = ")
--                        ((.)
--                           (showsPrec 0 b)
--                           ((.)
--                              showCommaSpace
--                              ((.)
--                                 (showString "field3 = ")
--                                 ((.)
--                                    (showsPrec 0 c)
--                                    ((.)
--                                       showCommaSpace
--                                       ((.)
--                                          (showString "int = ")
--                                          ((.)
--                                             (showsPrec 0 int)
--                                             (showString "}")))))))))))))
--





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
--
--
-- showCommaSpace :: ShowS
-- showCommaSpace = showString ", "



show x = shows x ""

shows = showsPrec 0

showsPrec precN a = showsPrecFlip a precN

showChar l r = l + r

showString l r = l + r

showParen b p =  if b then showChar "(" << p << showChar ")" else p

showSpace = \xs -> " " + xs

-- The core language (and the string provenance) doesn't have a concept of "character"
-- so we can't translate the Haskell precisely without losing string provenance.
--
-- Also, no escaping in displayed string.
showsPrecFlip : String -> Num -> String -> String
showsPrecFlip str _ s = '"' + str + '"' + s

showsPrecFlip : Num -> Num -> String -> String
showsPrecFlip num _ s = numToStringBuiltin num + s

--   instance Show Bool where
--     showsPrec _ False
--       = showString "False"
--     showsPrec _ True = showString "True"
showsPrecFlip : Bool -> Num -> String -> String
showsPrecFlip bool precN = case bool of
  False ->
    showString "False"
  True -> showString "True"



-- Translation to our language:
--
--
-- showCommaSpace :: ShowS
-- showCommaSpace = showString ", "
showCommaSpace = showString ", "
--
--
-- instance (Show a, Show b, Show c) =>
--          Show (Record a b c) where
--   showsPrec
--     precN
--     (Record a b c int)
--     = showParen
--         (precN >= 11)
--         ((.)
--            (showString "Record {")
--            ((.)
--               (showString "field1 = ")
--               ((.)
--                  (showsPrec 0 a)
--                  ((.)
--                     showCommaSpace
--                     ((.)
--                        (showString "field2 = ")
--                        ((.)
--                           (showsPrec 0 b)
--                           ((.)
--                              showCommaSpace
--                              ((.)
--                                 (showString "field3 = ")
--                                 ((.)
--                                    (showsPrec 0 c)
--                                    ((.)
--                                       showCommaSpace
--                                       ((.)
--                                          (showString "int = ")
--                                          ((.)
--                                             (showsPrec 0 int)
--                                             (showString "}")))))))))))))


showsPrecFlip : Record a b c -> Num -> String -> String
showsPrecFlip record precN = case record of
  Record a b c int ->
    showParen
      (precN >= 11)
      ((<<)
         (showString "Record {")
         ((<<)
            (showString "field1 = ")
            ((<<)
               (showsPrec 0 a)
               ((<<)
                  showCommaSpace
                  ((<<)
                     (showString "field2 = ")
                     ((<<)
                        (showsPrec 0 b)
                        ((<<)
                           showCommaSpace
                           ((<<)
                              (showString "field3 = ")
                              ((<<)
                                 (showsPrec 0 c)
                                 ((<<)
                                    showCommaSpace
                                    ((<<)
                                       (showString "int = ")
                                       ((<<)
                                          (showsPrec 0 int)
                                          (showString "}")))))))))))))




toString : a -> String
toString = show

nestedRecord = Record 1 2 3 4

-- (Record 10 True "asdf" 20 : Record Num Bool String)
(Record nestedRecord True "asdf" 20 : Record (Record Num Num Num) Bool String)
