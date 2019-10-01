{-# OPTIONS_GHC -ddump-deriv #-}

module ADTShowTest where

data ADT a
  = Ctor1
  | Ctor2 Int
  | Ctor3 a String
  | Ctor4 (ADT a)
  deriving (Show)


data Bool
  = False
  | True
  deriving (Show)


data Record a b c
  = Record { field1 :: a, field2 :: b, field3 :: c, int :: Int }
  deriving (Show)


-- $ ghc ADTShowTest.hs
--
-- After a bit of cleanup:
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
--