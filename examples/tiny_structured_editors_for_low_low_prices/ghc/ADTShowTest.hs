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
-- Derived type family instances:
--
--
--
-- ==================== Filling in method body ====================
-- Show [ADT a_a2ID[ssk:1]]
--   show = $dmshow @(ADT a_a2ID[ssk:1])
--
--
--
-- ==================== Filling in method body ====================
-- Show [ADT a_a2ID[ssk:1]]
--   showList = $dmshowList @(ADT a_a2ID[ssk:1])