-- LanguageTypes.hs
-- This contains the necessary type definitions for our toy language. In
-- particular, each new part of the expression must define its own instance of
-- the Parsoid typeclass.

--Must define types Exp and ParseTree
--TODO Explain
type Exp = Tup Exp Exp
         | Plus Exp Exp
         | Lit Int

--TODO Explain
type ParseTree = TupNode Exp ParseTree ParseTree
               | PlusNode Exp ParseTree ParseTree
               | LitNode Exp --Will only ever contain a Lit Int

--Language components
instance (Parsoid Exp ParseTree) Tup where
    parseExp = --TODO
    syncStep = --TODO
