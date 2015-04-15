-- Parsoid.hs
-- Defines the Parsoid typeclass. This typeclass contains the necessary
-- definitions to be compatible with the 'sync' approach that we're taking to
-- updating programs from a change in output.

--Necessary definitions to include:
-- parseExp :: Exp -> ParseTree
-- syncStep :: Exp -> ParseTree -> ParseTree

--Parsoid is of kind * -> * (-> * ?)
--For Language, Parsoid is Parsoid Exp ParseTree
