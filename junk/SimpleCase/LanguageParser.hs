-- LanguageParser.hs
-- This defines the parsing functions necessary for our toy language as
-- specified in LanguageTypes. This should specify everything needed to go from
-- Strings (code) to Outputs and from Outputs to Strings. :)

import LanguageTypes

--Parses a String and returns a ParseTree
parseLang :: String -> ParseTree

--Parses a String and returns
runLang :: String -> Exp
runLang = getVal . parseLang
