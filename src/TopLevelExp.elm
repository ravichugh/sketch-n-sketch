module TopLevelExp exposing
  ( TopLevelExp
  , fuseTopLevelExp
  , fuseTopLevelExps
  )

import Lang exposing (..)
import Info exposing (..)

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

type alias TopLevelExp = WithInfo (Exp -> Exp_)

--------------------------------------------------------------------------------
-- Top-Level Expression Fusing
--------------------------------------------------------------------------------

fuseTopLevelExp : TopLevelExp -> Exp -> Exp
fuseTopLevelExp tld rest =
  withInfo (tld.val rest) tld.start tld.end

fuseTopLevelExps : (List TopLevelExp) -> Exp -> Exp
fuseTopLevelExps tlds rest =
  List.foldr fuseTopLevelExp rest tlds
