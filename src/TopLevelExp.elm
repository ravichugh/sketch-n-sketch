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

fuseTopLevelExp : TopLevelExp -> WithInfo Exp_ -> WithInfo Exp_
fuseTopLevelExp tld rest =
  withInfo (tld.val (Expr rest)) tld.start tld.end

fuseTopLevelExps : (List TopLevelExp) -> WithInfo Exp_ -> WithInfo Exp_
fuseTopLevelExps tlds rest =
  List.foldr fuseTopLevelExp rest tlds
