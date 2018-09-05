module MissingNumberMethods exposing (..)

import Native.MissingNumberMethods

-- Exponentiation
exp : Float -> Float
exp a = Native.MissingNumberMethods.exp a

(**) : Float -> Float -> Float
(**) a b = Native.MissingNumberMethods.power a b

infixr 8 **

(%%): Float -> Float -> Float
(%%) a b = Native.MissingNumberMethods.modulo a b

infix 7 %%