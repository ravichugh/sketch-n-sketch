module Keys where

import Char

--------------------------------------------------------------------------------
-- Key Combinations

metaShift           = List.sort [keyMeta, keyShift]
escShift            = List.sort [keyEsc, keyShift]
escape              = List.sort [keyEsc]
enter               = List.sort [keyEnter]
c                   = List.sort [Char.toCode 'C']
d                   = List.sort [Char.toCode 'D']
e                   = List.sort [Char.toCode 'E']
z                   = List.sort [Char.toCode 'Z']
y                   = List.sort [Char.toCode 'Y']
-- keysShiftZ              = List.sort [keyShift, Char.toCode 'Z']
g                   = List.sort [Char.toCode 'G']
h                   = List.sort [Char.toCode 'H']
o                   = List.sort [Char.toCode 'O']
p                   = List.sort [Char.toCode 'P']
t                   = List.sort [Char.toCode 'T']
s                   = List.sort [Char.toCode 'S']
shift               = List.sort [keyShift]
shiftS              = List.sort [keyShift, Char.toCode 'S']
left                = List.sort [keyLeft]
right               = List.sort [keyRight]
up                  = List.sort [keyUp]
down                = List.sort [keyDown]
shiftLeft           = List.sort [keyShift, keyLeft]
shiftRight          = List.sort [keyShift, keyRight]
shiftUp             = List.sort [keyShift, keyUp]
shiftDown           = List.sort [keyShift, keyDown]
backspace           = List.sort [keyBackspace]
delete              = List.sort [keyDelete]

metaPlus keys       = List.sort (keyMeta :: keys)
commandPlus keys    = List.sort (keyCommand :: keys)

keyEnter            = 13
keyEsc              = 27
-- TODO meta vs. command
keyMeta             = 91
keyCommand          = 224
keyCtrl             = 17
keyShift            = 16
keyLeft             = 37
keyUp               = 38
keyRight            = 39
keyDown             = 40
keyBackspace        = 8
keyDelete           = 46 -- 127
