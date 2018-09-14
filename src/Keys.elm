module Keys exposing (..)

import Char

--------------------------------------------------------------------------------
-- Key Combinations

escShift            = List.sort [keyEsc, keyShift]
escape              = List.sort [keyEsc]
enter               = List.sort [keyEnter]
a                   = List.sort [keyA]
c                   = List.sort [Char.toCode 'C']
d                   = List.sort [Char.toCode 'D']
e                   = List.sort [Char.toCode 'E']
z                   = List.sort [Char.toCode 'Z']
x                   = List.sort [Char.toCode 'X']
y                   = List.sort [Char.toCode 'Y']
-- keysShiftZ              = List.sort [keyShift, Char.toCode 'Z']
g                   = List.sort [Char.toCode 'G']
h                   = List.sort [Char.toCode 'H']
o                   = List.sort [Char.toCode 'O']
p                   = List.sort [Char.toCode 'P']
q                   = List.sort [Char.toCode 'Q']
t                   = List.sort [Char.toCode 'T']
s                   = List.sort [Char.toCode 'S']
openParen           = List.sort [keyShift, Char.toCode '9']
openBrace           = List.sort [219]
openCurly           = List.sort [keyShift, 219]
backslash           = List.sort [220]
-- shift               = List.sort [keyShift]
-- shiftS              = List.sort [keyShift, Char.toCode 'S']
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

-- Mac command key is 17, 91, 93, or 224 depending on browser. https://stackoverflow.com/a/3922353
-- Note: Ctrl is also 17, so this catches cmd and ctrl.
isCommandKey code   = List.member code [17, 91, 93, 224]

keyEnter            = 13
keyEsc              = 27
keyCtrl             = 17
keyShift            = 16
keySpace            = 32
keyLeft             = 37
keyUp               = 38
keyRight            = 39
keyDown             = 40
keyBackspace        = 8
keyDelete           = 46 -- 127
keyA                = 65
keyB                = 66
keyC                = 67
keyD                = 68
keyE                = 69
keyG                = 71
keyH                = Char.toCode 'H'
keyI                = Char.toCode 'I'
keyJ                = Char.toCode 'J'
keyL                = Char.toCode 'L'
keyS                = Char.toCode 'S'
keyZ                = 90
keyPlusEqual        = 187
