module Info exposing
  ( WithInfo
  , withInfo
  , copyInfo
  , withDummyInfo
  , hasDummyInfo
  , mapInfoVal
  , parsedThingToLocation
  )

import Pos exposing (Pos, dummyPos)

type alias WithInfo a =
  { val : a
  , start : Pos
  , end : Pos
  }

withInfo : a -> Pos -> Pos -> WithInfo a
withInfo x start end =
  { val = x
  , start = start
  , end = end
  }

copyInfo : WithInfo a -> WithInfo b -> WithInfo b
copyInfo from to =
  { to | start = from.start, end = from.end }

withDummyInfo : a -> WithInfo a
withDummyInfo x =
  withInfo x dummyPos dummyPos

hasDummyInfo : WithInfo a -> Bool
hasDummyInfo w =
  (w.start, w.end) == (dummyPos, dummyPos)

mapInfoVal : (a -> b) -> WithInfo a -> WithInfo b
mapInfoVal f wa =
  { wa | val = f wa.val }

parsedThingToLocation : WithInfo a -> (Int, Int)
parsedThingToLocation parsedThing =
  (parsedThing.start.line, parsedThing.start.col)
