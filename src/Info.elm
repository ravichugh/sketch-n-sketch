module Info exposing
  ( WithInfo
  , withInfo
  , withDummyInfo
  , hasDummyInfo
  , mapInfo
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

withDummyInfo : a -> WithInfo a
withDummyInfo x =
  withInfo x dummyPos dummyPos

hasDummyInfo : WithInfo a -> Bool
hasDummyInfo w =
  (w.start, w.end) == (dummyPos, dummyPos)

mapInfo : (a -> b) -> WithInfo a -> WithInfo b
mapInfo f wa =
  { wa | val = f wa.val }

parsedThingToLocation : WithInfo a -> (Int, Int)
parsedThingToLocation parsedThing =
  (parsedThing.start.line, parsedThing.start.col)
