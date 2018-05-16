module Info exposing
  ( WithInfo
  , withInfo
  , withDummyInfo
  , hasDummyInfo
  , mapInfo
  , mapInfoWS
  , replaceInfo
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

mapInfoWS : (a -> b) -> WithInfo (c -> a) -> (c -> WithInfo b)
mapInfoWS f wca c =
  withInfo (f (wca.val c)) wca.start wca.end

replaceInfo: WithInfo a -> b -> WithInfo b
replaceInfo wa b = WithInfo b wa.start wa.end