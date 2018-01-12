module History exposing
  ( History
  , blank
  , commit
  , alwaysCommit
  , begin
  , hasPast
  , hasExtendedPast
  , hasFuture
  , forward
  , backward
  , prior
  , mostRecent
  , next
  , previousN
  )

type History a
  = H (List a, List a) -- (past, future)

blank : History a
blank =
  H ([], [])

-- May want to eventually have a maximum history length
commit : (a -> a -> Bool) -> a -> History a -> History a
commit updateCondition snapshot history =
  let
    (H (past, _)) =
      history
    updatedHistory =
      H (snapshot :: past, [])
  in
    case mostRecent history of
      Just prev ->
        if updateCondition prev snapshot then
          updatedHistory
        else
          history

      Nothing ->
        updatedHistory

alwaysCommit : a -> History a -> History a
alwaysCommit =
  commit (\_ _ -> True)

begin : a -> History a
begin snapshot =
  alwaysCommit snapshot blank

hasPast : History a -> Bool
hasPast (H (past, _)) =
  not <| List.isEmpty past

hasExtendedPast : History a -> Bool
hasExtendedPast (H (past, _)) =
  List.length past > 1

hasFuture : History a -> Bool
hasFuture (H (_, future)) =
  not <| List.isEmpty future

backward : History a -> Maybe (History a)
backward (H (past, future)) =
  case past of
    [] ->
      Nothing

    snapshot :: older ->
      Just <|
        H (older, snapshot :: future)

forward : History a -> Maybe (History a)
forward (H (past, future))=
  case future of
    [] ->
      Nothing

    snapshot :: newer ->
      Just <|
        H (snapshot :: past, newer)

prior : History a -> Maybe a
prior = previousN 2

mostRecent : History a -> Maybe a
mostRecent (H (past, _)) =
  List.head past

next : History a -> Maybe a
next (H (_, future)) =
  List.head future

previousN : Int -> History a -> Maybe a
previousN n history =
  if n < 1 then
    Nothing
  else if n == 1 then
    mostRecent history
  else
    history
      |> backward
      |> Maybe.andThen (previousN <| n - 1)
