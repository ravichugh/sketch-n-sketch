module Prose exposing (extractFromUserStudyTemplate)

import Utils

type alias CodeProse a =
  { a
      | code : String
      , lastRunCode : String
      , prose : Maybe String
  }

extractFromUserStudyTemplate : CodeProse a -> CodeProse a
extractFromUserStudyTemplate m =
  let
    (proseLines, restLines) =
      List.partition
        (String.startsWith "; <!-- PROSE --> ")
        (String.lines m.code)
    residualCode =
      String.join "\n" restLines
  in
  { m
      | code =
          residualCode
      , lastRunCode =
          residualCode
      , prose =
          case proseLines of
            []          -> Nothing
            [innerHTML] -> Just (String.dropLeft 1 innerHTML)
            _           -> Debug.log "Prose expected only 1 HTML line..." Nothing
  }
{-
  { m
      | prose =
          m.code
            |> String.lines
            |> List.filter (String.startsWith ";")
            |> List.map
                 ( Utils.compose
                     [ String.dropLeft 1
                     , String.trim
                     , (++) "<p>"
                     , flip (++) "</p>"
                     ]
                 )
            |> String.concat
            |> Just
  }
-}
