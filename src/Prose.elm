module Prose exposing (extractFromUserStudyTemplate)

import Utils

type alias CodeProse a =
  { a
      | code : String
      , prose : Maybe String
  }

extractFromUserStudyTemplate : CodeProse a -> CodeProse a
extractFromUserStudyTemplate m =
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
