module DeuceGeometry exposing
  ( CodePos
  , AbsolutePos
  , PosTransform
  , Hull
  , LineHulls
  , lineHulls
  , hull
  , hullPoints
  )

import Utils

--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- Indexed. 0-based for our uses.

type alias Indexed a =
  (Int, a)

-- Positions

type alias CodePos =
  (Int, Int)

type alias AbsolutePos =
  (Float, Float)

type alias PosTransform =
  CodePos -> AbsolutePos

-- Hulls

type alias Hull =
  List AbsolutePos

-- Lines

type alias Line =
  { startCol : Int
  , endCol : Int
  , val : String
  }

type alias LineHulls =
  List Hull

--------------------------------------------------------------------------------
-- Line Functions
--------------------------------------------------------------------------------

emptyLine : Line
emptyLine =
  { startCol = 0
  , endCol = 0
  , val = ""
  }

isBlankLine : Line -> Bool
isBlankLine line =
  line.startCol == line.endCol

computeMaxLineLength : List String -> Int
computeMaxLineLength strings =
  let
    lens =
      List.map String.length strings
  in
    Maybe.withDefault 0 <|
      List.maximum lens

untrimmedLine : Int -> List String -> List Line
untrimmedLine maxLen strings =
  let
    startCol =
      0
    endCol =
      maxLen + 1
    lineMapper s =
      { startCol =
          startCol
      , endCol =
          endCol
      , val =
          s
      }
  in
    List.map lineMapper strings

trimmedLine : List String -> List Line
trimmedLine =
  List.map <|
    \s ->
      let
        trimmed =
          String.trim s
        trimmedLen =
          String.length trimmed
        trimmedRightLen =
          (String.length << String.trimRight) s
        startCol =
          trimmedRightLen - trimmedLen
        endCol =
          startCol + trimmedLen
      in
        { startCol =
            startCol
        , endCol =
            endCol
        , val =
            trimmed
        }

lineHull : PosTransform -> Indexed Line -> Hull
lineHull c2a (row, line) =
  List.map c2a
    [ (line.startCol, row)
    , (line.startCol, row + 1)
    , (line.endCol, row + 1)
    , (line.endCol, row)
    ]

-- Returns: (untrimmed, trimmed, max line length)
lineHulls : PosTransform -> String -> (LineHulls, LineHulls, Int)
lineHulls c2a code =
  let
    lines =
      String.lines code
    maxLineLength =
      computeMaxLineLength lines
    pipeline lineKind =
      lines
        |> lineKind
        |> Utils.zipi0
        |> List.map (lineHull c2a)
  in
    ( pipeline <| untrimmedLine maxLineLength
    , pipeline trimmedLine
    , maxLineLength
    )

--------------------------------------------------------------------------------
-- Hull Functions
--------------------------------------------------------------------------------

zeroWidthPadding : Float
zeroWidthPadding = 2

specialEndFlag : Float
specialEndFlag =
  -123456789

addBleed : Float -> AbsolutePos -> AbsolutePos
addBleed bleedAmount (x, y) =
  if x == specialEndFlag then
    ( 0
    , y
    )
  else if x <= 0 then
    ( -bleedAmount
    , y
    )
  else
    (x, y)

addFinalEndBleed : AbsolutePos -> AbsolutePos
addFinalEndBleed (x, y) =
  if x <= 0 then
    ( specialEndFlag
    , y
    )
  else
    (x, y)

removeUpperCrust : Float -> AbsolutePos -> AbsolutePos
removeUpperCrust crustSize (x, y) =
  (x, y + crustSize)

removeLowerCrust : Float -> AbsolutePos -> AbsolutePos
removeLowerCrust crustSize (x, y) =
  (x, y - crustSize)

maybeDecrust :
  Float
    -> Bool
    -> List (Float -> AbsolutePos -> AbsolutePos)
    -> Hull
    -> Hull
maybeDecrust crustSize shouldDecrust decrusters =
  Utils.applyIf shouldDecrust <|
    List.map2 (\dc pos -> dc crustSize pos) decrusters

magicDecrust :
  PosTransform
    -> Float
    -> Bool
    -> List ((Float -> AbsolutePos -> AbsolutePos), Int, Int)
    -> Hull
magicDecrust c2a crustSize shouldDecrust posInfos =
  let modifier =
    (\(dc, col, row) ->
      Utils.applyIf shouldDecrust
        (dc crustSize)
        (c2a (col, row))
    )
  in
  List.map modifier posInfos

-- NOTE: Use 0-indexing for columns and rows.
hull :
  PosTransform
    -> Float -> Float
    -> LineHulls
    -> Bool -> Bool
    -> Int -> Int -> Int -> Int
    -> Hull
hull
  c2a
  crustSize bleedAmount
  lineHulls
  shouldAddBleed shouldDecrust
  startCol startRow endCol endRow =
    let
      relevantLines =
        Utils.slice (startRow + 1) endRow lineHulls
      modifier = if shouldAddBleed then List.map (addBleed bleedAmount) else identity
    in
      modifier <|
        -- Multi-line
        if startRow /= endRow then
          -- Left of first line
          ( magicDecrust c2a crustSize shouldDecrust
              [ (removeUpperCrust, startCol, startRow)
              , (always identity, startCol, startRow + 1)
              ]
          ) ++

          -- Left of middle lines
          ( List.concat <|
              List.map (List.take 2)
                relevantLines
          ) ++

          -- Left of last line
          ( maybeDecrust crustSize shouldDecrust [always identity, removeLowerCrust] <|
              List.take 2 <|
                Maybe.withDefault [] <|
                  Utils.maybeGeti0 endRow lineHulls
          ) ++

          -- Right of last line
          (
            magicDecrust c2a crustSize shouldDecrust
              [ (removeLowerCrust, endCol, endRow + 1)
              , (always identity, endCol, endRow)
              ] |>
                Utils.applyIf shouldAddBleed (List.map addFinalEndBleed)
          ) ++

          -- Right of middle lines
          ( List.concat <|
              List.map (List.drop 2) <|
                List.reverse relevantLines
          ) ++

          -- Right of first line
          ( maybeDecrust crustSize shouldDecrust [always identity, removeUpperCrust] <|
              List.drop 2 <|
                Maybe.withDefault [] <|
                  Utils.maybeGeti0 startRow lineHulls
          )
        -- Zero-width
        else if startCol == endCol then
          let
            (x, yTop) =
              c2a (startCol, startRow)
            (_, yBottom) =
              c2a (startCol, startRow + 1)
          in
            [ (x - zeroWidthPadding, yTop)
            , (x - zeroWidthPadding, yBottom)
            , (x + zeroWidthPadding, yBottom)
            , (x + zeroWidthPadding, yTop)
            ] |>
              maybeDecrust crustSize shouldDecrust
                [ removeUpperCrust
                , removeLowerCrust
                , removeLowerCrust
                , removeUpperCrust
                ]
        -- Single-line, nonzero-width
        else
          magicDecrust c2a crustSize shouldDecrust
            [ (removeUpperCrust, startCol, startRow)
            , (removeLowerCrust, startCol, startRow + 1)
            , (removeLowerCrust, endCol, startRow + 1)
            , (removeUpperCrust, endCol, startRow)
            ]

hullPoints : Hull -> String
hullPoints =
  let
    pairToString (x, y) =
      (toString x) ++ "," ++ (toString y) ++ " "
  in
    String.concat << List.map pairToString
