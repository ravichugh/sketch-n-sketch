module ColorNum exposing
  (maxColorNum, numToColor, numToColorScaled, convertStringToRgbAndHue)

import Utils
import Dict


type alias Num = Float -- Avoid importing all of Lang

maxColorNum = 530 -- See also ShapeWidgets.wColorSlider


-- numToColor : Int -> (Int, Int, Int, Float)
numToColor : Num -> (Int, Int, Int, Float)
numToColor = numToColorScaled maxColorNum


-- w -> i:[0,w) -> RGBA
numToColorScaled : Float -> Num -> (Int, Int, Int, Float)
numToColorScaled width i =
  let rescale (loIn, hiIn) (loOut, hiOut) x =
    let rangeIn  = hiIn  - loIn in
    let rangeOut = hiOut - loOut in
    ((toFloat x - loIn) / rangeIn) * rangeOut + loOut
  in
  let gray j = round <| rescale (380, 480) (0, 255) j in
  let j = round <| (i / width) * maxColorNum in
  if Utils.between j (  0, 360) then                           numToSaturatedColor j    else -- color
  if Utils.between j (360, 380) then                           (0, 0, 0, 1.0)           else -- black
  if Utils.between j (380, 480) then let x = gray j in         (x,x,x,1.0)              else -- grayscale
  if Utils.between j (480, 500) then                           (255, 255, 255, 1.0)          -- white
  else let opacity = sqrt (rescale (500, 529) (1.0, 0.0) j) in (255, 255, 255, opacity)      -- translucent white


-- Maps a value in [0,360) to an RGB value that lies on the rainbow.
-- Note that you cannot get white or black from this.
-- Note that the choice of 0 to 360 implies degrees as a natural representation.
numToSaturatedColor : Int -> (Int, Int, Int, Float)
numToSaturatedColor val =
  let
    n    = toFloat <| val % 360
    i    = floor n // 60
    max  = 200
    min  = 55
    diff = max - min
  in
   case i of
     0 -> (max, round <| min + diff * (1 - (60 - n) / 60),  min, 1.0)
     1 -> (round <| max - diff * (1 - (120 - n) / 60), max, min, 1.0)
     2 -> (min, max, round <| min + diff * (1 - (180 - n) / 60), 1.0)
     3 -> (min, round <| max - diff * (1 - (240 - n) / 60), max, 1.0)
     4 -> (round <| min + diff * (1 - (300 - n) / 60), min, max, 1.0)
     5 -> (max, min, round <| max - diff * (1 - (360 - n) / 60), 1.0)
     _ -> Debug.crash "numToColor"


-- RGB and HSL codes generated with scripts/colorNames.js, which uses
-- w3color.js (https://www.w3schools.com/lib/w3color.js) and node repl.

htmlColorNames =
  List.map (Tuple.mapFirst String.toLower)
    [ ("AliceBlue", ((240, 248, 255), (208, 1, 0.97)))
    , ("AntiqueWhite", ((250, 235, 215), (34, 0.78, 0.91)))
    , ("Aqua", ((0, 255, 255), (180, 1, 0.5)))
    , ("Aquamarine", ((127, 255, 212), (160, 1, 0.75)))
    , ("Azure", ((240, 255, 255), (180, 1, 0.97)))
    , ("Beige", ((245, 245, 220), (60, 0.56, 0.91)))
    , ("Bisque", ((255, 228, 196), (33, 1, 0.88)))
    , ("Black", ((0, 0, 0), (0, 0, 0)))
    , ("BlanchedAlmond", ((255, 235, 205), (36, 1, 0.9)))
    , ("Blue", ((0, 0, 255), (240, 1, 0.5)))
    , ("BlueViolet", ((138, 43, 226), (271, 0.76, 0.53)))
    , ("Brown", ((165, 42, 42), (0, 0.59, 0.41)))
    , ("BurlyWood", ((222, 184, 135), (34, 0.57, 0.7)))
    , ("CadetBlue", ((95, 158, 160), (182, 0.25, 0.5)))
    , ("Chartreuse", ((127, 255, 0), (90, 1, 0.5)))
    , ("Chocolate", ((210, 105, 30), (25, 0.75, 0.47)))
    , ("Coral", ((255, 127, 80), (16, 1, 0.66)))
    , ("CornflowerBlue", ((100, 149, 237), (219, 0.79, 0.66)))
    , ("Cornsilk", ((255, 248, 220), (48, 1, 0.93)))
    , ("Crimson", ((220, 20, 60), (348, 0.83, 0.47)))
    , ("Cyan", ((0, 255, 255), (180, 1, 0.5)))
    , ("DarkBlue", ((0, 0, 139), (240, 1, 0.27)))
    , ("DarkCyan", ((0, 139, 139), (180, 1, 0.27)))
    , ("DarkGoldenRod", ((184, 134, 11), (43, 0.89, 0.38)))
    , ("DarkGray", ((169, 169, 169), (0, 0, 0.66)))
    , ("DarkGrey", ((169, 169, 169), (0, 0, 0.66)))
    , ("DarkGreen", ((0, 100, 0), (120, 1, 0.2)))
    , ("DarkKhaki", ((189, 183, 107), (56, 0.38, 0.58)))
    , ("DarkMagenta", ((139, 0, 139), (300, 1, 0.27)))
    , ("DarkOliveGreen", ((85, 107, 47), (82, 0.39, 0.3)))
    , ("DarkOrange", ((255, 140, 0), (33, 1, 0.5)))
    , ("DarkOrchid", ((153, 50, 204), (280, 0.61, 0.5)))
    , ("DarkRed", ((139, 0, 0), (0, 1, 0.27)))
    , ("DarkSalmon", ((233, 150, 122), (15, 0.72, 0.7)))
    , ("DarkSeaGreen", ((143, 188, 143), (120, 0.25, 0.65)))
    , ("DarkSlateBlue", ((72, 61, 139), (248, 0.39, 0.39)))
    , ("DarkSlateGray", ((47, 79, 79), (180, 0.25, 0.25)))
    , ("DarkSlateGrey", ((47, 79, 79), (180, 0.25, 0.25)))
    , ("DarkTurquoise", ((0, 206, 209), (181, 1, 0.41)))
    , ("DarkViolet", ((148, 0, 211), (282, 1, 0.41)))
    , ("DeepPink", ((255, 20, 147), (328, 1, 0.54)))
    , ("DeepSkyBlue", ((0, 191, 255), (195, 1, 0.5)))
    , ("DimGray", ((105, 105, 105), (0, 0, 0.41)))
    , ("DimGrey", ((105, 105, 105), (0, 0, 0.41)))
    , ("DodgerBlue", ((30, 144, 255), (210, 1, 0.56)))
    , ("FireBrick", ((178, 34, 34), (0, 0.68, 0.42)))
    , ("FloralWhite", ((255, 250, 240), (40, 1, 0.97)))
    , ("ForestGreen", ((34, 139, 34), (120, 0.61, 0.34)))
    , ("Fuchsia", ((255, 0, 255), (300, 1, 0.5)))
    , ("Gainsboro", ((220, 220, 220), (0, 0, 0.86)))
    , ("GhostWhite", ((248, 248, 255), (240, 1, 0.99)))
    , ("Gold", ((255, 215, 0), (51, 1, 0.5)))
    , ("GoldenRod", ((218, 165, 32), (43, 0.74, 0.49)))
    , ("Gray", ((128, 128, 128), (0, 0, 0.5)))
    , ("Grey", ((128, 128, 128), (0, 0, 0.5)))
    , ("Green", ((0, 128, 0), (120, 1, 0.25)))
    , ("GreenYellow", ((173, 255, 47), (84, 1, 0.59)))
    , ("HoneyDew", ((240, 255, 240), (120, 1, 0.97)))
    , ("HotPink", ((255, 105, 180), (330, 1, 0.71)))
    , ("IndianRed ", ((0, 0, 0), (0, 0, 0)))
    , ("Indigo  ", ((0, 0, 0), (0, 0, 0)))
    , ("Ivory", ((255, 255, 240), (60, 1, 0.97)))
    , ("Khaki", ((240, 230, 140), (54, 0.77, 0.75)))
    , ("Lavender", ((230, 230, 250), (240, 0.67, 0.94)))
    , ("LavenderBlush", ((255, 240, 245), (340, 1, 0.97)))
    , ("LawnGreen", ((124, 252, 0), (90, 1, 0.49)))
    , ("LemonChiffon", ((255, 250, 205), (54, 1, 0.9)))
    , ("LightBlue", ((173, 216, 230), (195, 0.53, 0.79)))
    , ("LightCoral", ((240, 128, 128), (0, 0.79, 0.72)))
    , ("LightCyan", ((224, 255, 255), (180, 1, 0.94)))
    , ("LightGoldenRodYellow", ((250, 250, 210), (60, 0.8, 0.9)))
    , ("LightGray", ((211, 211, 211), (0, 0, 0.83)))
    , ("LightGrey", ((211, 211, 211), (0, 0, 0.83)))
    , ("LightGreen", ((144, 238, 144), (120, 0.73, 0.75)))
    , ("LightPink", ((255, 182, 193), (351, 1, 0.86)))] ++
    [ ("LightSalmon", ((255, 160, 122), (17, 1, 0.74)))
    , ("LightSeaGreen", ((32, 178, 170), (177, 0.7, 0.41)))
    , ("LightSkyBlue", ((135, 206, 250), (203, 0.92, 0.75)))
    , ("LightSlateGray", ((119, 136, 153), (210, 0.14, 0.53)))
    , ("LightSlateGrey", ((119, 136, 153), (210, 0.14, 0.53)))
    , ("LightSteelBlue", ((176, 196, 222), (214, 0.41, 0.78)))
    , ("LightYellow", ((255, 255, 224), (60, 1, 0.94)))
    , ("Lime", ((0, 255, 0), (120, 1, 0.5)))
    , ("LimeGreen", ((50, 205, 50), (120, 0.61, 0.5)))
    , ("Linen", ((250, 240, 230), (30, 0.67, 0.94)))
    , ("Magenta", ((255, 0, 255), (300, 1, 0.5)))
    , ("Maroon", ((128, 0, 0), (0, 1, 0.25)))
    , ("MediumAquaMarine", ((102, 205, 170), (160, 0.51, 0.6)))
    , ("MediumBlue", ((0, 0, 205), (240, 1, 0.4)))
    , ("MediumOrchid", ((186, 85, 211), (288, 0.59, 0.58)))
    , ("MediumPurple", ((147, 112, 219), (260, 0.6, 0.65)))
    , ("MediumSeaGreen", ((60, 179, 113), (147, 0.5, 0.47)))
    , ("MediumSlateBlue", ((123, 104, 238), (249, 0.8, 0.67)))
    , ("MediumSpringGreen", ((0, 250, 154), (157, 1, 0.49)))
    , ("MediumTurquoise", ((72, 209, 204), (178, 0.6, 0.55)))
    , ("MediumVioletRed", ((199, 21, 133), (322, 0.81, 0.43)))
    , ("MidnightBlue", ((25, 25, 112), (240, 0.64, 0.27)))
    , ("MintCream", ((245, 255, 250), (150, 1, 0.98)))
    , ("MistyRose", ((255, 228, 225), (6, 1, 0.94)))
    , ("Moccasin", ((255, 228, 181), (38, 1, 0.85)))
    , ("NavajoWhite", ((255, 222, 173), (36, 1, 0.84)))
    , ("Navy", ((0, 0, 128), (240, 1, 0.25)))
    , ("OldLace", ((253, 245, 230), (39, 0.85, 0.95)))
    , ("Olive", ((128, 128, 0), (60, 1, 0.25)))
    , ("OliveDrab", ((107, 142, 35), (80, 0.6, 0.35)))
    , ("Orange", ((255, 165, 0), (39, 1, 0.5)))
    , ("OrangeRed", ((255, 69, 0), (16, 1, 0.5)))
    , ("Orchid", ((218, 112, 214), (302, 0.59, 0.65)))
    , ("PaleGoldenRod", ((238, 232, 170), (55, 0.67, 0.8)))
    , ("PaleGreen", ((152, 251, 152), (120, 0.93, 0.79)))
    , ("PaleTurquoise", ((175, 238, 238), (180, 0.65, 0.81)))
    , ("PaleVioletRed", ((219, 112, 147), (340, 0.6, 0.65)))
    , ("PapayaWhip", ((255, 239, 213), (37, 1, 0.92)))
    , ("PeachPuff", ((255, 218, 185), (28, 1, 0.86)))
    , ("Peru", ((205, 133, 63), (30, 0.59, 0.53)))
    , ("Pink", ((255, 192, 203), (350, 1, 0.88)))
    , ("Plum", ((221, 160, 221), (300, 0.47, 0.75)))
    , ("PowderBlue", ((176, 224, 230), (187, 0.52, 0.8)))
    , ("Purple", ((128, 0, 128), (300, 1, 0.25)))
    , ("RebeccaPurple", ((102, 51, 153), (270, 0.5, 0.4)))
    , ("Red", ((255, 0, 0), (0, 1, 0.5)))
    , ("RosyBrown", ((188, 143, 143), (0, 0.25, 0.65)))
    , ("RoyalBlue", ((65, 105, 225), (225, 0.73, 0.57)))
    , ("SaddleBrown", ((139, 69, 19), (25, 0.76, 0.31)))
    , ("Salmon", ((250, 128, 114), (6, 0.93, 0.71)))
    , ("SandyBrown", ((244, 164, 96), (28, 0.87, 0.67)))
    , ("SeaGreen", ((46, 139, 87), (146, 0.5, 0.36)))
    , ("SeaShell", ((255, 245, 238), (25, 1, 0.97)))
    , ("Sienna", ((160, 82, 45), (19, 0.56, 0.4)))
    , ("Silver", ((192, 192, 192), (0, 0, 0.75)))
    , ("SkyBlue", ((135, 206, 235), (197, 0.71, 0.73)))
    , ("SlateBlue", ((106, 90, 205), (248, 0.53, 0.58)))
    , ("SlateGray", ((112, 128, 144), (210, 0.13, 0.5)))
    , ("SlateGrey", ((112, 128, 144), (210, 0.13, 0.5)))
    , ("Snow", ((255, 250, 250), (0, 1, 0.99)))
    , ("SpringGreen", ((0, 255, 127), (150, 1, 0.5)))
    , ("SteelBlue", ((70, 130, 180), (207, 0.44, 0.49)))
    , ("Tan", ((210, 180, 140), (34, 0.44, 0.69)))
    , ("Teal", ((0, 128, 128), (180, 1, 0.25)))
    , ("Thistle", ((216, 191, 216), (300, 0.24, 0.8)))
    , ("Tomato", ((255, 99, 71), (9, 1, 0.64)))
    , ("Turquoise", ((64, 224, 208), (174, 0.72, 0.56)))
    , ("Violet", ((238, 130, 238), (300, 0.76, 0.72)))
    , ("Wheat", ((245, 222, 179), (39, 0.77, 0.83)))
    , ("White", ((255, 255, 255), (0, 0, 1)))
    , ("WhiteSmoke", ((245, 245, 245), (0, 0, 0.96)))
    , ("Yellow", ((255, 255, 0), (60, 1, 0.5)))
    , ("YellowGreen", ((154, 205, 50), (80, 0.61, 0.5)))
    ]

--------------------------------------------------------------------------------

convertStringToRgbAndHue (eid, string) =
  let colorName = String.toLower string in
  let values = Utils.maybeFind colorName htmlColorNames in
  values |> Utils.mapMaybe (\((r,g,b), (h,_,_)) ->
    let colorNum =
      if colorName == "black" then 360
      else if colorName == "white" then 499
      else if String.contains "gray" colorName ||
              String.contains "grey" colorName then 450
                -- not dealing with different grays individually
      else h
    in
    (eid, (r,g,b), colorNum)
  )
