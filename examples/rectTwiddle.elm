absolutePosition x y =
  [["position", "fixed"], ["FIXED_LEFT", x], ["FIXED_TOP", y]]

drawFeaturesAndRect ({x,y,w,h,x2,y2} as features) =
  [ Html.h3 (absolutePosition 10 30) []
      (toString features)
  , Html.div (absolutePosition 10 100) []
      [["svg", [["width", 1000], ["height", 1000]],
         [rect "yellow" x y w h]]]
  ]

--type RectParams
--  = Rect1 {x:Int,y:Int,w:Int,h:Int} -- and dummy x2,y2 for now
--  | Rect2 {x:Int,y:Int,x2:Int,y2:Int} -- and dummy w,h for now

wrapRect1 args =
  Update.freeze ["Rect1", { args | x2=-999, y2=-999 }]

wrapRect2 args =
  Update.freeze ["Rect2", { args | w=-999, h=-999} ]

featuresOf rectParams =
  case rectParams of
    ["Rect1", {x,y,w,h}   as r] -> { r | x2=x+w, y2=y+h }
    ["Rect2", {x,y,x2,y2} as r] -> { r | w=x2-x, h=y2-y }

twiddleableFeaturesOf rectParams =
  Update.applyLens
    { apply = featuresOf
    , update {input,outputNew} =
        let twiddledValue = case input of
          ["Rect1", _] ->
            let {x,y,w,h} = outputNew in
            wrapRect2 {x=x,y=y,x2=x+w,y2=y+h}
            
          ["Rect2", _] ->
            let {x,y,x2,y2} = outputNew in
            wrapRect1 {x=x,y=y,w=x2-x,h=y2-y}
        in
      { values = [twiddledValue] } 
    }
    rectParams

rect3_ rectParams =
  drawFeaturesAndRect (twiddleableFeaturesOf rectParams)
  
myRectangle =
  ["Rect1",
    {x=10, y=10, w=200, h=300, x2=-999, y2=-999}]

strRectParams rectParams =
  case rectParams of
    ["Rect1", {x,y,w,h}] -> "Rect1 " + toString [x,y,w,h]
    ["Rect2", {x,y,x2,y2}] -> "Rect2 " + toString [x,y,x2,y2]
  
main = (
  Html.div [] []
    (p [] [] (strRectParams myRectangle) :: rect3_ myRectangle)

    
  )
