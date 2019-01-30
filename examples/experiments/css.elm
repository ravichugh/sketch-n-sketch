styleSheet = """
  #outputCanvas span.colored, div .extra {
    background: #FF8;
    color: green;
  }
  #outputCanvas span {
    color: red;
    outline: black 1px solid;
  }
  #outputCanvas div.wrapperdiv {
    color: blue;
    font-family : Georgia, sans-serif;
    font-weight: bold;
  }
  """

main = Html.forceRefresh <|
  <div>
  <div class="wrapperdiv">
  <style>@styleSheet</style>
  Run this file twice<br>
  Hello <span id="testspan" class="colored">World</span>!
  </div>
  <ul>
  @(List.map (\(key, (w, v)) -> 
    let content = String.joinAndSplitBack "\\s*:\\s*" ": " [key, v] in
    Update.freezeExcept (always "template") content <| \content ->
    <li>@content</li>) <|
    appliedStyles (parseStyle styleSheet) (elementOf "#testspan"))
  </ul>
  </div>

type alias Style = List (key, Value)
type alias StyleSheet = List (Space, Selector, Space, List (Space, Key, Space, Space, Value, Space))

removeFinalNothings l =
  let aux n l = case l of
        Nothing :: tail ->
          if List.all ((==) Nothing) tail then n else aux (n + 1) tail
        head :: tail ->
          aux (n + 1) tail
      n = aux 0 l
  in
  Tuple.first (List.split n l)

specialfilterMap f l = List.map f l |> removeFinalNothings |> List.filter (\x -> x /= Nothing) |> List.map (\(Just x) -> x)

-- Transforms a text style into an list of (ws0, selector, ws1, list of [ws0, key, ws1, ws2, value, ws3])
parseStyle: String -> StyleSheet
parseStyle styleText =
  Regex.splitReverse "}" """\}""" styleText
  |> List.filterMap (\str ->
    case Regex.extract """^(\s*)([^\{]+?)(\s*)\{([\s\S]*)$""" str of
      Just [ws0, selector, ws1, attrs] ->
        --let selectorList = Regex.split """\s*,\s*""" selectors in
        let attrList =
              Regex.splitReverse ";" ";" attrs
              |> Update.debug "attrs"
              |> specialfilterMap ( -- filterMap might align a Nothing with an inserted Just (e.g. when inserting), so it's better to handle the back-propagation ourselves.
                Update.lens {
                  apply = Regex.extract """^(\s*)([^:]+?)(\s*):(\s*)([\s\S]*?)(\s*)$"""
                  update {input=str, outputNew, outputOld} =
                    case outputNew of
                    Just [ws1,k,ws2,ws3,v,ws4] -> Ok <| Inputs [ws1 + k + ws2 + ":" + ws3 + v + ws4]
                    Nothing -> Ok <| Inputs <| [str]
                })
              |> Update.debug "filteredMap"
        in Just (ws0, selector, ws1, attrList)
      Nothing -> Nothing
  )

elementOf selector = 
  """document.querySelector(@(jsCode.stringOf selector))"""

-- Returns true if this element matches this selector
matches element selector =
  __jsEval__ <| """@(element) ? @(element).matches(@(jsCode.stringOf selector)) : false"""

-- Returns true if this element has a parent that matches this selector
inherits element selector =
  __jsEval__ <| """
    var element = @(element) ? @(element).parentNode : null;
    var matches = false;
    while(element && !matches) {
      matches = element.matches(@(jsCode.stringOf selector));
      element = element.parentNode;
    }
    matches
  """

isInheritable =
  let inheritableKeys = Set.fromList
        ["azimuth", "border-collapse", "border-spacing", "caption-side", "color", "cursor", "direction", "elevation", "empty-cells", "font-family", "font-size", "font-style", "font-variant", "font-weight", "font", "letter-spacing", "line-height", "list-style-image", "list-style-position", "list-style-type", "list-style", "orphans", "pitch-range", "pitch", "quotes", "richness", "speak-header", "speak-numeral", "speak-punctuation", "speak", "speech-rate", "stress", "text-align", "text-indent", "text-transform", "visibility", "voice-family", "volume", "white-space", "widows", "word-spacing"]
  in
  \key -> Set.member key inheritableKeys
  
weight = {
  tag = (+) 1
  class = (+) 10
  id = (+) 100
  inline = (+) 1000
  important = (+) 10000
}
  
-- Compute weight according to https://specifishity.com/
selectorWeight element selectors =
  Regex.split "," selectors
  |> List.map (\selector ->
    if matches element selector then
      Regex.find """(#)[^\.\s:#]+|(\.)[^\.\s:#]+|(::)[^\.\s:#]+|(\[)[^\]]\]""" selector
      |> List.map (\[_, id,cl,pseudo,attr] ->
        if id /= "" then weight.id
        else if cl /= "" then weight.class
        else if pseudo /= "" then weight.class
        else if attr /= "" then weight.class
        else identity
      )
      |> List.foldl (\f acc -> f acc) 0
    else 0
  ) |> List.foldl max 0

importantWeight basicWeight = 100000 + basicWeight

type alias WeightedStyle = (Key, (Weight, Value))

-- TODO:
-- * If customizing an inherited style, should provide the option to apply it to the element if there are more specific selectors
-- * If adding a style that was never obtained from element or parents, should add it to all possible selectors that match the element. If possible, close to the previously added element?
appliedStyles: StyleSheet -> Element -> List WeightedStyle
appliedStyles styles element =
  List.foldl (\(ws0, selector, ws1, keyValues) acc ->
    let toKeyWeightValue weight
          [ws2, key, ws3, ws4, value, ws5] =
             (key, (weight, value))
    in
    let thisWeight = selectorWeight element selector
        process ([[ws2, key, ws3, ws4, value, ws5]] as concKeyValue) acc = 
          let realWeight =
             if Regex.matchIn "!important" value then weight.important thisWeight else thisWeight
          in
          case listDict.get key acc of
            Just (prevWeight, prevValue) ->
              if prevWeight > realWeight then
                acc
              else
                listDict.insert2 (List.map (toKeyWeightValue realWeight) concKeyValue) acc
            Nothing -> 
              listDict.insert2 (List.map (toKeyWeightValue realWeight) concKeyValue) acc
    in
    if matches element selector then
      let result =
            List.foldl2 process (Update.debug "acc1" acc) (Update.debug "kv1" keyValues)
      in
      Update.lens2 { -- We gather all inserted elements elsewhere and insert them directly at the end of keyValues as one solution.
      -- Deleted elements are deleted from keyValues if there were there.
        apply (result, kvs) = result
        update ({input=(result, keyValues), outputOld, outputNew, diffs} as uInput) =
         Debug.log (toString uInput) <|
         let immediateSolution = 
           Update.foldDiff {
             start = ([], [], keyValues)
             onSkip (ls, ds, kvs) {count, index, newOutputs} =
               Ok [(ls ++ newOutputs, ds, kvs)]
             onUpdate (ls, ds, kvs) {newOutput, index, diffs} =
               Ok [(ls ++ [newOutput], ds ++ [(index, ListElemUpdate diffs)], kvs)]
             onRemove (ls, ds, kvs) {oldOutput=(key, (weight, value)) as oldOutput, index} =
               let newKvs = List.filter (\(_ :: key2 :: _ :: _ :: value2 :: _) -> key2 /= key || value2 /= value) kvs in
               let delayedSolution =
                     [(ls, ds ++ [(index, ListElemDelete 1)], kvs)] in
               if List.length newKvs /= List.length kvs then
                 Ok ([(ls ++ [oldOutput], ds, newKvs)] ++ delayedSolution)
               else
                 Ok delayedSolution
             onInsert (ls, ds, kvs) {newOutput=(key, (weight, value)) as newoutput, index} =
               let delayedSolution = 
                  [(ls ++ [newoutput], ds ++ [(index, ListElemInsert 1)], kvs)]
               in
               let (ws0, ws1, ws2, ws3) = case List.last kvs of
                 Just [ws0, _, ws1, ws2, _, ws3] -> (ws0, ws1, ws2, ws3)
                 Nothing -> ("\n  ", "", " ", "")
               in
               Ok (delayedSolution ++ [(ls, ds, kvs ++ [[ws0, key, ws1, ws2, value, ws3]])])
             onFinish x = Ok [x]
             onGather (ls, ds, kvs) = Ok <| 
               InputWithDiff ((ls, kvs), Update.mbPairDiffs (if ds == [] then Nothing else Just (VListDiffs ds), Update.diffs keyValues kvs))
           } outputOld outputNew diffs
         in
         immediateSolution
      } (Update.debug "result" result) (Update.debug "kv2" keyValues)
    else if False && inherits element selector then
      List.foldl2 (\(((_ :: key :: _)::keyTail) as keyValue) acc ->
        if isInheritable key then
          process keyValue acc
        else
          acc -- TODO: It should be possible to insert something there 
      ) acc keyValues
    else
      acc
  ) (Update.freezeWhen True (\_ -> "No place where to modify the rule") []) styles