inlineStyle = "color: orange"

styleSheets = ["""
  #outputCanvas span.colored, div .extra {
    background: #FF8;
    color: green;
  }
  @@media screen {
    #outputCanvas span {
      color: red !important;
      outline: black 1px solid;
    }
  }
  #outputCanvas h1 + div.wrapperdiv {
    color: purple;
  }""",
  """
  #outputCanvas div.wrapperdiv {
    color: blue;
    font-family : Georgia, sans-serif;
    font-size: 3em;
  }
  """,
  Update.freezeExcept (always "inline style") inlineStyle <| \inlineStyle ->
  """#outputCanvas span /*inline*/ {@inlineStyle}"""]

parsedStyle = List.concatMap parseStyle styleSheets

displayComputedStyles selector = 
  Update.freezeExcept (\diffs -> "template " + toString diffs) parsedStyle <| \parsedStyle ->
    <ul>@(List.map (\(key, (w, v)) -> 
    let content = String.joinAndSplitBack "\\s*:\\s*" ": " [key, v] in
    <li>@Html.text(content)</li>) <|
    appliedStyles parsedStyle (elementOf selector))</ul>
  
main = Html.forceRefresh <|
  <div>
  <h1>Style updater</h1>
  <script type="text/javascript" src="https://cdn.jsdelivr.net/gh/MikaelMayer/lossless-css-parser@d4d64a4a87f64606794a47ab58428900556c56dc/losslesscss.js"></script>
  @(List.map (\content -> <style>@content</style>) styleSheets)
  <div class="wrapperdiv" title="div.wrapperdiv">
  Hello <span style=@inlineStyle id="testspan" class="colored" title="span#testspan.colored">World</span>!
  </div>
  <table><tr><th>div.wrapperdiv</th><th>span#testspan.colored</th></tr>
  <tr><td>@(displayComputedStyles "div.wrapperdiv")</td>
      <td>@(displayComputedStyles "span#testspan")</td></tr></table>
  Instructions:
  <ul><li>Run this file twice, so that it can pick the styles of the span and use the selection matchers.</li>
  <li>Observe the programs line 1-18 to visualize style changes</li>
  </ul>
  Interesting experiments on the span (second column):
  <ul><li>Add an attribute 'border: 2px solid red' after the color. It can modify only the direct selectors. The third solution depicts an ambiguity about where to place in the second selector.</li>
  <li>Add an attribute 'font-weight: bold' after the color. Because this attribute is inherited, it can be added in all three selectors. Note the differences !</li>
  <li>Remove the !important from the color attribute. Now the color turns green.</li>
  <li>Delete the color attribute. It correctly delete the green attribute and it becomes red again.</li>
  </ul>
  </div>

type alias Style = List (key, Value)
type alias StyleSheet = List (Space, Selector, Space, List (Space, Key, Space, Space, Value, Space))

removeFinalNothings ll =
  let aux n l = case l of
        Nothing :: tail ->
          if List.all ((==) Nothing) tail then
            Tuple.first (List.split n ll)
          else aux (n + 1) tail
        head :: tail ->
          aux (n + 1) tail
        [] -> ll
  in aux 0 ll

specialfilterMap f l = List.map f l |> removeFinalNothings |> List.filter (\x -> x /= Nothing) |> List.map (\(Just x) -> x)

-- Transforms a text style into an list of (ws0, selector, ws1, list of [ws0, key, ws1, ws2, value, ws3])
parseStyle: String -> StyleSheet
parseStyle = Update.lens {
     apply styleText = __jsEval__ """typeof losslesscssjs != "undefined" ? new losslesscssjs().parseCSS(@(jsCode.stringOf styleText)) : []"""
     update {outputNew} = Ok (Inputs [unparseCSS outputNew])
     unparseCSS x =
       List.map (case of
         { wsBefore, selector, wsBeforeValue, value, wsBeforeAndSemicolon} ->
           wsBefore + selector + wsBeforeValue + value + wsBeforeAndSemicolon
         { wsBefore, selector, wsBeforeAtNameValue, atNameValue,
           wsBeforeOpeningBrace, content, wsBeforeClosingBrace =wsBeforeClosingBrace} ->
           wsBefore + selector + wsBeforeAtNameValue + atNameValue +
           wsBeforeOpeningBrace + "{" + unparseCSS content + wsBeforeClosingBrace + "}"
         { wsBefore, selector, wsBeforeOpeningBrace,
            rules, wsBeforeClosingBrace} ->
           wsBefore + selector + wsBeforeOpeningBrace + "{" + 
           (List.map (case of
             {wsBefore, directive, wsBeforeColon, wsBeforeValue, value, wsSemicolon} ->
             wsBefore + directive + wsBeforeColon + ":" + wsBeforeValue + value + wsSemicolon
           ) rules |> String.join "") + wsBeforeClosingBrace + "}"
         { ws =ws} -> ws
       ) x |> String.join ""
  }

elementOf selector = 
  """document.querySelector(@(jsCode.stringOf selector))"""

-- Returns true if this element matches this selector
matches element selector =
  __jsEval__ <| """@(element) ? typeof @(element).matches == 'function' ? @(element).matches(@(jsCode.stringOf selector)) : false : false"""

-- Returns true if this element has a parent that matches this selector
parentMatch: (CountName -> ElementName -> StringReturningA) -> StringReturningA -> ElementSelector -> Selector -> A
parentMatch ifFound ifNotfound element selector =
  __jsEval__ <| """
    var element = @(element);
    var matches = false;
    var count = 0;
    while(element && !matches && typeof element.matches == "function") {
      element = element.parentNode;
      matches = typeof element == "object" && typeof element.matches == "function" && element.matches(@(jsCode.stringOf selector));
      count++;
    }
    matches ? @(ifFound "count" "element") : @ifNotfound
  """

isInheritable =
  let inheritableKeys = Set.fromList
        ["azimuth", "border-collapse", "border-spacing", "caption-side", "color", "cursor", "direction", "elevation", "empty-cells", "font-family", "font-size", "font-style", "font-variant", "font-weight", "font", "letter-spacing", "line-height", "list-style-image", "list-style-position", "list-style-type", "list-style", "orphans", "pitch-range", "pitch", "quotes", "richness", "speak-header", "speak-numeral", "speak-punctuation", "speak", "speech-rate", "stress", "text-align", "text-indent", "text-transform", "visibility", "voice-family", "volume", "white-space", "widows", "word-spacing"]
  in
  \key -> Set.member key inheritableKeys
  
weight = {
  tag = (+) 1
  class = (+)          10
  id = (+)             100
  inline = (+)         1000
  important = (+)      10000
  inherited = flip (-) 100000
}
  
-- Compute weight according to https://specifishity.com/
selectorWeight element selectors =
  Regex.split "," selectors
  |> List.map (\selector ->
    if matches element selector then
      Regex.find """\/\*((?:(?!\*\/).)*)\*\/|(#)[^\.\s:#]+|(\.)[^\.\s:#]+|(::)[^\.\s:#]+|(:)[^\.\s:#]+|(\[)[^\]]\]|(\w+)""" selector
      |> List.map (\[_, comment, id,cl,pseudoels,pseudoclass, attr,tag] ->
        if comment /= "" then if comment == "inline" then weight.inline else identity
        else if id /= "" then weight.id
        else if cl /= "" then weight.class
        else if pseudoels /= "" then weight.class
        else if pseudoclass /= "" then identity
        else if attr /= "" then weight.class
        else weight.tag
      )
      |> List.foldl (\f acc -> f acc) 0
    else 0
  ) |> List.foldl max 0

type alias WeightedStyle = (Key, (Weight, Value))

-- TODO:
-- * If customizing an inherited style, should provide the option to apply it to the element if there are more specific selectors
-- * If adding a style that was never obtained from element or parents, should add it to all possible selectors that match the element. If possible, close to the previously added element?
appliedStyles: StyleSheet -> Element -> List WeightedStyle
appliedStyles styles element =
  let addTopLevel styleElem acc =
       if styleElem.kind == "@media" then
          if __jsEval__ """window.matchMedia(@(jsCode.stringOf styleElem.atNameValue)).matches""" then
            List.foldl addTopLevel acc styleElem.content
          else
            acc
       else if styleElem.kind == "cssBlock" then
        let { wsBefore=ws0, selector, wsBeforeOpeningBrace=ws1,
              rules=keyValues, wsBeforeClosingBrace=ws2 } = styleElem in
        let toKeyWeightValue weight
              {wsBefore=ws2, directive=key, wsBeforeColon=ws3, wsBeforeValue=ws4, value, wsSemicolon=ws5} =
                 (key, (weight, value))
        in
        let thisWeight = selectorWeight element selector in
        let process w ([{wsBefore=ws2, directive=key, wsBeforeColon=ws3,
                         wsBeforeValue=ws4, value, wsSemicolon=ws5}] as concKeyValue) acc = 
              let realWeight =
                 if Regex.matchIn "!important" value then weight.important w else w
              in
              case listDict.get key acc of
                Just (prevWeight, prevValue) ->
                  if prevWeight > realWeight then
                    acc
                  else
                    listDict.insert2 (List.map (toKeyWeightValue realWeight) concKeyValue) acc
                Nothing -> 
                  listDict.insert2 (List.map (toKeyWeightValue realWeight) concKeyValue) acc
            processElement = process thisWeight
        in
        if matches element selector then
          let result =
                List.foldl2 processElement acc keyValues
          in
          propagate (always True) result keyValues
        else 
          let mbParent = parentMatch (\countName varName -> 
               jsCode.datatypeOf "v" "Just" [jsCode.stringOf(element) + """ + '.parentNode'.repeat(@countName)"""]
             ) (jsCode.datatypeOf "v" "Nothing" []) element selector
          in
          case mbParent of
            Nothing -> acc
            Just parent ->
              let processAncestor = process (selectorWeight parent selector |> weight.inherited) in
              let result = 
                List.foldl2 (\([{wsBefore=ws2, directive=key, wsBeforeColon=ws3, wsBeforeValue=ws4, value, wsSemicolon=ws5}] as keyValue) acc ->
                  if isInheritable key then
                    processAncestor keyValue acc
                  else
                    acc 
                ) acc keyValues
              in
              propagate isInheritable result keyValues
       else acc -- Unsupported block (whitespace, keyframes)
  in
  List.foldl addTopLevel (Update.freezeWhen True (\_ -> "No place where to modify the rule") []) styles

propagate isKeyAdmissible =
  Update.lens2 {
      -- We gather all inserted elements elsewhere and insert them directly at the end of keyValues as one solution.
      -- Deleted elements are deleted from keyValues if there were there.
    apply (result, kvs) = result
    update ({input=(result, keyValues), outputOld, outputNew, diffs} as uInput) =
     --Debug.log (toString uInput) <|
     let immediateSolution = 
       Update.foldDiff {
         start = ([], [], keyValues)
         onSkip (ls, ds, kvs) {count, index, newOutputs} =
           Ok [(ls ++ newOutputs, ds, kvs)]
         onUpdate (ls, ds, kvs) {newOutput, index, diffs} =
           Ok [(ls ++ [newOutput], ds ++ [(index, ListElemUpdate diffs)], kvs)]
         onRemove (ls, ds, kvs) {oldOutput=(key, (weight, value)) as oldOutput, index} =
           let newKvs = List.filter (\{wsBefore=ws2, directive=key2, wsBeforeColon=ws3,
                         wsBeforeValue=ws4, value=value2, wsSemicolon=ws5} -> key2 /= key || value2 /= value) kvs in
           let delayedSolution =
                 [(ls, ds ++ [(index, ListElemDelete 1)], kvs)] in
           if List.length newKvs /= List.length kvs && isKeyAdmissible key then
             Ok ([(ls ++ [oldOutput], ds, newKvs)] ++ delayedSolution)
           else
             Ok delayedSolution
         onInsert (ls, ds, kvs) {newOutput=(key, (weight, value)) as newoutput, index} =
           let delayedSolution = 
              [(ls ++ [newoutput], ds ++ [(index, ListElemInsert 1)], kvs)]
           in
           let (ws0, ws1, ws2, ws3) = case List.last kvs of
             Just {wsBefore=ws0, directive=_, wsBeforeColon=ws1,
                   wsBeforeValue=ws2, value=_, wsSemicolon=ws3} -> (ws0, ws1, ws2, ws3)
             Nothing -> ("\n  ", "", " ", "")
           in
           Ok (delayedSolution ++ (
             if isKeyAdmissible key then
               [(ls, ds, kvs ++ [{wsBefore=ws0, directive=key, wsBeforeColon=ws1,
                   wsBeforeValue=ws2, value=value, wsSemicolon=ws3}])]
             else
               []))
         onFinish x = Ok [x]
         onGather (ls, ds, kvs) = Ok <| 
           InputWithDiff ((ls, kvs), Update.mbPairDiffs (if ds == [] then Nothing else Just (VListDiffs ds), Update.diffs keyValues kvs))
       } outputOld outputNew diffs
     in
     immediateSolution
  }