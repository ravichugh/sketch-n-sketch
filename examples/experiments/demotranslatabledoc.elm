content = """<h1>$translation1</h1>
In case your printer RGB4500 is stuck, please follow these steps:
<ul>
<li>$translation2</li>
<li>$translation3</li>
</ul>
"""

translations =
  [("English", 
    [("translation1", "How to fix paper jam")
    ,("translation2", "Open the green lid using the red handle at the back")
    ,("translation3", "Remove the paper, close the lid")])
  ,("Français", 
    [("translation1", "Réparer un bourrage papier")
    ,("translation2", "Ouvrir le couvercle vert en tirant la poignée rouge derrière")
    ,("translation3", "Enlever le papier bourré, fermer le couvercle")])
  ]

languages = List.map Tuple.first translations
languageIndex = 0
language = nth languages languageIndex
highlighttranslations = True

main = 
  Html.forceRefresh <|
  <div style="margin:20px;cursor:text">
      <span>@(
        Html.select [] languages languageIndex)
        <input style="margin-left:10px;width:180px;" type="text"
           v=(addLang translations)
           placeholder="New Language (e.g. German)"
           title="Enter the name of a new language here and press ENTER"
           onchange="this.setAttribute('v',this.value)">@(
            Html.checkbox "Highlights" "Highlight translatable text"
               highlighttranslations) <button
          title="Make the selected text translatable"
          onclick="""
            var r = window.getSelection().getRangeAt(0);
            var t = r.cloneContents().textContent;
            r.deleteContents();
            r.insertNode(document.createTextNode("{:" + t + ":}"))"""
          contenteditable="false">Make translatable</button>
        <br><hr>
  @(content |>
    translate {highlighttranslations=highlighttranslations} language translations
    |> Html.parse)
      </span>
  </div>
  
  
  
  
translate options language translations content =
  let allTranslationDicts = List.map (Tuple.mapSecond Dict.fromList) translations in
  let translationsLangDict = Dict.fromList allTranslationDicts in
  let currentTranslation = Dict.apply translationsLangDict language in
  let replaceVariables translationDict string =
    Regex.replace "\\$(\\w+|\\$)" (\m -> 
      if m.match == "$" then m.match else
      let key = nth m.group 1 in
      case Dict.get key translationDict of
        Nothing -> m.match
        Just definition -> 
          let finaldefinition = 
            if case options of {highlighttranslations=x} -> x; _ -> False then
              """<span style='outline:lightgreen 2px solid;' title='@key'>@definition</span>"""
            else definition
         in
         finaldefinition
    ) string
  in
  let freshVarName name i dictionary =
    if Dict.member (name + toString i) (dictionary) then freshVarName name (i + 1) (dictionary) else name + toString i
  in
  content |> replaceVariables currentTranslation |> \x ->
    { apply (x, _) = x
      update {input = (x, allTranslationDicts), newOutput} =
        Regex.find "\\{([^\\}]*(?!\\})\\S[^\\}]*)\\}" newOutput |>
        List.foldl (\(_::definition::_) (newOutput, currentTranslation, allTranslationDicts) ->
            let name = freshVarName "translation" 1 currentTranslation in
            let textToFind = "\\{" + Regex.replace "\\\\|\\{|\\}|\\[|\\]|\\$|\\.|\\?|\\+" (\m -> "\\" + m.match) definition + "\\}" in
            (Regex.replace textToFind (\_ -> "$" + name) newOutput,
             Dict.insert name definition currentTranslation,
             List.map (\(lang, d) -> (lang, Dict.insert name definition d)) allTranslationDicts)
        ) (newOutput, currentTranslation, allTranslationDicts) |> \(newOutput, _, newTranslationsLangDict) ->
          Ok (Inputs [(newOutput, newTranslationsLangDict)])
    }.apply (x, allTranslationDicts)

addLang translations = {
  apply alltranslationsLangDict = ""
  update {input, outputNew} =
    if not (outputNew == "") && not (Dict.member outputNew input) then 
      let toCopy = Dict.apply input language in
      Ok (InputsWithDiffs [(Dict.insert outputNew toCopy input
                           ,Just (VDictDiffs (Dict.fromList [(outputNew, VDictElemInsert)])))])
    else
      Ok (InputsWithDiffs [(input, Nothing)])
  }.apply (Dict.fromList translations)