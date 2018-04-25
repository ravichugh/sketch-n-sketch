editorInfo = """
<h1>Translation Editor</h1>
<p>
  This text editor is set up to allow you to <em>add translations for</em>
  text elements in different languages.
  It highlights texts that are available in different languages.
  You can edit translations, add translations, and add languages into which to translate.
</p>
<p>
  <b>Editing a translation</b>
  Choose a language by selecting it on the select box.
  Change the text. If the text is a translation in a language,
  it will change only for this language.
  You can display highlight around translateable text portions by enabling the option "Highlights".
</p>
<p>
  <b>Translating a static text</b>
  To make a sentence translatable in all languages,
  wrap the sentence it with curly braces like {a translatable sentence}.
  After update, braces disappear and you can edit its translation in different languages.
</p>
<p>
  <b>Add a language</b>
  You can add a language by entering its name in the input box below,
  and press ENTER. It duplicates the translations of the current language.
</p>
<p>
  In the example below, wrap the sentence "This sentence is true" with braces,
  update and afterwards translate it to the language of your choice.
</p>
"""

translations =
  [("English", 
    [("translation1", "How to fix paper jam")
    ,("translation2", "Open the green lid using the red handle at the back")
    ,("translation3", "Remove the paper, close the lid")])
  ,("Français", 
    [("translation1", "Réparer un bourrage papier")
    ,("translation2", "Ouvrir le couvercle vert en tirant la poignée rouge à l'arrière")
    ,("translation3", "Enlever le papier bourré, fermer le couvercle")])
  ]

allTranslationDicts = List.map (Tuple.mapSecond Dict.fromList) translations
  
translationsLangDict = Dict.fromList allTranslationDicts
languages = List.map Tuple.first translations
languageIndex = 0
language = nth languages languageIndex
currentTranslation = Dict.apply translationsLangDict language

highlighttranslations = True

replaceVariables translationDict string =
  Regex.replace "\\$(\\w+|\\$)" (\m -> 
    if m.match == "$" then m.match else
    let key = nth m.group 1 in
    case Dict.get key translationDict of
      Nothing -> m.match
      Just definition -> 
        let finaldefinition = 
          if highlighttranslations then
            """<span style='outline:lightgreen 2px solid;' title='@key'>@definition</span>"""
          else definition
       in
       replaceVariables (remove key translationDict) finaldefinition
  ) string

freshVarName name i dictionary =
  if Dict.member (name + toString i) (dictionary) then freshVarName name (i + 1) (dictionary) else name + toString i
  
content = """<h1>$translation1</h1>
In case your printer RGB4500 is stuck, please follow these steps:
<ul>
<li>$translation2</li>
<li>$translation3</li>
</ul>
""" |> replaceVariables currentTranslation |>
  (\x ->
    { apply (x, _) = freeze x
      update {input = (x, allTranslationDicts), newOutput} =
        Regex.find "\\{([^\\}]*(?!\\})\\S[^\\}]*)\\}" newOutput |>
        List.foldl (\(_::definition::_) (newOutput, currentTranslation, allTranslationDicts) ->
            let name = freshVarName "translation" 1 currentTranslation in
            let textToFind = "\\{" + Regex.replace "\\\\|\\{|\\}|\\[|\\]|\\$|\\.|\\?|\\+" (\m -> "\\" + m.match) definition + "\\}" in
            (Regex.replace textToFind (\_ -> "$" + name) newOutput,
             Dict.insert name definition currentTranslation,
             List.map (\(lang, d) -> (lang, Dict.insert name definition d)) allTranslationDicts)
        ) (newOutput, currentTranslation, allTranslationDicts) |> \(newOutput, _, newTranslationsLangDict) ->
          { values = [(newOutput, newTranslationsLangDict)] }
    }.apply (x, allTranslationDicts)
  )

alltranslationsLangDict = Dict.fromList translations
addLang alltranslationsLangDict = {
  apply alltranslationsLangDict = freeze ""
  update {input, outputNew} =
    if not (outputNew == "") && not (Dict.member outputNew alltranslationsLangDict) then 
      let toCopy = Dict.apply alltranslationsLangDict language in
      {values = [Dict.insert outputNew toCopy alltranslationsLangDict],
       diffs=[Just (VDictDiffs (Dict.fromList [(outputNew, VDictElemInsert)]))]}
    else
      { values = [input], diffs = [Nothing]}
  }.apply alltranslationsLangDict

main = 
  Html.forceRefresh <|
  Html.div [["margin", "20px"], ["cursor", "text"]] []
    [ Html.div [] [] <|
        Html.parse editorInfo
    , Html.div [["border", "4px solid black"], ["padding", "20px"]] [] <|
        [Html.span [] [] <|
          Html.select [] languages languageIndex ::
          ["input", [["style", [["margin-left", "10px"]]], ["type","text"], ["v", addLang alltranslationsLangDict],
            ["placeholder", "New Language (e.g. German)"], ["title", "Enter the name of a language here and press ENTER"],
            ["onchange","this.setAttribute('v',this.value)"]], []] ::
          Html.checkbox "Highlights" "Highlight translatable text" highlighttranslations ::
          ["br", [], []] ::
          Html.parse content
        ]
    ]