content = """
<h1>What Child is This / Child of the Poor</h1>
<h3>Marion (tune of verse):</h3>
What Child is, who, laid to rest,<br>
On Mary’s lap is sleeping Whom<br>
angels greet with <br>
anthems weet, while shep-<br>
herds watch are keeping
<h3>Mikaël (tune of verse):</h3>
Helpless and hungry, lowly, afraid,<br>
wrapped in the chill of mid winter<br>
comes now among us<br>
born into poverty’s embrace,<br>
new life for the world
<h3>Together (Mikaël tune of chorus, Marion 2<sup>nd</sup> voice)</h3>
Who is this who lives with the lowly,<br>
sharing their sorrows, knowing their hunger?<br>
This is Christ, revealed to the world in the eyes of a child,<br>
a child of the poor<br>
<table style="\n  width:100%; \n:"><tbody>
<tr><td>
<h3>Mikaël (tune of verse)</h3>
Who is the stranger here in our midst,<br>
Looking for shelter among us<br>
Who is the outcast?<br>
Who do we see amid the poor,<br>
The children of God?<br>
</td><td>
<h3>Marion (tune of verse)</h3>
What Child is, who, laid to rest,<br>
On Mary’s lap is sleeping Whom<br>
angels greet with <br>
anthems weet, while shep-<br>
herds watch are keeping
</td></tr></tbody></table>
<table style="\n  width:100%; \n:"><tbody>
<tr><td>
<h3>Mikaël (tune of chorus)</h3>
Who is this who lives with the lowly, sharing<br>
their sorrows, knowing their hunger?<br>
This is Christ, revealed<br>
to the world in the eyes of a child,<br>
a child of the poor
</td><td>
<h3>Marion (tune of chorus)</h3>
This, this is the Christ the King<br>
Whom shepherds guard and angels sing<br>
Haste, haste<br>
to bring him laud, the babe,<br>
the son of Mary
</td></tr></tbody></table>
<h3>Together (Marion tune of verse, Mikaël 2<sup>nd</sup> voice):</h3>
So bring him incense, gold, and myrrh,<br>
Come peasant, king, to own him;<br>
The King of Kings salvation brings,<br>
Let loving hearts enthrone him.
<h3>Together (Mikaël tune of verse, Marion tune of verse)</h3>
So bring all the thirsty all who seek peace;<br>
bring those with nothing to offer, Strengthen the feeble,<br>
say to the frightened heart: “Fear not here is your God!”
<table style="\n  width:100%; \n:"><tbody>
<tr><td>
<h3>Mikaël (tune of chorus)</h3>
Who is this who lives with the lowly, sharing<br>
their sorrows, knowing their hunger?<br>
This is Christ, revealed<br>
to the world in the eyes of a child,<br>
a child of the poor
</td><td>
<h3>Marion (tune of chorus)</h3>
This, this is the Christ the King<br>
Whom shepherds guard and angels sing<br>
Haste, haste<br>
to bring him laud, the babe,<br>
the son of Mary
</td></tr></tbody></table>
<h3>Mikaël</h3>
a child of the poor
<h3>Marion</h3>
the babe, the son of Mary"""

translations =
  [("English", 
    [])
  ,("Français", 
    [])
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
       replaceVariables (Dict.remove key translationDict) finaldefinition
  ) string

freshVarName name i dictionary =
  if Dict.member (name + (if i == 0 then "" else toString i)) dictionary
  then freshVarName name (i + 1) (dictionary) else name + (if i == 0 then "" else toString i)
  
content = content |> replaceVariables currentTranslation |>
  (\x ->
    { apply (x, _) = freeze x
      update {input = (x, allTranslationDicts), newOutput} =
        Regex.find "\\{([^\\}]*(?!\\})\\S[^\\}]*)\\}" newOutput |>
        List.foldl (\(_::definition::_) (newOutput, currentTranslation, allTranslationDicts) ->
            let basename = Regex.replace "[^\\w]" "" definition in 
            let name = freshVarName basename 0 currentTranslation in
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
  Html.div [["cursor", "text"]] []
    [ Html.div [["border", "4px solid black"], ["padding", "20px"]] [] <|
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