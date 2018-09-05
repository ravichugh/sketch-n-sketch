content = """
<h1>$phrase15</h1>
<h3>$Marion ($airdesoncouplet):</h3>
 $phrase1,<br>
 $phrase2<br>
 $phrase3 <br>
 $phrase4<br> 
 $phrase5.
<h3>$Mikal ($airdesoncouplet):</h3>
 $phrase7
 <h3>$Ensemble ($Mikal $airdesonrefrain, $Marion $phrase6)</h3>
$phrase8
<table style="\n  width:100%; \n:"><tbody>
<tr><td>
<h3>$Mikal ($airdesoncouplet)</h3>
$phrase10
</td><td>
<h3>$Marion ($airdesoncouplet)</h3>
$phrase1,<br>
$phrase2<br>
$phrase3 <br>
$phrase4<br>
$phrase5
</td></tr></tbody></table>
<table style="\n  width:100%; \n:"><tbody>
<tr><td>
<h3>$Mikal ($airdesonrefrain)</h3>$phrase8</td><td>
<h3>$Marion ($airdesonrefrain)</h3>
$phrase9</td></tr></tbody></table>
<h3>$Ensemble ($Marion $airdesoncouplet, $Mikal $phrase6):</h3>
 $phrase11
<h3>$Ensemble ($Mikal $airdesoncouplet, $Marion $airdesoncouplet)</h3>
 $phrase12
<table style="\n  width:100%; \n:"><tbody>
<tr><td>
<h3>$Mikal ($airdesonrefrain)</h3>$phrase8</td><td>
<h3>$Marion ($airdesonrefrain)</h3>
$phrase9
</td></tr></tbody></table>
<h3>$Mikal</h3>
 $phrase13
<h3>$Marion</h3>
 $phrase14"""

translations =
  [("English", 
    [( "Marion", "Marion"),
     ( "Mikal", "Mikaël"),
     ( "airdesoncouplet", "tune of verse"),
     ( "Ensemble", "Together"),
     ( "airdesonrefrain", "tune of chorus"),
     ( "phrase1", "What Child is, who, laid to rest"),
     ( "phrase2", "On Mary’s lap is sleeping Whom"),
     ( "phrase3", "angels greet with"),
     ( "phrase4", "anthems weet, while shep-"),
     ( "phrase5", "herds watch are keeping"),
     ( "phrase6", "2<sup>nd</sup> voice"),
     ( "phrase7", "Helpless and hungry, lowly, afraid,<br>wrapped in the chill of mid winter<br>comes now among us<br>born into poverty’s embrace,<br>new life for the world"),
     ( "phrase8", "Who is this who lives with the lowly, sharing<br>their sorrows, knowing their hunger?<br>This is Christ, revealed<br>to the world in the eyes of a child,<br>a child of the poor"),
     ( "phrase9", "This, this is the Christ the King<br>Whom shepherds guard and angels sing<br>Haste, haste<br>to bring him laud, the babe,<br>the son of Mary"),
     ( "phrase10", "Who is the stranger here in our midst,<br>Looking for shelter among us<br>Who is the outcast?<br>Who do we see amid the poor,<br>The children of God?<br>"),
     ( "phrase11", "So bring him incense, gold, and myrrh,<br>Come peasant, king, to own him;<br>The King of Kings salvation brings,<br>Let loving hearts enthrone him."),
     ( "phrase12", "So bring all the thirsty all who seek peace;<br>bring those with nothing to offer, Strengthen the feeble,<br>say to the frightened heart: “Fear not here is your God!”"),
     ( "phrase13", "a child of the poor"),
     ( "phrase14", "the babe, the son of Mary"),
     ( "phrase15", "What Child is This / Child of the Poor")])
  ,("Français", 
    [( "Marion", "Marion"),
     ( "Mikal", "Mikaël"),
     ( "airdesoncouplet", "air de son couplet"),
     ( "Ensemble", "Ensemble"),
     ( "airdesonrefrain", "air de son refrain"),
     ( "phrase1", "What Child is, who, laid to rest"),
     ( "phrase2", "On Mary’s lap is sleeping Whom"),
     ( "phrase3", "angels greet with"),
     ( "phrase4", "anthems weet, while shep-"),
     ( "phrase5", "herds watch are keeping"),
     ( "phrase6", "double"),
     ( "phrase7", "Helpless and hungry, lowly, afraid,<br>wrapped in the chill of mid winter<br>comes now among us<br>born into poverty’s embrace,<br>new life for the world"),
     ( "phrase8", "Who is this who lives with the lowly, sharing<br>their sorrows, knowing their hunger?<br>This is Christ, revealed<br>to the world in the eyes of a child,<br>a child of the poor"),
     ( "phrase9", "This, this is the Christ the King<br>Whom shepherds guard and angels sing<br>Haste, haste<br>to bring him laud, the babe,<br>the son of Mary"),
     ( "phrase10", "Who is the stranger here in our midst,<br>Looking for shelter among us<br>Who is the outcast?<br>Who do we see amid the poor,<br>The children of God?<br>"),
     ( "phrase11", "So bring him incense, gold, and myrrh,<br>Come peasant, king, to own him;<br>The King of Kings salvation brings,<br>Let loving hearts enthrone him."),
     ( "phrase12", "So bring all the thirsty all who seek peace;<br>bring those with nothing to offer, Strengthen the feeble,<br>say to the frightened heart: “Fear not here is your God!”"),
     ( "phrase13", "a child of the poor"),
     ( "phrase14", "the babe, the son of Mary"),
     ( "phrase15", "What Child is This / Child of the Poor")])
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
    { apply (x, _) = x
      update {input = (x, allTranslationDicts), newOutput} =
        Regex.find "\\{([^\\}]*(?!\\})\\S[^\\}]*)\\}" newOutput |>
        List.foldl (\(_::definition::_) (newOutput, currentTranslation, allTranslationDicts) ->
            --let basename = Regex.replace "[^\\w]" "" definition in 
            let name = freshVarName "phrase" 1 currentTranslation in
            let textToFind = "\\{" + Regex.replace "\\\\|\\{|\\}|\\[|\\]|\\$|\\.|\\?|\\+" (\m -> "\\" + m.match) definition + "\\}" in
            (Regex.replace textToFind (\_ -> "$" + name) newOutput,
             Dict.insert name definition currentTranslation,
             List.map (\(lang, d) -> (lang, Dict.insert name definition d)) allTranslationDicts)
        ) (newOutput, currentTranslation, allTranslationDicts) |> \(newOutput, _, newTranslationsLangDict) ->
          Ok (Inputs [(newOutput, newTranslationsLangDict)])
    }.apply (x, allTranslationDicts)
  )

alltranslationsLangDict = Dict.fromList translations
addLang alltranslationsLangDict = {
  apply alltranslationsLangDict = ""
  update {input, outputNew} =
    if not (outputNew == "") && not (Dict.member outputNew alltranslationsLangDict) then 
      let toCopy = Dict.apply alltranslationsLangDict language in
      Ok (InputsWithDiffs [(Dict.insert outputNew toCopy alltranslationsLangDict,
       Just (VDictDiffs (Dict.fromList [(outputNew, VDictElemInsert)])))])
    else
      Ok (InputsWithDiffs [(input, Nothing)])
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