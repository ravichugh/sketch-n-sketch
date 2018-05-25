content = """
<h1>$phrase15</h1>
<h3>$Marion ($airdesoncouplet):</h3>
$phrase1<br>
$phrase2<br>
$phrase3 <br>
$phrase4<br>
$phrase5.
<h3>$Mikal ($airdesoncouplet):</h3>$phrase7
<h3>$Ensemble ($Mikal $airdesonrefrain, $Marion $phrase6)</h3>$phrase8
<table style="\n  width:100%; \n:"><tbody>
<tr><td style="\n  width:50%; \n:">
<h3>$Mikal ($airdesoncouplet)</h3>
$phrase10
</td><td style="\n  width:50%; \n:">
<h3>$Marion ($airdesoncouplet)</h3>
$phrase1,<br>
$phrase2<br>
$phrase3 <br>
$phrase4<br>
$phrase5
</td></tr></tbody></table>
<table style="\n  width:100%; \n:"><tbody>
<tr><td style="\n  width:50%; \n:">
<h3>$Mikal ($airdesonrefrain)</h3>$phrase8</td>
<td style="\n  width:50%; \n:">
<h3>$Marion ($airdesonrefrain)</h3>
$phrase9</td></tr></tbody></table>
<h3>$Ensemble ($Marion $airdesoncouplet, $Mikal $phrase6):</h3>
$phrase11
<h3>$Ensemble ($Mikal $airdesoncouplet, $Marion $airdesoncouplet)</h3>
$phrase12
<table style="\n  width:100%; \n:"><tbody>
<tr><td style="\n  width:50%; \n:">
<h3>$Mikal ($airdesonrefrain)</h3>$phrase8</td>
<td style="\n  width:50%; \n:">
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
     ( "phrase12", "So bring all the thirsty all who seek peace;<br>bring those with nothing to offer.<br>Strengthen the feeble,<br>  ($Mikal) say to the frightened heart: “Fear not here is your God!”<br>  ($Marion) say to them, in time ''Fear not, Fear not''"),
     ( "phrase13", "a child of the poor"),
     ( "phrase14", "the babe, the son of Mary"),
     ( "phrase15", "What Child is This / Child of the Poor")])
  ,("Français", 
    [( "Marion", "Marion"),
     ( "Mikal", "Mikaël"),
     ( "airdesoncouplet", "air de son couplet"),
     ( "Ensemble", "Ensemble"),
     ( "airdesonrefrain", "air de son refrain"),
     ( "phrase1", "<i>Quel</i> est l'enfant, qui est né ce soir"),
     ( "phrase2", "Inconnu des gens de la terre ?"),
     ( "phrase3", "<i>Quel</i> est l'enfant, qui est"),
     ( "phrase4", "né ce soir, Que les pauvres"),
     ( "phrase5", "ont voulu recevoir"),
     ( "phrase6", "double"),
     ( "phrase7", "Sans aucune défense, hors d'une cité,<br>enveloppé dans un lange<br>il vient parmi nous<br>pour nous sauver de nos péchés,<br>annoncent les anges"),
     ( "phrase8", "<i>Qui est</i> celui qui va vers les faibles, souffrant<br>leurs chagrins, et vivant leur faim?<br><i>C'est le</i> Christ, qui vient<br>dans le monde sous les traits d'un enfant,<br>un enfant des pauvres"),
     ( "phrase9", "Il suffit d'un enfant ce soir<br>Pour unir le ciel et la terre<br>Il suffit<br>d'un enfant ce soir, Pour changer<br>notre vie en espoir."),
     ( "phrase10", "Qui est l'étranger, présent si tard,<br>Cherchant abri par ici<br>Qui est le paria?<br>Qui est-ce qu'on voit parmi les pauvres,<br>les enfants de Dieu?<br>"),
     ( "phrase11", "Quel est l'Enfant qui est né ce soir<br>Pour changer la nuit en lumière ?<br>Quel est l'Enfant qui est né ce soir<br>Tout joyeux comme un feu dans le noir ?"),
     ( "phrase12", "<i>Ap</i>portez les malades, <i>les</i> affligés;<br>venez ceux qui n'ont rien à donner.<br>Fortifiez le faible,<br>  ($Mikal) dites à l'appeuré: ''N'aie pas peur, voici ton Dieu!''<br>  ($Marion) et dites à chacun ''N'aie pas peur, n'aie pas peur''"),
     ( "phrase13", "un enfant des pauvres"),
     ( "phrase14", " changer notre vie en espoir"),
     ( "phrase15", "Quel est l'Enfant / L'Enfant des pauvres")])
  ]

allTranslationDicts = List.map (Tuple.mapSecond Dict.fromList) translations
  
translationsLangDict = Dict.fromList allTranslationDicts
languages = List.map Tuple.first translations
languageIndex = 0
language = nth languages languageIndex
currentTranslation = Dict.apply translationsLangDict language

highlighttranslations = False

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
            --let basename = Regex.replace "[^\\w]" "" definition in 
            let name = freshVarName "phrase" 1 currentTranslation in
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