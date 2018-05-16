# updatedelay: 0

{length, sum, range, filter, zipWithIndex} = List
{textNode, button, br, checkbox, h1, div, element, li, ul} = Html

{select} = {
  select attributes strArray defaultSelected =
    <select @attributes>@(List.indexedMap (\i s ->
      <option @(if i == defaultSelected then [["selected", "selected"]] else [])>@s</option>
      )  strArray)
    </select>
}

-- Each element of betselfs is a 2-element array containing the best and the own card number
players = [
  {name="John", betselfs=[], scores=[]}, 
  {name="Nick", betselfs=[], scores=[]},
  {name="Pete", betselfs=[], scores=[]}
]

displayreset = False
removeScores players =
  List.map (\p -> {p | betselfs=[], scores=[]}) players

mkError txt = Html.span [["color", "lightgray"]] [] (Html.text txt)

cartesDisponibles = range 1 (length players)

currentRound = length (nth players 0).scores

remainingbets = (length players - 1) - sum (map (\j ->  if (length j.betselfs) == currentRound then 0 else 1) players)
allbetsdone = remainingbets == 0

scoreIfEverybodyFound = 2
scoreIfNobodyFound = 3

currentPlayerId = "currentplayer"
tellPlayerId = "tellplayers"
cardNumberId = "cardnumberselect"
chooseCallback = "chooseCard"
chooseCardErrorId = "chooseCardError"
betNumberId = "betnumber"
betNumberChooseId = "betnumberchoose"
betNumberNotSameId = "betnumbersame"
chooseBetCallback = "selectBet"
buttonConfirmId = "confirmButton"
displayConfirmButtonCallback = "confirmButtonCallback"
clickbuttoncallback = "clickConfirmButtonCallback"
commitresultId = "resultsender"
foranotherplayerId = "foranotherplayer"
forcurrentplayerId = "forcurrentplayer"
addAnotherPlayerBet = "addAnotherPlayerBet"

commitPlayer players outputNew =
  case evaluate outputNew of
    (playerNum, card, bet) ->
      (List.take playerNum players) ++ (case List.drop playerNum players of
        player::otherPlayers ->
          {player | betselfs = player.betselfs ++ [[bet, card]]}::otherPlayers
        [] -> []
      )
    _ -> players

betself =
  <span id="betself"
><@(Html.refresh True)><script>
function setTransientVisibility(element, condition) {
  element.setAttribute("transient-visible", condition ? "true" : "false");
}
function fromId(id) {
  return document.querySelector("#" + id);
}
function @chooseCallback() {
  var player = fromId("@currentPlayerId");
  var tellplayers = fromId("@tellPlayerId");
  if(player !== null && tellplayers !== null) {
    for(var i = 0; i < tellplayers.children.length; i++) {
      setTransientVisibility(tellplayers.children[i], player.selectedIndex == i);
    }
  }
  var card   = fromId("@cardNumberId");
  var bet    = fromId("@betNumberId");
  var card_hint = fromId("@chooseCardErrorId");
  var bet_hint  = fromId("@betNumberChooseId");
  var bet_error = fromId("@betNumberNotSameId");
  var button    = fromId("@buttonConfirmId");
  if(player !== null && card !== null && bet !== null && card_hint !== null && bet_hint !== null && bet_error !== null && button !== null) {
    setTransientVisibility(card_hint, card.selectedIndex == 0);
    setTransientVisibility(bet_hint, bet.selectedIndex == 0);
    setTransientVisibility(bet_error, bet.selectedIndex == card.selectedIndex && bet.selectedIndex > 0);
    setTransientVisibility(button, card.selectedIndex > 0 && bet.selectedIndex > 0 && bet.selectedIndex !== card.selectedIndex);
  }
}
function @clickbuttoncallback() {
  // Commits the bets for the given player.
  var card = fromId("@cardNumberId");
  var bet = fromId("@betNumberId");
  var player = fromId("@currentPlayerId");
  var c = fromId("@commitresultId");
  c.setAttribute("v", "(" + player.selectedIndex + "," + card.selectedIndex + "," + bet.selectedIndex + ")");
  var d = fromId("@forcurrentplayerId");
  var e = fromId("@foranotherplayerId");
  setTransientVisibility(d, false);
  setTransientVisibility(e, true);
}

function @addAnotherPlayerBet() {
  var d = fromId("@forcurrentplayerId");
  var e = fromId("@foranotherplayerId");
  setTransientVisibility(d, true);
  setTransientVisibility(e, false);
  var card = fromId("@cardNumberId");
  var bet = fromId("@betNumberId");
  var player = fromId("@currentPlayerId");
  if(card !== null) card.selectedIndex = 0;
  if(bet !== null) bet.selectedIndex = 0;
  if(player !== null) player.selectedIndex = (player.selectedIndex + 1) % player.children.length;
  @chooseCallback();
}
</script></@
><style>
  .normallyNotVisible {
    display: none;
  }
  .normallyNotVisible[transient-visible="true"] {
    display: inline-block;
  }
  .normallyVisible[transient-visible="false"] {
    display: none;
  }
  </style
  ><span id=foranotherplayerId class="normallyNotVisible">
    <button onclick="""@addAnotherPlayerBet()""" title="Let another player place a bet on this screen">Place another bet</button>
  </span
  ><span id=forcurrentplayerId class="normallyVisible">Who is currently placing a bet? 
  @(select [["id", currentPlayerId], ["onchange", """@chooseCallback()"""]] (map (\j ->  j.name) players) 0)
  <br>
  <span id=tellPlayerId>@(List.map (\p -> <span class="normallyNotVisible">@p.name it's your turn to bet!</span>) players)</span>
<br>Your card:
  @(select [["id", cardNumberId], ["onchange", """@chooseCallback()"""]] 
      ("Choose your card number..." :: map (\x -> toString x) cartesDisponibles) 0)
  <span class="normallyNotVisible" id=chooseCardErrorId> Indicate what is your card. This is confidential.</span>
<br>Your bet:
  @(select [["id", betNumberId], ["onchange", """@chooseCallback()"""]] 
      ("You bet that the correct card is..." :: map (\x -> toString x) cartesDisponibles) 0)
  <span class="normallyNotVisible" id=betNumberChooseId>What card do you think is the dealer's one.</span>
  <span class="normallyNotVisible" id=betNumberNotSameId>You cannot bet on your own card.</span>
<br>
  <button
    class="normallyNotVisible"
    id=buttonConfirmId
    title="By clicking this button, I confirm I know the rules of the game"
    onclick="""@clickbuttoncallback()"""
  >I confirm by bet</button>
   <span
    id=commitresultId
    class="normallyNotVisible"
    v=(Html.onChangeAttribute players commitPlayer)>
    </span>
    <script>
      @chooseCallback();
    </script>
  </span>
</span>

nomDe j = li [] [] [textNode j.name]
playersnoms = map nomDe players
playersEnCours = filter (\j ->  length j.betselfs == currentRound ) players
playerFromName name =
  nth (filter (\j ->  j.name == name) players) 0
playerIndexFromName name =
  letrec aux i = 
    if (nth players i).name == name then i else
    if i >= length players then -1 else
    aux (i + 1) in aux 0

<div style="margin:20px">
  <span> 
  <img style="width:50%" src="https://images-cdn.asmodee.us/filer_public/9e/7e/9e7ea2a6-d531-4f3a-b984-4119925d4c9f/dix01_feature_c85e1c.png">
  <img style="float:right;width:50%;" src="https://cf.geekdo-images.com/large/img/QFUbpIeEFamJbgJ_Bs5ejDtF8UA=/fit-in/1024x1024/filters:no_upscale()/pic1003159.jpg">
  <div style="float:right">@(button "Reset" "Display the reset button" displayreset (\x -> if x then False else True))
  @(if displayreset then button "Reset scores" "Click here to reset scores" (displayreset, players) (\(_, oldPlayers) -> (False, removeScores oldPlayers)) else <span></span>)
  </div>
  <h1>Dixit Score sheet</h1>,
  To play to Dixit, please enter below the name of the @(length players) players. You can add or remove players.
  <ul>@(map nomDe players)</ul>
  It's turn number @(currentRound+1)<br>
  Current scores:<br>
  <table>
  <tr><th>Name</th><th>Score</th>@(if currentRound >= 2 then <th style="font-weight:normal"><i>by round</i>:</th> else [])@(List.map (\i -> <th>#@i</th>) (List.range 1 (if currentRound >= 2 then currentRound else 0)))</tr>
  @(List.map (\j -> 
    <tr>
      <td style="""color:@(if len j.betselfs > currentRound then "green" else "black")""">@(j.name)</td>
      <td style="text-align:center">@(toString (Update.freeze (sum (j.scores))))</td>
      @(if currentRound < 2 then [] else [<td></td>] ++
      List.map (\i -> <td>@(Update.freeze i)</td>) j.scores)
      </tr>) players)</table>
  @(if remainingbets > 0 then betself else 
  let playersWithIndex = zipWithIndex players in
  let dealerIndex = filter (\(index, j) -> length j.betselfs == currentRound) playersWithIndex |> flip nth 0 |> Tuple.first in
  let totalNumCards = sum cartesDisponibles in
  let totalInCards = sum (map (\j ->  if length j.betselfs == currentRound + 1 then nth (nth j.betselfs currentRound) 1 else 0) players) in
  let correctCard = totalNumCards - totalInCards in
  let guessedOk = players |> List.filterMap 
    (\j ->  if length j.betselfs == currentRound + 1 then if (j.betselfs |> flip nth currentRound |> flip nth 0) == correctCard then Just j.name else Nothing else Nothing) in
  let nGuessedOk = List.length guessedOk in
  let manyguessed = if nGuessedOk > 1 then "" else if nGuessedOk == 1 then " is the only one to have" else "Nobody" in
  let playersAyantVotePour nCarte = sum (map (\(i, j) ->  if i == dealerIndex then 0 else if nth (nth j.betselfs currentRound) 0 == nCarte then 1 else 0) playersWithIndex) in
  let playersAyantVotePourEux = flip List.concatMap playersWithIndex <|
    \(i, j) -> 
      if i == dealerIndex then [] else
      let ayantparie = playersWithIndex |>
        List.filterMap (\(i2, j2) -> if i2 == dealerIndex then Nothing else if nth (nth j.betselfs currentRound) 1 == nth (nth j2.betselfs currentRound) 0 then Just j2.name else Nothing) |>
        String.join ", " in
      if ayantparie == "" then [] else
      [br, textNode <| j.name + " made " + ayantparie + " to believe it was the card of " + j.name]
  in
  let playersWithNewScores = playersWithIndex |>
    map (\(index, player) ->
    (player,
       if index == dealerIndex then
         if nGuessedOk == 0 || nGuessedOk == length players - 1 then 0 else 3
       else (
         let [bet, self] = nth player.betselfs currentRound in
         (if nGuessedOk == 0 || nGuessedOk == length players - 1 then scoreIfEverybodyFound else (
           if bet == correctCard then scoreIfNobodyFound else 0)) +
           playersAyantVotePour self)))
  in
  <div>
     The bets are done! Here are the results of this turn:<br>
     The dealer was @((nth players dealerIndex).name), the dealer's card was the number @correctCard<br>
     @(String.join "," guessedOk)@manyguessed guessed it!
     <div>@(playersAyantVotePourEux)</div><br>
     <div>@(List.concatMap (\(j, score) -> 
       [<span>@(j.name): +@score</span>, <br>]) playersWithNewScores)</div>
     @(button "Next turn" "Passer au tour suivant" players <| \oldplayers ->
          map (\j -> 
                let ({betselfs=b, scores=s} as player, increment) = j in
                { player |
                  betselfs = if length b <= currentRound then b ++ [[0, 0]] else b
                  scores = s ++ [increment] }) playersWithNewScores
     )
  </div>)
  </span>
</div>