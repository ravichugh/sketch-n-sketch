# updatedelay: 0

{length, sum, range, filter, zipWithIndex} = List
{textNode, button, br, select, checkbox, h1, div, element, li, ul} = Html

-- Each element of betselfs is a 2-element array containing the best and the own card number
players = [
  {name="John", betselfs=[], scores=[],  card=0, bet=0}, 
  {name="Nick", betselfs=[], scores=[],   card=0, bet=0},
                                                      
  {name="Pete", betselfs=[], scores=[], card=0, bet=0}
]

cardError = False
betError = 0

mkError txt = Html.span [["color", "lightgray"]] [] (Html.text txt)

cartesDisponibles = range 1 (length players)

currentRound = length (nth players 0).scores

remainingbets = (length players - 1) - sum (map (\j ->  if (length j.betselfs) == currentRound then 0 else 1) players)
allbetsdone = remainingbets == 0

playerEnCoursIndex = 0

scoreIfEverybodyFound = 2
scoreIfNobodyFound = 3

betself i =
  let player = nth players i in
  [ textNode """@(player.name) it's your turn to bet!""", br,
    textNode "Your card:  ", select ("Choose your card number..." :: map (\x -> toString x) cartesDisponibles) player.card, 
    mkError (if player.card == 0 then " Indicate what is your card. This is confidential." else ""), br,
    textNode "Your bet: ", select ("You bet that the correct card is..." :: map (\x -> toString x) cartesDisponibles) player.bet, 
    mkError (if player.bet == 0 then " What card do you think is the dealer's one." else if player.bet == player.card then
      " You cannot bet on your own card." else ""),  br,
    if player.bet == 0 || player.card == 0 || player.bet == player.card then
      Html.span [] [] []
    else
    button "I confirm my bet" "By clicking this button, you confirm you know the rules of the game"
       (player,                          cardError, betError, playerEnCoursIndex) <|
      \({betselfs, card, bet} as player, cardError, betError, playerEnCoursIndex) ->
         if card > 0 && bet > 0 && card /= bet then
           ({ player | betselfs = betselfs ++ [[bet, card]], card = 0, bet = 0}, False, 0, 0)
         else
           (player, card == 0,
              if bet == 0 then 1 else if bet == card then 2 else 0, playerEnCoursIndex)
  ]

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

div [["margin", "20px"]] [] <| [Html.span [] [] 
  [["img", [["width", "100%"], ["src", "https://images-cdn.asmodee.us/filer_public/9e/7e/9e7ea2a6-d531-4f3a-b984-4119925d4c9f/dix01_feature_c85e1c.png"]], []],
   ["img", [["style", [["float","right"]]], ["width", "50%"], ["src", "https://cf.geekdo-images.com/large/img/QFUbpIeEFamJbgJ_Bs5ejDtF8UA=/fit-in/1024x1024/filters:no_upscale()/pic1003159.jpg"]], []],
   h1 [] [] "Dixit scoresheet",
   textNode """To play to Dixit, please enter below the name of the @(length players) players. You can add or remove players.""",
   ul [] [] <| map nomDe players,
  textNode """It's turn number @(currentRound+1).""", br,
  textNode "Current scores:", br,
  Html.table [] [] <| List.map (\j -> 
    Html.tr [] [] [Html.td [] [] j.name, Html.td [] [] (toString (Update.freeze (sum (j.scores))))]) players,
  if remainingbets > 1 then
    div [] [] [
    Html.textNode "Who is currently placing a bet? ",
    select (map (\j ->  j.name) playersEnCours) playerEnCoursIndex, br,
    div [] [] <|
      betself (playerIndexFromName (nth playersEnCours playerEnCoursIndex).name)
    ]
  else if remainingbets == 1 then
    div [] [] <|
      betself (playerIndexFromName (nth playersEnCours playerEnCoursIndex).name)
  else
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
      [br, textNode <| j.name + " made " + ayantparie + " to believe it was " + j.name + "'s card"]
  in
  let playersWithNewScores =  playersWithIndex |>
    map (\(index, player) ->
    (player,
       if index == dealerIndex then
         if nGuessedOk == 0 || nGuessedOk == length players - 1 then 0 else 3
       else (
         let [bet, self] = nth player.betselfs currentRound in
         (if nGuessedOk == 0 || nGuessedOk == length players - 1 then scoreIfEverybodyFound else (
           if bet == correctCard then scoreIfNobodyFound else 0)) +
           playersAyantVotePour self))) in
  div [] [] [
     textNode "The bets are done! Here are the results of this turn:",br,
     textNode """The dealer was @((nth players dealerIndex).name), the dealer's card was the number @correctCard""", br,
     textNode """@(String.join "," guessedOk)@manyguessed guessed it!""",
     div [] [] <| playersAyantVotePourEux, br,
     div [] [] <| List.concatMap (\(j, score) -> 
       let onebet = """@(j.name): @score""" in
       -- TODO: Debug why mulitline string literal does not parse for onebet
       [textNode onebet, br]) playersWithNewScores,
     button "Next turn" "Passer au tour suivant" players <| \oldplayers ->
       map (\j -> 
                let ({betselfs=b, scores=s} as player, increment) = j in
                { player |
                  betselfs = if length b <= currentRound then b ++ [[0, 0]] else b
                  scores = s ++ [increment] }) playersWithNewScores
    ]
  ]]