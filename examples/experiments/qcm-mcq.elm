username = "Cédric" -- Obtained from log in

userdata = [
  ("dummy", [ ("firstquestion", Numeric 1)
            ])
]

mbWrap i answerToRecord s = if i == answerToRecord then s + " (chosen answer)" else s

questions = [
  {
    id = "firstquestion"
    defaultAnswer = Phonetic ""  -- Will never be stored.
    render (Phonetic userAnswer) =
      let answer i hint name = 
        Html.button (mbWrap i userAnswer name) hint userAnswer (\_ -> i)
      in
      <div class="question">
        <span>Question 1: What sound cannot pronounced the same as the three others?<br></span>
         @(answer "e" "he" "e")
         @(answer "o" "women" "o")
         @(answer "i" "fish" "i")
         @(answer "a" "man" "a")
      </div>
  },
  {
    id = "secondquestion"
    defaultAnswer = Numeric -1 -- Will never be stored.
    render (Numeric userAnswer) =
      let answer i hint name = 
        Html.button (mbWrap i userAnswer name) hint userAnswer (\_ -> i)
      in
      <div class="question">
        <span>Question 2: What sound is definitely not the same as the others?<br></span>
          @(answer 0 "friendship" "f")
          @(answer 1 "pharmacy" "ph")
          @(answer 2 "twentieth" "th")
          @(answer 3 "enough" "gh")
      </div>
  }
]

-- Dictionary of questions id/answers made for this user
questionsAnswered = 
  listDict.get username userdata
  |> Maybe.withDefaultReplace (freeze [])

-- Compute the next question to ask (either Nothing or Just question)
questionToAsk = questions
  |> List.find (\question -> not <| listDict.member question.id questionsAnswered)

-- Displays a particular question
display {render, id, defaultAnswer} =
  render <|
      -- Note that because of withDefaultReplace,
      -- changing the argument will store the result at the right place.
    (listDict.get id questionsAnswered |> Maybe.withDefaultReplace (freeze defaultAnswer))
 
main = <div style="margin:10px" contenteditable="true">
  <h1>Evaluation quizz for @username</h1>
  @(Maybe.map display questionToAsk |> Maybe.withDefault "Fini")
  <br><br>
  <h2>Progression of @username</h2>
  @(List.map display questions)
</div>

dictLike = {
  apply this d x = case this.get x d of
    Just x -> x
    _ -> Debug.crash ("Expected element " + toString x + " in dict, got nothing")
  member this key list = this.get key list /= Nothing
  contains this = this.member
  delete this = this.remove
  update this k f d = case f <| this.get k d of
    Nothing -> this.delete k d
    Just v -> this.insert k v d
}

-- List of pair implementation
listDict = { dictLike |
  empty = []
  fromList = identity
  get key = Update.lens {
    apply list = case list of
      [] -> Nothing
      (k, v) :: tail -> if k == key then Just v else get key tail
    update = case of
      {input, outputOld = Nothing, outputNew = Just v} ->
        Ok (Inputs [insert key v input])
      uInput -> Update.default apply uInput
    }
  remove key list = case list of
    [] -> []
    ((k, v) as head) :: tail -> if k == key then tail else head :: delete key tail
  insert key value list = case list of
    [] -> [(key, value)]
    ((k, v) as head) :: tail -> if k == key then (k, value)::tail else head :: insert key value tail
}

-- Given a place where to store something,
--       the value to display
--       a callback on how to transform the new value to the place where to store
-- stores the result of transforming the value to the place
storeTo storingPlace outputHandler value = Update.lens {
  apply x = value
  update {input, outputNew} =
    Ok (Inputs [outputHandler outputNew])
} storingPlace