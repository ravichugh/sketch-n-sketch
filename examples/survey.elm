display question = 
  <span>
    <h2>@question.title</h2>
    @(Update.debug "answers" <| flip indexedMap question.answers <| \i answer ->
      let checkid = question.id + toString i in
      <div><input type="radio" id=checkid name=question.id>@("""@(i+1). """)<label for=checkid>@answer</label></div>
    )</span>

likert = [
 "I will lobby against it",
 "I hate it",
 "I do not like it",
 "It is ok.",
 "I like it",
 "I love it"]

questions = [
  { title = "How do you like Elm?",
    id = "like-elm",
    answers = likert
  },
  { title = "How do you like Scala?",
    id = "like-scala",
    answers = likert
  }]

main = <div><span>
<h1>Survey</h1>
@(map display questions)
</span></div>