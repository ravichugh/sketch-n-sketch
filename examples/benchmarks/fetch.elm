state = {posts = browser.localvar "posts" "[]"}

<div>
  <button onclick="""
    fetch('https://jsonplaceholder.typicode.com/posts')
    .then(response => response.json())
    .then(c => {posts = c; @browser.refresh; })
  """>Get posts</button>
  @(state.posts |> List.map (\post ->
    <div key=@post.id>
      <h2>@post.title</h2>
      <p>@post.body</p>
    </div>
  ))
</div>