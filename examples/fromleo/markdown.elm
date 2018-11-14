original =
  """#[Markdown](https://fr.wikipedia.org/wiki/Markdown) demo
This is *an **almost :"bidirectional":*** markdown
editor. You can **fully** edit the value of the variable `original` on the right,
and _partially_ edit the html on the right.

*Limitation*: Since this is a regex-based transformation, it cannot correctly nest italics into bold (unless you use underscores instead of stars)
#### Markdown source
* Headers of level n are prefixed with n #
* Use * or 1. to introduce lists

#### Html rendering
1. You can insert elements in lists.
2. Use SHIFT+ENTER for new lines.
3. Do not use CTRL+V

#### Anywhere
2. Add bold by wrapping with two stars.
3. Add emphasis by wrapping with underscorses.
1. Use backtick to insert `code`

>Markdown is a lightweight markup
>language with plain text formatting syntax

"""

{trim, sprintf, length} = String
{foldl = foldLeft} = List
{freeze, strDiffToConcreteDiff} = Update

-- Thanks to https://gist.github.com/jbroadway/2836900
markdown text =
  let self = {
    para regs =
      let line = nth regs.group 2 in
      let trimmed = trim line in
      if (matchIn "^</?(ul|ol|li|h|p|bl)" trimmed) then (
        (nth regs.group 1) + line
      ) else (
        sprintf "%s<p>%s</p>\n" [nth regs.group 1, line]
      )
    ul_list regs = 
      let item = nth regs.group 1 in
      sprintf "\n<ul>\n\t<li >%s</li>\n</ul>" (trim item)
    ol_list regs =
      let item = nth regs.group 1 in
      sprintf "\n<ol>\n\t<li >%s</li>\n</ol>" (trim item)
    blockquote regs =
      let item = nth regs.group 2 in
      sprintf "\n<blockquote>%s</blockquote>" (trim item)
    header regs =
      let {group= [tmp, nl, chars, header]} = regs in
      let level = toString (length chars) in
      sprintf "<h%s>%s</h%s>" [level, trim header, level]
  } in
  let rules = [
    ["(\n|^)(#+)(.*)", self.header],                              -- headers
    ["\\[([^\\]]+)\\]\\(([^\\)]+)\\)", "<a href='$2'>$1</a>"],    -- links
    ["(\\*\\*|__)(?=[^\\s\\*_])(.*?)\\1", "<strong>$2</strong>"], -- bold
    ["(\\*|_)(?=[^\\s\\*_])(.*?)\\1", "<em>$2</em>"],             -- emphasis
    ["\\~\\~(.*?)\\~\\~", "<del>$1</del>"],                       -- del
    ["\\:\"(.*?)\"\\:", "<q>$1</q>"],                             -- quote
    ["`\\b(.*?)\\b`", "<code>$1</code>"],                         -- inline code
    ["\r?\n\\*(.*)", self.ul_list, {
      postReverse = 
        updateReplace """\r?\n<ul>\r?\n\t<li>((?:(?!</li>)[\s\S])*)</li>\r?\n</ul>""" "\n* $1"
    }],                                  -- ul lists
    ["\r?\n[0-9]+\\.(.*)", self.ol_list, {
      postReverse  = 
        updateReplace """\r?\n<ol>\r?\n\t<li>((?:(?!</li>)[\s\S])*)</li>\r?\n</ol>""" "\n1. $1"
    }],                            -- ol lists
    ["\r?\n(&gt;|\\>)(.*)", self.blockquote],                        -- blockquotes
    ["\r?\n-{5,}", "\n<hr>"],                                        -- horizontal rule
    ["\r?\n\r?\n(?!<ul>|<ol>|<p>|<blockquote>)","<br>"],                -- add newlines
    ["\r?\n</ul>\\s?<ul>", "", {
      postReverse out diffs =
        updateReplace """(<li>(?:(?!</li>)[\s\S])*</li>)(?=[\s\S]*?</(ul|ol)>)""" "\n</$2>\n<$2>\n\t$1" out diffs |>
        case of
          (v, VStringDiffs l) ->
            if not (v == out) then
              (v, VStringDiffs (List.map (
                \StringUpdate a b r -> StringUpdate (a + 6) (b + 6) r) l))
            else (out, diffs)
    }],                                     -- fix extra ul
    ["\r?\n</ol>\\s?<ol>", ""],                                      -- fix extra ol, and extract blockquote
    ["</blockquote>\\s?<blockquote>", "\n"]
  ] in
  let finaltext = "\n" + text + "\n" in
  foldLeft (\elem acc -> case elem of
      [regex, replacement] -> Regex.replace regex replacement acc
      [regex, replacement, {postReverse = fun}] ->
        let newAcc = {
          apply acc = acc
          update {input,newOutput=out,diffs} =
            case fun out diffs of
              (value, diff) -> Ok (InputsWithDiffs [(value, Just diff)])
              value  -> case Update.diff input value of
                Ok n -> Ok (InputsWithDiffs [(value, n)])
                Err x -> Err x
          }.apply acc in
        Regex.replace regex replacement newAcc 
  ) finaltext rules 

-- Takes care of newlines inserted in the document.
newlines_lens x = {
  apply x = x
  update {outputNew,diffs} =
    let aux offset d strAcc = case d of
      [] -> strAcc
      ((ConcStringUpdate start end inserted) :: dtail) ->
        let left = String.take (start + offset) strAcc in
        let right =  String.dropLeft (start + offset + String.length inserted) strAcc in
        let newInserted =
          case Regex.extract """^<div>([\s\S]*)</div>""" inserted of
            Just [content] ->
              if Regex.matchIn "(?:^|\n)#(.*)$" left then -- Title, we jump only one line
                "\n" + content + "\n"
              else -- we jump TWO lines
                "\n\n" + content + "\n"
            Nothing ->    
              if Regex.matchIn """(?:^|\n)(#|\*|\d\.)(.*)$""" left then
                inserted
              else
                Regex.replace """<br>"""  "\n\n" inserted
        in
        left + newInserted + right |>
        aux (offset + String.length inserted - (end - start)) dtail
    in Ok (Inputs [aux 0 (strDiffToConcreteDiff outputNew diffs) outputNew])
  }.apply x

Html.forceRefresh <|
Html.span [] [] <| html ((freeze markdown) (newlines_lens original))