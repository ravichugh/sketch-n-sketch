let original =
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

""" in
let trim s =
  let trimmed_left = replaceAllIn "^\\s+" "" s in
  replaceAllIn "\\s+$" "" trimmed_left in
letrec sprintf str inline = case inline of
  a::tail -> sprintf (replaceFirstIn "%s" a str) tail
  [] -> str
  a -> replaceFirstIn "%s" a str
in
let strlen str = len (explode str)  in
letrec foldLeft init list fun = case list of
  [] -> init
  head:: tail -> let newInit = (fun init head) in
    foldLeft newInit tail fun
in
-- Thanks to https://gist.github.com/jbroadway/2836900
let markdown text =
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
      sprintf "\n<ul>\n\t<li>%s</li>\n</ul>" (trim item)
    ol_list regs =
      let item = nth regs.group 1 in
      sprintf "\n<ol>\n\t<li>%s</li>\n</ol>" (trim item)
    blockquote regs =
      let item = nth regs.group 2 in
      sprintf "\n<blockquote>%s</blockquote>" (trim item)
    header regs =
      let {group= [tmp, nl, chars, header]} = regs in
      let level = toString (strlen chars) in
      sprintf "<h%s>%s</h%s>" [level, trim header, level]
  } in
  let rules = [
    ["(\n|^)(#+)(.*)", self.header],                              -- headers
    ["\\[([^\\[]+)\\]\\(([^\\)]+)\\)", "<a href='$2'>$1</a>"],    -- links
    ["(\\*\\*|__)(?=[^\\s\\*_])(.*?)\\1", "<strong>$2</strong>"], -- bold
    ["(\\*|_)(?=[^\\s\\*_])(.*?)\\1", "<em>$2</em>"],             -- emphasis
    ["\\~\\~(.*?)\\~\\~", "<del>$1</del>"],                       -- del
    ["\\:\"(.*?)\"\\:", "<q>$1</q>"],                             -- quote
    ["`\\b(.*?)\\b`", "<code>$1</code>"],                         -- inline code
    ["\r?\n\\*(.*)", self.ul_list, {
      postReverse out = 
        replaceAllIn """\r?\n<ul>\r?\n\t<li>((?:(?!</li>)[\s\S])*)</li>\r?\n</ul>""" "\n* $1" out
    }],                                  -- ul lists
    ["\r?\n[0-9]+\\.(.*)", self.ol_list, {
      postReverse out = 
        replaceAllIn """\r?\n<ol>\r?\n\t<li>((?:(?!</li>)[\s\S])*)</li>\r?\n</ol>""" "\n1. $1" out
    }],                            -- ol lists
    ["\r?\n(&gt;|\\>)(.*)", self.blockquote],                        -- blockquotes
    ["\r?\n-{5,}", "\n<hr>"],                                        -- horizontal rule
    ["\r?\n\r?\n(?!<ul>|<ol>|<p>|<blockquote>)","<br>"],                -- add newlines
    ["\r?\n</ul>\\s?<ul>", "", {
      postReverse out = 
        replaceAllIn """(<(ul|ol)>(?:(?!</\2>)[\s\S])*)</li>\s*<li>""" "$1</li>\n</$2>\n<$2>\n\t<li>" out
    }],                                     -- fix extra ul
    ["\r?\n</ol>\\s?<ol>", ""],                                      -- fix extra ol, and extract blockquote
    ["</blockquote>\\s?<blockquote>", "\n"]
  ] in
  let finaltext = "\n" + text + "\n" in
  foldLeft finaltext rules (\acc elem -> case elem of
      [regex, replacement] -> replaceAllIn regex replacement acc
      [regex, replacement, {postReverse = fun}] ->
        let newAcc = { apply acc = acc, unapply out = ["Just",  fun out]}.apply acc in
        replaceAllIn regex replacement newAcc 
  )
in
--let converter_lens x = {
--  apply x = x,
--  unapply {outputNew} =
--    case extractFirstIn """^<div>([\s\S]*)</div>""" inserted of
--      ["Just", [content]] ->
--        if (matchIn "(?:^|\n)#(.*)$" left) then -- Title, we jump only one line
--          ["Just", left + "\n" + content + right]
--        else -- we jump TWO lines
--          ["Just", left + "\n\n" + content + right]
--      ["Nothing"] ->    
--        if(matchIn """(?:^|\n)(#|\*|\d\.)(.*)$""" left) then
--          ["Just", outputNew]
--        else
--          let newInserted = replaceAllIn """<br>"""  "\n\n" inserted in
--          ["Just", left + newInserted + right]
--  }.apply x in
let converter_lens x = x in
["span", [], html ((freeze markdown) (converter_lens original))]