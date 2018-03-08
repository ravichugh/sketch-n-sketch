map f =
  letrec aux l = case l of
    [] -> []
    head::tail -> f head::map f tail
  in aux
take n =
  letrec aux l = let _ = debug "take" in let _ = debug n in let _ = debug l in if n == 0 then [] else
    case l of
      [] -> []
      head::tail -> head :: (take (n - 1) tail)
  in aux
drop n =
  letrec aux l = if n == 0 then l else
    case l of
      [] -> []
      head::tail -> drop (n - 1) tail
  in aux

-- Returns a list of HTML nodes parsed from a string. This function is bidirectional
html string = {
  apply trees = 
    letrec domap tree = case tree of
      ["HTMLInner", v] -> let _ = debug ("HTMLInner" + v) in ["TEXT", replaceAllIn "&amp;|&lt;|&gt;|</[^>]*>" (\{match} -> case match of "&amp;" -> "&"; "&lt;" -> "<"; "&gt;" -> ">"; _ -> "") v]
      ["HTMLElement", tagName, attrs, ws1, endOp, children, closing] ->
        [ tagName
        , map (case of
          ["HTMLAttribute", ws0, name, value] -> case value of
            ["HTMLAttributeUnquoted", _, _, content ] -> [name, content]
            ["HTMLAttributeString", _, _, _, content ] -> [name, content]
            ["HTMLAttributeNoValue"] -> [name, ""]) attrs
        , map domap children]
      ["HTMLComment", _, content] -> ["comment", [["display", "none"]], [["TEXT", content]]]
    in map domap trees

  update {input, outputOld, outputNew} =
    let toHTMLAttribute [name, value] = ["HTMLAttribute", " ", name, ["HTMLAttributeString", "", "", "\"", value]] in
    let toHTMLInner text = ["HTMLInner", replaceAllIn "<|>|&" (\{match} -> case match of "&" -> "&amp;"; "<" -> "&lt;"; ">" -> "&gt;"; _ -> "") text] in
    letrec mergeAttrs acc ins d = case d of
      [] -> acc
      {kept}::dt -> mergeAttrs (append acc (take (len kept) ins)) (drop (len kept) ins) dt
      {deleted=[deleted]}::{inserted=[inserted]}::dt ->
        let newIn = case [take 1 ins, inserted] of
          [ [["HTMLAttribute", sp0, name, value]], [name2, value2 ]] ->
            case value of
              ["HTMLAttributeUnquoted", sp1, sp2, v] ->
                case extractFirstIn "\\s" v of
                  ["Nothing"] ->
                    ["HTMLAttribute", sp0, name2, ["HTMLAttributeUnquoted", sp1, sp2, value2]]
                  _ ->
                    ["HTMLAttribute", sp0, name2, ["HTMLAttributeString", sp1, sp2, "\"", value2]]
              ["HTMLAttributeString", sp1, sp2, delim, v] ->
                    ["HTMLAttribute", sp0, name2, ["HTMLAttributeString", sp1, sp2, delim, value2]]
              ["HTMLAttributeNoValue"] -> 
                 if value2 == "" then ["HTMLAttribute", sp0, name2, ["HTMLAttributeNoValue"]]
                 else toHTMLAttribute [name2, value2]
              _ -> "Error, expected HTMLAttributeUnquoted, HTMLAttributeString, HTMLAttributeNoValue" + 1
        in mergeAttrs (append acc [newIn]) (drop 1 ins) dt
      {deleted}::dt ->
        mergeAttrs acc (drop (len deleted) ins) dt
      {inserted}::dt ->
        let newIns = map toHTMLAttribute inserted in
        mergeAttrs (append acc newIns) ins dt
    in
    letrec toHTMLNode e = case e of
      ["TEXT",v2] -> toHTMLInner v2
      [tag, attrs, children] -> ["HTMLElement", tag, map toHTMLAttribute attrs, "",
           ["RegularEndOpening"], map toHTMLNode children, ["RegularClosing", ""]]
    in
    letrec mergeNodes acc ins d = let _ = debug "mergeNodes, acc=" in let _ = debug acc in case d of
      [] -> acc
      {kept}::dt -> mergeNodes (append acc (take (len kept) ins)) (drop (len kept) ins) dt
      {deleted=[deleted]}::{inserted=[inserted]}::dt ->
        let newElement = case [take 1 ins, deleted, inserted] of
          [ ["HTMLInner", v], _, ["TEXT",v2]] -> toHTMLInner v2
          [ ["HTMLElement", tagName, attrs, ws1, endOp, children, closing],
            [tag1, attrs1, children1], [tag2, attrs2, children2] ] ->
             if tag2 == tagName then
               ["HTMLElement", tag2, mergeAttrs [] attrs1 (diff attrs1 attrs2), ws1, endOp,
                  mergeNodes [] children (diff children1 children2), closing]
             else toHTMLNode inserted
          _ -> toHTMLNode inserted
        in
        mergeNodes (append acc [newElement]) (drop 1 ins) dt
      {deleted}::dt ->
        mergeNodes acc (drop (len deleted) ins) dt
      {inserted}::dt ->
        mergeNodes (append acc (map toHTMLNode inserted)) ins dt
    in
    let _ = debug "starting" in
    [mergeNodes [] input (diff outputOld outputNew)]
}.apply (parseHTML string)

-- temporary, put in library
let matchIn r x = case extractFirstIn r x of
  ["Nothing"] -> False
  _ -> True
in
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