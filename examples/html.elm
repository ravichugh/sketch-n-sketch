map f = case of [] -> []; head::tail -> f head::map f tail

-- Returns a list of HTML nodes parsed from a string. This function is bidirectional
html string = {
  apply trees = 
    letrec domap tree = case tree of
      ["HTMLInner", v] -> let _ = debug ("HTMLInner" + v) in ["TEXT", replaceAllIn "&amp;|&lt;|&gt;|</[^>]*>" (\{match} -> case match of "&amp;" -> "&"; "&lt;" -> "<"; "&gt;" -> ">"; _ -> "") v]
      ["HTMLElement", tagName, attrs, ws1, endOp, children, closing] ->
        let _ = debug "children" in
        let _ = debug children in
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
      {removed=[removed]}::{added=[added]}::dt ->
        let newIn = case [take 1 ins, added] of
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
      {removed}::dt ->
        mergeAttrs acc (drop (len removed) ins) dt
      {added}::dt ->
        let newIns = map toHTMLAttribute added in
        mergeAttrs (append acc newIns) ins dt
    in
    letrec toHTMLNode e = case e of
      ["TEXT",v2] -> toHTMLInner v2
      [tag, attrs, children] -> ["HTMLElement", tag, map toHTMLAttribute attrs, "",
           ["RegularEndOpening"], map toHTMLNode children, ["RegularClosing", ""]]
    in
    letrec mergeNodes acc ins d = case d of
      [] -> acc
      {kept}::dt -> mergeNodes (append acc (take (len kept) ins)) (drop (len kept) ins) dt
      {removed=[removed]}::{added=[added]}::dt ->
        case [take 1 ins, removed, added] of
          [ ["HTMLInner", v], _, ["TEXT",v2]] ->
             let newIn = toHTMLInner v2 in
             mergeNodes (append acc [newIn]) (drop 1 ins) dt
          [ ["HTMLElement", tagName, attrs, ws1, endOp, children, closing],
            [tag1, attrs1, children1],
            [tag2, attrs2, children2] ] ->
             if tag2 == tagName then
               ["HTMLElement", tag2, mergeAttrs [] attrs1 (diff attrs1 attrs2), ws1, endOp,
                  mergeNodes [] children1 (diff children1 children2), closing]
             else toHTMLNode added
          _ -> merge (append acc [toHTMLNode added]) (drop 1 ins) dt
      {removed}::dt ->
        mergeNodes acc (drop (len removed) ins) dt
      {added}::dt ->
        mergeNodes (append acc (map toHTMLNode added)) ins dt
    in
    mergeNodes [] input (diff outputOld outputNew)
}.apply (parseHTML string)

["span", [], html "<h3>Hello world</h3>"]