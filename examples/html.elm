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
      HTMLInner v -> let _ = debug (" HTMLInner" + v) in ["TEXT", replaceAllIn "&amp;|&lt;|&gt;|</[^>]*>" (\{match} -> case match of "&amp;" -> "&"; "&lt;" -> "<"; "&gt;" -> ">"; _ -> "") v]
      HTMLElement tagName attrs ws1 endOp children closing ->
        [ tagName
        , map (case of
          HTMLAttribute ws0 name value -> case value of
            HTMLAttributeUnquoted _ _ content -> [name, content]
            HTMLAttributeString _ _ _ content -> [name, content]
            HTMLAttributeNoValue -> [name, ""]) attrs
        , map domap children]
      HTMLComment _ content -> ["comment", [["display", "none"]], [["TEXT", content]]]
    in map domap trees

  update {input, outputOld, outputNew} =
    let toHTMLAttribute [name, value] = HTMLAttribute " " name (HTMLAttributeString "" "" "\"" value) in
    let toHTMLInner text = HTMLInner (replaceAllIn "<|>|&" (\{match} -> case match of "&" -> "&amp;"; "<" -> "&lt;"; ">" -> "&gt;"; _ -> "") text) in
    letrec mergeAttrs acc ins d = case d of
      [] -> acc
      {kept}::dt -> mergeAttrs (append acc (take (len kept) ins)) (drop (len kept) ins) dt
      {deleted=[deleted]}::{inserted=[inserted]}::dt ->
        let newIn = case [take 1 ins, inserted] of
          [ [HTMLAttribute sp0 name value], [name2, value2]] ->
            case value of
              HTMLAttributeUnquoted sp1 sp2 v ->
                case extractFirstIn "\\s" v of
                  Nothing ->
                    HTMLAttribute sp0 name2 (HTMLAttributeUnquoted sp1 sp2 value2)
                  _ ->
                    HTMLAttribute sp0 name2 (HTMLAttributeString sp1 sp2 "\"" value2)
              HTMLAttributeString sp1 sp2 delim v ->
                    HTMLAttribute sp0 name2 (HTMLAttributeString sp1 sp2 delim value2)
              HTMLAttributeNoValue -> 
                 if value2 == "" then HTMLAttribute sp0 name2 HTMLAttributeNoValue]
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
      [tag, attrs, children] -> HTMLElement tag (map toHTMLAttribute attrs) ""
           RegularEndOpening (map toHTMLNode children) RegularClosing ""
    in
    letrec mergeNodes acc ins d = let _ = debug "mergeNodes, acc=" in let _ = debug acc in case d of
      [] -> acc
      {kept}::dt -> mergeNodes (append acc (take (len kept) ins)) (drop (len kept) ins) dt
      {deleted=[deleted]}::{inserted=[inserted]}::dt ->
        let newElement = case [take 1 ins, deleted, inserted] of
          [ HTMLInner v, _, ["TEXT",v2]] -> toHTMLInner v2
          [ HTMLElement tagName attrs ws1 endOp children closing,
            [tag1, attrs1, children1], [tag2, attrs2, children2] ] ->
             if tag2 == tagName then
               HTMLElement tag2 mergeAttrs [] attrs1 (diff attrs1 attrs2) ws1 endOp
                  (mergeNodes [] children (diff children1 children2)) closing
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

["span", [], html "<h3>Hello world</h3>"]