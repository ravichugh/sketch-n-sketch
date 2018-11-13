main = styleLens <| 
  <div>
  <style>
  .colored, div .extra {
    background: #F88;
    color: green;
  }
  </style>
  Hello <span class="colored">world</span>!
  </div>
















-- TODO: Remove once in Prelude
List= {List |
  all pred list = case list of
    [] -> True
    hd :: tl -> if pred hd then all pred tl else False
  any pred list = case list of
    [] -> False
    hd :: tl -> if pred hd then True else any pred tl
  zip list1 list2 = List.map2 (,) list1 list2
}

-- Transforms a text style into an list of (list of selector, list of [key, value])
-- Careful: Regex.split does not properly allows the insertion of elements.
parseStyle style =
  Regex.split """\}""" style
  |> List.filterMap (\str ->
    case Regex.extract """^\s*([^\{]+?)\s*\{([\s\S]*)$""" str of
      Just [selectors, attrs] ->
        let selectorList = Regex.split """\s*,\s*""" selectors in
        let attrList = Regex.split """\s*;\s*""" attrs
              |> List.filterMap (\str ->
              Regex.extract """^\s*([^:]+?)\s*:\s*([\s\S]*?)\s*$""" str)
        in Just (selectorList, attrList)
      Nothing -> Nothing
  )

--selectorAttrs is a class/id/pseudo selector for a single node.
--nodeAttrs is the list of attributes of a node.
nodeMatchAttrs nodeAttrs selectorAttrs =
  selectorAttrs
  |> Regex.split """(?=:|\.|#)"""
  |> List.all (Regex.extract """^(.)(\w+)$""" >> case of
    Just [".", className] ->
      nodeAttrs
      |> List.mapFirstSuccess (case of
        ["class", classes] -> Just (
            let splittedClasses = Regex.split """\s+""" classes in
            List.any (== className) splittedClasses
          )
        _ -> Nothing)
      |> Maybe.withDefault False
    Just ["#", id] ->
      nodeAttrs
      |> List.mapFirstSuccess (case of
        ["id", nodeId] -> Just (nodeId == id))
      |> Maybe.withDefault False
    Just [":", pseudo] -> True
    _ -> False) -- TODO: Handle ":not", ":nth-child..."

-- Returns True if the selector matches the sequence of nodes.
-- selectorMatches False (css selector) (sequence of nodes)
selectorMatches mustMatch selector nodeSeq = 
  Debug.log ("""selectorMatches @mustMatch '@selector' @nodeSeq""") <|
  case Regex.extract """^(\w+)((?:[:\.#]\w+)*)\s*([\s\S]*)$""" selector of
    Just [tag, attrs, rem] ->
      let  _ = Debug.log """@tag @attrs @rem""" () in
      case nodeSeq of
        [] -> False
        [tagN, attrsN, _]::nodeTail ->
          if tagN == tag && nodeMatchAttrs attrsN attrs then
            if selectorMatches False rem nodeTail then True
            else
              if mustMatch then False
              else selectorMatches False selector nodeTail
          else
            if mustMatch then False
            else selectorMatches False selector nodeTail
    Nothing ->
  case Regex.extract """^((?:[:\.#]\w+)+)\s*([\s\S]*)$""" selector of
    Just [attrs, rem] ->
      case nodeSeq of
        [] -> False
        [tagN, attrsN, _]::nodeTail ->
         if nodeMatchAttrs attrsN attrs then
            if selectorMatches False rem nodeTail then True
            else
              if mustMatch then False
              else selectorMatches False selector nodeTail
          else
            if mustMatch then False
            else selectorMatches False selector nodeTail
    Nothing ->
  case Regex.extract """^>\s*([\s\S]+)$""" selector of
    Just [rem] ->
      selectorMatches True rem nodeSeq
    Nothing ->
  case Regex.extract """^\s*$""" selector of
     Just _ -> True
     Nothing -> False

collectStyle node = case node of
  ["style", [], [["TEXT", d]]] -> d
  [_, _, children] -> List.map collectStyle children |> String.join ""
  _ -> ""
     
styleLens node = Update.lens2 {
  apply (styleList, node) = node
  update {outputNew=newNode, input=(styleList, oldNode)} = -- We look for style changes, and we try to back-propagate them to <style> tags.
    -- Normally we should walk through the diffs, but for now let's assume it's not shape changing.
    let collectCssChanges newStyleList nodeSeq oldNode newNode =
      let newNodeSeq = nodeSeq ++ [newNode] in
      case oldNode of
        ["TEXT", _] -> (newStyleList, newNode)
        [tag, attrs, children] -> case newNode of
          [tag2, attrs2, children2] ->
            let (newChildrenStyleList, newChildren2) =
              List.foldl (\(oldChild, newChild) (newStyleList, accChild) ->
                let (newStyleList, finalChild) =
                  collectCssChanges newStyleList newNodeSeq oldChild newChild in
                (newStyleList, accChild ++ [finalChild])
              ) (newStyleList, []) (List.zip children children2)
            in
            let extractStyles acc newAttrs = case newAttrs of
              ["style", styles]::tail ->
                (acc ++ tail, Just styles)
              head :: tail -> extractStyles (acc ++ [head]) tail
              [] -> (acc, Nothing)
            in
            case extractStyles [] attrs2 of
              (newAttrs2, Just styles) ->
                let finalStyleList = newChildrenStyleList |>
                  List.map (\(selectors, keyValues) as skv ->
                    if List.any (\selector -> selectorMatches False selector newNodeSeq) selectors then
                      let newKeyValues = keyValues |>
                        List.map (\[key, value] as kv ->
                          styles
                          |> List.mapFirstSuccess (\[newStyleKey, newStyleValue] ->
                            if String.trim newStyleKey == key then
                              Just [key, newStyleValue]
                            else Nothing)
                          |> Maybe.withDefault kv
                        )
                      in
                      (selectors, newKeyValues)
                    else skv
                  )
                in
                (finalStyleList, [tag2, newAttrs2, newChildren2])
              (_, Nothing) ->
                (newChildrenStyleList, [tag2, attrs2, newChildren2])
    in
    let (newStyleList, newNode) = collectCssChanges styleList [] oldNode newNode in
    Ok (Inputs [(newStyleList, newNode)])
} (parseStyle (collectStyle node)) node