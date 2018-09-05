module GroupStartMap exposing (find, replace, Match)

import Regex exposing (HowMany(..), Regex, regex)
import Char
import Dict exposing (Dict)
import ImpureGoodies exposing (mutateRecordField)

stringCharAt i = Maybe.map String.fromChar << ImpureGoodies.stringCharAt i

-- GroupStartMap.find, .replace and everything else provide a Match with the position of all subgroups.

{-* The goal of a GroupStartMap is to retrieve the start position of each
  * group of a matching regular expression where only the strings of the
  * matched groups are known.
  * For that, we use the following observation:
  * If the regex /A(B)\1/ matches a string at a given index,
  * then         /(A)(B)\2/ matches the same string at the same index,
  * However, in the second regex, we can use the length of the first group (A)
  * to retrieve the start position of the second group (B).
  * Note that the back-references in the second regex are shifted, but this
  * does not change the matched strings.
  *
  * Implementation details:
  * - It parses the regular expression into a tree of type Node, that contains:
  *   + Either modifiers (e.g. +, *, (?=....)), or an original group number
  *   + Either a raw regular expression, or a list of children Node
  * - It converts this Node to a regex string, such that every sub-part of the
  *   regex which was not yet in a group now belongs to a group
  * - The new regex matches the original string at the original position
  * - It propagates the matched strings of all groups into the Node
  * - It computes the start of every group thanks to the groups before it
  * - It builds and returns the mapping of previous group number -> start
  *
  * @author Mikaël Mayer
  *-}

type alias Match =
    { match : String
    , submatches : List { match: Maybe String, start: Int }
    , index : Int
    , number : Int
    }

{--
startOf: String -> Int -> String -> (Int -> Int)
startOf reg start string =
  apply string start <| regex reg

apply: String -> Int -> Regex -> (Int -> Int)
apply string start pattern =
  mapping (GroupStartMap string start pattern)
--}



-- parentheses aspect of the regex
type OriginalRegex =
  -- If the regex had originally a group number, that is, with simple parentheses
  -- non-grouping parentheses (?: ...) are always added
  OriginallyGroupped Int
  --// If the regex was not originally grouped with parentheses,
  --// then left and right are modifiers (e.g. *, +, *?, +?, ?, (?= ...), (?! ...)  )
  | OriginallyWrapped String String

foldOriginalRegex: OriginalRegex -> (Int -> a) -> (String -> String -> a) -> a
foldOriginalRegex this numberE leftRightF =
  case this of
    OriginallyGroupped number -> numberE number
    OriginallyWrapped left right -> leftRightF left right

originallyWrapped: {
  isRepeatModifier: String -> Bool,
  repeaterUnapply: OriginalRegex -> Maybe String,
  absoluteUnapply: OriginalRegex -> Bool }
originallyWrapped = {
  isRepeatModifier = \modifier ->
      modifier == "?" || modifier == "??" || modifier == "*" || modifier == "+" ||
      modifier == "*?" || modifier == "+?" || String.startsWith "{" modifier
  , repeaterUnapply = \e ->
     case e of
       OriginallyWrapped left right ->
         if (left == "" && originallyWrapped.isRepeatModifier right) then Just right else Nothing
       _ -> Nothing
  , absoluteUnapply =  \e ->
      case e of
        OriginallyWrapped left right ->
           (left == "(?!" || left == "(?=") && right == ")"
        _ -> False
  }

-- Type of the node, a regex or a sequence of nodes.
type NodeType =
  -- A pure regex, without any subgroups
  RegexLeaf String
  -- A parent groupping, with children nodes.
  -- Modifiers to this group, such as star or plus, can be added using
  -- the field of Node originalGroup.
  | ParentNode (List Node)
  -- A parent groupping with children nodes which are in a disjunction
  | DisjunctNode (List Node)


foldNodeType: NodeType -> (String -> a) -> (List Node -> a) -> (List Node -> a) -> a
foldNodeType this regexF childrenF childrenD =
  case this of
    RegexLeaf regex -> regexF regex
    ParentNode children -> childrenF children
    DisjunctNode children -> childrenD children

-- Creates a ParentNode, but in the presence of disjunctions,
-- split at the disjunctions to create a DisjunctNode
--createParentNode

splitWhereAux: List a -> (a -> Bool) -> List (List a) -> List a -> List (List a)
splitWhereAux input splitter res acc =
  case input of
    [] -> List.reverse <| (List.reverse acc)::res
    head::tail ->
        if splitter head then
          splitWhereAux tail splitter
            ((List.reverse acc)::res) []
        else
          splitWhereAux tail splitter
            res (head::acc)

splitWhere: List a -> (a -> Bool) -> List (List a)
splitWhere = \input splitter -> splitWhereAux input splitter [] []

createParentNode: List Node -> NodeType
createParentNode children =
  let disjuncts = splitWhere children <| \c -> c.nodeType == RegexLeaf "|" in
  if List.length disjuncts == 1 then
    ParentNode children
  else
    DisjunctNode (List.map nodeApplyP disjuncts)

isBackReference: String -> Bool
isBackReference r =
  String.length r >= 2 && (
    case String.uncons r of
      Just (head, tail) -> head == '\\' && String.all Char.isDigit tail
      Nothing -> False
  )

backReferenceLeafUnapply: NodeType -> Maybe Int
backReferenceLeafUnapply leaf =
  case leaf of
    RegexLeaf r ->
      if isBackReference r then
        case String.uncons r of
          Nothing -> Nothing
          Just (_, tail) -> Result.toMaybe <| String.toInt tail
      else Nothing
    _ -> Nothing

nodeApply originalGroup nodeType = Node originalGroup nodeType 0 (Just "") 0
nodeApplyR regex = Node (OriginallyWrapped "" "") (RegexLeaf regex) 0 (Just "") 0
nodeApplyP nodes = Node (OriginallyWrapped "" "") (createParentNode nodes) 0 (Just "") 0
nodeDisjunct nodes = Node (OriginallyWrapped "" "") (DisjunctNode nodes) 0 (Just "") 0
nodeUnapply: Node -> (OriginalRegex, NodeType)
nodeUnapply n = (n.originalGroup, n.nodeType)

{-/** @param originalGroup Either the 1-based index of the original group that
           this node encloses, or the modifiers which are present before and
           after (ex: ?, *?, (?=...), (?!..))
  * @param nodeType The type of the node (Regexleaf, or ParentNode)
  *        Is a var because we need to shift the backreference groups
  */-}
type alias Node = { originalGroup: OriginalRegex, nodeType: NodeType, newGroup: Int, matched: Maybe String, start: Int }
  --var newGroup: Int = 0    // Assigned later after the tree of nodes is built
  --var matched: String = "" // Assigned later after the new regexp matches
  --var start = 0            // Assigned later after recovering the tree matches

foreach: (a -> b) -> List a -> c -> c
foreach f l return =
  case l of
    [] -> return
    head::tail -> let _ = f head in foreach f tail return

transformGroupNumber: (Dict Int Int) -> Node -> Node
transformGroupNumber mapping this =
  case backReferenceLeafUnapply this.nodeType of
    Just reference ->
      mutateRecordField this "nodeType" (RegexLeaf <| "\\" ++ (toString <| Maybe.withDefault 0 (Dict.get reference mapping)))
    Nothing ->
      case this.nodeType of
        RegexLeaf regex -> this
        ParentNode children ->
          foreach (transformGroupNumber mapping) children this
        DisjunctNode(children) ->
          foreach (transformGroupNumber mapping) children this

  --// Assigns consecutive group numbers starting from newGroupIndex to the nodes in this subtree, in a pre-order walk.
  --// Returns 1 plus the largest assigned group number.
setNewGroup: Node -> Int -> Int
setNewGroup this index =
  let _ = mutateRecordField this "newGroup" index in
  foldNodeType this.nodeType
    (\r -> index + 1)
    (\children -> List.foldl setNewGroup (index + 1) children)
    (\children -> List.foldl setNewGroup (index + 1) children)

  --// Recursively matched groups from a match of this regular expression
  --// to the field matched of each node.
  --// If a group did not match, stringForGroupNumber can return null
  --// This is consistent with JVM regexs
setMatch: (Int -> Maybe String) -> Node -> ()
setMatch stringForGroupNumber this =
  let _ = mutateRecordField this "matched" (stringForGroupNumber this.newGroup) in
  foldNodeType this.nodeType
    (\r -> ())
    (\children -> foreach (setMatch stringForGroupNumber) children ())
    (\children -> foreach (setMatch stringForGroupNumber) children ())

{-  /*
  When assigning group positions. I could not choose between assigning group
  numbers from left to right or from right to left, because there both failed
  in one case each. Normally, both directions give the same result. But there
  are corner cases.
  Consider the following regex matching `abbbbbbc`

      (?=ab*(c))ab

  After conversion, this becomes:

      (?=(ab*)(c))(ab)

  To recover the position of the group (c), we cannot do anything but compute
  it from the length of (ab*), that is, propagate the start, compute the
  length, and return the end, and this, recursively. This is what we need to
  do in a forward-matching regex.

  However, consider the following regex matching `abcbdbe`

      a(b.)*

  After conversion, it is transformed to:

      (a)((b.)*)

  The semantics of group matching under a star are that the last match is
  kept. Therefore, we cannot obtain the start position of (b.) from propaga-
  ting lengths from left to right. What we first do is to get the start, then
  the end, of the group `((b.)*)`, and then we propagate this end to the
  inner group.

  Note that when javascript will support back-matching `(?<= )` and `(?<! )`
  (hopefully one day), we will be able to implement the length transfer
  using the `setEndReturnStart` method, because we have no clue on where the
  match started (this is different from the `start` position because it can
  extend before it)

  `Absolute()` means a marker of the type `(?= )` or `(?! )`, they do not
  count to propagate the length on the right or on the left.

  `Repeater()` designates all the repeat-transformer which have more or less
  the semantics of *. Every group having a repeater uses teh semantics of
  propagating the end to the start.
  */-}

--  // Propagates the start position of this node to its descendants
propagateStart: Node -> ()
propagateStart this =
  let _ = foldNodeType this.nodeType
        (\regex -> 0)
        (\children -> List.foldl setStartReturnEnd this.start children)
        (\children -> foreach (\child -> setStartReturnEnd child this.start) children 0)
  in ()

--// Propagates the end position of this node to its descendants
propagateEnd: Node -> ()
propagateEnd this =
  let _ = foldNodeType this.nodeType
        (\regex -> 0)
        (\children -> List.foldr setEndReturnStart (end this) children)
        (\children -> foreach (\child -> setEndReturnStart child (end this)) children 0)
  in ()

setEndReturnStart: Node -> Int -> Int
setEndReturnStart this newEnd =
  let _ = mutateRecordField this "start" (
    if originallyWrapped.absoluteUnapply this.originalGroup
    then newEnd
    else case this.matched of
      Nothing -> -1
      Just x -> newEnd - String.length x
    )
  in
  let _ = propagateStart this in
  this.start

setStartReturnEnd: Node -> Int -> Int
setStartReturnEnd this offset =
    let _ = mutateRecordField this "start" (case this.matched of
      Nothing -> -1
      Just x -> offset) in
    let _ = case originallyWrapped.repeaterUnapply this.originalGroup of
      Just _ -> propagateEnd this
      Nothing -> propagateStart this
    in
    if originallyWrapped.absoluteUnapply this.originalGroup then
      offset
    else end this

end: Node -> Int
end this = this.start + (case this.matched of
  Nothing -> 0
  Just x -> String.length x)


buildRegex: Node -> String
buildRegex this =
  let leftRegex = foldOriginalRegex this.originalGroup
        (\groupNum -> "(")
        (\left right -> "((?:" ++ left)
  in
  let middleRegex = foldNodeType this.nodeType
        (\regex -> regex)
        (\children -> "(?:" ++ String.concat (List.map buildRegex children) ++ ")")
        (\children -> "(?:" ++ String.concat (List.intersperse "|" (List.map buildRegex children))++ ")")
  in
  let rightRegex = foldOriginalRegex this.originalGroup
        (\groupNum -> ")")
        (\left right -> ")" ++ right ++ ")")
  in
  leftRegex ++ middleRegex ++ rightRegex

getGroupNodeMap: Node -> Dict Int Node
getGroupNodeMap this =
  let thisGroupNodeMap = foldOriginalRegex this.originalGroup
        (\groupNum -> Dict.singleton groupNum this)
        (\_ _ -> Dict.empty)
  in
  let childGroupNodeMap = foldNodeType this.nodeType
        (\regex -> Dict.empty)
        (\children -> List.foldl (\child mapping -> Dict.union (getGroupNodeMap child) mapping ) Dict.empty children)
        (\children -> List.foldl (\child mapping -> Dict.union (getGroupNodeMap child) mapping ) Dict.empty children)
  in
  Dict.union childGroupNodeMap thisGroupNodeMap

simplifyNode: Node -> Node
simplifyNode this =
  case nodeUnapply this of
    (OriginallyGroupped nextGroupIndex,
          ParentNode [n]) ->
      case nodeUnapply n of
        (OriginallyWrapped "" "", RegexLeaf regex) ->
          nodeApply (OriginallyGroupped nextGroupIndex) (RegexLeaf regex)
        _ -> this
    _ -> this

type alias GroupStartMap = {string: String, start: Int, pattern: String}

find: HowMany -> String -> String -> List Match
find howMany reg string =
  let node = parseRegex reg in
  --let _ = Debug.log "computed node" node in
  let _ = setNewGroup node 1 in
  let groupNodeMap = getGroupNodeMap node in
  let _ = transformGroupNumber (Dict.map (\k v -> v.newGroup) groupNodeMap) node in
  let allMatchingRegexStr = buildRegex node in
  --let _ = Debug.log ("previous:" ++ reg) () in
  --let _ = Debug.log ("allmatchingRegex:" ++ allMatchingRegexStr) () in
  let allMatchingRegex = regex allMatchingRegexStr in
  let findResult = Regex.find howMany allMatchingRegex string in
  --let _ = Debug.log "Mapping: " groupNodeMap in
  List.map (\match ->
    let _ =  setMatch(\x -> case List.drop (x-1) match.submatches |> List.take 1 of
            [submatch] -> submatch
            e -> Debug.crash <| "[internal error]" ++ toString x ++ " Index ouf of bounds" ++ toString e
          ) node in
    let _ = setStartReturnEnd node match.index in
    --let _ = Debug.log "Mapping after first match: " groupNodeMap in
    let matchesWithStart = Dict.toList groupNodeMap
      |> List.sortBy Tuple.first
      |> List.map (\(k, node) -> {match = node.matched, start = node.start})
    in
    Match match.match
      matchesWithStart
      match.index
      match.number
  ) findResult

replace: HowMany -> String -> (Match -> String) -> String -> String
replace howMany reg replacementFun string =
    let node = parseRegex reg in
    let _ = setNewGroup node 1 in
    let groupNodeMap = getGroupNodeMap node in
    let _ = transformGroupNumber (Dict.map (\k v -> v.newGroup) groupNodeMap) node in
    let allMatchingRegexStr = buildRegex node in
    let allMatchingRegex = regex allMatchingRegexStr in
    let f match =
      let _ =  setMatch(\x -> case List.drop (x-1) match.submatches |> List.take 1 of
             [submatch] -> submatch
             e -> Debug.crash <| "[internal error]" ++ toString x ++ " Index ouf of bounds" ++ toString e
           ) node in
      let _ = setStartReturnEnd node match.index in
      let g = Dict.map (\k v -> v.start) groupNodeMap in
      let matchesWithStart = Dict.toList groupNodeMap
        |> List.sortBy Tuple.first
        |> List.map (\(k, node) -> {match = node.matched, start = node.start})
      in
      let m =  Match match.match
            matchesWithStart
            match.index
            match.number
      in replacementFun m
    in
    Regex.replace howMany allMatchingRegex f string

{--mapping: GroupStartMap -> (Int -> Int)
mapping this =
  if False {--JVM--} then
    let m = this.pattern.matcher(string) in
    m.find(start)
    let groupStart = (1 to m.groupCount()) map (i => (i, m.start(i))) in
    let origStart = 0 -> m.start() in
    (groupStart :+ origStart).toMap
  else
    let node = parseRegex(this.pattern.pattern()) in
    let flags = {
      "g" +
        (if (((pattern.flags() >> 1) & 1) == 1) "i" else "") +
        (if (((pattern.flags() >> 3) & 1) == 1) "m" else "")
    } in
    node.setNewGroup(1)
    let groupNodeMap = node.getGroupNodeMap in
    node.transformGroupNumber(groupNodeMap.mapValues(_.newGroup))
    let allMatchingRegexStr = node.buildRegex in
    let allMatchingRegex = new js.RegExp(allMatchingRegexStr, flags) in
    allMatchingRegex.lastIndex = start
    let allMatchResult = allMatchingRegex.exec(string) in
    if (allMatchResult == null) {
      throw new Exception(s"[Internal error] Executed '$allMatchingRegex' on " +
        s"'$string' at position $start, got an error.\n" +
        s"Original pattern '${pattern}' did match however.")
    }
    node.setMatch((x: Int) => allMatchResult(x).getOrElse(null))
    node.setStartReturnEnd(start)
    groupNodeMap.mapValues(_.start)
  }--}

{-
// Wraps every consecutive chars and simple expressions in the regexp in a group to find the intermediate positions.
// Same for subgroups.
// Input example:  (?:AB)C
// Output example: A node roughly representing (?:(AB))(?:(C))
// Input example:  (A(B))C(D)
// Output example: A node roughly representing (((A)(B))(C)(D))
-}
parseRegex: String -> Node
parseRegex pattern =
  let parseClosingParenthesis pIndex = pIndex + 1 in
  -- Returns the position just after the next ending brace
  let positionEndNextBrace pIndex =
    if stringCharAt pIndex pattern == Just "}" then pIndex + 1
    else if String.length pattern <= pIndex then pIndex
    else positionEndNextBrace (pIndex + 1)
  in
  -- Returns the position just after the next ending square bracket
  let positionEndSquareBracket pIndex =
     --let _ = Debug.log ("Position at " ++ toString pIndex) (stringCharAt pIndex pattern |> Maybe.map (String.fromChar) ) in
     --let _ = Debug.log (" = ']' ") ((stringCharAt pIndex pattern |> Maybe.map (String.fromChar)) == Just "]") in
     if String.length pattern <= pIndex then
       pIndex
     else if stringCharAt pIndex pattern == Just "\\" && String.length pattern > 1  then
       positionEndSquareBracket (pIndex + 2)
     else if stringCharAt pIndex pattern == Just "]" then
       pIndex + 1
     else
       positionEndSquareBracket (pIndex + 1)
  in
  let positionAfterLastDigit pIndex =
    if pIndex < String.length pattern && (String.all Char.isDigit (Maybe.withDefault "c" (stringCharAt pIndex pattern))) then
       positionAfterLastDigit (pIndex + 1)
    else
       pIndex
  in
  -- Returns a sequence of nodes, the remaining non-parsed, and the next group index
  -- Take care of escaped parentheses \( and \)
  -- Takes care of non-group parenthesis (?:)
  let parse pIndex nextGroupIndex = -- Int -> Int ->  (List Node, Int, Int)
    --let _ = Debug.log "parse" (pIndex, nextGroupIndex) in
    if (pIndex >= String.length pattern) then
       ([], pIndex, nextGroupIndex)
    else
     let simplify nodes = {-: List Node -> List Node}-}
       case nodes of
          n :: n2 :: tail ->
           case (nodeUnapply n, nodeUnapply n2) of
             ((OriginallyWrapped "" "", RegexLeaf(r1)),
              (OriginallyWrapped "" "", RegexLeaf(r2))) ->
                if not (isBackReference r1) && r1 /= "|" &&
                   not (isBackReference r2) && r2 /= "|" then
                  simplify(nodeApplyR (r1 ++ r2) :: tail)
                else
                  nodes
             ((nGroupped, m), (originallyGroupped, RegexLeaf "")) ->
               case originallyWrapped.repeaterUnapply originallyGroupped of
                 Just modifier ->
                   case nGroupped of
                     OriginallyWrapped left right ->
                       simplify (nodeApply (OriginallyWrapped left (right ++ modifier)) m :: tail)
                     OriginallyGroupped _ ->
                       simplify (nodeApply (OriginallyWrapped "" modifier) (ParentNode [n]) :: tail)
                 Nothing -> nodes
             _ -> nodes
          _ -> nodes
     in
     let addSiblings node {-: Node-} remaining {-: Int-} nextGroupIndex{-: Int-} = ---> (List Node, Int, Int) = {
       let (siblings, finalRemaining, finalGroupIndex) = parse remaining nextGroupIndex in
       (simplify (node :: siblings), finalRemaining, finalGroupIndex)
     in
     let default e =
       addSiblings (nodeApplyR <| e) (pIndex + 1) nextGroupIndex
     in
     case (stringCharAt pIndex pattern) of
       Nothing -> Debug.crash "Internal error: should have not reached the end of regex"
       Just c ->
         case c of
           "(" ->
             if (String.length pattern >= pIndex + 3 && stringCharAt (pIndex + 1) pattern == Just "?" &&
                 (stringCharAt (pIndex+2) pattern == Just "=" || stringCharAt (pIndex+2) pattern == Just "!")) then -- // Non-capturing test group
               let (parsed, remaining, newNextGroupIndex) = parse (pIndex + 3) nextGroupIndex in
               let remaining1 = parseClosingParenthesis remaining in
               addSiblings (nodeApply (OriginallyWrapped ("(?"++Maybe.withDefault "" (stringCharAt (pIndex + 2) pattern)) ")")
                                (createParentNode parsed))
                           remaining1 newNextGroupIndex
             else if (String.length pattern < pIndex + 3 || stringCharAt (pIndex + 1) pattern /= Just "?" ||
                        stringCharAt (pIndex + 2) pattern /= Just ":") then --// Capturing group.
               --let _ = Debug.log "capturing group" () in
               let (parsed, remaining, newNextGroupIndex) = parse (pIndex + 1) (nextGroupIndex + 1) in
               --let _ = Debug.log "parsed" (parsed, remaining, newNextGroupIndex) in
               let remaining1 = parseClosingParenthesis remaining in
               addSiblings (simplifyNode <| nodeApply (OriginallyGroupped nextGroupIndex) (createParentNode parsed))
                           remaining1 newNextGroupIndex
             else if (String.length pattern >= pIndex + 3 && stringCharAt (pIndex + 1) pattern == Just "?" &&
                        stringCharAt (pIndex + 2) pattern == Just ":") then -- Non-capturing group
               let (parsedNodes, remaining, newNextGroupIndex) = parse (pIndex + 3) nextGroupIndex in
               let remaining1 = parseClosingParenthesis remaining in
               addSiblings (simplifyNode (nodeApplyP parsedNodes)) remaining1 newNextGroupIndex
             else default c -- Should not happen.

           ")" -> ([], pIndex, nextGroupIndex)

           "\\" ->
             if (String.length pattern >= pIndex + 2) then
               let nextIndex = if (stringCharAt (pIndex + 1) pattern |> Maybe.map (String.all Char.isDigit) |> Maybe.withDefault False) then positionAfterLastDigit (pIndex + 1) else pIndex + 2 in
               let regexPart = String.slice pIndex nextIndex pattern in
               addSiblings (nodeApplyR regexPart) nextIndex nextGroupIndex
             else default c -- No escaped char, but this should not be called

           "+" -> -- greedy or not greedy
             let nextIndex = if (String.length pattern >= pIndex + 2 && stringCharAt (pIndex + 1) pattern == Just "?") then pIndex + 2 else pIndex + 1 in
             let repeater = String.slice pIndex nextIndex pattern in
             addSiblings (nodeApply (OriginallyWrapped "" repeater) (RegexLeaf "")) nextIndex nextGroupIndex

           "*" -> -- greedy or not greedy
             let nextIndex = if (String.length pattern >= pIndex + 2 && stringCharAt (pIndex + 1) pattern == Just "?") then pIndex + 2 else pIndex + 1 in
             let repeater = String.slice pIndex nextIndex pattern in
             addSiblings (nodeApply (OriginallyWrapped "" repeater) (RegexLeaf "")) nextIndex nextGroupIndex

           "?" -> -- greedy or not greedy
             let nextIndex = if (String.length pattern >= pIndex + 2 && stringCharAt (pIndex + 1) pattern == Just "?") then pIndex + 2 else pIndex + 1 in
             let repeater = String.slice pIndex nextIndex pattern in
             addSiblings (nodeApply (OriginallyWrapped "" repeater) (RegexLeaf "")) nextIndex nextGroupIndex

           "{" -> -- parse until end of occurrence
             let nextIndex = positionEndNextBrace (pIndex + 1) in
             let repeater = String.slice pIndex nextIndex pattern in
             addSiblings (nodeApply (OriginallyWrapped "" repeater) (RegexLeaf "")) nextIndex nextGroupIndex

           "[" ->
             --let _ = Debug.log "end position of square bracket" () in
             let remaining = positionEndSquareBracket (pIndex + 1) in
             --let _ = Debug.log "remaining" (remaining) in
             let inside =  String.slice pIndex remaining pattern in
             addSiblings (nodeApplyR inside) remaining nextGroupIndex

           _ -> default c
  in
  case parse 0 1 of
     ([], _, _) -> nodeApplyR ""
     ([n], _, _) -> n
     (children, _, _) -> nodeApplyP children



