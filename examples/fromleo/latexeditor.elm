latex = """\newcommand{\small}{mini}

\section{\LaTeX{} editing in \textsc{Html}\label{sec:introduction}}
This \small{} \LaTeX{} editor is \textit{bidirectional} and supports \small{} \textbf{textual} changes. Rename '\small{}' to 'lightweight' to see\ldots

\section{Reference update\label{sec:commands}}
References are supported:
Section \ref{sec:introduction}.
Change the previous number to 2 or 2.1 and see how it updates the source code.
\subsection{Others\label{others}}
Only frac, exponent and indices in math mode: $\frac{b^2-4ac}{2}$.
%TODO support more commands."""

-- The LaTeX tokenizer, parsers, interpreter, and linker.  
tokenize txt pos = 
  case String.uncons txt of
  Nothing -> [{tag="EOF", pos = pos, origText = txt}]
  Just (first, rem) ->
    case first of
      "\\" -> case String.uncons rem of
        Just ("\\", rem) ->
          case extractFirstIn """(\\\\)([\s\S]*)""" txt of
            Just [bs, rem] ->
              [{tag="newline", pos=pos, origText = bs}, rem, pos + 2]
        _ ->
          case extractFirstIn """^((\\\w+\b|\\.)\s*)([\s\S]*)""" txt of
            Just [commandspace, command, remainder] ->
              [{tag="command", name=command, pos=pos, origText = commandspace}, remainder, pos + String.length commandspace]
            _ ->
              [{tag="error", pos=pos, value="Expected a command after \\, got " + txt}, rem, pos + 1]
      "%" ->
        case extractFirstIn "^(%(.*(?:\r?\n|$)))([\\s\\S]*)" txt of
          Just [percentcomment, comment, remainder] ->
            [{tag="linecomment", value=comment, pos=pos, origText=percentcomment}, remainder, pos + String.length percentcomment]  
      "{" ->
        [{tag="open", pos=pos, origText=first}, rem, pos + 1]
      "}" ->
        [{tag="close", pos=pos, origText=first}, rem, pos + 1]
      "$" ->
        [{tag="equationdelimiter", pos=pos, origText=first}, rem, pos + 1]
      "#" ->
        case extractFirstIn """^(#(\d))([\s\S]*)""" txt of
          Just [original, integer, rem] ->
            [{tag="replacement", pos = pos, nth = String.toInt integer, origText = original}, rem, pos + 2]
          _ -> 
            [{tag="error", pos = pos + 1, value="Expected number after #"}, rem, pos + 1]
      "^" ->
        [{tag="command", name="^", pos=pos, origText=first}, rem, pos + 1]
      "_" ->
        [{tag="command", name="_", pos=pos, origText=first}, rem, pos + 1]
      _ -> case extractFirstIn """^(\r?\n\r?\n\s*)([\s\S]*)""" txt of
        Just [rawspace, remainder] ->
          [{tag="newpar", origText = rawspace}, remainder, pos + String.length rawspace]
        _ ->
          case extractFirstIn """^((?:(?!\r?\n\r?\n)[^\\\{\}\$%\^_#])+)([\s\S]*)""" txt of
            Just [rawtext, remainder] ->
              [{tag="rawtext", value=rawtext, pos = pos, origText = rawtext}, remainder, pos + String.length rawtext]
            res ->
              [{tag="error", pos = pos, value="Expected text, got " + txt}]

tokens txt =
  let aux txt revAcc pos =
    case tokenize txt pos of
      [{tag="EOF"} as t] -> reverse (t::revAcc)
      [{tag="error", pos = pos, value = value}] -> value + " at pos " + pos + ":" + txt
      [t, rem, newPos] ->
        let newAcc = t::revAcc in
        aux rem newAcc newPos
  in
  aux txt [] 0

parse tokens = -- Group blocks together.
  let aux revAcc tokens =
    case tokens of
      [] -> [List.reverse revAcc, tokens]
      [{tag="EOF"}] -> [List.reverse revAcc, []]
      ({tag="close"} :: rem) -> 
        [List.reverse revAcc, tokens]
      (({tag="open"} as x) :: rem) ->
        case aux [] rem of
          [res, rem] ->
            case rem of
              {tag="close"}::r2 ->
                let newrevAcc = {tag="block", children=res}::revAcc in
                aux newrevAcc r2
              _ ->
                [{tag="error", value="Unclosed { at " + x.pos }, []]
          x -> x
      (x :: rem) ->
        let newrevAcc = x :: revAcc in
        aux newrevAcc rem
  in nth (aux [] tokens) 0

incSectionCounter opts = 
  let newCounter = opts.sectionCounter  + freeze 1 in
  { opts |
    sectionCounter = newCounter
    subsectionCounter = 0
    currentLabelName = toString newCounter
  }

incSubsectionCounter opts = 
  let newCounter = opts.subsectionCounter + freeze 1 in
  { opts |
    subsectionCounter = newCounter
    currentLabelName = toString (opts.sectionCounter) + "." + toString newCounter
  }
 
htmlError help display = Html.span [["color", "red"]] [["title", help]] [
  case display of
    [] -> display
    head::tail -> display
    d -> ["TEXT", d]
  ]

htmlWrapper htmlArgsWrapper =
    { arity = 1
      toHtml toHtml opts args =
      let [argsHtml, newOpts] = toHtml opts args in
      [htmlArgsWrapper argsHtml, newOpts]}

htmlConst html =
    { arity = 0
      toHtml toHtml opts args = [html, opts] }

newcommandinstantiate args parsed =
  let aux parsed = case parsed of
    {tag="block", children=c} ->
      { parsed | children = List.map aux c }
    {tag="replacement", nth=n} ->
      if n <= len args then
        nth args (n - 1)
      else
        parsed
      aux head :: aux tail
    _ -> parsed
  in aux parsed

-- commandsDict: Dict String { arity: Int, toHTML: (Options -> Lists Tree -> HTMLNode) -> Options -> Trees -> (HTMLNode, Options)}
commandsDict = Dict.fromList [
  ("\\label",
    { arity = 1
    , toHtml toHtml opts args = case args of
      [{tag = "block", children = [{tag = "rawtext", value = v}]}] ->
        [["span", [["id", v],["class", "labellink"], ["title", v]], [["TEXT", "🔗"]]],
          let currentLabelName = opts.currentLabelName in
          { opts |
             labelToName = opts.labelToName |>
               Dict.insert v currentLabelName,
             nameToLabel = opts.nameToLabel |>
               Dict.insert currentLabelName v
          }
        ]
      _ ->
        let [argHtml, newOpts] = toHtml opts args in
        [htmlError "\\label must be followed by a {name}" argHtml, newOpts]
    }),
  ("\\ref",
    { arity = 1
    , toHtml toHtml opts args = case args of
      [{tag = "block", children = [{tag = "rawtext", value = v}]}] ->
        [["ref", v], opts]
      _ -> 
        let [argHtml, newOpts] = toHtml opts args in
        [htmlError "\\label must be followed by a {name}" argHtml, newOpts]
    }),
  ("\\LaTeX",
    { arity = 0
    , toHtml toHtml opts arg =
      [["span", [["class", "latex"]], html """L<sup>a</sup>T<sub>e</sub>X"""], opts]
    }),
  ("\\section", 
    { arity= 1
    , toHtml toHtml opts arg =
      let newOpts = { incSectionCounter opts | indent = False, newline = False } in
      let [argHtml, newOpts2] = toHtml newOpts arg in
      [["h1", [], [["TEXT", newOpts2.currentLabelName + ". "]] ++ argHtml],
        {newOpts2 | indent = True }]
    }),
  ("\\subsection", 
    { arity= 1
    , toHtml toHtml opts arg =
      let newOpts = { incSubsectionCounter opts | indent = False, newline = False } in
      let [argHtml, newOpts2] = toHtml newOpts arg in
      [["h2", [], [["TEXT", newOpts2.currentLabelName + ". "]] ++ argHtml],
        {newOpts2 | indent = True }]
    }),
  ("\\textbf", htmlWrapper (\argsHtml -> ["b", [], argsHtml])),
  ("\\textit", htmlWrapper (\argsHtml -> ["i", [], argsHtml])),
  ("\\textsc", htmlWrapper (\argsHtml -> ["span", [["style", [["font-variant", "small-caps"]]]], argsHtml])),
  ("\\ldots", htmlConst ["span", [], [["TEXT", "…"]]]),
  ("\\textbackslash", htmlConst ["span", [], [["TEXT", "\\"]]]),
  ("\\newcommand",
    let extractCommand block = case block of
      {tag="command", name=cmdName} -> {value = cmdName}
      {tag="block", children=[{tag="command", name=cmdName}]} -> {value = cmdName}
      _ -> {}
    in
    { inspect rightArgs = -- Returns the arguments to the command and the remaining that it does not parse.
        case rightArgs of
          cmdOpt::rem ->
            case extractCommand cmdOpt of
              {value= cmdName} -> case rem of
                ({tag="rawtext", value=text} :: definition :: rem) ->
                  case extractFirstIn """\[(\d+)\]""" text of
                    Just [d] -> [[cmdName, String.toInt d, definition], rem]
                    _ -> [["Expected [number] for the number of arguments, got " + text], rightArgs]
                (definition :: rem) ->
                  [[cmdName, 0, definition], rem]
                _ -> [["Expected \\newcommand{"+cmdName+"}[optional num args]{definition}"], rightArgs]
              _ ->  [["No command name after \\newcommand, from " + Debug.log "" cmdOpt], rightArgs]
          _ ->  [["Expacted a command name after \\newcommand, from " + Debug.log "" cmdOpt], rightArgs]
    , toHtml toHtml opts args =
        if len args == 1 then [htmlError (nth args 0) "???", opts] else (
        let [cmdName, arity, definition] = args in
        let newOpts = { opts |
          customCommands = opts.customCommands |> Dict.insert cmdName {
              arity = arity,
              toHtml toHtml opts args =
                -- Perform the replacement of #1 with argument 1, #2 with argument 2, and so on.
                -- For now, just output the definition.
                let instantiatedDefinition = newcommandinstantiate args definition in
                case toHtml opts [instantiatedDefinition] of
                  [[argHtml], newOpts] ->
                    [argHtml, newOpts]
                  [argsHtml, newOpts] -> error <| "command " + cmdName + " returned more than 1 arg:" + toString argsHtml
            } }
        in
        [["span", [["class", "newcommand"]], []], newOpts]
      )
    }),
  ("\\frac",
  { arity = 2
  , toHtml toHtml opts arg =
      if opts.mathmode then
        let [arg1html, newOpts1] = toHtml opts     [nth arg 0] in
        let [arg2html, newOpts2] = toHtml newOpts1 [nth arg 1] in
        [["div", [["class", "fraction"]], [
         ["span", [["class", "fup"]], arg1html],
         ["span", [["class", "bar"]], [["TEXT", "/"]]],
         ["span", [["class", "fdn"]], arg2html]]], newOpts2]
      else
        [htmlError "\\frac allowed only in math mode" "???", opts]
  }),
  ("\\_", htmlConst ["span", [], [["TEXT", "_"]]]),
  ("_",
  { arity = 1
  , toHtml toHtml opts args =
    if opts.mathmode then
      let [arghtml, newOpts] = toHtml opts args in
      [["sub", [], arghtml], newOpts]
    else
      [htmlError "_ allowed only in math mode" "???", opts]
  }),
  ("^",
  { arity = 1
  , toHtml toHtml opts args =
    if opts.mathmode then
      let [arghtml, newOpts] = toHtml opts args in
      [["sup", [], arghtml], newOpts]
    else
      [htmlError "^ allowed only in math mode" "???", opts]
  })]

commands x =
  Dict.get x commandsDict |> Maybe.withDefaultLazy (\_ ->
    { arity = 0
    , toHtml toHtml opts arg =    
      [htmlError "Unknown Command" x, opts]})

indent opts = if opts.indent then [["span", [["class", "paraindent"]], html "&nbsp;"]] else []

newline opts = if opts.newline then [Html.br] else []

splitargs n array =
  let aux revAcc n array =
    if n == 0 then [reverse revAcc, array] else
    case array of
      {tag="rawtext", value=text, pos = pos}:: rem ->
        case extractFirstIn """^\s*(\S)(.*)""" text of
          Just [arg, other] ->
            let newAcc = {tag="rawtext", value=arg, pos = pos}::revAcc in
            let newN = n - 1 in
            let newArray = {tag="rawtext", value=other, pos = pos + String.length arg} :: rem in
            aux newAcc newN newArray
          _ ->
            aux revAcc n rem
      (head :: rem) ->
        let newAcc = head::revAcc in
        let newN = n - 1 in
        aux newAcc newN rem
      [] ->
        [[], array]
  in aux [] n array

escape txt = txt |>
  replaceAllIn "\\\\" "\\textbackslash{}" |>
  replaceAllIn "%(\\w+) (\\w+)" (\{group=[_, a, b]} -> "\\" + a + "{" + b  + "}") |>
  replaceAllIn "<[bB]>" (\_ -> "\\textbf{") |>
  replaceAllIn "<[iI]>" (\_ -> "\\textit{") |>
  replaceAllIn "</[bBiI]>" (\_ -> "}")

toHtmlWithoutRefs opts tree =
  let aux opts revAcc tree = case tree of
    [] -> [List.reverse revAcc, opts]
    (head::rem) -> case head of
      {tag="block", children} ->
        let newTree = children ++ rem in
        aux opts revAcc newTree
      {tag="rawtext", value=text, pos = pos} ->
        let finalText = {
           apply x = x,
           update {input, oldOutput, newOutput, diffs} = 
             Ok (Inputs [Update.mapInserted escape newOutput diffs])
          }.apply text in
        if opts.indent && Regex.matchIn """^[\s]*\S""" text then
          let newOpts = { opts | indent = False,  newline = False } in
          let revAccWithParagraph = List.reverseInsert (newline opts ++ indent opts) revAcc in
          let newrevAcc = ["span", [["start", toString pos]], [["TEXT",finalText]]]::revAccWithParagraph in
          aux newOpts newrevAcc rem
        else
          let newrevAcc = ["span", [["start", toString pos]], [["TEXT",finalText]]]::revAcc in
          aux opts newrevAcc rem
      {tag="newpar"} ->
        let newOpts = { opts | indent = True, newline = True} in
        aux newOpts revAcc rem
      {tag="equationdelimiter"} -> -- Todo: Group the equation into an inline span?
        let newOpts = { opts | mathmode = if opts.mathmode then False else True } in
        aux newOpts revAcc rem
      {tag="replacement", nth=n} -> -- Create a dummy tag that can be later replaced.
        let newrevAcc = ["span", [["class", "replacement"]], [n]]::revAcc in
        aux opts newrevAcc rem
      {tag="command", name=cmdname} ->
        let tmpOpt = if cmdname == """\noindent""" then { opts | indent = False } else opts in
        -- TODO: Need to not convert to html, but expand the command first.
        let cmddef = Dict.get cmdname tmpOpt.customCommands |> Maybe.withDefaultLazy (\_ ->
          commands cmdname) in
        let [args, remainder] = case cmddef of
          {arity=n} ->
            splitargs n rem
          {inspect} ->
            inspect rem
        in
        let [toAdd, newOpts] = cmddef.toHtml toHtmlWithoutRefs tmpOpt args in
        let newrevAcc = toAdd::revAcc in
        aux newOpts newrevAcc remainder
      
      {tag="linecomment", value} ->
        let newrevAcc = ["span", [["style", [["background","#888"], ["color", "#FFF"]]]], [["TEXT", "(" + value + ")"]]]::revAcc in
        aux opts newrevAcc rem
  in 
  aux opts [] tree

initOptions = {
  indent = False
  newline = False
  customCommands = Dict.fromList []
  currentLabelName = freeze "0"
  sectionCounter = freeze 0
  subsectionCounter = freeze 0
  mathmode = False
  labelToName = Dict.fromList []
  nameToLabel = Dict.fromList []
}

htmlMapOf htmlOf trees = case trees of
  [] -> ""
  (head::tail) -> htmlOf head + htmlMapOf htmlOf tail

htmlOf text_tree = case text_tree of
  ["TEXT", value] -> -- Needs some escape here.
    value 
  [m, _, children] -> "<"+m+">" + htmlMapOf htmlOf children + "</"+m+">"

toHtml x =
  let [raw, opts] = toHtmlWithoutRefs initOptions x in
  let replaceMap replaceReferences trees = case trees of
    [] -> freeze []
    head :: tail -> {
        apply x = x
        update {input, outputNew, outputOriginal, diffs} =
          if (len outputNew /= len outputOriginal && len outputOriginal == 1) then
            Ok (Inputs [[["TEXT", htmlMapOf htmlOf outputNew outputNew]]]) else Ok (InputsWithDiffs [(outputNew, Just diffs)])
      }.apply [replaceReferences head] ++ replaceMap replaceReferences tail
  in
  let replaceReferences tree = case tree of
    ["ref", refname] -> Dict.get refname opts.labelToName  |> case of
      Nothing -> htmlError ("Reference " + refname + " not found.") "???"
      Just txt ->
        let replaceKey refNameTxt = {
           apply (refname,txt) = txt,
           update {input=(oldRefname, oldTxt), outputNew=newText} =  -- Lookup for the reference in the options.
             case Dict.get newText opts.nameToLabel of
              Just newRefname ->
                Ok (Inputs [(newRefname, oldTxt)])
              _ -> -- No solution, cancel update.
                Err ("could not find reference" + toString newText)
          }.apply refNameTxt
        in
        ["span", [
          ["class","reference"],
          ["onclick", "if(window.location.hash=='') window.location.hash = '" + refname + "';"],
          ["title", refname]
          ], [["TEXT", replaceKey (refname, txt)]]]
    [tag, attrs, c] -> [tag, attrs, replaceMap replaceReferences c]
    _ -> tree
  in
  replaceMap replaceReferences raw

latex2html latex = Update.lens2
  { apply (f, latex) = f latex,
    update {input = (f, latex), outputOld, outputNew, diffs = (VListDiffs ldiffs) as diffs} = 
      let gatherDiffsChild gatherDiffs cOld cNew childDiffs = case childDiffs of
        [] -> Ok [[]]
        ListElemSkip count :: diffTail ->
          gatherDiffsChild gatherDiffs (LensLess.List.drop count cOld) (LensLess.List.drop count cNew) diffTail
        ListElemUpdate d :: diffTail ->
          case (cOld, cNew) of
            (oldHead::oldTail, newHead::newTail) ->
              let subdiffs = gatherDiffs oldHead newHead d in
              subdiffs |> LensLess.Results.andThen (\replacements ->
                gatherDiffsChild gatherDiffs oldTail newTail diffTail |> LensLess.Results.map (\replacementTails ->
                  replacements ++ replacementTails
                )
              )
            _ -> error "Unexpected size of cOld and cNew"
        subdiff::diffTail -> Err ("Insertion or deletions, cannot short-circuit at " + toString j + ", " + toString subdiff)
      in
      let gatherDiffs outputOld outputNew diffs = case (outputOld, outputNew, diffs) of
        (["span", [["start", p]], [["TEXT", vOld]]], 
         ["span", [["start", p]], [["TEXT", vNew]]],
          VListDiffs [ListElemSkip 2, ListElemUpdate (VListDiffs [ListElemUpdate (VListDiffs [ListElemSkip 1, ListElemUpdate sd])])]) ->
           Ok [[(Debug.log ("escaped string '" + vNew + "' with diffs " + toString sd) <| Update.mapInserted escape vNew sd, String.toInt p, String.toInt p + String.length vOld)]]
        (["span", [["start", p]], [["TEXT", vOld]]],
         ["span", [["start", p]], [["TEXT", vNew]]],
          VListDiffs [ListElemSkip 1, ListElemSkip 1, ListElemUpdate (VListDiffs [ListElemUpdate (VListDiffs [ListElemSkip 1, ListElemUpdate sd])])]) ->
           Ok [[(Debug.log ("escaped string '" + vNew + "' with diffs " + toString sd) <| Update.mapInserted escape vNew sd, String.toInt p, String.toInt p + String.length vOld)]]
        ([_, _, cOld], [_, _, cNew], VListDiffs [ListElemSkip 2, ListElemUpdate (VListDiffs childDiffs)]) ->
           gatherDiffsChild gatherDiffs cOld cNew childDiffs
        ([_, _, cOld], [_, _, cNew], VListDiffs [ListElemSkip 1, ListElemSkip 1, ListElemUpdate (VListDiffs childDiffs)]) ->
           gatherDiffsChild gatherDiffs cOld cNew childDiffs
        _ -> Err ("Could not find text differences " + toString (outputOld, outputNew, diffs))
      in
      case gatherDiffsChild gatherDiffs outputOld outputNew ldiffs of
        Ok [replacements] ->
          let newLatex = foldl (\(newValue, start, end) acc ->
            String.substring 0 start acc + newValue + String.drop end acc
          ) latex replacements in
          Update.diff latex newLatex
          |> Result.map (\mbDiff ->
            let newDiff = Maybe.map (\d -> VRecordDiffs { _2 = d})  mbDiff in
            (InputsWithDiffs [((f, newLatex), newDiff)])
          )
        Err msg ->
          Update.updateApp {
            fun (f, x) = f x, input = (f, latex), outputOld = outputOld, output = outputNew, diffs = diffs
          }
  } (toHtml << parse << tokens) latex

Html.forceRefresh <|
<span style="margin:10px">
<style type="text/css">
#content {
  font-family: 'Computer Modern Serif';
}
#content h1 {
  font-size: 24px;
  margin-top: 10px;
}
#content h2 {
  font-size: 18px;
  margin-top: 10px;
}

.tex sub, .latex sub, .latex sup {
  text-transform: uppercase;
}

.tex sub, .latex sub {
  vertical-align: 0.3em;
  margin-left: -0.1667em;
  margin-right: -0.125em;
}

.tex, .latex, .tex sub, .latex sub {
  font-size: 1em;
}

.latex sup {
  font-size: 0.85em;
  vertical-align: -0.3em;
  margin-left: -0.36em;
  margin-right: -0.15em;
}
.fraction {
  display: inline-block;
  position: relative;
  vertical-align: middle; 
  letter-spacing: 0.001em;
  text-align: center;
  font-size: 12px;
  }
.fraction > span { 
  display: block; 
  padding: 0.1em; 
  }
.fraction span.fdn {border-top: thin solid black;}
.fraction span.bar {display: none;}
latex-sc {
  font-variant: small-caps;
}
.labellink {
  color: #CCC;
  font-size: 0.5em;
  opacity: 0.3;
}
.labellink:hover {
  color: blue;
  opacity: 1;
}
</style>
<textarea
   style="font-family:monospace;width:100%;min-height:200px"
   onchange="this.textContent = this.value"
   onkeyup="""if(typeof timer != "undefined") clearTimeout(timer); timer = setTimeout((function(self){ return function() { self.textContent = self.value; } })(this), 2000);"""
>@latex</textarea>
<span>
  <button type="button" class="btn btn-default btn-sm" onclick="document.execCommand( 'bold',false,null)" contenteditable="false">
    <span class="glyphicon glyphicon-bold"></span> Bold
  </button><button type="button" class="btn btn-default btn-sm" onclick="document.execCommand('italic',false,null);" contenteditable="false">
    <span class="glyphicon glyphicon-italic"></span> Italic
  </button>
  <br>
  <div style="display:inline-block" id="content">
  @(latex2html latex)</div></span></span>