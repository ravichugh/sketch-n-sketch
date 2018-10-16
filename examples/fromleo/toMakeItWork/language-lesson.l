// download: lecture05.html
// output: html

let abbr text symbol = ``<abbr title="@text">@symbol</abbr>`` in
let definitions = {
  def_eps  = ["empty word", "ε", "%eps"],
  def_conc = ["concatenation", "·", "%concat"],
  def_emp  = ["empty set", "∅", "%emp"],
  def_A    = ["alphabet", "A", "%A"],
  def_union = ["union","∪","%union"],
  def_intersect = ["intersection", "∩", "%intersect"],
  def_setdiff = ["set difference", "∖", "%setdiff"],
  def_setin = ["set membership", "∊", "%setin"]
} in
let defs = {
  default = \x.
   let [title, symbol, command]= definitions ("def_" + x) in
   command // Will be replaced later.
} in
import defs.[eps, conc, emp, A, union, intersect, setdiff, setin]
let macrosKeys = keysOf definitions in
let macrosValues = 
  letrec aux acc macros = macros match {
    case [] => acc
    case [key] ++ tail => let newAcc = acc ++ [definitions key] in aux newAcc tail
  } in aux [] macrosKeys in

letrec take n array =
  if(n==array.length) array else (array match {
    case [] => array
    case init ++ [last] =>
      take n init
  }) in
letrec drop n array =
  if(n==0) array else (array match {
    case [] => array
    case [head] ++ tail =>
      let newN = n- 1 in
      drop newN tail
  }) in
  
let notations = {
  apply=\input.
    letrec aux txt macros = macros match {
      case [] => txt
      case [[title, symbol, command]] ++ tail => 
          let newTxt = replaceAllIn (command+"\\b") txt (abbr title symbol) in
          aux newTxt tail
    } in input match { case input + "" => aux input macrosValues case _ => input },
  unapply=\text. {value = 
    letrec aux txt macros = macros match {
      case [] => txt
      case init ++ [[title, symbol, command]] =>
        let newTxt = replaceAllIn ((abbr title symbol) + "|<span style=\"text-decoration-line: underline;\">" +symbol+"</span>") txt command in
        aux newTxt init
    } in text match { case "" + text => aux text macrosValues case _ => text }
}
} in

letrec renderLanguage acc l = l match {
  case [] => acc + "...}"
  case [""] ++ tail =>
    let newAcc = acc + eps + ", " in
    renderLanguage newAcc tail
  case [w] ++ tail =>
    let newAcc = acc + w + ", " in
    renderLanguage newAcc tail
} in


// transforms ["a", "b", "c"] to { epsilon, a, b, c ...}
// Cartesian product of two sets mapped with f
let cartProduct setA setB f = 
  letrec m acc elem list = list match {
    case [] => acc
    case [head] ++ tail => let newAcc = acc ++ [f elem head] in
      m newAcc elem tail
  } in
  letrec aux acc setAtmp = setAtmp match {
    case [] => acc
    case [head] ++ tail => let newAcc = acc ++ (m [] head setB) in
      aux newAcc tail
  } in aux [] setA in
// The star of a language up to sizeStr.length elements.
let star sizeStr arr = 
  letrec aux acc prevGen s = s match {
    case "" => acc ++ prevGen
    case "#" + sizeTail =>
      let newAcc = acc ++ prevGen in
      let newGen = cartProduct prevGen arr (\x.\y. x + y) in
      aux newAcc newGen sizeTail
  } in
  aux [] [""] sizeStr in

// Renders document using a lens.
let render = { apply=\pars.
  letrec aux chNo secNo pars = pars match {
    case [] => []
    case [head] ++ tail => head match {
      case { title = t, type="chapter" } => 
         let chapterNo = (chNo + "i").length.toString in
         [{tag = "h1", children = "Chapter " + chapterNo + ". " + notations t}] 
         ++ aux (chNo + "i") "" tail
      case { title = t, type="section" } => 
         let sectionNo = chNo.length.toString + "." + (secNo + "i").length.toString in
         [{tag = "h2", children = sectionNo + " " + notations t}] 
         ++ aux chNo (secNo + "i") tail
      case "" + t => [{tag = "p", children = notations t}] ++ aux chNo secNo tail
      case t => [{tag = "p", children = t}] ++ aux chNo secNo tail
    }
  } in
  aux "" "" pars,
  updateAlign=\p.
    letrec aux acc outs = outs match {
      case [] => acc
      case [{tag = "h1", children= notations fullChapterTitle}] ++ tail =>
        let chapterTitle = replaceFirstIn "^\\w+\\s+\\d+\\.\\s+" fullChapterTitle "" in
        let newAcc = acc ++ [{title=chapterTitle, type="chapter"}] in
        aux newAcc tail
      case [{tag = "h2", children= notations fullSectionTitle}] ++ tail =>
        let sectionTitle = replaceFirstIn "^\\d+\\.\\d+\\s+" fullSectionTitle "" in
        let newAcc = acc ++ [{title=sectionTitle, type="section"}] in
        aux newAcc tail
      case [{children = notations content }] ++ tail =>
        let newAcc = acc ++ [content] in
        aux newAcc tail
      case [notations head] ++ tail =>
        let newAcc = acc ++ [head] in
        aux newAcc tail
    } in
    let reverseInserted = aux [] p.inserted in
    {value = (take p.insertionIndex p.input) ++ reverseInserted ++ (drop (p.insertionIndex+p.deletedCount) p.input)}
} in
// Final rendering:
{ tag = "DIV", children = ![
  { tag = "STYLE", children=``
h1 {
  font-size: large;
  font-family: Carlito;
  color: rgb(0, 0, 128);
  background-color: transparent;
  vertical-align: baseline;
  white-space: pre-wrap;
}
h2 {
  font-size: medium;
  font-family: Carlito;
  color: rgb(0, 80, 0);
  background-color: transparent;
  vertical-align: baseline;
  white-space: pre-wrap;
}
p {
  font-size: normal;
  font-family: Carlito;
  color: rgb(0, 0, 0);
  background-color: transparent;
  vertical-align: baseline;
  white-space: pre-wrap;
  text-align: justify;
  text-justify: inter-word;
}
abbr { 
  text-decoration: none;
}
``}] ++ render [
{title = "Introduction to Formal Languages", type="chapter"},  

{title="Languages", type="section"},

``Languages are symbolic means of communication and appear in many areas of science and humanities. Here we will present the notion of formal languages, which are a very general and fundamental notion in theoretical computer science. Formal languages appear in theory of computation, allowing us to talk about decidability and complexity, they arise in computer language processing, but they also appear in linguistics and natural language processing.``,

{title="Formal Language", type="section"},
``A <i>word</i> (also called a <i>string</i>) is a finite sequence of elements from some set, <i>A</i>. The word can be of length zero (<i>empty word</i>), or more (<i>non-empty word</i>). The set <i>@A</i> is called <i>the alphabet</i>. By <i>@A*</i> we denote the set of all words from <i>@A</i>. Notation <i>u @conc v</i> denotes the concatenation of words <i>u</i> and <i>v</i>. For brevity we often omit the centered dot and write just <i>uv</i>. By a <i>language</i> we mean simply a subset of <i>@A*</i>. A language is therefore a set of words: it can be empty, it can be finite or infinite. The usual operations on sets such as union (@union), intersection (@intersect), and set difference (@setdiff) therefore apply to languages as well.``,

// Alphabets used in examples.
let alphabetArray = ["0","1"] in
let alphabet = join alphabetArray "" in
``<b>Examples.</b> Let @A =@(!"{" + join alphabetArray "," + !"}"). Then
<i>@A* = @(renderLanguage "{" (star "###" alphabetArray))</i>``,

``One example of a language is an empty language @emp, which contains no words whatsoever. Another example is the language containing only the empty word, {@eps}, which is a set of cardinality one (i.e., with exactly one element, the empty word). Further example of a finite language is the language <i>{0, 01}</i>. The entire set <i>@A*</i> is also a language; this is the largest language with this alphabet and it is always infinite. Further examples of infinite languages are the set of words that contain only ones: L1 = {@eps, 1, 11, 111, ...} and the set of words of odd length: Lo = {0, 1, 000, 001, 010, 011, 100, 101, 110, 111, 00000, ... }. The intersection of these two languages is L1 @intersect Lo = {1, 11, 11111, ...}, which contains precisely the words of the odd length consisting of only ones.``,

{title="Monoid Laws", type="section"},
``The concatenation of languages is <i>associative</i>: for every three words <i>u</i>, <i>v</i>, <i>w</i>, we have the following property:
<i>u(v w) = (u v) w</i>
Indeed, both the left and the right side of the equation equal the same sequence of characters, consisting of the characters of <i>u</i>, followed by the characters of <i>v</i> and finally the characters of <i>w</i>. 
Furthermore, concatenating words with an empty word does not change it:
 u @eps = u = @eps u
This means that @eps is a neutral element for the concatenation operation. As a result, we say that the triple (@A*, @conc , @eps) forms a <i>monoid</i>.
Moreover, for any three words <i>u,v,w</i>, if <i>u w = v w</i>, then <i>u = v</i>. We call this property the <i>right cancellation law</i>, because it allows us to cancel the occurrence of the word <i>w</i>. We similarly have the left cancellation law: if <i>w u = w v</i> then <i>u = v</i>.``,

{title="Concatenation of Languages", type="section"},
``If <i>U</i> and <i>V</i> are languages, we define their concatenation, denoted <i>U @conc V</i> by
<i>U @conc V = {uv | u @setin U, v @setin V}</i>
As for words, we often omit the symbol for the binary @conc operator on languages and simply write <i>U V</i>. It is easy to verify that the concatenation is an associative operation on languages; we have:
<i>(U V) W = U (V W) = {uvw | u @setin U, v @setin V, w @setin W}</i>
Do we also have left and right cancellation laws for languages?
``
]}
