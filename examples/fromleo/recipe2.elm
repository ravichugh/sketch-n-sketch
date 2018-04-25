----------------------------------------------------------------------
-- Description of Scalable Recipe Editor

editorInfo = <span>
<h1>Scalable Recipe Editor</h1>
<p>
  This text editor is set up to allow <em>proportional quantities</em>
  to be defined relative to some <em>base</em> quantity.
  Whenever a proportional or base quantity is changed, all others are
  changed, too.
</p>
<p>
  <b>Defining Proportions:</b>
  Wrap a number with underscores (for example, "_20_") to make the
  quantity proportional to the base value.
</p>
<p>
  <b>Defining Plural Words:</b>
  To make a new word, such as "chip", appear in plural form,
  based on whether a proportional quantity is greater than one,
  write the word like "chip_20s_".
</p>
<p>
  In the example recipe below, try adding an ingredient as follows.
  Hit enter after the melted chocolate ingredient.
  Then type "_20_g of chocolate chip_20s_".
</p>
</span>

----------------------------------------------------------------------
-- Scalable Recipe Editor "Library"

introduceFractions res blah =
  case blah of
    0 -> if res == 0 then "<¼" else toString res
    1 -> if res == 0 then "¼"  else if res >= 4 then toString res else toString res + "¼"
    2 -> if res == 0 then "½"  else toString res + "½"
    3 -> if res == 0 then "¾"  else if res >= 4 then toString res else toString res + "¾"

updateFractions string =
  case Regex.extract "(.*)(¼|[ +]?[13]/[24]|½|¾)" string of
    Nothing ->
      4 * String.toInt string

    Just [i, complement] -> 
       let addi x = if i == "" then x else 4 * String.toInt i + x in
       case complement of
         "¼"    -> addi 1
         "1/4"  -> addi 1
         " 1/4" -> addi 1
         "+1/4" -> addi 1
         "½"    -> addi 2
         "1/2"  -> addi 2
         " 1/2" -> addi 2
         "+1/2" -> addi 2
         "¾"    -> addi 3
         "3/4"  -> addi 3
         " 3/4" -> addi 3
         "+3/4" -> addi 3
         a      -> error <| "Unexpected complement: " + complement

evaluateMacros base =
  Regex.replace "(multdivby|ifmany(\\w+))\\[(\\d+),(\\d+)\\]" (\m ->

    let match = nth m.group 0 in
    let macro = nth m.group 1 in
    let mult  = String.toInt <| nth m.group 3 in
    let div   = String.toInt <| nth m.group 4 in
    case macro of

      "multdivby" ->
        let res = floor (base * freeze mult / freeze div) in
        if res < 6 then -- We take into account 1/2, 1/4 and 3/4 until 5, else it makes no sense, but no more.
          Update.applyLens
            { apply (base, _) = freeze <|
                introduceFractions res (floor (base * mult * 4 / div) - 4*res)

              update {outputNew, outputOriginal} =
                if outputNew == outputOriginal then
                  { values = [(base, match)] }
                else
                  let newBase =
                    floor (updateFractions outputNew * div / mult / 4)
                  in
                  let newMatch =
                    let newMult = mult * String.toInt outputNew in
                    let newDiv  = div  * String.toInt outputOriginal in
                    """multdivby[@newMult,@newDiv]"""
                  in
                  { values = [(newBase, match), (base, newMatch)] }
            }
            (base, match)
        else
          toString res

      ifmanyEnding ->
        let ending = nth m.group 2 in
        let res = floor (base * freeze mult * freeze 4 / freeze div) in
        Update.applyLens
          { apply (res, ending) = freeze <|
              if res > 4 then ending else ""

            update {input=(res,ending), outputNew, outputOriginal} =
              if outputNew == "" && outputOriginal == ending then {values=[(4, ending)]}
              else if Regex.matchIn " " outputNew then {values = []}
              else {values = [(res, outputNew)]}
          }
          (res, ending)
  )

defineMacros base = 
  Update.applyLens
    { apply x = Update.freeze x
    , update {output} =
        { values = [Regex.replace "_(\\d+)(\\w*)_" (\m ->
            let amount = String.toInt (nth m.group 1) in
            let plural = nth m.group 2 in
            case plural of
              "" -> """multdivby[@amount,@base]"""
              _  -> """ifmany@plural[@amount,@base]"""
          ) output]
        }
    }

scalableRecipe base recipe =
  recipe |> defineMacros base |> evaluateMacros base

----------------------------------------------------------------------
-- Example: Scalable Recipe

-- base = 1000 ~ servings = 20
base = 1000

ingredients = """
<li>multdivby[4,1000] eggifmanys[4,1000]</li>
<li>multdivby[1,2000] cup of sugar</li>
<li>multdivby[200,1000]g of melted chocolate</li>
<li>multdivby[50,1000]g of almond powder</li>
<li>multdivby[2,1000] tbls of sunflower oil</li>
<li>Cinnamon</li>
<li>A pinch of salt</li>
"""

directions = """
<li>Preheat the oven at @(floor (180 * freeze 9 / freeze 5) + freeze 32)° Fahrenheit.</li>
<li>Mix all ingredients together.</li>
<li>Bake in the oven for 10 minutes in cupcakes pans.</li>
<li>Remove from oven and top with sliced almonds.</li>
"""

cupcakeRecipe = """

<img width="300px" src="https://tinyurl.com/yalbg6le" alt="cupcakes">

<h1>Chocolate almond cakes</h1>

<p>
  Servings: multdivby[20,1000] small cakes.
  
  <button
      onclick="this.setAttribute('x', parseInt(this.getAttribute('x'))*2 + '')"
      x="multdivby[1024,1024]">
    Double
  </button>,
  <button
      onclick="this.setAttribute('x', parseInt(this.getAttribute('x'))/2 + '')"
      x="multdivby[1024,1024]">
    Halve
  </button>,
  or edit the number of servings directly.
</p>

<h4>Ingredients for Cake Batter</h4>

@ingredients

<h4>Directions</h4>

@directions
"""

htmlCupcakeRecipe =
  Html.span [] [] <| Html.parse (scalableRecipe base cupcakeRecipe)

main = 
  Html.div [["margin", "20px"], ["cursor", "text"]] []
    [ Html.div [] [] [editorInfo]
    , Html.div [["border", "4px solid black"], ["padding", "20px"]] []
        [htmlCupcakeRecipe]
    ]
