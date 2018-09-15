# Draft Demo Script

## (Untyped) Code Tools

1. Type checker off to start.

1. All text-edits.

        rectangle =
          rect "gray" 0 0 200 200

        line1 =
          line "white" 10 0 0 200 200

        line2 =
          line "white" 10 0 200 200 0
  
        main =
          svg [rectangle, line1, line2]

1. <span style="color:green;">**OK:**</span>
   **Rename**, twice.

        rectangle =
          rect "gray" 0 0 200 200

        fullLine =
          line "white" 10 0 0 200 200

        halfLine =
          line "white" 10 0 200 200 0

        main =
          svg [rectangle, fullLine, halfLine]

1. <span style="color:green;">**OK:**</span>
   **Make Equal with Single Variable** on 3 ``0``. Names options `x` and `x1`.

1. <span style="color:green;">**OK:**</span>
   **Make Equal with Single Variable** on 3 ``0``. Name options `y` and `y1`.

1. <span style="color:green;">**OK:**</span>
   **Make Equal with Single Variable** on 6 ``200``. Name options...

        x = 0
        
        y = 0
        
        w = 200
        
        rectangle =
          rect "gray" x y w w

        fullLine =
          line "white" 10 x y w w

        halfLine =
          line "white" 10 x w w y
  
        main =
          svg [rectangle, fullLine, halfLine]

1. <span style="color:green;">**OK:**</span>
   **Rename** `w` to `size`.

1. <span style="color:green;">**OK:**</span>
   **Move Definition** `y` after `x`, then `size` after `y`.

        (x, y, size) = (0, 0, 200)

        rectangle =
          rect "gray" x y size size

        fullLine =
          line "white" 10 x y size size

        halfLine =
          line "white" 10 x size size y
  
        main =
          svg [rectangle, fullLine, halfLine]

1. <span style="color:red;">**TODO-Ravi:**</span>
   Live Sync, to show that bottom right corner isn't right yet.
   Output Sync > Live (Traces / Triggers) has stopped working...

1. Text edits to get X always moving together.

        (x, y, size) = (10, 10, 200)
        
        rectangle =
          rect "gray" x y size size

        fullLine =
          line "white" 10 x y (x + size) (y + size)

        halfLine =
          line "white" 10 x (y + size) (x + size) y
  
        main =
          svg [rectangle, fullLine, halfLine]

1. Text edits to get center point for `halfLine`.

        (x, y, size) = (10, 10, 200)

        (cx, cy) = (x + 0.5*size, y + 0.5*size)

        rectangle =
          rect "gray" x y size size

        fullLine =
          line "white" 10 x y (x + size) (y + size)

        halfLine =
          line "white" 10 x (y + size) cx cy

        main =
          svg [rectangle, fullLine, halfLine]

1. <span style="color:green;">**OK:**</span>
   **Introduce Variable** for shape list. Nice default name `shapes`.
   Text edit to add line break.

1. <span style="color:green;">**OK:**</span>
   **Rename** to `logo`.

        (x, y, size) = (10, 10, 200)
        
        (cx, cy) = (x + 0.5*size, y + 0.5*size)

        rectangle =
          rect "gray" x y size size

        fullLine =
          line "white" 10 x y (x + size) (y + size)

        halfLine =
          line "white" 10 x (y + size) cx cy
          
        logo =
          [rectangle, fullLine, halfLine]
  
        main =
          svg logo

1. <span style="color:green;">**OK:**</span>
   **Move Definitions** all 5 to before the list literal in `logo`.
          
        -- blank line
        -- blank line
        logo =
          let
            (x, y, size) = (10, 10, 200)
        
            (cx, cy) = (x + 0.5*size, y + 0.5*size)

            rectangle =
              rect "gray" x y size size

            fullLine =
              line "white" 10 x y (x + size) (y + size)

            halfLine =
              line "white" 10 x (y + size) cx cy
          in
          [rectangle, fullLine, halfLine]
  
        main =
          svg logo

1. <span style="color:green;">**OK:**</span>
   Select the `let` **Format**. Choose the option that looks like:

        logo =
          let
            (x, y, size) =
              (0, 0, 200)

            (cx, cy) =
              (x + 0.5*size, y + 0.5*size)

            rectangle =
              rect "gray" x y (x + size) (y + size)

            fullLine =
              line "white" 10 x y (x + size) (y + size)

            halfLine =
              line "white" 10 x (y + size) cx cy
          in
          [rectangle, fullLine, halfLine]

        main =
          svg logo

1. <span style="color:red;">**TODO:**</span>
   Select `logo` and **Create Function from Definition**.
   Could give options to keep tupled-values as tuple arguments.
   Or not, just an option with "flattened" args is fine.
          
        logo x y size =
          let
            (cx, cy) =
              (x + 0.5*size, y + 0.5*size)

            rectangle =
              rect "gray" x y size size

            fullLine =
              line "white" 10 x y (x + size) (y + size)

            halfLine =
              line "white" 10 x (y + size) cx cy
          in
          [rectangle, fullLine, halfLine]
  
        main =
          svg (logo 10 10 200)

1. <span style="color:red;">**TODO: Need to rewrite the call.**</span>
   Select `"gray"` and **Add Argument** adds to end of argument list.

1. <span style="color:orange;">**TODO: Could do without this if necessary.**</span>
   Select `fill` and before `x` and **Reorder Argument**.

        logo fill x y size =
          let
            (cx, cy) =
              (x + 0.5*size, y + 0.5*size)

            rectangle =
              rect fill x y size size

            fullLine =
              line "white" 10 x y (x + size) (y + size)

            halfLine =
              line "white" 10 x (y + size) cx cy
          in
          [rectangle, fullLine, halfLine]
  
        main =
          svg (logo "gray" 10 10 200)

1. <span style="color:green;">**OK:**</span>
   Select three shapes and **Inline Definitions**.

1. <span style="color:green;">**OK:**</span>
   Select the shape list and **Format**.
          
        logo fill x y size =
          let
            (cx, cy) = (x + 0.5*size, y + 0.5*size)
          in
          [ rect fill x y size size
          , line "white" 10 x y (x + size) (y + size)
          , line "white" 10 x (y + size) cx cy
          ]
  
        main =
          svg (logo "gray" 10 10 200)

1. <span style="color:orange;">**OK/TODO-Ravi: Could add another option, or not.**</span>
   Select the let and **Format**. Hover the option, just to see,
   but don't select it.

        logo fill x y size =
          let (cx, cy) = (x + 0.5*size, y + 0.5*size) in
            [ rect fill x y size size
            , line "white" 10 x y (x + size) (y + size)
            , line "white" 10 x (y + size) cx cy
            ]

        main =
          svg (logo "gray" 20 30 200)


## Type Inspector

1. Turn on type checking.

1. Hover around and see a bunch of things got type checked.

1. <span style="color:green;">**OK:**</span>
   Currently need annotations on functions, so there's an error.
   There's a tool to **Add dummy type annotation**.

        logo : _ -> _ -> _ -> _ -> _

1. <span style="color:orange;">**TODO-Nick: This could be a cool place for keyboard-based navigation,
   if text edits can simply operate on the selected token.**</span>
   Text-edit to add (buggy) annotation.

        logo : String -> Num -> Num -> Num -> Svg

1. <span style="color:red;">**TODO-Ravi:**</span>
   **Type Information** tool says return type doesn't match
   annotation. Has an option to **Change annotation for return value**.
   <span style="color:orange;">TODO: And perhaps **Insert call to `svgConcat`**.</span>

        logo : String -> Num -> Num -> Num -> List Svg

1. <span style="color:green;">**OK:**</span>
   Select `main`, **Type Information**, **Add inferred annotation**.

        main : Svg

1. <span style="color:red;">**TODO: Need to design nice view for type errors and fixes.**</span>
   Text-edit a `size` to `width`. Look at the nicely formatted
   error in **Type Information**.

1. <span style="color:orange;">**TODO: Ditto render. Fix is suggested.**</span>
   Text-edit the `width` to `sighs`. Close enough, so error suggests a fix.


## Type-Based Code Tools + Holes

1. <span style="color:red;">**TODO:**</span>
   Select three `Num`s and **Introduce Type Alias**. One option
   with tuple type, one with record type.

1. <span style="color:red;">**TODO:**</span>
   **Rename Type**.

        type alias Params = {x:Num, y:Num, size:Num}
        
        logo : String -> Params -> Svg
        logo fill {x, y, size} =
          ...

        main =
          svg (logo "gray" {x=10, y=10, size=200})

1. <span style="color:red;">**TODO:**</span>
   Select record type and **Format** to see a few different options. Keep as-is.

1. <span style="color:red;">**TODO:**</span>
   Select `Params` type and **Convert to Datatype**.

        type Params = Params {x:Num, y:Num, size:Num}
        
        logo : String -> Params -> Svg
        logo fill (Params {x, y, size}) =
          ...

        main =
          svg (logo "gray" (Params {x=10, y=10, size=200}))

1. <span style="color:red;">**TODO:**</span>
   **Rename Constructor** to `TopLeft`.

1. <span style="color:red;">**TODO:**</span>
   **Duplicate Constructor**. Let's have the dummy case just be a hole to
   start. Click `Params` and **Format** to choose multi-line option.

        type Params
          = TopLeft {x:Num, y:Num, size:Num}
          | NewConstructor {x:Num, y:Num, size:Num}
        
        logo : String -> Params -> Svg
        logo fill params =
          let
            {x, y, size} =
              case params of
                TopLeft xysize ->
                  xysize

                NewConstructor {x, y, size} ->
                  ??
            ...

1. <span style="color:red;">**TODO:**</span>
   **Rename** to `Center`. **Rename** fields to `cx`, `cy`, and `rad`.

1. <span style="color:red;">**TODO:**</span>
   **Type Information** shows error between branches. Because one
   branch synthesized, one fix for the hole is to convert to record
   of holes.

                Center {cx, cy, rad} ->
                  {x=??, y=??, size=??}

1. <span style="color:orange;">**TODO-Nick: Another nice place for keyboard-nav.**</span>
   Text edits.

                Center {cx, cy, rad} ->
                  {x=cx-rad, y=cy-rad, size=2*rad}

1. Text edit to `Center`-based logo.

        main : Svg
        main =
          svg (logo "gray" (Center {cx=300, cy=300, rad=40}))

1. <span style="color:red;">**TODO:**</span>
   Select the `svg` call and **Format**. Multiple options, such as

          svg <| logo "gray" <| Center {cx=300, cy=300, rad=40}

            and

          Center {cx=300, cy=300, rad=40}
            |> logo "gray"
            |> svg

1. Final-ish.

        type Params
          = TopLeft {x:Num, y:Num, size:Num}
          | Center {cx:Num, cy:Num, rad:Num}
        
        logo : String -> Params -> Svg
        logo fill params =
          let
            {x, y, size} =
              case params of
                TopLeft xysize ->
                  xysize

                Center {cx, cy, rad} ->
                  {x=cx-rad, y=cy-rad, size=2*rad}
                
            (cx, cy) =
              (x + 0.5*size, y + 0.5*size)
          in
          [ rect fill x y size size
          , line "white" 10 x y (x + size) (y + size)
          , line "white" 10 x (y + size) cx cy
          ]
  
        main =
          svg (logo "gray" (Center {cx=300, cy=300, rad=40}))



# Notes

## ICSE-style

- Rename
- Make Equal with Single Variable
- Move Definition(s)
- Inline Definition
- Create Function from Definition (don't need Create Function from Arguments)
- Add Argument / Remove Argument

## New

- Introduce Type Alias (two options: record or tuple)
- Rename Type
- Type Alias to Datatype
- Rename Data Constructor
- Duplicate Data Constructor
- Rename Record Field
- Replace Hole (based on type)
- Format List

## New (maybe)

- Format Let (equations on single lines or not, lines between equations or not, ...)
- Format Nested Call (nested parens, backwards/forwards pipeline, forwards pipeline with new lines)

## More Notes

- Add Element to List: add "background" to the svg list
- Rename: line1 / line2
- Make Equal with Single Variable: color, x/y position, width/height
   - It would be nice if Introduce Variable(s) used the nice naming feature
       - If there are multiple candidate names, there should be a menu of
         options
- Move Definitions: select y and put it on the same line as x
   - Maybe the first tool that you wouldn't expect from another editor
- Assume that math for top/left/bottom/right is done manually
- Move Definitions: move all the definitions to inside the logo function
   - Automatically takes care of indentation / local let syntax / etc.
- DESIRED TOOL: select a let, and shows you different format versions of a let
   - Same for lists!
   - Called "Format" (maybe): Works for lets, lists, lambdas, function
     application (pipeline style, parens, ...) etc.
- Inline Definition: background / longLine / halfLine
   - IF THERE'S TIME: Also add comment to end of line (with old variable name)!
- Create Function from Definition / Create Function from Arguments
   - Even if Create Function from Definition isn't 100% right, we can always use
     the Add Argument or Remove Argument tools
- Introduce Type Alias: select the four Num arguments in the type signature,
  then create a type alias
   - There are two options that it could give:
       (1) give a record where the fields are the name of the arguments in the
function
       (2) or an n-tuple
- Rename Type
- Type Alias to Datatype
- Rename Data Constructor
- Duplicate Data Constructor
- Rename Record Field
- Replace Hole [based on type] (?? -> {x=??, y=??, w=??, h=??})
- (?) Format svg call with backward pipeline operator (edited)


