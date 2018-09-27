# ICFP Tutorial - Justin's part

- Basic syntax
- Run button
- File storage features
- Language is currently untyped
- Simple HTML example
- Purposely trigger crash?
- Try a couple tools if wanted, perhaps rename?

# Steps

## HTML Structure

Let's start with a blank file:

```
main =
  [ "div"
  , []
  , []
  ]
```

We can construct HTML nodes with this tree structure:

```
main =
  [ "div" -- tag name
  , []    -- attributes
  , []    -- children
  ]
```

Let's add some text:

```
main =
  [ "div" -- tag name
  , []    -- attributes
  , [ [ "h1"
      , []
      , [ ["TEXT", "Hello, world!"]
        ]
      ]
    ]
  ]
```

## Variables

We can introduce a variable:

```
name =
  "Justin"

main =
  [ "div" -- tag name
  , []    -- attributes
  , [ [ "h1"
      , []
      , [ ["TEXT", "Hello," + name + "!"]
        ]
      ]
    ]
  ]
```

## Bidirectional Programming 1

Uh oh! Formatting issue ("Hello,Justin!")! We can add the space in the output, and add the space to the "Hello, " string (not the name string).

```
name =
  "Justin"

main =
  [ "div" -- tag name
  , []    -- attributes
  , [ [ "h1"
      , []
      , [ ["TEXT", "Hello, " + name + "!"] ]
      ]
    ]
  ]
```

## HTML Literals

This tree is really clunky/annoying to type out, though... luckily, we have HTML literals! Notice the @ sign.

```
name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
  </div>
```

## HTML List

Now let's make a shopping list.

```
name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      <li>red apples</li>
      <li>yellow potatoes</li>
      <li>green beans</li>
      <li>blue berries</li>
    </ul>
  </div>
```

## Bidirectional Programming 2

Let's add some additional information to these list items.

```
name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      <li>I need red apples</li>
      <li>I need yellow potatoes</li>
      <li>I need green beans</li>
      <li>I need blue berries</li>
    </ul>
  </div>
```

## Functions + List Mapping

That was a lot of typing! Let's abstract "needing" into a function so we don't have to ever do that again in the future, and map it over a list of items that we need to buy.

```
require entry =
  <li>I need @entry</li>

shoppingList =
  [ "red apples"
  , "yellow potatoes"
  , "green beans"
  , "blue berries"
  ]

name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      @(List.map require shoppingList)
    </ul>
  </div>
```

## Bidirectional Programming 3

Let's change our "require" function using the output canvas:

```
require entry =
  <li>I really need @entry</li>

shoppingList =
  [ "red apples"
  , "yellow potatoes"
  , "green beans"
  , "blue berries"
  ]

name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      @(List.map require shoppingList)
    </ul>
  </div>
```

And now let's add some more items to our shopping list!

```
require entry =
  <li>I really need @entry</li>

shoppingList =
  [ "red apples"
  , "yellow potatoes"
  , "green beans"
  , "blue berries"
  , "chocolate ice cream"
  , "tomato soup"
  ]

name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      @(List.map require shoppingList)
    </ul>
  </div>
```

## Saving

Now let's save the file. Once the file is saved, Sketch-n-Sketch
automatically enables backup recovery for the file.

## Exporting

We can also export our output as HTML!

## Styles

We can modify styles, too:

```
require entry =
  <li>I really need @entry</li>

shoppingList =
  [ "red apples"
  , "yellow potatoes"
  , "green beans"
  , "blue berries"
  , "chocolate ice cream"
  , "tomato soup"
  ]

name =
  "Justin"

main =
  <div style="padding: 10px">
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      @(List.map require shoppingList)
    </ul>
  </div>
```

## Let bindings

Let's add a lot of style, actually, using let bindings and string operations!

```
require entry =
  let
    color :: _ =
      Regex.split " " entry
    
    myStyle =
      "color: " + color
  in
    <li style=myStyle>I need @entry</li>

shoppingList =
  [ "red apples"
  , "yellow potatoes"
  , "green beans"
  , "blue berries"
  , "chocolate cake"
  , "tomato soup"
  ]

name =
  "Justin"

main =
  <div style="padding: 10px;">
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      @(List.map require shoppingList)
    </ul>
  </div>
```

## Bidirectional Programming 4

Let's change the output of our shopping list!

```
require entry =
  let
    color :: _ =
      Regex.split " " entry
    
    myStyle =
      "color: " + color
  in
    <li style=myStyle>I need @entry</li>

shoppingList =
  [ "green apples"
  , "purple potatoes"
  , "red beans"
  , "blue berries"
  , "chocolate cake"
  , "tomato soup"
  ]

name =
  "Justin"

main =
  <div style="padding: 10px;">
    <h1>Hello, @name!</h1>
    <h2>@name's Shopping List</h2>
    <ul>
      @(List.map require shoppingList)
    </ul>
  </div>
```
