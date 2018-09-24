# ICFP Tutorial - Justin's part

- Basic syntax
- Run button
- File storage features
- Language is currently untyped
- Simple HTML example
- Purposely trigger crash?
- Try a couple tools if wanted, perhaps rename?

# Steps

## Introduction

```
main =
  [ "div"
  , []
  , []
  ]
```

We can output HTML nodes by creating this tree structure:

```
main =
  [ "div" -- tag name
  , []    -- attributes
  , [ [ "h1"
      , []
      , [ ["TEXT", "Hello, world!"] ]
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
      , [ ["TEXT", "Hello," + name + "!"] ]
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

Now let's make a list of our favorite things.

```
name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>These are a few of my favorite things</h2>
    <ul>
      <li>Raindrops on roses</li>
      <li>Whiskers on kittens</li>
      <li>Bright copper kettles</li>
      <li>Warm woolen mittens</li>
      <li>Brown paper packages tied up with strings</li>
    </ul>
  </div>
```

Let's add some additional information to these list items.

```
name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>These are a few of my favorite things</h2>
    <ul>
      <li>I like raindrops on roses</li>
      <li>I like whiskers on kittens</li>
      <li>I like bright copper kettles</li>
      <li>I like warm woolen mittens</li>
      <li>I like brown paper packages tied up with strings</li>
    </ul>
  </div>
```

## Functions

That was a lot of typing! Let's abstract it into the function so we don't have to ever do that again in the future (change to "really like").

```
like x =
  <li>I really like @x</li>

name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>These are a few of my favorite things</h2>
    <ul>
      @(like "raindrops on roses")
      @(like "whiskers on kittens")
      @(like "bright copper kettles")
      @(like "warm woolen mittens")
      @(like "brown paper packages tied up with strings")
    </ul>
  </div>
```

## List Mapping

But we can do better! Let's make a list of our favorite things, and map over it:

```
like x =
  <li>I really like @x</li>

myFavoriteThings =
  [ "raindrops on roses"
  , "whiskers on kittens"
  , "bright copper kettles"
  , "warm woolen mittens"
  , "brown paper packages tied up with strings"
  ]

name =
  "Justin"

main =
  <div>
    <h1>Hello, @name!</h1>
    <h2>These are a few of my favorite things</h2>
    <ul>
      @(List.map like myFavoriteThings)
    </ul>
  </div>
```

## Saving

Now let's save the file. Once the file is saved, we Sketch-n-Sketch
automatically enables backup recovery for the file.

## Styles

We can modify styles, too:

```
like x =
  <li>I really like @x</li>

myFavoriteThings =
  [ "raindrops on roses"
  , "whiskers on kittens"
  , "bright copper kettles"
  , "warm woolen mittens"
  , "brown paper packages tied up with strings"
  ]

name =
  "Justin"

main =
  <div style="padding: 10px;">
    <h1>Hello, @name!</h1>
    <h2>These are a few of my favorite things</h2>
    <ul>
      @(List.map like myFavoriteThings)
    </ul>
  </div>
```

Let's add a lot of style, actually!

```
like x =
  <li>I really like @x</li>

myFavoriteThings =
  [ "raindrops on roses"
  , "whiskers on kittens"
  , "bright copper kettles"
  , "warm woolen mittens"
  , "brown paper packages tied up with strings"
  ]
  
shoppingList =
  [ ("red", "apples")
  , ("orange", "carrots")
  , ("yellow", "bananas")
  , ("green", "kale")
  , ("blue", "berries")
  , ("indigo", "dye")
  , ("violet", "flowers")
  ]
  
shoppingEntry (color, item) =
  <li style=("color:" + color)
     >@color @item</li>

name =
  "Justin"

main =
  <div style="padding: 10px;">
    <h1>Hello, @name!</h1>
    <h2>These are a few of my favorite things</h2>
    <ul>
      @(List.map like myFavoriteThings)
    </ul>
    <h2>Shopping list</h2>
    <ul>
      @(List.map shoppingEntry shoppingList)
    </ul>
  </div>
```

## Abstraction

And we'll abstract once more:

```
like x =
  <li>I really like @x</li>

myFavoriteThings =
  [ "raindrops on roses"
  , "whiskers on kittens"
  , "bright copper kettles"
  , "warm woolen mittens"
  , "brown paper packages tied up with strings"
  ]
  
shoppingList =
  [ ("red", "apples")
  , ("orange", "carrots")
  , ("yellow", "bananas")
  , ("green", "kale")
  , ("blue", "berries")
  , ("indigo", "dye")
  , ("violet", "flowers")
  ]
  
shoppingEntry (color, item) =
  <li style=("color:" + color)
     >@color @item</li>

info title listViewer list =
  <section>
    <h2>@title</h2>
    <ul>
      @(List.map listViewer list)
    </ul>
  </section>

name =
  "Justin"

main =
  <div style="padding: 10px;">
    <h1>Hello, @name!</h1>
    @(info "These are a few of my favorite things" like myFavoriteThings)
    @(info "Shopping list" shoppingEntry shoppingList)
  </div>
```

## Lambda

And to finish it off (note the lambda):

```
like x =
  <li>I really like @x</li>

myFavoriteThings =
  [ "raindrops on roses"
  , "whiskers on kittens"
  , "bright copper kettles"
  , "warm woolen mittens"
  , "brown paper packages tied up with strings"
  ]
  
shoppingList =
  [ ("red", "apples")
  , ("orange", "carrots")
  , ("yellow", "bananas")
  , ("green", "kale")
  , ("blue", "berries")
  , ("indigo", "dye")
  , ("violet", "flowers")
  ]
  
shoppingEntry (color, item) =
  <li style=("color:" + color)
     >@color @item</li>

info title listViewer list =
  <section>
    <h2>@title</h2>
    <ul>
      @(List.map listViewer list)
    </ul>
  </section>

name =
  "Justin"

myData =
  [ ("These are a few of my favorite things", like, myFavoriteThings)
  , ("Shopping list", shoppingEntry, shoppingList)
  ]

main =
  <div style="padding: 10px;">
    <h1>Hello, @name!</h1>
    @(List.map (\(t, lv, l) -> info t lv l) myData)
  </div>
```

## Bidirectional Programming 2

Let's change the output of our shopping list!

```
like x =
  <li>I really like @x</li>

myFavoriteThings =
  [ "raindrops on roses"
  , "whiskers on kittens"
  , "bright copper kettles"
  , "warm woolen mittens"
  , "brown paper packages tied up with strings"
  ]
  
shoppingList =
  [ ("yellow", "apples")
  , ("orange", "carrots")
  , ("yellow", "bananas")
  , ("green", "kale")
  , ("black", "berries")
  , ("cyan", "dye")
  , ("purple", "flowers")
  ]
  
shoppingEntry (color, item) =
  <li style=("color:" + color)
     >@color @item</li>

info title listViewer list =
  <section>
    <h2>@title</h2>
    <ul>
      @(List.map listViewer list)
    </ul>
  </section>

name =
  "Justin"

myData =
  [ ("These are a few of my favorite things", like, myFavoriteThings)
  , ("Shopping list", shoppingEntry, shoppingList)
  ]

main =
  <div style="padding: 10px;">
    <h1>Hello, @name!</h1>
    @(List.map (\(t, lv, l) -> info t lv l) myData)
  </div>
```