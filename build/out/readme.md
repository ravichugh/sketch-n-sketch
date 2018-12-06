# Sketch-n-sketch Application Programming Interface

This npm packages offers a quick but simple API for dealing with the Sketch-n-sketch bidirectional evaluator in Javascript. We hope to provide enough functionalities to enable the creation of web server that serve this language.

## Get familiar with Sketch-n-sketch

If you want to get familiar with Sketch-n-sketch for the language, first visit the following link:

[github.com/ravichugh/sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)

## API usage

In a javascript of your node.js project, simply include the following:

    const sns = require("sketch-n-sketch");

The evaluation of this line will take 3 seconds the first time, the time necessary to evaluate the prelude.

## Basic API documentation

Given that `JSObj` represents the native Javascript object type to store key/value pairs, and `JSAny` any javascript value (in our case: numbers, booleans, arrays and objects), we provide the following API along with their types.

    parse: String -> Result String Exp
    evaluate: Exp -> Result String Val
    update: Exp -> Val -> Val -> Result String (LazyList Exp)
    unparse: Exp -> String
    process: Result err a -> (a -> Result err b) -> Result err b
    valToNative: Val -> JSAny
    nativeToVal: JSAny -> Val
    valToString: Val -> String
    valToHTMLSource: Val -> Result String String

### Specialized API

Plug-in an environment of constants (as a JavaScript object):

    objEnv.evaluate: JSObj -> Exp -> Result String Val
    objEnv.update: JSObj -> Exp -> Val -> Result String (LazyList (JSObj, Exp))
    objEnv.updateWithOld: JSObj -> Exp -> Val -> Val -> Result String (LazyList (JSObj, Exp))

Combine parse and evaluate steps directly:

    string.evaluate: String -> Result String Val
    string.update: String -> Val -> Result String (LazyList String)
    string.updateWithOld: String -> Val -> Val -> Result String (LazyList String)

Plug-in an environment of constants AND combine parse and evaluate (`string.objEnv.` is also a valid prefix):

    objEnv.string.evaluate: JSObj -> String -> Result String Val
    objEnv.string.update: JSObj -> String -> Val -> Result String (LazyList (JSObj, String))
    objEnv.string.updateWithOld: JSObj -> String -> Val -> Val -> Result String (LazyList (JSObj, String))

`update` and `updateWithOld` are doing the same thing, except that the first `Val` of `updateWithOld` should be the previously computed value and the second `Val` the new value, whereas `update` only takes in the new value and recomputes the old value itself.

## Basic API Example usage

Here is a detailed workflow using variable assignments, expression parsing and unparsing:

    // sns.parse returns either
    //    {ctor: "Ok", _0: SNS AST of this program}
    // or {ctor:"Err", _0: Parse error message}
    var prog = sns.parse(
      `let f x = x + 1 in
       let who = "world" in
       [f 2, """Hello @who""", <span>Nice</span>]`)._0;
    
    // sns.evaluate returns either 
    //    {ctor: "Ok",  _0: Val}
    // or {ctor: "Err", _0: Evaluation error message}
    var outVal = sns.evaluate(prog)._0;
    
    /* Prints
       [ 3,
         "Hello world",
         [ "span",
           [],
           [ [ "TEXT",
               "Nice"
             ]
           ]
         ]
       ] */
    console.log(sns.valToString(outVal))
    
    // Returns either
    //    {ctor: "Ok",  _0: native val}
    // or {ctor: "Err", _0: Conversion error message}
    // Here nativeOutVal is equal to [3, "Hello world", ["span", [], [["TEXT", "Nice"]]]]
    var nativeOutVal = sns.valToNative(outVal)._0;
    console.log(nativeOutVal);
    
    // Convert this Javascript value to a SNS value. This always succeed for base types.
    var newOutVal = sns.nativeToVal([2, "Hello earth", ["span", [], [["TEXT", "Nice"]]]]);
    
    // Invokes our update procedure with the given new out value.
    // The result is either
    //    {ctor: "Ok",  _0: Lazy list of new exps}
    // or {ctor: "Err", _0: Update error message}
    var newProgs = sns.update(prog)(outVal)(newOutVal);
    
    var solutions = newProgs._0;
    // sns.lazyList.nonEmpty(solutions) == true
    var solutionExp1 = sns.lazyList.head(solutions);
    
    // Display the first solution to the update problem.
    console.log(sns.unparse(solutionExp1))
    /*let f x = x + 1 in
      let who = "earth" in
      [f 1, """Hello @who""", <span>Nice</span>]
    */

    // Tail of solutions, computed lazily.
    var solutionsTail = sns.lazyList.tail(solutions);
    
    var solutionExp2 = sns.lazyList.head(solutionsTail);
    console.log(sns.unparse(solutionExp2))
    /*let f x = x + 0 in
      let who = "earth" in
      [f 2, """Hello @who""", <span>Nice</span>]
     */
    // Third solution, etc.
    // sns.lazyList.isEmpty(sns.lazyList.tail(solutionsTail)) == true

We can also encode this workflow in a way that carries errors until the result must be recovered:

    var resProg = sns.parse(
      `let f x = x + 1 in
       let who = "world" in
       [f 2, """Hello @who""", <span>Nice</span>]`);
    
    var resOutVal = sns.process(resProg)(prog =>
      sns.evaluate(prog));
    
    // Returns either {ctor: "Ok", _0: native val} or {ctor: "Err", _0: Conversion error message}
    // Here nativeOutVal._0 is equal to [3, "Hello world", ["span", [], [["TEXT", "Nice"]]]]
    var nativeOutVal = sns.process(resOutVal)(outVal =>
      sns.valToNative(outVal));
    console.log(nativeOutVal);
    
    // Convert this Javascript value to a SNS value. This always succeed if we don't use functions.
    var newOutVal = sns.nativeToVal([2, "Hello earth", ["span", [], [["TEXT", "Nice"]]]]);
    
    // Invokes our update procedure with the given new out value.
    // The result is either {ctor: "Ok", _0: Lazy list of new exps} or {ctor: "Err", _0: Update error message}
    var newProgs =
      sns.process(resProg)(prog =>
      sns.process(resOutVal)(outVal =>
        sns.update(prog)(outVal)(newOutVal)
      ));

    // The remaining works as before.

`sns.valToHTMLSource` converts a value to a string in such a way it can be served as a web page (e.g. as the content of a http response). It takes care of

* Special characters in attributes.
* < > & in the script and style nodes.

Here is how you can use it.

    var prog = sns.parse(
      `let x = "world" in
       <span>Hello @x</span>`)._0;
    
    var outVal = sns.evaluate(prog)._0;
    
    // Returns either a
    //    {ctor:"Ok", _0: The HTML string}
    // or {ctor:"Err", _0: the error message}.
    var outHTML = sns.valToHTMLSource(outVal);
    
    // Displays '<span>Hello world</span>'
    console.log(outHTML._0);

## Contributing: Package Sketch-n-sketch for npm (node.js):

After building Sketch-n-sketch, go to `build/out` and run

    npm version patch
    npm publish

You might need to register a user to do the last command.
After publishing, make sure you update the dependency to SNS in your project via

    npm update sketch-n-sketch



