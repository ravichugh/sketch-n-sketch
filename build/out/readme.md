# Sketch-n-sketch Application Programming Interface

This npm packages offers a quick but simple API for dealing with the Sketch-n-sketch bidirectional evaluator in Javascript. We hope to provide enough functionalities to enable the creation of web server that serve this language.

## Get familiar with Sketch-n-sketch

If you want to get familiar with Sketch-n-sketch for the language, first visit the following link:

[github.com/ravichugh/sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)

## How to include the API.

In a javascript of your node.js project, simply include the following:

    const sns = require("sketch-n-sketch");

The evaluation of this line will take 3 seconds the first time, the time necessary to evaluate the prelude.

## API functions:

Given that `JSObj` represents the native Javascript object type to store key/value pairs, and `JSAny` any javascript value (in our case: numbers, booleans, arrays and objects), we provide the following API along with their types.

    parse: String -> Result String Exp
    evalExp: Exp -> Result String Val
    updateExp: Exp -> Val -> Val -> Result String (LazyList Exp)
    unparse: Exp -> String
    andThen: (a -> Result err b) -> Result err a -> Result err b
    valToNative: Val -> JSAny
    nativeToVal: JSAny -> Val
    valToString: Val -> String
    valToHTMLSource: Val -> Result String String
    evaluate: String -> Result String Val
    update: String -> Val -> Result String (List String)
    evaluateEnv: JSObj -> String -> Result String Val
    updateEnv: JSObj -> String -> Val -> Result String (List (JSObj, String))

## Example usage

Here is a detailed workflow using variable assignments, expression parsing and unparsing:

    // sns.parse returns either
    //    {ctor: "Ok", _0: SNS AST of this program}
    // or {ctor:"Err", _0: Parse error message}
    var prog = sns.parse(
      `let f x = x + 1 in
       let who = "world" in
       [f 2, """Hello @who""", <span>Nice</span>]`)._0;
    
    // sns.evalExp returns either 
    //    {ctor: "Ok",  _0: Val}
    // or {ctor: "Err", _0: Evaluation error message}
    var outVal = sns.evalExp(prog)._0;
    
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
    var newProgs = sns.updateExp(prog)(outVal)(newOutVal);
    
    // First solution:
    var solutionLazyList = newProgs._0;
    // solutionLazyList.ctor == "Cons" here, se we can access the head _0
    var solutionExp1 = solutionLazyList._0;
    
    // Display the first solution to the update problem.
    console.log(sns.unparse(solutionExp1))
    /*let f x = x + 1 in
      let who = "earth" in
      [f 1, """Hello @who""", <span>Nice</span>]
    */

    // Tail of solutions.
    var solutionLazyListTail = solutionLazyList._1._0();
    
    // If there is none, then solutionLazyListTail.ctor == "Nil". Else, it is "Cons" and we can access _0.
    var solutionExp2 = solutionLazyListTail._0
    console.log(sns.unparse(solutionExp2))
    /*let f x = x + 0 in
      let who = "earth" in
      [f 2, """Hello @who""", <span>Nice</span>]
     */
    // Third solution, etc.
    // var solutionLazyListTailTail = solutionLazyListTail._1._0();

We can also encode this workflow in a way that carries errors until the result must be recovered:

    var resProg = sns.parse(
      `let f x = x + 1 in
       let who = "world" in
       [f 2, """Hello @who""", <span>Nice</span>]`);
    
    var resOutVal = sns.andThen(prog => sns.evalExp(prog))(resProg);
    
    // Returns either {ctor: "Ok", _0: native val} or {ctor: "Err", _0: Conversion error message}
    // Here nativeOutVal._0 is equal to [3, "Hello world", ["span", [], [["TEXT", "Nice"]]]]
    var nativeOutVal = sns.andThen(outVal => sns.valToNative(outVal))(resOutVal);
    console.log(nativeOutVal);
    
    // Convert this Javascript value to a SNS value. This always succeed if we don't use functions.
    var newOutVal = sns.nativeToVal([2, "Hello earth", ["span", [], [["TEXT", "Nice"]]]]);
    
    // Invokes our update procedure with the given new out value.
    // The result is either {ctor: "Ok", _0: Lazy list of new exps} or {ctor: "Err", _0: Update error message}
    var newProgs =
      sns.andThen(prog =>
        sns.andThen(outVal =>
          sns.updateExp(prog)(outVal)(newOutVal))(resOutVal)
        )(resProg);
    
    // The remaining works as before.

`sns.valToHTMLSource` converts a value to a string in such a way it can be served as a web page (e.g. as the content of a http response). It takes care of

* Special characters in attributes.
* < > & in the script and style nodes.

Here is how you can use it.

    var prog = sns.parse(
      `let x = "world" in
       <span>Hello @x</span>`)._0;
    
    var outVal = sns.evalExp(prog)._0;
    
    // Returns either a
    //    {ctor:"Ok", _0: The HTML string}
    // or {ctor:"Err", _0: the error message}.
    var outHTML = sns.valToHTMLSource(outVal);
    
    // Displays '<span>Hello world</span>'
    console.log(outHTML._0);

