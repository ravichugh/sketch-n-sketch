# Sketch-n-sketch Application Programming Interface

This npm packages offers a quick but simple API for dealing with the Sketch-n-sketch bidirectional evaluator in Javascript. We hope to provide enough functionalities to enable the creation of applications server that serve the Leo language, which is a superset of Elm and HTML, for web applications.

## Get familiar with Sketch-n-sketch

If you want to get familiar with Sketch-n-sketch for the language, first visit the following link:

[github.com/ravichugh/sketch-n-sketch](https://github.com/ravichugh/sketch-n-sketch)

# API usage

In *node.js* projects, run the following command to install sketch-n-sketch for your project.

    npm install sketch-n-sketch

Then, in your javascript files, place the following declaration:

    const sns = require("sketch-n-sketch");

*Browser* projects (not described here), simply include `sns-core.js` as an external script file.

The evaluation of this line will take 3 seconds the first time, the time necessary to evaluate the prelude.

## Basic API documentation

Given that `JSObj` represents the native Javascript object type to store key/value pairs, and `JSAny` any javascript value (in our case: numbers, booleans, arrays and objects), we provide the following API along with their types.

    parse   : String -> Result String Exp
    evaluate: Exp -> Result String (Val, ComputationCache)
    update  : Exp -> (Val, ComputationCache) -> Val -> Result String (LazyList Exp)
    unparse : Exp -> String
    process : Result err a -> (a -> Result err b) -> Result err b
    valToNative: Val -> JSAny
    nativeToVal: JSAny -> Val
    valToString: Val -> String
    valToHTMLSource: Val -> Result String String
    
    evaluateWithoutCache: Exp -> Result String Val
    updateWithoutCache: Exp -> Val -> Result String (LazyList Exp)
    
    lazyList: {
      nonEmpty: LayList a -> Bool
      isEmpty: LayList a -> Bool
      head: LayList a -> a
      tail: LayList a -> LazyList a
    }
    fromOk: Result err a -> a
    foldResult: (err -> b) -> (a -> b) -> Result err a -> b
    first: (a, b) -> a
    second (a, b) -> b

### Specialized API

Plug-in an environment of constants (as a JavaScript object), that can be modified in return:

    objEnv.evaluate: JSObj -> Exp -> Result String (Val, ComputationCache)
    objEnv.update  : JSObj -> Exp -> (Val, ComputationCache) -> Val -> Result String (LazyList (JSObj, Exp))
    objEnv.evaluateWithoutCache: JSObj -> Exp -> Result String Val
    objEnv.updateWithoutCache  : JSObj -> Exp -> Val -> Result String (LazyList (JSObj, Exp))

Combine parse and evaluate steps directly:

    string.evaluate: String -> Result String (Val, ComputationCache)
    string.update  : String -> (Val, ComputationCache) -> Val -> Result String (LazyList String)
    string.evaluateWithoutCache: String -> Result String Val
    string.updateWithoutCache  : String -> Val -> Result String (LazyList String)

Plug-in an environment of constants AND combine parse and evaluate (`string.objEnv.` is also a valid prefix):

    objEnv.string.evaluate: JSObj -> String -> Result String (Val, ComputationCache)
    objEnv.string.update  : JSObj -> String -> (Val, ComputationCache) -> Val -> Result String (LazyList (JSObj, String))
    objEnv.string.evaluateWithoutCache: JSObj -> String -> Result String Val
    objEnv.string.updateWithoutCache  : JSObj -> String -> Val -> Result String (LazyList (JSObj, String))

`update` and `updateWithoutCache` are doing the same thing, except that the first `(Val, ComputationCache)` of `update` should be the previously computed output and the second `Val` the new value, whereas `updateWithoutCache` only takes in the new value and recomputes the old value and the cache itself, which might take longer.

## Basic API Example usage

Here is a detailed workflow using variable assignments, expression parsing and unparsing:

    // sns.parse returns either
    //    {ctor: "Ok", _0: SNS AST of this program}
    // or {ctor:"Err", _0: Parse error message}
    var prog = sns.fromOk(sns.parse(
      `let f x = x + 1 in
       let who = "world" in
       [f 2, """Hello @who""", <span>Nice</span>]`)); // Could replace fromOk(x) by x._0
    
    // sns.evaluate returns either 
    //    {ctor: "Ok",  _0: Val}
    // or {ctor: "Err", _0: Evaluation error message}
    var outValCache = sns.fromOk(sns.evaluate(prog));
    var outVal = sns.first(outValCache);
    
    console.log(sns.valToString(outVal))
    /* Prints
        [ 3,
          "Hello world",[ "span",[],[[ "TEXT",
            "Nice"
          ]]]
        ]
    */
    
    // Returns either
    //    {ctor: "Ok",  _0: native val}
    // or {ctor: "Err", _0: Conversion error message}
    // Here nativeOutVal is equal to [3, "Hello world", ["span", [], [["TEXT", "Nice"]]]]
    var nativeOutVal = sns.fromOk(sns.valToNative(outVal));
    console.log(nativeOutVal);
    
    // Convert this Javascript value to a SNS value. This always succeed for base types.
    var newOutVal = sns.nativeToVal([2, "Hello earth", ["span", [], [["TEXT", "Nice"]]]]);
    
    // Invokes our update procedure with the given new out value.
    // The result is either
    //    {ctor: "Ok",  _0: Lazy list of new exps}
    // or {ctor: "Err", _0: Update error message}
    var newProgs = sns.update(prog)(outValCache)(newOutVal);
    
    var solutions = sns.fromOk(newProgs);
    // sns.lazyList.nonEmpty(solutions) == true
    var solutionExp1 = sns.lazyList.head(solutions);
    
    // Display the first solution to the update problem.
    console.log(sns.unparse(solutionExp1));
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

We can also encode this workflow in a way that carries errors until the result must be recovered.

    var resProg = sns.parse(
      `let f x = x + 1 in
       let who = "world" in
       [f 2, """Hello @who""", <span>Nice</span>]`);
    
    var resOutValCache = sns.process(resProg)(prog =>
      sns.evaluate(prog));
    
    // Returns either {ctor: "Ok", _0: native val} or {ctor: "Err", _0: Conversion error message}
    // Here nativeOutVal._0 is equal to [3, "Hello world", ["span", [], [["TEXT", "Nice"]]]]
    var nativeOutVal = sns.process(resOutValCache)(outValCache =>
      sns.valToNative(sns.first(outValCache)));
    console.log(nativeOutVal);
    
    // Convert this Javascript value to a SNS value. This always succeed if we don't use functions.
    var newOutVal = sns.nativeToVal([2, "Hello earth", ["span", [], [["TEXT", "Nice"]]]]);
    
    // Invokes our update procedure with the given new out value.
    // The result is either {ctor: "Ok", _0: Lazy list of new exps} or {ctor: "Err", _0: Update error message}
    var newProgs =
      sns.process(resProg)(prog =>
      sns.process(resOutValCache)(outValCache =>
        sns.update(prog)(outValCache)(newOutVal)
      ));

    // The remaining works as before.

`sns.valToHTMLSource` converts a value to a string in such a way it can be served as a web page (e.g. as the content of a http response). It takes care of

* Special characters in attributes.
* < > & in the script and style nodes.

Here is how you can use it.

    var prog = sns.parse(
      `let x = "world" in
       <span>Hello @x</span>`)._0;
    
    var outVal = sns.evaluateWithoutCache(prog)._0; // or sns.first
    
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

## How to speed up update?

There are a few kinds of expressions that Sketch-n-sketch remembers without having to recompute them, and some that it automatically recomputes. Recomputation can take a quadratic time, so beware of some pitfalls.

* Consecutive let definitions are cached.
  Every time you assign compute and assign a value to a variable, the original value of the expression can be retrieved from the variable itself.
  Except if
  * the pattern matching is a record pattern, because record extraction makes it impossible to retrieve the original value, e.g., from the cached value of `a`, we cannot obtain the original value of `A` in
    `let {a} = A in X` (`A` could evaluate to more fields).
    A work-around for this is either to define  
    `let a = A.a`  
    or add the "as" keyword to redefine A:  
    `let {a} as x = A`
  * the pattern matching is a wilcard pattern like `_`.
  This will only trigger the recomputation of the right-hand-side, but it won't propagate to anything else.
  To avoid that, just put a dummy variable to ensure it caches the result.

* Intermediate results of pipelines
  In the expression `S |> F |> G`, the intermediate results are cached because the expression is rewritten to a let definition like above.
  But the expressions F and G are not cached and will be recomputed.
* List literals, record literals, branches of if-then-else (NOT the conditional), branches of case statement (NOT the input)

So in general, it's better to name any intermediate computation result.
It's a bad idea (for now) to pipeline anything inside a branch (if or case) because the pipeline will need to be recomputed.