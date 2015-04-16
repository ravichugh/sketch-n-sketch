# Parsing and Syncing in a Simple Case

In this portion of the project, we implement parsing and syncing of a program
with its output in the case of a simple language. The specification of the
simple language and the desired behavior is below.

## Language
The syntax of the language will allow for the designation of tuples, integer 
constants, and addition. They are designated as follows:

- (+) is the addition operator. It is a prefix operator and only allows addition
  of two integer constants or expressions that evaluate to integer constants.
- (,) is the tuple operator. It is a prefix operator and allows grouping of
  either integer constants or other tuple expressions.
- Integer constants are expressed as base ten numbers with the standard 0-9
  characters.

Further, parentheses may be used to allow for grouping of otherwise ambiguous
statements. For example:

```
Cryptic:
(,) (+) 2 4 (+) 3 (+) 6 7

More clear:
(,) ((+) 2 4) ((+) 3 ((+) 6 7))
```

## Desired Behavior
An interface to manipulate programs and their output should match the following
specifications:

- Display both the textual representation of the program as well as its output
  in the same window.
- Allow for manipulation of the textual representation that, when requested by
  the user, changes the representation of the output.
- Allow for manipulation of the output that, when requested by the user, changes
  the textual representation of the program in such a way that minimizes the
  change to the program and the output but is still sufficiently similar to the
  specified output.

As discussed, this will likely take the form of a two-paned window with the
program text on the left and the program output on the right.
