# Deuce: Lightweight Structured Editing in Sketch-n-Sketch

*April 17, 2017* <br><br>

Representing programs in plain text is extremely useful, but there are
often tedious, error-prone editing tasks that detract from the more
creative and interesting parts of the programming process. A couple of
approaches have helped mitigate this problem. On one hand, structured
editors help eliminate classes of syntax errors, but often require a
workflow that is quite different than working with plain text. On the
other hand, many IDEs augment text with automated refactorings. As the
number of refactorings grows, however, complicated hierarchies of
menus make it increasingly hard to identity and invoke a desired
transformation.

The goal of this work is to enable a workflow that enjoys the benefits
of both approaches. Specifically, programs ought to be represented in
plain text for familiar and flexible editing by expert programmers,
and the editing environment ought to provide automated support for a
variety of structured code transformations without deviating from the
text-editing workflow.

Earlier versions of Sketch-n-Sketch introduced features for
directly manipulating the output of a program.
This release ([v0.6.0](../releases/v0.6.0/)) presents Deuce,
a lightweight structured editor for directly manipulating the program text,
with automated transformations that perform a variety of programming tasks
that are tedious and error-prone with text-edits alone.
Here are some videos to demonstrate the new features
(also available as a [YouTube playlist][YouTube]).

<center>

<h3 id="01">Sketch-n-Sketch Logo</h3>

<iframe width="560" height="315" frameborder="0" allowfullscreen
  src="https://www.youtube.com/embed/DD0HRYKjAhI?list=PLWFCLxeg6NJl-q-c9VjlEypI_DXGbk9LP"
></iframe>

<br>

<h3 id="02">Target</h3>

<iframe width="560" height="315" frameborder="0" allowfullscreen
  src="https://www.youtube.com/embed/oJaRkyelA-8?list=PLWFCLxeg6NJl-q-c9VjlEypI_DXGbk9LP"
></iframe>

<br>

<h3 id="03">Battery Icon</h3>

<iframe width="560" height="315" frameborder="0" allowfullscreen
  src="https://www.youtube.com/embed/aWZGN43wkME?list=PLWFCLxeg6NJl-q-c9VjlEypI_DXGbk9LP"
></iframe>

<br>

<h3 id="04">Coffee Mug</h3>

<iframe width="560" height="315" frameborder="0" allowfullscreen
  src="https://www.youtube.com/embed/2ndEUIeCyqI?list=PLWFCLxeg6NJl-q-c9VjlEypI_DXGbk9LP"
></iframe>

<br>

<h3 id="05">Mondrian Arch</h3>

<iframe width="560" height="315" frameborder="0" allowfullscreen
  src="https://youtube.com/embed/jsOvJqWpi3o?list=PLWFCLxeg6NJl-q-c9VjlEypI_DXGbk9LP"
></iframe>

NOTE: This video is not narrated.

<br>

[YouTube]: https://www.youtube.com/playlist?list=PLWFCLxeg6NJl-q-c9VjlEypI_DXGbk9LP
