# Implementation Notes for retarget-html Branch

The intent behind this document is to note all of the major changes we made to
the 'vanilla' Sketch-n-Sketch source to enable HTML output instead of SVG
output. Further, this document will hope 

## Target Goals

* Basically future objectives

## Theory

In this section we explain each of the concepts we wanted to employ as well as the rationale behind each one.

### Element Abstraction

* What the Element Abstraction allows: Known width/height lets Flows happen
* The pipelined style that this lends itself towards

#### Element Versions of HTML Nodes

* The general guidelines we've been following when it comes to defining the functions for each node type

#### Flow

* What flow is meant to do
* The type of direct manipulation that we're aiming for

### CSS Pseudoselectors

* Why this is an issue
* The syntax we decided to go with

## Practice

This section explains the implementation details of the above concepts.

* For each section:
  * Relevant Functions
  * Caveats for said functions
  * Why any strangeness exists
  * What hasn't been implemented yet, but is/was intended to be

### Element Abstraction in Prelude

#### Element Versions of HTML Nodes

#### Implementations of Flow

### Collection of CSS Styles
