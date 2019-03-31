# Oro in K Framework

This project will present the Oro programming language in K Framework semantics.  The language uses the syntax of the YAML data representation language, and the `oro-parser` subdirectory contains a Haskell program to parse YAML into a form more easily consumable by the K tools.

## Building the Parser and Interpreter

This project includes a _Rakefile_ to support building with the [Ruby "gem"
`rake`](https://github.com/ruby/rake).

## Running the Interpreter

The interpreter for this language is `koro-run` and takes the top level file of the program as its single argument.  To run, `stack` (Haskell Stack) must be installed (to support the YAML parser), as must a version of the K Framework -- current work is using release d310c7a.

## Testing

Testing support is under development.  Currently, the _Rakefile_ includes
support for selecting and running one of the test cases at a time; this
requires installation of the [HighLine gem](https://github.com/JEG2/highline).
The command to run a single test case is:

    rake test:one

## Included Libraries

In addition to the fundamental Oro functions for manipulating built-in types, `koro-run` supports (or will support) the *Standard Streams* library.
