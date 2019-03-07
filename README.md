# Oro in K Framework

This project will present the Oro programming language in K Framework semantics.  The language uses the syntax of the YAML data representation language, and the `oro-parser` subdirectory contains a Haskell program to parse YAML into a form more easily consumable by the K tools.

## Running the Interpreter

The interpreter for this language is `koro-run` and takes the top level file of the program as its single argument.  To run, `stack` (Haskell Stack) must be installed (to support the YAML parser), as must a version of the K Framework -- current work is using release d310c7a.

## Included Libraries

In addition to the fundamental Oro functions for manipulating built-in types, `koro-run` supports (or will support) the *Standard Streams* library.
