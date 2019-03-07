/*
  The syntax format here is a isomorphic simplification of the YAML format.
  The oro-parser subdirectory of this project provides a Haskell program
  to read YAML and produce the intermediate form *kast* would generate from
  the semantic equivalent in the simplified notation.
*/
module ORO-SYNTAX
  syntax  Node      ::= Value
                      | "&" Int Value
                      | "*" Int
  syntax  Value     ::= Scalar
                      | Sequence
                      | Mapping
  syntax  Scalar    ::= "." String String
  syntax  Sequence  ::= "[" String Nodes "]"
  syntax  Mapping   ::= "{" String KeyValueNodePairs "}"

  syntax  Nodes             ::= List{Node, ","}
  syntax  KeyValueNodePairs ::= List{KeyValueNodePair, ","}
  syntax  KeyValueNodePair  ::= Node ":" Node
endmodule
