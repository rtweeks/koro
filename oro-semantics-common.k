require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> source($PGM:Sequence) </k>
  </T>

  syntax K  ::= "source" "(" K ")"
endmodule
