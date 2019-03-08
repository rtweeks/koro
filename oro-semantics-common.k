require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSource </k>
    <source> $PGM:Sequence </source>
    <stmt> .Map </stmt>
    <prog> .List </prog>
  </T>

  syntax K  ::= "readSource"
              | "readStatement" "(" K ")"
              | "executeProgram"

  rule  <k> readSource => readStatement( StmtPairs ) ~> readSource </k>
        <source> [ Tag ( { _ StmtPairs },Stmts) ] => [ Tag Stmts ] </source>
        <stmt> _ => .Map </stmt>
  rule  <k> readSource => executeProgram </k>
        <source> [ _ .Nodes ] => . </source>

  rule  <k> readStatement( ( . _ Key : . VTag Value ),StmtPairs => StmtPairs ) ...</k>
        <stmt> .Map => Key |-> Value </stmt>
        requires  (VTag =/=String "?")
          andBool (VTag =/=String "!expr")
  rule  <k> readStatement( .Nodes ) => . ...</k>
        <stmt> S => . </stmt>
        <prog>... .List => ListItem(S) </prog>
        // TODO: This requires that S is not an "actionable" intrinsic

endmodule
