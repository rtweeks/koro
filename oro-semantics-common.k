require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
  </T>

  syntax K  ::= "readSourceSeq" "(" Sequence ")"
              | "readSource" "(" Nodes "->" K ")"
              | "readStatement" "(" KeyValueNodePairs "->" Map ")"
              | "executeProgram"

  rule  readSourceSeq( [ _ Ns ] ) => readSource( Ns -> . )
  rule  readSource( .Nodes -> P ) => P
  rule  readSource( ({ _ StmtPairs },Stmts) -> P ) => readStatement( StmtPairs -> .Map ) ~> readSource( Stmts -> P )

  rule  readStatement( (( . _ Key : . VTag Value ),StmtPairs) -> S ) => readStatement( StmtPairs -> S[Key <- Value] )
        requires  (VTag =/=String "?")
        andBool   (VTag =/=String "!expr")
  rule  readStatement( .KeyValueNodePairs -> S) ~> readSource( Stmts -> P) => readSource( Stmts -> (P ~> S) )
        // TODO: This "requires" S is not an "actionable" intrinsic or macro invocation

endmodule
