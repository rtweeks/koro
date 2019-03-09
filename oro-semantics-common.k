require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
  </T>

  syntax K  ::= "readSourceSeq" "(" Sequence ")"
              | "readSource" "(" Nodes "->" K ")"
              | "readStatement" "(" KeyValueNodePairs "->" Map ")"
              | "readExpr" "(" K ":" KeyValueNodePairs "->" Map ")"
              | "executeProgram"

  rule  readSourceSeq( [ _ Ns ] ) => readSource( Ns -> . )
  rule  readSource( .Nodes -> P ) => P
  rule  readSource( ({ _ Pieces },Stmts) -> P ) => readStatement( Pieces -> .Map ) ~> readSource( Stmts -> P )

  rule  readStatement( (( . _ Key : . VTag Value ),Pieces) -> S ) => readStatement( Pieces -> S[Key <- Value] )
        requires  (VTag =/=String "?")
        andBool   (VTag =/=String "!expr")
  rule  readStatement( (( . _ Key : { _ ExprPieces } ),Pieces) -> S ) => readExpr( Key : ExprPieces -> .Map ) ~> readStatement( Pieces -> S )
  rule  readStatement( .KeyValueNodePairs -> S) ~> readSource( Stmts -> P) => readSource( Stmts -> (P ~> S) )
        // TODO: This "requires" S is not an "actionable" intrinsic or macro invocation

  rule  readExpr( OK : (( . _ Key : . "!" Value ),Pieces) -> E ) => readExpr( OK : Pieces -> E[Key <- Value] )
  rule  readExpr( OK : (( . _ Key : { _ SubExprPieces } ),Pieces) -> E) => readExpr( Key : SubExprPieces -> .Map ) ~> readExpr( OK : Pieces -> E )
  rule  readExpr( IK : .KeyValueNodePairs -> SE ) ~> readExpr( OK : Pieces -> E ) => readExpr( OK : Pieces -> E[IK <- SE])
  rule  readExpr( K : .KeyValueNodePairs -> E ) ~> readStatement( Pieces -> S ) => readStatement( Pieces -> S[K <- E])

endmodule
