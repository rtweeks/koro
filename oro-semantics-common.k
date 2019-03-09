require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
  </T>

  syntax K  ::= "readSourceSeq" "(" Sequence ")"
              | "readSource" "(" Nodes "->" K ")"
              | "readStatement" "(" KeyValueNodePairs "->" Map ")"
              | "readExpr" "(" Value ":" KeyValueNodePairs "->" Map ")"
              | "readKey" "(" Value "->" Data "=" K ")"
              | "setInMap" "(" Data "->" K ")"

  syntax Data ::= String
                | "yseq" "(" "|" String "|" Datas ")"
                | "ymap" "(" "|" String "|" Map ")"
                | "null"

  syntax Datas      ::= List{Data, ","}

  rule  readSourceSeq( [ _ Ns ] ) => readSource( Ns -> . )
  rule  readSource( .Nodes -> P ) => P
  rule  readSource( ({ _ Pieces },Stmts) -> P ) => readStatement( Pieces -> .Map ) ~> readSource( Stmts -> P )

  rule  readStatement( (( Key:Value : . VTag Value ),Pieces) -> S ) => readKey( Key -> "null" = Value ) ~> readStatement( Pieces -> S )
        requires  (VTag =/=String "?")
        andBool   (VTag =/=String "!expr")
  rule  readStatement( (( Key:Value : { _ ExprPieces } ),Pieces) -> S ) => readExpr( Key : ExprPieces -> .Map ) ~> readStatement( Pieces -> S )
  rule  readStatement( .KeyValueNodePairs -> S) ~> readSource( Stmts -> P) => readSource( Stmts -> (P ~> S) )
        // TODO: This "requires" S is not an "actionable" intrinsic or macro invocation

  rule  readExpr( OK : (( Key:Value : . "!" Value ),Pieces) -> E ) => readKey(Key -> "null" = Value) ~> readExpr( OK : Pieces -> E )
  rule  readExpr( OK : (( Key:Value : { _ SubExprPieces } ),Pieces) -> E) => readExpr( Key : SubExprPieces -> .Map ) ~> readExpr( OK : Pieces -> E )
  // Disolving readExpr to read the expression key
  rule  readExpr( K : .KeyValueNodePairs -> E ) => readKey( K -> "null" = E )

  rule readKey( (. "!" S) -> "null" = E ) => setInMap( S -> E )
  rule readKey( (. "?" S) -> "null" = E ) => setInMap( S -> E )
    // TODO: This should require that S not be any parseable pattern (e.g. an
    // integer), which should each be handled by its own rule
  rule readKey( ( [ T Ns ] ) -> "null" = E ) => readKey( ( [ T Ns ] ) -> yseq( | T | .Datas ) = E )
  rule readKey( ( { T Es } ) -> "null" = E ) => readKey( ( { T Es } ) -> ymap( | T | .Map ) = E )
  /* TODO
  rule readKey( ( [ _ (N,Ns) ] ) -> yseq( | T | L ) = E) => ???
  rule readKey( ( { _ ((K : V),Es) } ) -> ymap( | T | M ) = E) => ???
  */
  rule readKey( ( [ _ .Nodes ] ) -> K = E) => setInMap( K -> E )
  rule readKey( ( { _ .KeyValueNodePairs } ) -> K = E) => setInMap( K -> E )
  // Disolving setInMap into a parent expression or statement
  rule setInMap( K -> V ) ~> readExpr( OK : Pieces -> E ) => readExpr( OK : Pieces -> E[K <- V] )
  rule setInMap( K -> V ) ~> readStatement( Pieces -> S ) => readStatement( Pieces -> S[K <- V] )
endmodule
