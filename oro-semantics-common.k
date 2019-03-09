require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
    <refs> .Map </refs>
  </T>

  syntax K  ::= "readSourceSeq" "(" Sequence ")"
              | "readSource" "(" Nodes "->" K ")"
              | "resolve" "(" Node "->" Data "," ResolveCompletion ")"
              | "resolveKey" "(" Node "->" Data "," Node ")"
              | "completeResolve" "(" Data "," ResolveCompletion ")"

  syntax Data ::= String
                | "yseq" "(" "|" String "|" Datas ")"
                | "ymap" "(" "|" String "|" Map ")"
                | "null"

  syntax Datas      ::= List{Data, ","}

  syntax ResolveCompletion  ::= "append"
                              | "assocToKey" "(" Data ")"
                              | "assocWithResolved" "(" Node ")"

  rule  readSourceSeq( [ _ Ns ] ) => readSource( Ns -> . )
  rule  readSource( .Nodes -> P ) => P
  rule  readSource( ({ T Pieces },Stmts) -> P ) => resolve( { T Pieces } -> null, append ) ~> readSource( Stmts -> P )
  rule  completeResolve( ymap( | _ | DStmt ), append ) ~> readSource( Stmts -> P ) => readSource( Stmts -> (P ~> DStmt) )

  rule  resolve( ( . T Value ) -> null, C ) => completeResolve( Value , C )
        requires  (T =/=String "?")
        andBool   (T =/=String "!expr")
  rule  resolve( ( [ T Items ] ) -> null , C ) => resolve( ( [ T Items ] ) -> yseq( | T | .Datas ) , C )
  rule  resolve( ( [ _ .Nodes ] ) -> yseq( | T | L ) , C ) => completeResolve( yseq( | T | L ) , C )
  rule  resolve( ( [ _ (Item,Items) ] ) -> yseq( | T | L ) , C ) => resolve( Item -> null , append ) ~> resolve( ( [ T Items ] ) -> yseq( | T | L ) , C )
  rule  completeResolve( D , append ) ~> resolve( ( [ _ Items ] ) -> yseq( | T | L ), C ) => resolve( ( [ T Items ] ) -> yseq( | T | L,D ) , C )
  rule  resolve( ( { T Pieces } ) -> null , C ) => resolve( ( { T Pieces } ) -> ymap( | T | .Map ), C )
  rule  resolve( ( { _ .KeyValueNodePairs } ) -> ymap( | T | M ) , C ) => completeResolve( ymap( | T | M), C )
  rule  resolve( ( { _ ((Key : Value),Pairs) } ) -> ymap( | T | M ) , C ) => resolveKey( Key -> null, Value ) ~> resolve( ( { T Pairs } ) -> ymap( | T | M ), C )
  rule  resolveKey( ( . _ KeyVal ) -> null, V ) => resolve( V -> null, assocToKey(KeyVal) )
  rule  resolveKey( ( [ T Items ] ) -> null, V ) => resolve( ( [ T Items ] ) -> null, assocWithResolved(V) )
  rule  resolveKey( ( { T Pairs } ) -> null, V ) => resolve( ( { T Pairs } ) -> null, assocWithResolved(V) )
  rule  <k>resolveKey( ( & R K ) -> null, V) => resolveKey( K -> null, V ) ...</k>
        <refs>... R |-> (_ => K) ...</refs>
  rule  <k>resolveKey( ( * R ) -> null, V) => resolveKey( K -> null, V ) ...</k>
        <refs>... R |-> K ...</refs>
  rule  completeResolve( K, assocWithResolved( V ) ) => resolve( V -> null, assocToKey( K ) )
  rule  completeResolve( V, assocToKey( K ) ) ~> resolve( ( { _ Pairs } ) -> ymap( | T | M ), C ) => resolve( ( { T Pairs } ) -> ymap( | T | M[K <- V] ), C )
  rule  <k>resolve( ( & R V ) -> null , C ) => resolve( V -> null , C ) ...</k>
        <refs>... R |-> (_ => V) ...</refs>
  rule  <k>resolve( ( * R ) -> null , C ) => resolve( V -> null , C ) ...</k>
        <refs>... R |-> V ...</refs>
endmodule
