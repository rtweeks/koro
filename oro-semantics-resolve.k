require "oro-semantics-common.k"
require "oro-semantics-extrinsics.k"
require "oro-semantics-nsskeys.k"

module ORO-SEMANTICS-RESOLVE
  imports ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-EXTRINSICS
  imports ORO-SEMANTICS-NSSKEYS
  imports LIST

  syntax K  ::= "resolveSource" "(" Nodes "->" K ")"
              | "nsskeyResolvedInSource"
              | "concatStatements" "(" Nodes "|" Nodes "->" Nodes ")"
              | "resolve" "(" Node "->" Data "," ResolveCompletion ")"
              | "resolveKey" "(" Node "->" Data "," Node ")"
              | "completeResolve" "(" Data "," ResolveCompletion ")"
              | "yseqReverse" "(" "|" String "|" Datas "->" Datas "," ResolveCompletion ")"

  syntax ResolveCompletion  ::= "append"
                              | "assocToKey" "(" Data ")"
                              | "assocWithResolved" "(" Node ")"

  rule  readSourceSeq( [ _ Ns ] ) => resolveSource( Ns -> . )
  rule  resolveSource( .Nodes -> P ) => registerExtrinsics( P ) ~> P
  rule  resolveSource( ({ T Pieces },Stmts) -> P ) => resolve( { T Pieces } -> null, append ) ~> resolveSource( Stmts -> P )
  rule  completeResolve( ymap( | _ | DStmt ), append ) ~> resolveSource( Stmts -> P ) => resolveSource( Stmts -> (P ~> DStmt) )
  rule  resolveSource( ([ T NStmts ],Stmts) -> P ) => concatStatements(NStmts | .Nodes -> Stmts) ~> resolveSource( .Nodes -> P )
  rule  concatStatements((S,Ss) | Rs -> Stmts) => concatStatements(Ss | (S,Rs) -> Stmts)
  rule  concatStatements(.Nodes | (S,Rs) -> Stmts) => concatStatements(.Nodes | Rs -> (S,Stmts))
  rule  concatStatements(.Nodes | .Nodes -> Stmts) ~> resolveSource( .Nodes -> P ) => resolveSource( Stmts -> P )

  rule  resolve( ( . T Value ) -> null, C ) => completeResolve( Value , C )
        requires  (T ==String "!")
        orBool    (T ==String "tag:yaml.org,2002:str")
  rule  resolve( ( . T Value ) -> null, C ) => completeResolve( yval( | T | Value ) , C )
        requires  (T =/=String "!")
        andBool   (T =/=String "tag:yaml.org,2002:str")
  /* TODO: Non-string scalar types */

  rule  resolve( ( [ T Items ] ) -> null , C ) => resolve( ( [ T Items ] ) -> yseq( | T | .Datas ) , C )
  // Because we can only cons into L, we need to reverse L when creating the completeResolve
  rule  resolve( ( [ _ .Nodes ] ) -> yseq( | T | L ) , C ) => yseqReverse( | T | L -> .Datas, C )
  rule  yseqReverse( | T | (I,Is) -> L, C ) => yseqReverse( | T | Is -> (I,L), C)
  rule  yseqReverse( | T | .Datas -> L, C ) => completeResolve( yseq( | T | L ) , C )
  rule  resolve( ( [ _ (Item,Items) ] ) -> yseq( | T | L ) , C ) => resolve( Item -> null , append ) ~> resolve( ( [ T Items ] ) -> yseq( | T | L ) , C )
  rule  completeResolve( D , append ) ~> resolve( ( [ _ Items ] ) -> yseq( | T | L ), C ) => resolve( ( [ T Items ]) -> yseq( | T | D,L ), C )
  rule  resolve( ( { T Pieces } ) -> null , C ) => resolve( ( { T Pieces } ) -> ymap( | T | .Map ), C )
  rule  resolve( ( { _ .KeyValueNodePairs } ) -> ymap( | T | M ) , C ) => completeResolve( ymap( | T | M), C )
  rule  resolve( ( { _ ((Key : Value),Pairs) } ) -> ymap( | T | M ) , C ) => resolveKey( Key -> null, Value ) ~> resolve( ( { T Pairs } ) -> ymap( | T | M ), C )
  // rule  resolveKey( ( . _ KeyVal ) -> null, V ) => resolve( V -> null, assocToKey(KeyVal) )
  rule  resolveKey( ( . T KeyVal ) -> null, V ) => resolve( ( . T KeyVal ) -> null, assocWithResolved(V) )
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

  rule  <k>M:Map => nonspecResolve(M, nsskeyResolvedInSource) ~> .Map ...</k>
        requires containsQMarkYval( keys(M) )
  rule  nsskeyResolvedInSource ~> resolvedNonspecKey(R) ~> nonspecResolve(M, Tag) ~> M':Map => nonspecResolve(M, Tag) ~> (M' R)
endmodule
