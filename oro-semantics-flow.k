require "oro-semantics-intrinsics.k"
require "oro-semantics-nsskeys.k"
require "oro-semantics-variables.k"

module ORO-SEMANTICS-FLOW
  imports ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-NSSKEYS
  imports ORO-SEMANTICS-VARIABLES

  syntax K  ::= "jumpContext" "(" K "," Map "," Map ")"
              | "forwardJump" "(" Data ")"
              | "nsskeyResolvedForFlow"
              | "flowResolvedStmt" "(" Map ")"

  rule  <k>("Oro/Core/Flow/Do" |-> [ L ]) => yseqToK(L) ...</k>

  rule  <k>("Oro/Core/Flow/Label" |-> D) ~> K => K</k>
        <labels> L => L[D <- jumpContext((("Oro/Core/Flow/Label" |-> D) ~> K), L, E)] </labels>
        <env> E </env>

  rule  <k>("Oro/Core/Flow/Go to label" |-> D) ~> K => NewContext</k>
        <labels>... D |-> NewContext ...</labels>
  rule  <k>jumpContext(K, L, E) ~> _ => K</k>
        <labels> _ => L </labels>
        <env> _ => E </env>
  rule  <k>("Oro/Core/Flow/Go to label" |-> D) => forwardJump( D ) ...</k>
        <labels>Ls</labels>
        requires notBool(D in keys(Ls))

  rule  forwardJump( D ) ~> M:Map
      =>  nonspecResolve(M, nsskeyResolvedForFlow)
          ~> flowResolvedStmt(.Map)
  rule  nsskeyResolvedForFlow
        ~> resolvedNonspecKey(R)
        ~> nonspecResolve(M, Tag)
        ~> flowResolvedStmt(M')
      =>  nonspecResolve(M, Tag)
          ~> flowResolvedStmt(M' R)
  rule  forwardJump( D ) ~> flowResolvedStmt("Oro/Core/Flow/Label" |-> D) => .
  rule  <k>forwardJump( D )
          ~> flowResolvedStmt("Oro/Core/Flow/Label" |-> D')
          ~> K
        =>  forwardJump( D )
            ~> K
        </k>
        <labels>... .Map => (D' |-> K)</labels>
        requires notBool( D' in SetItem(D) )
  rule  forwardJump( D )
        ~> flowResolvedStmt("Oro/Core/Flow/Do" |-> [ .Datas ]) => .
  rule  forwardJump( D )
        ~> flowResolvedStmt("Oro/Core/Flow/Do" |-> [ M:Map, Ss ])
      =>  forwardJump( D )
          ~> M
          ~> ("Oro/Core/Flow/Do" |-> [Ss])
  rule  forwardJump( D ) ~> flowResolvedStmt(M) => forwardJump( D )
        requires  notBool(
          (keys(M) in SetItem(skeysLabel))
          orBool
          (keys(M) in SetItem(skeysDo))
        )
  rule  forwardJump( D ) ~> flowResolvedStmt(.Map) => forwardJump( D )
  rule  forwardJump( D ) ~> yseqToK(.Datas) => forwardJump( D )
  rule  forwardJump( D ) ~> yseqToK(S,Ss) => forwardJump( D ) ~> S ~> yseqToK(Ss)
  rule  forwardJump( D ) ~> exitScope( B, E, S ) => exitScope( B, E, S ) ~> forwardJump( D )
  rule  flowResolvedStmt( S ) => S

endmodule
