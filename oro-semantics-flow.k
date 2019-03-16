require "oro-semantics-intrinsics.k"
require "oro-semantics-nsskeys.k"

module ORO-SEMANTICS-FLOW
  imports ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-NSSKEYS

  syntax K  ::= "forwardJump" "(" Data ")"
              | "nsskeyResolvedForFlow"
              | "flowResolvedStmt" "(" Map ")"

  rule  <k>("Oro/Core/Flow/Do" |-> yseq( | _ | L)) => yseqToK(L) ...</k>

  rule  <k>("Oro/Core/Flow/Label" |-> D) ~> K => K</k>
        <labels>... .Map => (D |-> K)</labels>

  rule  <k>("Oro/Core/Flow/Go to label" |-> D) ~> K => K'</k>
        <labels>... D |-> K' ...</labels>
  rule  <k>("Oro/Core/Flow/Go to label" |-> D) => forwardJump( D ) ...</k>
        <labels>Ls</labels>
        requires notBool(D in_keys(Ls))

  rule  forwardJump( D ) ~> M:Map => nonspecResolve(M, nsskeyResolvedForFlow) ~> flowResolvedStmt(.Map)
  rule  nsskeyResolvedForFlow ~> resolvedNonspecKey(R) ~> nonspecResolve(M, Tag) ~> flowResolvedStmt(M') => nonspecResolve(M, Tag) ~> flowResolvedStmt(M' R)
  rule  forwardJump( D ) ~> flowResolvedStmt("Oro/Core/Flow/Label" |-> D) => .
  rule  <k>forwardJump( D ) ~> flowResolvedStmt("Oro/Core/Flow/Label" |-> D') ~> K => forwardJump( D ) ~> K</k>
        <labels>... .Map => (D' |-> K)</labels>
        requires notBool( D' in SetItem(D) )
  rule  forwardJump( D ) ~> flowResolvedStmt("Oro/Core/Flow/Do" |-> yseq( | _ | .Datas)) => .
  rule  forwardJump( D ) ~> flowResolvedStmt("Oro/Core/Flow/Do" |-> yseq( | ST | ymap( | _ | M),Ss)) => forwardJump( D ) ~> M ~> ("Oro/Core/Flow/Do" |-> yseq( | ST | Ss))
  rule  forwardJump( D ) ~> flowResolvedStmt(M) => forwardJump( D )
        requires  notBool(
          (keys(M) in SetItem(skeysLabel))
          orBool
          (keys(M) in SetItem(skeysDo))
        )
  rule  forwardJump( D ) ~> flowResolvedStmt(.Map) => forwardJump( D )
  rule  flowResolvedStmt( S ) => S

endmodule
