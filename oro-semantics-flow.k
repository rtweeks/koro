require "oro-semantics-intrinsics.k"

module ORO-SEMANTICS-FLOW
  imports ORO-SEMANTICS-INTRINSICS

  rule <k>("Oro/Core/Flow/Do" |-> yseq( | _ | L)) => yseqToK(L) ...</k>


endmodule
