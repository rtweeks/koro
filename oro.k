require "oro-semantics-flow.k"
require "oro-semantics-intrinsics.k"
require "oro-semantics-resolve.k"
require "oro-semantics-variables.k"

module ORO
  imports ORO-SEMANTICS-FLOW
  imports ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-RESOLVE
  imports ORO-SEMANTICS-VARIABLES

  syntax K  ::= "extrinsicCall" "(" Map ")"
              | "subrCall" "(" Map "," Map "," K ")"

  // rule  <k>M:Map => extrinsicCall(M) ...</k>
  //       requires  notBool oroIsIntrinsic(M)

  // rule  <k>extrinsicCall(M) => subrCall(M, ArgTypes, Body)</k>
  //       <extrinsics>... keys(M) |-> subroutine(ArgTypes : Body) ...</extrinsics>
endmodule
