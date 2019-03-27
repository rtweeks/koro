require "oro-semantics-common.k"

// NSSKEYS - Non-specific scalar keys

module ORO-SEMANTICS-NSSKEYS
  imports ORO-SEMANTICS-COMMON

  syntax K  ::= "nonspecResolve" "(" Map "," K ")"
              | "nonspecResolveKey"
              | "resolvedNonspecKey" "(" Map ")"

  /*
  General Pattern
  ===============

  The general pattern (found in oro-semantics-resolve.k) can be modified by
  placing the accumulator Map in a token as necessary.

  rule  <k>M:Map => nonspecResolve(M, someTag) ~> .Map ...</k>
        requires containsQMarkYval( keys(M) )
  rule  someTag ~> resolvedNonspecKey(R) ~> nonspecResolve(M, Tag) ~> M':Map => nonspecResolve(M, Tag) ~> (M' R)
  */
  rule  nonspecResolve(.Map, _) => .
  rule  nonspecResolve(M, Tag)
      =>  nonspecResolveKey
          ~> choice(M)
          ~> nonspecResolve(M, Tag)
        requires size(M) >Int 0
  rule  nonspecResolveKey ~> yval(|"?"| K) ~> nonspecResolve(M, Tag)
      =>  Tag
          ~> resolvedNonspecKey(K |-> M[yval(|"?"| K)])
          ~> nonspecResolve(M[yval(|"?"| K) <- undef], Tag)
  rule  nonspecResolveKey ~> yval(|T| K) ~> nonspecResolve(M, Tag)
      =>  Tag
          ~> resolvedNonspecKey(yval(|T| K) |-> M[yval(|T| K)])
          ~> nonspecResolve(M[yval(|T| K) <- undef], Tag)
        requires T =/=String "?"
  rule  nonspecResolveKey ~> K:FixedTypeData ~> nonspecResolve(M, Tag)
      =>  Tag
          ~> resolvedNonspecKey(K |-> M[K])
          ~> nonspecResolve(M[K <- undef], Tag)
endmodule
