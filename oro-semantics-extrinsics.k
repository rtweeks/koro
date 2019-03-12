require "oro-semantics-intrinsics.k"

module ORO-SEMANTICS-EXTRINSICS
  imports ORO-SEMANTICS-INTRINSICS

  syntax K  ::= "registerExtrinsics" "(" K ")"
              | "registerSubr" "(" Map ")"
              | "registerFun" "(" Map ")"
              | "registerImports" "(" Map ")"
              | "loadMacros" "(" Map ")"
              | "subroutine" "(" Map ":" K ")"                [strict(2)]
              | "function" "(" Map "->" String ":" K ")"      [strict(3)]

  rule  registerExtrinsics( . ) => .
  rule  <k>registerExtrinsics( M:Map ~> K ) => registerSubr( M ) ~> registerExtrinsics( K ) ...</k>
        requires  keys(M) in SetItem(skeysDefineSubr)
  rule  <k>registerExtrinsics( M:Map ~> K ) => registerFun( M ) ~> registerExtrinsics( K ) ...</k>
        requires  keys(M) in SetItem(skeysDefineFun)
  rule  <k>registerExtrinsics( M:Map ~> K ) => registerImports( M ) ~> registerExtrinsics( K ) ...</k>
        requires  keys(M) in SetItem(skeysImportFrom)
  rule  <k>registerExtrinsics( M:Map ~> K ) => loadMacros( M ) ~> registerExtrinsics( K ) ...</k>
        requires  keys(M) in SetItem(skeysDefineFun)

  rule  registerExtrinsics( M:Map ~> K ) => registerExtrinsics( K )
        requires notBool(
          (keys(M) in SetItem(skeysDefineSubr))
          orBool
          (keys(M) in SetItem(skeysDefineFun))
          orBool
          (keys(M) in SetItem(skeysImportFrom))
          orBool
          (keys(M) in SetItem(skeysUseMacros))
        )

  rule  <k>registerSubr( ("Oro/Core/Subroutine/Define" |-> ymap( | _ | Args)) ("as" |-> yseq( | _ | Body)) ) => . ...</k>
        <extrinsics>... .Map => keys(Args) |-> subroutine( Args : yseqToK( Body ) )</extrinsics>
        requires notBool (keys(Args) in oroIntrinsics)
  rule  <k>registerFun( ("Oro/Core/Subroutine/Define" |-> ymap( | _ | Args)) ("returning" |-> expr( RetType )) ("as" |-> yseq( | _ | Body)) ) => . ...</k>
        <extrinsics>... .Map => keys(Args) |-> function( Args -> RetType : yseqToK( Body ) )</extrinsics>
        requires notBool (keys(Args) in oroIntrinsics)
  // TODO: "Oro/Core/Subroutine/Import" "from"
  // TODO: "Oro/Core/Macro/Use from"

  // Don't know why, but the <k>...</k> notation is required here for the disolve rules
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "returning" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Macro/Use from" |-> _ ) => . ...</k>
endmodule
