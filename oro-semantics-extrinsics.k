require "oro-semantics-intrinsics.k"
require "oro-semantics-nsskeys.k"

module ORO-SEMANTICS-EXTRINSICS
  imports ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-NSSKEYS

  syntax K  ::= "registerExtrinsics" "(" K ")"
              | "registerResolvedExtrinsic" "(" Map ")"
              | "nsskeyResolvedInExtrinsicScan"
              | "registerSubr" "(" Map ")"
              | "nsskeyResolvedInSubrArgs"
              | "registerFun" "(" Map ")"
              | "nsskeyResolvedInFunArgs"
              | "registerImports" "(" Map ")"
              | "loadMacros" "(" Map ")"
              | "subroutine" "(" Map ":" K ")"                [strict(2)]
              | "function" "(" Map "->" String ":" K ")"      [strict(3)]

  rule  registerExtrinsics( . ) => .
  rule  registerExtrinsics( M:Map ~> Rest ) => nonspecResolve(M, nsskeyResolvedInExtrinsicScan) ~> registerResolvedExtrinsic( .Map ) ~> registerExtrinsics( Rest )
  rule  nsskeyResolvedInExtrinsicScan ~> resolvedNonspecKey(R) ~> nonspecResolve(M, Tag) ~> registerResolvedExtrinsic( M':Map ) => nonspecResolve(M, Tag) ~> registerResolvedExtrinsic(M' R)
  rule  registerResolvedExtrinsic( M ) => registerSubr( M )
        requires  keys(M) in SetItem(skeysDefineSubr)
  rule  registerResolvedExtrinsic( M ) => registerFun( M )
        requires  keys(M) in SetItem(skeysDefineFun)
  rule  registerResolvedExtrinsic( M ) => registerImports( M )
        requires  keys(M) in SetItem(skeysImportFrom)
  rule  registerResolvedExtrinsic( M ) => loadMacros( M )
        requires  keys(M) in SetItem(skeysUseMacros)
  rule  registerResolvedExtrinsic( M ) => .
        requires notBool(
          (keys(M) in SetItem(skeysDefineSubr))
          orBool
          (keys(M) in SetItem(skeysDefineFun))
          orBool
          (keys(M) in SetItem(skeysImportFrom))
          orBool
          (keys(M) in SetItem(skeysUseMacros))
        )

  rule  registerSubr( ("Oro/Core/Subroutine/Define" |-> ymap( | _ | Args )) ("as" |-> yseq( | _ | Body)) ) => nonspecResolve(Args, nsskeyResolvedInSubrArgs) ~> subroutine(.Map : yseqToK( Body ))
        requires notBool (keys(Args) in oroIntrinsics)
  rule  nsskeyResolvedInSubrArgs ~> resolvedNonspecKey(R) ~> nonspecResolve(M, Tag) ~> subroutine(Args : Body) => nonspecResolve(M, Tag) ~> subroutine((Args R) : Body)
  rule  <k>subroutine(Args : Body) => . ...</k>
        <extrinsics>... .Map => keys(Args) |-> subroutine( Args : Body )</extrinsics>
  rule  registerFun( ("Oro/Core/Subroutine/Define" |-> ymap( | _ | Args)) ("returning" |-> RetType:String) ("as" |-> yseq( | _ | Body)) ) => nonspecResolve(Args, nsskeyResolvedInFunArgs) ~> function(.Map -> RetType : yseqToK(Body))
        requires notBool (keys(Args) in oroIntrinsics)
  rule  registerFun( ("Oro/Core/Subroutine/Define" |-> ymap( | _ | Args)) ("returning" |-> yval(|"?"| RetType)) ("as" |-> yseq( | _ | Body)) ) => nonspecResolve(Args, nsskeyResolvedInFunArgs) ~> function(.Map -> RetType : yseqToK(Body))
        requires notBool (keys(Args) in oroIntrinsics)
  rule  nsskeyResolvedInFunArgs ~> resolvedNonspecKey(R) ~> nonspecResolve(M, Tag) ~> function(Args -> RetType : Body) => nonspecResolve(M, Tag) ~> function((Args R) -> RetType : Body)
  rule  <k>function(Args -> RetType : Body) => . ...</k>
        <extrinsics>... .Map => keys(Args) |-> function( Args -> RetType : Body )</extrinsics>
  // TODO: "Oro/Core/Subroutine/Import" "from"
  // TODO: "Oro/Core/Macro/Use from"

  // Don't know why, but the <k>...</k> notation is required here for the disolve rules
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "returning" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Macro/Use from" |-> _ ) => . ...</k>
endmodule
