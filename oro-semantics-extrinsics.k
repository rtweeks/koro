require "oro-semantics-expression.k"
require "oro-semantics-intrinsics.k"
require "oro-semantics-nsskeys.k"
require "oro-semantics-types.k"

module ORO-SEMANTICS-EXTRINSICS
  imports ORO-SEMANTICS-EXPRESSION
  imports ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-NSSKEYS
  imports ORO-SEMANTICS-TYPES

  syntax K  ::= "registerExtrinsics" "(" K ")"
              | "registerResolvedExtrinsic" "(" Map ")"
              | "nsskeyResolvedInExtrinsicScan"
              | "registerSubr" "(" Map ")"
              | "subrAccumArg"
              | "registerFun" "(" Map ")"
              | "nsskeyResolvedInFunArgs"
              | "registerImports" "(" Map ")"
              | "loadMacros" "(" Map ")"
              | StatementCallable

  syntax StatementCallable  ::= "subroutine" "(" VarDecls ":" K ")"
                              // TODO: function return type should be DataType
                              | "function" "(" VarDecls "->" String ":" K ")"

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

  rule  registerSubr( ("Oro/Core/Subroutine/Define" |-> yseq( | _ | Args )) ("as" |-> yseq( | _ | Body)) ) => resolveTypes(Args, subrAccumArg) ~> subroutine(.VarDecls : yseqToK( Body ))
  rule  subrAccumArg ~> D:VarDecl ~> Rest ~> subroutine(Args : Body) => Rest ~> subroutine((D,Args) : Body)
  rule  <k>subroutine(Args : Body) => . ...</k>
        <extrinsics>... .Map => varNames(Args) |-> subroutine( Args : Body )</extrinsics>
        // TODO: requires notOverridingIntrinsic(Args)
  rule  registerFun( ("Oro/Core/Subroutine/Define" |-> yseq( | _ | Args)) ("returning" |-> RetType:String) ("as" |-> yseq( | _ | Body)) ) => resolveTypes(Args, subrAccumArg) ~> function(.VarDecls -> RetType : yseqToK(Body))
  rule  registerFun( ("Oro/Core/Subroutine/Define" |-> yseq( | _ | Args)) ("returning" |-> yval(|"?"| RetType)) ("as" |-> yseq( | _ | Body)) ) => resolveTypes(Args, subrAccumArg) ~> function(.VarDecls -> RetType : yseqToK(Body))
  rule  subrAccumArg ~> D:VarDecl ~> Rest ~> function(Args -> RetType : Body) => Rest ~> function((D,Args) -> RetType : Body)
  rule  <k>function(Args -> RetType : Body) => . ...</k>
        <extrinsics>... .Map => varNames(Args) |-> function( Args -> RetType : Body )</extrinsics>
        // TODO: requires notOverridingIntrinsic(Args)
  // TODO: "Oro/Core/Subroutine/Import" "from"
  // TODO: "Oro/Core/Macro/Use from"

  // Don't know why, but the <k>...</k> notation is required here for the disolve rules
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "returning" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Macro/Use from" |-> _ ) => . ...</k>

  syntax K  ::= "generateArgEvalPlan" "(" VarDecls "," Map ")"
              | "evaluateArgs" "(" ArgEvals ")"
              | "executeSubroutine" "(" VarDecls "," Map "," K ")"
              | "actualArg" "(" Data ")"
  syntax ArgEval  ::= "evalArg" "(" Data ":" DataType "<-" K ")"
  syntax ArgEvals ::= List{ArgEval, ","}
  rule  <k> Call:Map => generateArgEvalPlan(Args, Call) ~> evaluateArgs(.ArgEvals) ~> executeSubroutine(Args, .Map, Body) ...</k>
        <extrinsics>... keys(Call) |-> subroutine(Args : Body) ...</extrinsics>

  rule  generateArgEvalPlan(.VarDecls, _) => .
  rule  generateArgEvalPlan((varDecl(Name : Type),VDs), Call) ~> evaluateArgs(AEs) => generateArgEvalPlan(VDs, Call) ~> evaluateArgs(evalArg(Name : Type <- Call[Name] ),AEs)
endmodule
