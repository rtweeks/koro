require "oro-semantics-expression.k"
require "oro-semantics-intrinsics.k"
require "oro-semantics-nsskeys.k"
require "oro-semantics-types.k"
require "oro-semantics-variables.k"

module ORO-SEMANTICS-EXTRINSICS
  imports ORO-SEMANTICS-EXPRESSION
  imports ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-NSSKEYS
  imports ORO-SEMANTICS-TYPES
  imports ORO-SEMANTICS-VARIABLES

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

  /*
    subroutine and function capture <env/> after the first semicolon,
    <extrinsics/> after the second, and the subroutine body as the final
    argument
  */
  syntax StatementCallable  ::= "subroutine" "(" VarDecls ";" Map ";" Map ":" K ")"
                              // TODO: function return type should be DataType
                              | "function" "(" VarDecls "->" String ";" Map ";" Map ":" K ")"

  rule  registerExtrinsics( . ) => .
  rule  registerExtrinsics( yseqToK( .Datas ) ) => .
  rule  registerExtrinsics( yseqToK( D, Ds ) ) => registerExtrinsics( D ) ~> registerExtrinsics( yseqToK( Ds ) )
  rule  registerExtrinsics( ymap(|T| M) ) => registerExtrinsics( M )
        requires  T ==String "?"
        orBool    T ==String "!"
        orBool    T ==String "tag:yaml.org,2002:map"
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

  rule  <k>registerSubr( ("Oro/Core/Subroutine/Define" |-> yseq( | _ | Args )) ("as" |-> yseq( | _ | Body)) ) => resolveTypes(Args, subrAccumArg) ~> subroutine(.VarDecls ; E ; XS : yseqToK( Body )) ...</k>
        <env> E </env>
        <extrinsics> XS </extrinsics>
  rule  subrAccumArg ~> D:VarDecl ~> Rest ~> subroutine(Args; E; XS : Body) => Rest ~> subroutine((D,Args); E; XS : Body)
  rule  <k>subroutine(Args; E; XS : Body) => . ...</k>
        <extrinsics>... .Map => varNames(Args) |-> subroutine( Args; E; XS : Body )</extrinsics>
        // TODO: requires notOverridingIntrinsic(Args)
  rule  <k>registerFun( ("Oro/Core/Subroutine/Define" |-> yseq( | _ | Args)) ("returning" |-> RetType:String) ("as" |-> yseq( | _ | Body)) ) => resolveTypes(Args, subrAccumArg) ~> function(.VarDecls -> RetType; E; XS : yseqToK(Body)) ...</k>
        <env> E </env>
        <extrinsics> XS </extrinsics>
  rule  <k>registerFun( ("Oro/Core/Subroutine/Define" |-> yseq( | _ | Args)) ("returning" |-> yval(|"?"| RetType)) ("as" |-> yseq( | _ | Body)) ) => resolveTypes(Args, subrAccumArg) ~> function(.VarDecls -> RetType; E; XS : yseqToK(Body)) ...</k>
        <env> E </env>
        <extrinsics> XS </extrinsics>
  rule  subrAccumArg ~> D:VarDecl ~> Rest ~> function(Args -> RetType; E; XS : Body) => Rest ~> function((D,Args) -> RetType; E; XS : Body)
  rule  <k>function(Args -> RetType; E; XS : Body) => . ...</k>
        <extrinsics>... .Map => varNames(Args) |-> function( Args -> RetType; E; XS : Body )</extrinsics>
        // TODO: requires notOverridingIntrinsic(Args)
  // TODO: "Oro/Core/Subroutine/Import" "from"
  // TODO: "Oro/Core/Macro/Use from"

  // Don't know why, but the <k>...</k> notation is required here for the disolve rules
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "returning" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Macro/Use from" |-> _ ) => . ...</k>

  syntax K  ::= "generateArgEvalPlan" "(" VarDecls "," Map ")"
              | "evaluateArgs" "(" ArgEvals ")"
              | "executeSubroutine" "(" VarDecls "," Map /* argVals */ "," Map /* env */ "," Map /* extrinsics */ "," K /* body */ ")"
              | "actualArg" "(" Data ")"
  syntax ArgEval  ::= "evalArg" "(" Data ":" DataType "<-" K ")"
  syntax ArgEvals ::= List{ArgEval, ","}
  rule  <k> Call:Map => generateArgEvalPlan(Args, Call) ~> evaluateArgs(.ArgEvals) ~> executeSubroutine(Args, .Map, E, XS, Body) ...</k>
        <extrinsics>... keys(Call) |-> subroutine(Args; E; XS : Body) ...</extrinsics>
  //rule  <k> Call:Map => generateArgEvalPlan(Args, Call) ~> evaluateArgs(.ArgEvals) ~> executeSubroutine((varDecl("result" : RetType),Args), .Map, E, XS, Body) ...</k>
  //      <extrinsics>... keys(Call) |-> function(Args -> RetType; E; XS : Body) ...</extrinsics>

  rule  generateArgEvalPlan(.VarDecls, _) => .
  rule  generateArgEvalPlan((varDecl(Name : Type),VDs), Call) ~> evaluateArgs(AEs) => generateArgEvalPlan(VDs, Call) ~> evaluateArgs(evalArg(Name : Type <- Call[Name] ),AEs)

  rule  evaluateArgs(.ArgEvals) => .
  rule  evaluateArgs(AE,AEs) => AE ~> evaluateArgs(AEs)
  rule  evalArg(N : T <- Expr) => evaluate(T : Expr, actualArg(N))
  rule  actualArg(N) ~> V:Data ~> Rest ~> executeSubroutine(Args, ArgVals, E, XS, Body) => Rest ~> executeSubroutine(Args, ArgVals[N <- V], E, XS, Body)

  /*
    --- What does subroutine execution look like (once we have values to assign each argument)? ---

    We have to:
    * Reset <env/> and <extrinsics/> to only contain globals visible at the subroutine's definition
    * Empty the labels, but store for recovery on exit
    * Register any extrinsics declared in the subroutine body
    * Enter a variable scope to create variables for all the arguments, which may involve changing <startup/>
    * Assign each of the variables its previously computed value
    * Execute all body statements
    * Exit the variable scope, which may involve restoring <startup/>
  */
  syntax K  ::= "enterSubroutine" "(" Map /* extrinsics */ "," Map /* env */ ")"
              | "assignArguments" "(" Map ")"
              | "assignArgument"
              | "exitSubroutine" "(" Map /* extrinsics */ "," Map /* labels */ ")"
  rule  <k>executeSubroutine(Args, ArgVals, E', XS', Body) =>
            enterSubroutine(XS', E')
            ~> registerExtrinsics(Body)
            ~> enterScope(Args, ScopeBin)
            ~> assignArguments(ArgVals)
            ~> Body
            ~> exitScope(ScopeBin, E)
            ~> exitSubroutine(XS, L) ...</k>
        <scopeBin> ScopeBin => ScopeBin +Int 1 </scopeBin>
        <storeBins>... .Map => (ScopeBin |-> .Ints) ...</storeBins>
        <env> E </env>
        <extrinsics> XS </extrinsics>
        <labels> L </labels>

  rule  <k>enterSubroutine( XS, E ) => . ...</k>
        <env> _ => E </env>
        <extrinsics> _ => XS </extrinsics>
        <labels> _ => .Map </labels>

  rule  assignArguments( .Map ) => .
  rule  assignArguments( ArgVals ) => assignArgument ~> choice(ArgVals) ~> assignArguments( ArgVals )
        requires size(ArgVals) >Int 0
  rule  <k>assignArgument ~> N ~> assignArguments( ArgVals ) => assignArguments( ArgVals[N <- undef] ) ...</k>
        <env>... N |-> var(_, opaque(L)) ...</env>
        <store>... L |-> (_ => ArgVals[N]) ...</store>
  /* TODO: Handle <env>... N |-> var(_, bytes(L, 0)) ...</env> */

  rule  <k>exitSubroutine(XS, L) => . ...</k>
        <extrinsics> _ => XS </extrinsics>
        <labels> _ => L </labels>
endmodule
