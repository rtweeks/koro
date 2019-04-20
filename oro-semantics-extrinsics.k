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
              | "regextrNewScope"
              | "regextr" "(" K ")"
              | "regextrCaptureScope"
              | "regextrScanBodies" "(" List ")"
              | "regextrUpdateSubr" "(" Int ")"
              | "regextrRestoreOuter" "(" Map ")"
              | "registerResolvedExtrinsic" "(" Map ")"
              | "nsskeyResolvedInExtrinsicScan"
              | "registerSubr" "(" Map ")"
              | "subrReturns"
              | "subrAccumArg"
              | "registerFun" "(" Map ")"
              | "nsskeyResolvedInFunArgs"
              | "registerImport" "(" Map ")"
              | "loadMacros" "(" Map ")"
              | Subroutine
              | StatementCallableAgg

  /*
    subroutine and function capture <env/> after the first semicolon,
    <extrinsics/> after the second, and the subroutine body as the final
    argument
  */
  syntax Subroutine ::= "subroutine" "(" Int /* <subr> <id> N </id> </subr> */ ")"
  syntax StatementCallableAgg ::= "subroutine-agg" "(" VarDecls ";" Map ";" Int ":" K ")"
                                | "subroutine-returning-agg" "(" VarDecls "->" DataType ";" Map ";" Int ":" K ")"

  rule  <k>registerExtrinsics( Stmts )
          =>  regextrNewScope
              ~> regextr( Stmts )
              ~> regextrCaptureScope
        ...</k>
  rule  <k>regextrNewScope => . ...</k>
        <extrinsicScopes>... .Map => regextrNewScope |-> !Scope:Int </extrinsicScopes>
  rule  <k>regextrCaptureScope => regextrScanBodies( NewSubrs ) ...</k>
        <extrinsics> XS </extrinsics>
        <subr>...
          <attrs>... "extrScope" |-> XSKey ...</attrs>
        ...</subr>
        <extrinsicScopes>... (regextrNewScope |-> XSKey) => (XSKey |-> XS) ...</extrinsicScopes>
        <unscannedSubrs> NewSubrs => .List </unscannedSubrs>
        requires size(NewSubrs) >Int 0
  rule  <k>regextrCaptureScope => . ...</k>
        <extrinsicScopes>... (regextrNewScope |-> _) => .Map ...</extrinsicScopes>
        <unscannedSubrs> .List </unscannedSubrs>

  rule  regextrScanBodies( .List ) => .
  rule  <k>regextrScanBodies( ListItem(SubrId:Int) Rest:List )
          =>  registerExtrinsics( Body )
              ~> regextrUpdateSubr( SubrId )
              ~> regextrRestoreOuter( XS )
              ~> regextrScanBodies( Rest )
        ...</k>
        <subr>...
          <id> SubrId </id>
          <attrs>... "extrScope" |-> XSKey ...</attrs>
          <body> Body </body>
        ...</subr>
        <extrinsicScopes>... XSKey |-> XS' ...</extrinsicScopes>
        <extrinsics> XS => XS' </extrinsics>
  rule  <k>regextrUpdateSubr( SubrId ) => . ...</k>
        <subr>...
          <id> SubrId </id>
          <attrs>... "extrScope" |-> (OuterXSKey => !XSKey:Int) ...</attrs>
        ...</subr>
        <extrinsics> XS </extrinsics>
        <extrinsicScopes> XSS => XSS[!XSKey <- XS] </extrinsicScopes>
        requires XSS[OuterXSKey] =/=K XS
  rule  <k>regextrUpdateSubr( SubrId ) => . ...</k>
        <subr>...
          <id> SubrId </id>
          <attrs>... "extrScope" |-> XSKey:Int ...</attrs>
        ...</subr>
        <extrinsics> XS </extrinsics>
        <extrinsicScopes>... XSKey |-> XS ...</extrinsicScopes>
  rule  <k>regextrRestoreOuter( XS ) => . ...</k>
        <extrinsics> _ => XS </extrinsics>
  rule  regextr( . ) => .
  rule  regextr( yseqToK( .Datas ) ) => regextr( . )
  rule  regextr( yseqToK( D, Ds ) ) =>
          regextr( D )
          ~> regextr( yseqToK( Ds ) )
  rule  regextr( ymap(|T| M) ) => regextr( M )
        requires isGenericMapping( ymap(|T| M) )
  rule  regextr( M:Map ~> Rest ) =>
          nonspecResolve(M, nsskeyResolvedInExtrinsicScan)
          ~> registerResolvedExtrinsic( .Map )
          ~> regextr( Rest )
  rule  nsskeyResolvedInExtrinsicScan
        ~> resolvedNonspecKey(R)
        ~> nonspecResolve(M, Tag)
        ~> registerResolvedExtrinsic( M':Map )
      =>  nonspecResolve(M, Tag)
          ~> registerResolvedExtrinsic(M' R)
  rule  registerResolvedExtrinsic( M ) => registerSubr( M )
        requires  keys(M) in SetItem(skeysDefineSubr)
  rule  registerResolvedExtrinsic( M ) => registerFun( M )
        requires  keys(M) in SetItem(skeysDefineFun)
  rule  registerResolvedExtrinsic( M ) => registerImport( M )
        requires  keys(M) in SetItem(skeysImportFrom)
        orBool    keys(M) in SetItem(skeysImportReturningFrom)
  rule  registerResolvedExtrinsic( M ) => loadMacros( M )
        requires  keys(M) in SetItem(skeysUseMacros)
  syntax K  ::= "extrinsicRegistrationWithVars"
  rule  <k>registerResolvedExtrinsic( M:Map )
          =>  extrinsicRegistrationWithVars
              ~> M["Oro/Core/Variable/Define"]
              ~> M["for"]
        ...</k>
        <startup> true </startup>
        requires (
          (keys(M) in SetItem(skeysDefineVar))
          orBool
          (keys(M) in SetItem(skeysDefineVarRet))
        )
  rule  extrinsicRegistrationWithVars ~> [ Vars ] ~> [ StmtsList ]
      =>  resolveTypes(Vars, typeResolvedVar)
          ~> varScope(.VarDecls : regextr( yseqToK( StmtsList ) ))
  syntax K  ::= "extrinsicRegistrationSkipVars"
  rule  <k>registerResolvedExtrinsic( M:Map )
          =>  extrinsicRegistrationSkipVars
              ~> M["for"]
        ...</k>
        <startup> false </startup>
        requires (
          (keys(M) in SetItem(skeysDefineVar))
          orBool
          (keys(M) in SetItem(skeysDefineVarRet))
        )
  rule  extrinsicRegistrationSkipVars ~> [ StmtsList ]
      => regextr( yseqToK( StmtsList) )
  rule  registerResolvedExtrinsic( M:Map ) => .
        requires notBool(
          (keys(M) in SetItem(skeysDefineSubr))
          orBool
          (keys(M) in SetItem(skeysDefineFun))
          orBool
          (keys(M) in SetItem(skeysImportFrom))
          orBool
          (keys(M) in SetItem(skeysUseMacros))
          orBool
          (keys(M) in SetItem(skeysDefineVar))
          orBool
          (keys(M) in SetItem(skeysDefineVarRet))
        )

  rule  <k>registerSubr(
          ("Oro/Core/Subroutine/Define" |-> [ Args ])
          ("as" |-> [ Body ])
        ) =>
          resolveTypes(Args, subrAccumArg)
          ~> subroutine-agg(.VarDecls ; E ; XSKey : yseqToK( Body ))
        ...</k>
        <env> E </env>
        <extrinsicScopes>... regextrNewScope |-> XSKey ...</extrinsicScopes>
  rule  subrAccumArg
        ~> D:VarDecl
        ~> Rest
        ~> subroutine-agg(Args; E; XS : Body)
      =>  Rest ~> subroutine-agg((D,Args); E; XS : Body)
  rule  <k>subroutine-agg(Args; E; XS : Body) => . ...</k>
        <sourceFile> CurrentUnit </sourceFile>
        <extrinsics>... .Map => varNames(Args) |-> subroutine( !SId )</extrinsics>
        <unit>
          <filepath> CurrentUnit </filepath>
          <subrs>... .Map => varNames(Args) |-> subroutine( !SId ) </subrs>
        </unit>
        (.Bag => <subr>...
          <id> !SId:Int </id>
          <attrs>
            "args" |-> Args
            "env" |-> E
            "extrScope" |-> XS
          </attrs>
          <body> Body </body>
        ...</subr>)
        <unscannedSubrs>... .List => ListItem(!SId) </unscannedSubrs>
        // TODO: requires notOverridingIntrinsic(Args)
  rule  <k>registerFun(
          ("Oro/Core/Subroutine/Define" |-> [ Args ])
          ("returning" |-> RetType:Data)
          ("as" |-> [ Body ])
        ) =>
          resolveType(RetType, subrReturns)
          ~> resolveTypes(Args, subrAccumArg)
          ~> subroutine-agg(.VarDecls; E; XSKey : yseqToK(Body))
        ...</k>
        <env> E </env>
        <extrinsicScopes>... regextrNewScope |-> XSKey ...</extrinsicScopes>
  rule  subrReturns
        ~> RetType:DataType
        ~> ArgResolution
        ~> subroutine-agg(Vs; E; XS : Body)
      =>  ArgResolution
          ~> subroutine-returning-agg(Vs -> RetType; E; XS : Body)
  rule  subrAccumArg
        ~> D:VarDecl
        ~> Rest
        ~> subroutine-returning-agg(Args -> RetType; E; XS : Body)
      =>  Rest
          ~> subroutine-returning-agg((D,Args) -> RetType; E; XS : Body)
  rule  <k>subroutine-returning-agg(Args -> RetType; E; XS : Body) => . ...</k>
        <sourceFile> CurrentUnit </sourceFile>
        <extrinsics>...
          .Map => varNames(Args) |-> subroutine( !SId )
        </extrinsics>
        <unit>
          <filepath> CurrentUnit </filepath>
          <subrs>... .Map => varNames(Args) |-> subroutine( !SId ) </subrs>
        </unit>
        (.Bag => <subr>...
          <id> !SId:Int </id>
          <attrs>
            "args" |-> Args
            "returns" |-> RetType
            "env" |-> E
            "extrScope" |-> XS
          </attrs>
          <body> Body </body>
        ...</subr>)
        <unscannedSubrs>... .List => ListItem(!SId) </unscannedSubrs>
        // TODO: requires notOverridingIntrinsic(Args)
  // TODO: "Oro/Core/Subroutine/Import" "from"
  // TODO: "Oro/Core/Macro/Use from"

  // Don't know why, but the <k>...</k> notation is required here for the disolve rules
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Subroutine/Define" |-> _ "returning" |-> _ "as" |-> _ ) => . ...</k>
  rule  <k>( "Oro/Core/Macro/Use from" |-> _ ) => . ...</k>

  syntax K  ::= "subrCall" "(" K /* Args:VarDecls */ ";" K /* E:Map */ ";" K /* XSKey:Int */ ";" K /* Body:K */ ";" Map /* Call */ ")"
              | ReturningSubrCall
              | "generateArgEvalPlan" "(" VarDecls "," Map ")"
              | "evaluateArgs" "(" ArgEvals ")"
              | "executeSubroutine" "(" VarDecls "," Map /* argVals */ "," Map /* env */ "," Map /* extrinsics */ "," K /* body */ ")"
              | "executeReturningSubroutine" "(" VarDecls "," Map /* argVals */ "," Map /* env */ "," Map /* extrinsics */ "," K /* body */ "," K /* continuation tag */ ")"
              | "actualArg" "(" Data ")"
  syntax ReturningSubrCall  ::= "subrCall" "(" K /* Args:VarDecls */ "->" K /* RetType:DataType */ ";" K /* E:Map */ ";" K /* XSKey:Int */ ";" K /* Body:K */ ";" Map /* Call */ ")"
  syntax ArgEval  ::= "evalArg" "(" Data ":" DataType "<-" K ")"
  syntax ArgEvals ::= List{ArgEval, ","}
  syntax K  ::= "stop" "(" K ")"
  rule  <k> Call:Map => subrCall(
          Subr["args"];
          Subr["env"];
          Subr["extrScope"];
          Body;
          Call
        )
      ...</k>
      <extrinsics>... keys(Call) |-> subroutine(SubrId) ...</extrinsics>
      <subr>
        <id> SubrId </id>
        <attrs> Subr </attrs>
        <body> Body </body>
      </subr>
      requires notBool ("returns" in keys(Subr))
  rule  <k> subrCall( Args:VarDecls; E:Map; XSKey:Int; Body:K; Call) =>
        generateArgEvalPlan(Args, Call)
        ~> evaluateArgs(.ArgEvals)
        ~> executeSubroutine(Args, .Map, E, XS, Body)
      ...</k>
      <extrinsics>... keys(Call) |-> subroutine(SubrId) ...</extrinsics>
      <extrinsicScopes>... XSKey |-> XS:Map ...</extrinsicScopes>
  rule  <k> Call:Map => subrCall(
            Subr["args"] -> Subr["returns"];
            Subr["env"];
            Subr["extrScope"];
            Body;
            Call
          )
        ...</k>
        <extrinsics>... keys(Call) |-> subroutine(SubrId) ...</extrinsics>
        <subr>
          <id> SubrId </id>
          <attrs> Subr </attrs>
          <body> Body </body>
        </subr>
        requires "returns" in keys(Subr)
  rule  <k> subrCall( Args:VarDecls -> RetType:DataType; E:Map; XSKey:Int; Body:K; Call) =>
          generateArgEvalPlan(Args, Call)
          ~> evaluateArgs(.ArgEvals)
          ~> executeSubroutine((varDecl("result" : RetType), Args), .Map, E, XS, Body)
        ...</k>
        <extrinsicScopes>... XSKey |-> XS:Map ...</extrinsicScopes>

  syntax K  ::= "evaluate" "(" DataType ":" ReturningSubrCall "," K ")"
  rule  <k> evaluate(T : (Call:Map), Tag ) =>
          evaluate(T : subrCall(
            Subr["args"] -> Subr["returns"];
            Subr["env"];
            Subr["extrScope"];
            Body;
            Call
          ), Tag)
        ...</k>
        <extrinsics>... keys(Call) |-> subroutine(SubrId) ...</extrinsics>
        <subr>
          <id> SubrId </id>
          <attrs> Subr </attrs>
          <body> Body </body>
        </subr>
        requires "returns" in keys(Subr)
  rule  <k> evaluate(T : subrCall( Args:VarDecls -> T':DataType; E:Map; XSKey:Int; Body:K; Call), Tag ) =>
          generateArgEvalPlan(Args, Call)
          ~> evaluateArgs(.ArgEvals)
          ~> executeReturningSubroutine((varDecl("result" : T'), Args), .Map, E, XS, Body, Tag)
        ...</k>
        <extrinsicScopes>... XSKey |-> XS:Map ...</extrinsicScopes>
        requires typeConversionAllowed(T' -> T)

  rule  generateArgEvalPlan(.VarDecls, _) => .
  rule  generateArgEvalPlan((varDecl(Name : Type),VDs), Call)
        ~> evaluateArgs(AEs)
      =>  generateArgEvalPlan(VDs, Call)
          ~> evaluateArgs(evalArg(Name : Type <- Call[Name] ),AEs)

  rule  evaluateArgs(.ArgEvals) => .
  rule  evaluateArgs(AE,AEs) => AE ~> evaluateArgs(AEs)
  rule  evalArg(N : T <- Expr:Data) => evaluate(T : Expr, actualArg(N))
  rule  actualArg(N)
        ~> V:Data
        ~> Rest
        ~> executeSubroutine(Args, ArgVals, E, XS, Body)
      =>  Rest
          ~> executeSubroutine(Args, ArgVals[N <- V], E, XS, Body)

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

  syntax K  ::= "captureSubroutineResult" "(" K ")"
  rule  <k>executeReturningSubroutine(Args, ArgVals, E', XS', Body, Tag) =>
            enterSubroutine(XS', E')
            ~> registerExtrinsics(Body)
            ~> enterScope(Args, ScopeBin)
            ~> assignArguments(ArgVals)
            ~> Body
            ~> captureSubroutineResult(
              exitScope(ScopeBin, E)
              ~> exitSubroutine(XS, L)
              ~> Tag
            )
        ...</k>
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

  rule  <k>captureSubroutineResult(CloseScope) => CloseScope ~> RetVal ...</k>
        <env>... "result" |-> var(_, opaque(L)) ...</env>
        <store>... L |-> RetVal ...</store>
  /* TODO: Handle <env>... "result" |-> var(_, bytes(L, 0)) ...</env> */
endmodule
