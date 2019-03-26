require "oro-semantics-types.k"

module ORO-SEMANTICS-VARIABLES
  imports ORO-SEMANTICS-TYPES

  /*
  General theory, for statement:
  * Create variables at beginning of scope
  * Put statements from "for" at the front of the computation, followed by
  * A token to dump the variable scope (and leave the identified variable's value as the expression value, when invoked as an expression), followed by
  * Whatever computation was there after the "define...for"
  */

  syntax K  ::= "varScope" "(" VarDecls ":" K ")"
              | "varScopeExpr" "(" VarDecls ":" K "->" Data ")"
              | "typeResolvedVar"
              | Variable
  syntax Variable ::= "var" "(" DataType "," Storage ")"
                    | "initialValue" "(" OpaqueDataType ")"       [function]

  // An "opaque" storage just references the key for accessing <store>
  // A "bytes" storage includes the key for accessing <store> and the offset into the bytes within the storage
  syntax Storage  ::= BytesStorage
                    | "opaque" "(" Int ")"
  syntax BytesStorage ::= "bytes" "(" Int "," Int ")"

  rule  <k>("Oro/Core/Variable/Define" |-> yseq(|_| Vars) "for" |-> yseq(|_| Stmts)) => resolveTypes(Vars, typeResolvedVar) ~> varScope(.VarDecls : yseqToK(Stmts) ) ...</k>
  rule  <k>("Oro/Core/Variable/Define" |-> yseq(|_| Vars) "for" |-> yseq(|_| Stmts) "returning value of" |-> _) => resolveTypes(Vars, typeResolvedVar) ~> varScope(.VarDecls : yseqToK(Stmts) ) ...</k>
  rule  typeResolvedVar ~> D:VarDecl ~> Rest ~> varScope(L : S) => Rest ~> varScope((D,L) : S)

  /* TODO: Oro/Core/Variable/Define as expression */

  syntax K  ::= "enterScope" "(" VarDecls "," Int ")"
              | "exitScope" "(" Int "," Map ")"
              | "consStoreBin" "(" Int "," Int ")"
              | "killSKeys" "(" Ints ")"
  rule  <k>varScope(Decls: Stmts) => enterScope(Decls, 0) ~> Stmts ~> exitScope(0, E) ...</k>
        <env> E </env>
        <startup> true </startup>
  rule  <k>varScope(Decls: Stmts) => enterScope(Decls, ScopeBin) ~> Stmts ~> exitScope(ScopeBin, E) ...</k>
        <scopeBin> ScopeBin => ScopeBin +Int 1 </scopeBin>
        <storeBins>... .Map => (ScopeBin |-> .Ints) ...</storeBins>
        <env> E </env>
        <startup> false </startup>

  rule  initialValue( memaddr_type ) => invalidPointer
  rule  initialValue( string_type ) => ""
  rule  initialValue( sequence_type ) => yseq(|"?"| .Datas)
  rule  initialValue( mapping_type ) => ymap(|"?"| .Map)
  rule  initialValue( yamlnode_type ) => yval(|"tag:yaml.org,2002:null"| "")

  rule  <k>enterScope(.VarDecls, N) => . ...</k>
        <startup> _ => (N ==Int 0)</startup>
  rule  <k>enterScope((varDecl(Name: (T:OpaqueDataType)),Ds), ScopeBin) => consStoreBin(ScopeBin, !N) ~> enterScope(Ds, ScopeBin) ...</k>
        <env> E => E[Name <- var(T, opaque(!N))] </env>
        <store>... .Map => (!N:Int |-> initialValue(T)) ...</store>
        requires ScopeBin >Int 0
  rule  <k>enterScope((varDecl(Name: Type),Ds), ScopeBin) => consStoreBin(ScopeBin, !N) ~> enterScope(Ds, ScopeBin) ...</k>
        <env> E => E[Name <- var(Type, bytes(!N, 0))] </env>
        <store>... .Map => (!N:Int |-> initMemory(sizeDataType(Type))) ...</store>
        requires ScopeBin >Int 0 andBool notBool(isOpaqueType(Type))
  rule  <k>enterScope((varDecl(Name: Type),Ds), 0) => enterScope(Ds, 0) ...</k>
        <globals>... Name|->var(T, S) ...</globals>
        <env> E => E[Name <- var(T, S)] </env>
        <startup> true </startup>
  rule  <k>enterScope((varDecl(Name: (T:OpaqueDataType)),Ds), 0) => enterScope(Ds, 0) ...</k>
        <globals> G => (G  Name|->var(T, opaque(!N))) </globals>
        <env> E => E[Name <- var(T, opaque(!N))] </env>
        <store>... .Map => (!N:Int |-> initialValue(T)) ...</store>
        <startup> true </startup>
        requires notBool(Name in_keys(G))
  rule  <k>enterScope((varDecl(Name: Type),Ds), 0) => enterScope(Ds, 0) ...</k>
        <globals> G => (G  Name|->var(Type, bytes(!N, 0))) </globals>
        <env> E => E[Name <- var(Type, bytes(!N, 0))] </env>
        <store>... .Map => (!N:Int |-> initMemory(sizeDataType(Type))) ...</store>
        <startup> true </startup>
        requires notBool(isOpaqueType(Type) orBool (Name in_keys(G)))

  rule  <k>consStoreBin(0, _) => . ...</k>
  rule  <k>consStoreBin(Bin, SKey) => . ...</k>
        <storeBins>... Bin |-> (SKeys => (SKey, SKeys)) ...</storeBins>
        requires Bin >Int 0

  rule  <k>exitScope(0, E) => . ...</k>
        <env> _ => E </env>
  rule  <k>exitScope(Bin, E) => killSKeys(SKeys) ...</k>
        <storeBins>... Bin |-> SKeys ...</storeBins>
        <startup> _ => (Bin ==Int 1)</startup>
        <env> _ => E </env>
        requires Bin >Int 0

  rule  killSKeys( .Ints ) => .
  rule  <k>killSKeys(SKey, SKeys) => killSKeys(SKeys) ...</k>
        <store> S => S[SKey <- undef] </store>
endmodule
