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
              | "stop" "(" K ")"
  rule  <k>("Oro/Core/Variable/Define" |-> yseq(|_| Vars) "for" |-> yseq(|_| Stmts)) => resolveTypes(Vars, typeResolvedVar) ~> varScope(.VarDecls : yseqToK(Stmts) ) ...</k>
  rule  <k>("Oro/Core/Variable/Define" |-> yseq(|_| Vars) "for" |-> yseq(|_| Stmts) "returning value of" |-> _) => resolveTypes(Vars, typeResolvedVar) ~> varScope(.VarDecls : yseqToK(Stmts) ) ...</k>
  rule  typeResolvedVar ~> D:VarDecl ~> Rest ~> varScope(L : S) => Rest ~> varScope((D,L) : S)

  /* TODO: Oro/Core/Variable/Define as expression */
endmodule
