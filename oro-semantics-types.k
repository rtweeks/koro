require "oro-semantics-common.k"
require "oro-semantics-nsskeys.k"

module ORO-SEMANTICS-TYPES
  imports ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-NSSKEYS

  syntax Signedness ::= "signed"
                      | "unsigned"

  syntax DataType ::= "bool_type"
                    | "string_type"
                    | "int_type" "(" Signedness Int ")"
                    | "float_type" "(" Int ")"
                    | "memaddr_type"
                    | "buffer_type" "(" Int ")"
                    | "sequence_type"
                    | "mapping_type"
                    | "yamlnode_type"

  syntax K  ::= "resolveTypes" "(" Datas "," K ")"
              | "varDefMap" "(" Map ")"
              | "varDefEntry"
              | "resolveTypeEntry"
              | "normalizeType"
              | VarDecl
              | "varNames" "(" VarDecls ")"                 [function]
              | "varNamesAccum" "(" VarDecls "->" Set ")"   [function]

  syntax VarDecl ::= "varDecl" "(" Data ":" DataType ")"
  syntax VarDecls ::= List{VarDecl, ","}

  rule  resolveTypes(.Datas, _) => .
  rule  resolveTypes((ymap(|_| VarInfo),Rest), Tag) => nonspecResolve(VarInfo, varDefEntry) ~> varDefMap(.Map) ~> resolveTypes(Rest, Tag)
  rule  varDefEntry ~> resolvedNonspecKey(R) ~> Rest ~> varDefMap(M) => Rest ~> varDefMap(M R)
  rule  varDefMap("variable" |-> Var "as" |-> Type) => resolveTypeEntry ~> Var ~> Type
  rule  varDefMap("argument" |-> Var "as" |-> Type) => resolveTypeEntry ~> Var ~> Type

  rule  resolveTypeEntry ~> yval(|"?"| Var) ~> yval(|"?"| Type) => normalizeType ~> Type ~> Var
  rule  resolveTypeEntry ~> yval(|VT| Var) ~> yval(|"?"| Type) => normalizeType ~> Type ~> yval(|VT| Var)
        requires (VT =/=String "?")
        andBool  (VT =/=String "!expr")
        andBool  (VT =/=String "tag:the-oro-project.com,2010:expr")
  rule  resolveTypeEntry ~> Var:FixedTypeData ~> yval(|"?"| Type) => normalizeType ~> Type ~> Var
  rule  resolveTypeEntry ~> yval(|"?"| Var) ~> yval(|T| Type) => normalizeType ~> yval(|T| Type) ~> Var
        requires T =/=String "?"
  rule  resolveTypeEntry ~> yval(|VT| Var) ~> yval(|T| Type) => normalizeType ~> yval(|T| Type) ~> yval(|VT| Var)
        requires (T =/=String "?")
        andBool (VT =/=String "?")
        andBool  (VT =/=String "!expr")
        andBool  (VT =/=String "tag:the-oro-project.com,2010:expr")
  rule  resolveTypeEntry ~> Var:FixedTypeData ~> yval(|T| Type) => normalizeType ~> yval(|T| Type) ~> Var
        requires T =/=String "?"
  rule  resolveTypeEntry ~> yval(|"?"| Var) ~> Type:FixedTypeData => normalizeType ~> Type ~> Var
  rule  resolveTypeEntry ~> yval(|VT| Var) ~> Type:FixedTypeData => normalizeType ~> Type ~> yval(|VT| Var)
        requires (VT =/=String "?")
        andBool  (VT =/=String "!expr")
        andBool  (VT =/=String "tag:the-oro-project.com,2010:expr")
  rule  resolveTypeEntry ~> Var:FixedTypeData ~> Type:FixedTypeData => normalizeType ~> Type ~> Var

  rule  normalizeType ~> "boolean" => bool_type

  rule  normalizeType ~> "string" => string_type

  rule  normalizeType ~> "signed 8 bit integer" => int_type(signed 8)
  rule  normalizeType ~> "s8int" => int_type(signed 8)
  rule  normalizeType ~> "unsigned 8 bit integer" => int_type(unsigned 8)
  rule  normalizeType ~> "u8int" => int_type(unsigned 8)
  rule  normalizeType ~> "signed 16 bit integer" => int_type(signed 16)
  rule  normalizeType ~> "s16int" => int_type(signed 16)
  rule  normalizeType ~> "unsigned 16 bit integer" => int_type(unsigned 16)
  rule  normalizeType ~> "u16int" => int_type(unsigned 16)
  rule  normalizeType ~> "signed 32 bit integer" => int_type(signed 32)
  rule  normalizeType ~> "s32int" => int_type(signed 32)
  rule  normalizeType ~> "unsigned 32 bit integer" => int_type(unsigned 32)
  rule  normalizeType ~> "u32int" => int_type(unsigned 32)
  rule  normalizeType ~> "signed 64 bit integer" => int_type(signed 64)
  rule  normalizeType ~> "s64int" => int_type(signed 64)
  rule  normalizeType ~> "unsigned 64 bit integer" => int_type(unsigned 64)
  rule  normalizeType ~> "u64int" => int_type(unsigned 64)
  rule  normalizeType ~> "signed 128 bit integer" => int_type(signed 128)
  rule  normalizeType ~> "s128int" => int_type(signed 128)
  rule  normalizeType ~> "unsigned 128 bit integer" => int_type(unsigned 128)
  rule  normalizeType ~> "u128int" => int_type(unsigned 128)

  rule  normalizeType ~> "32 bit variable precision number" => float_type(32)
  rule  normalizeType ~> "float32" => float_type(32)
  rule  normalizeType ~> "64 bit variable precision number" => float_type(64)
  rule  normalizeType ~> "float64" => float_type(64)
  rule  normalizeType ~> "memory address" => memaddr_type

  // TODO: Support all integer formats for tag:yaml.org,2002:int
  rule  normalizeType ~> ymap(|_| "memory buffer" |-> yval(|T| Size)) => buffer_type(String2Int(Size))
        requires T in (SetItem("?") SetItem("tag:yaml.org,2002:int"))

  // TODO: ymap(|_| "abstract subroutine" |-> ymap(|_| Keys))
  // TODO: ymap(|_| "abstract subroutine" |-> ymap(|_| Keys) "returning" |-> RetType)

  rule  normalizeType ~> "YAML sequence" => sequence_type
  rule  normalizeType ~> "YAML mapping" => mapping_type
  rule  normalizeType ~> "YAML node" => yamlnode_type

  // TODO: Requires Var not tagged as expr if scalar, not tagged ! or tag:yaml.org,2002:map if mapping
  rule  Type:DataType ~> Var:Data ~> resolveTypes(L, Tag) => Tag ~> varDecl(Var : Type) ~> resolveTypes(L, Tag)

  rule  varNames(L) => varNamesAccum(L -> .Set)
  rule  varNamesAccum(.VarDecls -> S) => S
  rule  varNamesAccum((varDecl(Name: _),Ds) -> S) => varNamesAccum(Ds -> (S SetItem(Name)))
endmodule
