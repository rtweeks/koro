require "oro-semantics-common.k"
require "oro-semantics-memory.k"
require "oro-semantics-nsskeys.k"

module ORO-SEMANTICS-TYPES
  imports ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-MEMORY
  imports ORO-SEMANTICS-NSSKEYS
  imports MINT

  syntax Signedness ::= "signed"
                      | "unsigned"

  syntax DataType ::= "bool_type"
                    | "int_type" "(" Signedness Int ")"
                    | "float_type" "(" Int ")"
                    | "buffer_type" "(" Int ")"
                    | OpaqueDataType

  syntax OpaqueDataType ::= "memaddr_type"
                          | "string_type"
                          | "sequence_type"
                          | "mapping_type"
                          | "yamlnode_type"

  syntax K  ::= "resolveTypes" "(" Datas "," K ")"
              | "resolveType" "(" Data "," K ")"
              | "varDefMap" "(" Map ")"
              | "varDefEntry"
              | "resolveTypeEntry"
              | "normalizeType" "(" K ")"
              | VarDecl
  syntax Set  ::= "varNames" "(" VarDecls ")"                 [function]
                | "varNamesAccum" "(" VarDecls "->" Set ")"   [function]
  syntax Int  ::= "sizeDataType" "(" DataType ")"             [function]
  syntax MInt ::= "extractBytesVal" "(" DataType "," MInt "," Int "," K ")" [function]
  syntax Bool ::= "typeConversionAllowed" "(" DataType "->" DataType ")"    [function]

  syntax VarDecl ::= "varDecl" "(" Data ":" DataType ")"
  syntax VarDecls ::= List{VarDecl, ","}

  rule  resolveTypes(.Datas, _) => .
  rule  resolveTypes(((VarInfo:Map),Rest), Tag)
      =>  nonspecResolve(VarInfo, varDefEntry)
          ~> varDefMap(.Map)
          ~> resolveTypes(Rest, Tag)
  rule  varDefEntry ~> resolvedNonspecKey(R) ~> Rest ~> varDefMap(M)
      => Rest ~> varDefMap(M R)
  rule  varDefMap("variable" |-> Var "as" |-> Type)
      => resolveTypeEntry ~> Var ~> Type
  rule  varDefMap("argument" |-> Var "as" |-> Type)
      => resolveTypeEntry ~> Var ~> Type

  rule  resolveTypeEntry ~> yval(|"?"| Var) ~> yval(|"?"| Type)
      => normalizeType(varDefEntry) ~> Type ~> Var
  rule  resolveTypeEntry ~> yval(|VT| Var) ~> yval(|"?"| Type)
      => normalizeType(varDefEntry) ~> Type ~> yval(|VT| Var)
        requires (VT =/=String "?")
        andBool  (VT =/=String "!expr")
        andBool  (VT =/=String "tag:the-oro-project.com,2010:expr")
  rule  resolveTypeEntry ~> Var:FixedTypeData ~> yval(|"?"| Type)
      => normalizeType(varDefEntry) ~> Type ~> Var
  rule  resolveTypeEntry ~> yval(|"?"| Var) ~> yval(|T| Type)
      => normalizeType(varDefEntry) ~> yval(|T| Type) ~> Var
        requires T =/=String "?"
  rule  resolveTypeEntry ~> yval(|VT| Var) ~> yval(|T| Type)
      => normalizeType(varDefEntry) ~> yval(|T| Type) ~> yval(|VT| Var)
        requires (T =/=String "?")
        andBool (VT =/=String "?")
        andBool  (VT =/=String "!expr")
        andBool  (VT =/=String "tag:the-oro-project.com,2010:expr")
  rule  resolveTypeEntry ~> Var:FixedTypeData ~> yval(|T| Type)
      => normalizeType(varDefEntry) ~> yval(|T| Type) ~> Var
        requires T =/=String "?"
  rule  resolveTypeEntry ~> yval(|"?"| Var) ~> Type:FixedTypeData
      => normalizeType(varDefEntry) ~> Type ~> Var
  rule  resolveTypeEntry ~> yval(|VT| Var) ~> Type:FixedTypeData
      => normalizeType(varDefEntry) ~> Type ~> yval(|VT| Var)
        requires (VT =/=String "?")
        andBool  (VT =/=String "!expr")
        andBool  (VT =/=String "tag:the-oro-project.com,2010:expr")
  rule  resolveTypeEntry ~> Var:FixedTypeData ~> Type:FixedTypeData
      => normalizeType(varDefEntry) ~> Type ~> Var

  rule  resolveType(yval(|"?"| Type), C) => normalizeType(C) ~> Type
  rule  resolveType(yval(|T| Type), C) => normalizeType(C) ~> yval(|T| Type)
        requires T =/=String "?"
  rule  resolveType(Type:FixedTypeData, C) => normalizeType(C) ~> Type

  rule  normalizeType(C) ~> "boolean" => C ~> bool_type

  rule  normalizeType(C) ~> "string" => C ~> string_type

  rule  normalizeType(C) ~> "signed 8 bit integer" => C ~> int_type(signed 8)
  rule  normalizeType(C) ~> "s8int" => C ~> int_type(signed 8)
  rule  normalizeType(C) ~> "unsigned 8 bit integer" => C ~> int_type(unsigned 8)
  rule  normalizeType(C) ~> "u8int" => C ~> int_type(unsigned 8)
  rule  normalizeType(C) ~> "signed 16 bit integer" => C ~> int_type(signed 16)
  rule  normalizeType(C) ~> "s16int" => C ~> int_type(signed 16)
  rule  normalizeType(C) ~> "unsigned 16 bit integer" => C ~> int_type(unsigned 16)
  rule  normalizeType(C) ~> "u16int" => C ~> int_type(unsigned 16)
  rule  normalizeType(C) ~> "signed 32 bit integer" => C ~> int_type(signed 32)
  rule  normalizeType(C) ~> "s32int" => C ~> int_type(signed 32)
  rule  normalizeType(C) ~> "unsigned 32 bit integer" => C ~> int_type(unsigned 32)
  rule  normalizeType(C) ~> "u32int" => C ~> int_type(unsigned 32)
  rule  normalizeType(C) ~> "signed 64 bit integer" => C ~> int_type(signed 64)
  rule  normalizeType(C) ~> "s64int" => C ~> int_type(signed 64)
  rule  normalizeType(C) ~> "unsigned 64 bit integer" => C ~> int_type(unsigned 64)
  rule  normalizeType(C) ~> "u64int" => C ~> int_type(unsigned 64)
  rule  normalizeType(C) ~> "signed 128 bit integer" => C ~> int_type(signed 128)
  rule  normalizeType(C) ~> "s128int" => C ~> int_type(signed 128)
  rule  normalizeType(C) ~> "unsigned 128 bit integer" => C ~> int_type(unsigned 128)
  rule  normalizeType(C) ~> "u128int" => C ~> int_type(unsigned 128)

  rule  normalizeType(C) ~> "32 bit variable precision number" => C ~> float_type(32)
  rule  normalizeType(C) ~> "float32" => C ~> float_type(32)
  rule  normalizeType(C) ~> "64 bit variable precision number" => C ~> float_type(64)
  rule  normalizeType(C) ~> "float64" => C ~> float_type(64)
  rule  normalizeType(C) ~> "memory address" => C ~> memaddr_type

  // TODO: Support all integer formats for tag:yaml.org,2002:int
  rule  normalizeType(C) ~> ymap(|_| "memory buffer" |-> yval(|T| Size))
      => C ~> buffer_type(String2Int(Size))
        requires T in (SetItem("?") SetItem("tag:yaml.org,2002:int"))
  rule  normalizeType(C) ~> ymap(|_| yval(|"?"| "memory buffer") |-> yval(|T| Size))
      => C ~> buffer_type(String2Int(Size))
        requires T in (SetItem("?") SetItem("tag:yaml.org,2002:int"))

  // TODO: ymap(|_| "abstract subroutine" |-> ymap(|_| Keys))
  // TODO: ymap(|_| "abstract subroutine" |-> ymap(|_| Keys) "returning" |-> RetType)

  rule  normalizeType(C) ~> "YAML sequence" => C ~> sequence_type
  rule  normalizeType(C) ~> "YAML mapping" => C ~> mapping_type
  rule  normalizeType(C) ~> "YAML node" => C ~> yamlnode_type

  // TODO: Requires Var not tagged as expr if scalar, not tagged ! or tag:yaml.org,2002:map if mapping
  rule  varDefEntry ~> Type:DataType ~> Var:Data ~> resolveTypes(L, Tag)
      => Tag ~> varDecl(Var : Type) ~> resolveTypes(L, Tag)

  rule  varNames(L) => varNamesAccum(L -> .Set)
  rule  varNamesAccum(.VarDecls -> S) => S
  rule  varNamesAccum((varDecl(Name: _),Ds) -> S)
      => varNamesAccum(Ds -> (S SetItem(Name)))

  syntax Bool ::= "isOpaqueType" "(" DataType ")"         [function]
  rule  isOpaqueType(T:OpaqueDataType) => true
  rule  isOpaqueType(_) => false    [owise]

  rule  <k>sizeDataType( _:OpaqueDataType ) => ((Bits +Int 7) /Int 8) ...</k>
        <arch>... wordSize |-> Bits ...</arch>
  rule  sizeDataType( bool_type ) => 1
  rule  sizeDataType( int_type(_ Bits) ) => ((Bits +Int 7) /Int 8)
  rule  sizeDataType( float_type( Bits ) ) => ((Bits +Int 7) /Int 8)
  rule  sizeDataType( buffer_type( Bytes ) ) => Bytes

  rule  extractBytesVal(T, Content, Offset, Tag)
      => Tag ~> extractMInt(Content, Offset, sizeDataType(T))
endmodule
