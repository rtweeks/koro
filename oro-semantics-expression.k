require "oro-semantics-common.k"
require "oro-semantics-types.k"
require "oro-semantics-variables.k"

module ORO-SEMANTICS-EXPRESSION
  imports ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-TYPES
  imports ORO-SEMANTICS-VARIABLES
  imports MINT

  syntax Bool ::= "isScalarExprTag" "(" String ")"                [function]
  syntax K  ::= "evaluate" "(" DataType ":" Data "," K ")"

  // Literal values
  rule  evaluate(string_type : S:String, Tag ) => Tag ~> S
  rule  evaluate(string_type : yval(|"tag:yaml.org,2002:str"| S), Tag) => evaluate(string_type : S, Tag)
  rule  evaluate(bool_type : yval(|ST| V), Tag) => Tag ~> true
        requires ST in (SetItem("?") SetItem("tag:yaml.org,2002:bool"))
        andBool V in (SetItem("true") SetItem("True") SetItem("TRUE"))
  rule  evaluate(bool_type : yval(|ST| V), Tag) => Tag ~> false
        requires ST in (SetItem("?") SetItem("tag:yaml.org,2002:bool"))
        andBool V in (SetItem("false") SetItem("False") SetItem("FALSE"))
  rule  evaluate(int_type(unsigned Bits) : yval(|"?"| ValStr), Tag) => Tag ~> mi(Bits, String2Int(ValStr))
        requires notBool uoverflowMInt(Bits, String2Int(ValStr))
  rule  evaluate(int_type(unsigned Bits) : yval(|"tag:yaml.org,2002:int"| ValStr), Tag) => Tag ~> mi(Bits, String2Int(ValStr))
        requires notBool uoverflowMInt(Bits, String2Int(ValStr))
  rule  evaluate(int_type(signed Bits) : yval(|"?"| ValStr), Tag) => Tag ~> mi(Bits, String2Int(ValStr))
        requires notBool soverflowMInt(Bits, String2Int(ValStr))
  rule  evaluate(int_type(signed Bits) : yval(|"tag:yaml.org,2002:int"| ValStr), Tag) => Tag ~> mi(Bits, String2Int(ValStr))
        requires notBool soverflowMInt(Bits, String2Int(ValStr))
  rule  evaluate(float_type(_) : yval(|"?"| ValStr), Tag) => Tag ~> String2Float(ValStr)
  rule  evaluate(float_type(_) : yval(|"tag:yaml.org,2002:int"| ValStr), Tag) => Tag ~> String2Float(ValStr)
  rule  evaluate(float_type(_) : yval(|"tag:yaml.org,2002:float"| ValStr), Tag) => Tag ~> String2Float(ValStr)
  rule  evaluate(yamlnode_type : S:String, Tag) => Tag ~> S
  rule  evaluate(yamlnode_type : yval(|T| S), Tag ) => Tag ~> yval(|T| S)
        requires notBool isScalarExprTag(T)


  rule  isScalarExprTag("?") => true
  rule  isScalarExprTag("!expr") => true
  rule  isScalarExprTag("tag:the-oro-project.com,2010:expr") => true
  rule  isScalarExprTag(_) => false    [owise]
endmodule
