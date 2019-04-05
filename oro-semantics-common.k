require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
    <refs> .Map </refs>
    <extrinsics> .Map </extrinsics>
    <extrs>
      <subr multiplicity="*">
        <id> -1 </id>
        <attrs> .Map </attrs> /* "args", "returns"?, "env", "extrScope"  */
        <body> . </body>
      </subr>
      <unscannedSubrs> .List </unscannedSubrs>
    </extrs>
    <extrinsicScopes> .Map </extrinsicScopes>
    <scopeBin> 1 </scopeBin>
    <startup> true </startup>
    <context>
      <labels> .Map </labels>
      <env> .Map </env>
    </context>
    <globals> .Map </globals>
    <store> .Map </store>
    <storeBins> .Map </storeBins>
    <arch> wordSize|->64 endian|->little </arch>
    //<arch> $ARCH:Map </arch>
  </T>

  syntax ArchDetail ::= "wordSize"
                      | "endian"
  syntax Endianness ::= "little"
                      | "big"
  syntax K  ::= "readSourceSeq" "(" Sequence ")"
              | "yseqToK" "(" Datas ")"                       [macro]
              | "invalidPointer"

  syntax Data           ::= TaggedScalar
                          | FixedTypeData
                          | "null"
  syntax TaggedScalar   ::= "yval" "(" "|" String "|" String ")"
  syntax FixedTypeData  ::= String
                          | Map
                          | BSeq
                          | YamlSeq
                          | "ymap" "(" "|" String "|" Map ")"
  syntax YamlSeq        ::= "yseq" "(" "|" String "|" Datas ")"
  syntax BSeq           ::= "[" Datas "]"

  syntax Datas      ::= List{Data, ","}
                      | "datasOfYseq" "(" YamlSeq ")"         [function]
  syntax Ints ::= List{Int, ","}

  rule  <k>yseqToK( .Datas ) => . ...</k>
  rule  <k>yseqToK( M:Map, Ds ) => M ~> yseqToK( Ds ) ...</k>

  rule  datasOfYseq( yseq(|_| L) ) => L


  syntax Bool ::= "containsQMarkYval" "(" Set ")"             [function]
                | "isQMarkYval" "(" Data ")"                  [function]
  rule  containsQMarkYval(.Set) => false
  rule  containsQMarkYval(SetItem(I) Is) => isQMarkYval(I) orElseBool containsQMarkYval(Is)
  rule  isQMarkYval( yval(|"?"| _) ) => true
  rule  isQMarkYval( yval(|T| _) ) => false
        requires T =/=String "?"
  rule  isQMarkYval( _:FixedTypeData ) => false

  syntax Bool ::= "isGenericMapping" "(" Data ")"             [function]
  rule  isGenericMapping(ymap(|"?"| _)) => true
  rule  isGenericMapping(ymap(|"!"| _)) => true
  rule  isGenericMapping(ymap(|"tag:yaml.org,2002:map"| _)) => true
  rule  isGenericMapping(_) => false      [owise]

  syntax Bool ::= "isGenericSequence" "(" Data ")"            [function]
  rule  isGenericSequence(yseq(|"?"| _)) => true
  rule  isGenericSequence(yseq(|"!"| _)) => true
  rule  isGenericSequence(yseq(|"tag:yaml.org,2002:seq"| _)) => true
  rule  isGenericSequence(_) => false     [owise]
endmodule
