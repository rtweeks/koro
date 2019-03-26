require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
    <refs> .Map </refs>
    <extrinsics> .Map </extrinsics>
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
                          | "yseq" "(" "|" String "|" Datas ")"
                          | "ymap" "(" "|" String "|" Map ")"

  syntax Datas      ::= List{Data, ","}
  syntax Ints ::= List{Int, ","}

  rule  <k>yseqToK( .Datas ) => . ...</k>
  rule  <k>yseqToK( ymap( | _ | M ),Ds ) => M ~> yseqToK( Ds ) ...</k>


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
endmodule
