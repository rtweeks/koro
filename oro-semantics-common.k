require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> unpackSourcePath( $MFPATH:Scalar ) ~> readSourceSeq( $PGM:Sequence ) </k>
    <sourceFile> "" </sourceFile>
    <refs> .Map </refs>
    <extrinsics> .Map </extrinsics>
    <extrs>
      <definedSubrs>
        <subr multiplicity="*">
          <id> -1 </id>
          <attrs> .Map </attrs> /* "args", "returns"?, "env", "extrScope"  */
          <body> . </body>
        </subr>
      </definedSubrs>
      <definedMacros>
        <macro multiplicity="*">
          <mid> -1 </mid>
          <mruntime> -3 </mruntime>
          <mkeys> .Set </mkeys>
          <menv> .Map </menv>
          <mextrScope> -1 </mextrScope>
          <mbody> . </mbody>
        </macro>
      </definedMacros>
      <unscannedSubrs> .List </unscannedSubrs>
    </extrs>
    <pendingImports> .List </pendingImports>
    <currentRuntime> 1 </currentRuntime>
    <nextRuntime> 2 </nextRuntime>
    <units>
      <unit multiplicity="*">
        <runtime> -1 </runtime>
        <loadedByRuntime> . </loadedByRuntime>
        <filepath> "" </filepath>
        <subrs> .Map </subrs>
        <macros> .Map </macros>
      </unit>
    </units>
    <loadedUnits> .Set </loadedUnits> /* Set of (runtime filepath) tuples :( */
    <extrinsicScopes> .Map </extrinsicScopes>
    <scopeBin> 1 </scopeBin>
    <startup> true </startup>
    <declScanInSubrs> false </declScanInSubrs>
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
  syntax K  ::= "unpackSourcePath" "(" Scalar ")"
              | "readSourceSeq" "(" Sequence ")"              [function]
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

  rule  <k>unpackSourcePath( . _ Val ) => . ...</k>
        <currentRuntime> Runtime </currentRuntime>
        <sourceFile> _ => Val </sourceFile>
        (.Bag => <unit>...
          <runtime> Runtime </runtime>
          <filepath>Val</filepath>
        ...</unit>)

  rule  <k>(.Map) => . ...</k>
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

  syntax Set  ::= "keySequence2Set" "(" Datas ")"              [function]
  rule  keySequence2Set(  .Datas  ) => .Set
  rule  keySequence2Set(  yval(|"?"| V), Rest:Datas ) => SetItem(V) keySequence2Set(  Rest  )
  rule  keySequence2Set(  yval(|T| V), Rest:Datas ) => SetItem(yval(|T| V)) keySequence2Set(  Rest  )
        requires T =/=String "?"
  rule  keySequence2Set(  V:FixedTypeData, Rest:Datas ) => SetItem(V) keySequence2Set(  Rest  )
endmodule
