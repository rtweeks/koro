require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
    <refs> .Map </refs>
    <extrinsics> .Map </extrinsics>
    <labels> .Map </labels>
  </T>

  syntax K  ::= "readSourceSeq" "(" Sequence ")"
              | "yseqToK" "(" Datas ")"                       [macro]

  syntax Data           ::= TaggedScalar
                          | FixedTypeData
                          | "null"
  syntax TaggedScalar   ::= "yval" "(" "|" String "|" String ")"
  syntax FixedTypeData  ::= String
                          | "yseq" "(" "|" String "|" Datas ")"
                          | "ymap" "(" "|" String "|" Map ")"

  syntax Datas      ::= List{Data, ","}

  rule  <k>yseqToK( .Datas ) => . ...</k>
  rule  <k>yseqToK( ymap( | _ | M ),Ds ) => M ~> yseqToK( Ds ) ...</k>


  syntax K  ::= "containsQMarkYval" "(" Set ")"             [function]
              | "isQMarkYval" "(" Data ")"                  [function]
  rule  containsQMarkYval(.Set) => false
  rule  containsQMarkYval(SetItem(I) Is) => isQMarkYval(I) orElseBool containsQMarkYval(Is)
  rule  isQMarkYval( yval(|"?"| _) ) => true
  rule  isQMarkYval( yval(|T| _) ) => false
        requires T =/=String "?"
  rule  isQMarkYval( _:FixedTypeData ) => false
endmodule
