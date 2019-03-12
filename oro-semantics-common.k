require "oro-semantics-syntax.k"

module ORO-SEMANTICS-COMMON
  imports ORO-SEMANTICS-SYNTAX

  configuration <T>
    <k> readSourceSeq( $PGM:Sequence ) </k>
    <refs> .Map </refs>
    <extrinsics> .Map </extrinsics>
  </T>

  syntax K  ::= "readSourceSeq" "(" Sequence ")"
              | "yseqToK" "(" Datas ")"                       [macro]

  syntax Data ::= String
                | "yseq" "(" "|" String "|" Datas ")"
                | "ymap" "(" "|" String "|" Map ")"
                | "expr" "(" String ")"
                | "null"

  syntax Datas      ::= List{Data, ","}

  rule  <k>yseqToK( .Datas ) => . ...</k>
  rule  <k>yseqToK( ymap( | _ | M ),Ds ) => M ~> yseqToK( Ds ) ...</k>
endmodule
