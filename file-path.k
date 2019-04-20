module FILE-PATH

  syntax String ::= "#dirnamePath" "(" String ")"             [function]
                  | "#joinPath" "(" String "," String ")"     [function]

  rule  #dirnamePath( Value ) => substrString(Value, 0, rfindString(Value, "/", lengthString(Value)))
        requires rfindString(Value, "/", lengthString(Value)) >=Int 0
  rule  #dirnamePath( _ ) => "."                              [owise]

  rule  #joinPath(Left, Right) => Left +String "/" +String Right
endmodule
