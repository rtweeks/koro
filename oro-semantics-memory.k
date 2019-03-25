module ORO-SEMANTICS-MEMORY
  imports MINT

  syntax Memory ::= "memory" "(" Bytes "/" Int ")"
                  | "initMemory" "(" Int ")"              [function]
  syntax Byte   ::= MInt
                  | "nil"
  syntax Bytes  ::= List{Byte, " "}
                  | "initBytes" "(" Int ")"               [function]

  rule  initMemory( N ) => memory(initBytes(N) / N)
  rule  initBytes( 0 ) => .Bytes
  rule  initBytes( N ) => (nil initBytes(N -Int 1))
        requires N >Int 0
endmodule
