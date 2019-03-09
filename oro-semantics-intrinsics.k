require "oro-semantics-common.k"

module ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-COMMON

  syntax Set  ::= "oroIntrinsics"                 [function]
  syntax Bool ::= "oroIsIntrinsic" "(" Map ")"    [function]

  rule oroIntrinsics => (
    // Program flow
    SetItem( SetItem("Oro/Core/Flow/Label") )
    SetItem( SetItem("Oro/Core/Flow/Go to label") )
    SetItem( SetItem("Oro/Core/Flow/Go to label") SetItem("if") )
    SetItem( SetItem("Oro/Core/Flow/Do") ) /* NOT a loop */

    // Variables
    SetItem( SetItem("Oro/Core/Variable/Define") SetItem("for") )
    SetItem( SetItem("Oro/Core/Variable/Address of") )

    // Subroutines
    SetItem( SetItem("Oro/Core/Subroutine/Define") SetItem("as") )
    SetItem( SetItem("Oro/Core/Subroutine/Define") SetItem("as") SetItem("returning") )
    SetItem( SetItem("Oro/Core/Subroutine/Import") SetItem("from") )
    SetItem( SetItem("Oro/Core/Subroutine/Abstract") )
    SetItem( SetItem("Oro/Core/Subroutine/Indirectly invoke") SetItem("with") )

    // Macros
    SetItem( SetItem("Oro/Core/Macro/Define") SetItem("as") )
    SetItem( SetItem("Oro/Core/Macro/Expand") SetItem("with substitutions") )
    SetItem( SetItem("Oro/Core/Define expression handler") )
    SetItem( SetItem("Oro/Core/Macro/Use from") )
  )

  rule oroIsIntrinsic( M ) => keys(M) in oroIntrinsics
endmodule
