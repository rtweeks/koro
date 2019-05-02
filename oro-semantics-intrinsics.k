require "oro-semantics-common.k"

module ORO-SEMANTICS-INTRINSICS
  imports ORO-SEMANTICS-COMMON

  syntax Set  ::= "oroIntrinsics"                 [function]
                | "setFromList" "(" List ")"      [function]
                | "skeysDefineSubr"               [function]
                | "skeysDefineFun"                [function]
                | "skeysImportFrom"               [function]
                | "skeysImportReturningFrom"      [function]
                | "skeysUseMacros"                [function]
                | "skeysLabel"                    [function]
                | "skeysDo"                       [function]
                | "skeysDefineVar"                [function]
                | "skeysDefineVarRet"             [function]
                | "skeysDefineMacro"              [function]
  syntax Bool ::= "oroIsIntrinsic" "(" Map ")"    [function]

  rule setFromList( .List ) => .Set
  rule setFromList( ListItem(V) Is ) => (SetItem(V) setFromList(Is))

  rule skeysDefineSubr => (
    SetItem("Oro/Core/Subroutine/Define")
    SetItem("as")
  )

  rule skeysDefineFun => (
    SetItem("Oro/Core/Subroutine/Define")
    SetItem("returning")
    SetItem("as")
  )

  rule skeysImportFrom => (
    SetItem("Oro/Core/Subroutine/Import")
    SetItem("from")
  )

  rule skeysImportReturningFrom => (
    SetItem("Oro/Core/Subroutine/Import")
    SetItem("returning")
    SetItem("from")
  )

  rule skeysUseMacros => SetItem("Oro/Core/Macro/Use from")
  rule skeysDefineMacro => (
    SetItem("Oro/Core/Macro/Define")
    SetItem("as")
  )

  rule skeysLabel => SetItem("Oro/Core/Flow/Label")

  rule skeysDo => SetItem("Oro/Core/Flow/Do")

  rule skeysDefineVar => (
    SetItem("Oro/Core/Variable/Define")
    SetItem("for")
  )

  rule skeysDefineVarRet => (
    skeysDefineVar
    SetItem("returning value of")
  )

  rule oroIntrinsics => (
    // Program flow
    SetItem( skeysLabel )
    SetItem( SetItem("Oro/Core/Flow/Go to label") )
    SetItem( SetItem("Oro/Core/Flow/Go to label") SetItem("if") )
    SetItem( skeysDo ) /* NOT a loop */

    // Variables
    SetItem( skeysDefineVar )
    SetItem( skeysDefineVarRet )
    SetItem( SetItem("Oro/Core/Variable/Address of") )

    // Subroutines
    SetItem( skeysDefineSubr )
    SetItem( skeysDefineFun )
    SetItem( skeysImportFrom )
    SetItem( skeysImportReturningFrom )
    SetItem( SetItem("Oro/Core/Subroutine/Abstract") )
    SetItem( SetItem("Oro/Core/Subroutine/Indirectly invoke") SetItem("with") )

    // Macros
    SetItem( skeysDefineMacro )
    SetItem( SetItem("Oro/Core/Macro/Expand") SetItem("with substitutions") )
    SetItem( SetItem("Oro/Core/Define expression handler") )
    SetItem( skeysUseMacros )
  )

  rule oroIsIntrinsic( M ) => keys(M) in oroIntrinsics
endmodule
