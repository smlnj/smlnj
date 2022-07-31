(* Copyright (c) 1999 by Lucent Technologies *)
(* bindings.sml *)

(* types the bindings of program identifiers, including types,
 * labels, objects (variables and function names) in environments,
 * i.e. tidtabs and symbol tables *)

(* the old pidInfo corresponds to the identifier types now in Ast,
 * and the old symInfo to symBinding *)

structure Bindings =
struct

  datatype namedCtype
    = Struct of
        Tid.uid * (Ast.ctype * Ast.member option * LargeInt.int option) list
        (* pid is optional because of anonymous bit fields *)
    | Union of Tid.uid * (Ast.ctype * Ast.member) list
        (* pid is mandatory for unions *)
    | Enum of Tid.uid * (Ast.member * LargeInt.int) list
    | Typedef of Tid.uid * Ast.ctype

  (* type info contained in tidtabs bindings *)
  (* name = NONE for anonymous structs, unions, enums -- can't refer to it *)
  (* ntype = NONE means is a "partial" type -- has been used, but not defined *)
  type tidBinding =
    {name: string option,
     ntype: namedCtype option,
     global: bool, (* is it a top level definition? *)     
     location: SourceMap.location}

  (* info used in environment symbol tables *)

  (* coincidentally the same as Ast.member *)
  type typeIdInfo =
    {name: Symbol.symbol,
     uid : Pid.uid,        (* unique identifier *)
     location : SourceMap.location,
     ctype: Ast.ctype}

  (* type of bindings in symbol tables *)
  datatype symBinding
    = MEMBER of Ast.member
    | ID of Ast.id          (* objects and functions *)
    | TYPEDEF of typeIdInfo
    | TAG of typeIdInfo

end (* structure Bindings *)
