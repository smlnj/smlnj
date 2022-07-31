(* Copyright (c) 1998 by Lucent Technologies *)

(* K&R2, A11.1, C names fall into four spaces. Same name may be used for different
 * purposes in the same scope if the uses are in different name spaces. These spaces
 * are: 1. Objects, functions, typedef names, and enum constants.
 *      2. Labels
 *      3. Tags of structures, unions, and enumerations
 *      4. Member of structures are unions individually.
 *
 * Member is (id * id), the first id is the id of the structure and the second is the
 * real member. Whenever you look up a Member, you must know the struct_ty. If you
 * know the struct_ty, you know the first id (see CTYPE), and then you can get a 
 * unique mapping in the lookup.
 *
 *)

signature SYMBOL = sig

  datatype symbolKind = OBJECT | FUNCTION | TYPEDEF | ENUMCONST | LABEL | TAG | MEMBER of Tid.uid

  type symbol

  val symbol  : {name:string, kind:symbolKind} -> symbol
  val name    : symbol -> string
  val kind    : symbol -> symbolKind
  val equal   : symbol * symbol -> bool
  val compare : symbol * symbol -> order

  val label     : string -> symbol
  val object    : string -> symbol
  val func      : string -> symbol
  val typedef   : string -> symbol
  val enumConst : string -> symbol
  val tag       : string -> symbol
  val member    : Tid.uid * string -> symbol

end

