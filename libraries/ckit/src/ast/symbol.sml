(* Copyright (c) 1998 by Lucent Technologies *)

(* K&R2, A11.1, C names fall into four spaces. Same name may be used for
 * different purposes in the same scope if the uses are in different name
 * spaces. These spaces are:
 *
 *      0. Objects, functions, typedef names, and enum constants,
 *      1. Labels,
 *      2. Tags of structures, unions, and enumerations,
 *      3. Members of structures and unions individually.
 *)


structure Symbol :> SYMBOL = struct

  datatype symbolKind
    = OBJECT | FUNCTION | TYPEDEF | ENUMCONST | LABEL | TAG | MEMBER of Tid.uid

  type symbol = {name:string, hash:Word.word, kind:symbolKind, namespace:int}

  val hash = HashString.hashString

  fun namespace OBJECT       = 0
    | namespace FUNCTION     = 0
    | namespace TYPEDEF      = 0
    | namespace ENUMCONST    = 0
    | namespace LABEL        = 1
    | namespace TAG          = 2
    | namespace (MEMBER tid) = 3 + tid  (* DBM: beware negative tid *)

  fun symbol {name, kind} =
      {name=name, hash=hash name, kind=kind, namespace=namespace kind}

  fun name ({name,...}:symbol) = name

  fun kind ({kind,...}:symbol) = kind

  fun equal (sym1:symbol,sym2:symbol) =
      (#hash sym1) = (#hash sym2)           andalso
      (#namespace sym1) = (#namespace sym2) andalso
      (#name sym1) = (#name sym2)

  fun compare (sym1:symbol,sym2:symbol) =
      case Word.compare (#hash sym1,#hash sym2)
	of EQUAL => (case Int.compare (#namespace sym1,#namespace sym2)
		       of EQUAL => String.compare (#name sym1,#name sym2)
			| x => x)
	 | x => x

  fun object name       = symbol {name=name, kind=OBJECT}
  fun func name         = symbol {name=name, kind=FUNCTION}
  fun typedef name      = symbol {name=name, kind=TYPEDEF}
  fun enumConst name    = symbol {name=name, kind=ENUMCONST}
  fun label name        = symbol {name=name, kind=LABEL}
  fun tag name          = symbol {name=name, kind=TAG}
  fun member (tid,name) = symbol {name=name, kind=MEMBER tid}

end (* structure Symbol *)
