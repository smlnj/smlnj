(* smlnj-lib/PrettyPrint/examples/strdecl.sml, for PrettyPrint, Version 8.3 *)

local

  structure F = Formatting

in

datatype str
  = SVAR of string
  | STRUCT of decl list

and decl
  = VALd of string * string
  | TYPd of string * string
  | STRd of string * str

(* formatStr : (decl -> format) -> str -> format *)
fun formatStr _ (SVAR name) = F.text name
  | formatStr fdecl (STRUCT decls) = F.vBlock [
        F.text "struct",
        F.indent 2 (F.vBlock (map fdecl decls)),
        F.text "end"
      ]

(* formatDecl1 : decl -> format *)
and formatDecl1 decl = (case decl
       of VALd (name, def) => F.pBlock [
              F.hBlock [F.text "val", F.text name, F.text "="],
              F.indent 2 (F.text def)
            ]
	| TYPd (name, def) => F.pBlock [
              F.hBlock [F.text "type", F.text name, F.text "="],
              F.indent 2 (F.text def)
            ]
	| STRd (name, str) => F.pBlock [
              F.hBlock [F.text "structure", F.text name, F.text "="],
              F.indent 2 (F.tryFlat (formatStr formatDecl1 str))
            ]
      (* end case *))

fun formatDecl2 (STRd (name, STRUCT decls)) = F.vBlock [
        F.hBlock [F.text "structure", F.text name, F.text "=", F.text "struct"],
        F.indent 2 (F.vBlock (map formatDecl2 decls)),
        F.text "end"
      ]
  | formatDecl2 decl = formatDecl1 decl

(* examples *)

val str1 = STRUCT [TYPd ("t", "int list"), VALd ("x", "3")]

val str2 = STRUCT [TYPd ("s", "bool"), VALd ("y", "true"), STRd ("S", str1)]

val strd1 = STRd ("A", str1)
val strd2 = STRd ("B", str2)

fun test fmt n = printFormatLW n fmt

val t11 = test (formatDecl1 strd1)
val t12 = test (formatDecl1 strd2)

val t21 = test (formatDecl2 strd1)
val t22 = test (formatDecl2 strd2)

end; (* local *)

(* results:

-- using formatStr1:

- val p = printFormat' (formatDecl1 strd1);
val p = fn : int -> unit
- p 80;
structure A = struct type t = int list val x = 3 end
val it = () : unit
- p 10;
structure A = struct
                type t = int list
                val x = 3
              end

-- using formatStr2:

- val p = printFormat' (formatDecl2 strd1);
val p = fn : int -> unit
- p 80;
structure A = struct type t = int list val x = 3 end
val it = () : unit
- p 10;
structure A =
struct
  type t = int list
  val x = 3
end


-- using formatDecl3:

val p = printFormat' (formatDecl3 strd1);
val p = fn : int -> unit
- p 80;
structure A = struct
  type t = int list
  val x = 3
end

*)
