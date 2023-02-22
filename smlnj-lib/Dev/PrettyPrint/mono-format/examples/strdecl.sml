(* smlnj-lib/PrettyPrint/examples/strdecl.sml, for PrettyPrint, Version 8.3 *)

local

  structure PP = PrettyPrint

in

datatype str
  = SVAR of string
  | STRUCT of decl list

and decl
  = VALd of string * string
  | TYPd of string * string
  | STRd of string * str

(* formatStr : (decl -> format) -> str -> format *)
fun formatStr _ (SVAR name) = PP.text name
  | formatStr fdecl (STRUCT decls) =
      PP.vBlock [PP.text "struct", indent 2 (PP.vBlock (map fdecl decls)), PP.text "end"]

(* formatDecl1 : decl -> format *)
and formatDecl1 decl = (case decl
       of VALd (name, def) => PP.pBlock [
              PP.hBlock [PP.text "val", PP.text name, PP.text "="],
              PP.indent 2 (PP.text def)
            ]
	| TYPd (name, def) => PP.pBlock [
              PP.hBlock [PP.text "type", PP.text name, PP.text "="],
              PP.indent 2 (PP.text def)
            ]
	| STRd (name, str) => PP.pBlock [
              PP.hBlock [PP.text "structure", PP.text name, PP.text "="],
              PP.indent 2 (PP.tryFlat (formatStr formatDecl1 str)))
            ]

fun formatDecl2 (STRd (name, STRUCT decls)) = PP.vBlock [
        PP.hBlock [PP.text "structure", PP.text name, PP.text "=", PP.text "struct"],
        PP.indent 2 (PP.vBlock (map formatDecl2 decls)),
        PP.text "end"
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

end;

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
