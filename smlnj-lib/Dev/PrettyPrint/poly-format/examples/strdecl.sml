(* smlnj-lib/PrettyPrint/examples/strdecl.sml, for PrettyPrint, Version 8.3 *)

local

  open PrettyPrint

in

datatype str
  = SVAR of string
  | STRUCT of decl list

and decl
  = VALd of string * string
  | TYPd of string * string
  | STRd of string * str

(* formatStr : (decl -> format) -> str -> format *)
fun formatStr _ (SVAR name) = text name
  | formatStr fdecl (STRUCT decls) = 
      vblock [text "struct", indent 2 (vblock (map fdecl decls)), text "end"]

(* formatDecl1 : decl -> format *)
and formatDecl1 decl =
    (case decl
       of VALd (name, def) => pcat (hblock [text "val", text name, text "="], indent 2 (text def))
	| TYPd (name, def) => pcat (hblock [text "type", text name, text "="], indent 2 (text def))
	| STRd (name, str) => pcat (hblock [text "structure", text name, text "="],
				    indent 2 (tryFlat (formatStr formatDecl1 str))))

fun formatDecl2 (STRd (name, STRUCT decls)) =
      vblock
        [hblock [text "structure", text name, text "=", text "struct"],
	 indent 2 (vblock (map formatDecl2 decls)),
	 text "end"]
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
