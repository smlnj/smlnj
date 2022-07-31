(*
 * Abstract interface for GC types.
 *)
functor GCType(ObjType : OBJ_TYPE) : GC_TYPE =
struct

   structure O = ObjType

   type ty = int

   datatype gctype = 
     CONST of int                 (* integer constant *)
   | NONREF of O.objtype ref      (* non-reference value *)
   | REF of O.objtype ref         (* a reference, pointer to a gc object *)
   | ADD of ty * gctype * gctype  (* address arithmetic + *)
   | SUB of ty * gctype * gctype  (* address arithmetic - *)
   | BOT
   | TOP

   type gcmap = gctype Intmap.intmap

   exception GCTYPE

   fun int i = if i >= 0 then Int.toString i else "-"^Int.toString(~i)

   fun toString BOT = "bot"
     | toString TOP = "top"
     | toString (CONST i) = int i
     | toString (NONREF(ref obj)) = "nonref "^O.toString obj
     | toString (REF(ref obj)) = "ref "^O.toString obj
     | toString (ADD(ty,a,b)) = "("^toString a^"+"^toString b^")"
     | toString (SUB(ty,a,b)) = "("^toString a^"-"^toString b^")"

   fun mapToString gcmap =
   let val lookup = Intmap.map gcmap
       fun f r = "{"^toString(lookup r)^"}" handle _ => "{?}"
   in  f end

   val GCMAP       = Annotations.new NONE : gcmap Annotations.property
   val GCSAFEPOINT = Annotations.newFlag ""

end
