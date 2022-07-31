(*
 * Abstract interface for GC types.
 *)
signature GC_TYPE =
sig

   type gctype 
   type ty = int  (* width of addressing mode *)

   val CONST  : IntInf.int -> gctype                   (* integer constant *) 

   val INT    : gctype                          (* machine integer *)
   val REAL32 : gctype                          (* machine float *)
   val REAL64 : gctype                          (* machine float *)
   val PTR    : gctype                          (* gc pointers *)

   val ADD    : ty * gctype * gctype -> gctype  (* address arithmetic *)
   val SUB    : ty * gctype * gctype -> gctype  (* address arithmetic *)
   val BOT    : gctype
   val TOP    : gctype

   val ==     : gctype * gctype -> bool
   val join   : gctype * gctype -> gctype
   val meet   : gctype * gctype -> gctype

   val toString : gctype -> string

   (*
    * Annotations for gc type
    *)
   exception GCTYPE of gctype
   val GC_TYPE : gctype Annotations.property

end
