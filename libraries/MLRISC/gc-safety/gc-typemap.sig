signature GC_TYPEMAP =
sig
  
   structure GC : GC_TYPE

   type typemap = (CellsBasis.cell * GC.gctype) list

   val empty    : typemap
   val fromList : typemap -> typemap
   val ==       : typemap * typemap -> bool
   val meet     : typemap * typemap -> typemap
   val join     : typemap * typemap -> typemap
   val meets    : typemap list -> typemap
   val joins    : typemap list -> typemap
   val gen      : typemap * typemap -> typemap  
   val kill     : typemap * typemap -> typemap

end
