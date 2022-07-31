(* (C) 1999 Lucent Technologies, Bell Laboratories *)

(* Lazy can't be a substructure of SMLofNJ because the magical
   property of the susp datatype would be lost in signature
   matching? DBM *)
structure Lazy =
  struct
    datatype susp = datatype PrimTypes.susp
  end
