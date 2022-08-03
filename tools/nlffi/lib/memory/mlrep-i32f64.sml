(* mlrep-i32f64.sml
 *
 *   User-visible ML-side representation of certain primitive C types.
 *   x86/Sparc/PPC version (all ints: 32 bit, all floats: 64 bit)
 *
 * Copyright (c) 2004 by The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure MLRep = struct
    structure Signed = Int32
    structure LongLongSigned = Int64
    structure Unsigned = Word32
    structure LongLongUnsigned = Word64
    structure Real = Real64

    (* word-style bit-operations on integers... *)
    structure SignedBitops =
        IntBitOps (structure I = Signed structure W = Unsigned)
end
