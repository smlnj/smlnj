(* basis-structs.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * These are basis structures with only types, so that the basis signatures
 * can compile.
 *)

structure Int = struct type int = PrimTypes.int end
structure Int32 = struct type int = PrimTypes.int32 end
structure Int64 = struct type int = PrimTypes.int64 end
structure IntInf = struct type int = PrimTypes.intinf end

structure Word = struct type word = PrimTypes.word end
structure Word8 = struct type word = PrimTypes.word8 end
structure Word32 = struct type word = PrimTypes.word32 end
structure Word64 = struct type word = PrimTypes.word64 end

structure Real64 = struct type real = PrimTypes.real end

structure String = struct type string = PrimTypes.string end

(* structure aliases *)
structure FixedInt = Int64
structure LargeInt = IntInf
structure Position = Int64
structure LargeWord = Word64
structure Real = Real64
structure LargeReal = Real64
