(*
 * Encoding the C type system in SML.
 *
 * DEBUG VERSION with CHECKED POINTER DEREFERENCING.
 * 
 *   (C) 2002, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume
 *)
signature C_DEBUG = sig
    exception NullPointer
    include C
end
