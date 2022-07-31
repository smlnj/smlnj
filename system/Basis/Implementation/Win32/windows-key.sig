(* windows-key.sig
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Signature with the windows basic flags for registry access levels
 *
 *)

signature WINDOWS_KEY =
  sig
    include BIT_FLAGS
    
    val allAccess : flags
    val createLink : flags
    val createSubKey : flags
    val enumerateSubKeys : flags
    val execute : flags
    val notify : flags
    val queryValue : flags
    val read : flags
    val setValue : flags
    val write : flags
  end 


