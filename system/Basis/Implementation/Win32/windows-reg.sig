(* windows-reg.sig
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Signature with the windows basic flags for registry access levels
 *
 *)

local
    structure Key = Windows_KEY
in
signature WINDOWS_REG = sig
    eqtype hkey
    
    val classesRoot     : hkey
    val currentUser     : hkey
    val localMachine    : hkey
    val users           : hkey
    val performanceData : hkey
    val currentConfig   : hkey
    val dynData         : hkey
    
    datatype create_result
      = CREATED_NEW_KEY of hkey
      | OPENED_EXISTING_KEY of hkey
    val createKeyEx : hkey * string * Key.flags
                        -> create_result
    val openKeyEx : hkey * string * Key.flags -> hkey
    val closeKey : hkey -> unit
    val deleteKey : hkey * string -> unit
    val deleteValue : hkey * string -> unit
    val enumKeyEx : hkey * int -> string option
    val enumValueEx : hkey * int -> string option
    
    datatype value
      = SZ of string
      | DWORD of SysWord.word
      | BINARY of Word8Vector.vector
      | MULTI_SZ of string list
      | EXPAND_SZ of string
    val queryValueEx : hkey * string -> value option
    val setValueEx : hkey * string * value -> unit
  end
end
