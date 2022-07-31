(* windows-reg.sml
 *
 * COPYRIGHT (c) 2008 Fellowship of SML/NJ
 *
 * Structure for windows registry access
 *
 *)

local
    structure SysWord = SysWordImp
    structure Key = Windows_KEY
    structure String = StringImp
in
structure Windows_REG : WINDOWS_REG =
  struct
    type hkey = Handle.t  (* HKEY is an alias for HANDLE *)

    val classesRoot     = 0wx80000000 : Word32.word
    val currentUser     = 0wx80000001 : Word32.word
    val localMachine    = 0wx80000002 : Word32.word
    val users           = 0wx80000003 : Word32.word
    val performanceData = 0wx80000004 : Word32.word
    val currentConfig   = 0wx80000005 : Word32.word
    val dynData         = 0wx80000006 : Word32.word

    fun cfun x = CInterface.c_function "WIN32" x

    datatype create_result
      = CREATED_NEW_KEY of hkey
      | OPENED_EXISTING_KEY of hkey
    val openKeyEx : hkey * string * Key.flags -> hkey = cfun "reg_open_key"
    fun createKeyEx (key, name, flags) = let
	  val createKey : (hkey * string * Key.flags) -> Word32.word = Unsafe.CInterface.c_function "WIN32" "reg_create_key"
	  in
	    case createKey(key, name, flags)
	     of 0w1 => CREATED_NEW_KEY(openKeyEx(key, name, flags))
	      | 0w2 => OPENED_EXISTING_KEY(openKeyEx(key, name, flags))
	      | x => raise Fail ("Key creation failed in an unknown way.")
	  end
    val closeKey : hkey -> unit = cfun "reg_close_key"
    val deleteKey : hkey * string -> unit = cfun "reg_delete_key"
    val deleteValue : hkey * string -> unit = cfun "reg_delete_value"
    val enumKeyEx : hkey * int -> string option = cfun "reg_enum_key"
    val enumValueEx : hkey * int -> string option = cfun "reg_enum_value"

    val queryValueType : (Word32.word * string) -> Word32.word = cfun "reg_query_value_type";
    val queryValueString : (Word32.word * string) -> string = cfun "reg_query_value_string";
    val queryValueMultiString : (Word32.word * string) -> string list = cfun "reg_query_value_multi_string";
    val queryValueExpandString : (Word32.word * string) -> string = cfun "reg_query_value_expand_string";
    val queryValueDword : (Word32.word * string) -> Word32.word = cfun "reg_query_value_dword";
    val queryValueBinary : (Word32.word * string) -> Word8Vector.vector = cfun "reg_query_value_binary";
    val setValueDword : (Word32.word * string * Word32.word) -> unit = cfun "reg_set_value_dword";
    val setValueString : (Word32.word * string * string) -> unit = cfun "reg_set_value_string";
    val setValueExpandString : (Word32.word * string * string) -> unit = cfun "reg_set_value_expand_string";
    val setValueMultiString : (Word32.word * string * string list) -> unit = cfun "reg_set_value_multi_string";
    val setValueBinary : (Word32.word * string * Word8Vector.vector) -> unit = cfun "reg_set_value_binary";

    datatype value
      = SZ of string
      | DWORD of SysWord.word
      | BINARY of Word8Vector.vector
      | MULTI_SZ of string list
      | EXPAND_SZ of string

    (* val queryValueEx : hkey * string -> value option *)
    fun queryValueEx (key, name) =
	case queryValueType (key, name)
	 of 0w1 => SOME (SZ (queryValueString(key, name)))
	  | 0w2 => SOME (EXPAND_SZ (queryValueExpandString(key, name)))
	  | 0w3 => SOME (BINARY (queryValueBinary(key, name)))
	  | 0w4 => SOME (DWORD (queryValueDword(key, name)))
	  | 0w7 => SOME (MULTI_SZ (rev(queryValueMultiString(key, name))))
	  | x => NONE

    (* val setValueEx : hkey * string * value -> unit *)
    fun setValueEx (key, name, SZ(string)) =
	  setValueString(key, name, string)
      | setValueEx (key, name, DWORD(dw)) =
	  setValueDword(key, name, dw)
      | setValueEx (key, name, BINARY(bin)) =
	  setValueBinary(key, name, bin)
      | setValueEx (key, name, MULTI_SZ(multi)) =
	  setValueMultiString(key, name, multi)
      | setValueEx (key, name, EXPAND_SZ(expand)) =
	  setValueExpandString(key, name, expand)

  end

end (* local *)
