/* cfun-list.h
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * utility win32 C functions
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"WIN32"
#define CLIB_VERSION	"0.1"
#define CLIB_DATE	"October 11, 1996"
#endif

CFUNC("get_const",	   _ml_win32_get_const,	"string -> word32")
CFUNC("get_last_error",    _ml_win32_get_last_error, "unit -> word32")
CFUNC("reg_open_key", _ml_win32_REG_open_key_ex, "word32 * string * word32 -> word32")
CFUNC("reg_create_key", _ml_win32_REG_create_key_ex, "word32 * string * word32 -> word32")
CFUNC("reg_close_key", _ml_win32_REG_close_key_ex, "word32 -> unit")
CFUNC("reg_delete_key", _ml_win32_REG_delete_key, "word32 * string -> unit")
CFUNC("reg_delete_value", _ml_win32_REG_delete_value, "word32 * string -> unit")
CFUNC("reg_enum_key", _ml_win32_REG_enum_key_ex, "word32 * word32 -> string option")
CFUNC("reg_enum_value", _ml_win32_REG_enum_value_ex, "word32 * word32 -> string option")
CFUNC("reg_query_value_type", _ml_win32_REG_query_value_type, "word32 * string -> word32")
CFUNC("reg_query_value_string", _ml_win32_REG_query_value_string, "word32 * string -> string")
CFUNC("reg_query_value_multi_string", _ml_win32_REG_query_value_multi_string, "word32 * string -> string")
CFUNC("reg_query_value_expand_string", _ml_win32_REG_query_value_expand_string, "word32 * string -> string")
CFUNC("reg_query_value_dword", _ml_win32_REG_query_value_dword, "word32 * string -> word32")
CFUNC("reg_query_value_binary", _ml_win32_REG_query_value_binary, "word32 * string -> Word8Vector.vector")
CFUNC("reg_set_value_dword", _ml_win32_REG_set_value_dword, "word32 * string * word32 -> unit")
CFUNC("reg_set_value_string", _ml_win32_REG_set_value_string, "word32 * string * string -> unit")
CFUNC("reg_set_value_expand_string", _ml_win32_REG_set_value_expand_string, "word32 * string * string -> unit")
CFUNC("reg_set_value_multi_string", _ml_win32_REG_set_value_multi_string, "word32 * string * string -> unit")
CFUNC("reg_set_value_binary", _ml_win32_REG_set_value_binary, "word32 * string * Word8Vector.vector -> unit")
CFUNC("config_get_version_ex", _ml_win32_CONFIG_get_version_ex, "unit -> word32 * word32 * word32 * word32 * string ")
CFUNC("config_get_windows_directory", _ml_win32_CONFIG_get_windows_directory, "unit -> string")
CFUNC("config_get_system_directory", _ml_win32_CONFIG_get_system_directory, "unit -> string")
CFUNC("config_get_computer_name", _ml_win32_CONFIG_get_computer_name, "unit -> string")
CFUNC("config_get_user_name", _ml_win32_CONFIG_get_user_name, "unit -> string")
CFUNC("config_get_volume_information", _ml_win32_CONFIG_get_volume_information, "string -> string * string * SysWord.word * int")
CFUNC("dde_start_dialog", _ml_win32_DDE_start_dialog, "string * string -> word32")
CFUNC("dde_execute_string", _ml_win32_DDE_execute_string, "word32 * string * int * int -> unit")
CFUNC("dde_stop_dialog", _ml_win32_DDE_stop_dialog, "word32 -> unit")
