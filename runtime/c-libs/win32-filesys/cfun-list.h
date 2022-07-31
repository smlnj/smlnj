/* cfun-list.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * win32 C functions for IO
 */

#ifndef CLIB_NAME
#define CLIB_NAME	"WIN32-FILESYS"
#define CLIB_VERSION	"0.1"
#define CLIB_DATE	"October 15, 1996"
#endif

CFUNC("find_first_file",		_ml_win32_FS_find_first_file, "")
CFUNC("find_next_file",			_ml_win32_FS_find_next_file, "")
CFUNC("find_close",			_ml_win32_FS_find_close, "")
CFUNC("set_current_directory",		_ml_win32_FS_set_current_directory, "")
CFUNC("get_current_directory",		_ml_win32_FS_get_current_directory, "")
CFUNC("create_directory",		_ml_win32_FS_create_directory, "")
CFUNC("remove_directory",		_ml_win32_FS_remove_directory, "")
CFUNC("get_file_attributes",		_ml_win32_FS_get_file_attributes, "")
CFUNC("get_file_attributes_by_handle",	_ml_win32_FS_get_file_attributes_by_handle, "")
CFUNC("get_full_path_name",		_ml_win32_FS_get_full_path_name, "")
CFUNC("get_file_size",			_ml_win32_FS_get_file_size, "")
CFUNC("get_file_size_by_name",		_ml_win32_FS_get_file_size_by_name, "")
CFUNC("get_file_time",			_ml_win32_FS_get_file_time, "")
CFUNC("set_file_time",			_ml_win32_FS_set_file_time, "")
CFUNC("delete_file",			_ml_win32_FS_delete_file, "")
CFUNC("move_file",			_ml_win32_FS_move_file, "")
CFUNC("get_temp_file_name",		_ml_win32_FS_get_temp_file_name, "")
