(* win32-io.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for hooks to Win32 IO system.
 *)

signature WIN32_IO =
  sig
    type hndl = Win32_General.hndl

    val setFilePointer' : (hndl * Position.int * Win32_General.word) -> Position.int

    val FILE_BEGIN : Win32_General.word
    val FILE_CURRENT : Win32_General.word
    val FILE_END : Win32_General.word

    val readVec : hndl * int -> Word8Vector.vector
    val readArr : hndl * Word8ArraySlice.slice -> int
    val readVecTxt : hndl * int -> CharVector.vector
    val readArrTxt : hndl * CharArraySlice.slice -> int

    val close : hndl -> unit

    val GENERIC_READ : Win32_General.word
    val GENERIC_WRITE : Win32_General.word

    val FILE_SHARE_READ : Win32_General.word
    val FILE_SHARE_WRITE : Win32_General.word

    val FILE_FLAG_WRITE_THROUGH : Win32_General.word
    val FILE_FLAG_OVERLAPPED : Win32_General.word
    val FILE_FLAG_NO_BUFFERING : Win32_General.word
    val FILE_FLAG_RANDOM_ACCESS : Win32_General.word
    val FILE_FLAG_SEQUENTIAL_SCAN : Win32_General.word
    val FILE_FLAG_DELETE_ON_CLOSE : Win32_General.word
    val FILE_FLAG_BACKUP_SEMANTICS : Win32_General.word
    val FILE_FLAG_POSIX_SEMANTICS : Win32_General.word

    val CREATE_NEW : Win32_General.word
    val CREATE_ALWAYS : Win32_General.word
    val OPEN_EXISTING : Win32_General.word
    val OPEN_ALWAYS : Win32_General.word
    val TRUNCATE_EXISTING : Win32_General.word

    val createFile : {
	    name:string,
	    access:Win32_General.word,
	    share:Win32_General.word,
	    mode:Win32_General.word,
	    attrs:Win32_General.word
	  } -> hndl

    val writeVec : hndl * Word8VectorSlice.slice -> int
    val writeArr : hndl * Word8ArraySlice.slice -> int
    val writeVecTxt : hndl * CharVectorSlice.slice -> int
    val writeArrTxt : hndl * CharArraySlice.slice -> int

    val STD_INPUT_HANDLE : Win32_General.word
    val STD_OUTPUT_HANDLE : Win32_General.word
    val STD_ERROR_HANDLE : Win32_General.word

    val getStdHandle : Win32_General.word -> hndl
  end
