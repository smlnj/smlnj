(* pre-os.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This sthe OS structure(s) with only types, so that the signatures can compile.
 *
 *)

structure OS =
  struct
    type syserror = int	    (* the integer code; we may need to beef this up *)

    structure Process =
      struct
	type status = int (* should this be Word8.word ?*)
      end

    structure IO =
      struct
	datatype iodesc = IODesc of int
(** This probably should be
	datatype iodesc = IODesc of Posix.FileSys.file_desc
 **)
      end

  end;

structure PreOS = OS;


