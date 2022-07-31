(* pre-os.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the OS structure(s) with only types, for compiling signatures.
 *)

structure OS =
  struct
    structure W32G = Win32_General

    type syserror = W32G.word

    structure Process =
      struct
	type status = W32G.word
      end

    structure IO =
      struct
	datatype iodesc
	  = IODesc of Win32_General.hndl ref
	  | SockDesc of int
      end

  end;

structure PreOS = OS;
