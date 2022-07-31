(* basis-time.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the basis Time structure with only the time type, so that the
 * basis signatures can compile.  It has to be in a separate file from
 * pre-basis-structs.sml, since it depends on the binding of LargeInt.
 *)

structure Time =
  struct
    datatype time = TIME of { usec : LargeInt.int }
  end;
