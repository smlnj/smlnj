(* asdl.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Definition of the primitive ASDL types.
 *)

signature ASDL =
  sig

    datatype bool = datatype bool
    type int = int
    type uint = word
    type integer = IntInf.int
    type string = string
    type identifier = Atom.atom

    exception DecodeError

  end
