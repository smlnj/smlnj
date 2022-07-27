(* asdl-pickle.sig
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common interface for basic pickling/unpickling operations.
 *)

signature ASDL_PICKLE =
  sig

    include ASDL_READ_PICKLE
    include ASDL_WRITE_PICKLE

  end
