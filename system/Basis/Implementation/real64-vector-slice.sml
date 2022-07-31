(* real64-vector-slice.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *
 * TODO: change to proper packed monomorphic representation
 *)

structure Real64VectorSlice :> MONO_VECTOR_SLICE
			           where type elem = real
				   where type vector = Real64Vector.vector
= struct

    open VectorSlice

    type elem = real
    type vector = Real64Vector.vector
    type slice = elem VectorSlice.slice

  end
