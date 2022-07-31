(* mono-array-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This simple functor allows easy construction of new monomorphic array
 * structures.
 *)

functor MonoArrayFn (type elem) :> MONO_ARRAY where type elem = elem
  = struct
    open Array
    type elem = elem
    type array = elem Array.array
    type vector = elem Vector.vector
  end

