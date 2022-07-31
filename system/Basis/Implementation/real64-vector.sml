(* real64-vector.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Vectors of Real64.real values.
 * NOTE: currently, we do not have sufficient tag bits to use a packed
 * representation for this type.
 *)

structure Real64Vector : MONO_VECTOR
    where type elem = Real64.real
  = struct
    type elem = Real64.real
    type vector = elem Vector.vector
    
    val maxLen = Vector.maxLen

    val fromList = Vector.fromList
    val tabulate = Vector.tabulate

    val length   = Vector.length
    val sub      = Vector.sub
    val update   = Vector.update
    val concat   = Vector.concat

    val appi   = Vector.appi
    val app    = Vector.app
    val mapi   = Vector.mapi
    val map    = Vector.map
    val foldli = Vector.foldli
    val foldri = Vector.foldri
    val foldl  = Vector.foldl
    val foldr  = Vector.foldr

    val findi   = Vector.findi
    val find    = Vector.find
    val exists  = Vector.exists
    val all     = Vector.all
    val collate = Vector.collate

  (* added for Basis Library proposal 2015-003 *)
    val toList = Vector.toList
    val append = Vector.append
    val prepend = Vector.prepend

  end
